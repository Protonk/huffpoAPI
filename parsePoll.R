library(plyr)
library(RJSONIO)

# Poll grab works across pages of polls. So just load up a page (or more) from
# the API and use pollGrab(page) for whatever your page object name is

# singleQGrab() is only called inside another function.

singleQGrab <- function(poll) {
  if (length(poll) == 1) {
    poll <- poll[[1]]
  }
  responses <- poll[[5]]
  poll[[5]] <- NULL
  # these are the poll responses. We need to preserve their structure
  resp.mat <- do.call(rbind, responses)[, 1:2]
  # the rest do not vary across choice
  resp.list <- list(percent = unlist(resp.mat[, 2]),
                    choice = unlist(resp.mat[, 1]),
                    subpopulation = ifelse(is.null(responses[[1]][["subpopulation"]]), NA, responses[[1]][["subpopulation"]]),
                    observations = ifelse(is.null(responses[[1]][["observations"]]), NA, responses[[1]][["observations"]]),
                    margin_of_error = ifelse(is.null(responses[[1]][["margin_of_error"]]), NA, responses[[1]][["margin_of_error"]]))
  # some questions have no information on observations
  # this catches those and assigns an NA string
  if (length(Filter(length, poll)) == 4) {
    poll.out <- poll
  } else {
    names(pollnames) <- pollnames <- names(poll)
    pollnames[!pollnames %in% names(unlist(Filter(length, poll)))] <- NA
    pollnames[pollnames %in% names(unlist(Filter(length, poll)))] <- unlist(Filter(length, poll))
    poll.out <- as.list(pollnames)
  }
  return(c(poll.out, resp.list))
}

# Wrapper for singleQGrab specifically for polls with multiple questions
# have to match poll characteristics to varying questions

multQGrab <- function(poll, features) {
  spread <- lapply(poll, singleQGrab)
  poll.out <- list()
  for (i in seq_along(poll)) {
    poll.out[[i]] <- c(features, spread[[i]])
  }
  poll.out
}

pollGrab <- function(page) {
  # select poll info for each poll
  poll.mat <- mapply(function(x) lapply(page, `[[`, x), 1:6)
  poll.list <- alply(poll.mat, .margins = 1, fun = identity)
  # only the "questions" from each poll
  questions <- llply(page, `[[`, 7)
  # Which polls have 1 question or multiple
  singletons <- which(sapply(questions, length) == 1)
  multiples <- which(!sapply(questions, length) == 1)
  
  # seperate loops for single and multiple. Can maybe be done w/
  # plyr but might not be worth the trouble.
  
  single.out <- list()
  for (i in singletons) {
    single.out[[i]] <- c(poll.list[[i]], singleQGrab(questions[[i]]))
  }
  single.out <- Filter(length, single.out)
  multiple.out <- list()
  for (i in multiples) {
    m.intermediate <- multQGrab(questions[[i]], features = poll.list[[i]])
    multiple.out <- append(multiple.out, m.intermediate)
  }
  return(c(single.out, multiple.out))
}

# pollToDf operates on a single list. Forunately all the parsed lists have the same structure
# so you just call it w/ lapply, e.g. lapply(parsed.polls, pollToDf)
# afterwards you can run ldply(whatever) to merge to 1 df

pollToDf <- function(parsed.list) {
  outcomes <- unlist(parsed.list[["percent"]])
  choices <- unlist(parsed.list[["choice"]])
  parsed.list[["choice"]] <- parsed.list[["percent"]] <-  NULL
  info <- unlist(parsed.list)
  poll.df <- matrix(c(info, outcomes), 1, length(c(info, outcomes)))
  poll.df <- data.frame(poll.df)
  names(poll.df) <- c("id", "pollster", "start_date", "end_date", "method", "source",
                      Filter(nchar, names(parsed.list))[1:7],
                      choices)
  return(poll.df)
}


# Not vectorized but the logic isn't the binding constraint
# as we're downloding this over the internet
# also it allows us to put in a wait statement if we want

stateDownload <- function(state) {
  n <- 10
  i <- 1
  while (n == 10) {
    page.in <- fromJSON(huffpoSlim(path = "polls", format = "json", 
                                   state = state, page = i))
    assign(paste("poll.page", i, sep = ""), page.in)
    if (i == 1) {
      poll.full <<- poll.page1
    } else {
      poll.full <<- append(poll.full, get(paste("poll.page", i, sep = "")))
    }
    n <- length(laply(page.in, `[[`, 1))
    i <- i + 1
  }
  poll.full
}

# Calls each of the above functions. Accepts a state as an argument

stateDFgen <- function(state) {
  dl <- stateDownload(state)
  polls <- pollGrab(dl)
  df.out <- ldply(lapply(polls, pollToDf))
  # Poll responses were coded as factors
  for (i in 14:ncol(df.out)) {
    df.out[,i] <- as.numeric(as.character(df.out[,i]))
  }
  df.out[, "start_date"] <- as.Date(df.out[, "start_date"])
  df.out[, "end_date"] <- as.Date(df.out[, "end_date"])
  # Some longer polls might be better coded as avg. date
  df.out <- ddply(df.out, c("start_date", "end_date"), 
                  transform, avg_date = end_date - difftime(end_date, start_date)/2)
  df.out <- df.out[, c(1:4, ncol(df.out), 5:(ncol(df.out) - 1))]
  df.out[, "source"] <- as.character(df.out[, "source"])
  return(df.out)
}

