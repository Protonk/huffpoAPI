library(plyr)

# PollGrab works on a single poll with multiple possible child questions

pollGrab <- function(poll) {
  pollToDf <- function(parsed.list) {
    outcomes <- unlist(parsed.list[["percent"]])
    choices <- unlist(parsed.list[["choice"]])
    parsed.list[["choice"]] <- parsed.list[["percent"]] <-  NULL
    ques.names <- Filter(nchar, names(parsed.list))
    info <- unlist(parsed.list)
    poll.df <- matrix(c(info, outcomes), 1, length(c(info, outcomes)))
    poll.df <- data.frame(poll.df)
    names(poll.df) <- c("id", "pollster", "start_date", "end_date", "method", "source",
                   ques.names,
                   choices)
    return(poll.df)
  }
  questions <- poll[[7]]
  poll[[7]] <- NULL
  qualities <- do.call(rbind, poll)
  quesGrab <- function(q.in) {
    responses <- q.in[["responses"]]
    q.in[["responses"]] <- NULL
    resp.mat <- do.call(rbind, responses)
    resp.mat <- matrix(unlist(resp.mat), dim(resp.mat))
    resp.list <- list(choice = resp.mat[, 1],
                      percent = resp.mat[, 2],
                      subpopulation = resp.mat[1, 3],
                      observations = resp.mat[1, 4],
                      margin_of_error = resp.mat[1, 5])
    c(q.in, resp.list)
  }
  ques.flat <- llply(questions, function(x) c(qualities, quesGrab(x)))
  df.out <- llply(ques.flat, pollToDf)
  return(df.out)
}

# Not vectorized but the logic isn't the binding constraint
# as we're downloding this over the internet
# also it allows us to put in a wait statement if we want

stateDfGen <- function(state, topic) {
  n <- 10
  i <- 1
  while (n == 10) {
    page.in <- huffpoJSON(path = "polls", format = "json", 
                          state = state, topic = topic, page = i)
    assign(paste("poll.page", i, sep = ""), page.in)
    if (i == 1) {
      poll.full <- poll.page1
    } else {
      poll.full <- append(poll.full, get(paste("poll.page", i, sep = "")))
    }
    n <- length(laply(page.in, `[[`, 1))
    i <- i + 1
  }
  # some states have no polls for certain topics.
  if (length(poll.full) == 0) {
    return(NULL)
  }
  # pages of polls are lists of lists with each top element only having one child
  polls.by.question <- llply(poll.full, pollGrab)
  # each poll may have multiple questions, so some may have more than one
  # child. To keep pollToDf simple, we drop the first level of nesting
  polls.by.question <- unlist(polls.by.question, recursive = FALSE)
  # ldply to stitch each list into a data frame
  df.out <- ldply(polls.by.question)
  return(df.out)
}



