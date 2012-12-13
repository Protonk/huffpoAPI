library(plyr)

# PollGrab works on a single poll with multiple possible child questions

pollGrab <- function(poll) {
  pollToDf <- function(parsed.list) {
    mergeResponses <- function(res, res.name) {
      # Don't bother if it isn't job approval
      # some polls use 3 p's!
      if (any(grepl("appp?rove|favor|wrong", res.name, ignore.case = TRUE))) {
        # bin into 3 categories
        reg.vec <-  c("(^|\\s+)disappp?rove|wrong|unfavorable|negative",
                      "(^|\\s+)appp?rove|right|(^|\\s+)favorable|positive",
                      "undecided|neither|not heard enough")
        names.out <- c("Merged.Disapproval", "Merged.Approval", "Merged.Neutral")
        for (i in 1:3) {
          merge.ind <- grepl(reg.vec[i], res.name, ignore.case = TRUE)
          if (!any(merge.ind)) {
            return(list(outcomes = res, choices = res.name))
          }
          summed <- sum(as.numeric(res[merge.ind]), na.rm = TRUE)
          res <- append(res, as.character(summed))
          res.name <- append(res.name, names.out[i])
        }
        return(list(outcomes = res, choices = res.name))
      } else {
        return(list(outcomes = res, choices = res.name))
      }
    }
    if (merge = TRUE) {
      results <- mergeResponses(res = unlist(parsed.list[["percent"]]),
                                res.name = unlist(parsed.list[["choice"]]))
      outcomes <- results$outcomes
      choices <- results$choices
    } else {
      outcomes <- unlist(parsed.list[["percent"]])
      choices <- unlist(parsed.list[["choice"]])
    }                         
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
    # Some polls squish multiple subgroups into one set of responses
    # will eventually disambiguate these but drop for now.
    if (!all(is.na(resp.mat[, 4]))) {
      retain.ind <- which(resp.mat[, 4] == resp.mat[which.max(resp.mat[, 4]), 4])
      resp.mat <- resp.mat[retain.ind, ]
    }
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

stateDfGen <- function(state, topic = NULL, merge = FALSE) {
  n <- 10
  i <- 1
  while (n == 10) {
    page.in <- huffpoJSON(path = "polls", format = "json", 
                          state = state, topic = topic, page = i)
    assign(paste("poll.page", i, sep = ""), page.in)
    if (i == 1) {
      state.poll.full <- poll.page1
    } else {
      state.poll.full <- append(state.poll.full, get(paste("poll.page", i, sep = "")))
    }
    n <- length(laply(page.in, `[[`, 1))
    i <- i + 1
  }
  # some states have no polls for certain topics.
  if (length(state.poll.full) == 0) {
    return(NULL)
  }
  # pages of polls are lists of lists with each top element only having one child
  polls.by.question <- llply(state.poll.full, pollGrab)
  # each poll may have multiple questions, so some may have more than one
  # child. To keep pollToDf simple, we drop the first level of nesting
  polls.by.question <- unlist(polls.by.question, recursive = FALSE)
  # ldply to stitch each list into a data frame
  df.out <- ldply(polls.by.question)
  return(df.out)
}





