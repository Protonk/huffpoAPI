library(plyr)

pollGrab <- function(page) {
  questionGrab <- function(poll) {
    responses <- poll[[1]][[5]]
    poll[[1]][[5]] <- NULL
    # these are the poll responses. We need to preserve their structure
    resp.mat <- do.call(rbind, responses)[, 1:2]
    # the rest do not vary across choice
    resp.list <- list(percent = resp.mat[, 2],
                      choice = resp.mat[, 1],
                      subpopulation = responses[[1]][["subpopulation"]],
                      observations = responses[[1]][["observations"]],
                      margin_of_error = responses[[1]][["margin_of_error"]])
    return(c(poll[[1]], resp.list))
  }
  # select poll info for each poll
  poll.mat <- mapply(function(x) lapply(page, `[[`, x), 1:6)
  poll.list <- alply(poll.mat, .margins = 1, fun = identity)
  # only the "questions" from each poll
  questions <- llply(page, `[[`, 7)
  questions.flat <- lapply(questions, questionGrab)
  for (i in seq_along(poll.list)) {
    poll.list[[i]] <- c(poll.list[[i]], questions.flat[[i]])
  }
  return(poll.list)
}
