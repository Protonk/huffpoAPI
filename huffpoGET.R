library(httr)
library(XML)

huffpoGETmultiple <- function(path, format = "xml", state = NULL, topic = NULL, page = NULL) {

huffpo.base <- "http://elections.huffingtonpost.com"
path <- match.arg(tolower(path), c("charts", "polls"))

url.premod <- switch(path,
                     charts = "pollster/api/charts",
                     polls =  "pollster/api/polls")
url.premod <- paste(url.premod, switch(format, 
                                         json = ".json", 
                                         xml = ".xml"), 
                                         sep = "")
arg.names <- character(0)

if (!is.null(state) & !grepl("us", state, ignore.case = TRUE)) {
  state <- gsub("^\\s+|\\s+$", "", state)
  fullstate.parm <- sapply(state.name, function(y) { agrep(y, state, ignore.case = TRUE)})
  fullstate.parm <- Filter(length, fullstate.parm)
  if (length(fullstate.parm) > 0) {
    state <- state.abb[state.name %in% names(fullstate.parm)]
  } else {
    state <- match.arg(toupper(state), state.abb)
  }
  arg.names <- append(arg.names, "state")
}

if (!is.null(topic)) {
  topic.args <- c("house", "senate", "governor", 
                  "president","job approval", 
                  "primary")
  topic.API.value <- c("2012-house", "2012-senate", "2012-governor", 
                       "2012-president", "obama-job-approval", 
                       "2012-gop-primary")

  topic <- gsub("^\\s+|\\s+$", "", topic)
  if (grepl("^2012|^obama", topic, ignore.case = TRUE)) {
    topic <- gsub("\\s+", "-", topic)
    topic <- match.arg(tolower(topic), topic.API.value)
  } else {
    topic <- match.arg(tolower(topic), topic.args)
    topic <- topic.API.value[topic.args %in% topic]
  }
  arg.names <- append(arg.names, "topic")
}

if (path != "polls") {
  page <- NULL
} else {
  page <- as.numeric(page[1])
  arg.names <- append(arg.names, "page")
}

arg.values <- c(state, topic, page)
final.query <- paste(arg.names, arg.values, sep = "=", collapse = "&")

GET(url = huffpo.base, path = url.premod, query = final.query)

}