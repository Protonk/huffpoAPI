## function to send GET requests for charts and polls (not singular chart)

# httr is not on CRAN. In order to install it you need the development tools for R
# and package "devtools" (which is on CRAN, thankfully!)

# then you can get httr w/ install_github("httr")
library(httr)


huffpoGETbase <- function(path, format = "xml", state = NULL, topic = NULL, page = NULL) {

# builds the path (minus queries) so we don't have to worry about it later
huffpo.base <- "http://elections.huffingtonpost.com"
path <- match.arg(tolower(path), c("charts", "polls"))
url.premod <- switch(path,
                     charts = "pollster/api/charts",
                     polls =  "pollster/api/polls")
url.premod <- paste(url.premod, switch(format, 
                                         json = ".json", 
                                         xml = ".xml"), 
                                         sep = "")

# Not all arguments are required (non are, actually)
arg.names <- character(0)

# State can be "US", in which case we won't find it among the state abbreviations
# rather than add another if statement, we just add it in.
if (!is.null(state)) {
  # Fuzzy search for full state names
  stateconvert <- function(state) {
    state.abb.plus <- c(state.abb, "US")
    state.name.plus <- c(state.name, "United States")
    fullstate.parm <- sapply(state.name.plus, function(y) { agrep(y, state, ignore.case = TRUE)})
    # returns only the state we asked for
    fullstate.parm <- Filter(length, fullstate.parm)
    # if we got a match, convert the name to the abbreviation
    if (length(fullstate.parm) > 0) {
      state <- state.abb.plus[state.name.plus %in% names(fullstate.parm)]
    } else {
      state <- match.arg(toupper(state), state.abb.plus)
    }
  }
  state <- gsub("^\\s+|\\s+$", "", state)
  state <- stateconvert(state)
  arg.names <- append(arg.names, "state")
}

# Topic can be colloquial or exact.
if (!is.null(topic)) {
  topic.args <- c("house", "senate", "governor", 
                  "president","job approval", 
                  "primary")
  topic.API.value <- c("2012-house", "2012-senate", "2012-governor", 
                       "2012-president", "obama-job-approval", 
                       "2012-gop-primary")

  topic <- gsub("^\\s+|\\s+$", "", topic)
  if (grepl("^2012|^obama", topic, ignore.case = TRUE)) {
    # hyphens as it will eventually go into a URL
    topic <- gsub("\\s+", "-", topic)
    topic <- match.arg(tolower(topic), topic.API.value)
  } else {
    topic <- match.arg(tolower(topic), topic.args)
    topic <- topic.API.value[topic.args %in% topic]
  }
  arg.names <- append(arg.names, "topic")
}

# ignores page names otherwise takes only the first one
# this will eventually become a proper match statement
if (path != "polls") {
  page <- NULL
} else {
  page <- as.numeric(page[1])
  arg.names <- append(arg.names, "page")
}

# we've populated the names already. Any NULL object will not show up in
# arg.values. 

arg.values <- c(state, topic, page)
final.query <- NULL

if (!is.null(arg.values)) {
	final.query <- paste(arg.names, arg.values, sep = "=", collapse = "&")	
}
# all this for a call to GET()!
# this'll be sad if I put this into a UI and all the above is a waste
GET(url = huffpo.base, path = url.premod, query = final.query)
}