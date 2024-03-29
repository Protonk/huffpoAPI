# User friendly URL generator of sorts.
# depends on huffurl()

# Fuzzy search for state names, exact or colloquial topic names 

huffpoFriendly <- function(path, format = "xml", state = NULL, topic = NULL, page = NULL) {
  trim <- function (x) {
    gsub("^\\s+|\\s+$", "", x)
  }
  # Not all query arguments are required (non are, actually)
  
  # State can be "US", in which case we won't find it among the state abbreviations
  # rather than add another if statement, we just add it in.
  if (!is.null(state)) {
    # Fuzzy search for full state names
    stateconvert <- function(state) {
      fullstate.parm <- sapply(c(state.name, "United States"), function(y) { agrep(y, state, ignore.case = TRUE)})
      # returns only the state we asked for
      fullstate.parm <- Filter(length, fullstate.parm)
      # if we got a match, convert the name to the abbreviation
      if (length(fullstate.parm) > 0) {
        state <- c(state.abb, "US")[c(state.name, "United States") %in% names(fullstate.parm)]
      } else {
        state <- match.arg(toupper(state), c(state.abb, "US"))
      }
    }
    state <- trim(state)
    state <- stateconvert(state)
  }
  
  if (!is.null(topic)) {
    topic.args <- c("house", "senate", "governor", "president","job approval", "primary")
    topic.API.value <- c("2012-house", "2012-senate", "2012-governor", "2012-president", 
                         "obama-job-approval", "2012-gop-primary")
    topic <- trim(topic)
    if (grepl("^2012|^obama", topic, ignore.case = TRUE)) {
      # hyphens as it will eventually go into a URL
      topic <- gsub("\\s+", "-", topic)
      topic <- match.arg(tolower(topic), topic.API.value)
    } else {
      topic <- match.arg(tolower(topic), topic.args)
      topic <- topic.API.value[topic.args %in% topic]
    }
  }
  
  # For now, page is ignored for non-poll requests
  if (path != "polls") {
    page <- NULL
  }
  # Filter() is awesome. Drop null optional arguments (even if specified as NULL)
  arglist <- Filter(length, list(state = state, topic = topic, page = page))
  # Names on the left, values on the right
  query <- paste(names(arglist), sapply(arglist, `[`), sep = "=", collapse = "&")
  return(paste(huffurl(path), query, sep = "?"))
}