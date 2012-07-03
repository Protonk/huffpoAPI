## function to build GET requests for charts and polls

huffpoFriendly <- function(path, state = NULL, topic = NULL, page = NULL) {
	trim <- function (x) {
		gsub("^\\s+|\\s+$", "", x)
	}
	# builds the path (minus queries) so we don't have to worry about it later
	huffpo <- "http://elections.huffingtonpost.com/pollster/api"
	path <- paste(match.arg(tolower(path), c("charts", "polls")), "xml", sep = ".")
	baseurl <- paste(huffpo, path, sep = "/")
	# Not all query arguments are required (non are, actually)
	
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
		state <- trim(state)
		state <- stateconvert(state)
	}
	
	# Topic can be colloquial or exact.
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
	
	# For now, page isn't ignored for non-poll requests
	if (path != "polls") {
		page <- NULL
	}
	
	arg.names <- c("state", "topic", "page")
	arg.values <- c(state, topic, page)
	arg.index <- sapply(list(state, topic, page), function(x) !is.null(x))
	# if statement needed here because request can be made w/o queries
	query <- NULL
  out <- baseurl
	if (!is.null(arg.values)) {
		query <- paste(arg.names[arg.index], arg.values[arg.index], sep = "=", collapse = "&")
	  out <- paste(baseurl, query, sep = "?")
  }
	return(out)
}


# Much shorter function to generate queries
# Very rigid assumptions about correctness of inputs

huffpoSlim <- function(path, state = NULL, topic = NULL, page = NULL) {
  huffpo <- "http://elections.huffingtonpost.com/pollster/api"
  path <- paste(match.arg(tolower(path), c("charts", "chart", "polls")), "xml", sep = ".")
  baseurl <- paste(huffpo, path, sep = "/")
  
  if (is.null(c(state, topic))) {
		stop("Enter at least one query")
	}
	arg.names <- c("state", "topic", "page")
	arg.values <- c(state, topic, page)
	arg.index <- sapply(list(state, topic, page), function(x) !is.null(x))
  query <- paste(arg.names[arg.index], arg.values[arg.index], sep = "=", collapse = "&")
  return(paste(baseurl, query, sep = "?"))
}