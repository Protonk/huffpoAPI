library(RJSONIO)
## function to build GET requests for charts and polls
# builds the path (minus queries) so we don't have to worry about it later
huffurl <- function(path, format) {
  huffpo <- "http://elections.huffingtonpost.com/pollster/api"
  path <- paste(match.arg(path, c("charts", "polls")),
                 match.arg(format, c("json", "xml")), sep = ".")
  return(paste(huffpo, path, sep = "/"))
}

# Generate R objects from JSON, given path names

huffpoJSON <- function(path, format = "json", ...) {
  # Filter() is awesome. Drop null optional arguments and
  # preserve ordering (not important here, but still)
  arglist <- Filter(length, list(...))
  # Names on the left, values on the right
  query <- paste(names(arglist), sapply(arglist, `[`), sep = "=", collapse = "&")
  full.url <- paste(huffurl(path, format), query, sep = "?")
  fromJSON(full.url, nullValue = NA)
}