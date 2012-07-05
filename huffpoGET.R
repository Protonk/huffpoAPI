## function to build GET requests for charts and polls
# builds the path (minus queries) so we don't have to worry about it later
huffurl <- function(path, format) {
  huffpo <- "http://elections.huffingtonpost.com/pollster/api"
  path <- paste(match.arg(path, c("charts", "polls")),
                 match.arg(format, c("json", "xml")), sep = ".")
  return(paste(huffpo, path, sep = "/"))
}

# Shorter function to generate queries
# flexible query construction, which allows the user to pass
# inalid queries. But it isn't in the way

huffpoSlim <- function(path, format = "json", ...) {
  # Filter() is awesome. Drop null optional arguments and
  # preserve ordering (not important here, but still)
  arglist <- Filter(length, list(...))
  # Names on the left, values on the right
  query <- paste(names(arglist), sapply(arglist, `[`), sep = "=", collapse = "&")
  return(paste(huffurl(path, format), query, sep = "?"))
}