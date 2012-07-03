### xPath Helper functions

# Generate simple xPath arguments
# Children and continuation are optional
# continuation e.g. //root[test>10]/one/two/three
# can be added w/ a variable like continuation = c("one", "two", "three")
# can also be specified like continuation = "one/two/three" 
# same w/ children
# arbitrarily many children and continuation variables can be added

xPathGen <- function(root, children = NULL, comp.arg = NULL, ...) {  
  # The paste(c("arg1", "arg2"), collapse = "/") trick is to get around
  # unwanted behavior when what would be the 2nd arg is missing
  root <- paste0("//", root)
  children <- unlist(Filter(length, list(children)))
  root <- paste(c(root, gsub("^/|/$", "", children)), collapse = "/")
  
  # Single quotes and brackets complicate this a bit
  # Numbers can be single quoted too and work fine
  if (length(comp.arg) == 3) {
    comp.arg[3] <- paste0("'",comp.arg[3], "'")
    comp.flat <- paste(comp.arg, collapse = "")
    root <- paste0(root, "[", comp.flat, "]")
  }
  remainder <- unlist(Filter(length, list(...)))
  paste(c(root, gsub("^/|/$", "", remainder)), collapse = "/")
}
