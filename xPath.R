### xPath Helper functions


###### xPathGen()
### Generate simple xPath arguments
# All child elements are optional
# nodes must be added in order but can be added as:
## el1 = "bob", el2 = "alice", el3 = "eve"
## el123 = "bob/alice/eve"
## elvector = c("bob", "alice", "eve")
# comparisons can be added (again, in order)
## elcomp = "c("bob", ">", "eve")
## elcomp = "bob>20"
# I assume the single quoted string is on the right if you
# offfer 3 elements.
# none of the arguments need be named. You can just pass
# a list if you want

xPathGen <- function(root, ...) {
  genComp <- function(x) {
    x <- unlist(x)
    if (length(x) == 3) {
      x[3] <- paste0("'",x[3], "'")
      x.flat <- paste(x, collapse = "")
    } else {
      x.flat <- x
    }
    paste0("[", x.flat, "]")
  }
  if (!is.list(...)) {list(...)}
  
  # plucks out comparisons and applies genComp (formatting them properly)
  # preserves order
  children <- Filter(length, ...)
  names(children) <- 1:length(children)
  children.comp <- lapply(Filter(function(x) grep("[<>=]", x), children), genComp)
  children[names(children.comp)] <- children.comp
  
  # The paste(c("arg1", "arg2"), collapse = "/") trick is to get around
  # unwanted behavior when what would be the 2nd arg is missing
  root <- paste0("//", root)
  root <- paste(c(root, gsub("^/|/$", "", unlist(children))), collapse = "/")
  gsub("/(?=\\[)", "", root, perl = TRUE)
}
