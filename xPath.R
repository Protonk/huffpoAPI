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

# ONLY TAKES A SINGLE LIST for optional elements

xPathGen <- function(root, ...) {
  # comparisons still must be length 3 char vectors
  # with the third element as the quoted string
  genComp <- function(x) {
    comp <- unlist(x)
    if (length(comp) == 3) {
      comp[3] <- paste0("'",x[3], "'")
      comp.flat <- paste(comp, collapse = "")
    } else {
      comp.flat <- comp
    }
    paste0("[", comp.flat, "]")
  }

  # plucks out comparisons and applies genComp (formatting them properly)
  # preserves order
  children <- Filter(length, ...)
  names(children) <- 1:length(children)
  children.comp <- lapply(Filter(function(x) grep("[<>=]", x), children), genComp)
  children[names(children.comp)] <- children.comp
  
  # The paste(c("arg1", "arg2"), collapse = "/") trick is to get around
  # unwanted behavior when what would be the 2nd arg is missing
  root <- paste0("//", root)
  final <- paste(c(root, gsub("^/|/$", "", unlist(children))), collapse = "/")
  gsub("/(?=\\[)", "", final, perl = TRUE)
}