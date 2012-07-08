# Load up the data with
# pres.df <- presidentGrab()

presidentGrab <- function(merge = TRUE) {
  all.states <- as.list(c(state.abb, "US"))
  poll.full <- llply(all.states, stateDfGen, topic = "2012-president")
  df.out <- ldply(poll.full)
  return(df.out)
}

# we make a few assumptions about structure. Thankfully most
# of them stem from the structure we created w/ presidentGrab()

# pres.scratch is our working file for most of this. We don't want to 
# do anything TOO destructive to pres.df in case we need some info but
# we want to be able to quickly generate graphics 


# regexes for merging polls



# Drop some extraneous variables

varDrop <- function(data) {
  qualities <- c("id",
                 "pollster",
                 "avg_date",
                 "method",
                 "chart",
                 "state",
                 "subpopulation",
                 "observations",
                 "margin_of_error")
  results <- c("Obama",
               "Romney",
               "Other",
               "Undecided",
               "Merged.Disapproval",
               "Merged.Approval", 
               "Merged.Neutral")
  # add avg date
  data[, "start_date"] <- as.Date(data[, "start_date"])
  data[, "end_date"] <- as.Date(data[, "end_date"])
  data <- ddply(data, c("start_date", "end_date"), 
                        transform, avg_date = end_date - difftime(end_date, start_date)/2)
  return(data[, c(qualities, results)])
}
pres.scratch <- varDrop(pres.df)




## DROP
## anything that drops rows goes here

# the topic parameter isn't very good at filtering
pres.scratch <- pres.scratch[grep("president|obama|romney|job-approval", pres.scratch[, "chart"]), ]
# Anything before the start of the year goes.
pres.scratch <- pres.scratch[pres.scratch[, "avg_date"] > as.Date("2012-01-01"), ]


## After dropping
# Regenerate rownames and drop NA columns
# see http://kbroman.wordpress.com/2012/03/21/row-names-in-data-frames-beware-of-1nrow/
pres.scratch <- pres.scratch[, colSums(is.na(pres.scratch)) < nrow(pres.scratch)]
rownames(pres.scratch) <- as.character(1:nrow(pres.scratch))

## RELEVEL
## Anything that changes factor levels goes here

# Easier to consider "mixed" polls as at least partially automated
pres.scratch[pres.scratch[, "method"] == "Mixed", "method"] <- "Automated Phone"

# If it changes the factor level, make sure it is in fact.cols
fact.cols <- c("id", "pollster", "state", "method", "chart", "subpopulation")

# Convert to character first to remove factors associated w/ 0 rows
# relevel w/ lapply and assign to original columns
pres.scratch[, fact.cols] <- as.character(unlist(pres.scratch[, fact.cols]))
pres.scratch[, fact.cols] <- llply(pres.scratch[, fact.cols], as.factor)

## CONVERSION
## e.g. factor to numeric, etc.

# Simpler to mass-reclass columns here on one df

# lapply() works here because it retains the list structure. 
# laply() and sapply (and friends) cause problems
result.cols <- c("observations", "margin_of_error",
                 "Obama","Romney","Other",
                 "Undecided","Merged.Disapproval",
                 "Merged.Approval","Merged.Neutral")
pres.scratch[, result.cols] <- lapply(pres.scratch[, result.cols], function(x) as.numeric(as.character(x)))

  
pres.out <- pres.scratch