library(ggplot2)

# Load up the data with
# pres.df <- presidentGrab()

# we make a few assumptions about structure. Thankfully most
# of them stem from the structure we created w/ presidentGrab()

# pres.scratch is our working file for most of this. We don't want to 
# do anything TOO destructive to pres.df in case we need some info but
# we want to be able to quickly generate graphics 

# Drop some extraneous variables
reduced.names <- c("id",
                   "pollster",
                   "avg_date",
                   "method",
                   "chart",
                   "state",
                   "subpopulation",
                   "observations",
                   "margin_of_error",
                   names(pres.df)[15:ncol(pres.df)])
pres.scratch <- pres.df[, reduced.names]

## DROP
## anything that drops rows goes here

### Drop based by row name
# Drop polls which are outlandishly far from 100%

# Job approval
# toss in a function to avoid global assignment
# I'll eventually make sure each is local
constrainApproval <- function(data) {
  rownames(data) <- as.character(1:nrow(data))
  approval.chart <- unique(as.character(data[grep("job-approval", data[, "chart"]), "chart"]))
  outcome.names <- names(data)[grep("approve|mixed|undecided|neither", names(data), ignore.case = TRUE)]
  int.sum <- rowSums(data[data[, "chart"] %in% approval.chart, outcome.names], na.rm = TRUE)
  range.drop <- names(int.sum[int.sum > 110 | int.sum < 80])
  # resulting rownames() have gaps
  data[!rownames(data) %in% range.drop, ]
}
pres.scratch <- constrainApproval(pres.scratch)

# Anything before the start of the year goes.
pres.scratch <- pres.scratch[pres.scratch[, "avg_date"] > as.Date("2012-01-01"), ]
# some polls don't indicate a date (this will improve as this package gets better
# at detecting them) and aren't all that helpful
# eventually we'll assign a guess based on id interpolation (german tank problem!)
pres.scratch <- pres.scratch[!is.na(pres.scratch[, "avg_date"]), ]


# Regenerate rownames after dropping
# see http://kbroman.wordpress.com/2012/03/21/row-names-in-data-frames-beware-of-1nrow/

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
pres.scratch[, fact.cols] <- lapply(pres.scratch[, fact.cols], as.factor)

## CONVERSION
## e.g. factor to numeric, etc.
# For state level polls we can operate on this as a factor but w/ >400 levels
# it doesn't make sense
pres.scratch[, "observations"] <- as.numeric(pres.scratch[, "observations"])
pres.scratch[, "margin_of_error"] <- as.numeric(pres.scratch[, "margin_of_error"])

## REIFY
## convert "approve, don't approve, etc" to some value for Obama and Romney

# "obama-job-approval" is coded into "Disapprove"/"Approve"/"Undecided"
# job approval by state is coded more expansively
catAppVariants <- function(data) {
  approval.chart <- unique(as.character(data[grep("job-approval", data[, "chart"]), "chart"]))
  app.index <- data[, "chart"] %in% approval.chart
  # group coding for approval, etc.
  approve.names <- names(data)[grep("[^dis]approve|^approve", names(data), ignore.case = TRUE)]
  disapprove.names <- names(data)[grep("disappp?rove", names(data), ignore.case = TRUE)]
  neutral.names <- names(data)[grep("mixed|undecided|neither", names(data), ignore.case = TRUE)]
  # create and fill columns
  data[, "Sum.Job.Neutral"] <- data[, "Sum.Job.Disapprove"] <- data[, "Sum.Job.Approve"] <- NA
  synth.cols <- c("Sum.Job.Neutral", "Sum.Job.Disapprove", "Sum.Job.Approve")
  data[app.index, synth.cols] <- lapply(list(neutral.names,
                                    disapprove.names,
                                    approve.names), 
                               function(x) rowSums(data[app.index, x], na.rm = TRUE))
  
  # group codes for "not voting" etc.
  vote.names <- names(data)[grep("vot|refused", names(data), ignore.case = TRUE)]
  data[, "Non.Response"] <- NA
  data[, "Non.Response"] <- rowSums(data[, vote.names], na.rm = TRUE)
  #group codes for "other"
  data[, "Other"] <- rowSums(data[, c("Other", "Depends")], na.rm = TRUE)
  
  #drop columns
  keep.cols <- names(data)[!names(data) %in% c(approve.names, disapprove.names,
                                               neutral.names, vote.names,
                                               "Depends", "Johnson")]
  data[data == 0] <- NA
  data <- data[, keep.cols]
  return(data)
}
pres.out <- catAppVariants(pres.scratch)
  
  
