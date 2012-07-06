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

# Anything before the start of the year goes.
pres.scratch <- pres.scratch[pres.scratch[, "avg_date"] > as.Date("2012-01-01"), ]
# some polls don't indicate a date (this will improve as this package gets better
# at detecting them) and aren't all that helpful
# eventually we'll assign a guess based on id interpolation (german tank problem!)
pres.scratch <- pres.scratch[!is.na(pres.scratch[, "avg_date"]), ]

# Easier to consider "mixed" polls as at least partially automated
pres.scratch[pres.scratch[, "method"] == "Mixed", "method"] <- "Automated Phone"
pres.scratch[, "method"] <- factor(as.character(pres.scratch[, "method"]))

# For state level polls we can operate on this as a factor but w/ >400 levels
# it doesn't make sense
pres.scratch[, "observations"] <- as.numeric(pres.scratch[, "observations"])
pres.scratch[, "margin_of_error"] <- as.numeric(pres.scratch[, "margin_of_error"])

# relevel "chart" and drop some levels
pres.scratch[, "chart"] <- factor(as.character(pres.scratch[, "chart"]))

