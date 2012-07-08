library(XML)

toStateAbb <- function(data, name) {
  if (any(grepl("^[a-z]", data[, name]))) {
    sname <- tolower(state.name)
  } else {
    sname <- state.name
  }
  # drop non-matching rows and rename
  data <- data[data[, name] %in% sname, ]
  data[, name] <- state.abb[match(data[, name], sname)]
  return(data)
}



# 2008 results from http://www.electoral-vote.com/evp2008/Pres/Final-2008.csv
results.2008 <- read.csv("http://www.electoral-vote.com/evp2008/Pres/Final-2008.csv", as.is = TRUE)
results.2008 <- results.2008[, c("State", "Obama.Pct", "McCain.Pct")]
results.2008 <- toStateAbb(results.2008, "State")

# registration data http://www.census.gov/hhes/www/socdemo/voting/publications/historical/index.html

# electoral votes from http://en.wikipedia.org/wiki/List_of_electoral_votes_by_US_state
# Canonical, machine-readable sources for 2012 EVs are thin on the ground
EV.in <- htmlParse("http://en.wikipedia.org/wiki/List_of_electoral_votes_by_US_state")
EV.table <- readHTMLTable(EV.in)[[2]][, c(1,2)]
EV.table <- toStateAbb(EV.table, "State")
names(EV.table)[2] <- "2012 EV"

# senate polls from http://www.electoral-vote.com/evp2012/Senate/senate_polls.csv
# I know I have the same polls but this tells me who is (R) and who is (D)

senate.in <- htmlParse("http://www.electoral-vote.com/evp2012/Senate/senate_polls.html")
sen.table <- readHTMLTable(senate.in)[[1]]

sen.table[, names(sen.table)] <- llply(sen.table[, names(sen.table)], as.character)
names(sen.table) <- as.character(sen.table[1, ])
sen.table <- sen.table[-1, ]

sen.table <- toStateAbb(sen.table, "State")
sen.table[, c("D %", "R %", "I %")] <- llply(sen.table[, c("D %", "R %", "I %")],
                                             function(x) as.numeric(gsub("%", "", x = x)))

sen.table[, c("Start", "End")] <- llply(sen.table[, c("Start", "End")], 
                                        function(x) {
                                          as.Date(paste(x, "2012", sep = " "), 
                                                  format = "%b %d %Y")})
sen.table[grep("Unknown", sen.table[, "Democrat"]), "Democrat"] <- NA
sen.table[grep("Unknown", sen.table[, "Republican"]), "Republican"] <- NA
sen.table[!grepl("\\w+", sen.table[, "I"]), "I"] <- NA
