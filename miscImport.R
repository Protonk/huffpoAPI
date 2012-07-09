library(XML)

toStateAbb <- function(data, name) {
  data[, name] <- gsub("^\\s+|\\s+$", "", data[, name])
  sname <- tolower(state.name)
  # drop non-matching rows and rename
  if (is.character(data[, name])) {
    data <- data[tolower(data[, name]) %in% sname, ]
    data[, name] <- state.abb[match(tolower(data[, name]), sname)]
    return(data)
  } else {
    data <- data[tolower(as.character(data[, name])) %in% sname, ]
    data[, name] <- factor(state.abb[match(to.lower(as.character(data[, name])), sname)])
    return(data)
  }
}



# 2008 results from http://www.electoral-vote.com/evp2008/Pres/Final-2008.csv
results.2008 <- read.csv("http://www.electoral-vote.com/evp2008/Pres/Final-2008.csv", as.is = TRUE)
results.2008 <- results.2008[, c("State", "Obama.Pct", "McCain.Pct")]
results.2008 <- toStateAbb(results.2008, "State")

# registration data http://www.census.gov/hhes/www/socdemo/voting/publications/p20/2010/tables.html
reg.table <- read.csv("http://www.census.gov/hhes/www/socdemo/voting/publications/p20/2010/Table4a_2010.csv",
                      skip = 8, nrows = 52, header = FALSE, as.is = TRUE)
names(reg.table) <- c("State",
                      "Population",
                      "Citizen Population",
                      "Registered",
                      "Percent Registered",
                      "P.Registered MOE",
                      "Percent Citizen Registered",
                      "PC.Registered MOE",
                      "Total Voted",
                      "Percent Voted",
                      "P.Voted MOE",
                      "Percent Citizen Voted",
                      "PC.Voted MOE")
reg.table <- reg.table[, c("State", "Population", "Percent Registered", "Percent Voted")]
reg.table[, "Population"] <- gsub(",", "", reg.table[, "Population"])
reg.table <- toStateAbb(reg.table, "State")
                      
                      
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



# National Polls. Start of simple imputation

national.polls <- pres.out[pres.out[, "state"] == "US", c("Obama", "observations", "Merged.Approval")]

