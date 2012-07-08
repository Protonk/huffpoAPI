library(ggplot2)
library(animation)

reg.10 <- read.csv("http://www.census.gov/hhes/www/socdemo/voting/publications/p20/2010/Table4a_2010.csv",
											skip = 8, nrows = 52, header = FALSE, as.is = TRUE)
reg.06 <- read.csv("http://www.census.gov/hhes/www/socdemo/voting/publications/p20/2010/Table4a_2010.csv",
									 skip = 7, nrows = 52, header = FALSE, as.is = TRUE)
pres.vote <- read.csv("http://www.census.gov/hhes/www/socdemo/voting/publications/historical/tabA-5a.csv",
												skip = 7, nrows = 60, header = FALSE, as.is = TRUE)
pres.vote <- pres.vote[, 1:13]
# NA rows removed
pres.vote <- pres.vote[complete.cases(pres.vote), ]
names(pres.vote) <- c("State", 2008 - seq(0, 44, 4))
pres.reg <- read.csv("http://www.census.gov/hhes/www/socdemo/voting/publications/historical/tabA-5b.csv",
										 skip = 7, nrows = 51, header = FALSE, as.is = TRUE)
pres.reg <- pres.reg[, 1:13]
names(pres.reg) <- c("State", 2008 - seq(0, 44, 4))
cong.vote <- read.csv("http://www.census.gov/hhes/www/socdemo/voting/publications/historical/tabA-3a.csv",
											skip = 7, nrows = 51, header = FALSE, as.is = TRUE)
cong.vote <- cong.vote[, 1:12]
names(cong.vote) <- c("State",  2002 - seq(0, 40, 4))
cong.reg <- read.csv("http://www.census.gov/hhes/www/socdemo/voting/publications/historical/tabA-3b.csv",
										 skip = 7, nrows = 51, header = FALSE, as.is = TRUE)
cong.reg <- cong.reg[, 1:12]
names(cong.reg) <- c("State",  2002 - seq(0, 40, 4))


Eta.pres <- 1 - pres.vote[, -1]/pres.reg[, -1]
Eta.cong <- 1 - cong.vote[, -1]/cong.reg[, -1]
Eta.tot <- cbind(pres.vote[, 1], Eta.pres, Eta.cong)
Eta.tot[, "2010"] <- 1 - reg.10[-1, 12]/reg.10[-1, 7]
Eta.tot[, "2006"] <- 1 - reg.06[-1, 12]/reg.06[-1, 7]
names(Eta.tot)[1] <- "State"
Eta.tot <- Eta.tot[, c("State", sort(names(Eta.tot)[-1]))]
Eta.tot[, "State"] <- gsub("^\\s+|\\s+$", "", Eta.tot[, "State"])
Eta.tot <- Eta.tot[Eta.tot[, "State"] %in% state.name, ]
Eta.tot[, "State"] <- factor(Eta.tot[, "State"])


