library(ggplot2)
library(scales)
library(grid)


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
Eta.tot[, "State"] <- factor(tolower(Eta.tot[, "State"]))

states_map <- map_data("state")

# map theme from https://groups.google.com/forum/?fromgroups#!topic/ggplot2/ckA8aZe3nDs
theme_map <- function(size=12) {
  o <-  list(axis.line=theme_blank(), 
             axis.text.x=theme_blank(), 
             axis.text.y=theme_blank(), 
             axis.ticks=theme_blank(), 
             axis.ticks.length=unit(0.3, "lines"), 
             axis.ticks.margin=unit(0.5, "lines"), 
             axis.title.x=theme_text(face="bold", size=20), 
             axis.title.y=theme_blank(), 
             legend.background=theme_rect(fill="white", colour=NA), 
             legend.key=theme_rect(colour="white"), 
             legend.key.size=unit(1.2, "lines"), 
             legend.position="right", 
             legend.text=theme_text(size=size*0.8), 
             legend.title=theme_text(size=size*0.8, face="bold", 
                                     hjust=0), 
             panel.background=theme_blank(), 
             panel.border=theme_blank(), 
             panel.grid.major=theme_blank(), 
             panel.grid.minor=theme_blank(), 
             panel.margin=unit(0, "lines"), 
             plot.background=theme_blank(), 
             plot.margin=unit(c(1, 1, 0.5, 0.5), "lines"), 
             plot.title=theme_text(size=size*1.2), 
             strip.background=theme_rect(fill="grey90", 
                                         colour="grey50"), 
             strip.text.x=theme_text(size=size*0.8), 
             strip.text.y=theme_text(size=size*0.8, angle=-90))  
  return(structure(o, class="options")) 
}

states_map <- map_data("state")

etaUnscaledPlot <- function() {
  yearlist <- llply(Eta.tot[, -1], function(x) data.frame(State = Eta.tot[, 1],
                                                          Eta = x))
  for (Y in 1:length(yearlist)) {
    xaxt.tit <- paste(names(Eta.tot)[Y + 1], "Ratio of Turnout to Registration - Darker is Higher", sep = " : ")
    p <- ggplot(data = yearlist[[Y]], aes(map_id = State)) + xlab(xaxt.tit) +
      geom_map(aes(fill = Eta), map = states_map) + theme_map() +
      expand_limits(x = states_map$long, y = states_map$lat) +
      opts(legend.position = "none")
    ggsave(filename = paste0("etaUnscaled", letters[Y], ".png"), plot = p,
           path = "~/R/Eta/unscaled", height = 6, width = 8, dpi = 100)
  }
}


etaUnscaledPlot()

# I just converted it to a gif with imagemagick. 
# something like
# convert -delay 200 -loop 0 *.png out.gif 
# should do the trick. 



