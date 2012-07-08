library(ggplot2)
library(scales)

by.state.df <- data.frame(state = tolower(state.name),
                          Obama = NA)
state.polls <- ddply(pres.out, "state", summarise, 
                     Obama = mean(Obama, na.rm = TRUE))
state.polls <- state.polls[state.polls[, "state"] != "US", ]
state.polls[, "state"] <- tolower(state.name[state.abb %in% state.polls[, "state"]])

by.state.df[by.state.df[, "state"] %in% state.polls[, "state"], ] <- state.polls
by.state.df[, "state"] <- factor(by.state.df[, "state"])

states_map <- map_data("state")

ggplot(by.state.df, aes(map_id = state)) + geom_map(aes(fill = Obama), map = states_map) +
  expand_limits(x = states_map$long, y = states_map$lat)



