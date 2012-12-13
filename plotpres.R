library(ggplot2)
library(scales)

state.polls <- ddply(pres.out, "state", summarise, 
                     Obama = mean(Obama, na.rm = TRUE),
                     Merged = mean(Merged.Approval, na.rm = TRUE),
                     Polls = length(Obama))

state.polls <- state.polls[state.polls[, "state"] != "US", ]
state.polls[, "state"] <- tolower(state.name[state.abb %in% state.polls[, "state"]])

by.state.df <- merge(data.frame(state = tolower(state.name)), state.polls, by = "state")

by.state.df[, "state"] <- factor(by.state.df[, "state"])
by.state.df[, "Obama"] <- rowMeans(by.state.df[, c("Obama", "Merged")], na.rm = TRUE)

states_map <- map_data("state")


ggplot(by.state.df, aes(map_id = State)) + geom_map(aes(fill = Obama), map = states_map) +
  expand_limits(x = states_map$long, y = states_map$lat)



