#initial steps
library(tidyverse)
games <- read_csv("games.csv")
view(games)
#selecting variables of interest 
games_winner<- games %>% select("rated", turns, "victory_status", "winner", white_rating, black_rating)
View(games_winner)
# QUESTION 1: Answering question of whether black or white wins more often
games_winner %>% count(winner)
#remove data containing games ending in "draws" as it is not of interest and plot
games_winner <- games_winner %>% filter(winner %in% c("black","white"))
ggplot(games_winner, aes(winner))+geom_bar()
#QUESTION 2: What is the victory status associated with a white or black win?
games_status<-games_winner %>% count(victory_status, winner)
view(games_status)
ggplot(games_status, aes(winner,n))+geom_col(aes(fill=victory_status), position ="dodge")
#QUESTION 3: what is the average number of turns associated with a black or white win and victory status
games_winner %>% group_by(winner,victory_status) %>% summarise(avg_turns=mean(turns)) %>% ggplot(aes(winner,avg_turns)) +geom_col() +facet_wrap(~victory_status)
#Question 4: what is the correlation between chess player rating and number of turns to win?
#solve by first merging white rating and black ratings into one coloumn known just as rating (use tidyr) and then do a group_by command!
#perhaps use mutate?
#the following is just for white_rating (winners only)

#the following is just for black_rating (winners only)
games_rating_black <-games_winner %>% filter(winner %in% c("black"))
ggplot(games_rating_black, aes(black_rating, turns))+geom_smooth(aes(colour=victory_status))
#QUESTION 5: The profile of a player winning in less than 5 moves using the popular sicilian defense opening
win_fast<- games %>% filter(turns<5)
win_fast<-win_fast %>% filter(winner %in% c("black", "white"))
win_fast <- win_fast %>% filter(opening_name=="Sicilian Defense")
ggplot(win_fast, aes(turns)) +geom_freqpoly(aes(colour=winner)) + facet_wrap(~victory_status)
#can separate like this: 
table3 %>% separate(rate, into= c("cases", "population"), sep= "+", convert=TRUE)
