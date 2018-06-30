library(dplyr)
install.packages("randomForest")
library(randomForest)
install.packages("caret")
library(caret)
library(ggplot2)
install.packages("e1071")
library(e1071)
install.packages("caTools")
library(caTools)

setwd("C:/Users/divyanair/Downloads")

fifadata2018<- read.csv("fifadata2018.csv", stringsAsFactors = FALSE, header = TRUE)
copy_fifadata2018 <-fifadata2018
nrow(fifadata2018)#471
ncol(fifadata2018)#100
colnames(fifadata2018)
str(fifadata2018)
fifadata2018$outcome<-as.factor(fifadata2018$outcome)
levels(fifadata2018$outcome)




fifa_rank <- read.csv("fifa_ranking.csv", stringsAsFactors = FALSE, header = TRUE)
fifa_rank <- fifa_rank[fifa_rank$country_full %in% fifadata2018, ]

elo_ratings <- read.csv("elo_ratings.csv", stringsAsFactors = FALSE, header = TRUE)

fifa_rank <- fifa_rank[fifa_rank$country_full %in% fifadata2018, ]

#Common function
countryrankings <- function(data) {
  return(fifa_rank %>% group_by(country_full, year = format(as.Date(rank_date), "%Y")) %>% arrange(desc(rank_date)) %>% slice(which.max(as.Date(rank_date))))
}

# taking only the latest ranking for a team in that year
latest <- countryrankings(fifa_rank)

# Combining Elo Rating for each country
latest <- merge(latest, elo_ratings, by.x = c("country_full", "year"), by.y = c("country", "year"), all.x = TRUE)

# Set seed
set.seed(100)

# Create Train and Test Datasets (70/30 split)
indices <- sample(1:nrow(fifadata2018), floor(0.7*nrow(fifadata2018)))
train = fifadata2018[indices,]
test = fifadata2018[-indices,]



##########################
# Random Forest Model
##########################




# Random Forest model with all features
rf <- randomForest(outcome ~ ., train, ntree = 20000, mtry = 6, nodesize = 0.01 * nrow(train))
predict_output <- predict(rf, test)


confusionMatrix(test$outcome, predict_output, positive = "1")


# Let us use feature engineering to improve accuracy and balance the sensitivity and specificity values

# Feature Engineering
# Which are the important variables to predict game outcome?
varImpPlot(rf, main = "Important features")

# Important features after feature engineering
important_features <- ("outcome ~ home_rank + away_rank + home_elo_rating + away_elo_rating + home_rank_change + 
away_rank_change + home_elo_rank_change + away_elo_rank_change + home_elo_rating_change + away_elo_rating_change + 
home_cur_year_avg + away_cur_year_avg + home_cur_year_avg_weighted + away_cur_year_avg_weighted + home_two_year_ago_avg + 
away_two_year_ago_avg + home_two_year_ago_weighted + away_two_year_ago_weighted + home_three_year_ago_avg + away_three_year_ago_avg + 
home_three_year_ago_weighted + away_three_year_ago_weighted + home_total_points + away_total_points + home_previous_points + away_previous_points")

formula_1 <- as.formula(important_features)

# Random Forest with selected features
rf_1 <- randomForest(formula_1, train, ntree = 20000, mtry = 2, nodesize = 0.01 * nrow(train))
predict_output_outcome <- predict(rf_1, test)
confusionMatrix(test$outcome, predict_output_outcome, positive = "1")


#functions to generate dataset
mapping <- function(rank_data, orig_data, home_team, away_team) {
  home <- rank_data[rank_data$country_full == home_team, ]
  home_except_country_name <- home[, -1]
  colnames(home_except_country_name) <- paste("home", colnames(home_except_country_name), sep = "_")
  home_team_data <- cbind(home_team=home$country_full, home_except_country_name)
  
  away_team_stats <- rank_data[rank_data$country_full == away_team, ]
  away_except_country_name <- away_team_stats[, -1]
  colnames(away_except_country_name) <- paste("away", colnames(away_except_country_name), sep = "_")
  away_team_data <- cbind(away_team=away_team_stats$country_full, away_except_country_name)
  
  game_data <- cbind(home_team_data, away_team_data)
  
  #  original result amendment
  original = orig_data[orig_data$home_team == home_team & orig_data$away_team == away_team & orig_data$game_year == "2018", "outcome"]
  if (length(original) == 0) {
    game_data <- cbind(game_data, original="N/A")
  } else {
    game_data <- cbind(game_data, actual_result=as.factor(original))
  }
  return(game_data)  
}

# Test predictions using Random Forest Model
# Create Test Data for Group gamees
rank <- c("country_full","rank", "elo_rating", "rank_change", "elo_rank_change", "elo_rating_change", "cur_year_avg", 
                        "cur_year_avg_weighted", "two_year_ago_avg", "two_year_ago_weighted", "three_year_ago_avg", 
                        "three_year_ago_weighted", "total_points", "previous_points")
finaldata <- as.data.frame(latest[latest$country_full %in% fifadata2018 & latest$year == "2018", which(colnames(latest) %in% rank)])


  

# Declared Group winners:
# Group A       Group B     Group C     Group D     Group E       Group F     Group G       Group H
# 1 Uruguay     Spain       France      Croatia     Brazil        Sweden      Belgium       Colombia
# 2 Russia      Portugal    Denmark     Argentina   Switzerland   Mexico      England       Japan



#predicting the Round 16, Quarter Finalists, Semi-Finalists and Runner-up and Winner 

# Knock out phase - round 16
 
# Since no draw so prediction has to be a value of 0 or 1

game1 <- mapping(finaldata, copy_fifadata2018, "Uruguay", "Portugal")
game2 <- mapping(finaldata, copy_fifadata2018, "France", "Argentina")
game3 <- mapping(finaldata, copy_fifadata2018, "Brazil", "Mexico")
game4 <- mapping(finaldata, copy_fifadata2018, "Belgium", "Japan")
game5 <- mapping(finaldata, copy_fifadata2018, "Spain", "Russia")
game6 <- mapping(finaldata, copy_fifadata2018, "Denmark", "Croatia")
game7 <- mapping(finaldata, copy_fifadata2018, "Sweden", "Switzerland")
game8 <- mapping(finaldata, copy_fifadata2018, "Colombia", "England")

knockout_game_data <- rbind(game1, game2, game3, game4, game5, game6, game7, game8)

# Using Random Forest to select winners of knockout gamees. Use type = prob to get probabilities of
# a team winning or losing
knockout_predict_outcomes <- as.data.frame(predict(rf_1, knockout_game_data, type = "prob"))
# As draw is not an outcome, decide winner by checking probability of a win or lose
knockout_game_data$pred_outcome <- as.factor(ifelse(knockout_predict_outcomes[3] > knockout_predict_outcomes[1], 1, 0))

print("---  Round 16 winners ---") 


# Winners from knockout phase - round 16
#  Spain Croatia Portugal France Brazil Switzerland England Belgium 

# Quarter-finals
game1 <- mapping(finaldata, copy_fifadata2018, "Portugal", "France")
game2 <- mapping(finaldata, copy_fifadata2018, "Brazil", "Belgium")
game3 <- mapping(finaldata, copy_fifadata2018, "Croatia", "Spain")
game4 <- mapping(finaldata, copy_fifadata2018, "Switzerland", "England")

quarterfinal_data <- rbind(game1, game2, game3, game4)

# Select winners of quarter-final gamees
quarterfinal_predict_outcomes <- as.data.frame(predict(rf_1, quarterfinal_data, type = "prob"))
quarterfinal_data$pred_outcome <- as.factor(ifelse(quarterfinal_predict_outcomes[3] > quarterfinal_predict_outcomes[1], 1, 0))

print("--- Predictions of Quarter Finals winners ---") 

# Spain  Portugal Switzerland  Belgium 

# Semi-finals
game1 <- mapping(finaldata, copy_fifadata2018, "Brazil", "Portugal")
game2 <- mapping(finaldata, copy_fifadata2018, "England", "Spain")

semifinal_data <- rbind(game1, game2)

# Select winners of semi-final gamees
semifinal_predict_outcomes <- as.data.frame(predict(rf_1, semifinal_data, type = "prob"))
semifinal_data$pred_outcome <- as.factor(ifelse(semifinal_predict_outcomes[3] > semifinal_predict_outcomes[1], 1, 0))

print("--- Semi Finals winners ---") 

#  Spain Brazil

# Finals
final <- mapping(finaldata, copy_fifadata2018, "Brazil", "Spain")
final_predict_outcome <- predict(rf_1, final, type = "prob")
final$pred_outcome <- as.factor(ifelse(final_predict_outcome[3] > final_predict_outcome[1], 1, 0))
print("---  Final winner ---") 

#Brazil is the final winner


