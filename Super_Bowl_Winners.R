
Title: "Super Bowl Winners"
Author: "Gavin Baskette"
Date: Sys.Date()

#install.packages("ppcor")
#install.packages("randomForest")
#install.packages("glmnet")
#install.packages("corrplot")
library(corrplot)
library(data.table)
library(ggplot2)
library(ggthemes)
library(car)
library(lmtest)
library(caret)
library(pROC)
library(nnet)
library(MASS)
library(dplyr)
library(glmnet)
library(randomForest)
library(ppcor)
library(Matrix)


Super_bowl_winners_clean_2 <- read.csv("C:/Users/gavin/Downloads/intro_lessons_1-2_data_reviews_primers/Intro, Lessons 1-2, Data, Reviews, Primers/Data/Super_bowl_winners_clean_2.csv",
                                     header= TRUE)
Super_bowl_winners_clean_2

non_numeric_columns <- c("Team","Top.10.in.both.", "Top.5.in.either.",
                  "Highest.MVP.Finisher","Highest.OPOY.Finisher",
                    "Highest.DPOY.Finisher","Tight.End.Pro.Bowler.in.Past.5.Years.",
                      "Multiple.Time.Pro.Bowl.QB.","Career.playoff.Fgs.made")

numeric_data <- Super_bowl_winners_clean_2[, !(names(Super_bowl_winners_clean_2) %in% 
                                               non_numeric_columns)]

numeric_data <- numeric_data %>%
  mutate(across(everything(), ~ as.numeric(as.character(.))))

clean_numeric_data <- numeric_data[complete.cases(numeric_data), ]

clean_numeric_data <- clean_numeric_data[, -which(names(clean_numeric_data) == "Season")]

clean_numeric_data$dummy_y <- rnorm(nrow(clean_numeric_data))
lm_model <- lm(dummy_y ~ ., data = clean_numeric_data)

print(names(clean_numeric_data))

predictors <- clean_numeric_data[, !names(clean_numeric_data) %in% "dummy_y"]
target <- clean_numeric_data$dummy_y

cor_matrix <- cor(predictors)
high_corr <- which(abs(cor_matrix) > 0.9 & abs(cor_matrix) < 1, arr.ind = TRUE)
print(high_corr)

variables_to_drop <- c("SRS.Rank", "Point.Differential.Rank", "SOS.Rank", 
        "Win.pct..Vs..above..500", "QB.Playoff.Wins", "HC.Playoff.Wins ")  # Use high_corr results
predictors_reduced <- predictors[, !(names(predictors) %in% variables_to_drop)]

final_data <- cbind(predictors_reduced, dummy_y = target)

lm_model_final <- lm(dummy_y ~ ., data = final_data)

alias_results_final <- alias(lm_model_final)
print(alias_results_final)

final_reduced_data <- final_reduced_data[, !(names(final_reduced_data) %in% 
  c("X..of.FGs.made","X..of.starters.injuries","Win.pct..Vs..same.conference",
  "Away.win.pct.","Home.win.pct.","Win.pct..Vs..above..500","Close.Game.Win.."))]

lm_model_final <- lm(dummy_y ~ ., data = final_reduced_data)

apply(final_reduced_data, 2, var)

vif_values_final <- vif(lm_model_final)
print(vif_values_final)


# Original weights (z-scores from your dataset)
weights <- c(2.405960384, 2.057037189, 0.033, 2.792997976, 3.505122302,
             1.25085886, 1.43020342, 1.224083413, 1.047717137,
             1.084158978, 1.130376225, 1.321526518, 0.631412819,
             0.091818668, 0.800152815, 0.104851174, 0.808210297,
             1.572868699, 0.043925836)

# Actual VIF values from your earlier result
vif_values <- c(3337.146447, 5199.310042, 1167.165564, 18.521399, 
                11.415933 ,8.274397, 5.578469, 8.943843, 15.201023, 
                8.023721, 25.575662, 35.625605, 6.843405, 4.017534,
                15.635336, 3.833295, 22.429809, 1.949274, 3.195940)


# Calculate adjusted weights based on VIF
adjusted_weights <- weights / sqrt(log(vif_values))

# Optional: Normalize adjusted weights to maintain the same total as original weights
normalized_weights <- adjusted_weights / sum(adjusted_weights) * sum(weights)

# Create a summary table to compare
results <- data.frame(
  Variable = c("SRS", "Point Differential", "SOS", "Wins", "Seeding",
        "Second.half.wins", "Points.For.Rank", "Points.Against.Rank",
         "X..of.pro.bowlers.", "X..of.all.pro.players.",
          "Pre.Season.Odds", "Pre.Season.Odds.Rank", "QB.Playoff.Wins",
           "QB.Playoff.Win.pct.", "HC.Playoff.Wins", "HC.Playoff.Win.pct.",
            "Wins.prior.year", "Win.pct..Vs..other.conference",
             "Close.Games..Single.Digits."),
  Original_Weight = weights,
  VIF = vif_values,
  Adjusted_Weight = adjusted_weights,
  Normalized_Weight = normalized_weights
)

# Display results
print(results)





subset_data <- Super_bowl_winners_clean_2[, c("Points.For.Rank", "Points.Against.Rank", "SRS.Rank", "Point.Differential.Rank", "Wins.vs..above..500")]
lm_subset <- lm(Points.For.Rank ~ SRS.Rank + Point.Differential.Rank, Wins.vs..above..500, Points.Against.Rank, data = subset_data)
vif_subset <- vif(lm_subset)
print(vif_subset)

subset_data <- Super_bowl_winners_clean_2[, c("Points.For.Rank", "Points.Against.Rank", "Point.Differential", "Point.Differential.Rank", "Wins")]
lm_subset <- lm(Points.For.Rank ~ Point.Differential.Rank + Point.Differential, Wins, Points.Against.Rank, data = subset_data)
vif_subset <- vif(lm_subset)
print(vif_subset)

subset_data <- Super_bowl_winners_clean_2[, c("Win.pct..Vs..above..500", "Wins", "Seeding", "Second.half.wins", "Close.Game.Win..")]
lm_subset <- lm(Seeding ~ Win.pct..Vs..above..500 + Wins, Second.half.wins, Close.Game.Win.., data = subset_data)
vif_subset <- vif(lm_subset)
print(vif_subset)

subset_data <- Super_bowl_winners_clean_2[, c("Highest.OPOY.Finisher", "Points.For.Rank", "Point.Differential.Rank")]
subset_data$Highest.OPOY.Finisher <- as.numeric(ifelse(subset_data$Highest.OPOY.Finisher == "N/A", NA, subset_data$Highest.OPOY.Finisher))
str(subset_data)
lm_subset <- lm(Point.Differential.Rank ~ Highest.OPOY.Finisher + Points.For.Rank, data = subset_data)
vif_subset <- vif(lm_subset)
print(vif_subset)

subset_data <- Super_bowl_winners_clean_2[, c("HC.Playoff.Wins", "HC.Playoff.Experience", "QB.Playoff.Experience")]
lm_subset <- lm(QB.Playoff.Experience ~ HC.Playoff.Experience + HC.Playoff.Wins, data = subset_data)
vif_subset <- vif(lm_subset)
print(vif_subset)

subset_data <- Super_bowl_winners_clean_2[, c("Home.win.pct.", "Wins", "Seeding", "Second.half.wins")]
lm_subset <- lm(Seeding ~ Home.win.pct. + Wins, Second.half.wins, data = subset_data)
vif_subset <- vif(lm_subset)
print(vif_subset)

subset_data <- Super_bowl_winners_clean_2[, c("Home.win.pct.", "Away.win.pct.", "Wins", "Seeding", "Second.half.wins")]
lm_subset <- lm(Home.win.pct. ~ Away.win.pct. + Seeding, Wins, Second.half.wins, data = subset_data)
vif_subset <- vif(lm_subset)
print(vif_subset)

subset_data <- Super_bowl_winners_clean_2[, c("Kicker.s.FG.", "Kicker.s.career.FG.", "Career.playoff.Fgs.made")]
str(subset_data)
unique(subset_data$Career.playoff.Fgs.made)
subset_data$Career.playoff.Fgs.made <- gsub("[^0-9.]", "", subset_data$Career.playoff.Fgs.made)
subset_data$Career.playoff.Fgs.made <- as.numeric(subset_data$Career.playoff.Fgs.made)
lm_subset <- lm(Career.playoff.Fgs.made ~ Kicker.s.career.FG. + Kicker.s.FG., data = subset_data)
vif_subset <- vif(lm_subset)
print(vif_subset)

subset_data <- Super_bowl_winners_clean_2[, c("Wins.vs..above..500", "Wins", "Seeding", "Second.half.wins", "Close.Game.Win..")]
lm_subset <- lm(Seeding ~ Wins.vs..above..500 + Wins, Second.half.wins, Close.Game.Win.., data = subset_data)
vif_subset <- vif(lm_subset)
print(vif_subset)

subset_data <- Super_bowl_winners_clean_2[, c("Point.Differential.Rank", "Highest.MVP.Finisher", "Seeding", "Points.For.Rank")]
subset_data$Highest.MVP.Finisher <- as.numeric(ifelse(subset_data$Highest.MVP.Finisher == "N/A", NA, subset_data$Highest.MVP.Finisher))
str(subset_data)
lm_subset <- lm(Point.Differential.Rank ~ Highest.MVP.Finisher + Points.For.Rank, Seeding, data = subset_data)
vif_subset <- vif(lm_subset)
print(vif_subset)

subset_data <- Super_bowl_winners_clean_2[, c("Win.pct..Vs..above..500", "Wins", "Seeding", "Second.half.wins", "Close.Game.Win..")]
lm_subset <- lm(Second.half.wins ~ Close.Game.Win.. + Seeding, Wins, Win.pct..Vs..above..500, data = subset_data)
vif_subset <- vif(lm_subset)
print(vif_subset)

subset_data <- Super_bowl_winners_clean_2[, c("Win.pct..Vs..same.conference", "Wins", "Seeding", "Second.half.wins")]
lm_subset <- lm(Seeding ~ Win.pct..Vs..same.conference + Wins, Second.half.wins, data = subset_data)
vif_subset <- vif(lm_subset)
print(vif_subset)

subset_data <- Super_bowl_winners_clean_2[, c("Highest.DPOY.Finisher", "Wins", "Seeding", "Second.half.wins", "Points.Against.Rank")]
subset_data$Highest.DPOY.Finisher <- as.numeric(ifelse(subset_data$Highest.DPOY.Finisher == "N/A", NA, subset_data$Highest.DPOY.Finisher))
str(subset_data)
lm_subset <- lm(Points.Against.Rank ~ Highest.DPOY.Finisher + Wins, Seeding, data = subset_data)
vif_subset <- vif(lm_subset)
print(vif_subset)

subset_data <- Super_bowl_winners_clean_2[, c("Kicker.s.FG.", "Career...of.FGs.made", "Career.playoff.Fgs.made")]
lm_subset <- lm(Kicker.s.FG. ~ Career...of.FGs.made + Career.playoff.Fgs.made, data = subset_data)
vif_subset <- vif(lm_subset)
print(vif_subset)

subset_data <- Super_bowl_winners_clean_2[, c("X..of.starters.injuries", "Wins", "Close.Game.Win..")]
lm_subset <- lm(Wins ~ X..of.starters.injuries + Close.Game.Win.., data = subset_data)
vif_subset <- vif(lm_subset)
print(vif_subset)

subset_data <- Super_bowl_winners_clean_2[, c("Wins.vs..above..500", "SOS", "SOS.Rank", "SRS.Rank", "SRS")]
lm_subset <- lm(Wins.vs..above..500 ~ SOS.Rank + SOS, SRS.Rank, SRS, data = subset_data)
vif_subset <- vif(lm_subset)
print(vif_subset)

subset_data <- Super_bowl_winners_clean_2[, c("QB.Playoff.Experience", "QB.Playoff.Wins", "HC.Playoff.Experience")]
lm_subset <- lm(HC.Playoff.Experience ~ QB.Playoff.Experience + QB.Playoff.Wins, data = subset_data)
vif_subset <- vif(lm_subset)
print(vif_subset)

subset_data <- Super_bowl_winners_clean_2[, c("X..of.FGs.made", "Kicker.s.FG.", "Career.playoff.Fgs.made")]
lm_subset <- lm(Career.playoff.Fgs.made ~ X..of.FGs.made + Kicker.s.FG., data = subset_data)
vif_subset <- vif(lm_subset)
print(vif_subset)



original_weight <- c(3.190325042, 2.548257951, 0.795476701, 0.724381017, 
                     0.893790447, 1.286236781, 1.791610287, 0.350796929, 
                     0.478207202, 1.003860637, 0.603497783, 2.113973863, 
                     0.936394019, 0.346214898, 0.596453454, 0.818, 0.051, 
                     0.680505982, 0.420794618, 0.25216015)

vif_values <- c(22.1309, 68.65649, 6.334269, 1.842299, 19.82042, 10.00523, 
                6.356913, 1.324527, 7.229504, 6.460455, 4.611588, 12.76245, 
                2.199412, 2.887064, 2.887064, 1.018685, 6.413885, 30.99686, 
                2.852366, 2.852366)

adjusted_weights <- original_weight / sqrt(log(vif_values))

normalized_weight <- adjusted_weights / sum(adjusted_weights) * sum(original_weight)

results <- data.frame(
  Variable = c("SRS.Rank", "Point.Differential.Rank", "Win.pct..Vs..above..500", 
        "Highest.OPOY.Finisher", "HC.Playoff.Experience", "Home.win.pct.", 
        "Away.win.pct.", "Kicker.s.career.FG.", "Wins.vs..above..500", 
        "Highest.MVP.Finisher", "Close.Game.Win..", "Win.pct..Vs..same.conference", 
        "Highest.DPOY.Finisher", "Career...of.FGs.made", "Career.playoff.Fgs.made", 
        "X..of.starters.injuries", "SOS.Rank", "QB.Playoff.Experience", 
        "Kicker.s.FG.", "X..of.FGs.made"),
  Original_Weight = original_weight,
  VIF = vif_values,
  Adjusted_Weight = adjusted_weights,
  Normalized_Weight = normalized_weight
)

print(results)





#PCA approach:
pca_result <- prcomp(clean_numeric_data, scale. = TRUE)
summary(pca_result)
pca_data <- as.data.frame(pca_result$x)
summary(pca_data)


                               