library(caTools)
library(pROC)

#HOUSE EVENTS MODEL ACCURACY
{
# Split the data into training and testing sets
set.seed(123)
house_split = sample.split(house_events$goal, SplitRatio = 0.7)
house_train = subset(house_events, house_split == TRUE)
house_test = subset(house_events, house_split == FALSE)

# Fit the logistic regression model
house_train_glm = glm(goal ~ one_timer + behind_net_shot + through_middle_shot + shot_after_pass + goal_dist + shot_angle + period_seconds + traffic + advantage, data = house_train, family = "binomial")

# Make predictions on the test set
house_predictions = predict(house_train_glm, newdata = house_test, type = "response")
house_predicted_classes = ifelse(house_predictions > mean(house_events$prob), 1, 0)

# Create the confusion matrix
house_conf_matrix = table(house_test$goal, house_predicted_classes)
house_conf_matrix

# Calculate the accuracy
house_accuracy = sum(diag(house_conf_matrix))/sum(house_conf_matrix)
house_accuracy

# Calculate total predicted goals and actual goals scored for non-house shots
house_pred_goals <- sum(house_events$prob)
house_pred_goals
house_goals <- house_events%>%filter(event == "goal")%>%count()%>%as.numeric()
house_goals
house_goals_diff <- house_goals - house_pred_goals
house_goals_diff

#find difference in actual shot pct of house shots and mean of predicted shot pct
house_events_shot_pct - mean(house_events$prob)
}

#NON-HOUSE EVENTS MODEL ACCURACY
{
# Split the data into training and testing sets
set.seed(123)
non_house_split = sample.split(non_house_events$goal, SplitRatio = 0.7)
non_house_train = subset(non_house_events, non_house_split == TRUE)
non_house_test = subset(non_house_events, non_house_split == FALSE)

# Fit the logistic regression model
non_house_train_glm = glm(goal ~ one_timer + behind_net_shot + through_middle_shot + shot_after_pass + goal_dist + shot_angle + period_seconds + traffic + advantage, data = non_house_train, family = "binomial")

# Make predictions on the test set
non_house_predictions = predict(non_house_train_glm, newdata = non_house_test, type = "response")
non_house_predicted_classes = ifelse(non_house_predictions > non_house_events_shot_pct, 1, 0)

# Create the confusion matrix
non_house_conf_matrix = table(non_house_test$goal, non_house_predicted_classes)
non_house_conf_matrix

non_house_test_v_predicted_classes <- data.frame(non_house_test, non_house_predicted_classes, diff = case_when(
  non_house_test$goal == F & non_house_predicted_classes == 0 ~ "correct - true negative",
  non_house_test$goal == T & non_house_predicted_classes == 1 ~ "correct - true positive",
  non_house_test$goal == F & non_house_predicted_classes == 1 ~ "incorrect - false positive",
  non_house_test$goal == T & non_house_predicted_classes == 0 ~ "incorrect - false negative"))
# Calculate the accuracy
non_house_accuracy = sum(diag(non_house_conf_matrix))/sum(non_house_conf_matrix)
non_house_accuracy

# Calculate total predicted goals and actual goals scored for non-house shots
non_house_pred_goals <- sum(non_house_events$prob)
non_house_pred_goals
non_house_goals <- non_house_events%>%filter(event == "goal")%>%count()%>%as.numeric()
non_house_goals
non_house_goals_diff <- non_house_goals - non_house_pred_goals
non_house_goals_diff

#find difference in actual shot pct of non-house shots and mean of predicted shot pct
non_house_events_shot_pct - mean(non_house_events$prob)
}

#ALL SHOTS MODEL ACCURACY
{
# Split the data into training and testing sets
set.seed(123)
split = sample.split(shots$goal, SplitRatio = 0.7)
train = subset(shots, split == TRUE)
test = subset(shots, split == FALSE)

# Fit the logistic regression model
full_train_glm = glm(goal ~ one_timer + behind_net_shot + through_middle_shot + shot_after_pass + goal_dist + shot_angle + period_seconds + traffic + advantage, data = train, family = "binomial")

# Make predictions on the test set
predictions = predict(full_train_glm, newdata = test, type = "response")
predicted_classes = ifelse(predictions > mean(shots$prob), 1, 0)

test_v_predicted_classes <- data.frame(test, predicted_classes, diff = case_when(
  test$goal == F & predicted_classes == 0 ~ "correct - true negative",
  test$goal == T & predicted_classes == 1 ~ "correct - true positive",
  test$goal == F & predicted_classes == 1 ~ "incorrect - false positive",
  test$goal == T & predicted_classes == 0 ~ "incorrect - false negative"))
# Create the confusion matrix
conf_matrix = table(test$goal, predicted_classes)
conf_matrix

# Calculate the accuracy
accuracy = sum(diag(conf_matrix))/sum(conf_matrix)
accuracy

# Calculate total predicted goals and actual goals scored
full_pred_goals <- sum(shots$prob)
full_pred_goals
full_goals <- shots%>%filter(event == "goal")%>%count()%>%as.numeric()
full_goals
full_goals_diff <- full_goals - full_pred_goals
full_goals_diff

#find difference in actual shot pct of all shots and mean of predicted shot pct
full_shot_pct - mean(shots$prob)
}
