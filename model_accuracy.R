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
non_house_predicted_classes = ifelse(non_house_predictions > mean(non_house_events$prob), 1, 0)

# Create the confusion matrix
non_house_conf_matrix = table(non_house_test$goal, non_house_predicted_classes)
non_house_conf_matrix

# Calculate the accuracy
non_house_accuracy = sum(diag(non_house_conf_matrix))/sum(non_house_conf_matrix)
non_house_accuracy
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
predicted_classes = ifelse(predictions > 0.02949372, 1, 0)

# Create the confusion matrix
conf_matrix = table(test$goal, predicted_classes)
conf_matrix

# Calculate the accuracy
accuracy = sum(diag(conf_matrix))/sum(conf_matrix)
accuracy
}
