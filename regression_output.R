library(lmtest)
library(knitr)
library(stargazer)

#full EVENTS WALD TEST, LIKLIHOOD RATIO TEST, AND LOGISTIC REGRESSION OUTPUT
{
list(waldtest(full_glm_1), waldtest(full_glm_2), waldtest(full_glm_3), waldtest(full_glm_4),
     waldtest(full_glm_5), waldtest(full_glm))

list(kable(as.data.frame(waldtest(full_glm_1)), format = 'latex', digits = 3 ),
kable(as.data.frame(waldtest(full_glm_2)), format = 'latex', digits = 3 ),
kable(as.data.frame(waldtest(full_glm_3)), format = 'latex', digits = 3 ),
kable(as.data.frame(waldtest(full_glm_4)), format = 'latex', digits = 3 ),
kable(as.data.frame(waldtest(full_glm_5)), format = 'latex', digits = 3 ),
kable(as.data.frame(waldtest(full_glm)), format = 'latex', digits = 3 ))

list(lmtest::lrtest(full_glm_1), lmtest::lrtest(full_glm_2), lmtest::lrtest(full_glm_3), lmtest::lrtest(full_glm_4), 
     lmtest::lrtest(full_glm_5), lmtest::lrtest(full_glm))

list(kable(as.data.frame(lmtest::lrtest(full_glm_1)), format = 'latex', digits = 3 ),
kable(as.data.frame(lmtest::lrtest(full_glm_2)), format = 'latex', digits = 3 ),
kable(as.data.frame(lmtest::lrtest(full_glm_3)), format = 'latex', digits = 3 ),
kable(as.data.frame(lmtest::lrtest(full_glm_4)), format = 'latex', digits = 3 ),
kable(as.data.frame(lmtest::lrtest(full_glm_5)), format = 'latex', digits = 3 ),
kable(as.data.frame(lmtest::lrtest(full_glm)), format = 'latex', digits = 3 ))

full_models <- list(full_glm_1, full_glm_2, full_glm_3, full_glm_4, full_glm_5, full_glm)

list(kable(as.data.frame(summary(full_glm_1)$coefficients), format = 'latex', digits = 3 ),
kable(as.data.frame(summary(full_glm_2)$coefficients), format = 'latex', digits = 3 ),
kable(as.data.frame(summary(full_glm_3)$coefficients), format = 'latex', digits = 3 ),
kable(as.data.frame(summary(full_glm_4)$coefficients), format = 'latex', digits = 3 ),
kable(as.data.frame(summary(full_glm_5)$coefficients), format = 'latex', digits = 3 ),
kable(as.data.frame(summary(full_glm)$coefficients), format = 'latex', digits = 3 ))

stargazer(full_models, type = "latex", title = "Log-Odds of All Shots Logistic Regression Models",
   dep.var.labels = c("goal"),
  covariate.labels = c("one_timerTRUE", "behind_net_shotTRUE", "through_middle_shotTRUE", "shot_after_passTRUE", "goal_dist", "shot_angle", "period_seconds,", "trafficTRUE", "advantage"),
  column.labels = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6"),
  out = "full_models.tex")
}

#HOUSE EVENTS WALD TEST, LIKLIHOOD RATIO TEST, AND LOGISTIC REGRESSION OUTPUT
{
list(waldtest(house_glm_1), waldtest(house_glm_2), waldtest(house_glm_3), waldtest(house_glm_4),
     waldtest(house_glm_5), waldtest(house_glm))

list(kable(as.data.frame(waldtest(house_glm_1)), format = 'latex', digits = 3 ),
kable(as.data.frame(waldtest(house_glm_2)), format = 'latex', digits = 3 ),
kable(as.data.frame(waldtest(house_glm_3)), format = 'latex', digits = 3 ),
kable(as.data.frame(waldtest(house_glm_4)), format = 'latex', digits = 3 ),
kable(as.data.frame(waldtest(house_glm_5)), format = 'latex', digits = 3 ),
kable(as.data.frame(waldtest(house_glm)), format = 'latex', digits = 3 ))

list(lmtest::lrtest(house_glm_1), lmtest::lrtest(house_glm_2), lmtest::lrtest(house_glm_3), lmtest::lrtest(house_glm_4), 
     lmtest::lrtest(house_glm_5), lmtest::lrtest(house_glm))

list(kable(as.data.frame(lmtest::lrtest(house_glm_1)), format = 'latex', digits = 3 ),
kable(as.data.frame(lmtest::lrtest(house_glm_2)), format = 'latex', digits = 3 ),
kable(as.data.frame(lmtest::lrtest(house_glm_3)), format = 'latex', digits = 3 ),
kable(as.data.frame(lmtest::lrtest(house_glm_4)), format = 'latex', digits = 3 ),
kable(as.data.frame(lmtest::lrtest(house_glm_5)), format = 'latex', digits = 3 ),
kable(as.data.frame(lmtest::lrtest(house_glm)), format = 'latex', digits = 3 ))

house_models <- list(house_glm_1, house_glm_2, house_glm_3, house_glm_4, house_glm_5, house_glm)

list(kable(as.data.frame(summary(house_glm_1)$coefficients), format = 'latex', digits = 3 ),
kable(as.data.frame(summary(house_glm_2)$coefficients), format = 'latex', digits = 3 ),
kable(as.data.frame(summary(house_glm_3)$coefficients), format = 'latex', digits = 3 ),
kable(as.data.frame(summary(house_glm_4)$coefficients), format = 'latex', digits = 3 ),
kable(as.data.frame(summary(house_glm_5)$coefficients), format = 'latex', digits = 3 ),
kable(as.data.frame(summary(house_glm)$coefficients), format = 'latex', digits = 3 ))

stargazer(house_models, type = "latex", title = "Log-Odds of House Logistic Regression Models",
   dep.var.labels = c("goal"),
  covariate.labels = c("one_timerTRUE", "behind_net_shotTRUE", "through_middle_shotTRUE", "shot_after_passTRUE", "goal_dist", "shot_angle", "period_seconds,", "trafficTRUE", "advantage"),
  column.labels = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6"),
  out = "house_models.tex")
}

#NON-HOUSE EVENTS WALD TEST, LIKLIHOOD RATIO TEST, AND LOGISTIC REGRESSION OUTPUT
{
list(waldtest(non_house_glm_1), waldtest(non_house_glm_2), waldtest(non_house_glm_3), waldtest(non_house_glm_4),
     waldtest(non_house_glm_5), waldtest(non_house_glm))

list(kable(as.data.frame(waldtest(non_house_glm_1)), format = 'latex', digits = 3 ),
kable(as.data.frame(waldtest(non_house_glm_2)), format = 'latex', digits = 3 ),
kable(as.data.frame(waldtest(non_house_glm_3)), format = 'latex', digits = 3 ),
kable(as.data.frame(waldtest(non_house_glm_4)), format = 'latex', digits = 3 ),
kable(as.data.frame(waldtest(non_house_glm_5)), format = 'latex', digits = 3 ),
kable(as.data.frame(waldtest(non_house_glm)), format = 'latex', digits = 3 ))

list(lmtest::lrtest(non_house_glm_1), lmtest::lrtest(non_house_glm_2), lmtest::lrtest(non_house_glm_3), lmtest::lrtest(non_house_glm_4), 
     lmtest::lrtest(non_house_glm_5), lmtest::lrtest(non_house_glm))

list(kable(as.data.frame(lmtest::lrtest(non_house_glm_1)), format = 'latex', digits = 3 ),
kable(as.data.frame(lmtest::lrtest(non_house_glm_2)), format = 'latex', digits = 3 ),
kable(as.data.frame(lmtest::lrtest(non_house_glm_3)), format = 'latex', digits = 3 ),
kable(as.data.frame(lmtest::lrtest(non_house_glm_4)), format = 'latex', digits = 3 ),
kable(as.data.frame(lmtest::lrtest(non_house_glm_5)), format = 'latex', digits = 3 ),
kable(as.data.frame(lmtest::lrtest(non_house_glm)), format = 'latex', digits = 3 ))

list(kable(as.data.frame(summary(non_house_glm_1)$coefficients), format = 'latex', digits = 3 ),
kable(as.data.frame(summary(non_house_glm_2)$coefficients), format = 'latex', digits = 3 ),
kable(as.data.frame(summary(non_house_glm_3)$coefficients), format = 'latex', digits = 3 ),
kable(as.data.frame(summary(non_house_glm_4)$coefficients), format = 'latex', digits = 3 ),
kable(as.data.frame(summary(non_house_glm_5)$coefficients), format = 'latex', digits = 3 ),
kable(as.data.frame(summary(non_house_glm)$coefficients), format = 'latex', digits = 3 ))

kable(data.frame(house_likelihood_goal, 'P_value' = summary(house_glm)$coefficients[,4]), format = 'latex', digits = 4)

non_house_models <- list(non_house_glm_1, non_house_glm_2, non_house_glm_3, non_house_glm_4, non_house_glm_5, non_house_glm)
stargazer(non_house_models, type = "latex", title = "Log-Odds of Non-House Logistic Regression Models",
   dep.var.labels = c("goal"),
  covariate.labels = c("one_timerTRUE", "behind_net_shotTRUE", "through_middle_shotTRUE", "shot_after_passTRUE", "goal_dist", "shot_angle", "period_seconds,", "trafficTRUE", "advantage"),
  column.labels = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6"),
  out = "non_house_models.tex")
}
