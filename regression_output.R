library(knitr)
library(stargazer)

list(kable(as.data.frame(summary(house_glm_1)$coefficients), format = 'simple', digits = 3 ),
kable(as.data.frame(summary(house_glm_2)$coefficients), format = 'simple', digits = 3 ),
kable(as.data.frame(summary(house_glm_3)$coefficients), format = 'simple', digits = 3 ),
kable(as.data.frame(summary(house_glm_4)$coefficients), format = 'simple', digits = 3 ),
kable(as.data.frame(summary(house_glm_5)$coefficients), format = 'simple', digits = 3 ),
kable(as.data.frame(summary(house_glm)$coefficients), format = 'simple', digits = 3 ))


house_models <- list(house_glm_1, house_glm_2, house_glm_3, house_glm_4, house_glm_5, house_glm)
stargazer(house_models, type = "latex", title = "House Logistic Regression Models",
   dep.var.labels = c("goal"),
  covariate.labels = c("one_timerTRUE", "behind_net_shotTRUE", "through_middle_shotTRUE", "shot_after_passTRUE", "goal_dist", "shot_angle", "period_seconds,", "trafficTRUE", "advantage"),
  column.labels = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6"),
  out = "house_models.tex")


list(kable(as.data.frame(summary(non_house_glm_1)$coefficients), format = 'simple', digits = 3 ),
kable(as.data.frame(summary(non_house_glm_2)$coefficients), format = 'simple', digits = 3 ),
kable(as.data.frame(summary(non_house_glm_3)$coefficients), format = 'simple', digits = 3 ),
kable(as.data.frame(summary(non_house_glm_4)$coefficients), format = 'simple', digits = 3 ),
kable(as.data.frame(summary(non_house_glm_5)$coefficients), format = 'simple', digits = 3 ),
kable(as.data.frame(summary(non_house_glm)$coefficients), format = 'simple', digits = 3 ))


non_house_models <- list(non_house_glm_1, non_house_glm_2, non_house_glm_3, non_house_glm_4, non_house_glm_5, non_house_glm)
stargazer(non_house_models, type = "latex", title = "Non-House Logistic Regression Models",
   dep.var.labels = c("goal"),
  covariate.labels = c("one_timerTRUE", "behind_net_shotTRUE", "through_middle_shotTRUE", "shot_after_passTRUE", "goal_dist", "shot_angle", "period_seconds,", "trafficTRUE", "advantage"),
  column.labels = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6"),
  out = "non_house_models.tex")
