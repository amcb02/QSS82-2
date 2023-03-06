install.packages("clarify")
library(clarify)

est <- sim_ame(sim(house_glm, n = 1000), var = "one_timer", verbose = T)
coef(est)
main_est <- sim(house_glm, n = 1000)
# Compute average marginal effects on risk difference
# (RD) and risk ratio (RR) scale
est <- transform(est,
RD = `E[Y(TRUE)]` - `E[Y(FALSE)]`,
RR = `E[Y(TRUE)]` / `E[Y(FALSE)]`)
# Compute confidence intervals and p-values,
# using given null values for computing p-values
summary(est, null = c(`RD` = 0, `RR` = 1))
# Same tests using normal approximation and alternate
# syntax for `null`
summary(est, null = c(NA, NA, 0, 1),
normal = TRUE)
# Plot the RD and RR with a reference distribution
plot(est, parm = c("RD", "RR"), reference = TRUE,
ci = FALSE)
plot(est, parm = c("RD", "RR"), ci = TRUE)


sim_adrf(main_est, var = "goal_dist", at = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10, 15, 20, 25, 30, 35, 40, 45, 50), verbose = T)

 mean(sim_ame(sim(house_glm, n = 500), var = "through_middle_shot", verbose = T))

marginal_means(house_glm, vcov = T, type = 'response')


             