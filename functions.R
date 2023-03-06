#functions
rename_teams <-function(team){
  recode(as.factor(team),
         `Olympic (Women) - Canada` = "CAN",
         `Olympic (Women) - Olympic Athletes from Russia` = "RUS",
         `Olympic (Women) - Finland` = "FIN",
         `Olympic (Women) - United States` = "USA",
         `Olympic (Women) - Switzerland` = "SWZ",
         `St. Lawrence Saints` = "St_Lawrence",
         `Clarkson Golden Knights` = "Clarkson",
         `Boston Pride` = "Boston",
         `Minnesota Whitecaps` = "Minnesota",
         `Connecticut Whale` = "Connecticut",
         `Buffalo Beauts` = "Buffalo",
         `Toronto Six` = "Toronto",
         `Metropolitan Riveters` = "Metropolitan"
  )
}

name_leagues <-function(team){
  case_when(team == "CAN" | team == "RUS" | team == "USA" | team == "FIN" | team == "SWZ" ~ "olympic",
            team == "St_Lawrence" | team == "Clarkson" ~ "NCAA",
            team == "Boston" | team == "Minnesota" | team == "Connecticut" | team == "Buffalo"
            | team == "Toronto" | team == "Metropolitan" ~ "phf")
}

rename_events <- function(event){
  as.factor(case_when(event == "Faceoff Win" ~ "faceoff_win",
                      event == "Goal" ~ "goal",
                      event == "Shot" ~ "shot",
                      event == "Incomplete Play" ~ "incomplete_pass",
                      event == "Play" ~ "complete_pass",
                      event == "Dump In/Out" ~ "dump_in_out",
                      event == "Puck Recovery" ~ "puck_recovery",
                      event == "Zone Entry" ~ "zone_entry",
                      event == "Takeaway" ~ "takeaway",
                      event == "Penalty Taken" ~ "penalty"))
}

## PLOTTING of 2D density plot of all shots and goals in offensive zone by given team
    plot_team_shots_goals <- function(team, name, league, gpg){
     p<- plot_half_rink(ggplot()) +
      stat_density_2d_filled(data=shots_passes %>% 
               filter((event == "goal")  & team == !!team), 
             aes(x = x, y = y, fill = ..level.., alpha = ..level..),
             inherit.aes = FALSE,
             breaks = c(0, 0.00025, 0.0005, 0.00075, 0.001, 0.00125, 0.0015, 0.00175, 0.002, 0.00225, 0.0025, 0.003),
             geom = "polygon",
             size = 0.5) + 
              geom_point(data=shots_passes %>% 
               filter((event == "goal" | event == "shot") & one_timer == T & team == !!team), 
             aes(x = x, y = y, fill = event, color = event),
             size = 1.2, alpha = 0.7, shape = 21) + 
scale_color_manual(name = "Type of Event", 
                   values = c(goal = "#FF0061", shot = "green"), 
                   labels = c("One-Timer Goal", "One-Timer Shot"),
                   guide = guide_legend(override.aes = list(fill = c("red", "green"), color = c("red", "green"), size = 4))) +
scale_fill_manual(values = c(rev(brewer.pal(n = 11, name = "RdYlGn")), "red", "green"), guide = FALSE) +
      scale_alpha_discrete(guide = FALSE)+
  ggtitle(paste("Density Plot of All One-Timer Shots and Goals\nby", name, "(",league,")"))+
      ylim(-2,87)+
       labs(subtitle = paste("One-Timer Goals Per Game:", gpg))+
       theme(plot.subtitle = element_text(hjust = 0.5))
      
     rink_overlay(p)
    }
    
     plot_team_shots_no_goals <- function(team, name, league, gpg){
     p<- plot_half_rink(ggplot()) +
      stat_density_2d_filled(data=shots_passes %>% 
               filter((event == "goal")  & team == !!team), 
             aes(x = x, y = y, fill = ..level.., alpha = ..level..),
             inherit.aes = FALSE,
             breaks = c(0, 0.00025, 0.0005, 0.00075, 0.001, 0.00125, 0.0015, 0.00175, 0.002, 0.00225, 0.0025, 0.003),
             geom = "polygon",
             size = 0.5) + 
              geom_point(data=shots_passes %>% 
               filter((event == "goal" | event == "shot") & one_timer == T & team == !!team), 
             aes(x = x, y = y, fill = event, color = event),
             size = 1.2, alpha = 0.7, shape = 21) + 
scale_color_manual(name = "Type of Event", 
                   values = c(shot = "green"), 
                   labels = c("One-Timer Shot"),
                   guide = guide_legend(override.aes = list(fill = c("green"), color = c("green"), size = 4))) +
scale_fill_manual(values = c(rev(brewer.pal(n = 11, name = "RdYlGn")), "green"), guide = FALSE) +
      scale_alpha_discrete(guide = FALSE)+
  ggtitle(paste("Density Plot of All One-Timer Shots and Goals\nby", name, "(",league,")"))+
      ylim(-2,87)+
       labs(subtitle  = paste("One-Timer Goals Per Game:", gpg))
      
     rink_overlay(p)
     }
     
## Convert Regression Coefficients into percent change predicted probabilities
     summary(house_glm)
percent_change_prob <- function(coef) {
  (exp(coef) - 1) * 100
}

likelihood_goal <- function(coef) {
  exp(coef) / (1 + exp(coef))
}