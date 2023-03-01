## PLOTTING of 2D density plot of all shots and goals in offensive zone by given team
    plot_team_shots_goals <- function(team, league){
     p<- plot_half_rink(ggplot()) +
      stat_density_2d_filled(data=shots_passes %>% 
               filter((event == "goal")  & team == team), 
             aes(x = x, y = y, fill = ..level.., alpha = ..level..),
             inherit.aes = FALSE,
             breaks = c(0, 0.00025, 0.0005, 0.00075, 0.001, 0.00125, 0.0015, 0.00175, 0.002, 0.00225, 0.0025, 0.003),
             geom = "polygon",
             size = 0.5) + 
              geom_point(data=shots_passes %>% 
               filter((event == "goal" | event == "shot") & one_timer == T & team == team), 
             aes(x = x, y = y, fill = event, color = event),
             size = 0.5, alpha = 0.7, shape = 21) + 
scale_color_manual(name = "Type of Event", 
                   values = c(goal = "#FF0061", shot = "green"), 
                   labels = c("One-Timer Goal", "One-Timer Shot"),
                   guide = guide_legend(override.aes = list(fill = c("red", "green"), color = c("red", "green"), size = 4))) +
scale_fill_manual(values = c(rev(brewer.pal(n = 11, name = "RdYlGn")), "red", "green"), guide = FALSE) +
      scale_alpha_discrete(guide = FALSE)+
  ggtitle(paste("Density Plot of All Shots and Goals\nby", team, "(",league,")"))+
      ylim(-2,87)
      
     rink_overlay(p)
    }