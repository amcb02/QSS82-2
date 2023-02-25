## ANDY MCBURNEY
## QSS 82
## WINTER 2023
## QUANTIFYING ONE-TIMER passes IN WOMEN"S ICE HOCKEY

{
  #libraries
  library(tidyverse)
  library(chron)
  library(remotes)
  library(tidymodels)
  library(arsenal)
  library(ggforce)
  library(cowplot)
  library(MplusAutomation)
  library(mclust)
  library(units)
  library(gridExtra)
  library(scales)
  library(ggpubr)
  library(RColorBrewer)
  library(splancs)
}

#download data from github
{
  phf_2021 <-
    read.csv(
      "https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/hackathon_nwhl.csv"
    )
  olympic_data <-
    read.csv(
      "https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/pxp_womens_oly_2022_v2.csv"
    )
  womens <-
    read.csv(
      "https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/hackathon_womens.csv"
    )

}

#source functions
{
source("plot_rink.R")
source("plot_half_rink.R")
source("functions.R")
source("rink_overlay.R")
source("upper_outline.R")
source("house.R")
}

#Reformatting, cleaning, and data combination
{
#Rename Columns
{
  colnames(olympic_data) <-
    c(
      "date",
      "year",
      "team",
      "opponent",
      "venue",
      "period",
      "period_sec_remaining",
      "situation",
      "goals_for",
      "goals_against",
      "player",
      "event",
      "event_success",
      "x",
      "y",
      "detail_1",
      "player_2",
      "x2",
      "y2",
      "detail_2",
      "traffic",
      "one_timer"
    )
  colnames(phf_2021) <-
    c(
      "date",
      "home_team",
      "away_team",
      "period",
      "clock",
      "home_skaters",
      "away_skaters",
      "home_goals",
      "away_goals",
      "team",
      "player",
      "event",
      "x",
      "y",
      "detail_1",
      "detail_2",
      "traffic",
      "one_timer",
      "player_2",
      "x2",
      "y2"
    )
  colnames(womens) <-
    c(
      "date",
      "home_team",
      "away_team",
      "period",
      "clock",
      "home_skaters",
      "away_skaters",
      "home_goals",
      "away_goals",
      "team",
      "player",
      "event",
      "x",
      "y",
      "detail_1",
      "detail_2",
      "traffic",
      "one_timer",
      "player_2",
      "x2",
      "y2"
    )
}

#Recode olympic data
{
  oly_num <- nrow(olympic_data)
  olympic_data_recode <- olympic_data %>%
    mutate(date = as.Date(date, tryFormats = c("%d/%m/%Y"))) %>%
    mutate(date = as.character(format(date, "%Y-%m-%d"))) %>%
    mutate(home_team = case_when(venue == "away" ~ opponent,
                                 venue == "home" ~ team)) %>%
    mutate(away_team = case_when(venue == "away" ~ team,
                                 venue == "home" ~ opponent)) %>%
    mutate(minutes = floor(period_sec_remaining / 60)) %>%
    mutate(seconds = period_sec_remaining %% 60) %>%
    separate(situation, c("team_skaters", "opponent_skaters"), sep = " on ") %>%
    mutate(home_skaters = as.integer(
      case_when(
        venue == "home" ~ team_skaters,
        venue == "away" ~ opponent_skaters
      )
    )) %>%
    mutate(away_skaters = as.integer(
      case_when(
        venue == "away" ~ team_skaters,
        venue == "home" ~ opponent_skaters
      )
    )) %>%
    mutate(home_goals = as.integer(
      case_when(venue == "home" ~ goals_for,
                venue == "away" ~ goals_against)
    )) %>%
    mutate(away_goals = as.integer(
      case_when(venue == "away" ~ goals_for,
                venue == "home" ~ goals_against)
    )) %>%
    mutate(event = as.factor(
      case_when(
        event == "Faceoff Win" ~ "faceoff_win",
        event == "Shot" &
          event_success == "t" ~ "goal",
        event == "Shot" &
          event_success == "f" ~ "shot",
        event == "Play" &
          event_success == "f" ~ "incomplete_pass",
        event == "Play" &
          event_success == "t" ~ "complete_pass",
        event == "Dump In/Out" ~ "dump_in_out",
        event == "Puck Recovery" ~ "puck_recovery",
        event == "Zone Entry" ~ "zone_entry",
        event == "Takeaway" ~ "takeaway",
        event == "Penalty Taken" ~ "penalty"
      )
    )) %>%
    mutate(detail_1 = factor(detail_1)) %>%
    mutate(detail_2 = factor(detail_2)) %>%
    mutate(traffic = as.logical(case_when(traffic == "t" ~ T,
                                          traffic == "f" ~ F))) %>%
    mutate(one_timer = as.logical(case_when(one_timer == "t" ~ T,
                                            one_timer == "f" ~ F))) %>%
    mutate(game_change = case_when(#create logical for when a new game is started
      period == 1 & minutes == 20 & event == "faceoff_win" ~ T,
      TRUE ~ F)) %>%
    mutate(row_num = row_number()) %>%
    mutate(
      Start_end = factor(
        ifelse(period == 1 &
                 minutes == 20 &
                 event == "faceoff_win", "start", "")
      ),
      #creates start of game and end of game variables
      Start_end = factor(ifelse((lead(Start_end) == "start" &
                                   period >= 3),
                                "end",
                                paste(Start_end)
      )),
      change_poss_two = factor(ifelse(
        Start_end != "",
        paste(Start_end),
        ifelse(lead(team) != team, "Turnover", "No Change")
      )),
      change_poss = factor(ifelse(
        Start_end != "",
        "No Change",
        ifelse(lead(team) != team, "Turnover", "No Change")
      ))
    ) %>%
    mutate(gameID = cumsum(game_change) + 28) %>% #create gameID that goes up by 1 for each new game
    mutate(x_dist = abs(x - x2)) %>%
    mutate(y_dist = abs(y - y2)) %>%
    mutate(dist = sqrt(x_dist ^ 2 + y_dist ^ 2)) %>%
    mutate(home_team = factor(rename_teams(home_team))) %>%
    mutate(away_team = factor(rename_teams(away_team))) %>%
    mutate(team = rename_teams(team)) %>%
    mutate(league = factor(name_leagues(team))) %>%
    mutate(final_home_score = case_when(Start_end == "end" ~ home_goals)) %>%
    mutate(final_away_score = case_when(Start_end == "end" ~ away_goals)) %>%
    mutate(x_goal_dist = abs(x - 188)) %>%
    mutate(y_goal_dist = abs(y - 42.5)) %>%
    mutate(goal_dist = sqrt(x_goal_dist ^ 2 + y_goal_dist ^ 2)) %>%
    mutate(shot_angle = asin(abs(y - 42.5) / goal_dist) * 180 / pi) %>%
    select(
      "date",
      "team",
      "home_team",
      "away_team",
      "period",
      "minutes",
      "seconds",
      "home_skaters",
      "away_skaters",
      "home_goals",
      "away_goals",
      "player",
      "event",
      "x",
      "y",
      "detail_1",
      "detail_2",
      "traffic",
      "one_timer",
      "player_2",
      "x2",
      "y2",
      "game_change",
      "row_num",
      "Start_end",
      "change_poss",
      "change_poss_two",
      "gameID",
      "x_dist",
      "y_dist",
      "dist",
      "league",
      "x_goal_dist",
      "y_goal_dist",
      "goal_dist",
      "shot_angle",
      -'year',
      -'period_sec_remaining',
      -'event_success',
      -'opponent'
    )
}

#combine womens and phf_2021
combined_data <- rbind(womens, phf_2021)

#total rows in combined_data
num <- nrow(combined_data)

#Combine all data frames
{
  combined_data_recode <- combined_data %>%
    mutate(clock = as.character(clock)) %>%
    separate(clock, c("minutes", "seconds"), sep = ":") %>%
    mutate(minutes = as.double(minutes)) %>%
    mutate(seconds = as.double(seconds)) %>%
    mutate(event = rename_events(event)) %>%
    mutate(traffic = as.logical(case_when(traffic == "t" ~ T,
                                          traffic == "f" ~ F))) %>%
    mutate(one_timer = as.logical(case_when(one_timer == "t" ~ T,
                                            one_timer == "f" ~ F))) %>%
    # mutate(date = as.Date(date, tryFormats = c("%Y-%m-%d")))%>%
    mutate(game_change = case_when(#create logical for when a new game is started
      period == 1 & minutes == 20 & event == "faceoff_win" ~ T,
      TRUE ~ F)) %>%
    mutate(row_num = row_number()) %>%
    mutate(
      Start_end = factor(
        ifelse(period == 1 &
                 minutes == 20 &
                 event == "faceoff_win", "start", "")
      ),
      #creates start of game and end of game variables
      Start_end = factor(ifelse((lead(Start_end) == "start" &
                                   period >= 3) | (row_num >= num),
                                "end",
                                paste(Start_end)
      )),
      change_poss_two = factor(ifelse(
        Start_end != "",
        paste(Start_end),
        ifelse(lead(team) != team, "Turnover", "No Change")
      )),
      change_poss = factor(ifelse(
        Start_end != "",
        "No Change",
        ifelse(lead(team) != team, "Turnover", "No Change")
      ))
    ) %>%
    mutate(gameID = cumsum(game_change)) %>% #create gameID that goes up by 1 for each new game
    mutate(x_dist = abs(x - x2)) %>%
    mutate(y_dist = abs(y - y2)) %>%
    mutate(dist = sqrt(x_dist ^ 2 + y_dist ^ 2)) %>%
    mutate(home_team = as.factor(rename_teams(home_team))) %>%
    mutate(away_team = as.factor(rename_teams(away_team))) %>%
    mutate(detail_1 = factor(detail_1)) %>%
    mutate(detail_2 = factor(detail_2)) %>%
    mutate(player = as.character(player)) %>%
    mutate(player_2 = as.character(player_2)) %>%
    mutate(team = rename_teams(team)) %>%
    mutate(league = factor(name_leagues(team))) %>%
    mutate(x_goal_dist = abs(x - 188)) %>%
    mutate(y_goal_dist = abs(y - 42.5)) %>%
    mutate(goal_dist = sqrt(x_goal_dist ^ 2 + y_goal_dist ^ 2)) %>%
    mutate(shot_angle = asin(abs(y - 42.5) / goal_dist) * 180 / pi) %>%
    select(
      "date",
      "team",
      "home_team",
      "away_team",
      "period",
      "minutes",
      "seconds",
      "home_skaters",
      "away_skaters",
      "home_goals",
      "away_goals",
      "player",
      "event",
      "x",
      "y",
      "detail_1",
      "detail_2",
      "traffic",
      "one_timer",
      "player_2",
      "x2",
      "y2",
      "game_change",
      "row_num",
      "Start_end",
      "change_poss",
      "change_poss_two",
      "gameID",
      "x_dist",
      "y_dist",
      "dist",
      "league",
      "x_goal_dist",
      "y_goal_dist",
      "goal_dist",
      "shot_angle"
    )

  #combine all data
  combined_data_2 <-
    full_join(combined_data_recode, olympic_data_recode)
  #make last row in Start_end == end
  combined_data_2[61493, 'Start_end'] <- "end"
}

#find first play of each game
first_play <- combined_data_2 %>%
  filter(minutes == 20 &
           period == 1 & event == "faceoff_win")


#find final play of each game and winner of each game
{
final_game <- combined_data_2 %>%
  filter(Start_end %in% "end") %>% #final play of each game
  select(date, gameID, home_goals, away_goals, home_team, away_team) %>%
  mutate(winner = as.factor(ifelse(
    #find winner of the game
    home_goals > away_goals,
    paste(home_team),
    ifelse(
      away_goals > home_goals,
      paste(away_team),
      ifelse(home_goals == away_goals, "tie", "error")
    )
  )),
  winner = as.factor(ifelse(
    date == "2018-10-19", paste(home_team), paste(winner) #game ended in tie but ST. Lawrence was the winner
  ))) %>%
  mutate(final_home_score = home_goals,
         final_away_score = away_goals) %>%
  select(date,
         gameID,
         home_team,
         away_team,
         winner,
         final_home_score,
         final_away_score)
}

#join final_game data (final play of each game and its winner) to full dataset
games <- full_join(combined_data_2, final_game, by = c("gameID", "date", "home_team", "away_team"))%>%
  mutate(one_timer_pass = ifelse(lead(one_timer) == T, T, F))%>%
  mutate(goal = event == "goal")%>%
  mutate_at(c('traffic', 'one_timer', 'one_timer_pass'), ~ replace_na(., F))

distinct(games, home_team) #missing "SWZ" in home_team, so add them to factor to use later
distinct(games, away_team)
levels(games$home_team) <- c(levels(games$home_team), "SWZ")
}

offensive_events <- games%>%
  filter(event == "goal" | event == "shot" | event == "complete_pass",
         x>=125,
         x2>=125 | is.na(x2) == T
  )%>%
  mutate(through_middle_pass = case_when( #find whether or not pass went through center line of ice
    (event == "complete_pass" & y >=42.5 & y2 <= 42.5 & x2 <189) ~ T,
    (event == "complete_pass" & y2 >=42.5 & y <= 42.5 & x2 <189) ~ T,
    T ~ F
  ))%>% 
  mutate(through_middle_shot = ifelse(lag(through_middle_pass) == T, T, F))%>%
  mutate(behind_net_pass = case_when(
      (event == "complete_pass" & x >= 189 & x2 < 189 & x2 >= 125) ~ T,
      T ~ F
    ))%>%
  mutate(behind_net_shot = ifelse(lag(behind_net_pass) == T, T, F))

house_shot_df <- offensive_events%>%
  select(x, y)
house_shot_df <- house_shot_df%>%
  mutate(house_shot = 1:dim(offensive_events)[1] %in% inpip(house_shot_df, the_house, bound = TRUE))%>%
  select(house_shot) #find whether x, y coordinates are in the house
house_pass_df <- offensive_events%>%
  select(x2,y2)%>%
  mutate_all(~replace_na(., 0))
colnames(house_pass_df) <- c('x','y')
house_pass_df <- house_pass_df%>%
  mutate(house_pass = 1:dim(offensive_events)[1] %in% inpip(house_pass_df, the_house))%>%
  select(house_pass) #find whether x2, y2 coordinates are in the house

shots_passes <- cbind(offensive_events, house_shot_df, house_pass_df)

shots_by_house <- shots_passes%>%
  group_by(house_shot)%>%
  mutate(shot_pct = (sum(event == "goal") / sum(sum(event == "shot"), sum(event == "goal")))*100) 

house_events <- shots_passes%>%
  filter(house_shot == T | house_pass == T,
         detail_1 != "Fan",
         detail_1 != "Wrap Around")%>%
  group_by(behind_net_shot, one_timer, through_middle_shot)%>%
  mutate(shot_pct = (sum(event == "goal") / sum(sum(event == "shot"), sum(event == "goal")))*100)%>%
  ungroup()%>%
  mutate(avg_shot_pct = (sum(event == "goal") / sum(sum(event == "shot"), sum(event == "goal")))*100)

house_glm <- glm(goal ~ behind_net_shot + one_timer + through_middle_shot + goal_dist + shot_angle + detail_1 + traffic, data = house_events, family = 'binomial')
summary(house_glm)

table(house_events$shot_pct)%>%prop.table

non_house_events <- shots_passes%>%
  filter(house_shot == F & house_pass == F,
         detail_1 != "Fan",
         detail_1 != "Wrap Around")%>%
  group_by(behind_net_shot, one_timer, through_middle_shot)%>%
  mutate(shot_pct = (sum(event == "goal") / sum(sum(event == "shot"), sum(event == "goal")))*100)%>%
   ungroup()%>%
  mutate(avg_shot_pct = (sum(event == "goal") / sum(sum(event == "shot"), sum(event == "goal")))*100)

non_house_glm <- glm(goal ~ behind_net_shot  + one_timer + through_middle_shot +  goal_dist + shot_angle + detail_1 + traffic, data = non_house_events, family = 'binomial')
summary(non_house_glm)

table(non_house_events$shot_pct)%>%prop.table

#one_timer house_pass behind_net
{
  T_T_T <- shots_passes%>%
        filter((one_timer == T | one_timer_pass == T), (house_pass == T | house_shot == T), (behind_net_pass == T | behind_net_shot == T))

  T_T_F <- shots_passes%>%
    filter((one_timer == T | one_timer_pass == T), (house_pass == T | house_shot == T), (behind_net_pass == F & behind_net_shot == F))

  T_F_T <- shots_passes%>%
        filter((one_timer == T | one_timer_pass == T), (house_pass == F & house_shot == F), (behind_net_pass == T | behind_net_shot == T))

  T_F_F <- shots_passes%>%
        filter((one_timer == T | one_timer_pass == T), (house_pass == F & house_shot == F), (behind_net_pass == F & behind_net_shot == F))

  F_T_T <- shots_passes%>%
      filter((one_timer == F & one_timer_pass == F), (house_pass == T | house_shot == T), (behind_net_pass == T | behind_net_shot == T))
  
  F_T_F <- shots_passes%>%
      filter((one_timer == F & one_timer_pass == F), (house_pass == T | house_shot == T), (behind_net_pass == F & behind_net_shot == F))
 
  F_F_T <- shots_passes%>%
     filter((one_timer == F & one_timer_pass == F), (house_pass == F & house_shot == F), (behind_net_pass == T | behind_net_shot == T))
 
  F_F_F <- shots_passes%>%
      filter((one_timer == F & one_timer_pass == F), (house_pass == F & house_shot == F), (behind_net_pass == F & behind_net_shot == F))
}

  #function to find shot_pct of each possible combination
  combo_fn <- function(x){
    y<- x%>%
    filter(event == "shot" | event == "goal")%>%
    summarize(shot_pct = (sum(event == "goal") / sum(sum(event == "shot"), sum(event == "goal")))) 
    return(y)
  }
  
  as.numeric(combo_fn(T_T_T))
  as.numeric(combo_fn(T_T_F)) #one timer house
  as.numeric(combo_fn(T_F_T))
  as.numeric(combo_fn(T_F_F)) #one timer no house
  as.numeric(combo_fn(F_T_T))
  as.numeric(combo_fn(F_T_F)) #non one timer house
  as.numeric(combo_fn(F_F_T))
  as.numeric(combo_fn(F_F_F)) #non one timer no house
 
  house_table <- as.data.frame(matrix(c(
  as.numeric(combo_fn(T_T_T)),
  as.numeric(combo_fn(T_T_F)),
  as.numeric(combo_fn(F_T_T)),
  as.numeric(combo_fn(F_T_F)))*100,
  nrow = 2, ncol = 2, byrow = F,
  dimnames = list(c("one_timer", "non_one_timer"),
                  c("behind_net", "not_behind_net"))))
  

  non_house_table <- as.data.frame(matrix(c(
  as.numeric(combo_fn(T_F_T)),
  as.numeric(combo_fn(T_F_F)),
  as.numeric(combo_fn(F_F_T)),
  as.numeric(combo_fn(F_F_F)))*100,
  nrow = 2, ncol = 2, byrow = F,
  dimnames = list(c("one_timer", "non_one_timer"),
                  c("behind_net", "not_behind_net"))))

#behind_net == T, house == T, through_middle = T&F
gg_behind_house_plays <- plot_half_rink(ggplot()) +
   geom_segment(data = shots_passes %>% 
                 filter(event == "complete_pass" & house_pass == T & behind_net_pass == T),
               aes(x = x, xend = x2, y = y, yend = y2, color = one_timer_pass),
               alpha = 0.3,
               linewidth = 0.6,
               lineend = "round",
               linejoin = "bevel",
               arrow = arrow(length = unit(0.15, 'cm'))
  ) +
  geom_point(data=shots_passes %>% 
               filter((event == "goal" | event == "shot") & house_shot == T & behind_net_shot == T), 
             aes(x = x, y = y, color = event),
             size = 0.7) + 
  scale_color_manual(name = "Type of Event", 
                      values = c(goal = "#FF0061", 
                                 shot = "green", 
                                 `TRUE` = 'orange', 
                                 `FALSE`  = "blue"),
                      labels = c("Non-One-Timer Pass", "Goal", "Shot", "One-Timer Pass")) +
  ggtitle("All Passes From Behind the Net to 'The House'")

  rink_overlay(gg_behind_house_plays)
  
  #behind_net == T, house == F, through_middle = T&F
  gg_behind_no_house_plays <- plot_half_rink(ggplot()) +
   geom_segment(data = shots_passes %>% 
                 filter(event == "complete_pass" & house_pass == F & behind_net_pass == T),
               aes(x = x, xend = x2, y = y, yend = y2, color = one_timer_pass),
               alpha = 0.3,
               linewidth = 0.6,
               lineend = "round",
               linejoin = "bevel",
               arrow = arrow(length = unit(0.15, 'cm'))
  ) +
  geom_point(data=shots_passes %>% 
               filter((event == "goal" | event == "shot") & house_shot == F & behind_net_shot == T), 
             aes(x = x, y = y, color = event),
             size = 0.7) + 
  scale_color_manual(name = "Type of Event", 
                      values = c(goal = "#FF0061", 
                                 shot = "green", 
                                 `TRUE` = 'orange', 
                                 `FALSE`  = "blue"),
                      labels = c("Non-One-Timer Pass", "Goal", "Shot", "One-Timer Pass")) +
  ggtitle("All Passes From Behind the Net to outside 'The House'")
  
    rink_overlay(gg_behind_no_house_plays)

   #behind_net == T, house == T, through_middle = T
  gg_behind_house_middle_plays <- plot_half_rink(ggplot()) +
   geom_segment(data = shots_passes %>% 
                 filter(event == "complete_pass" & house_pass == T & behind_net_pass == T & through_middle_pass == T),
               aes(x = x, xend = x2, y = y, yend = y2, color = one_timer_pass),
               alpha = 0.3,
               linewidth = 0.6,
               lineend = "round",
               linejoin = "bevel",
               arrow = arrow(length = unit(0.15, 'cm'))
  ) +
  geom_point(data=shots_passes %>% 
               filter((event == "goal" | event == "shot") & house_shot == T & behind_net_shot == T & through_middle_shot == T), 
             aes(x = x, y = y, color = event),
             size = 0.7) + 
  scale_color_manual(name = "Type of Event", 
                      values = c(goal = "#FF0061", 
                                 shot = "green", 
                                 `TRUE` = 'orange', 
                                 `FALSE`  = "blue"),
                      labels = c("Non-One-Timer Pass", "Goal", "Shot", "One-Timer Pass")) +
  ggtitle("All Passes From Behind the Net to 'The House'\nwith all passes through midline")
  
    rink_overlay(gg_behind_house_middle_plays)

  #behind_net == F, house == T, through_middle = T
  gg_no_behind_house_middle_plays <- plot_half_rink(ggplot()) +
   geom_segment(data = shots_passes %>% 
                 filter(event == "complete_pass" & house_pass == T & behind_net_pass == F & through_middle_pass == T),
               aes(x = x, xend = x2, y = y, yend = y2, color = one_timer_pass),
               alpha = 0.3,
               linewidth = 0.6,
               lineend = "round",
               linejoin = "bevel",
               arrow = arrow(length = unit(0.15, 'cm'))
  ) +
  geom_point(data=shots_passes %>% 
               filter((event == "goal" | event == "shot") & house_shot == T & behind_net_shot == F & through_middle_shot == T), 
             aes(x = x, y = y, color = event),
             size = 0.7) + 
  scale_color_manual(name = "Type of Event", 
                      values = c(goal = "#FF0061", 
                                 shot = "green", 
                                 `TRUE` = 'orange', 
                                 `FALSE`  = "blue"),
                      labels = c("Non-One-Timer Pass", "Goal", "Shot", "One-Timer Pass")) +
  ggtitle("All Passes From not Behind the Net to 'The House'\nwith all passes through midline")
  
    rink_overlay(gg_no_behind_house_middle_plays)

      #behind_net == F, house == F, through_middle = T
  gg_no_behind_no_house_middle_plays <- plot_half_rink(ggplot()) +
   geom_segment(data = shots_passes %>% 
                 filter(event == "complete_pass" & house_pass == F & behind_net_pass == F & through_middle_pass == T),
               aes(x = x, xend = x2, y = y, yend = y2, color = one_timer_pass),
               alpha = 0.15,
               linewidth = 0.6,
               lineend = "round",
               linejoin = "bevel",
               arrow = arrow(length = unit(0.15, 'cm'))
  ) +
  geom_point(data=shots_passes %>% 
               filter((event == "goal" | event == "shot") & house_shot == F & behind_net_shot == F & through_middle_shot == T), 
             aes(x = x, y = y, color = event),
             size = 0.7) + 
  scale_color_manual(name = "Type of Event", 
                      values = c(goal = "#FF0061", 
                                 shot = "green", 
                                 `TRUE` = 'orange', 
                                 `FALSE`  = "blue"),
                      labels = c("Non-One-Timer Pass", "Goal", "Shot", "One-Timer Pass")) +
  ggtitle("All Passes From not Behind the Net\nto outside 'The House'\nwith all passes through midline")
  
    rink_overlay(gg_no_behind_no_house_middle_plays)
    
  #behind_net == F, house == F, through_middle = F
  gg_no_behind_no_house_no_middle_plays <- plot_half_rink(ggplot()) +
   geom_segment(data = shots_passes %>% 
                 filter(event == "complete_pass" & house_pass == F & behind_net_pass == F & through_middle_pass == F),
               aes(x = x, xend = x2, y = y, yend = y2, color = one_timer_pass),
               alpha = 0.1,
               linewidth = 0.6,
               lineend = "round",
               linejoin = "bevel",
               arrow = arrow(length = unit(0.15, 'cm'))
  ) +
  geom_point(data=shots_passes %>% 
               filter((event == "goal" | event == "shot") & house_shot == F & behind_net_shot == F & through_middle_shot == F), 
             aes(x = x, y = y, color = event),
             size = 0.7) + 
  scale_color_manual(name = "Type of Event", 
                      values = c(goal = "#FF0061", 
                                 shot = "green", 
                                 `TRUE` = 'orange', 
                                 `FALSE`  = "blue"),
                      labels = c("Non-One-Timer Pass", "Goal", "Shot", "One-Timer Pass")) +
  ggtitle("All Passes From not Behind the Net\nto outside 'The House'\nwith all passes not through midline")
  
    rink_overlay(gg_no_behind_no_house_no_middle_plays)

      #behind_net == F, house == T, through_middle = F
  gg_no_behind_house_no_middle_plays <- plot_half_rink(ggplot()) +
   geom_segment(data = shots_passes %>% 
                 filter(event == "complete_pass" & house_pass == T & behind_net_pass == F & through_middle_pass == F),
               aes(x = x, xend = x2, y = y, yend = y2, color = one_timer_pass),
               alpha = 0.25,
               linewidth = 0.5,
               lineend = "round",
               linejoin = "bevel",
               arrow = arrow(length = unit(0.15, 'cm'))
  ) +
  geom_point(data=shots_passes %>% 
               filter((event == "goal" | event == "shot") & house_shot == T & behind_net_shot == F & through_middle_shot == F), 
             aes(x = x, y = y, color = event),
             size = 0.3, alpha = 0.8) + 
  scale_color_manual(name = "Type of Event", 
                      values = c(goal = "#FF0061", 
                                 shot = "green", 
                                 `TRUE` = 'orange', 
                                 `FALSE`  = "blue"),
                      labels = c("Non-One-Timer Pass", "Goal", "Shot", "One-Timer Pass")) +
  ggtitle("All Passes From not Behind the Net to 'The House'\nwith all passes not through midline")
  
    rink_overlay(gg_no_behind_house_no_middle_plays)
