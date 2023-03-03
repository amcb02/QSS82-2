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
  library(units)
  library(gridExtra)
  library(scales)
  library(ggpubr)
  library(RColorBrewer)
  library(splancs)
  library(forestplot)
  library(rms)
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
  # Set scipen option to turn off scientific notation for small numbers
  options(scipen = 999)
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
      dplyr::select("date",
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
      dplyr::select(
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
      dplyr::select(date, gameID, home_goals, away_goals, home_team, away_team) %>%
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
      dplyr::select(date,
             gameID,
             home_team,
             away_team,
             winner,
             final_home_score,
             final_away_score)
    }
  
  #join final_game data (final play of each game and its winner) to full dataset AND REDUCE TO OFFENSIVE EVENTS ONLY
  games <-
    full_join(combined_data_2,
              final_game,
              by = c("gameID", "date", "home_team", "away_team")) %>%
    mutate(one_timer_pass = as.logical(ifelse((lead(event) == "shot" |
                                                 lead(event) == "goal") & lead(one_timer) == T, T, F
    ))) %>%
    mutate(goal = event == "goal") %>%
    mutate_at(c('traffic', 'one_timer', 'one_timer_pass'),
              ~ replace_na(., F))
  
  distinct(games, home_team) #missing "SWZ" in home_team, so add them to factor to use later
  distinct(games, away_team)
  levels(games$home_team) <- c(levels(games$home_team), "SWZ")


offensive_events <- games %>%
  filter(event == "goal" |
           event == "shot" | event == "complete_pass") %>%
  filter(x >= 125) %>%
  filter(x2 >= 125 |
           is.na(x2) == T) %>% #x2 only !NA for passes, x2 == NA for goals and shots
  mutate(through_middle_pass = as.logical(case_when(
    #find whether or not pass went through center line of ice
    (event == "complete_pass" &
       y >= 42.5 & y2 <= 42.5 & x2 < 189) ~ T,
    (event == "complete_pass" &
       y2 >= 42.5 & y <= 42.5 & x2 < 189) ~ T,
    T ~ F
  ))) %>%
  mutate(through_middle_shot = as.logical(ifelse((event == "shot" |
                                                    event == "goal") &
                                                   lag(event) == "complete_pass" &
                                                   lag(through_middle_pass) == T,
                                                 T,
                                                 F
  ))) %>%
  mutate(behind_net_pass = as.logical(case_when(
    (event == "complete_pass" & x >= 189 & x2 < 189 & x2 >= 125) ~ T,
    T ~ F
  ))) %>%
  mutate(behind_net_shot = as.logical(ifelse((event == "shot" |
                                                event == "goal") &
                                               lag(event) == "complete_pass" &
                                               lag(behind_net_pass) == T,
                                             T,
                                             F
  ))) %>%
  mutate(period_seconds = minutes * 60 + seconds) %>%
  mutate(shot_after_pass = case_when(one_timer == T ~ F,
                                     one_timer == F ~ as.logical(
                                       ifelse((event == "shot" |
                                                 event == "goal") &
                                                lag(event) == "complete_pass" &
                                                lag(period_seconds) - period_seconds < 2,
                                              T,
                                              F
                                       )
                                     ))) %>%
  mutate(
    advantage = case_when(
      team == home_team ~ home_skaters - away_skaters,
      team == away_team ~ away_skaters - home_skaters
    )
  ) %>%
  mutate(home = ifelse(team == home_team, T, F))
}

## FINDING WHETHER EVENTS ARE IN OR OUT OF THE HOUSE
{
house_shot_df <- offensive_events %>%
  dplyr::select(x, y)
house_shot_df <- house_shot_df %>%
  mutate(
    house_shot = 1:dim(offensive_events)[1] %in% inpip(house_shot_df, the_house, bound = TRUE)
  ) %>%
  dplyr::select(house_shot) #find whether x, y coordinates are in the house
house_pass_df <- offensive_events %>%
  dplyr::select(x2, y2) %>%
  mutate_all( ~ replace_na(., 0))
colnames(house_pass_df) <- c('x', 'y')
house_pass_df <- house_pass_df %>%
  mutate(house_pass = 1:dim(offensive_events)[1] %in% inpip(house_pass_df, the_house)) %>%
  dplyr::select(house_pass) #find whether x2, y2 coordinates are in the house

shots_passes <-
  cbind(offensive_events, house_shot_df, house_pass_df)

#only shots and goals
shots <- shots_passes%>%
  filter(event == "goal" | event == "shot")

#overall shot percentage of all shots 
full_shot_pct <- shots%>%
  ungroup()%>%
  dplyr::summarize(shot_pct = (sum(event == "goal") / sum(sum(event == "shot"), sum(event == "goal"))))%>%
  as.numeric()
}
#GLM for all shots anywhere in offensive zone
{
full_glm_1 <-
  glm(
    goal ~ one_timer,
    data = shots,
    family = 'binomial'
  )
summary(full_glm_1)
write.csv(as.data.frame(summary(full_glm_1)$coefficients), "full_glm_1.csv")
full_glm_2 <-
  glm(
    goal ~ one_timer + behind_net_shot + through_middle_shot,
    data = shots,
    family = 'binomial'
  )
summary(full_glm_2)
full_glm_3 <-
  glm(
    goal ~ one_timer + behind_net_shot + through_middle_shot + shot_after_pass,
    data = shots,
    family = 'binomial'
  )
summary(full_glm_3)
full_glm_4 <-
  glm(
    goal ~ one_timer + behind_net_shot + through_middle_shot + shot_after_pass + goal_dist,
    data = shots,
    family = 'binomial'
  )
summary(full_glm_4)
full_glm_5 <-
  glm(
    goal ~ one_timer + behind_net_shot + through_middle_shot + shot_after_pass + goal_dist + shot_angle,
    data = shots,
    family = 'binomial'
  )
summary(full_glm_5)
house_glm <-
  glm(
    goal ~ one_timer + behind_net_shot + through_middle_shot + shot_after_pass + goal_dist + shot_angle + period_seconds +  traffic + advantage,
    data = shots,
    family = 'binomial'
  )
summary(full_glm)
full_glm_results <-
  round(summary.glm(full_glm)$coefficients, digits = 4)
full_glm_results

shots$prob <-
  predict(full_glm, newdata = shots, type = "response")
shots[, c(
  "one_timer_decomp",
  "behind_net_shot_decomp",
  "through_middle_shot_decomp",
  "shot_after_pass_decomp",
  "goal_dist_decomp",
  "shot_angle_decomp",
  "period_seconds_decomp",
  "traffic_decomp",
  "advantage_decomp"
)] <- predict(full_glm, newdata = shots, type = "terms")
}

#HOUSE ANALYSIS
{
  #actual shot percentage of all house shots
  house_events_shot_pct <- house_events%>%
  filter(event == "shot" | event == "goal")%>%
  ungroup()%>%
  dplyr::summarize(shot_pct = (sum(event == "goal") / sum(sum(event == "shot"), sum(event == "goal"))))%>%
  as.numeric()

house_events <- shots_passes %>%
  filter(house_shot == T | house_pass == T,
         detail_1 != "Fan",
         detail_1 != "Wrap Around") %>%
  group_by(one_timer,
           shot_after_pass,
           through_middle_shot,
           behind_net_shot) %>%
  mutate(shot_pct = round(sum(event == "goal") / sum(sum(event == "shot"), sum(event == "goal")) *
                            100, digits = 2)) %>%
  ungroup() %>%
  mutate(avg_shot_pct = (sum(event == "goal") / sum(sum(event == "shot"), sum(event == "goal"))) *
           100)

house_events_summary <- shots_passes %>%
  filter(house_shot == T | house_pass == T) %>%
  group_by(one_timer,
           shot_after_pass,
           through_middle_shot,
           behind_net_shot) %>%
  dplyr::summarize(shot_pct = round(sum(event == "goal") / sum(sum(event == "shot"), sum(event == "goal")) *
                               100, digits = 2))
house_glm_null <- 
  glm(goal ~ 0,
    data = house_events,
    family = 'binomial'
  )
house_glm_1 <-
  glm(
    goal ~ one_timer,
    data = house_events,
    family = 'binomial'
  )
summary(house_glm_1)
write.csv(as.data.frame(summary(house_glm_1)$coefficients), "house_glm_1.csv")
house_glm_2 <-
  glm(
    goal ~ one_timer + behind_net_shot + through_middle_shot,
    data = house_events,
    family = 'binomial'
  )
summary(house_glm_2)
house_glm_3 <-
  glm(
    goal ~ one_timer + behind_net_shot + through_middle_shot + shot_after_pass,
    data = house_events,
    family = 'binomial'
  )
summary(house_glm_3)
house_glm_4 <-
  glm(
    goal ~ one_timer + behind_net_shot + through_middle_shot + shot_after_pass + goal_dist,
    data = house_events,
    family = 'binomial'
  )
summary(house_glm_4)
house_glm_5 <-
  glm(
    goal ~ one_timer + behind_net_shot + through_middle_shot + shot_after_pass + goal_dist + shot_angle,
    data = house_events,
    family = 'binomial'
  )
summary(house_glm_5)
house_glm <-
  glm(
    goal ~ one_timer + behind_net_shot + through_middle_shot + shot_after_pass + goal_dist + shot_angle + period_seconds +  traffic + advantage,
    data = house_events,
    family = 'binomial'
  )
summary(house_glm)
house_glm_results <-
  round(summary.glm(house_glm)$coefficients, digits = 4)
write.csv(house_glm_results, "house_glm_results.csv")
write2word(house_glm_results, "house_glm_results.doc")

rms::vif(house_glm)


# Create a data frame with odds ratios and confidence intervals
house_glm_odds <- broom::tidy(house_glm, exponentiate = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(lci = estimate - 1.96 * std.error,
         uci = estimate + 1.96 * std.error)

forestplot(
  mean = house_glm_odds$estimate,
  lower = house_glm_odds$lci,
  upper = house_glm_odds$uci,
  labeltext = house_glm_odds$term,
  clip = c(0.01, Inf),
  xlab = "Odds Ratio",
  zero = 1,
  boxsize = 0.15,
  cex = 1,
  col = fpColors(box = "royalblue", line = "darkblue"),
  title = "Odds Ratios of All Variables\nfor Shots from 'The House'"
)

# Predict using the model
house_events$prob <-
  predict(house_glm, newdata = house_events, type = "response")
house_events[, c(
  "behind_net_shot_decomp",
  "one_timer_decomp",
  "through_middle_shot_decomp",
  "shot_after_pass_decomp",
  "goal_dist_decomp",
  "shot_angle_decomp",
  "period_seconds_decomp",
  "traffic_decomp",
  "advantage_decomp"
)] <- predict(house_glm, newdata = house_events, type = "terms")

#total goals scored by each player in the house
player_house_goals <- house_events %>%
  filter(event == "goal") %>%
  ungroup() %>%
  group_by(player) %>%
  dplyr::summarize(player_goals = sum(event == "goal")) %>%
  arrange(-player_goals)

#predicted goals scored by each player in the house
player_pred_house_goals <- house_events %>%
  filter(event == "shot" | event == "goal") %>%
  ungroup() %>%
  group_by(player) %>%
  dplyr::summarize(player_prob = sum(prob)) %>%
  arrange(-player_prob)

#difference in actual and predicted goals scored in the house. Positive = more goals than expected, Negative = less goals than expected
delta_player_house_goals <-
  right_join(player_house_goals, player_pred_house_goals, by = "player") %>%
  mutate_at('player_goals', ~ replace_na(., 0)) %>%
  mutate(delta = player_goals - player_prob) %>%
  arrange(-delta)

#total goals scored by each team in the house
team_house_goals <- house_events %>%
  filter(event == "goal") %>%
  ungroup() %>%
  group_by(team) %>%
  dplyr::summarize(team_goals = sum(event == "goal")) %>%
  arrange(-team_goals)

#predicted goals scored by each team in the house
team_pred_house_goals <- house_events %>%
  filter(event == "shot" | event == "goal") %>%
  ungroup() %>%
  group_by(team) %>%
  dplyr::summarize(team_prob = sum(prob)) %>%
  arrange(-team_prob)

#difference in actual and predicted goals scored in the house. Positive = more goals than expected, Negative = less goals than expected
delta_team_house_goals <-
  right_join(team_house_goals, team_pred_house_goals, by = "team") %>%
  mutate_at('team_goals', ~ replace_na(., 0)) %>%
  mutate(delta = team_goals - team_prob) %>%
  arrange(-delta)

# Find mean probability of a goal being scored based on one_timer
house_events_mean_prob <- house_events %>%
  ungroup() %>%
  group_by(one_timer) %>%
  dplyr::summarize(mean_prob = mean(prob))

# Graph mean probability of a goal being scored based on one_timer
ggplot(house_events_mean_prob, aes(x = one_timer, y = mean_prob)) +
  geom_bar(stat = "identity") +
  ylab("Mean Probability of Scoring") +
  xlab("One-Timer")

# Find mean probability of a goal being scored based on one_timer by team
team_house_events_mean_prob <- house_events %>%
  ungroup() %>%
  group_by(one_timer, team) %>%
  dplyr::summarize(mean_prob = mean(prob))

# Graph mean probability of a goal being scored based on one_timer by team
ggplot(
  team_house_events_mean_prob %>% filter(one_timer == T),
  aes(x = team, y = mean_prob, fill = mean_prob)
) +
  geom_bar(stat = "identity") +
  geom_text(
    aes(label = percent(mean_prob, accuracy = 0.01), hjust = 2.2),
    size = 4,
    angle = 90,
    color = "white"
  ) +
  scale_color_continuous(type = 'viridis') +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 0.1),
    breaks = seq(0, 0.20, 0.025)
  ) +
  ylab("Mean Probability") +
  xlab("Team") +
  ggtitle("Mean Probability of Scoring a\nOne-Timer in 'The House' by Team") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, size = 10),
    legend.position = 'none',
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5)
  )


# create a scatter plot of predicted probabilities vs. goal_dist
ggplot(house_events, aes(x = goal_dist, y = prob)) +
  geom_point() +
  stat_smooth(
    method = "glm",
    method.args = (family = "binomial"),
    se = T
  ) +
  labs(x = "Goal Distance", y = "Predicted Probability of Goal")
}
  
#NON-HOUSE EVENT ANALYSIS
{
# MEAN SHOT PCT FOR ALL NON-HOUSE SHOTS
  non_house_events_shot_pct <- non_house_events%>%
  filter(event == "shot" | event == "goal")%>%
  ungroup()%>%
  dplyr::summarize(shot_pct = (sum(event == "goal") / sum(sum(event == "shot"), sum(event == "goal"))))%>%
  as.numeric()
  
non_house_events <- shots_passes %>%
  filter(house_shot == F & house_pass == F,
         detail_1 != "Fan",
         detail_1 != "Wrap Around") %>%
  group_by(one_timer,
           shot_after_pass,
           through_middle_shot,
           behind_net_shot) %>%  mutate(shot_pct = round(sum(event == "goal") / sum(sum(event == "shot"), sum(event == "goal")) *
                                                           100, digits = 2)) %>%
  ungroup() %>%
  mutate(avg_shot_pct = (sum(event == "goal") / sum(sum(event == "shot"), sum(event == "goal"))) *
           100)

non_house_events_summary <- shots_passes %>%
  filter(house_shot == F & house_pass == F) %>%
  group_by(one_timer,
           shot_after_pass,
           through_middle_shot,
           behind_net_shot) %>%
  dplyr::summarize(shot_pct = round(sum(event == "goal") / sum(sum(event == "shot"), sum(event == "goal")) *
                               100, digits = 2))

non_house_glm_null <- 
  glm(goal ~ 0,
    data = non_house_events,
    family = 'binomial'
  )
summary(non_house_glm_null)
non_house_glm_1 <-
  glm(
    goal ~ one_timer,
    data = non_house_events,
    family = 'binomial'
  )
summary(non_house_glm_1)
non_house_glm_2 <-
  glm(
    goal ~ one_timer + behind_net_shot + through_middle_shot,
    data = non_house_events,
    family = 'binomial'
  )
summary(non_house_glm_2)
non_house_glm_3 <-
  glm(
    goal ~ one_timer + behind_net_shot + through_middle_shot + shot_after_pass,
    data = non_house_events,
    family = 'binomial'
  )
summary(non_house_glm_3)
non_house_glm_4 <-
  glm(
    goal ~ one_timer + behind_net_shot + through_middle_shot + shot_after_pass + goal_dist,
    data = non_house_events,
    family = 'binomial'
  )
summary(non_house_glm_4)
non_house_glm_5 <-
  glm(
    goal ~ one_timer + behind_net_shot + through_middle_shot + shot_after_pass + goal_dist + shot_angle,
    data = non_house_events,
    family = 'binomial'
  )
summary(non_house_glm_5)
non_house_glm <-
  glm(
    goal ~ one_timer + behind_net_shot  + through_middle_shot + shot_after_pass +  goal_dist + shot_angle + period_seconds + traffic + advantage,
    data = non_house_events,
    family = 'binomial'
  )
summary(non_house_glm)
non_house_glm_results <-
  round(summary.glm(non_house_glm)$coefficients, digits = 4)
write.csv(non_house_glm_results, "non_house_glm_results.csv")
write2word(non_house_glm_results, "non_house_glm_results.doc")

# Create a data frame with odds ratios and confidence intervals
non_house_glm_odds <-
  broom::tidy(non_house_glm, exponentiate = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(lci = estimate - 1.96 * std.error,
         uci = estimate + 1.96 * std.error)

forestplot(
  mean = non_house_glm_odds$estimate,
  lower = non_house_glm_odds$lci,
  upper = non_house_glm_odds$uci,
  labeltext = non_house_glm_odds$term,
  clip = c(0.01, Inf),
  xlab = "Odds Ratio",
  zero = 1,
  boxsize = 0.15,
  cex = 1,
  col = fpColors(box = "royalblue", line = "darkblue"),
  title = "Odds Ratios of All Variables\nfor Shots not from 'The House'"
)

# Predict using the model
non_house_events$prob <-
  predict(non_house_glm, newdata = non_house_events, type = "response")
non_house_events[, c(
  "behind_net_shot_decomp",
  "one_timer_decomp",
  "through_middle_shot_decomp",
  "shot_after_pass_decomp",
  "goal_dist_decomp",
  "shot_angle_decomp",
  "period_seconds_decomp",
  "traffic_decomp",
  "advantage_decomp"
)] <-
  predict(non_house_glm, newdata = non_house_events, type = "terms")

plogis(predict(non_house_glm, newdata = non_house_events, type = "link"))
#total goals scored by each player in the house
player_non_house_goals <- non_house_events %>%
  filter(event == "goal") %>%
  ungroup() %>%
  group_by(player) %>%
  dplyr::summarize(player_goals = sum(event == "goal")) %>%
  arrange(-player_goals)

#predicted goals scored by each player in the house
player_pred_non_house_goals <- non_house_events %>%
  filter(event == "shot" | event == "goal") %>%
  ungroup() %>%
  group_by(player) %>%
  dplyr::summarize(player_prob = sum(prob)) %>%
  arrange(-player_prob)

#difference in actual and predicted goals scored in the house. Positive = more goals than expected, Negative = less goals than expected
delta_player_non_house_goals <-
  right_join(player_non_house_goals, player_pred_non_house_goals, by = "player") %>%
  mutate_at('player_goals', ~ replace_na(., 0)) %>%
  mutate(delta = player_goals - player_prob) %>%
  arrange(-delta)

# Find mean probability of a goal being scored based on one_timer
non_house_events_mean_prob <- non_house_events %>%
  ungroup() %>%
  group_by(one_timer) %>%
  dplyr::summarize(mean_prob = mean(prob))

# Graph mean probability of a goal being scored based on one_timer
ggplot(non_house_events_mean_prob, aes(x = one_timer, y = mean_prob)) +
  geom_bar(stat = "identity") +
  ylab("Mean Probability of Scoring") +
  xlab("One-Timer")

# Find mean probability of a goal being scored based on one_timer by team
team_non_house_events_mean_prob <- non_house_events %>%
  ungroup() %>%
  group_by(one_timer, team) %>%
  dplyr::summarize(mean_prob = mean(prob))

# Graph mean probability of a goal being scored based on one_timer by team
ggplot(
  team_non_house_events_mean_prob %>% filter(one_timer == T),
  aes(x = team, y = mean_prob, fill = mean_prob)
) +
  geom_bar(stat = "identity") +
  geom_text(
    aes(label = percent(mean_prob, accuracy = 0.01), hjust = 1.7),
    size = 4,
    angle = 90,
    color = "white"
  ) +
  scale_color_continuous(type = 'viridis') +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1),
                     breaks = seq(0, 0.05, 0.01)) +
  ylab("Mean Probability") +
  xlab("Team") +
  ggtitle("Mean Probability of Scoring a\nOne-Timer not in 'The House' by Team") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, size = 10),
    legend.position = 'none',
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5)
  )

# create a scatter plot of predicted probabilities vs. goal_dist
ggplot(non_house_events, aes(x = goal_dist, y = prob)) +
  geom_point() +
  stat_smooth(
    method = "glm",
    method.args = (family = "binomial"),
    se = T
  ) +
  ylim(0, 0.4) +
  labs(x = "Goal Distance", y = "Predicted Probability of Goal")
}

#find difference in house vs. no house shooting pct
house_delta <-
  cbind(
    house_events_summary[, 1:4],
    house_shot_pct = house_events_summary$shot_pct,
    non_house_shot_pct = non_house_events_summary$shot_pct
  ) %>%
  mutate(delta = house_shot_pct - non_house_shot_pct) %>%
  dplyr::select(-house_shot_pct, -non_house_shot_pct)

#PASS SEGMENT AND SHOT/GOAL POINT PLOTS
{
#behind_net == T, house == T, through_middle = T&F
gg_behind_house_plays <- plot_half_rink(ggplot()) +
  geom_segment(
    data = shots_passes %>%
      filter(event == "complete_pass" &
               house_pass == T & behind_net_pass == T),
    aes(
      x = x,
      xend = x2,
      y = y,
      yend = y2,
      color = one_timer_pass
    ),
    alpha = 0.3,
    linewidth = 0.6,
    lineend = "round",
    linejoin = "bevel",
    arrow = arrow(length = unit(0.15, 'cm'))
  ) +
  geom_point(
    data = shots_passes %>%
      filter((event == "goal" |
                event == "shot") & house_shot == T & behind_net_shot == T
      ),
    aes(x = x, y = y, color = event),
    size = 0.7
  ) +
  scale_color_manual(
    name = "Type of Event",
    values = c(
      goal = "#FF0061",
      shot = "green",
      `TRUE` = 'orange',
      `FALSE`  = "blue"
    ),
    labels = c("Non-One-Timer Pass", "Goal", "Shot", "One-Timer Pass")
  ) +
  ggtitle("All Passes From Behind the Net to 'The House'")

rink_overlay(gg_behind_house_plays)

#behind_net == T, house == F, through_middle = T&F
gg_behind_no_house_plays <- plot_half_rink(ggplot()) +
  geom_segment(
    data = shots_passes %>%
      filter(
        event == "complete_pass" &
          house_pass == F & behind_net_pass == T & through_middle_pass == T
      ),
    aes(
      x = x,
      xend = x2,
      y = y,
      yend = y2,
      color = one_timer_pass
    ),
    alpha = 0.3,
    linewidth = 0.6,
    lineend = "round",
    linejoin = "bevel",
    arrow = arrow(length = unit(0.15, 'cm'))
  ) +
  geom_point(
    data = shots_passes %>%
      filter((event == "goal" |
                event == "shot") &
               house_shot == F &
               behind_net_shot == T & through_middle_shot == T
      ),
    aes(x = x, y = y, color = event),
    size = 0.7
  ) +
  scale_color_manual(
    name = "Type of Event",
    values = c(
      goal = "#FF0061",
      shot = "green",
      `TRUE` = 'orange',
      `FALSE`  = "blue"
    ),
    labels = c("Non-One-Timer Pass", "Goal", "Shot", "One-Timer Pass")
  ) +
  ggtitle("All Passes From Behind the Net to outside 'The House'")

rink_overlay(gg_behind_no_house_plays)

#behind_net == T, house == T, through_middle = T
gg_behind_house_middle_plays <- plot_half_rink(ggplot()) +
  geom_segment(
    data = shots_passes %>%
      filter(
        event == "complete_pass" &
          house_pass == T & behind_net_pass == T & through_middle_pass == T
      ),
    aes(
      x = x,
      xend = x2,
      y = y,
      yend = y2,
      color = one_timer_pass
    ),
    alpha = 0.3,
    linewidth = 0.6,
    lineend = "round",
    linejoin = "bevel",
    arrow = arrow(length = unit(0.15, 'cm'))
  ) +
  geom_point(
    data = shots_passes %>%
      filter((event == "goal" |
                event == "shot") &
               house_shot == T &
               behind_net_shot == T & through_middle_shot == T
      ),
    aes(x = x, y = y, color = event),
    size = 0.7
  ) +
  scale_color_manual(
    name = "Type of Event",
    values = c(
      goal = "#FF0061",
      shot = "green",
      `TRUE` = 'orange',
      `FALSE`  = "blue"
    ),
    labels = c("Non-One-Timer Pass", "Goal", "Shot", "One-Timer Pass")
  ) +
  ggtitle("All Passes From Behind the Net to 'The House'\nwith all passes through midline")

rink_overlay(gg_behind_house_middle_plays)

#behind_net == F, house == T, through_middle = T
gg_no_behind_house_middle_plays <- plot_half_rink(ggplot()) +
  geom_segment(
    data = shots_passes %>%
      filter(
        event == "complete_pass" &
          house_pass == T & behind_net_pass == F & through_middle_pass == T
      ),
    aes(
      x = x,
      xend = x2,
      y = y,
      yend = y2,
      color = one_timer_pass
    ),
    alpha = 0.3,
    linewidth = 0.6,
    lineend = "round",
    linejoin = "bevel",
    arrow = arrow(length = unit(0.15, 'cm'))
  ) +
  geom_point(
    data = shots_passes %>%
      filter((event == "goal" |
                event == "shot") &
               house_shot == T &
               behind_net_shot == F & through_middle_shot == T
      ),
    aes(x = x, y = y, color = event),
    size = 0.7
  ) +
  scale_color_manual(
    name = "Type of Event",
    values = c(
      goal = "#FF0061",
      shot = "green",
      `TRUE` = 'orange',
      `FALSE`  = "blue"
    ),
    labels = c("Non-One-Timer Pass", "Goal", "Shot", "One-Timer Pass")
  ) +
  ggtitle("All Passes From not Behind the Net to 'The House'\nwith all passes through midline")

rink_overlay(gg_no_behind_house_middle_plays)

#behind_net == F, house == F, through_middle = T
gg_no_behind_no_house_middle_plays <- plot_half_rink(ggplot()) +
  geom_segment(
    data = shots_passes %>%
      filter(
        event == "complete_pass" &
          house_pass == F & behind_net_pass == F & through_middle_pass == T
      ),
    aes(
      x = x,
      xend = x2,
      y = y,
      yend = y2,
      color = one_timer_pass
    ),
    alpha = 0.15,
    linewidth = 0.6,
    lineend = "round",
    linejoin = "bevel",
    arrow = arrow(length = unit(0.15, 'cm'))
  ) +
  geom_point(
    data = shots_passes %>%
      filter((event == "goal" |
                event == "shot") &
               house_shot == F &
               behind_net_shot == F & through_middle_shot == T
      ),
    aes(x = x, y = y, color = event),
    size = 0.7
  ) +
  scale_color_manual(
    name = "Type of Event",
    values = c(
      goal = "#FF0061",
      shot = "green",
      `TRUE` = 'orange',
      `FALSE`  = "blue"
    ),
    labels = c("Non-One-Timer Pass", "Goal", "Shot", "One-Timer Pass")
  ) +
  ggtitle(
    "All Passes From not Behind the Net\nto outside 'The House'\nwith all passes through midline"
  ) +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5))


rink_overlay(gg_no_behind_no_house_middle_plays)

#behind_net == F, house == F, through_middle = F
gg_no_behind_no_house_no_middle_plays <-
  plot_half_rink(ggplot()) +
  geom_segment(
    data = shots_passes %>%
      filter(
        event == "complete_pass" &
          house_pass == F & behind_net_pass == F & through_middle_pass == F
        &
          (lead(event) == "goal" | lead(event) == "shot") & x2 < 189
      ),
    aes(
      x = x,
      xend = x2,
      y = y,
      yend = y2,
      color = one_timer_pass
    ),
    alpha = 0.1,
    linewidth = 0.6,
    lineend = "round",
    linejoin = "bevel",
    arrow = arrow(length = unit(0.15, 'cm'))
  ) +
  geom_point(
    data = shots_passes %>%
      filter((event == "goal" |
                event == "shot") &
               house_shot == F &
               behind_net_shot == F &
               through_middle_shot == F & lag(event) == "complete_pass" & x < 189
      ),
    aes(x = x, y = y, color = event),
    size = 0.7
  ) +
  scale_color_manual(
    name = "Type of Event",
    values = c(
      goal = "#FF0061",
      shot = "green",
      `TRUE` = 'orange',
      `FALSE`  = "blue"
    ),
    labels = c("Non-One-Timer Pass", "Goal", "Shot", "One-Timer Pass")
  ) +
  ggtitle(
    "All Passes From not Behind the Net\nto outside 'The House'\nwith all passes not through midline"
  )

rink_overlay(gg_no_behind_no_house_no_middle_plays)

#behind_net == F, house == T, through_middle = F
gg_no_behind_house_no_middle_plays <- plot_half_rink(ggplot()) +
  geom_segment(
    data = shots_passes %>%
      filter(
        event == "complete_pass" &
          house_pass == T & behind_net_pass == F & through_middle_pass == F
      ),
    aes(
      x = x,
      xend = x2,
      y = y,
      yend = y2,
      color = one_timer_pass
    ),
    alpha = 0.25,
    linewidth = 0.5,
    lineend = "round",
    linejoin = "bevel",
    arrow = arrow(length = unit(0.15, 'cm'))
  ) +
  geom_point(
    data = shots_passes %>%
      filter((event == "goal" |
                event == "shot") &
               house_shot == T &
               behind_net_shot == F & through_middle_shot == F
      ),
    aes(x = x, y = y, color = event),
    size = 0.3,
    alpha = 0.8
  ) +
  scale_color_manual(
    name = "Type of Event",
    values = c(
      goal = "#FF0061",
      shot = "green",
      `TRUE` = 'orange',
      `FALSE`  = "blue"
    ),
    labels = c("Non-One-Timer Pass", "Goal", "Shot", "One-Timer Pass")
  ) +
  ggtitle("All Passes From not Behind the Net to 'The House'\nwith all passes not through midline")

rink_overlay(gg_no_behind_house_no_middle_plays)
}

#plotting of all teams shots and goals
{
ggsave("team_plays/CAN_2d.jpeg",
       plot_team_shots_goals('CAN', 'Team Canada', 'Olympic'))
ggsave(
  "team_plays/RUS_2d.jpeg",
  plot_team_shots_no_goals('RUS', 'Team Russia', 'Olympic')
)
ggsave("team_plays/FIN_2d.jpeg",
       plot_team_shots_goals('FIN', 'Team Finland', 'Olympic'))
ggsave("team_plays/USA_2d.jpeg",
       plot_team_shots_goals('USA', 'Team USA', 'Olympic'))
ggsave(
  "team_plays/SWZ_2d.jpeg",
  plot_team_shots_goals('SWZ', 'Team Switzerland', 'Olympic')
)
ggsave(
  "team_plays/Clarkson_2d.jpeg",
  plot_team_shots_goals('Clarkson', 'the Clarkson Golden Knights', 'NCAA')
)
ggsave(
  "team_plays/St_Lawrence_2d.jpeg",
  plot_team_shots_goals('St_Lawrence', 'the St. Lawrence Saints', 'NCAA')
)
ggsave(
  "team_plays/Boston_2d.jpeg",
  plot_team_shots_goals('Boston', 'the Boston Pride', 'NWHL')
)
ggsave(
  "team_plays/Minnesota_2d.jpeg",
  plot_team_shots_goals('Minnesota', 'the Minnesota Whitecaps', 'NWHL')
)
ggsave(
  "team_plays/Buffalo_2d.jpeg",
  plot_team_shots_no_goals('Buffalo', 'the Buffalo Beauts', 'NWHL')
)
ggsave(
  "team_plays/Connecticut_2d.jpeg",
  plot_team_shots_goals('Connecticut', 'the Connecticut Whale', 'NWHL')
)
ggsave(
  "team_plays/Toronto_2d.jpeg",
  plot_team_shots_goals('Toronto', 'the Toronto Six', 'NWHL')
)
ggsave(
  "team_plays/Metropolitan_2d.jpeg",
  plot_team_shots_no_goals('Metropolitan', 'the Metropolitan Riveters', 'NWHL')
)
}
