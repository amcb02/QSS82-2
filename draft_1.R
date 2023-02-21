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
source("plot_rink.R")
source("plot_half_rink.R")
source("functions.R")
source("rink_overlay.R")
source("upper_outline.R")

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
games <-
  full_join(combined_data_2,
            final_game,
            by = c("gameID", "date", "home_team", "away_team"))
}

distinct(games, home_team) #missing "SWZ" in home_team, so add them to factor to use later
distinct(games, away_team)
levels(games$home_team) <- c(levels(games$home_team), "SWZ")

#total number of games
sum(games$Start_end == "end")

#descriptive statistics
summary(games)
table(games$event)%>% prop.table()

source("Coordinates.R")
games2 <- right_join(games, points, by = c("coord_id"))
passes_coord_id <- games%>%
  mutate(one_timer_pass = ifelse(lead(one_timer) == T, T, F))

passes_coord_id <- right_join(passes_coord_id, points, by = c("coord_id_2" = "coord_id"))%>%
  mutate_at(c("one_timer_pass"), ~ replace_na(., F))

passes_coord_id <- plyr::rename(passes_coord_id,
                                replace = c("x_group" = "receiving_x_group",  
                                            "y_group" = "receiving_y_group"))

games3 <- games2 %>%
  mutate(one_timer_pass = ifelse(lead(one_timer) == T, T, F))%>%
  group_by(coord_id) %>%
  mutate(goal = event == "goal") %>%
  mutate_at(c('traffic', 'one_timer'), ~ replace_na(., F))

  
#COUNTS OF PASSES
{
#all passes
passes <- games3 %>%
  filter(event == "complete_pass" | event == "incomplete_pass")
count(passes)

#one_timer passes
one_timer_pass_count <- games3%>%
  ungroup()%>%
  filter(one_timer_pass == T)
  count(one_timer_pass_count)

#all indirect passes
ind_pass <- passes %>%
  filter(detail_1 == "Indirect")
count(ind_pass)

#all direct passes
dir_pass <- passes %>%
  filter(detail_1 == "Direct")
count(dir_pass)

#all complete passes
good_pass <- passes %>%
  filter(event == "complete_pass")
count(good_pass)

#all incomplete passes
bad_pass <- passes %>%
  filter(event == "incomplete_pass")
count(bad_pass)
  }

coord_shot_pct <- games3 %>%
  filter(event == "shot" | event == "goal") %>%
  group_by(coord_id) %>%
  summarize(shot_pct = (sum(goal == T) / sum(sum(event == "shot"), sum(goal == T))))


#Mean Shot Pct by shot type
mean_shot_type_pct <- games3 %>%
  filter(event == "shot" | event == "goal") %>%
  group_by(detail_1) %>%
  summarize(shot_pct = (sum(goal == T) / sum(sum(event == "shot"), sum(goal == T))))

##Shot percentage by coordinate and shot type
shot_type_pct <- games3 %>%
  filter(event == "shot" | event == "goal") %>%
  group_by(coord_id, detail_1) %>%
  summarize(shot_pct = (sum(goal == T) / sum(sum(event == "shot"), sum(goal == T))))%>%
  pivot_wider(values_from = shot_pct, names_from = detail_1)%>%
  mutate_all(~ replace_na(., 0))

#all goals and shots from offensive zone (remove 2 goals scored from neutral zone)
offensive_events <- games3 %>%
  filter(event == "shot" | event == "goal",
         x >= 125) %>%
  group_by(coord_id) %>%
  mutate(shot_pct = (sum(goal == T) / sum(sum(event == "shot"), sum(goal == T)))) 

#average shot percentage of all shots total in offensive zone (exclude shots from neutral and defensive zone)
median_shot_pct <- offensive_events %>%
  filter(one_timer == F)%>%
  group_by(coord_id, x_group, y_group) %>%
  summarize(id_shot_pct = mean(shot_pct)) 

#all one-timers
one_timers <- games3 %>%
  filter(one_timer == T) %>%
  mutate(shot_pct = (sum(goal == T) / sum(sum(event == "shot"), sum(goal == T))))%>%
  mutate(fill = as.factor(shot_pct))

#one_timer shot percentage by coordinate id
one_timer_shot_pct <- one_timers %>%
  group_by(coord_id, x_group, y_group) %>%
  summarize(id_shot_pct = mean(shot_pct))

#merge all shot percentage and one_timer shot percentage
{
delta_shot_pct <-
  full_join(median_shot_pct,
            one_timer_shot_pct,
            by = c('coord_id', 'x_group', 'y_group'))%>%
  mutate_all(~ replace_na(., 0)) %>%
  mutate(delta = id_shot_pct.y - id_shot_pct.x)
colnames(delta_shot_pct) <-
  c(
    "coord_id",
    "x_group",
    "y_group",
    "all_shot_pct",
    "one_timer_shot_pct",
    "delta_one_timer_all_shots"
  )
  }

#density plot of difference in one_timer pct vs regular shot pct
delta_shot_pct %>%
  ungroup() %>%
  ggplot(aes(delta_one_timer_all_shots)) +
  geom_density() +
  geom_vline(xintercept = 0, linetype = "longdash", color = "red")+
  scale_x_continuous(limits = c(-.2, .2), labels = percent)+
  theme_minimal()+
  labs(title = "Density of the Difference in Shot Percentage\n of One Timers and Non-One Timers by Coordinate",
       x = "Difference",
       y = "# of Coordinates")+
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5))

pass_value <- games3 %>%
  filter(event == "shot" |
           event == "goal" | event == "complete_pass") %>%
  group_by(coord_id) %>%
  mutate(shot_pct = (sum(goal == T) / sum(sum(event == "shot"), sum(goal == T)))) %>% #shot pct by coord id
  ungroup() %>%
  mutate(prev_coord_id = lag(coord_id)) #coord id of previous event


# add shot pct from previous coord id
{
pass_value2 <-
  right_join(
    pass_value,
    delta_shot_pct %>% select(coord_id, all_shot_pct, x_group, y_group),
    by = c('prev_coord_id' = 'coord_id')
  ) %>%
  group_by(event) %>%
  mutate(mean_pass_dist = ifelse(
    event == "complete_pass" |
      event == "incomplete_pass",
    mean(dist),
    0
  )) %>%
  mutate(
    norm_pass_dist = ifelse(
      event == "complete_pass" |
        event == "incomplete_pass",
      dist / mean_pass_dist,
      0
    )
  ) %>%
  ungroup() %>%
  mutate(one_timer_pass = ifelse(lead(one_timer) == T, T, F))%>%
  mutate(through_middle = case_when( 
    y >=42.5 & y2 <= 42.5 ~ T,
    y2 >=42.5 & y <= 42.5 ~ T,
    T ~ F
  )) #find whether or not pass went through center line of ice
  the_house <- data.frame(x = c(189, 189, 169, 154, 154, 169), xend = c(189, 169, 154, 154, 169, 189), y = c(39.5, 45.5, 64.5, 64.5, 20.5, 20.5), yend = c(45.5, 64.5, 64.5, 20.5, 20.5, 39.5)) #create df of the house polygon
house_shot_df <- pass_value2%>%
  select(x,y)
house_shot_df <- house_shot_df%>%
  mutate(house_shot = 1:dim(pass_value2)[1] %in% inpip(house_shot_df, the_house, bound = TRUE))%>%
  select(house_shot) #find whether x, y coordinates are in the house
house_pass_df <- pass_value2%>%
  select(x2,y2)%>%
  mutate_all(~replace_na(., 0))
colnames(house_pass_df) <- c('x','y')
house_pass_df <- house_pass_df%>%
  mutate(house_pass = 1:dim(pass_value2)[1] %in% inpip(house_pass_df, the_house, bound= TRUE))%>%
  select(house_pass) #find whether x2, y2 coordinates are in the house

shots_passes <- cbind(pass_value2, house_shot_df, house_pass_df)

passes_to_house <- shots_passes%>%
  filter(house_pass == T)

#rename columns
colnames(pass_value2)[52:54] <-
  c("prev_all_shot_pct", "prev_x_group", "prev_y_group")
}

#find difference between shot pct at current location and shot pct at previous location
pass_value2$delta_shot_pct_prev_shot_pct <-
  pass_value2$shot_pct - pass_value2$prev_all_shot_pct

#grouped_diff = (shot_pct for given coord_id of one_timer) - (shot_pct for given coord_id of non-one_timer)
x<- pass_value2%>%
  filter(event == "goal" | event == "shot")%>%
  group_by(one_timer, coord_id)%>%
  summarize(grouped_shot_pct = (sum(goal == T) / sum(sum(event == "shot"), sum(goal == T))))%>%
  pivot_wider(values_from = grouped_shot_pct, names_from = one_timer)%>%
  mutate_all(~ replace_na(., 0)) %>%
  mutate(grouped_diff = `TRUE` - `FALSE`)%>%
  select(coord_id, grouped_diff)

#attempt at finding value of given one-timer pass
pass_value3 <- right_join(pass_value2, x, by=c('coord_id'))%>%
  filter(event == "complete_pass",
         one_timer_pass == TRUE)%>%
  mutate(value = case_when( #KEY VARIABLE
                      delta_shot_pct_prev_shot_pct + grouped_diff >= 0 ~ 
                      norm_pass_dist * (delta_shot_pct_prev_shot_pct + grouped_diff),
                      delta_shot_pct_prev_shot_pct + grouped_diff < 0 ~ 
                      norm_pass_dist * (delta_shot_pct_prev_shot_pct + grouped_diff),
                                          ))

#pass value by player
player_pass_value <- pass_value3%>%
  ungroup()%>%
  group_by(player)%>%
  summarize(player_value = as.numeric(mean(value)))

#find primary assists for each player
assists <- pass_value2%>%
  filter(lead(event) == "goal")%>%
  mutate(assist = T)%>%
  group_by(player)%>%
  summarize(primary_assists = sum(assist))

#compare primary assists and overall passing value for each player
comp_table <- left_join(player_pass_value, assists, by="player")%>%
  mutate_all(~ replace_na(., 0))

#find all passes that originate behind the goalline
behind_net_passes <- passes_coord_id%>%
  filter(event == "complete_pass",
         x>=189,
         x2>=125,
         x2<189,
         )%>%
  mutate(avg_pass_area_x = mean(x2),
         avg_pass_area_y = mean(y2))

#find total passes completed in each coord_id 
y<- behind_net_passes%>%
  group_by(coord_id_2)%>%
  count()

behind_net_passes_2 <- left_join(behind_net_passes, y, by="coord_id_2")
behind_net_passes_2%>%
  filter(one_timer_pass == T)%>%
  count()
gg_behind <- plot_half_rink(ggplot(behind_net_passes_2))+
  geom_tile(aes(
    x = receiving_x_group,
    y = receiving_y_group,
    width = 10,
    height = 6.07,
    fill = n
  ))+  #tile heatmap
  ggtitle("All Passes From Behind the Net") +
   scale_fill_gradientn(colors = alpha(c(brewer.pal(5, "Blues")))) + 
  scale_alpha(range = c(0.2, 1))+
  geom_segment(
    aes(
      x = x,
      xend = x2,
      y = y,
      yend = y2,
      color = one_timer_pass
    ),
    alpha =0.3,
    size = 0.7,
    lineend = "round",
    linejoin = "bevel",
    arrow = arrow(length = unit(0.2, 'cm'))
  ) +
  labs(fill = "Count of total passes to given location",
       color = "One-Timer Pass") +
  scale_color_manual(values = c("lightgreen", "#FF0061"))+
  geom_point(
    aes(x = x2, y = y2),
    size = 0.1,
    color = "black",
  ) 
  rink_overlay(gg_behind)

 #GLM for difference in shot pct from given area and shot pct from previous area of pass
pass_value_glm <-
  glm(
     delta_shot_pct_prev_shot_pct ~  grouped_diff + norm_pass_dist,
    data = pass_value3
  )
summary(pass_value_glm)

#all one-timer goals
goal_one_timers <- one_timers %>%
  filter(event == "goal")

#all one-timer passes
one_timer_passes <- games3 %>%
  mutate(one_timer_pass = lead(one_timer) == T) %>%
  filter(one_timer_pass == T)

#all one-timer goal passes
goals_one_timer_passes <- games %>%
  mutate(one_timer_pass = lead(one_timer) == T) %>%
  filter(lead(event) == "goal" & one_timer_pass == T)

#non one timers
non_one_timers <- games3 %>%
  filter(one_timer == F)

#non one-timer goals
goal_non_one_timers <- non_one_timers %>%
  filter(goal == T)

#non one_timer goal passes
goal_non_one_timers_passes <- games3 %>%
  filter(lead(event) == "goal",
         lead(one_timer) == F,
         event == "complete_pass")

#all passes that created goals on the power play
{
pp_events_passes <- games3 %>%
  ungroup() %>%
  mutate(goal_pass = lead(event) == "goal") %>%
  filter(goal_pass == T & event == "complete_pass") %>%
  filter(home_skaters != away_skaters) %>%
  filter(
    case_when(
      team == home_team ~ home_skaters > away_skaters,
      team == away_team ~ away_skaters > home_skaters
    )
  ) %>%
  mutate(advantage = factor(abs(home_skaters - away_skaters), ordered = TRUE))
  }

#all PP goals
{
pp_events <- games3 %>%
  ungroup() %>%
  mutate(event == "goal" | event == "shot") %>%
  filter(home_skaters != away_skaters) %>%
  filter(
    case_when(
      team == home_team ~ home_skaters > away_skaters,
      team == away_team ~ away_skaters > home_skaters
    )
  ) %>%
  mutate(advantage = factor(abs(home_skaters - away_skaters), ordered = TRUE)) %>%
  group_by(coord_id) %>%
  mutate(shot_pct = (sum(goal == T) / sum(sum(event == "shot"), sum(goal == T))))
}

## PLOTTING ##
#Plot all passes in offensive zone, color by one_timer_pass
{
gg_passes <- plot_half_rink(ggplot(
  passes%>%
  filter(event == "complete_pass",
         !is.na(one_timer_pass),
         ))) +
  geom_segment(
    aes(
      x = x,
      xend = x2,
      y = y,
      yend = y2,
      color = one_timer_pass
    ),
    size = 0.1,
    lineend = "round",
    linejoin = "bevel",
    arrow = arrow(length = unit(0.05, 'cm'))
  ) +
  scale_color_manual(values = c("lightgreen", "#FF0061")) 
  }

#plot all offensive zone goals and shots
{
gg_offensive_events <- plot_half_rink(ggplot(offensive_events)) +
  geom_tile(aes(
    x = x_group,
    y = y_group,
    width = 10,
    height = 6.07,
    fill = shot_pct,
    alpha = shot_pct
  )) + #tile heatmap
  scale_alpha(range = c(0.01, 1))+
  labs(fill = "Shot Percentage") +
  ggtitle("All Goals and Shots") +
  scale_fill_gradientn(limits = c(-0.01, .35),
                       colors = c("blue", "green"),
                       labels = percent)+
  geom_point(
    data = offensive_events %>% filter(event == "shot"),
    aes(x = x, y = y),
    size = 0.001,
    color = "lightblue",
  ) + #all shots
  geom_point(
    data = offensive_events %>% filter(event == "goal"),
    aes(x = x, y = y),
    size = 0.3,
    color = "green"
  )  #all goals
  OG_plot <- rink_overlay(gg_offensive_events)+
  theme(
    axis.title = element_blank(),
    axis.ticks  = element_blank(),
    axis.text = element_blank(),
    plot.title = element_text(hjust = 0.5),
    panel.grid = element_blank()
  )
  OG_plot
}

#plot all one timer goals and shots
{
gg_one_timers <- plot_half_rink(ggplot(one_timers)) +
  geom_tile(aes(
    x = x_group,
    y = y_group,
    width = 10,
    height = 6.07,
    fill = shot_pct,
    alpha = shot_pct
  )) + #tile heatmap
  labs(fill = "Shot Percentage") +
  ggtitle("One Timer Goals and Shots") +
  scale_fill_gradientn(limits = c(-0.01, .35),
                       colors = c("blue", "green"),
                       labels = percent) +
  scale_alpha(range = c(0.1, 1))+#color for tile heatmap, scale to same as offensive goals plot heatmap
  geom_point(
    data = one_timers %>% filter(event == "shot"),
    aes(x = x, y = y),
    size = 0.001,
    color = "lightblue"
  ) +
  geom_point(
    data = one_timers %>% filter(event == "goal"),
    aes(x = x, y = y),
    size = 0.3,
    color = "green"
  ) 
  
  OT_plot<- rink_overlay(gg_one_timers)+
    theme(
    axis.title = element_blank(),
    axis.ticks  = element_blank(),
    axis.text = element_blank(),
    plot.title = element_text(hjust = 0.5),
    panel.grid= element_blank()
  )
  OT_plot
}

#arrange all goals and shot plot (OG_plot) and one-timer goals and shots (OT_plot)
shot_plot <- ggarrange(OG_plot, OT_plot, ncol=2, common.legend = TRUE, legend = "bottom")
annotate_figure(shot_plot, top = text_grob("All Shots & Goals vs. One-Timer Shots & Goals",  color = "black", face = "bold", size = 14), bottom = text_grob("Shots: Light Yellow\n Goals: Green", color = "black", size = 10))

#2d Density plot of one timer shots and goals
{
gg_one_timers_density <- plot_half_rink(ggplot(one_timers)) +
#geom_density2d(aes(x=x, y=y))+
    stat_density2d(aes(x=x, y=y, fill = ..level.., alpha = ..level..), geom = "polygon")+
  scale_fill_continuous(type = "viridis") +
  scale_alpha(range = c(0.2, 1))+#color for tile heatmap, scale to same as offensive goals plot heatmap
    xlim(121,200)+
    ylim(-3,88)+
    ggtitle("One Timer Goals and Shots") +
  geom_point(
    data = one_timers %>% filter(event == "shot"),
    aes(x = x, y = y),
    size = 0.001,
    color = "lightblue"
  ) +
  geom_point(
    data = one_timers %>% filter(event == "goal"),
    aes(x = x, y = y),
    size = 0.3,
    color = "red"
  ) 
  OT_density <- rink_overlay(gg_one_timers_density)+
  theme(
    axis.title = element_blank(),
    axis.ticks  = element_blank(),
    axis.text = element_blank(),
    plot.title = element_text(hjust = 0.5),
    panel.grid= element_blank(),
    legend.position = 'none'
  )
  OT_density
}

#2D density plot of non-one-timer shots and goals
{
  gg_offensive_events_density <- plot_half_rink(ggplot(offensive_events)) +
    #geom_density2d(aes(x=x, y=y))+
    stat_density2d(aes(x=x, y=y, fill = ..level.., alpha = ..level..), geom = "polygon")+
    scale_fill_continuous(type = "viridis") +
    scale_alpha(range = c(0.2, 1))+#color for tile heatmap, scale to same as offensive goals plot heatmap
    xlim(121,200)+
    ylim(-3,88)+
    ggtitle("Non-One Timer Goals and Shots") +
    geom_point(
      data = one_timers %>% filter(event == "shot"),
      aes(x = x, y = y),
      size = 0.001,
      color = "lightblue"
    ) +
    geom_point(
      data = one_timers %>% filter(event == "goal"),
      aes(x = x, y = y),
      size = 0.3,
      color = "red"
    ) 
  OG_density <- rink_overlay(gg_offensive_events_density)+
    theme(
      axis.title = element_blank(),
      axis.ticks  = element_blank(),
      axis.text = element_blank(),
      plot.title = element_text(hjust = 0.5),
      panel.grid= element_blank(),
      legend.position = 'none'
    )
  OG_density
}

#arrange all goals and shot plot (OG_density) and one-timer goals and shots (OT_density)
shot_density <- ggarrange(OG_density, OT_density, ncol=2)
annotate_figure(shot_density, top = text_grob("2D Density plots of Non-One Timer Shots & Goals vs. One-Timer Shots & Goals",  color = "black", face = "bold", size = 14), bottom = text_grob("Shots: Light Blue\n Goals: Red", color = "black", size = 10))

#Plot the Difference in one-timer and regular shot pct by coordinate
{
gg_delta_shot_pct<- plot_half_rink(ggplot(delta_shot_pct)) +
  geom_tile(aes(
    x = x_group,
    y = y_group,
    width = 10,
    height = 6.07,
    fill = delta_one_timer_all_shots
  )) + #tile heatmap
  labs(fill = "Difference") +
  ggtitle("Difference in one-timer and regular shot pct by coordinate") +
  scale_fill_gradientn(
                       colors = c("red", "white", "green"), limits = c(-.2,.2), labels = percent) 
  rink_overlay(gg_delta_shot_pct)
}

#difference in shot pct at shot location and shot pct at pass origin location
{
gg_difference <- plot_half_rink(ggplot(pass_value2%>%filter(one_timer==T))) +
  geom_tile(
    aes(
      x = x_group.x,
      y = y_group.x,
      width = 10,
      height = 6.07,
      fill = delta_shot_pct_prev_shot_pct
    )
  ) +
  labs(fill = "Shot Percentage\nDifference (%)") +
  ggtitle("Difference in Shot % of One-Timer\nfrom Shot area and Shot % from Pass Area") +
  scale_fill_gradientn(limits = c(-.2, .2),
                       colors = c("lightblue", "white", "red"),
                       labels = percent)  
  rink_overlay(gg_difference)
}

#plot passes through middle of ice
{
  gg_through_middle <- plot_half_rink(ggplot(pass_value2%>%filter(through_middle==T, x2<189)%>%  mutate_at(c('through_middle','one_timer_pass'), ~ replace_na(., F))%>%select(x,y,x2,y2,one_timer_pass,through_middle))) +
     geom_segment(
    aes(
      x = x,
      xend = x2,
      y = y,
      yend = y2,
      color = one_timer_pass
    ),
    size = 0.15,
    lineend = "round",
    linejoin = "bevel",
    arrow = arrow(length = unit(0.2, 'cm'))
  ) +
  scale_color_manual(values = c("lightgreen", "#FF0061"))+
  labs(color = "One-Timer Pass") +
  ggtitle("Passes through the middle line of the ice")+
  annotate("segment", x = 125, xend = 200, y = 42.5, yend = 42.5, linetype = "dashed", color='yellow', size= 1.5) #Middle Line

    rink_overlay(gg_through_middle)
}



