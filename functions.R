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