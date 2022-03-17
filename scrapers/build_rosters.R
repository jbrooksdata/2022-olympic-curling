library(tidyverse)
library(jsonlite)
library(webshot)

event_ID <- list() # insert desired tournament event ids here!!!

compile_rosters <- data.frame() # re-running this will zero out compile_rosters; OK to do if starting over

for(i in event_ID){
  
url_json <- paste0("https://api-gracenote.nbcolympics.com/svc/games_v2.svc/json/GetMatchResult_Extended?sportId=212&matchId=",i,"&languageCode=2")
raw_json <- jsonlite::fromJSON(url_json)
  
# assign team names, abbrs, and ids
Team1 <- raw_json$MatchInfo$Competitor1$c_Name
Team2 <- raw_json$MatchInfo$Competitor2$c_Name
Team1_short <- raw_json$MatchInfo$Competitor1$c_Short
Team2_short <- raw_json$MatchInfo$Competitor2$c_Short
Team1_id <- raw_json$MatchInfo$Competitor1$n_TeamID
Team2_id <- raw_json$MatchInfo$Competitor2$n_TeamID

# team 1
team1_players <- raw_json$PersonStatistics$Competitor1PersonList
  
team1_info <- team1_players %>%
  summarise(player_id = n_PersonID,
            first = c_PersonFirstName,
            last = c_PersonLastName,
            full = c_Person,
            position = c_FunctionShort,
            team_abr = Team1_short,
            team_id = Team1_id)

# team 2
team2_players <- raw_json$PersonStatistics$Competitor2PersonList
  
team2_info <- team2_players %>%
  summarise(player_id = n_PersonID,
            first = c_PersonFirstName,
            last = c_PersonLastName,
            full = c_Person,
            position = c_FunctionShort,
            team_abr = Team2_short,
            team_id = Team2_id)

# columns for roster group_by
group_cols <- c("player_id","first","last","full","team_abr","team_id") # couldnt figure out group_by for agg_summary otherwise

# binds team 1 and team 2 info for compiling
player_summary <- rbind(team1_info,team2_info)

# this data frame compiles each player_summary before it is erased in the loop
compile_rosters <- rbind(compile_rosters,player_summary)
  
# groups compule_test by group_cols, lists each position played during tournament and removes duplicates
rosters <- compile_rosters%>%
  group_by(across(all_of(group_cols)))%>%
  filter(duplicated(position))%>%
  summarise(positions = paste0(unique(position), collapse = ','))

}

write.csv(rosters,"player_info.csv")
