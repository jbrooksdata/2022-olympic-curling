library(tidyverse)
library(jsonlite)
library(webshot)

event_ID <- list() # insert desired tournament event ids here!!!

compile_stats <- data.frame() # re-running this will zero out compile_stats; OK to do if starting over

for(i in event_ID){


url_json <- paste0("https://api-gracenote.nbcolympics.com/svc/games_v2.svc/json/GetMatchResult_Extended?sportId=212&matchId=",i,"&languageCode=2")
raw_json <- jsonlite::fromJSON(url_json)

# assign team names, tournament stages, and game ids
Team1 <- raw_json$MatchInfo$Competitor1$c_Short
Team2 <- raw_json$MatchInfo$Competitor2$c_Short
stage <- raw_json$MatchInfo$c_Phase
game_id <- i

# reveal stat categories
stat_type <- raw_json$PersonStatistics$StatisticList$c_ODF_Code
stat_type <- lapply(stat_type, unlist)
stat_type <- lapply(stat_type, FUN = function(x){data.frame(t(x),stringsAsFactors = F)})

# team 1
team1_players <- raw_json$PersonStatistics$Competitor1PersonList
team1_stats <- team1_players$StatisticsList
team1_stats <- lapply(team1_stats, unlist)
team1_stats <- lapply(team1_stats, FUN = function(x){data.frame(t(x),stringsAsFactors = F)})
team1_stats <- do.call("bind_rows", team1_stats)
team1_stats <- team1_stats %>%
  summarise(c_Value1,c_Value2,c_Value3,c_Value4,c_Value5,c_ValueRelative1,c_ValueRelative2,c_ValueRelative3,c_ValueRelative4,c_ValueRelative5)

team1_stats <- cbind(team1_players,team1_stats)

team1_stats <- team1_stats %>% # select necessary stat categories
  summarise(player_id = n_PersonID,
            full = c_Person,
            position = c_FunctionShort,
            team = Team1,
            opp = Team2,
            stage = stage,
            throws = c_Value1,
            throw_accuracy = c_ValueRelative1,
            draws = c_Value4,
            draw_accuracy = c_ValueRelative4,
            takeouts = c_Value5,
            takeout_accuracy = c_ValueRelative5,
            clockwise_throws = c_Value2,
            clockwise_accuracy = c_ValueRelative2,
            counter_clockwise_throws = c_Value3,
            counter_clockwise_accuracy = c_ValueRelative3,
            game_id = game_id)

# team 2
team2_players <- raw_json$PersonStatistics$Competitor2PersonList
team2_stats <- team2_players$StatisticsList
team2_stats <- lapply(team2_stats, unlist)
team2_stats <- lapply(team2_stats, FUN = function(x){data.frame(t(x),stringsAsFactors = F)})
team2_stats <- do.call("bind_rows", team2_stats)
team2_stats <- team2_stats %>%
  summarise(c_Value1,c_Value2,c_Value3,c_Value4,c_Value5,c_ValueRelative1,c_ValueRelative2,c_ValueRelative3,c_ValueRelative4,c_ValueRelative5)

team2_stats <- cbind(team2_players,team2_stats)

team2_stats <- team2_stats %>% # select necessary stat categories
  summarise(player_id = n_PersonID,
            full = c_Person,
            position = c_FunctionShort,
            team = Team2,
            opp = Team1,
            stage = stage,
            throws = c_Value1,
            throw_accuracy = c_ValueRelative1,
            draws = c_Value4,
            draw_accuracy = c_ValueRelative4,
            takeouts = c_Value5,
            takeout_accuracy = c_ValueRelative5,
            clockwise_throws = c_Value2,
            clockwise_accuracy = c_ValueRelative2,
            counter_clockwise_throws = c_Value3,
            counter_clockwise_accuracy = c_ValueRelative3,
            game_id = game_id)

# convert stats to numeric if necessary
player_stats_summary <- rbind(team1_stats,team2_stats)%>%
  mutate(across(throws:counter_clockwise_accuracy,as.numeric))

# leave in df for compiling during loop
compile_stats <- rbind(compile_stats,player_stats_summary)

}

# give each player a game number; event_ids should be in order for this (they are from the source)
  group_by(full)%>%
  mutate(team_game_number = row_number())

# move game number toward front
rearrange <- compile_stats[,c(1,2,3,4,5,18,17,6,7,8,9,10,11,12,13,14,15,16)]

write.csv(rearrange,"gamelog_players.csv")
