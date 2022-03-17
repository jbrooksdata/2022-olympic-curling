library(tidyverse)
library(jsonlite)
library(webshot)

event_ID <- list() # insert desired tournament event ids here!!!

compile_stats <- data.frame() # re-running this will zero out compile_stats; OK to do if starting over

for(i in event_ID){

url_json <- paste0("https://api-gracenote.nbcolympics.com/svc/games_v2.svc/json/GetMatchResult_Extended?sportId=212&matchId=",i,"&languageCode=2")
raw_json <- jsonlite::fromJSON(url_json)

# assign team names, abbrs, ids, records, and points
Team1 <- raw_json$MatchInfo$Competitor1$c_Name
Team2 <- raw_json$MatchInfo$Competitor2$c_Name
Team1_short <- raw_json$MatchInfo$Competitor1$c_Short
Team2_short <- raw_json$MatchInfo$Competitor2$c_Short
Team1_id <- raw_json$MatchInfo$Competitor1$n_TeamID
Team2_id <- raw_json$MatchInfo$Competitor2$n_TeamID
team1_score <- raw_json$MatchInfo$Competitor1$c_Result
team2_score <- raw_json$MatchInfo$Competitor2$c_Result
team1_time <- raw_json$MatchInfo$Competitor1$c_ResultInfo_1
team2_time <- raw_json$MatchInfo$Competitor2$c_ResultInfo_1
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
team1_stats <- do.call("bind_rows", team1_stats) # run this line to find other c_values - could apply shot percentages?
team1_stats <- team1_stats %>%
  summarise(c_Value1,c_Value2,c_Value3,c_Value4,c_Value5,c_ValueRelative1,c_ValueRelative2,c_ValueRelative3,c_ValueRelative4,c_ValueRelative5)

team1_stats <- cbind(team1_players,team1_stats)

team1_stats <- team1_stats %>% # select necessary stat categories
  summarise(player_id = n_PersonID,
            First = c_PersonFirstName,
            Last = c_PersonLastName,
            Full = c_Person,
            Position = c_FunctionShort,
            team = Team1_short,
            opp = Team2_short,
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
            stage = stage,
            team_score = team1_score,
            opp_score = team2_score,
            game_id = game_id,
            team_time_remaining = team1_time)

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
            First = c_PersonFirstName,
            Last = c_PersonLastName,
            Full = c_Person,
            Position = c_FunctionShort,
            team = Team2_short,
            opp = Team1_short,
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
            stage = stage,
            team_score = team2_score,
            opp_score = team1_score,
            game_id = game_id,
            team_time_remaining = team2_time)

# columns to group by for aggregate team stats
group_cols <- c("game_id","team","opp","stage","team_score","opp_score","team_time_remaining") # couldnt figure out group_by for agg_summary otherwise

# convert stats to numeric if necessary
game_stats_summary <- rbind(team1_stats,team2_stats)%>%
  mutate(across(throws:counter_clockwise_accuracy,as.numeric))

# filter out players with 0 throws for game averages
game_stats_summary <- game_stats_summary%>%
  filter(game_stats_summary$throws != "0")

# aggregate summary per game by team based on group_cols
agg_summary <- game_stats_summary%>%
  group_by(across(all_of(group_cols)))%>%
  summarise(throws = sum(throws),
            draws = sum(draws),
            takeouts = sum(takeouts),
            clockwise_throws = sum(clockwise_throws),
            counter_clockwise_throws = sum(counter_clockwise_throws),
            throw_accuracy = mean(throw_accuracy, na.rm = TRUE),
            draw_accuracy = mean(draw_accuracy, na.rm = TRUE),
            takeout_accuracy = round(mean(takeout_accuracy, na.rm = TRUE),2),
            clockwise_accuracy = round(mean(clockwise_accuracy, na.rm = TRUE),2),
            counter_clockwise_accuracy = mean(counter_clockwise_accuracy, na.rm = TRUE))#%>%

# leave in df for compiling during loop
compile_stats <- rbind(compile_stats,agg_summary)

}

# give each team a game number; event_ids should be in order for this (they are from the source)
add_game_num <- compile_stats%>%
  group_by(team)%>%
  mutate(team_game_number = row_number())

# arrange columns in desired order; move game number toward front
# this could be handled with manual rearranging in agg_summary
rearrange <- add_game_num[,c(1,2,3,18,7,5,6,4,8,13,9,14,10,15,11,16,12,17)]%>%
  rename(team_time_remaining = `time_remaining`)

write.csv(rearrange,"gamelog_teams.csv")
