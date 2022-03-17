library(tidyverse)
library(jsonlite)
library(webshot)

flags <- c("https://upload.wikimedia.org/wikipedia/en/thumb/a/a4/Flag_of_the_United_States.svg/1920px-Flag_of_the_United_States.svg.png",
          "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d9/Flag_of_Canada_%28Pantone%29.svg/1920px-Flag_of_Canada_%28Pantone%29.svg.png",
          "https://upload.wikimedia.org/wikipedia/en/thumb/4/4c/Flag_of_Sweden.svg/1920px-Flag_of_Sweden.svg.png",
          "https://upload.wikimedia.org/wikipedia/en/thumb/a/ae/Flag_of_the_United_Kingdom.svg/1280px-Flag_of_the_United_Kingdom.svg.png",
          "https://upload.wikimedia.org/wikipedia/commons/thumb/f/fa/Flag_of_the_People%27s_Republic_of_China.svg/800px-Flag_of_the_People%27s_Republic_of_China.svg.png",
          "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d9/Flag_of_Norway.svg/800px-Flag_of_Norway.svg.png",
          "https://upload.wikimedia.org/wikipedia/commons/thumb/f/f2/Civil_Ensign_of_Switzerland.svg/800px-Civil_Ensign_of_Switzerland.svg.png",
          "https://upload.wikimedia.org/wikipedia/commons/4/48/ROC_flag_%282021_NWSCh%29.png",
          "https://upload.wikimedia.org/wikipedia/commons/thumb/9/9c/Flag_of_Denmark.svg/800px-Flag_of_Denmark.svg.png",
          "https://upload.wikimedia.org/wikipedia/en/thumb/0/03/Flag_of_Italy.svg/1024px-Flag_of_Italy.svg.png",
          "https://upload.wikimedia.org/wikipedia/en/thumb/9/9e/Flag_of_Japan.svg/800px-Flag_of_Japan.svg.png",
          "https://upload.wikimedia.org/wikipedia/commons/thumb/0/09/Flag_of_South_Korea.svg/800px-Flag_of_South_Korea.svg.png",
          "https://upload.wikimedia.org/wikipedia/commons/thumb/8/88/Flag_of_Australia_%28converted%29.svg/800px-Flag_of_Australia_%28converted%29.svg.png",
          "https://upload.wikimedia.org/wikipedia/commons/thumb/c/cb/Flag_of_the_Czech_Republic.svg/800px-Flag_of_the_Czech_Republic.svg.png")
teams <- c("USA","CAN","SWE","GBR","CHN","NOR","SUI","ROC","DEN","ITA","JPN","KOR","AUS","CZE")
country_flags <- data.frame(team_abbr = teams,flags) # this contains flags for all 14 qualified nations across the 3 tournaments

event_ID <- list() # insert desired tournament event ids here!!!

standings <- data.frame() # running this will zero out compile_test; OK to do if starting over

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
  team1_wins <- raw_json$MatchInfo$Competitor1$n_MatchesWon
  team1_losses <- raw_json$MatchInfo$Competitor1$n_MatchesLost
  team2_wins <- raw_json$MatchInfo$Competitor2$n_MatchesWon
  team2_losses <- raw_json$MatchInfo$Competitor2$n_MatchesLost
  team1_points <- raw_json$MatchInfo$Competitor1$n_Result
  team2_points <- raw_json$MatchInfo$Competitor2$n_Result
  event_id = i

  # team 1
  team1_stats <- data.frame()
  
  team1_stats <- team1_stats%>%
    summarise(team_id = Team1_id,
              team = Team1,
              team_abbr = Team1_short,
              wins = team1_wins,
              losses = team1_losses,
              points_for = team1_points,
              points_against = team2_points)

  # team 2
  team2_stats <- data.frame()
  
  team2_stats <- team2_stats%>%
    summarise(team_id = Team2_id,
              team = Team2,
              team_abbr = Team2_short,
              wins = team2_wins,
              losses = team2_losses,
              points_for = team2_points,
              points_against = team1_points)
  
  # bind team1_stats and team2_stats
  team_stats_summary <- rbind(team1_stats,team2_stats)

  # leave in df for compiling during loop
  agg_summaries <- rbind(agg_summaries,team_stats_summary)
  
}
  
# columns to group by for standings aggregate
group_cols <- c("team_id","team","team_abbr")

# group by group_cols for aggregated standings, joining flag logos
final_standings <- agg_summaries%>%
  group_by(across(all_of(group_cols)))%>%
  summarise(wins = max(wins),
            losses = max(losses),
            pf = sum(points_for),
            pa = sum(points_against))%>%
  left_join(country_flags)
  
write.csv(final_standings,"standings.csv")
