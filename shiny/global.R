library(dplyr)
library(ggplot2)
library(plotly)
library(shiny)
library(shinyWidgets)

team_abbv <- c('Arizona Cardinals'='ARI', 'Atlanta Falcons'='ATL', 'Baltimore Ravens'='BAL', 'Buffalo Bills'='BUF', 'Carolina Panthers'='CAR', 'Chicago Bears'='CHI', 'Cincinnati Bengals'='CIN', 'Cleveland Browns'='CLE', 'Dallas Cowboys'='DAL', 'Denver Broncos'='DEN', 'Detroit Lions'='DET', 'Green Bay Packers'='GB', 'Houston Texans'='HOU', 'Indianapolis Colts'='IND', 'Jacksonville Jaguars'='JAC', 'Kansas City Chiefs'='KC', 'Los Angeles Chargers'='LAC', 'Los Angeles Rams'='LAR', 'Miami Dolphins'='MIA', 'Minnesota Vikings'='MIN', 'New England Patriots'='NE', 'New Orleans Saints'='NO', 'New York Giants'='NYG', 'New York Jets'='NYJ', 'Oakland Raiders'='OAK', 'Philadelphia Eagles'='PHI', 'Pittsburgh Steelers'='PIT', 'San Francisco 49ers'='SF', 'Seattle Seahawks'='SEA', 'Tampa Bay Buccaneers'='TB', 'Tennessee Titans'='TEN', 'Washington Redskins'='WAS')
team_divisions <- c('Arizona Cardinals'='NFC West', 'Atlanta Falcons'='NFC South', 'Baltimore Ravens'='AFC North', 'Buffalo Bills'='AFC East', 'Carolina Panthers'='NFC South', 'Chicago Bears'='NFC North', 'Cincinnati Bengals'='AFC North', 'Cleveland Browns'='AFC North', 'Dallas Cowboys'='NFC East', 'Denver Broncos'='AFC West', 'Detroit Lions'='NFC North', 'Green Bay Packers'='NFC North', 'Houston Texans'='AFC South', 'Indianapolis Colts'='AFC South', 'Jacksonville Jaguars'='AFC South', 'Kansas City Chiefs'='AFC West', 'Los Angeles Chargers'='AFC West', 'Los Angeles Rams'='NFC West', 'Miami Dolphins'='AFC East', 'Minnesota Vikings'='NFC North', 'New England Patriots'='AFC East', 'New Orleans Saints'='NFC South', 'New York Giants'='NFC East', 'New York Jets'='AFC East', 'Oakland Raiders'='AFC West', 'Philadelphia Eagles'='NFC East', 'Pittsburgh Steelers'='AFC North', 'San Francisco 49ers'='NFC West', 'Seattle Seahawks'='NFC West', 'Tampa Bay Buccaneers'='NFC South', 'Tennessee Titans'='AFC South', 'Washington Redskins'='NFC East')

html_text <- '<small><br>This application showcases the sentiment of every NFL fanbase for each week in the 2018 season.<br/><br>After every week concludes, a <a href="https://github.com/saisenberg/nfl-week1">Python script</a> scrapes the posts and comments of all 32 team subreddits - beginning five minutes before each game\'s respective end time and ending twenty-four hours after the game\'s start - and utilizes the <i><a href="https://www.nltk.org/">nltk</a></i> library to compute a "positivity" and "negativity" score for each comment. Finally, the program aggregates results on a team-by-team level.<hr>The <b>Weekly Sentiment</b> tab illustrates these results across a number of dimensions. Every data point represents a particular team\'s fanbase after a given week.<br><br>A data point\'s location along the x- and y-axes indicate that fanbase\'s mean positivity and negativity score, respectively; the most positive teams appear in the bottom-right corner of the graph, and the most negative appear to the upper-left. Point colors represent a team\'s outcome of a given game, and sizes represent a team\'s margin of victory or defeat. The sidebar contains options with which to filter by week, team, game result, and game margin.<br><br>The <b>Team Averages</b> tab aggregates the year\'s fanbase results on a team-by-team level, with data point colors representing team winning percentages.<br><br>Finally, the <b>Data Table</b> tab lists the raw sentiment data in tabular form, along with a field for "Net Positivity" (i.e., Positivity less Negativity).<hr>For more details about the project, visit <b><a href="https://saisenberg.com/projects/nfl-week1.html">saisenberg.com</a></b>.</small>'

# Read and clean data----
res <- read.csv('weekly_results.csv')
res <- res %>% rename(Team.Full = Team, Score = Game.Result)
res$Team <- unname(team_abbv[res$Team.Full])
res$Division <- unname(team_divisions[res$Team.Full])
res$Result <- substr(res$Score, 1, 1)
res$Result <- factor(res$Result, levels = c('W', 'T', 'L'))
res$Score <- gsub(pattern = '\\(', replacement = '', x = res$Score)
res$Score <- gsub(pattern = '\\)', replacement = '', x = res$Score)
res$Score <- substr(res$Score, 3, nchar(res$Score))
res$Opponent <- trimws(unlist(lapply(strsplit(res$Score, ','), '[[', 2)))
res$Game.Score <- unlist(lapply(strsplit(res$Score, ','), '[[', 1))
res$Team.Score <- as.numeric(unlist(lapply(strsplit(res$Game.Score, '-'), '[[', 1)))
res$Oppt.Score <- as.numeric(unlist(lapply(strsplit(res$Game.Score, '-'), '[[', 2)))
res$Game <- paste0('Week ', res$Week, ': ', res$Result, ' (', res$Score, ')')

res$Margin <- abs(res$Team.Score - res$Oppt.Score)
res$Margin_scale <- as.numeric(cut(res$Margin, 
                                   breaks = c(-1,0,6,13,20,27,34,41,48,55), 
                                   labels = c(2,4,6,8,10,12,14,16,18)))

result_scale <- c('forestgreen', 'orange', 'firebrick3')
names(result_scale) <- levels(res$Result)

# Team avg. results
team_res <- res %>% 
  group_by(Team) %>% 
  summarise(Positivity = mean(Positivity), Negativity = mean(Negativity)) %>%
  mutate(Net.Positivity = Positivity - Negativity)
team_res <- res %>% 
  filter(Result != 'T') %>% 
  group_by(Team) %>% 
  summarise(games = length(Team), wins = length(Team[Result == 'W'])) %>% 
  mutate(WinPct = wins/games, PointSize = 4) %>% 
  right_join(team_res, by='Team')
team_res$WinPct <- round(team_res$WinPct, 3)

team_res$WinPct_scale <- cut(team_res$WinPct, 
                             breaks = c(-1, 0.33, 0.499, 0.67, 2), 
                             labels = c('.000-.333', '.333-.500', '.500-.667', '.667-1.000'))
winpct_scale <- c('firebrick3', 'firebrick1', 'chartreuse2', 'forestgreen')
names(winpct_scale) <- levels(team_res$WinPct_scale)