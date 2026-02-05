suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(data.table)
  library(DBI)
  library(ggplot2)
  library(RPostgreSQL)
  library(ggtext)
  library(tidyr)
  library(ggnewscale)
  library(plotly)
  library(ztable)
  library(reactable)
})

### functions
`%!in%` = Negate(`%in%`)

orgmap_df <- fread('orgmap_df.csv')
### athl
ath = orgmap_df |> filter(parentOrgId == 133)
ath[org == 'OAK', 'org'] = 'ATH'
ath[org == 'ATH', 'parentOrgName'] = 'Athletics'
orgmap_df = rbind(orgmap_df,ath)

### orgcol
orglist <- orgmap_df$org
orgcol <- orgmap_df$Color
names(orgcol) <- orglist

cum_bat <- fread('cum_bat_24.csv')
### as numeric
cum_bat$WAR <- as.numeric(cum_bat$WAR)
cum_bat_22 <- fread('cum_bat_25.csv')

# ### as numeric
cum_bat_22$WAR <- as.numeric(cum_bat_22$WAR)
cum_bat_22$playerid <- as.character(cum_bat_22$playerid)

hit_proj_all <- fread('hit_proj_all.csv')
hit_proj_all_2021 <- fread('hit_proj_all_2021.csv')

dailyroster <- fread('dailyroster_25.csv')
setnames(dailyroster, 'personfullName', 'person.fullName')
setnames(dailyroster, 'position_abbreviation', 'position.abbreviation')
## join fangraphs
player_mapper = fread('PLAYERIDMAP.csv')
player_mapper = player_mapper |> select(MLBID, IDFANGRAPHS)
setnames(player_mapper,'MLBID', 'mlbid')
setnames(player_mapper,'IDFANGRAPHS', 'fangraphs')
## re-init
dailyroster = dailyroster |> left_join(player_mapper, by = 'mlbid')
dailyroster$fangraphs <- as.integer(dailyroster$fangraphs)

dailyroster$team_abbreviation %>% table()
dailyroster <- data.table(dailyroster)

## rename
dailyroster[team_abbreviation == 'CHC', 'team_abbreviation'] <- 'CHI'
dailyroster[team_abbreviation == 'LAD', 'team_abbreviation'] <- 'LA'
dailyroster[team_abbreviation == 'NYM', 'team_abbreviation'] <- 'NY'
dailyroster[team_abbreviation == 'WSH', 'team_abbreviation'] <- 'WAS'

### join FG WITH PLAYERINFO TABLE
## init
stattrack_df <- cum_bat %>% #left_join(day_team, by = c('Date', 'playerid') ) %>%
  left_join(dailyroster %>% select(fangraphs, mlbid, name, team_abbreviation, position.abbreviation), by = c('playerid' = 'fangraphs') )

### NO PITCHERS
stattrack_df <- stattrack_df %>% filter(position.abbreviation != 'P')

fullstats = fread('fullstats2025.csv')
setnames(fullstats, 'MLBAMID', 'mlbid')
setnames(fullstats, 'BB%', 'BB_rate')
setnames(fullstats, 'K%', 'K_rate')
## round
fullstats$`wRC+` = round(fullstats$`wRC+`, 0)
fullstats$WAR = round(fullstats$WAR, 1)

fullstats <- data.table(fullstats)
fullstats <- fullstats %>% select(Name, Team, mlbid, G, PA, HR, R, RBI, SB, BB_rate, K_rate, ISO, BABIP, AVG, OBP, SLG, wOBA, `wRC+`, BsR, Off, Def, WAR)
fullstats[, logo := Team]
fullstats[, logo := ifelse(logo == 'LAA', 'ANA',
                        ifelse(logo == 'LA', 'LAD',
                               ifelse(logo == 'TB', 'TBD',
                                      ifelse(logo == 'CHI', 'CHC',
                                             ifelse(logo == 'WAS', 'WSN',
                                                    ifelse(logo == 'KC', 'KCR',
                                                           ifelse(logo == 'MIA', 'FLA',
                                                                  ifelse(logo == 'NY', 'NYM',
                                                                         ifelse(logo == 'SD', 'SDP',
                                                                                ifelse(logo == 'CWS', 'CHW',
                                                                                       ifelse(logo == 'SF', 'SFG', logo)))))))))))]
### change fullstats Team to make it same as org_map
fullstats[, Team := ifelse(Team == 'ATH', 'OAK',
                           ifelse(Team == 'CHC', 'CHI',
                                  ifelse(Team == 'CHW', 'CWS',
                                         ifelse(Team == 'KCR', 'KC',
                                                ifelse(Team == 'LAD', 'LA',
                                                       ifelse(Team == 'NYM', 'NY',
                                                              ifelse(Team == 'SDP', 'SD',
                                                                     ifelse(Team == 'SFG', 'SF',
                                                                            ifelse(Team == 'TBR', 'TB',
                                                                                   ifelse(Team == 'WSN', 'WAS', Team))))))))))]

### join with teamnames 
fullstats <- fullstats %>% left_join(orgmap_df, by = c('Team' = 'org') )

# #### player bios ###### #### player bios ###### #### player bios ###### #### player bios #####

playerbio = fread('playerbio_25.csv')
setnames(playerbio, 'current_org', 'org')
setnames(playerbio, 'batter_stance', 'bats')
setnames(playerbio, 'pitcher_stance', 'throws')
playerbio = playerbio |> left_join(orgmap_df, by = 'org') |>
  select(-org, Color, urlnames, viz_name)

### changing throws and bats to full words
playerbio[, throws := ifelse(throws == 'R', 'Right',
                             ifelse(throws == 'L', 'Left', NA) )][
                               , bats := ifelse(bats == 'R', 'Right',
                                                ifelse(bats == 'L', 'Left',
                                                       ifelse(bats == 'S', 'Switch', NA) ) )]


day_bat <- fread('day_bat.csv')
projwar22 <- fread('projwar26.csv')

##### gauge charts ##### gauge charts ##### gauge charts ##### gauge charts #####

savant2021_master <- fread('savant2024_master.csv')
savant2021_master$currentteam %>% table()
### NA entiles to 0.001

##### visuals
### colors
colorint <- seq(0,100,1) %>% as.character()
mycolortable = gradientColor(low = 'blue', mid = 'white', high = 'red', n = colorint %>% length(), plot = FALSE)
names(mycolortable) = colorint

##### 2022 ##### 2022 ##### 2022 ##### 2022 ##### 2022 ##### 2022 ##### 2022 ##### 2022 #####
savant2022_master <- fread('savant2025_master.csv')

savant2022_master$currentteam %>% table()

### change current team abbreviation
savant2022_master <- data.table(savant2022_master)
savant2022_master[, currentteam := ifelse(currentteam == 'CHC', 'CHI', currentteam)]
savant2022_master[, currentteam := ifelse(currentteam == 'NYM', 'NY', currentteam)]
savant2022_master[, currentteam := ifelse(currentteam == 'LAD', 'LA', currentteam)]
savant2022_master[, currentteam := ifelse(currentteam == 'WSH', 'WAS', currentteam)]

##### visuals
### colors
colorint <- seq(0,100,1) %>% as.character()
mycolortable = gradientColor(low = 'blue', mid = 'white', high = 'red', n = colorint %>% length(), plot = FALSE)
names(mycolortable) = colorint

####### 2nd tab ####### 2nd tab ####### 2nd tab ####### 2nd tab ####### 2nd tab  ####### 

## ntile cols
### init
ntilecolz = gradientColor(low = 'blue', mid = 'white', high = 'red', n = colorint %>% length(), plot = FALSE)
### init
savant2022_master_df <- savant2022_master %>% mutate(logo = currentteam, .after = currentteam) 
### putting full team name in here 
### re-init
savant2022_master_df <- savant2022_master_df %>% left_join(stattrack_df_22 %>% select(team_abbreviation, name) %>% unique(), by = c('currentteam' = 'team_abbreviation') )
savant2022_master_df$currentteam %>% table()
savant2022_master_df <- data.table(savant2022_master_df)
savant2022_master_df[, logo := ifelse(logo == 'LAA', 'ANA',
                                      ifelse(logo == 'LA', 'LAD',
                                             ifelse(logo == 'TB', 'TBD',
                                                    ifelse(logo == 'CHI', 'CHC',
                                                           ifelse(logo == 'WAS', 'WSN',
                                                                  ifelse(logo == 'KC', 'KCR',
                                                                         ifelse(logo == 'MIA', 'FLA',
                                                                                ifelse(logo == 'NY', 'NYM',
                                                                                       ifelse(logo == 'SD', 'SDP',
                                                                                              ifelse(logo == 'CWS', 'CHW',
                                                                                                     ifelse(logo == 'SF', 'SFG', logo)))))))))))]

