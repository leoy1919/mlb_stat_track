#' Create Organization-Level Projection Bar Chart
#'
#' Creates an interactive bar chart showing rest-of-season WAR projections aggregated by organization for a specific position, with optional player highlighting.
#'
#' @param posz Character string specifying the position to filter (e.g., "SP", "C", "SS")
#' @param mlbidz Numeric MLB ID of the player to highlight in the chart
#'
#' @return A plotly interactive bar chart object showing organizational WAR rankings
#'
#' @details
#' The function:
#' \itemize{
#'   \item Aggregates projected WAR by organization and position
#'   \item Ranks organizations by total projected WAR
#'   \item Highlights the specified player's organization using team colors
#'   \item Creates an interactive tooltip with player details
#' }
#'
#' @examples
#' \dontrun{
#' proj_rank_bar("SP", 545361)
#' }
proj_rank_bar <- function(posz, mlbidz){
  
  ### data we're worried about 
  proj_rank = projwar22 %>% filter(Position == posz) %>% 
    group_by(Org) %>% summarise(sum_war = sum(fWAR, na.rm = TRUE) ) %>%
    mutate(Rank = rank(-sum_war, ties.method = 'first') ) %>% arrange(Rank)
  
  ### data for ggplot
  dfs = projwar22 %>% filter(Position == posz) %>% mutate(highlight = ifelse(mlbid == mlbidz, '1', '0') ) %>% 
    arrange(desc(fWAR) ) %>% left_join(proj_rank, by = 'Org')
  dfs = data.table(dfs)
  ## fill NA mlbid as 0 and highlight as 0 string 
  dfs[is.na(mlbid), 'mlbid'] = 0
  dfs[is.na(highlight), 'highlight'] = '0'
  
  ### color to use for highlight 
  highcol = projwar22 %>% left_join(orgmap_df, by = c('Org' = 'org') ) %>% filter(mlbid == mlbidz) %>% pull(Color)
  
  plotz <- dfs %>% 
    ggplot(aes(x=reorder(Org, Rank), y = fWAR, fill = highlight, group = fWAR, text = paste0('Player: ', Name,
                                                                                             '<br>Org: ', Org, 
                                                                                             '<br>ROS Proj WAR: ', fWAR) ) ) + 
    geom_bar(stat = 'identity', color = 'white') + theme_classic() + xlab('Org') + ylab('ROS Projected WAR') + 
    labs(title = paste0(posz, ' ROS Projections'),
         subtitle = 'Rest-of-Season Projections from today') +
    scale_fill_manual(values = c('gray', highcol) ) + theme(axis.text.x = element_text(angle = 45, hjust = 1),
                                                            legend.position = 'none',
                                                            panel.background = element_rect(fill = '#FFFFFF', color = '#FFFFFF'),
                                                            plot.title = element_text(size = 16),
                                                            plot.subtitle = element_text(size = 12, face = 'italic') ) 
  
  ggplotly(plotz, tooltip = 'text')
  
}

#' Create Player-Level Projection Bar Chart
#'
#' Creates an interactive bar chart showing top 30 players by rest-of-season WAR
#' projections for a specific position, with rank annotation for the highlighted player.
#'
#' @param posz Character string specifying the position to filter (e.g., "SP", "C", "SS")
#' @param mlbidz Numeric MLB ID of the player to highlight and rank
#'
#' @return A plotly interactive bar chart object showing top 30 players with rank annotation
#'
#' @details
#' The function:
#' \itemize{
#'   \item Filters and ranks players by projected WAR for the specified position
#'   \item Displays top 30 players in descending WAR order
#'   \item Highlights the specified player using their team colors
#'   \item Annotates the chart with the player's rank and projected WAR
#' }
#'
#' @examples
#' \dontrun{
#' proj_rank_bar_player("SP", 545361)
#' }
proj_rank_bar_player <- function(posz, mlbidz){
  
  ### data for ggplot
  dfs = projwar22 %>% filter(Position == posz) %>% mutate(highlight = ifelse(mlbid == mlbidz, '1', '0') ) %>% 
    arrange(desc(fWAR) )
  dfs = data.table(dfs)
  ## fill NA mlbid as 0 and highlight as 0 string 
  dfs[is.na(mlbid), 'mlbid'] = 0
  dfs[is.na(highlight), 'highlight'] = '0'
  
  ### war rank 
  dfs <- dfs %>% mutate(Rank = rank(-fWAR, ties.method = 'min') )
  rankz = dfs %>% filter(mlbid == mlbidz) %>% pull(Rank)
  warz = dfs %>% filter(mlbid == mlbidz) %>% pull(fWAR)
  maxwar = dfs$fWAR %>% max()
  
  ### color to use for highlight 
  highcol = projwar22 %>% left_join(orgmap_df, by = c('Org' = 'org') ) %>% filter(mlbid == mlbidz) %>% pull(Color)
  
  plotz <- dfs %>% arrange(desc(fWAR) ) %>% head(30) %>% 
    ggplot(aes(x=reorder(Name, -fWAR), y = fWAR, fill = highlight, group = fWAR, text = paste0('Player: ', Name,
                                                                                               '<br>Org: ', Org, 
                                                                                               '<br>ROS Proj WAR: ', fWAR) ) ) + 
    geom_bar(stat = 'identity', color = 'white') + theme_classic() + xlab('Player') + ylab('ROS Projected WAR') + 
    geom_text(aes(x=25, y=maxwar-(maxwar*.1), label = paste0('Rank: ', rankz,
                                                             '<br>ROS WAR: ', warz) ) ) +
    labs(title = paste0(posz, ' ROS Projections'),
         subtitle = 'Rest-of-Season Projections from today') +
    scale_fill_manual(values = c('gray', highcol) ) + theme(axis.text.x = element_text(angle = 45, hjust = 1),
                                                            legend.position = 'none',
                                                            panel.background = element_rect(fill = '#FFFFFF', color = '#FFFFFF'),
                                                            plot.title = element_text(size = 16),
                                                            plot.subtitle = element_text(size = 12, face = 'italic') ) 
  
  ggplotly(plotz, tooltip = 'text')
  
}

#' Create Gauge Chart for Player Rate Statistics (2024)
#'
#' Generates a polar coordinate gauge chart displaying a player's percentile ranking
#' for various rate statistics from the 2024 season.
#'
#' @param playerz Numeric MLB ID of the player (batter)
#' @param statz Character string specifying the statistic to display. Options include:
#'   \itemize{
#'     \item "swm" - Swing and Miss Rate
#'     \item "ch" - Chase Rate
#'     \item "sw" - Swing Rate
#'     \item "gb_r" - Ground Ball Rate
#'     \item "med_ev" - Median Exit Velocity
#'     \item "avg_ev" - Average Exit Velocity
#'     \item "max_ev" - Maximum Exit Velocity
#'   }
#'
#' @return A gauge chart object showing the percentile and raw value,
#'   or a "Data Not Available" message if player data is missing
#'
#' @details
#' The function uses a polar coordinate system to create a gauge visualization.
#' The fill color represents the percentile ranking, with values displayed as
#' percentages (for rates) or MPH (for exit velocities).
#'
#' @examples
#' \dontrun{
#' gauge_charts_rates(545361, "swm")
#' gauge_charts_rates(545361, "max_ev")
#' }
gauge_charts_rates <- function(playerz, statz){
  
  ### what are we making chart of?
  statofchoice = if(statz == 'swm'){
    'SWM'
  } else if(statz == 'ch'){
    'Chase'
  } else if(statz == 'sw'){
    'Swing'
  } else if(statz == 'gb_r'){
    'GB'
  } else if(statz == 'med_ev'){
    'Median EV'
  } else if(statz == 'avg_ev'){
    'Avg EV'
  } else if(statz == 'max_ev'){
    'Max EV'
  } else 'NA'
  
  statofchoice_ntile = if(statz == 'swm'){
    'swm_n'
  } else if(statz == 'ch'){
    'ch_n'
  } else if(statz == 'sw'){
    'sw_n'
  } else if(statz == 'gb_r'){
    'gb_r_n'
  } else if(statz == 'med_ev'){
    'med_ev_n'
  } else if(statz == 'avg_ev'){
    'avg_ev_n'
  } else if(statz == 'max_ev'){
    'max_ev_n'
  } else 'NA'
  
  dataz = savant2021_master %>% filter(batter == playerz)
  
  if(nrow(dataz) == 0){
    
    df = data.table(x=0, y=0) 
    
    plotz <- ggplot(df, aes(x,y,) ) + geom_text(aes(label = 'Data Not Available')) + theme_classic() + 
      theme(axis.text.x = element_blank() ,
            axis.text.y = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank() )
    
  } else if(nrow(dataz) > 0 ){
    
    if(statz %in% c('swm', 'ch', 'sw', 'gb_r') ){
      
      plotz <- dataz %>% ggplot(aes(ymax = .data[[statofchoice_ntile]], ymin = 0, xmax=2, xmin=1) ) + 
        geom_rect(aes(ymax=1, ymin=0, xmax=2,xmin=1) ) +
        geom_rect(aes(fill = as.character(round(.data[[statofchoice_ntile]], 2)*100) ) ) +
        coord_polar(theta = 'y', start = -pi/2) + xlim(0,2) + ylim(0,2) + 
        geom_text(aes(x=0, y=0, label = paste0(round(100*.data[[statz]], 1), '%'), size = 5)  ) + # paste0(statofchoice, ': ', round(100*dataz[[statz]], 1), '%', 
        # '\nNtile: ', round(dataz[[statofchoice_ntile]], 2)*100 ) ), size = 5 ) +
        theme_void() +
        scale_fill_manual(values = mycolortable) + theme(legend.position = 'none',
                                                         panel.background = element_rect(fill = '#FFFFFF', color = '#FFFFFF') )
      
    } else if(statz %!in% c('swm', 'ch', 'sw', 'gb_r') ){
      
      plotz <- dataz %>% ggplot(aes(ymax = .data[[statofchoice_ntile]], ymin = 0, xmax=2, xmin=1) ) + 
        geom_rect(aes(ymax=1, ymin=0, xmax=2,xmin=1) ) +
        geom_rect(aes(fill = as.character(round(.data[[statofchoice_ntile]], 2)*100) ) ) +
        coord_polar(theta = 'y', start = -pi/2) + xlim(0,2) + ylim(0,2) + 
        geom_text(aes(x=0, y=0, label = paste0(round(.data[[statz]], 1), ' MPH'), size = 5) ) +  # paste0(statofchoice, ': ', round(dataz[[statz]], 1), ' MPH', 
        # '\nNtile: ', round(dataz[[statofchoice_ntile]], 2)*100 ) ), size = 5 ) +
        theme_void() +
        scale_fill_manual(values = mycolortable) + theme(legend.position = 'none',
                                                         panel.background = element_rect(fill = '#FFFFFF',color = '#FFFFFF') )
      
    }
    
  }
  
  return(plotz)
  
}

#' Create Gauge Chart for Player Rate Statistics (2025)
#'
#' Generates a polar coordinate gauge chart displaying a player's percentile ranking
#' for various rate statistics from the 2025 season against all pitcher handedness.
#' Note: This is a replica of the above function gauge_charts_rates, just specific to 2025 stats.
#' Future iteration should combine the two functions together to make it input-agnostic.
#'
#' @param playerz Numeric MLB ID of the player (batter)
#' @param statz Character string specifying the statistic to display. Options include:
#'   \itemize{
#'     \item "swm" - Swing and Miss Rate
#'     \item "ch" - Chase Rate
#'     \item "sw" - Swing Rate
#'     \item "gb_r" - Ground Ball Rate
#'     \item "med_ev" - Median Exit Velocity
#'     \item "avg_ev" - Average Exit Velocity
#'     \item "max_ev" - Maximum Exit Velocity
#'   }
#'
#' @return A gauge chart object showing the percentile and raw value,
#'   or a "Data Not Available" message if player data is missing
#'
#' @details
#' This function is similar to \code{gauge_charts_rates} but uses 2022 season data
#' and filters for aggregated stats against all pitcher types (p_throws == 'All').
#' The gauge uses a polar coordinate system with color-coded percentile fills.
#'
#' @examples
#' \dontrun{
#' gauge_charts_rates_22(545361, "ch")
#' gauge_charts_rates_22(660271, "avg_ev")
#' }
gauge_charts_rates_22 <- function(playerz, statz){
  
  ### what are we making chart of?
  statofchoice = if(statz == 'swm'){
    'SWM'
  } else if(statz == 'ch'){
    'Chase'
  } else if(statz == 'sw'){
    'Swing'
  } else if(statz == 'gb_r'){
    'GB'
  } else if(statz == 'med_ev'){
    'Median EV'
  } else if(statz == 'avg_ev'){
    'Avg EV'
  } else if(statz == 'max_ev'){
    'Max EV'
  } else 'NA'
  
  statofchoice_ntile = if(statz == 'swm'){
    'swm_n'
  } else if(statz == 'ch'){
    'ch_n'
  } else if(statz == 'sw'){
    'sw_n'
  } else if(statz == 'gb_r'){
    'gb_r_n'
  } else if(statz == 'med_ev'){
    'med_ev_n'
  } else if(statz == 'avg_ev'){
    'avg_ev_n'
  } else if(statz == 'max_ev'){
    'max_ev_n'
  } else 'NA'
  
  dataz = savant2022_master %>% filter(batter == playerz & p_throws == 'All')
  
  if(nrow(dataz) == 0){
    
    df = data.table(x=0, y=0) 
    
    plotz <- ggplot(df, aes(x,y,) ) + geom_text(aes(label = 'Data Not Available')) + theme_classic() + 
      theme(axis.text.x = element_blank() ,
            axis.text.y = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank() )
    
  } else if(nrow(dataz) > 0 ){
    
    if(statz %in% c('swm', 'ch', 'sw', 'gb_r') ){
      
      plotz <- dataz %>% ggplot(aes(ymax = .data[[statofchoice_ntile]], ymin = 0, xmax=2, xmin=1) ) + 
        geom_rect(aes(ymax=1, ymin=0, xmax=2,xmin=1) ) +
        geom_rect(aes(fill = as.character(round(.data[[statofchoice_ntile]], 2)*100) ) ) +
        coord_polar(theta = 'y', start = -pi/2) + xlim(0,2) + ylim(0,2) + 
        geom_text(aes(x=0, y=0, label = paste0(round(100*.data[[statz]], 1), '%'), size = 5)  ) + # paste0(statofchoice, ': ', round(100*dataz[[statz]], 1), '%', 
        # '\nNtile: ', round(dataz[[statofchoice_ntile]], 2)*100 ) ), size = 5 ) +
        theme_void() +
        scale_fill_manual(values = mycolortable) + theme(legend.position = 'none',
                                                         panel.background = element_rect(fill = '#FFFFFF', color = '#FFFFFF') )
      
    } else if(statz %!in% c('swm', 'ch', 'sw', 'gb_r') ){
      
      plotz <- dataz %>% ggplot(aes(ymax = .data[[statofchoice_ntile]], ymin = 0, xmax=2, xmin=1) ) + 
        geom_rect(aes(ymax=1, ymin=0, xmax=2,xmin=1) ) +
        geom_rect(aes(fill = as.character(round(.data[[statofchoice_ntile]], 2)*100) ) ) +
        coord_polar(theta = 'y', start = -pi/2) + xlim(0,2) + ylim(0,2) + 
        geom_text(aes(x=0, y=0, label = paste0(round(.data[[statz]], 1), ' MPH'), size = 5) ) +  # paste0(statofchoice, ': ', round(dataz[[statz]], 1), ' MPH', 
        # '\nNtile: ', round(dataz[[statofchoice_ntile]], 2)*100 ) ), size = 5 ) +
        theme_void() +
        scale_fill_manual(values = mycolortable) + theme(legend.position = 'none',
                                                         panel.background = element_rect(fill = '#FFFFFF',color = '#FFFFFF') )
      
    }
    
  }
  
  return(plotz)
}

#' Create Statcast Radar Chart by Pitcher Handedness
#'
#' Generates an interactive radar chart displaying a player's percentile rankings
#' across multiple Statcast metrics, split by pitcher handedness (RHP/LHP).
#'
#' @param playerz Numeric MLB ID of the player (batter)
#' @param pthrowz Character string specifying pitcher handedness: "R" for right-handed
#'   or "L" for left-handed pitchers
#'
#' @return A plotly radar chart object showing percentile rankings across multiple metrics,
#'   or a message indicating "Not in MLB" if the player's team is not in the organization map
#'
#' @details
#' The function creates a radar chart with the following metrics:
#' \itemize{
#'   \item SWM - Swing and Miss Rate
#'   \item CH - Chase Rate
#'   \item SW - Swing Rate
#'   \item GB_R - Ground Ball Rate
#'   \item MED_EV - Median Exit Velocity
#'   \item MAX_EV - Maximum Exit Velocity
#' }
#'
#' The chart is color-coded using the player's current team colors and includes
#' interactive tooltips showing both raw values and percentile rankings.
#'
#' @examples
#' \dontrun{
#' statcast_radar(545361, "R")
#' statcast_radar(660271, "L")
#' }
statcast_radar <- function(playerz, pthrowz){
  
  ### loading statcast data 
  walker <- savant2022_master %>% filter(batter == playerz)
  
  ### hexcolor
  hexteam = walker %>% pull(currentteam) %>% unique()
  hexcolorz = orgmap_df %>% filter(org == hexteam) %>% pull(Color)
  
  walker_df = walker %>% select(game_year, batter, player_name, p_throws, currentteam, swm, swm_n, ch, ch_n, sw, sw_n, 
                                med_ev, med_ev_n, max_ev, max_ev_n, gb_r, gb_r_n)
  
  #### making it so that table is good for radar chart 
  walker_df_p = walker_df %>% filter(p_throws == pthrowz) %>% 
    pivot_longer(cols = c('swm', 'swm_n', 'ch', 'ch_n', 'sw', 'sw_n', 
                          'med_ev', 'med_ev_n', 'max_ev', 'max_ev_n', 'gb_r', 'gb_r_n'),
                 names_to = 'metric',
                 values_to = 'value') %>%
    mutate(label = gsub("_N", '', toupper(metric) ) ) %>% 
    mutate(cat = ifelse(grepl('_n', metric), 'ntile', 'rate')) %>%
    #### pivot wider again 
    pivot_wider(id_cols = c('game_year', 'batter', 'p_throws', 'player_name', 'currentteam', 'label'),
                values_from = 'value',
                names_from = 'cat') %>%
    #### put on labels for tooltip
    mutate(type = ifelse(grepl('_EV', label), 'ev', 'percent') ) %>% 
    mutate(text = ifelse(type == 'percent', paste0(label, ': ', round(rate, 3)*100, '%',
                                                   '<br>Ntile: ', round(ntile, 2)*100), 
                         paste0(label, ': ', rate, ' MPH',
                                '<br>Ntile: ', round(ntile, 2)*100) ) )
  
  
  #### plotly radar chart
  walkerfig = plot_ly(hoverinfo = 'text',
                      walker_df_p,
                      type = 'scatterpolar', 
                      r = ~ntile,
                      theta = ~label,
                      text = ~text,
                      fill = 'toself',
                      fillcolor = hexcolorz,
                      opacity = 0.7,
                      hoveron = 'points',
                      marker = list(color = hexcolorz)
  ) %>%
    ### layout stuff 
    layout(
      polar = list(
        radialaxis = list(
          visible = F,
          range = c(0,1)
        )
      ),
      showlegend = F
    )
  
  ###### IF TEAM IS NOT IN HEXCOLOR
  emptydf = data.table(x=1, y=1)
  emptyplotz <- emptydf %>% ggplot(aes(x, y,) ) + geom_text(aes(label = 'Not in MLB')) + theme_classic() +
    theme(axis.text.x = element_blank() ,
          axis.text.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank() )
  
  emptyplotly <- ggplotly(emptyplotz, tooltip = FALSE) %>% config(displayModeBar = FALSE)
  
  
  #### plotz to return 
  plotz <- if(hexteam %!in% orgmap_df$org){
    
    emptyplotly #%>% config(displayModeBar = FALSE)
    
  } else if (hexteam %in% orgmap_df$org){
    
    walkerfig
    
  }
  
  return(plotz)
}


