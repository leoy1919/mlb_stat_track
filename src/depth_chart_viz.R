library(ggtext)

### functions
`%!in%` = Negate(`%in%`)

MLBStadiumsPathData <- fread('MLBStadiumsPathData.csv')

projwar22 <- fread('projwar26.csv')

### init
projwar22_onepos <- projwar22 %>% group_by(mlbid) %>% slice(which.max(fWAR) )
projwar22_onepos <- projwar22_onepos %>% ungroup() %>% group_by(Org, Position) %>% mutate(Rank = rank(-fWAR, ties.method = 'first') ) 

####################### DEPTH CHART VISUAL ####################### DEPTH CHART VISUAL ##################################
### position coordinate
pos_list <- c('C', '1B', '2B', '3B', 'SS', 'LF', 'CF', 'RF', 'DH', 'IF', 'OF', 'UTL', 'SP', 'RP')
position_coord_x = c(0, 100, 60, -100, -60, -130, 0, 130, 150, 60, 0, -60, 350, 350)
position_coord_y = c(-30, 70, 125, 70, 125, 230, 300, 230, 10, 125, 300, 1250, 250, 20)
### init
position_coord = c(rbind(pos_list, position_coord_x, position_coord_y)) %>% matrix(ncol = 3, byrow = TRUE) %>% data.table()
colnames(position_coord) <- c('position', 'x_coord', 'y_coord')
position_coord$x_coord <- as.numeric(position_coord$x_coord)
position_coord$y_coord <- as.numeric(position_coord$y_coord)


### mapper
## init
visual_team_mapper <- orgmap_df %>% select(org, viz_name)

#################
### functions ###
#################
#' Transform MLBAM Coordinate System
#'
#' Converts MLBAM  hit coordinate data from pixel coordinates to feet-based field coordinates. 
#'
#' @param data Data frame containing MLBAM hit coordinate data
#' @param x Character string specifying the column name for x-coordinates (default: "hc_x")
#' @param y Character string specifying the column name for y-coordinates (default: "hc_y")
#' @param column_suffix Character string to append to new coordinate column names (default: "_")
#' @param scale Numeric scaling factor to convert pixels to feet (default: 2.495671)
#'
#' @return The input data frame with two additional columns containing transformed coordinates
#'
#' @details
#' The transformation applies the following formulas:
#' \itemize{
#'   \item x' = scale * (x - 125)
#'   \item y' = scale * (199 - y)
#' }
#' This centers the coordinate system at home plate and scales from pixels to feet.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(hc_x = 100, hc_y = 150)
#' mlbam_xy_transformation(df)
#' }
mlbam_xy_transformation = function(data,
                                   x="hc_x", y="hc_y",
                                   column_suffix="_",
                                   scale=2.495671) {
  data[,paste0(x, column_suffix)] = scale * (data[,x] - 125)
  data[,paste0(y, column_suffix)] = scale * (199 - data[,y])
  data
}

#' Add MLB Stadium Geometry to ggplot
#'
#' A ggplot2 layer function that adds MLB stadium outfield wall geometries to a plot,
#' with support for specific stadiums or generic field dimensions.
#'
#' @param mapping Aesthetic mapping (automatically set to x, y, and group)
#' @param data Data frame containing stadium path data (automatically loaded from MLBStadiumsPathData)
#' @param stat Character string specifying the statistical transformation (default: "identity")
#' @param position Character string specifying position adjustment (default: "identity")
#' @param na.rm Logical indicating whether to remove NA values (default: FALSE)
#' @param show.legend Logical indicating whether to show legend (default: NA)
#' @param inherit.aes Logical indicating whether to inherit aesthetics (default: FALSE)
#' @param stadium_ids Character vector of stadium identifiers. Options include:
#'   \itemize{
#'     \item "generic" - Generic MLB field (default)
#'     \item "all" - All stadiums including generic
#'     \item "all_mlb" - All MLB team stadiums excluding generic
#'     \item Team abbreviations (e.g., "NYY", "BOS", "LAD")
#'   }
#' @param stadium_segments Character vector specifying which stadium segments to draw.
#'   Default is "outfield_outer". Use "all" for complete stadium geometry.
#' @param stadium_transform_coords Logical indicating whether to apply MLBAM coordinate
#'   transformation to stadium coordinates (default: FALSE)
#' @param ... Additional arguments passed to GeomPath
#'
#' @return A ggplot2 layer object containing stadium geometry
#'
#' @details
#' This function creates a custom ggplot2 layer that overlays MLB stadium outfield walls
#' on a plot. It's particularly useful for visualizing batted ball locations and spray charts.
geom_mlb_stadium = function(mapping = NULL, data = NULL, stat = "identity",
                            position = "identity", na.rm = FALSE, show.legend = NA,
                            inherit.aes = FALSE,
                            stadium_ids = NULL,
                            stadium_segments = "outfield_outer",
                            stadium_transform_coords=FALSE,
                            ...) {
  
  mapping = aes(x=x, y=y, group=segment, ...)
  data = MLBStadiumsPathData
  
  if (is.null(stadium_ids)) {
    stadium_ids = "generic"
  } else if ("all" %in% stadium_ids) {
    stadium_ids = unique(data$team)
  } else if ("all_mlb" %in% stadium_ids) {
    stadium_ids = unique(data$team)
    cc = which(stadium_ids == "generic")
    if (length(cc) > 0) {
      stadium_ids = stadium_ids[-cc]
    }
  }
  
  data =
    do.call(rbind.data.frame,
            lapply(stadium_ids, function(s) {
              data[ (data$team == s),]
            })
    )
  
  if ("all" %in% stadium_segments) {
    # noop
  } else if (!is.null(stadium_segments)) {
    data = do.call(rbind.data.frame, lapply(stadium_segments, function(s) {
      data[ (data$segment == s),]
    })
    )
  }
  
  if (stadium_transform_coords) {
    data = mlbam_xy_transformation(data, x="x", y="y", column_suffix="")
  }
  
  layer(
    geom = GeomPath, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' Create Organizational Depth Chart Visualization
#'
#' Generates a visual depth chart showing top 3 players at each position for a specified
#' organization, overlaid on their home stadium diagram.
#'
#' @param orgname Character string specifying the organization's parent name 
#'   (e.g., "Yankees", "Red Sox", "Dodgers")
#' @param mlbidz Numeric MLB ID of a player to highlight in red and bold on the depth chart
#'
#' @return A ggplot2 object displaying the depth chart with player names positioned
#'   at their respective field positions over the stadium outline
#'
#' @details
#' The function:
#' \itemize{
#'   \item Filters top 3 players by projected WAR at each position for the specified organization
#'   \item Overlays player names on a stadium diagram using team-specific field dimensions
#'   \item Highlights the specified player (mlbidz) in red and bold
#'   \item Uses coordinate mapping to position players at their defensive positions
#'   \item Supports rich text formatting to display multiple players per position
#' }
#'
#' @examples
#' \dontrun{
#' depth_chart_viz("Yankees", 592450)
#' depth_chart_viz("Dodgers", 660271)
#' depth_chart_viz("Athletics", 545361)
#' }
depth_chart_viz <- function(orgname, mlbidz){
  
  teamz = orgmap_df %>% filter(parentOrgName == orgname) %>% pull(org)
  if(orgname == 'Athletics'){
    teamz = 'OAK'
  }
  ### for plot
  mlbgeom_abbrev <- orgmap_df %>% filter(org == teamz) %>% pull(viz_name)
  ## pick team 
  tb_df <- projwar22_onepos %>% filter(Org == teamz) %>% filter(Rank <= 3)
  ## as english ranks
  # tb_df <- tb_df %>% mutate(Rank = as.english(Rank) %>% as.character() )
  
  tb_df <- tb_df %>% left_join(position_coord, by = c('Position' = 'position') )
  tb_df <- data.table(tb_df)
  
  ### bold name of interest
  tb_df[, Name := ifelse(mlbid == mlbidz, paste0("<b><span style='color:red'>", Name, '</span></b>'), Name)]#[
        # , chosen := ifelse(mlbid == mlbidz, 1, 0)] 
  
  ## cols to unite when pivotting
  unitecols <- paste0('Name',  1:3 )
  # unitecols <- c('Nameone', 'Nametwo', 'Namethree')
  ## init
  tb_df_pivot <- tb_df %>% pivot_wider(id_cols = c('Position', 'x_coord', 'y_coord'),
                                       values_from = 'Name',
                                       names_from = 'Rank',
                                       names_prefix = 'Name') %>% unite(names, unitecols, sep = '<br>' )
  
  
  ### get rid of \nNA
  tb_df_pivot$names <- gsub('<br>NA', '', tb_df_pivot$names)
  
  ### Plot
  plotz <- ggplot() + geom_mlb_stadium(alpha = 0.3,
                                       stadium_ids = mlbgeom_abbrev,
                                       stadium_transform_coords = TRUE,
                                       stadium_segments = "all") + #geom_image(aes(x = 0, y = 220, image =  paste0('./logos/', 'TEX', '.png') ), size = 0.2,  image_fun = transparent) +
    xlim(-250, 250) +
    theme_classic() + coord_fixed() +
    geom_richtext(data = tb_df_pivot, aes(x=x_coord, y=y_coord, label = names ),
                  size = 4, fill = NA, label.color = NA ) + ##, fontface = 'bold' ) +
    # geom_text(data = depth_chart_df_pivot, aes(x=x_coord, y=y_coord, label = names ), size = 4, fontface = "bold" ) +
    # scale_size_manual(values = scalesizez_size) + #scale_size(range = c(1, 12) ) +
    theme(legend.position = 'none',
          # panel.background = element_rect(fill = '#FCFBF8', color = '#FCFBF8'),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.x = element_blank(),
          axis.line.y = element_blank() )
  
  return(plotz)
}
