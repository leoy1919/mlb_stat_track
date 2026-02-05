suppressPackageStartupMessages({
  library(shiny)
  library(shinyBS)
  library(shinydashboard)
  library(shinydashboardPlus)
  library(waiter)
  library(shinyWidgets)
  library(reactablefmtr)
})

source('./ref/stattrack_backend.R')
source('./src/stattrack_functions.R')
source('./src/depth_chart_viz.R')

ui = dashboardPage( ## whole page
      options = list(sidebarExpandOnHover = TRUE),
      header = dashboardHeader(title = '', titleWidth = 230), # title = 'MLB Hitters'), ## top left title
      sidebar = dashboardSidebar(collapsed = TRUE, minified = TRUE, 
                       sidebarMenu(
                         menuItem('Hitter Dashboard', icon = icon('desktop'), tabName = 'hitterdash'),
                         menuItem('Leaderboard', icon = icon('chart-line'), tabName = 'leaderboard')
                       ) ### sidebarmenu
          ), ## sidebar

    body = dashboardBody(

      tags$head(
      tags$script(HTML("$('body').addClass('sidebar-mini');"
                       ) ),
      tags$style(HTML("
                        /* body */
                        .content-wrapper, .right-side {
                        background-color: #FAF9F6;
                        }
              
                        /* box colors */
                        .box.box-solid.box-primary>.box-header {
                        color:#FFFFFF;
                        background:#666666
                        }

                        .box.box-solid.box-primary{
                         border-bottom-color:#666666;
                         border-left-color:#666666;
                         border-right-color:#666666;
                         border-top-color:#666666;
                        }
              
                        /* logo */
                        .skin-blue .main-header .logo {
                        background-color: #FAF9F6;
                        }

                        /* logo when hovered */
                        .skin-blue .main-header .logo:hover {
                        background-color: #001A3F;
                        }

                        /* navbar (rest of the header) */
                        .skin-blue .main-header .navbar {
                        background-color: #001A3F;
                        }        

                        /* main sidebar */
                        .skin-blue .main-sidebar {
                        background-color: #001A3F;
                        }
                      
                        /* main sidebar collapsed */
                        .skin-blue .main-sidebar-collapse {
                        width: 200px;
                        background-color: #001A3F;
                        }

                        # /* active selected tab in the sidebarmenu */
                        # .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                        # background-color: #ff0000;
                        # }
                        # 
                        # /* other links in the sidebarmenu */
                        # .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                        # background-color: #00ff00;
                        # color: #000000;
                        # }
                        # 
                        # /* other links in the sidebarmenu when hovered */
                        # .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                        # background-color: #C0111F;
                        # }
                        # /* toggle button when hovered  */                    
                        # .skin-blue .main-header .navbar .sidebar-toggle:hover{
                        # background-color: #C0111F;
                        # }

                      ") ),
      ),


        tabItems(
          tabItem(tabName = 'hitterdash',

######## START UP MODAL ##################### START UP MODAL ##################### START UP MODAL ##################### START UP MODAL #############
               
                fluidRow(
                  bsModal(
                    id = 'startupModal',
                    title = h1(strong('Welcome to MLB Stat Track!'), align = 'center'), 
                    trigger = 'Project Title', size = 'large',
                    
                    HTML("<h3><b>What Does This Dashboard Tell You?</b></h3>
                           <ul><li><h4>This dashboard aims to summarize player performance and contribution 
                           of all players across Major League Baseball this season</h4></li>
                           <li><h4>The goal is for users to get an <em>accurate snapshot</em> 
                           of a player's true skill by observing the various visuals 
                           provided on the screen</h4></li></ul>
                           <br>
                           
                           <h3><b>How To Use</b></h3>
                           <ul><li><h4>Select an <b>organization</b> and a <b>player</b> from the top right box to populate
                           each visual</h4></li>
                           <li><h4>Each visual on the first page <em>(Hitter Dashboard)</em> provides a different snapshot of the player</h4></li>
                           <li><h4>Click on the <b>dropdown button</b> in each section 
                           to better understand the meaning of each visual!</h4></li>
                           <li><h4>Go to the second page <em>(Leaderboard)</em> to see the KPI and Stats leaderboard per team</h4></li></ul>
                           <br>
                           
                           <h4><b>Click on the <em>How To Navigate Dashboard?</em> button on the top left 
                           to navigate back to this pop-up at any time</b></h4>
                           ")
                  )
                ),


fluidRow(
  bsModal(
    id = 'infoModal',
    title = h1(strong('Welcome to MLB Stat Track!'), align = 'center'), 
    trigger = 'infomodalbutton', size = 'large',
    
    HTML("<h3><b>What Does This Dashboard Tell You?</b></h3>
                           <ul><li><h4>This dashboard aims to summarize player performance and contribution 
                           of all players across Major League Baseball this season</h4></li>
                           <li><h4>The goal is for users to get an <em>accurate snapshot</em> 
                           of a player's true skill by observing the various visuals 
                           provided on the screen</h4></li></ul>
                           <br>
                           
                           <h3><b>How To Use</b></h3>
                           <ul><li><h4>Select an <b>organization</b> and a <b>player</b> from the top right box to populate
                           each visual</h4></li>
                           <li><h4>Each visual on the first page <em>(Hitter Dashboard)</em> provides a different snapshot of the player</h4></li>
                           <li><h4>Click on the <b>dropdown button</b> in each section 
                           to better understand the meaning of each visual!</h4></li>
                           <li><h4>Go to the second page <em>(Leaderboard)</em> to see the KPI and Stats leaderboard per team</h4></li></ul>
                           <br>
         
                           <h4><b>Click on the <em>How To Navigate Dashboard?</em> button on the top left 
                           to navigate back to this pop-up at any time</b></h4>
                           ")
  )
),

######## START UP MODAL ##################### START UP MODAL ##################### START UP MODAL ##################### START UP MODAL #############

          actionButton("infomodalbutton", "How To Navigate Dashboard?"),

          hr(),

          h2('Player Info'),

          box(width = 12, solidHeader = TRUE, collapsible = TRUE,
                fluidRow(
                  column(5, align = 'left', 
                         dropdownButton(
                           inputId = 'dropdown1.1',
                           label = 'How To View',
                           icon = icon('baseball-ball'),
                           circle = FALSE,
                           
                           HTML("<h4><b>How To View The Depth Chart Visual:</b></h4>
                                  <ul><li><h5>This chart shows which position the player of interest will accrue the most playing time with his current team</h5></li>
                                  <li><h5>The name of the player of the page you are on will be highlighted in red</h5></li></ul>
                                               ")
                         ) ## dropdown
                  ) ## column
                ), ## row

                br(),

                fluidRow(
                 
                 uiOutput('depth'),
                 
                 uiOutput('bio'),
                 
                 column(3, align = 'center',
                    box(width = 12,
                        column(12, align = 'center',
                               plotOutput('logo', height = '125px'),
                               selectInput('org', 'Select a Team:',
                                           choices = orgmap_df$parentOrgName %>% unique() %>% sort() ),
                               
                               selectInput('player', 'Select a Player:',
                                           choices = c(stattrack_df$Name, stattrack_df_22$Name) %>% unique() %>% sort() )
                        ) ## col
                    ), ## box
                 ) ## col
               ) ## row
              
          ), ### box
  
               hr(),
  
               br(),
               
               h2('Player Summary'),

              fluidRow(
                column(5, align = 'left', 
                       dropdownButton(
                         inputId = 'dropdown1',
                         label = 'How To View',
                         icon = icon('baseball-ball'),
                         circle = FALSE,
                         
                         HTML("<h4><b>How To View This Visual:</b></h4>
                                <h5>This radar chart provides a quick summary of a player's offensive skills. 
                                The chart is broken up into three sections- vs All pitchers, vs Right-Handed Pitchers (RHP), vs Left-Handed Pitchers (LHP)</h5>
                                <br>
                                <h5><b>SWM: </b> Swing and Miss Rate</b></h5>
                                <ul><li><h5>The percentage of the time a hitter swings and misses</h5></li></ul>
                                <h5><b>CH: </b> Chase Rate</b></h5>
                                <ul><li><h5>The percentage of the time a hitter swings at a pitch outside of the strike zone</h5></li></ul>
                                <h5><b>SW: </b> Swing Rate</b></h5>
                                <ul><li><h5>The percentage of the time a hitter swings at a pitch</h5></li></ul>
                                <h5><b>MED_EV: </b>Median Exit Velocity</b></h5>
                                <ul><li><h5>The median value of how hard a hitter hits the ball</h5></li></ul>
                                <h5><b>MAX_EV: </b>Max Exit Velocity</b></h5>
                                <ul><li><h5>The maximum value of how hard a hitter hits the ball</h5></li></ul>
                                <h5><b>GB_R: </b>Ground Ball Rate</b></h5>
                                <ul><li><h5>The percentage of the time a hitter hits the ball on the ground</h5></li></ul>
                                             ")
                       ) ## dropdown
                ) ## column
              ), ## row

              br(),

               fluidRow(
                 
                 uiOutput('radar22')
                 
               ), ## row

               hr(),

               br(),

               h2('Evaluation'),
  
              fluidRow(
                column(5, align = 'left', 
                       dropdownButton(
                         inputId = 'dropdown2',
                         label = 'How To View',
                         icon = icon('baseball-ball'),
                         circle = FALSE,
                         
                         HTML("<h4><b>How To View This Visual:</b></h4>
                              <h5>The gauge chart provides a quick breakdown of the <em>six</em> KPIs when evaluating a hitter</h5>
                              <h5>The gauge chart color represents the percentile with respect to the rest of the league</h5>
                              <h5>The higher the percentile, the better the performance<h5>
                              <h5><em>All stats represented by the gauge charts are hitter performance vs All pitchers</em></h5>
                              <br>
                              <h5><b>SWM: </b> Swing and Miss Rate</b></h5>
                              <ul><li><h5>The percentage of the time a hitter swings and misses</h5></li></ul>
                              <h5><b>Chase: </b> Chase Rate</b></h5>
                              <ul><li><h5>The percentage of the time a hitter swings at a pitch outside of the strike zone</h5></li></ul>
                              <h5><b>Swing: </b> Swing Rate</b></h5>
                              <ul><li><h5>The percentage of the time a hitter swings at a pitch</h5></li></ul>
                              <h5><b>Median EV: </b>Median Exit Velocity</b></h5>
                              <ul><li><h5>The median value of how hard a hitter hits the ball</h5></li></ul>
                              <h5><b>Max EV: </b>Max Exit Velocity</b></h5>
                              <ul><li><h5>The maximum value of how hard a hitter hits the ball</h5></li></ul>
                              <h5><b>GB%: </b>Ground Ball Rate</b></h5>
                              <ul><li><h5>The percentage of the time a hitter hits the ball on the ground</h5></li></ul>
                                           ")
                       ) ## dropdown
                ) ## column
              ), ## row
              
              br(),

               fluidRow(
  
                 uiOutput('eval22')
                 
               ), ## row
  
               fluidRow(
  
                 uiOutput('eval21')
                 
               ), ## row
  
               hr(),

               br(),

               h2('Player Value'),

              fluidRow(
                column(5, align = 'left',
                       dropdownButton(
                         inputId = 'dropdown3',
                         label = 'How To View',
                         icon = icon('baseball-ball'),
                         circle = FALSE,
                         
                         HTML("<h4><b>How To View This Visual:</b></h4>
                              <h5>The bar charts represent the rest-of-season <b>WAR</b> projections of a player<h5>
                              <h5>The chart on the left looks at projected performance by team. It aims to answer the question of 
                              much of the team's total positional WAR the player of interest is projected to contribute to 
                              for the rest of the season (stacked bar chart)</h5>
                              <h5>The chart on the right looks at projected performance by player. It aims to answer the question of 
                              how does his projected performance stack up against all other players who play the same position</h5>
                              <br>
                              <h5><em><b>What is WAR?</b></em></h5>
                              <h5>WAR measures a player's value in all facets of the game by deciphering how many more 
                              wins he's worth than a replacement-level player at his same position</h5> 
                                           "),
                         a(href="https://www.mlb.com/glossary/advanced-stats/wins-above-replacement", "Click here to read more about WAR", target="_blank")
                         
                       ) ## dropdown
                ) ## column
              ), ## row

              br(),


               fluidRow(
                 
                 uiOutput('value')
                 
               ), ## row

               hr(),
              
               br()
  
          ), ## item

########################
###### SECOND TAB ######
########################

    tabItem(tabName = 'leaderboard',
            
            useWaiter(),
            
            h2('Leaderboard'),
            
            fluidRow(
              column(12, align = 'center',
                      selectInput('leader_org', 'Select a Team:',
                                  choices = c(savant2022_master_df$name %>% unique() %>% sort() ) ), ## selectinput
              ) ## col
            ), ## row
            
            hr(),
            
            h3('KPI Leaderboard'),
            
            br(),
            
            fluidRow(
              
              column(5, align = 'left', 
                     dropdownButton(
                       inputId = 'dropdown5',
                       label = 'How To View',
                       icon = icon('baseball-ball'),
                       circle = FALSE,
                       
                       HTML('<h4><b>What is this Table?:</b></h4>
                              <h5>This sortable table shows all <em>six</em> of the KPIs per player</h5>
                              <h5>Users can:</h5>
                              <ul><li><h5>Filter by team to see which players are considered "top performers" for any given team</h5></li></ul>
                                           ')
                       
                     ) ## dropdown
              ) ## column
            ), ## row
            
            br(),
            
            fluidRow(
              column(12, align = 'center',
                     reactableOutput('leaderboard')
                     ) ## col
            ), ## row
            
            hr(),
            
            h3('Stats Leaderboard'),
            
            br(),
            
            fluidRow(
              
              column(6, align = 'left', 
                     dropdownButton(
                       inputId = 'dropdown7',
                       label = 'Glossary',
                       icon = icon('baseball-ball'),
                       circle = FALSE,
                       
                       HTML("<h4><b>List of Stats:</b></h4>
                              <br>
                                <h5><b>PA: </b>Plate Appearances</b></h5>
                                <ul><li><h5>Number of times a hitter comes up to bat</h5></li></ul>
                                <h5><b>HR: </b>Homeruns</b></h5>
                                <h5><b>RBI: </b>Runs Batted In</b></h5>
                                <h5><b>SB: </b>Stolen Bases</b></h5>
                                <h5><b>K%: </b>Strikeout Percentage</b></h5>
                                <ul><li><h5>The rate in which a hitter strikes out</h5></li></ul>
                                <h5><b>BB%: </b>Walk Percentage</b></h5>
                                <ul><li><h5>The rate in which a hitter walks</h5></li></ul>
                                <h5><b>AVG: </b>Batting Average</b></h5>
                                <ul><li><h5>Probability of the batter getting a hit</h5></li></ul>
                                <h5><b>OBP: </b>On-Base Percentage</b></h5>
                                <ul><li><h5>Probability of the batter getting on-base</h5></li></ul>
                                <h5><b>SLG: </b>Slugging Percentage</b></h5>
                                <ul><li><h5>Probability of the batter hitting an extra-base hit</h5></li></ul>
                                <h5><b>wRC+: </b>Weighted Runs Created</b></h5>
                                <ul><li><h5>A statistical measurements that quantifies a player’s total offensive value and measure it by runs</h5></li></ul>
                                           "),
                       a(href="https://library.fangraphs.com/offense/wrc/", "Click here to read more about wRC+", target="_blank")
                       
                     ) ## dropdown
              ) ## column
            ), ## row
            
            br(),
            
            fluidRow(
              column(12, align = 'center',
                     reactableOutput('leaderboard2')
                     ) ## col
            ) ## row
            
            ) ### item

        ) ## items

    ), ## dashboardbody

    controlbar = dashboardControlbar(),
    title = 'Stat Track'

  ) ## page


##################
##### SERVER #####
##################

# Define server logic required to draw a histogram
server = function(input, output, session) {
  
  #### startup modal
  toggleModal(session, "startupModal", toggle = "open")
  # toggleModal(session, "infoModal", toggle = "toggle")
  

    mlbidz <- reactive({

      rbind(stattrack_df_22, stattrack_df) %>% filter(Name == input$player) %>% pull(mlbid) %>% unique()

    })

    observeEvent(input$org, {
      noids = savant2022_master %>% filter(batter %!in% c(609275, 606993, 680728) ) %>% pull(batter) %>% unique() ## no Mondesi, Hager
      choices <- rbind(stattrack_df, stattrack_df_22) %>% filter(mlbid %in% noids) %>% filter(name == input$org) %>% pull(Name) %>% unique() %>% sort()
      updateSelectInput(inputId = 'player', choices = choices)
    })
    
    ##### full statline 
    output$fullstats <- renderReactable({
      
      fullstats %>% filter(mlbid == mlbidz() ) %>% select(Name, PA, HR, BB_rate, K_rate, BABIP, `wRC+`, WAR) %>%
        reactable(
          theme = fivethirtyeight(centered = TRUE, header_font_size = 11),
          columns = list(
            BB_rate = colDef(
              name = 'BB%',
              format = colFormat(percent = TRUE, digits = 1)
            ), ## coldef
            K_rate = colDef(
              name = 'K%',
              format = colFormat(percent = TRUE, digits = 1)
            ) ## coldef
      ) ## columns
        ) ## reactable
        
    
      }) 
    
    ##### playerbio text ##### playerbio text ##### playerbio text 
    output$biotext <- renderText({
      
      df = playerbio %>% filter(mlbid == mlbidz() )
      
      namez = df %>% pull(name)
      dob = df %>% pull(dob)
      age = df %>% pull(age)
      # ht = df %>% pull(height)
      # wt = df %>% pull(weight)
      bats = df %>% pull(bats)
      throws = df %>% pull(throws)
      pos = df %>% pull(pos)
      draft = df %>% pull(r4year)
      
      paste0(
        '<font size=8><b>', namez, '</b></font>', 
        '<br>',
        '<h4><b>Age:</b> ', age, 
        ' | <b>Bats:</b> ', bats, 
        ' | <b>Throws:</b> ', throws,
        ' | <b>Position:</b> ', pos
      )
      
    })
    
    ###########
    ### box ###
    ###########
    
    output$bio <- renderUI({
    
      hexcol = projwar22 %>% left_join(orgmap_df, by = c('Org' = 'org') ) %>% filter(mlbid == mlbidz() ) %>% pull(Color) %>% unique()
      
      tags$div(class = "another-box", id = "bio",
               
               box(width = 6, title = "Player Bio", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                   column(12, align = 'center',
                                 uiOutput('actionshot'),
                                 br(),
                                 htmlOutput('biotext')
                   ) ## col
               ), ## box
               
               tags$style(HTML(paste0("
                        #bio .box.box-solid.box-primary>.box-header {
                        color:#fff;
                        background:", hexcol,
                        "}

                        .box.box-solid.box-primary {
                        border-bottom-color:", hexcol,
                        "border-left-color:", hexcol,
                        "border-right-color:", hexcol,
                        "border-top-color:", hexcol,
                        "}

                        ") ) )
      ) ## div
      
    })
    
    ###### DEPTH CHART VIZ ###### DEPTH CHART VIZ ###### DEPTH CHART VIZ ###### DEPTH CHART VIZ  ###### 
    
    output$depth_viz <- renderPlot({
      
      depth_chart_viz(input$org, mlbidz() )
      
    })
    
    ###########
    ### box ###
    ###########
    output$depth <- renderUI({

      hexcol = projwar22 %>% left_join(orgmap_df, by = c('Org' = 'org') ) %>% filter(mlbid == mlbidz() ) %>% pull(Color) %>% unique()
      
      tags$div(class = "another-box", id = "depth",
               
               box(width = 3, title = "Depth Chart", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                 plotOutput('depth_viz')
               ), ## box
               
               tags$style(HTML(paste0("
                        #depth .box.box-solid.box-primary>.box-header {
                        color:#fff;
                        background:", hexcol,
                        "}

                        .box.box-solid.box-primary {
                        border-bottom-color:", hexcol,
                        "border-left-color:", hexcol,
                        "border-right-color:", hexcol,
                        "border-top-color:", hexcol,
                        "}

                        ") ) )
      ) ## div
      
    })


    #### box coloring
    output$value <- renderUI({

      hexcol = projwar22 %>% left_join(orgmap_df, by = c('Org' = 'org') ) %>% filter(mlbid == mlbidz() ) %>% pull(Color) %>% unique()

      tags$div(class = "another-box", id = "value",
               box(width = 6, title = "Value by Team", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                   # column(12, align = 'center',
                       radioButtons('ros_pos', 'Select a Position:',
                                   choices = 'C', inline = TRUE),
                       br(),
                       plotlyOutput('ros_bar')
                          #        ) ## col
               ),
               box(width = 6, title = "Value by Player", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                       radioButtons('ros_pos_player', 'Select a Position:',
                                    choices = 'C', inline = TRUE),
                   # column(12, align = 'center',
                       br(),
                       plotlyOutput('ros_bar_player')
                   #        ) ## col
               ),
               tags$style(HTML(paste0("
                        #value .box.box-solid.box-primary>.box-header {
                        color:#fff;
                        background:", hexcol,
                        "}

                        .box.box-solid.box-primary {
                        border-bottom-color:", hexcol,
                        "border-left-color:", hexcol,
                        "border-right-color:", hexcol,
                        "border-top-color:", hexcol,
                        "}

                        ") ) )
      ) ### div

    })

####################
### gauge charts ###
####################
    output$swm_gauge <- renderPlot({
      gauge_charts_rates(mlbidz(), 'swm')
    })

    output$ch_gauge <- renderPlot({
      gauge_charts_rates(mlbidz(), 'ch')
    })

    output$sw_gauge <- renderPlot({
      gauge_charts_rates(mlbidz(), 'sw')
    })

    output$med_ev_gauge <- renderPlot({
      gauge_charts_rates(mlbidz(), 'med_ev')
    })

    output$max_ev_gauge <- renderPlot({
      gauge_charts_rates(mlbidz(), 'max_ev')
    })

    output$gb_r_gauge <- renderPlot({
      gauge_charts_rates(mlbidz(), 'gb_r')
    })

    ### 2022

    output$swm_gauge22 <- renderPlot({
      gauge_charts_rates_22(mlbidz(), 'swm')
    })

    output$ch_gauge22 <- renderPlot({
      gauge_charts_rates_22(mlbidz(), 'ch')
    })

    output$sw_gauge22 <- renderPlot({
      gauge_charts_rates_22(mlbidz(), 'sw')
    })

    output$med_ev_gauge22 <- renderPlot({
      gauge_charts_rates_22(mlbidz(), 'med_ev')
    })

    output$max_ev_gauge22 <- renderPlot({
      gauge_charts_rates_22(mlbidz(), 'max_ev')
    })

    output$gb_r_gauge22 <- renderPlot({
      gauge_charts_rates_22(mlbidz(), 'gb_r')
    })

    #######################
    ### gauge chart box ###
    #######################
    output$eval22 <- renderUI({

      hexcol = projwar22 %>% left_join(orgmap_df, by = c('Org' = 'org') ) %>% filter(mlbid == mlbidz() ) %>% pull(Color) %>% unique()

      tags$div(class = "another-box", id = "eval22",
               box(width = 12, title = "Eval 2025", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                   fluidRow(
                     column(12, align = 'left',
                     # h3('2022')
                     ) ## col
                   ), ## row
                   fluidRow(
                     column(2, align = 'center',
                            h4('SWM'),
                            plotOutput('swm_gauge22', height = '200px', width = '200px')
                     ), ## col
                     column(2, align = 'center',
                            h4('Chase'),
                            plotOutput('ch_gauge22', height = '200px', width = '200px')
                     ), ## col
                     column(2, align = 'center',
                            h4('Swing'),
                            plotOutput('sw_gauge22', height = '200px', width = '200px')
                     ), ## col
                     column(2, align = 'center',
                            h4('Median EV'),
                            plotOutput('med_ev_gauge22', height = '200px', width = '200px')
                     ), ## col
                     column(2, align = 'center',
                            h4('Max EV'),
                            plotOutput('max_ev_gauge22', height = '200px', width = '200px')
                     ), ## col
                     column(2, align = 'center',
                            h4('GB%'),
                            plotOutput('gb_r_gauge22', height = '200px', width = '200px')
                     ) ## col
                   ) ## row
               ), ## box
               tags$style(HTML(paste0("
                        #eval22 .box.box-solid.box-primary>.box-header {
                        color:#fff;
                        background:", hexcol,
                        "}

                        .box.box-solid.box-primary {
                        border-bottom-color:", hexcol,
                        "border-left-color:", hexcol,
                        "border-right-color:", hexcol,
                        "border-top-color:", hexcol,
                        "}

                        ") ) )
      ) ### div

    })

    ##### 2021 #####
    output$eval21 <- renderUI({

      hexcol = projwar22 %>% left_join(orgmap_df, by = c('Org' = 'org') ) %>% filter(mlbid == mlbidz() ) %>% pull(Color) %>% unique()

      tags$div(class = "another-box", id = "eval21",
               box(width = 12, title = "Eval 2024", status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                   fluidRow(
                     column(12, align = 'left'#,
                            # h3('2024')
                     ) ## col
                   ), ## row
                   fluidRow(
                     column(2, align = 'center',
                            h4('SWM'),
                            plotOutput('swm_gauge', height = '200px', width = '200px')
                     ), ## col
                     column(2, align = 'center',
                            h4('Chase'),
                            plotOutput('ch_gauge', height = '200px', width = '200px')
                     ), ## col
                     column(2, align = 'center',
                            h4('Swing'),
                            plotOutput('sw_gauge', height = '200px', width = '200px')
                     ), ## col
                     column(2, align = 'center',
                            h4('Median EV'),
                            plotOutput('med_ev_gauge', height = '200px', width = '200px')
                     ), ## col
                     column(2, align = 'center',
                            h4('Max EV'),
                            plotOutput('max_ev_gauge', height = '200px', width = '200px')
                     ), ## col
                     column(2, align = 'center',
                            h4('GB%'),
                            plotOutput('gb_r_gauge', height = '200px', width = '200px')
                     ) ## col
                   ) ## row
               ), ## box
               tags$style(HTML(paste0("
                        #eval21 .box.box-solid.box-primary>.box-header {
                        color:#fff;
                        background:", hexcol,
                        "}

                        .box.box-solid.box-primary {
                        border-bottom-color:", hexcol,
                        "border-left-color:", hexcol,
                        "border-right-color:", hexcol,
                        "border-top-color:", hexcol,
                        "}

                        ") ) )
      ) ### div

    })

###############
### ROS Bar ###
###############

    ### per org plot
    observeEvent(input$player, {

      choices = projwar22 %>% filter(mlbid == mlbidz() & fWAR > 0) %>% arrange(desc(fWAR) ) %>% pull(Position)
      updateRadioButtons(inputId = 'ros_pos',
                        choices = choices, inline = TRUE)

    })
    
    ### per player plot 
    observeEvent(input$player, {
      
      choices = projwar22 %>% filter(mlbid == mlbidz() & fWAR > 0) %>% arrange(desc(fWAR) ) %>% pull(Position)
      updateRadioButtons(inputId = 'ros_pos_player',
                         choices = choices, inline = TRUE)
      
    })

    ### by team 
    output$ros_bar <- renderPlotly({

      proj_rank_bar(input$ros_pos, mlbidz() )

    })
    
    ### per player 
    output$ros_bar_player <- renderPlotly({
      
      proj_rank_bar_player(input$ros_pos_player, mlbidz() )
      
    })

######################
### Player Profile ###
######################
    
    output$actionshot <- renderUI({

      mlbidz = stattrack_df %>% filter(Name == input$player) %>% pull(mlbid) %>% unique()
      src = paste0('https://img.mlbstatic.com/mlb-photos/image/upload/w_800,q_100/v1/people/', mlbidz, '/action/hero/current')

      tags$img(src = src,
                 height="90%", 
                 width="90%")
               # width = "700",
               # height = "220")
    })
    
    
    
####################
### radar charts ###
####################

    output$radar_vs_all <- renderPlotly({
      
      statcast_radar(mlbidz(), 'All')
    })

    output$radar_vs_R <- renderPlotly({
      
      statcast_radar(mlbidz(), 'R')
    })
    
    
    output$radar_vs_L <- renderPlotly({
      
      statcast_radar(mlbidz(), 'L')
    })
    
    
    output$radar22 <- renderUI({
      
      hexcol = projwar22 %>% left_join(orgmap_df, by = c('Org' = 'org') ) %>% filter(mlbid == mlbidz() ) %>% pull(Color) %>% unique()
      
      tags$div(class = "another-box", id = "radar22",
               box(width = 12, title = "Process by Handedness", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                   fluidRow(
                     column(12, align = 'left',
                            # h3('2022')
                     ) ## col
                   ), ## row
                   fluidRow(
                     column(4, aling = 'center',
                            h3('vs All'),
                            plotlyOutput('radar_vs_all')
                     ),
                     column(4, aling = 'center',
                            h3('vs RHP'),
                            plotlyOutput('radar_vs_R')
                     ), 
                     column(4, aling = 'center',
                            h3('vs LHP'),
                            plotlyOutput('radar_vs_L')
                     ) ## col
                   ) ## row
               ), ## box
               tags$style(HTML(paste0("
                        #radar22 .box.box-solid.box-primary>.box-header {
                        color:#fff;
                        background:", hexcol,
                        "}

                        .box.box-solid.box-primary {
                        border-bottom-color:", hexcol,
                        "border-left-color:", hexcol,
                        "border-right-color:", hexcol,
                        "border-top-color:", hexcol,
                        "}

                        ") ) )
      ) ### div
      
    })
    
### logos ### logos ### logos ### logos ### logos ### logos ### logos ### logos ### logos ###
    
    output$logo <- renderImage({
      filename <- normalizePath(file.path('./logos', paste0(input$org, '.png') ) )
      ## generate png
      list(src = filename, width = 125, height = 125 ) }, deleteFile = FALSE)

    
####### 2nd tab ####### 2nd tab ####### 2nd tab ####### 2nd tab ####### 2nd tab ####### 2nd tab  ####### 
    
    output$leaderboard <- renderReactable({
      
      # if(input$leader_org != 'All'){
        
        savant2022_master_df %>% filter(name == input$leader_org) %>% 
        filter(p_throws == 'All' & pitches >= 50) %>%
          select(batter, currentteam, logo, player_name, p_throws, pitches, swm, swm_n, ch, ch_n, sw, sw_n, med_ev, med_ev_n, max_ev, max_ev_n, gb_r, gb_r_n) %>%
          mutate(logo = paste0('https://d2p3bygnnzw9w3.cloudfront.net/req/202205232/tlogo/br/', logo, '.png'), .after = 'currentteam') %>%
          reactable(.,
                    theme = fivethirtyeight(centered = TRUE, header_font_size = 11),
                    # defaultSorted = "currentteam"
                    defaultPageSize = 30,
                    ##### columns ######
                    columns = list(
                      logo = colDef(
                        name = "",
                        maxWidth = 30,
                        # sortable = FALSE,
                        style = background_img()
                      ), ## coldef
                      currentteam = colDef(
                        show = FALSE
                      ),
                      batter = colDef(show = FALSE),
                      player_name = colDef(maxWidth = 300),
                      p_throws = colDef(name = 'vs', maxWidth = 40),
                      pitches = colDef(maxWidth = 70),
                      ##### percentage columns #####
                      swm = colDef(
                        # format = colFormat(percent = TRUE, digits = 1),
                        cell = data_bars(., text_size = 13, 
                                         # fill_color = viridis(5), 
                                         box_shadow = TRUE,
                                         text_position = 'center',
                                         number_fmt = scales::percent,
                                         force_outside = c(0,0.27), ## values from ntile
                                         fill_by = 'swm_n',
                                         fill_color = ntilecolz),
                        name = 'swm%'
                      ), ## coldef
                      ch = colDef(
                        cell = data_bars(., text_size = 13, 
                                         # fill_color = viridis(5), 
                                         box_shadow = TRUE,
                                         text_position = 'center',
                                         number_fmt = scales::percent,
                                         force_outside = c(0, 0.27),
                                         fill_by = 'ch_n',
                                         fill_color = ntilecolz),
                        name = 'ch%'
                      ), ## coldef
                      sw = colDef(
                        cell = data_bars(., text_size = 13, 
                                         # fill_color = viridis(5), 
                                         box_shadow = TRUE,
                                         text_position = 'center',
                                         number_fmt = scales::percent,
                                         force_outside = c(0, 0.27),
                                         fill_by = 'sw_n',
                                         fill_color = ntilecolz),
                        name = 'sw%'
                      ), ## coldef
                      med_ev = colDef(
                        cell = data_bars(., text_size = 13, 
                                         # fill_color = viridis(5), 
                                         box_shadow = TRUE,
                                         text_position = 'center',
                                         # force_outside = c(0, .2),
                                         fill_by = 'med_ev_n',
                                         fill_color = ntilecolz)
                      ),
                      max_ev = colDef(
                        cell = data_bars(., text_size = 13, 
                                         # fill_color = viridis(5), 
                                         box_shadow = TRUE,
                                         text_position = 'center',
                                         # force_outside = c(0, 0.27),
                                         fill_by = 'max_ev_n',
                                         fill_color = ntilecolz)
                      ), ## coldef
                      gb_r = colDef(
                        cell = data_bars(., text_size = 13, 
                                         # fill_color = viridis(5), 
                                         box_shadow = TRUE,
                                         text_position = 'center',
                                         number_fmt = scales::percent,
                                         # force_outside = c(0, 0.27),
                                         fill_by = 'gb_r_n',
                                         fill_color = ntilecolz),
                        name = 'gb%'
                      ), ## coldef
                      ##### hide cols ######
                      swm_n = colDef(show = FALSE),
                      ch_n = colDef(show = FALSE),
                      sw_n = colDef(show = FALSE),
                      med_ev_n = colDef(show = FALSE),
                      max_ev_n = colDef(show = FALSE),
                      gb_r_n = colDef(show = FALSE)
                      
                    ) ## columnDef
                    
          )
      
    }) ## render 
    
    
    output$leaderboard2 <- renderReactable((
      
      
      fullstats %>%  filter(parentOrgName == input$leader_org) %>% filter(PA > 10) %>% arrange(desc(WAR) ) %>%
        select(logo, Name, PA, HR, RBI, SB, BB_rate, K_rate, AVG, OBP, SLG, `wRC+`, WAR)  %>%
        mutate(logo = paste0('https://d2p3bygnnzw9w3.cloudfront.net/req/202205232/tlogo/br/', logo, '.png'), .after = 'Name') %>%
        mutate(AVG = str_sub(AVG, 2,5) , OBP = str_sub(OBP, 2,5), SLG = str_sub(SLG, 2,5) ) %>%
        reactable(., 
                  theme = fivethirtyeight(centered = TRUE, header_font_size = 11),
                  defaultPageSize = 30,
                  defaultColDef = colDef(align = "center" ),
                  columns = list(
                    logo = colDef(
                      name = "",
                      maxWidth = 30,
                      # sortable = FALSE,
                      style = background_img()
                    ), ## coldef)
                    BB_rate = colDef(
                      format = colFormat(percent = TRUE, digits = 1),
                      name = 'BB%'
                    ),
                    K_rate = colDef(
                      format = colFormat(percent = TRUE, digits = 1),
                      name = 'K%'
                    ) #, 
                    # AVG = colDef(
                    #   format = colFormat(digits = 5)
                    # )
                  )
        )
      
    ))
    
} ## server

# Run the application
shinyApp(ui = ui, server = server)

