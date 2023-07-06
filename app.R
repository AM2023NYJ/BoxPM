library(nflreadr)
library(ggplot2)
library(ggimage)
library(tidyverse)
library(ggrepel)
library(dplyr)
library(gt)
library(paletteer)
library(webshot)
library(ggthemes)
library(readr)
library(ggtext)
library(ggforce)
library(stats)
library(mclust)
library(mdthemes)
library(gghighlight)
library(na.tools)
library(stringr)
library(magick)
library(ggbeeswarm)
library(vip)
library(gtExtras)
library(nflfastR)
library(shiny)
library(reactable)
library(reactablefmtr)
library(shinyWidgets)
library(shinythemes)
library(bslib)

Offense <- read_csv("https://raw.githubusercontent.com/AM2023NYJ/BoxPM/main/OffensePlusMinus.csv")
Defense <- read_csv("https://raw.githubusercontent.com/AM2023NYJ/BoxPM/main/DefensePlusMinus.csv")
Offense$team <- clean_team_abbrs(Offense$team)
Defense$team <- clean_team_abbrs(Defense$team)

logos <- nflfastR::teams_colors_logos |> select(team_abbr, team_logo_espn)

Offense <- left_join(Offense, logos, by = c('team' = 'team_abbr'))
Defense <- left_join(Defense, logos, by = c('team' = 'team_abbr'))



# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bs_theme(bootswatch = "simplex", base_font = font_google(family = "Exo 2", display = "block"), font_scale = 1.2,
                   bg = "white", fg = "black"),

    # Application title
    titlePanel("Jets Plus Minus"),


        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
          tabPanel("Offense",
                   fluidRow(column(3, align = 'center', selectInput(inputId = "Season", label = "Season:",
                                              choices = c(2015, 2016, 2017, 2018, 2019, 2020, 2021,
                                                          2022),
                                              selected = 2022)),
                                  column(5, align = 'center',
                                         selectInput(
                                    inputId =  "Position",
                                    label = "Position:",
                                    choices = c('RB', 'WR', 'TE',
                                                'OT', 'OG', 'C'),
                                    selected = 'WR'
                                  )),
                                  column(7, align = 'center', pickerInput("teaminput","Team:", choices=c("ARI", 'ATL', 'BAL', 'BUF',
                                                                             'CAR', 'CHI', 'CIN', 'CLE', 
                                                                             'DAL', 'DEN', 'DET', 'GB',
                                                                             'HOU', 'IND', 'JAX', 'KC',
                                                                             'LV', 'LAC', 'LA', 'MIA',
                                                                             'MIN', 'NE', 'NO', 'NYG',
                                                                             'NYJ', 'PHI', 'PIT', 'SF',
                                                                             'SEA', 'TB', 'TEN', 'WAS'), 
                                              options = list(`actions-box` = TRUE),multiple = T,
                                              selected = c("ARI", 'ATL', 'BAL', 'BUF',
                                                           'CAR', 'CHI', 'CIN', 'CLE', 
                                                           'DAL', 'DEN', 'DET', 'GB',
                                                           'HOU', 'IND', 'JAX', 'KC',
                                                           'LV', 'LAC', 'LA', 'MIA',
                                                           'MIN', 'NE', 'NO', 'NYG',
                                                           'NYJ', 'PHI', 'PIT', 'SF',
                                                           'SEA', 'TB', 'TEN', 'WAS')))),
                     mainPanel(
                       reactableOutput("Offense_table"), width = 12
                     )),
          tabPanel("Defense",
                   fluidRow(column(3, align = 'center', selectInput(inputId = "SeasonD", label = "Season:",
                                                                    choices = c(2015, 2016, 2017, 2018, 2019, 2020, 2021,
                                                                                2022),
                                                                    selected = 2022)),
                            column(5, align = 'center',
                                   selectInput(
                                     inputId =  "PositionD",
                                     label = "Position:",
                                     choices = c('IDL', 'EDGE', 'LB',
                                                 'CB', 'S'),
                                     selected = 'EDGE'
                                   )),
                            column(7, align = 'center', pickerInput("teaminputD","Team:", choices=c("ARI", 'ATL', 'BAL', 'BUF',
                                                                                                   'CAR', 'CHI', 'CIN', 'CLE', 
                                                                                                   'DAL', 'DEN', 'DET', 'GB',
                                                                                                   'HOU', 'IND', 'JAX', 'KC',
                                                                                                   'LV', 'LAC', 'LA', 'MIA',
                                                                                                   'MIN', 'NE', 'NO', 'NYG',
                                                                                                   'NYJ', 'PHI', 'PIT', 'SF',
                                                                                                   'SEA', 'TB', 'TEN', 'WAS'), 
                                                                    options = list(`actions-box` = TRUE),multiple = T,
                                                                    selected = c("ARI", 'ATL', 'BAL', 'BUF',
                                                                                 'CAR', 'CHI', 'CIN', 'CLE', 
                                                                                 'DAL', 'DEN', 'DET', 'GB',
                                                                                 'HOU', 'IND', 'JAX', 'KC',
                                                                                 'LV', 'LAC', 'LA', 'MIA',
                                                                                 'MIN', 'NE', 'NO', 'NYG',
                                                                                 'NYJ', 'PHI', 'PIT', 'SF',
                                                                                 'SEA', 'TB', 'TEN', 'WAS')))),
                   mainPanel(
                     reactableOutput("Defense_table"), width = 12
                   ))
        )
        )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  offense_data <- reactive({
    Offense %>%
      filter(Season == input$Season) %>%
      select(pff_PLAYERNAME, team_logo_espn, TotalBoxPM, TotalEPAR, TotalPassBoxPM, TotalRunBoxPM, Position, team) |> 
      group_by(Position) |> 
      mutate(
        `Total Rk` = rank(-TotalBoxPM),
        `Total Pass Rk` = rank(-TotalPassBoxPM),
        `Total Run Rk` = rank(-TotalRunBoxPM)
      ) |> ungroup() |>  filter(team %in% input$teaminput, Position == input$Position) |> select(-Position, -team) |> 
      select(pff_PLAYERNAME, team_logo_espn, TotalEPAR, TotalBoxPM, `Total Rk`, 
                  TotalPassBoxPM, `Total Pass Rk`, TotalRunBoxPM, `Total Run Rk`) |> 
      mutate(
        TotalBoxPM = round(TotalBoxPM, 1),
        TotalEPAR = round(TotalEPAR, 1),
        TotalPassBoxPM = round(TotalPassBoxPM, 1),
        TotalRunBoxPM = round(TotalRunBoxPM, 1)
      )
  })
  
  output$Offense_table <- renderReactable({
    off_tbl <- offense_data()
    
   
    reactable(off_tbl,
              compact = FALSE,
              pagination = TRUE,
              columns = list(
                pff_PLAYERNAME = colDef(name = "Player",
                              maxWidth = 100,
                              align = "left", sticky = 'left'),
                team_logo_espn = colDef(name = "Team",
                                        maxWidth = 60,
                                        cell = embed_img(),
                                        align = "center", sticky = 'left'),
                `Total Rk` = colDef(name = "Total Rank", maxWidth = 90, align = "center",
                                   cell = color_tiles(off_tbl, colors = c("#59a14f", "#8cd17d", "white" ,"#ff9d9a", "#e15759"))),
                `Total Pass Rk` = colDef(name = "Total Pass Rank", maxWidth = 90, align = "center",
                                    cell = color_tiles(off_tbl, colors = c("#59a14f", "#8cd17d", "white" ,"#ff9d9a", "#e15759"))),
                `Total Run Rk` = colDef(name = "Total Run Rank", maxWidth = 90, align = "center",
                                    cell = color_tiles(off_tbl, colors = c("#59a14f", "#8cd17d", "white" ,"#ff9d9a", "#e15759"))),
                TotalBoxPM = colDef(name = 'Total Box PM', maxWidth = 90),
                TotalPassBoxPM = colDef(name = 'Total Pass Box PM', maxWidth = 90),
                TotalRunBoxPM = colDef(name = 'Total Run Box PM', maxWidth = 90),
                TotalEPAR = colDef(name = 'Total EPAR', maxWidth = 90)
              ), fullWidth = TRUE, defaultPageSize = 15)
  })
  
  defense_data <- reactive({
    Defense %>%
      filter(Season == input$SeasonD) %>%
      select(pff_PLAYERNAME, team_logo_espn, TotalBoxPM, TotalEPAR, TotalPassBoxPM, TotalRunDBoxPM, Position, team) |> 
      group_by(Position) |> 
      mutate(
        `Total Rk` = rank(TotalBoxPM),
        `Total Pass Rk` = rank(TotalPassBoxPM),
        `Total Run Rk` = rank(TotalRunDBoxPM)
      ) |> ungroup() |> 
      filter(team %in% input$teaminputD, Position == input$PositionD) |> select(-Position, -team) |> 
      select(pff_PLAYERNAME, team_logo_espn, TotalEPAR, TotalBoxPM, `Total Rk`,
             TotalPassBoxPM, `Total Pass Rk`, TotalRunDBoxPM, `Total Run Rk`) |> 
      mutate(
        TotalBoxPM = round(TotalBoxPM, 1),
        TotalEPAR = round(TotalEPAR, 1),
        TotalPassBoxPM = round(TotalPassBoxPM, 1),
        TotalRunDBoxPM = round(TotalRunDBoxPM, 1)
      ) 
  })
  
  output$Defense_table <- renderReactable({
    def_tbl <- defense_data()
    
    
    reactable(def_tbl,
              compact = FALSE,
              pagination = TRUE,
              columns = list(
                pff_PLAYERNAME = colDef(name = "Player",
                                        maxWidth = 100,
                                        align = "left", sticky = 'left'),
                team_logo_espn = colDef(name = "Team",
                                        maxWidth = 60,
                                        cell = embed_img(),
                                        align = "center"),
                `Total Rk` = colDef(name = "Total Rank", maxWidth = 90, align = "center",
                                    cell = color_tiles(def_tbl, colors = c("#59a14f", "#8cd17d", "white" ,"#ff9d9a", "#e15759"))),
                `Total Pass Rk` = colDef(name = "Total Pass Rank", maxWidth = 90, align = "center",
                                         cell = color_tiles(def_tbl, colors = c("#59a14f", "#8cd17d", "white" ,"#ff9d9a", "#e15759"))),
                `Total Run Rk` = colDef(name = "Total Run Rank", maxWidth = 90, align = "center",
                                        cell = color_tiles(def_tbl, colors = c("#59a14f", "#8cd17d", "white" ,"#ff9d9a", "#e15759"))),
                TotalBoxPM = colDef(name = 'Total Box PM', maxWidth = 90),
                TotalPassBoxPM = colDef(name = 'Total Pass Box PM', maxWidth = 90),
                TotalRunDBoxPM = colDef(name = 'Total RunD Box PM', maxWidth = 90),
                TotalEPAR = colDef(name = 'Total EPAR', maxWidth = 90)
              ), fullWidth = TRUE, defaultPageSize = 15)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
