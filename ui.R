#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(ggplot2)

# Define UI for application
shinyUI(dashboardPage(
  dashboardHeader(title = "Analysis on the Video Games Industry and How It Grew", 
                  titleWidth = 550),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "Overview"),
      menuItem("Teams and Countries", tabName = 'TeamsAndCountries'),
      menuItem("Players", tabName = "Players"),
      menuItem("Dota2", tabName = "Dota2")
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "Overview",
              navbarPage("Video Games Sales by Region",
                         tabPanel(
                           "Sales",
                           fluidPage(
                             selectInput(
                               inputId = "vgsalesinput",
                               label = 'Platform',
                               choices = c('All', unique(vgs$Platform))
                             ),
                             selectInput(
                               inputId = 'vginput2',
                               label = 'Genre',
                               choices = c('All', 'Platform', 'Sports', 'Racing', 'Adventure', 'Role-Playing', 'Puzzle', 'Misc', 'Shooter', 'Action', 'Strategy', 'Fighting', 'Simulation')
                             ),
                             mainPanel(
                               title = "Video Games Sales", fluid = TRUE, DT::dataTableOutput('tablevgsales'),
                               theme = 'custom.css',
                               wellPanel(
                                 tags$table(
                                   id = "overalldashboard", style = 'width: 100%',
                                   tags$td(
                                     column(3, h3(textOutput("totalgames")),
                                            h4("Total Number of Games"))),
                                   
                                   tags$td(
                                     column(3, offset = 3, h3(textOutput("totalplatform")),

                                            h4("Total Number of Platforms"))),
                                   tags$td(
                                     column(3, offset = 3, h3(textOutput("totalpublisher")),
                                            h4("Total Number of Publishers"))),
                                   tags$td(
                                     column(3, offset = 3, h3(textOutput("totalgenre")),
                                            h4("Types of Genre")))
                                 )
                               ),
                               plotOutput('GlobalSales'),
                               plotOutput('genre')
                             )
                             )
                           ),
                         )
              ),
      tabItem(tabName = "Dota2",
              navbarPage("Tournaments and Prize Pools",
                         tabPanel(
                           "Games",
                           fluidPage(
                             selectInput(
                               inputId = "dota2input",
                               label = "Game",
                               choices = c('All', unique(dota2data$Game))
                            ),
                            mainPanel(
                              title = 'Dota2', fluid = TRUE, DT::dataTableOutput('table1'),
                              theme = 'custom.css',
                              wellPanel(
                                tags$table(
                                  id = "anotherdashboard", style = 'width: 100%',
                                  tags$td(
                                    column(3, h3(textOutput("highestprizeforteam")),
                                           h4("Highest Prize for a team"))),
                                  
                                  tags$td(
                                    column(3, offset = 3, h3(textOutput("highestprizeforplayer")),
                                           
                                           h4("Highest Prize for a player"))),
                                  tags$td(
                                    column(3, offset = 5, h3(textOutput("averageprizeforteam")),
                                           h4("Average Prize for a team"))),
                                  tags$td(
                                    column(3, offset = 5, h3(textOutput("averageprizeforplayer")),
                                           h4("Average Prize for a player")))
                                )
                              ),
                              plotOutput('barplot'),
                              plotOutput('lineplot')
                            )
                            )
                          )
                )
        ),
      tabItem(tabName = "TeamsAndCountries",
              navbarPage("Teams and Countries",
                         tabPanel(
                           "Team",
                           fluidPage(
                             selectInput(
                               inputId = 'selectYear',
                               label = 'Year',
                               choices = c('All', unique(topteams$Year))
                             ),
                             selectInput(
                               inputId = 'teamInput',
                               label = 'Team',
                               choices = c('All', 'Alliance', 'Atlanta Faze', 'Altiora', 'Cloud9', 'Dallas Empire', 'ENCE eSports', 'Evil Geniuses', 'Fnatic', 'Four Angry Men', 'G2 Esports', 'Gambit Esports', 'Natus Vincere', 'Nigma', 'Ninjas in Pyjamas', 'Nova eSports', 'OG', 'Paris Saint-Germain Esports', 'Royal Never Give Up', 'San Franciso Shock', 'Sentinels', 'Spacestation Gaming', 'T1', 'Team Liquid', 'Team Secret', 'Team SoloMid', 'Top Esports', 'Turnso Gaming', 'Valhalla Vikings', 'Virtus.Pro', 'Vitality')
                             ),
                             mainPanel(
                               title = 'Teams', fluid = TRUE, DT::dataTableOutput('teamtable'),
                               plotOutput('barteam'),
                               plotOutput('barplot2')
                             )
                             )
                          ),
                         tabPanel(
                           "Countries",
                           fluidPage(
                             selectInput(
                               inputId = 'selectyearcountry',
                               label = 'Year',
                               choices = c('All', unique(topcountries$Year))
                             ),
                             selectInput(
                               inputId = 'countryInput',
                               label = 'Country',
                               choices = c('All', unique(topcountries$Country))
                             ),
                             mainPanel(
                               title = 'Countries', fluid = TRUE, DT::dataTableOutput('countrytable'),
                               theme = "custom.css",
                               wellPanel(
                                 tags$table(
                                   id = "tableDashboard", 
                                   tags$td(
                                     column(3, h3(textOutput("totalPlayer2")),
                                            h4("Total Number of Players"))),
                                   
                                   tags$td(
                                     column(3, offset = 3, h3(textOutput("totalCountry")),
                                            h4("Total Countries Participated"))),
                                   
                                   tags$td(
                                     column(3, offset = 5, h3(textOutput("totalPrizeMoney2")),
                                            h4("Total Prize Money by Country"))),
                                 )
                               )
                             )
                           )
                         ),
                         tabPanel(
                           "Plots for Countries",
                           fluidPage(
                             mainPanel(
                               title = 'Plot for Countries',
                               plotOutput('histotpm'),
                               plotOutput('histoplayer'),
                               plotOutput('barcountry')
                             )
                               
                             )
                           )
                         )
                
        
      ),
      tabItem(tabName = "Players",
              navbarPage("Player's Earnings",
                         tabPanel(
                           "EDA",
                           fluidPage(
                             selectInput("showYear",
                                         label = ("Select a Year"),
                                         choices = unique(esport_earnings_players$Year)),
                             DTOutput("earnings_table"),
                             tags$div(id = "plot", style = "width: 100%",
                                      tags$table(
                                        id = "ggPlot", 
                                        tags$td(
                                          h4("Average Prize Money From 1998-2020"),
                                          plotOutput("averagePrizeMoney")
                                        ),
                                        tags$td(
                                          h4("Prize Money Distribution"),
                                          plotOutput("prizeDistribution")
                                        ),
                                        tags$td(
                                          h4("Total Player Particapted"),
                                          plotOutput("totalPlayers")
                                        )
                                        ),
                                      ),
                             )
                           ),
                         tabPanel(
                           "Summary",
                           fluidPage(
                             theme = "custom.css",
                             selectInput("selected_year",
                                         label = ("Select a Year"),
                                         choices = c("All", unique(esport_earnings_players$Year))
                                         ),
                             wellPanel(
                               tags$table(
                                 id = "tableDashboard",
                                 tags$td(
                                   h3(textOutput("totalPlayer")),
                                   h4("Total Players")),
                                 tags$td(
                                   h3(textOutput("totalPrizeMoney")),
                                   h4("Total Prize Money")),
                                 tags$td(
                                   h3(textOutput("overallPrizeMoney")),
                                   h4("Overall Prize Money"))
                                 )
                           )
                           )
                           )
                         )
              )
    )
      
    )
  )
)

