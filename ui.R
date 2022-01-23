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
      menuItem("How to use", tabName = 'howto'),
      menuItem("Overview", tabName = "Overview"),
      menuItem("Consoles", tabName = "Consoles"),
      menuItem("Teams and Countries", tabName = 'TeamsAndCountries'),
      menuItem("Players", tabName = "Players"),
      menuItem("Games", tabName = "Games")
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "howto",
              navbarPage("How to use this app",
                         tabPanel(
                           'Introduction',
                           fluidPage(
                             mainPanel(
                               h2('Welcome!'),
                               p('1. Welcome to our app! Here, we will explore the video games industry and what it has offered to our fellow professional esports athletes and to the community of gamers worldwide.'),
                               p('2. Refer to the tabs to the right on how to operate our app.'),
                               p('3. Use the sidebar panels on the left to maneuver across the different aspects of this app.'),
                               p('4. We hope this will give valuable insights to our users. Enjoy!'),
                             )
                           )
                         ),
                         tabPanel(
                           'Overview',
                           fluidPage(
                             mainPanel(
                               h2('How to use the Overview menu'),
                               p('1. The', strong('overview'), ' dashboard explores the value of sales globally and also by region: North America (NA), Europe (EU), Japan (JP), and Others. It also has critic score and count, user score and count, name of developers and ratings.'),
                               p('2. There are two drop-down menus that you can filter based on what you are looking for: Platform and Genre.'),
                               p('3. The table will filter accordingly based on your selection. The dashboard below will also filter accordingly.'),
                               p('4. The first chart is a vertical bar graph that plots the genre against Global Sales. This chart', strong('only reacts to the Platform filter.')),
                               p('5. The second chart is a vertical bar graph that plots the platform against Global Sales. This chart',  strong('only reacts to the Genre filter.')),
                               p('6. You can use the search bar at the top right of the table to search specifically what you are looking for within the table, assuming that it is in the table.')
                             )
                           )
                         ),
                         tabPanel(
                           'Consoles',
                           fluidPage(
                             mainPanel(
                               h2('How to use the Consoles menu'),
                               p('1. There are four drop-down menus that you can use to filter the table below: Console, Game, Genre, and Publisher.'),
                               p('2. It is recommended to filter the type of Console you want to see first, either PS4 or XBox.'),
                               p('3. The table will filter accordingly to the selections that you have given.')
                             )
                           )
                         ),
                         tabPanel(
                           'Teams and Countries',
                           fluidPage(
                             mainPanel(
                               h2('How to use the Teams and Countries menu'),
                               p('1. This dashboard has 3 tabs: Team, Countries and Plots for Countries.'),
                               p('2. In the', strong('Teams'), (' tab, two drop-down menus are available that will filter the table below accordingly.')),
                               p('3. The first chart is a horizontal bar graph that plots the Total Winnings of a team against the Team Name. This chart', strong('only reacts to the Year filter.')),
                               p('4. You can see the top 10 teams who earns the highest overall or in a specific year if the Year menu is filtered.'),
                               p('5. The second chart is a horizontal bar graph that plots the Winnings against the Year. This chart',  strong('only reacts to the Team filter.')),
                               p('6. You can see how much a team earns through the years or in a single year if the Team menu is filtered.'),
                               p('7. In the', strong('Countries'), (' tab, two drop-down menus are available that will filter the table below it accordingly.')),
                               p('8. The mini dashboard below the table gives you a summarized view of the table data.'),
                               p('9. You can use the search bar at the top right of the table to search specifically what you are looking for within the table, assuming that it is in the table.'),
                               p('10. Lastly, in the', strong('Plots for Countries'), (' tab, there are 3 charts, which shows the how much total prize money is involved each year, the number of players that have participated in tournaments, and the top 11 countries ranked by how much they have earned.'))
                             )
                           )
                         ),
                         tabPanel(
                           'Players',
                           fluidPage(
                             mainPanel(
                               h2('How to use the Players menu'),
                               p('1. There are two tabs available: EDA and Summary. '),
                               p('2. In the', strong('EDA '), ('tab, there is one drop-down menu indicating the year you wish to filter.')),
                               p('3. The table will filter according to the year selected and you will be able to see top players by their total and overall earnings.'),
                               p('4. The three charts below the table'), strong('DO NOT'), ('react to the Year filter. These are static charts that explain the evolution of prize money, the distribution, and the frequency of number of players in tournaments.'),
                               p('5. In the', strong('Summary'), ('tab, there is one drop-down menu indicating the year you wish to filter.')),
                               p('6. The Year selected will filter the three mini dashboards and the bar chart below it.')
                             )
                           )
                         ),
                         tabPanel(
                           'Games',
                           fluidPage(
                             mainPanel(
                               h2('How to use the Games menu'),
                               p('1. This dashboard only has 1 filter, Game. This will filter the type of game you want to see and all the tournaments that this game has organized or hosted during the respective year.'),
                               p('2. If a filter is selected, the table, mini dashboard, and two charts below will react accordingly.'),
                               p('3. The first chart shows, in descending order, the top tournaments by Prize Pool filtered by game if filter is applied.'),
                               p('4. The second chart shows how much one player earns on average each year.'),
                               p('5. You can use the search at the top right of the table to search specifically what you are looking for within the table, assuming that it is in the table.')
                             )
                           )
                         )
              )
              ),
              tabItem(tabName = "Overview",
                      navbarPage(title = "Video Games Sales by Region",
                                 tabPanel(
                                   title = "Sales",
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
                                       wellPanel(
                                         tags$table(
                                           id = 'overviewplot', style = 'width: 100%',
                                           tags$td(
                                             h2(textOutput("totalgames")),
                                             h4("Total Number of Games")
                                           ),
                                           tags$td(
                                             h2(textOutput('sumofglobalsales')),
                                             h4("Total Global Sales, in millions")
                                           ),
                                           tags$td(
                                             h2(textOutput("totalplatform")),
                                             h4("Total Number of Platforms")
                                           ),
                                           tags$td(
                                             h2(textOutput("totalpublisher")),
                                             h4("Total Number of Publishers")
                                           ),
                                           tags$td(
                                             h2(textOutput("totalgenre")),
                                             h4("Types of Genre")
                                           )
                                         )
                                       ),
                                       splitLayout(
                                         plotOutput('GlobalSales'),
                                         plotOutput('genre')
                                       )
                                       )
                                     )
                           ),
                         )
              ),
              tabItem(tabName = "Games",
                      navbarPage("Tournaments and Prize Pools",
                                 tabPanel(
                                   "Games",
                                   fluidPage(theme = 'custom.css',
                                     selectInput(
                                      inputId = "dota2input",
                                      label = "Game",
                                      choices = c('All', unique(dota2data$Game))
                                      ),
                                     mainPanel(
                                      title = 'Dota2', fluid = TRUE, DT::dataTableOutput('table1'),
                                      wellPanel(
                                        tags$div(
                                          id = 'plotgames', style = 'width: 100%',
                                          tags$table(
                                            id = 'plotgames2',
                                            tags$td(
                                              column(3, offset = 3,
                                                     h2(textOutput("highestprizeforteam")),
                                                     h4("Highest Prize for a team")
                                              )
                                              ),
                                            tags$td(
                                              column(3, offset = 3,
                                                     h2(textOutput("highestprizeforplayer")),
                                                     h4("Highest Prize for a player")
                                              )
                                            ),
                                            tags$td(
                                              column(3, offset = 3,
                                                     h2(textOutput("averageprizeforteam")),
                                                     h4("Average Prize for a team")
                                              )
                                            ),
                                            tags$td(
                                              column(3, offset = 3,
                                                     h2(textOutput("averageprizeforplayer")),
                                                     h4("Average Prize for a player")
                                              )
                                            )
                                          )
                                        )

                                      ),
                                      plotOutput('barplot'),
                                      plotOutput('lineplot')
                                
                                    )
                            )
                            )
                          )
                ),
      tabItem(tabName = 'Consoles',
              navbarPage('Video Game sales by Consoles',
                         tabPanel(
                           'Consoles',
                           fluidPage(
                             column(3,
                                    selectInput("console",
                                                "Console:",
                                                c("All",
                                                  unique(as.character(GS_DF$console))))
                             ),
                             column(3,
                                    selectInput("genre",
                                                "Genre:",
                                                c("All",
                                                  unique(as.character(GS_DF$genre))))
                             ),
                             column(3,
                                    selectInput("publisher",
                                                "Publisher:",
                                                c("All",
                                                  unique(as.character(GS_DF$publisher))))
                             ),
                             column(3,
                                    selectInput("game",
                                                "Game:",
                                                c("All",
                                                  unique(as.character(GS_DF$game))))
                             )
                           ),
                           mainPanel(
                             title = "Consoles", fluid = TRUE, DT::dataTableOutput("table"),
                             wellPanel(
                               tags$div(id = 'plotconsole', style = 'width: 100%',
                                        tags$table(
                                          id = 'plotconsole2',
                                          tags$td(
                                            column(3, offset = 3,
                                                   h3(textOutput("ps4games")),
                                                   h4("Total Number of Games on PS4")
                                            )
                                          ),
                                          tags$td(
                                            column(3, offset = 3,
                                                   h3(textOutput("xboxgames")),
                                                   h4("Total Number of Games on XBox")
                                            )
                                          ),
                                          tags$td(
                                            column(3, offset = 3,
                                                   h3(textOutput("consoleglobalsales")),
                                                   h4("Total Global Sales, in millions")
                                            )
                                          )
                                        )
                               )
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
                               splitLayout(
                                 plotOutput('barteam'),
                                 plotOutput('barplot2')
                               )
                             )
                             )
                          ),
                         tabPanel(
                           "Countries",
                           fluidPage(theme = 'custom.css',
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
                               wellPanel(
                                 tags$div(id = 'plotteam', style = 'width: 100%',
                                        tags$table(
                                          id = 'plotteam2',
                                          tags$td(
                                            column(3, offset = 3,
                                                   h3(textOutput("totalPlayer2")),
                                                   h4("Total Number of Players")
                                          )
                                          ),
                                          tags$td(
                                            column(3, offset = 3,
                                                   h3(textOutput("totalCountry")),
                                                   h4("Total Countries Participated")
                                            )
                                          ),
                                          tags$td(
                                            column(3, offset = 3,
                                                   h3(textOutput("totalPrizeMoney2")),
                                                   h4("Total Prize Money by Country")
                                            )
                                          )
                                        )
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
                                         choices = c('2020', '2019', '2018', '2017', '2016', '2015', '2014', '2013', '2012', '2011', '2010', '2009', '2008', '2007', '2006', '2005', '2004', '2003', '2002', '2001', '2000', '1999', '1998')
                                         ),
                             mainPanel(
                               title = "Players", fluid = TRUE, DT::dataTableOutput("earnings_table"),
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
                                      )
                             )
                             )
                           ),
                         tabPanel(
                           "Summary",
                           fluidPage(
                             theme = "custom.css",
                             selectInput("selected_year",
                                         label = ("Select a Year"),
                                         choices = c("All", '2020', '2019', '2018', '2017', '2016', '2015', '2014', '2013', '2012', '2011', '2010', '2009', '2008', '2007', '2006', '2005', '2004', '2003', '2002', '2001', '2000', '1999', '1998')
                                         ),
                             wellPanel(
                               tags$table(
                                 id = 'tableDashboard',
                                 tags$td(
                                   h3(textOutput("totalPlayer")),
                                   h4("Total Players"),
                                 ),
                                 tags$td(
                                   h3(textOutput("totalPrizeMoney")),
                                   h4("Total Prize Money"),
                                 ),
                                 tags$td(
                                   h3(textOutput("overallPrizeMoney")),
                                   h4("Overall Prize Money (Cumulative)")
                                 )
                               )
                           ),
                           plotOutput('topPlayers')
                           )
                           )
                         )
              )
    )
  )
)
)

