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

df1 = read.csv("C:\\Users\\KSAR\\Downloads\\WQD7001-Group-Assignment-main (1)\\WQD7001-Group-Assignment-main\\vgsales.csv")

# Define UI for application that draws a histogram
shinyUI(dashboardPage(
  dashboardHeader(title = "Analysis on the Video Games Industry and How It Grew", 
                  titleWidth = 550),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "Overview"),
      menuItem("Consoles", tabName = "Consoles"),
      menuItem("Dota2", tabName = "Dota2"),
      menuItem("Players", tabName = "Players")
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
                               plotOutput('GlobalSales'),
                               plotOutput('genre')
                             )
                             )
                           )
                         )
              ),
      
      
      tabItem(tabName = "Consoles",
              fluidPage(
                h3("Type of Consoles")
              )),
      
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
                              title = 'Dota 2', fluid = TRUE, DT::dataTableOutput('table1'),
                              plotOutput('barplot'),
                              plotOutput('lineplot')
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

