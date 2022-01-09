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

df1 = read.csv("VideoGamesSales/reactives/vgsales.csv")

# Define UI for application that draws a histogram
shinyUI(dashboardPage(
  dashboardHeader(title = "Analysis on the Video Games Industry and It's Growth", 
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
              box(plotOutput("plot"), width = 8),
              box(
                selectInput("features", "Features:",
                            c("Name", "Platform", "Global_Sales", 
                              "NA_Sales", "JP_Sales", "EU_Sales", "Other_Sales")), width = 4)
      ),
      
      tabItem(tabName = "Consoles",
              fluidPage(
                h3("Type of Consoles")
              )),
      
      tabItem(tabName = "Dota2",
              fluidPage(
                h3("Dota 2 Journey"),
                h4("How did Dota start and where it is now.")
              )),
      
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
                             ),
                           )
                           )))
      
    )
  )
)
)
