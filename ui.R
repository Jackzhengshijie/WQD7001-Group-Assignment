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
ui = fluidPage(

    # Application title
    titlePanel("Analysis on the Video Games Industry and It's Growth"),
    
    # Dota 2 storyline
    tabsetPanel(
      tabPanel(
        title = "dota 2",
        sidebarLayout(
          sidebarPanel(
            selectInput(
              inputId = "dota2input",
              label = "Game",
              choices = c("All", "Dota 2", "Fortnite", "League of Legends", "Arena Of Valor", "PLAYERUNKNOWN'S BATTLEGROUNDS", 'Overwatch', 'SMITE', 'Rainbow Six Siege', 'Call of Duty: Modern Warfare', "PLAYERUNKNOWN'S BATTLEGROUNDS Mobile", 'Counter-Strike: Global Offensive')
            ),
          ),
          mainPanel(
            title = 'Dota 2', fluid= TRUE, DT::dataTableOutput('table1'),
            plotOutput('barplot'),
            plotOutput('lineplot')
          ),
        )
      ),
      
      # Video Games story line
      tabPanel(
        title = 'video games sales',
        sidebarLayout(
          sidebarPanel(
            selectInput(
              inputId = "vgsalesinput",
              label = 'Platform',
              choices = c('All', 'Wii', 'WiiU', 'NES', 'GB', 'DS', 'X360', 'PS2', 'PS3', 'PS4', 'GBA', 'SNES', '3DS')
            ),
            selectInput(
              inputId = 'vginput2',
              label = 'Genre',
              choices = c('All', 'Action', 'Adventure', 'Fighting', 'Misc', 'Platform', 'Puzzle', 'Racing', 'Role-Playing', 'Shooter', 'Simulation', 'Sports', 'Strategy')
            )
            
          ),
          
          mainPanel(
            titles = "video games sales", DT::dataTableOutput('tablevgsales'),
            plotOutput('GlobalSales'),
            plotOutput('genre')
          )
        )
    ),
    
    
    # 

    
    
)
)
