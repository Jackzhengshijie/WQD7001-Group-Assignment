#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)

# Our dataset
df1


# Define UI for application
ui <- dashboardPage(
  dashboardHeader(title = "Analysis on the Video Games Industry and It's Growth", 
                  titleWidth = 550),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "Overview"),
      menuItem("Consoles", tabName = "Consoles"),
      menuItem("Dota2", tabName = "Dota2")
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
              )
      )
      )
    )
  )

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$plot = renderPlot({
    plot(df1$Global_Sales, df1[[input$features]],
         xlab = "Component",
         ylab = "Global Sales, in millions USD")
  })

    
}

# Run the application 
shinyApp(ui = ui, server = server)
