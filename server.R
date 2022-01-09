#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library("DT")
library("tidyverse")
library(readr)
library(readxl)


# Load dataset player earnings
# import data and rename column names
names <- c('Year', 'PlayerID', 'PlayerName', 'TotalPrizeMoneyYear', 'OverallPrizeMoney', 'TotalPercentage')
esport_earnings_players <- read_excel("Datasets/eSports Earnings 1998-2020.xlsx", sheet = "Top 100 Players 1998-2020", col_names = names, skip = 1)
esport_earnings <- esport_earnings_players %>%
  mutate(PlayerID=as.factor(PlayerID), PlayerName=as.factor(PlayerName), 
         TotalPrizeMoneyYear=as.numeric(TotalPrizeMoneyYear), OverallPrizeMoney=as.numeric(OverallPrizeMoney))

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$plot = renderPlot({
    plot(df1$Global_Sales, df1[[input$features]],
         xlab = "Component",
         ylab = "Global Sales, in millions USD")
    
    output  
  })
  
  output$earnings_table <- renderDT({
    
    esport_earnings %>%
      select(PlayerID, PlayerName, TotalPrizeMoneyYear, OverallPrizeMoney, TotalPercentage) %>%
      arrange(desc(TotalPrizeMoneyYear)) %>%
      datatable(rownames = input$showYear,
                extensions = "Responsive")
    
  })
  
  # Overall players by year
  output$totalPlayer <- renderText({
    if(input$selected_year == "All") {
      
      total_players_overall = esport_earnings$PlayerName
      
      total_players_overall = length(total_players_overall)
      
      print(total_players_overall)
      
      
    } else {
      
      total_players <- esport_earnings %>% 
        select(Year, PlayerName) %>%
        filter(Year == input$selected_year)
      
      print(length(unique(total_players$PlayerName)))
      
    }
  })
  
  # Overall total prize money by year
  output$totalPrizeMoney <- renderText({
    
    if(input$selected_year == "All") {
      
      totalPrizeMoneyAll = sum(esport_earnings$TotalPrizeMoneyYear)
      
      print(paste("$", totalPrizeMoneyAll, sep = ""))
      
    } else {
      
      total = esport_earnings %>%
        group_by(Year) %>%
        summarise(total = sum(TotalPrizeMoneyYear)) %>%
        filter(Year==input$selected_year) %>%
        select(total)
      
      print(paste("$", as.character(total), sep=""))
    }
      
  })
  
  # Overall prize money by year
  output$overallPrizeMoney <- renderText({
    
    if(input$selected_year == "All") {
      
      overallPrizeMoney = sum(esport_earnings$OverallPrizeMoney)
      
      print(paste("$", overallPrizeMoney, sep = ""))
      
    } else {
      
      total = esport_earnings %>%
        group_by(Year) %>%
        summarise(total = sum(OverallPrizeMoney)) %>%
        filter(Year==input$selected_year) %>%
        select(total)
      
      print(paste("$", as.character(total), sep=""))
    }
    
  })
  
  # Average prize money from 1998-2020
  output$averagePrizeMoney <- renderPlot({
    esport_earnings %>%
      group_by(Year) %>%
      summarize(average_prizeMoney=mean(TotalPrizeMoneyYear)) %>%
      ggplot() +
      geom_col(mapping = aes(x=Year, y=average_prizeMoney))
  })
  
  output$prizeDistribution <- renderPlot({
    ggplot(esport_earnings,
           aes(TotalPrizeMoneyYear, OverallPrizeMoney, color = Year)) +
      geom_point(size = 3) +
      geom_smooth(method = lm) +
      geom_density2d(alpha = .5) +
      theme(legend.position = "bottom")
  })
  
  output$totalPlayers <- renderPlot({
    
    # Total players from 1998-2020
    players <- esport_earnings %>%
      group_by(Year) %>%
      summarize(total_playerName = length(PlayerName))
    
    ggplot(data.frame(x = players$total_playerName), aes(x = x)) +
      geom_histogram(binwidth = 10,
                     fill = "#1D7685",
                     color = "white") +
      ylab("Frequency") +
      xlab("Total Players")
  
  })
  
})
