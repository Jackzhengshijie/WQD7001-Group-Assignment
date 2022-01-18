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
library(dplyr)
library(scales)
library(reshape)
library(stringr)

# Load dataset player earnings
# import data and rename column names
names <- c('Year', 'PlayerID', 'PlayerName', 'TotalPrizeMoneyYear', 'OverallPrizeMoney', 'TotalPercentage')
esport_earnings_players <- read_excel("C:\\Users\\KSAR\\Downloads\\WQD7001-Group-Assignment-main (1)\\WQD7001-Group-Assignment-main\\eSports Earnings 1998-2020.xlsx", sheet = "Top 100 Players 1998-2020", col_names = names, skip = 1)
esport_earnings <- esport_earnings_players %>%
  mutate(PlayerID=as.factor(PlayerID), PlayerName=as.factor(PlayerName), 
         TotalPrizeMoneyYear=as.numeric(TotalPrizeMoneyYear), OverallPrizeMoney=as.numeric(OverallPrizeMoney))

# Kamal: upload dota 2 data and clean

dota2data = read_excel("C:\\Users\\KSAR\\OneDrive\\Documents\\FnaticsGroupProjectv2\\dota2shiny\\Largest Overall Prize Pools in Esports.xlsx")
dota2data = as.data.frame(dota2data)
colnames(dota2data) = c("No.", "TournamentName", "PrizePool", "Game", "NumberOfTeams", "NumberOfPlayers")
dota2data = select(dota2data, -c("No.")) 
dota2data$PrizePool = as.integer(dota2data$PrizePool)

dota2data = dota2data %>% mutate(PrizePerPlayer = PrizePool / NumberOfPlayers) %>% # create new prize per player column
  mutate(PrizePerTeam = PrizePool / NumberOfTeams) # Create new prize per team column

dota2data$PrizePerPlayer = as.integer(dota2data$PrizePerPlayer)
dota2data$PrizePerTeam = as.integer(dota2data$PrizePerTeam)

# extract year from tournament names
yearExtract <- function(string) {
  t <- regmatches(string, regexec("[0-9]{4}", string))
  sapply(t, function(x) {
    if(length(x) > 0){
      return(as.numeric(x))
    } else {
      return(NA)    
    }
  })
}

dota2data$Year = yearExtract(as.character(dota2data$TournamentName))
dota2data

# END dota 2------------------------------------------------------------------------------------------------------------------------

## Upload vgsales and steam top 100 data and clean

vgs = read.csv("C:\\Users\\KSAR\\OneDrive\\Documents\\FnaticsGroupProjectv2\\dota2shiny\\vgsales.csv")
colnames(vgs)
str(vgs)
vgs$Genre = as.factor(vgs$Genre)

steam100 = read.csv("C:\\Users\\KSAR\\OneDrive\\Documents\\FnaticsGroupProjectv2\\dota2shiny\\steam_top_100.csv")

# END vgs ---

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  ## Kamal: Render dota 2 table output ----------------------------------------------------------------------------------
  
  output$table1 = DT::renderDataTable({
    if (input$dota2input != "All"){
      dota2data = filter(dota2data, Game == input$dota2input)
    }
    
    DT::datatable(dota2data, options = list(orderClasses = TRUE))
  })
  
  # Plot 1
  
  output$barplot = renderPlot({
    
    if (input$dota2input != "All"){
      dota2data = filter(dota2data, Game == input$dota2input)
    }
    
    groupdata = group_by(dota2data, PrizePool) %>%
      arrange(desc(PrizePool))
    
    ggplot(groupdata, aes(reorder(TournamentName, PrizePool))) + 
      geom_bar(aes(weight = PrizePool), fill = 'tomato1') + 
      scale_y_continuous(labels = comma) +
      coord_flip() + 
      ggtitle("Tournaments") + 
      xlab("Tournament Name") + 
      ylab("Prize Pool") + 
      theme_bw(base_size = 16)
  })
  
  # Plot 2
  
  output$lineplot = renderPlot({
    
    if (input$dota2input != "All"){
      dota2data = filter(dota2data, Game == input$dota2input)
    }
    
    groupdata = group_by(dota2data, PrizePerPlayer) %>%
      arrange(desc(Year))
    
    ggplot(groupdata, aes(reorder(Year, PrizePerPlayer))) + 
      geom_bar(aes(weight = PrizePerPlayer), fill = 'blue') +
      scale_y_continuous(labels = comma) + 
      coord_flip() + 
      ggtitle("Over the years") + 
      xlab("Year") + 
      ylab("Prize Per Player") + 
      theme_light(base_size = 15) + 
      theme(axis.text.x = element_text(angle = 0, hjust = 1))
  })
  
  # END -----------------------------------------------------------------------------------------------------------------
  
  ## Render vgsales output --------------------------------------------------------------------------------------------
  
  # Plot data table
  
  output$tablevgsales = DT::renderDataTable({
    if (input$vgsalesinput != 'All'){
      vgs = vgs %>% 
        filter(Platform == input$vgsalesinput)
    }
    
    if (input$vginput2 != 'All'){
      vgs = vgs %>%
        filter(Genre == input$vginput2)
    }
    
    DT::datatable(vgs, options = list(orderClasses = TRUE))
  })
  
  # Plot 1
  
  output$GlobalSales = renderPlot({
    if (input$vgsalesinput != 'All'){
      vgs = vgs %>% 
        filter(Platform == input$vgsalesinput)
    }
    
    ggplot(vgs, aes(reorder(Genre, Global_Sales))) + 
      geom_bar(aes(weight = Global_Sales)) +
      ggtitle("Global Sales by Genre") + 
      xlab('Genre') + 
      ylab('Global Sales in million') + 
      theme_classic(base_size = 15) +
      theme(axis.text.x = element_text(angle = 60, hjust = 1))
  })
  
  # Plot 2
  
  output$genre = renderPlot({
    if (input$vginput2 != 'All'){
      vgs = vgs %>%
        filter(Genre == input$vginput2)
    }
    
    ggplot(data = vgs, aes(x = Platform, y = Global_Sales, fill = input$vginput2)) +
      geom_col() +
      ggtitle("Global Sales by Platform") + 
      xlab('Platform') + 
      ylab('Global_Sales in million') +
      guides(fill = guide_legend(title = input$vginput2)) +
      theme_classic(base_size = 15) +
      theme(axis.text.x = element_text(angle = 60, hjust = 1))
  })
  
  # END ---
  
  ## Aby: render output
  
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
  
  # END ---
  
})