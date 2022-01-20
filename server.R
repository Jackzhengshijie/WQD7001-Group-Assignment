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
library(readxl)
library(scales)
library(reshape)
library(ggplot2)

# Load dataset player earnings
# Aby: import data and rename column names
names <- c('Year', 'PlayerID', 'PlayerName', 'TotalPrizeMoneyYear', 'OverallPrizeMoney', 'TotalPercentage')
esport_earnings_players <- read_excel("C:\\Users\\KSAR\\OneDrive\\Documents\\FnaticsGroupProjectv2\\WQD7001-Group-Assignment-main\\eSports Earnings 1998-2020.xlsx", sheet = "Top 100 Players 1998-2020", col_names = names, skip = 1)
esport_earnings <- esport_earnings_players %>%
  mutate(PlayerID=as.factor(PlayerID), PlayerName=as.factor(PlayerName), 
         TotalPrizeMoneyYear=as.numeric(TotalPrizeMoneyYear), OverallPrizeMoney=as.numeric(OverallPrizeMoney))

# For vgsales input
df1 = read.csv("C:\\Users\\KSAR\\OneDrive\\Documents\\FnaticsGroupProjectv2\\WQD7001-Group-Assignment-main\\vgsales.csv")

# Iesha: For Team and countries input
topteams <- read_excel("C:\\Users\\KSAR\\OneDrive\\Documents\\FnaticsGroupProjectv2\\WQD7001-Group-Assignment-main\\topteamscleaned.xlsx")
yearchoice <- c("All", unique(topteams$Year))
teamchoice <- c("All", unique(topteams$Team))

topcountries <- read_excel("C:\\Users\\KSAR\\OneDrive\\Documents\\FnaticsGroupProjectv2\\WQD7001-Group-Assignment-main\\topcountriescleaned2.xlsx")
colnames(topcountries) = c('Year', 'Country', 'TotalPrizeMoney', 'NumberofPlayers')

# Kamal: upload dota 2 data and clean

dota2data = read_excel("C:\\Users\\KSAR\\OneDrive\\Documents\\FnaticsGroupProjectv2\\WQD7001-Group-Assignment-main\\Largest Overall Prize Pools in Esports.xlsx")
dota2data = as.data.frame(dota2data)
dota2data = select(dota2data, -c("No.")) 

dota2data = dota2data %>%
  mutate(NumberOfPlayers2 = ifelse(dataset$NumberOfPlayers == 0, 2, dataset$NumberOfPlayers))
  
dota2data = dota2data %>%
  mutate(NumberofTeams4 = ifelse(is.na(dataset$NumberOfTeams), 1, dataset$NumberOfTeams))

colnames(dota2data) = c('TournamentName', "PrizePool", "Game", "NumberOfTeams", "NumberOfPlayers", "NumberofPlayers2", 'NumberofTeams2')

dota2data$NumberofTeams2 = as.numeric(dota2data$NumberofTeams2)

dota2data = dota2data %>% mutate(PrizePerPlayer = PrizePool / NumberofPlayers2) %>% # create new prize per player column
  mutate(PrizePerTeam = PrizePool / NumberofTeams2) # Create new prize per team column

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

# END dota 2 clean -------------------------------------------------------------------------------------------------------------------

## Upload vgsales and clean

vgs = read.csv("C:\\Users\\KSAR\\OneDrive\\Documents\\FnaticsGroupProjectv2\\WQD7001-Group-Assignment-main\\vgsales.csv")
colnames(vgs)
str(vgs)
vgs$Genre = as.factor(vgs$Genre)

# END vgs ---

# Define server logic
shinyServer(function(input, output) {
  
  # Iesha: Team and Countries
  
  #DATATABLE
  output$teamtable <- DT::renderDataTable({
    
    if (input$selectYear == "All" & input$teamInput == "All"){
      result <- topteams
    }
    else if (input$selectYear == "All" & input$teamInput != "All"){
      df1 <- topteams %>%
        filter(Team %in% input$teamInput)
      result1 <- df1
    }
    else if(input$selectYear != "All" & input$teamInput == "All"){
      df2 <- topteams %>%
        filter(Year %in% input$selectYear)
      result2 <- subset(df2)
    }
    else{
      df3 <- topteams %>%
        filter(Year %in% input$selectYear & Team %in% input$teamInput)
      result3 <- subset(df3)
    }
  })
  
  # vgsales dashboard ---
  
  output$totalgames <- renderText({
    print(comma(sum(length(unique(vgs$Name)))))
    
  })
  
  output$totalplatform <- renderText({
    print(comma(sum(length(unique(vgs$Platform)))))
    
  })
  
  output$totalpublisher <- renderText({
    print(comma(sum(length(unique(vgs$Publisher)))))
    
  })
  
  output$totalgenre <- renderText({
    print(comma(sum(length(unique(vgs$Genre)))))
    
  })
  
  # END ---
  
  # Plotting top earnings by TEAMS bar chart
  output$barteam <- renderPlot({
    if (input$selectYear != "All"){
      topteams = topteams %>%
        filter(Year == input$selectYear)
    }
    
    topteams[order(topteams$TotalPrizeMoneyYear, decreasing = TRUE),] %>%
      slice(1:15) %>%
      ggplot(aes(x = reorder(Team, TotalPrizeMoneyYear), y = TotalPrizeMoneyYear)) + 
      geom_bar(stat = "identity", 
               fill = rainbow(n = 15 , start = .55, end = 1)) + 
      scale_y_continuous(labels = comma) +
      coord_flip() +
      ggtitle("Top Earnings by Team") +
      labs(x = "Team Name", y = "Total Winnings") +
      theme(axis.text = element_text(size = 15), axis.title = element_text(size = 20))
    
  })
  
  ##ABORT PLOT TAK JADI
  output$barplot2 = renderPlot({
    
    if (input$teamInput != "All" ){
      topteams = filter(topteams, Team == input$teamInput)
    }
    
    groupdata = group_by(topteams, TotalPrizeMoneyYear) %>%
      arrange(desc(TotalPrizeMoneyYear))
    
    ggplot(groupdata, aes(reorder(Year, TotalPrizeMoneyYear))) + 
      geom_bar(aes(weight = TotalPrizeMoneyYear), fill = 'tomato1') + 
      scale_y_continuous(labels = comma) +
      coord_flip() + 
      ggtitle("Top Teams") + 
      xlab("Year") + 
      ylab("Winnings") + 
      theme_bw(base_size = 16) +
      theme(axis.text = element_text(size = 15), axis.title = element_text(size = 20))
  })
  
  output$countrytable <- DT::renderDataTable({
    
    if (input$selectyearcountry == "All" & input$countryInput == "All"){
      result <- topcountries
    }
    else if (input$selectyearcountry == "All" & input$countryInput != "All"){
      df1 <- topcountries %>%
        filter(Country %in% input$countryInput)
      result1 <- df1
    }
    else if(input$selectyearcountry != "All" & input$countryInput == "All"){
      df2 <- topcountries %>%
        filter(Year %in% input$selectyearcountry)
      result2 <- subset(df2)
    }
    else{
      df3 <- topcountries %>%
        filter(Year %in% input$selectyearcountry & Country %in% input$countryInput)
      result3 <- subset(df3)
    }
    
  })
  
  # Overall number of players by year DONE
  output$totalPlayer2 <- renderText({
    if(input$selectyearcountry == "All" & input$countryInput == 'All') {
      
      total_players_overall = sum(topcountries$NumberofPlayers)
      print(comma(total_players_overall))
      
      
    } else if(input$selectyearcountry != "All" & input$countryInput != 'All'){
      
      total_players <- topcountries %>% 
        select(Year, Country, NumberofPlayers) %>%
        filter(Year == input$selectyearcountry) %>%
        filter(Country == input$countryInput)
      
      print(comma(sum(total_players$NumberofPlayers)))
      
    } else if(input$selectyearcountry != "All" & input$countryInput == 'All'){
      
      total_players = topcountries %>%
        select(Year, Country, NumberofPlayers) %>%
        filter(Year == input$selectyearcountry)
      
      print(comma(sum(total_players$NumberofPlayers)))
      
    }else if(input$selectyearcountry == "All" & input$countryInput != 'All'){
      total_players = topcountries %>%
        select(Year, Country, NumberofPlayers) %>%
        filter(Country == input$countryInput)
      
      print(comma(sum(total_players$NumberofPlayers)))
            
    }
  })
  
  # total number of countries participated by year DONE
  output$totalCountry <- renderText({
    if(input$selectyearcountry == "All" & input$countryInput == 'All') {
      
      sumofcountriesbyyear = length(topcountries$Country)
      print(comma(sumofcountriesbyyear))
      
      
    } else if(input$selectyearcountry != "All" & input$countryInput != 'All'){
      
      numofcountriesbyyear <- topcountries %>% 
        select(Year, Country) %>%
        filter(Country == input$countryInput) %>%
        filter(Year == input$selectyearcountry)
      
      print(comma(length(unique(numofcountriesbyyear$Country))))
      
    } else if(input$selectyearcountry != "All" & input$countryInput == 'All'){
      
      numofcountriesbyyear = topcountries %>%
        select(Year, Country) %>%
        filter(Year == input$selectyearcountry)
      
      print(comma(length(unique(numofcountriesbyyear$Country))))
      
    } else if(input$selectyearcountry == "All" & input$countryInput != 'All'){
      
      numofcountriesbyyear = topcountries %>%
        select(Year, Country) %>%
        filter(Country == input$countryInput)
      
      print(comma(length(unique(numofcountriesbyyear$Country))))
      
    }
  })
  
  # Overall total prize money by country DONE
  output$totalPrizeMoney2 <- renderText({
    
    if(input$countryInput == "All" & input$selectyearcountry == 'All') {
      
      totalPrizeMoneyAll = sum(topcountries$TotalPrizeMoney)
      
      print(paste("$", comma(sum(totalPrizeMoneyAll)), sep = ""))
      
    } else if(input$countryInput != "All" & input$selectyearcountry != 'All'){
      
      total = topcountries %>%
        select(Year, Country, TotalPrizeMoney) %>%
        group_by(Country) %>%
        filter(Country == input$countryInput) %>%
        filter(Year == input$selectyearcountry)
      
      print(paste("$", comma(sum(total$TotalPrizeMoney)), sep = ""))
      
    } else if(input$countryInput != "All" & input$selectyearcountry == 'All'){
      
      total = topcountries %>%
        select(Year, Country, TotalPrizeMoney) %>%
        group_by(Country) %>%
        filter(Country == input$countryInput)
      
      print(paste("$", comma(sum(total$TotalPrizeMoney)), sep = ''))
      
    } else if(input$countryInput == "All" & input$selectyearcountry != 'All'){
      
      total = topcountries %>%
        select(Year, Country, TotalPrizeMoney) %>%
        group_by(Country) %>%
        filter(Year == input$selectyearcountry)
      
      print(paste('$', comma(sum(total$TotalPrizeMoney)), sep = ''))
      
    }
    
  })
  
  # HISTOGRAM TPM by year from 1998-2020
  output$histotpm <- renderPlot({
    
    if (input$countryInput != 'All'){
      topcountries = topcountries %>%
        filter(Country == input$countryInput)
    }
    
    topcountries %>%
      group_by(Year) %>%
      summarise(TotalEarnings = sum(TotalPrizeMoney)) %>%
      mutate(Condition = ifelse(Year == 2020,TRUE,FALSE)) %>%
      ggplot(aes(x = Year, y = TotalEarnings, fill = Condition)) +
      geom_bar(stat = "Identity", color = "black",alpha = 0.75) +
      geom_text(aes(label = round(TotalEarnings/1E6,2)), vjust = -0.3, size = 3) +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
            plot.caption = element_text(size = 10)) + 
      labs(title = "Total prize money per year*", subtitle = "In millions of dollars", y = element_blank(),x = "Year") +
      scale_fill_manual(values = c("lightblue","lightblue4")) +
      guides(fill = "none")
  })
  
  # HISTOGRAM total players by year from 1998-2020
  output$histoplayer <- renderPlot({
    
    if (input$countryInput != 'All'){
    
    topcountries = topcountries %>%
      filter (Country == input$countryInput)
      
    }
      
    topcountries %>%
      group_by(Year) %>%
      summarise(TotalPlayers = sum(NumberofPlayers)) %>%
      mutate(Condition = ifelse(Year == 2020, TRUE, FALSE)) %>%
      ggplot(aes(x = Year, y = TotalPlayers, fill = Condition)) +
      geom_bar(stat = "Identity", color = "black", alpha = 0.75) +
      geom_text(aes(label = comma(TotalPlayers)), vjust = -0.3, size = 3) +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
            plot.caption = element_text(size = 10)) + 
      labs(title = "Total number of players per year", y = element_blank(), x = "Year",) +
      scale_fill_manual(values = c("lightblue","lightblue4")) +
      guides(fill = "none")
    
  })
  
  # Plotting top earnings by country bar chart
  output$barcountry <- renderPlot({
    
    topcountries[order(topcountries$TotalPrizeMoney, decreasing = TRUE),] %>%
      slice(1:30) %>%
      ggplot(aes(reorder(Country, TotalPrizeMoney))) + 
      geom_bar(aes(weight = TotalPrizeMoney), fill = 'red', col = 'black') + 
      scale_y_continuous(labels = comma) +
      coord_flip() +
      labs(x = "Country", y = "Total Winnings", title = "Top Earnings by Country") +
      theme_bw(base_size = 16) +
      theme(axis.text = element_text(size = 15), axis.title = element_text(size = 12))
    
  })
  
  # END ---
  
  ## Render vgsales table and chart output --------------------------------------------------------------------------------------------
  
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
  
  ## Kamal: Render dota 2 table output ----------------------------------------------------------------------------------
  
  output$table1 = DT::renderDataTable({
    if (input$dota2input != "All"){
      dota2data = filter(dota2data, Game == input$dota2input)
    }
    
    DT::datatable(dota2data, options = list(orderClasses = TRUE))
  })
  
  # player and team earnings dashboard ---
  
  output$highestprizeforteam <- renderText({
    print(paste("$", comma(max(dota2data$PrizePerTeam), sep = '')))
    
  })
  
  output$highestprizeforplayer <- renderText({
    print(paste("$",comma(max(dota2data$PrizePerPlayer), sep = '')))
    
  })
  
  output$averageprizeforteam <- renderText({
    print(paste("$", comma(mean(dota2data$PrizePerTeam), sep = '')))
    
  })
  
  output$averageprizeforplayer <- renderText({
    print(paste("$", comma(mean(dota2data$PrizePerPlayer), sep = '')))
    
  })
  
  # END ---
  
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
      arrange(desc(PrizePerPlayer))
    
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

shinyApp (ui = shinyUI, server = shinyServer)