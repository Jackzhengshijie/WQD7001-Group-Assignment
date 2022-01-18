#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readxl)
library(dplyr)
library(scales)
library(reshape)
library(DT)
library(stringr)

# upload dota 2 data and clean

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

# END ------------------------------------------------------------------------------------------------------------------------

## Upload vgsales and steam top 100 data and clean

vgs = read.csv("C:\\Users\\KSAR\\OneDrive\\Documents\\FnaticsGroupProjectv2\\dota2shiny\\vgsales.csv")
colnames(vgs)
str(vgs)
vgs$Genre = as.factor(vgs$Genre)

steam100 = read.csv("C:\\Users\\KSAR\\OneDrive\\Documents\\FnaticsGroupProjectv2\\dota2shiny\\steam_top_100.csv")

# Define server logic required to draw a histogram
server = function(input, output) {
  
  ## Render dota 2 table output ----------------------------------------------------------------------------------
  
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
  
  output$GlobalSales = renderPlot({
    if (input$vgsalesinput != 'All'){
      vgs = vgs %>% 
        filter(Platform == input$vgsalesinput)
    }
    
    ggplot(vgs, aes(reorder(Genre, Global_Sales))) + 
      geom_bar(aes(weight = Global_Sales)) +
      ggtitle("Global Sales by Platform") + 
      xlab('Genre') + 
      ylab('Global Sales in million') + 
      theme_classic(base_size = 15) +
      theme(axis.text.x = element_text(angle = 60, hjust = 1))
  })
  
  
  output$genre = renderPlot({
    if (input$vginput2 != 'All'){
      vgs = vgs %>%
        filter(Genre == input$vginput2)
    }
    
    ggplot(data = vgs, aes(x = Platform, y = Global_Sales, fill = input$vginput2)) +
      geom_col() +
      ggtitle("Global Sales by Genre") + 
      xlab('Platform') + 
      ylab('Global_Sales in million') +
      guides(fill = guide_legend(title = input$vginput2)) +
      theme_classic(base_size = 15) +
      theme(axis.text.x = element_text(angle = 60, hjust = 1))
  })

# END ------------------------------------------------------------------------------------------------------------------------     
    
    
}