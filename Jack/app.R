library(shiny)
library(shinyWidgets)
library(reactable)
library(purrr)
library(readr)
library(janitor)
library(data.table)
library(scales)
library(echarts4r)
library(shinydashboardPlus)
library(shinydashboard)
library(fontawesome)
library("DT")
library("tidyverse")
library(readr)
library(readxl)
source_dir <- function(dir_name){
  files = list.files(dir_name, full.names = TRUE, pattern = ".\\.R$")
  files %>% walk(source)  
}
names1 <- c('Year', 'PlayerID', 'PlayerName', 'TotalPrizeMoneyYear', 'OverallPrizeMoney', 'TotalPercentage')
esport_earnings_players <- read_excel("C:/Users/76862/Desktop/VideoGamesSales/WQD7001-Group-Assignment/eSports Earnings 1998-2020.xlsx", sheet = "Top 100 Players 1998-2020", col_names = names1, skip = 1)
esport_earnings <- esport_earnings_players %>%
  mutate(PlayerID=as.factor(PlayerID), PlayerName=as.factor(PlayerName), 
         TotalPrizeMoneyYear=as.numeric(TotalPrizeMoneyYear), OverallPrizeMoney=as.numeric(OverallPrizeMoney))
df1 <- read.csv("vgsales.csv")


kpi <- function(number = NULL, numberColor = NULL, numberIcon = NULL, 
                header = NULL, text = NULL, rightBorder = TRUE, marginBottom = FALSE) 
{
  cl <- "description-block"
  if (isTRUE(rightBorder)) 
    cl <- paste0(cl, " border-right")
  if (isTRUE(marginBottom)) 
    cl <- paste0(cl, " margin-bottom")
  numcl <- "description-percentage"
  if (!is.null(numberColor)) numcl <- paste0(numcl, " text-", numberColor)
  shiny::tags$div(
    class = cl
    , style = "margin-left: 10px; margin-right: 10px;"
    , shiny::tags$span(class = numcl, number, if (!is.null(numberIcon)) shiny::icon(numberIcon))
    # , shiny::tags$h5(class = "description-header", header)
    , shiny::tags$span(style = 'font-size: 24px; color: #3c8dbc;', header)
    , br()
    # , shiny::tags$span(class = "description-text", text)
    , shiny::tags$span(style = 'color: #3c8dbc;', text)
  )
}

library(data.table)
v <- reactiveValues(
  critic_range = c(0, 100)
  , user_range = c(0, 100)
)

vgs <- reactive({
  
  df <- fread(
    "vgsales.csv"
    , colClasses = c(
      Critic_Score = "double" 
    )
  ) %>%
    clean_names()
  
  # Convert user score to number - sets 'tbc' to NAs
  df <- df[, 
           user_score := as.numeric(user_score)
  ]
  
  # Filter on 1983-2016
  df <- df[
    year_of_release %in% 1983:2016
  ]
  
  # Rating to factor
  # In 1998 K-A rating changed to E (https://www.esrb.org/history/)
  df <- df[rating == "K-A", 
           rating := "E"
  ]
  
  # Change missing rating to NA
  df <- df[rating == "", 
           rating := NA
  ]
  
  # Create factor
  rating_levels <- c("EC", "E", "E10+", "T", "M", "AO", "RP")
  df <- df[,
           rating := factor(rating, levels = rating_levels)
  ]
  
  # Sales to units
  # Ratings to decimal
  df <- df[,
           ':='(
             na_sales = na_sales*1000000
             , eu_sales = eu_sales*1000000
             , jp_sales = jp_sales*1000000
             , other_sales = other_sales*1000000
             , global_sales = global_sales*1000000
             , critic_score = critic_score/100
           )
  ]
  
  df    
})

vgs_filtered <- reactive({
  
  df <- vgs()
  
  critic_range <- v$critic_range/100
  user_range <- v$user_range/100
  
  if(any(critic_range != c(0, 1))){
    df <- df[
      critic_score %inrange% critic_range
    ]
  }
  
  if(any(user_range != c(0, 1))){
    df <- df[
      user_score %inrange% user_range
    ]
  }
  
  cols <- c("name", "platform", "year_of_release", "genre", "publisher", 
            "na_sales", "eu_sales", "jp_sales", "other_sales", "global_sales", 
            "critic_score", "critic_count", "user_score", "user_count", "developer", 
            "rating")
  
  # Summary of unit sales for table view
  summary_unit_sales <- df[, 
                           .(
                             global_sales = sum(global_sales, na.rm = TRUE)
                             , critic_score = mean(critic_score, na.rm = TRUE)
                           )
                           , by = c("platform", "genre", "publisher")
  ][order(-global_sales)]
  
  # Summary of sales by country for chart view
  summary_country_year_sales <- df[,
                                   .(
                                     na_sales = sum(na_sales)/1000000000
                                     , eu_sales = sum(eu_sales)/1000000000
                                     , jp_sales = sum(jp_sales)/1000000000
                                     , other_sales = sum(other_sales)/1000000000
                                     , games = .N
                                   )
                                   , by = c("year_of_release")
  ][order(year_of_release)]
  
  list(
    data = df
    , summary = list(
      unit_sales = summary_unit_sales
      , country_year_sales = summary_country_year_sales
    )
  )
})

column_definitions <- reactive({
  
  cols <- c("name", "platform", "year_of_release", "genre", "publisher", 
            "na_sales", "eu_sales", "jp_sales", "other_sales", "global_sales", 
            "critic_score", "critic_count", "user_score", "user_count", "developer", 
            "rating")
  
  list(
    name = colDef(
      name = "Game"
      , minWidth = 180
    )
    , platform = colDef(
      name = "Platform"
      , width = 60
      , footer = "Total"
    )
    , year_of_release = colDef(
      name = "Released"
      , width = 50
    )
    , genre = colDef(
      name = "Genre"
      , width = 80
    )
    , publisher = colDef(
      name = "Publisher"
      , minWidth = 150
    )
    , developer = colDef(
      name = "Developer"
      , minWidth = 150
    )
    , global_sales = colDef(
      name = "Sales"
      , minWidth = 80
      , format = colFormat(separators = TRUE, digits = 0)
      , footer = function(x){ format(sum(x, na.rm = TRUE), digits = 0, big.mark = ",")}
    )
    , critic_score = colDef(
      name = "Critic Score (Avg)"
      , minWidth = 100
      , format = colFormat(percent = TRUE, digits = 0)
      , footer = function(x) { percent(mean(x, na.rm = TRUE))}
    )
  )
  
})

easy_ui <- function(id){
  ns <- NS(id)
  uiOutput(ns("easy_ui"))
}

easy_server <- function(input, output, session){
  
  ns <- session$ns
  
  output$easy_ui <- renderUI({
    
    tagList(
      fluidRow(
        column(
          width = 12
          , style = "display: flex;"
          , h1("Video Game Sales (1982-2016)")
        )
        , 
      )
      , fluidRow(
        column(
          width = 12
          , uiOutput(ns("kpi_ui"))
        )
      )
      , hr()
      , fluidRow(
        column(
          width = 12
          , tags$style(HTML("
            #easy-unit_sale_summary_rt > div > div.rt-table > div.rt-thead.-header {
              color: white;
              background-color: #3c8dbc;
            }
            #easy-unit_sale_summary_rt > div > div.rt-table > div.rt-tfoot {
              color: white;
              background-color: #3c8dbc;
            }
                            "))
          , div(
            style = "font-size: 80%;"
            , reactableOutput(ns("unit_sale_summary_rt"))
          )
        )
        , column(
          width = 12
          , echarts4rOutput(ns("sales_by_year_chart"))
        )
      )
    )
    
  })
  
  # KPI --------------------------------------------------------------------------------------------
  
  output$kpi_ui <- renderUI({
    
    vgs <- vgs_filtered()
    
    # Country totals
    country_year_sales <- vgs$summary$country_year_sales
    country_totals <- country_year_sales[, 
                                         .(
                                           na_sales = sum(na_sales)
                                           , eu_sales = sum(eu_sales)
                                           , jp_sales = sum(jp_sales)
                                           , other_sales = sum(other_sales)
                                         )
    ] %>% 
      as.list() %>% 
      map(
        function(x){
          x <- x %>% round(digits = 2)
          if(x > 1){
            paste0(x, "bn")
          } else {
            paste0(1000*x, "M")
          }  
        }
      )
    
    # summary
    df <- vgs$data[, 
                   .(
                     critic_count = sum(critic_count, na.rm = TRUE)
                     , user_count = sum(user_count, na.rm = TRUE)
                     , platforms = uniqueN(platform)
                     , publishers = uniqueN(publisher)
                     , developers = uniqueN(developer)
                     , games = .N 
                     , years = uniqueN(year_of_release)
                   )
    ] %>%
      as.list()
    
    # games/year
    df$games_per_year <- round(df$games/df$years, 0)
    
    df <- df %>%
      map(
        function(x){
          if(x > 1000000) return(paste0(round(x/1000000, 2), "M"))
          if(x > 1000) return(paste0(round(x/1000, 1), "K"))
          x            
        }
      )
    
    fluidRow(
      kpi_helper("Sales (EU)", country_totals$eu_sales)
      , kpi_helper("Sales (JP)", country_totals$jp_sales)
      , kpi_helper("Sales (USA)", country_totals$na_sales)
      , kpi_helper("Sales (Other)", country_totals$other_sales)
      , kpi_helper("Critic Reviews", df$critic_count)
      , kpi_helper("User Reviews", df$user_count)
      , kpi_helper("Games/Year", df$games_per_year)
      , kpi_helper("Platforms", df$platforms)
      , kpi_helper("Publishers", df$publishers)
      , kpi_helper("Developers", df$developers)
      , kpi_helper("Games", df$games)
    )
    
  })
  
  kpi_helper <- function(text, header){
    column(
      width = 1
      , kpi(header = header, text = text)
    )
  }
  
  
  # Summary reactable ----------------------------------------------------------------------------- 
  
  output$unit_sale_summary_rt <- renderReactable({
    
    df <- vgs_filtered()$summary$unit_sales
    columns <- column_definitions()
    
    # Filter columns on those in data
    columns <- columns[names(columns) %in% names(df)] 
    
    reactable(
      df
      , columns = columns
      , compact = TRUE
      , bordered = TRUE
      , striped = TRUE
      , highlight = TRUE
      , defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
    )
    
  })
  
  # Sales by year chart --------------------------------------------------------------------------- 
  
  output$sales_by_year_chart <- renderEcharts4r({
    
    df <- vgs_filtered()$summary$country_year_sales
    
    df %>% 
      e_charts(year_of_release) %>% 
      e_grid(left = "12%") %>%
      e_y_axis(name = "Total Unit Sales", index = 0, position = "left", nameLocation = "center", nameGap = 50) %>%
      # e_y_axis(name = "Sales", index = 0, position = "left") %>%
      e_x_axis(name = "Release Year", nameLocation = "center", nameGap = 25) %>%
      e_bar(other_sales, name = "Sales (Other)", stack = "country") %>% 
      e_bar(na_sales, name = "Sales (USA)", stack = "country") %>% 
      e_bar(jp_sales, name = "Sales (Japan)", stack = "country") %>% 
      e_bar(eu_sales, name = "Sales (EU)", stack = "country") %>% 
      e_format_y_axis(suffix = "bn") %>%
      e_line(games, symbol = "circle", showSymbol = FALSE, name = "Games", y_index = 1, color = "red"
             , symbolStyle = list(normal = list(opacity = 0.5)), lineStyle = list(normal = list(opacity = 1))) %>%
      e_y_axis(name = "Games Released", index = 1, position = "right", nameLocation = "center", nameGap = 40) %>%
      # e_y_axis(name = "Games", index = 1, position = "right") %>%
      e_hide_grid_lines(which = "y") %>%
      e_legend(top = 30) %>%
      e_title("Unit Sales by Region") %>%
      e_theme("macarons")})
  
  # END -------------------------------------------------------------------------------------------
}
hard_ui <- function(id){
  ns <- NS(id)
  uiOutput(ns("hard_ui"))
}

hard_server <- function(input, output, session){
  
  ns <- session$ns
  
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
  
  
  
  # END -------------------------------------------------------------------------------------------
}


# Load dataset player earnings
# import data and rename column names

# Define server logic required to draw a histogram

dash_header <- function(){
  dashboardHeader(
    title = tagList(
      span(class = "logo-lg", "Video game sales")
    )
  )
}
dash_side_bar <- function(){
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        text = "Presentation"
        , tabName = "easy"
        , icon = icon("cube")
      ),
      menuItem("Overview", tabName = "Overview", icon = icon("cube")),
      menuItem("Consoles", tabName = "Consoles", icon = icon("cube")),
      menuItem("Dota2", tabName = "Dota2", icon = icon("cube")),
      menuItem("Players", tabName = "Players", icon = icon("cube"))
    )
  )
}

dash_body <- function(){
  dashboardBody(
    tabItems(
      easy_tab,
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
}

easy_tab <- tabItem(
  tabName = "easy"
  , easy_ui("easy")
)
shinyApp(
  ui = dashboardPage(
    header = dash_header()
    , sidebar = dash_side_bar()
    , body = dash_body()
  ),
  server = function(input, output, session){ 
    callModule(easy_server, "easy")
    callModule(hard_server, "hard")
  }
)

