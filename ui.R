library(shiny)
library(shinydashboard)
library(ggplot2)
library(maps)
library(googleVis)
library(leaflet)
library(ggthemes)
sidebar <- dashboardSidebar(
  sidebarUserPanel("Jason Xu",
                     image = "https://media.licdn.com/mpr/mpr/shrinknp_400_400/p/7/005/008/220/2225030.jpg"),
  sidebarMenu(
#   menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
 #   menuItem("Widgets", icon = icon("th"), tabName = "widgets",
  #           badgeLabel = "new", badgeColor = "green"),
    menuItem("Time", tabName = "time", icon = icon("dashboard")),
    menuItem("Space", tabName = "space", icon = icon("th")),
    menuItem("Time & Space", tabName = "time_sp", icon =icon("th"))
    )
)

body <- dashboardBody(
  tabItems(tabItem(tabName = "dashboard",
                   fluidRow(box(title = "Introduction", status = "primary", solidHeader = FALSE, width = 6))
                  ),
           tabItem(tabName = "accd_space",
                   fluidRow(box(title = "Accidents maps", status = "primary", solidHeader = TRUE, "something here"))
                  ),
           tabItem(tabName = "time",
                   fluidRow(
                     tabBox(
                       title = "Accident Numbers", id = "tabset1", width = 12,
                                tabPanel("Year", plotOutput("accd_plot_year", width = 800, height = 350)),
                                tabPanel("Month", plotOutput("accd_plot_month", width = 800, height = 350)),
                                tabPanel("Day", plotOutput("accd_plot_day", width = 800, height = 350)), 
                                tabPanel("Hour", plotOutput("accd_plot_hour", width = 800, height = 350))
                       ),
                     conditionalPanel(
                       condition = "input.tabset1 == 'Year'",
                       box(title = "Years by Season", width = 8, plotOutput("accd_plot_year_bar"))
                     ),
                     conditionalPanel(
                       condition = "input.tabset1 == 'Day'",
                       box(title = "Days by Hour", width = 8, plotOutput("accd_plot_day_bar"))
                     ),
                     conditionalPanel(
                       condition = "input.tabset1 == 'Year'",
                       box(title = "Percentage of Greater London", width = 4, plotOutput("ratio_london", height =200))
                     ),
                     conditionalPanel(
                       condition = "input.tabset1 == 'Month'",
                       box(title = "Road Surface Condition", width = 8, plotOutput("road_surf_cond", height =350))
                     ),
                     conditionalPanel(
                       condition = "input.tabset1 == 'Hour'",
                       box(title = "Accidents for Each Hour in a Day", width = 8, plotOutput("accd_hour_day", height =350))
                     )
                   )
                  ),
           tabItem(tabName = "space",
                   fluidRow(
                     tabBox(title = "Areas", id = "tab_space", width = 12,
                     leafletOutput("accd_map_sel", width = 800, height = 400)
                     ),
                     selectizeInput("accd_maps_sp", "Regions", choice = city_choi_small),
                     box(title = "Distance to City Center", id = "tab_space_city", width = 6,
                            plotOutput("accd_city_r", width = 400, height = 250)
                     ),
                     box(title = "Mean Distance to City Center", id ="tab_mean_distance", width = 6,
                         plotOutput("accd_r_mean", width = 400, height = 250))
                  )
                  
           ),
           tabItem(tabName = "time_sp",
                   fluidRow(
                     tabBox(title = "Accident Maps", id = "tab_time_sp", width = 12,
                            leafletOutput("accd_map_time_sp", width = 800, height = 350)
                     ),
                     selectizeInput("accd_maps_ts", "Regions", choice = city_choi),
                     selectizeInput("accd_year_ts", "Choose a Year", choice = year_choi),
                     tabBox(title = "Choose a Month", id = "time_sp_month", width = 6,
                            sliderInput("slider_month", "Choose a Month of a Year:", 0, 12, 0)),
                     tabBox(title = "Choose a Day", id = "time_sp_day", width = 6,
                            sliderInput("slider_day", "Choose a Day of a Week:", 0, 7, 0)),
                     tabBox(title = "Choose an Hour", id = "time_sp_hour", width = 6,
                            sliderInput("slider_hour", "Choose an hour of a day:", 0, 24, 0, animate = TRUE))
                   )
          )
  )
)

shinyUI(dashboardPage(
  dashboardHeader(title = "UK Traffic Accidents"),
  sidebar,
  body
))
