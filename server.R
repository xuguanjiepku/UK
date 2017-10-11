library(DT)
library(shiny)
library(googleVis)
library(ggthemes)


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  #updateSelectInput(session, )
  output$accd_plot_year <- renderPlot({
    ggplot(accd_df %>% group_by(Year) %>% summarise(Num = n()), aes(x = Year, y = Num)) + geom_point(size = 3) +
      geom_line() + scale_x_discrete(limits=2005:2014)
  })
  
  output$accd_plot_year_bar <- renderPlot({
    ggplot(accd_df %>% mutate(Month = as.numeric(month(Date)), Season = as.character((Month-1) %/% 3 +1)) %>% select(Year, Month, Season),
           aes(x = Year)) + geom_bar(aes(fill = Season)) + scale_x_discrete(limits=2005:2014) +
      ylab("Accident Numbers") + xlab("Years") +
      scale_fill_manual(labels = c('1' = 'Jan. - Mar.','2' = 'Apr. - Jun.','3'='Jul. - Sept.','4'='Oct. - Dec.'), values =
                                                                     c('grey','orange','red','purple'))
  })
  
  output$ratio_london <- renderPlot({
    ggplot(accd_df %>% group_by(Year) %>% mutate(Num_by_year=n()) %>% group_by(Year, Authority) %>% mutate(Num_city_year = n()) %>%
             distinct(Authority, .keep_all = TRUE) %>% filter(Authority %in% London_boros) %>% mutate(ratio = Num_city_year/Num_by_year) %>%
             group_by(Year) %>% mutate(london_ratio = sum(ratio)*100) %>% distinct(london_ratio), aes(x=Year, y=london_ratio)) +
      geom_point(size = 3) + geom_line() + scale_x_discrete(limits=2005:2014) + ylab("Percentage of London") + xlab("Years")
  })
  
  output$road_surf_cond <- renderPlot({
    ggplot(accd_df %>% mutate(Month = as.numeric(month(Date))) %>% select(Year, Month, Road_Surf_Cond), aes(x=Month)) +
      geom_bar(aes(fill=Road_Surf_Cond)) + facet_wrap( ~ Year) + scale_x_discrete(limits=1:12) + ylab("Accident Numbers") +
      scale_fill_manual(labels = c('Others', 'Dry', 'Flood','Frost/Ice', 'Snow', 'Wet/Damp'),
                        values = c('red','grey','green','white','blue','purple'))
  })
  
  output$accd_plot_day_bar <- renderPlot({
  ggplot(accd_df %>% select(Day_of_Week, Time) %>% mutate(Hours = as.character(Time %/% 4)), aes(x = Day_of_Week)) +
    geom_bar(aes(fill = Hours)) + scale_x_discrete(limits=c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat")) + ylab("Accident Numbers") +
    xlab("The Day of a Week") + scale_fill_manual(labels = c('0' = '0-3','1' = '4-7','2'='8-11','3'='12-15','4'='16-19','5'='20-23'), values =
                                                    c('red','orange','green','blue','purple','yellow','grey'))
  })
  
  output$accd_plot_month <- renderPlot({
    ggplot(accd_df %>% mutate(Month_num = as.numeric(month(accd_df$Date))) %>%
             group_by(Month_num) %>% summarize(mean_num = n()), aes(x = Month_num, y = mean_num)) + xlab('Month') +
      ylab('Total Number of the Accidents') + geom_point(size = 3) + geom_line() +
      scale_x_discrete(limits=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec"))
  })
  
  output$accd_plot_day <- renderPlot({
    ggplot(accd_df %>% group_by(Day_of_Week) %>% summarise(mean_num = n()/n_distinct(Date)) %>%
                     mutate(Week = c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun")), aes(x = Day_of_Week, y = mean_num)) +
      xlab('Day') + ylab('Mean Number of Accidents') +  geom_point(size = 3) + geom_line() +
      scale_x_discrete(limits=c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun"))
  })
  
  output$accd_plot_hour <- renderPlot({
    ggplot(data = (accd_df %>% group_by(Time) %>% summarize(mean_num = n()/n_distinct(accd_df$Date))), aes(x = Time, y = mean_num)) +
      xlab('The Hour in a Day') + ylab('Mean Number of the Accidents') + geom_line() +geom_point(size = 3)
  })
  
  output$accd_hour_day <- renderPlot({
    ggplot(accd_df %>% select(Time, Day_of_Week), aes(x=Time)) + geom_bar() + xlab('The Hour in a Day') + ylab('Number of the Accidents') +
      facet_wrap(~Day_of_Week, labeller = as_labeller(c('1'="Sun", '2'='Mon', '3'='Tues','4'= "Wed",'5'= "Thurs",'6'= "Fri",'7'= "Sat")))
  })

  output$accd_map_sel <- renderLeaflet({
    leaflet(data = (accd_df %>% filter(city_name == input$accd_maps_sp))) %>%
      addTiles() %>% addCircles(~Long, ~Lat, radius = 1, layerId = 1, fill = TRUE, highlightOptions(highlightOptions(color = "white", weight = 2)), fillOpacity = 0.5)
  })
  
  output$accd_city_r <- renderPlot({
    ggplot(accd_df %>% filter(city_name == input$accd_maps_sp), aes(x=r_dis)) +
      geom_histogram(binwidth = 0.2) + coord_cartesian(xlim=c(0:15)) + xlab("Distance to City Center/Km") +
      ylab("Number of Accidents")
  })
  
  output$accd_r_mean <- renderPlot({
    ggplot(accd_df %>% filter(city_name %in% city_choi_small) %>% group_by(Authority) %>% mutate(r_mean = sum(r_dis)/n()) %>% distinct(Authority, .keep_all = TRUE),
           aes(x=r_mean)) + geom_histogram(binwidth = 0.3) + coord_cartesian(xlim = c(0, 12)) + xlab("Mean Distance to City Center/Km") +
      ylab("Number of Cities")
  })
  
  
  output$accd_map_time_sp <- renderLeaflet({
    leaflet(accd_df %>% filter((city_name == input$accd_maps_ts | input$accd_maps_ts =='All' |
                                  (input$accd_maps_ts =='Greater London' & Authority %in% London_boros)) & #if it belongs to London Boro
                                 (input$accd_year_ts == 'All' | as.character(Year) == input$accd_year_ts) &
                                 (input$slider_month == 0 | month(Date) == as.character(input$slider_month)) &
                                 (input$slider_day == 0 | Day_of_Week == input$slider_day) &
                                 (input$slider_hour == 0 | Time == input$slider_hour))) %>% 
      addTiles() %>% addCircles(~Long, ~Lat, radius = 1,
                                fill = TRUE, highlightOptions(highlightOptions(color = "white", weight = 2)),
                                fillOpacity = 0.5)
  })
})