library(ggplot2)
library(dplyr)
library(lubridate)
library(leaflet)
library(maps)
library(googleVis)
library(ggthemes)
setwd("/Users/Strider/Shiny_Project/ShinyProj_Guanjie/UK_traffic_and_accidents_10years/")

#load the three csvs have the accidents record in UK from 2005 to 2014 without 2008.
raw_df_ac_part1 <-  fread.csv('accidents_2005_to_2007.csv', stringsAsFactors = FALSE, header = TRUE)
raw_df_ac_part2 <-  fread.csv('accidents_2009_to_2011.csv', stringsAsFactors = FALSE, header = TRUE)
raw_df_ac_part3 <-  fread.csv('accidents_2012_to_2014.csv', stringsAsFactors = FALSE, header = TRUE)
UK_ct = read.csv('england_ct_2011.csv', header = TRUE, stringsAsFactors = FALSE) #load the UK counties
district_code <-  fread('infuse_dist_lyr_2011.csv', stringsAsFactors = FALSE, header = TRUE)
#combine these three date frames into one. They have exactly column names and column numbers. Create df_accd.
raw_df <- rbind(rbind(raw_df_ac_part1, raw_df_ac_part2), raw_df_ac_part3)

#change the column Date format from character like "30/01/2005" to Date "2005"
#change the column names of Longitude and Latitude to "Long" and "Lat"
#change the column name of Local_Authority_.district
#select the columns that are used in this shiny app
accd_df <- raw_df %>% select(Long = Longitude, Lat = Latitude, Num_Cal = Number_of_Casualties, Date,
                             Day_of_Week, Time, Authority = `Local_Authority_(Highway)`, Year)
accd_df$Date <- as.Date(accd_df$Date, format = "%d/%m/20%y")

#get the names of each district
district_code <- district_code %>% select(Authority = geo_code, city_name = name)
df_city_choi <-  accd_df %>% distinct(Authority) %>% left_join(district_code, by = "Authority") %>% filter(city_name != 'NA')
accd_df <- accd_df %>% left_join(df_city_choi, by = "Authority")

#load the tracfic flow records in UK from 2000 to 2016. Create data frame df_traf.
#raw_df_traf <- fread.csv('ukTrafficAADF.csv', stringsAsFactors = FALSE, header = TRUE)
#trac_df <- raw_df_traf
#The plot of years by seasons
output$accd_plot_year_bar <- renderPlot({
  ggplot(accd_df %>% mutate(Month = as.numeric(month(Date)), Season = as.character((Month-1) %/% 3 +1)) %>% select(Year, Month, Season),
         aes(x = Year)) + geom_bar(aes(fill = Season)) + scale_x_discrete(limits=2005:2014) + ylab("Accident Numbers")
})

###### The following code plot the Number of accidents for each year
#count_acc_year = (accd_df %>% group_by(Year) %>% summarise(Num = n()))
accd_plot_year <- ggplot(data = (accd_df %>% group_by(Year) %>% summarise(Num = n())), aes(x = Year, y = Num)) + geom_point(size = 3) +
    geom_line() + scale_x_discrete(limits=2005:2014) #point_plot
###### The plot ends here
###### The following code plot the Number of casualties for each year
#count_Cas_year = (accd_df %>% group_by(Year) %>% summarise(Num = sum(Number_of_Casualties)))
accd_plot_casu <- ggplot(data = (accd_df %>% group_by(Year) %>% summarise(Num = sum(Number_of_Casualties))), aes(x = Year, y = Num)) + geom_point(size = 3) +
  geom_line() + scale_x_discrete(limits=2005:2014)
###### The plot ends here
###### The following code plot the Number of casualties for each year
#mean_Cas_year = (accd_df %>% group_by(Year) %>% summarise(Num = mean(Number_of_Casualties)))
accd_plot_casu_mean <- ggplot(data = (accd_df %>% group_by(Year) %>% summarise(Num = mean(Number_of_Casualties))), aes(x = Year, y = Num)) + geom_point(size = 3) +
  geom_line() + scale_x_discrete(limits=2005:2014)#point_plot
###### The plot ends here

#The following code plot the mean Number of accidents for each day in a week
accd_plot_day <- ggplot(data = (accd_df %>% group_by(Day_of_Week) %>% summarise(mean_num = n()/n_distinct(Date)) %>%
                 mutate(Week = c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun"))), aes(x = factor(Week), y = mean_num)) +
  xlab('Days') + ylab('Mean Number of Accidents') + geom_point(size = 3) +
  scale_x_discrete(limits=c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun"))
#The plot ends here

#The following code plot the mean hour of accidents fo each hour in a day
accd_plot_Hour <- ggplot(data = (accd_df %>% mutate(Hour = as.numeric(substr(accd_df$Time, 1, 2))) %>%
                 group_by(Hour) %>% summarize(mean_num = n()/n_distinct(accd_df$Date))), aes(x = Hour, y = mean_num)) +
  xlab('Hour in a Day') + ylab('Mean Number of the Accidents') + geom_point(size = 3)
#The plot ends here

#The road suf cond plot
ggplot(accd_df %>% mutate(Month = as.numeric(month(Date))) %>% select(Year, Month, Road_Surf_Cond), aes(x=Month)) +
  geom_bar(aes(fill=Road_Surf_Cond)) + facet_wrap( ~ Year) + scale_x_discrete(limits=1:12) + ylab("Accident Numbers") +
  scale_fill_manual(labels = c('Others', 'Dry', 'Flood','Frost/Ice', 'Snow', 'Wet/Damp'),
                    values = c('red','grey','green','brown','blue','purple'))

#The following code plot the mean Month of accidents for each month
accd_plot_Hour <- ggplot(accd_df %>% mutate(Month_num = as.numeric(month(accd_df$Date))) %>%
         group_by(Month_num) %>% summarize(mean_num = n()/(n_distinct(accd_df$Year))), aes(x = Month_num, y = mean_num)) + xlab('Month') + ylab('Mean Number of the Accidents') + geom_point(size = 3) +
  scale_x_discrete(limits=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec"))
#The plot ends here

#the following code print the Days by Hour
ggplot(accd_df %>% select(Day_of_Week, Time) %>% mutate(Hours = as.character(Time %/% 4)), aes(x = Day_of_Week)) +
  geom_bar(aes(fill = Hours)) + scale_x_discrete(limits=c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun")) + ylab("Accident Numbers") +
  xlab("The Day of a Week") + scale_fill_manual(labels = c('0' = '0-3','1' = '4-7','2'='8-11','3'='12-15','4'='16-19','5'='20-23'), values =
                                                  c('red','orange','green','blue','purple','yellow','grey'))

###plot of each year facet wrap
ggplot(accd_df %>% mutate(Month = as.numeric(month(Date))) %>% select(Year, Month), aes(x=Month)) +
  geom_bar() + facet_wrap( ~ Year) + scale_x_discrete(limits=1:12) + ylab("Accident Numbers")

#####################The following code use leaflet package to map
#try a small sample, pick year 2005 and all the Jans 
#accd_df_2005 <- accd_df %>% filter(Year == 2005) %>% filter(Day_of_Week == 1) %>%
 # group_by(District) %>% mutate(Num_accd = n()) %>% arrange(District, Location_Easting_OSGR)
#colnames(accd_df_2005)[14] = "code"
#UK_ct = UK_ct %>% select(code, name)
#left join the UK_ct to give the city names

#get 
district_code <-  fread('infuse_dist_lyr_2011.csv', stringsAsFactors = FALSE, header = TRUE)
View(district_code)
district_code <- district_code %>% select(Authority = geo_code, city_name = name)
df_city_choi = accd_df %>% select(Authority) %>% distinct(Authority) %>% left_join(district_code, by = "Authority") %>% filter(city_name != 'NA')


  ggplot(accd_df %>% group_by(Year) %>% summarise(Num = n()), aes(x = Year, y = Num)) + geom_point(size = 3) +
    geom_line() + scale_x_discrete(limits=2005:2014)
  
temp = accd_df %>% mutate(Month = as.numeric(month(Date))) %>% select(Year, Month) %>% mutate(Season = ((Month-1) %/% 3 +1))
#%>% group_by(Year)


temp =accd_df %>% group_by(Year) %>% mutate(Num_by_year=n()) %>% group_by(Year, Authority) %>% mutate(Num_city_year = n()) %>%
  distinct(Authority, .keep_all = TRUE) %>% filter(Authority %in% London_boros) %>% mutate(ratio = Num_city_year/Num_by_year) %>%
  group_by(Year) %>% mutate(london_ratio = sum(ratio)) %>% distinct(london_ratio)

ggplot(data = (accd_df %>% group_by(Time) %>% summarize(mean_num = n()/n_distinct(accd_df$Date))), aes(x = Time, y = mean_num)) +
  xlab('The Hour in a Day') + ylab('Number of the Accidents') + geom_point(size = 3)

ggplot(accd_df %>% select(Time, Day_of_Week), aes(x=Time)) + geom_bar() +
  facet_wrap(~Day_of_Week, labeller = as_labeller(c('1'='Mon', '2'='Tues', '3'="Wed",'4'= "Thurs",'5'= "Fri",'6'= "Sat",'7'= "Sun"))) 

ggplot(accd_df %>% group_by(Time) %>% mutate(Num = n()), aes(x = Time, y = Num )) +
  xlab('The Hour in a Day') + ylab('Total Number of the Accidents') + geom_line() + geom_point(size = 3)
#####################

View(citis_UK)
citis_uk <-  fread('Local_Administrative.csv', stringsAsFactors = FALSE, header = TRUE)
citis_uk = citis_uk %>% select(Authority = lau118cd, Long0=long, Lat0=lat)
accd_df = accd_df %>% left_join(citis_uk, by = "Authority") %>% mutate(r_dis = 6300*pi*sqrt((Long-Long0)^2+(Lat-Lat0)^2)/180)

ggplot(accd_df %>% filter(city_name %in% city_choi_small) %>% group_by(Authority) %>% mutate(r_mean = sum(r_dis)/n()) %>% distinct(Authority, .keep_all = TRUE),
       aes(x=r_mean)) + geom_histogram(binwidth = 0.3) + coord_cartesian(xlim = c(0, 12))
  
  
  accd_df %>% filter(city_name %in% city_choi_small) %>% group_by(Authority) %>% mutate(r_mean = sum(r_dis)/n()) %>% distinct(Authority, .keep_all = TRUE)
