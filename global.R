library(DT)
library(ggplot2)
library(dplyr)
library(lubridate)
library(leaflet)
library(maps)
library(data.table)
setwd("/Users/Strider/Shiny_Project/ShinyProj_Guanjie/UK_traffic_and_accidents_10years/")


#load the three csvs have the accidents record in UK from 2005 to 2014 without 2008.
raw_df <-  fread('accident.csv', stringsAsFactors = FALSE, header = TRUE)
#UK_ct = fread('england_ct_2011.csv', header = TRUE, stringsAsFactors = FALSE) #load the UK counties
district_code <-  fread('infuse_dist_lyr_2011.csv', stringsAsFactors = FALSE, header = TRUE)
citis_uk <-  fread('Local_Administrative.csv', stringsAsFactors = FALSE, header = TRUE) #this csv has the Long and lat for each city
citis_uk = citis_uk %>% select(Authority = lau118cd, Long0=long, Lat0=lat)

#combine these three date frames into one. They have exactly column names and column numbers. Create df_accd.
#raw_df <- rbind(rbind(raw_df_ac_part1, raw_df_ac_part2), raw_df_ac_part3)
#accd_df <- as.data.frame(raw_df)

#change the column Date format from character like "30/01/2005" to Date "2005", Longitude and Latitude to "Long" and "Lat"
#change the column name of Local_Authority_.district, select the columns that are used in this shiny app
accd_df <- raw_df %>% select(Long = Longitude, Lat = Latitude, Num_Cal = Number_of_Casualties, Date,
                             Day_of_Week, Time, Authority = `Local_Authority_(Highway)`, Year, Road_Surf_Cond = Road_Surface_Conditions)
accd_df$Date <- as.Date(accd_df$Date, format = "%d/%m/20%y")

#get the names of each district, obtain a data frame of city names and authority code. mutate accd_df with a new column named city_name
#note: some of the values in accd_df$city_name are 'NA'
district_code <- district_code %>% select(Authority = geo_code, city_name = name)
df_city_choi <-  accd_df %>% distinct(Authority) %>% left_join(district_code, by = "Authority") %>% filter(city_name != 'NA')
accd_df <- accd_df %>% left_join(df_city_choi, by = "Authority") %>% left_join(citis_uk, by = "Authority") %>% mutate(r_dis = 6300*pi*sqrt((Long-Long0)^2+(Lat-Lat0)^2)/180)
accd_df$Time = as.numeric(substr(accd_df$Time, 1, 2))
#creat choices
year_choi = c('All', '2005', '2006', '2007', '2009', '2010', '2011', '2012', '2013', '2014')
city_choi = df_city_choi[,"city_name"]
city_choi = sort(city_choi)
city_choi = append(city_choi, 'Greater London')
city_choi = append(city_choi, 'All')
city_choi_small = accd_df %>% filter(r_dis != 'NA') %>% distinct(city_name) %>% filter(city_name!='NA')
city_choi_small = city_choi_small[,"city_name"]
city_choi_small = sort(city_choi_small)
London_boros = c(paste0('E0900000',1:9), paste0('E090000',10:33))
#it will be so slow to print the whole map, as ALL


###### The following code plot the Number of accidents for each year
#count_acc_year = (accd_df %>% group_by(Year) %>% summarise(Num = n()))
###### The plot ends here
###### The following code plot the Number of casualties for each year
#count_Cas_year = (accd_df %>% group_by(Year) %>% summarise(Num = sum(Number_of_Casualties)))
#accd_plot_casu <- ggplot(data = (accd_df %>% group_by(Year) %>% summarise(Num = sum(Number_of_Casualties))), aes(x = Year, y = Num)) + geom_point(size = 3) +
 # geom_line() + scale_x_discrete(limits=2005:2014)
###### The plot ends here
###### The following code plot the Number of casualties for each year
#mean_Cas_year = (accd_df %>% group_by(Year) %>% summarise(Num = mean(Number_of_Casualties)))
#accd_plot_casu_mean <- ggplot(data = (accd_df %>% group_by(Year) %>% summarise(Num = mean(Number_of_Casualties))), aes(x = Year, y = Num)) + geom_point(size = 3) +
#  geom_line() + scale_x_discrete(limits=2005:2014)#point_plot
###### The plot ends here

###### The following code plot the Number of accidents for each year
#count_acc_year = (accd_df %>% group_by(Year) %>% summarise(Num = n()))
#####################The following code use leaflet package to map
#try a small sample, pick year 2005 and all the Jans 
#accd_df_2005 <- accd_df %>% filter(Year == 2005) %>% filter(Day_of_Week == 1) %>%
#  group_by(District) %>% mutate(Num_accd = n()) %>% arrange(District, Location_Easting_OSGR)

#colnames(accd_df_2005)[14] = "code"
#UK_ct = UK_ct %>% select(code, name)

#left join the UK_ct to give the city names
#temp = accd_df_2005 %>% left_join(UK_ct, by = 'code') %>% group_by(code) %>% summarise(Num_accd = n()) %>% full_join(UK_ct, by = 'code')
#View(temp)
#temp = temp %>% filter(name != 'NA')