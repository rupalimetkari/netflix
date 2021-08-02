# print name
Name <-("Rupali Shivaji Metkari")  
Name

# import  library
library(ggplot2) 
library(plyr)
library(dplyr)
library(magrittr)
library(tidyverse)
install.packages("scales")


df_netflix <- read.csv("D:/Assignmentas/corona/netflix_titles.csv")
netflix_titles

df_netflix$date_added <- as.Date(df_netflix$date_added, format = "%B %d, %Y")


#Display the head, tail and structure of <bio>
head(df_netflix)
tail(df_netflix)
str(df_netflix) 
summary(df_netflix)


df_by_date <- netflix_titles %>% group_by(date_added,type) %>% summarise(addedToday = n()) %>% 
ungroup() %>% group_by(type) %>% mutate(Total_Number_of_Shows = cumsum(addedToday), label = if_else(date_added == max(date_added,na.rm = T), as.character(type), NA_character_))

df_by_date  %>% 
  ggplot(aes(x = date_added, y = Total_Number_of_Shows, color = type)) + geom_line(size = 2) + 
  theme_bw(base_size = 20) + 
  scale_x_date(date_breaks = '2 years', date_labels = "%Y") + 
  theme(legend.position = 'none') +
  geom_text_repel(aes(label = label), size = 8,na.rm = TRUE, nudge_y = 100)
