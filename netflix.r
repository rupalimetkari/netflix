#import  library
library(ggplot2) 
library(plyr)
library(dplyr)
library(magrittr)
library(tidyverse)
library(scales)

#loaded netflix.csv
netflix <- read.csv("D:/Assignmentas/corona/Netflix Shows.csv")

#Display the head, tail and structure of <bio>
head(netflix)
tail(netflix)
str(netflix) 
summary(netflix)
table(netflix$rating)
a<-unique(netflix$rating)
length(a)

#find null values
is.na(netflix)
netflix[is.na(netflix)] <- names(sort(-table(netflix$user.rating.score)))[1]
is.na(netflix)

#plot histogram for rating
ggplot(netflix, aes(x = rating )) + geom_histogram(stat = "count")  + ggtitle("Rating V/S Count on entire dataset")

# draw pie chart
slices <- table(netflix$rating)
lbls <- levels(netflix$rating)
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) 
lbls <- paste(lbls,"%",sep="") 
pie(slices,labels = lbls, col=rainbow(length(lbls)),main="Pie Chart of NetFlix Rating by Category")


#Lets see when the movies / shows are released on netflix
releases_per_year <- netflix %>% select(title, release.year) %>% group_by(release.year) %>% summarise( movies_per_year = n()) %>% arrange(desc(movies_per_year))

ggplot(releases_per_year, aes(x = (release.year), y = movies_per_year)) + geom_bar(stat = "identity") + geom_smooth( se = F) + xlab("Year") + ylab("Count") + ggtitle("Shows released per year on Netflix")

#Which type of show / movie is mostly common year on year basis.
ratings_per_year <- netflix %>% group_by(rating, release.year) %>% summarise(count = n())
ratings_per_year <- ratings_per_year %>% group_by(release.year) %>% filter(count == max(count))
ggplot(ratings_per_year, aes(x= release.year, y = count, fill = rating)) + geom_bar(stat = "identity") + xlab("Release Year") + ylab("Number of shows in the category") + ggtitle("Most common ratings of shows year on year basis")

#avg user rating score per rating category
avg_score <- netflix %>% group_by(rating) %>% summarise(avg_score = mean(user.rating.score, na.rm = T))

ggplot(avg_score, aes(x = as.factor(rating), y = avg_score)) + geom_bar(stat = "identity") + xlab("Category") + ylab("Average user score") + ggtitle("Mean user score per category")

ggplot(netflix,aes(x=release.year)) + geom_bar(stat='count') + scale_x_continuous(limits=c(1995,2017))
ggplot(netflix,aes(x=release.year,y=user.rating.size)) + geom_bar(stat='identity') + scale_x_continuous(limits=c(1995,2017))


