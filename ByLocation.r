library(readxl)
library(reshape2)
library(ggplot2)
library(plotly)
library(lubridate)
library(dplyr)
setwd("/Users/Sangeetha/Downloads/")


actuaries <- read_excel("Actuaries-Climate-Index-Values-Through-May-2017_Spring-2017_English.xlsx", sheet = 2)

#actuaries <- actuaries[, -1]
actuaries <- na.omit(actuaries)

#two data structures {years, months}
#2 for loops
#concatenation in inner loop

years <- 1961:2017
months <- 1:12

#format(Sys.time(), "%a %b %d %X %Y")


field_names <- c("Sea_Level_Monthly")

for(i in 1:length(years)){
  
  #print(years[i])
  
  for(j in 1:length(months)){
    
    #date tmp in YYYY-MM-DD
    tmp <- paste0(years[i], "-", months[j], "-1")
    
    
    #print(tmp)
    
    field_names <- c(field_names, tmp)
    
  }
  
}

#trim the last 7 months
field_names <- field_names[1:(length(field_names)-7)]

colnames(actuaries) <- field_names


actuaries_part1 <- actuaries[4,]


actuaries <- melt(actuaries, id=c("Sea_Level_Monthly", "value"))
View(actuaries)

longform<-actuaries
View(longform)

names(longform) <- c("region", "month_date", "sea_level")
longform$month_date <- as.Date(longform$month_date)


longform <- longform %>% arrange(region, month_date) 


View(longform)
sectioned_plot <- ggplot( data = longform, aes(y=sea_level, x=month_date) ) + geom_line() + facet_grid(.~region )
plot(sectioned_plot)





#actuaries_part1 <- melt(actuaries_part1, id=c("Sea_Level_Monthly"))
#View(actuaries_part1)
#actuaries_part1 <- actuaries_part1[, -1]
#names(actuaries_part1) <- c("month_date", "sea_level")
#actuaries_part1$month_date <- as.Date(actuaries_part1$month_date)


plot1 <- ggplot(data = actuaries_part1, aes(x = month_date, y = sea_level)) + geom_line()

ggplotly(plot1)

View(actuaries_part1)



#sectionning in time series
#facetting
actuaries_part1$year <- format(actuaries_part1$month_date, "%Y")

#interval must be years
interval = 10

#lets create a section column which has the same value for all years in the same interval.
#intervals being [1961-1970], [1971, 1980], etc..

actuaries_part1$year <- as.integer(actuaries_part1$year)
actuaries_part1 <- actuaries_part1 %>% filter(year <= 2010)

sections <- actuaries_part1$year %>% unlist() %>% c()

interval <- interval*12

sections <- c()

i <- 1
j <- 1

for(i in 1:nrow(actuaries_part1)){
  sections <- c(sections, j)
  
  if( (i%%interval) == 0)
    j <- j + 1
  
  i <- i + 1 
  
}

actuaries_part1$section <- sections
actuaries_part1$section <- paste("Section", actuaries_part1$section)


plot_data <- actuaries_part1[, c("sea_level", "section", "month_date")]


sectioned_plot <- ggplot( data = plot_data, aes(x=sea_level, y=month_date) ) + geom_line() + facet_grid(.~section )

print(sectioned_plot)
