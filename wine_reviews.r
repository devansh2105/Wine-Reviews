---
title: "Project1"
author: "Devansh Chadngothia, Milica Cvrkota, and Timothy Brennhofer"
date: "2/16/2019"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE) ## Keep this, it sets up options for the rest of the document
```

```{r cars}
#setup
install.packages("ggplot2")
install.packages("mosaic")
install.packages("manipulate")
install.packages("plotly")
install.packages("countrycode")
library(ggplot2)
library(mosaic)
library (manipulate)
library(plotly)
library(countrycode)
```

```{r} 
#reading data and cleaning data
WineReviews_raw <- read.csv("/Users/devansh/Downloads/winemag-data-130k-v2.csv")
head(WineReviews_raw)
WineReviews <- select (WineReviews_raw, -X, -description, -designation, -region_1, -region_2, -taster_twitter_handle)
head(WineReviews)


#data visualization
#countries
countries <-WineReviews %>% group_by(country) %>% filter(n() >1000)
countries2 <-WineReviews %>% group_by(country) %>% filter(n() <=1000)
#not showing 2,567 rows/entries (ones that have less than 1000 wines are not included) in the 2 visuals

wine <- filter (WineReviews, price < 250)
wine2<- filter( WineReviews, price >=250)
#not showing 505 entries for price <250 because of the distribution
#distribution of wines amongst countries
ggplot(countries) + geom_bar(mapping=aes(country), fill="steelblue") + labs (title= "Countries") + theme(axis.text.x = element_text(angle = 90))
#distribution of points
ggplot (WineReviews) + geom_density(aes (points), col= "red", fill = "steelblue")
#distribution of price
ggplot (wine) + geom_density(aes (price), col= "hotpink", fill = "steelblue")
#showing how many reviews each taster made
ggplot(WineReviews,mapping=aes(taster_name))+ geom_bar(fill = "steelblue") + theme(axis.text.x = element_text(angle = 90))



#distribution of points and price
ggplot(WineReviews, aes(x=points, y=price), col="red", fill="grey") + geom_point(shape = 5, color = "red")

#do wines in certain countries tend to have higher prices? 
wine_by_country <- group_by(WineReviews, country)
#deleting rows with missing price and point information
wine_by_country <- wine_by_country[complete.cases(wine_by_country$price),] #removing rows with missing price
wine_by_country <- wine_by_country[complete.cases(wine_by_country$points),] #removing missing rows with missing points

price_and_points <- summarize (wine_by_country, meanPrice = round(mean(price), digits = 2), meanPoints = round(mean(points), digits = 2), count =n()) 
price_and_points <- price_and_points[complete.cases(price_and_points$country),] #removing rows with missing countries
price_and_points <- price_and_points[-c(1),]

#Add Continent Data
price_and_points$continent <- countrycode(sourcevar = price_and_points$country,
                            origin = "country.name",
                            destination = "continent")
#Correct England
price_and_points[14,"continent"] <- "Europe"

#prep color list for continent grouping
color_prep <- c("darkorange", "blue", "forestgreen", "red", "darkviolet")
plot_ly (price_and_points, x = ~price_and_points$meanPrice, y = ~price_and_points$meanPoints,  alpha = 0.5) %>% 
  add_markers(size = ~count, 
              sizes = c(50,7000), 
              text = ~paste (country, "-", continent, "<br />", "Avg. Price (USD):   ", "$",meanPrice, "<br />", "Avg. Rating:   ", meanPoints),
              hoverinfo="text", 
              color =  ~continent, 
              colors = ~color_prep) %>%
  layout(title = "Average Wine Enthusiast Rating and Price",
         legend = list(x = .75, y =.25, title = "Region of Origin"), 
         yaxis = list(title = "Avg. Points Rating"), 
         xaxis = list(title = "Avg. Bottle Price"))

## where is the best wine in the states
#reading new data to combine it with our WineReviews
states <- read.csv("https://raw.githubusercontent.com/jasonong/List-of-US-States/master/states.csv")
states <- select (states, -Abbreviation)
head (states)
wine_by_country <- wine_by_country[complete.cases(wine_by_country$price),] #removing rows with missing price

USA_wines <- filter (wine_by_country, country == "US")
USA_wines <- USA_wines [complete.cases(USA_wines$price),]
USA_wines <- group_by(USA_wines, province)
USA_wines <- summarize (USA_wines, meanPrice = round(mean(price), digits = 2), meanPoints = round(mean(points), digits = 2), count =n())
USA_wines <- mutate(USA_wines, ratio = meanPoints/meanPrice)

USA_wines <- left_join(x = states, y= USA_wines, by=c("State" = "province"))
USA_wines <- USA_wines [-c(9),]

ratio <- round(USA_wines$ratio, digits =2)
g <- list (scope = 'usa', projection = list(type ='albers usa'), lakecolor = toRGB('white'))

plot_geo() %>%
  add_trace( z = ~ratio, text = state.name, locations = state.abb, locationmode = 'USA-states') %>%
  add_markers(x = state.center[["x"]], y = state.center[["y"]],size = I(2), symbol = I(8), color = I("white"), hoverinfo = "none") %>%
  layout(geo = g)
```
