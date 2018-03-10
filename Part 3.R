### Function reference: Latitude Longitude to State Name - stackoverflow
###https://stackoverflow.com/questions/8751497/latitude-longitude-coordinates-to-state-code-in-r
latlong2state <- function(pointsDF) {
  states <- map('state', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
  states_sp <- map2SpatialPolygons(states, IDs=IDs,proj4string=CRS("+proj=longlat +datum=WGS84"))
  pointsSP <- SpatialPoints(pointsDF,proj4string=CRS("+proj=longlat +datum=WGS84"))
  indices <- over(pointsSP, states_sp)
  stateNames <- sapply(states_sp@polygons, function(x) x@ID)
  stateNames[indices]
}

install.packages('ggmap')
install.packages("sp")
install.packages("devtools")
install.packages("ggplot2")
install.packages("stringr")
install.packages("maptools")
install.packages("maps")
install.packages("googlevis")
library(devtools)
install_github("geoffjentry/twitteR")
require("twitteR")
library(stringr)
library(ggmap)
library(sp)
library(maps)
library(maptools)
library(googleVis)
suppressPackageStartupMessages(library(googleVis))

twitteR::setup_twitter_oauth(consumer_key = "CKuP9VUXhKVBmUR4MewoRxeoI", consumer_secret = "A4EohnxM0wZ45Bb12KW79IblyRp4nsbNU2HjtwnFrrutNRg2IB", access_token = "1405595161-sOkfPJofHrjMPlhF9h67E9hxcq3WrAHCqJBjxVl", access_secret = "EXjiyaCSac1iDXx6DRCalb0Of60gbsmTnsea3QY8kg8O8")
tweets <- twitteR::twListToDF(twitteR::searchTwitteR("influenza+flu",n=10000,since="2017-8-1",retryOnRateLimit = 1000))
write.csv(tweets, "tweets_final.csv")
users <- twListToDF(lookupUsers(tweets$screenName))
write.csv(users, "users_final.csv")
subset_users <- subset(users, !(users$location == ""))
subset_users$location <- stri_conv(subset_users$location, to="ascii")
length(subset_users$location)

#geocodes <- read.csv("lat_long.csv")
geocodes <- geocode(subset_users$location[0:2499])
geocodes$screenName <- subset_users$screenName[0:2499]
geocodes <- subset(geocodes, !(geocodes$lat == "NA"))

#User Defined Function
geocodes$state <- latlong2state(data.frame(x=geocodes$lon,y=geocodes$lat))
geocodes <- subset(geocodes, !(geocodes$state == "<NA>"))
final <- merge(tweets,geocodes)
final <- data.frame(final$state)
final$count <- rep(1,nrow(final))
final$state <- final$final.state
final$final.state <- NULL
aggregated <- aggregate(count ~ state,final,sum)
write.csv(aggregated,"states.csv")

gchart <- gvisGeoChart(aggregated, "state","count",options=list(region="US",displayMode="regions",resolution="provinces",width=1000, height=600))
plot(gchart)

