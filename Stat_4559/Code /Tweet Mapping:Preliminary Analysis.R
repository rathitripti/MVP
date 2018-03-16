install.packages("sp")
install.packages("maps")
install.packages("maptools")
install.packages("rgdal")
install.packages("dplyr")
install.packages("ggplot2")
library("sp")
library("maps")
library("maptools")
library("rgdal")
library("dplyr")
library("ggplot2")
getwd()
setwd("/Users/triptirathi/Documents/Stat-Experiment-")
##Plot Tweets on a Map of the US
flu_32 <- read.csv("flu_32.csv", header = TRUE)
par(mar = c(0, 0, 0, 0))
maps::map("state", lwd = .25)
with(flu_32, points(lng, lat, pch = 20, cex = .75, col = rgb(0, .3, .7, .75)))
##goals: determine how to add title to plot

##Convert Latitude and Longitude to State Names
##Resources: https://stackoverflow.com/questions/28421353/how-to-add-hawaii-and-alaska-to-spatial-polygons-in-r
tmp_dl <- tempfile()
download.file("http://www2.census.gov/geo/tiger/GENZ2013/cb_2013_us_state_20m.zip", tmp_dl)
unzip(tmp_dl, exdir=tempdir())
ST <- readOGR(tempdir(), "cb_2013_us_state_20m")

latlong2state <- function(pointsDF) {
  # Just copied the earlier code with some key changes
  states <- ST
  
  # Convert pointsDF to a SpatialPoints object 
  # USING THE CRS THAT MATCHES THE SHAPEFILE
  pointsCRS <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
  pointsSP <- SpatialPoints(pointsDF, proj4string=CRS(pointsCRS))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, states)
  
  # Return the state names of the Polygons object containing each point
  as.vector(indices$NAME)
}

#Function to automate the following process
lat_lng2df <- function(df) {
  df_geo <- df[which((df$lng != "NA" & df$lat != "NA")),]
  df_geo_lat <- df_geo$lat
  df_geo_lng <- df_geo$lng
  testPoints2.0 <- cbind(df_geo_lng,df_geo_lat)
  testPoints3.0 <- data.frame(testPoints2.0)
  df_geo["state"] <- latlong2state(testPoints3.0)
  return(data.frame(df_geo))
}

flu_32_geo <- lat_lng2df(flu_32)
fwrite(flu_32_geo, file = "flu_32_geo.csv")
flu_32_geo <- read.csv("flu_32_geo.csv", header = TRUE)

#Or the Manual way
flu_32_geo <- flu_32[which((flu_32$lng != "NA" & flu_32$lat != "NA")),]
flu_32_lat <- flu_32_geo$lat
flu_32_lng <- flu_32_geo$lng
testPoints2.0 <- cbind(flu_32_lng,flu_32_lat)
testPoints2.0 <- data.frame(testPoints2.0)
flu_32_geo["state"] <- latlong2state(testPoints2.0)
flu_32_geo$state

##CDC Flu Data 
ili <- read.csv("ILINetUPDATED.csv", header = TRUE)
flu_date <- arrange(ili, REGION) #arranges by state
colnames(flu_date) #gathers column names
col_names <- c("REGION", "WEEK","YEAR","X.UNWEIGHTED.ILI","ILITOTAL","NUM..OF.PROVIDERS","TOTAL.PATIENTS") #desired columns
state_ili <- flu_date[,col_names]#state data with only the columns we want
state_ili["ILITOTAL"] <- as.numeric(state_ili$ILITOTAL)
state_ili_w <- state_ili[which(state_ili$YEAR == 2018),]

alabama <- state_ili_w[1:9,]
ggplot(alabama, aes(x = WEEK, y = ILITOTAL, colour = REGION)) + geom_point()
alabama_plot <- ggplot(alabama, aes(x = WEEK, y = ILITOTAL, colour = REGION)) + geom_point()
state_line_plot <- ggplot(state_ili_w[1:36,], aes(x = WEEK, y = ILITOTAL, colour = REGION)) + geom_point()

###Example Linear Regression
tweet_ct_al <- length(flu_32_geo[which(flu_32_geo$state == "Alabama"),]$text)
tweet_ct_az <- length(flu_32_geo[which(flu_32_geo$state == "Arizona"),]$text)
tweet_ct_ar <- length(flu_32_geo[which(flu_32_geo$state == "Arkansas"),]$text)

flu_ct_al <- state_ili_w$ILITOTAL[which(state_ili_w$REGION == "Alabama" & state_ili_w$WEEK == 9)]
flu_ct_az <- state_ili_w$ILITOTAL[which(state_ili_w$REGION == "Arizona" & state_ili_w$WEEK == 9)]
flu_ct_ar <- state_ili_w$ILITOTAL[which(state_ili_w$REGION == "Arkansas" & state_ili_w$WEEK == 9)]
tweets_w9 <- c(tweet_ct_al,tweet_ct_az,tweet_ct_ar)
cdc_w9 <- c(flu_ct_al, flu_ct_az, flu_ct_ar)
test_df <- data.frame(tweets_w9, cdc_w9)

simple_LR <- lm(cdc_w9 ~ ., data = test_df)
summary(simple_LR)##not significant
