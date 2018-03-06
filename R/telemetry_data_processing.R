############################################################################
####
#### Batch Telemetry Data Processing for Triangulation via LOAS
####              Bryant Dossman, bd342@cornell.edu
####
############################################################################
# This data can likely be proccessed and analyzed completely through R, 
# however, at the moment, LOAS has features that are favorable to current
# R packages. 

# FIX: Current code is written to process 2018 data, but will be made to
# batch process multiple years of data simulataneously 

#### loading required libraries
library(tidyverse)
library(sp)

#### reading in data files

fh <- read_csv("~/Desktop/fh.csv") # font hill grid points

fh <- fh[!duplicated(fh$plotlocation),]

ht <- read_csv("~/Desktop/ht_2018.csv") # telemtry data


ht <- ht %>%  mutate(ts =  as.POSIXct(paste(date, time, sep=" "), 
                                 format="%d-%b-%y %H:%M"))


ht$location <- str_replace(ht$location, "-", " ")

ht$plotlocation <- paste(ht$plot, ht$location, sep=" ")

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

ht$plotlocation <- trim(ht$plotlocation)

ht$longitude <- abs(ht$longitude)*-1 # enures that latitude is in the western hemisphere

true_ht <- ht %>% filter(truelocation=="Y") %>% 
  select(ts, tagID, fix, bearing, longitude, latitude, plotlocation)

new_ht <- ht %>% filter(truelocation=="N") %>% left_join(fh[,c(1,4,5)], by="plotlocation") %>%
  select(ts, tagID, fix, bearing, longitude.x, latitude.x,longitude.y, latitude.y, plotlocation)

names(new_ht)[7:8] <- c("longitude","latitude")

complete_ht <- new_ht %>% mutate(day = format(ts, "%j"))

for(i in 1:nrow(complete_ht)){
  if(!is.na(complete_ht$longitude.x[i])){
    complete_ht$longitude[i] = complete_ht$longitude.x[i]
    complete_ht$latitude[i] = complete_ht$latitude.x[i]
    }
}

print(complete_ht[is.na(complete_ht$longitude),c("plotlocation")], n=50) %>% distinct(plotlocation)

complete_ht <- complete_ht %>% mutate(GID = paste(tagID,fix,day,sep="-")) %>% 
  select(GID,bearing,longitude,latitude, tagID, ts)

mean_times <- complete_ht %>% group_by(GID) %>% summarise(avg_ts = mean(ts, na.rm=T))

complete_ht <- complete_ht %>% left_join(mean_times, by="GID")

complete_ht <- complete_ht[complete.cases(complete_ht),]

latlongcoor<-SpatialPoints(cbind(complete_ht$longitude,complete_ht$latitude), proj4string=CRS("+proj=longlat"))

utmcoor<-spTransform(latlongcoor,CRS("+proj=utm +zone=18")) %>% as.data.frame()

names(utmcoor) <- c("y","x")

complete_ht <- cbind(complete_ht,utmcoor)

complete_ht <- complete_ht %>% select(GID,x,y,bearing,avg_ts,tagID)

write.csv(complete_ht, "~/Desktop/ht2018.csv", row.names = F)


#### Bringing back Estimated locations and merging with True locations

locs <- read_csv("locs2018.csv")

sub <- true_ht %>% select(tagID,longitude,latitude,ts)

locs <- locs %>% select(tagID,X_Estimate,Y_Estimate, avg_ts)

names(locs) <- c("tagID","longitude","latitude","ts")

locs <- rbind(locs,sub)

ggplot(data=locs %>% filter(longitude < -77.88, longitude > -77.96 & latitude < 18.1), 
       aes(x=longitude, y=latitude, colour=factor(tagID))) + 
  geom_point()# +
  #geom_path()

table(locs$tagID)
