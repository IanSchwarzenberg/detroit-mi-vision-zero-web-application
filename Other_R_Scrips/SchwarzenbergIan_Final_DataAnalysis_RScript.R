#CPLN 680 Final Project: Data Analysis Script

rm(list=ls())

library(sf)
library(ggmap)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(tidyverse)
library(stargazer)
library(Hmisc)
library(corrplot)

setwd("C:/Users/Ian/Documents/Documents/University_Of_Pennsylvania/Courses/CPLN_680_AdvancedTopicsGIS/Data")



#LOAD DATA

#Neighborhoods:
neighborhoods <- st_read("geo_export_ec48e3a3-31d4-4ae4-a1fd-a72a3b8c39a8.shp")
neighborhoods <- st_transform(neighborhoods, 4326)

neighborhoods_copy <- st_read("geo_export_ec48e3a3-31d4-4ae4-a1fd-a72a3b8c39a8.shp") #DID THE ACTUAL 
#ANALYSIS WORK ON A COPY OF THE ORIGINAL NEIGHBORHOODS SHAPEFILE INSTEAD OF DOING THAT WORK ON 
#THE THE ORIGINAL NEIGHBORHOODS SHAPEFILE ITSELF. THIS IS BECAUSE ON ANOTHER CLASS'S ASSIGNMENT LAST YEAR, 
#WHEN I WOULD SPATIAL JOIN DIRECTLY TO THE ORIGINAL SHAPEFILE, THAT SHAPEFILE WOULD THEN NOT BECOME 
#MAPPABLE ON SHINY FOR SOME REASON. TO SOLVE THIS, I FIRST DID ALL SPATIAL JOINS ONTO THE COPY, THEN 
#TRANSFERRED ALL SPATIAL JOIN OUTPUT COLUMNS FROM THE COPY BACK TO THE ORIGINAL
neighborhoods_copy <- st_transform(neighborhoods_copy, 4326)

#Crashes:
KSI_pedestrian_crashes_SHP <- st_read("KSI_pedestrian_crashes.shp")

#Youth-only Trip Generators (Schools, Childcare Providers):
youth_only_trip_generators_SHP <- st_read("youth_only_trip_generators_SHP.shp")

#Youth-Heavy Tracts:
young_tracts_Detroit_SHP <- st_read("young_tracts_Detroit_SHP.shp")

#Older Adult-only Trip Generators (Senior Centers, Nursing Homes):
older_adult_only_trip_generators_SHP <- st_read("older_adult_only_trip_generators_SHP.shp")

#Older Adult-Heavy Tracts:
old_tracts_Detroit_SHP <- st_read("old_tracts_Detroit_SHP.shp")

#General Trip Generators (factories, bars, arcades, pool halls, bowling alleys, restaurants, movie theaters, stores,
#office buildings):
general_trip_generators_SHP <- st_read("general_trip_generators_SHP.shp")

#Health Clinics (Hospitals, Health Centers):
health_clinics_SHP <- st_read("health_clinics_SHP.shp")

#Transit Stops:
transit_stops_Detroit_SHP <- st_read("health_clinics_SHP.shp")

#Parks:
parks_Detroit_SHP <- st_read("parks_Detroit_SHP.shp")

#Recreation Centers:
rec_centers_SHP <- st_read("recreation_centers_SHP.shp")

#Places of Worship:
worship_SHP <- st_read("places_of_worship_SHP.shp")



#SPATIAL JOIN KSI PEDESTRIAN CRASHES AND ALL PREDICTORS TO THE NEIGHBORHOODS

#KSI Pedestrian Crashes:
SpatialJoinOutput <- sapply(st_within(KSI_pedestrian_crashes_SHP,
                                      neighborhoods_copy), 
                            function(z) if (length(z)==0) NA_integer_ else z[1]) #SPATIAL JOIN KSI PED CRASH
#POINTS TO EACH CITY COUNCIL NEIGHBORHOOD, but each of the KSI pedestrian crash points is given the NEIGHBORHOOD NUMBER
#of the neighborhood it lies within
summary(SpatialJoinOutput) #Enables me to see what column in neighborhoods SpatialJoinOutput is talking about


KSI_pedestrian_crashes_SHP$Nghbrhd <- SpatialJoinOutput #MAKES NEW COLUMN IN KSI PEDESTRIAN CRASHES SHP ASSIGNING
#EACH CRASH ITS CITY COUNCIL NEIGHBORHOOD
Nghbrhdricts_CrashCount <- data.frame(table(KSI_pedestrian_crashes_SHP$Nghbrhd)) #COUNTS HOW MANY CRASHES EACH 
#NEIGHBORHOOD HAS. table() TABULATES EACH NEIGHBORHOOD'S CRASH COUNT. THAT IS R'S EQUIVALENT BY SUMMARIZING
#AN ARCGIS SHAPEFILE ATTRIBUTE TABLE FIELD.
colnames(Nghbrhdricts_CrashCount) <- c("neighborho", "KSIPdCrshs")

neighborhoods_copy <- merge(x=neighborhoods_copy, y=Nghbrhdricts_CrashCount, by="neighborho", all.x=TRUE)
#Gives each neighborhood its total number of KSI ped crashes in the shapefile
neighborhoods_copy$KSIPdCrshs[is.na(neighborhoods_copy$KSIPdCrshs)] <- 0 #Gives any neighborhood without
#crashes a value of 0 instead of NA
neighborhoods$KSIPdCrshs <- neighborhoods_copy$KSIPdCrshs #THIS ATTACHED MY SPATIAL JOIN 
#RESULTS TO THE ORIGINAL NEIGHBORHOODS SHAPEFILE SO IT CAN BE MAPPABLE


#Youth-only Trip Generators (Schools, Childcare Providers):
SpatialJoinOutput <- sapply(st_within(youth_only_trip_generators_SHP,
                                      neighborhoods_copy), 
                            function(z) if (length(z)==0) NA_integer_ else z[1])
youth_only_trip_generators_SHP$Nghbrhd <- SpatialJoinOutput
Nghbrhdricts_YouthOnlyTGCount <- data.frame(table(youth_only_trip_generators_SHP$Nghbrhd)) 

colnames(Nghbrhdricts_YouthOnlyTGCount) <- c("neighborho", "YthOnlyTGs")
neighborhoods_copy <- merge(x=neighborhoods_copy, y=Nghbrhdricts_YouthOnlyTGCount, by="neighborho", all.x=TRUE)
neighborhoods_copy$YthOnlyTGs[is.na(neighborhoods_copy$YthOnlyTGs)] <- 0
neighborhoods$YthOnlyTGs <- neighborhoods_copy$YthOnlyTGs


#Youth-Heavy Tracts:
SpatialJoinOutput <- sapply(st_within(young_tracts_Detroit_SHP,
                                      neighborhoods_copy), 
                            function(z) if (length(z)==0) NA_integer_ else z[1])
young_tracts_Detroit_SHP$Nghbrhd <- SpatialJoinOutput
Nghbrhdricts_YoungCTCount <- data.frame(table(young_tracts_Detroit_SHP$Nghbrhd)) 

colnames(Nghbrhdricts_YoungCTCount) <- c("neighborho", "YoungCTs")
neighborhoods_copy <- merge(x=neighborhoods_copy, y=Nghbrhdricts_YoungCTCount, by="neighborho", all.x=TRUE)
neighborhoods_copy$YoungCTs[is.na(neighborhoods_copy$YoungCTs)] <- 0
neighborhoods$YoungCTs <- neighborhoods_copy$YoungCTs


#Older Adult-only Trip Generators (Senior Centers, Nursing Homes):
SpatialJoinOutput <- sapply(st_within(older_adult_only_trip_generators_SHP,
                                      neighborhoods_copy), 
                            function(z) if (length(z)==0) NA_integer_ else z[1])
older_adult_only_trip_generators_SHP$Nghbrhd <- SpatialJoinOutput
Nghbrhdricts_OldOnlyTGCount <- data.frame(table(older_adult_only_trip_generators_SHP$Nghbrhd)) 

colnames(Nghbrhdricts_OldOnlyTGCount) <- c("neighborho", "OldOnlyTGs")
neighborhoods_copy <- merge(x=neighborhoods_copy, y=Nghbrhdricts_OldOnlyTGCount, by="neighborho", all.x=TRUE)
neighborhoods_copy$OldOnlyTGs[is.na(neighborhoods_copy$OldOnlyTGs)] <- 0
neighborhoods$OldOnlyTGs <- neighborhoods_copy$OldOnlyTGs


#Older Adult-Heavy Tracts:
SpatialJoinOutput <- sapply(st_within(old_tracts_Detroit_SHP,
                                      neighborhoods_copy), 
                            function(z) if (length(z)==0) NA_integer_ else z[1])
old_tracts_Detroit_SHP$Nghbrhd <- SpatialJoinOutput
Nghbrhdricts_OldCTCount <- data.frame(table(old_tracts_Detroit_SHP$Nghbrhd)) 

colnames(Nghbrhdricts_OldCTCount) <- c("neighborho", "OldCTs")
neighborhoods_copy <- merge(x=neighborhoods_copy, y=Nghbrhdricts_OldCTCount, by="neighborho", all.x=TRUE)
neighborhoods_copy$OldCTs[is.na(neighborhoods_copy$OldCTs)] <- 0
neighborhoods$OldCTs <- neighborhoods_copy$OldCTs


#General Trip Generators (factories, bars, arcades, pool halls, bowling alleys, restaurants, movie theaters, stores,
#office buildings and factories):
SpatialJoinOutput <- sapply(st_within(general_trip_generators_SHP,
                                      neighborhoods_copy), 
                            function(z) if (length(z)==0) NA_integer_ else z[1])
general_trip_generators_SHP$Nghbrhd <- SpatialJoinOutput
Nghbrhdricts_GeneralTGCount <- data.frame(table(general_trip_generators_SHP$Nghbrhd)) 

colnames(Nghbrhdricts_GeneralTGCount) <- c("neighborho", "GeneralTGs")
neighborhoods_copy <- merge(x=neighborhoods_copy, y=Nghbrhdricts_GeneralTGCount, by="neighborho", all.x=TRUE)
neighborhoods_copy$GeneralTGs[is.na(neighborhoods_copy$GeneralTGs)] <- 0
neighborhoods$GeneralTGs <- neighborhoods_copy$GeneralTGs


#Health Clinics (Hospitals, Health Centers):
SpatialJoinOutput <- sapply(st_within(health_clinics_SHP,
                                      neighborhoods_copy), 
                            function(z) if (length(z)==0) NA_integer_ else z[1]) #District 1 has 0 health 
#clinics
health_clinics_SHP$Nghbrhd <- SpatialJoinOutput
Nghbrhdricts_HealthClinicsCount <- data.frame(table(health_clinics_SHP$Nghbrhd)) 

health_clinics_neighborhood_1 <- data.frame(cbind(1,0)) #First creates a row with the values 1 and 0
names(health_clinics_neighborhood_1) <- names(Nghbrhdricts_HealthClinicsCount) #Matches names between dataframe 
#containing new row info and rest of Nghbrhdricts_HealthClinicsCount
Nghbrhdricts_HealthClinicsCount <- rbind(health_clinics_neighborhood_1, Nghbrhdricts_HealthClinicsCount)

colnames(Nghbrhdricts_HealthClinicsCount) <- c("neighborho", "HlthClncs")
neighborhoods_copy <- merge(x=neighborhoods_copy, y=Nghbrhdricts_HealthClinicsCount, by="neighborho", all.x=TRUE)
neighborhoods_copy$HlthClncs[is.na(neighborhoods_copy$HlthClncs)] <- 0
neighborhoods$HlthClncs <- neighborhoods_copy$HlthClncs


#Transit Stops:
SpatialJoinOutput <- sapply(st_within(transit_stops_Detroit_SHP,
                                      neighborhoods_copy), 
                            function(z) if (length(z)==0) NA_integer_ else z[1])#District 1 has 0 transit 
#stops
transit_stops_Detroit_SHP$Nghbrhd <- SpatialJoinOutput
Nghbrhdricts_TransitStopsCount <- data.frame(table(transit_stops_Detroit_SHP$Nghbrhd))

Nghbrhdricts_TransitStopsCount <- rbind(health_clinics_neighborhood_1, Nghbrhdricts_TransitStopsCount) #Because it's
#exact same neighborhood as health centers that has 0 transit stops, that same new row appllies for transit stops
#too

colnames(Nghbrhdricts_TransitStopsCount) <- c("neighborho", "TrnstStps")
neighborhoods_copy <- merge(x=neighborhoods_copy, y=Nghbrhdricts_TransitStopsCount, by="neighborho", all.x=TRUE)
neighborhoods_copy$TrnstStps[is.na(neighborhoods_copy$TrnstStps)] <- 0
neighborhoods$TrnstStps <- neighborhoods_copy$TrnstStps


#Parks:
SpatialJoinOutput <- sapply(st_within(parks_Detroit_SHP,
                                      neighborhoods_copy), 
                            function(z) if (length(z)==0) NA_integer_ else z[1])
parks_Detroit_SHP$Nghbrhd <- SpatialJoinOutput
Nghbrhdricts_ParksCount <- data.frame(table(parks_Detroit_SHP$Nghbrhd)) 

colnames(Nghbrhdricts_ParksCount) <- c("neighborho", "Parks")
neighborhoods_copy <- merge(x=neighborhoods_copy, y=Nghbrhdricts_ParksCount, by="neighborho", all.x=TRUE)
neighborhoods_copy$Parks[is.na(neighborhoods_copy$Parks)] <- 0
neighborhoods$Parks <- neighborhoods_copy$Parks


#Recreation Centers:
SpatialJoinOutput <- sapply(st_within(rec_centers_SHP,
                                      neighborhoods_copy), 
                            function(z) if (length(z)==0) NA_integer_ else z[1])
rec_centers_SHP$Nghbrhd <- SpatialJoinOutput
Nghbrhdricts_RecCentersCount <- data.frame(table(rec_centers_SHP$Nghbrhd)) 

colnames(Nghbrhdricts_RecCentersCount) <- c("neighborho", "RecCenters")
neighborhoods_copy <- merge(x=neighborhoods_copy, y=Nghbrhdricts_RecCentersCount, by="neighborho", all.x=TRUE)
neighborhoods_copy$RecCenters[is.na(neighborhoods_copy$RecCenters)] <- 0
neighborhoods$RecCenters <- neighborhoods_copy$RecCenters


#Places of Worship:
SpatialJoinOutput <- sapply(st_within(worship_SHP,
                                      neighborhoods_copy), 
                            function(z) if (length(z)==0) NA_integer_ else z[1])
worship_SHP$Nghbrhd <- SpatialJoinOutput
Nghbrhdricts_PlacesofWorshipCount <- data.frame(table(worship_SHP$Nghbrhd)) 

colnames(Nghbrhdricts_PlacesofWorshipCount) <- c("neighborho", "WrshpPlcs")
neighborhoods_copy <- merge(x=neighborhoods_copy, y=Nghbrhdricts_PlacesofWorshipCount, by="neighborho", all.x=TRUE)
neighborhoods_copy$WrshpPlcs[is.na(neighborhoods_copy$WrshpPlcs)] <- 0
neighborhoods$WrshpPlcs <- neighborhoods_copy$WrshpPlcs



#Find total number of miles of road per neighborhood, and find each neighborhood's KSI
#crash rate per mile:
roads_Detroit_SHP <- st_read("roads_Detroit_SHP.shp")

SpatialJoinOutput <- sapply(st_within(roads_Detroit_SHP,
                                      neighborhoods), 
                            function(z) if (length(z)==0) NA_integer_ else z[1])

summary(SpatialJoinOutput) #This checks to see if each road was given the 
#neighborhoods by seeing the min and max neighborhood value for each road, it was
(3797/37456)*100 #Line above said how that many road segments were not assigned to a neighborhood. This
#line shows that 10.13723% of all road segments were not assigned a neighborhood
SpatialJoinOutput[is.na(SpatialJoinOutput)] <- 0
roads_Detroit_SHP$Nghbrhd <- SpatialJoinOutput 


#Found each neighborhood's mileage of roadway: 
roads_Detroit_SHP$length_m <- st_length(roads_Detroit_SHP) #This and line below find length of each
#roadway in METERS, but I want it in miles
#According to https://www.metric-conversions.org/length/meters-to-miles.htm, miles = meters * 0.00062137
roads_Detroit_SHP$length_mi <- as.numeric(roads_Detroit_SHP$length_m) * 0.00062137
summary(roads_Detroit_SHP$length_mi)

Nghbrhds_byRdMi <- data.frame(aggregate(
  roads_Detroit_SHP$length_mi, 
  by=list(roads_Detroit_SHP$Nghbrhd), 
  FUN=sum)) #Sums up total miles of roadway by neighborhood. Found out how to do from https://stackoverflow.com/questions/1660124/how-to-sum-a-variable-by-group

colnames(Nghbrhds_byRdMi) = c("neighborho", "ttl_rd_mi") #Changes column names to prepare for joining with
#neighborhoods with counts

neighborhoods <- merge(x=neighborhoods,
                       y=Nghbrhds_byRdMi,
                       by="neighborho",
                       keep.all=FALSE) #Makes it so each neighborhood has
#number of roads per mile

neighborhoods$KSIPCrshPc <- (neighborhoods$KSIPdCrshs/sum(neighborhoods$KSIPdCrshs))*100
#Line above creates column showing each neighborhood's percent of KSI pedestrian crashes for app
summary(neighborhoods$KSIPCrshPc) #No neighborhood has a majority of crashes, HOWEVER there are some
#that will stick out as shown by how the max is much higher than all the other stats, important for 
#the app and project

neighborhoods$KSIPCrPrMi <- neighborhoods$KSIPdCrshs/neighborhoods$ttl_rd_mi
#Line above calculates each neighborhood's number of KSI pedestrian crashes per mile of roadway
summary(neighborhoods$KSIPCrPrMi) #some neighborhoods will stick out as shown by how the max is much 
#higher than all the other stats, important for the app and project

st_write(neighborhoods, "neighborhoods_withEverything.shp", 
         delete_layer = TRUE)



#Notable neighborhoods by Share of KSI crashes graph:
#neighborhoods <- st_read("neighborhoods_withEverything.shp")
colnames(neighborhoods)
topshare <- neighborhoods[order(-neighborhoods$KSIPCrshPc),]
topshare <- topshare[1:10,]
topshare <- topshare[c("nhood_name", "KSIPCrshPc")]
topshare$BarColor <- c("#E7B22F", "#E7B22F", "#E7B22F", "#E7B22F", "#2FA4E7", "#2FA4E7", "#2FA4E7", 
                    "#2FA4E7", "#2FA4E7", "#2FA4E7") #Conditional bar coloring, neighborhoods I want
#to highlight
st_geometry(topshare) <- NULL

ggplot(topshare, aes(reorder(nhood_name, -KSIPCrshPc), (KSIPCrshPc))) + 
  geom_bar(stat="identity", fill=topshare$BarColor) +
  theme_light()+
  labs(title="Top 10 Detroit Neighborhoods by Share of KSI Pedestrian Crashes, 2011-16",
       x="Neighborhood",
       y="% of Detroit KSI Pedestrian Crashes, 2011-16") + 
  theme(axis.text.x = element_text(angle = 90))

#Notable neighborhoods by KSI crash rate per mile graph:
topshare <- neighborhoods[order(-neighborhoods$KSIPCrPrMi),]
topshare <- topshare[1:5,]
topshare <- topshare[c("nhood_name", "KSIPCrPrMi")]
topshare$BarColor <- c("#E7B22F", "#E7B22F", "#2FA4E7", "#2FA4E7", "#DCE72F") #Eastern Market
#higlighted in different color because even thogh it fits in with the other neighborhoods in 
#terms of KSI crash rate per mile of road, it is not only in the top 4 of share of KSI crashes,
#it is also in the top 5 of KSI crash rate per mile
st_geometry(topshare) <- NULL

ggplot(topshare, aes(reorder(nhood_name, -KSIPCrPrMi), (KSIPCrPrMi))) + 
  geom_bar(stat="identity", fill=topshare$BarColor) +
  theme_light()+
  labs(title="Top 5 Detroit Neighborhoods by KSI Pedestrian Crash Rate per Mile of Road, 2011-16",
       x="Neighborhood",
       y="KSI Pedestrian Crash Rate per Mile") + 
  theme(axis.text.x = element_text(angle = 90))



#MAKING OF CORRELATION GRAPHS (FOUND OUT HOW TO DO FROM https://www.displayr.com/how-to-create-a-correlation-matrix-in-r/)
#all_variables = neighborhoods[4:14]

#mydata.cor = cor(all_variables)
#mydata.cor = cor(all_variables, method = c("spearman"))

#variable_names <- c("KSI Ped. Crashes", "Child-Centered Facilities", "Younger CTs", 
#                "Senior-Centered Facilities", "Older CTs", 
#                "General Trip Generators", "Health Clinics", "Transit Stops", 
#                "Parks", "Recreation Centers", "Religious Centers")

#colnames(mydata.cor) <- variable_names
#row.names(mydata.cor) <- variable_names

#corrplot(mydata.cor, col=c("white", "grey"))



#FISHNET DENSITY ANALYSIS FOR KSI PEDESTRIAN CRASHES AND TRANSIT STOPS
census_tracts_Detroit_SHP_with_ages <- st_read("census_tracts_Detroit_SHP_with_ages.shp") #Loads in census
#tracts to be mapped under the fishnet so I can create the appropriate sized fishnet

fishnet_grid <- st_make_grid(neighborhoods_SHP, cellsize = 0.0125, square = TRUE) #CREATES FISHNET OVER 
#DETROIT

ggplot(data = census_tracts_Detroit_SHP_with_ages) +
  geom_sf(fill=NA, colour = "black") + 
  geom_sf(data = fishnet_grid, fill=NA, colour = "red")

fishnet_Detroit_SHP <- st_intersection(fishnet_grid, neighborhoods_buffer)

ggplot(data = census_tracts_Detroit_SHP_with_ages) +
  geom_sf(fill=NA, colour = "black") + 
  geom_sf(data = fishnet_Detroit_SHP, fill=NA, colour = "red")

st_crs(fishnet_Detroit_SHP)
st_write(fishnet_Detroit_SHP, "fishnet_Detroit_Geometries.shp", delete_layer=TRUE)
#DID SPATIAL JOINING OF KSI PEDESTRIAN CRASHES, TRANSIT STOPS, GENERAL TRIP GENERATORS AND PLACES
#OF WORSHIP TO FISHNET IN QGIS USING JOIN ATTRIBUTES BY LOCATION (SUMMARY) TOOL


#THEN MADE KERNEL DENSITY OF CRASHES IN QGIS BY: 1) FIRST REPROJECT KSI CRASHES TO EPSG 2253 (MICHIGAN
#SOUTH - FT), THEN USING THE HEATMAP (KERNEL DENSITY ESTIMATION) TOOL WHERE I USED THESE INPUTS:
#16000 FT RADIUS, 160 FOR BOTH PIXEL SIZE X AND Y, AND EVERYTHING ELSE STAYED THE SAME
