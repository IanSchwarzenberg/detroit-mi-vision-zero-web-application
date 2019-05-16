#CPLN 680 Final Project: Data Preparation Script

rm(list=ls())

library(sf)
library(ggmap)
library(dplyr)

setwd("C:/Users/Ian/Documents/Documents/University_Of_Pennsylvania/Courses/CPLN_680_AdvancedTopicsGIS/Data")



#Neighborhoods
neighborhoods <- st_read("geo_export_ec48e3a3-31d4-4ae4-a1fd-a72a3b8c39a8.shp") #Source: https://data.detroitmi.gov/Government/Detroit-Neighborhoods/5mn6-ihjv 

neighborhoods_DD <- st_transform(neighborhoods, 32610) #This and 2 lines below create inner
#buffer of neighborhoods for clippings
neighborhoods_buffer <- st_buffer(neighborhoods_DD, -5)
neighborhoods_buffer <- st_transform(neighborhoods_buffer, 4326)



#Tracts
census_tracts <- st_read("tl_2018_26_tract.shp")
census_tracts <- st_transform(census_tracts, 4326)
st_crs(census_tracts)

census_tracts_new <- st_as_sf(census_tracts %>% distinct) #Gets rid of census tracts that were duplicated
#for some reason above using this dplyr command from https://stackoverflow.com/questions/13967063/remove-duplicated-rows

census_tracts_Detroit_SHP <- st_intersection(census_tracts_new, neighborhoods_buffer) #Clips census
#tracts to Detroit to get census tracts just in Detroit
st_write(census_tracts_Detroit_SHP, "census_tracts_Detroit_SHP.shp", delete_layer=TRUE)


#Streets
#All Michigan roads shapefile data source: http://gis-michigan.opendata.arcgis.com/datasets/all-roads-v17a
michigan_roads <- st_read("michigan_roads.shp")
roads_Detroit_SHP <- st_intersection(michigan_roads, neighborhoods_buffer) #Clips to get just
#Detroit roads
roads_Detroit_SHP <- st_transform(roads_Detroit_SHP, 4326)


#Crashes
crashes <- read.csv("Traffic_Crashes.csv")
options(digits=15)

crashes$Lon <- gsub("location", "", crashes$location)
crashes$Lon <- gsub(".*,", "", crashes$Lon)
crashes$Lon <- gsub("\\)", "", crashes$Lon)
crashes$Lon <- as.numeric(crashes$Lon)

crashes$Lat <- gsub("location", "", crashes$location)
crashes$Lat <- gsub(",.*", "", crashes$Lat)
crashes$Lat <- gsub("\\(", "", crashes$Lat)
crashes$Lat <- as.numeric(crashes$Lat)


pedestrian_crashes <- crashes[ which(crashes$Pedestrian.=='1'), ]
fatal_pedestrian_crashes <- pedestrian_crashes[ which(pedestrian_crashes$K.Level.Fatal.Injuries > 0), ]
severe_pedestrian_crashes <- pedestrian_crashes[ which(pedestrian_crashes$A.Level.Injuries > 0), ]
KSI_pedestrian_crashes <- rbind(fatal_pedestrian_crashes, severe_pedestrian_crashes) 

KSI_pedestrian_crashes_SHP <- st_as_sf(KSI_pedestrian_crashes, coords=c("Lon","Lat"),
                        crs=st_crs("+init=epsg:4326"))
st_write(KSI_pedestrian_crashes_SHP, "KSI_pedestrian_crashes.shp", delete_layer=TRUE)



#Schools
schools <- read.csv("All_Schools_2017_2018.csv")
options(digits=15)

schools$Lon <- gsub("location", "", schools$location)
schools$Lon <- gsub(".*,", "", schools$Lon)
schools$Lon <- gsub("\\)", "", schools$Lon)
schools$Lon <- as.numeric(schools$Lon)

schools$Lat <- gsub("location", "", schools$location)
schools$Lat <- gsub(",.*", "", schools$Lat)
schools$Lat <- gsub("\\(", "", schools$Lat)
schools$Lat <- as.numeric(schools$Lat)

schools_SHP <- st_as_sf(schools, coords=c("Lon","Lat"),
                        crs=st_crs("+init=epsg:4326"))

st_write(schools_SHP, "schools_SHP.shp", delete_layer=TRUE)



#Childcare
childcare <- st_read("child_care_providers.shp")
childcare <- st_transform(childcare, 4326)
childcare <- childcare[-c(3), ]
options(digits=15)

childcare$Lat <- gsub("c\\(", "", childcare$geometry)
childcare$Lat <- gsub(".*,", "", childcare$Lat)
childcare$Lat <- gsub("\\)", "", childcare$Lat)
childcare$Lat <- as.numeric(childcare$Lat)

childcare$Lon <- gsub(",.*", "", childcare$geometry)
childcare$Lon <- gsub("c\\(", "", childcare$Lon)
childcare$Lon <- as.numeric(childcare$Lon)

childcare_Detroit_SHP <- st_intersection(childcare, neighborhoods_buffer)
st_write(childcare_Detroit_SHP, "childcare_Detroit_SHP.shp", delete_layer=TRUE)


# THEN MERGED SCHOOLS AND CHILDCARE SHAPEFILES IN QGIS TO GET YOUTH-ONLY TRIP GENERATORS



#Businesses
businesses <- read.csv("Business_Licenses.csv")
options(digits=15)

businesses$Lon <- gsub("location", "", businesses$Location)
businesses$Lon <- gsub(".*,", "", businesses$Lon)
businesses$Lon <- gsub("\\)", "", businesses$Lon)
businesses$Lon <- as.numeric(businesses$Lon)

businesses$Lat <- gsub("location", "", businesses$Location)
businesses$Lat <- gsub(",.*", "", businesses$Lat)
businesses$Lat <- gsub("\\(", "", businesses$Lat)
businesses$Lat <- as.numeric(businesses$Lat)

restaurant_rows <- grep("Restaurant", businesses$License.Description)
restaurants <- businesses[restaurant_rows,]
restaurants <- na.omit(restaurants)
restaurants_SHP <- st_as_sf(restaurants, coords=c("Lon","Lat"),
                           crs=st_crs("+init=epsg:4326"))
st_write(restaurants_SHP, "restaurants_SHP.shp", delete_layer=TRUE)

arcade_rows <- grep("Arcade", businesses$License.Description)
games_rows <- grep("Game", businesses$License.Description)
arcades <- businesses[arcade_rows,]
games <- businesses[games_rows,]
arcades <- rbind(arcades, games)
arcades <- na.omit(arcades)
arcades_SHP <- st_as_sf(arcades, coords=c("Lon","Lat"),
                            crs=st_crs("+init=epsg:4326"))
st_write(arcades_SHP, "arcades_SHP.shp", delete_layer=TRUE)

bar_rows <- grep("Bar", businesses$License.Description)
bars <- businesses[bar_rows,]
bars <- na.omit(bars)
bars_SHP <- st_as_sf(bars, coords=c("Lon","Lat"),
                            crs=st_crs("+init=epsg:4326"))
st_write(bars_SHP, "bars_SHP.shp", delete_layer=TRUE)

store_rows <- grep("Store", businesses$License.Description)
stores <- businesses[store_rows,]
stores <- na.omit(stores)
stores_SHP <- st_as_sf(stores, coords=c("Lon","Lat"),
                     crs=st_crs("+init=epsg:4326"))
st_write(stores_SHP, "stores_SHP.shp", delete_layer=TRUE)

billiards_rows <- grep("Billiard", businesses$License.Description)
billiards <- businesses[billiards_rows,]
billiards <- na.omit(billiards)
billiards_SHP <- st_as_sf(billiards, coords=c("Lon","Lat"),
                       crs=st_crs("+init=epsg:4326"))
st_write(billiards_SHP, "billiards_SHP.shp", delete_layer=TRUE)

bowling_rows <- grep("Bowling", businesses$License.Description)
bowling <- businesses[bowling_rows,]
bowling <- na.omit(bowling)
bowling_SHP <- st_as_sf(bowling, coords=c("Lon","Lat"),
                          crs=st_crs("+init=epsg:4326"))
st_write(bowling_SHP, "bowling_SHP.shp", delete_layer=TRUE)

theater_rows <- grep("Theatre", businesses$License.Description)
theaters <- businesses[theater_rows,]
theaters <- na.omit(theaters)
theaters_SHP <- st_as_sf(theaters, coords=c("Lon","Lat"),
                        crs=st_crs("+init=epsg:4326"))
st_write(theaters_SHP, "theaters_SHP.shp", delete_layer=TRUE)



#Parcels
all_parcels <- read.csv("Parcel_Points_Ownership.csv")
vacant_parcel_rows <- grep("DETROIT LAND BANK AUTHORITY", all_parcels$Owner)
occupied_parcels <- all_parcels[-c(vacant_parcel_rows), ]

office_building_rows <- grep("204", occupied_parcels$Property.Class)
office_buildings <- occupied_parcels[office_building_rows,]
office_buildings <- na.omit(office_buildings)
office_buildings_SHP <- st_as_sf(office_buildings, coords=c("Longitude","Latitude"),
                                    crs=st_crs("+init=epsg:4326"))
st_write(office_buildings_SHP, "office_buildings_SHP.shp", delete_layer=TRUE)

factories_rows <- grep("301", occupied_parcels$Property.Class)
factories <- occupied_parcels[factories_rows,]
factories <- na.omit(factories)
factories_SHP <- st_as_sf(factories, coords=c("Longitude","Latitude"),
                                 crs=st_crs("+init=epsg:4326"))
st_write(factories_SHP, "factories_SHP.shp", delete_layer=TRUE)

office_buildin_factories <- rbind(office_buildings, factories)
myvars <- c("Longitude", "Latitude")
office_buildin_factories <- office_buildin_factories[myvars]
colnames(office_buildin_factories) <- c("Lon", "Lat")

general_trip_generators <- rbind(restaurants, arcades, bars, stores, billiards, bowling, theaters)
myvars <- c("Lon", "Lat")
general_trip_generators <- general_trip_generators[myvars]

general_trip_generators <- rbind(general_trip_generators, office_buildin_factories)

general_trip_generators_SHP <- st_as_sf(general_trip_generators, coords=c("Lon","Lat"),
                                        crs=st_crs("+init=epsg:4326"))
st_write(general_trip_generators_SHP, "general_trip_generators_SHP.shp", delete_layer=TRUE)

general_trip_generators_SHP <- st_intersection(general_trip_generators_SHP, neighborhoods_buffer)

general_trip_generators <- data.frame(general_trip_generators_SHP)
options(digits=15)

general_trip_generators$Lat <- gsub("c\\(", "", general_trip_generators$geometry)
general_trip_generators$Lat <- gsub(".*,", "", general_trip_generators$Lat)
general_trip_generators$Lat <- gsub("\\)", "", general_trip_generators$Lat)
general_trip_generators$Lat <- as.numeric(general_trip_generators$Lat)

general_trip_generators$Lon <- gsub("c\\(", "", general_trip_generators$geometry)
general_trip_generators$Lon <- gsub(",.*", "", general_trip_generators$Lon)
general_trip_generators$Lon <- as.numeric(general_trip_generators$Lon)



#Health Clinics
hospitals <- st_read("hospitals.shp")
health_centers <- read.csv("Federally_Qualified_Health_Centers.csv")

health_centers$Location.1 <- gsub("MI [0-9]{5}.*", "", health_centers$Location.1)
health_centers$Location.1 <- gsub(",,", ", MI", health_centers$Location.1)

register_google(key = "AIzaSyAZtr7RNwOKj-cw3Q-trTN6AtzChUsdks0") #NECESSARY FOR GEOCODING

health_center_coordinates <- geocode(health_centers$Location.1,
                                     output = "more", messaging=TRUE,
                                     source="google") #GEOCODES THE ADDRESSES

health_center_coordinates <- health_center_coordinates[c(-3:-4,-6:-9)] #Keeps columns I want

write.csv(health_center_coordinates, "health_center_coordinates.csv")

health_centers_SHP <- st_as_sf(health_center_coordinates, coords=c("lon","lat"),
                                     crs=st_crs("+init=epsg:4326")) # converts health_center_coordinates to
#an sf object (shapefile)

st_write(health_centers_SHP, "health_centers_SHP.shp", delete_layer=TRUE)

#MERGED HOSPITALS AND HEALTH CENTERS SHAPEFILES IN QGIS TO CREATE health_clinics_SHP


#Transit Stops -- Merging of light rail stops, city bus stops and suburban bus stops DONE IN QGIS
transit_stops_SHP <- st_read("transit_stops_SHP.shp") #it's in 4326 already

transit_stops_Detroit_SHP <- st_intersection(transit_stops_SHP, neighborhoods_buffer) #Clips transit
#stops to Detroit to get stops just in Detroit
st_crs(transit_stops_Detroit_SHP)
st_write(transit_stops_Detroit_SHP, "transit_stops_Detroit_SHP.shp", delete_layer=TRUE)



#Parks:
parks <- st_read("parks.shp") #it's in 4326 already
parks_SHP <- st_transform(parks, 4326)

parks_Detroit_SHP <- st_intersection(parks_SHP, neighborhoods_buffer)
st_crs(parks_Detroit_SHP)
st_write(parks_Detroit_SHP, "parks_Detroit_SHP.shp", delete_layer=TRUE)

#recreation centers:
rec_centers <- st_read("Detroit_Recreation_Centers.shp")
rec_centers_SHP <- st_transform(rec_centers, 4326)
st_crs(rec_centers_SHP) 
st_write(rec_centers_SHP, "recreation_centers_SHP.shp")



#Places of Worship:
worship <- st_read("Detroit_Churches_2011.shp")
worship_SHP <- st_transform(worship, 4326)
st_crs(worship_SHP) 
st_write(worship_SHP, "places_of_worship_SHP.shp")



#Youth-Heavy Tracts:

#First modified the original ACS_17_5YR_S0101_with_ann.csv in Excel to only have the age columns I want and 
#modified column names to be the same as census tract shapefile column names to create census_tract_age_data.csv

#Then joined census_tract_age_data.csv to census_tracts_Detroit_SHP.shp in QGIS, wasn't working properly in
#R for some reason

#Then removed 11 census tracts that were somehow duplicated again and messed up the data

census_tracts_Detroit_SHP_with_ages <-st_read("census_tracts_Detroit_SHP_with_ages.shp")

census_tracts_Detroit_SHP_with_ages$PCTUND18 <- as.character(census_tracts_Detroit_SHP_with_ages$census_t_3)
census_tracts_Detroit_SHP_with_ages$PCTUND18 <- as.numeric(census_tracts_Detroit_SHP_with_ages$PCTUND18) #To 
#find average of the percent under 18 column, first had to convert factor column containing that data first to a
#character column, then to numeric from there

young_tracts_Detroit_SHP <- st_as_sf(census_tracts_Detroit_SHP_with_ages[ which(census_tracts_Detroit_SHP_with_ages$PCTUND18 > mean(as.numeric(census_tracts_Detroit_SHP_with_ages$PCTUND18))), ])
#Creates new shapefile of the Detroit census tracts with youth population averages above the citywide average
#for this

st_crs(young_tracts_Detroit_SHP)
st_write(young_tracts_Detroit_SHP, "young_tracts_Detroit_SHP.shp")



#Older Adult-Heavy Tracts:
census_tracts_Detroit_SHP_with_ages$PCTOV65 <- as.character(census_tracts_Detroit_SHP_with_ages$census_t_5)
census_tracts_Detroit_SHP_with_ages$PCTOV65 <- as.numeric(census_tracts_Detroit_SHP_with_ages$PCTOV65)

old_tracts_Detroit_SHP <- st_as_sf(census_tracts_Detroit_SHP_with_ages[ which(census_tracts_Detroit_SHP_with_ages$PCTOV65 > mean(as.numeric(census_tracts_Detroit_SHP_with_ages$PCTOV65))), ])

st_crs(old_tracts_Detroit_SHP)
st_write(old_tracts_Detroit_SHP, "old_tracts_Detroit_SHP.shp")



#Restaurants (from businesses.csv)
business_descriptions <- data.frame(unique(businesses$License.Description))
grep("Non-Profit", businesses$License.Description)
businesses$License.Description[8509]

restaurant_rows <- grep("Restaurant", businesses$License.Description)
restaurants <- businesses[restaurant_rows,]
restaurants <- na.omit(restaurants)
restaurants_SHP <- st_as_sf(restaurants, coords=c("Lon","Lat"),
                            crs=st_crs("+init=epsg:4326"))
st_write(restaurants_SHP, "restaurants_SHP.shp", delete_layer=TRUE)



#Senior Centers (web scraped from https://www.seniorcitizensguide.com/detroit/listings/seniorcenters.htm):

a <- scan("https://www.seniorcitizensguide.com/detroit/listings/seniorcenters.htm",what="",sep="\n") 

b <- gsub("<[^>]*>", "", a) #Gets rid of all html text from website and stores everything in new object called
#"b"
b <- gsub("\\t", "", b) #gets rid of those \t\t\t etc.'s that didn't go away before
#write.table(b, "senior_centers_website_text.txt") #stores the text from that into a text file

c <- b[258:373] #subsets out rows from b that just contain names, addresses, etc. of Wayne County senior centers

c <- gsub("[0-9]{9}|[0-9]{3}-[0-9]{3}-[0-9]{4}|\\([0-9]{3}\\)[0-9]{3}-[0-9]{4}", "", c) #Gets rid of all 
#phone numbers 

senior_center_addresses <- c[grep(".*[0-9]", c)] #Extracts all buidling and street parts of Wayne County senior
#center addresses. THIS IS THE END OF THE ACTUAL WEB SCRAPING TECHNICALLY since the web site does not give
#the town names for each senior center, just te county ones

senior_center_addresses <- data.frame(senior_center_addresses)
names(senior_center_addresses) <- c("Addr_Orig")

senior_center_addresses$County <- "Wayne County, MI"
senior_center_addresses$Addr_toGC <- paste(senior_center_addresses$Addr_Orig,senior_center_addresses$County,sep=", ")
#concats the original address and county columns together so that they are in one cell and separated by a comma
#and space to get ready for geocoding

register_google(key = "AIzaSyAZtr7RNwOKj-cw3Q-trTN6AtzChUsdks0") #NECESSARY FOR GEOCODING

senior_center_coordinates <- geocode(senior_center_addresses$Addr_toGC,
                                     output = "more", messaging=TRUE,
                                     source="google") #GEOCODES THE ADDRESSES. 2 out of the 38 were not found,
##since this means 95% (36/38) were found, I will just discard the 2 that weren't:
senior_center_coordinates_final <- senior_center_coordinates[ which(senior_center_coordinates$loctype!='approximate'),]

senior_center_coordinates_final <- senior_center_coordinates_final[c(-3:-4,-6:-9)] #Keeps columns I want

write.csv(senior_center_coordinates_final, "senior_center_coordinates_final.csv")

senior_centers_final_SHP <- st_as_sf(senior_center_coordinates_final, coords=c("lon","lat"),
                                    crs=st_crs("+init=epsg:4326")) # converts senior_center_coordinates_final to
#an sf object (shapefile)

senior_centers_Detroit_SHP <- st_intersection(senior_centers_final_SHP, neighborhoods_buffer) #Clips 
#Wayne County senior centers to Detroit to get census tracts just in Detroit

length(grep("detroit", senior_centers_final_SHP$address)) #Shows how only 6 address had Detroit in their name
#from the geocoding process, showing the clip to Detroit worked properly

st_write(senior_centers_Detroit_SHP, "senior_centers_Detroit_SHP.shp", delete_layer=TRUE)



#Nursing homes/retirement communities (web scraped from https://www.dibbern.com/nursing-homes/michigan/detroit-nursing-homes-directory.htm):
#rm(a,b,c)
a <- scan("https://www.dibbern.com/nursing-homes/michigan/detroit-nursing-homes-directory.htm",what="",sep="\n") 

write.table(a, "a.txt") #Exported as table to look at rows

a <- a[755:819] #After looking at the text file, I saw how all the info I needed was in rows 755-819
a <- a[c(1:2, 21:45, 64:65)] #Finally extracts just what I want
a <- gsub("<[^>]*>", "", a)
a <- gsub("\\([0-9]{3}\\) [0-9]{3}-[0-9]{4}", "", a) #Removes all phone numbers, had to add space between area
#code
a <- gsub("For.*", "", a)

a <- data.frame(b)
names(a) <- c("Addr_toGC")
a$Addr_toGC <- as.character(a$Addr_toGC)

nursing_home_coordinates <- geocode(a$Addr_toGC,
                                     output = "more", messaging=TRUE,
                                     source="google") #GEOCODES, it succesfully got all the addresses without
#me doing anything to modify the addresses at all

nursing_home_coordinates_final <- nursing_home_coordinates[c(-3:-4,-6:-9)] #Keeps columns I want

nursing_homes_final_SHP <- st_as_sf(nursing_home_coordinates_final, coords=c("lon","lat"),
                                     crs=st_crs("+init=epsg:4326")) 

nursing_homes_Detroit_SHP <- st_intersection(nursing_homes_final_SHP, neighborhoods_buffer) #Only 1
#nursing home was not in Detroit
st_crs(nursing_homes_Detroit_SHP)

st_write(nursing_homes_Detroit_SHP, "nursing_homes_Detroit_SHP.shp", delete_layer=TRUE)


older_adult_only_trip_generators_SHP <- st_as_sf(rbind(nursing_homes_Detroit_SHP, senior_centers_Detroit_SHP)) 
#MERGES SENIOR CENTERS AND NURSING HOMES SHAPEFILES TO GET OLDER ADULT ONLY TRIP GENERATORS
st_crs(older_adult_only_trip_generators_SHP)
st_write(older_adult_only_trip_generators_SHP, "older_adult_only_trip_generators_SHP.shp", delete_layer=TRUE)


