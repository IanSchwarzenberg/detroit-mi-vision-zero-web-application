#------------------------------------------
# Final Web Application: Priority Neighborhoods for Detroit, MI KSI Pedestrian Crash Reducers
# by Ian Schwarzenberg
# CPLN 680, Spring 2019
#-----------------------------------------



#------------------------------------------
# Link to final application: https://ischwarz.shinyapps.io/schwarzenbergian_finalapp_cpln680/
#-----------------------------------------



#Load libraries (ALPHABETIZED):
library(ggplot2)
library(ggthemes)
library(leaflet)
library(raster)
library(rgdal)
library(RColorBrewer)
library(sf)
library(shiny)
library(shinythemes)
library(stargazer)
library(viridis)

options(scipen=999) #Makes every number not show in scientific notation if it arises






#--------------------------
# Server
#--------------------------
server<-function(input, output) ({
  
  
  
  #Load data (./www/ signifies relative file paths so displayed can be displayed online):
  ksi_ped_crash_KD <- raster("./www/KSICrshKD.tif") #Created in QGIS using the "Heatmap (Kernel Density 
  #Estimation)" tool
  ksi_ped_crash_FN <- st_read("./www/ksicrashes_fishnetDetroit_spatialjoin.shp") #KSI crash fishnet, MAP
  #THE 3RD COLUMN Crsh_ID_un
  ksi_ped_crash_SHP <- st_read("./www/KSI_pedestrian_crashes.shp") #KSI pedestrian crash points for 
  #interactive predictor graph
  
  neighborhoods <- st_read("./www/neighborhoods_withEverything.shp")
  
  transit_stops_FN <- st_read("./www/transitstops_fishnetDetroit_spatialjoin.shp") #transit stops fishnet,
  #MAP THE 2ND COLUMN OBJECTID_c
  transit_stops_Detroit_SHP <- st_read("./www/transit_stops_Detroit_SHP.shp") #transit stop points for interactive
  #predictor graph
  
  parks_Detroit_SHP <- st_read("./www/parks_Detroit_SHP.shp") #Parks
  young_tracts_Detroit_SHP <- st_read("./www/young_tracts_Detroit_SHP.shp") #Youth-Heavy Tracts
  old_tracts_Detroit_SHP <- st_read("./www/old_tracts_Detroit_SHP.shp") #Older Adult-Heavy Tracts
  
  youth_only_trip_generators_SHP_forPredGraph <- st_read("./www/youth_only_trip_generators_SHP.shp") 
  #Youth-only Trip Generators (Schools, Childcare Providers)
  youth_only_trip_generators_SHP <- data.frame(st_coordinates(youth_only_trip_generators_SHP_forPredGraph)) 
  #This is done so the point shapefiles can be mapped on leaflet, this does not have to be done for polygon
  #shapefiles
  
  older_adult_only_trip_generators_SHP_forPredGraph <- st_read("./www/older_adult_only_trip_generators_SHP.shp") 
  #Older-Adult-only Trip Generators (Senior Centers, Nursing Homes)
  older_adult_only_trip_generators_SHP <- data.frame(st_coordinates(older_adult_only_trip_generators_SHP_forPredGraph))
  
  general_trip_generators_FN <- st_read("./www/generaltripgenerators_fishnetDetroit_spatialjoin.shp") 
  #General Trip Generators (factories, bars, arcades, pool halls, bowling alleys, restaurants, 
  #movie theaters, stores, office buildings) fishnet, MAP THE 2ND COLUMN FID_count
  general_trip_generators_SHP <- st_read("./www/general_trip_generators_SHP.shp") #general trip 
  #generator points for interactive predictor graph
  
  health_clinics_SHP_forPredGraph <- st_read("./www/health_clinics_SHP.shp") #Health Clinics (Hospitals, Health Centers)
  health_clinics_SHP <- data.frame(st_coordinates(health_clinics_SHP_forPredGraph))
  
  rec_centers_SHP_forPredGraph <- st_read("./www/recreation_centers_SHP.shp") #Recreation Centers
  rec_centers_SHP <- data.frame(st_coordinates(rec_centers_SHP_forPredGraph))
  
  worship_FN <- st_read("./www/placesofworship_fishnetDetroit_spatialjoin.shp") #places of worship 
  #fishnet, MAP THE 2ND COLUMN ObjectID_c
  worship_SHP <- st_read("./www/places_of_worship_SHP.shp") #places of worship points for interactive 
  #predictor graph
  
  
  
  #Create color scehemes for map tabs:
  #Creates color scheme for KSI pedestrian crashes KD:
  val_ksi_ped_crash_KD = as.numeric(seq(0,62,1)) #First makes a sequence of numbers from 0-62 which capture all of the KSI
  #ped crash KD raster's values (found this by running the ksi_ped_crash_KD@data@min and 
  #ksi_ped_crash_KD@data@max R commands)
  pal_ksi_ped_crash_KD = colorNumeric(c("lightyellow", "orange", "red"), val_ksi_ped_crash_KD,
                     na.color = "transparent") #Sets color scale of map
  
  #Creates color scheme for KSI ped crashes fishnet:
  bins_ksi_ped_crash_FN <- c(1,2,4,6,11) #Found the natural breaks for the column 
  #in QGIS 
  pal_ksi_ped_crash_FN <- colorBin("YlOrRd", domain = ksi_ped_crash_FN$Crsh_ID_un, 
                                   bins = bins_ksi_ped_crash_FN,
                                   na.color = "transparent")
  
  #Creates color scheme for KSIPCrshPc column in neighborhoods:
  bins_KSIPCrshPc <- c(0, 0.1812, 0.7246, 1.4493, 2.1739, 4) #Found the natural breaks for the column in QGIS 
  pal_KSIPCrshPc <- colorBin("Blues", domain = neighborhoods$KSIPCrshPc, 
                             bins = bins_KSIPCrshPc,
                   na.color = "transparent")
  
  #Creates color scheme for KSIPCrPrMi column in neighborhoods:
  bins_KSIPCrPrMi <- c(0, 0.3873, 1.2041, 2.8847, 11.2337, 31) #Found the natural breaks for the column in 
  #QGIS 
  pal_KSIPCrPrMi <- colorBin("Blues", domain = neighborhoods$KSIPCrPrMi, 
                             bins = bins_KSIPCrPrMi,
                   na.color = "transparent")
  
  #Creates color scheme for transit stops fishnet:
  bins_transit_stops_FN <- c(0,7,18,31,62,129) #Found the natural breaks for the column 
    #in QGIS 
  pal_transit_stops_FN <- colorBin("Blues", domain = transit_stops_FN$OBJECTID_c, 
                                     bins = bins_transit_stops_FN, na.color = "transparent")

  #Creates color scheme for places of worship fishnet:
  bins_worship_FN <- c(0,2,5,9,17,27) #Found the natural breaks for the column 
    #in QGIS 
  pal_worship_FN <- colorBin("Blues", domain = worship_FN$ObjectID_c, 
                                     bins = bins_worship_FN, na.color = "transparent")
    
  #Creates color scheme for general trip generators fishnet:
  bins_general_trip_generators_FN <- c(0,17,40,79,152,446) #Found the natural breaks for the column 
    #in QGIS 
  pal_general_trip_generators_FN <- colorBin("Blues", domain = general_trip_generators_FN$FID_count, 
                                     bins = bins_general_trip_generators_FN, na.color = "transparent")
  
  
  #Interactive Neighborhood Analysis tab map
  output$NeighborhoodsMap <- renderLeaflet({
    
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -83.081659, lat = 42.360304, zoom = 10.5) %>% #Set center and zoom
      
      addRasterImage(x = ksi_ped_crash_KD, colors = pal_ksi_ped_crash_KD, 
                     opacity = 0.75, group = "KSI Ped. Crash Kernel Density", project=FALSE) %>%
      #project=FALSE makes it so all values are included
      addLegend(pal = pal_ksi_ped_crash_KD, values = val_ksi_ped_crash_KD, 
                title = "KSI Ped. Crash Kernel Density", opacity = 1) %>% #Adds legend 
      #for raster
      
      addPolygons(data = ksi_ped_crash_FN, group = "KSI Ped. Crash Fishnet Density", 
                  fillColor = ~pal_ksi_ped_crash_FN(as.numeric(ksi_ped_crash_FN$Crsh_ID_un)), 
                  weight = 0.5, opacity = 1.0, color = "#9B9B9C", fillOpacity = 0.75) %>%
      addLegend(position = "topright", pal = pal_ksi_ped_crash_FN, values = bins_ksi_ped_crash_FN, 
                title = "KSI Ped. Crash Fishnet Density",
                opacity = 1) %>%
    
      addPolygons(data = neighborhoods, group = "Neighborhoods", 
                  color = "#000000", weight = 0.5, opacity = 1.0, fillOpacity = 0,
                  popup= paste0("<strong>Neighborhood: </strong>", 
                                neighborhoods$nhood_name)) %>%
      
      addPolygons(data = neighborhoods, 
                  fillColor = ~pal_KSIPCrshPc(as.numeric(neighborhoods$KSIPCrshPc)), 
                  weight = 0.5, opacity = 1.0, color = "#000000", fillOpacity = 0.75,
                  group = "% of all KSI Pedestrian Crashes",
                  popup= paste0("<strong>Neighborhood: </strong>", 
                                neighborhoods$nhood_name, 
                                "<br>",
                                "<strong>% of all KSI Ped. Crashes: </strong>", 
                                neighborhoods$KSIPCrshPc)) %>%
      addLegend(position = "topright", pal = pal_KSIPCrshPc, values = bins_KSIPCrshPc, 
                title = "% of all KSI Pedestrian Crashes", opacity = 1) %>%
      
      addPolygons(data = neighborhoods, 
                  fillColor = ~pal_KSIPCrPrMi(as.numeric(neighborhoods$KSIPCrPrMi)), 
                  weight = 0.5, opacity = 1.0, color = "#000000", fillOpacity = 0.75,
                  group = "KSI Pedestrian Crashes per Mi.",
                  popup= paste0("<strong>Neighborhood: </strong>", 
                                neighborhoods$nhood_name,  
                                "<br>",
                                "<strong>KSI Ped. Crashes per Mi. of Rd.: </strong>", 
                                neighborhoods$KSIPCrPrMi)) %>%
      addLegend(position = "topright", pal = pal_KSIPCrPrMi, values = bins_KSIPCrPrMi, 
                title = "KSI Pedestrian Crashes per Mi. of Rd.",
                opacity = 1) %>%
      
      hideGroup("KSI Ped. Crash Fishnet Density") %>% #Unchecks most layers by default 
      addLayersControl(
        baseGroups = c("Neighborhoods", "% of all KSI Pedestrian Crashes", "KSI Pedestrian Crashes per Mi."),
        overlayGroups = c("KSI Ped. Crash Kernel Density", "KSI Ped. Crash Fishnet Density"),
        position = c("topleft"),
        options = layersControlOptions(collapsed=FALSE))
    
  })
  
  
  
  #Interactive notable neighborhoods graph
  output$NeighborhoodsGraph <- renderUI({
    
    if(input$NeighborhoodsGraphChoice == "KSIPCrshPc_Graph"){            
      img(height = 600, width = 900, src = "KSIPCrshPc_Graph.jpeg")
    }                                        
    else if(input$NeighborhoodsGraphChoice == "KSIPCrPrMi_Graph"){
      img(height = 600, width = 900, src = "KSIPCrPrMi_Graph.jpeg")
    }
    
  })
  
  
  
  #Interactive predictor map tab
  output$PredictorsMap <- renderLeaflet({
    
    leaflet() %>%
      setView(lng = -83.081659, lat = 42.360304, zoom = 10.5) %>%
      addProviderTiles("CartoDB.Positron") %>%
      
      addRasterImage(x = ksi_ped_crash_KD, colors = pal_ksi_ped_crash_KD, 
                     opacity = 0.75, group = "KSI Ped. Crash Kernel Density", project=FALSE) %>%
      addLegend(pal = pal_ksi_ped_crash_KD, values = val_ksi_ped_crash_KD, 
                title = "KSI Ped. Crash Kernel Density", opacity = 1) %>%
      
      addPolygons(data = ksi_ped_crash_FN, group = "KSI Ped. Crash Fishnet Density", 
                  fillColor = ~pal_ksi_ped_crash_FN(as.numeric(ksi_ped_crash_FN$Crsh_ID_un)), 
                  weight = 0.5, opacity = 1.0, color = "#9B9B9C", fillOpacity = 0.75) %>%
      addLegend(position = "topright", pal = pal_ksi_ped_crash_FN, values = bins_ksi_ped_crash_FN, 
                title = "KSI Ped. Crash Fishnet Density",
                opacity = 1) %>%
      
      addPolygons(data = young_tracts_Detroit_SHP, group = "Youthful Census Tracts", color = "#000000", 
                  weight = 0.375, opacity = 1.0, fillOpacity = 0
      ) %>%
      addPolygons(data = old_tracts_Detroit_SHP, group = "Elderly Census Tracts", color = "#000000", 
                  weight = 0.375, opacity = 1.0, fillOpacity = 0
      ) %>% 
      addPolygons(data = parks_Detroit_SHP, group = "Parks", color = "#000000", weight = 0.5, opacity = 1.0, 
                  fillOpacity = 0) %>% 
      
      addPolygons(data = transit_stops_FN, fillColor = ~pal_transit_stops_FN(as.numeric(transit_stops_FN$OBJECTID_c)), 
                  weight = 0.5, opacity = 1.0, color = "#9B9B9C", fillOpacity = 0.75, 
                  group = "Transit Stops Fishnet Density") %>%
      addLegend(position = "topright", pal = pal_transit_stops_FN, values = bins_transit_stops_FN, 
                title = "# of Transit Stops", opacity = 1) %>%
      
      addPolygons(data = general_trip_generators_FN, 
                  fillColor = ~pal_general_trip_generators_FN(as.numeric(general_trip_generators_FN$FID_count)), 
                  weight = 0.5, opacity = 1.0, color = "#9B9B9C", fillOpacity = 0.75, 
                  group = "General Trip Generators Fishnet Density") %>%
      addLegend(position = "topright", pal = pal_general_trip_generators_FN, 
                values = bins_general_trip_generators_FN, 
                title = "# of General Trip Generators", opacity = 1) %>%

      addPolygons(data = worship_FN, 
                  fillColor = ~pal_worship_FN(as.numeric(worship_FN$ObjectID_c)), 
                  weight = 0.5, opacity = 1.0, color = "#9B9B9C", fillOpacity = 0.75, 
                  group = "Places of Worship Fishnet Density") %>%
      addLegend(position = "topright", pal = pal_worship_FN, values = bins_worship_FN, 
                title = "# of Places of Worship", opacity = 1) %>%
      
      addCircleMarkers(data = youth_only_trip_generators_SHP, group = "Youth-Only Trip Generators", 
                       lng = ~X, lat = ~Y,  radius = 0.1
      ) %>%
      addCircleMarkers(data = older_adult_only_trip_generators_SHP, 
                       group = "Senior-Citizen-Only Trip Generators", lng = ~X, lat = ~Y, radius = 1
      ) %>%
      addCircleMarkers(data = health_clinics_SHP, group = "Health Clinics", lng = ~X, lat = ~Y,  
                       radius = 1
      ) %>%
      addCircleMarkers(data = rec_centers_SHP, group = "Recreation Centers", lng = ~X, lat = ~Y, 
                       radius = 1
      ) %>%
     
      hideGroup("KSI Ped. Crash Fishnet Density") %>% #Unchecks most layers by default 
      addLayersControl(
        baseGroups = c("Youth-Only Trip Generators", "Youthful Census Tracts", "Elderly Census Tracts",
                       "Places of Worship Fishnet Density", "Transit Stops Fishnet Density", 
                       "General Trip Generators Fishnet Density", "Health Clinics", 
                       "Senior-Citizen-Only Trip Generators", "Parks", "Recreation Centers"),
        #base groups (map layers viewable through radio buttons) are organized from being strongest
        #visual correlation with KSI pedestrian crashes at top with ones with least visual correlation
        #at bottom of list
        overlayGroups = c("KSI Ped. Crash Kernel Density", "KSI Ped. Crash Fishnet Density"),
        position = c("topleft"),
        options = layersControlOptions(collapsed=FALSE))
    
  })
  
  
  
  output$PredictorDistanceGraph <- renderPlot({
    
    #How many KSI pedestrian crashes within user-inputted feet of transit stops:
    TSs_DD <- st_transform(transit_stops_Detroit_SHP, 32610) #First reprojects the POINTS into 
    #decimal degrees because st_buffer only works on decimal degree shapefiles for some reason
    TSs_buffer <- st_buffer(TSs_DD, as.numeric(input$selectDistance)) #Executes the buffer (user-
    #inputted distance)
    TSs_buffer <- st_transform(TSs_buffer, 4326) #Transforms that buffer back into the 4326 projection
    
    SpatialJoinOutput <- sapply(st_within(ksi_ped_crash_SHP,
                                          TSs_buffer), 
                                function(z) if (length(z)==0) NA_integer_ else z[1]) #Spatial joins
    #KSI pedestrian crashes to the user-inputted buffer
    TSs_Pct <- (sum(!is.na(SpatialJoinOutput))/nrow(ksi_ped_crash_SHP))*100 #Stores the percentage
    #of KSI pedestrian crashes that occur within the user-inputted distance of the predictor for the
    #graph at the end. !is.na means factor in anything that ISN'T NA into the equation and leave
    #NA's out
    
    
    #How many KSI pedestrian crashes within user-inputted feet of parks:
    parks_DD <- st_transform(parks_Detroit_SHP, 32610)
    parks_buffer <- st_buffer(parks_DD, as.numeric(input$selectDistance))
    parks_buffer <- st_transform(parks_buffer, 4326)
    
    SpatialJoinOutput <- sapply(st_within(ksi_ped_crash_SHP,
                                          parks_buffer), 
                                function(z) if (length(z)==0) NA_integer_ else z[1])
    parks_Pct <- (sum(!is.na(SpatialJoinOutput))/nrow(ksi_ped_crash_SHP))*100 
    
    
    #How many KSI pedestrian crashes within user-inputted feet of youthful tracts:
    YTs_DD <- st_transform(young_tracts_Detroit_SHP, 32610)
    YTs_buffer <- st_buffer(YTs_DD, as.numeric(input$selectDistance))
    YTs_buffer <- st_transform(YTs_buffer, 4326)
    
    SpatialJoinOutput <- sapply(st_within(ksi_ped_crash_SHP,
                                          YTs_buffer), 
                                function(z) if (length(z)==0) NA_integer_ else z[1])
    YTs_Pct <- (sum(!is.na(SpatialJoinOutput))/nrow(ksi_ped_crash_SHP))*100 
    
    
    #How many KSI pedestrian crashes within user-inputted feet of elderly tracts:
    OTs_DD <- st_transform(old_tracts_Detroit_SHP, 32610)
    OTs_buffer <- st_buffer(OTs_DD, as.numeric(input$selectDistance))
    OTs_buffer <- st_transform(OTs_buffer, 4326)
    
    SpatialJoinOutput <- sapply(st_within(ksi_ped_crash_SHP,
                                          OTs_buffer), 
                                function(z) if (length(z)==0) NA_integer_ else z[1])
    OTs_Pct <- (sum(!is.na(SpatialJoinOutput))/nrow(ksi_ped_crash_SHP))*100 
    
    
    #How many KSI pedestrian crashes within user-inputted feet of youth-only trip generators:
    YTGs_DD <- st_transform(youth_only_trip_generators_SHP_forPredGraph, 32610)
    YTGs_buffer <- st_buffer(YTGs_DD, as.numeric(input$selectDistance))
    YTGs_buffer <- st_transform(YTGs_buffer, 4326)
    
    SpatialJoinOutput <- sapply(st_within(ksi_ped_crash_SHP,
                                          YTGs_buffer), 
                                function(z) if (length(z)==0) NA_integer_ else z[1])
    YTGs_Pct <- (sum(!is.na(SpatialJoinOutput))/nrow(ksi_ped_crash_SHP))*100 
    
    
    #How many KSI pedestrian crashes within user-inputted feet of senior-citizen-only trip generators:
    OTGs_DD <- st_transform(older_adult_only_trip_generators_SHP_forPredGraph, 32610)
    OTGs_buffer <- st_buffer(OTGs_DD, as.numeric(input$selectDistance))
    OTGs_buffer <- st_transform(OTGs_buffer, 4326)
    
    SpatialJoinOutput <- sapply(st_within(ksi_ped_crash_SHP,
                                          OTGs_buffer), 
                                function(z) if (length(z)==0) NA_integer_ else z[1])
    OTGs_Pct <- (sum(!is.na(SpatialJoinOutput))/nrow(ksi_ped_crash_SHP))*100 
    
    
    #How many KSI pedestrian crashes within user-inputted feet of general trip generators:
    GTGs_DD <- st_transform(general_trip_generators_SHP, 32610)
    GTGs_buffer <- st_buffer(GTGs_DD, as.numeric(input$selectDistance))
    GTGs_buffer <- st_transform(GTGs_buffer, 4326)
    
    SpatialJoinOutput <- sapply(st_within(ksi_ped_crash_SHP,
                                          GTGs_buffer), 
                                function(z) if (length(z)==0) NA_integer_ else z[1])
    GTGs_Pct <- (sum(!is.na(SpatialJoinOutput))/nrow(ksi_ped_crash_SHP))*100
    
    
    #How many KSI pedestrian crashes within user-inputted feet of health clinics:
    HCs_DD <- st_transform(health_clinics_SHP_forPredGraph, 32610)
    HCs_buffer <- st_buffer(HCs_DD, as.numeric(input$selectDistance))
    HCs_buffer <- st_transform(HCs_buffer, 4326)
    
    SpatialJoinOutput <- sapply(st_within(ksi_ped_crash_SHP,
                                          HCs_buffer), 
                                function(z) if (length(z)==0) NA_integer_ else z[1])
    HCs_Pct <- (sum(!is.na(SpatialJoinOutput))/nrow(ksi_ped_crash_SHP))*100
    
    
    #How many KSI pedestrian crashes within user-inputted feet of recreation centers:
    RCs_DD <- st_transform(rec_centers_SHP_forPredGraph, 32610)
    RCs_buffer <- st_buffer(RCs_DD, as.numeric(input$selectDistance))
    RCs_buffer <- st_transform(RCs_buffer, 4326)
    
    SpatialJoinOutput <- sapply(st_within(ksi_ped_crash_SHP,
                                          RCs_buffer), 
                                function(z) if (length(z)==0) NA_integer_ else z[1])
    RCs_Pct <- (sum(!is.na(SpatialJoinOutput))/nrow(ksi_ped_crash_SHP))*100
    
    
    #How many KSI pedestrian crashes within user-inputted feet of places of worship:
    POWs_DD <- st_transform(worship_SHP, 32610)
    POWs_buffer <- st_buffer(POWs_DD, as.numeric(input$selectDistance))
    POWs_buffer <- st_transform(POWs_buffer, 4326)
    
    SpatialJoinOutput <- sapply(st_within(ksi_ped_crash_SHP,
                                          POWs_buffer), 
                                function(z) if (length(z)==0) NA_integer_ else z[1])
    POWs_Pct <- (sum(!is.na(SpatialJoinOutput))/nrow(ksi_ped_crash_SHP))*100
    
    
    #Graph creation:
    Pcts <- data.frame(c(TSs_Pct, parks_Pct, YTs_Pct, OTs_Pct, YTGs_Pct, OTGs_Pct, GTGs_Pct, 
                         HCs_Pct, RCs_Pct, POWs_Pct)) #Takes all those percentages for each 
    #predictor and puts into a data frame
    Pcts$PredName <- c("Transit Stops", "Parks", "Youthful Tracts", "Elderly Tracts", 
                       "Youth-only Trip Generators", "Older-Adult-Only Trip Generators", 
                       "General Trip Generators", "Health Clinics", "Recreation Centers",
                       "Places of Worship") #Makes a new column in the data frame above with the
    #name of each predictor corresponding to each percentage
    
    names(Pcts) <- c("Percentage", "Predictor") #Makes names for axis
    Pcts$Percentage <- as.numeric(Pcts$Percentage) #Makes percents column numeric
    
    Pcts <- data.frame(Pcts[order(-Pcts$Percentage),]) #ORDERS THE GRAPH WHERE PREDICTORS WITH THE 
    #HIGHEST PERCENTAGES OF KSI PEDESTRIAN CRASHES HAPPENING NEAR THEM (THE USER-INPUTTED BUFFER) GET 
    #PUT ON THE LEFT
    
    Pcts$BarColor <- c("#E7B22F", "#E7B22F", "#E7B22F", "#2FA4E7", "#2FA4E7", "#2FA4E7", "#2FA4E7", 
                           "#2FA4E7", "#2FA4E7", "#2FA4E7") #Conditional bar coloring which highlights
    #the top 3 predictors in terms of share of KSI pedestrian crashes happening near it. This way, the 
    #top 3 predictors in terms of shares of KSI pedestrian crashes happening near them can always be 
    #highlighted to the user no matter what buffer is used. This is purposely put after the Pcts 
    #dataframe to be graphed is sorted
    names(Pcts) <- c("Percentage", "Predictor", "BarColor") #Makes names for axis again because the
    #ordering command 2 commands above negated the first copy of this line
    Pcts$Percentage <- as.numeric(Pcts$Percentage) #Makes percents column numeric again because the
    #ordering command 2 commands above negated the first copy of this line
    
    ggplot(Pcts, aes(reorder(Predictor, -as.numeric(Percentage)), as.numeric(Percentage))) + 
      geom_bar(stat="identity", fill=Pcts$BarColor) +
      theme_light()+
      labs(title=paste("% of Detroit KSI Pedestrian Crashes",input$selectDistance,"ft. from Predictors, 2011-16"),
           x="Predictor",
           y="% of Detroit KSI Pedestrian Crashes, 2011-16") + 
      theme(axis.text.x = element_text(angle = 90)) #Makes the graph that changes based on user-inputted
    #buffer distance. Note how the title of the graph is also reactive to what the user types in on
    #top of the data that gets graphed
    
  })
  
  
  
  
  
  # Interactive predictor regression table
  RegressionFormula<- reactive({
    as.formula(paste("KSIPdCrshs"," ~ ",paste(input$PredictorsForRegression))) 
  }) #Sets up a user-inputted regression where it regresses the KSI pedestrian crash amount column
  #from the neighborhoods shapefile against the predictor of the user's choice. This only regresses
  #against raw KSI crash amount because if I wanted to regress against say KSI pedestrian crashes per mile of
  #road, my predictors would also have to be in per mile of road.
  
  model <- reactive({
    lm(RegressionFormula(),neighborhoods) #Sets up the regression equation
  })
  
  output$RegressionTable <- renderText({
    stargazer(model(),type="html",dep.var.labels ="Relationship with KSI Pedestrian Crashes", 
              omit.stat = c("f","ser","aic","adj.rsq"))
  }) #Creates the clean and organized regression table output using stargazer and omits some statistics
  #I do not want shown
  
  
  
  #Final target neighborhoods and solutions map
  target_neighborhoods <- neighborhoods[ which(neighborhoods$nhood_name=='Ravendale' 
                                                  | neighborhoods$nhood_name=='Eastern Market'
                                                  | neighborhoods$nhood_name=='Greenfield-Grand River'
                                                  | neighborhoods$nhood_name=='Martin Park'), ] #Selects 
  #out the neighborhoods highlighted in orange and yellow in the graphs on the first tab
  
  output$TargetNeighborhoodsMap <- renderLeaflet({
    
    leaflet() %>%
      setView(lng = -83.081659, lat = 42.360304, zoom = 10.5) %>%
      addProviderTiles("CartoDB.Positron") %>%
      
      addPolygons(data = target_neighborhoods, fillColor = "#2FA4E7", 
                  weight = 0.5, opacity = 1.0, color = "#000000", fillOpacity = 1,
                  popup= paste0("<strong>Neighborhood: </strong>", 
                                target_neighborhoods$nhood_name)) %>%
      addLegend(position = "topright", colors = c("#2FA4E7"), 
                labels = c("Potential Target Neighborhoods"),
                opacity = 1) %>%
      
      addCircleMarkers(data = youth_only_trip_generators_SHP, group = "Youth-Only Trip Generators", 
                       color = "#343434", lng = ~X, lat = ~Y,  radius = 0.1,
                       popup= paste0("<strong>Name: </strong>", 
                                     youth_only_trip_generators_SHP_forPredGraph$business_n) #Gets the
                       #popup info from youth_only_trip_generators_SHP_forPredGraph as opposed to 
                       #youth_only_trip_generators_SHP because youth_only_trip_generators_SHP is 
                       #just each place's points and that's it
      ) %>%
      addPolygons(data = young_tracts_Detroit_SHP, group = "Youthful Census Tracts", color = "#000000", 
                  weight = 0.375, opacity = 1.0, fillOpacity = 0, 
                  popup= paste0("<strong>Census Tract Number: </strong>",
                                young_tracts_Detroit_SHP$NAME, 
                                "<br>",
                                "<strong>% of Pop. Under 18: </strong>", 
                                young_tracts_Detroit_SHP$PCTUND18, 
                                "<br>",
                                "<strong>City Avg. for Tract % of Pop. Under 18: </strong>", 
                                3.71)
      ) %>%
      addPolygons(data = old_tracts_Detroit_SHP, group = "Elderly Census Tracts", color = "#000000", 
                  weight = 0.375, opacity = 1.0, fillOpacity = 0, 
                  popup= paste0("<strong>Census Tract Number: </strong>",
                                old_tracts_Detroit_SHP$NAME, 
                                "<br>",
                                "<strong>% of Pop. Over 65: </strong>", 
                                old_tracts_Detroit_SHP$PCTOV65, 
                                "<br>",
                                "<strong>City Avg. for Tract % of Pop. Over 65: </strong>", 
                                3.47)
      ) %>% 
      
      hideGroup("Youth-Only Trip Generators") %>%
      hideGroup("Youthful Census Tracts") %>%
      hideGroup("Elderly Census Tracts") %>%
      addLayersControl(
      overlayGroups = c("Youth-Only Trip Generators", "Youthful Census Tracts", "Elderly Census Tracts"),
      position = c("topleft"),
      options = layersControlOptions(collapsed=FALSE))
      
  })
  
  
  
})






#--------------------------
# UI
#--------------------------
ui<-fluidPage(theme = shinytheme("cerulean"),
              # navbarPage() makes the multiple tabs
              
              tags$style(type="text/css",
                         ".shiny-output-error { visibility: hidden; }",
                         ".shiny-output-error:before { visibility: hidden; }"
              ), #THIS SUPPRESSES ERROR TEXT. Was taught this in my Penn GAFL 531 Spring 2019 class
              
              navbarPage(
                "Detroit Vision Zero",
                #title for navigation bar
                
                tabPanel(
                  "Welcome",
                  #tabPanel() creates tab
                  tags$head(tags$style( #2FA4E7 is the exact color of the cerulean theme's tag text. 
                    #Found this out by inspecting the element of this page https://bootswatch.com/cerulean/ 
                    "headerPanel {color: #2FA4E7; }
                    h4 {color: #2FA4E7; }
                    h3 {color: #2FA4E7; }
                    h2 {color: #2FA4E7; }
                    h1 {color: #2FA4E7}; }"
                  )),
                  fluidRow(
                    column(12, align="center",
                           headerPanel("Priority Neighborhoods for Detroit, MI KSI Pedestrian Crash Reducers"),
                           br(),
                           h3("By Ian Schwarzenberg, CPLN 680, Spring 2019")
                    )),
                  br(),
                  br(),
                  
                  fluidRow(
                    column(12, align="center",
                           h2("Background")
                    )),
                  h4(tags$ul(
                    tags$li("Vision Zero is a public policy agenda where city governments work to reduce their traffic deaths to zero (source: https://visionzeronetwork.org/about/what-is-vision-zero)."),
                    tags$li("Vision Zero began in Sweden in 1997 and was very successful there (source: https://centerforactivedesign.org/visionzero). The success of it in Sweden gradually inspired other cities and countries around the world to adopt Vision Zero policy agendas."),
                    tags$li("Why Detroit: The city has the highest per-capita pedestrian death rate among large cities in the United States (source: https://www.freep.com/story/money/cars/2018/06/28/suvs-killing-americas-pedestrians/646139002/). It is also one of the few major cities in the US that is not undertaking the Vision Zero policy agenda (source: https://visionzeronetwork.org/wp-content/uploads/2019/01/vision-zero-cities-jan-2019.pdf/).")
                  )),
                  br(),
                  
                  fluidRow(
                    column(12, align="center",
                           h2("Purpose")
                    )),
                  h4(tags$ul(
                    tags$li("Detroit has had many horrible instances of pedestrians, particularly ones who are especially young or elderly, die from getting hit by cars on the city's streets (source: https://www.usatoday.com/story/money/cars/2018/07/03/death-foot-where-youre-most-likely-die-5-most-dangerous-cities/754474002/). This made me create the hypothesis going into this project that predictors which particularly relate to the presences of especially young and old residents would have the strongest associations with where KSI pedestrian crashes are happening the most in Detroit. This makes the application help the user pinpoint which Detroit neighborhoods could especially be targets for street improvements that help the especially young and elderly."), 
                    tags$li("As a result, this is literally a life or death issue, as working towards making streets safer for all users can help delay the ends of their lives. In other words, working to make streets safer saves lives. I was inspired to make this app after getting hit by a car two years ago, which inspired me to devote my career towards working to make streets safer for all users."),
                    tags$li("Detroit policy makers would be interested in this app because analyzing where in Detroit there is most immediate need for road safety improvements, especially ones geared towards helping pedestrians, will save them work in the future if they decide to undertake Vision Zero. Its goal is to use data to inform them on how to potentially best use limited resources towards making the most impact when it comes to pedestrian safety.")
                  )),
                  br(),
                  
                  fluidRow(
                    column(12, align="center",
                           h2("How to Use this App")
                    ),
                    h4(tags$ul(
                      tags$li("In the Neighborhoods Analysis tab, click through the radio buttons on the left side of the map to alternate between layers. The legends for each map view option are on the right side of the map. In all map view options, you can click on the neighborhoods to see their names and other relevant information about them for their respective map options. The point of this map is to show which neighborhoods are receiving the 'most' KSI pedestrian crashes as defined in two ways to visually highlight potential target neighborhoods for future Vision Zero resources that can help pedestrians. For the graphs below the map, choose which graph to view from the drop-down menu. These graphs further highlight which neighborhoods keep appearing as having 'many' KSI pedestrian crashes."),
                      tags$li("In the Predictors Analysis tab, click through the radio buttons on the left side of the map to alternate between layers, like in the previous tab. You can also uncheck and check the KSI pedestrian crashes kernel and fisnet density layers separately. This map analyzes visual correlations between the locations of predictors and KSI pedestrian crashes; some predictors will strongly overlap with KSI crash clusters, and some will not as that map will show. For the distances graph, type in the number of feet from all predictors you want to know how many KSI pedestrian crashes are occurring within, and both the graph data and title will change accordingly. For the regressions, you are only able to choose one predictor at a time on purpose to let you see all of the predictors' coefficients individually when regressed against the crashes. Every time you click a predictor, the statistics will change."),
                      tags$li("The Target Neighborhoods and Solutions tab is simply meant to conclude the findings presented by the app through a map and short description that identify potential target neighborhoods for future Vision Zero solutions that relate to 3 of the top predictors that relate to age which were found to have the strongest associations with KSI pedestrian crash concentrations. Click on each neighborhood and youth-only trip generator in the map to view their names. This can help officials know which youth-friendly institutions to potentially talk to about installing solutions near, and where the neighborhoods are. Also click on each youthful or elderly census tract to see their percentage of elderly or young residents compared to the city averages. Once you have reached this last tab, you have seen the entire app."),
                      tags$li("Note: Maps, graphs and other interactive figures of the app will be slow to load at first, but they will load shortly. Please be patient.")
                    ))
                  ),
                  br(),
                  
                  fluidRow(
                    column(12, align="center",
                           h2("Data Sources")
                    )),
                  h4(tags$ul(
                    tags$li("KSI pedestrian crashes, 2011-16: https://data.detroitmi.gov/Transportation/Traffic-Crashes/9fph-m2jv"),
                    tags$li("Neighborhoods: https://data.detroitmi.gov/Government/Detroit-Neighborhoods/5mn6-ihjv"),
                    tags$li("Roads: http://gis-michigan.opendata.arcgis.com/datasets/all-roads-v17a"),
                    tags$li("Transit stops: Woodward Avenue Light Rail stops from https://d3-d3.opendata.QGIS.com/datasets/ae0e1f2d9078426da08f51b318b7e314_0, city-owned bus stops from https://d3-d3.opendata.QGIS.com/datasets/9d24b9ce27294726aa8d769a0478b4d7_0, suburb-owned bus stops from https://d3-d3.opendata.QGIS.com/datasets/93092f782bae4b97993954ba8c42e8ee_0?geometry=-83.163%2C42.321%2C-82.908%2C42.359 (many of these lines' stops are in Detroit)"),
                    tags$li("Parks: https://data.detroitmi.gov/Fun/Parks-2016/yu9n-k8rd"),
                    tags$li("Census tracts: US Census TIGER/Line Shapefiles"),
                    tags$li("Census tract age data 2012-16: US Census American Community Survey 2012-16 Table S0101"),
                    tags$li("Youth-only trip generators: Schools from https://data.detroitmi.gov/Education/All-Schools-2017-2018/wn8n-5a4d, childcare providers from https://data.detroitmi.gov/Children-Families/Child-Care-Providers/9ssd-ypf9"),
                    tags$li("Older-adult-only trip generators: Senior centers from https://www.seniorcitizensguide.com/detroit/listings/seniorcenters.htm, nursing homes from https://www.dibbern.com/nursing-homes/michigan/detroit-nursing-homes-directory.htm"),
                    tags$li("General trip generators: factories, bars, arcades, pool halls, bowling alleys, restaurants, movie theaters and stores from https://data.detroitmi.gov/Business/Business-Licenses/pugj-2dh4, office buildings and factory locations from https://data.detroitmi.gov/Property-Parcels/Parcel-Points-Ownership/dxgi-9s8s"),
                    tags$li("Health clinics: Hospitals from https://data.detroitmi.gov/Public-Health/Hospitals/9irz-u76s, health centers from https://data.detroitmi.gov/Public-Health/Federally-Qualified-Health-Centers/uiy2-dk3s"),
                    tags$li("Recreation centers: https://d3-d3.opendata.QGIS.com/datasets/8ee240e505cb493eaa63cf0c5c390d34_0"),
                    tags$li("Places of worship: https://d3-d3.opendata.QGIS.com/datasets/f6c93d04857c4bd4b0783867def3ccf5_0")
                  ))
                ), 
                
                
                
                #neighborhoods analysis tab
                tabPanel(
                  "Neighborhoods Analysis",
                  tags$head(tags$style(
                    "headerPanel {color: #2FA4E7; }
                    h2 {color: #2FA4E7; }
                    h4 {color: #2FA4E7}; }"
                  )),
                  fluidRow(
                    column(12, align="center",
                           h2("KSI Pedestrian Crashes vs. Neighborhoods Map")
                    )),
                  
                  #Map
                  leafletOutput("NeighborhoodsMap", width = 1425, height = 600), #Height slightly more
                  #to fit legends
                  br(),
                  h4(tags$ul(
                    tags$li("The first map view option that displays neighborhoods versus KSI crash density above shows how there are 3 primary clusters of KSI pedestrian crashes: in Northwest Detroit, Northeast Detroit and Downtown Detroit."),
                    tags$li("The second map view option that displays neighborhoods color coded by their shares of all Detroit KSI pedestrian crashes from 2011-16 shows how some neighborhoods with the highest shares of KSI pedestrian crashes are located in the three primary crash clusters such as Eastern Market, while some are not, such as Grixdale Farms and Penrose in North-Central Detroit, and Gold Coast just east of Downtown."),
                    tags$li("The third map view option that displays neighborhoods color coded by their KSI crash rate per mile of roadway shows how most of the neighborhoods with the highest rates per mile of road line up with the three predominant crash clusters except for Martin Park in North-Central Detroit for example."),
                    tags$li("Main takeaway: All of the map options show a consistent group of neighborhoods, most by the three primary crash clusters but some less so, that consistently come up as having high shares of KSI pedestrian crashes. Detroit officials should consider looking into neighborhoods like those for targeting future Vision Zero resources.")
                  )),
                  br(),
                  br(),
                  
                  #Interactive notable neighborhoods graph
                  fluidRow(
                    column(12, align="center",
                           h2("Notable Neighborhoods Graphs"),
                           selectInput("NeighborhoodsGraphChoice", h4("Choose Graph to see which Neighborhoods stick out:"), 
                                       choices = c("by Share of KSI Ped. Crashes" = "KSIPCrshPc_Graph", 
                                                   "by KSI Ped. Crash Rate Per Mi." = "KSIPCrPrMi_Graph")),
                           
                           uiOutput("NeighborhoodsGraph")
                    )),
                  br(),
                  h4(tags$ul(
                    tags$li("The first graph showing the top 10 Detroit neighborhoods in terms of share of KSI pedestrian crashes shows how the top 4 neighborhoods in this category have notably higher shares compared to every single other Detroit neighborhood: Ravendale, Eastern Market, Greenfield-Grand River and Martin Park. Even though this graph shows the top 10, the remaining 204 neighborhoods all have smaller shares than the 10th highest neighborhood. In short, this graph shows how there are 4 neighborhoods which have notably higher shares than the remaining 204, which Detroit officials should take notice of."),
                    tags$li("The second graph showing the top 5 Detroit neighborhoods in terms of KSI pedestrian crashes per mile shows how there are 3 neighborhoods that were in the top 10 by shares of all KSI pedestrian crashes that come up again as being notable in terms of KSI pedestrian crashes per mile: Eastern Market, Greenfield-Grand River and Martin Park. Greenfield-Grand River and Martin Park have notably higher KSI pedestrian crash rates per mile compared to the other 206 neighborhoods, making them colored orange. Even though Eastern Market has a more level KSI pedestrian crash rate per mile compared to the other 206 neighborhoods, it also came up in the first graph and is still in the top 5 in terms of KSI pedestrian crash rate per mile, giving it a yellow color since it is still notable, even though it has a more level KSI pedestrian crash rate per mile. In short, this graph further highlights 3 neighborhoods that Detroit officials should take notice of."),
                    tags$li("Main takeaway: There are four neighborhoods which Detroit officials should especially consider prioritizing pedestrian safety solutions if it decides to undertake Vision Zero in the future: Ravendale, Eastern Market, Greenfield-Grand River and Martin Park. These were selected because of how they either come up as notable neighborhoods across both measures used determining which neighborhoods have the 'most' KSI pedestrian crashes, or they come up as having the highest in either category.")
                  ))
                ),
                
                
                
                #predictor analysis tab
                tabPanel(
                  "Predictors Analysis",
                  tags$head(tags$style(
                    "headerPanel {color: #2FA4E7; }
                    radioButtons {color: #2FA4E7; }
                    h2 {color: #2FA4E7; }
                    h4 {color: #2FA4E7}; }"
                  )),
                  fluidRow(
                    column(12, align="center",
                           h2("KSI Pedestrian Crashes vs. Predictors Map")
                    )),
                  leafletOutput("PredictorsMap", width = 1425, height = 725), 
                  br(),
                  h4(tags$ul(
                    tags$li("The map above shows how the youth-only trip generators, youthful census tracts and elderly census tracts predictors have strong visual correlation with all three of the biggest KSI pedestrian crash clusters. This makes these 3 predictors have the strongest visual correlations with KSI pedestrian crash clusters out of all the other predictors."),
                    tags$li("It also shows how the places of worship and transit stops predictors have at least some visual correlation with roughly two of the biggest KSI pedestrian crash clusters. Fishnets are used to display some of the predictors on this map since their high amounts of points would make it difficult to compare their concentrations to KSI pedestrian crash clusters on this map. Each fishnet cell on all maps in this application has the same area of the average area of a Detroit census tract. The census tract was used as a guideline when creating all fishnets to show fine-grained densities of crashes and predictors."),
                    tags$li("It also shows how the health clinics and senior citizens predictors have visual correlations with roughly one of the biggest KSI pedestrian crash clusters."),
                    tags$li("It also shows how the parks and recreation centers predictors have no visual correlation with any of the biggest KSI pedestrian crash clusters."),
                    tags$li("Main takeaway: Predictors associated with the presences of residents under 18 and over 65 years old tend to have the strongest visual correlations with KSI pedestrian crash clusters.")
                  )),
                  br(),
                  br(),
                  
                  #Interactive predictor distances analysis graph
                  fluidRow(
                    column(12, align="center",
                           h2("KSI Crashes vs. Predictors Distances"),
                           numericInput("selectDistance", 
                                        label = h4("Type distance from predictors in feet:"), 
                                        value = 50) #Starting default value, users can change this
                    )),
                  br(),
                  plotOutput("PredictorDistanceGraph"),
                  br(),
                  h4(tags$ul(
                    tags$li("As the graph above shows, any buffer distance you type in up until around 200 or 225 feet will always show youth-heavy tracts, older-adult-heavy tracts and transit stops containing not only majorities of KSI pedestrian crashes happening near them, but notably higher shares of the crashes happening near them compared to any other predictor. This is especially interesting considering how for there are 201 especially elderly tracts and 223 youthful tracts versus 311 parks, but especially within less than 200-225 feet, elderly and youthful tracts experience significantly higher percentages of KSI pedestrian crashes happening near them as an example. Transit stops also have very high shares of the crashes happening near them within this distance, but this can be explained by the very high number of transit stops between all bus and train stops."),
                    tags$li("Even though more predictors experience higher shares of KSI pedestrian crashes past 225 feet, when buffers of up to 500 feet are used, the top predictors in terms of shares of KSI pedestrian crashes happening near them are mostly related to the nearby presences of residents under 18 and over 65. For example, when a 500 feet buffer is used, half of the predictors that have above 50% of all KSI pedestrian crashes from 2011-16 happening near them are especially related to the presences of residents under 18 and over 65."),
                    tags$li("Main takeaway: The results from this graph show how within 500 feet of crashes, predictors that especially relate to the presences of residents under 18 and over 65 have a very strong association with KSI pedestrian crash locations.")
                  )),
                  br(),
                  br(),
                  
                  #Interactive predictor regression table
                  fluidRow(
                    column(12, align="center",
                           h2("KSI Crashes vs. Predictors Regressions"),
                           h4("Regress predictors against KSI pedestrian crashes by choosing each predictor to check its coefficient compared to KSI pedestrian crashes citywide. See which ones have the highest coefficients (the strongest associations) to KSI pedestrian crashes:")
                    )),
                  br(),
                  
                  fluidRow(
                    column(5, align="right",
                           radioButtons(
                             "PredictorsForRegression",
                             label = NA,
                             list(
                               "Youthful Census Tracts" = "YoungCTs",
                               "Elderly Census Tracts" = "OldCTs",
                               "Transit Stops" = "TrnstStps",
                               "Health Clinics" = "HlthClncs",
                               "Parks" = "Parks",
                               "Senior-Citizen-Only Trip Generators" = "OldOnlyTGs",
                               "Recreation Centers" = "RecCenters",
                               "Youth-Only Trip Generators" = "YthOnlyTGs",
                               "Places of Worship" = "WrshpPlcs",
                               "General Trip Generators" = "GeneralTGs"
                             )), #Ordered where top predictor in list has highest correlation with
                           #KSI pedestrian crashes and one on bottom has weakest. Each radio button
                           #is labeled with each predictor's clean name, and it links back to each
                           #predictor's corresponding column name in the neighborhoods shapefile
                           selected = "YoungCTs" #Selects the most associated one by default
                    ),
                    column(7, align="left",
                           tableOutput("RegressionTable")
                    ),
                    br(),
                    column(12, align="left",
                           h4(tags$ul(
                             tags$li("Main takeaway: The same predictors that have majorities of KSI pedestrian crashes happening close to them tend to be the ones that have the highest coefficients with KSI pedestrian crashes as well. Predictors that suggest higher than average amounts of people under 18 and over 65 living nearby have the two highest correlations with KSI pedestrian crashes. This is especially true for youthful census tracts, which has a much higher correlation than all of the other variables.")
                           ))
                    )
                             
                  )
              ),
              
              
              
              #Final target locations and solutions tab
              tabPanel(
                "Target Neighborhoods and Solutions",
                tags$head(tags$style(
                  "headerPanel {color: #2FA4E7; }
                  h2 {color: #2FA4E7; }
                  h4 {color: #2FA4E7}; }"
                )),
                fluidRow(
                  column(12, align="center",
                         h2("Target Neighborhoods Map")
                  )),
                leafletOutput("TargetNeighborhoodsMap", width = 1425, height = 475),
                br(),
                fluidRow(
                  column(12, align="center",
                         h2("Main Findings")
                  )),
                h4(tags$ul(
                  tags$li("1) Detroit should prioritize pedestrian safety solutions in the neighborhoods of Ravendale, Eastern Market, Greenfield-Grand River and Martin Park if it decides to undertake Vision Zero in the future. This is because those four neighborhoods consistently come up across neighborhood analyses as being notable for KSI pedestrian crashes. Due to the city's especially high rate of pedestrian deaths compared to the rest of the country, the city should prioritize implementing pedestrian-oriented solutions first if it decides to undertake Vision Zero in the future."),
                  tags$li("2) Across all predictor analyses, the presences of especially youthful and elderly census tracts and other age-related predictors like youth-only trip generators came up as having the strongest associations with KSI pedestrian crash locations. Therefore, solutions Detroit takes if it decides to implement Vision Zero in the future should especially geared towards making streets safer for pedestrians under 18 and over 65 in the neighborhoods outlined above, and near those predictors."),
                  tags$li("3) Potential solutions geared towards especially young and elderly pedestrians that Detroit officials can consider prioritizing these 4 neighborhoods for in the future include: a) Strategies that help pedestrians who walk slowly like curb extensions, refuge islands and increased crossing times, b) Strategies that help people who are hard of hearing and/or vision such as Accessible Pedestrian Signals, and c) Designating at least some of those 4 neighborhoods as special senior or even youth safety zones to target both physical engineering improvements and community outreach programs geared towards helping older adult pedestrians travel more safely down streets.")
                ))
              )
              )
)



shinyApp(ui = ui, server = server) # Merges the server and ui objects into the final app