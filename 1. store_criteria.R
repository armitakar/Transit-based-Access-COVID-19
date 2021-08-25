######### Selection of Health Facilities #########

### Function to cluster facilities using dbscan 
poi_cent_dbscan = function(data, Type){
  data$long = as.numeric(unlist(map(data$geometry,1)))
  data$lat = as.numeric(unlist(map(data$geometry,2)))
  st_geometry(data) <- NULL
  dat =  data[c(83,82)]
  
  #epsilon value 400 meters
  #minimum clustering threshold 3 points
  db = dbscan::dbscan(dat,eps=400,minPts = 3)
  dat$clust = db$cluster
  
  #identify noises that do not belong to any cluster
  noise = dat[dat$clust == 0,]
  noise1 = noise %>% st_as_sf(., coords = c("long", "lat"), 
                              crs = st_crs(5070), agr = "constant")  %>%
    st_transform(., 4326) %>% 
    mutate(long = unlist(map(geometry,1)),lat = unlist(map(geometry,2)))
  noise = noise1[-c(1:2)]
  st_geometry(noise) = NULL
  
  #identify cluster centroids
  cluster = dat[dat$clust != 0,]
  n = max(cluster$clust)[1]
  centroid <- data.frame(cluster = unique(cluster$clust))
  centroid$geometry = 0
  
  for (i in 1:n){
    d = subset(cluster, clust == i)
    sf_poi = st_as_sf(d, coords = c("long", "lat"), 
                      crs = st_crs(5070), agr = "constant") 
    sf_poi = sf_poi %>% mutate(long = unlist(map(sf_poi$geometry,1)),
                               lat = unlist(map(sf_poi$geometry,2)))
    d <- st_centroid(sf_poi) %>% 
      st_transform(., 4326) %>%
      st_geometry()
    centroid$geometry[i] <- d
    
  }
  #combine noises and clusters to generate list of data points
  centroid$long = unlist(map(centroid$geometry,1))
  centroid$lat = unlist(map(centroid$geometry,2))
  centroid = centroid[-c(1:2)]
  dat = rbind(centroid, noise)
  dat$type = Type
  return(dat)
}

#### Function to select non-urgent health care facilities 
health_poi_nonurgent = function(shapefile_name, service_city){
  #read data
  area = st_read(paste("D:/Accessibility_study/Shapefiles1/",shapefile_name,"_coverage.shp",sep=""))
  area %>% ggplot() +
    geom_sf(color = "black") 
  spe = st_read('D:/Accessibility_study/Final isochrones1/Shapefiles_new/Original shapefiles/specialty_hospitals.shp')
  pm = st_read('D:/Accessibility_study/Final isochrones1/Shapefiles_new/Original shapefiles/primary_care_all.shp')
  
  #identify specialty hospitals located in a study area
  dat1 = st_transform(spe, crs = "EPSG:5070")
  dat2 = st_intersection(dat1, area)
  dat2 = st_transform(dat2, 4326)
  dat2$long = as.numeric(unlist(map(dat2$geometry,1)))
  dat2$lat = as.numeric(unlist(map(dat2$geometry,2)))
  st_geometry(dat2) <- NULL
  spe1 =  dat2[c(82,83)]
  spe1$type = "Specialty Hospital"
  
  #identify and cluster primary care facilities located in a study area
  pm1 = st_transform(pm, crs = "EPSG:5070")
  pm2 = st_intersection(pm1, area)
  pm3 = poi_cent_dbscan(pm2, "Primary Care")
  #combine them as non-urgent facilities
  poi_all = rbind(spe1, pm3)
  poi_all$latlong = paste(poi_all$lat, poi_all$long, sep=",")
  write.csv(poi_all, paste("D:/Accessibility_study/POI_cities/health/poi_",service_city,"_nonurgent_health.csv",sep=""))
  return(poi_all)
}

#### Function to select urgent helath care facilities 
health_poi_urgent = function(shapefile_name, service_city){
  #read data
  area = st_read(paste("D:/Accessibility_study/Shapefiles1/",shapefile_name,"_coverage.shp",sep=""))
  area %>% ggplot() +
    geom_sf(color = "black") 
  gen = st_read('D:/Accessibility_study/Final isochrones1/Shapefiles_new/Original shapefiles/general_hospitals.shp')
  em = st_read('D:/Accessibility_study/Final isochrones1/Shapefiles_new/Original shapefiles/emergency_care_services.shp')
  
  #identify general hospitals located in a study area
  dat1 = st_transform(gen, crs = "EPSG:5070")
  dat2 = st_intersection(dat1, area)
  dat2 = st_transform(dat2, 4326)
  dat2$long = as.numeric(unlist(map(dat2$geometry,1)))
  dat2$lat = as.numeric(unlist(map(dat2$geometry,2)))
  st_geometry(dat2) <- NULL
  gen1 =  dat2[c(82,83)]
  gen1$type = "General Hospital"
  
  #identify and cluster emergency care facilities located in a study area
  em1 = st_transform(em, crs = "EPSG:5070")
  em2 = st_intersection(em1, area)
  em3 = poi_cent_dbscan(em2, "Emergency Care")
  #combine them as urgent facilities
  poi_all = rbind(gen1, em3)
  poi_all$latlong = paste(poi_all$lat, poi_all$long, sep=",")
  write.csv(poi_all, paste("D:/Accessibility_study/POI_cities/health/poi_",service_city,"_urgent_health.csv",sep=""))
  return(poi_all)
}



######### Selection of Grocery Stores ##################

#### Function to identify InfoGroup data for the study area 
info_data = function(shapefile_name,state_code){
  #read study area shapefile
  area = st_read(paste("D:/Accessibility_study/Shapefiles1/",shapefile_name,"_coverage.shp",sep=""))
  area %>% ggplot() +
    geom_sf(color = "black") 
  
  #read InfoGroup data for specific state where the study area is located
  layout = read.csv('D:/Accessibility_study/BUSINESS_HISTORICAL_2020/2020 Academic Business Layout.csv', header = FALSE)
  poi_state = read.csv(paste("D:/Accessibility_study/BUSINESS_HISTORICAL_2020/Business-2020-",state_code, ".csv",sep=""),
                       stringsAsFactors = FALSE, header = FALSE)
  names(poi_state) = layout
  dat1 = poi_state[complete.cases(poi_state$Longitude), ]
  dat = dat1[complete.cases(dat1$Latitude), ]
  sf_poi = st_as_sf(dat, coords = c("Longitude", "Latitude"), 
                    crs = st_crs(4326), agr = "constant") 
  sf_poi = sf_poi %>% mutate(long = unlist(map(sf_poi$geometry,1)),
                             lat = unlist(map(sf_poi$geometry,2)))
  sf_poi_prj = st_transform(sf_poi, crs = "EPSG:5070")
  
  #identify the busniess locations within the study area
  poi_all = st_intersection(sf_poi_prj, area)
  poi_all %>% ggplot() +
    geom_sf(color = "black") 
  st_geometry(poi_all) <- NULL
  return(poi_all)
}


#### Function to identify grocery stores
poi_grocery = function(poi_all, service_city){
  poi_all$NAICS6 = substring(poi_all$Primary.NAICS.Code, 1, 6)
  
  #identify supermarket and grocery stores based on NAICS 445110
  poi_all1 = subset(poi_all, NAICS6 == "445110" & complete.cases(Sales.Volume..9....Location)
         & complete.cases(Employee.Size..5....Location))
  #identify top 10th percentile among all supermarket and grocery stores (Employee size > 80 and sales volume > 17.5 million)
  grocery = subset(poi_all1, Sales.Volume..9....Location>17506 & Employee.Size..5....Location >80)
  
  #idetify departmental stores based on NAICS code 452111 and self-selection
  dept = subset(poi_all, NAICS6 == "452111")
  dept_gro = dept[dept$Company == "WALMART" | 
                    dept$Company == "WALMART SUPERCENTER" | 
                    dept$Company == "TARGET",]
  
  #identify warehouse clubs based on NAICS code 452910 and self-selection
  ware = subset(poi_all, NAICS6 == "452910")
  ware_gro = ware[ware$Company == "WALMART" | 
                    ware$Company == "WALMART SUPERCENTER" |
                    ware$Company == "COSTCO WHOLESALE" | 
                    ware$Company == "COSTCO" | 
                    ware$Company == "COSTCO CORP" |
                    ware$Company == "COSTCO BUSINESS CTR" | 
                    ware$Company == "SAM'S CLUB" | 
                    ware$Company == "SAM'S CLUB NOW"|
                    ware$Company == "BJ'S WHOLESALE CLUB",]
  
  #combine all categories
  poi_grocery = rbind(grocery, dept_gro, ware_gro)
  poi_grocery$latlong = paste(poi_grocery$lat, poi_grocery$long, sep=",")
  write.csv(poi_grocery, paste("D:/Accessibility_study/POI_cities/poi_",service_city,"_grocery.csv",sep=""))
  return(poi_grocery)
}

