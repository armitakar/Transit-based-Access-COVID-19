#dev.opentripplanner.org/apidoc/0.15.0/resource_PlannerResource.html
#https://developers.google.com/transit/gtfs/reference#routestxt

library(httr)
library(otpr)
library(progress)
library(geojson)
library(geojsonR)
library(geojsonio)

library(sf)
library(mapdeck)
library(geojsonsf)
library(geosphere)
library(dismo)
library(rgeos)
library(dbscan)


library(tidycensus)
library(tigris)
library(magrittr)
library(dplyr)
library(tidyverse)

#states() %>% plot()
options(tigris_use_cache = TRUE)


library(ggplot2)
library(tidytransit)
library(dplyr)

### cmd instructions
#cd C:\Users\armit\otp
#set path=%path%;C:\Program Files\jre1.8.0_261\bin
#java
#java -Xmx2G -jar otp.jar --build graphs/current
#java -Xmx2G -jar otp.jar --router current --graphs graphs --server


####### getting census data ########
census_api_key("0374af53cfe8d2728204af53395977de1b54b17d")

# variable serach
v18 <- load_variables(2018, "acs5", cache = TRUE)

### function to extract blockgroup-level ACS data for each state
acs_state = function(state_code){
  acs_data = get_acs(geography = "block group",
          state = state_code,
          #county = "Franklin County",
          variables = c(tot_pop = "B01003_001", #total population
                        pop_over25 = "B15003_001", #population over age 25
                        white = "B02001_002", #total white population
                        black = "B02001_003", #total black population
                        NHW = "B03002_003", #total non-hispanic white population
                        hispanic = "B03002_012", #total hispanic population
                        high_school = "B15003_018", #total population with a high school degree
                        med_inc = "B19013_001", #median income
                        poverty = "B17017_002", #total households living below poverty level
                        no_ins_under19 = "B27010_017", #population group under 19 with no medical insurance
                        no_ins_19_34 = "B27010_033", #population group within age 19-34 with no medical insurance
                        no_ins_35_64 = "B27010_050", #population group within age 35-64 with no medical insurance
                        no_ins_over64 = "B27010_066", #population group over 64 with no medical insurance
                        HH_size = "B11001_001", #total number of households
                        owner_no_veh = "B25044_003", #households with no vehicle (owners of the housing unit)
                        renter_no_veh = "B25044_010", #households with no vehicle (renters of the housing unit)
                        house_own = "B25003_002"), #housing units occupied by owners
          year = 2018,
          survey = "acs5",
          geometry = TRUE)
  print("acs_data_downloaded")
  acs_data <- select(acs_data, -moe)
  acs_data1 <- spread(acs_data, key = variable, value = estimate)
  acs_data1$p_n_whi = ((1- acs_data1$white)/acs_data1$tot_pop)*100
  acs_data1$p_his = (acs_data1$hispanic/acs_data1$tot_pop)*100
  acs_data1$p_hs = (acs_data1$high_school/acs_data1$pop_over25)*100
  acs_data1$p_pov = (acs_data1$poverty/acs_data1$HH_size)*100
  acs_data1$p_noins = ((acs_data1$no_ins_under19 + acs_data1$no_ins_19_34 +
                             acs_data1$no_ins_35_64 + acs_data1$no_ins_over64)/acs_data1$tot_pop)*100
  acs_data1$p_ow_nv = (acs_data1$owner_no_veh/acs_data1$HH_size)*100
  acs_data1$p_re_nv = (acs_data1$renter_no_veh/acs_data1$HH_size)*100
  acs_data1$p_own = (acs_data1$house_own/acs_data1$HH_size)*100
  acs_data1_prj = st_transform(acs_data1, crs = "EPSG:5070") 
  return(acs_data1_prj)
}


#### determine study area ####
study_area = function(acs_state_area, gtfs_city_date, shapefile_name){
  #stop = read.csv(paste("D:/Accessibility_study/gtfs_data/",gtfs_city_date,"./stops.txt",sep=""),
  #                stringsAsFactors = FALSE)
  file = paste("D:/Accessibility_study/gtfs_data/",gtfs_city_date,".zip",sep="")
  gtfs = read_gtfs(file, quiet = TRUE)
  select_service_id <- gtfs$calendar %>% pull(service_id)
  
  #route type (0: light rail, 1:subway, 3:bus, 5: cable tram)
  select_route_id <- filter(gtfs$routes, route_type == 0|route_type == 1|route_type == 3|route_type == 5) %>% pull(route_id)
  stop <- filter_stops(gtfs, select_service_id, select_route_id)
  sf_stop = st_as_sf(stop, coords = c("stop_lon", "stop_lat"), 
                     crs = 4326, agr = "constant")
  sf_stop_prj = sf_stop %>%
    st_transform(sf_stop, crs = "EPSG:5070")
  sf_stop_prj %>% ggplot() +
    geom_sf(color = "black")
  st_write(sf_stop_prj, dsn = paste("D:/Accessibility_study/city_shapefiles/",shapefile_name,"_stop.shp",sep=""),
           driver = "ESRI Shapefile")
  print("exported stop data to shapefiles")
  
  #half mile buffer around each stop
  stop_area = st_buffer(sf_stop_prj, 805) %>%
    st_union() #half mile (approx 10min walk)
  area = acs_state_area%>%
    filter(st_intersects(x = ., y = stop_area, sparse = FALSE))
  st_write(area, dsn = paste("D:/Accessibility_study/city_shapefiles/",shapefile_name,"_coverage.shp",sep=""),
           driver = "ESRI Shapefile")
  print("exported study area to shapefiles")
  return(area)
}


#### isochrones for point of interests 
iso <- function(poi, Date, Time, Cutoff, Date_str, poi_type, city, type, excluded_routes) {
  Total = nrow(poi)
  for (i in 1:Total) {
    print(poi[i, ]$latlong)
    transit_current <- GET(
      "http://localhost:8080/otp/routers/current/isochrone",
      query = list(
        fromPlace = poi[i, ]$latlong,
        toPlace = poi[i, ]$latlong,
        arriveBy = TRUE,
        mode = "WALK, TRANSIT", # modes we want the route planner to use
        bannedRoutes = excluded_routes, #exclude the routes except light rail, subway, bus, cable tram
        maxWalkDistance = 800, #half mile (approx 10min walk)
        maxTransfers = 1,
        minTransferTime = 300, #5 min
        date = Date,
        time= Time,
        cutoffSec = Cutoff
      )
    )
    transit_current <- content(transit_current, as = "text", encoding = "UTF-8")
    write(transit_current, file = paste("D:/Accessibility_study/isochrones/",i, ".geojson", sep = ""))
  }
  p1 = st_transform(st_simplify(geojson_sf("D:/Accessibility_study/isochrones/1.geojson")),"EPSG:5070")
  i = 2
  while (i < (Total+1)){
    sf = geojson_sf(paste("D:/Accessibility_study/isochrones/",i,".geojson",sep=""))
    p = st_transform(st_simplify(sf), "EPSG:5070")
    sf_new <- st_union(p, st_make_valid(p1))
    print(i)
    i = i + 1
    p1 = sf_new
  }
  p1 = p1[,1:1]
  write(sf_geojson(p1), file = paste("D:/Accessibility_study/isochrones_by_cities/EPSG 5070/", city, "/", type, "/", poi_type,"_", Date_str,"_", Time, "_", Cutoff, ".geojson", sep = ""))
}

#### Function to check available route types for each city
check_route_type <- function(gtfs_city_date) {
  zipF<- paste("D:/Accessibility_study/gtfs_data/",gtfs_city_date,".zip",sep="")
  outDir<-paste("D:/Accessibility_study/gtfs_data/",gtfs_city_date, sep="")
  unzip(zipF, exdir = outDir)
  route = read.csv(paste("D:/Accessibility_study/gtfs_data/",gtfs_city_date,"/routes.txt",sep=""))
  print(unique(route$route_type))
  route1 = route[route$route_type== 0 | route$route_type== 1 | route$route_type== 3|route$route_type== 5, ]
  route2 = route1[complete.cases(route1$route_type),]
  write.csv(route2, paste("D:/Accessibility_study/gtfs_data/",gtfs_city_date,"/routes.txt",sep=""))
}

### Function to exclude the routes except light rail, subway, bus, cable tram
get_excluded_routes <- function(gtfs_city_date){
  file = paste("D:/Accessibility_study/gtfs_data/",gtfs_city_date,".zip",sep="")
  gtfs = read_gtfs(file, quiet = TRUE)
  select_route <- gtfs$routes[gtfs$routes$route_type == 2 | 
                                gtfs$routes$route_type == 4 |
                                gtfs$routes$route_type == 6 |
                                gtfs$routes$route_type == 7 |
                                gtfs$routes$route_type == 11 |
                                gtfs$routes$route_type == 12, ]
  excluded_routes = c()
  for (i in 1:length(select_route)) {
    val = paste(select_route$agency_id[i],select_route$route_id[i],sep="_")
    excluded_routes = c(excluded_routes, val )}
  excluded_routes = list(excluded_routes)
  #select_service_id <- gtfs$calendar %>% pull(service_id)
  #select_route_id <- filter(gtfs$routes, route_type == 0|route_type == 1|route_type == 3) %>% pull(route_id)
  #filtered_stops_df <- filter_stops(gtfs, select_service_id, select_route_id)
  return(excluded_routes)
}

### Function to identify stops for selected route types
get_stops <- function(gtfs_city_date){
  file = paste("D:/Accessibility_study/gtfs_data/",gtfs_city_date,".zip",sep="")
  gtfs = read_gtfs(file, quiet = TRUE)
  select_service_id <- gtfs$calendar %>% pull(service_id)
  select_route_id <- filter(gtfs$routes, route_type == 0|route_type == 1|route_type == 3|route_type == 5) %>% pull(route_id)
  filtered_stops_df <- filter_stops(gtfs, select_service_id, select_route_id)
  return(filtered_stops_df)
}

### list of sample times for measuring isochrones
comb = list(list("9.00 am", 1800), list("9.10 am", 1800),
            list("9.20 am", 1800), list("9.30 am", 1800),
            list("9.40 am", 1800), list("9.50 am", 1800),
            list("1.00 pm", 1800),
            list("1.10 pm", 1800), list("1.20 pm", 1800),
            list("1.30 pm", 1800), list("1.40 pm", 1800),
            list("1.50 pm", 1800), 
            list("9.00 am", 2700), list("9.10 am", 2700),
            list("9.20 am", 2700), list("9.30 am", 2700),
            list("9.40 am", 2700), list("9.50 am", 2700),
            list("1.00 pm", 2700),
            list("1.10 pm", 2700), list("1.20 pm", 2700),
            list("1.30 pm", 2700), list("1.40 pm", 2700),
            list("1.50 pm", 2700))


#### getting ACS data and study area #####
#Ann Arbor
acs_MI = acs_state("MI")
annarbor_study_area = study_area(acs_MI, "gtfs_TheRide_annarbor/gtfs_21_january", 
                                 "TheRide_annarbor")

#Atlanta
acs_GA = acs_state("GA")
altanta_study_area = study_area(acs_GA, "gtfs_MARTA_atlanta/gtfs_6_december", 
                                "MARTA_atlanta")

#Boston
acs_MA = acs_state("MA")
boston_study_area = study_area(acs_MA, "gtfs_MBTA_boston/gtfs_3_january", 
                               "MBTA_boston")

#Chicago, Champaign
acs_IL = acs_state("IL")
champaign_study_area = study_area(acs_IL, "gtfs_CUMTD_champaign/gtfs_3_january", 
                                  "CUMTD_champaign")
chicago_study_area = study_area(acs_IL, "gtfs_CTA_chicago/gtfs_21_december", 
                                "CTA_chicago")

#Columbus
acs_OH = acs_state("OH")
columbus_study_area = study_area(acs_OH, "gtfs_COTA_columbus/gtfs_4_january_columbus", 
                                 "cota_columbus")

#Austin, Dallas
acs_TX = acs_state("TX")
dallas_study_area = study_area(acs_TX, "gtfs_DART_dallas/gtfs_1_january", 
                               "DART_dallas")
austin_study_area = study_area(acs_TX, "gtfs_capitalmetro_austin/gtfs_7_january", 
                               "capitalmetro_austin")

#Denver
acs_CO = acs_state("CO")
denver_study_area = study_area(acs_CO, "gtfs_RTD_denver/gtfs_1_january_denver", 
                               "denver_RTD")

#LA, SJ, SF
acs_CA = acs_state("CA")
LA_study_area_bus = study_area(acs_CA, "gtfs_LAmetro_la/gtfs_9_january_bus", 
                               "LAmetro_bus_la")
LA_study_area_rail = study_area(acs_CA, "gtfs_LAmetro_la/gtfs_9_january_rail", 
                                "LAmetro_rail_la")
sanjose_study_area = study_area(acs_CA, "gfts_VTA_sanJose/gtfs_23_january", 
                                "VTA_sanJose")
SF_study_area = study_area(acs_CA, "gtfs_SFMTA_sanfrancisco/gtfs_22_january", 
                           "SFMTA_sanfrancisco")

#Louisville
acs_KY = acs_state("KY")
louisville_study_area = study_area(acs_KY, "gfts_TARC_louisville/gtfs_23_january", 
                                   "TARC_louisville")

#Madison
acs_WI = acs_state("WI")
madison_study_area = study_area(acs_WI, "gtfs_metro_madison/gtfs_30_january", 
                                "metro_madison")

#Miami
acs_FL = acs_state("FL")
miami_study_area = study_area(acs_FL, "gfts_MDC_miami/gtfs_24_february", 
                              "MDC_miami")

#Nashville
acs_TN = acs_state("TN")
nashvville_study_area = study_area(acs_TN, "gtfs_MTA_nashville/gtfs_24_january", 
                                   "MTA_nashville")

#NY
acs_NY = acs_state("NY")
nyc_study_area_subway = study_area(acs_NY, "gtfs_MTA_NYC/gtfs_31_december_subway", 
                                 "MTA_subway_nyc")
nyc_study_area_bus_company = study_area(acs_NY, "gtfs_MTA_NYC/gtfs_4_january_bus_company", 
                                   "MTA_bus_company_nyc")
nyc_study_area_bronx = study_area(acs_NY, "gtfs_MTA_NYC/gtfs_3_january_bronx", 
                                        "MTA_bronx_nyc")
nyc_study_area_queens = study_area(acs_NY, "gtfs_MTA_NYC/gtfs_3_january_queens", 
                                        "MTA_queens_nyc")
nyc_study_area_brooklyn = study_area(acs_NY, "gtfs_MTA_NYC/gtfs_4_january_brooklyn", 
                                        "MTA_brooklyn_nyc")
nyc_study_area_manhattan = study_area(acs_NY, "gtfs_MTA_NYC/gtfs_4_january_manhattan", 
                                        "MTA_manhattan_nyc")
nyc_study_area_staten_island = study_area(acs_NY, "gtfs_MTA_NYC/gtfs_4_january_staten_island", 
                                      "MTA_staten_island_nyc")

#Philadelphia
acs_PA = acs_state("PA")
philadelphia_study_area = study_area(acs_PA, "gtfs_SEPTA_philadelphia/gtfs_24_january", 
                                     "SEPTA_philadelphia")

#Phoenix
acs_AZ = acs_state("AZ")
phoenix_study_area = study_area(acs_AZ, "gtfs_valleymetro_phoenix/gtfs_17_january", 
                                "valleymetro_phoenix")
 #Portland
acs_OR = acs_state("OR")
portland_study_area = study_area(acs_OR, "gtfs_TriMet_portland/gtfs_11_january_portland", 
                                 "TriMet_portland")

#Seatle
acs_WA = acs_state("WA")
check_route_type("gtfs_king_county_metro_seattle/gtfs_7_january")
seattle_study_area = study_area(acs_WA, "gtfs_king_county_metro_seattle/gtfs_29_january", 
                               "king_county_metro_seattle")

#Salt Lake City
acs_UT = acs_state("UT")
slc_study_area = study_area(acs_UT, "gtfs_UTA_slc/gtfs_7_january", 
                                 "UTA_slc")





##### isochrones calculation ######

#### isochrones for columbus ####
transfer = read.csv("D:/Accessibility_study/gtfs_data/gtfs_COTA_columbus/gtfs_6_december/transfers.txt")
transfer1 = transfer[is.na(transfer$transfer_type) == FALSE,]
write.csv(transfer1, "D:/Accessibility_study/gtfs_data/gtfs_COTA_columbus/gtfs_6_december/transfers.txt")

#delete pathways, export the file to zip
dat = "D:/Accessibility_study/gtfs_data/gtfs_COTA_columbus/gtfs_6_december_edited.zip"
dat_gtfs = read_gtfs(dat, quiet = TRUE)
write_gtfs(dat_gtfs,
           "D:/Accessibility_study/gtfs_data/gtfs_COTA_columbus/gtfs_6_december1.zip")


cota_poi = info_data("columbus_cota","OH")
grocery = poi_grocery(cota_poi, "cota_columbus")
poi_urgent = health_poi_urgent("columbus_cota", "cota_columbus")
poi_nonurgent = health_poi_nonurgent("columbus_cota", "cota_columbus")

for (i in comb){iso(grocery, "12/8/2020", i[[1]], i[[2]], "Dec08", "grocery", "isochrones_columbus", "grocery")}
for (i in comb){iso(poi_urgent, "5/5/2020", i[[1]], i[[2]], "May05", "health", "isochrones_columbus", "health_urgent")}
for (i in comb){iso(poi_nonurgent, "5/5/2020", i[[1]], i[[2]], "May05", "health", "isochrones_columbus", "health_nonurgent")}

#### isochrones for Philadelphia ####
SEPTA_poi = info_data("Philadelphia_SEPTA","PA")
grocery = poi_grocery(SEPTA_poi, "SEPTA_Philadelphia")
poi_urgent = health_poi_urgent("Philadelphia_SEPTA", "SEPTA_Philadelphia")
poi_nonurgent = health_poi_nonurgent("Philadelphia_SEPTA", "SEPTA_Philadelphia")
for (i in comb){iso(grocery, "11/17/2020", i[[1]], i[[2]], "Nov17", "grocery", "isochrones_philadelphia", "grocery")}
for (i in comb){iso(poi_urgent, "11/17/2020", i[[1]], i[[2]], "Nov17", "health", "isochrones_philadelphia", "health_urgent")}
for (i in comb){iso(poi_nonurgent, "11/17/2020", i[[1]], i[[2]], "Nov17", "health", "isochrones_philadelphia", "health_nonurgent")}


#### isochrones for Atlanta #####
dat = "X:/Accessibility_study/gtfs_data/gtfs_MARTA_atlanta/gtfs_4_december_2020.zip"
dat_gtfs = read_gtfs(dat, quiet = TRUE)
write_gtfs(dat_gtfs,
           "X:/Accessibility_study/gtfs_data/gtfs_MARTA_atlanta/gtfs_4_december_2020.zip")


MARTA_poi = info_data("atlanta_MARTA","GA")
grocery = poi_grocery(MARTA_poi, "MARTA_atlanta")
poi_urgent = health_poi_urgent("atlanta_MARTA", "MARTA_atlanta")
poi_urgent1 = poi_urgent %>% distinct(latlong, .keep_all = TRUE)
poi_nonurgent = health_poi_nonurgent("atlanta_MARTA", "MARTA_atlanta")
poi_nonurgent1 = poi_nonurgent %>% distinct(latlong, .keep_all = TRUE)
for (i in comb){iso(grocery, "12/8/2020", i[[1]], i[[2]], "Dec08", "grocery", "isochrones_atlanta", "grocery")}
for (i in comb){iso(poi_urgent1, "1/7/2020", i[[1]], i[[2]], "Jan07", "health", "isochrones_atlanta", "health_urgent")}
for (i in comb){iso(poi_nonurgent1, "1/7/2020", i[[1]], i[[2]], "Jan07", "health", "isochrones_atlanta", "health_nonurgent")}

#### isochrones for Louisville #####
TARC_poi = info_data("louisville_TARC","KY")
grocery = poi_grocery(TARC_poi, "TARC_louisville")
poi_urgent = health_poi_urgent("louisville_TARC", "TARC_louisville")
poi_nonurgent = health_poi_nonurgent("louisville_TARC", "TARC_louisville")
for (i in comb){iso(grocery, "11/17/2020", i[[1]], i[[2]], "Nov17", "grocery", "isochrones_louisville", "grocery")}
for (i in comb){iso(poi_urgent, "11/17/2020", i[[1]], i[[2]], "Nov17", "health", "isochrones_louisville", "health_urgent")}
for (i in comb){iso(poi_nonurgent, "11/17/2020", i[[1]], i[[2]], "Nov17", "health", "isochrones_louisville", "health_nonurgent")}

#### isochrones for Miami #####

MDC_poi = info_data("miami_MDC","FL")
grocery = poi_grocery(MDC_poi, "MDC_miami")
poi_urgent = health_poi_urgent("miami_MDC", "MDC_miami")
poi_nonurgent = health_poi_nonurgent("miami_MDC", "MDC_miami")
excluded_routes = get_excluded_routes("gfts_MDC_miami/gtfs_15_november_2019")
for (i in comb){iso(grocery, "11/3/2020", i[[1]], i[[2]], "Nov03", "grocery", "isochrones_miami", "grocery", excluded_routes)}
for (i in comb){iso(poi_urgent, "11/3/2020", i[[1]], i[[2]], "Nov03", "health", "isochrones_miami", "health_urgent",excluded_routes)}
for (i in comb){iso(poi_nonurgent, "11/3/2020", i[[1]], i[[2]], "Nov03", "health", "isochrones_miami", "health_nonurgent",excluded_routes)}


###isochrones for nashville #####
MTA_poi = info_data("nashville_MTA","TN")
grocery = poi_grocery(MTA_poi, "MTA_nashville")
poi_urgent = health_poi_urgent("nashville_MTA", "MTA_nashville")
poi_nonurgent = health_poi_nonurgent("nashville_MTA", "MTA_nashville")
excluded_routes = get_excluded_routes("gtfs_MTA_nashville/gtfs_17_november")
for (i in comb){iso(grocery, "11/24/2020", i[[1]], i[[2]], "Nov24", "grocery", "isochrones_nashville", "grocery", excluded_routes)}
for (i in comb){iso(poi_urgent, "11/24/2020", i[[1]], i[[2]], "Nov24", "health", "isochrones_nashville", "health_urgent", excluded_routes)}
for (i in comb){iso(poi_nonurgent, "11/24/2020", i[[1]], i[[2]], "Nov24", "health", "isochrones_nashville", "health_nonurgent", excluded_routes)}


#### isochrones for Dallas ####
DART_poi = info_data("dallas_DART","TX")
grocery = poi_grocery(DART_poi, "DART_dallas")
poi_urgent = health_poi_urgent("dallas_DART", "DART_dallas")
poi_nonurgent = health_poi_nonurgent("dallas_DART", "DART_dallas")
excluded_routes = get_excluded_routes("gtfs_DART_dallas/gtfs_2_december")
for (i in comb){iso(grocery, "12/08/2020", i[[1]], i[[2]], "Dec08", "grocery", "isochrones_dallas", "grocery",excluded_routes)}
for (i in comb){iso(poi_urgent, "12/08/2020", i[[1]], i[[2]], "Dec08", "health","isochrones_dallas", "health_urgent", excluded_routes)}
for (i in comb){iso(poi_nonurgent, "12/08/2020", i[[1]], i[[2]], "Dec08", "health","isochrones_dallas", "health_nonurgent", excluded_routes)}


#### isochrones for denver ####
RTD_poi = info_data("denver_RTD","CO")
grocery = poi_grocery(RTD_poi, "RTD_denver")
poi_urgent = health_poi_urgent("denver_RTD", "RTD_denver")
poi_nonurgent = health_poi_nonurgent("denver_RTD", "RTD_denver")
excluded_routes = get_excluded_routes("gtfs_RTD_denver/gtfs_21_november")
for (i in comb){iso(grocery, "11/24/2020", i[[1]], i[[2]], "Nov24", "grocery","isochrones_denver", "grocery",excluded_routes)}
for (i in comb){iso(poi_urgent, "11/24/2020", i[[1]], i[[2]], "Nov24", "health","isochrones_denver", "health_urgent",excluded_routes)}
for (i in comb){iso(poi_nonurgent, "11/24/2020", i[[1]], i[[2]], "Nov24", "health","isochrones_denver", "health_nonurgent",excluded_routes)}


#### isochrones for Portland ####
TriMet_poi = info_data("portland_TriMet","OR")
grocery = poi_grocery(TriMet_poi, "TriMet_portland")
poi_urgent = health_poi_urgent("portland_TriMet", "TriMet_portland")
poi_nonurgent = health_poi_nonurgent("portland_TriMet", "TriMet_portland")
excluded_routes = get_excluded_routes("gtfs_TriMet_portland/gtfs_11_january_portland")
for (i in comb){iso(grocery, "12/15/2020", i[[1]], i[[2]], "Dec15", "grocery", "isochrones_portland", "grocery",excluded_routes)}
for (i in comb){iso(poi_urgent, "12/15/2020", i[[1]], i[[2]], "Dec15", "health","isochrones_portland", "health_urgent",excluded_routes)}
for (i in comb){iso(poi_nonurgent, "12/15/2020", i[[1]], i[[2]], "Dec15", "health","isochrones_portland", "health_nonurgent",excluded_routes)}


#### isochrones for SLC ####
UTA_poi = info_data("slc_UTA","UT")
grocery = poi_grocery(UTA_poi, "UTA_slc")
poi_urgent = health_poi_urgent("slc_UTA", "UTA_slc")
poi_urgent1 = poi_urgent %>% distinct(latlong, .keep_all = TRUE)
poi_urgent2 = poi_urgent1 %>% slice(-c(267,268))

poi_nonurgent = health_poi_nonurgent("slc_UTA", "UTA_slc")
poi_nonurgent1 = poi_nonurgent %>% distinct(latlong, .keep_all = TRUE)
#poi_urgent1 = poi_urgent %>% slice(-c(514,515))
poi_nonurgent2 = poi_nonurgent1 %>% slice(-c(122))
poi_nonurgent3 = poi_nonurgent2 %>% slice(-c(371))
excluded_routes = get_excluded_routes("gtfs_UTA_slc/gtfs_7_january")
for (i in comb){iso(grocery, "11/24/2020", i[[1]], i[[2]], "Nov24", "grocery","isochrones_slc", "grocery",excluded_routes)}
for (i in comb){iso(poi_urgent2, "11/24/2020", i[[1]], i[[2]], "Nov24", "health","isochrones_slc", "health_urgent",excluded_routes)}
for (i in comb){iso(poi_nonurgent3, "11/24/2020", i[[1]], i[[2]], "Nov24", "health","isochrones_slc", "health_nonurgent",excluded_routes)}


#### isochrones for LA ####
lametro_poi = poi_data("la_LAmetro","CA")
health = poi_health(lametro_poi, "LAmetro_la")
lametro_poi = info_data("la_LAmetro","CA")
grocery = poi_grocery(lametro_poi, "LAmetro_la")
grocery_95 = poi_grocery_95(lametro_poi, "LAmetro_la")
for (i in comb){iso(grocery, "12/1/2020", i[[1]], i[[2]], "Dec01", "grocery", "isochrones_LA", "grocery")}
for (i in comb){iso(grocery_95, "12/1/2020", i[[1]], i[[2]], "Dec01", "grocery_95", "isochrones_LA", "grocery_95")}
for (i in comb){iso(health, "12/01/2020", i[[1]], i[[2]], "Dec01", "health")}


#### isochrones for Seattle ####
kingcounty_poi = info_data("seattle_king_county_metro","WA")
grocery = poi_grocery(kingcounty_poi, "kingcounty_seattle")
poi_urgent = health_poi_urgent("seattle_king_county_metro", "kingcounty_seattle")
poi_nonurgent = health_poi_nonurgent("seattle_king_county_metro", "kingcounty_seattle")
ex_routes = get_excluded_routes("gtfs_king_county_metro_seattle/gtfs_4_december")
for (i in comb){iso(grocery, "12/8/2020", i[[1]], i[[2]], "Dec08", "grocery","isochrones_seattle", "grocery", ex_routes)}
for (i in comb){iso(poi_urgent, "12/8/2020", i[[1]], i[[2]], "Dec08", "health","isochrones_seattle", "health_urgent", ex_routes)}
for (i in comb){iso(poi_nonurgent, "12/8/2020", i[[1]], i[[2]], "Dec08", "health","isochrones_seattle", "health_nonurgent", ex_routes)}


#### isochrones for Austin ####
Capitalmetro_poi = info_data("Austin_capitalmetro","TX")
grocery = poi_grocery(Capitalmetro_poi, "Capitalmetro_Austin")
poi_urgent = health_poi_urgent("Austin_capitalmetro", "Capitalmetro_Austin")
poi_nonurgent = health_poi_nonurgent("Austin_capitalmetro", "Capitalmetro_Austin")
poi_nonurgent1 <- poi_nonurgent %>% slice(-c(339))
for (i in comb){iso(grocery, "12/1/2020", i[[1]], i[[2]], "Dec01", "grocery", "isochrones_austin", "grocery")}
for (i in comb){iso(poi_urgent, "12/1/2020", i[[1]], i[[2]], "Dec01", "health", "isochrones_austin", "health_urgent")}
for (i in comb){iso(poi_nonurgent1, "12/1/2020", i[[1]], i[[2]], "Dec01", "health", "isochrones_austin", "health_nonurgent")}


#### isochrones for Phoenix ####
valleymetro_poi = info_data("phoenix_valleymetro","AZ")
grocery = poi_grocery(valleymetro_poi, "valleymetro_phoenix")
poi_urgent = health_poi_urgent("phoenix_valleymetro", "valleymetro_phoenix")
poi_urgent1 = poi_urgent %>% distinct(latlong, .keep_all = TRUE)
poi_nonurgent = health_poi_nonurgent("phoenix_valleymetro", "valleymetro_phoenix")
poi_nonurgent1 = poi_nonurgent %>% distinct(latlong, .keep_all = TRUE)
for (i in comb){iso(grocery, "12/1/2020", i[[1]], i[[2]], "Dec01", "grocery", "isochrones_phoenix", "grocery")}
for (i in comb){iso(poi_urgent1, "1/21/2020", i[[1]], i[[2]], "Jan21", "health", "isochrones_phoenix", "health_urgent")}
for (i in comb){iso(poi_nonurgent1, "1/21/2020", i[[1]], i[[2]], "Jan21", "health", "isochrones_phoenix", "health_nonurgent")}


#### isochrones for san francisco ####
SFMTA_poi = info_data("sanfrancisco_SFMTA","CA")
grocery = poi_grocery(SFMTA_poi, "SFMTA_sanfrancisco")
poi_urgent = health_poi_urgent("sanfrancisco_SFMTA", "SFMTA_sanfrancisco")
poi_urgent1 = poi_urgent %>% distinct(latlong, .keep_all = TRUE)
poi_nonurgent = health_poi_nonurgent("sanfrancisco_SFMTA", "SFMTA_sanfrancisco")
for (i in comb){iso(grocery, "12/1/2020", i[[1]], i[[2]], "Dec01", "grocery", "isochrones_sanfrancisco", "grocery")}
for (i in comb){iso(poi_urgent1, "12/1/2020", i[[1]], i[[2]], "Dec01", "health", "isochrones_sanfrancisco", "health_urgent")}
for (i in comb){iso(poi_nonurgent, "12/1/2020", i[[1]], i[[2]], "Dec01", "health", "isochrones_sanfrancisco", "health_nonurgent")}


###isochrones for sanjose ####
VTA_poi = info_data("sanjose_VTA","CA")
grocery = poi_grocery(VTA_poi, "VTA_sanjose")
poi_urgent = health_poi_urgent("sanjose_VTA", "VTA_sanjose")
poi_urgent1 = poi_urgent %>% distinct(latlong, .keep_all = TRUE)
poi_nonurgent = health_poi_nonurgent("sanjose_VTA", "VTA_sanjose")
poi_nonurgent1 = poi_nonurgent %>% distinct(latlong, .keep_all = TRUE)
for (i in comb){iso(grocery, "12/1/2020", i[[1]], i[[2]], "Dec01", "grocery", "isochrones_sanjose", "grocery")}
for (i in comb){iso(poi_urgent1, "12/1/2020", i[[1]], i[[2]], "Dec01", "health", "isochrones_sanjose", "health_urgent")}
for (i in comb){iso(poi_nonurgent1, "12/1/2020", i[[1]], i[[2]], "Dec01", "health", "isochrones_sanjose", "health_nonurgent")}

###isochrones for chicago ####
CTA_poi = info_data("chicago_CTA","IL")
grocery = poi_grocery(CTA_poi, "CTA_chicago")
poi_urgent = health_poi_urgent("chicago_CTA", "CTA_chicago")
poi_urgent1 = poi_urgent %>% distinct(latlong, .keep_all = TRUE)
poi_nonurgent = health_poi_nonurgent("chicago_CTA", "CTA_chicago")
poi_nonurgent1 = poi_nonurgent %>% distinct(latlong, .keep_all = TRUE)
for (i in comb){iso(grocery, "4/21/2020", i[[1]], i[[2]], "Apr21", "grocery", "isochrones_chicago", "grocery")}
for (i in comb){iso(poi_urgent, "11/24/2020", i[[1]], i[[2]], "Nov24", "health", "isochrones_chicago", "health_urgent")}
for (i in comb){iso(poi_nonurgent, "11/24/2020", i[[1]], i[[2]], "Nov24", "health", "isochrones_chicago", "health_nonurgent")}


###isochrones for nyc ####
MTA_poi = info_data("nyc_MTA","NY")
grocery = poi_grocery(MTA_poi, "MTA_nyc")
poi_urgent = health_poi_urgent("nyc_MTA", "MTA_nyc")
poi_urgent1 = poi_urgent %>% distinct(latlong, .keep_all = TRUE)
#poi_nonurgent = health_poi_nonurgent("nyc_MTA", "MTA_nyc")
poi_nonurgent = read.csv("D:/Accessibility_study/POI_cities/health/poi_MTA_nyc_nonurgent_health.csv")
poi_nonurgent1 = poi_nonurgent %>% distinct(latlong, .keep_all = TRUE)
for (i in comb){iso(grocery, "12/8/2020", i[[1]], i[[2]], "Dec08", "grocery", "isochrones_nyc", "grocery")}
for (i in comb){iso(poi_urgent1, "12/8/2020", i[[1]], i[[2]], "Dec08", "health", "isochrones_nyc", "health_urgent")}
for (i in comb){iso(poi_nonurgent1, "12/8/2020", i[[1]], i[[2]], "Dec08", "health", "isochrones_nyc", "health_nonurgent")}


#### isochrones for Madison ####
metro_poi = info_data("metro_madison","WI")
grocery = poi_grocery(metro_poi, "metro_madison")
poi_urgent = health_poi_urgent("metro_madison", "metro_madison")
poi_urgent1 = poi_urgent %>% distinct(latlong, .keep_all = TRUE)
poi_nonurgent = health_poi_nonurgent("metro_madison", "metro_madison")
poi_nonurgent1 = poi_nonurgent %>% distinct(latlong, .keep_all = TRUE)
for (i in comb){iso(grocery, "12/8/2020", i[[1]], i[[2]], "Dec08", "grocery", "isochrones_madison", "grocery")}
for (i in comb){iso(poi_urgent1, "3/24/2020", i[[1]], i[[2]], "Mar24", "health", "isochrones_madison", "health_urgent")}
for (i in comb){iso(poi_nonurgent1, "3/24/2020", i[[1]], i[[2]], "Mar24", "health", "isochrones_madison", "health_nonurgent")}


#### isochrones for ann arbor ####
Ride_poi = info_data("annarbor_TheRide","MI")
grocery = poi_grocery(Ride_poi, "TheRide_annarbor")
poi_urgent = health_poi_urgent("annarbor_TheRide", "TheRide_annarbor")
poi_urgent1 = poi_urgent %>% distinct(latlong, .keep_all = TRUE)
poi_nonurgent = health_poi_nonurgent("annarbor_TheRide", "TheRide_annarbor")
poi_nonurgent1 = poi_nonurgent %>% distinct(latlong, .keep_all = TRUE)
for (i in comb){iso(grocery, "11/17/2020", i[[1]], i[[2]], "Nov17", "grocery", "isochrones_annarbor", "grocery")}
for (i in comb){iso(poi_urgent1, "3/31/2020", i[[1]], i[[2]], "Mar31", "health", "isochrones_annarbor", "health_urgent")}
for (i in comb){iso(poi_nonurgent1, "3/31/2020", i[[1]], i[[2]], "Mar31", "health", "isochrones_annarbor", "health_nonurgent")}


#### isochrones for champaign ####
CUMTD_poi = info_data("champaign_CUMTD","IL")
grocery = poi_grocery(CUMTD_poi, "champaign_CUMTD")
poi_urgent = health_poi_urgent("champaign_CUMTD", "champaign_CUMTD")
poi_nonurgent = health_poi_nonurgent1("champaign_CUMTD", "champaign_CUMTD")
for (i in comb){iso(grocery, "11/24/2020", i[[1]], i[[2]], "Nov24", "grocery", "isochrones_champaign", "grocery")}
for (i in comb){iso(poi_urgent, "11/24/2020", i[[1]], i[[2]], "Nov24", "health", "isochrones_champaign", "health_urgent")}
for (i in comb){iso(poi_nonurgent, "11/24/2020", i[[1]], i[[2]], "Nov24", "health", "isochrones_champaign", "health_nonurgent")}



###isochrones for boston ####
stop = read.csv("D:/Accessibility_study/gtfs_data/gtfs_MBTA_boston/gtfs_15_december/stops.txt")
stop1 = stop[is.na(stop$stop_lat) == FALSE,]
stop2 = stop1[stop1$stop_lat != 42.365578 & stop1$stop_lon!=-71.063973,]
write.csv(stop2, "D:/Accessibility_study/gtfs_data/gtfs_MBTA_boston/gtfs_15_december/stops.txt")

stop_time = read.csv("D:/Accessibility_study/gtfs_data/gtfs_MBTA_boston/gtfs_15_december/stop_times.txt")
stop_time1 = stop_time[stop_time$stop_id != 21458,]
write.csv(stop_time1, "D:/Accessibility_study/gtfs_data/gtfs_MBTA_boston/gtfs_15_december/stop_times.txt")

transfer = read.csv("D:/Accessibility_study/gtfs_data/gtfs_MBTA_boston/gtfs_15_december/transfers.txt")
transfer1 = transfer[transfer$from_stop_id != 21458,]
transfer2 = transfer1[transfer1$to_stop_id != 21458,]
write.csv(transfer2, "D:/Accessibility_study/gtfs_data/gtfs_MBTA_boston/gtfs_15_december/transfers.txt")

#delete pathways, export the file to zip
dat = "D:/Accessibility_study/gtfs_data/gtfs_MBTA_boston/gtfs_15_december (2).zip"
dat_gtfs = read_gtfs(dat, quiet = TRUE)
write_gtfs(dat_gtfs,
           "D:/Accessibility_study/gtfs_data/gtfs_MBTA_boston/gtfs_15_december1.zip")


MBTA_poi = info_data("boston_MBTA","MA")
grocery = poi_grocery(MBTA_poi, "MBTA_boston")
poi_urgent = health_poi_urgent("boston_MBTA", "MBTA_boston")
poi_nonurgent = health_poi_nonurgent("boston_MBTA", "MBTA_boston")
excluded_routes = get_excluded_routes("gtfs_MBTA_boston/gtfs_3_january")
for (i in comb){iso(grocery, "12/15/2020", i[[1]], i[[2]], "Dec15", "grocery", "isochrones_boston", "grocery", excluded_routes)}
for (i in comb){iso(poi_urgent, "1/7/2020", i[[1]], i[[2]], "Jan07", "health", "isochrones_boston", "health_urgent", excluded_routes)}
for (i in comb){iso(poi_nonurgent, "1/7/2020", i[[1]], i[[2]], "Jan07", "health", "isochrones_boston", "health_nonurgent", excluded_routes)}






#### Shapefile post-processing####
#### edit geojson 
names = list.files("D:/Accessibility_study/Final isochrones1/health/health_covid_1800_midday",
                   pattern = ".geojson", include.dirs = FALSE)
for (i in names){
  dat1 = geojson_sf(paste("D:/Accessibility_study/isochrones_for_cities/EPSG 5070/isochrones_austin/",i, sep=""))
  dat3 = dat1  %>% st_set_crs(5070)
  dat4 = (st_transform(dat3, 4326))
  dat5 = as.geojson(dat4)
  dat6 = to_geojson(crs_add(dat5,  crs = "{ \"type\": \"name\", \"properties\": { \"name\": \"urn:ogc:def:crs:OGC:1.3:CRS84\" } }"))
  geo_write(dat6, paste("D:/Accessibility_study/Final isochrones1/austin_", i, sep =""))
  print(i)
}



### create shapefile
names = list.files("D:/Accessibility_study/Final isochrones1",
                    pattern = ".geojson", include.dirs = FALSE)
for (i in names){
  dat1 = geojson_sf(paste("D:/Accessibility_study/Final isochrones1/",i, sep=""))
  #dat2 = st_transform(dat1, 5070)
  st_write(dat1, dsn = paste("D:/Accessibility_study/Final isochrones1/shapefiles/",i,sep=""),
           driver = "ESRI Shapefile")
}


rm(list = ls())
