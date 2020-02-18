###########################################################
#################Script for Processing eBird############### 
##################data from default database###############
####################By: Spencer R Keyser###################
###########################2/13/2020#######################
###########################################################

#Package Loading
pacman::p_load('tidyverse', 'data.table', 'DBI', 'RSQLite', 'maptools', 'rgdal', 'rgeos', 'sf', 'sp') #, 'usmap')

#Connect to eBird DB
con.ebird <- DBI::dbConnect(RSQLite::SQLite(), "E:/eBird/ERD2018_DATA_400_SPECIES_Lat25to50_Lng-100to-65.db")

#Check out the eBird tables (only contains one "checklists")
dbListTables(con.ebird)

#Check the fields in the DB
sp <- dbListFields(con.ebird, "checklists")[232:630]

#Wrtie a small query to look at the data 
q1 <- "SELECT Ammospiza_leconteii, LATITUDE, LONGITUDE,
MONTH, DAY, TIME, YEAR FROM checklists WHERE YEAR = 2010 
AND Ammospiza_leconteii = 1 LIMIT 5"

q2 <- "SELECT * FROM checklists LIMIT 5"

q3 <- "SELECT LATITUDE, LONGITUDE WHERE DAY >= 15 & DAY <= 45 FROM checklists"

leconts <- dbGetQuery(con.ebird, q1)
peek <- dbGetQuery(con.ebird, q2)

#Pull out a list of all the species and save as csv to subset DB
#Load in CSV with most prevalent species 
species.list <- read.csv(here::here("Bird_Data/ERD2018_DATA_400_SPECIES_Lat25to50_Lng-100to-65.db.csv"))
SPECIES_NAMES <- species.list$SCI_NAME_KEY[1:300]



#Pull out relevant variables
var_list <- c("SAMPLING_EVENT_ID", 
              "OBSERVER_ID", "LONGITUDE", "LATITUDE", 
              "YEAR","DAY","TIME",
              "SCORE","EFFORT_HRS","EFFORT_DISTANCE_KM",
              "NUMBER_OBSERVERS","I_STATIONARY",
              'ELEV_MEDIAN', 'NORTHNESS_MEDIAN', 'EASTNESS_MEDIAN',   
              "MCD12Q1_LCCS1_FS_C1_1500_PLAND",
              "MCD12Q1_LCCS1_FS_C11_1500_PLAND","MCD12Q1_LCCS1_FS_C12_1500_PLAND",
              "MCD12Q1_LCCS1_FS_C13_1500_PLAND","MCD12Q1_LCCS1_FS_C14_1500_PLAND",
              "MCD12Q1_LCCS1_FS_C16_1500_PLAND","MCD12Q1_LCCS1_FS_C43_1500_PLAND",
              "MCD12Q1_LCCS2_FS_C36_1500_PLAND","MCD12Q1_LCCS2_FS_C9_1500_PLAND",
              "MCD12Q1_LCCS1_FS_C42_1500_PLAND","MCD12Q1_LCCS1_FS_C41_1500_PLAND",
              "MCD12Q1_LCCS1_FS_C31_1500_PLAND","MCD12Q1_LCCS1_FS_C32_1500_PLAND",
              "MCD12Q1_LCCS3_FS_C27_1500_PLAND","MCD12Q1_LCCS3_FS_C50_1500_PLAND",
              as.character(SPECIES_NAMES))

#Create a spatial extent taken from a clipped version of eBird data within the US
# spatial_extent <- list(type = "rectangle",
#                        lon.min = -125.4442444,
#                        lon.max = -66.9850159,
#                        lat.min = 18.924589,
#                        lat.max = 70.284249)
# 
# SPATIAL_EXTENT_LIST <- list(type="rectangle",
#                             lon.min = -100,
#                             lon.max = -65,
#                             lat.min = 35,
#                             lat.max = 50)
# spatial_extent <- list(type = "rectangle",
#                        lon.min = -16,
#                        lon.max = -66.5,
#                        lat.min = 18.5,
#                        lat.max = 70.5)
                       
#Filter data for extracting prtevalent winter species
winter.sql <- tbl(con.ebird, "checklists") %>% 
  # filter(LATITUDE >= SPATIAL_EXTENT_LIST$lat.min &
  #          LATITUDE <= SPATIAL_EXTENT_LIST$lat.max &
  #          LONGITUDE >= SPATIAL_EXTENT_LIST$lon.min &
  #          LONGITUDE <= SPATIAL_EXTENT_LIST$lon.max) %>%
  filter(YEAR >= 2004 & YEAR <= 2018) %>%
  filter(DAY >= 15 & DAY <= 45) %>% #change for months, jan=1-31, jun=152-181
  filter(PRIMARY_CHECKLIST_FLAG == 1) %>%
  filter(I_TRAVELING == 1 | I_STATIONARY == 1) %>%
  filter(EFFORT_HRS <= 3 & EFFORT_HRS > 0) %>%
  filter(EFFORT_DISTANCE_KM <= 5) %>%
  filter(EFFORT_AREA_HA <= 0) %>%
  filter(TIME >= 0 & TIME <= 24) %>%
  filter(SCORE >= -4 & SCORE <= 4) %>%
  filter(NUMBER_OBSERVERS > 0) %>%
  dplyr::select(var_list)

winter.bird <- collect(winter.sql)  


#Load in a shapefile of the US for clipping
unproj <- CRS("+proj=longlat +datum=WGS84")
state_map <- readShapeSpatial("E:/eBird/US_Map/states.shp", proj4string = unproj)

#Remove AK and HI
state_map <- state_map[state_map$STATE_NAME != "Alaska" & state_map$STATE_NAME != "Hawaii", ]

#Pull out the coordinates for the ebird data
dat <- winter.bird %>% dplyr::select(LONGITUDE, LATITUDE)

#Use "coordinates" to create a spatial class for lat/lon of ebird data 
coordinates(dat) <- c("LONGITUDE", "LATITUDE")

#Tell R both coordinates are on the same GCS
proj4string(dat) <- proj4string(state_map)

#Check for lists inside US
inside.us <- !is.na(over(dat, state_map))
glimpse(inside.us)

#Keeps Name of State for the overlaid eBird points
dat$state <- over(dat, state_map)$STATE_NAME

#Pull coordinates from dat that occur inside the US
dat2 <- dat[state_map,]
dat2.df <- as_tibble(dat2)
#Check out the points remaining 
plot(state_map)
points(dat2, pch = 8, col = "red")

max.lat <- max(dat2.df$LATITUDE)
min.lat <- min(dat2.df$LATITUDE)
max.lon <- max(dat2.df$LONGITUDE)
min.lon <- min(dat2.df$LONGITUDE)

rm("dat", "dat2", "leconts", "peek", "inside.us", "dat2.df")

#Merge the datasets to get filtered eBird checklists
winter.bio <- winter.bird %>% filter(LATITUDE <= max.lat &
                                       LATITUDE >= min.lat &
                                       LONGITUDE <= max.lon &
                                       LONGITUDE >= min.lon)

#write.csv(winter.bio, file = here::here("Bird_Data/Jan_Feb_US_Bird_400.csv"))

RSQLite::dbDisconnect(con.ebird)
