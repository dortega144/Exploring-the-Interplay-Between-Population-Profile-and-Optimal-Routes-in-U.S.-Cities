library(openrouteservice)
library(tidycensus)
library(tidyverse)
library(tigris)
library(sf)
library(dplyr)
library(BH)
library(ggplot2)
library(crsuggest)
library(terra)
library(geostatsp)
library(ggplot2)
library(moments)
library(fitdistrplus)
#census key
#INSERT YOUR KEY
options(tigris_use_cache = TRUE)
atx <- tracts('OH', year=2021)
#SELCET CRS
suggest_crs(atx)
################################################## TRACTS OH STATE CRS
CCRRSS=6548
atx <- st_transform(atx, crs = CCRRSS)
st_crs(atx)
################################################### CITIES 

#CITY
kc_metro <- get_acs(
  state = "OH",
  geography = "place",
  variables = c(avhoinc="B19013_001",
                topopu = "B01003_001"), #av median in, total popul
  geometry = TRUE, #TRUE PARA MAPAS
  output="wide",
  year = 2021
)
View(kc_metro)
suggest_crs(kc_metro)

##################################################### PLACE (Cleveland)

kc_metro <- get_acs(
  state = "OH",
  geography = "place",
  variables = c(avhoinc="B19013_001",
                topopu = "B01003_001"), #av median in, total popul
  geometry = TRUE, #TRUE PARA MAPAS
  output="wide",
  year = 2021
)%>%
 # filter(str_detect(NAME, "Cleveland city, Ohio")) %>%
  filter(str_detect(topopuE, "374861")) %>% #USE POP AS A CODE TO IDENTIFY
    st_transform(CCRRSS)

pop=374861

##################################################### COOKIE CUT
# TRACTS THAT INTERSECTS
atx2 <- atx[kc_metro,op=st_intersects]
plot(atx2)
############################################### CENTROIDS INSIDE
# CENTROIDS FROM THE TRACTS 
dc<-st_centroid(atx2)
#ONLY INSIDE
dc2<-dc[kc_metro,op=st_within]
df<-st_coordinates(dc2)
# DISTANCE MATRIX
matrix=st_distance(dc2,dc2)
teo_dis=c(matrix)
write.csv(matrix,"teod.csv")
#PLOT TO BE SURE####################################################
city<-ggplot() +
  geom_sf(data = atx, fill = "white", color = "grey") +
  geom_sf(data = kc_metro, fill = NA, color = "red") +
  geom_sf(data=dc,color="red") +
  geom_sf(data=dc2,color="blue") +
  theme_void()
#SAVE
pdf("city.pdf")
print(city) 
dev.off() 
city
################################################ OPEN ROUTE SERVICE. DOCKER MUST BE STARTED
#NAD83
#dc2<-st_transform(dc2,"+proj=longlat +datum=NAD83")
#ORS
dc2 <- st_transform(dc2, crs = 4326)
df<-as.data.frame(st_coordinates(dc2))
options(openrouteservice.url = "http://localhost:8080/ors")
res = ors_matrix(df,profile="driving-car", metrics = c("duration", "distance"), units = "km")
# duration in hours
times=res$durations / 3600
times=c(times)
# distance in km
dis=res$distances
dis=c(dis)
# MATRICES TIMES AND DISTANCES
write.csv(times,"orstimes.csv")
write.csv(dis,"orsdis.csv")
################################################################
# FOR CITIES CSV
dfsalida=data.frame(nrow(dc2),pop,mean(times),mean(dis),mean(teo_dis),sd(times),sd(dis),sd(teo_dis),sum(times),sum(dis),sum(teo_dis),skewness(times),skewness(dis),skewness(teo_dis))
write.csv(dfsalida,"fin.csv")
##################################################################
