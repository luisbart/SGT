library(raster)
library(sp)
library(sf)
library(terra)
library(dplyr)

library(roxygen2)
roxygenise()

station <- "75.23.0"

#neccesary paths
path_dtm <-"K:/RESSURSER/Maps_DTMs/Norge/DTM/Norway/Norway_DTM_10m_2023_UTM33/DTM10_UTM33_20230915"
path_BestPar <- "C:\\Users\\luis.barreiro.TERRATEC-OSL\\Documents\\GitHub\\master_thesis\\DDD_modell\\Best24hSN2018EB"
path_GIS <- "C:/Users/luis.barreiro.TERRATEC-OSL/Documents/GitHub/master_thesis/GIS"

#Everything under this line: only do first time. After that you just load ar50f and ar50w as objects
#ar50w <- read_sf("D:/temp/ar50w.gpkg")
#ar50f <- read_sf("D:/temp/ar50f.gpkg")

ar50 <- "K:\\RESSURSER\\Maps_DTMs\\Norge\\AR50\\0000_25833_ar50_geodatabase\\ar50gdb.gdb"

#load AR50 geodatabase
ar50_gdb <- file.path("K:\\RESSURSER\\Maps_DTMs\\Norge\\AR50\\0000_25833_ar50_geodatabase\\ar50gdb.gdb")
layers <- st_layers(ar50_gdb)
ar50 <- st_read(dsn = ar50_gdb, layer = "ArealressursFlate")
new_crs <- CRS("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs")
ar50 <- st_transform(ar50, new_crs)

#create and store a file with only water bodies
ar50w <- ar50[ar50$artype == 81,]
ar50w <- st_union(ar50w)
st_write(ar50w, "D:/temp/ar50w.gpkg", driver = "GPKG")


#load ar50 water bodies (artype=81)
layers <- st_layers(ar50)
ar50 <- st_read(dsn = ar50, layer = "ArealressursFlate")
new_crs <- CRS("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs")
ar50 <- st_transform(ar50, new_crs)

ar50w <- ar50[ar50$artype == 81,]
ar50w <- st_union(ar50w)


#First load ar50 forest & wetland polygons (artype 30 & 60)
ar50f <- ar50[ar50$artype %in% c(30, 60),]
ar50f <- st_union(ar50f)
#st_write(ar50f, "D:/temp/ar50f.geojson", driver = "GeoJSON")
#st_write(ar50f, "D:/temp/ar50f.shp")
st_write(ar50f, "D:/temp/ar50f.gpkg", driver = "GPKG")


zones_sf <- st_as_sf(zones_sp)
ar50f_sf <- st_as_sf(ar50f)
ar50f_sf2 <- st_transform(ar50f_sf, new_crs)
zones_sf2 <- st_transform(zones_sf, new_crs)
zones_sf2$area <- st_area(zones_sf2)







