rm(list=ls())

library(raster)
library(sp)
library(sf)
library(terra)
library(dplyr)
library(lubridate)
#library(hydroGOF)

#library(roxygen2)
#roxygenise()

station <- "75.23.0"
dtm_res <- 1
cloud <- 30 #max cloud cover in modis derived sca

source("R/SGT_par.R")
source("R/KGE.R")
source("R/NSE.R")
source("R/stat_DDD.R")


#neccesary paths and files
path_dtm <-"K:/RESSURSER/Maps_DTMs/Norge/DTM/Norway/Norway_DTM_10m_2023_UTM33/DTM10_UTM33_20230915"
path_BestPar <- "C:\\Users\\luis.barreiro.TERRATEC-OSL\\Documents\\GitHub\\master_thesis\\DDD_modell\\Best24hSN2018EB"
path_GIS <- "C:/Users/luis.barreiro.TERRATEC-OSL/Documents/GitHub/master_thesis/GIS"
path_DDD <- "C:/Users/luis.barreiro.TERRATEC-OSL/Documents/GitHub/master_thesis/DDD_modell"
path_DDD_R <- "C:/Users/luis.barreiro.TERRATEC-OSL/Documents/GitHub/master_thesis/DDD_modell/utdata2018EB"
path_DDD_T <- "C:/Users/luis.barreiro.TERRATEC-OSL/Documents/GitHub/master_thesis/DDD_modell/utdata2018EB_SD_terrain_v04"
path_MODIS <- "C:/Users/luis.barreiro.TERRATEC-OSL/Documents/GitHub/master_thesis/other_docs/Felt_0310_0710/"

shp <- shapefile(file.path(path_GIS, "HBVFelt_2012_geo_UTM33.shp"))

#load ar50
ar50w <- read_sf("D:/temp/ar50w.gpkg")
ar50f <- read_sf("D:/temp/ar50f.gpkg")


#calculate snow gamma parameters
table1 <- SGT_par(path_dtm, shp, "75.23.0", path_BestPar, ar50f, ar50w)

#assess the efficiency of SG_R and SG_T with DDD output
table2 <- stat_DDD(station, dtm_res, path_DDD_R, path_DDD_T, path_MODIS, cloud = 30)
