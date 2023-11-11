library(sf)


#working with ar50: only do first time. After that you just load ar50f and ar50w as objects
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
