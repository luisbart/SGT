#' @title Compute snow gamma parameters per elevation zone
#' @author Luis Barreiro
#' @description Clip raster to polygon. It uses a shapefile (or sf) as input, locate the extent on the raster catalog, crops the raster, then clip
#' @param path path with the raster catalog
#' @param shp single catchment polygon feature (as shapefile or sf)
#' @param station station number, finishin with ".0". F eg "7.23.0"
#' @param path_Bestpar path with catchment parameter file to extract the elevation zone breaks
#' @param ar50w water bodies from AR50 dataset (artype=81)
#' @param ar50f forest & wetlands from AR50 dataset (artype=30 & 60)
#' @returns A table with all elevation zones, Std_SqS, percentage of vegetation, and gamma parameters a0 & D

SGT_par <- function(path_dtm, shp, station, path_BestPar, ar50f, ar50w){
  #select station
  shp <- shp[shp$STASJON_NR == station,]
  # Reproject the shapefile to the ETRS89 / UTM zone 33N CRS
  new_crs <- CRS("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs")
  shp <- st_transform(shp, new_crs)

  ####Clip raster####
  # Create an empty list to store the raster objects
  cropped_list <- list()

  # Loop through each raster file in the catalog, read it, clip it, and add to the list
  # Create raster catalog of all DTMs in the directory
  raster_files <- list.files(path_dtm, pattern = ".tif$", full.name=TRUE)
  for (file in raster_files) {
    raster_obj <- raster(file)
    crop_raster <- crop(raster_obj, extent(shp))
    cropped_list <- append(cropped_list, list(crop_raster))
  }

  # Merge the list of cropped rasters into a single raster
  merged_raster <- do.call(merge, cropped_list)
  dtm <- mask(merged_raster, shp)


  ####Create square slope raster####
  # Calculate slope in radians
  slope_rad <- terrain(dtm, opt='slope', unit='radians')

  # Calculate square slope
  Sqs <- slope_rad ^2


  ####Create one polygon per elevation zone####
  # Create a list of breaks for each elevation zone
  station2 <- substr(station, 1, nchar(station)-2)
  best_par <- data.frame(read.csv(paste(path_BestPar,"\\Best_par_",station2,"_24h.csv", sep=""), sep=";"))
  breaks <- best_par[3:11,]
  breaks_list <- c(0, breaks[, 2], Inf)
  names(breaks_list) <- c(paste0("zone", seq_along(breaks_list) - 1))

  #create a polygon feature for each elevation zone
  zones <- list()
  for (i in 1:10) {
    upper_break <- breaks_list[i + 1]
    lower_break <- breaks_list[i]

    # Create a mask using the DTM raster for the current elevation zone
    mask <- dtm >= lower_break & dtm < upper_break
    #convert it to polygon. Export returns 2 polygons, one with the raster extent and another one with the elevation zone, select the latter
    zone_poly <- rasterToPolygons(mask, dissolve = TRUE)
    zone_poly <- zone_poly[zone_poly$layer == 1, ]

    # Assign an ID to each zone
    zone_poly$ID <- i

    zones[[i]] <- zone_poly
  }


  # Save the elevation zone polygons as a shapefile
  El_z <- do.call(rbind, zones)


  ####Delete water bodies from polygons####
  El_z2 <- st_difference(El_z, ar50w)


  ####Compute percentage of forest/wetland####
  El_z2 <- st_as_sf(El_z2)
  ar50f <- st_as_sf(ar50f)
  new_crs <- CRS("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs")
  ar50f <- st_transform(ar50f, new_crs)
  El_z2 <- st_transform(El_z2, new_crs)
  El_z2$area <- st_area(El_z2)
  El_z2 <- as.data.frame(El_z2)

  # Get the intersection between ar50f and El_z2
  intersection_sf <- st_intersection(El_z2, ar50f)
  intersection_sf <- as.data.frame(intersection_sf)

  # Calculate the area of each intersecting polygon
  intersection_sf$area2 <- st_area(intersection_sf)
  zones_sf$vegetation <- st_area() / st_area(zones_sf) * 100

  ####Compute standard deviation of square slope per elevation area####
  # Initialize an empty vector to store the results
  Std_SqS <- vector("numeric", length = nrow(El_z2))

  # Iterate through each polygon
  for (i in 1:nrow(El_z2)) {
    # Extract the standard deviation of the Sqs raster values within each elevation zone polygon
    values <- extract(Sqs, El_z2[El_z2$ID== i,], fun = sd, na.rm = TRUE)

    # Store the result
    Std_SqS[i] <- values
  }

  Std_SqS <- data.frame(Std_SqS)
  Std_SqS$ID <- 1:10

  ####Assign a0 and D. Join all results in a table####
  #Make a table with the results
  result_df <- left_join(El_z2, Std_SqS, by = "ID")
  result_df <- left_join(result_df, intersection_sf, by = "ID")

  result_df[is.na(result_df)] <- 0
  result_df <- result_df[, c("ID", "Std_SqS", "vegetation")]

  #Assign a0 and D values
  result_df <- result_df %>%
    mutate(
      vegetation = gsub(" \\[1\\]$", "", vegetation),
      a0 = ifelse(vegetation >= 20, 94.585,
                  ifelse(Std_SqS >= 0.1, 10.22,
                         ifelse(Std_SqS >= 0.05 & Std_SqS < 0.1, 13.995, 37.135))),
      D = ifelse(vegetation >= 20, 1000000,
                 ifelse(Std_SqS >= 0.1, 6533,
                        ifelse(Std_SqS >= 0.05 & Std_SqS < 0.1, 8100.5, 1000000)))
    )
  #####
  return(result_df)


}


