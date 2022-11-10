
library(raster)
library(rgdal)
library(UScensus2010)
library(rgeos)
library(sp)
library(maptools)
library(sf)

# avec une zone editee sous qgis

zone <- readOGR("/home/thesardfou/Documents/projets/ADMIRE/ADMIR/sig/zone_points.shp")
zone <- spTransform(zone, CRS("+proj=utm +zone=58 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# zone <- readOGR("/home/thesardfou/Documents/projets/Reliques/postdoc_kuebini/GIS/sample_points/mini_zone_test.shp")

plot(zone)
#patchs <- intersect(zone[k,], fba_kone_tiwaka)

# à découper sous Qgis avec gdal : couche foret par zone
pinus <- readOGR("/home/thesardfou/Documents/projets/ADMIRE/ADMIR/01_DONNEES/Parcelle_v9_peupl1.shp")
plot(pinus)
# crs to utm
pinus <- spTransform(pinus, CRS("+proj=utm +zone=58 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
patchs = pinus
patchs <- disaggregate(patchs)
patchs_area <- area(patchs)

# patch aire minimum (eviter les polygons trop petits qui sont des erreurs)
#forest_zone_patchs <- patchs[patchs_area>1000,]
#plot(forest_zone_patchs)

# get forest for zone to sample
pinus_zone <- gIntersection(pinus, zone)
plot(pinus_zone)

# get DEM raster
dem <- raster("/home/thesardfou/Documents/projets/ADMIRE/ADMIR/sig/mnt_zone.tif")
plot(dem)
dem_forest_zone <- crop(dem, zone)
plot(dem_forest_zone)

# get slope raster
slope <- raster("/home/thesardfou/Documents/projets/ADMIRE/ADMIR/sig/slope_zone.tif")
# keep only weak slope
# slope_zone_limit <- slope
# slope_zone_limit[slope>30] <- 0
# slope_zone_limit[slope<30] <- 1
# writeRaster(slope_zone_limit,'/home/thesardfou/Documents/projets/ADMIRE/ADMIR/sig/slope_limit.tif')
slope_limit <- raster("/home/thesardfou/Documents/projets/ADMIRE/ADMIR/sig/slope_limit.tif")
# crop with zone
slope_limit_zone <- crop(slope_limit, zone)
plot(slope_limit_zone)

# transform weak slope areas to polygons
slope_limit_zone_poly <- rasterToPolygons(slope_limit_zone, dissolve = T)

##################################################################
##### sample points from various distance to edge ######
##################################################################
# same CSR
pinus_zone <- spTransform(pinus_zone, CRS("+proj=utm +zone=58 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
slope_limit_zone_poly <- spTransform(slope_limit_zone_poly, CRS("+proj=utm +zone=58 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#### sample points ####
# first nb sampling points on each buffer 
npts_on_lines <- 50
# final nb points on each buffer
n_pts_buffer <- 10
distances_buffer <- c(-40, -20, -10, 10, 20, 40)
bind_coord <- data.frame()
bind_pts_edge_dist <- c()
for(i in 1:length(distances_buffer)){
  dist_tmp <- distances_buffer[i]
  pinus_zone_buf_tmp <- gBuffer(pinus_zone, width = dist_tmp)
  pinus_zone_buf_tmp <- as(pinus_zone_buf_tmp, "SpatialLines")
  points_spl_buf_tmp <- spsample(pinus_zone_buf_tmp, n = npts_on_lines, type = "regular")
  # no points on strong slopes 
  points_spl_buf_tmp <- gIntersection(points_spl_buf_tmp, slope_limit_zone_poly)
  plot(slope_zone)
  plot(pinus_zone, add = T)
  plot(points_spl_buf_tmp, add = T)
  # no point at less than 25 m from last points
  if(i>1){
    buf_around_last_pts <- gBuffer(points_spl_buf_ok_tmp, byid=T, width = 25)
    points_spl_buf_tmp <- gDifference(points_spl_buf_tmp, buf_around_last_pts)
  }
  # get points
  print(length(points_spl_buf_tmp))
  points_spl_buf_ok_tmp <- points_spl_buf_tmp
  # sample n points for each buffer
  points_spl_buf_ok_tmp <- points_spl_buf_ok_tmp[sample(length(points_spl_buf_ok_tmp), n_pts_buffer)]
  bind_coord <- rbind(bind_coord, points_spl_buf_ok_tmp@coords)
  bind_pts_edge_dist <- c(bind_pts_edge_dist, rep(dist_tmp, nrow(points_spl_buf_ok_tmp@coords)))
}

data_pts <- cbind(bind_coord, dist_edge = bind_pts_edge_dist)

sample_points <- SpatialPointsDataFrame(coords=bind_coord,data=data_pts,
                                                proj4string =CRS("+proj=utm +zone=58 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")) 


sample_points_all <- sample_points

sample_points_all$ID <- paste(1:nrow(sample_points_all),"(", sample_points_all$dist_edge,")")

sample_points_all <- spTransform(sample_points_all, CRS("+proj=longlat + ellps=WGS84"))

sample_points_all@data$name <-  paste(1:nrow(sample_points_all),"(", sample_points_all$dist_edge,")")

# export for GPS
df_gpx <- data.frame(ID = paste(1:nrow(sample_points), sample_points$dist_edge, sep = "-"),
                     Long = sample_points$x,
                     Lat = sample_points$y)

writeOGR(obj=sample_points_all,
         dsn=   paste("/home/thesardfou/Documents/projets/Reliques/postdoc_kuebini/GIS/sample_points/sample_points_all_bis.gpx"),
         layer="waypoints",
         dataset_options="GPX_USE_EXTENSIONS=yes",  driver="GPX", overwrite_layer = T)

