library(elevatr)
library(sp)
library(raster)
prj_dd <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
dfsp <- sp::SpatialPoints(all_dfs[,c(4,5)], proj4string = sp::CRS(prj_dd)) #it needs to have Longitude first and Latitude second!!!!
df_spdf <- sp::SpatialPointsDataFrame(dfsp, proj4string = sp::CRS(prj_dd), data = all_dfs)
#Flight1_left <- sp::SpatialPoints(dfsp, proj4string = sp::CRS(prj_dd))

elevation <- get_elev_raster(df_spdf, z = 10)
plot(elevation)
elevation@data

cord.UTM <- spTransform(df_spdf, crs(elevation))
df_spdf$DEM_ex <-raster::extract(elevation, df_spdf@coords, sp = TRUE)
df_spdf %>%
  head() %>%
  DT::datatable(options=list(scrollX = TRUE))

plot(df_spdf$DEM_ex, df_spdf$alt_above_ground)
