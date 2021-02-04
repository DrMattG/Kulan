#' Get raster values from DEM
#' @param Flight_file The flight file as a dataframe
#' @param Image_Width Width of image
#' @param Focal_Length Focal length
#' @param Angle Angle of the camera
#' @return Ground surface distance
#' @export
#'

##################################################################################################

# Import flight file

#df <- read_excel("K:/E_GIS/Kazachstan/2020_Drohne survey Altyn Emel/2020_09_partial survey/AE_Flight_Sep2020_tables_with_DEM/Flight1_LeftCamera.xls", sheet = "Flight1_LeftCamera")
Flight1_LeftCamera <- readRDS("C:/Users/matthew.grainger/Documents/Projects_in_development/Kulan/data/Flight1_LeftCamera.rds")
df<-Flight1_LeftCamera

df <- as.data.frame(df)


prj_dd <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"



dfsp <- sp::SpatialPoints(df[,c(5,4)], proj4string = sp::CRS(prj_dd)) #it needs to have Longitude first and Latitude second!!!!



# Create SpatialPoints

Flight1_left <- sp::SpatialPoints(dfsp, proj4string = sp::CRS(prj_dd))



# Create an SpatialPointsDataFrame

df_spdf <- sp::SpatialPointsDataFrame(Flight1_left, proj4string = sp::CRS(prj_dd), data = df)



# import regional DEM30
library(raster)

DEM30_BK <- raster::raster(paste("data/DEM30_BK/Barsa-kelmes.tif",sep='/'))

raster::crs(DEM30_BK)

plot(DEM30_BK, col= terrain.colors(n=15, alpha=1, rev=FALSE))
points(df_spdf$Lon_dec,df_spdf$Lat_dec,cex=0.5,pch=15) #plot points based on long/lat

crs(df_spdf)

head(df_spdf, n=5)



# Extract raster values to points

# and add it to a.t.

cord.UTM <- spTransform(df_spdf, crs(DEM30_BK))



df_spdf$DEM_ex <- extract(DEM30_BK, Flight1_left@coords, sp = T)
head(df_spdf, n=5)

names(df_spdf)[names(df_spdf) == "Alt above the Sea level"] <- "ASL" #Rename Altitude above ground to ASL

df_spdf$AAG_Drone <- df_spdf$ASL - df_spdf$DEM_ex

df_new <- as.data.frame(df_spdf)



### Export data if needed

#write.csv(ds_new, "/XXX.csv")

################################################################################################
