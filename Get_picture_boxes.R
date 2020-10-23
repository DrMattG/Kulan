# Here the camera details:
#
#   Camera: SONY DCS-RX1RM2
#
# Sensor: 35.9 x 24.0 mm
#
# VIEWING ANGLE LENS (CORRESPONDING 35 MM FORMAT) 63 degrees (35mm))
#
# Focal length: 35mm
#
# Flight height: 200-250m
#
# Image width in pixel: 7952
#
# Image height in pixel: 5304
# Mounted on drone at 30o angle

Flight1_RightCamera_eng$UTM_1<-rep(NA,1676)
Flight1_RightCamera_eng$UTM_2<-rep(NA,1676)
Flight1_RightCamera_eng$dutmx1<-rep(NA,1676)
Flight1_RightCamera_eng$dutmx2<-rep(NA,1676)
Flight1_RightCamera_eng$dutmx3<-rep(NA,1676)
Flight1_RightCamera_eng$dutmx4<-rep(NA,1676)
Flight1_RightCamera_eng$dutmy1<-rep(NA,1676)
Flight1_RightCamera_eng$dutmy2<-rep(NA,1676)
Flight1_RightCamera_eng$dutmy3<-rep(NA,1676)
Flight1_RightCamera_eng$dutmy4<-rep(NA,1676)
Flight1_RightCamera_eng$grd_in_img<-rep(NA,1676)
Flight1_RightCamera_eng$horizontal_grd<-rep(NA,1676)
Flight1_RightCamera_eng$vertical_grd<-rep(NA,1676)


for (i in 1:1676){
  xsensor=35.9
  ysensor=24
  focallen=35
  altitude=Flight1_RightCamera_eng$`Alt above ground`[i]
  pitch=Flight1_RightCamera_eng$Tangage[i]
  roll=Flight1_RightCamera_eng$`Banking angle`[i]


  xview = 2*REdaS::rad2deg(atan(xsensor/(2*focallen)))
  yview = 2*REdaS::rad2deg(atan(ysensor/(2*focallen)))

  #REdaS::deg2rad(63)
Flight1_RightCamera_eng$grd_in_img[i]=(tan(REdaS::deg2rad(30)/2)*altitude)*2
Flight1_RightCamera_eng$horizontal_grd[i]=(tan(REdaS::deg2rad(xsensor)/2)*altitude)*2
Flight1_RightCamera_eng$vertical_grd[i]=(tan(REdaS::deg2rad(ysensor)/2)*altitude)*2
dx1=(tan((horizontal_grd/2)+REdaS::deg2rad(roll))*altitude)
dy1=(tan((vertical_grd/2)+REdaS::deg2rad(pitch))*altitude)
dx2=(tan((horizontal_grd/2)+REdaS::deg2rad(roll))*altitude)
dy2=(tan((-vertical_grd/2)+REdaS::deg2rad(pitch))*altitude)
dx3=(tan((-horizontal_grd/2)+REdaS::deg2rad(roll))*altitude)
dy3=(tan((vertical_grd/2)+REdaS::deg2rad(pitch))*altitude)
dx4=(tan((-horizontal_grd/2)+REdaS::deg2rad(roll))*altitude)
dy4=(tan((-vertical_grd/2)+REdaS::deg2rad(pitch))*altitude)

library(sf)
library(tidyverse)
test = data.frame(lon =Flight1_RightCamera_eng$Lon_dec[i],
                  lat =Flight1_RightCamera_eng$Lat_dec[i] ) %>%
  st_as_sf(coords = c("lon", "lat")) %>%
  st_set_crs(., 4326)


  # lonlat2UTM = function(lonlat) {
  #   utm = (floor((lonlat[1] + 180) / 6) %% 60) + 1
  #   if(lonlat[2] > 0) {
  #     utm + 32600
  #   } else{
  #     utm + 32700
  #   }
  # }
  #
  #
  # lonlat2UTM(c(61.21,45.75))


#st_crs(32641)$proj4string
test2<-st_transform(test, crs = "+proj=utm +zone=41 +datum=WGS84 +units=m +no_defs")
Flight1_RightCamera_eng$UTM_1[i]<-st_coordinates(test2)[1]
Flight1_RightCamera_eng$UTM_2[i]<-st_coordinates(test2)[2]
Flight1_RightCamera_eng$dutmx1[i] =dx1 - dy1
Flight1_RightCamera_eng$dutmy1[i]= -dx1-dy1
Flight1_RightCamera_eng$dutmx2[i] =dx2 - dy2
Flight1_RightCamera_eng$dutmy2[i]= -dx2-dy2
Flight1_RightCamera_eng$dutmx3[i] =dx3 - dy3
Flight1_RightCamera_eng$dutmy3[i]= -dx3-dy3
Flight1_RightCamera_eng$dutmx4[i] =dx4 - dy4
Flight1_RightCamera_eng$dutmy4[i]= -dx4-dy4


}

x=c(st_coordinates(test2)[1]+dutmx1,st_coordinates(test2)[1]+dutmx2,st_coordinates(test2)[1]+dutmx3,st_coordinates(test2)[1]+dutmx4)
y=c(st_coordinates(test2)[2]+dutmy1,st_coordinates(test2)[2]+dutmy2,st_coordinates(test2)[2]+dutmy3,st_coordinates(test2)[2]+dutmy4)
pts1<-cbind(x,y)
#pts2<-cbind(x,y)

drone1<-cbind(st_coordinates(test2)[1],st_coordinates(test2)[2])
# drone2<-cbind(st_coordinates(test2)[1],st_coordinates(test2)[2])
plot(pts1)
points(pts2)
points(drone1)
points(drone3)

polygon(pts1[order(atan2(x-mean(x),y-mean(y))),])
polygon(pts2[order(atan2(x-mean(x),y-mean(y))),])






# bottom=altitude*tan(REdaS::deg2rad(xgimbal-.5*xview))
# top=altitude*tan(REdaS::deg2rad(xgimbal+.5*xview))
# left=altitude*tan(REdaS::deg2rad(ygimbal-.5*yview))
# right=altitude*tan(REdaS::deg2rad(ygimbal+.5*yview))
# footprint_height=right-left
# footprint_width=top-bottom
