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
# library(readr)
# Flight1_RightCamera_eng <- read_delim("Flight1_RightCamera_eng.csv",
#                                              ";", escape_double = FALSE, trim_ws = TRUE)
# Flight2_RightCamera_eng <- read_csv("Flight2_RightCamera_eng.csv")
# Flight3_RightCamera_eng <- read_csv("Flight3_RightCamera_eng.csv")
# Flight4_RightCamera_eng <- read_csv("Flight4_RightCamera_eng.csv")
# Flight5_RightCamera_eng <- read_csv("Flight5_RightCamera_eng.csv")
# Flight6_RightCamera_eng <- read_csv("Flight6_RightCamera_eng.csv")
# Flight7_RightCamera_eng <- read_csv("Flight7_RightCamera_eng.csv")
# Flight8_RightCamera_eng <- read_csv("Flight8_RightCamera_eng.csv")
#
# All_Flight_Right<-bind_rows(Flight1_RightCamera_eng,Flight2_RightCamera_eng,
#                             Flight3_RightCamera_eng,Flight4_RightCamera_eng,
#                             Flight5_RightCamera_eng, Flight6_RightCamera_eng,
#                             Flight7_RightCamera_eng, Flight8_RightCamera_eng)
#
# All_Flight_Right$X18<-NULL
#
#
# saveRDS(All_Flight_Right, "All_Flight_Right.RDS")


350/All_Flight_Right$`Alt above ground`[10]

All_Flight_Right<- readRDS("C:/Users/matthew.grainger/Documents/Projects_in_development/Kulan/data/All_Flight_Right.RDS")

All_Flight_Right$UTM_1<-rep(NA,12670)
All_Flight_Right$UTM_2<-rep(NA,12670)
All_Flight_Right$dutmx1<-rep(NA,12670)
All_Flight_Right$dutmx2<-rep(NA,12670)
All_Flight_Right$dutmx3<-rep(NA,12670)
All_Flight_Right$dutmx4<-rep(NA,12670)
All_Flight_Right$dutmy1<-rep(NA,12670)
All_Flight_Right$dutmy2<-rep(NA,12670)
All_Flight_Right$dutmy3<-rep(NA,12670)
All_Flight_Right$dutmy4<-rep(NA,12670)
All_Flight_Right$horizontal_grd<-rep(NA,12670)
All_Flight_Right$vertical_grd<-rep(NA,12670)
All_Flight_Right$bbx1<-rep(NA,12670)
All_Flight_Right$bbx2=All_Flight_Right$bbx3=
  All_Flight_Right$bbx4=All_Flight_Right$bby1=
  All_Flight_Right$bby2=All_Flight_Right$bby3=
  All_Flight_Right$bby4= rep(NA,12670)

altitude<-pitch<-roll<-NA

for (i in 1:12670){

  xsensor=35.9
  ysensor=24
  focallen=63
  altitude[i]=All_Flight_Right$`Alt above ground`[i]
  pitch[i]=All_Flight_Right$Tangage[i]
  roll[i]=All_Flight_Right$`Banking angle`[i]


  xview = 2*REdaS::rad2deg(atan(xsensor/(2*focallen)))
  yview = 2*REdaS::rad2deg(atan(ysensor/(2*focallen)))

  #REdaS::deg2rad(63)

All_Flight_Right$horizontal_grd[i]=(tan(REdaS::deg2rad(xsensor)/2)*altitude[i])*2
All_Flight_Right$vertical_grd[i]=(tan(REdaS::deg2rad(ysensor)/2)*altitude[i])*2
dx1=(tan((All_Flight_Right$horizontal_grd[i]/2)+REdaS::deg2rad(roll[i]))*altitude[i])
dy1=(tan((All_Flight_Right$vertical_grd[i]/2)+REdaS::deg2rad(pitch[i]))*altitude[i])
dx2=(tan((All_Flight_Right$horizontal_grd[i]/2)+REdaS::deg2rad(roll[i]))*altitude[i])
dy2=(tan((-All_Flight_Right$vertical_grd[i]/2)+REdaS::deg2rad(pitch[i]))*altitude[i])
dx3=(tan((-All_Flight_Right$horizontal_grd[i]/2)+REdaS::deg2rad(roll[i]))*altitude[i])
dy3=(tan((All_Flight_Right$vertical_grd[i]/2)+REdaS::deg2rad(pitch[i]))*altitude[i])
dx4=(tan((-All_Flight_Right$horizontal_grd[i]/2)+REdaS::deg2rad(roll[i]))*altitude[i])
dy4=(tan((-All_Flight_Right$vertical_grd[i]/2)+REdaS::deg2rad(pitch[i]))*altitude[i])

library(sf)
library(tidyverse)
test = data.frame(lon =All_Flight_Right$Lon_dec[i],
                  lat =All_Flight_Right$Lat_dec[i] ) %>%
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
All_Flight_Right$UTM_1[i]<-st_coordinates(test2)[1]
All_Flight_Right$UTM_2[i]<-st_coordinates(test2)[2]
All_Flight_Right$dutmx1[i] =dx1 - dy1
All_Flight_Right$dutmy1[i]= -dx1-dy1
All_Flight_Right$dutmx2[i] =dx2 - dy2
All_Flight_Right$dutmy2[i]= -dx2-dy2
All_Flight_Right$dutmx3[i] =dx3 - dy3
All_Flight_Right$dutmy3[i]= -dx3-dy3
All_Flight_Right$dutmx4[i] =dx4 - dy4
All_Flight_Right$dutmy4[i]= -dx4-dy4

All_Flight_Right$bbx1[i]<-st_coordinates(test2)[1]+All_Flight_Right$dutmx1[i]
All_Flight_Right$bby1[i]<-st_coordinates(test2)[2]+All_Flight_Right$dutmy1[i]
All_Flight_Right$bbx2[i]<-st_coordinates(test2)[1]+All_Flight_Right$dutmx2[i]
All_Flight_Right$bby2[i]<-st_coordinates(test2)[2]+All_Flight_Right$dutmy2[i]
All_Flight_Right$bbx3[i]<-st_coordinates(test2)[1]+All_Flight_Right$dutmx3[i]
All_Flight_Right$bby3[i]<-st_coordinates(test2)[2]+All_Flight_Right$dutmy3[i]
All_Flight_Right$bbx4[i]<-st_coordinates(test2)[1]+All_Flight_Right$dutmx4[i]
All_Flight_Right$bby4[i]<-st_coordinates(test2)[2]+All_Flight_Right$dutmy4[i]
}





df<-data.frame(bbx1=All_Flight_Right$bbx1[15],
          bbx2=All_Flight_Right$bbx2[15],
          bby1=All_Flight_Right$bby1[15],
          bby3=All_Flight_Right$bby2[15])

df

# x=c(st_coordinates(test2)[1]+dutmx1,st_coordinates(test2)[1]+dutmx2,st_coordinates(test2)[1]+dutmx3,st_coordinates(test2)[1]+dutmx4)
# y=c(st_coordinates(test2)[2]+dutmy1,st_coordinates(test2)[2]+dutmy2,st_coordinates(test2)[2]+dutmy3,st_coordinates(test2)[2]+dutmy4)
# pts1<-cbind(x,y)
# #pts2<-cbind(x,y)
#
# drone1<-cbind(st_coordinates(test2)[1],st_coordinates(test2)[2])
# # drone2<-cbind(st_coordinates(test2)[1],st_coordinates(test2)[2])
# plot(pts1)
# points(pts2)
# points(drone1)
# points(drone3)
#
# polygon(pts1[order(atan2(x-mean(x),y-mean(y))),])
# polygon(pts2[order(atan2(x-mean(x),y-mean(y))),])
#





# bottom=altitude*tan(REdaS::deg2rad(xgimbal-.5*xview))
# top=altitude*tan(REdaS::deg2rad(xgimbal+.5*xview))
# left=altitude*tan(REdaS::deg2rad(ygimbal-.5*yview))
# right=altitude*tan(REdaS::deg2rad(ygimbal+.5*yview))
# footprint_height=right-left
# footprint_width=top-bottom

# dt<-data.frame(altitude=All_Flight_Right$`Alt above ground`,
#                horizontal_grd=All_Flight_Right$horizontal_grd)
#
# dt<-tibble(dt)
# lm_eq <- function(df){
#
#   m <- lm(horizontal_grd ~ altitude, df);
#
#   eq <- substitute((y) == a + b %.% (x)*","~~(r)^2~"="~r2,
#
#                    list(a = format(unname(coef(m)[1]), digits = 2),
#
#                         b = format(unname(coef(m)[2]), digits = 2),
#
#                         r2 = format(summary(m)$r.squared, digits = 3)))
#
#   as.character(as.expression(eq));
#
# }
#
# lm_eq(dt)
#
# dt %>%
#   ggplot(aes(altitude,horizontal_grd))+
#   geom_point(col="blue", size=4)+
#   geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
#   geom_text(x = 100, y = 10, label = lm_eq(dt), parse = TRUE)+
#   labs(x="Altitude", y="Horizontal_Rightside")+
#   theme_classic()
#
# # library(pracma)
# # polyarea(x=c(All_Flight_Right$bbx1[1],All_Flight_Right$bbx2[1],
# #              All_Flight_Right$bbx3[1],All_Flight_Right$bbx4[1]),
# #          y=c(All_Flight_Right$bby1[1],All_Flight_Right$bby2[1],
# #          All_Flight_Right$bby3[1],All_Flight_Right$bby4[1]))
#
#
# library(sf)
#
#
# x_coordinates_1<-c(All_Flight_Right$bbx1[10],All_Flight_Right$bbx2[10],
#                      All_Flight_Right$bbx4[10],All_Flight_Right$bbx3[10],
#                    All_Flight_Right$bbx1[10])
#
# y_coordinates_1<-c(All_Flight_Right$bby1[10],All_Flight_Right$bby2[10],
#                    All_Flight_Right$bby4[10],All_Flight_Right$bby3[10],
#                    All_Flight_Right$bby1[10])
#
# # Transform your list of points to a polygon, create a simple feature from them and set its crs according to its EPSG code (here I set it to UTM 15 N)
# polygon <- st_sfc(st_polygon(list(cbind(x_coordinates_1,y_coordinates_1)))) %>%
#   st_set_crs(32641)
#
# # Plot the polygon to check if geoemtry is the expected
# plot(polygon[order(atan2(x-mean(x),y-mean(y))),],axes = TRUE)
# points(x=All_Flight_Right$UTM_1[10], y=All_Flight_Right$UTM_2[10])
# # Calculate area of the polygon
# st_area(polygon)
#




altitude=234.2
angle=25

cos(REdaS::deg2rad(angle))

cos(angle)

altitude/0.9063078

All_Flight_Right$`Alt above ground`/cos(REdaS::deg2rad(angle))


grd_in_img<-function(altitude,angle=25,distance_between_cameras=0){
    grd<-altitude/cos(REdaS::deg2rad(angle))
    grd<-sum(grd+distance_between_cameras, na.rm=TRUE)
    return(grd)
    }

grd_in_img(234.2,25, 0.03)

xsensor=35.9
ysensor=24
focallen=63

horizontal_grd=(tan(REdaS::deg2rad(xsensor)/2)*350)*2
vertical_grd=(tan(REdaS::deg2rad(ysensor)/2)*350)*2

350/cos(REdaS::deg2rad(xsensor))*REdaS::deg2rad(25/2))
350/cos(REdaS::deg2rad(ysensor))*REdaS::deg2rad(25/2))
350/cos(REdaS::deg2rad(25))


148*2


grd_in_img(altitude = 350
           )
grd_in_img(altitude=650)

(horizontal_grd=(tan(REdaS::deg2rad(xsensor)/2)*650)*2
)
(vertical_grd=(tan(REdaS::deg2rad(ysensor)/2)*650)*2)

