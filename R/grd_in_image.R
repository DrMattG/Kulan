#' Calculate the ground in the image (two cameras on the drone)
#' @param altitude the height of the drone above the ground
#' @param angle1 the angle of camera1
#' @param angle2 the angle of camera2
#' @param distance_between_cameras added value for the distance between cameras
#' @return distance along the ground
#' @export
#'
grd_in_img<-function(altitude,angle1=25,angle2=25.7, distance_between_cameras=0.03){
  grd1<-altitude/cos(REdaS::deg2rad(angle1))
  grd2<-altitude/cos(REdaS::deg2rad(angle2))
  grd<-sum(grd1+grd2+distance_between_cameras, na.rm=TRUE)
  return(grd)
}

#grd_in_img(234.2,25,25.7, 0.03)
# library(tidyverse)
# All_Flight_Right %>%
#   rowwise() %>%
#   mutate("grd_in_img"=grd_in_img(`Alt above ground`)) %>%
#   select(grd_in_img)
