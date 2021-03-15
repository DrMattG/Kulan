#' Calculate the strip width (two cameras on the drone)
#' @param altitude the height of the drone above the ground
#' @param angle1 the angle of camera1
#' @param angle2 the angle of camera2
#' @param distance_between_cameras added value for the distance between cameras
#' @return distance along the ground
#' @export
#'
get_strip_width<-function(altitude,angle1=25,angle2=25.7, distance_between_cameras=0.03){
  grd1<-altitude/cos(REdaS::deg2rad(angle1))
  grd2<-altitude/cos(REdaS::deg2rad(angle2))
  grd<-sum(grd1+grd2+distance_between_cameras, na.rm=TRUE)
  return(grd)
}

