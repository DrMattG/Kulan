#' Calculate the ground surface distance
#' @param altitude the height of the drone above the ground
#' @param Sensor_Height Height of sensor
#' @param Sensor_Width Width of sensor
#' @param Image_Height Height of image
#' @param Image_Width Width of image
#' @param Focal_Length Focal length
#' @return Ground surface distance
#' @export
#'
get_GSD<-function(Sensor_Height=24,Sensor_Width=35,Image_Width=7952,Image_Height=5304,Focal_Length=35,Altitude){
  GSDh=((Altitude*100)*(Sensor_Height/10))/((Focal_Length/10)*Image_Height)
  GSDh
  GSDw=((Altitude*100)*(Sensor_Width/10))/((Focal_Length/10)*Image_Width)
  GSDw
  GSD=max(GSDh,GSDw)
  return(GSD)}


#' Calculate the ground surface distance with angle distortion
#' @param altitude the height of the drone above the ground
#' @param Sensor_Height Height of sensor
#' @param Sensor_Width Width of sensor
#' @param Image_Height Height of image
#' @param Image_Width Width of image
#' @param Focal_Length Focal length
#' @param Angle Angle of the camera
#' @return Ground surface distance
#' @export
#'
get_GSD_dist<-function(Sensor_Height=24,Sensor_Width=35,Image_Width=7952,Image_Height=5304,Focal_Length=35,Altitude, Angle){
  GSDh=((Altitude*100)*(Sensor_Height/10))/((Focal_Length/10)*Image_Height)
  GSDh
  GSDw=((Altitude*100)*(Sensor_Width/10))/((Focal_Length/10)*Image_Width)
  GSDw
  GSD=max(GSDh,GSDw)
  GSDc<-GSD*(cos(REdaS::deg2rad(25*Angle))^-1)
  df=data.frame("GSDc"=GSDc, GSD=max(GSDh,GSDw),"GSDh"= GSDh, "GSDw"=GSDw)
  return(df)}

#get_GSD_dist(Altitude = 229, Angle=-0.5)
