# Spatial Resolution (Ground sampling Distance)
#' @title Ground sampling distance (Spatial Resolution)
#' @description Calculates the ground sampling distance (spatial resolution). This version of the function ignores angular distortion
#' @param altitude the height of the drone above the ground
#' @param angle_of_camera the angle of the camera is set
#' @param pixel pixel size in micrometers
#' @param focallen Focal length
#' @details
#' Camera: SONY DCS-RX1RM2
#'
#' Sensor: 35.9 x 24.0 mm
#'
#' Viewing angle: (CORRESPONDING 35 MM FORMAT) 63 degrees (35mm))
#'
#' Focal length: 35mm
#'
#' Flight height: 200-250m
#'
#' Pixels: 4.5 micrometers
#'
#' Image width in pixels: 7952
#'
#' Image height in pixels: 5304
#'
#' @return Ground surface distance m per pixel
#' @export

GSD=function(altitude, angle_of_camera=25, pixel=4.5, focallen=35){
  h=altitude
  #convert to meters
  focallen=focallen/1000
  pixel=pixel/1e+06


  # angle of camera
  # 25 degrees
  theta <- Kulan::deg_to_rad(angle_of_camera)
  # horizontal field of view
  phi <- Kulan::deg_to_rad(Kulan::get_HFOV())
  # vertical field of view
  omega <-Kulan::deg_to_rad(Kulan::get_VFOV())
  Dc= h*tan(theta-phi/2)
  Df= h*tan(theta+phi/2)
  Dm= h*tan(theta+phi*0.5/2)
  Rc=sqrt(h^2+Dc^2)
  Rm=sqrt(h^2+Dm^2)
  Rf=sqrt(h^2+Df^2)


  #GSDH = FLIGHT HEIGHT*Sensor Height/Focal length*Image height
  #GSDW = FLIGHT HEIGHT*Sensor Width/Focal length*Image height
  near=(Rc)*(pixel)/focallen
  mid=(Rm)*(pixel)/focallen
  far=(Rf)*(pixel)/focallen
  list("near"=round(near,3),
       "mid"=round(mid,3),
       "far"=round(far,3))
}


