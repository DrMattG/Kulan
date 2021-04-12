#' Calculate the side overlap
#' @param altitude = Drone altitude
#' @param angle_of_camera1 The set angle of the camera on the left
#' #' @param angle_of_camera2 The set angle of the camera on the right
#' @param banking_angle The banking angle of the drone
#' @return side overlap in meters
#' @examples
#' side_overlap(altitude = 200, banking_angle = 0)
#' @export
side_overlap=function(altitude, angle_of_camera1=25, angle_of_camera2=25.7, banking_angle){
  h=altitude
  theta <- Kulan::deg_to_rad(angle_of_camera1+banking_angle)
  # horizontal field of view
  phi <- Kulan::deg_to_rad(Kulan::get_HFOV()+banking_angle)
  Dc= h*tan(theta-phi/2)

  theta1 <- Kulan::deg_to_rad(angle_of_camera2+banking_angle)
  # horizontal field of view
  phi1 <- Kulan::deg_to_rad(Kulan::get_HFOV()+banking_angle)
  Dc1= h*tan(theta-phi/2)

  max(abs(Dc),abs(Dc1))

}




