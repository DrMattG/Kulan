#' Calculate the area of a drone image
#' @param altitude = Drone altitude
#' @param angle_of_camera The set angle of the camera
#' @return The area of the photograph in m2
#' @export
get_photo_area=function(altitude, angle_of_camera=25, banking_angle){
  h=altitude
  # angle of camera
  # 25 degrees
  theta <- Kulan::deg_to_rad(angle_of_camera)
  # horizontal field of view
  phi <- Kulan::deg_to_rad(Kulan::get_HFOV()+banking_angle)
  # vertical field of view
  omega <-Kulan::deg_to_rad(Kulan::get_VFOV())
  # pixels
  pixels <- c(7952, 5304)
  Dc= h*tan(theta-phi/2)
  Df= h*tan(theta+phi/2)
  Dm= h*tan(theta+phi*0.5/2)
  delta <- Df - Dc
  Wc=2*(Dc^2+h^2)^0.5*tan(omega/2)
  Wm=2*((Dc+Df/2)^2+h^2)^0.5*tan(omega/2)
  Wf=2*(Df^2+h^2)^0.5*tan(omega/2)
  area <-
    #A=(a+b/2)*h
    ((Wc+Wf)/2)*(Df-Dc)
  return(area)
}
