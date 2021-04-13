#' Calculate forward overlap
#' @param altitude1 Drone altitude at position1
#' @param altitude2 Drone altitude at position2
#' @param forward_distance distance between points where image was taken
#' @param angle_of_camera angle of the camera (25 or 25.7 degrees)
#' @param banking_angle1 The banking angle in the first position
#' @param banking_angle2 The banking angle in the second position
#' @export

forward_overlap=function(altitude1,altitude2,
                         forward_distance,
                         angle_of_camera=25,
                         banking_angle1,
                         banking_angle2){


#Picture1
  h1=altitude1
  # angle of camera1
  theta1 <- Kulan::deg_to_rad(angle_of_camera)
  # horizontal field of view
  phi1 <- Kulan::deg_to_rad(Kulan::get_HFOV()+banking_angle1)
  # vertical field of view
  omega1 <- Kulan::deg_to_rad(Kulan::get_VFOV())
  Dc1= h1*tan(theta1-phi1/2)
  Df1= h1*tan(theta1+phi1/2)
  Dm1= h1*tan(theta1+phi1*0.5/2)
  Rc1=sqrt(h1^2+Dc1^2)
  Rm1=sqrt(h1^2+Dm1^2)
  Rf1=sqrt(h1^2+Df1^2)

  #Picture2
  h2=altitude2
  # angle of camera2
  theta2 <- Kulan::deg_to_rad(angle_of_camera)
  # horizontal field of view
  phi2 <- Kulan::deg_to_rad(Kulan::get_HFOV()+banking_angle2)
  # vertical field of view
  omega2 <- Kulan::deg_to_rad(Kulan::get_VFOV())
  Dc2= h2*tan(theta2-phi2/2)
  Df2= h2*tan(theta2+phi2/2)
  Dm2= h2*tan(theta2+phi2*0.5/2)
  Rc2=sqrt(h2^2+Dc2^2)
  Rm2=sqrt(h2^2+Dm2^2)
  Rf2=sqrt(h2^2+Df2^2)

  #Work out overlap between trapezoids
  Wc1=2*(Dc1^2+h1^2)^0.5*tan(omega1/2)
  Wm1=2*((Dc1+Df1/2)^2+h1^2)^0.5*tan(omega1/2)
  Wf1=2*(Df1^2+h1^2)^0.5*tan(omega1/2)
  positions1 <- data.frame(
    x = c(0, 0, Df1-Dc1, Df1-Dc1),
    y = c(-Wc1/2, Wc1/2, -Wf1/2, Wf1/2)
  )

  Wc2=2*(Dc2^2+h2^2)^0.5*tan(omega2/2)
  Wm2=2*((Dc2+Df2/2)^2+h2^2)^0.5*tan(omega2/2)
  Wf2=2*(Df2^2+h2^2)^0.5*tan(omega2/2)
  positions2 <- data.frame(
    x = c(0, 0, Df2-Dc2, Df2-Dc2),
    y = c(-Wc2/2+forward_distance, Wc2/2+forward_distance,
          -Wf2/2+forward_distance, Wf2/2+forward_distance)
  )

# positions=rbind(positions1,positions2)
# positions$group=c(rep("positions1",4), rep("positions2",4))
#
# positions[c(1,2,4,3,5,6,8,7),] %>%
#   ggplot(aes(x=x,y=y)) +
#   geom_polygon(aes(fill = group, alpha=0.4))+
#   labs(x="", y="")+
#   theme_classic()+
#   theme(legend.position = "None")


library(sp)
p = Polygon(positions1[c(1,2,4,3),] )
ps = Polygons(list(p),1)
sps = SpatialPolygons(list(ps))
p2 = Polygon(positions2[c(1,2,4,3),] )
ps2 = Polygons(list(p2),1)
sps2 = SpatialPolygons(list(ps2))
#plot(sps)
#plot(rgeos::gIntersection(sps,sps2), add=TRUE)
area=rgeos::gIntersection(sps,sps2)
area@polygons[[1]]@area
}
