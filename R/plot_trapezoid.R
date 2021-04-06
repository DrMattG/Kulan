#' Plot the dimensions of the photograph
#' @param altitude = Drone altitude
#' @param angle_of_camera The set angle of the camera
#' @param banking_angle The banking angle of the drone
#' @return plot of the horizontal and vertical profile of the photograph and the
#' resulting trapezoid
#' @export

plot_trapezoid=function(altitude, angle_of_camera=25, banking_angle){
  h=altitude
    # angle of camera
  # 25 degrees
  theta <- Kulan::deg_to_rad(angle_of_camera)
  # horizontal field of view
  phi <- Kulan::deg_to_rad(Kulan::get_HFOV()+banking_angle)
  # vertical field of view
  omega <- Kulan::deg_to_rad(Kulan::get_VFOV())
Dc= h*tan(theta-phi/2)
Df= h*tan(theta+phi/2)
Dm= h*tan(theta+phi*0.5/2)
Rc=sqrt(h^2+Dc^2)
Rm=sqrt(h^2+Dm^2)
Rf=sqrt(h^2+Df^2)
dt.triangle <-data.table::data.table(group = c(1,1,1), polygon.x = c(0,Dc,0), polygon.y = c(h,0,0))
dt.triangle2 <-data.table::data.table(group = c(1,1,1), polygon.x = c(0,Df,0), polygon.y = c(h,0,0))
p <- ggplot()
p <- p + geom_polygon(
  data = dt.triangle
  ,aes(
    x=polygon.x
    ,y=polygon.y
    ,group=group
  )
)
p1=p+geom_polygon(
  data = dt.triangle2
  ,aes(
    x=polygon.x
    ,y=polygon.y
    ,group=group
    , alpha=0.2
  ))+
  labs(x="Horizontal distance", y="Vertical distance")+
  theme(legend.position = "None")
Wc=2*(Dc^2+h^2)^0.5*tan(omega/2)
Wm=2*((Dc+Df/2)^2+h^2)^0.5*tan(omega/2)
Wf=2*(Df^2+h^2)^0.5*tan(omega/2)
positions <- data.frame(
 x = c(0, 0, Df-Dc, Df-Dc),
  y = c(-Wc/2, Wc/2, -Wf/2, Wf/2)
)
p2=ggplot(positions[c(1,2,4,3),], aes(x = x, y = y)) +
  geom_polygon(aes(fill = "red"))+
  labs(x="", y="")+
  theme_classic()+
  theme(legend.position = "None")

library(patchwork)
p1+p2
}






