#' Calculate the area of a drone image
#' @param alt = Drone altitude
#' @param AOV = Angle of view
#' @export

# assume perpendicular to the ground
photo_area<-function(alt, AOV){
  gr_in_image<-(tan(AOV/2)*alt)*2
  return(gr_in_image)
}


#' Calculate the length of the sides of image
#' @param alt = Drone altitude
#' @export

# Assume drone looking directly down
# Specific to this drone
# 15% overlap?
#Camera: SONY DCS-RX1RM2
#Sensor: 35.9 x 24.0 mm
#VIEWING ANGLE LENS (CORRESPONDING 35 MM FORMAT) 63 degrees (35mm))
#Focal length: 35mm
#Flight height: 200-250m
#Image width in pixel: 7952
#Image height in pixel: 5304


Image_sides=function(Altitude=Altitude){
  b=Altitude #height
  C=63/2*pi/180  #angle
  A=90*pi/180  #right angle
  B=A-C
  a=b*(sin(A)/sin(B))
  c=b*(sin(C)/sin(B))
  side1=a
  side2=c*2
  df=data.frame(side1,side2)
  return(df)

}


