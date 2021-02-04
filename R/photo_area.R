#' Calculate the area of a drone image
#' @param alt = Drone altitude
#' @param AOV = Angle of view
#' @export

# assume perpendicular to the ground
ground_in_image<-function(alt, AOV){
  gr_in_image<-(tan(AOV/2)*alt)*2
  return(gr_in_image)
}

ground_in_image(200,63/2*pi/180)


#15% overlap

#Camera: SONY DCS-RX1RM2
#Sensor: 35.9 x 24.0 mm
#VIEWING ANGLE LENS (CORRESPONDING 35 MM FORMAT) 63 degrees (35mm))
#Focal length: 35mm
#Flight height: 200-250m
#Image width in pixel: 7952
#Image height in pixel: 5304
#Mounted on drone at 30o angle

b=200 #height
C=63/2*pi/180  #angle
A=90*pi/180  #right angle
B=A-C



a=b*(sin(A)/sin(B))
c=b*(sin(C)/sin(B))
a
c*2
