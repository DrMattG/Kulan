# Spatial Resolution (Ground sampling Distance)

#' @title Ground sampling distance (Spatial Resolution)
#' @description Calculates the ground sampling distance (spatial resolution). This version of the function ignores angular distortion
#' @param H the height of the drone above the ground
#' @param pixel pixel size in micrometers
#' @param Focal_Length Focal length
#' @details This version of the function assumes no angular distortion. The spatial resolution is calculated using the scale (focal length of the camera / Flying height above ground level). This equates to the sensor pixel size/ Ground pixel size. To calculate the ground sample distance we need to know the flying height, the camera sensor size and the lens focal length.
#'
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
#' @return Ground surface distance
#' @export


GSD=function(f=35, H=Altitude, pixel=4.5){
  #convert everything to meters
  f=f/1000
  pixel=pixel/1e+06
  # calculate the GSD = Height*pixel/focal_length
  GSD=(H)*(pixel)/f
  return(GSD)
  }

#' @title Calculate the ground footprint
#' @param GSD Spatial resolution
#' @param Image_Height Height of image
#' @param Image_Width Width of image
#' @details This function calculates the ground footprint with no angular distortion. The width and the length of the sensor are separately multiplied by the ground sampling distance.
#' Then the resulting width and length are multiplied to return the area of the image
#' @return Ground footprint area (no angular distortion)
#' @export

Ground_footprint=function(GSD=GSD, Image_Height=7952, Image_Width=5304){
  W=Image_Width*GSD
  L=Image_Height*GSD
  Ground_footprint=W*L
}

