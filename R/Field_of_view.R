#'Get Horizontal Field of view
#' @param HSensor Horizontal sensor in mm
#' @param Focallen Focal length of the camera
#' @export

# Camera: SONY DCS-RX1RM2
# Sensor: 35.9 x 24.0 mm
# Viewing angle: (CORRESPONDING 35 MM FORMAT) 63 degrees (35mm))
# Focal length: 35mm
# Flight height: 200-250m
# Pixels: 4.5 micrometers
# Image width in pixels: 7952
# Image height in pixels: 5304

get_HFOV=function(HSensor=35.9, Focallen=35){
  HFOV=2*atan(HSensor/(2*Focallen))
  (REdaS::rad2deg(HFOV))
  }
#'Get Vertical Field of view
#' @param VSensor Vertical sensor in mm
#' @param Focallen Focal length of the camera
#' @export

get_VFOV=function(VSensor=24, Focallen=35){
  VFOV=2*atan(VSensor/(2*Focallen))
  (REdaS::rad2deg(VFOV))
}

#'Convert degree to radians
#' @param deg degrees
#' @export
deg_to_rad <- function(deg) {
  deg * pi / 180
}

#'Convert radians to degrees
#' @param deg degrees
#' @export
rad_to_deg <- function(rad) {
  rad * 180/pi
  }
