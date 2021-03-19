#' Calculate forward overlap
#' @param H. = Drone altitude
#' @param dforward. = distance between points where image was taken
#' @param f.= focal length (default = 35mm)
#' @param w.= sensor width (default = 24mm)
#' @references \url{https://doi.org/10.3390/rs11101252}
#' @export


#Camera: SONY DCS-RX1RM2
#Sensor: 35.9 x 24.0 mm
#VIEWING ANGLE LENS (CORRESPONDING 35 MM FORMAT) 63 degrees (35mm))
#Focal length: 35mm
#Flight height: 200-250m
#Image width in pixel: 7952
#Image height in pixel: 5304


#oforward=(1-((dforward*f)/(H*w)))*100

oforward=function(dforward, f= 35, H=H, w=35.9){
  oforward=(1-((dforward*f)/(H*w)))*100
}

