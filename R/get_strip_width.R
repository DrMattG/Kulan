#' Calculate the strip width (two cameras on the drone)
#' @param xsensor horizontal sensor in mm
#' @param ysensor vertical sensor in mm
#' @param focallen focal length of the camera
#' @param ygim angle of the camera on the y axis
#' @param xgim angle of the camera on the x axis
#' @param alt the altitude of the drone
#' @return value of the horizontal strip-width (one side)
#' @export
get_strip_width=function(xsensor=35.9,ysensor=24,
                         focallen=35, ygim=0,xgim=25,alt, banking_angle){
    FOV.wide<-Kulan::rad_to_deg(2*atan(xsensor/(2*focallen)))
    FOV.tall<-Kulan::rad_to_deg(2*atan(ysensor/(2*focallen)))


    Drone2bottom<-alt*tan(Kulan::deg_to_rad((banking_angle+xgim)-0.5*FOV.wide))
    Drone2top<-alt*tan(Kulan::deg_to_rad((banking_angle+xgim)+0.5*FOV.wide))
    Drone2left<-alt*tan(Kulan::deg_to_rad(ygim-0.5*FOV.tall))
    Drone2right<-alt*tan(Kulan::deg_to_rad(ygim+0.5*FOV.tall))

    #Height<-Drone2right-Drone2left
    Width<-Drone2top-Drone2bottom
    return(Width)

  }


