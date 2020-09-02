#' Simulate a dataset
#' @param n number of transects
#' @param ma mean number of animals counted where they are counted
#' @param p0 proportion of the transects that will be empty
#' @param ml mean length of the transects
#' @param w width of the transects (assuming a constant width)
#' @return simulated data in a dataframe
#' @examples
#' sim_kulan_data(n=100,ma=25, p0=0.5, ml=20, w=0.2)
#' @export

sim_kulan_data=function(n=100,ma=25,p0=0.5,ml=20,w=0.20){
  sp_count=VGAM::rzipois(n,ma,p0)
  Trans_length=stats::rpois(n,ml)
  Trans_area=Trans_length*w
  sp_area=sp_count/Trans_area

  out<-data.frame(sp_count=sp_count, Trans_length=Trans_length,
                  Trans_area= Trans_area, sp_area=sp_area)
  return(out)
}


