#' Calculate Jolly II estimate and 95% Confidence Interval
#' @param species_count the column from the dataset with the species counts in
#' @param Transect_area the column from the dataset with the transect areas in
#' @param Z the stratum area (the total area that we are extrapolating to)
#' @return dataframe with Jolly II estimate and 95% CI
#' @examples
#' data=Kulan::sim_kulan_data(n=100,ma=25, p0=0.5, ml=20, w=0.2)
#' jolly2(data$sp_count, data$Trans_area, Z=164*5)
#' @export

jolly2<-function(species_count, Transect_area, Z=Z){
  Z=Z
  y= species_count
  z= Transect_area
  R=sum(y)/sum(z)
  Y=Z*R
  n=dim(data)[1]
  N=n*Z/sum(z)
  S2y=stats::var(y)
  S2z=stats::var(z)
  Sxy=stats::cov(y,z)
  VarY=((N*(N-n))/n)*(S2y-2*R*Sxy+R^2*S2z)
  se<-sqrt(VarY)
  CL=se*stats::qt(0.95,n-1)
  out<-data.frame("Estimate"=Y, "lower_CI"=Y-CL, "upper_CI"=Y+CL)
  return(out)
}

