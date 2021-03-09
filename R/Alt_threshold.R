#' Apply a threshold to the lower locations that occur at takeoff and landing
#' @param df The dataframe
#' @param Alt Altitude column in the dataframe
#' @param height The threshold height (remove values lower than the height)
#' @export

Alt_threshold=function(df=df,Alt=Alt, height=100){
  df=as.data.frame(df)
  sub_df=df[Alt>as.numeric(height),]
  return(sub_df)
  }

#Alt_threshold(df=All_Flight_Left, Alt=All_Flight_Left$alt_above_ground,height=100)

# function plot first and then if needed use quantiles to discard



# All_Flight_Left %>%
#   ggplot(aes(Time,`Alt above ground`))+
#   geom_point()
