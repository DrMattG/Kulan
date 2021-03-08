#' Extract all csv files from a nested folder on your local computer
#' @param path the path to the nested folders
#' @export

get_tables=function(path=path){

file_list=list.files(path =path ,pattern = "_LeftCamera.csv$", recursive = TRUE)

#initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
dataset <- data.frame()
require(data.table)
for (i in 1:length(file_list)){
  temp_data <- fread(paste0(path,"/",file_list[i]), stringsAsFactors = F) #read in files using the fread function from the data.table package
  temp_data$Class <- sapply(strsplit(gsub(".csv", "", file_list[i]), "/"), function(x){x[2]})
  dataset <- rbindlist(list(dataset, temp_data), use.names = T) #for each iteration, bind the new data to the building dataset
}
dataset
}
#df=get_tables(path="data/tables/")
#names(df)
#head(df)
