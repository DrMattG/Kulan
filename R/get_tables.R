#' Extract all csv files from a nested folder on your local computer
#' @param path the path to the nested folders
#' @param filetype the file type (supports ".csv", ".xls" & ".xlsx" only)
#' @details This function only finds files with the word "RightCamera" in their name. It is important to follow the project naming conventions for all files
#'@examples
#'\dontrun{
#'df=get_tables(path="data/tables/", filetype = ".csv")
#'}
#' @export

# Need to add differenr flie types and we have different formats and flight_right

get_tables=function(path=path, filetype=".csv"){

if(filetype==".csv"){
file_list=grep("(?=.*RightCamera)", list.files(path = path, pattern = "*.csv$", recursive = TRUE), value = TRUE, perl = TRUE)
#initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
dataset <- data.frame()
require(data.table)
for (i in 1:length(file_list)){
  temp_data <- fread(paste0(path,"/",file_list[i]), stringsAsFactors = F) #read in files using the fread function from the data.table package
  temp_data$Class <- gsub(".csv", "", file_list[i])
  temp_data$Class<-gsub("\\/.*", "", temp_data$Class)
  dataset <- rbindlist(list(dataset, temp_data), use.names = T, fill = TRUE) #for each iteration, bind the new data to the building dataset
}
return(dataset)
}
  if(filetype==".xls"){
    file_list=grep("(?=.*RightCamera)", list.files(path = path, pattern = "*.xls$", recursive = TRUE), value = TRUE, perl = TRUE)
    #initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
    dataset <- data.frame()
    require(data.table)
    for (i in 1:length(file_list)){
      temp_data <- readxl::read_xls(paste0(path,"/",file_list[i])) #read in files using the readxl package
      temp_data$Class <- gsub(".xls", "", file_list[i])
      temp_data$Class<-gsub("\\/.*", "", temp_data$Class)
      dataset <- rbindlist(list(dataset, temp_data), use.names = T, fill = TRUE) #for each iteration, bind the new data to the building dataset
    }
    return(dataset)

  }
  if(filetype==".xlsx"){
    file_list=grep("(?=.*RightCamera)", list.files(path = path, pattern = "*.xlsx$", recursive = TRUE), value = TRUE, perl = TRUE)
    #initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
    dataset <- data.frame()
    require(data.table)
    for (i in 1:length(file_list)){
      temp_data <- readxl::read_xlsx(paste0(path,"/",file_list[i])) #read in files using the readxl package
      temp_data$Class <- gsub(".xlsx", "", file_list[i])
      temp_data$Class<-gsub("\\/.*", "", temp_data$Class)
      dataset <- rbindlist(list(dataset, temp_data), use.names = T, fill = TRUE) #for each iteration, bind the new data to the building dataset
    }
    return(dataset)

  }
  if(!filetype ==".csv"|!filetype==".xls"| !filetype==".xlsx"){
    print("file type not supported yet")
  }
  }
#df=get_tables(path="data/tables/", filetype = ".csv")
#names(df)
#head(df)
#tail(df)
#path="data/tables/"

