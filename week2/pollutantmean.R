pollutantmean <- function(directory, pollutant, id = 1:332){

    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    
    data_path <- paste(directory, "//*.csv", sep = "")  #Splicing data file
    all_files <- Sys.glob(data_path)        #get all files by directory
    data_files <- all_files[id]             
    
    all_data <- c()                                   
    for (data_file in data_files) {
        x_data <- read.csv(data_file)					#load data
        x_pollutant <- x_data[,pollutant]
        all_data <- c(all_data, x_pollutant)       	#all pollutant data 
    }
    
    mean(all_data,na.rm = TRUE)		
}

##---------------------------------------------------------------------------
# example:
# 
# > source('pollutantmean.R')
# > pollutantmean("specdata", "sulfate", 1:10)
# [1] 4.064128
#
# > pollutantmean("specdata", "nitrate", 70:72)
# [1] 1.706047
#
# > pollutantmean("specdata", "nitrate", 23)
# [1] 1.280833


