loadHospitalData <- function(){
    ## Read outcome data
    read.csv('data/outcome-of-care-measures.csv',stringsAsFactors = FALSE)
}

## Convert parameters:outcome to column names
## If the parameters is invalid, the program will be stopped
getColName <- function(outcome){
    outcome_colname <- c("")        
    if (outcome == "heart attack") {
        outcome_colname <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    } else if (outcome == "heart failure") {
        outcome_colname <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    } else if (outcome == "pneumonia"){
        outcome_colname <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    } else{
        ## Check that outcome is valid
        stop("invalid outcome")
    }
    
    return(outcome_colname)
}

## If the return value of the function is -1, 
## it is not a valid ranking data, it represents the last one
## You should be like this£º
## if(num == -1) num <- nrow(your_data)
numAsNumeric <- function(num){
    ## num translate to numeric
    num <- if (num == "best"){
        1
    }
    else if(num == "worst"){
        ## -1 is not valid data, just because of efficiency
        ## It should be the last line of valid data
        -1      
    }
    else{
        ## Check that state is valid
        x <- suppressWarnings(as.numeric(num))
        if(is.na(x)){
            stop("invalid num")
        }
        x
    }
    
    return(num)
}
    

getOutcomeDataByState <- function(outcome_data,outcome_colname,state){
    
    ## Check that state is valid
    states_all <- unique(outcome_data$State)    ##get kinds of state
    if(!is.element(state, states_all)){
        stop("invalid state")
    }

    ## data translate to "numeric", but the data hava "NA", it will cause a warning ,
    suppressWarnings(outcome_data[,outcome_colname] <- as.numeric(outcome_data[,outcome_colname]))
    
    ## ignoring NA values
    outcome_complete = outcome_data[complete.cases(outcome_data[outcome_colname]),] 
    outcome_complete <- outcome_complete[outcome_complete$State == state,] 
    
    return(outcome_complete)
}

