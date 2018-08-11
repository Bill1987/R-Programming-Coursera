source("hospitalHelper.R")

rankhospital <- function(state, outcome, num = "best") {
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    
    ## num translate to numeric
    ## Put it here because if the parameters are invalid, 
    ## you don't have to waste time taking data.
    num <- numAsNumeric(num)
    
    outcome_colname <- getColName(outcome)
    
    hospital_data <- loadHospitalData()
    outcome_data <- getOutcomeDataByState(hospital_data,outcome_colname,state)
    

    
    ## Death..Mortality order by hospitals
    ## in alphabetical order by hospitals
    outcome_rank <- outcome_data[order( outcome_data[,outcome_colname] ,outcome_data[,"Hospital.Name"]),]
    
    ## -1 is the last row
    if(num == -1){
        num <- nrow(outcome_rank)
    }
    
    return(outcome_rank[,"Hospital.Name"][num])
}


#----------------------------------------------------
#example:
# 
# > source("rankhospital.R")
# > rankhospital("TX", "heart failure", 4)
# [1] "DETAR HOSPITAL NAVARRO"
# 
# > rankhospital("MD", "heart attack", "worst")
# [1] "HARFORD MEMORIAL HOSPITAL"
# 
# > rankhospital("MN", "heart attack", 5000)
# [1] NA

