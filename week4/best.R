source("hospitalHelper.R")

best <- function(state, outcome) {
    ## Return hospital name in that state with lowest 30-day death
    ## rate 
    outcome_colname <- getColName(outcome)
    
    hospital_data <- loadHospitalData()
    outcome_data <- getOutcomeDataByState(hospital_data,outcome_colname,state)
    
	## get lowest death data
    min_Death <- min(outcome_data[,outcome_colname]) 
    outcome_best <- outcome_data[outcome_data[outcome_colname] == min_Death,]

	## in alphabetical order by hospitals
    hospitals <- sort(outcome_best[,"Hospital.Name"])    

    return(hospitals[1])
}

#----------------------------------------------------
# # example:
#  > source("best.R")
#  > best("TX", "heart attack")
#  [1] "CYPRESS FAIRBANKS MEDICAL CENTER"
# 
#  > best("TX", "heart failure")
#  [1] "FORT DUNCAN MEDICAL CENTER"
# 
#  > best("MD", "heart attack")
#  [1] "JOHNS HOPKINS HOSPITAL, THE"
# 
#  > best("MD", "pneumonia")
#  [1] "GREATER BALTIMORE MEDICAL CENTER"
# 
#  > best("BB", "heart attack")
#  Error in best("BB", "heart attack") : invalid state
# 
#  > best("NY", "hert attack")
#  Error in best("NY", "hert attack") : invalid outcome
