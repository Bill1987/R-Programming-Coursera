source("hospitalHelper.R")

rankall <- function(outcome, num = "best") {
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    
    ## num translate to numeric
    ## Put it here because if the parameters are invalid, 
    ## you don't have to waste time taking data.
    num <- numAsNumeric(num)
    
    outcome_colname <- getColName(outcome)
    
    hospital_data <- loadHospitalData()
    
    hospitals <- c()
    states <- c()
    states_all <- sort(unique(hospital_data$State))    ##get kinds of 

    for(state in states_all)
    {
        outcome_data <- getOutcomeDataByState(hospital_data,outcome_colname,state)
        
        ## Death..Mortality order by hospitals
        ## in alphabetical order by hospitals
        outcome_rank <- outcome_data[order( outcome_data[,outcome_colname] ,outcome_data[,"Hospital.Name"]),]
        
        ## -1 is the last row
        num_rank <- if(num == -1){
             nrow(outcome_rank)
        }
        else{
            num
        }
        
        hospitals <- c(hospitals, outcome_rank[,"Hospital.Name"][num_rank])
        states <- c(states, state)
    }
    
    return(data.frame(hospital = hospitals,state = states ,row.names = states ))
}

#----------------------------------------------------
# example:
# 
# > source("rankall.R")
# > head(rankall("heart attack", 20), 10)
# hospital state
# AK                                <NA>    AK
# AL      D W MCMILLAN MEMORIAL HOSPITAL    AL
# AR   ARKANSAS METHODIST MEDICAL CENTER    AR
# AZ JOHN C LINCOLN DEER VALLEY HOSPITAL    AZ
# CA               SHERMAN OAKS HOSPITAL    CA
# CO            SKY RIDGE MEDICAL CENTER    CO
# CT             MIDSTATE MEDICAL CENTER    CT
# DC                                <NA>    DC
# DE                                <NA>    DE
# FL      SOUTH FLORIDA BAPTIST HOSPITAL    FL
# 
# > tail(rankall("pneumonia", "worst"), 3)
# hospital state
# WI MAYO CLINIC HEALTH SYSTEM - NORTHLAND, INC    WI
# WV                     PLATEAU MEDICAL CENTER    WV
# WY           NORTH BIG HORN HOSPITAL DISTRICT    WY
# 
# > tail(rankall("heart failure"), 10)
# hospital state
# TN                         WELLMONT HAWKINS COUNTY MEMORIAL HOSPITAL    TN
# TX                                        FORT DUNCAN MEDICAL CENTER    TX
# UT VA SALT LAKE CITY HEALTHCARE - GEORGE E. WAHLEN VA MEDICAL CENTER    UT
# VA                                          SENTARA POTOMAC HOSPITAL    VA
# VI                            GOV JUAN F LUIS HOSPITAL & MEDICAL CTR    VI
# VT                                              SPRINGFIELD HOSPITAL    VT
# WA                                         HARBORVIEW MEDICAL CENTER    WA
# WI                                    AURORA ST LUKES MEDICAL CENTER    WI
# WV                                         FAIRMONT GENERAL HOSPITAL    WV
# WY                                        CHEYENNE VA MEDICAL CENTER    WY

