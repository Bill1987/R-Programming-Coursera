best <- function(state, outcome) {
    
    ## Read outcome data
    outcome_data <- read.csv('data/outcome-of-care-measures.csv',stringsAsFactors = FALSE)
    
    ## Check that state and outcome are valid
    states_all <- unique(outcome_data$State)    ##全部状态标�?
    if(!is.element(state, states_all)){
        stop("invalid state")
    }

    outcome_colname <- c("")        ##获取对应列名�?
    if (outcome == "heart attack") {
        outcome_colname <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    } else if (outcome == "heart failure") {
        outcome_colname <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    } else if (outcome == "pneumonia"){
        outcome_colname <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    } else{
        stop("invalid outcome")
    }
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate 
    suppressWarnings(outcome_data[,outcome_colname] <- as.numeric(outcome_data[,outcome_colname]))  ##将数据强制转换为数值型

    outcome_complete = outcome_data[complete.cases(outcome_data[outcome_colname]),]         ##去掉空数据的全部有效数据
    outcome_state <- outcome_complete[outcome_complete$State == state,]      ##得到匹配state的数�?

    min_Death <- min(outcome_state[,outcome_colname])     ##获取死亡率最低的数据
    outcome_best <- outcome_state[outcome_state[outcome_colname] == min_Death,]

    hospitals <- sort(outcome_best[,"Hospital.Name"])       ##按字母排序医�?

    return(hospitals[1])
}

#----------------------------------------------------
# # 测试数据
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
