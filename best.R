best <- function(state, outcome) {
  ## Read outcome data
  hosp_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # in this particular case we do not need all the data, so let's take a subset
  data <- subset(hosps, select = c(2, 7, 11, 17, 23))
  colnames(data) <- c('hospital.name', 'State', 'heart attack', 'heart failure', 'pneumonia')
  
  #transform columns to numeric
  #data <- lapply(data[3:5], as.numeric)
  
  ## Check that state and outcome are valid
  if(!state %in% unique(data$State)) {
    stop('invalid state')
  }
  if(!outcome %in% c('heart attack', 'heart failure', 'pneumonia')) {
    stop('invalid outcome')
  }
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  data <- data[data$State == state, ]
  data$hospital.name[data[, outcome] == min(as.numeric(data[, outcome]), na.rm = TRUE)]
}