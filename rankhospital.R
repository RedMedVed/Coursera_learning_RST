rankhospital <- function(state, outcome, num = 'best') {
  ## Read outcome data
  hosp_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # in this particular case we do not need all the data, so let's take a subset
  data <- subset(hosps, select = c(2, 7, 11, 17, 23))
  colnames(data) <- c('hospital.name', 'State', 'heart attack', 'heart failure', 'pneumonia')
  
  
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

  #can't find any ways to remove 'Not Available' from dataframe
  #will use other method
  #and choose the hospital with desired rank
  if(num == 'best') {
    data <- data[order(as.numeric(data[, outcome]), data$hospital.name),]
    data[1, 1]
  } else if(num == 'worst') {
    data <- data[order(as.numeric(data[, outcome]), data$hospital.name, na.last = FALSE),]
    tail(data, n = 1)[, 1]
  } else {
    data <- data[order(as.numeric(data[, outcome]), data$hospital.name),]
    data[num, 1]
  }
  
}