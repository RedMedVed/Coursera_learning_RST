rankall <- function(outcome, num = 'best') {
  ## Read outcome data
  hosp_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  result <- data.frame(hospital = character(), state = character())
  #colnames(result) <- c('hospital', 'state')
  
  # in this particular case we do not need all the data, so let's take a subset
  data <- subset(hosps, select = c(2, 7, 11, 17, 23))
  colnames(data) <- c('hospital.name', 'State', 'heart attack', 'heart failure', 'pneumonia')
  
  ## Check that outcome is valid
  if(!outcome %in% c('heart attack', 'heart failure', 'pneumonia')) {
    stop('invalid outcome')
  }
  
  
  ## For each state, find the hospital of the given rank
  states <- unique(data$State)
  states <- sort(states)
  for(state in states) {
    #print(state)
    state_data <- subset(data, State == state)
    #print(state_data)
    if(num == 'best') {
      #print(head(data))
      state_data <- state_data[order(as.numeric(state_data[, outcome]), 
                                     state_data$hospital.name),]
      #print(head(state_data))
      hospital <- state_data[1, 1]
      #print(hospital)
    } else if(num == 'worst') {
      state_data <- state_data[order(as.numeric(state_data[, outcome]), 
                                     state_data$hospital.name, na.last = FALSE),]
      hospital <- tail(state_data, n = 1)[, 1]
    } else {
      state_data <- state_data[order(as.numeric(state_data[, outcome]), 
                                     state_data$hospital.name),]
      hospital <- state_data[num, 1]
    }
    new.row <- data.frame(hospital = hospital, state = state)
    result <- rbind(result, new.row)
  }
  #result <- order(result, result$state)
  result
  #print(result)
}