best <- function(state, outcome) {
  
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  valid_outcomes <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23)
  
  # Check that state and outcome are valid
  if (!(state %in% data$State)) {
    stop("Invalid state")
  }
  
  if (!(outcome %in% names(valid_outcomes))) {
    stop("Invalid outcome")
  }
  
  # Subset data by state
  state_data <- data[data$State == state, ]
  outcome_col <- valid_outcomes[outcome]
  outcome_data <- as.numeric(state_data[, outcome_col])
  
 
  min_mortality <- min(outcome_data, na.rm = TRUE)
  
  #Minimum mortality rate
  best_hospitals <- subset(state_data, outcome_data == min_mortality)
  
  #Sort hospitals alphabetically
  best_hospitals <- best_hospitals[order(best_hospitals$Hospital.Name), ]
  
  
  return(best_hospitals$Hospital.Name[1])
}

