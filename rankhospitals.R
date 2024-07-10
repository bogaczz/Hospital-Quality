rankhospital <- function(state, outcome, num = "best") {
  
  # Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available")
  valid_outcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
  
  # Check that state and outcome are valid
  if (!(state %in% data$State)) {
    stop("Invalid state")
  }
  
  if (!(outcome %in% names(valid_outcomes))) {
    stop("Invalid outcome")
  }
  
  # Subset data by state and outcome
  outcome_col <- valid_outcomes[outcome]
  state_data <- subset(data, State == state)
  outcome_data <- as.numeric(state_data[[outcome_col]])
  
  # Remove NA values from outcome_data and state_data
  state_data <- state_data[!is.na(outcome_data), ]
  outcome_data <- outcome_data[!is.na(outcome_data)]
  
  # Validate num argument
  if (num != "best" && num != "worst" && !is.numeric(num)) {
    stop("Invalid num argument.")
  }
  
  num_hospitals <- nrow(state_data)
  sorted_data <- state_data[order(outcome_data, state_data$Hospital.Name), ]
  
  
  if (num == "best") {
    hospital_name <- sorted_data[1, "Hospital.Name"]
  } else if (num == "worst") {
    hospital_name <- sorted_data[num_hospitals, "Hospital.Name"]
  } else if (is.numeric(num) && num <= num_hospitals && num > 0) {
    hospital_name <- sorted_data[num, "Hospital.Name"]
  } else {
    stop("Invalid num argument.")
  }
  
  return(hospital_name)
}

# Example usage:
print( rankhospital("MD", "heart attack", "worst"))
