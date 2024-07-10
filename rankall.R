rankall <- function(outcome, num = "best") {
  results <- list()
  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that outcome is valid
  valid_outcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
  
  if (!(outcome %in% names(valid_outcomes))) {
    stop("invalid outcome")
  }
  
  oc <- valid_outcomes[outcome]
  
  
  outcome_data <- outcome_data[complete.cases(outcome_data), ]
  splitted_state <- split(outcome_data, outcome_data$State)
  
  for (state in unique(names(splitted_state))) {
    state_data <- splitted_state[[state]]
    
    
    state_data[[oc]] <- as.numeric(state_data[[oc]])
    #Remove NA
    state_data <- state_data[!is.na(state_data[[oc]]), ]
    #Number of hospitals in state
    num_hospitals <- nrow(state_data)
    
    if (num == "best") {
      selected_hospital <- state_data[which.min(state_data[[oc]]), "Hospital.Name"]
    } else if (num == "worst") {
      selected_hospital <- state_data[which.max(state_data[[oc]]), "Hospital.Name"]
    } else if (is.numeric(num)) {
      if (num > num_hospitals || num <= 0) {
        selected_hospital <- state_data[NA, "Hospital.Name"]
      }
      selected_hospital <- state_data[order(state_data[[oc]])[num], "Hospital.Name"]
    } else {
      stop("invalid num")
    }
    
    results[[state]] <- list(hospital = selected_hospital, state = state)
  }
  
  results <- do.call(rbind, results)
  
  return(results)
}

print( tail(rankall("heart failure"), 10))
