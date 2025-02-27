#Writing siiimple funciton for yearly lake level (low, normal, high)
#' @param O is outflow in m^3/yr (dam release)
#' @param I is inflow in m^3/yr (south fork of the san joaquin)
#' @param initial is the initial lake level in m^3
#' @param min_level is the minimum lake level in m^3
#' @param max_level is the maximum lake level in m^3
#' @param P is precipitation in m/yr
#' @param E is evaporation in m/yr
#' @return current

lake_level <- function(initial, I, O, P, E, min_level=8000000, max_level=80000000) {
  # Initialize the current lake level
  level <- initial
  
  # Loop through each time step based on the length of the inflows (or other input vectors)
  for (i in seq_along(I)) {
    # Check for minimum level and adjust outflow if below minimum
    if (level < min_level) {
      O[i] <- 0  # No outflow if below minimum level
    }
    
    # Calculate the new lake level
    level <- level + I[i] - O[i] + P[i] - E[i]
    
    # Check for overflow
    if (level > max_level) {
      print("Overflow!")
      level <- max_level  # Set to max level to prevent overflow
    }
    
    # Print status based on the current level
    if (level < min_level) {
      print("Low")
    } else if (level >= min_level & level <= max_level) {
      print("Normal")
    } else {
      print("High")
    }
  }
  
  return(level)  # Return the current lake level after all iterations
}

# quick test comment for Github conflict - Bella