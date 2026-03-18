createAreaAndPiLabel <- function(dt, area, pi, newColumnName) {
  
  # Check if both columns exist in the data frame
  
  if (!all(area %in% names(dt) & pi %in% names(dt))) {
    stop("One or both specified columns do not exist in the data frame.")
  }
  
  # Create the new column by concatenating the two specified columns
  
  dt[[newColumnName]] <- paste0(dt[[area]], "_", dt[[pi]])
  
  return(dt)
}
