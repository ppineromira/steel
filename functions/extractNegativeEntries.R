extractNegativeEntries <- function(mat) {

  # Find positions of negative values
  negPositions <- which(mat < 0, arr.ind = TRUE)
  
  # If no negative values found
  if (nrow(negPositions) == 0) {
    return(data.frame(
      rowname = character(0),
      colname = character(0),
      value = numeric(0)
    ))
  }
  
  # Extract row names, column names, and values
  result <- data.frame(
    rowname = rownames(mat)[negPositions[,1]],
    colname = colnames(mat)[negPositions[,2]],
    value = mat[negPositions]
  )
  
  return(result)
}
