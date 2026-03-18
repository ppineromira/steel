convertFactorsToCharacter <- function(dt) {
  # Convert all factors in the dataframe to character
  dt[] <- lapply(dt, function(x) {
    if (is.factor(x)) {
      return(as.character(x))
    } else {
      return(x)
    }
  })
  return(dt)
}
