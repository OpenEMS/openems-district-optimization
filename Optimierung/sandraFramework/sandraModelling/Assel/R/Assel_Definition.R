# ---------------------------------------------------------
# the validity function of Assel
check.Assel <- function(object) {
  errors <- character()

  if (length(errors) == 0) {
    return(TRUE)
  }else{

    return(errors)
  }
}
# ---------------------------------------------------------
#' Class Assel
#'
#'The Assel represents the class AdditionalSinkSourcePower
#'
#'@export
setClass(
  "Assel",
  contains = "Root",
  validity = check.Assel
)
# constructor ---------------------------------------------
# ---------------------------------------------------------
#' new.Assel
#'
#'Constructor of Assel creates a new obejct of class Assel
#'
#'@return New object of class Assel
#'@export
new.Assel <- function() {
  return(new(Class = "Assel", variables= c("P_el_minus", "P_el_plus")
  ))
}
