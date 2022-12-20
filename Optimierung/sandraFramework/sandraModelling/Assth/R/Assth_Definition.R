# ---------------------------------------------------------
# the validity function of Assth
check.Assth <- function(object) {
  errors <- character()



  if (length(errors) == 0) {
    return(TRUE)
  }else{

    return(errors)
  }
}
# ---------------------------------------------------------
#' Class Assth
#'
#'The Assth represents the class AdditionalSinkSourceHeat
#'@export
setClass(
  "Assth",
  contains = "Root",
  validity = check.Assth
)
# constructor ---------------------------------------------
# ---------------------------------------------------------
#' new.Assth
#'
#'Constructor of Assth creates a new obejct of class Assth
#'
#'@return New object of class Assth
#'@export
new.Assth <- function() {
  return(new(Class = "Assth", variables= c("P_th_minus", "P_th_plus")
  ))
}
