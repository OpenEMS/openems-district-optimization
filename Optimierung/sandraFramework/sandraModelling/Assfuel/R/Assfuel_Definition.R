# ---------------------------------------------------------
# the validity function of Assfuel
check.Assfuel <- function(object) {
  errors <- character()



  if (length(errors) == 0) {
    return(TRUE)
  }else{

    return(errors)
  }
}
#---Class Assfuel------------------------------------------------------
#'Class Assfuel
#'
#'The Assfuel represents the class AdditionalSinkSourceFuel
#'
#'@export
setClass(
  "Assfuel",
    contains = "Root",
  validity = check.Assfuel
)
# constructor ---------------------------------------------
# ---------------------------------------------------------
#' new.Assfuel
#'
#'Constructor of Assfuel creates a new obejct of class Assfuel
#'
#'@return New object of class Assfuel
#'@export
new.Assfuel <- function() {
  return(new(Class = "Assfuel", variables= c("P_fuel_minus", "P_fuel_plus")
  ))
}
