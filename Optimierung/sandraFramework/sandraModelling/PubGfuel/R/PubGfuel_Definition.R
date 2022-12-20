# ---------------------------------------------------------
# the validity function of PubGfuel
check.PubGfuel <- function(object) {
  errors <- character()

  if (length(object@maxP_fuel_minus) == 1 &  length(object@minP_fuel_minus) == 1) {
    if (object@maxP_fuel_minus < object@minP_fuel_minus & object@maxP_fuel_minus != -1) {
      msg <- "maxP_fuel_minus has to bigger than minP_fuel_minus or maxP_fuel_minus has to be -1 (unlimited)"
      errors <- c(msg, errors)
    }
  }

  if (length(object@maxP_fuel_plus) == 1 &  length(object@minP_fuel_plus) == 1) {
    if (object@maxP_fuel_plus < object@minP_fuel_plus & object@maxP_fuel_plus != -1) {
      msg <- "maxP_fuel_plus has to bigger than minP_fuel_plus or maxP_fuel_plus has to be -1 (unlimited)"
      errors <- c(msg, errors)
    }
  }





  if (length(errors) == 0) {
    return(TRUE)
  }else{

    return(errors)
  }
}
# ---------------------------------------------------------
#' Class PubGfuel
#'
#'The PubGfuel represents the class PublicGridFuel
#'
#'@slot maxP_fuel_minus  maximal input of electric power in kW
#'@slot minP_fuel_minus minimal input of electric power in kW
#'@slot maxP_fuel_plus  maximal output of electric power in kW
#'@slot minP_fuel_plus minimal output of electric power in kW
#'@export
setClass(
  "PubGfuel",
  slots = list(
    maxP_fuel_minus = "numeric" ,
    minP_fuel_minus = "numeric" ,
    maxP_fuel_plus = "numeric" ,
    minP_fuel_plus = "numeric"
  ),
  contains = "Root",
  validity = check.PubGfuel
)
# constructor ---------------------------------------------
# ---------------------------------------------------------
#' new.PubGfuel
#'
#'Constructor of PubGfuel creates a new obejct of class PubGfuel
#'
#'@return New object of class PubGfuel
#'@export
new.PubGfuel <- function() {
  return(new(Class = "PubGfuel",variables = c("P_fuel_plus","P_fuel_minus")  ))
}
