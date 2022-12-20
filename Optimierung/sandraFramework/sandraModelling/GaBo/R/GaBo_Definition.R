# ---------------------------------------------------------
# the validity function of GaBo
check.GaBo <- function(object) {
  errors <- character()


  if (length(errors) == 0) {
    return(TRUE)
  }else{

    return(errors)
  }
}
# ---------------------------------------------------------
#' Class GaBo
#'
#'The GaBo represents the class GasBoilers
#'
#'@slot maxP_th_plus the maximal output of thermal power in kW
#'@slot effMaxP_th_plus the degree of efficiency between thermal production and fuel provided, i.e. percentage of thermal power, which is usable inside of a cluster of energy prosumers.
#'@slot maxP_fuel_minus   maximal input of fuel power in kW

#'@export
setClass(
  "GaBo",
  slots = list(
    maxP_fuel_minus = "numeric" ,
    effMaxP_th_plus = "numeric" ,
    maxP_th_plus = "numeric"
  ),
  contains = "Root",
  validity = check.GaBo
)
# constructor ---------------------------------------------
# ---------------------------------------------------------
#' new.GaBo
#'
#'Constructor of GaBo creates a new obejct of class GaBo
#'
#'@return New object of class GaBo
#'@export
new.GaBo <- function() {
  return(new(Class = "GaBo", variables= c("P_th_plus", "P_fuel_minus")
  ))
}
