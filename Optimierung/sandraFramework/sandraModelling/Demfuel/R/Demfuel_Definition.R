# ---------------------------------------------------------
# the validity function of Demfuel
check.Demfuel <- function(object) {
  errors <- character()

  if (length(object@timegrid) != 0 & length(object@load_15min_fuel) >= 1) {
    if (sum(object@timegrid) > 15 * length(object@load_15min_fuel)) {
      msg <- "time window of load_15min_fuel to short.
      simulation time defined by timegrid has to be shorter
      than the time window defined by load_15min_fuel"
      errors <- c(errors, msg)
    }
  }

  if (length(object@timegrid) != 0 & length(object@load_abstract_fuel) >= 1) {
    if (length(object@timegrid) != length(object@load_abstract_fuel) ) {
      msg <- "time grid has to of the same length as load_abstract_fuel"
      errors <- c(errors, msg)
    }
  }

  if (length(object@load_15min_fuel) != 0 & length(object@load_abstract_fuel) >= 1) {

      msg <- "load_15min_fuel and load_abstract_fuel may not both be set"
      errors <- c(errors, msg)

  }

  if (length(errors) == 0) {
    return(TRUE)
  }else{

    return(errors)
  }
  }
# ---------------------------------------------------------
#' Class Demfuel
#'
#'The Demfuel represents the class DemandFuel
#'
#'@slot load_15min_fuel  the fuel load in kW in a 15 min timegrid which can be used to store timeseries on a 15 minute timegrid.
#'@slot load_abstract_fuel the fuel load in kW
#'@export
setClass(
  "Demfuel",
  slots = list(
    load_15min_fuel = "numeric",
    load_abstract_fuel = "numeric"
  ),
  contains = "Root",
  validity = check.Demfuel
)
# constructor ---------------------------------------------
# ---------------------------------------------------------
#' new.Demfuel
#'
#'Constructor of Demfuel creates a new obejct of class Demfuel
#'
#'@return New object of class Demfuel
#'@export
new.Demfuel <- function() {
  return(new(Class = "Demfuel", variables= c("P_fuel_minus")
  ))
}
