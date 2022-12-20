# ---------------------------------------------------------
# the validity function of Demth
check.Demth <- function(object) {
  errors <- character()

  if (length(object@timegrid) != 0 & length(object@load_15min_th) > 1) {
    if (sum(object@timegrid) > 15 * length(object@load_15min_th)) {
      msg <- "time window of load_15min_th to short.
      simulation time defined by timegrid has to be shorter
      than the time window defined by load_15min_th"
      errors <- c(errors, msg)
    }
  }

  if (length(object@timegrid) != 0 & length(object@load_abstract_th)!=0) {

    if (length(object@timegrid) != length(object@load_abstract_th) ) {
      msg <- "time grid has to be of the same length as load_abstract_th"
      errors <- c(errors, msg)
    }
  }
  if (length(object@load_15min_th) != 0 & length(object@load_abstract_th) > 1) {
    msg <- "load_abstract_th and load_15min_th may not both be set"
    errors <- c(errors, msg)

  }
  if (length(errors) == 0) {
    return(TRUE)
  }else{

    return(errors)
  }
  }
# ---------------------------------------------------------
#' Class Demth
#'
#'The Demth represents the class DemandHeat
#'
#'@slot load_15min_th  the thermal load in kW in a 15 min timegrid which can be used to store timeseries on a 15 minute timegrid.
#'@slot load_abstract_th  the thermal load in kW
#'@slot price_th_minus the thermal price (output) in €/kWh
#'@export
setClass(
  "Demth",
  slots = list(
    load_15min_th = "numeric"  ,
    load_abstract_th = "numeric"),
  contains = "Root",
  validity = check.Demth
)
# constructor ---------------------------------------------
# ---------------------------------------------------------
#' new.Demth
#'
#'Constructor of Demth creates a new obejct of class Demth
#'
#'@return New object of class Demth
#'@export
new.Demth <- function() {
  return(new(Class = "Demth", variables= c("P_th_minus")
  ))
}
