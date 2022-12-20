# ---------------------------------------------------------
# the validity function of PV
check.Prodth <- function(object) {
  errors <- character()

  if (length(object@timegrid) != 0 & length(object@load_15min_th) >= 1) {
    if (sum(object@timegrid) > 15 * length(object@load_15min_th)) {
      msg <- "time window of load_15min_th to short.
      simulation time defined by timegrid has to be shorter
      than the time window defined by load_15min_th"
      errors <- c(errors, msg)
    }
  }

  if (length(object@timegrid) != 0 & length(object@load_abstract_th) >= 1) {
    if (length(object@timegrid) != length(object@load_abstract_th)) {
      msg <- "time grid has to have the same length as load_abstract_th"
      errors <- c(errors, msg)
    }
  }

  if (length(object@load_15min_th) >= 1 & length(object@load_abstract_th) >= 1) {

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
#' Class Prodth
#'
#'The class Prodth represents a thermal producer
#'
#'@slot load_15min_th  the thermal load in kW in a 15 min timegrid which can be used to store timeseries on a 15 minute timegrid.
#'@slot load_abstract_th
#'@export
setClass(
  "Prodth",
  slots = list(
    load_15min_th = "numeric",
    load_abstract_th = "numeric"
  ),
  contains = "Root",
  validity = check.Prodth
)
# constructor ---------------------------------------------
# ---------------------------------------------------------
#' new.Prodth
#'
#'Constructor of Prodth creates a new obejct of class Prodth
#'
#'@return New object of class Prodth
#'@export
new.Prodth <- function() {
  return(new(Class = "Prodth", variables= c("P_th_plus")
  ))
}
