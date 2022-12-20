# ---------------------------------------------------------
# the validity function of PV
check.PV <- function(object) {
  errors <- character()

  if (length(object@timegrid) != 0 & length(object@load_15min_el) >= 1) {
    if (sum(object@timegrid) > 15 * length(object@load_15min_el)) {
      msg <- "time window of load_15min_el to short.
      simulation time defined by timegrid has to be shorter
      than the time window defined by load_15min_el"
      errors <- c(errors, msg)
    }
  }

  if (length(object@timegrid) != 0 & length(object@load_abstract_el) >= 1) {
    if (length(object@timegrid) != length(object@load_abstract_el)) {
      msg <- "time grid has to have the same length as load_abstract_el"
      errors <- c(errors, msg)
    }
  }

  if (length(object@load_15min_el) >= 1 & length(object@load_abstract_el) >= 1) {

      msg <- "load_abstract_el and load_15min_el may not both be set"
      errors <- c(errors, msg)

  }

  if (length(errors) == 0) {
    return(TRUE)
  }else{

    return(errors)
  }
  }
# ---------------------------------------------------------
#' Class PV
#'
#'The PV represents the class Photovoltaic
#'
#'@slot load_15min_el  the electrical load in kW in a 15 min timegrid which can be used to store timeseries on a 15 minute timegrid.
#'@slot load_abstract_el
#'@export
setClass(
  "PV",
  slots = list(
    load_15min_el = "numeric",
    load_abstract_el = "numeric"
  ),
  contains = "Root",
  validity = check.PV
)
# constructor ---------------------------------------------
# ---------------------------------------------------------
#' new.PV
#'
#'Constructor of PV creates a new obejct of class PV
#'
#'@return New object of class PV
#'@export
new.PV <- function() {
  return(new(Class = "PV", variables= c("P_el_plus")
  ))
}
