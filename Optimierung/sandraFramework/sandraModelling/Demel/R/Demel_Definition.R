# ---------------------------------------------------------
# the validity function of Demel
check.Demel <- function(object) {
  errors <- character()

  if (length(object@timegrid) != 0 & length(object@load_15min_el) > 1) {
    if (sum(object@timegrid) > 15 * length(object@load_15min_el)) {
      msg <- "time window of load_15min_el to short.
      simulation time defined by timegrid has to be shorter
      than the time window defined by load_15min_el"
      errors <- c(errors, msg)
    }
  }
  if (length(object@timegrid) != 0 & length(object@load_abstract_el) > 1) {
    if(length(object@timegrid)!=length(object@load_abstract_el)){
      msg <- "time grid and load_abstract_el have to be of the same length"
      errors <- c(errors, msg)
    }
  }

  if(length(object@load_abstract_el)!=0 & length(object@load_15min_el)!=0){
    msg <- "load_abtstract_el and load_15min_el may not both be set"
    errors <- c(errors, msg)
  }

  if (length(errors) == 0) {
    return(TRUE)
  }else{

    return(errors)
  }
}
# ---------------------------------------------------------
#' Class Demel
#'
#'The Demel represents the class DemandPower
#'
#'@slot load_15min_el  the electrical load in kW in a 15 min timegrid which can be used to store timeseries on a 15 minute timegrid.
#'@slot load_abstract_el
#'@export
setClass(
  "Demel",
  slots = list(
    load_15min_el = "numeric",
    load_abstract_el = "numeric"
  ),
  contains = "Root",
  validity = check.Demel
)
# constructor ---------------------------------------------
# ---------------------------------------------------------
#' new.Demel
#'
#'Constructor of Demel creates a new obejct of class Demel
#'
#'@return New object of class Demel
#'@export
new.Demel <- function() {
  return(new(Class = "Demel", variables= c("P_el_minus")
  ))
}
