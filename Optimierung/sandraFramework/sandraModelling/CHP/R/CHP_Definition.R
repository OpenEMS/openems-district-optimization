#---validity function of CHP-------------------------------
check.CHP<-function(object){
  errors<-character();

  # if (length(object@effMaxP_el_plus) == 1 &  length(object@effMinP_el_plus) == 1) {
  #   if (object@effMaxP_el_plus < object@effMinP_el_plus) {
  #     msg <- "effMaxP_el_plus has to bigger than effMinP_el_plus"
  #     errors <- c(msg, errors)
  #   }
  # }

  # if (length(object@effMaxP_th_plus) == 1 &  length(object@effMinP_th_plus) == 1) {
  #   if (object@effMaxP_th_plus < object@effMinP_th_plus) {
  #     msg <- "effMaxP_th_plus has to bigger than effMinP_th_plus"
  #     errors <- c(msg, errors)
  #   }
  # }

  # if (length(object@effMaxP_fuel_minus) == 1 &  length(object@effMinP_fuel_minus) == 1) {
  #   if (object@effMaxP_fuel_minus < object@effMinP_fuel_minus) {
  #     msg <- "effMaxP_fuel_minus has to bigger than effMinP_fuel_minus (Note:effMinP_fuel_minus is calculated by effMinP_th_plus(object) + effMinP_el_plus(object), effMaxP_fuel_minus is calculated by effMaxP_th_plus(object) + effMaxP_el_plus(object)"
  #     errors <- c(msg, errors)
  #   }
  # }

   if (length(object@maxP_el_plus) == 1 &  length(object@minP_el_plus) == 1) {
    if (object@maxP_el_plus < object@minP_el_plus & object@maxP_el_plus != -1) {
      msg <- "maxP_el_plus has to bigger than minP_el_plus or maxP_el_plus has to be -1 (unlimited)"
      errors <- c(msg, errors)
    }
   }
  # if (length(object@maxP_th_plus) == 1 &  length(object@minP_th_plus) == 1) {
  #   if (object@maxP_th_plus < object@minP_th_plus) {
  #      msg <- "maxP_th_plus has to bigger than minP_th_plus (Note: maxP_th_plus is calcuated by effMaxP_th_plus* maxP_el_plus/effMaxP_el_plus and minP_th_plus is calculated by effMinP_th_plus* minP_el_fuel/effMinP_el_plus)"
  #      errors <- c(msg, errors)
  #    }
  # }
#
#   if (length(object@maxP_fuel_minus) == 1 &  length(object@minP_fuel_minus) == 1) {
#     if (object@maxP_fuel_minus < object@minP_fuel_minus & object@maxP_fuel_minus != -1) {
#       msg <- "maxP_fuel_minus has to bigger than minP_fuel_minus (Note: maxP_fuel_minus is calcuated by maxP_el_plus/effMaxP_el_plus and minP_fuel_minus is calculated by minP_el_fuel/effMinP_el_plus) or maxP_fuel_minus (set maxP_el_plus -1) has to be -1 (unlimited) "
#       errors <- c(msg, errors)
#     }
#   }

  if (length(object@maxP_fuel_minus) == 1 &  length(object@maxP_th_plus)==1 & length(object@maxP_el_plus)  == 1 ) {
    if (object@maxP_fuel_minus < object@maxP_th_plus + object@maxP_el_plus & object@maxP_el_plus != -1) {
      msg <- "maxP_fuel_minus has to bigger than maxP_th_plus + maxP_el_plus  or maxP_el_plus has to be -1 (if maxP_el_plus is set to -1, than maxP_th_plus and maxP_fuel_minus are also set to -1) (unlimited)"
      errors <- c(msg, errors)
    }
  }

  if (length(object@minP_fuel_minus) == 1 &  length(object@minP_th_plus)==1 & length(object@minP_el_plus)  == 1) {
    if (object@minP_fuel_minus < object@minP_th_plus + object@minP_el_plus) {
      msg <- "minP_fuel_minus has to bigger than minP_th_plus + minP_el_plus "
      errors <- c(msg, errors)
    }
  }

  if(length(object@load_P_th_plus)*length(object@maxP_th_plus)>0){
    if(any(object@load_P_th_plus>object@maxP_th_plus  & object@maxP_th_plus != -1)){
      msg <- paste("all values of the profil have to be smaller or equal maxP_th_plus=",object@maxP_th_plus)
      errors <- c(msg, errors)
    }
  }

  if(length(errors)==0) {
    return(TRUE)
  } else{
    return(errors)
  }
}
#---Definition of CHP--------------------------------------
#'The class CHP
#'
#'CHP describes a combined heat and electric power plant
#'
#'@export
#'@slot maxP_el_plus The maximal electric power output in kW
#'@slot maxP_fuel_minus The maximal fuel power input in kW
#'@slot maxP_th_plus The maximal thermal power output in kW
#'@slot minP_el_plus The minimal electric power output before shutdown in kW
#'@slot minP_fuel_minus The minimal fuel power input before shutdown in kW
#'@slot minP_th_plus The minimal thermal power output before shutdown in kW
#'@slot effMaxP_el_plus The electric efficiency at maximum power in \%
#'@slot effMaxP_fuel_minus the fuel efficiency at maximum power in \%
#'@slot effMaxP_th_plus The thermal efficiency at maximum power in \%
#'@slot effMinP_el_plus The electric efficiency at minimum power before shutdown in \%
#'@slot effMinP_fuel_minus The fuel efficiency at minimum power before shutdown in \%
#'@slot effMinP_th_plus The thermal efficiency at minimum power before shutdown in \%
#'@slot price_maintenance The maintenance price for every produced kilowatthour in €/kWh_el
#'@slot price_opChange The price for every change of the operation status €/kWh_el
#'@slot minRuntime Minimum Runtime of the CHP in hours
#'@slot minDowntime Minimum Downtime of CHP in hours
#'@slot load_P_th_plus Profil of P_th_plus of a CHP
#'@slot initial_state initil state
#'@slot initial_stateEndurance endurance of initial state
#'@slot initial_hours hours of initial state
#'@slot modulated do you want to schedule the chp modulated

setClass("CHP",
         slots = list(
           maxP_el_plus = "numeric",
           maxP_fuel_minus = "numeric",
           maxP_th_plus = "numeric",
           minP_el_plus = "numeric",
           minP_fuel_minus = "numeric",
           minP_th_plus = "numeric",
           effMaxP_el_plus = "numeric",
           effMaxP_fuel_minus = "numeric",
           effMaxP_th_plus = "numeric",
           effMinP_el_plus = "numeric",
           effMinP_fuel_minus = "numeric",
           effMinP_th_plus = "numeric",
           price_maintenance = "numeric",
           price_opChange = "numeric",
           minRuntime = "numeric",
           minDowntime = "numeric",
           initial_state = "numeric",
           initial_stateEndurance = "numeric",
           initial_hours = "numeric",
           load_P_th_plus = "numeric",
           modulated = "numeric"
         ),
         contains = "Root",
         validity = check.CHP
)
#---constructor--------------------------------------------
#'new.CHP
#'
#'creat a new obejct of class CHP
#'It has the standard variables included, which are:
#'- Fuele input (P_fuel_minus)
#'- Power output (P_el_plus)
#'- Heat output (P_th_plus)
#'- State of use (Op) to be able to simulate a minimum of load before shutdown
#'
#'@export
#'@return New object of class CHP
new.CHP <- function() {
  return(new(Class = "CHP", variables = c("P_fuel_minus", "P_el_plus", "P_th_plus",
                                          "Op")))
}
