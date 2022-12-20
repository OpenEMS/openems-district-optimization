#---validity function of Electrolyseur-------------------------------
check.Electrolyseur<-function(object){
  errors<-character();

  if (length(object@maxP_el_minus) == 1 &  length(object@minP_el_minus) == 1) {
    if (object@maxP_el_minus < object@minP_el_minus & object@maxP_el_minus != -1) {
      msg <- "maxP_el_minus has to be bigger than minP_el_minus or maxP_el_minus has to be -1 (unlimited)"
      errors <- c(msg, errors)
    }
   }
 
  if (length(object@maxP_el_minus) == 1 &  length(object@maxP_th_plus)==1 & length(object@maxP_fuel_plus)  == 1 ) {
    if (object@maxP_el_minus < object@maxP_th_plus + object@maxP_fuel_plus & object@maxP_el_minus != -1) {
      msg <- "maxP_el_minus has to be bigger than maxP_th_plus + maxP_fuel_plus  or maxP_el_minus has to be -1 (if maxP_el_minus is set to -1, than maxP_th_plus and maxP_fuel_plus are also set to -1) (unlimited)"
      errors <- c(msg, errors)
    }
  }

  if (length(object@minP_el_minus) == 1 &  length(object@minP_th_plus)==1 & length(object@minP_fuel_plus)  == 1) {
    if (object@minP_el_minus < object@minP_th_plus + object@minP_fuel_plus) {
      msg <- "minP_el_minus has to be bigger than minP_th_plus + minP_fuel_plus"
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
#'@slot maxP_el_minus The maximal electric power input in kW
#'@slot maxP_fuel_plus The maximal fuel power output in kW
#'@slot maxP_th_plus The maximal thermal power output in kW
#'@slot minP_el_minus The minimal electric power input before shutdown in kW
#'@slot minP_fuel_plus The minimal fuel power output before shutdown in kW
#'@slot minP_th_plus The minimal thermal power output before shutdown in kW
#'@slot effMaxP_el_minus The electric efficiency at maximum power in \%
#'@slot effMaxP_fuel_plus the fuel efficiency at maximum power in \%
#'@slot effMaxP_th_plus The thermal efficiency at maximum power in \%
#'@slot effMinP_el_minus The electric efficiency at minimum power before shutdown in \%
#'@slot effMinP_fuel_plus The fuel efficiency at minimum power before shutdown in \%
#'@slot effMinP_th_plus The thermal efficiency at minimum power before shutdown in \%
#'@slot price_maintenance The maintenance price for every produced kilowatthour hydrogen in €/kWh_fuel
#'@slot minRuntime Minimum Runtime of the electrolyseur in hours
#'@slot minDowntime Minimum Downtime of electrolyseur in hours
#'@slot load_P_th_plus Profil of P_th_plus of an electrolyseur
#'@slot initial_state initil state
#'@slot initial_stateEndurance endurance of initial state
#'@slot modulated do you want to schedule the electrolyseur modulated

setClass("Electrolyseur",
         slots = list(
           maxP_el_minus = "numeric",
           maxP_fuel_plus = "numeric",
           maxP_th_plus = "numeric",
           minP_el_minus = "numeric",
           minP_fuel_plus = "numeric",
           minP_th_plus = "numeric",
           effMaxP_el_minus = "numeric",
           effMaxP_fuel_plus = "numeric",
           effMaxP_th_plus = "numeric",
           effMinP_el_minus = "numeric",
           effMinP_fuel_plus = "numeric",
           effMinP_th_plus = "numeric",
           price_maintenance = "numeric",
           minRuntime = "numeric",
           minDowntime = "numeric",
           initial_state = "numeric",
           initial_stateEndurance = "numeric",
           load_P_th_plus = "numeric",
           modulated = "numeric"
         ),
         contains = "Root",
         validity = check.Electrolyseur
)
#---constructor--------------------------------------------
#'new.Electrolyseur
#'
#'creat a new obejct of class Electrolyseur
#'It has the standard variables included, which are:
#'- Fuele output (P_fuel_plus)
#'- Power input (P_el_minus)
#'- Heat output (P_th_plus)
#'- State of use (Op) to be able to simulate a minimum of load before shutdown
#'
#'@export
#'@return New object of class Electrolyseur
new.Electrolyseur <- function() {
  return(new(Class = "Electrolyseur", variables = c("P_el_minus", "P_fuel_plus", "P_th_plus",
                                          "Op")))
}
