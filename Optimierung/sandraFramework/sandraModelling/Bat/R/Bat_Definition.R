#----------------------------------------------------------
#----the validity function of Bat--------------------------
check.Bat <- function(object) {
  errors <- character()

  # if (length(object@maxE_el) == 1 &  length(object@minE_el) == 1) {
  #   if (object@maxE_el < object@minE_el) {
  #     msg <- "maxE_el has to bigger than minE_el"
  #     errors <- c(msg, errors)
  #   }
  # }



  if (length(errors) == 0) {
    return(TRUE)
  }else{

    return(errors)
  }
}
#----Definition of Class Bat-------------------------------
#'Class Bat
#'
#'The Bat represents the class battery
#'
#'@slot maxE_el the maximal electirc energy in kWh
#'@slot maxP_el_minus Maximum power for loading the electric storage system in kW
#'@slot maxP_el_plus Maximum power for unloading the electric storage system in kW
#'@slot effMaxP_el_minus Efficiency factor for loading the electric storage system in \%
#'@slot eff_losses Efficiency factor of losses in \% over 24 hours
#'@slot percentageEnd Filling level at the end of simulation time in \%
#'@slot percentageStart Filling level at the start of simulation time in \%
#'@slot EnergyStart Stored energy at the start of simulation in kWh
#'@slot EnergyEnd Stored energy at the end of simulation in kWh
#'@slot lifeTime expected life time in years
#'@slot fullChargingCycles expected number of full charging cycles
#'
#'@export
setClass(
  "Bat",
  slots = list(
    maxE_el = "numeric" ,
    #minE_el = "numeric" ,
    maxP_el_minus = "numeric" ,
    maxP_el_plus = "numeric" ,
    effMaxP_el_minus = "numeric" ,
    eff_losses = "numeric" ,
    percentageEnd = "numeric" ,
    percentageStart = "numeric" ,
    EnergyStart = "numeric",
    EnergyEnd = "numeric",
    lifeTime = "numeric",
    fullChargingCycles = "numeric"
  ),
  contains = "Root",
  validity = check.Bat
)
#----constructor-------------------------------------------
#'new.Bat
#'
#'Constructor of Bat creates a new obejct of class Bat
#'
#'@return New object of class Bat
#'@export
new.Bat <- function() {
  return(new(Class = "Bat", variables= c("P_el_plus", "P_el_minus", "E_el")
  ))
}
