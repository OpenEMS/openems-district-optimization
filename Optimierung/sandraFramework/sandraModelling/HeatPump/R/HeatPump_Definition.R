#----the validity function of HeatPump-------------------------
check.HeatPump <- function(object) {
  errors <- character()

if(length(object@timegrid)!=0 & length(object@COP)!=0 &length(object@COP)!=1 ){
  if(length(object@timegrid)!=length(object@COP) ){
    msg <- "time grid has to be of the same length as COP (if COP is not a single value)"
    errors <- c(errors,msg)
  }

}

  if (length(errors) == 0) {
    return(TRUE)
  }else{

    return(errors)
  }
}
#----Definition of Class HeatPump------------------------------
#'Class HeatPump
#'
#'The HeatPump represents the class for a heat pump running with air
#'
#'@slot maxP_th_plus the maximal output of thermal power in kW
#'@slot nominalP_th_plus the nominal output of thermal power in kW (capacity)
#'@slot COP is the coeffitent of performance (COP) 
#'@slot COP_nominal is the nominal coeffitent of performance (COP) between electrical power and thermal provided at operation point 2
#'@slot maxP_el_minus the maximal input of electric power in kW
#'@export
setClass(
  "HeatPump",
  slots = list(
    maxP_th_plus = "numeric" ,
    nominalP_th_plus = "numeric" ,
    COP = "numeric" ,
    COP_nominal = "numeric" ,
    maxP_el_minus = "numeric" 
  ),
  contains = "Root",
  validity = check.HeatPump
)
#----constructor-------------------------------------------
#'new.HeatPump
#'
#'Constructor of HeatPump creates a new obejct of class HeatPump
#'
#'@return New object of class HeatPump
#'@export
new.HeatPump <- function() {
  return(new(Class = "HeatPump", variables= c("P_th_plus", "P_el_minus")  ))
}
