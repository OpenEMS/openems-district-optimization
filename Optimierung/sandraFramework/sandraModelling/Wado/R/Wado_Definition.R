#---validity function of Wado-------------------------------
check.Wado<-function(object){
  errors<-character();

  if (length(object@maxP_el_plus) == 1 &  length(object@minP_el_plus) == 1) {
    if (object@maxP_el_plus < object@minP_el_plus & object@maxP_el_plus != 1) {
      msg <- "maxP_el_plus has to bigger than minP_el_plus or maxP_el_plus has to be -1 (unlimited)"
      errors <- c(msg, errors)
    }
  }

  if (length(object@maxP_fuel_plus) == 1 &  length(object@minP_fuel_plus) == 1) {
    if (object@maxP_fuel_plus < object@minP_fuel_plus & object@maxP_fuel_plus != 1) {
      msg <- "maxP_fuel_plus has to bigger than minP_fuel_plus or maxP_fuel_plus has to be -1 (unlimited)"
      errors <- c(msg, errors)
    }
  }

  if (length(object@maxP_th_plus) == 1 &  length(object@minP_th_plus) == 1) {
    if (object@maxP_th_plus < object@minP_th_plus & object@maxP_th_plus != 1) {
      msg <- "maxP_th_plus has to bigger than minP_th_plus or maxP_th_plus has to be -1 (unlimited)"
      errors <- c(msg, errors)
    }
  }

  if(any(variables(object)=="P_el_plus") | any(variables(object)=="P_el_minus")){
   if(!all(c("P_el_plus","P_el_minus") %in% variables(object))){
     msg <-"Wado show have P_el_plus AND P_el_minus"
     errors<-c(msg,errors)}
  }


  if(any(variables(object)=="P_fuel_plus") | any(variables(object)=="P_fuel_minus")){
    if(!all(c("P_fuel_plus","P_fuel_minus") %in% variables(object))){
      msg <-"Wado show have P_fuel_plus AND P_fuel_minus"
      errors<-c(msg,errors)}
  }

  if(any(variables(object)=="P_th_plus") | any(variables(object)=="P_th_minus")){
    if(!all(c("P_th_plus","P_th_minus") %in% variables(object))){
      msg <-"Wado show have P_th_plus AND P_th_minus"
      errors<-c(msg,errors)}
  }
  if(length(errors)==0) {
    return(TRUE)
  } else{
    return(errors)
  }
}
#---Definition of Wado--------------------------------------
#'The class Wado
#'
#'Wado describes a watchdog
#'
#'@export
#'@slot maxP_el_minus The maximal electric power input in kW
#'@slot maxP_fuel_minus The maximal fuel power input in kW
#'@slot maxP_th_minus The maximal thermal power input in kW
#'@slot maxP_el_plus The maximal electric power output in kW
#'@slot maxP_fuel_plus The maximal fuel power output in kW
#'@slot maxP_th_plus The maximal thermal power output in kW
#'@slot minP_el_minus The minimal electric power input in kW
#'@slot minP_fuel_minus The minimal fuel power input in kW
#'@slot minP_th_minus The minimal thermal power input in kW
#'@slot minP_el_plus The minimal electric power output in kW
#'@slot minP_fuel_plus The minimal fuel power output in kW
#'@slot minP_th_plus The minimal thermal power output in kW
#'@slot ts_virtual query if there is a virtual TS representing a system of several units with different input temperatur (TRUE or FALSE)

setClass("Wado",
         slots = list(
           maxP_el_minus = "numeric",
           maxP_fuel_minus = "numeric",
           maxP_th_minus = "numeric",
           maxP_el_plus = "numeric",
           maxP_fuel_plus = "numeric",
           maxP_th_plus = "numeric",
           minP_el_minus = "numeric",
           minP_fuel_minus = "numeric",
           minP_th_minus = "numeric",
           minP_el_plus = "numeric",
           minP_fuel_plus = "numeric",
           minP_th_plus = "numeric",
           ts_virtual = "logical"

         ),
         contains = "Root",
         validity = check.Wado
)
#---constructor--------------------------------------------
#'new.Wado
#'
#'creat a new obejct of class Wado

#'@export
#'@return New object of class Wado
new.Wado <- function() {
  return(new(Class = "Wado", variables = c("Op"),
             maxP_el_minus = (10)^9,
             maxP_fuel_minus = (10)^9,
             maxP_th_minus = (10)^9,
             maxP_el_plus = (10)^9,
             maxP_fuel_plus = (10)^9,
             maxP_th_plus = (10)^9,
             minP_el_minus = 0,
             minP_fuel_minus = 0,
             minP_th_minus = 0,
             minP_el_plus = 0,
             minP_fuel_plus = 0,
             minP_th_plus = 0,
             ts_virtual  = F))
}
