#----Overview----------------------------------------------
# COP
# maxP_el_minus
# maxP_th_plus
# nominal_P_th_plus
# COP_nominal
# 
#----------------------------------------------------------

#----COP--------------------------------------------------
#'COP (get)
#'
#'Get COP of HeatPump. It is the coefficient of performance.
#'
#'@importFrom Root COP
#'@param object A HeatPump
#'@return  COP of HeatPump
#'@export
#'

setMethod("COP", signature(object = "HeatPump"), function(object) {
  return(object@COP)
})
#----COP_nominal--------------------------------------------------
#'COP_nominal (get)
#'
#'Get COP_nominal of HeatPump. It is the nominal coefficient of performance 
#'
#'@importFrom Root COP_nominal
#'@param object A HeatPump
#'@return  COP_nominal of HeatPump
#'@export
#'
setMethod("COP_nominal", signature(object = "HeatPump"), function(object) {
  return(object@COP_nominal)
})

#----maxP_th_plus------------------------------------------
#'maxP_th_plus (get)
#'
#'Get maxP_th_plus of HeatPump in kW
#'
#'@importFrom Root maxP_th_plus
#'@param object A HeatPump
#'@return  maxP_th_plus of HeatPump
#'@export
#'
setMethod("maxP_th_plus", signature(object = "HeatPump"), function(object) {
  return(object@maxP_th_plus)
})


#----nominalP_th_plus------------------------------------------
#'nominalP_th_plus (get)
#'
#'Get nominalP_th_plus of HeatPump in kW
#'
#'@importFrom Root nominalP_th_plus
#'@param object A HeatPump
#'@return  nominalP_th_plus of HeatPump
#'@export
#'
setMethod("nominalP_th_plus", signature(object = "HeatPump"), function(object) {
  return(object@nominalP_th_plus)
})


#----maxP_el_minus------------------------------------------
#'maxP_el_minus (get)
#'
#'Get maxP_el_minus of HeatPump in kW
#'
#'@importFrom Root maxP_el_minus
#'@param object A HeatPump
#'@return  maxP_el_minus of HeatPump
#'@export
#'
setMethod("maxP_el_minus", signature(object = "HeatPump"), function(object) {
  return(object@maxP_el_minus)
})

