#----Overview----------------------------------------------
# SourceTemp1
# SourceTemp2
# SourceTemp_15min
# COP1
# COP2
# CurveOffset
# CurvePitch
# maxP_th_plus
#----------------------------------------------------------
#----SourceTemp1----------------------------------------------
#'SourceTemp1 (get)
#'
#'Get SourceTemp1 of HePu in °C. It is the source temperature at operation point 1
#'in which the COP of the heat pump is COP1
#'
#'@importFrom Root SourceTemp1
#'@param object A HePu
#'@return  SourceTemp1 of HePu
#'@export
#'
setMethod("SourceTemp1", signature(object = "HePu"), function(object) {
  return(object@SourceTemp1)
})
#----SourceTemp2----------------------------------------------
#'SourceTemp2 (get)
#'
#'Get SourceTemp2 of HePu in °C. It is the source temperature at operation point 2
#'in which the COP of the heat pump is COP2
#'
#'@importFrom Root SourceTemp2
#'@param object A HePu
#'@return  SourceTemp2 of HePu
#'@export
#'
setMethod("SourceTemp2", signature(object = "HePu"), function(object) {
  return(object@SourceTemp2)
})
#----SourceTemp_15min-----------------------------------------
#'SourceTemp_15min (get)
#'
#'Get SourceTemp_15min of HePu in °C. It is the profile for the source temperature
#'in a 15 min timestep
#'
#'@importFrom Root SourceTemp_15min
#'@param object A HePu
#'@return  SourceTemp_15min of HePu
#'@export
#'
setMethod("SourceTemp_15min", signature(object = "HePu"), function(object) {
  return(object@SourceTemp_15min)
})

#----SourceTemp_abstract-----------------------------------------
#'SourceTemp_abstract (get)
#'
#'Get SourceTemp_abstract of HePu in °C. It is the profile for the source temperature
#'in a 15 min timestep
#'
#'@importFrom Root SourceTemp_abstract
#'@param object A HePu
#'@return  SourceTemp_abstract of HePu
#'@export
#'
setMethod("SourceTemp_abstract", signature(object = "HePu"), function(object) {
  return(object@SourceTemp_abstract)
})


#----COP1--------------------------------------------------
#'COP1 (get)
#'
#'Get COP1 of HePu. It is the coeffitient of performance at operation point 1
#'in which the source has the temperature of SourceTemp1
#'
#'@importFrom Root COP1
#'@param object A HePu
#'@return  COP1 of HePu
#'@export
#'
setMethod("COP1", signature(object = "HePu"), function(object) {
  return(object@COP1)
})
#----COP2--------------------------------------------------
#'COP2 (get)
#'
#'Get COP2 of HePu. It is the coeffitient of performance at operation point 2
#'in which the source has the temperature of SourceTemp2
#'
#'@importFrom Root COP2
#'@param object A HePu
#'@return  COP2 of HePu
#'@export
#'
setMethod("COP2", signature(object = "HePu"), function(object) {
  return(object@COP2)
})
#----CurveOffset-------------------------------------------
#'CurveOffset (get)
#'
#'Get CurveOffset of HePu. It is the offset of the curve describing the change
#'of COP between operation point 1 and 2
#'
#'@importFrom Root CurveOffset
#'@param object A HePu
#'@return  CurveOffset of HePu
#'@export
#'
setMethod("CurveOffset", signature(object = "HePu"), function(object) {
  return(object@CurveOffset)
})
#----CurvePitch--------------------------------------------
#'CurvePitch (get)
#'
#'Get CurvePitch of HePu. It is the pitch of the curve describing the change
#'of COP between operation point 1 and 2
#'
#'@importFrom Root CurvePitch
#'@param object A HePu
#'@return  CurvePitch of HePu
#'@export
#'
setMethod("CurvePitch", signature(object = "HePu"), function(object) {
  return(object@CurvePitch)
})
#----maxP_th_plus------------------------------------------
#'maxP_th_plus (get)
#'
#'Get maxP_th_plus of HePu in kW
#'
#'@importFrom Root maxP_th_plus
#'@param object A HePu
#'@return  maxP_th_plus of HePu
#'@export
#'
setMethod("maxP_th_plus", signature(object = "HePu"), function(object) {
  return(object@maxP_th_plus)
})
