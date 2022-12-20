#----the validity function of HeSw-------------------------
check.HePu <- function(object) {
  errors <- character()

if(length(object@timegrid)!=0 & length(object@SourceTemp_abstract)!=0 ){
  if(length(object@timegrid)!=length(object@SourceTemp_abstract) ){
    msg <- "time grid has to be of the same length as SourceTemp_abstract"
    errors <- c(errors,msg)
  }

}

  if(length(object@SourceTemp_15min)!=0 & length(object@SourceTemp_abstract)!=0 ){

      msg <- "SourceTemp_15min and SourceTemp_abstract may not both be set"
      errors <- c(errors,msg)


  }
  if (length(errors) == 0) {
    return(TRUE)
  }else{

    return(errors)
  }
}
#----Definition of Class HePu------------------------------
#'Class HePu
#'
#'The HePu represents the class for a heat pump running with air
#'
#'@slot maxP_th_plus the maximal output of thermal power in kW
#'@slot COP1 is the coeffitent of performance (COP) between electrical power and thermal provided at operation point 1
#'@slot COP2 is the coeffitent of performance (COP) between electrical power and thermal provided at operation point 2
#'@slot SourceTemp1 is the source temperature at operation point 1 with the COP 1
#'@slot SourceTemp2 is the source temperature at operation point 2 with the COP 2
#'@slot CurvePitch is the pitch of the curve describing the change of COP between operation point 1 and 2
#'@slot CurveOffset is the offset of the curve describing the change of COP between operation point 1 and 2
#'@slot SourceTemp_15min is the profile for the source temperature at a 15 min timestep
#'@slot SourceTemp_abstract is the profile for the source temperature
#'@export
setClass(
  "HePu",
  slots = list(
    maxP_th_plus = "numeric" ,
    COP1 = "numeric" ,
    COP2 = "numeric" ,
    SourceTemp1 = "numeric" ,
    SourceTemp2 = "numeric" ,
    CurvePitch = "numeric",
    CurveOffset = "numeric",
    SourceTemp_15min = "numeric",
    SourceTemp_abstract = "numeric"
  ),
  contains = "Root",
  validity = check.HePu
)
#----constructor-------------------------------------------
#'new.HePu
#'
#'Constructor of HePu creates a new obejct of class HePu
#'
#'@return New object of class HePu
#'@export
new.HePu <- function() {
  return(new(Class = "HePu", variables= c("P_th_plus", "P_el_minus") #, "P_th_minus") #Auskommentieren, wenn Umweltwärme mitbetrachtete werden soll!
  ))
}
