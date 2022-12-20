#----Overview----------------------------------------------
# SourceTemp1
# SourceTemp2
# SourceTemp_15min
# COP1
# COP2
# maxP_th_plus
#----------------------------------------------------------
#----SourceTemp1-----------------------------------------
#'SourceTemp1 (set)
#'
#'Set the SourceTemp1 of class HePu in °C
#'
#'@importFrom Root SourceTemp1<-
#'@param object A HePu
#'@param value A numeric
#'@return HePu
#'@examples
#'hepu <- new.HePu()
#'SourceTemp1(hepu) <- 50
#'@export
setMethod("SourceTemp1<-", signature(object = "HePu", value="numeric"), function(object, value) {

  errors<-character();
  if (length(value) != 1) {
    msg <- "SourceTemp1 has to be of length one"
    errors <- c(msg, errors)
  }

  if(any(value < -40 | value > 60)) {
    msg <- "SourceTemp1 has to be between -40 and 60"
    errors <- c(msg, errors)
  }


  if (length(errors)==0) {
    object@SourceTemp1 <- value
    if (length(SourceTemp2(object))*
        length(COP1(object))*
        length(COP2(object))>0){
      object@CurvePitch <- (COP1(object) - COP2(object))/(value - SourceTemp2(object))
      object@CurveOffset <- COP2(object) - CurvePitch(object)*SourceTemp2(object)
    }
    validObject(object)
  }else{
    print(errors)
  }
  return(object)
})
#----SourceTemp2-----------------------------------------
#'SourceTemp2 (set)
#'
#'Set the SourceTemp2 of class HePu in °C
#'
#'@importFrom Root SourceTemp2<-
#'@param object A HePu
#'@param value A numeric
#'@return HePu
#'@examples
#'hepu <- new.HePu()
#'SourceTemp2(hepu)<- 40
#'@export
setMethod("SourceTemp2<-", signature(object = "HePu", value="numeric"), function(object, value) {

  errors<-character();
  if (length(value) != 1) {
    msg <- "SourceTemp2 has to be of length one"
    errors <- c(msg, errors)
  }

  if(any(value < -40 | value > 60)) {
    msg <- "SourceTemp2 has to be between -40 and 60"
    errors <- c(msg, errors)
  }


  if (length(errors)==0) {
    object@SourceTemp2 <- value
    if (length(SourceTemp1(object))*
        length(COP1(object))*
        length(COP2(object))>0){
      object@CurvePitch <- (COP1(object) - COP2(object))/(SourceTemp1(object) - value)
      object@CurveOffset <- COP2(object) - CurvePitch(object)*value
    }
    validObject(object)
  }else{
    print(errors)
  }
  return(object)
})
#----SourceTemp_15min-----------------------------------------
#'SourceTemp_15min (set)
#'
#'Set the SourceTemp_15min of class HePu in °C
#'
#'@importFrom Root SourceTemp_15min<-
#'@param object A HePu
#'@param value A numeric
#'@return HePu
#'@examples
#'hepu <- new.HePu()
#'SourceTemp_15(hepu) <- 55
#'@export
setMethod("SourceTemp_15min<-", signature(object = "HePu", value="numeric"), function(object, value) {

  errors<-character()

  if(any(value < -40 | value > 60)) {
    msg <- "All values of SourceTemp_15min have to be between -40 and 60"
    errors <- c(msg, errors)
  }

  if (length(errors)==0) {
    object@SourceTemp_15min <- value
    validObject(object)
  }else{
    print(errors)
  }
  return(object)
})



#----SourceTemp_abstract-----------------------------------------
#'SourceTemp_abstract (set)
#'
#'Set the SourceTemp_abstract of class HePu in °C
#'
#'@importFrom Root SourceTemp_abstract<-
#'@param object A HePu
#'@param value A numeric
#'@return HePu
#'@examples
#'hepu <- new.HePu()
#'SourceTemp_abstract(hepu) <- 55
#'@export
setMethod("SourceTemp_abstract<-", signature(object = "HePu", value="numeric"), function(object, value) {

  errors<-character()

  if(any(value < -40 | value > 60)) {
    msg <- "All values of SourceTemp_abstract have to be between -40 and 60"
    errors <- c(msg, errors)
  }

  if (length(errors)==0) {
    object@SourceTemp_abstract <- value
    validObject(object)
  }else{
    print(errors)
  }
  return(object)
})

#----COP1--------------------------------------------------
#'COP1 (set)
#'
#'Set the COP1 of class HePu
#'
#'@importFrom Root COP1<-
#'@param object A HePu
#'@param value A numeric
#'@return HePu
#'@examples
#'hepu <- new.HePu()
#'COP1(hepu) <- 4.4
#'@export
setMethod("COP1<-", signature(object = "HePu", value="numeric"), function(object, value) {

  errors<-character();
  if (length(value) != 1) {
    msg <- "COP1 has to be of length one"
    errors <- c(msg, errors)
  }

  if(value < 0) {
    msg <- "COP1 has to be positive"
    errors <- c(msg, errors)
  }


  if (length(errors)==0) {
    object@COP1 <- value
    if (length(SourceTemp1(object))*
        length(SourceTemp2(object))*
        length(COP2(object))>0){
      object@CurvePitch <- (value - COP2(object))/(SourceTemp1(object) - SourceTemp2(object))
      object@CurveOffset <- COP2(object) - CurvePitch(object)*SourceTemp2(object)
    }
    validObject(object)
  }else{
    print(errors)
  }
  return(object)
})
#----COP2--------------------------------------------------
#'COP2 (set)
#'
#'Set the COP2 of class HePu
#'
#'@importFrom Root COP2<-
#'@param object A HePu
#'@param value A numeric
#'@return HePu
#'@examples
#'hepu <- new.HePu()
#'COP2(hepu) <- 4.5
#'@export
setMethod("COP2<-", signature(object = "HePu", value="numeric"), function(object, value) {

  errors<-character();
  if (length(value) != 1) {
    msg <- "COP2 has to be of length one"
    errors <- c(msg, errors)
  }

  if(value < 0) {
    msg <- "COP2 has to be positive"
    errors <- c(msg, errors)
  }


  if (length(errors)==0) {
    object@COP2 <- value
    if (length(SourceTemp1(object))*
        length(SourceTemp2(object))*
        length(COP1(object))>0){
      object@CurvePitch <- (COP1(object) - value)/(SourceTemp1(object) - SourceTemp2(object))
      object@CurveOffset <- COP2(object) - CurvePitch(object)*SourceTemp2(object)
    }
    validObject(object)
  }else{
    print(errors)
  }
  return(object)
})
#----maxP_th_plus------------------------------------------
#maxP_th_plus (set)
#'
#'Set the maxP_th_plus of class HePu in kW. If the maxP_th_plus is set to -1, the HePu can produce thermal energy unlimited.
#'
#'@importFrom Root maxP_th_plus<-
#'@param object A HePu
#'@param value A numeric
#'@return HePu
#'@examples
#'hepu <- new.HePu()
#'maxP_th_plus(hepu) <- 10
#'@export

setMethod("maxP_th_plus<-", signature(object = "HePu",value="numeric"), function(object, value) {

  errors<-character();
  if (length(value) != 1) {
    msg <- "maxP_th_plus has to be of length one"
    errors <- c(msg, errors)
  }

  if (value < 0 & value != -1) {
    msg <- "maxP_th_plus has to be positiv or -1"
    errors <- c(msg, errors)
  }


  if (length(errors)==0) {
    object@maxP_th_plus <- value
  }else{
    print(errors)

  }
  return(object)
})
