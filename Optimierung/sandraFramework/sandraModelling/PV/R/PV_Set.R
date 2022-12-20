#----load_15min_el-----------------------------------------
#'load_15min_el (set)
#'
#'Set the load_15min_el of class PV in kW per 15 min
#'
#'@importFrom Root load_15min_el<-
#'@param object A PV
#'@param value A numeric
#'@return PV
#'@export

setMethod("load_15min_el<-", signature(object = "PV",value="numeric"), function(object, value) {

  errors<-character();
  if (!all(value>=0)) {
    msg <- "A production always has to be positive or zero"
    errors <- c(msg, errors)
  }


  if (length(errors)==0) {
    object@load_15min_el <- value
    validObject(object)
  }else{
    print(errors)

  }
  return(object)
})




#----load_abstract_el-----------------------------------------
#'load_abstract_el (set)
#'
#'Set the load_abstract_el of class PV in kW per 15 min
#'
#'@importFrom Root load_abstract_el<-
#'@param object A PV
#'@param value A numeric
#'@return PV
#'@export

setMethod("load_abstract_el<-", signature(object = "PV",value="numeric"), function(object, value) {

  errors<-character();
  if (!all(value>=0)) {
    msg <- "A production always has to be positive or zero"
    errors <- c(msg, errors)
  }


  if (length(errors)==0) {
    object@load_abstract_el <- value
    validObject(object)
  }else{
    print(errors)

  }
  return(object)
})

