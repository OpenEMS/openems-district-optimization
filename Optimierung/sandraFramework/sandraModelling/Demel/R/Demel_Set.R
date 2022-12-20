#----load_15min_el-----------------------------------------
#'load_15min_el (set)
#'
#'Set the load_15min_el of class Demel in kW per 15 min
#'
#'@importFrom Root load_15min_el<-
#'@param object A Demel
#'@param value A numeric
#'@return Demel
#'@examples
#'demel <- new.Demel()
#'load_15min_el(demel) <- c(10, 4.2, 5, 6)
#'@export

setMethod("load_15min_el<-", signature(object = "Demel",value="numeric"), function(object, value) {

  errors<-character();
  if (!all(value>=0)) {
    msg <- "A demand always has to be positive or zero"
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
#'Set the load_abstract_el of class Demel in kW per 15 min
#'
#'@importFrom Root load_abstract_el<-
#'@param object A Demel
#'@param value A numeric
#'@return Demel
#'@examples
#'demel <- new.Demel()
#'load_abstract_el(demel) <- c(10, 4.2, 5, 6)
#'@export

setMethod("load_abstract_el<-", signature(object = "Demel",value="numeric"), function(object, value) {

  errors<-character();
  if (!all(value>=0)) {
    msg <- "A demand always has to be positive or zero"
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
