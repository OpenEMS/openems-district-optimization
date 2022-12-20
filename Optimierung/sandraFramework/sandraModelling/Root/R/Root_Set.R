#---Overview-----------------------------------------------
# info
# name
# timegrid
# variables
#----------------------------------------------------------


#---info (set)---------------------------------------------
#'info (set)
#'
#'Set info of Root
#'
#'@export
#'@param object A Root
#'@param value A list.
#'@examples
#'root <- new.Root()
#'info(root) <- list(name= "gewuenschter Name")
#'@return object The new object of Root with set info

setMethod("info<-", signature(object = "Root", value = "list"), function(object, value) {
  object@info<-value
  return(object)

})

#---name (set)---------------------------------------------
#'name (set)
#'
#'Set name of Root
#'
#'@export
#'@param object A Root
#'@param value A character. Note the name of a Root has to be unique.
#'@return object The new object of Root with set name
#'@examples
#'root <- new.Root()
#'name(root) <- "Rooty"
setMethod("name<-", signature(object = "Root", value = "character"), function(object, value) {
  errors<-character()
  if (length(value) != 1) {
    msg <- "name has to be of length one"
    errors <- c(msg, errors)
  }
  if (length(errors)==0) {
    object@name <- value
    # if (length(variables(object)) * length(timegrid(object)) > 0) {
    #   object@coord <- coordi(object)
    # }

  } else{
    print(errors)
  }
  return(object)
})

#---timegrid (set)---------------------------------------------
#'timegrid (set)
#'
#'Set timegrid of Root
#'
#'@export
#'@param object A Root
#'@param value A numeric. Vector containing the time timegrid to be considered, each component a time block in minutes
#'@return object The new object of Root with set timegrid.
#'@examples
#'root <- new.Root()
#'timegrid(root) <- rep(15,5)
setMethod("timegrid<-", signature(object = "Root", value = "numeric"), function(object, value) {
  errors<-character();
  if (any((value %% 1) != 0)) {
    msg <- "timegrid has to be integer"
    errors <- c(errors, msg)
  }
  if (!all(value>=0)) {
    msg <- "every entry of timegrid has to be positive"
    errors <- c(msg, errors)
  }
  if (length(errors)==0) {
    object@timegrid <- value
    # if (length(variables(object)) * length(name(object)) > 0) {
    #   object@coord <- coordi(object)
    # }
  } else{
    print(errors)
  }
  return(object)
})
#---variables (set)----------------------------------------
#'variables (set)
#'
#'Set variables of Root
#'
#'@export
#'@param object A Root
#'@param value A character or list of characters see details
#'@details
#'For variables only subsets of the list are allowed:
#'\itemize{
#'\item "Op": on/off
#'\item "P_el_plus": electric production
#'\item "P_el_minus": electric consumption
#'\item "P_th_plus": thermal production
#'\item "P_th_minus": thermal consumption
#'\item "P_fuel_plus": fuel production
#'\item "P_fuel_minus": fuel consumption
#'\item "E_el": electric energy
#'\item "E_th": thermal energy
#'\item "E_fuel": fuel energy
#'}
#'Note: If variables should be deleted use \code{variable(object)<-character()}
#'@return object The new object of Root with set variables
#'@examples
#'root <- new.Root()
#'variables(root) <- c("P_el_plus", "Op")
setMethod("variables<-", signature(object = "Root", value = "character"), function(object, value) {

  if(length(value)==0)
  {object@variables<-character();
  }else{
    errors<-character();

    if (length(errors)==0) {
      #add new variables (if variables(object)<-character(), the standard variables of the class set see new.xy in xy_Definition.R)
      object@variables <- c(variables(object),value)

      if(validObject(object)==TRUE){
        # if (length(timegrid(object)) * length(name(object)) > 0) {
        #   object@coord <- coordi(object)
        # }
      }else{print(errors)}

    }else{
      print(errors)
    }
  }
  return(object)
})


