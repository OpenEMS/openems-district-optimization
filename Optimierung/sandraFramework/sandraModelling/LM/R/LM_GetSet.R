# ---------------------------------------------------------
# getter --------------------------------------------------
# ---------------------------------------------------------
#' LM.matrix (get)
#'
#'Get the matrix of coefficients
#'
#'@param object A LM
#'@return The matrix of coefficients of a LM
#'@export
setGeneric("LM.matrix", valueClass = "matrix", function(object) {
  standardGeneric("LM.matrix")
})
setMethod("LM.matrix", signature(object = "LM"), function(object) {
  return(object@matrix)
})
# ---------------------------------------------------------
#' LM.vector (get)
#'
#'Get the vector of constants
#'
#'@param object A LM
#'@return The vector of constants of a LM
#'@export
setGeneric("LM.vector", valueClass = "numeric", function(object) {
  standardGeneric("LM.vector")
})
setMethod("LM.vector", signature(object = "LM"), function(object) {
  return(object@vector)
})
# ---------------------------------------------------------
#' LM.direction (get)
#'
#'Get the enequality signs
#'
#'@param object A LM
#'@return The vector of inequality signs of a LM
#'@export
setGeneric("LM.direction", valueClass = "character", function(object) {
  standardGeneric("LM.direction")
})
setMethod("LM.direction", signature(object = "LM"), function(object) {
  return(object@direction)
})
# ---------------------------------------------------------
#' LM.cost (get)
#'
#'Get the costs
#'
#'@param object A LM
#'@return The vector of costs of a LM
#'@export
setGeneric("LM.cost", valueClass = "numeric", function(object) {
  standardGeneric("LM.cost")
})
setMethod("LM.cost", signature(object = "LM"), function(object) {
  return(object@cost)
})
# ---------------------------------------------------------
#' LM.binary (get)
#'
#'Get which variables are binary
#'
#'@param object A LM
#'@return A logical indicting which of the variables of the LM are binary
#'@export
setGeneric("LM.binary", valueClass = "logical", function(object) {
  standardGeneric("LM.binary")
})
setMethod("LM.binary", signature(object = "LM"), function(object) {
  return(object@binary)
})
# ---------------------------------------------------------
# the dimension method ------------------------------------
# ---------------------------------------------------------
#' LM.dim (get)
#'
#'Get the dimension
#'
#'@param object A LM
#'@return The dimension of the matrix of a LM
#'@export
setGeneric("LM.dim", valueClass = "numeric", function(object) {
  standardGeneric("LM.dim")
})
setMethod("LM.dim", signature(object = "LM"), function(object) {
  dim(object@matrix)
})
# ---------------------------------------------------------
# setter --------------------------------------------------
# ---------------------------------------------------------
#' LM.matrix (set)
#'
#'Set the matrix of constants
#'
#'@param object A LM
#'@param value A matrix
#'@return LM
#'@export
setGeneric("LM.matrix<-", function(object, value) {
  standardGeneric("LM.matrix<-")
})
setMethod("LM.matrix<-", signature(object = "LM"), function(object, value) {
  objectTest <- object
  object@matrix <- value
  if (validObject(object) == TRUE) {
    return(object)
  }
  else{
    return(objectTest)
  }
})
# ---------------------------------------------------------
#' LM.vector (set)
#'
#'Set the vector of constants
#'
#'@param object A LM
#'@param value A numeric
#'@return LM
#'@export
setGeneric("LM.vector<-", function(object, value) {
  standardGeneric("LM.vector<-")
})
setMethod("LM.vector<-", signature(object = "LM"), function(object, value) {
  objectTest <- object
  object@vector <- value
  if (validObject(object) == TRUE) {
    return(object)
  }
  else{
    return(objectTest)
  }
})
# ---------------------------------------------------------
#' LM.direction (set)
#'
#'Set the direction
#'
#'@param object A LM
#'@param value A character consisting of the signs "=","<=", ">="
#'@return LM
#'@export
setGeneric("LM.direction<-", function(object, value) {
  standardGeneric("LM.direction<-")
})
setMethod("LM.direction<-", signature(object = "LM"), function(object, value) {
  objectTest <- object
  object@direction <- value
  if (validObject(object) == TRUE) {
    return(object)
  }
  else{
    return(objectTest)
  }
})
# ---------------------------------------------------------
#' LM.cost (set)
#'
#'Set the costs
#'
#'@param object A LM
#'@param value A numeric
#'@return LM
#'@export
setGeneric("LM.cost<-", function(object, value) {
  standardGeneric("LM.cost<-")
})
setMethod("LM.cost<-", signature(object = "LM"), function(object, value) {
  objectTest <- object
  object@cost <- value
  if (validObject(object) == TRUE) {
    return(object)
  }
  else{
    return(objectTest)
  }
})
# ---------------------------------------------------------
#' LM.binary (set)
#'
#'Set the vector of constants
#'
#'@param object A LM
#'@param value A logical
#'@return LM
#'@export
setGeneric("LM.binary<-", function(object, value) {
  standardGeneric("LM.binary<-")
})
setMethod("LM.binary<-", signature(object = "LM"), function(object, value) {
  objectTest <- object
  object@binary <- value

  if (validObject(object) == TRUE) {
    return(object)
  }
  else{
    return(objectTest)
  }
})
# ---------------------------------------------------------
#' as.data.frame.LM
#'
#'LM as data.frame
#'
#'@param object A LM
#'@return data.frame
#'@export
setGeneric("as.data.frame.LM", function(object)
  standardGeneric("as.data.frame.LM"))
setMethod("as.data.frame.LM", "LM", function(object) {
  x<-data.frame(
    matrix = object@matrix,
    direction = object@direction,
    vector = object@vector

  )
  colnames(x)<-c(colnames(object@matrix),"direction","rhs");
  row.names(x)<-row.names(object@matrix);

  return(x)
})
