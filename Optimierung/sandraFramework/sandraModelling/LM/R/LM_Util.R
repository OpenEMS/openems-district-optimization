#'print method for LM
#'
#'@param object A LM
#'@export
#'
setGeneric("LM.print",function(object){
  standardGeneric("LM.print")
})
setMethod("LM.print",signature(object="LM"),function(object){
  print(cbind(object@matrix,object@direction,object@vector))

})

#'add method for LM
#'
#'@param object1 A LM
#'@param object2 A LM
#'@return LM
#'@export
#'@examples
#'object1 <- new("LM",matrix = t(c(1,2)),direction = "=",vector = 3,cost = c(4,5),binary = c(T,F))
#'object2 <- new("LM",matrix = t(c(1,2)),direction = "=",vector = 3,cost = c(4,5),binary = c(F,T))
#'LM.add(object1,object2)
#'
setGeneric("LM.add",function(object1,object2){
  standardGeneric("LM.add")
})
setMethod("LM.add",signature(object1="LM",object2 ="LM"),function(object1,object2){
  new("LM",matrix = rbind(object1@matrix,object2@matrix),
         vector = c(object1@vector,object2@vector),
         direction = c(object1@direction,object2@direction),
         cost = object1@cost + object2@cost,
         binary = object1@binary & object2@binary)

})
