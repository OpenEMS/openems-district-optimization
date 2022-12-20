# ---------------------------------------------------------
# the validity function of LM -----------------------------
# ---------------------------------------------------------
check.LM <- function(object) {
  errors <- character()
  if (length(object@direction) > 0) {
    if (!all(object@direction == "=" |
             object@direction == "<=" | object@direction == ">=")) {
      message <- "Only = ,<=, >= are allowed for direction"
      errors <- c(errors, message)
    }
  }
  if (length(object@direction) * length(object@vector) > 0 &
      length(object@direction) != length(object@vector)) {
    msg <- "vector and direction have to be of the same length"
    errors <- c(errors, msg)
  }
  if (dim(object@matrix)[1] * length(object@vector) > 0 &
      dim(object@matrix)[1] != length(object@vector)) {
    msg <- "length of vector has to equal the number of rows of matrix"
    errors <- c(errors, msg)
  }
  if (dim(object@matrix)[1] * length(object@direction) > 0 &
      dim(object@matrix)[1] != length(object@direction)) {
    msg <- "length of direction has to equal the number of rows of matrix"
    errors <- c(errors, msg)
  }
  if (dim(object@matrix)[2] * length(object@cost) > 0 &
      dim(object@matrix)[2] != length(object@cost)) {
    msg <- "length of cost has to equal the number of columns of matrix"
    errors <- c(errors, msg)
  }
  if (length(object@cost) > 0 & length(object@binary) > 0) {
    if (length(object@cost) != length(object@cost)) {
      msg <- "length of cost hast to equal length binary"
      errors <- c(errors, msg)
    }
  }
  if (length(errors) == 0) {
    return(TRUE)
  }
  else{
    return(errors)
  }
}
# ---------------------------------------------------------
# ---------------------------------------------------------
#' Class LM
#'
#'LM describes formally a system of linear inequalities together with a cost functional and binary variables
#'
#'@slot matrix The matrix of coefficients (a_ij)_{i=1,...,m  j=1,....n} in front of the independent variables x_1,....,x_n:
#' describing the system of inequalities: Each row an inequality,
#'  each column a variable
#'@slot vector A numeric consisting of constants used on the right hand side of the inequalities
#'@slot direction A charcater with the inequality signs for each row (inequality)
#'@slot cost A numeric with the costs for each variable x_i
#'@slot binary A logical: TRUE variable in the cost functional is binary, FALSE the variable is continuous
#'
#'@examples #We model the system:
#' --------------------------------------------------------
#' #x_1 + 2*x_2 = 3
#' #4*x_1 <= 5
#' #x_2 - 6*x_3 <= 0
#' #x_1 costs 7, x_2 costs 8, x_3 costs 9
#' #x_1 ,x_2 continuous, x_3 binary
#' --------------------------------------------------------
#' #The matrix of coefficients
#' a<-cbind(c(1,4,0),c(2,0,1),c(0,0,-6));
#' #The direction
#' dir<-c("=","<=","<=");
#' #The vector of constants on the rigth hand side
#' v<-c(3,5,0);
#' #The cost vector is
#' co<-c(7,8,9);
#' #The binary vector is
#' b<-c(F,F,T);
#' new("LM",matrix=A,direction=dir,vector=v,cost=co,binary=b)
#'@export
setClass(
  "LM",
  representation(
    #matrix of coefficients of the inequality system
    #each row a new inequality
    matrix = "matrix",
    #for each row the corresponding inequality sign
    direction = "character",
    #the constants on the rigth hand side of the inequality
    vector = "numeric",
    #the vector with variable names
    cost = "numeric",
    #position of the binary variables
    binary = "logical"
  ),
  validity = check.LM
)
# ---------------------------------------------------------
# constructor ---------------------------------------------
# ---------------------------------------------------------
#' new.LM
#'
#'Constructor of LM. Creat a new obejct of class LM
#'
#'@return New object of class LM
#'@export
new.LM <- function() {
  return(new(Class = "LM"))
}
