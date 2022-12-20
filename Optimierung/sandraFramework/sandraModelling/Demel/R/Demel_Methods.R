#----Overview----------------------------------------------
# load_el
#
#----------------------------------------------------------
#----load_el---------------------------------------------
#'load_el
#'
#'Get the electrical load on timegrid in kW. The timeseries on the 15 minute
#'timegrid is brougth to the one minute timegrid by considering minute values
#'constant on a 15 minute block. Then the timeseries is brought to timegrid by
#'taking means
#'
#'@importFrom Root load_el
#'@export
#'@param object An Demel
#'@return numeric. A timeseries on the timegrid defined by \code{object}
#'@examples
#'a<-new.Demel()
#'#if no load is specified you get zero!
#'load_el(a)
#'timegrid(a)<-c(5, 10, 15, 30)
#'load_15min_el(a) <-c(10, 4.2, 6, 5)
#'load_el(a)

setMethod("load_el", signature(object = "Demel"), function(object) {

  if(length(load_15min_el(object))!=0){
    if(length(object@timegrid)==0){
      return(0)
    }

  #if slots are empty it serves as default
  #bring 15-minute sharp timeseries to minute-grid
  minload_el <-
    c(t(matrix(
      load_15min_el(object), length(load_15min_el(object)), 15
    )))
  #cut timeseries to simulation time defined by timegrid(object)
  minload_el <- minload_el[1:sum(timegrid(object))]
  #now projecting to timegrid(object)
  l <- list(length(timegrid(object)))
  for (i in 1:length(timegrid(object))) {
    l[[i]] <- t(rep(timegrid(object)[i] ^ {
      -1
    }, timegrid(object)[i]))
  }
  P <- Matrix::bdiag(l)
  return(c(as.matrix(P %*% minload_el)))

  }else{

    if(length(load_abstract_el(object))==0){
      return(0)
    }else{
    return(load_abstract_el(object))
    }
  }
})
