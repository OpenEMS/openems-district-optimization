#----Overview----------------------------------------------
# load_th
#
#----------------------------------------------------------
#----load_th-----------------------------------------------
#'load_th
#'
#'Get the thermal load on timegrid in kW. The timeseries on the 15 minute
#'timegrid is brougth to the one minute timegrid by considering minute values
#'constant on a 15 minute block. Then the timeseries is brought to timegrid by
#'taking means
#'
#'@importFrom Root load_th
#'@export
#'@param object An Demth
#'@return numeric. A timeseries on the timegrid defined by \code{object}
#'@examples
#'a<-new.Demth()
#'#if no load is specified you get zero!
#'load_th(a)
#'timegrid(a)<-c(5, 10, 30, 15)
#'load_15min_th(a) <-c(10, 4.2, 6, 5)
#'load_th(a)

setMethod("load_th", signature(object = "Demth"), function(object) {

  if(length(load_15min_th(object))!=0){
    if(length(object@timegrid)==0){
      return(0)
    }

    #if slots are empty it serves as default
    #bring 15-minute sharp timeseries to minute-grid
    minload_el <-
      c(t(matrix(
        load_15min_th(object), length(load_15min_th(object)), 15
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

    if(length(load_abstract_th(object))==0){
      return(0)
    }else{
      return(load_abstract_th(object))
    }
  }
})
