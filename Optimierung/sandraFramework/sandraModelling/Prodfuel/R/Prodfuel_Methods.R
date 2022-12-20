#----Overview----------------------------------------------
# load_fuel
#
#----------------------------------------------------------
#----load_fuel---------------------------------------------
#'load_fuel
#'
#'Get the thermal load on timegrid in kW. The timeseries on the 15 minute
#'timegrid is brougth to the one minute timegrid by considering minute values
#'constant on a 15 minute block. Then the timeseries is brought to timegrid by
#'taking means
#'
#'@export
#'@importFrom Root load_fuel
#'@param object A Prodfuel
#'@return numeric. A timeseries on the timegrid defined by \code{object}
#'@examples
#'a<-new.Prodfuel()
#'#if no load is specified you get zero!
#'load_fuel(a)
#'timegrid(a)<-c(5, 10, 15, 30)
#'load_15min_fuel(a) <-c(10, 4.2, 6, 5)
#'load_fuel(a)

setMethod("load_fuel", signature(object = "Prodfuel"), function(object) {

  if(length(load_15min_fuel(object))!=0){
    if(length(object@timegrid)==0){
      return(0)
    }

    #if slots are empty it serves as default
    #bring 15-minute sharp timeseries to minute-grid
    minload_fuel <-
      c(t(matrix(
        load_15min_fuel(object), length(load_15min_fuel(object)), 15
      )))
    #cut timeseries to simulation time defined by timegrid(object)
    minload_fuel <- minload_fuel[1:sum(timegrid(object))]
    #now projecting to timegrid(object)
    l <- list(length(timegrid(object)))
    for (i in 1:length(timegrid(object))) {
      l[[i]] <- t(rep(timegrid(object)[i] ^ {
        -1
      }, timegrid(object)[i]))
    }
    P <- Matrix::bdiag(l)
    return(c(as.matrix(P %*% minload_fuel)))

  }else{

    if(length(load_abstract_fuel(object))==0){
      return(0)
    }else{
      return(load_abstract_fuel(object))
    }
  }
})
