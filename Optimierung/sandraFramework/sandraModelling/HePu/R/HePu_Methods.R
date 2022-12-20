#----Overview----------------------------------------------
# profile_source
#
#----------------------------------------------------------
#----profile_source----------------------------------------
#'profile_source
#'
#'Get the source profile on timegrid in °C. The timeseries on the 15 minute
#'timegrid is brougth to the one minute timegrid by considering minute values
#'constant on a 15 minute block. Then the timeseries is brought to timegrid by
#'taking means
#'
#'@export
#'@importFrom Root profile_source
#'@param object An HePu
#'@return numeric. A timeseries on the timegrid defined by \code{object}
#'@examples
#'a<-new.HePu()
#'#if no SourceTemp_15min is specified you get zero!
#'profile_source(a)
#'timegrid(a)<-c(5, 10, 15, 30)
#'SourceTemp_15min(a) <-c(10, 4.2, 6, 5)
#'profile_source(a)

setMethod("profile_source", signature(object = "HePu"), function(object) {
  if(length(SourceTemp_15min(object))!=0){
    if(length(object@timegrid)==0){
      return(0)
    }

  #if slots are empty it serves as default
  #bring 15-minute sharp timeseries to minute-grid
  minprofile_source <-
    c(t(matrix(
      SourceTemp_15min(object), length(SourceTemp_15min(object)), 15
    )))
  #cut timeseries to simulation time defined by timegrid(object)
  minprofile_source <- minprofile_source[1:sum(timegrid(object))]
  #now projecting to timegrid(object)
  l <- list(length(timegrid(object)))
  for (i in 1:length(timegrid(object))) {
    l[[i]] <- t(rep(timegrid(object)[i] ^ {
      -1
    }, timegrid(object)[i]))
  }
  P <- Matrix::bdiag(l)
  return(c(as.matrix(P %*% minprofile_source)))
  }else{

  if(length(SourceTemp_abstract(object))==0){
    return(0)
  }else{
    return(SourceTemp_abstract(object))
  }
  }
})
