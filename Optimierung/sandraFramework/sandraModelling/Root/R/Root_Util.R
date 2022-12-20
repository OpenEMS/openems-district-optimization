#---Overview-----------------------------------------------
# coordi
# var
# proj.Root
# standardGenericVar
# Tminus
# Tend
# transform_loadprofile_mean
# transform_loadprofile_sum

#---coordi-------------------------------------------------
#'coordi
#'
#'function to generate all coordinates acording to name, timegrid and used variables
#'@export 
coordiii<- function(object){
     v<-length(variables(object))
   g<-length(timegrid(object))
   return(paste0(name(object),"_",t(matrix(rep(variables(object),g),nrow=v)),"_time",seq(g)))
 }

#---var----------------------------------------------------
var <- c(
  "Op",
  "P_el_plus",
  "P_el_minus",
  "P_th_plus",
  "P_th_minus",
  "P_fuel_plus",
  "P_fuel_minus",
  "E_el",
  "E_th",
  "E_fuel"
)
#---proj.Root----------------------------------------------
#' proj.Root (all in one)
#'
#'It returns a matrix including all variables and their time steps as
#'col-names and the requestes variable and its time steps as row-names.
#'If there is no used variable the function returns a matrix containing
#'only zeros.
#'
#'@export
proj.Root <- sapply(var, function(i)
  function(object) {
    if(length(object@variables)==0){
      return(numeric())
    }
    co <- coord(as.Root(object))
    P <-
      matrix(0, length(timegrid(object)), length(coord(as.Root(object))))
    if (length(co) > 0) {
      colnames(P) <- co

      row.names(P) <-
        paste0(name(object), "_", i, "_time", as.character(1:length(timegrid(object))))

      if (any(variables(object) == i)) {
        id <- diag(length(co))
        P <- t(id[, grep(i, co)])

        colnames(P) <- co

        row.names(P) <-
          paste0(name(object), "_", i, "_time", as.character(1:length(timegrid(object))))
      }
      return(P)
    }
  })
#---standardGenericVar-------------------------------------
#'standardGenericVar
#'
#'for automatic construction of methods with variable names
#'
#'@export
setMethod("standardGenericVar",signature(x="character"),function(x){
  eval(parse(text = paste0("standardGeneric('", x, "')")))
})
#---Tminus-------------------------------------------------
#'Tminus
#'
#'Make a negative time shift with matrix. the first row is replaced by a zero row.
#'Then all the other rows follow, but not the last
#'
#'@export
#'@examples
#' Tminus(matrix(1:9,3))
setMethod("Tminus",signature(object="matrix"),function(object){
  if(dim(object)[1]>0){
    return(rbind(t(rep(0,dim(object)[2])),object[-dim(object)[1],]))
  }
  0
})
setMethod("Tminus",signature(object="numeric"),function(object){
  if(length(object)>0){
    return(c(0,object[-length(object)]));
  }
  0
})
#---Tend-------------------------------------------------
#'Tend
#'
#'Finds the last position of used object.
#'
#'@return Returns a matrix containing only the last row (timestep)
#'@export
#'@examples
#' Tend(matrix(1:9,3))

setMethod("Tend",signature(object="matrix"),function(object){
  te<-dim(object)[1]
  dia<-(diag(te)[te, ])%*%t(diag(te)[te, ])
  return(dia%*%object)
})
#---transform_loadprofile_sum----------------------------------
#'transform_loadprofile_sum
#'
#'Transforms the used loadprofile into a 15 min timesteped loadprofile
#'
#'@param Profil used loadprofile
#'@param timestep timestep of loadprofile in minutes (has to be an multitude integer of 1 or an integer by dividing 15)
#'@return returns a vector representing the new profile with a 15 min timestep
#'@export
#'@examples
#' Profil_5min <- c(1,2,3,4,5,6,7,8,9,10)
#' timestep <- 3
#' Profil_15min <- transform_loadprofile_sum(Profil_5min, timestep)
#' Profil_15min
setMethod("transform_loadprofile_sum", definition = function(Profil = "numeric", timestep = "numeric"){
  if(timestep > 0) {
    if (timestep%% 15 ==0 ) {
      factor <- timestep/15
      #if (round(factor)==factor) {
      vec <- rep(1,factor)
      newProfil <- kronecker(Profil,vec)
      newProfil <- as.vector(newProfil)
      return(newProfil)
      #}
    } else if (15%% timestep == 0) {
      factor <- 15/timestep
      #if (round(factor)==factor) {
      leng <- length(Profil)/factor
      if (round(leng)==leng) {
        mat <- matrix(Profil,factor)
        newProfil <- colSums(mat)
        newProfil <- as.vector(newProfil)
        return(newProfil)
      } else {
        print("Warning: length of Profil is not a multitude integer of 15. The last values will be deleted to be able to convert!")
        leng2 <- floor(leng)
        numb <- leng2*15/timestep
        Profil2 <- Profil[1:numb]
        mat <- matrix(Profil2,factor)
        newProfil <- colSums(mat)
        newProfil <- as.vector(newProfil)
        return(newProfil)
      }
      # }
    } else {
      print("timestep is not a multitude integer of 15 or an integer by dividing 15! Allowed timesteps are for example 1, 3, 5, 15, 30, 45, 60!")
    }
  }else {stop("timestep has to be bigger than 0") }
})

#---transform_loadprofile_mean----------------------------------
#'transform_loadprofile_mean
#'
#'Transforms the used loadprofile into a 15 min timesteped loadprofile, if the timesteps of the profil are smaller than 15 minutes, the mean is built
#'
#'@param Profil used loadprofile
#'@param timestep timestep of loadprofile in minutes (has to be an multitude integer of 1 or an integer by dividing 15)
#'@return returns a vector representing the new profile with a 15 min timestep
#'@export
#'@examples
#' Profil_5min <- c(1,2,3,4,5,6,7,8,9,10)
#' timestep <- 3
#' Profil_15min <- transform_loadprofile_mean(Profil_5min, timestep)
#' Profil_15min
setMethod("transform_loadprofile_mean", definition = function(Profil = "numeric", timestep = "numeric"){
  if(timestep > 0) {
    if (timestep%% 15 ==0 ) {
      factor <- timestep/15
      #if (round(factor)==factor) {
      vec <- rep(1,factor)
      newProfil <- kronecker(Profil,vec)
      newProfil <- as.vector(newProfil)
      return(newProfil)
      #}
    } else if (15%% timestep == 0) {
      factor <- 15/timestep
      #if (round(factor)==factor) {
      leng <- length(Profil)/factor
      if (round(leng)==leng) {
        mat <- matrix(Profil,factor)
        newProfil <- colMeans(mat)
        newProfil <- as.vector(newProfil)
        return(newProfil)
      } else {
        print("Warning: length of Profil is not a multitude integer of 15. The last values will be deleted to be able to convert!")
        leng2 <- floor(leng)
        numb <- leng2*15/timestep
        Profil2 <- Profil[1:numb]
        mat <- matrix(Profil2,factor)
        newProfil <- colMeans(mat)
        newProfil <- as.vector(newProfil)
        return(newProfil)
      }
      # }
    } else {
      print("timestep is not a multitude integer of 15 or an integer by dividing 15! Allowed timesteps are for example 1, 3, 5, 15, 30, 45, 60!")
    }
  }else {stop("timestep has to be bigger than 0") }
})




AntoRuntime<-function(object){
  if (any(grepl("Op", variables(object))==TRUE)) {
    if (length(unique(timegrid(object)))!=1) {
      print("WARNING: timegrid with different stepsizes! Minimal runntime cannot be used!")
      m <-
        new(
          "LM",
          matrix = numeric(),
          vector = numeric(),
          direction = numeric(),
          cost = numeric(),
          binary = numeric()
        )
      return(m)
    } else {
      if (length(timegrid(object))==1) {
        stop("Timegrid has just one timestep. Consequently there cannot be a minimal runtime!")
      } else {
        numVar <- length(variables(object))
        PosOp <- grep("Op", variables(object))
        minRun <- minRuntime(object)
        n <- 1
        while (sum(tau(object)[1:n])<minRun & n<length(tau(object))) {
          n <- n+1
        }
        LZ <- n
        te <- length(timegrid(object))
        minRTime <- list()
        minRTime$M <- numeric()
        minRTime$V <- numeric()
        if (te>1) {
          e<-diag(te);
          u<- t(c(rep(0, te * (PosOp - 1)),
                  LZ*e[,1]  - rowSums(cbind(rep(0,te),e[,1:min(LZ,te)])),
                  rep(0, te * (numVar - PosOp))))
          for(i in 2:te){
            u<-rbind(u,
                     t(c(rep(0, te * (PosOp - 1)),
                         LZ*(e[,i] - e[,i-1]) - rowSums(cbind(rep(0,te),e[,i:min(i+LZ-1,te)])),
                         rep(0, te * (numVar - PosOp)))))
          }

          v<-numeric()
          for(i in 1:te){
            v<-c(v,max(0,(i+LZ-1)-te))
          }
          minRTime$M<-u
          colnames(minRTime$M) <- coord(object)
          row.names(minRTime$M) <-
            paste0(name(object), " minRuntime_timeime", as.character(1:te))
          minRTime$V<-v
          minRTime$D <- rep("<=", te)
        }
        m <-
          new(
            "LM",
            matrix = minRTime$M,
            vector = minRTime$V,
            direction = minRTime$D,
            cost = rep(0, length(coord(object))),
            binary = grepl("Op_", coord(object))
          )
        return(m)
      }
    }
  } else {
    print("No variable 'Op' in object. Consequently there is no ability of modelling
          a minimal runtime for this object!")
  }
}

AntoDowntime<-function(object){
  if (any(grepl("Op", variables(object))==TRUE)) {
    if (length(unique(timegrid(object)))!=1) {
      print("WARNING: timegrid with different stepsizes! Minimal downtime cannot be used!")
    } else {
      if (length(timegrid(object))==1) {
        print("Timegrid has just one timestep. Consequently there cannot be a minimal downtime!")
        m <-
          new(
            "LM",
            matrix = numeric(),
            vector = numeric(),
            direction = numeric(),
            cost = numeric(),
            binary = numeric()
          )
        return(m)
      } else {
        numVar <- length(variables(object))
        PosOp <- grep("Op", variables(object))
        minDown <- minDowntime(object)
        n <- 1
        while (sum(tau(object)[1:n])<minDown & n<length(tau(object))) {
          n <- n+1
        }
        SZ <- n
        te <- length(timegrid(object))
        e<- diag(te)
        u<-numeric()
        minDTime<-list()
        minDTime$M<-numeric()
        minDTime$V<-numeric()
        if(te>1){
          for(i in 2:te){
            u <-
              rbind(u, t(c(
                rep(0, te * (PosOp - 1)),
                SZ * (e[, i - 1] - e[, i]) + rowSums(cbind(rep(0, te), e[, i:min(i + SZ - 1, te)])),
                rep(0, te * (numVar - PosOp))
              )))
          }
          minDTime$M <- u
          colnames(minDTime$M) <- coord(object)
          row.names(minDTime$M) <-
            paste0(name(object), " minDowntime_time", as.character(2:te))
          minDTime$V<- rep(SZ,te-1)
          minDTime$D <- rep("<=", te-1)
        }
        m <-
          new(
            "LM",
            matrix = minDTime$M,
            vector = minDTime$V,
            direction = minDTime$D,
            cost = rep(0, length(coord(object))),
            binary = grepl("Op_", coord(object))
          )
        return(m)
      }
    }
  } else {
    print("No variable 'Op' in object. Consequently there is no ability of modelling
          a minimal downtime for this object!")
  }

}
