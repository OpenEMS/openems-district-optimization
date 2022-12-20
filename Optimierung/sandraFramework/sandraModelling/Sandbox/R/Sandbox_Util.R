######################################################################
#Utility functions for the Class Sandbox
########################################################################
#---------------------------------------------
#projector methods for Sandbox
#every coordinate of a Sandbox, not the coordinates of the components,
#gets a projector. The projection matrix however has columns for all
#coordinates of the Sandbox including those of the components.

#  #---P_el(i,j)----------------------------------------------------------
#  #' P_el
#  #'
#  #'coordinate projection matrix for the (i,j) connection
#  #'
#  #'@param object Sandbox Sandbox
#  #'@param i numeric. Represents the i-th component of \code{object}
#  #'@param j numeric. Represents the j-th component of \code{object}
# #'@return matrix. Coordinate projection matrix.
#
# P_el <- function(object, i, j) {
#   stopifnot(is(object, "Sandbox"))
#   stopifnot(is(i, "numeric"))
#   stopifnot(is(j, "numeric"))
#
#   te <- length(object@timegrid)
#
#   co <- coord(object)
#   comp<-str_split_fixed(compoNames(object),"_",2)[,2]
#
#   v <- grep(paste0("Pel_",comp[i],"_",comp[j]),co)
#   u <- paste0("Pel_from_", comp[i], "_to_", comp[j], "_time", 1:te)
#
#   if (all(grepl(u, co)==FALSE)) {
#     x <- matrix(0, te, length(co))
#
#     colnames(x) <- co
#
#     row.names(x) <- u
#
#     return(x)
#   }
#   id <- diag(length(co))
#
#   colnames(id) <- co
#
#   x <- t(id[,v])
#
#   colnames(x) <- co
#
#   rownames(x) <- u
#
#   return(x)
#
# }

# #---P_el_from----------------------------------------------------------
# #projection matrix for all the electricity coming from a component
# P_el_from <- function(object, i) {
#   stopifnot(is(object, "Sandbox"))
#   stopifnot(is(i, "numeric"))
#
#   te <- length(object@timegrid)
#
#   co <- coord(object)
#
#   comp<-str_split_fixed(compoNames(object)[i],"_",2)[,2]
#   v <- grep(paste0("Pel_",comp), co)
#
#   u <- paste0("Pel_from_",comp[i],"_time", 1:te)
#
#   if (length(v) == 0) {
#     x <- matrix(0, te, length(co))
#
#     colnames(x) <- co
#
#     row.names(x) <- u
#
#     return(x)
#   }
#   id <- diag(length(co))
#
#   colnames(id) <- co
#
#   x <- t(id[, v])
#
#   a <- c(t(x))
#
#   w <- object@adjacency_el[i, ]
#
#   w <- length(which(w != 0))
#
#   if (te * w != length(v)) {
#     stop("number of production variables wrong!?")
#   }
#   a <- matrix(a, byrow = T, nrow = w)
#
#   a <- colSums(a)
#
#   x <- matrix(a, byrow = T, ncol = length(co))
#
#   colnames(x) <- co
#
#   row.names(x) <- u
#
#   return(x)
# }
# #---P_el_to----------------------------------------------------------
# #projection matrix for all the electricity coming to a component
# P_el_to <- function(object, i) {
#   stopifnot(is(object, "Sandbox"))
#   stopifnot(is(i, "numeric"))
#
#   te <- length(object@timegrid)
#
#   co <- coord(object)
#
#   names<-str_split_fixed(compoNames(object)[i],"_",2)[,2]
#   exp<-"Pel_"
#   #at which positions are the coordinates of the consuming components?
#   el<-grep(exp,co);
#   compo<-grep(paste0(names,"_time"),co)
#   v<- which( el%in% compo)
#
#
#   #at which positions are the coordinates of the consuming components?
#   u <- paste0("Pel_to_",names,"_time", 1:te)
#
#   if (length(v) == 0) {
#     x <- matrix(0, te, length(co))
#
#     colnames(x) <- co
#
#     row.names(x) <- u
#
#     return(x)
#   }
#   id <- diag(length(co))
#
#   colnames(id) <- co
#
#   x <- t(id[, v])
#
#   a <- c(t(x))
#
#   w <- object@adjacency_el[, i]
#
#   w <- length(which(w != 0))
#
#   if (te * w != length(v)) {
#     stop("number of production variables wrong!?")
#   }
#   a <- matrix(a, byrow = T, nrow = w)
#
#   a <- colSums(a)
#
#   x <- matrix(a, byrow = T, ncol = length(co))
#
#   colnames(x) <- co
#
#   row.names(x) <- u
#
#   return(x)
# }
# #---P_th----------------------------------------------------------
# #projection matrix for all the thermal energy coming from a component i to j
# P_th <- function(object, i, j) {
#   stopifnot(is(object, "Sandbox"))
#   stopifnot(is(i, "numeric"))
#   stopifnot(is(j, "numeric"))
#
#   te <- length(object@timegrid)
#
#   co <- coord(object)
#
#   comp<-str_split_fixed(compoNames(object),"_",2)[,2]
#
#   v <- grep(paste0("Pth_",comp[i],"_",comp[j]),co)
#   u <- paste0("Pth_from_", comp[i], "_to_", comp[j], "_time", 1:te)
#
#
#
#   if (length(v)==0) {
#     x <- matrix(0, te, length(co))
#
#     colnames(x) <- co
#
#     row.names(x) <- u
#
#     return(x)
#   }
#   id <- diag(length(co))
#
#   colnames(id) <- co
#
#   x <- t(id[,v])
#
#   colnames(x) <- co
#
#   row.names(x) <- u
#
#   return(x)
#
# }
# #---P_th_from----------------------------------------------------------
# #projection matrix for all the thermal energy coming from a component i
# P_th_from <- function(object, i) {
#   stopifnot(is(object, "Sandbox"))
#   stopifnot(is(i, "numeric"))
#
#   te <- length(object@timegrid)
#
#   co <- coord(object)
#
#   comp<-str_split_fixed(compoNames(object)[i],"_",2)[,2]
#   v <- grep(paste0("Pth_",comp), co)
#
#   u <- paste0("Pth_from_",comp[i],"_time", 1:te)
#
#   if (length(v) == 0) {
#     x <- matrix(0, te, length(co))
#
#     colnames(x) <- co
#
#     row.names(x) <- u
#
#     return(x)
#   }
#   id <- diag(length(co))
#
#   colnames(id) <- co
#
#   x <- t(id[, v])
#
#   a <- c(t(x))
#
#   w <- object@adjacency_th[i, ]
#
#   w <- length(which(w != 0))
#
#   if (te * w != length(v)) {
#     stop("number of production variables wrong!?")
#   }
#   a <- matrix(a, byrow = T, nrow = w)
#
#   a <- colSums(a)
#
#   x <- matrix(a, byrow = T, ncol = length(co))
#
#   colnames(x) <- co
#
#   row.names(x) <- u
#
#   return(x)
#
# }

# #---P_th_to----------------------------------------------------------
# #projection matrix for all the electricity coming to a component i
# P_th_to <- function(object, i) {
#   stopifnot(is(object, "Sandbox"))
#   stopifnot(is(i, "numeric"))
#
#   te <- length(object@timegrid)
#
#   co <- coord(object)
#
#   names<-str_split_fixed(compoNames(object)[i],"_",2)[,2]
#   exp<-"Pth_"
#   #at which positions are the coordinates of the consuming components?
#   th<-grep(exp,co);
#   compo<-grep(paste0(names,"_time"),co)
#   v<- which( th%in% compo)
#
#   u<- paste0("Pth_to_",names, "_time", 1:te)
#   if (length(v) == 0) {
#     x <- matrix(0, te, length(co))
#
#     colnames(x) <- co
#
#     row.names(x) <- u
#
#     return(x)
#   }
#   id <- diag(length(co))
#
#   colnames(id) <- co
#
#   x <- t(id[, v])
#
#   a <- c(t(x))
#
#   w <- object@adjacency_th[, i]
#
#   w <- length(which(w != 0))
#
#   if (te * w != length(v)) {
#     stop("number of production variables wrong!?")
#   }
#   a <- matrix(a, byrow = T, nrow = w)
#
#   a <- colSums(a)
#
#   x <- matrix(a, byrow = T, ncol = length(co))
#
#   colnames(x) <- co
#
#   row.names(x) <- u
#
#   return(x)
#
# }
#
# #---P_fuel----------------------------------------------------------
# #projection matrix for all the fuel coming from a component i to j
#
# P_fuel <- function(object, i, j) {
#   stopifnot(is(object, "Sandbox"))
#   stopifnot(is(i, "numeric"))
#   stopifnot(is(j, "numeric"))
#
#   te <- length(object@timegrid)
#
#   co <- coord(object)
#
#   comp<-str_split_fixed(compoNames(object),"_",2)[,2]
#
#   v <- grep(paste0("Pfuel_",comp[i],"_",comp[j]),co)
#   u <- paste0("Pfuel_from_", comp[i], "_to_", comp[j], "_time", 1:te)
#
#
#
#   if (length(v)==0) {
#     x <- matrix(0, te, length(co))
#
#     colnames(x) <- co
#
#     row.names(x) <- u
#
#     return(x)
#   }
#   id <- diag(length(co))
#
#   colnames(id) <- co
#
#   x <- t(id[,v])
#
#   colnames(x) <- co
#
#   row.names(x) <- u
#
#   return(x)
#
# }
#
# #---P_fuel_from----------------------------------------------------------
# #projection matrix for all the fuel coming from a component i
# P_fuel_from <- function(object, i) {
#   stopifnot(is(object, "Sandbox"))
#   stopifnot(is(i, "numeric"))
#
#   te <- length(object@timegrid)
#
#   co <- coord(object)
#
#   comp<-str_split_fixed(compoNames(object)[i],"_",2)[,2]
#   v <- grep(paste0("Pfuel_",comp), co)
#
#   u <- paste0("Pfuel_from_",comp[i],"_time", 1:te)
#   if (length(v) == 0) {
#     x <- matrix(0, te, length(co))
#
#     colnames(x) <- co
#
#     row.names(x) <- u
#
#     return(x)
#   }
#   id <- diag(length(co))
#
#   colnames(id) <- co
#
#   x <- t(id[, v])
#
#   a <- c(t(x))
#
#   w <- object@adjacency_fuel[i, ]
#
#   w <- length(which(w != 0))
#
#   if (te * w != length(v)) {
#     stop("number of production variables wrong!?")
#   }
#   a <- matrix(a, byrow = T, nrow = w)
#
#   a <- colSums(a)
#
#   x <- matrix(a, byrow = T, ncol = length(co))
#
#   colnames(x) <- co
#
#   row.names(x) <- u
#
#   return(x)
#
# }
# #---P_fuel_to----------------------------------------------------------
# #projection matrix for all the electricity coming to a component i
# P_fuel_to <- function(object, i) {
#   stopifnot(is(object, "Sandbox"))
#   stopifnot(is(i, "numeric"))
#
#   te <- length(object@timegrid)
#
#   co <- coord(object)
#
#   names<-str_split_fixed(compoNames(object)[i],"_",2)[,2]
#   exp<-"Pfuel_"
#   #at which positions are the coordinates of the consuming components?
#   fuel<-grep(exp,co);
#   compo<-grep(paste0(names,"_time"),co)
#   v<- which( fuel%in% compo)
#
#   u <- paste0("Pfuel_to", names, "_time", 1:te)
#
#   if (length(v) == 0) {
#     x <- matrix(0, te, length(co))
#
#     colnames(x) <- co
#
#     row.names(x) <- u
#
#     return(x)
#   }
#   id <- diag(length(co))
#
#   colnames(id) <- co
#
#   x <- t(id[, v])
#
#   a <- c(t(x))
#
#   w <- object@adjacency_fuel[, i]
#
#   w <- length(which(w != 0))
#
#   if (te * w != length(v)) {
#     stop("number of production variables wrong!?")
#   }
#   a <- matrix(a, byrow = T, nrow = w)
#
#   a <- colSums(a)
#
#   x <- matrix(a, byrow = T, ncol = length(co))
#
#   colnames(x) <- co
#
#   row.names(x) <- u
#
#   return(x)
#
# }

# #---coordi----------------------------
# # coordi: help function coordi to calculate coordinates
# coordi <- function(object) {
#   #if(length(object@name)==0 | length(object@timegrid)==0){
#   #  object@coord<-character()
#   #}else{
#   u <- character()
#   #if variables, name, timegrid of the Sandbox available the coordinates of the variables are generated
#
#   if (length(object@variables) > 0) {
#     v <- length(object@variables)
#     te <- length(object@timegrid)
#     u <-
#       paste0(object@name, "_", t(matrix(rep(
#         object@variables, te
#       ), nrow = v)), "_time", seq(te))
#
#     rm(v, te)
#
#   }
#   x <- character(0)
#   if (is(object, 'Sandbox')) {
#     x <- c(u, coordiComp(object))
#     n <- length(object@components)
#     y <- character()
#     if (n > 0) {
#       if (dim(object@adjacency_el)[1] != 0) {
#         x_el <-
#           paste0(object@name,
#                  "_P_el(",
#                  seq_len(n ),
#                  ",",
#                  kronecker(seq_len(n ), rep(1, n )),
#                  ")")
#         x_el <- as.data.frame(matrix(x_el, nrow = n ))
#         x_el <- x_el[object@adjacency_el != 0]
#         y <- c(y, x_el)
#       }
#       if (dim(object@adjacency_th)[1] != 0) {
#         x_th <-
#           paste0(object@name,
#                  "_P_th(",
#                  seq_len(n ),
#                  ",",
#                  kronecker(seq_len(n ), rep(1, n )),
#                  ")")
#         x_th <- as.data.frame(matrix(x_th, nrow = n ))
#         x_th <- x_th[object@adjacency_th != 0]
#         y <- c(y, x_th)
#       }
#       if (dim(object@adjacency_fuel)[1] != 0) {
#         x_fuel <-
#           paste0(object@name,
#                  "_P_fuel(",
#                  seq_len(n ),
#                  ",",
#                  kronecker(seq_len(n ), rep(1, n )),
#                  ")")
#         x_fuel <- as.data.frame(matrix(x_fuel, nrow = n ))
#         x_fuel <- x_fuel[object@adjacency_fuel != 0]
#         y <- c(y, x_fuel)
#       }
#       if (length(y) > 0) {
#         te <- length(object@timegrid)
#         y <-
#           paste0(t(matrix(rep(y, te), nrow = length(y))), "_time", seq(te))
#       }
#     }
#     x <- c(y, x)
#     rm(y)
#   }
#   if (length(x) == 0) {
#     x <- u
#   }
#   if (validObject(object) == T) {
#     return(x)
#   }
# }
# #---coordicomp-----------------------------------
# #coordicomp
# #if the Sandbox has components the coordinates of the components are built
# coordiComp <- function(object) {
#   return(rapply(object@components, coordi))
# }
#
#
# #----finalcoordinates---
# #  finalcoordinates<-function(object){
# #   if(length(timegrid(object))*length(name(object))==0){
# #     stop("Timegrid and name of the sandbox are necessary")
# #   }else{
# #       coordi <- character()
# #       #if variables, name, timegrid of the Sandbox available the coordinates of the variables are generated
# #
# #
# #     co<-compoNames(object)
# #     k<-length(components(object))
# #     comp<-str_split_fixed(co,"_",2)[,2]
# #
# #     variables <- c(
# #       "P_el_plus",
# #       "P_el_minus",
# #       "P_th_plus",
# #       "P_th_minus",
# #       "P_fuel_plus",
# #       "P_fuel_minus",
# #       "E_el",
# #       "E_th",
# #       "E_fuel",
# #       "Op"
# #     )
# #     v<-length(variables)
# #
# #
# #     tmp<-data.frame(matrix(0,nrow=v,ncol=k))
# #
# #     #tmp<-data.frame(variable=variables,tmp)
# #     rownames(tmp)<-variables
# #     colnames(tmp)<-comp
# #
# #     for(i in seq_len(k)){
# #   if(length(variables(components(object)[[i]]))>0){
# #     tmp[match(variables(components(object)[[i]]),rownames(tmp)),i]<-1
# #   }else{stop("Components are missing")}
# # }
# #     tmp1<-tmp[grep("P_",rownames(tmp)),]
# #     tmp2<-tmp[!rownames(tmp)%in%rownames(tmp1),]
# #
# #
# #   if(dim(adjacency_el(object))[1]==0){
# #   #get P_el_plus and P_el_minus
# #   tmpel<-tmp1[grepl("_el",rownames(tmp1)),]
# #   if(all(tmpel[1,]==0)&any(tmpel[2,]!=0)|any(tmpel[1,]!=0)&all(tmpel[2,]==0) ){
# #     stop("There is a electrical consumer but no producer or the other way around, i.e. no components has the variable P_el_minus, but at least one component hast the variable P_el_plus or no components has the variable P_el_plus, but at least one component hast the variable P_el_minus " )
# #   }else{
# #     #create adjacency_el
# #     adjael<-as.numeric(tmpel[1,])%*%t(as.numeric(tmpel[2,]))
# #     diagel<-diag(adjael)
# #     diag(adjael)<-0
# #     if(sum(adjael)==0 & any(diagel!=0)){
# #       stop("There is only one compenent that consumes and produces by itself. If there is a electrical consumer at least one electrical producer is nesseccary and the way around")
# #     }else{
# #       rownames(adjael)<-comp
# #       colnames(adjael)<-comp
# #       adjacency_el(object)<-adjael
# #     }}
# # }
# #     rownames(adjacency_el(object))<-comp
# #     tmpel<-which(adjacency_el(object)>0,arr.ind = TRUE)
# #   for(i in seq_len(dim(tmpel)[1])){
# #     coordi<-c(coordi,paste0("Pel_",comp[tmpel[i,1]],"_",comp[tmpel[i,2]]))
# #   }
# #
# #   if(dim(adjacency_fuel(object))[1]==0){
# #   #get P_fuel_plus and P_fuel_minus
# #   tmpfuel<-tmp1[grepl("_fuel",rownames(tmp1)),]
# #   if(all(tmpfuel[1,]==0)&any(tmpfuel[2,]!=0)|any(tmpfuel[1,]!=0)&all(tmpfuel[2,]==0) ){
# #     stop("There is a fuel consumer but no producer or the other way around, i.e. no components has the variable P_fuel_minus, but at least one component hast the variable P_fuel_plus or no components has the variable P_fuel_plus, but at least one component hast the variable P_fuel_minus " )
# #   }else{
# #     #create adjacency_fuel
# #     adjafuel<-as.numeric(tmpfuel[1,])%*%t(as.numeric(tmpfuel[2,]))
# #     diagfuel<-diag(adjafuel)
# #     diag(adjafuel)<-0
# #     if(sum(adjafuel)==0 & any(diagfuel!=0)){
# #       stop("There is only one compenent that consumes and produces by itself. If there is a fuel consumer at least one fuel producer is nesseccary and the way around")
# #     }else{
# #       rownames(adjafuel)<-comp
# #       colnames(adjafuel)<-comp
# #       adjacency_fuel(object)<-adjafuel
# #     }
# #   }
# #   }
# #   rownames(adjacency_fuel(object))<-comp
# #   tmpfuel<-which(adjacency_fuel(object)>0,arr.ind = TRUE)
# #   for(i in seq_len(dim(tmpfuel)[1])){
# #   coordi<-c(coordi,paste0("Pfuel_",comp[tmpfuel[i,1]],"_",comp[tmpfuel[i,2]]))
# # }
# #   if(dim(adjacency_th(object))[1]==0){
# #   #get P_th_plus and P_th_minus
# #   tmpth<-tmp1[grepl("_th",rownames(tmp1)),]
# #   if(all(tmpth[1,]==0)&any(tmpth[2,]!=0)|any(tmpth[1,]!=0)&all(tmpth[2,]==0) ){
# #     stop("There is a thermal consumer but no producer or the other way around, i.e. no components has the variable P_th_minus, but at least one component hast the variable P_th_plus or no components has the variable P_th_plus, but at least one component hast the variable P_th_minus " )
# #
# #     }else{
# #     #create adjacency_th
# #     adjath<-as.numeric(tmpth[1,])%*%t(as.numeric(tmpth[2,]))
# #     diagth<-diag(adjath)
# #     diag(adjath)<-0
# #     if(sum(adjath)==0 & any(diagth!=0)){
# #       stop("There is only one compenent that consumes and produces by itself. If there is a thermal consumer at least one producer is nesseccary and the way around")
# #     }else{
# #       rownames(adjath)<-comp
# #       colnames(adjath)<-comp
# #       adjacency_th(object)<-adjath
# #     }}
# #   }
# #     rownames(adjacency_th(object))<-comp
# #   tmpth<-which(adjacency_th(object)>0,arr.ind = TRUE)
# #   for(i in seq_len(dim(tmpth)[1])){
# #   coordi<-c(coordi,paste0("Pth_",comp[tmpth[i,1]],"_",comp[tmpth[i,2]]))
# # }
# #
# #   rownames(tmp2)<-sub("_","",rownames(tmp2))
# #   if(dim(tmp2)[1]>0){
# #   for(i in seq_len(dim(tmp2)[1])){
# #     for(j in seq_len(dim(tmp2)[2])){
# #       if(tmp2[i,j]>0){
# #         coordi<-c(coordi,paste0(row.names(tmp2)[i],"_",colnames(tmp2)[j]))
# #       }
# #     }
# #
# #   }}
# #
# #     te<-length(timegrid(object))
# #   coordi<- paste0(t(matrix(rep(coordi, te), nrow = length(coordi))), "_time", seq(te))
# #
# #
# #   for(i in seq_len(k)){
# #     name<-str_split_fixed(name(components(object)[[i]]),"_",2)[,2]
# #
# #     object@components[[i]]@coord<-coordi[grepl(name,coordi)]
# #   }
# #  object@coord<-coordi
# #   return(object)
# #   }
# # }
#---equationTSvirtual----
equationTSvirtual <- function(object, wado) {
  minT <- numeric()
  maxT <- numeric()
  equation <- numeric()
  # get components which are delivering P_th to wado
  conwado <-
    adjacency_th(object)[-dim(adjacency_th(object))[1], wado]

  ts_com <- compoClass(object) == 'TS'
  ts_vir <- which(ts_com * conwado != 0)
  wado_com <- compoClass(object) == "Wado"
  wado_wado <- which(wado_com * conwado != 0)

  # if there is a cascade TS get the variables/equation and temperature of the direct connected TS and recursive via the cascade wado the indirect connected TS
  if (length(ts_vir) == 1 & length(wado_wado) == 1) {
    if (length(minTemp(components(object)[[ts_vir]])) * length(minTemp(components(object)[[ts_vir]])) >
        0) {
      minT <- c(minT, minTemp(components(object)[[ts_vir]]))
      maxT <- c(maxT, maxTemp(components(object)[[ts_vir]]))
      if (length(equation) > 0) {
        equation <-
          equation + (maxTemp(components(object)[[ts_vir]]) - minTemp(components(object)[[ts_vir]])) *
          P_th_plus(object, ts_vir)
      } else{
        equation <-
          (maxTemp(components(object)[[ts_vir]]) - minTemp(components(object)[[ts_vir]])) *
          P_th_plus(object, ts_vir)
      }
    } else{
      stop(print(
        paste(
          "maxTemp and/or minTemp of component",
          ts_vir,
          "is missing!"
        )
      ))
    }
    tmp <- equationTSvirtualsub(object, wado_wado)
    minT <- c(minT, tmp$minT)
    maxT <- c(maxT, tmp$maxT)
    if (length(equation) > 0) {
      equation <- equation + tmp$equation
    } else{
      equation <-  tmp$equation
    }
  }

  # If the wado is direct connected with the wado, get the temperature and the variables/equation
  if (length(ts_vir) == 2) {
    if (volume(components(object)[[ts_vir[1]]]) == volume(components(object)[[ts_vir[2]]]) &
        (
          maxTemp(components(object)[[ts_vir[1]]]) == minTemp(components(object)[[ts_vir[2]]]) |
          maxTemp(components(object)[[ts_vir[2]]]) == minTemp(components(object)[[ts_vir[1]]])
        )) {
      minT <-
        c(minT, c(minTemp(components(object)[[ts_vir[1]]]), minTemp(components(object)[[ts_vir[2]]])))

      maxT <-
        c(maxT, c(maxTemp(components(object)[[ts_vir[1]]]), maxTemp(components(object)[[ts_vir[2]]])))

      if (length(equation) > 0) {
        equation <-
          equation + (maxTemp(components(object)[[ts_vir[1]]]) - minTemp(components(object)[[ts_vir[1]]])) *
          P_th_plus(object, ts_vir[1]) +
          (maxTemp(components(object)[[ts_vir[2]]]) - minTemp(components(object)[[ts_vir[2]]])) *
          P_th_plus(object, ts_vir[2])
      } else{
        equation <-
          (maxTemp(components(object)[[ts_vir[1]]]) - minTemp(components(object)[[ts_vir[1]]])) *
          P_th_plus(object, ts_vir[1]) +
          (maxTemp(components(object)[[ts_vir[2]]]) - minTemp(components(object)[[ts_vir[2]]])) *
          P_th_plus(object, ts_vir[2])

      }
    } else{
      stop(print(
        paste(
          "Volume of virtual components does not correspond, or minTemp and maxTemp of the components does not correspond!"
        )
      ))
    }




  }
  return(list(
    equation = equation,
    minT = minT,
    maxT = maxT
  ))

}

#------------equationTSvirtualsub--------
equationTSvirtualsub <- function(object, wado_wado) {
  return(equationTSvirtual(object, wado_wado))

}

#NOT IN USE

# #------------getspyvar-----
# getspyvar<-function(object,variable,compnr){
#
#   var<-variable
#   lapply(components(object),var)
# }
# #------------getspythreshold------
# getspythreshold<-function(object,variable,compnr){
#
#   var<-variable
#   lapply(list(components(object)[[compnr]]),var)[[1]]
# }
#
#




#'aggregation functions for timeseries objects
#'
#'@export
#'@examples data<-matrix(1:12,6);
#' grid<-c(1/2,1);
#' aggr(data,grid,"Power");
#' aggr(data,grid,"Energy");

ag<-function(data,grid,type = "Power"){

  data<-as.data.frame(data)
  if(!any(type==c("Power","Energy"))){
    stop(writeLines("type has to be \"Power\" or \"Energy\" "))
  }


    if(dim(data)[1]!=sum(grid)/15){
      stop(writeLines("\nsimulation time defined by grid has to match dimension of data:\ndim(data)[1]==sum(grid)*4"))
    }
  #grid auf Stundenwerte
  grid <- grid/60

  #we are just interested in the numeric matrix
  data<-as.matrix(data);

  te<-length(grid);
  #construction of the projector/aggregator matrix
  l<-list();
  for(i in 1:te){
    if(type =="Power"){
      l[[i]]<-t(rep(1/(grid[i]*4),grid[i]*4));
    }else{
      loc<-rep(0,grid[i]*4);
      loc[grid[i]*4]<-1;
      l[[i]]<-t(loc);
    }
  }
  l<-bdiag(l);
  data<-as.matrix(l%*%data)
  data

}


# #'@export
# setGeneric("FlexMax", function(object)
#   standardGeneric("FlexMax"))


#'FlexMax
#'
#'get upper bound  for available flexibility in a sandbox
#'@importFrom Root FlexMax
#'@param object A Sandbox
#'
#'@return numeric Upper bound for flexibility for all time steps in kW
#'@export

setMethod("FlexMax",signature(object = "Sandbox"),function(object){
  #
  #the components
  #
  l<-components(object);
  #
  if(length(l)==0){
    return(0)
  }
  #
  flexMax <- numeric()
  #
  for (i in 1:length(l)) {
    x <- l[[i]]

    flexMax <- c(
      flexMax,
      maxP_el_minus(x),
      maxP_el_plus(x),
      maxP_th_minus(x),
      maxP_th_plus(x),
      maxP_fuel_minus(x),
      maxP_fuel_plus(x)
    )
    rm(x)
  }
  flexMax <- sum(flexMax) * 100
  return(flexMax)
})




#'LM.Cheap
#'
#'for i :Op_i(which_time) = schedule(whichtime,i)!=0
#'
#'
#'@export
#'@examples
#'coordinates <-c("Op_CHP1_t1_","Op_CHP1_t2_","Op_CHP2_t1_","Op_CHP2_t2_","a");
#'which_time <-c(FALSE,TRUE);
#'schedule <- data.frame(RB_CHP1 = c(10,20),RB_CHP2 = c(7,6));
#'model <- LM.Cheap(coordinates,which_time,schedule)
#'cbind(LM.matrix(model),LM.direction(model),LM.vector(model))
#'print(LM.binary(model))
#'
#'
LM.Cheap<-function(coordinates,which_time,schedule){

  if(!is.character(coordinates)){
    stop("coordinates have to be character")
  }
  if(length(coordinates)==0){
    stop("coordinates chould be of length >= 1")
  }
  if(!is.data.frame(schedule)){
    stop("schedule has to be data.frame")
  }
  if(!is.logical(which_time)){
    stop("which_time has to be logical")
  }
  if(length(which_time)!=dim(schedule)[1]){
    stop("length of which time has to be equal to dim(schedule)[1]")
  }

  Flex_cheap <- matrix(0,0,length(coordinates));
  Flex_cheap_V <- numeric(0);
  Flex_cheap_D <- character(0);
  #remark: we use this vecotr later with an and operation, so TRUE ensures that apriori nothing happens.
  Flex_cheap_B <- rep(TRUE,length(coordinates));
  id <- diag(length(coordinates));
  nam <- colnames(schedule);

  which_time <- which(which_time)
  tet <- length(which_time);

  for (i in nam) {

    a <- grepl(paste0("Op_",  strsplit(i, "_")[[1]][2]), coordinates)
    if (any(a)) {

      #the projector onto the Op variable of i at times which_time

      x <- t(id[, a][, which_time])

      row.names(x) <-
        paste0("FlexA_cheapFix_", i, "_t", which_time)

      colnames(x) <- coordinates


      Flex_cheap  <- rbind(Flex_cheap, x)

      Flex_cheap_V   <-
        c(Flex_cheap_V, as.numeric(schedule[which_time, i] != 0))

      Flex_cheap_D  <-
        c(Flex_cheap_D, rep("=", tet))



      #if cheap, the scheduled prosumers are fixed in their status
      #and do not have to be considered binary
      #the and operation will now change all TRUE to FALSE
      Flex_cheap_B[a][which_time] <- FALSE


    }
  }


  return(new("LM",
             matrix = Flex_cheap,
             vector = Flex_cheap_V ,
             direction = Flex_cheap_D,
             binary = Flex_cheap_B,
             cost = rep(0,length(Flex_cheap_B))
  )
  )
}

#'LM.FlexaSchedule
#'
#'sum_i Pel_i(which_time) = Flex + sgn(direction)*sum_i schedule(which_time,i)
#'
#'@export
#'@examples
#' coordinates <-c("Flex_positive_t2", "Pel_CHP1_t1_","Pel_CHP1_t2_",
#'                 "Pel_CHP2_t1_","Pel_CHP2_t2_",
#'                 "a");
#'
#' which_time <-c(FALSE,TRUE);
#' schedule <- data.frame(RB_CHP1 = c(10,20),RB_CHP2 = c(7,6));
#' direction <- "positive"
#'
#' model <- LM.FlexaSchedule(coordinates,schedule,which_time,direction);
#' cbind(LM.matrix(model),LM.direction(model),LM.vector(model))
#'
#'
LM.FlexaSchedule<-function(coordinates,schedule,which_time,direction){
  if(!is.character(coordinates)){
    stop("coordinates have to be character")
  }
  if(length(coordinates)==0){
    stop("coordinates chould be of length >= 1")
  }
  if(!is.data.frame(schedule)){
    stop("schedule has to be data.frame")
  }
  if(!is.logical(which_time)){
    stop("which_time has to be logical")
  }
  if(length(which_time)!=dim(schedule)[1]){
    stop("length of which time has to be equal to dim(schedule)[1]")
  }

  if(!any(direction ==c("positive","negative"))){
    stop(writeLines("direction has to be \"positive\" or \"negative\""))
  }
  Flex_flex_B <- rep(T,length(coordinates))
  nam<-colnames(schedule)
  which_time <-which(which_time);
  if(length(which_time)>0){
    if(length(which(grepl("^Flex_positive_t\\d", coordinates)|grepl("^Flex_negative_t\\d", coordinates)))==0){
    stop("No Flex coordinates!")
    }
  }
  tet<-length(which_time)
  #flexibility direction
  #
  sgn <- 1 - 2 * ("positive" != direction)
  te <- dim(schedule)[1]
  id_FlexA<-diag(length(coordinates))
  #the transposed projector onto the flex variables of i within the subspace
  #generated by all flex, and binary flex variables


  flex <-
    id_FlexA[, grepl("^Flex_positive_t\\d", coordinates)|grepl("^Flex_negative_t\\d", coordinates)]

  #
  #equality connecting the desired electric power of an object
  #to the sum of its flexibility and its current schedule
  #P[which_time] = schedule[which_time] + sgn*FlexA
  #

  #the projector onto the el. production of the machines in the schedule within the subspace generated
  #by all the coordinates coord of the Sandbox
  P <- 0;
  for (i in nam) {

    P <- P +
      t(t(P_el_plus(coordinates, i,te))[, which_time]) - t(t(P_el_minus(coordinates, i,te))[, which_time])
  }
  x <- P - sgn * t(flex)

  row.names(x) <- paste0("FlexA_flex_t", which_time)

  colnames(x) <- coordinates

  Flex_flex <- x

  Flex_flex_V <- rowSums(as.data.frame(schedule[which_time, ]))
  Flex_flex_D <- rep("=", tet)




  return(new("LM",
             matrix = Flex_flex,
             vector = Flex_flex_V,
             direction = Flex_flex_D,
             binary = Flex_flex_B,
             cost = rep(0,length(Flex_flex_B))
  ))
}

#'LM.FlexaScheduleFree
#'
#'sum_i Pel_i(which_time) = Flex + sgn(direction)*sum_i schedule(which_time,i)
#'
#'@export
#'@examples
#' coordinates <-c("Flex_positive_t2","Flex_bin_t2" ,"Pel_CHP1_t1_","Pel_CHP1_t2_",
#'                 "Pel_CHP2_t1_","Pel_CHP2_t2_",
#'                 "a");
#'
#' which_time <-c(FALSE,TRUE);
#' schedule <- data.frame(RB_CHP1 = c(10,20),RB_CHP2 = c(7,6));
#' direction <- "positive"
#' flexMax <-1000
#'
#' model <- LM.FlexaScheduleFree(coordinates,schedule,which_time,direction,flexMax);
#' cbind(LM.matrix(model),LM.direction(model),LM.vector(model))
#'
#'
LM.FlexaScheduleFree<-function(coordinates,schedule,which_time,direction,flexMax){
  if(!is.character(coordinates)){
    stop("coordinates have to be character")
  }
  if(length(coordinates)==0){
    stop("coordinates chould be of length >= 1")
  }
  if(!is.data.frame(schedule)){
    stop("schedule has to be data.frame")
  }
  if(!is.logical(which_time)){
    stop("which_time has to be logical")
  }
  if(length(which_time)!=dim(schedule)[1]){
    stop("length of which time has to be equal to dim(schedule)[1]")
  }

  if(!any(direction ==c("positive","negative"))){
    stop(writeLines("direction has to be \"positive\" or \"negative\""))
  }
  Flex_flex_B <- rep(T,length(coordinates))
  nam<-colnames(schedule)
  which_time <-which(which_time);
  if(length(which_time)>0){
    if(length(which(grepl("^Flex_positive_t\\d", coordinates)|grepl("^Flex_negative_t\\d", coordinates)))==0){
      stop("No Flex coordinates!")
    }
  }
  tet<-length(which_time)
  #flexibility direction
  #
  sgn <- 1 - 2 * ("positive" != direction)
  te <- dim(schedule)[1]
  id_FlexA<-diag(length(coordinates));


  #the transposed projector onto the flex variables of i within the subspace
  #generated by all flex, and binary flex variables


  flex <-
    id_FlexA[, grepl("^Flex_positive_t\\d", coordinates)|grepl("^Flex_negative_t\\d", coordinates)];
  bin <-
    id_FlexA[, grepl("^Flex_bin_t\\d", coordinates)];

  #
  #equality connecting the desired electric power of an object
  #to the sum of its flexibility and its current schedule
  #P[which_time] = schedule[which_time] + sgn*FlexA
  #

  #the projector onto the el. production of the machines in the schedule within the subspace generated
  #by all the coordinates coord of the Sandbox
  P <- 0;
  for (i in nam) {
    P <- P +
      t(t(P_el_plus(coordinates, i,te))[, which_time]) - t(t(P_el_minus(coordinates, i,te))[, which_time])
  }
  x <- P - sgn * t(flex) + t(bin)*flexMax

  row.names(x) <- paste0("FlexA_ScheduleUpper_t", which_time)

  colnames(x) <- coordinates

  Flex_flex <- x

  Flex_flex_V <- rowSums(as.data.frame(schedule[which_time, ])) + flexMax
  Flex_flex_D <- rep("<=", tet);

  x <- -(P - sgn * t(flex)) + t(bin)*flexMax

  row.names(x) <- paste0("FlexA_ScheduleLower_t", which_time)

  colnames(x) <- coordinates

  Flex_flex <- rbind(Flex_flex,x)

  Flex_flex_V <- c(Flex_flex_V,-rowSums(as.data.frame(schedule[which_time, ])) + flexMax)
  Flex_flex_D <- c(Flex_flex_D,rep("<=", tet));









  return(new("LM",
             matrix = Flex_flex,
             vector = Flex_flex_V,
             direction = Flex_flex_D,
             binary = Flex_flex_B,
             cost = rep(0,length(Flex_flex_B))
  ))
}



#'LM.ScheduleFix
#'
#'sum_i Pel_i(which_time) = sum_i schedule(which_time,i)
#'
#'@export
#'@examples
#'
#' coordinates <-c("Pel_CHP1_t1_","Pel_CHP1_t2_",
#'                 "Pel_CHP2_t1_","Pel_CHP2_t2_",
#'                 "a");
#'
#' which_time <-c(FALSE,TRUE);
#' schedule <- data.frame(RB_CHP1 = c(10,20),RB_CHP2 = c(7,6));
#' nam <- NULL
#'
#' model <- LM.ScheduleFix(coordinates,which_time,schedule);
#' cbind(LM.matrix(model),LM.direction(model),LM.vector(model))
#' nam <- colnames(schedule);
#'schedule<-rowSums(schedule);
#'model <- LM.ScheduleFix(coordinates,which_time,schedule,nam);
#'cbind(LM.matrix(model),LM.direction(model),LM.vector(model))
#'
LM.ScheduleFix<-function(coordinates,which_time,schedule,nam=NULL){
 
  if(!is.character(coordinates)){
    stop("coordinates have to be character")
  }
  if(length(coordinates)==0){
    stop("coordinates chould be of length >= 1")
  }
  if(!(is.data.frame(schedule)|is.numeric(schedule))){
    stop("schedule has to be data.frame")
  }
  if(!is.logical(which_time)){
    stop("which_time has to be logical")
  }
  if(is.data.frame(schedule)){
    if(length(which_time)!=dim(schedule)[1]){
      stop("length of which time has to be equal to dim(schedule)[1]")
      }
    nam<-colnames(schedule);
    te <- dim(schedule)[1]
  }else{
    if(length(which_time)!=length(schedule)){
      stop("length of which time has to be equal length of schedule")
    }
    if(is.null(nam)){
      stop("if schedule is numeric component names have to be specified")
    }
    te <- length(schedule)
  }

  Flex_fix<-matrix(0,0,length(coordinates));
  Flex_fix_V<-numeric(0);
  Flex_fix_D<-character(0);
  Flex_fix_B<-rep(T,length(coordinates));

  which_time <- which(which_time);
  tet<-length(which_time);


  x<-0;
  if(tet!=0){
    for (i in nam) {
     

      x <- x +
        t(t(P_el_plus(coordinates, i,te))[, which_time]) - 
        t(t(P_el_minus(coordinates, i,te))[, which_time])

       

    }
    row.names(x) <- paste0("FlexA_fix_t", which_time)

    colnames(x) <- coordinates;

    Flex_fix <- rbind(Flex_fix, x)
    #allows to treat more than one column when considering schedule
    #we can treat aggregated and not aggregated schedules at the same time
    Flex_fix_V <- c(Flex_fix_V, rowSums(as.data.frame(as.data.frame(schedule)[which_time, ])))
    Flex_fix_D <- c(Flex_fix_D, rep("=", tet))

  }

  return(new("LM",
             matrix = Flex_fix,
             vector = Flex_fix_V,
             direction = Flex_fix_D,
             binary = Flex_fix_B,
             cost = rep(0,length(Flex_fix_B))
  ))

}

#'LM.ScheduleFix
#'
#'sum_i Pel_i(which_time) = sum_i schedule(which_time,i)
#'
#'@export
#'@examples
#'
#' coordinates <-c("Pel_CHP1_t1_","Pel_CHP1_t2_",
#'                 "Pel_CHP2_t1_","Pel_CHP2_t2_",
#'                 "a");
#'
#' which_time <-c(FALSE,TRUE);
#' schedule <- data.frame(RB_CHP1 = c(10,20),RB_CHP2 = c(7,6));
#' nam <- NULL
#'
#' model <- LM.ScheduleFix(coordinates,which_time,schedule);
#' cbind(LM.matrix(model),LM.direction(model),LM.vector(model))
#' nam <- colnames(schedule);
#'schedule<-rowSums(schedule);
#'model <- LM.ScheduleFix(coordinates,which_time,schedule,nam);
#'cbind(LM.matrix(model),LM.direction(model),LM.vector(model))
#'
LM.ScheduleFix_new<-function(coordinates,schedules){

  if(!is.character(coordinates)){
    stop("coordinates have to be character")
  }
  if(length(coordinates)==0){
    stop("coordinates chould be of length >= 1")
  }
  

  Flex_fix<-matrix(0,0,length(coordinates));
  Flex_fix_V<-numeric(0);
  Flex_fix_D<-character(0);
  Flex_fix_B<-rep(T,length(coordinates));
  id <- diag(length(coordinates))
  slack_plus <- t(
    rowSums(
      id[,
      grepl("^Pel_Assel",coordinates)
      ]
      )
      )
  slack_minus <- t(
    rowSums(
      id[,
      grepl("^Pel_",coordinates)&grepl("Assel//d$",coordinates)
      ]
      )
      )
  


    for (j in 1:length(schedules)) {
        
        i = names(schedules)[[j]]
        schedule = schedules[[j]]
        te = dim(schedule)[1]
        which_time <- which(as.logical(schedule$fixed));
        tet<-length(which_time);
          
        if(tet!=0){




          x <- 
            t(t(P_el_plus(coordinates, i,te))[, which_time]) -
            t(t(P_el_minus(coordinates, i,te))[, which_time])
          row.names(x) <- paste0("FlexA_fix_t", which_time)

          colnames(x) <- coordinates;

          Flex_fix <- rbind(Flex_fix, x)
          #allows to treat more than one column when considering schedule
          #we can treat aggregated and not aggregated schedules at the same time
          Flex_fix_V <- c(Flex_fix_V, schedule$value[which_time])
          Flex_fix_D <- c(Flex_fix_D, rep("=", tet))

          
    }
    

  }

  return(new("LM",
             matrix = Flex_fix,
             vector = Flex_fix_V,
             direction = Flex_fix_D,
             binary = Flex_fix_B,
             cost = rep(0,length(Flex_fix_B))
  ))

}


#'LM.ScheduleFix
#'
#'sum_i Pel_i(which_time) = sum_i schedule(which_time,i)
#'
#'@export
#'@examples
#'
#' coordinates <-c("Pel_CHP1_t1_","Pel_CHP1_t2_",
#'                 "Pel_CHP2_t1_","Pel_CHP2_t2_",
#'                 "a");
#'
#' which_time <-c(FALSE,TRUE);
#' schedule <- data.frame(RB_CHP1 = c(10,20),RB_CHP2 = c(7,6));
#' nam <- NULL
#'
#' model <- LM.ScheduleFix(coordinates,which_time,schedule);
#' cbind(LM.matrix(model),LM.direction(model),LM.vector(model))
#' nam <- colnames(schedule);
#'schedule<-rowSums(schedule);
#'model <- LM.ScheduleFix(coordinates,which_time,schedule,nam);
#'cbind(LM.matrix(model),LM.direction(model),LM.vector(model))
#'
LM.ScheduleFix_slack<-function(coordinates,schedules){

  if(!is.character(coordinates)){
    stop("coordinates have to be character")
  }
  if(length(coordinates)==0){
    stop("coordinates chould be of length >= 1")
  }
  

  Flex_fix<-matrix(0,0,length(coordinates));
  Flex_fix_V<-numeric(0);
  Flex_fix_D<-character(0);
  Flex_fix_B<-rep(T,length(coordinates));
  id <- diag(length(coordinates))
  colnames(id) = coordinates
  row.names(id) = coordinates
  

    for (j in 1:length(schedules)) {
        
        i = names(schedules)[[j]]
        schedule = schedules[[j]]
        te = dim(schedule)[1]
        which_time <- which(as.logical(schedule$fixed));
        tet<-length(which_time);
        nam = str_split_fixed(i,"_",2)[,2]
        if(tet!=0){
       
        slack_pos = t(id[,grep(paste0("^slack_",nam,"_pos"),coordinates)][, which_time])
        slack_neg = t(id[,grep(paste0("^slack_",nam,"_neg"),coordinates)][, which_time]) 
        
          x <- slack_pos - slack_neg +
            t(t(P_el_plus(coordinates, i,te))[, which_time]) -
            t(t(P_el_minus(coordinates, i,te))[, which_time])
          row.names(x) <- paste0("FlexA_fix_t", which_time)

          colnames(x) <- coordinates;

          Flex_fix <- rbind(Flex_fix, x)
          #allows to treat more than one column when considering schedule
          #we can treat aggregated and not aggregated schedules at the same time
          Flex_fix_V <- c(Flex_fix_V, schedule$value[which_time])
          Flex_fix_D <- c(Flex_fix_D, rep("=", tet))

          
    }
    

  }

  return(new("LM",
             matrix = Flex_fix,
             vector = Flex_fix_V,
             direction = Flex_fix_D,
             binary = Flex_fix_B,
             cost = rep(0,length(Flex_fix_B))
  ))

}



#'LM.FlexaDom
#'
#'Flex <= Flex_bin*flexMax
#'
#'@export
#'@examples
#' coordinates <- c("Flex_positive_t2","Flex_bin_t2")
#' which_time <- c(FALSE,TRUE)
#' flexMax <- 10
#' model <- LM.FlexaDom(coordinates,which_time ,flexMax);
#' cbind(LM.matrix(model),LM.direction(model),LM.vector(model))
#'
#'
LM.FlexaDom<-function(coordinates,which_time,flexMax){

  if(!is.character(coordinates)){
    stop("coordinates have to be character")
  }
  if(length(coordinates)==0){
    stop("coordinates chould be of length >= 1")
  }

  if(!is.logical(which_time)){
    stop("which_time has to be logical")
  }
  if(!is.numeric(flexMax)){
    stop(writeLines("flexMax has to be numeric"))
  }
  if(!length(flexMax)==1){
    stop(writeLines("flexMax has to be of length 1"))
  }
  if(length(which(grepl("^Flex_positive_t", coordinates)|
            grepl("^Flex_negative_t", coordinates)))==0){
    stop("No Flex coordinates")
  }
  if(length(which(grepl("^Flex_bin_t", coordinates)
                  ))==0){
    stop("No Flex binary coordinates")
  }
  Flex_dom<-matrix(0,0,length(coordinates));
  Flex_dom_V<-numeric(0);
  Flex_dom_D<-character(0);
  Flex_dom_B<-rep(T,length(coordinates))
  id_FlexA<-diag(length(coordinates));
  which_time <- which(which_time)
  tet<-length(which_time)

  # flex binary = 0 => flex = 0
  #the transposed projector onto the flex variables of i within the subspace
  #generated by all flex, and binary flex variables
  flex <-
    id_FlexA[, grepl("^Flex_positive_t", coordinates)|
               grepl("^Flex_negative_t", coordinates)]
  #the transposed projector onto the binary flex variables of i within the subspace
  #generated by all flex, and binary flex variables
  bin <- id_FlexA[, grepl(paste0("Flex_bin"), coordinates)]


  x <- t(flex) - flexMax * t(bin)

  row.names(x) <- paste0("FlexA_dom_t", which_time)

  colnames(x) <- coordinates


  Flex_dom <- rbind(Flex_dom, x)

  Flex_dom_V <- c(Flex_dom_V, rep(0, tet))
  Flex_dom_D <- c(Flex_dom_D, rep("<=", tet))



  return(new("LM",
             matrix = Flex_dom,
             vector = Flex_dom_V,
             direction = Flex_dom_D,
             binary = Flex_dom_B,
             cost = rep(0,length(Flex_dom_B))
  ))
}




#'LM.FlexaMon
#'
#'Flex(t) <= Flex(t-1)
#'
#'@export
#'@examples
#' coordinates <- c("Flex_positive_t1","Flex_positive_t2")
#' which_time <- c(FALSE,TRUE,TRUE)
#' flexMax <- 10
#' model <- LM.FlexaMon(coordinates,which_time );
#' cbind(LM.matrix(model),LM.direction(model),LM.vector(model))

LM.FlexaMon<-function(coordinates,which_time){
  if(!is.character(coordinates)){
    stop("coordinates have to be character")
  }
  if(length(coordinates)==0){
    stop("coordinates chould be of length >= 1")
  }

  if(!is.logical(which_time)){
    stop("which_time has to be logical")
  }
  id_FlexA<-diag(length(coordinates));

  if(length(which(grepl("^Flex_positive_t", coordinates)|
                  grepl("^Flex_negative_t", coordinates)))==0){
    stop("No Flex coordinates")
  }
  which_time <- which(which_time)
  tet<-length(which_time);

  Flex_mon <- matrix(0,0,length(coordinates));
  Flex_mon_V <- numeric(0);
  Flex_mon_D <- character(0);
  Flex_mon_B <- rep(T,length(coordinates));




  if (tet > 1) {
    flex <-
      id_FlexA[, grepl("^Flex_positive_t\\d", coordinates)|grepl("^Flex_negative_t\\d", coordinates)]
    #the transposed projector onto the binary flex variables of i within the subspace
    #
    #Flexibility is monotonically decreasing
    #FlexA[t+1] - FlexA[t] <= 0
    #
    x <- t(flex[, -1]) - t(flex[, -tet])

    row.names(x) <-
      paste0("FlexA_mon_t", which_time[-1])

    colnames(x) <- coordinates



    Flex_mon <- rbind(Flex_mon, x)
    Flex_mon_V <- c(Flex_mon_V, rep(0, tet-1))
    Flex_mon_D <-
      c(Flex_mon_D, rep("<=", tet-1))

  }

  return(new("LM",
             matrix = Flex_mon,
             vector = Flex_mon_V,
             direction = Flex_mon_D,
             binary = Flex_mon_B,
             cost = rep(0,length(Flex_mon_B))
  ))

}



#'LM.FlexaMonBin
#'
#'Flex_bin(t) <= Flex_bin(t-1)
#'
#'
#'@export
#'@examples
#' coordinates <- c("Flex_bin_t1","Flex_bin_t2")
#' which_time <- c(FALSE,TRUE,TRUE)
#' flexMax <- 10
#' model <- LM.FlexaMonBin(coordinates,which_time );
#' cbind(LM.matrix(model),LM.direction(model),LM.vector(model))

LM.FlexaMonBin<-function(coordinates,which_time){
  if(!is.character(coordinates)){
    stop("coordinates have to be character")
  }
  if(!is.logical(which_time)){
    stop("which_time has to be logical")
  }
  if(length(which(grepl("^Flex_bin_t", coordinates)
  ))==0){
    stop("No Flex binary coordinates")
  }
  which_time <- which(which_time)
  tet<-length(which_time);
  id_FlexA<-diag(length(coordinates));


  Flex_mon_bin <- matrix(0,0,length(coordinates));
  Flex_mon_bin_V <- numeric(0);
  Flex_mon_bin_D <- character(0);
  Flex_mon_bin_B <- rep(T,length(coordinates));





  if (tet > 1) {

    #
    #flex binary is monotonically decreasing
    #bin[t+1] - bin[t] <= 0
    #



    #the transposed projector onto the binary flex variables of i within the subspace
    #generated by all flex, and binary flex variables
    bin <- id_FlexA[, grepl(paste0("Flex_bin"), coordinates)]



    x<- t(bin[,-1]) - t(bin[,-tet])

    row.names(x)<-paste0("FlexA_mon_bin_t", which_time[-1])
    colnames(x)<-coordinates


    Flex_mon_bin <- rbind(Flex_mon_bin,x);
    Flex_mon_bin_V<-c(Flex_mon_bin_V,rep(0,tet-1))
    Flex_mon_bin_D<-c(Flex_mon_bin_D,rep("<=",tet-1))

  }


  return(new("LM",
             matrix=Flex_mon_bin,
             vector=Flex_mon_bin_V,
             direction=Flex_mon_bin_D,
             binary=rep(T,length(coordinates)),
             cost =rep(0,length(coordinates))

  ))
}



#'LM.FlexaConst
#'
#'Flex(t) - Flex(t+1) + flexMax*Flex_bin(t+1) <= flexMax
#'
#'
#'@export
#'@examples
#' coordinates <- c("Flex_positive_t2","Flex_positive_t3",
#'                  "Flex_bin_t2","Flex_bin_t3")
#' which_time <- c(FALSE,TRUE,TRUE)
#' flexMax <- 10
#' model <- LM.FlexaConst(coordinates,which_time ,flexMax);
#' cbind(LM.matrix(model),LM.direction(model),LM.vector(model))


LM.FlexaConst<-function(coordinates,which_time,flexMax){
  if(!is.character(coordinates)){
    stop("coordinates have to be character")
  }
  if(!is.logical(which_time)){
    stop("which_time has to be logical")
  }
  if(!is.numeric(flexMax)){
    stop("flexMax has to be numeric")
  }
  if(length(flexMax)!=1){
    stop("flexMax has to have length one")
  }
  if(length(which(grepl("^Flex_positive_t", coordinates)|
                  grepl("^Flex_negative_t", coordinates)))==0){
    stop("No Flex coordinates")
  }
  if(length(which(grepl("^Flex_bin_t", coordinates)
  ))==0){
    stop("No Flex binary coordinates")
  }
  id_FlexA<-diag(length(coordinates));

  which_time <- which(which_time)
  tet<-length(which_time);


  Flex_const <- matrix(0,0,length(coordinates));
  Flex_const_V <- numeric(0);
  Flex_const_D <- character(0);
  Flex_const_B <- rep(F,length(coordinates));





  if (tet > 1) {
    #the transposed projector onto the flex variables of i within the subspace
    #generated by all flex, and binary flex variables
    flex <-
      id_FlexA[, grepl("^Flex_positive_t\\d", coordinates)|grepl("^Flex_negative_t\\d", coordinates)]
    #the transposed projector onto the binary flex variables of i within the subspace
    #generated by all flex, and binary flex variables
    bin <- id_FlexA[, grepl(paste0("Flex_bin"), coordinates)]

    #
    #flex is locally constant
    # flex[t] - flex[t+1] + flexMax*bin[t+1] <= flexMax
    #

    x<- t(flex[,-tet]) - t(flex[,-1]) + flexMax*t(bin[,-1])

    row.names(x)<-paste0("FlexA_const_t", which_time[-1])
    colnames(x)<-coordinates


    Flex_const <- rbind(Flex_const,x);
    Flex_const_V<-c(Flex_const_V,rep(flexMax,tet-1))
    Flex_const_D<-c(Flex_const_D,rep("<=",tet-1))
  }

  return(new("LM",
             matrix = Flex_const,
             vector = Flex_const_V,
             direction = Flex_const_D,
             binary = Flex_const_B,
             cost = rep(0,length(coordinates))
  ))
}



#'LM.ShaveDom
#'
#'for i,t: Pel_i(t) <= maxP_el:_i
#'
#'@export
#'@examples
#' coordinates <-c("P_max-el_CHP1", "P_max-el_CHP1", "Pel_CHP1_t1_","Pel_CHP1_t2_",
#'                 "Pel_CHP2_t1_","Pel_CHP2_t2_",
#'                 "a");
#'
#' which_time <-c(FALSE,TRUE);
#' nam<-c("RB_CHP1","RB_CHP2")
#'
#'
#' model <- LM.ShaveDom(coordinates,which_time,nam);
#' cbind(LM.matrix(model),LM.direction(model),LM.vector(model))


LM.ShaveDom<-function(coordinates,which_time,nam){
  if(!is.character(coordinates)){
    stop("coordinates have to be character")
  }
  if(length(coordinates)==0){
    stop("coordinates chould be of length >= 1")
  }
  if(!is.character(nam)){
    stop("nam has to be character")
  }
  if(length(nam)==0){
    stop("nam has to have length > 0")
  }
  if(!is.logical(which_time)){
    stop("which_time has to be logical")
  }

  if(length(grep("P_max-el",coordinates))!=length(nam)){
    stop("number of P_max-el coordinates does not match length nam")

  }


  #
  #the relevant steps
  #
  te <- length(which_time)
  which_time <- which(which_time);
  tet<-length(which_time);
  #
  #the matrices
  #
  shave_M <- matrix(0,0,length(coordinates));
  shave_V <- numeric(0);
  shave_D <- character(0);
  shave_B <- rep(F,length(coordinates));
  shave_cost <- rep(0,length(coordinates))
  #
  #names where to shave
  #






  for (i in nam) {
    id <- rep(0,length(coordinates))
    id[grepl(paste0("P_max-el_",str_split_fixed(i,"_",Inf)[,2]),coordinates)] <- 1;
    Shave <- kronecker(rep(1,tet),t(id));


    x <-
      t(t(P_el_plus(coordinates, i,te))[, which_time]) -
      t(t(P_el_minus(coordinates, i,te))[,which_time]) -
      Shave;


    row.names(x) <- paste0("Shave_dom_",i, "_t", which_time)

    colnames(x) <- coordinates;

    shave_M <- rbind(shave_M, x)

    shave_V <- c(shave_V, rep(0,tet))
    shave_D <- c(shave_D, rep("<=", tet))

  }



  return(new("LM",
             matrix = shave_M,
             vector = shave_V,
             direction = shave_D,
             binary = shave_B,
             cost = shave_cost))

}


#'physical
#'
#'@export
#'

minimum_assist_price<-function(sandbox){
  stopifnot(any(is(sandbox)=="Sandbox"))
  #
  #adjust prices for Flexa optimization
  #
  #
  classes <- compoClass(sandbox)

  #the non assists
  assists <- which(is.element(classes, c("Assel", "Assfuel", "Assth")))

  #set the prices for the assists

  w<-c(
    sandbox@adjacencyPrice_el[assists, ,],
    sandbox@adjacencyPrice_el[,assists ,],
    sandbox@adjacencyPrice_th[assists, ,],
    sandbox@adjacencyPrice_th[,assists ,],
    sandbox@adjacencyPrice_fuel[assists, ,],
    sandbox@adjacencyPrice_fuel[,assists ,]
  )

  x<-min(w[w!=0])


  #--------------------------------------------------------
  return(x)


}

maximum_nonassist_price<-function(sandbox){
  stopifnot(any(is(sandbox)=="Sandbox"))
  #
  #adjust prices for Flexa optimization
  #
  #
  classes <- compoClass(sandbox)

  #the non assists
  nonassists <- which(!is.element(classes, c("Assel", "Assfuel", "Assth")))

  #set the prices for the assists
  w<-c(
    sandbox@adjacencyPrice_el[nonassists, nonassists,],
    sandbox@adjacencyPrice_th[nonassists, nonassists,],
    sandbox@adjacencyPrice_fuel[nonassists, nonassists,]
  )
  x<-max(w)
  #
  #--------------------------------------------------------
  return(x)


}
