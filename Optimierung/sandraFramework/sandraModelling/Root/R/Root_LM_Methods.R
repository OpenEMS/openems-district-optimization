#---Overview-----------
# LM.E_el
# LM.E_fuel
# LM.E_th
# LM.maxE_th
# LM.maxP_el_minus
# LM.maxP_el_minus_Op
# LM.maxP_el_plus
# LM.maxP_el_plus_Op
# LM.maxP_fuel_minus
# LM.maxP_fuel_minus_Op
# LM.maxP_fuel_plus
# LM.maxP_fuel_plus_Op
# LM.maxP_th_minus
# LM.maxP_th_minus_Op
# LM.maxP_th_plus
# LM.maxP_th_plus_Op
# LM.minP_th_plus_Op
# LM.Profile_el_minus
# LM.Profile_el_plus
# LM.Profile_fuel_minus
# LM.Profile_fuel_plus
# LM.Profile_th_minus
# LM.Profile_th_plus
# LM.P_th_plus
# LM.Transform_elth_th
# LM.Transform_el_fuel
# LM.Transform_el_th
# LM.Transform_el_th_COP
# LM.Transform_fuel_el
# LM.Transform_fuel_th
# LM.Tunnel_el
# LM.Tunnel_fuel
# LM.Tunnel_th
# LM.MinDowntime
# LM.MinRuntime

#----LM.E_el-----------------------------------------------
#' LM.E_el
#'
#'@describeIn LM.E_el Energy conservation in a Root
#'@param object Root
#'@return LM
#'@export
setMethod("LM.E_el", signature(object = "Root"), function(object) {
  if (length(eff_losses(object))*
      length(effMaxP_el_minus(object))*
      length(coord(object)) > 0) {
    n <- length(coord(object))
    te <- length(timegrid(object))

    eta1 <- eff_losses(object)^{tau(object)/24}
    eta2 <- effMaxP_el_minus(object)

    #get the weights determining the flow in
    co <- coord(object)
    weight<-rep(1,n)
    if(length(object@linelossel)>0){
      weight[grepl("Pel_",co)& grepl(paste0(str_split_fixed(object@name, "_",2)[,2],"_time", sep="", collapse="|"),co)] <-kronecker(object@linelossel[object@linelossel>0], rep(1, te))
    }
    #if we  have EnergyStart we use it
if(EnergyStart(object) >= 0 ){

    V <- eta1  * EnergyStart(object) * diag(te)[1, ]

    M <-
      E_el(object) - eta1 * Tminus((E_el)(object)) - tau(object) * (eta2 * t(weight * t(P_el_minus(object))) - P_el_plus(object))






}else{
  #if EnergyStart is negative, we say E_start = E_end
  #roll E_el
  u <- E_el(object);
  u<-u[c(te,1:(te-1)),]
  #u plays the role of E_el(t-1) with E_el(0)=E_el(te)
  M <-
    E_el(object) - eta1 * u - tau(object) * (eta2 * t(weight * t(P_el_minus(object))) - P_el_plus(object))

  V<-rep(0,te)

}
    row.names(M) <-
      paste0(name(object), " E_el_time", as.character(1:te))
    #fix the Energy at the end
    if(length(EnergyEnd(object))!=0){
      if(EnergyEnd(object)>=0){
        E_end <- t(E_el(object)[te,]);
        row.names(E_end) <- paste0(name(object), " E_el_end")
        colnames(E_end) <-colnames(M)

        M <- rbind(M,
                   E_end
        );
        V <- c(V,EnergyEnd(object))

      }
    }

    C <- rep(0,n)

  m<-new(
      "LM",
      matrix = M,
      direction = rep("=", length(V)),
      vector = V,
      cost = C,
      binary = grepl("Op_", coord(object))
    )
  return(m)
  } else{
    print("Attributes are missing: eff_losses,effMaxP_el_minus and coordinates are needed")
  }
})


#----LM.E_fuel-----------------------------------------------
#' LM.E_fuel
#'
#'@describeIn LM.E_fuel Energy conservation in a Root
#'@param object Root
#'@return LM
#'@export
setMethod("LM.E_fuel", signature(object = "Root"), function(object) {
  if (length(eff_losses(object))*
      length(coord(object)) > 0) {
    n <- length(coord(object))
    te <- length(timegrid(object))

    eta1 <- eff_losses(object)^{tau(object)/24}
    # eta2 <- effMaxP_fuel_minus(object)
    eta2 <- 1
    #get the weights determining the flow in
    co <- coord(object)
    weight<-rep(1,n)
    if(length(object@linelossel)>0){
      weight[grepl("Pfuel_",co)& grepl(paste0(str_split_fixed(object@name, "_",2)[,2],"_time", sep="", collapse="|"),co)] <-kronecker(object@linelossfuel[object@linelossfuel>0], rep(1, te))
    }
    #if we  have EnergyStart we use it
    if(EnergyStart(object) >= 0 ){

      V <- eta1  * EnergyStart(object) * diag(te)[1, ]

      M <-
        E_fuel(object) - eta1 * Tminus((E_fuel)(object)) - tau(object) * (eta2 * t(weight * t(P_fuel_minus(object))) - P_fuel_plus(object))






    }else{
      #if EnergyStart is negative, we say E_start = E_end
      #roll E_fuel
      u <- E_fuel(object);
      u<-u[c(te,1:(te-1)),]
      #u plays the role of E_fuel(t-1) with E_fuel(0)=E_fuel(te)
      M <-
        E_fuel(object) - eta1 * u - tau(object) * (eta2 * t(weight * t(P_fuel_minus(object))) - P_fuel_plus(object))

      V<-rep(0,te)

    }
    row.names(M) <-
      paste0(name(object), " E_fuel_time", as.character(1:te))
    #fix the Energy at the end
    if(length(EnergyEnd(object))!=0){
      if(EnergyEnd(object)>=0){
        E_end <- t(E_fuel(object)[te,]);
        row.names(E_end) <- paste0(name(object), " E_fuel_end")
        
        M <- rbind(M,
                   E_end
        );
        V <- c(V,EnergyEnd(object))

      }
    }

    C <- rep(0,n)

   m<- new(
      "LM",
      matrix = M,
      direction = rep("=", length(V)),
      vector = V,
      cost = C,
      binary = grepl("Op_", coord(object))
    )
  return(m)
  } else{
    print("Attributes are missing: eff_losses and coordinates are needed")
  }
})


#----LM.E_th-----------------------------------------------
#' LM.E_th
#'
#'@describeIn LM.E_th Energy conservation in a Root
#'@param object Root
#'@return LM
#'@export
setMethod("LM.E_th", signature(object = "Root"), function(object) {
  if (length(eff_losses(object))*
      length(effMaxP_th_minus(object))*
      length(coord(object)) > 0) {
    n <- length(coord(object))
    te <- length(timegrid(object))

    eta1 <- eff_losses(object)^{tau(object)/24}
    eta2 <- effMaxP_th_minus(object)

    #get the weights determining the flow in
    co <- coord(object)
    weight<-rep(1,n)
    if(length(object@linelossel)>0){
      weight[grepl("Pth_",co)& grepl(paste0(str_split_fixed(object@name, "_",2)[,2],"_time", sep="", collapse="|"),co)] <-kronecker(object@linelossth[object@linelossth>0], rep(1, te))
    }
    #if we  have EnergyStart we use it
    if(EnergyStart(object) >= 0 ){

      V <- eta1  * EnergyStart(object) * diag(te)[1, ]

      M <-
        E_th(object) - eta1 * Tminus((E_th)(object)) - tau(object) * (eta2 * t(weight * t(P_th_minus(object))) - P_th_plus(object))






    }else{
      #if EnergyStart is negative, we say E_start = E_end
      #roll E_fuel
      u <- E_th(object);
      u<-u[c(te,1:(te-1)),]
      #u plays the role of E_th(t-1) with E_th(0)=E_th(te)
      M <-
        E_th(object) - eta1 * u - tau(object) * (eta2 * t(weight * t(P_th_minus(object))) - P_th_plus(object))

      V<-rep(0,te)

    }
    row.names(M) <-
      paste0(name(object), " E_th_time", as.character(1:te))
    #fix the Energy at the end
    if(length(EnergyEnd(object))!=0){
      if(EnergyEnd(object)>=0){
        E_end <- t(E_th(object)[te,]);
        row.names(E_end) <- paste0(name(object), " E_th_end")
        colnames(E_end) <-colnames(M)

        M <- rbind(M,
                   E_end
        );
        V <- c(V,EnergyEnd(object))

      }
    }

    C <- rep(0,n)

    m<-new(
      "LM",
      matrix = M,
      direction = rep("=", dim(M)[1]),
      vector = V,
      cost = C,
      binary = grepl("Op_", coord(object))
    )
  return(m)
  } else{
    print("Attributes are missing: eff_losses and coordinates are needed")
  }
})
#----LM.maxE_th--------------------------------------------
#'LM.maxE_th
#'
#'Build the constraint LM fo maximal thermal capacity of an thermal storage in kWh
#'
#'@export

setMethod("LM.maxE_th", signature(object = "Root"), function(object) {
  if (length(maxE_th(object)) * length(coord(object)) > 0) {
    E_max <- maxE_th(object)

    te <- length(timegrid(object))

    n <- length(coord(object))

    energyMaxTh <- list()

    energyMaxTh$M <- E_th(object)

    colnames(energyMaxTh$M) <- coord(object)

    row.names(energyMaxTh$M) <-
      paste0(name(object), " maxE_th_time", as.character(1:te))

    energyMaxTh$V <- rep(E_max, te)

    energyMaxTh$D <- rep("<=", te)

    m <-
      new(
        "LM",
        matrix = energyMaxTh$M,
        vector = energyMaxTh$V,
        direction = energyMaxTh$D,
        cost = rep(0, n),
        binary = grepl("Op_", coord(object))
      )


    return(m)
  } else{
    print("Attributes are missing:  maxE_th, coordinates are needed")
  }
})

#----LM.maxE_el--------------------------------------------
#'LM.maxE_el
#'
#'Build the constraint LM fo maximal thermal capacity of an thermal storage in kWh
#'
#'@export

setMethod("LM.maxE_el", signature(object = "Root"), function(object) {
  if (length(maxE_el(object)) * length(coord(object)) > 0) {
    E_max <- maxE_el(object)

    te <- length(timegrid(object))

    n <- length(coord(object))

    energyMaxEl <- list()

    energyMaxEl$M <- E_el(object)

    colnames(energyMaxEl$M) <- coord(object)

    row.names(energyMaxEl$M) <-
      paste0(name(object), " maxE_el_time", as.character(1:te))

    energyMaxEl$V <- rep(E_max, te)

    energyMaxEl$D <- rep("<=", te)

    m <-
      new(
        "LM",
        matrix = energyMaxEl$M,
        vector = energyMaxEl$V,
        direction = energyMaxEl$D,
        cost = rep(0, n),
        binary = grepl("Op_", coord(object))
      )


    return(m)
  } else{
    print("Attributes are missing:  maxE_el, coordinates are needed")
  }
})
#----LM.maxE_fuel--------------------------------------------
#'LM.maxE_fuel
#'
#'Build the constraint LM fo maximal fuel capacity of an storage in kWh
#'
#'@export

setMethod("LM.maxE_fuel", signature(object = "Root"), function(object) {
  if (length(maxE_fuel(object)) * length(coord(object)) > 0) {
    E_max <- maxE_fuel(object)

    te <- length(timegrid(object))

    n <- length(coord(object))

    energyMaxEl <- list()

    energyMaxEl$M <- E_fuel(object)

    colnames(energyMaxEl$M) <- coord(object)

    row.names(energyMaxEl$M) <-
      paste0(name(object), " maxE_fuel_time", as.character(1:te))

    energyMaxEl$V <- rep(E_max, te)

    energyMaxEl$D <- rep("<=", te)

    m <-
      new(
        "LM",
        matrix = energyMaxEl$M,
        vector = energyMaxEl$V,
        direction = energyMaxEl$D,
        cost = rep(0, n),
        binary = grepl("Op_", coord(object))
      )


    return(m)
  } else{
    print("Attributes are missing:  maxE_fuel, coordinates are needed")
  }
})
#----LM.maxP_el_minus---------------------------------------
#'LM.maxP_el_minus
#'
#'Build the constraint LM fo maximal electrical input power in kW
#'
#'@export

setMethod("LM.maxP_el_minus", signature(object = "Root"), function(object) {
  if (length(maxP_el_minus(object)) * length(coord(object)) > 0) {
  if(maxP_el_minus(object)!=-1){
    P_max <- maxP_el_minus(object)

    te <- length(timegrid(object))

    n <- length(coord(object))

    powerMax <- list()

    powerMax$M <- P_el_minus(object)

    colnames(powerMax$M) <- coord(object)

    row.names(powerMax$M) <-
      paste0(name(object), " maxP_el_minus_time", as.character(1:te))

    powerMax$V <- rep(P_max, te)

    powerMax$D <- rep("<=", te)

    m <-
      new(
        "LM",
        matrix = powerMax$M,
        vector = powerMax$V,
        direction = powerMax$D,
        cost = rep(0, n),
        binary = grepl("Op_", coord(object))
      )


    return(m)
  }
  } else{
    print("Attributes are missing:  maxP_el_minus, coordinates are needed")
  }
})

#----LM.maxP_el---------------------------------------
#'LM.maxP_el
#'
#'Build the constraint LM for maximal electrical flow in kW. Neccessary for batteries
#'
#'@export

setMethod("LM.maxP_el", signature(object = "Root"), function(object) {
  if (length(maxP_el_minus(object)) * length(coord(object)) > 0 |
      length(maxP_el_plus(object)) * length(coord(object)) > 0) {

    if((maxP_el_minus(object) >= 0 | !is.null(maxP_el_minus(object)) ) &
       (maxP_el_plus(object) >= 0 | !is.null(maxP_el_plus(object)) )){
      P_minus <- maxP_el_minus(object)
      P_plus <- maxP_el_plus(object)

      te <- length(timegrid(object))

      n <- length(coord(object))

      powerMax <- list()

      powerMax$M <- P_el_minus(object) + P_el_plus(object)

      colnames(powerMax$M) <- coord(object)

      row.names(powerMax$M) <-
        paste0(name(object), " maxP_el_time", as.character(1:te))

      powerMax$V <- rep(max(c(P_minus,P_plus)), te)

      powerMax$D <- rep("<=", te)

      m <-
        new(
          "LM",
          matrix = powerMax$M,
          vector = powerMax$V,
          direction = powerMax$D,
          cost = rep(0, n),
          binary = grepl("Op_", coord(object))
        )


      return(m)
    }
  } else{
    print("Attributes are missing:  maxP_el_minus/plus, coordinates are needed")
  }
})

#----LM.maxP_fuel---------------------------------------
#'LM.maxP_fuel
#'
#'Build the constraint LM for maximal electrical flow in kW. Neccessary for batteries
#'
#'@export

setMethod("LM.maxP_fuel", signature(object = "Root"), function(object) {
  if (length(maxP_fuel_minus(object)) * length(coord(object)) > 0 |
      length(maxP_fuel_plus(object)) * length(coord(object)) > 0) {

    if((maxP_fuel_minus(object) >= 0 | !is.null(maxP_fuel_minus(object)) ) &
       (maxP_fuel_plus(object) >= 0 | !is.null(maxP_fuel_plus(object)) )){
      P_minus <- maxP_fuel_minus(object)
      P_plus <- maxP_fuel_plus(object)

      te <- length(timegrid(object))

      n <- length(coord(object))

      powerMax <- list()

      powerMax$M <- P_fuel_minus(object) + P_fuel_plus(object)

      colnames(powerMax$M) <- coord(object)

      row.names(powerMax$M) <-
        paste0(name(object), " maxP_fuel_time", as.character(1:te))

      powerMax$V <- rep(max(c(P_minus,P_plus)), te)

      powerMax$D <- rep("<=", te)

      m <-
        new(
          "LM",
          matrix = powerMax$M,
          vector = powerMax$V,
          direction = powerMax$D,
          cost = rep(0, n),
          binary = grepl("Op_", coord(object))
        )


      return(m)
    }
  } else{
    print("Attributes are missing:  maxP_fuel_minus/plus, coordinates are needed")
  }
})


#----LM.maxP_th---------------------------------------
#'LM.maxP_th
#'
#'Build the constraint LM for maximal thermal flow in kW. Neccessary for batteries
#'
#'@export

setMethod("LM.maxP_th", signature(object = "Root"), function(object) {
  if (length(maxP_th_minus(object)) * length(coord(object)) > 0 |
      length(maxP_th_plus(object)) * length(coord(object)) > 0) {

    if((maxP_th_minus(object) >= 0 | !is.null(maxP_th_minus(object)) ) &
       (maxP_th_plus(object) >= 0 | !is.null(maxP_th_plus(object)) )){
      P_minus <- maxP_th_minus(object)
      P_plus <- maxP_th_plus(object)

      te <- length(timegrid(object))

      n <- length(coord(object))

      powerMax <- list()

      powerMax$M <- P_th_minus(object) + P_th_plus(object)

      colnames(powerMax$M) <- coord(object)

      row.names(powerMax$M) <-
        paste0(name(object), " maxP_th_time", as.character(1:te))

      powerMax$V <- rep(max(c(P_minus,P_plus)), te)

      powerMax$D <- rep("<=", te)

      m <-
        new(
          "LM",
          matrix = powerMax$M,
          vector = powerMax$V,
          direction = powerMax$D,
          cost = rep(0, n),
          binary = grepl("Op_", coord(object))
        )


      return(m)
    }
  } else{
    print("Attributes are missing:  maxP_th_minus/plus, coordinates are needed")
  }
})

#----LM.maxP_el_minus_Op------------------------------------
#'LM.maxP_el_minus_Op
#'
#'Build the constraint LM fo maximal electrical input power in kW. Here an
#'additional variable "Op" is included to be able to simulate a constraint
#'that there is only an input if a variable of an other object is on. That
#'is used for example in the package "WaDo" (WatchDog)
#'
#'@export
setMethod("LM.maxP_el_minus_Op", signature(object = "Root"), function(object) {
  if (length(maxP_el_minus(object)) * length(coord(object)) > 0) {
    if(maxP_el_minus(object)!=-1){
    P_max <- maxP_el_minus(object)

    te <- length(timegrid(object))

    n <- length(coord(object))

    powerMax <- list()

    powerMax$M <- P_el_minus(object) - Op(object) * P_max

    colnames(powerMax$M) <- coord(object)

    row.names(powerMax$M) <-
      paste0(name(object), " maxP_el_minus_Op_time", as.character(1:te))

    powerMax$V <- rep(0, te)

    powerMax$D <- rep("<=", te)

    m <-
      new(
        "LM",
        matrix = powerMax$M,
        vector = powerMax$V,
        direction = powerMax$D,
        cost = rep(0, n),
        binary = grepl("Op_", coord(object))
      )


    return(m)
    }
  } else{
    print("Attributes are missing: maxP_el_minus, coordinates are needed")
  }
})
#----LM.maxP_el_plus---------------------------------------
#'LM.maxP_el_plus
#'
#'Build the constraint LM fo maximal electrical output power in kW
#'
#'@export
setMethod("LM.maxP_el_plus", signature(object = "Root"), function(object) {
  if (length(maxP_el_plus(object)) * length(coord(object)) > 0) {
    if(maxP_el_plus(object)!=-1){
    P_max <- maxP_el_plus(object)

    te <- length(timegrid(object))

    n <- length(coord(object))

    powerMax <- list()

    powerMax$M <- P_el_plus(object)

    colnames(powerMax$M) <- coord(object)

    row.names(powerMax$M) <-
      paste0(name(object), " maxP_el_plus_time", as.character(1:te))

    powerMax$V <- rep(P_max, te)

    powerMax$D <- rep("<=", te)

    m <-
      new(
        "LM",
        matrix = powerMax$M,
        vector = powerMax$V,
        direction = powerMax$D,
        cost = rep(0, n),
        binary = grepl("Op_", coord(object))
      )


    return(m)}
  } else{
    print("Attributes are missing: maxP_el_plus, coordinates are needed")
  }
})

#----LM.maxP_el_plus_Op------------------------------------
#'LM.maxP_el_plus_Op
#'
#'Build the constraint LM fo maximal electrical input power in kW. Here an
#'additional variable "Op" is included to be able to simulate a constraint
#'that there is only an input if a variable of an other object is on. That
#'is used for example in the package "WaDo" (WatchDog)
#'
#'@export
setMethod("LM.maxP_el_plus_Op", signature(object = "Root"), function(object) {
  if (length(maxP_el_plus(object)) * length(coord(object)) > 0) {
    if(maxP_el_plus(object)!=-1){
    P_max <- maxP_el_plus(object)

    te <- length(timegrid(object))

    n <- length(coord(object))

    powerMax <- list()

    powerMax$M <- P_el_plus(object) - Op(object) * P_max

    colnames(powerMax$M) <- coord(object)

    row.names(powerMax$M) <-
      paste0(name(object), " maxP_el_plus_Op_time", as.character(1:te))

    powerMax$V <- rep(0, te)

    powerMax$D <- rep("<=", te)

    m <-
      new(
        "LM",
        matrix = powerMax$M,
        vector = powerMax$V,
        direction = powerMax$D,
        cost = rep(0, n),
        binary = grepl("Op_", coord(object))
      )


    return(m)}
  } else{
    print("Attributes are missing: maxP_el_plus, coordinates are needed")
  }
})
#----LM.minP_el_plus---------------------------------------
#'LM.minP_el_plus
#'
#'Build the constraint LM fo minimal electrical output power in kW
#'
#'@export
setMethod("LM.minP_el_plus", signature(object = "Root"), function(object) {
  if (length(minP_el_plus(object)) * length(coord(object)) > 0) {
    if(minP_el_plus(object)>0){
    P_min <- minP_el_plus(object)

    te <- length(timegrid(object))

    n <- length(coord(object))

    powerMin <- list()

    powerMin$M <- -P_el_plus(object)

    colnames(powerMin$M) <- coord(object)

    row.names(powerMin$M) <-
      paste0(name(object), " minP_el_plus_time", as.character(1:te))

    powerMin$V <- rep(-P_min, te)

    powerMin$D <- rep("<=", te)

    m <-
      new(
        "LM",
        matrix = powerMin$M,
        vector = powerMin$V,
        direction = powerMin$D,
        cost = rep(0, n),
        binary = grepl("Op_", coord(object))
      )


    return(m)}
  } else{
    print("Attributes are missing: minP_el_plus, coordinates are needed")
  }
})

#----LM.minP_el_plus_Op------------------------------------
#'LM.minP_el_plus_Op
#'
#'Build the constraint LM fo minimal electrical input power in kW. Here an
#'additional variable "Op" is included to be able to simulate a constraint
#'that there is only an input if a variable of an other object is on. That
#'is used for example in the package "WaDo" (WatchDog)
#'
#'@export
setMethod("LM.minP_el_plus_Op", signature(object = "Root"), function(object) {
  if (length(minP_el_plus(object)) * length(coord(object)) > 0) {
    if(minP_el_minus(object)>0){
    P_min <- minP_el_plus(object)

    te <- length(timegrid(object))

    n <- length(coord(object))

    powermin <- list()

    powermin$M <- - P_el_plus(object) + Op(object) * P_min

    colnames(powermin$M) <- coord(object)

    row.names(powermin$M) <-
      paste0(name(object), " minP_el_plus_Op_time", as.character(1:te))

    powermin$V <- rep(0, te)

    powermin$D <- rep("<=", te)

    m <-
      new(
        "LM",
        matrix = powermin$M,
        vector = powermin$V,
        direction = powermin$D,
        cost = rep(0, n),
        binary = grepl("Op_", coord(object))
      )


    return(m)
    }
  } else{
    print("Attributes are missing: minP_el_plus, coordinates are needed")
  }
})
#----LM.minP_el_minus---------------------------------------
#'LM.minP_el_minus
#'
#'Build the constraint LM fo minimal electrical output power in kW
#'
#'@export
setMethod("LM.minP_el_minus", signature(object = "Root"), function(object) {
  if (length(minP_el_minus(object)) * length(coord(object)) > 0) {
    if(minP_el_minus(object)>0){
    P_min <- minP_el_minus(object)

    te <- length(timegrid(object))

    n <- length(coord(object))

    powerMin <- list()

    powerMin$M <- -P_el_minus(object)

    colnames(powerMin$M) <- coord(object)

    row.names(powerMin$M) <-
      paste0(name(object), " minP_el_minus_time", as.character(1:te))

    powerMin$V <- rep(-P_min, te)

    powerMin$D <- rep("<=", te)

    m <-
      new(
        "LM",
        matrix = powerMin$M,
        vector = powerMin$V,
        direction = powerMin$D,
        cost = rep(0, n),
        binary = grepl("Op_", coord(object))
      )


    return(m)}
  } else{
    print("Attributes are missing: minP_el_minus, coordinates are needed")
  }
})


#----LM.minP_el_minus_Op------------------------------------
#'LM.minP_el_minus_Op
#'
#'Build the constraint LM fo minimal electrical input power in kW. Here an
#'additional variable "Op" is included to be able to simulate a constraint
#'that there is only an input if a variable of an other object is on. That
#'is used for example in the package "WaDo" (WatchDog)
#'
#'@export
setMethod("LM.minP_el_minus_Op", signature(object = "Root"), function(object) {
  if (length(minP_el_minus(object)) * length(coord(object)) > 0) {
    if(minP_el_minus(object)>0){
    P_min <- minP_el_minus(object)

    te <- length(timegrid(object))

    n <- length(coord(object))

    powermin <- list()

    powermin$M <- - P_el_minus(object) + Op(object) * P_min

    colnames(powermin$M) <- coord(object)

    row.names(powermin$M) <-
      paste0(name(object), " minP_el_minus_Op_time", as.character(1:te))

    powermin$V <- rep(0, te)

    powermin$D <- rep("<=", te)

    m <-
      new(
        "LM",
        matrix = powermin$M,
        vector = powermin$V,
        direction = powermin$D,
        cost = rep(0, n),
        binary = grepl("Op_", coord(object))
      )


    return(m)}
  } else{
    print("Attributes are missing: minP_el_minus, coordinates are needed")
  }
})



#----LM.maxP_fuel_plus---------------------------------------
#'LM.maxP_fuel_plus
#'
#'Build the constraint LM fo maximal fuel output power in kW
#'
#'@export
setMethod("LM.maxP_fuel_plus", signature(object = "Root"), function(object) {
  if (length(maxP_fuel_plus(object)) * length(coord(object)) > 0) {


    
    if(maxP_fuel_plus(object)!=-1){
    P_max <- maxP_fuel_plus(object)

    te <- length(timegrid(object))

    n <- length(coord(object))

    fuelMax <- list()

    fuelMax$M <- P_fuel_plus(object)

    colnames(fuelMax$M) <- coord(object)

    row.names(fuelMax$M) <-
      paste0(name(object), " maxP_fuel_plus_time", as.character(1:te))

    fuelMax$V <- rep(P_max, te)

    fuelMax$D <- rep("<=", te)

    m <-
      new(
        "LM",
        matrix = fuelMax$M,
        vector = fuelMax$V,
        direction = fuelMax$D,
        cost = rep(0, n),
        binary = grepl("Op_", coord(object))
      )


    return(m)}
  } else{
    print("Attributes are missing: maxP_fuel_plus, coordinates are needed")
  }
})

#----LM.minP_fuel_plus_Op------------------------------------
#'LM.minP_fuel_plus_Op
#'
#'Build the constraint LM fo minimal fuel input power in kW. Here an
#'additional variable "Op" is included to be able to simulate a constraint
#'that there is only an input if a variable of an other object is on. That
#'is used for example in the package "WaDo" (WatchDog)
#'
#'@export
setMethod("LM.minP_fuel_plus_Op", signature(object = "Root"), function(object) {
  if (length(minP_fuel_plus(object)) * length(coord(object)) > 0) {
    if(minP_fuel_plus(object)>0){
    P_min <- minP_fuel_plus(object)

    te <- length(timegrid(object))

    n <- length(coord(object))

    fuelmin <- list()

    fuelmin$M <- -P_fuel_plus(object) + Op(object) * P_min

    colnames(fuelmin$M) <- coord(object)

    row.names(fuelmin$M) <-
      paste0(name(object), " minP_fuel_plus_Op_time", as.character(1:te))

    fuelmin$V <- rep(0, te)

    fuelmin$D <- rep("<=", te)

    m <-
      new(
        "LM",
        matrix = fuelmin$M,
        vector = fuelmin$V,
        direction = fuelmin$D,
        cost = rep(0, n),
        binary = grepl("Op_", coord(object))
      )


    return(m)}
  } else{
    print("Attributes are missing: minP_fuel_plus, coordinates are needed")
  }
})
#----LM.maxP_fuel_plus_Op------------------------------------
#'LM.maxP_fuel_plus_Op
#'
#'Build the constraint LM fo maximal fuel input power in kW. Here an
#'additional variable "Op" is included to be able to simulate a constraint
#'that there is only an input if a variable of an other object is on. That
#'is used for example in the package "WaDo" (WatchDog)
#'
#'@export
setMethod("LM.maxP_fuel_plus_Op", signature(object = "Root"), function(object) {
  if (length(maxP_fuel_plus(object)) * length(coord(object)) > 0) {
    if(maxP_fuel_plus(object)!=-1){
    P_max <- maxP_fuel_plus(object)

    te <- length(timegrid(object))

    n <- length(coord(object))

    fuelMax <- list()

    fuelMax$M <- P_fuel_plus(object) - Op(object) * P_max

    colnames(fuelMax$M) <- coord(object)

    row.names(fuelMax$M) <-
      paste0(name(object), " maxP_fuel_plus_Op_time", as.character(1:te))

    fuelMax$V <- rep(0, te)

    fuelMax$D <- rep("<=", te)

    m <-
      new(
        "LM",
        matrix = fuelMax$M,
        vector = fuelMax$V,
        direction = fuelMax$D,
        cost = rep(0, n),
        binary = grepl("Op_", coord(object))
      )


    return(m)}
  } else{
    print("Attributes are missing: maxP_fuel_plus, coordinates are needed")
  }
})
#----LM.maxP_fuel_minus---------------------------------------
#'LM.maxP_fuel_minus
#'
#'Build the constraint LM fo maximal fuel input power in kW
#'
#'@export

setMethod("LM.maxP_fuel_minus", signature(object = "Root"), function(object) {
  if (length(maxP_fuel_minus(object)) * length(coord(object)) > 0) {
    if(maxP_fuel_minus(object)!=-1){
    P_max <- maxP_fuel_minus(object)

    te <- length(timegrid(object))

    n <- length(coord(object))

    fuelMax <- list()

    fuelMax$M <- P_fuel_minus(object)

    colnames(fuelMax$M) <- coord(object)

    row.names(fuelMax$M) <-
      paste0(name(object), " maxP_fuel_minus_time", as.character(1:te))

    fuelMax$V <- rep(P_max, te)

    fuelMax$D <- rep("<=", te)

    m <-
      new(
        "LM",
        matrix = fuelMax$M,
        vector = fuelMax$V,
        direction = fuelMax$D,
        cost = rep(0, n),
        binary = grepl("Op_", coord(object))
      )


    return(m)}
  } else{
    print("Attributes are missing: maxP_fuel_minus, coordinates are needed")
  }
})
#----LM.maxP_fuel_minus_Op------------------------------------
#'LM.maxP_fuel_minus_Op
#'
#'Build the constraint LM fo maximal fuel input power in kW. Here an
#'additional variable "Op" is included to be able to simulate a constraint
#'that there is only an input if a variable of an other object is on. That
#'is used for example in the package "WaDo" (WatchDog)
#'
#'@export
setMethod("LM.maxP_fuel_minus_Op", signature(object = "Root"), function(object) {
  if (length(maxP_fuel_minus(object)) * length(coord(object)) > 0) {
    if(maxP_fuel_minus(object)!=-1){
    P_max <- maxP_fuel_minus(object)

    te <- length(timegrid(object))

    n <- length(coord(object))

    fuelMax <- list()

    fuelMax$M <- P_fuel_minus(object) - Op(object) * P_max

    colnames(fuelMax$M) <- coord(object)

    row.names(fuelMax$M) <-
      paste0(name(object), " maxP_fuel_minus_Op_time", as.character(1:te))

    fuelMax$V <- rep(0, te)

    fuelMax$D <- rep("<=", te)

    m <-
      new(
        "LM",
        matrix = fuelMax$M,
        vector = fuelMax$V,
        direction = fuelMax$D,
        cost = rep(0, n),
        binary = grepl("Op_", coord(object))
      )


    return(m)}
  } else{
    print("Attributes are missing: maxP_fuel_minus, coordinates are needed")
  }
})

#----LM.minP_fuel_minus_Op------------------------------------
#'LM.minP_fuel_minus_Op
#'
#'Build the constraint LM fo minimal fuel input power in kW. Here an
#'additional variable "Op" is included to be able to simulate a constraint
#'that there is only an input if a variable of an other object is on. That
#'is used for example in the package "WaDo" (WatchDog)
#'
#'@export
setMethod("LM.minP_fuel_minus_Op", signature(object = "Root"), function(object) {
  if (length(minP_fuel_minus(object)) * length(coord(object)) > 0) {
    if(minP_fuel_minus(object)>0){
    P_min <- minP_fuel_minus(object)

    te <- length(timegrid(object))

    n <- length(coord(object))

    fuelmin <- list()

    fuelmin$M <- -P_fuel_minus(object) + Op(object) * P_min

    colnames(fuelmin$M) <- coord(object)

    row.names(fuelmin$M) <-
      paste0(name(object), " minP_fuel_minus_Op_time", as.character(1:te))

    fuelmin$V <- rep(0, te)

    fuelmin$D <- rep("<=", te)

    m <-
      new(
        "LM",
        matrix = fuelmin$M,
        vector = fuelmin$V,
        direction = fuelmin$D,
        cost = rep(0, n),
        binary = grepl("Op_", coord(object))
      )


    return(m)}
  } else{
    print("Attributes are missing: minP_fuel_minus, coordinates are needed")
  }
})

#----LM.maxP_th_minus---------------------------------------
#'LM.maxP_th_minus
#'
#'Build the constraint LM fo maximal thermal input power in kW
#'
#'@export

setMethod("LM.maxP_th_minus", signature(object = "Root"), function(object) {
  if (length(maxP_th_minus(object)) * length(coord(object)) > 0) {
    if(maxP_th_minus(object)!=-1){
    P_max <- maxP_th_minus(object)

    te <- length(timegrid(object))

    n <- length(coord(object))

    thermalMax <- list()

    thermalMax$M <- P_th_minus(object)

    colnames(thermalMax$M) <- coord(object)

    row.names(thermalMax$M) <-
      paste0(name(object), " maxP_th_minus_time", as.character(1:te))

    thermalMax$V <- rep(P_max, te)

    thermalMax$D <- rep("<=", te)

    m <-
      new(
        "LM",
        matrix = thermalMax$M,
        vector = thermalMax$V,
        direction = thermalMax$D,
        cost = rep(0, n),
        binary = grepl("Op_", coord(object))
      )


    return(m)}
  } else{
    print("Attributes are missing: maxP_th_minus, coordinates are needed")
  }
})
#----LM.maxP_th_minus_Op------------------------------------
#'LM.maxP_th_minus_Op
#'
#'Build the constraint LM fo maximal thermal input power in kW. Here an
#'additional variable "Op" is included to be able to simulate a constraint
#'that there is only an input if a variable of an other object is on. That
#'is used for example in the package "WaDo" (WatchDog)
#'
#'@export
setMethod("LM.maxP_th_minus_Op", signature(object = "Root"), function(object) {
  if (length(maxP_th_minus(object)) * length(coord(object)) > 0) {
    if(maxP_th_minus(object)!=-1){
    P_max <- maxP_th_minus(object)

    te <- length(timegrid(object))

    n <- length(coord(object))

    heatMax <- list()

    heatMax$M <- P_th_minus(object) - Op(object) * P_max

    colnames(heatMax$M) <- coord(object)

    row.names(heatMax$M) <-
      paste0(name(object), " maxP_th_minus_Op_time", as.character(1:te))

    heatMax$V <- rep(0, te)

    heatMax$D <- rep("<=", te)

    m <-
      new(
        "LM",
        matrix = heatMax$M,
        vector = heatMax$V,
        direction = heatMax$D,
        cost = rep(0, n),
        binary = grepl("Op_", coord(object))
      )


    return(m)}
  } else{
    print("Attributes are missing: maxP_th_minus, coordinates are needed")
  }
})
#----LM.minP_th_minus_Op------------------------------------
#'LM.minP_th_minus_Op
#'
#'Build the constraint LM fo minimal thermal input power in kW. Here an
#'additional variable "Op" is included to be able to simulate a constraint
#'that there is only an input if a variable of an other object is on. That
#'is used for example in the package "WaDo" (WatchDog)
#'
#'@export
setMethod("LM.minP_th_minus_Op", signature(object = "Root"), function(object) {
  if (length(minP_th_minus(object)) * length(coord(object)) > 0) {
    if(minP_th_minus(object)>0){
    P_min <- minP_th_minus(object)

    te <- length(timegrid(object))

    n <- length(coord(object))

    heatmin <- list()

    heatmin$M <- -P_th_minus(object) + Op(object) * P_min

    colnames(heatmin$M) <- coord(object)

    row.names(heatmin$M) <-
      paste0(name(object), " minP_th_minus_Op_time", as.character(1:te))

    heatmin$V <- rep(0, te)

    heatmin$D <- rep("<=", te)

    m <-
      new(
        "LM",
        matrix = heatmin$M,
        vector = heatmin$V,
        direction = heatmin$D,
        cost = rep(0, n),
        binary = grepl("Op_", coord(object))
      )


    return(m)}
  } else{
    print("Attributes are missing: minP_th_minus, coordinates are needed")
  }
})
#----LM.maxP_th_plus---------------------------------------
#'LM.maxP_th_plus
#'
#'Build the constraint LM fo maximal thermal output power in kW
#'
#'@export
setMethod("LM.maxP_th_plus", signature(object = "Root"), function(object) {
  if (length(maxP_th_plus(object)) * length(coord(object)) > 0) {
    if(maxP_th_plus(object)!=-1){
    P_max <- maxP_th_plus(object)

    te <- length(timegrid(object))

    n <- length(coord(object))

    thermalMax <- list()

    thermalMax$M <- P_th_plus(object)

    colnames(thermalMax$M) <- coord(object)

    row.names(thermalMax$M) <-
      paste0(name(object), " maxP_th_plus_time", as.character(1:te))

    thermalMax$V <- rep(P_max, te)

    thermalMax$D <- rep("<=", te)

    m <-
      new(
        "LM",
        matrix = thermalMax$M,
        vector = thermalMax$V,
        direction = thermalMax$D,
        cost = rep(0, n),
        binary = grepl("Op_", coord(object))
      )


    return(m)}
  } else{
    print("Attributes are missing: maxP_th_plus, coordinates are needed")
  }
})


#----LM.maxP_th_plus_nominal---------------------------------------
#'LM.maxP_th_plus_nominal
#'
#'Build the constraint LM fo maximal thermal output power in kW based on nominal thermal output
#'
#'@export
setMethod("LM.maxP_th_plus_nominal", signature(object = "Root"), function(object) {
  if (length(maxP_th_plus(object))*length(nominalP_th_plus(object)) * length(coord(object)) > 0) {
    if(maxP_th_plus(object)!=-1){
      P_max <- maxP_th_plus(object)*nominalP_th_plus(objet)
      
      te <- length(timegrid(object))
      
      n <- length(coord(object))
      
      thermalMax <- list()
      
      thermalMax$M <- P_th_plus(object)
      
      colnames(thermalMax$M) <- coord(object)
      
      row.names(thermalMax$M) <-
        paste0(name(object), " maxP_th_plus_time", as.character(1:te))
      
      thermalMax$V <- rep(P_max, te)
      
      thermalMax$D <- rep("<=", te)
      
      m <-
        new(
          "LM",
          matrix = thermalMax$M,
          vector = thermalMax$V,
          direction = thermalMax$D,
          cost = rep(0, n),
          binary = grepl("Op_", coord(object))
        )
      
      
      return(m)}
  } else{
    print("Attributes are missing: maxP_th_plus, coordinates are needed")
  }
})




#----LM.maxP_th_plus_Op------------------------------------
#'LM.maxP_th_plus_Op
#'
#'Build the constraint LM fo maximal thermal input power in kW. Here an
#'additional variable "Op" is included to be able to simulate a constraint
#'that there is only an input if a variable of an other object is on. That
#'is used for example in the package "WaDo" (WatchDog)
#'
#'@export
setMethod("LM.maxP_th_plus_Op", signature(object = "Root"), function(object) {
  if (length(maxP_th_plus(object)) * length(coord(object)) > 0) {
    if(maxP_th_plus(object)!=-1){
    P_max <- maxP_th_plus(object)

    te <- length(timegrid(object))

    n <- length(coord(object))

    heatMax <- list()

    heatMax$M <- P_th_plus(object) - Op(object) * P_max

    colnames(heatMax$M) <- coord(object)

    row.names(heatMax$M) <-
      paste0(name(object), " maxP_th_plus_Op_time", as.character(1:te))

    heatMax$V <- rep(0, te)

    heatMax$D <- rep("<=", te)

    m <-
      new(
        "LM",
        matrix = heatMax$M,
        vector = heatMax$V,
        direction = heatMax$D,
        cost = rep(0, n),
        binary = grepl("Op_", coord(object))
      )


    return(m)}
  } else{
    print("Attributes are missing: maxP_th_plus, coordinates are needed")
  }
})
#----LM.minP_th_plus_Op------------------------------------
#'LM.minP_th_plus_Op
#'
#'Build the constraint LM fo minimal thermal output power in kW. Here an additional
#'variable "Op" is included to be able to simulate objects which have not a
#'continuous production from zero to full load. For example a CHP can be turned on
#'and off. If it is on the continuous production is between 50 \% and 100 \%. Below
#'50 \% there is no production.
#'
#'@export
setMethod("LM.minP_th_plus_Op", signature(object = "Root"), function(object) {
  if (length(minP_th_plus(object)) * length(coord(object)) > 0) {
    if(minP_th_plus(object)>0){
    P_min <- minP_th_plus(object)

    te <- length(timegrid(object))

    n <- length(coord(object))

    heatmin <- list()

    heatmin$M <- - P_th_plus(object)+ Op(object) * P_min

    colnames(heatmin$M) <- coord(object)

    row.names(heatmin$M) <-
      paste0(name(object), " minP_th_plus_Op_time", as.character(1:te))

    heatmin$V <- rep(0, te)

    heatmin$D <- rep("<=", te)

    m <-
      new(
        "LM",
        matrix = heatmin$M,
        vector = heatmin$V,
        direction = heatmin$D,
        cost = rep(0, n),
        binary = grepl("Op_", coord(object))
      )


    return(m)}
  } else{
    print("Attributes are missing: minP_th_plus, coordinates are needed")
  }
})
#----LM.minP_fuel_plus---------------------------------------
#'LM.minP_fuel_plus
#'
#'Build the constraint LM fo minimal fuel output power in kW
#'
#'@export
setMethod("LM.minP_fuel_plus", signature(object = "Root"), function(object) {
  if (length(minP_fuel_plus(object)) * length(coord(object)) > 0) {
    if(minP_fuel_plus(object)>0){
    P_min <- minP_fuel_plus(object)

    te <- length(timegrid(object))

    n <- length(coord(object))

    powerMin <- list()

    powerMin$M <- -P_fuel_plus(object)

    colnames(powerMin$M) <- coord(object)

    row.names(powerMin$M) <-
      paste0(name(object), " minP_fuel_plus_time", as.character(1:te))

    powerMin$V <- rep(-P_min, te)

    powerMin$D <- rep("<=", te)

    m <-
      new(
        "LM",
        matrix = powerMin$M,
        vector = powerMin$V,
        direction = powerMin$D,
        cost = rep(0, n),
        binary = grepl("Op_", coord(object))
      )


    return(m)}
  } else{
    print("Attributes are missing: minP_fuel_plus, coordinates are needed")
  }
})

#----LM.minP_fuel_minus---------------------------------------
#'LM.minP_fuel_minus
#'
#'Build the constraint LM fo minimal fuel output power in kW
#'
#'@export
setMethod("LM.minP_fuel_minus", signature(object = "Root"), function(object) {
  if (length(minP_fuel_minus(object)) * length(coord(object)) > 0) {
    if(minP_fuel_minus(object)>0){
    P_min <- minP_fuel_minus(object)

    te <- length(timegrid(object))

    n <- length(coord(object))

    powerMin <- list()

    powerMin$M <- -P_fuel_minus(object)

    colnames(powerMin$M) <- coord(object)

    row.names(powerMin$M) <-
      paste0(name(object), " minP_fuel_minus_time", as.character(1:te))

    powerMin$V <- rep(-P_min, te)

    powerMin$D <- rep("<=", te)

    m <-
      new(
        "LM",
        matrix = powerMin$M,
        vector = powerMin$V,
        direction = powerMin$D,
        cost = rep(0, n),
        binary = grepl("Op_", coord(object))
      )


    return(m)}
  } else{
    print("Attributes are missing: minP_fuel_minus, coordinates are needed")
  }
})

#----LM.minP_th_plus---------------------------------------
#'LM.minP_th_plus
#'
#'Build the constraint LM fo minimal thermal output power in kW
#'
#'@export
setMethod("LM.minP_th_plus", signature(object = "Root"), function(object) {
  if (length(minP_th_plus(object)) * length(coord(object)) > 0) {
    if(minP_th_plus(object)>0){
    P_min <- minP_th_plus(object)

    te <- length(timegrid(object))

    n <- length(coord(object))

    powerMin <- list()

    powerMin$M <- -P_th_plus(object)

    colnames(powerMin$M) <- coord(object)

    row.names(powerMin$M) <-
      paste0(name(object), " minP_th_plus_time", as.character(1:te))

    powerMin$V <- rep(-P_min, te)

    powerMin$D <- rep("<=", te)

    m <-
      new(
        "LM",
        matrix = powerMin$M,
        vector = powerMin$V,
        direction = powerMin$D,
        cost = rep(0, n),
        binary = grepl("Op_", coord(object))
      )


    return(m)}
  } else{
    print("Attributes are missing: minP_th_plus, coordinates are needed")
  }
})

#----LM.minP_th_minus---------------------------------------
#'LM.minP_th_minus
#'
#'Build the constraint LM fo minimal thermal output power in kW
#'
#'@export
setMethod("LM.minP_th_minus", signature(object = "Root"), function(object) {
  if (length(minP_th_minus(object)) * length(coord(object)) > 0) {
    if(minP_th_minus(object)>0){
    P_min <- minP_th_minus(object)

    te <- length(timegrid(object))

    n <- length(coord(object))

    powerMin <- list()

    powerMin$M <- -P_th_minus(object)

    colnames(powerMin$M) <- coord(object)

    row.names(powerMin$M) <-
      paste0(name(object), " minP_th_minus_time", as.character(1:te))

    powerMin$V <- rep(-P_min, te)

    powerMin$D <- rep("<=", te)

    m <-
      new(
        "LM",
        matrix = powerMin$M,
        vector = powerMin$V,
        direction = powerMin$D,
        cost = rep(0, n),
        binary = grepl("Op_", coord(object))
      )


    return(m)}
  } else{
    print("Attributes are missing: minP_th_minus, coordinates are needed")
  }
})
#----LM.P_th_plus---------------------------------------
#'LM.P_th_plus
#'
#'Build the constraint LM if a P_th_plus profile is load
#'
#'@export
setMethod("LM.P_th_plus", signature(object = "Root"), function(object) {
  if (length(load_P_th_plus(object)) * length(coord(object)) > 0) {

    co <- coord(object)
    n <- length(co)

    value <- load_P_th_plus(object)
    P_th_plus_profil<-list()
    P_th_plus_profil$M<-numeric()

    rownames <- character()

    for(time in seq_len(length(value))){
      if(value[time]>0){
        id <- diag(n)

        colnames(id) <- co

        id <- t(id[, grep(paste0(object@name, "_P_th_plus"), co)])[time,]
        P_th_plus_profil$M <- rbind(P_th_plus_profil$Matrix,id)

        rownames<- c(rownames,paste0(name(object), " P_th_plus_profil", time))

      }
    }



    colnames(P_th_plus_profil$M) <- coord(object)

    row.names(P_th_plus_profil$M) <-rownames

    P_th_plus_profil$V <- rep(P_max, dim(P_th_plus_profil$M)[1,])

    P_th_plus_profil$D <- rep("<=",  dim(P_th_plus_profil$M)[1,])

    m <-
      new(
        "LM",
        matrix = P_th_plus_profil$M,
        vector = P_th_plus_profil$V,
        direction = P_th_plus_profil$D,
        cost = rep(0, n),
        binary = grepl("Op_", coord(object))
      )


    return(m)
  } else{
    print("Attributes are missing: P_th_plus, coordinates are needed")
  }
})


#----LM.Profile_el_minus-----------------------------------
#'LM.Profile_el_minus
#'
#'Build the constraint LM for the profile for the demand of power in kW
#'per timestep
#'
#'@export

setMethod("LM.Profile_el_minus", signature(object = "Root"), function(object) {

  if(length(load_15min_el(object))*length(coord(object))>0|length(coord(object))*length(load_abstract_el(object))>0){

    te <- length(timegrid(object))

    n <- length(coord(object))
    co <- coord(object)
    Prof <- list()

    weight<-rep(1,n)
    if(length(object@linelossel)>0){
      weight[grepl("Pel_",co)& grepl(paste0(str_split_fixed(object@name, "_",2)[,2],"_time", sep="", collapse="|"),co)] <-kronecker(object@linelossel[object@linelossel>0], rep(1, te))
    }
    Prof$M <-t(weight * t(P_el_minus(object)))

    colnames(Prof$M) <- coord(object)

    row.names(Prof$M) <-
      paste0(name(object), " Profile_time", as.character(1:te))

    Prof$V <- load_el(object)

    Prof$D <- rep("=", te)

    m <-
      new(
        "LM",
        matrix = Prof$M,
        vector = Prof$V,
        direction = Prof$D,
        cost = rep(0, n),
        binary = grepl("Op_", coord(object))
      )


    return(m)
  }else{
    print("Attributes are missing: load_15min_el, coordinates or load_abstract_el are needed")

  }

})
#----LM.Profile_el_plus------------------------------------
#'LM.Profile_el_plus
#'
#'Build the constraint LM for the profile for the production of power in kW
#'per timestep
#'
#'@export

setMethod("LM.Profile_el_plus", signature(object = "Root"), function(object) {
  if (length(load_15min_el(object)) * length(coord(object)) > 0|length(coord(object))*length(load_abstract_el(object)) >0) {

    te <- length(timegrid(object))

    n <- length(coord(object))

    Prof <- list()

    Prof$M <- P_el_plus(object)

    colnames(Prof$M) <- coord(object)

    row.names(Prof$M) <-
      paste0(name(object), " Profile_time", as.character(1:te))

    Prof$V <- load_el(object)

    Prof$D <- rep("=", te)

    m <-
      new(
        "LM",
        matrix = Prof$M,
        vector = Prof$V,
        direction = Prof$D,
        cost = rep(0, n),
        binary = grepl("Op_", coord(object))
      )


    return(m)
  } else{
    print("Attributes are missing:  load_15min_el, coordinates or load_abstract_el are needed")
  }
})


#----LM.Profile_fuel_minus---------------------------------
#'LM.Profile_fuel_minus
#'
#'Build the constraint LM for the profile for the demand of fuel in kW
#'per timestep
#'
#'@export

setMethod("LM.Profile_fuel_minus", signature(object = "Root"), function(object) {
  if (length(load_15min_fuel(object)) * length(coord(object)) > 0|length(coord(object))*length(load_abstract_fuel(object))>0) {

    te <- length(timegrid(object))

    n <- length(coord(object))
    co <- coord(object)

    Prof <- list()

    weight<-rep(1,n)
    if(length(object@linelossfuel)>0){
      weight[grepl("Pfuel_",co)& grepl(paste0(str_split_fixed(object@name, "_",2)[,2],"_time", sep="", collapse="|"),co)] <-kronecker(object@linelossfuel[object@linelossfuel>0], rep(1, te))
    }
    Prof$M <-t(weight * t(P_fuel_minus(object)))

    colnames(Prof$M) <- coord(object)

    row.names(Prof$M) <-
      paste0(name(object), " Profile_time", as.character(1:te))

    Prof$V <- load_fuel(object)

    Prof$D <- rep("=", te)

    m <-
      new(
        "LM",
        matrix = Prof$M,
        vector = Prof$V,
        direction = Prof$D,
        cost = rep(0, n),
        binary = grepl("Op_", coord(object))
      )


    return(m)
  } else{
    print("Attributes are missing:  load_15min_fuel, coordinates or load_abstract_fuel are needed")
  }
})
#----LM.Profile_fuel_plus----------------------------------
#'LM.Profile_fuel_plus
#'
#'Build the constraint LM for the profile for the production of fuel in kW
#'per timestep
#'
#'@export

setMethod("LM.Profile_fuel_plus", signature(object = "Root"), function(object) {
  if (length(load_15min_fuel(object)) * length(coord(object)) > 0 |length(coord(object))*length(load_abstract_fuel(object))>0) {

    te <- length(timegrid(object))

    n <- length(coord(object))

    Prof <- list()

    Prof$M <- P_fuel_plus(object)

    colnames(Prof$M) <- coord(object)

    row.names(Prof$M) <-
      paste0(name(object), " Profile_time", as.character(1:te))

    Prof$V <- load_fuel(object)

    Prof$D <- rep("=", te)

    m <-
      new(
        "LM",
        matrix = Prof$M,
        vector = Prof$V,
        direction = Prof$D,
        cost = rep(0, n),
        binary = grepl("Op_", coord(object))
      )


    return(m)
  } else{
    print("Attributes are missing:  load_15min_fuel, coordinates or load_abstract_fuel are needed")
  }
})

#----LM.Profile_th_minus-----------------------------------
#'LM.Profile_th_minus
#'
#'Build the constraint LM for the profile for the demand of thermal in kW
#'per timestep
#'
#'@export

setMethod("LM.Profile_th_minus", signature(object = "Root"), function(object) {
  if (length(load_15min_th(object)) * length(coord(object)) > 0|length(coord(object))*length(load_abstract_th(object))>0) {

    te <- length(timegrid(object))

    n <- length(coord(object))
    co <- coord(object)

    Prof <- list()
    weight<-rep(1,n)
    if(length(object@linelossth)>0){
      weight[grepl("Pth_",co)& grepl(paste0(str_split_fixed(object@name, "_",2)[,2],"_time", sep="", collapse="|"),co)] <-kronecker(object@linelossth[object@linelossth>0], rep(1, te))
    }
    Prof$M <-t(weight * t(P_th_minus(object)))


    colnames(Prof$M) <- coord(object)

    row.names(Prof$M) <-
      paste0(name(object), " Profile_time", as.character(1:te))

    Prof$V <- load_th(object)

    Prof$D <- rep("=", te)

    m <-
      new(
        "LM",
        matrix = Prof$M,
        vector = Prof$V,
        direction = Prof$D,
        cost = rep(0, n),
        binary = grepl("Op_", coord(object))
      )


    return(m)
  } else{
    print("Attributes are missing:  load_15min_th, coordinates or load_abstract_th are needed")
  }
})
#----LM.P_th_plus---------------------------------------
#'LM.P_th_plus
#'
#'Build the constraint LM if a P_th_plus profile is load
#'
#'@export
setMethod("LM.P_th_plus", signature(object = "Root"), function(object) {
  if (length(load_P_th_plus(object)) * length(coord(object)) > 0) {
    if(length(load_P_th_plus(object))>length(timegrid(object))){
      stop("P_th_plus profil has more timesteps then simulations. Please use at least the same length, e.g. length of P_th_plus_profil has the same length as timegrid")
    }else{
      co <- coord(object)
      n <- length(co)

      value <- load_P_th_plus(object)
      M<-numeric()

      rownames <- character()


      for(time in seq_len(length(value))){
        if(value[time]>=0){
          id <- diag(n)

          colnames(id) <- co

          id <- t(id[, grep(paste0(object@name, "_P_th_plus"), co)])[time,]
          M <- rbind(M,id)

          rownames<- c(rownames,paste0(name(object), " P_th_plus_profil", time))

        }
      }



      colnames(M) <- coord(object)

      row.names(M) <-rownames

      V <- value[value>=0]

      D <- rep("=",  dim(M)[1])

      m <-
        new(
          "LM",
          matrix = M,
          vector = V,
          direction = D,
          cost = rep(0, n),
          binary = grepl("Op_", coord(object))
        )


      return(m)
    }
  } else{
    print("There is no load_P_th_plus profil and/or coordinates are missing")
  }
})



#----LM.Profile_th_plus------------------------------------
#'LM.Profile_th_plus
#'
#'Build the constraint LM for the profile for the production of thermal in kW
#'per timestep
#'
#'@export

setMethod("LM.Profile_th_plus", signature(object = "Root"), function(object) {
  if (length(load_15min_th(object)) * length(coord(object)) > 0|length(coord(object))*length(load_abstract_th(object))>0 ) {

    te <- length(timegrid(object))

    n <- length(coord(object))

    Prof <- list()

    Prof$M <- P_th_plus(object)

    colnames(Prof$M) <- coord(object)

    row.names(Prof$M) <-
      paste0(name(object), " Profile_time", as.character(1:te))

    Prof$V <- load_th(object)

    Prof$D <- rep("=", te)

    m <-
      new(
        "LM",
        matrix = Prof$M,
        vector = Prof$V,
        direction = Prof$D,
        cost = rep(0, n),
        binary = grepl("Op_", coord(object))
      )


    return(m)
  } else{
    print("Attributes are missing:  load_15min_th, coordinates or load_abstract_th are needed")
  }
})


#----LM.Transform_elth_th------------------------------------
#'LM.Transform_elth_th
#'
#'Build the constraint LM for the transformation of power and heat to heat
#'in kW. It is used for example in a heat pump driven by source.
#'
#'@export

setMethod("LM.Transform_elth_th", signature(object = "Root"), function(object) {
  if (length(CurveOffset(object))*
      length(CurvePitch(object))*
      length(SourceTemp_15min(object))*
      length(coord(object)) > 0) {
    COP <- CurvePitch(object)*profile_source(object) + CurveOffset(object)

    te <- length(timegrid(object))

    n <- length(coord(object))
    co <- coord(object)

    TF_elth_th <- list()

    co <- coord(object)
    weight<-rep(1,n)
    if(length(object@linelossel)>0){
      weight[grepl("Pel_",co)& grepl(paste0(str_split_fixed(object@name, "_",2)[,2],"_time", sep="", collapse="|"),co)] <-kronecker(object@linelossel[object@linelossel>0], rep(1, te))
    }

    TF_elth_th$M <- t(weight * t(P_el_minus(object))) * COP - P_th_plus(object)

    colnames(TF_elth_th$M) <- coord(object)

    row.names(TF_elth_th$M) <-
      paste0(name(object), " Transform_elth_th_time", as.character(1:te))

    TF_elth_th$V <- rep(0, te)

    TF_elth_th$D <- rep("=", te)

    m <-
      new(
        "LM",
        matrix = TF_elth_th$M,
        vector = TF_elth_th$V,
        direction = TF_elth_th$D,
        cost = rep(0, n),
        binary = grepl("Op_", coord(object))
      )

    return(m)
  } else{
    print("Attributes are missing: SourceTemp1, SourceTemp2, COP1, COP2, SourceTemp_15min and coordinates are needed")
  }
})
#----LM.Transform_el_fuel------------------------------------
#'LM.Transform_el_fuel
#'
#'Build the constraint LM for the transformation of power to fuel (gas/hydrogen) in kW
#'
#'@export

setMethod("LM.Transform_el_fuel", signature(object = "Root"), function(object) {
  if (length(effMaxP_fuel_plus(object)) * length(coord(object)) > 0) {
    
    token <- FALSE
    if(length(maxP_fuel_plus(object))*length(minP_fuel_plus(object))*
       length(maxP_el_minus(object))*length(minP_el_minus(object))){
      if(maxP_fuel_plus(object)!=minP_fuel_plus(object) &
         maxP_el_minus(object)!=minP_el_minus(object)
      ){
        token <- TRUE
      }
    }
    
    if(token){
      
      #P_fuel = a*P_el + b
      #maxP_fuel = a*maxP_el + b und minP_fuel = a*minP_el + b
      #Differenz gibt  a = (maxP_fuel - minP_fuel)/(maxP_el - minP_el)
      #Einsetzen b = maxP_fuel - a*maxP_el
      
      
      a <-   (maxP_fuel_plus(object) - minP_fuel_plus(object))/
        (maxP_el_minus(object) - minP_el_minus(object))
      
      b <- maxP_fuel_plus(object) - a*maxP_el_minus(object)
      
      te <- length(timegrid(object))
      
      n <- length(coord(object))
      
      TF_el_fuel <- list()
      
      co <- coord(object)
      weight<-rep(1,n)
      if(length(object@linelossel)>0){
        weight[grepl("Pel_",co)& grepl(paste0(str_split_fixed(object@name, "_",2)[,2],"_time", sep="", collapse="|"),co)] <-kronecker(object@linelossel[object@linelossel>0], rep(1, te))
      }
      TF_el_fuel$M <- t(weight * t(P_el_minus(object))) * a + b*Op(object) - P_fuel_plus(object)
      
      colnames(TF_el_fuel$M) <- coord(object)
      
      row.names(TF_el_fuel$M) <-
        paste0(name(object), " Transform_el_fuel_time", as.character(1:te))
      
      TF_el_fuel$V <- rep(0, te)
      
      TF_el_fuel$D <- rep("=", te)
      
      m <-
        new(
          "LM",
          matrix = TF_el_fuel$M,
          vector = TF_el_fuel$V,
          direction = TF_el_fuel$D,
          cost = rep(0, n),
          binary = grepl("Op_", coord(object))
        )
      
      return(m)
      
    }else{
  
        eta_fuel <- effMaxP_fuel_plus(object)
        
        te <- length(timegrid(object))
        
        n <- length(coord(object))
        
        TF_el_fuel <- list()
        
        co <- coord(object)
        weight<-rep(1,n)
        if(length(object@linelossel)>0){
          weight[grepl("Pel_",co)& grepl(paste0(str_split_fixed(object@name, "_",2)[,2],"_time", sep="", collapse="|"),co)] <-kronecker(object@linelossel[object@linelossel>0], rep(1, te))
        }
        TF_el_fuel$M <- eta_fuel *t(weight * t(P_el_minus(object))) - P_fuel_plus(object)
        
        colnames(TF_el_fuel$M) <- coord(object)
        
        row.names(TF_el_fuel$M) <-
          paste0(name(object), " Transform_el_fuel_time", as.character(1:te))
        
        TF_el_fuel$V <- rep(0, te)
        
        TF_el_fuel$D <- rep("=", te)
        
        m <-
          new(
            "LM",
            matrix = TF_el_fuel$M,
            vector = TF_el_fuel$V,
            direction = TF_el_fuel$D,
            cost = rep(0, n),
            binary = grepl("Op_", coord(object))
          )
        
        return(m)
      
    }
  } else{
    print("Attributes are missing: effMaxP_fuel_plus, coordinates are needed")
  }
})


#----LM.Transform_el_th------------------------------------
#'LM.Transform_el_th
#'
#'Build the constraint LM for the transformation of power to heat in kW
#'
#'@export

setMethod("LM.Transform_el_th", signature(object = "Root"), function(object) {
  if (length(effMaxP_th_plus(object)) * length(coord(object)) > 0) {
    
    token <- FALSE
    if(length(maxP_th_plus(object))*length(minP_th_plus(object))*
       length(maxP_el_minus(object))*length(minP_el_minus(object))){
      if(maxP_th_plus(object)!=minP_th_plus(object) &
         maxP_el_minus(object)!=minP_el_minus(object)
      ){
        token <- TRUE
      }
    }
    
    if(token){
      
      #P_th = a*P_el + b
      #maxP_th = a*maxP_el + b und minP_th = a*minP_el + b
      #Differenz gibt  a = (maxP_th - minP_th)/(maxP_el - minP_el)
      #Einsetzen b = maxP_th - a*maxP_el
      
      
      a <-   (maxP_th_plus(object) - minP_th_plus(object))/
        (maxP_el_minus(object) - minP_el_minus(object))
      
      b <- maxP_th_plus(object) - a*maxP_el_minus(object)
      
      te <- length(timegrid(object))
      
      n <- length(coord(object))
      
      TF_el_th <- list()
      
      co <- coord(object)
      weight<-rep(1,n)
      if(length(object@linelossel)>0){
        weight[grepl("Pel_",co)& grepl(paste0(str_split_fixed(object@name, "_",2)[,2],"_time", sep="", collapse="|"),co)] <-kronecker(object@linelossel[object@linelossel>0], rep(1, te))
      }
      TF_el_th$M <- t(weight * t(P_el_minus(object))) * a + b*Op(object) - P_th_plus(object)
      
      colnames(TF_el_th$M) <- coord(object)
      
      row.names(TF_el_th$M) <-
        paste0(name(object), " Transform_el_th_time", as.character(1:te))
      
      TF_el_th$V <- rep(0, te)
      
      TF_el_th$D <- rep("=", te)
      
      m <-
        new(
          "LM",
          matrix = TF_el_th$M,
          vector = TF_el_th$V,
          direction = TF_el_th$D,
          cost = rep(0, n),
          binary = grepl("Op_", coord(object))
        )
      
      return(m)
      
    }else{
        eta_th <- effMaxP_th_plus(object)
    
        te <- length(timegrid(object))
    
        n <- length(coord(object))
    
        TF_el_th <- list()
    
        co <- coord(object)
        weight<-rep(1,n)
        if(length(object@linelossel)>0){
          weight[grepl("Pel_",co)& grepl(paste0(str_split_fixed(object@name, "_",2)[,2],"_time", sep="", collapse="|"),co)] <-kronecker(object@linelossel[object@linelossel>0], rep(1, te))
        }
        TF_el_th$M <- eta_th *t(weight * t(P_el_minus(object))) - P_th_plus(object)
    
        colnames(TF_el_th$M) <- coord(object)
    
        row.names(TF_el_th$M) <-
          paste0(name(object), " Transform_el_th_time", as.character(1:te))
    
        TF_el_th$V <- rep(0, te)
    
        TF_el_th$D <- rep("=", te)
    
        m <-
          new(
            "LM",
            matrix = TF_el_th$M,
            vector = TF_el_th$V,
            direction = TF_el_th$D,
            cost = rep(0, n),
            binary = grepl("Op_", coord(object))
          )
    
        return(m)
      
    }
  } else{
    print("Attributes are missing: effMaxP_th_plus, coordinates are needed")
  }
})
#----LM.Transform_el_th_COP------------------------------------
#'LM.Transform_el_th_COP
#'
#'Build the constraint LM for the transformation of power to heat in kW with COP
#'
#'@export

setMethod("LM.Transform_el_th_COP", signature(object = "Root"), function(object) {
  
  
  if(length(maxP_th_plus(object))*length(maxP_el_minus(object))*
       length(nominalP_th_plus(object))*length(COP(object))){
      
    te <- length(timegrid(object))
    
    n <- length(coord(object))
    
    TF_el_th_COP <- list()
      
    co <- coord(object)
    weight<-rep(1,n)
    if(length(object@linelossel)>0){
      weight[grepl("Pel_",co)& grepl(paste0(str_split_fixed(object@name, "_",2)[,2],"_time", sep="", collapse="|"),co)] <-kronecker(object@linelossel[object@linelossel>0], rep(1, te))
    }
    TF_el_th$M <- t(weight * t(P_el_minus(object))) * COP(object) - P_th_plus(object)
      
    colnames(TF_el_th$M) <- coord(object)
    
    row.names(TF_el_th$M) <-
      paste0(name(object), " Transform_el_th_COP_time", as.character(1:te))
    
    TF_el_th$V <- rep(0, te)
      
      TF_el_th$D <- rep("=", te)
      
      m <-
        new(
          "LM",
          matrix = TF_el_th$M,
          vector = TF_el_th$V,
          direction = TF_el_th$D,
          cost = rep(0, n),
          binary = grepl("Op_", coord(object))
        )
      
      return(m)
      
    
     
      
      
    
  } else{
    print("Attributes are missing: effMaxP_th_plus, coordinates are needed")
  }
})
#----LM.Transform_fuel_el----------------------------------
#'LM.Transform_fuel_el
#'
#'Build the constraint LM for the transformation of fuel to power in kW
#'
#'@export

setMethod("LM.Transform_fuel_el", signature(object = "Root"), function(object) {
  if (length(effMaxP_el_plus(object)) * length(coord(object)) > 0) {

    token <- FALSE
    if(length(maxP_el_plus(object))*length(minP_el_plus(object))*
       length(maxP_fuel_minus(object))*length(minP_fuel_minus(object))){
      if(maxP_el_plus(object)!=minP_el_plus(object) &
         maxP_fuel_minus(object)!=minP_fuel_minus(object)
      ){
        token <- TRUE
      }
    }

    if(token){

      #P_el = a*P_fuel + b
      #maxP_el = a*maxP_fuel + b und minP_el = a*minP_fuel + b
      #Differenz gibt  a = (maxP_el - minP_el)/(maxP_fuel - minP_fuel)
      #Einsetzen b = maxP_el - a*maxP_fuel


      a <-   (maxP_el_plus(object) - minP_el_plus(object))/
        (maxP_fuel_minus(object) - minP_fuel_minus(object))

      b <- maxP_el_plus(object) - a*maxP_fuel_minus(object)


      te <- length(timegrid(object))

      n <- length(coord(object))

      TF_fuel_el <- list()

      co <- coord(object)
      weight<-rep(1,n)
      if(length(object@linelossfuel)>0){
        weight[grepl("Pfuel_",co)& grepl(paste0(str_split_fixed(object@name, "_",2)[,2],"_time", sep="", collapse="|"),co)] <-kronecker(object@linelossfuel[object@linelossfuel>0], rep(1, te))
      }
      TF_fuel_el$M <-t(weight * t(P_fuel_minus(object))) * a + b*Op(object) - P_el_plus(object)

      colnames(TF_fuel_el$M) <- coord(object)

      row.names(TF_fuel_el$M) <-
        paste0(name(object), " Transform_fuel_el_time", as.character(1:te))

      TF_fuel_el$V <- rep(0, te)

      TF_fuel_el$D <- rep("=", te)

      m <-
        new(
          "LM",
          matrix = TF_fuel_el$M,
          vector = TF_fuel_el$V,
          direction = TF_fuel_el$D,
          cost = rep(0, n),
          binary = grepl("Op_", coord(object))
        )

      return(m)
    }else{

    eta_el <- effMaxP_el_plus(object)

    te <- length(timegrid(object))

    n <- length(coord(object))

    TF_fuel_el <- list()

    co <- coord(object)
    weight<-rep(1,n)
    if(length(object@linelossfuel)>0){
      weight[grepl("Pfuel_",co)& grepl(paste0(str_split_fixed(object@name, "_",2)[,2],"_time", sep="", collapse="|"),co)] <-kronecker(object@linelossfuel[object@linelossfuel>0], rep(1, te))
    }
    TF_fuel_el$M <-t(weight * t(P_fuel_minus(object))) * eta_el - P_el_plus(object)

    colnames(TF_fuel_el$M) <- coord(object)

    row.names(TF_fuel_el$M) <-
      paste0(name(object), " Transform_fuel_el_time", as.character(1:te))

    TF_fuel_el$V <- rep(0, te)

    TF_fuel_el$D <- rep("=", te)

    m <-
      new(
        "LM",
        matrix = TF_fuel_el$M,
        vector = TF_fuel_el$V,
        direction = TF_fuel_el$D,
        cost = rep(0, n),
        binary = grepl("Op_", coord(object))
      )

    return(m)
    }
  } else{
    print("Attributes are missing: effMaxP_el_minus, coordinates are needed")
  }
})
#----LM.Transform_fuel_th----------------------------------
#'LM.Transform_fuel_th
#'
#'Build the constraint LM for the transformation of fuel to heat in kW
#'
#'@export

setMethod("LM.Transform_fuel_th", signature(object = "Root"), function(object) {
  if (length(effMaxP_th_plus(object)) * length(coord(object)) > 0) {

    token <- FALSE
    if(length(maxP_th_plus(object))*length(minP_th_plus(object))*
       length(maxP_fuel_minus(object))*length(minP_fuel_minus(object))){
      if(maxP_th_plus(object)!=minP_th_plus(object) &
         maxP_fuel_minus(object)!=minP_fuel_minus(object)
         ){
        token <- TRUE
      }
    }

    if(token){

      #P_th = a*P_fuel + b
      #maxP_th = a*maxP_fuel + b und minP_th = a*minP_fuel + b
      #Differenz gibt  a = (maxP_th - minP_th)/(maxP_fuel - minP_fuel)
      #Einsetzen b = maxP_th - a*maxP_fuel


    a <-   (maxP_th_plus(object) - minP_th_plus(object))/
      (maxP_fuel_minus(object) - minP_fuel_minus(object))

    b <- maxP_th_plus(object) - a*maxP_fuel_minus(object)

    te <- length(timegrid(object))

    n <- length(coord(object))

    TF_fuel_th <- list()

    co <- coord(object)
    weight<-rep(1,n)
    if(length(object@linelossfuel)>0){
      weight[grepl("Pfuel_",co)& grepl(paste0(str_split_fixed(object@name, "_",2)[,2],"_time", sep="", collapse="|"),co)] <-kronecker(object@linelossfuel[object@linelossfuel>0], rep(1, te))
    }
    TF_fuel_th$M <- t(weight * t(P_fuel_minus(object))) * a + b*Op(object) - P_th_plus(object)

    colnames(TF_fuel_th$M) <- coord(object)

    row.names(TF_fuel_th$M) <-
      paste0(name(object), " Transform_fuel_th_time", as.character(1:te))

    TF_fuel_th$V <- rep(0, te)

    TF_fuel_th$D <- rep("=", te)

    m <-
      new(
        "LM",
        matrix = TF_fuel_th$M,
        vector = TF_fuel_th$V,
        direction = TF_fuel_th$D,
        cost = rep(0, n),
        binary = grepl("Op_", coord(object))
      )

    return(m)

    }else{
    eta_th <- effMaxP_th_plus(object)

    te <- length(timegrid(object))

    n <- length(coord(object))

    TF_fuel_th <- list()

    co <- coord(object)
    weight<-rep(1,n)
    if(length(object@linelossfuel)>0){
      weight[grepl("Pfuel_",co)& grepl(paste0(str_split_fixed(object@name, "_",2)[,2],"_time", sep="", collapse="|"),co)] <-kronecker(object@linelossfuel[object@linelossfuel>0], rep(1, te))
    }
    TF_fuel_th$M <- t(weight * t(P_fuel_minus(object))) * eta_th - P_th_plus(object)

    colnames(TF_fuel_th$M) <- coord(object)

    row.names(TF_fuel_th$M) <-
      paste0(name(object), " Transform_fuel_th_time", as.character(1:te))

    TF_fuel_th$V <- rep(0, te)

    TF_fuel_th$D <- rep("=", te)

    m <-
      new(
        "LM",
        matrix = TF_fuel_th$M,
        vector = TF_fuel_th$V,
        direction = TF_fuel_th$D,
        cost = rep(0, n),
        binary = grepl("Op_", coord(object))
      )

    return(m)

    }
  } else{
    print("Attributes are missing: effMaxP_th_plus, coordinates are needed")
  }
})
#----LM.Tunnel_el------------------------------------------
#'LM.Tunnel_el
#'
#'Build the constraint LM to link the input directly to the output for
#'power in kW
#'
#'@export

setMethod("LM.Tunnel_el", signature(object = "Root"), function(object) {

  te <- length(timegrid(object))

  n <- length(coord(object))

  Link_el <- list()

  co <- coord(object)
  weight<-rep(1,n)
  if(length(object@linelossel)>0){
    weight[grepl("Pth_",co)& grepl(paste0(str_split_fixed(object@name, "_",2)[,2],"_time", sep="", collapse="|"),co)] <-kronecker(object@linelossel[object@linelossel>0], rep(1, te))
  }
  Link_el$M <- - t(weight * t(P_el_minus(object))) + P_el_plus(object)

  colnames(Link_el$M) <- coord(object)

  row.names(Link_el$M) <-
    paste0(name(object), " Tunnel_el_time", as.character(1:te))

  Link_el$V <- rep(0, te)

  Link_el$D <- rep("=", te)

  m <-
    new(
      "LM",
      matrix = Link_el$M,
      vector = Link_el$V,
      direction = Link_el$D,
      cost = rep(0, n),
      binary = grepl("Op_", coord(object))
    )

  return(m)
})
#----LM.Tunnel_fuel----------------------------------------
#'LM.Tunnel_fuel
#'
#'Build the constraint LM to link the input directly to the output for
#'fuel in kW
#'
#'@export

setMethod("LM.Tunnel_fuel", signature(object = "Root"), function(object) {

  te <- length(timegrid(object))

  n <- length(coord(object))

  Link_fuel <- list()

  co <- coord(object)
  weight<-rep(1,n)
  if(length(object@linelossfuel)>0){
    weight[grepl("Pth_",co)& grepl(paste0(str_split_fixed(object@name, "_",2)[,2],"_time", sep="", collapse="|"),co)] <-kronecker(object@linelossfuel[object@linelossfuel>0], rep(1, te))
  }

  Link_fuel$M <- - t(weight * t(P_fuel_minus(object))) + P_fuel_plus(object)

  colnames(Link_fuel$M) <- coord(object)

  row.names(Link_fuel$M) <-
    paste0(name(object), " Tunnel_fuel_time", as.character(1:te))

  Link_fuel$V <- rep(0, te)

  Link_fuel$D <- rep("=", te)

  m <-
    new(
      "LM",
      matrix = Link_fuel$M,
      vector = Link_fuel$V,
      direction = Link_fuel$D,
      cost = rep(0, n),
      binary = grepl("Op_", coord(object))
    )

  return(m)
})
#----LM.Tunnel_th------------------------------------------
#'LM.Tunnel_th
#'
#'Build the constraint LM to link the input directly to the output for
#'heat in kW
#'
#'@export

setMethod("LM.Tunnel_th", signature(object = "Root"), function(object) {

  te <- length(timegrid(object))

  n <- length(coord(object))

  Link_th <- list()

  co <- coord(object)
  weight<-rep(1,n)
  if(length(object@linelossth)>0){
    weight[grepl("Pth_",co)& grepl(paste0(str_split_fixed(object@name, "_",2)[,2],"_time", sep="", collapse="|"),co)] <-kronecker(object@linelossth[object@linelossth>0], rep(1, te))
  }

  Link_th$M <- - t(weight * t(P_th_minus(object))) + P_th_plus(object)

  colnames(Link_th$M) <- coord(object)

  row.names(Link_th$M) <-
    paste0(name(object), " Tunnel_th_time", as.character(1:te))

  Link_th$V <- rep(0, te)

  Link_th$D <- rep("=", te)

  m <-
    new(
      "LM",
      matrix = Link_th$M,
      vector = Link_th$V,
      direction = Link_th$D,
      cost = rep(0, n),
      binary = grepl("Op_", coord(object))
    )

  return(m)
})
#----LM.minDowntime----------------------------------------
#'LM.minDowntime
#'
#'Build the constraint LM for the minimal downtime of a unit in hours.
#'Notice: If there is a timegrid containing steps which do not end at a
#'full hour block the unit will not run at the next time step!
#'WARNING: If there is a timegrid with different stepsizes there could be
#'errors in modelling a minimal downtime!
#'
#'@export
#'
setMethod("LM.minDowntime", signature(object = "Root"), function(object) {
  m <-
    new(
      "LM",
      matrix = matrix(0,0,length(coord(object))),
      vector = numeric(),
      direction = character(),
      cost = rep(0,length(coord(object))),
      binary = grepl("Op",coord(object))
    )


  if(any(grepl("Op", variables(object))==TRUE)&
     length(object@minDowntime)!=0&
     length(timegrid(object))!=1
  ){
    if(object@minRuntime <= 0){
        return(m)
      }

        schrittweite<-timegrid(object)/60
        te<-length(schrittweite);
        SZ<-object@minDowntime

        if(length(object@initial_state)==0){
          object@initial_state = 0
          print(paste0("The ",as.character(class(object))," with name " ,object@name, " has no initial state. It will be treated as 0."))
        }

        Opi<-function(t=1:te){
          t(t(Op(object))[,t])
        }


          ta<-schrittweite[1:te];
          taa<-cumsum(ta);
          k<-which(taa>=SZ)


          if(length(k)==0){
              j<-te;
          }else{
              j<-(1+min(k)-1)
          }
         
          u<-1:j;
          x<-length(u)*( -  Opi(1)) + t(colSums(Opi(u)));
          v<-length(u) - object@initial_state*length(u)
          
          for(i in 2:te){
            ta<-schrittweite[i:te];
            taa<-cumsum(ta);
            k<-which(taa>=SZ)
            if(length(k)==0){
              j<-te;
            }else{
              j<-(i+min(k)-1)
            }
            
            u<-i:j;
            x<-rbind(x,length(u)*(Opi(i-1) -  Opi(i)) +
                      t(colSums(Opi(u)))
            );
            v<-c(v,length(u))
            
          }


          colnames(x)<-coord(object);

          row.names(x)<- paste0(name(object),"_minDowntime_time",1:te)

          co <- coord(object)
          n<-length(co)
          C <- rep(0,n)
          names(C)<-co

        m <-
          new(
            "LM",
            matrix = x,
            vector = v,
            direction = rep("<=",te),
            cost = C,
            binary = grepl("Op_", coord(object))
          )

  }
        return(m)

  })
#----LM.minRuntime----------------------------------------
#'LM.minRuntime
#'
#'Build the constraint LM for the minimal runtime of a unit in hours.
#'NOTICE: If there is a timegrid containing steps which do not end at a
#'full hour block the unit will also run at the next time step!
#'WARNING: If there is a timegrid with different stepsizes there could be
#'errors in modelling a minimal runtime!
#'
#'@export
#'
#'
setMethod("LM.minRuntime", signature(object = "Root"), function(object) {

   m <-
        new(
          "LM",
          matrix = matrix(0,0,length(coord(object))),
          vector = numeric(),
          direction = character(),
          cost = rep(0,length(coord(object))),
          binary = grepl("Op",coord(object))
        )

      
      if(any(grepl("Op", variables(object))==TRUE)&
         length(object@minRuntime)!=0&
         length(timegrid(object))!=1
      ){
      if(object@minRuntime <= 0){
        return(m)
      }
      

      if(length(object@initial_state)==0){
          object@initial_state = 0
           print(paste0("The ",as.character(class(object))," with name " ,object@name, " has no initial state. It will be treated as 0."))
      }


       schrittweite<-timegrid(object)/60
        te<-length(schrittweite);
        LZ<-object@minRuntime;

        Opi<-function(t=1:te){
          t(t(Op(object))[,t])
        }



        ta<-schrittweite;
        taa<-cumsum(ta);

        k<-which(taa>=LZ)

        if(length(k)==0){
          j<-te;

        }else{
          j<-min(k)
        }

        u<-1:j


        x<-j*Opi(1) - t(colSums(Opi(u)));
        v <- object@initial_state*j;
        if(te>1){


          for(i in 2:te){

            ta<-schrittweite[i:te];
            taa<-cumsum(ta);

            k<-which(taa>=LZ)

            if(length(k)==0){
              j<-te;

            }else{
              j<-(i+min(k)-1)
            }

            u<-i:j;


            x<-rbind(x,length(u)*(Opi(i) -  Opi(i-1)) -
                       t(colSums(Opi(u)))
            )
            v<-c(v,0);
          }

minRTime<-list()

          minRTime$M<-x
          colnames(minRTime$M) <- coord(object)
          row.names(minRTime$M) <-
            paste0(name(object), "_minRuntime_time", as.character(1:te))
          minRTime$V<-v
          minRTime$D <- rep("<=", te)
        }
        co <- coord(object)
        n<-length(co)
        C <- rep(0,n)
        names(C)<-co

        m <-
          new(
            "LM",
            matrix = minRTime$M,
            vector = minRTime$V,
            direction = minRTime$D,
            cost = C,
            binary = grepl("Op_", coord(object))
          )

      }
        return(m)

})



#----LM.maxChargingCycles----------------------------------------
#'LM.maxChargingCycles
#'
#'the linear constraint matrix giving a restriction on the fullCharging cycles in the simulation time
#'It is given by sum(ta*( P_in + Pout ))<= 2*maxE*cycles
#'where cycles are the expected charging cycles in the simulation time in hours
#'
#'@export
#'
#'
setMethod("LM.maxChargingCycles", signature(object = "Root"), function(object) {
  if(length(object@coordinates)==0){
    stop("coordinates missing")
  }
  m <-
    new(
      "LM",
      matrix = matrix(0,0,length(coord(object))),
      vector = numeric(),
      direction = character(),
      cost = rep(0,length(coord(object))),
      binary = grepl("Op",coord(object))
    )



  if(length(object@fullChargingCycles)==0|length(object@lifeTime)==0){
    return(m)
  }

  if(object@fullChargingCycles<0){
    return(m)
  }

  if(length(maxE_el(object))==0){
    return(m)
  }

  #simulation time in hours
  duration = sum(timegrid(object)/60)
  #cycles per hour
  cycles = (object@fullChargingCycles/object@lifeTime)/(365*24)
  cycles = duration*cycles

  #coefficient matrix
  M = t(colSums(timegrid(object)/60*(P_el_plus(object) + P_el_minus(object))))
  colnames(M) = coord(object)
  row.names(M) = paste0(name(object),"_maxChargingCycles")
  #energy which may be moved by charging cycles
  E = 2*maxE_el(object)*cycles



    m <-
      new(
        "LM",
        matrix = M,
        vector = E,
        direction = "<=",
        cost = rep(0, length(coord(object))),
        binary = grepl("Op_", coord(object))
      )


  return(m)

})


#----LM.modulated----------------------------------------
#'LM.modulated
#'
#'the linear constraint matrix giving a restriction setting the chp to run not modulated
#'
#'@export
#'
#'
setMethod("LM.modulated", signature(object = "Root"), function(object) {
  if(length(coord(object))==0){
    stop("coordinates missing")
  }
  m <-
    new(
      "LM",
      matrix = matrix(0,0,length(coord(object))),
      vector = numeric(),
      direction = character(),
      cost = rep(0,length(coord(object))),
      binary = grepl("Op",coord(object))
    )
  
  if(!any('modulated'==names(attributes(object)))){
    return(m)
  }
  if(length(object@modulated)==0){
    return(m)
  }
  if(as.logical(object@modulated)){
    return(m)
  }

  if(!any(grepl('Op',coord(object)))){
    return(m)
  }
  mat = P_el_plus(object) - maxP_el_plus(object)*Op(object)
  vec = rep(0,dim(mat)[1])
  direc = rep('=',dim(mat)[1])
  return(new(
      "LM",
      matrix = mat,
      vector = vec,
      direction = direc,
      cost = rep(0,length(coord(object))),
      binary = grepl("Op",coord(object))
    ))


})