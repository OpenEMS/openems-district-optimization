#----Overview----------------------------------------------
#
# LM.Adja
# LM.Adja_Price
# LM.Adja_Lim_el
# LM.Adja_Lim_fuel
# LM.Adja_Lim_th
# LM.Adja_Wat_el
# LM.Adja_Wat_fuel
# LM.Adja_Wat_th
# LM.ComponentsConstraints
# LM.Constraints
#
#
#
#----LM.Adja_Price-----------------------------------------
#
#'LM.Adja_Price
#'
#'build the prices based on the topolgy of adjacencyPrice_xy (where xy=el/fuel/th)
#'
#'@importFrom Root LM.Adja_Price
#'@export
#'@param object Sandbox
#'@return value cost vector
#'@examples
#'library(Sandbox)
#'library(Bat)
#'library(CHP)
#'library(PubGfuel)
#'library(Demth)
#'library(Demel)
#'cl <- new.Sandbox()
#'name(cl) <- 'Sandy'
#'timegrid(cl) <- c(15,30,60) #rep(15,3)
#'chp <- new.CHP()
#'bat1 <- new.Bat()
#'bat2 <- new.Bat()
#'fuel <- new.PubGfuel()
#'variables(fuel) <- "P_fuel_plus"
#'demth <- new.Demth()
#'demel <- new.Demel()
#'#Let us connect these things
#'components(cl) <- list(chp,bat1,bat2,fuel,demth,demel)
#'#connect CHP with both batteries (limited to 100 kW)
#'adjacency_el(cl) <- rbind(c(0,100,100,0,0,100),c(0,0,0,0,0,100),c(0,0,0,0,0,50),c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0));
#'adjacency_th(cl) <- rbind(c(0,0,0,0,100,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0));
#'adjacency_fuel(cl) <- rbind(c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(100,0,0,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0));
#'#we may add efficiency of the grid to our model
#'adjacencyEff_el(cl) <- rbind(c(0,0.9,0.8,0,0,0.9),c(0,0,0,0,0,0.7),c(0,0,0,0,0,0.9),c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0));
#'# add prices
#'adjacencyPrice_el(cl) <- rbind(c(0,-0.06,0.08,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0));
#'cl <- finalcoordinates(cl)
#'x <- LM.Adja_Price(cl)
#'a <- as.data.frame.LM(x)
setMethod("LM.Adja_Price", signature(object = "Sandbox"), function(object) {
  if (length(coord(object)) > 0) {
    co <- coord(object)
    k <- length(components(object))
    n <- length(co)
    te <- length(timegrid(object))


    cost <- rep(0, n)
    names(cost)<- co

    if (dim(adjacencyPrice_el(object))[1] != 0) {

      #topology and prices have to be defined both
      stopifnot(dim(adjacencyPrice_el(object))[1]==dim(adjacency_el(object))[1])

      price <- rep(0, length(co))
      #get the price array and the topology and integrate
      u <- array(t(matrix(tau(object),te,k*k)),dim = c(k,k,te))*adjacencyPrice_el(object);
      x <- adjacency_el(object);
      #transform u to a 2d-array: we got a line for every slot in the matrix x;
      #in the line is the corresponding cost vector for the represented edge/connection
      u <- array(u,dim = c(dim(u)[1]*dim(u)[2],dim(u)[3]));
      #take the non trivial lines (the lines representing an edge/connection)
      u <- u[which(x!=0),];
      #the coordinates are build exactly in the order x!=0 + tensor timesteps therefore
      #we have to transpose to as the array reads in columnwise and we want it linewise
      price[grep("Pel_", co)] <- array(t(u));


      m <- x
      nn <- dim(m)[1]
      te <-  length(timegrid(object))
      tmpi <- numeric()
      for (i in 1:nn) {
        for (j in 1:nn) {
          if (m[i, j] != 0) {
            tmpi <- rbind(tmpi, (P_el(object, i, j)))
          }
        }
      }
      tmp <- t(price * t(tmpi))

      cost <- cost + colSums(tmp)
    }

    if (dim(adjacencyPrice_fuel(object))[1] != 0) {

      #topology and prices have to be defined both
      stopifnot(dim(adjacencyPrice_fuel(object))[1]==dim(adjacency_fuel(object))[1])

      price <- rep(0, length(co))
      #get the price array and the topology and integrate
      u <- array(t(matrix(tau(object),te,k*k)),dim = c(k,k,te))*adjacencyPrice_fuel(object);
      x <- adjacency_fuel(object);
      #transform u to a 2d-array: we got a line for every slot in the matrix x;
      #in the line is the corresponding cost vector for the represented edge/connection
      u <- array(u,dim = c(dim(u)[1]*dim(u)[2],dim(u)[3]));
      #take the non trivial lines (the lines representing an edge/connection)
      u <- u[which(x!=0),];
      #the coordinates are build exactly in the order x!=0 + tensor timesteps therefore
      #we have to transpose to as the array reads in columnwise and we want it linewise
      price[grep("Pfuel_", co)] <- array(t(u));


      m <- x

      nn <- dim(m)[1]
      te <-  length(timegrid(object))
      tmpi <- numeric()
      for (i in 1:nn) {
        for (j in 1:nn) {
          if (m[i, j] != 0) {
            tmpi <- rbind(tmpi, (P_fuel(object, i, j)))
          }
        }
      }
      tmp <- t(price * t(tmpi))
      cost <- cost + colSums(tmp)
    }

    if (dim(adjacencyPrice_th(object))[1] != 0) {

      #topology and prices have to be defined both
      stopifnot(dim(adjacencyPrice_th(object))[1]==dim(adjacency_th(object))[1])

      price <- rep(0, length(co))
      #get the price array and the topology and integrate
      u <- array(t(matrix(tau(object),te,k*k)),dim = c(k,k,te))*adjacencyPrice_th(object);
      x <- adjacency_th(object);
      #transform u to a 2d-array: we got a line for every slot in the matrix x;
      #in the line is the corresponding cost vector for the represented edge/connection
      u <- array(u,dim = c(dim(u)[1]*dim(u)[2],dim(u)[3]));
      #take the non trivial lines (the lines representing an edge/connection)
      u <- u[which(x!=0),];
      #the coordinates are build exactly in the order x!=0 + tensor timesteps therefore
      #we have to transpose to as the array reads in columnwise and we want it linewise
      price[grep("Pth_", co)] <- array(t(u));


      m <- x

      nn <- dim(m)[1]
      te <-  length(timegrid(object))
      tmpi <- numeric()
      for (i in 1:nn) {
        for (j in 1:nn) {
          if (m[i, j] != 0) {
            tmpi <- rbind(tmpi, (P_th(object, i, j)))
          }
        }
      }
      tmp <- t(price * t(tmpi))
      cost <- cost + colSums(tmp)
    }


    ####Auskommentiert, da Johannes die AdjacencyPrice_fuel und _th auf _el anpasst (Sicherheitskopie)
    # if (dim(adjacencyPrice_fuel(object))[1] != 0) {
    #
    #
    #   price <- rep(0, length(co))
    #   price[grep("Pfuel_", co)] <-
    #     kronecker(adjacencyPrice_fuel(object)[adjacency_fuel(object) != 0], rep(1, te))
    #
    #   m <- adjacencyPrice_fuel(object)
    #   nn <- dim(m)[1]
    #   te <-  length(timegrid(object))
    #   tmpi <- numeric()
    #   for (i in 1:nn) {
    #     for (j in 1:nn) {
    #       if (m[i, j] != 0) {
    #         tmpi <- rbind(tmpi, (P_fuel(object, i, j)))
    #       }
    #     }
    #   }
    #   tmp <- t(price * t(tmpi))
    #
    #   cost <- cost + colSums(tmp)
    # }
    #
    # if (dim(adjacencyPrice_th(object))[1] != 0) {
    #   price <- rep(0, length(co))
    #   price[grep("Pth_", co)] <-
    #     kronecker(adjacencyPrice_th(object)[adjacency_th(object) != 0], rep(1, te))
    #
    #   m <- adjacencyPrice_th(object)
    #   nn <- dim(m)[1]
    #   te <-  length(timegrid(object))
    #   tmpi <- numeric()
    #   for (i in 1:nn) {
    #     for (j in 1:nn) {
    #       if (m[i, j] != 0) {
    #         tmpi <- rbind(tmpi, (P_th(object, i, j)))
    #       }
    #     }
    #   }
    #   tmp <- t(price * t(tmpi))
    #
    #   cost <- cost + colSums(tmp)
    # }


    x <- matrix(0, te, length(co))


    return(new("LM",
               cost = cost))

  } else{
    stop(print("Coordinates (name and timegrid) are necessary!"))
  }
})



#----LM.Adja_Lim_el------------------------------------------------------
#'LM.Adja_Lim_el
#'
#'build the LM describing all the power constraints in the electric Sandbox
#'
#'@importFrom Root LM.Adja_Lim_el
#'@export
#'@param object Sandbox
#'@return LM
#'@examples
#'library(Sandbox)
#'library(Bat)
#'library(CHP)
#'library(PubGfuel)
#'library(Demth)
#'library(Demel)
#'cl<-new.Sandbox()
#'name(cl) <- 'Sandy'
#'timegrid(cl) <- rep(15,10)
#'chp <- new.CHP()
#'bat1 <- new.Bat()
#'bat2 <- new.Bat()
#'fuel <- new.PubGfuel()
#'variables(fuel) <- "P_fuel_plus"
#'demth <- new.Demth()
#'demel <- new.Demel()
#'#Let us connect these things
#'components(cl) <- list(chp,bat1,bat2,fuel,demth,demel)
#'#connect CHP with both batteries (limited to 100 kW)
#'adjacency_el(cl) <- rbind(c(0,100,100,0,0,100),c(0,0,0,0,0,100),c(0,0,0,0,0,50),c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0));
#'adjacency_th(cl) <- rbind(c(0,0,0,0,100,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0));
#'adjacency_fuel(cl) <- rbind(c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(100,0,0,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0));
#'cl <- finalcoordinates(cl)
#'x <- LM.Adja_Lim_el(cl)
#'a <- as.data.frame.LM(x)
#'a

setMethod("LM.Adja_Lim_el", signature(object = "Sandbox"), function(object) {
  if (length(coord(object)) > 0) {
    if (dim(adjacency_el(object))[1] != 0) {
      m <- adjacency_el(object)

      n <- dim(m)[1]
      M <- matrix(0,0,length(coord(object)))

      V <- numeric()

      te <- length(timegrid(object))

      for (i in 1:n) {
        for (j in 1:n) {
          if (m[i, j] > 0) {
            M <- rbind(M, P_el(object, i, j))

            V <- c(V, rep(m[i, j], te))

          }
        }
      }
      if (length(V) == 0) {
        M <- matrix(0, 0, length(coord(object)))
        colnames(M) <- coord(object)

      } else{
        row.names(M) <- paste0(name(object), " Adja_Lim_el ", row.names(M))
      }
      new(
        "LM",
        matrix = M,
        direction = rep("<=", length(V)),
        vector = V,
        binary = grepl("Op", coord(object)),
        cost = rep(0, dim(M)[2])
      )

    } else{
      print('no adjacency_el available')
    }
  } else{
    stop(print("Coordinates (name and timegrid) are necessary!"))
  }

})

#----LM.Adja_Lim_fuel------------------------------------------------------
#'LM.Adja_Lim_fuel
#'
#'build the LM describing all the power constraints in the fuel Sandbox
#'
#'@importFrom Root LM.Adja_Lim_fuel
#'@export
#'@param object Sandbox
#'@return LM
#'@examples
#'library(Sandbox)
#'library(Bat)
#'library(CHP)
#'library(PubGfuel)
#'library(Demth)
#'library(Demel)
#'cl<-new.Sandbox()
#'name(cl) <- 'Sandy'
#'timegrid(cl) <- rep(15,10)
#'chp <- new.CHP()
#'bat1 <- new.Bat()
#'bat2 <- new.Bat()
#'fuel <- new.PubGfuel()
#'variables(fuel) <- "P_fuel_plus"
#'demth <- new.Demth()
#'demel <- new.Demel()
#'#Let us connect these things
#'components(cl) <- list(chp,bat1,bat2,fuel,demth,demel)
#'#connect CHP with both batteries (limited to 100 kW)
#'adjacency_el(cl) <- rbind(c(0,100,100,0,0,100),c(0,0,0,0,0,100),c(0,0,0,0,0,50),c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0));
#'adjacency_th(cl) <- rbind(c(0,0,0,0,100,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0));
#'adjacency_fuel(cl) <- rbind(c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(100,0,0,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0));
#'cl <- finalcoordinates(cl)
#'x <- LM.Adja_Lim_fuel(cl)
#'a <- as.data.frame.LM(x)
#'a
setMethod("LM.Adja_Lim_fuel", signature(object = "Sandbox"), function(object) {
  if (length(coord(object)) > 0) {
    if (dim(adjacency_fuel(object))[1] != 0) {
      m <- adjacency_fuel(object)

      n <- dim(m)[1]
      M <- matrix(0,0,length(coord(object)))

      V <- numeric()

      te <- length(timegrid(object))

      for (i in 1:n) {
        for (j in 1:n) {
          if (m[i, j] > 0) {
            M <- rbind(M, P_fuel(object, i, j))

            V <- c(V, rep(m[i, j], te))

          }
        }
      }
      if (length(V) == 0) {
        M <- matrix(0, 0, length(coord(object)))
        colnames(M) <- coord(object)

      } else{
        row.names(M) <-
          paste0(name(object), " Adja_Lim_fuel ", row.names(M))
      }
      new(
        "LM",
        matrix = M,
        direction = rep("<=", length(V)),
        vector = V,
        binary = grepl("Op", coord(object)),
        cost = rep(0, dim(M)[2])
      )

    } else{
      print("no adajency_fuel available")
    }
  } else{
    stop(print("Coordinates (name and timegrid) are necessary!"))
  }

})



#----LM.Adja_Lim_th------------------------------------------------------
#'LM.Adja_Lim_th
#'
#'build the LM describing all the power constraints in the thermal Sandbox
#'
#'@importFrom Root LM.Adja_Lim_th
#'@export
#'@param object Sandbox
#'@return LM
#'@examples
#'library(Sandbox)
#'library(Bat)
#'library(CHP)
#'library(PubGfuel)
#'library(Demth)
#'library(Demel)
#'cl<-new.Sandbox()
#'name(cl) <- 'Sandy'
#'timegrid(cl) <- rep(15,10)
#'chp <- new.CHP()
#'bat1 <- new.Bat()
#'bat2 <- new.Bat()
#'fuel <- new.PubGfuel()
#'variables(fuel) <- "P_fuel_plus"
#'demth <- new.Demth()
#'demel <- new.Demel()
#'#Let us connect these things
#'components(cl) <- list(chp,bat1,bat2,fuel,demth,demel)
#'#connect CHP with both batteries (limited to 100 kW)
#'adjacency_el(cl) <- rbind(c(0,100,100,0,0,100),c(0,0,0,0,0,100),c(0,0,0,0,0,50),c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0));
#'adjacency_th(cl) <- rbind(c(0,0,0,0,100,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0));
#'adjacency_fuel(cl) <- rbind(c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(100,0,0,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0));
#'cl <- finalcoordinates(cl)
#'x <- LM.Adja_Lim_th(cl)
#'a <- as.data.frame.LM(x)
#'a

setMethod("LM.Adja_Lim_th", signature(object = "Sandbox"), function(object) {
  if (length(coord(object)) > 0) {
    if (dim(adjacency_th(object))[1] != 0) {
      m <- adjacency_th(object)

      n <- dim(m)[1]
      M <- matrix(0,0,length(coord(object)))
      V <- numeric()

      te <- length(timegrid(object))

      for (i in 1:n) {
        for (j in 1:n) {
          if (m[i, j] > 0) {
            M <- rbind(M, P_th(object, i, j))

            V <- c(V, rep(m[i, j], te))

          }
        }
      }
      if (length(V) == 0) {
        M <- matrix(0, 0, length(coord(object)))
        colnames(M) <- coord(object)

      } else{
        row.names(M) <- paste0(name(object), " Adja_Lim_th ", row.names(M))
      }

      new(
        "LM",
        matrix = M,
        direction = rep("<=", length(V)),
        vector = V,
        binary = grepl("Op", coord(object)),
        cost = rep(0, dim(M)[2])
      )

    } else{
      print("no adajency_th available")
    }

  } else{
    stop(print("Coordinates (name and timegrid) are necessary!"))
  }

})


#----LM.Adja_Wat_el-----------
#'LM.Adja_Wat_el
#'
#'@importFrom Root LM.Adja_Wat_el
#'@param object Sandbox
#'@return LM
#'
#'build the LM describing all the electric constraints of the Wados in the  Sandbox
#'
#'@param object Sandbox
#'@return LM
#'@export
#'@examples
#'library(Sandbox)
#'library(Bat)
#'library(CHP)
#'library(PubGfuel)
#'library(Demth)
#'library(Demel)
#'library(Wado)
#'cl<-new.Sandbox()
#'name(cl)<-'Sandy'
#'timegrid(cl)<-rep(15,3)
#'chp<-new.CHP()
#'bat1<-new.Bat()
#'maxE_el(bat1)<-100
#'maxP_el_plus(bat1)<-15
#'bat2 <- new.Bat()
#'maxE_el(bat2)<-100
#'maxP_el_plus(bat2)<-15
#'fuel<-new.PubGfuel()
#'variables(fuel)<- "P_fuel_plus"
#'demth<-new.Demth()
#'demel<-new.Demel()
#'wado <- new.Wado()
#'variables(wado)<-c("P_el_plus","P_el_minus")
#'#Let us connect these things
#'components(cl)<-list(chp,bat1,bat2,fuel,demth,demel,wado)
#'#let bat1 and bat2 deliver electric energy via wado (limited by 200 kW/ limited by 150 kW) to demel
#'adjacency_el(cl)<-rbind(c(0,100,100,0,0,100,0),c(0,0,0,0,0,0,200),c(0,0,0,0,0,0,150),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,100,0));
#'adjacency_th(cl)<-rbind(c(0,0,0,0,100,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0));
#'adjacency_fuel(cl)<-rbind(c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(100,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0));
#' #Let spy wado bat1 with variable "P_el_plus"
#'adjacencyWatch_el(cl)<- rbind(c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0.9),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0));
#'adjacencyWatch_el_var(cl)<- rbind(c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,"P_el_plus"),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0));
#'cl<-finalcoordinates(cl)
#'x <- LM.Adja_Wat_el(cl)
#'a <- as.data.frame.LM(x)
#'a

setMethod("LM.Adja_Wat_el", signature(object = "Sandbox"), function(object) {
  if (length(coord(object)) > 0) {
    if (dim(adjacencyWatch_el(object))[1] > 0) {
      te <- length(timegrid(object))
      wados <- which(compoClass(object) == "Wado")

      n <- length(coord(object))
      M <- matrix(0,0,n)
      V <- numeric()
      D <- character()

      rownametmp <- character()
      #which Wado spys which components
      for (wa in wados) {
        spy <- which(adjacencyWatch_el(object)[, wa] != 0)
        for (sp in spy) {
          var <- adjacencyWatch_el_var(object)[sp, wa]
          if (length(do.call(paste0("max", var), components(object)[sp])) >
              0) {
            x1 <-
              -do.call(var, list(object, sp)) + do.call(paste0("max", var), components(object)[sp]) * adjacencyWatch_el(object)[sp, wa] *
              Op(object, wa)
            x2 <-
              do.call(var, list(object, sp)) -  do.call(paste0("max", var), components(object)[sp]) * (1 - adjacencyWatch_el(object)[sp, wa]) *
              Op(object, wa)
            x <- rbind(x1, x2)

            M <- rbind(M, x)
            V <-
              c(V,
                rep(0, te),
                rep(
                  do.call(paste0("max", var), components(object)[sp]) * adjacencyWatch_el(object)[sp, wa],
                  te
                ))
            D <- c(D, rep("<=", dim(x)[1]))

            rownametmp <- c(
              rownametmp,
              paste(
                "Upper bound: ",
                name(components(object)[[wa]]),
                " spy " ,
                name(components(object)[[sp]]),
                var,
                " time",
                seq_len(te)
              ),
              paste(
                "Lower bound: ",
                name(components(object)[[wa]]),
                " spy " ,
                name(components(object)[[sp]]),
                var,
                " time",
                seq_len(te)
              )
            )



          } else{
            stop(print(paste(
              paste0("max", var),
              "of component",
              sp,
              "is missing!"
            )))
          }
        }
        # which component gets energy from wa ?
        # deliver_wado <- which(adjacency_el(object)[wa,] != 0)
        # for (del in deliver_wado) {
        #   x <- P_el(object, wa, del) - P_el_plus(object, wa)
        #   M <- rbind(M, x)
        #   V <- c(V, rep(0, te))
        #   D <- c(D, rep("=", te))
        #   rownametmp <-
        #     c(rownametmp,
        #       paste0(
        #         name(components(object)[[wa]]),
        #         " controls " ,
        #         name(components(object)[[del]]),
        #         "time",
        #         seq_len(te)
        #       ))
        #
        # }


      }

      rownames(M) <- rownametmp

      return(new(
        "LM",
        matrix = M,
        direction = D,
        vector = V,
        binary = grepl("Op", coord(object)),
        cost = rep(0, dim(M)[2])
      ))
    } else{
      stop(print("adjacencyWat_el is necessary!"))
    }


  } else{
    stop(print("Coordinates (name and timegrid) are necessary!"))
  }

})


#----LM.Adja_Wat_fuel-----------
#'LM.Adja_Wat_fuel
#'
#'@importFrom Root LM.Adja_Wat_fuel
#'@export
#'@examples
#'library(Sandbox)
#'library(Bat)
#'library(CHP)
#'library(PubGfuel)
#'library(Demth)
#'library(Demel)
#'library(Wado)
#'cl<-new.Sandbox()
#'name(cl) <- 'Sandy'
#'timegrid(cl) <- rep(15,3)
#'chp <- new.CHP()
#'bat1 <- new.Bat()
#'maxE_el(bat1) <- 100
#'maxP_el_plus(bat1) <- 15
#'bat2 <- new.Bat()
#'maxE_el(bat2) <- 100
#'maxP_el_plus(bat2) <- 15
#'fuel <- new.PubGfuel()
#'variables(fuel) <- "P_fuel_plus"
#'maxP_fuel_plus(fuel) <- 300
#'minP_fuel_plus(fuel) <- 0
#'demth <- new.Demth()
#'demel <- new.Demel()
#'wado <- new.Wado()
#'variables(wado) <- c("P_fuel_plus","P_fuel_minus")
#'#Let us connect these things
#'components(cl) <- list(chp,bat1,bat2,fuel,demth,demel,wado)
#'#let fuel deliver fuel  via wado (limited by 200 kW) to chp
#'adjacency_el(cl) <- rbind(c(0,100,100,0,0,100,0),c(0,0,0,0,0,200,0),c(0,0,0,0,0,150,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0));
#'adjacency_th(cl) <- rbind(c(0,0,0,0,100,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0));
#'adjacency_fuel(cl) <- rbind(c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,100),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(200,0,0,0,0,0,0));
#' #Let spy wado fuel with variable "P_fuel_plus"
#'adjacencyWatch_fuel(cl) <- rbind(c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0.9),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0));
#'adjacencyWatch_fuel_var(cl) <- rbind(c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,"P_fuel_plus"),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0));
#'cl <- finalcoordinates(cl)
#'x <- LM.Adja_Wat_fuel(cl)
#'a <- as.data.frame.LM(x)
setMethod("LM.Adja_Wat_fuel", signature(object = "Sandbox"), function(object) {
  if (length(coord(object)) > 0) {
    if (dim(adjacencyWatch_fuel(object))[1] > 0) {
      te <- length(timegrid(object))
      wados <- which(compoClass(object) == "Wado")
      n <- length(coord(object))
      M <- matrix(0,0,n)
      V <- numeric()
      D <- character()
      rownametmp <- character()
      #which Wado spys which components
      for (wa in wados) {
        spy <- which(adjacencyWatch_fuel(object)[, wa] != 0)
        for (sp in spy) {
          var <- adjacencyWatch_fuel_var(object)[sp, wa]
          if (length(do.call(paste0("max", var), components(object)[sp])) >
              0) {
            x1 <-
              -do.call(var, list(object, sp)) + do.call(paste0("max", var), components(object)[sp]) * adjacencyWatch_fuel(object)[sp, wa] *
              Op(object, wa)
            x2 <-
              do.call(var, list(object, sp)) -  do.call(paste0("max", var), components(object)[sp]) * (1 - adjacencyWatch_fuel(object)[sp, wa]) *
              Op(object, wa)
            x <- rbind(x1, x2)

            M <- rbind(M, x)
            V <-
              c(V,
                rep(0, te),
                rep(
                  do.call(paste0("max", var), components(object)[sp]) * adjacencyWatch_fuel(object)[sp, wa],
                  te
                ))
            D <- c(D, rep("<=", dim(x)[1]))

            rownametmp <- c(
              rownametmp,
              paste(
                "Upper bound: ",
                name(components(object)[[wa]]),
                " spy " ,
                name(components(object)[[sp]]),
                var,
                " time",
                seq_len(te)
              ),
              paste(
                "Lower bound: ",
                name(components(object)[[wa]]),
                " spy " ,
                name(components(object)[[sp]]),
                var,
                " time",
                seq_len(te)
              )
            )



          } else{
            stop(print(paste(
              paste0("max", var),
              "of component",
              sp,
              "is missing!"
            )))
          }
        }



        # which component gets energy from wado ?
        # deliver_wado <- which(adjacency_fuel(object)[wa,] != 0)
        # for (del in deliver_wado) {
        #   x <- P_fuel(object, wa, del) - P_fuel_plus(object, wa)
        #   M <- rbind(M, x)
        #   V <- c(V, rep(0, te))
        #   D <- c(D, rep("=", te))
        #   rownametmp <-
        #     c(rownametmp,
        #       paste0(
        #         name(components(object)[[wa]]),
        #         " controls " ,
        #         name(components(object)[[sp]]),
        #         "time",
        #         seq_len(te)
        #       ))
        #
        # }


      }

      rownames(M) <- rownametmp

      return(new(
        "LM",
        matrix = M,
        direction = D,
        vector = V,
        binary = grepl("Op", coord(object)),
        cost = rep(0, dim(M)[2])
      ))
    } else{
      stop(print("adjacencyWat_fuel is necessary!"))
    }

  } else{
    stop(print("Coordinates (name and timegrid) are necessary!"))
  }

})

#----LM.Adja_Wat_th-----------
#'LM.Adja_Wat_th
#'
#'build the LM describing all the thermal constraints of the Wados in the  Sandbox
#'
#'If a wado spys a component the two equations are built:
#'
#'\itemize{
#'\item \eqn{maxE_th(spy)*threshold*Wado_Op \leq spy_E_th}
#'\item \eqn{maxE_th(spy)*(1-threshold)*Wado_Op \geq spy_E_th - maxE_th(spy)*threshold}
#'}
#'where threshold is the matrix entry of the adjacencyWatch_th.
#'
#'@importFrom Root LM.Adja_Wat_th
#'@param object Sandbox
#'@return LM
#'@export
#'@examples
#'library(Sandbox)
#'library(Bat)
#'library(CHP)
#'library(PubGfuel)
#'library(Demth)
#'library(Demel)
#'library(Wado)
#'cl<-new.Sandbox()
#'name(cl) <- 'Sandy'
#'timegrid(cl) <- rep(15,3)
#'chp <- new.CHP()
#'maxP_el_plus(chp) <- 13.2
#'minP_el_plus(chp) <- 13.2
#'effMaxP_el_plus(chp) <- 0.3
#'effMinP_el_plus(chp) <- effMaxP_el_plus(chp)
#'effMaxP_th_plus(chp) <- 0.62
#'effMinP_th_plus(chp) <- effMaxP_th_plus(chp)
#'price_maintenance(chp) <- 0
#'minDowntime(chp) <- 0
#'minRuntime(chp) <- 0
#'bat1 <- new.Bat()
#'maxE_el(bat1) <- 100
#'maxP_el_plus(bat1) <- 15
#'bat2 <- new.Bat()
#'maxE_el(bat2) <- 100
#'maxP_el_plus(bat2) <- 15
#'fuel <- new.PubGfuel()
#'variables(fuel) <- "P_fuel_plus"
#'# unlimited fuel
#'maxP_fuel_plus(fuel) <- -1
#'minP_fuel_plus(fuel) <- 0
#'demth <- new.Demth()
#'demel <- new.Demel()
#'wado <- new.Wado()
#'variables(wado) <- c("P_th_plus","P_th_minus")
#'#Let us connect these things
#'components(cl) <- list(chp,bat1,bat2,fuel,demth,demel,wado)
#'#let chp deliver thermal energy via wado (limited by 200 kW) to demth
#'adjacency_el(cl) <- rbind(c(0,100,100,0,0,100,0),c(0,0,0,0,0,200,0),c(0,0,0,0,0,150,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0));
#'adjacency_th(cl) <- rbind(c(0,0,0,0,0,0,200),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,200,0,0));
#'adjacency_fuel(cl) <- rbind(c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(100,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0));
#' #Let spy wado th with variable "P_th_plus"
#'adjacencyWatch_th(cl) <- rbind(c(0,0,0,0,0,0,0.9),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0));
#'adjacencyWatch_th_var(cl) <-  rbind(c(0,0,0,0,0,0,"P_th_plus"),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0));
#'cl <- finalcoordinates(cl)
#'x <- LM.Adja_Wat_th(cl)
#'a <- as.data.frame.LM(x)
setMethod("LM.Adja_Wat_th", signature(object = "Sandbox"), function(object) {
  if (length(coord(object)) > 0) {
    if (dim(adjacencyWatch_th(object))[1] > 0 &
        dim(adjacencyWatch_th_var(object))[1] > 0) {
      te <- length(timegrid(object))
      wados <- which(compoClass(object) == "Wado")
      n <- length(coord(object))
      M <- matrix(0,0,n)
      V <- numeric()
      D <- character()
      rownametmp <- character()
      #which Wado spys which components
      for (wa in wados) {
        spy <- which(adjacencyWatch_th(object)[, wa] != 0)
        for (sp in spy) {
          var <- adjacencyWatch_th_var(object)[sp, wa]
          if (length(do.call(paste0("max", var), components(object)[sp])) >
              0) {
            x1 <-
              -do.call(var, list(object, sp)) + do.call(paste0("max", var), components(object)[sp]) * adjacencyWatch_th(object)[sp, wa] *
              Op(object, wa)
            x2 <-
              do.call(var, list(object, sp)) -  do.call(paste0("max", var), components(object)[sp]) * (1 - adjacencyWatch_th(object)[sp, wa]) *
              Op(object, wa)
            x <- rbind(x1, x2)

            M <- rbind(M, x)
            V <-
              c(V,
                rep(0, te),
                rep(
                  do.call(paste0("max", var), components(object)[sp]) * adjacencyWatch_th(object)[sp, wa],
                  te
                ))
            D <- c(D, rep("<=", dim(x)[1]))

            rownametmp <- c(
              rownametmp,
              paste(
                "Upper bound: ",
                name(components(object)[[wa]]),
                " spy " ,
                name(components(object)[[sp]]),
                var,
                " time",
                seq_len(te)
              ),
              paste(
                "Lower bound: ",
                name(components(object)[[wa]]),
                " spy " ,
                name(components(object)[[sp]]),
                var,
                " time",
                seq_len(te)
              )
            )



          } else{
            stop(print(paste(
              paste0("max", var),
              "of component",
              sp,
              "is missing!"
            )))
          }
        }

        #has wado a virtual storage?
        if (ts_virtual(components(object)[[wa]]) == T) {
          #get all TS off wado


          conwado <-
            adjacency_th(object)[-dim(adjacency_th(object))[1], wa]
          ts_com <- compoClass(object) == 'TS'
          ts_vir <- which(ts_com * conwado != 0)
          x3 <- numeric()
          x4 <- numeric()
          maxT <- numeric()
          minT <- numeric()
          for (i in ts_vir) {
            x3 <-
              rbind(x3, P_th_plus(object, i) / (maxTemp(components(object)[[i]]) - minTemp(components(object)[[i]])))
            minT <- c(minT, minTemp(components(object)[[i]]))
            maxT <- c(maxT, maxTemp(components(object)[[i]]))
            x4 <- rbind(x4, P_th_minus(object, wa))

            rownametmp <-
              c(rownametmp,
                paste0(
                  name(components(object)[[wa]]),
                  " spy virtual TS " ,
                  name(components(object)[[i]]),
                  " time",
                  seq_len(te)
                ))
          }
          deltaT <- max(maxT) - min(minT)
          x5 <- x3 - x4 / deltaT


          M <- rbind(M, x5)
          V <- c(V, rep(0, dim(x5)[1]))
          D <- c(D, rep("=", dim(x5)[1]))

        }

      }


      rownames(M) <- rownametmp

      return(new(
        "LM",
        matrix = M,
        direction = D,
        vector = V,
        binary = grepl("Op", coord(object)),
        cost = rep(0, dim(M)[2])
      ))
    } else{
      stop(print(
        "adjacencyWatch_th and adjacencyWatch_th_var are necessary!"
      ))
    }

  } else{
    stop(print("Coordinates (name and timegrid) are necessary!"))
  }


})

#----LM.ComponentsConstraints---------------------------------------
#'LM.ComponentsConstraints
#'
#'build together the LM of the components ready to append it to the LM of the Sandbox
#'
#'@importFrom Root LM.ComponentsConstraints
#'@export
#'@param object Sandbox with components
#'@return LM. The block diagonal matrix of all the constraint matrices of the components
#'@examples
#'# build the Sandbox
#'library(Sandbox)
#'library(Demth)
#'library(GaBo)
#'library(HeSw)
#'library(PubGfuel)
#'library(PubGel)
#'cl <- new.Sandbox()
#'name(cl) <- "Sandy"
#'# build the heat demand
#'dem_th <- new.Demth()
#'load_15min_th(dem_th) <- rep(c(10, 4.2, 6, 7),50)
#'# build a gas boiler
#'gabo <- new.GaBo()
#'maxP_th_plus(gabo) <- 5
#'effMaxP_th_plus(gabo) <- 0.9
#'# build a heat sword
#'hesw <- new.HeSw()
#'maxP_el_minus(hesw) <- 10
#'effMaxP_el_minus(hesw) <- 0.99
#'pubgfuel <- new.PubGfuel()
#'variables(pubgfuel) <- "P_fuel_plus"
#'maxP_fuel_plus(pubgfuel)<- 1000
#'minP_fuel_plus(pubgfuel)<- 0
#'pubgel<-new.PubGel()
#'variables(pubgel) <- "P_el_plus"
#'maxP_el_plus(pubgel)<- 1000
#'minP_el_plus(pubgel)<- 0
#'components(cl) <- c(dem_th, gabo, hesw, pubgfuel,pubgel)
#'adjacency_th(cl) <- rbind(c(0,0,0,0,0),c(100,0,0,0,0),c(100,0,0,0,0),c(0,0,0,0,0),c(0,0,0,0,0))
#'adjacency_el(cl) <- rbind(c(0,0,0,0,0),c(0,0,0,0,0),c(0,0,0,0,0),c(0,0,0,0,0),c(0,0,100,0,0))
#'adjacency_fuel(cl) <- rbind(c(0,0,0,0,0),c(0,0,0,0,0),c(0,0,0,0,0),c(0,100,0,0,0),c(0,0,0,0,0))
#'adjacencyEff_th(cl) <- rbind(c(0,0,0,0,0),c(0.9,0,0,0,0),c(0.8,0,0,0,0),c(0,0,0,0,0),c(0,0,0,0,0))
#'adjacencyPrice_th(cl) <- rbind(c(0,0,0,0,0),c(0.9,0,0,0,0),c(0.8,0,0,0,0),c(0,0,0,0,0),c(0,0,0,0,0))
#'timegrid(cl) <- rep(15,10)
#'cl <- finalcoordinates(cl)
#'x <- LM.ComponentsConstraints(cl)
#'a<-as.data.frame.LM(x)

setMethod("LM.ComponentsConstraints", signature(object = "Sandbox"), function(object) {
  if (length(coord(object)) > 0) {
    #packages<-c("Sandbox",unique(compoClass(object)))
    # numCores <- detectCores()
    # registerDoParallel(numCores/2)

    k <- length(components(object))

    co <- coord(object)

    n <- length(co)

    if (k == 0) {
      m <- matrix(0, 0, n)

      colnames(m) <- co

      return(new("LM", matrix = m, cost = 0))
    }

    #coComp <- coordComp(object)
    co <- coord(object)

    MM <- list()

    DD <- character()

    VV <- numeric()

    CC <- rep(0,n)
    names(CC)<-co


    nn <- character()
    col <- character()
    coordtmp <- data.frame(Bezeichnung = coord(object))

    for (i in seq_len(k)) {

      s <- LM.Constraints(components(object)[[i]])
      print(paste("constraints successfully built: ",components(object)[[i]]@name))
      if(dim(s@matrix)[1]!=0){

          MM[[i]] <- s@matrix
          tmp <- t(MM[[i]])
          tmp <- data.frame(cbind(Bezeichnung = rownames(tmp), tmp))
          coordtmp <- merge(coordtmp, tmp, all = TRUE, sort = FALSE,by="Bezeichnung")
          DD <- c(DD, s@direction)
          cctmp<-s@cost
          #names(cctmp)<-coord(components(object)[[i]])
          CC[names(cctmp)]<- CC[names(cctmp)]+cctmp
          VV <- c(VV, s@vector)
     }
    }
    #s<-foreach(i=seq_len(k),.packages =packages )%dopar% LM.Constraints(components(object)[[i]])
    # for (i in seq_len(k)) {
    #   MM[[i]] <- s[[i]]@matrix
    #   tmp<-t(MM[[i]])
    #   tmp<-data.frame(cbind(Bezeichnung=rownames(tmp),tmp))
    #   coordtmp<-merge(coordtmp,tmp,all=TRUE,sort=FALSE)
    #   #coordtmp[-Bezeichnung,]
    #   #col <- c(col, colnames(s@matrix))
    #   DD <- c(DD, s[[i]]@direction)
    #
    #   VV <- c(VV, s[[i]]@vector)
    #
    #   # costtmp<-s@cost
    #   # costtmp<-data.frame(cbind(Bezeichnung=colnames(MM[[i]]),costtmp))
    #   # colnames(costtmp)<-c("Bezeichnung",paste0("cost",i))
    #   # costcoord<-merge(costcoord,costtmp,all=TRUE,by="Bezeichnung")
    #   # CC <- c(CC,s@cost)
    #
    #   #nn <- c(nn, row.names(MM[[i]]))
    # }


    MM <- t(coordtmp)
    colname <- MM[1, ]
    rowname <- rownames(MM)[-1]
    MM <- matrix(MM[-1, ], ncol = dim(MM)[2])
    MM[is.na(MM)] <- 0
    MM <- as.numeric(MM)
    MM <- matrix(MM, ncol = dim(coordtmp)[1])
    colnames(MM) <- colname
    rownames(MM) <- rowname

    order <- numeric(n)

    for (i in seq_len(n)) {
      order[i] <- grep(coord(object)[i], colnames(MM))
    }

    MM <- MM[, order]
    # costcoord[is.na(costcoord)]<-0
    # CC<-t(costcoord)
    # colname<-CC[1,]
    # CC<-matrix(CC[-1,],ncol = dim(CC)[2])
    # CC[is.na(CC)]<-0
    # CC<-as.numeric(CC)
    # CC<-matrix(CC,ncol = dim(costcoord)[1])
    # colnames(CC) <- colname
    # CC<-colSums(CC)


    # MM <- as.matrix(Matrix::bdiag(MM))

    #colnames(MM) <- coComp

    #row.names(MM) <- nn

    # if (n > length(coComp)) {
    #   MN <- matrix(0, dim(MM)[1], n - length(coComp))
    #
    #   MM <- cbind(MN, MM)
    #
    #   row.names(MM) <- nn
    #
    #   colnames(MM) <- coord(object)
    #
    #   CC <- c(rep(0, n - length(coComp)), CC)
    # }

    BB <- grepl("Op_", coord(object))

    P <- new(
      "LM",
      matrix = MM,
      vector = VV,
      direction = DD,
      cost = CC,
      #cost = rep(0, dim(MM)[2]),
      binary = BB
    )
    return(P)
  } else{
    stop(print("Coordinates (name and timegrid) are necessary!"))
  }

})
#----LM.Constraints-------------------------------------------------
#'LM.Constraints
#'
#'The Constraint LM for the DA optimization problem. This is done by binding
#'together are taken into account conditionally: If we have (P_el_plus or
#'P_el_minus) and maxP_el, then the inequality will be built in....
#'
#'@importFrom Root LM.Constraints
#'@export
#'@param object Sandbox
#'@return LM. The linear Model of DA optimization
#'@examples
#'# build the Sandbox
#'library(Sandbox)
#'library(Demth)
#'library(GaBo)
#'library(HeSw)
#'library(PubGfuel)
#'library(PubGel)
#'cl <- new.Sandbox()
#'name(cl) <- "Sandy"
#'# build the heat demand
#'dem_th <- new.Demth()
#'load_15min_th(dem_th) <- rep(c(10, 4.2, 6, 7),50)
#'# build a gas boiler
#'gabo <- new.GaBo()
#'maxP_th_plus(gabo) <- 5
#'effMaxP_th_plus(gabo) <- 0.9
#'# build a heat sword
#'hesw <- new.HeSw()
#'maxP_el_minus(hesw) <- 10
#'effMaxP_el_minus(hesw) <- 0.99
#'pubgfuel <- new.PubGfuel()
#'variables(pubgfuel) <- "P_fuel_plus"
#'maxP_fuel_plus(pubgfuel)<- 1000
#'minP_fuel_plus(pubgfuel)<- 0
#'pubgel<-new.PubGel()
#'variables(pubgel) <- "P_el_plus"
#'maxP_el_plus(pubgel)<- 1000
#'minP_el_plus(pubgel)<- 0
#'components(cl) <- c(dem_th, gabo, hesw, pubgfuel,pubgel)
#'adjacency_th(cl) <- rbind(c(0,0,0,0,0),c(100,0,0,0,0),c(100,0,0,0,0),c(0,0,0,0,0),c(0,0,0,0,0))
#'adjacency_el(cl) <- rbind(c(0,0,0,0,0),c(0,0,0,0,0),c(0,0,0,0,0),c(0,0,0,0,0),c(0,0,100,0,0))
#'adjacency_fuel(cl) <- rbind(c(0,0,0,0,0),c(0,0,0,0,0),c(0,0,0,0,0),c(0,100,0,0,0),c(0,0,0,0,0))
#'adjacencyEff_th(cl) <- rbind(c(0,0,0,0,0),c(0.9,0,0,0,0),c(0.8,0,0,0,0),c(0,0,0,0,0),c(0,0,0,0,0))
#'adjacencyPrice_th(cl) <- rbind(c(0,0,0,0,0),c(0.9,0,0,0,0),c(0.8,0,0,0,0),c(0,0,0,0,0),c(0,0,0,0,0))
#'timegrid(cl) <- rep(15,10)
#'cl <- finalcoordinates(cl)
#'x <- LM.Constraints(cl)

setMethod("LM.Constraints", signature(object = "Sandbox"), function(object) {
  if (length(coord(object)) > 0) {
    M <- list()

    x <- LM.ComponentsConstraints(object)

    M$Adja <- LM.Adja(object)

    for (i in 1:length(M)) {
      x@matrix <- rbind(x@matrix, M[[i]]@matrix)

      x@vector <- c(x@vector, M[[i]]@vector)

      x@direction <- c(x@direction, M[[i]]@direction)

      x@cost <- x@cost + M[[i]]@cost
    }
    validObject(x)
    return(x)

  } else{
    stop(print("Coordinates (name and timegrid) are necessary!"))
  }

})

#----LM.Adja-----------------------------------------------------
#'LM.Adja
#'
#'Build the LM describing the Energy conservation law in the Sandbox
#'as sum over all energy sources and sinks
#'
#'@importFrom Root LM.Adja
#'@export
#'@param object Sandbox
#'@return LM of linear equalities describing energy conservation
#'@examples
#'library(Sandbox)
#'library(Bat)
#'library(CHP)
#'library(PubGfuel)
#'library(Demth)
#'library(Demel)
#'cl<-new.Sandbox()
#'name(cl) <- 'Sandy'
#'timegrid(cl) <- rep(15,10)
#'chp <- new.CHP()
#'bat1 <- new.Bat()
#'bat2 <- new.Bat()
#'fuel <- new.PubGfuel()
#'variables(fuel) <- "P_fuel_plus"
#'demth <- new.Demth()
#'demel <- new.Demel()
#'#Let us connect these things
#'components(cl) <- list(chp,bat1,bat2,fuel,demth,demel)
#'#connect CHP with both batteries (limited to 100 kW)
#'adjacency_el(cl) <- rbind(c(0,100,100,0,0,100),c(0,0,0,0,0,100),c(0,0,0,0,0,50),c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0));
#'adjacency_th(cl) <- rbind(c(0,0,0,0,100,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0));
#'adjacency_fuel(cl) <- rbind(c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(100,0,0,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0));
#'cl <- finalcoordinates(cl)
#'x <- LM.Adja(cl)
#'a <- as.data.frame.LM(x)
setMethod("LM.Adja", signature(object = "Sandbox"), function(object) {
  if (length(coord(object)) > 0) {
    co <- coord(object)

    n <- length(co)
    te <- length(timegrid(object))
    k <- length(components(object))
    compConEl <- logical()

    M <- numeric()

    V <- numeric()

    C <- rep(0, n)

    D <- character()



    v <- LM.Adja_Lim_el(object)

    if (dim(adjacencyWatch_el(object))[1] != 0) {
      w <- LM.Adja_Wat_el(object)
      M <- rbind(M,  v@matrix, w@matrix)
      V <- c(V,  v@vector, w@vector)
      D <- c(D, v@direction, w@direction)
      C <- C + v@cost + w@cost



    } else{
      M <- rbind(M,  v@matrix)

      V <- c(V, v@vector)

      D <- c(D, v@direction)
      C <- C + v@cost
    }



    v <- LM.Adja_Lim_th(object)

    if (dim(adjacencyWatch_th(object))[1] != 0) {
      w <- LM.Adja_Wat_th(object)
      M <- rbind(M, v@matrix, w@matrix)
      V <- c(V, v@vector, w@vector)

      D <- c(D, v@direction, w@direction)
      C <- C + v@cost + w@cost

    } else{
      M <- rbind(M,  v@matrix)

      V <- c(V, v@vector)

      D <- c(D, v@direction)
      C <- C  + v@cost
    }
    v <- LM.Adja_Lim_fuel(object)

    if (dim(adjacencyWatch_fuel(object))[1] != 0) {
      w <- LM.Adja_Wat_fuel(object)
      M <- rbind(M,  v@matrix, w@matrix)
      V <- c(V, v@vector, w@vector)

      D <- c(D,  v@direction, w@direction)
      C <- C + v@cost + w@cost


    } else{
      M <- rbind(M,  v@matrix)

      V <- c(V, v@vector)

      D <- c(D, v@direction)
      C <- C +  v@cost
    }

    #add prices
    p <- LM.Adja_Price(object)
    C <- C + p@cost

    return(new(
      "LM",
      matrix = M,
      vector = V,
      direction = D,
      cost = C,
      binary = grepl("Op_", coord(object))
    ))
  } else{
    stop(print("Coordinates (name and timegrid) are necessary!"))
  }

})




#----LM.Flexa-----------------------------------------------------
#'LM.Flexa
#'
#'Build the LM describing the Energy conservation law in the Sandbox
#'as sum over all energy sources and sinks
#'@importFrom Root LM.Flexa
#'@param object Sandbox
#'@return LM method calculating the possible deviation of a given schedule
#'@export
#'
setMethod("LM.Flexa",
          signature(
            object = "Sandbox",
            schedule = "data.frame",
            direction = "character",
            which_time = "logical",
            cheap = "logical",
            flexMax = "missing",
            cost_flex ="missing"),
          function(object,
                   schedule,
                   direction,
                   which_time,
                   cheap
          ) {
           flexMax <- FlexMax(object)
           object <- LM.Constraints(object);

           LM.Flexa(object,
                    schedule,
                    direction,
                    which_time,
                    cheap,
                    flexMax,
                    numeric())

          })
#----LM.Flexa-----------------------------------------------------
#'LM.Flexa
#'
#'Build the LM describing the Energy conservation law in the Sandbox
#'as sum over all energy sources and sinks
#'
#'@importFrom Root LM.Flexa
#'@param object Sandbox
#'@return LM method calculating the possible deviation of a given schedule
#'@export
#'
setMethod("LM.Flexa",
          signature(
            object = "Sandbox",
            schedule = "data.frame",
            direction = "character",
            which_time = "logical",
            cheap = "logical",
            flexMax = "numeric",
            cost_flex = "numeric"
            ),
          function(object,
                   schedule,
                   direction,
                   which_time,
                   cheap,
                   flexMax,
                   cost_flex
          ) {

            object <- LM.Constraints(object);

            LM.Flexa(object,
                     schedule,
                     direction,
                     which_time,
                     cheap,
                     flexMax,
                     cost_flex)

          })




#----LM.Flexa-----------------------------------------------------
#'LM.Flexa
#'
#'Build the LM describing the Energy conservation law in the Sandbox
#'as sum over all energy sources and sinks
#'
#'@importFrom Root LM.Flexa
#'@export
#'@param object Sandbox
#'@return LM method calculating the possible deviation of a given schedule
#'
setMethod("LM.Flexa",
          signature(
            object = "LM",
            schedule = "data.frame",
            direction = "character",
            which_time = "logical",
            cheap = "logical",
            flexMax = "numeric",
            cost_flex = "numeric"
            ),
          function(object,
                   schedule,
                   direction,
                   which_time,
                   cheap,
                   flexMax,
                   cost_flex
          ) {
            #
            #---------------------------------------------------------
            #
            #errors and warnings
            #
            if (all(direction != c("positive", "negative"))) {
              #writeLines wegen Anfuehrungszeichen
              stop(writeLines("direction has to be \"positive\" or \"negative\""))
            }




            if (length(which_time) != dim(schedule)[1]) {
              stop(
                writeLines(
                  "which_time has to be of length dim(schedule)[1]."
                )
              )

            }

            te <- length(which_time)
            tet <- length(which(which_time))

            if (dim(schedule)[1] == 0) {
              stop(print("schedule dimension is zero"))
            }

            if(length(cost_flex)>0){
              stopifnot(length(which(which_time)) == length(cost_flex))
            }

            #---------------------------------------------------------
            #
            #the constraints of the system
            #
            x <- object

            xM <- x@matrix

            xV <- x@vector

            xD <- x@direction
            xB <- x@binary
            xC <- x@cost
            #


            #
            #the scheduled machines providing flexibility
            #
            nam <- colnames(schedule)

            #
            #define the additional coordinates, which are neccessary for FlexA
            #use a matrix trick and the recycling-technique of paste
            #
            coord_FlexA <- c(paste0("Flex_",direction,"_t", which(which_time)),

                             paste0("Flex_bin_t", which(which_time)))
            #
            #all coordinates
            #
            coordinates <- c(colnames(xM), coord_FlexA)


            #if cheap, then no change of operational status, when offering flexibility
            Flex_cheap<-new("LM",
                            matrix=matrix(0,0,length(coordinates)),
                            vector=numeric(0),
                            direction=character(0),
                            binary=rep(T,length(coordinates)),
                            cost = rep(0,length(coordinates))
            )



            if(cheap){
              Flex_cheap <- LM.Cheap(coordinates,which_time,schedule);
            }


            #P[which_time] = schedule[which_time] + sgn*flex

            Flex_flex <- LM.FlexaSchedule(coordinates,schedule,which_time,direction)
           
            


            #equality connecting the desired electric power of an object
            #to its schedule
            #P[!which_time] = schedule[!which_time]
            fixed_time <- !which_time


            Flex_fix<-LM.ScheduleFix(coordinates,fixed_time,
                                              schedule)
           

            
            nam<-colnames(schedule)

         
            # flex binary = 0 => flex = 0
            Flex_dom<-LM.FlexaDom(coordinates,which_time,flexMax)




            #
            #Flexibility is monotonically decreasing
            #FlexA[t+1] - FlexA[t] <= 0
            #
            Flex_mon<-LM.FlexaMon(coordinates,which_time)


            #
            #flex binary is monotonically decreasing
            #bin[t+1] - bin[t] <= 0
            #
            Flex_mon_bin<-LM.FlexaMonBin(coordinates,which_time)
            #
            #flex is locally constant
            # flex[t] - flex[t+1] + FlexMax*bin[t+1] <= FlexMax
            # 
            Flex_const<-LM.FlexaConst(coordinates,which_time,flexMax);
            #
            #the costs for flexibility
            #
            FlexAcost<-cost_flex;
            #
            if(length(cost_flex)==0){
            FlexAcost <- rep(-1,tet)/(1:tet)

            #
            # the flexibility is not more worth than the punish terms
            # (all other cost terms should be zero)
            # as we consider a physical model
            if (length(xC[xC > 0]) != 0) {
              FlexAcost[1:tet] <- 0.001 * min(xC[xC > 0])*FlexAcost[1:tet]

            }
            }
            #
            #The Flex_bin variables are weighted in a decreasing order
            #
            FlexAcostBin<- FlexAcost
            #
            #adjust the cost
            #
            Cost<-c(xC, FlexAcost,FlexAcostBin);
            
            names(Cost) <-coordinates
            #
            #
            #----------------------------------------------
            #
            #adjust the constraint matrix by adding zero columns
            xM <- cbind(xM, matrix(0, dim(xM)[1], length(coord_FlexA)))

            colnames(xM) <- coordinates
            #
            #----------------------------------------------

            #merge it with the FlexA constraint and expand the variables
            lm <- new(
              "LM",
              matrix = rbind(
                xM,
                Flex_flex@matrix,
                Flex_fix@matrix,
                Flex_mon@matrix,
                Flex_mon_bin@matrix,
                Flex_dom@matrix,
                Flex_const@matrix,
                Flex_cheap@matrix
              ),
              vector = c(
                xV,
                Flex_flex@vector,
                Flex_fix@vector,
                Flex_mon@vector,
                Flex_mon_bin@vector,
                Flex_dom@vector,
                Flex_const@vector,
                Flex_cheap@vector
              ),
              direction = c(
                xD,
                Flex_flex@direction,
                Flex_fix@direction,
                Flex_mon@direction,
                Flex_mon_bin@direction,
                Flex_dom@direction,
                Flex_const@direction,
                Flex_cheap@direction
              ),
              #at the end the Flex binaries and Flex_cheap for correction of Op's
              binary = c(xB, rep(F, length(coord_FlexA) / 2), rep(T, length(coord_FlexA) /
                                                                    2)) & Flex_cheap@binary,
              cost = Cost
            )

            return(lm)

          })

#----LM.Flexa-----------------------------------------------------
#'LM.Flexa
#'
#'Build the LM describing the Energy conservation law in the Sandbox
#'as sum over all energy sources and sinks
#'
#'@importFrom Root LM.Flexa
#'@export
#'@param object Sandbox
#'@return LM method calculating the possible deviation of a given schedule
#'
setMethod("LM.Flexa",
          signature(
            object = "LM",
            schedule = "data.frame",
            direction = "character",
            which_time = "logical",
            cheap = "logical",
            flexMax = "numeric",
            cost_flex = "missing"),
          function(object,
                   schedule,
                   direction,
                   which_time,
                   cheap,
                   flexMax

          ) {

            LM.Flexa(object,
                     schedule,
                     direction,
                     which_time,
                     cheap,
                     flexMax,
                     numeric())
          })

#'LM.Schedule
#'
#'build up linear model for fullfilling a schedule
#'
#'@importFrom Root LM.Schedule
#'@export
#'@param object Sandbox
#'@return LM
#'
setMethod("LM.Schedule",
          signature(
            object = "Sandbox",
            schedule = "data.frame",
            which_time = "logical",
            call = "data.frame",
            cheap = "logical"
            ),
          function(object,
                   schedule,
                   which_time ,
                   call,
                   cheap
          ) {

            #column names of schedule have to occure in component names of object
            nam<-compoNames(object);
            if(dim(schedule)[1]>0){

              #dimension of schedule has to match length of time grid
              if(length(timegrid(object))!=dim(schedule)[1]){
                stop("dim(schedule)[1] has to equal length(timegrid(object))")
              }


              #which_time has to lie within timesteps of schedule

              if(length(which_time)!=0){
                if(dim(schedule)[1]!=length(which_time)){
                  stop("need length(which_time) == dim(schedule)[1]")
                }
              }



              if(!all(is.element(colnames(schedule),nam))){
                stop("column names of schedule have to occure as component names of object")
              }
              if(dim(call)[1]>0){
                if(dim(call)[1]!=dim(schedule)[1]){
                  stop("dim(call)[1] has to equal dim(schedule)[1]")
                }
                if(!all(is.element(c("Abruf+","Abruf-"),colnames(call)))){
                  stop(writeLines("call needs columns \"Abruf+\" and \"Abruf-\" "))
                }
              }
            }




            #defining the coordinates for the schedule problem
            #
            #get coordinates of the sandbox
            coordinates <- coord(object)
            #
            #schedule changes have to be done without change of operational status
            #
            #
            Flexa_cheap<-Flexa_fix<-new("LM",
              matrix = matrix(0,0,length(coordinates)),
                             vector = numeric(0),
                             direction = character(0),
                             binary = rep(T,length(coordinates)),
              cost = rep(0,length(coordinates))
            );
            #
            #The two constraints above only can become non trivially, if there is a schedule
            #
            if(dim(schedule)[1]>0){
            #cheap or not cheap
            if(cheap){
              Flexa_cheap <- LM.Cheap(coordinates,which_time,schedule);
            }
            #
            #the new aggregated schedule
            #
            #
            nam <- colnames(schedule)
            if(dim(call)[1]>0){
              schedule <- as.numeric(rowSums(schedule) + call[,"Abruf+"] - call[,"Abruf-"]);
            }

            #
            #fix the schedule: for all objects i in schedule: P_el(i)[which_time,] = schedule[which_time,i]
            #
            Flexa_fix <- LM.ScheduleFix(coordinates,which_time,schedule,nam);

            }
            #
            #get the rest of the constraints
            #
            x <- LM.Constraints(object);
            xB <- x@binary & Flexa_cheap@binary
            #
            lm<-new("LM",
                    matrix = rbind(x@matrix,
                                   Flexa_cheap@matrix,
                                   Flexa_fix@matrix
                    ),
                    direction = c(x@direction,
                                  Flexa_cheap@direction,
                                  Flexa_fix@direction
                    ),
                    vector = c(x@vector,
                               Flexa_cheap@vector,
                               Flexa_fix@vector
                    ),
                    binary = xB,
                    cost = x@cost
            )

            return(lm)


          })
#'LM.Schedule_fix
#'
#'build up linear model for fullfilling schedules at different times
#'
#'@importFrom Root LM.Schedule_fix
#'@export
#'@param object Sandbox
#'@return LM
#'
setMethod("LM.Schedule_new",
          signature(
            object = "Sandbox",
            schedules = "list"
            ),
          function(object,
                   schedules

          ) {
            #column names of schedule have to occure in component names of object
            nam<-compoNames(object);
            if(!(length(schedules)==0)){
              if(is.null(names(schedules))){
                stop("schedules need names")
              }

              if(!all(is.element(names(schedules),nam))){
                stop("schedule names have to occure in component names")
              }
              for(schedule in schedules){
                if(!is(schedule,"data.frame")){
                  stop("schedule has to be data.frame")
                }
                if(length(timegrid(object))!=dim(schedule)[1]){
                    stop("dim(schedule)[1] has to equal length(timegrid(object))")
                }

                if(!all(is.element(c("fixed","value"),colnames(schedule)))){
                  stop("schedule has to have columns 'fixed' and 'value'")
                  }
              }
            }





            #defining the coordinates for the schedule problem
            #
            #get coordinates of the sandbox
            coordinates <- coord(object)
            #
            #schedule changes have to be done without change of operational status
            #
            #
            Flexa_cheap<-Flexa_fix<-new("LM",
              matrix = matrix(0,0,length(coordinates)),
                             vector = numeric(0),
                             direction = character(0),
                             binary = rep(T,length(coordinates)),
              cost = rep(0,length(coordinates))
            );
            #
            #The two constraints above only can become non trivially, if there is a schedule
            #
            if(length(schedules)!=0){

              Flexa_fix <- LM.ScheduleFix_new(coordinates,schedules);
            }
            #
            #get the rest of the constraints
            #
            x <- LM.Constraints(object);
            xB <- x@binary & Flexa_cheap@binary
            #
            lm<-new("LM",
                    matrix = rbind(x@matrix,
                                   Flexa_cheap@matrix,
                                   Flexa_fix@matrix
                    ),
                    direction = c(x@direction,
                                  Flexa_cheap@direction,
                                  Flexa_fix@direction
                    ),
                    vector = c(x@vector,
                               Flexa_cheap@vector,
                               Flexa_fix@vector
                    ),
                    binary = xB,
                    cost = x@cost
            )

            return(lm)


          })

#'LM.Schedule_fix
#'
#'build up linear model for fullfilling schedules at different times
#'
#'@importFrom Root LM.Schedule_fix
#'@export
#'@param object Sandbox
#'@return LM
#'
setMethod("LM.Schedule_slack",
          signature(
            object = "Sandbox",
            schedules = "list"
            ),
          function(object,
                   schedules
       
          ) {

            #column names of schedule have to occure in component names of object
            nam<-compoNames(object);
            if(!(length(schedules)==0)){
              if(is.null(names(schedules))){
                stop("schedules need names")
              }
      
              if(!all(is.element(names(schedules),nam))){
                stop("schedule names have to occure in component names")
              }
              
              for(schedule in schedules){
                if(!is(schedule,"data.frame")){
                  stop("schedule has to be data.frame")
                }
                if(length(timegrid(object))!=dim(schedule)[1]){
                    stop("dim(schedule)[1] has to equal length(timegrid(object))")
                }
                
                if(!all(is.element(c("fixed","value"),colnames(schedule)))){
                  stop("schedule has to have columns 'fixed' and 'value'")
                  }
              }
            }
            slack = character()
            if(length(schedules)>0){
  
              for(i in 1:length(schedules)){
                
                nam = str_split_fixed(names(schedules)[[i]],"_",2)[,2]
                
                slack = c(slack,paste0("slack_",nam,"_pos_time",1:length(timegrid(object))),
                paste0("slack_",nam,"_neg_time",1:length(timegrid(object)))
                )
              } 
            }
            #defining the coordinates for the schedule problem
            #
            #get coordinates of the sandbox
            coordinates <- coord(object)
            coordinates <- c(coordinates,slack)
            #
            #schedule changes have to be done without change of operational status
            #
            zero = matrix(0,0,length(coordinates))                 
            colnames(zero) = coordinates 
            #
            Flexa_fix<-new("LM",
              matrix = zero,
              cost = rep(0,length(coordinates)),
              vector = numeric(0),
              direction = character(0),
              binary = rep(T,length(coordinates))
            );
            #
            #The two constraints above only can become non trivially, if there is a schedule
            #
            if(length(schedules)!=0){
              Flexa_fix <- LM.ScheduleFix_slack(coordinates,schedules);
            }
            #
            #get the rest of the constraints
            #
            x <- LM.Constraints(object);
            xB <- c(x@binary,rep(FALSE,length(slack)))
            constr = cbind(x@matrix,matrix(0,dim(x@matrix)[1],length(slack)))
            colnames(constr) = coordinates

            where_ass <- grepl("(_Assfuel\\d_|_Assel\\d_|_Assth\\d_)", coord(object))
            # we get every cost term associated to an assist
            p1<- min(x@cost[where_ass])
            p2<-max(x@cost[!where_ass])
            if(p2==-Inf|p2==Inf|p2<0){
              p2<-0
            }
            if(p1==-Inf|p1==Inf|(p1<=p2)){
              print("either no assists or costs of assists lower than maximal non-assist costs:")
              print("setting slack costs for contracts simply higher than max of costs...")
              p1<-(p2 + 110)*10
            }
            # if the minimal assist prices are smaller than the ususal prices 
            # we do not consider them as punishing terms 
            p <- (p1/2 + p2/2)
            
           
          
            lm<-new("LM",
                    matrix = rbind(constr,
                                   Flexa_fix@matrix
                    ),
                    direction = c(x@direction,
                                  Flexa_fix@direction
                    ),
                    vector = c(x@vector,
                               Flexa_fix@vector
                    ),
                    binary = xB,
                    cost = c(x@cost,rep(p,length(slack))*tau(object))
            )
            # print("The cost function")
            # print(data.frame(cost=c(x@cost,rep(p,length(slack))*tau(object)),coord=coordinates))

            return(lm)


          })
#'LM.Shave
#'
#'Build the LM describing the Energy conservation law in the Sandbox
#'as sum over all energy sources and sinks
#'
#'@importFrom Root LM.Shave
#'@export
#'@param object Sandbox
#'@return LM method minimizing the  maximal Power at an  object
#'
setMethod("LM.Shave",
            signature(
            object = "Sandbox",
            which_time = "logical",
            compo_names = "character",
            cost_shave = "numeric"),
          function(object,
                   which_time,
                   compo_names,
                   cost_shave

          ) {
            #
            #if no timesteps are specified, all timesteps are considered
            #
            if(length(which_time)==0){
              which_time <- rep(T,length(object@timegrid))
            }



            #which_time has to lie within timesteps of schedule

            if(length(which_time)!=length(object@timegrid)){

                stop("length(which_time) has to be equal to length(object@timegrid)")

            }

            nam <- compoNames(object);
            classes <-compoClass(object);
            if(!any(classes=="PubGel")){
              stop(writeLines("Need a component of class \"PubGel\" in object to perform shaving"))
            }

            if(length(compo_names)!=0){
              if(!all(is.element(compo_names,nam))){
                stop("compo_names have to occure in compoNames(object)")
              }
            }else{

              compo_names <- nam[classes=="PubGel"];
            }




            #defining the coordinates for the schedule problem
            #
            #get coordinates of the sandbox
            #
            coordinates <- c(coord(object),paste0("P_max-el_",str_split_fixed(compo_names,"_",Inf)[,2])
                             );
            #
            #Pel <= PelMax
            #
            Shave_dom <- LM.ShaveDom(coordinates,which_time,compo_names)
            #
            #the model
            #
            x <- LM.Constraints(object);
            #
            #
            xM <- x@matrix
            #
            #add the new columns
            #
            xM <- cbind(xM,matrix(0,dim(xM)[1],length(compo_names)))
            #
            colnames(xM)<-coordinates
            #

            xV <- x@vector

            xD <- x@direction

            xB <- x@binary
            #
            #the new variables/the shave_level is no binary variable
            #
            xB <- c(xB,rep(F,length(compo_names)))
            #

            xC <- x@cost
            #
            #adjust the cost function
            #
            shave_cost <- cost_shave;
            if(length(cost_shave)==0){
            shave_cost <- 1

            #
            #if there are Assists, the shaving costs are smaller than the costs of the assists
            #
            if (length(xC[xC > 0]) != 0){
              shave_cost <- 0.1 * min(xC[xC > 0])
            }
            }
            #
            #add the costs for shaving
            #
            xC <- c(xC,rep(shave_cost,length(compo_names)));
            #

            lm <- new("LM",
                      matrix = rbind(xM,
                                     Shave_dom@matrix),

                      direction = c(xD,
                                    Shave_dom@direction),

                      vector = c(xV,
                                 Shave_dom@vector),

                      binary = xB,

                      cost = xC
                      )


            return(lm)


          })

#'LM.Shave
#'
#'Build the LM describing the Energy conservation law in the Sandbox
#'as sum over all energy sources and sinks
#'
#'@importFrom Root LM.Shave
#'@export
#'@param object Sandbox
#'@return LM method minimizing the  maximal Power at an  object
#'
setMethod("LM.Shave",
          signature(
            object = "Sandbox",
            which_time = "logical",
            compo_names = "character",
            cost_shave = "missing"),
          function(object,
                   which_time,
                   compo_names


          ) {
            LM.Shave(object,
                     which_time,
                     compo_names,
                     numeric())

          })
