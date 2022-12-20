#----The Validity of Sandbox-------------------------------
check.Sandbox<-function(object){
  errors<-character();
  l<-logical();
  if(length(object@components)>=1){
    for(i in length(object@components)){
      l<-c(l,is(object@components[[i]],"Root"));
    }
    if(any(l)==F){
      msg<-"All components have to be Root";
      errors<-c(msg,errors)
    }
  }



  #adjacency
  ###############




  if(dim(object@adjacencyEff_el)[1]!=0&dim(adjacency_el(object))[1]!=0){
    if(any(dim(object@adjacencyEff_el)!=dim(adjacency_el(object)))){
      msg<-"adjacencyEff_el has to be of the same dimension as adjacency_el"
      errors<-c(msg,errors)
    }else{if(any(object@adjacencyEff_el[adjacency_el(object)==0]!=0)){
     msg<-"adjacencyEff_el has to be zero where adjacency_el is zero"
     errors<-c(msg,errors)
     }
    }
  }

  if(dim(object@adjacencyEff_th)[1]!=0&dim(object@adjacency_th)[1]!=0){
    if(any(dim(object@adjacencyEff_th)!=dim(object@adjacency_th))){
      msg<-"adjacencyEff_th has to be of the same dimension as adjacency_th"
      errors<-c(msg,errors)
    }else{ if(any(object@adjacencyEff_th[object@adjacency_th==0]!=0)){
     msg<-"adjacencyEff_th has to be zero where adjacency_th is zero"
     errors<-c(msg,errors)
     }
    }
  }

  if(dim(object@adjacencyEff_fuel)[1]!=0&dim(object@adjacency_fuel)[1]!=0){
    if(any(dim(object@adjacencyEff_fuel)!=dim(object@adjacency_fuel))){
      msg<-"adjacencyEff_fuel has to be of the same dimension as adjacency_fuel"
      errors<-c(msg,errors)
    }
       if(any(object@adjacencyEff_fuel[object@adjacency_fuel==0]!=0)){
     msg<-"adjacencyEff_fuel has to be zero where adjacency_fuel is zero"
       errors<-c(msg,errors)
       }

  }



  if(length(object@adjacencyPrice_el)!=0){
    if(dim(object@adjacencyPrice_el)[1]!=0 & dim(adjacency_el(object))[1]!=0){
      if(any(dim(object@adjacencyPrice_el)[-3]!=dim(adjacency_el(object)))){
        msg<-"adjacencyPrice_el has to be of the same dimension as adjacency_el"
        errors<-c(msg,errors)
      }
      # if(any(object@adjacencyPrice_el[adjacency_el(object)==0]!=0)){
      #   msg<-"adjacencyPrice_el has to be zero where adjacency_el is zero"
      #   errors<-c(msg,errors)
      # }

    }

    # if(dim(object@adjacencyPrice_el)[3]!=0 & length(object@timegrid)!=0){
    #   if(dim(object@adjacencyPrice_el)[3]!=length(object@timegrid)){
    #     msg <- "time dimension of adjacencyPrice_el and length of timegrid have to agree"
    #     errors<-c(msg,errors)
    #   }
    #
    # }
  }
  if(length(object@adjacencyPrice_th)!=0){
    if(dim(object@adjacencyPrice_th)[1]!=0&dim(adjacency_th(object))[1]!=0){
      if(any(dim(object@adjacencyPrice_th)[-3]!=dim(adjacency_th(object)))){
        msg<-"adjacencyPrice_th has to be of the same dimension as adjacency_th"
        errors<-c(msg,errors)
      # }else{ if(any(object@adjacencyPrice_th[object@adjacency_th==0]!=0)){
      #  msg<-"adjacencyPrice_th has to be zero where adjacency_th is zero"
      #  errors<-c(msg,errors)
      #  }
      }
    }
  }

  
  if(length(object@adjacencyPrice_fuel)!=0){
    if(dim(object@adjacencyPrice_fuel)[1]!=0&dim(adjacency_fuel(object))[1]!=0){
      if(any(dim(object@adjacencyPrice_fuel)[-3]!=dim(adjacency_fuel(object)))){
        msg<-"adjacencyPrice_fuel has to be of the same dimension as adjacency_fuel"
        errors<-c(msg,errors)
      }
         # if(any(object@adjacencyPrice_fuel[object@adjacency_fuel==0]!=0)){
         # msg<-"adjacencyPrice_fuel has to be zero where adjacency_fuel is zero"
         # errors<-c(msg,errors)
         # }
    }
  }



  if(dim(object@adjacencyWatch_el)[1]!=0&dim(adjacency_el(object))[1]!=0){
    if(any(dim(object@adjacencyWatch_el)!=dim(adjacency_el(object)))){
      msg<-"adjacencyWatch_el has to be of the same dimension as adjacency_el"
      errors<-c(msg,errors)
    }
  }

  if(dim(adjacencyWatch_el(object))[1]!=0&dim(adjacencyWatch_el_var(object))[1]!=0){
    if(any(dim(object@adjacencyWatch_el)!=dim(adjacencyWatch_el_var(object)))){
      msg<-"adjacencyWatch_el has to be of the same dimension as adjacencyWatch_el_var"
      errors<-c(msg,errors)
    }
    if(any(object@adjacencyWatch_el[object@adjacencyWatch_el_var==0]!=0) | any(object@adjacencyWatch_el_var[object@adjacencyWatch_el==0]!=0)){
        msg<-"adjacencyWatch_el has to be zero where adjacencyWatch_el_var is zero and vise versa"
        errors<-c(msg,errors)
        }
  }

  if(dim(object@adjacencyWatch_fuel)[1]!=0&dim(object@adjacency_fuel)[1]!=0){
    if(any(dim(object@adjacencyWatch_fuel)!=dim(object@adjacency_fuel))){
      msg<-"adjacencyWatch_fuel has to be of the same dimension as adjacency_fuel"
      errors<-c(msg,errors)
    }

  }

  if(dim(adjacencyWatch_fuel(object))[1]!=0 & dim(adjacencyWatch_fuel_var(object))[1]!=0){
    if(any(dim(object@adjacencyWatch_fuel)!=dim(adjacencyWatch_fuel_var(object)))){
      msg<-"adjacencyWatch_fuel has to be of the same dimension as adjacencyWatch_fuel_var"
      errors<-c(msg,errors)
    }
    if(any(object@adjacencyWatch_fuel[object@adjacencyWatch_fuel_var==0]!=0) | any(object@adjacencyWatch_fuel_var[object@adjacencyWatch_fuel==0]!=0)){
      msg<-"adjacencyWatch_fuel has to be zero where adjacencyWatch_fuel_var is zero and vise versa"
      errors<-c(msg,errors)
    }
  }

  if(dim(object@adjacencyWatch_th)[1]!=0 & dim(object@adjacency_th)[1]!=0){
    if(any(dim(object@adjacencyWatch_th)!=dim(object@adjacency_th))){
      msg<-"adjacencyWatch_th has to be of the same dimension as adjacency_th"
      errors<-c(msg,errors)
    }

  }

  if(dim(adjacencyWatch_th(object))[1]!=0 & dim(adjacencyWatch_th_var(object))[1]!=0){
    if(any(dim(object@adjacencyWatch_th)!=dim(adjacencyWatch_th_var(object)))){
      msg<-"adjacencyWatch_th has to be of the same dimension as adjacencyWatch_th_var"
      errors<-c(msg,errors)
    }
    if(any(object@adjacencyWatch_th[object@adjacencyWatch_th_var==0]!=0) | any(object@adjacencyWatch_th_var[object@adjacencyWatch_th==0]!=0)){
      msg<-"adjacencyWatch_th has to be zero where adjacencyWatch_th_var is zero and vise versa"
      errors<-c(msg,errors)
    }
  }

  if (length(errors) == 0) {
    return(TRUE)
  } else{
    return(errors)
  }
}


#----------------------------------------------------------
#----Definition of Class Sandbox---------------------------
#'Sandbox Class
#'
#'Sandbox represents a Sandbox of energyprosumers (EP), which can be correlated
#'by energy conversation laws. A Sandbox may be used to model a house or to
#'model simple timegrid.
#'
#'@export
#'@slot adjacency_el The adjacency matrix of the electric grid inside the Sandbox
#'@slot adjacency_th The adjacency matrix of the thermal grid inside the Sandbox
#'@slot adjacency_fuel The adjacency matrix of the fuel grid inside the Sandbox
#'@slot adjacencyEff_el The adjacencyEff matrix of the electric grid inside the Sandbox
#'@slot adjacencyEff_th The adjacencyEff matrix of the thermal grid inside the Sandbox
#'@slot adjacencyEff_fuel The adjacencyEff matrix of the fuel grid inside the Sandbox
#'@slot adjacencyPrice_el The adjacencyPrice matrix contains the  electric prices of the single connection between the components of the Sandbox
#'@slot adjacencyPrice_th The adjacencyPrice matrix contains the  thermal prices of the single connection between the components of the Sandbox
#'@slot adjacencyPrice_fuel The adjacencyPrice matrix  contains the  fuel prices of the single connection between the components of the Sandbox
#'@slot adjacencyWatch_el The adjacencyWatch matrix of the electric grid inside the Sandbox
#'@slot adjacencyWatch_th The adjacencyWatch matrix of the thermal grid inside the Sandbox
#'@slot adjacencyWatch_fuel The adjacencyWatch matrix of the fuel grid inside the Sandbox
#'@slot adjacencyWatch_el_var The adjacencyWatch matrix, containing the spy variable, of the electric grid inside the Sandbox
#'@slot adjacencyWatch_th_var The adjacencyWatch matrix, containing the spy variable, of the thermal grid inside the Sandbox
#'@slot adjacencyWatch_fuel_var The adjacencyWatch matrix, containing the spy variable, of the fuel grid inside the Sandbox
#'@slot components An object of class list. The list entries have to be Root, which are the components of the Sandbox
setClass("Sandbox",representation = list(adjacency_el = "matrix",
                                         adjacency_th = "matrix",
                                         adjacency_fuel = "matrix",
                                         adjacencyEff_el = "matrix",
                                         adjacencyEff_th = "matrix",
                                         adjacencyEff_fuel = "matrix",
                                         adjacencyPrice_el = "array",
                                         adjacencyPrice_th = "array",
                                         adjacencyPrice_fuel = "array",
                                         adjacencyWatch_el = "matrix",
                                         adjacencyWatch_th = "matrix",
                                         adjacencyWatch_fuel = "matrix",
                                         adjacencyWatch_el_var = "matrix",
                                         adjacencyWatch_th_var = "matrix",
                                         adjacencyWatch_fuel_var = "matrix",
                                         components = "list"
),
contains = "Root",
validity = check.Sandbox);
#------------------------------------------------
#Constructor of Class Sandbox
#'
#'create a new obejct of class Sandbox
#'
#'@export
#'@return New object of class Sandbox
new.Sandbox <- function() {
  return(new(Class = "Sandbox"))
}

