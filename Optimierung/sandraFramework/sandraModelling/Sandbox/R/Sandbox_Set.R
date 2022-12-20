#--Overview-------
# adjacency_el
# adjacency_fuel
# adjacency_th
# adjacencyEff_el
# adjacencyEff_fuel
# adjacencyEff_th
# adjacencyPrice_el
# adjacencyPrice_fuel
# adjacencyPrice_th
# adjacencyWatch_el
# adjacencyWatch_el_var
# adjacencyWatch_fuel
# adjacencyWatch_fuel_var
# adjacencyWatch_th
# adjacencyWatch_th_var
# name
# timegrid
#----------------------
#----adjacency_el------------------------------------------
#'adjacency_el (Set)
#'
#'Set adjacency_el, the adjacency matrix of the graph of the electric Sandbox
#'The matrix has to be built in the following way
#'\itemize{
#'\item The matrix has to be quadratic with the dimension k (where k is the number of components )
#'\item If a connection between two components is available, the value of th matrix is non zero, e.g. if th first component is delivering to the second component adjacency_el(object)[1,2]!=0
# #'\item The value describes the upper limitation of the connection
#'\item Each row contains the delivery of a component, i.e. each column contains the consume of a component.
#'\item If the connection should be unlimted the value has to be negativ, otherwise the value of the entry of matrix describes the limitation in kW of the connection.
#'\item A component can only consume if the component has the variable "P_el_minus"
#'\item A component can only deliver if the component has the varialbe "P_el_plus"
#'}
#'@importFrom Root adjacency_el<-
#'@examples
#'library(Sandbox)
#'library(Bat)
#'library(PubGel)
#'library(Demel)
#'cl<-new.Sandbox()
#'name(cl)<-'Sandy'
#'timegrid(cl)<-rep(15,10)
#'bat1<-new.Bat()
#'bat2<-new.Bat()
#'pubgel<-new.PubGel()
#'variables(pubgel)<- "P_el_plus"
#'demel<-new.Demel()
#'components(cl)<-list(pubgel,bat1,bat2,demel)
#'#Let bat1 and bat2 get electrical energy from pubgel(limited by 100/150) and deliver the energy to demel (limited by 100/150)
#'adjacency_el(cl)<-rbind(c(0,100,150,0),c(0,0,0,100),c(0,0,0,150),c(0,0,0,0));
#'cl<-finalcoordinates(cl)
#'coord(cl)
#'
#'#if all possible components should be connected with each other.
#'#Within the following example, pubgel delivers energy to bat1 and demel and bat1 delivers also energy to demel
#'cl<-new.Sandbox()
#'name(cl)<-'Sandy'
#'timegrid(cl)<-rep(15,10)
#'bat1<-new.Bat()
#'pubgel<-new.PubGel()
#'variables(pubgel)<-"P_el_plus"
#'demel<-new.Demel()
#'components(cl)<-list(pubgel,bat1,demel)
#'cl<-finalcoordinates(cl)
#'coord(cl)
#'@export
setMethod("adjacency_el<-", signature(object="Sandbox",value="matrix"), function(object, value) {

  errors<-character();
  if(dim(value)[1]!=dim(value)[2]){
    msg<-"adjacency_el has to be quadratic";
    errors<-c(msg,errors)

  }
  k<-length(components(object))

  if(dim(value)[2]>0){
    if((k)!=dim(value)[2]){
      msg<-"Dimension of adjacency_el does not correspond to the number of components";
      errors<-c(msg,errors)
    }
  }

  if((k)*dim(value)[1]>0){
    for(i in seq_len(k)){
      if(any(value[i,]!=0)&all(object@components[[i]]@variables!="P_el_plus")){
        msg<-paste("adjacency_el implies that component",i,"can deliver electric energy, but there is no variable P_el_plus")
        errors<-c(msg,errors)
      }

      if(any(value[,i]!=0)&all(object@components[[i]]@variables!="P_el_minus")){
        msg<-paste("adjacency_el implies that component",i,"can consume electric energy, but there is no variable P_el_minus")
        errors<-c(msg,errors)
      }
    }


  }

  if(length(errors)==0){
    co<-compoNames(object)
    comp<-str_split_fixed(co,"_",2)[,2]
    rownames(value)<-comp
    colnames(value)<-comp
    object@adjacency_el <- value
    #update coordinates
    #name(object)<-name(object)
    validObject(object)
  }else{print(errors)}


  return(object)
})

#----adjacency_th------------------------------------------
#'adjacency_th (Set)
#'
#'Set adjacency_th, the adjacency matrix of the graph of the thermal Sandbox
#'
#'The matrix has to be built in the following way
#'\itemize{
#'\item The matrix has to be quadratic with the dimension k (where k is the number of components )
#'\item If a connection between two components is available, the value of th matrix is non zero, e.g. if th first component is delivering to the second component adjacency_th(object)[1,2]!=0
# #'\item The value describes the upper limitation of the connection
#'\item Each row contains the delivery of a component, i.e. each column contains the consume of a component.
#'\item If the connection should be unlimted the value has to be negativ, otherwise the value of the entry of matrix describes the limitation in kW of the connection.
#'\item A component can only consume if the component has the variable "P_th_minus"
#'\item A component can only deliver if the component has the varialbe "P_th_plus"
#'}
#'@importFrom Root adjacency_th<-
#'@examples
#'library(Sandbox)
#'library(PubGth)
#'library(Demth)
#'library(TS)
#'cl <- new.Sandbox()
#'name(cl) <- 'Sandy'
#'timegrid(cl)<-rep(15,3)
#'ts1 <- new.TS()
#'ts2 <- new.TS()
#'pubgth <- new.PubGth()
#'variables(pubgth)<- "P_th_plus"
#'demth <- new.Demth()
#'#Let us connect these things
#'components(cl)<-list(pubgth,ts1,ts2,demth)
#'#Let ts1 and ts2 get thermal energy from pubgth(limited by 100/150) and deliver the energy to demth (limited by 100/150)
#'adjacency_th(cl)<-rbind(c(0,100,150,0),c(0,0,0,100),c(0,0,0,150),c(0,0,0,0));
#'cl<-finalcoordinates(cl)
#'coord(cl)
#'
#'#if all possible components should be connected with each other.
#'#Within the following example, pubgth delivers energy to ts1 and demth and ts1 delivers also energy to demth
#'cl<-new.Sandbox()
#'name(cl)<-'Sandy'
#'timegrid(cl)<-rep(15,10)
#'ts1<-new.TS()
#'pubgth<-new.PubGth()
#'variables(pubgth)<-"P_th_plus"
#'demth<-new.Demth()
#'components(cl)<-list(pubgth,ts1,demth)
#'cl<-finalcoordinates(cl)
#'coord(cl)
#'@export

setMethod("adjacency_th<-", signature(object="Sandbox",value="matrix"), function(object, value) {

  errors<-character();
  if(dim(value)[1]!=dim(value)[2]){
    msg<-"adjacency_th has to be quadratic";
    errors<-c(msg,errors)

  }
  k<-length(object@components)

  if(dim(value)[2]>0){
    if((k)!=dim(value)[2]){
      msg<-"Dimension of adjacency_th does not correspond to the number of components";
      errors<-c(msg,errors)
    }
  }

  if((k)*dim(value)[1]>0){
    for(i in seq_len(k)){
      if(any(value[i,]!=0)&all(object@components[[i]]@variables!="P_th_plus")){
        msg<-paste("adjacency_th implies that component",i,"can deliver thermal energy, but there is no variable P_th_plus")
        errors<-c(msg,errors)
      }
      if(any(value[,i]!=0)&all(object@components[[i]]@variables!="P_th_minus")){
        msg<-paste("adjacency_th implies that component",i,"can consume electric energy, but there is no variable P_th_minus")
        errors<-c(msg,errors)
      }
    }

  }


  if(length(errors)==0){
    co<-compoNames(object)
    comp<-str_split_fixed(co,"_",2)[,2]
    rownames(value)<-comp
    colnames(value)<-comp
    object@adjacency_th <- value
    #update coordinates
    #name(object)<-name(object)
    validObject(object)
  }else{print(errors)}
  return(object)
})

#----adjacency_fuel----------------------------------------
#'adjacency_fuel (Set)
#'
#'Set adjacency_fuel, the adjacency matrix of the graph of the fuel Sandbox
#'
#'The matrix has to be built in the following way
#'\itemize{
#'\item The matrix has to be quadratic with the dimension k (where k is the number of components)
#'\item If a connection between two components is available, the value of th matrix is non zero, e.g. if th first component is delivering to the second component adjacency_fuel(object)[1,2]!=0
# #'\item The value describes the upper limitation of the connection
#'\item Each row contains the delivery of a component, i.e. each column contains the consume of a component.
#'\item If the connection should be unlimted the value has to be negativ, otherwise the value of the entry of matrix describes the limitation in kW of the connection.
#'\item A component can only consume if the component has the variable "P_fuel_minus"
#'\item A component can only deliver if the component has the varialbe "P_fuel_plus"
#'}
#'@importFrom Root adjacency_fuel<-
#'@examples
#'library(Sandbox)
#'library(PubGfuel)
#'library(Demfuel)
#'cl <- new.Sandbox()
#'name(cl) <- 'Sandy'
#'timegrid(cl)<-rep(15,3)
#'pubgfuel <- new.PubGfuel()
#'demfuel <- new.Demfuel()
#'#Let us connect these things
#'components(cl)<-list(pubgfuel,demfuel)
#'#Let demfuel  from pubgfuel (limited by 100)
#'adjacency_fuel(cl)<-rbind(c(0,100),c(0,0));
#'cl<-finalcoordinates(cl)
#'coord(cl)
#'
#'@export

setMethod("adjacency_fuel<-", signature(object="Sandbox",value="matrix"), function(object, value) {
  errors<-character();
  if(dim(value)[1]!=dim(value)[2]){
    msg<-"adjacency_fuel has to be quadratic";
    errors<-c(msg,errors)

  }

  k<-length(components(object))

  if(dim(value)[2]>0){
    if((k)!=dim(value)[2]){
      msg<-"Dimension of adjacency_fuel does not correspond to the number of components";
      errors<-c(msg,errors)
    }
  }


  if((k)*dim(value)[1]>0){
    for(i in seq_len(k)){
      if(any(value[i,]!=0)&all(object@components[[i]]@variables!="P_fuel_plus")){
        msg<-paste("adjacency_fuel implies that component",i,"can deliver electric energy, but there is no variable P_fuel_plus")
        errors<-c(msg,errors)
      }
      if(any(value[,i]!=0)&all(object@components[[i]]@variables!="P_fuel_minus")){
        msg<-paste("adjacency_fuel implies that component",i,"can consume electric energy, but there is no variable P_fuel_minus")
        errors<-c(msg,errors)
      }
    }

  }


  if(length(errors)==0){
    co<-compoNames(object)
    comp<-str_split_fixed(co,"_",2)[,2]
    rownames(value)<-comp
    colnames(value)<-comp
    object@adjacency_fuel <- value
    #update coordinates
    #name(object)<-name(object)
    validObject(object)
  }else{print(errors)}
  return(object)
})
#----adjacencyEff_el---------------------------------------
#'adjacencyEff_el (Set)
#'
#'Set adjacencyEff_el, the matrix describing the efficieny of delivery in
#'the electric Sandbox
#'
#'NOTE! The matrix can only be set, if the matrix adjacency_el of the object
#'exists.
#'
#'The matrix has to be built in the following way
#'\itemize{
#'\item The matrix has to be quadratic with the dimension k (where k is the number of components )
#'\item If a connection between two components is available, the value (between 0 and 1) of th matrix describes the efficiency of the delivery.
#'\item Each row contains the efficiency of the delivery of a component, i.e. each column contains the efficiency of the consume of a component.
#'}
#'@importFrom Root adjacencyEff_el<-
#'@examples
#'library(Sandbox)
#'library(Bat)
#'library(PubGel)
#'library(Demel)
#'cl<-new.Sandbox()
#'name(cl)<-'Sandy'
#'timegrid(cl)<-rep(15,10)
#'bat1<-new.Bat()
#'bat2<-new.Bat()
#'pubgel<-new.PubGel()
#'demel<-new.Demel()
#'components(cl)<-list(pubgel,bat1,bat2,demel)
#'#Let bat1 and bat2 get electrical energy from pubgel(limited by 100/150) and deliver the energy to demel (limited by 100/150)
#'adjacency_el(cl)<-rbind(c(0,100,150,0),c(0,0,0,100),c(0,0,0,150),c(0,0,0,0));
#'#add line losses
#'adjacencyEff_el(cl)<-rbind(c(0,0.8,0.9,0),c(0,0,0,0.8),c(0,0,0,0.9),c(0,0,0,0));
#'cl<-finalcoordinates(cl)
#'adjacencyEff_el(cl)
#'@export

setMethod("adjacencyEff_el<-", signature(object="Sandbox",value="matrix"), function(object, value) {

  errors=character();
  if(dim(value)[1]!=dim(value)[2]){
    msg<-"adjacencyEff_el has to be a quadratic matrix"
    errors<-c(msg,errors)

  }

  if(any(value>1)|any(value<0)){
    msg<-"values of adjacencyEff_el are not between 0 and 1"
    errors<-c(msg,errors)

  }


  if (length(errors)==0){
    co<-compoNames(object)
    comp<-str_split_fixed(co,"_",2)[,2]
    rownames(value)<-comp
    colnames(value)<-comp
    object@adjacencyEff_el <- value
    if(any(object@adjacencyEff_el[adjacency_el(object)!=0]==0)){
     print("Warning: There is at least one efficieny with value zero")
    }
    validObject(object)
  }else{print(errors)}


  return(object)
})
#----adjacencyEff_th---------------------------------------
#'adjacencyEff_th (Set)
#'
#'Set adjacencyEff_th, the matrix describing the efficieny of delivery in
#'the thermal Sandbox
#'#'
#'NOTE! The matrix can only be set, if the matrix adjacency_th of the object
#'exists.
#'
#'The matrix has to be built in the following way
#'\itemize{
#'\item The matrix has to be quadratic with the dimension k (where k is the number of components )
#'\item If a connection between two components is available, the value (between 0 and 1) of th matrix describes the efficiency of the delivery.
#'\item Each row contains the efficiency of the delivery of a component, i.e. each column contains the efficiency of the consume of a component.
#'}
#'@importFrom Root adjacencyEff_th<-
#'@examples
#'library(Sandbox)
#'library(PubGth)
#'library(Demth)
#'library(TS)
#'cl <- new.Sandbox()
#'name(cl) <- 'Sandy'
#'timegrid(cl)<-rep(15,3)
#'ts1 <- new.TS()
#'ts2 <- new.TS()
#'pubgth <- new.PubGth()
#'demth <- new.Demth()
#'#Let us connect these things
#'components(cl)<-list(pubgth,ts1,ts2,demth)
#'#Let ts1 and ts2 get thermal energy from pubgth(limited by 100/150) and deliver the energy to demth (limited by 100/150)
#'adjacency_th(cl)<-rbind(c(0,100,150,0),c(0,0,0,100),c(0,0,0,150),c(0,0,0,0));
#'#add line losses
#'adjacencyEff_th(cl)<-rbind(c(0,0.7,0.9,0),c(0,0,0,0.8),c(0,0,0,0.88),c(0,0,0,0));
#'cl<-finalcoordinates(cl)
#'@export

setMethod("adjacencyEff_th<-", signature(object="Sandbox",value="matrix"), function(object, value) {

  errors=character();
  if(dim(value)[1]!=dim(value)[2]){
    msg<-"adjacencyEff_th has to be a quadratic matrix"
    errors<-c(msg,errors)

  }

  if(any(value>1)|any(value<0)){
    msg<-"values of adjacencyEff_th are not between 0 and 1"
    errors<-c(msg,errors)

  }

  if (length(errors)==0){
    co<-compoNames(object)
    comp<-str_split_fixed(co,"_",2)[,2]
    rownames(value)<-comp
    colnames(value)<-comp
    object@adjacencyEff_th <- value;
    if(any(object@adjacencyEff_th[adjacency_th(object)!=0]==0)){
      print("Warning: There is at least one efficieny with value zero")
    }
    validObject(object)
  }else{print(errors)}


  return(object)
})
#----adjacencyEff_fuel-------------------------------------
#'adjacencyEff_fuel (Set)
#'
#'Set adjacencyEff_fuel, the matrix describing the efficieny of delivery in
#'the fuel Sandbox
#'
#'#'NOTE! The matrix can only be set, if the matrix adjacency_fuel of the
#'object exists.
#'
#'The matrix has to be built in the following way
#'\itemize{
#'\item The matrix has to be quadratic with the dimension k (where k is the number of components )
#'\item If a connection between two components is available, the value (between 0 and 1) of th matrix describes the efficiency of the delivery.
#'\item Each row contains the efficiency of the delivery of a component, i.e. each column contains the efficiency of the consume of a component.
#'}
#'
#'@examples
#'#Building a Sandbox with a PtG and a CHP, where PtG should be able to deliver and the CHP should be able to consume
#'library(Sandbox)
#'library(PubGfuel)
#'library(Demfuel)
#'cl <- new.Sandbox()
#'name(cl) <- 'Sandy'
#'timegrid(cl)<-rep(15,3)
#'pubgfuel <- new.PubGfuel()
#'demfuel <- new.Demfuel()
#'#Let us connect these things
#'components(cl)<-list(pubgfuel,demfuel)
#'#Let demfuel  from pubgfuel (limited by 100)
#'adjacency_fuel(cl)<-rbind(c(0,100),c(0,0));
#'#add line losses
#'adjacency_fuel(cl)<-rbind(c(0,0.9),c(0,0));
#'cl<-finalcoordinates(cl)
#'
#'@export
#'@importFrom Root adjacencyEff_fuel<-




setMethod("adjacencyEff_fuel<-", signature(object="Sandbox",value="matrix"), function(object, value) {

  errors<-character();
  if(dim(value)[1]!=dim(value)[2]){
    msg<-"adjacencyEff_fuel has to be a quadratic matrix"
    errors<-c(msg,errors)
  }

  if(any(value>1)|any(value<0)){
    msg<-"values of adjacencyEff_fuel are not between 0 and 1"
    errors<-c(msg,errors)
  }

  if(length(errors)==0){
    co<-compoNames(object)
    comp<-str_split_fixed(co,"_",2)[,2]
    rownames(value)<-comp
    colnames(value)<-comp
    object@adjacencyEff_fuel <- value;
    if(any(object@adjacencyEff_fuel[adjacency_fuel(object)!=0]==0)){
      print("Warning: There is at least one efficieny with value zero")
    }
    validObject(object)
  }else(print(errors))

  return(object)
})





#----adjacencyPrice_el---------------------------------------
#'adjacencyPrice_el (Set)
#'
#'Set adjacencyPrice_el, the matrix containing the electric prices in € of the single connection between the components of the Sandbox
#'
#'NOTE! The matrix can only be set, if the matrix adjacency_el of the object
#'exists.
#'
#'The matrix has to be built in the following way
#'\itemize{
#'\item The matrix has to be quadratic with the dimension k (where k is the number of components )
#'\item If a connection between two components is available, the value of th matrix describes the price of the delivery.
#'\item Each row contains the price of the delivery of a component to another component, i.e. each column contains the price of the consume of a component.
#'}
#'
#'@importFrom Root adjacencyPrice_el<-
#'@examples
#'library(Sandbox)
#'library(Bat)
#'library(PubGel)
#'library(Demel)
#'cl<-new.Sandbox()
#'name(cl)<-'Sandy'
#'timegrid(cl)<-rep(15,10)
#'bat1<-new.Bat()
#'bat2<-new.Bat()
#'pubgel<-new.PubGel()
#'variables(pubgel) <- "P_el_plus"
#'demel<-new.Demel()
#'components(cl)<-list(pubgel,bat1,bat2,demel)
#'#Let bat1 and bat2 get electrical energy from pubgel(limited by 100/150) and deliver the energy to demel (limited by 100/150)
#'adjacency_el(cl)<-rbind(c(0,100,150,0),c(0,0,0,100),c(0,0,0,150),c(0,0,0,0));
#'#add prices
#'adjacencyPrice_el(cl)<-rbind(c(0,0.08,0.08,0),c(0,0,0,0.18),c(0,0,0,0.04),c(0,0,0,0));
#'cl<-finalcoordinates(cl)
#'adjacencyPrice_el(cl)
#'@export

setMethod("adjacencyPrice_el<-", signature(object="Sandbox",value="matrix"), function(object, value) {

  errors=character();
  if(dim(value)[1]!=dim(value)[2]){
    msg<-"adjacencyPrice_el has to be a quadratic matrix"
    errors<-c(msg,errors)

  }

  te <- length(object@timegrid);
  value <- array(value,dim = c(dim(value),te));
  nam <- compoNames(object);
  nam <- str_split_fixed(nam,"_",Inf)[,2]
  dimnames(value) <- list(nam,nam,paste0("time",1:te));




  if (length(errors)==0){

    object@adjacencyPrice_el <- value
    validObject(object)
  }else{print(errors)}


  return(object)
})


#----adjacencyPrice_el---------------------------------------
#'adjacencyPrice_el (Set)
#'
#'Set adjacencyPrice_el, the matrix containing the electric prices in € of the single connection between the components of the Sandbox
#'
#'NOTE! The matrix can only be set, if the matrix adjacency_el of the object
#'exists.
#'
#'The matrix has to be built in the following way
#'\itemize{
#'\item The matrix has to be quadratic with the dimension k (where k is the number of components )
#'\item If a connection between two components is available, the value of th matrix describes the price of the delivery.
#'\item Each row contains the price of the delivery of a component to another component, i.e. each column contains the price of the consume of a component.
#'}
#'@importFrom Root adjacencyPrice_el<-
#'@examples
#'library(Sandbox)
#'library(Bat)
#'library(PubGel)
#'library(Demel)
#'cl<-new.Sandbox()
#'name(cl)<-'Sandy'
#'timegrid(cl)<-rep(15,10)
#'bat1<-new.Bat()
#'bat2<-new.Bat()
#'pubgel<-new.PubGel()
#'demel<-new.Demel()
#'components(cl)<-list(pubgel,bat1,bat2,demel)
#'#Let bat1 and bat2 get electrical energy from pubgel(limited by 100/150) and deliver the energy to demel (limited by 100/150)
#'adjacency_el(cl)<-rbind(c(0,100,150,0),c(0,0,0,100),c(0,0,0,150),c(0,0,0,0));
#'#add prices
#'adjacencyPrice_el(cl)<-rbind(c(0,0.08,0.08,0),c(0,0,0,0.18),c(0,0,0,0.04),c(0,0,0,0));
#'cl<-finalcoordinates(cl)
#'adjacencyEff_el(cl)
#'@export

setMethod("adjacencyPrice_el<-", signature(object="Sandbox",value="array"), function(object, value) {

  errors=character();
  if(dim(value)[1]!=dim(value)[2]){
    msg<-"adjacencyPrice_el has to be a quadratic array"
    errors<-c(msg,errors)

  }


  if (length(errors)==0){
    te<-length(object@timegrid)
    nam <- compoNames(object);
    nam <- str_split_fixed(nam,"_",Inf)[,2]
    dimnames(value) <- list(nam,nam,paste0("time",1:te));
    object@adjacencyPrice_el <- value

    validObject(object)
  }else{
    print(errors)
    }


  return(object)
})
#----adjacencyPrice_th---------------------------------------
#'adjacencyPrice_th (Set)
#'
#'Set adjacencyPrice_th, the matrix containing the thermal prices in € of the single connection between the components of the Sandbox
#'
#'NOTE! The matrix can only be set, if the matrix adjacency_th of the object
#'exists.
#'
#'The matrix has to be built in the following way
#'\itemize{
#'\item The matrix has to be quadratic with the dimension k (where k is the number of components )
#'\item If a connection between two components is available, the value of th matrix describes the price of the delivery.
#'\item Each row contains the price of the delivery of a component to another component, i.e. each column contains the price of the consume of a component.
#'}
#'@importFrom Root adjacencyPrice_th<-
#'@examples
#'library(Sandbox)
#'library(PubGth)
#'library(Demth)
#'library(TS)
#'cl <- new.Sandbox()
#'name(cl) <- 'Sandy'
#'timegrid(cl)<-rep(15,3)
#'ts1 <- new.TS()
#'ts2 <- new.TS()
#'pubgth <- new.PubGth()
#'demth <- new.Demth()
#'#Let us connect these things
#'components(cl)<-list(pubgth,ts1,ts2,demth)
#'#Let ts1 and ts2 get thermal energy from pubgth(limited by 100/150) and deliver the energy to demth (limited by 100/150)
#'adjacency_th(cl)<-rbind(c(0,100,150,0),c(0,0,0,100),c(0,0,0,150),c(0,0,0,0));
#'#add line losses
#'adjacencyPrice_th(cl)<-rbind(c(0,0.079,0.079,0),c(0,0,0,0.18),c(0,0,0,0.04),c(0,0,0,0));
#'cl<-finalcoordinates(cl)
#'@export

setMethod("adjacencyPrice_th<-", signature(object="Sandbox",value="matrix"), function(object, value) {

  errors=character();
  if(dim(value)[1]!=dim(value)[2]){
    msg<-"adjacencyPrice_th has to be a quadratic matrix"
    errors<-c(msg,errors)    
  }
  
  te <- length(object@timegrid);
  value <- array(value,dim = c(dim(value),te));
  nam <- compoNames(object);
  nam <- str_split_fixed(nam,"_",Inf)[,2]
  dimnames(value) <- list(nam,nam,paste0("time",1:te));
  
  
  
  
  if (length(errors)==0){
    
    object@adjacencyPrice_th <- value

    validObject(object)
  }else{print(errors)}


  return(object)
})
#----adjacencyPrice_th---------------------------------------
#'adjacencyPrice_th (Set)
#'
#'Set adjacencyPrice_th, the matrix containing the thermal prices in € of the single connection between the components of the Sandbox
#'
#'NOTE! The matrix can only be set, if the matrix adjacency_th of the object
#'exists.
#'
#'The matrix has to be built in the following way
#'\itemize{
#'\item The matrix has to be quadratic with the dimension k (where k is the number of components )
#'\item If a connection between two components is available, the value of th matrix describes the price of the delivery.
#'\item Each row contains the price of the delivery of a component to another component, i.e. each column contains the price of the consume of a component.
#'}
#'@importFrom Root adjacencyPrice_th<-
#'@examples
#'library(Sandbox)
#'library(TS)
#'library(PubGth)
#'library(Demth)
#'cl<-new.Sandbox()
#'name(cl)<-'Sandy'
#'timegrid(cl)<-rep(15,10)
#'TS1<-new.TS()
#'TS2<-new.TS()
#'pubgth<-new.PubGth()
#'demth<-new.Demth()
#'components(cl)<-list(pubgth,TS1,TS2,demth)
#'#Let TS1 and TS2 get thermal energy from pubgth(limited by 100/150) and deliver the energy to demth (limited by 100/150)
#'adjacency_th(cl)<-rbind(c(0,100,150,0),c(0,0,0,100),c(0,0,0,150),c(0,0,0,0));
#'#add prices
#'adjacencyPrice_th(cl)<-rbind(c(0,0.08,0.08,0),c(0,0,0,0.18),c(0,0,0,0.04),c(0,0,0,0));
#'cl<-finalcoordinates(cl)
#'adjacencyEff_th(cl)
#'@export

setMethod("adjacencyPrice_th<-", signature(object="Sandbox",value="array"), function(object, value) {
  
  errors=character();
  if(dim(value)[1]!=dim(value)[2]){
    msg<-"adjacencyPrice_th has to be a quadratic array"
    errors<-c(msg,errors)
    
  }
  
  
  if (length(errors)==0){
    te<-length(object@timegrid)
    nam <- compoNames(object);
    nam <- str_split_fixed(nam,"_",Inf)[,2]
    dimnames(value) <- list(nam,nam,paste0("time",1:te));
    object@adjacencyPrice_th <- value
    
    validObject(object)
  }else{
    print(errors)
  }
  
  
  return(object)
})
#----adjacencyPrice_fuel-------------------------------------
#'adjacencyPrice_fuel (Set)
#'
#'Set adjacencyPrice_fuel, the matrix containing the fuel prices in € of the single connection between the components of the Sandbox
#'
#'NOTE! The matrix can only be set, if the matrix adjacency_fuel of the object
#'exists.
#'
#'The matrix has to be built in the following way
#'\itemize{
#'\item The matrix has to be quadratic with the dimension k (where k is the number of components )
#'\item If a connection between two components is available, the value of th matrix describes the price of the delivery.
#'\item Each row contains the price of the delivery of a component to another component, i.e. each column contains the price of the consume of a component.
#'}
#'@importFrom Root adjacencyPrice_fuel<-
#'@examples
#'library(Sandbox)
#'library(PubGfuel)
#'library(Demfuel)
#'cl <- new.Sandbox()
#'name(cl) <- 'Sandy'
#'timegrid(cl)<-rep(15,3)
#'pubgfuel <- new.PubGfuel()
#'demfuel <- new.Demfuel()
#'#Let us connect these things
#'components(cl)<-list(pubgfuel,demfuel)
#'#Let demfuel  from pubgfuel (limited by 100)
#'adjacency_fuel(cl)<-rbind(c(0,100),c(0,0));
#'#add price
#'adjacencyPrice_fuel(cl)<-rbind(c(0,0.09),c(0,0));
#'cl<-finalcoordinates(cl)
#'@export

setMethod("adjacencyPrice_fuel<-", signature(object="Sandbox",value="matrix"), function(object, value) {

  errors=character();
  if(dim(value)[1]!=dim(value)[2]){
    msg<-"adjacencyPrice_fuel has to be a quadratic matrix"
    errors<-c(msg,errors)
    
  }
  
  te <- length(object@timegrid);
  value <- array(value,dim = c(dim(value),te));
  nam <- compoNames(object);
  nam <- str_split_fixed(nam,"_",Inf)[,2]
  dimnames(value) <- list(nam,nam,paste0("time",1:te));
  
  
  
  
  if (length(errors)==0){
    
    object@adjacencyPrice_fuel <- value
    validObject(object)
  }else{print(errors)}

  return(object)
})
#----adjacencyPrice_fuel---------------------------------------
#'adjacencyPrice_fuel (Set)
#'
#'Set adjacencyPrice_fuel, the matrix containing the fuel prices in € of the single connection between the components of the Sandbox
#'
#'NOTE! The matrix can only be set, if the matrix adjacency_fuel of the object
#'exists.
#'
#'The matrix has to be built in the following way
#'\itemize{
#'\item The matrix has to be quadratic with the dimension k (where k is the number of components )
#'\item If a connection between two components is available, the value of th matrix describes the price of the delivery.
#'\item Each row contains the price of the delivery of a component to another component, i.e. each column contains the price of the consume of a component.
#'}
#'@importFrom Root adjacencyPrice_fuel<-
#'@examples
#'library(Sandbox)
#'library(PubGfuel)
#'library(Demfuel)
#'cl <- new.Sandbox()
#'name(cl) <- 'Sandy'
#'timegrid(cl)<-rep(15,3)
#'pubgfuel <- new.PubGfuel()
#'demfuel <- new.Demfuel()
#'#Let us connect these things
#'components(cl)<-list(pubgfuel,demfuel)
#'#Let demfuel get fuel from pubgfuel(limited by 100/150) 
#'adjacency_fuel(cl)<-rbind(c(100,0),c(0,0));
#'#add prices
#'adjacencyPrice_fuel(cl)<-rbind(c(0,0.08),c(0,0));
#'cl<-finalcoordinates(cl)
#'adjacencyEff_fuel(cl)
#'@export

setMethod("adjacencyPrice_fuel<-", signature(object="Sandbox",value="array"), function(object, value) {
  
  errors=character();
  if(dim(value)[1]!=dim(value)[2]){
    msg<-"adjacencyPrice_fuel has to be a quadratic array"
    errors<-c(msg,errors)
    
  }
  
  
  if (length(errors)==0){
    te<-length(object@timegrid)
    nam <- compoNames(object);
    nam <- str_split_fixed(nam,"_",Inf)[,2]
    dimnames(value) <- list(nam,nam,paste0("time",1:te));
    object@adjacencyPrice_fuel <- value
    
    validObject(object)
  }else{
    print(errors)
  }
  
  

  return(object)
})


#----adjacencyWatch_el---------------------------------------
#'adjacencyWatch_el (Set)
#'
#'Set adjacencyWatch_el, the matrix describing the efficieny of delivery in
#'the electric Sandbox
#'
#'NOTE! The matrix can only be set, if the matrix adjacency_el of the object
#'exists.
#'@importFrom Root adjacencyWatch_el<-
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
#'cl<-finalcoordinates(cl)
#'coord(cl)
setMethod("adjacencyWatch_el<-", signature(object="Sandbox",value="matrix"), function(object, value) {

  errors=character();
  if(dim(value)[1]!=dim(value)[2]){
    msg<-"adjacencyWatch_el has to be a quadratic matrix"
    errors<-c(msg,errors)

  }

  k<-length(components(object))
  if((k)*dim(value)[1]>0){
    for(i in seq_len(k)){
      if(any(value[,i]!=0)& is(components(object)[[i]])[1] !="Wado"){
        msg<-paste("component",i,"has to be a wado")
        errors<-c(msg,errors)
      }
    }
  }

  if(any(value>1)|any(value<0)){
    msg<-"values of adjacencyWatch_el are not between 0 and 1"
    errors<-c(msg,errors)

  }



  if (length(errors)==0){
    object@adjacencyWatch_el <- value
    validObject(object)
  }else{print(errors)}


  return(object)
})

#----adjacencyWatch_el_var---------------------------------------
#'adjacencyWatch_el_var (Set)
#'
#'Set adjacencyWatch_el_var, the matrix  describing  which variable, e.g. P_el_plus, of which component(s) is/are spyed by which wado.
#'Following variables are allowed:
#'\itemize{
#'\item P_el_plus
#'\item P_el_minus
#'\item E_el}
#'
#'NOTE! The matrix can only be set, if the matrix adjacencyWatch_el of the object
#'exists.
#'@importFrom Root adjacencyWatch_el_var<-
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
#'coord(cl)
setMethod("adjacencyWatch_el_var<-", signature(object="Sandbox",value="matrix"), function(object, value) {

  errors=character();
  allowed <- c(
    "P_el_plus",
    "P_el_minus",
    "E_el"
  )


  if (all(value[value!=0]%in% allowed) == FALSE) {
    msg <- "not allowed variables. the allowed variables are: P_el_plus, P_el_minus, E_el"
    errors <- c(errors, msg)
  }


  if(dim(value)[1]!=dim(value)[2]){
    msg<-"adjacencyWatch_el_var has to be a quadratic matrix"
    errors<-c(msg,errors)

  }

  k<-length(components(object))
  if((k)*dim(value)[1]>0){
    for(i in seq_len(k)){
      if(any(value[,i]!=0)& is(components(object)[[i]])[1] !="Wado"){
        msg<-paste("component",i,"has to be a wado")
        errors<-c(msg,errors)
      }
    }
  }

  # if(!is(value[value!=0],"character")){
  #   msg<-"values of adjacencyWatch_el_var are not characters"
  #   errors<-c(msg,errors)
  #
  # }



  if (length(errors)==0){
    object@adjacencyWatch_el_var <- value
    validObject(object)
  }else{print(errors)}


  return(object)
})

#----adjacencyWatch_fuel---------------------------------------
#'adjacencyWatch_fuel (Set)
#'
#'Set adjacencyWatch_fuel, the matrix describing the efficieny of delivery in
#'the electric Sandbox
#'
#'NOTE! The matrix can only be set, if the matrix adjacency_fuel of the object
#'exists.
#'@importFrom Root adjacencyWatch_fuel<-
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
#'maxP_fuel_plus(fuel)<-300
#'minP_fuel_plus(fuel)<-0
#'maxP_fuel_minus(fuel)<-300
#'minP_fuel_minus(fuel)<-0
#'demth<-new.Demth()
#'demel<-new.Demel()
#'wado <- new.Wado()
#'variables(wado)<-c("P_fuel_plus","P_fuel_minus")
#'#Let us connect these things
#'components(cl)<-list(chp,bat1,bat2,fuel,demth,demel,wado)
#'#let fuel deliver fuel  via wado (limited by 200 kW) to chp
#'adjacency_el(cl)<-rbind(c(0,100,100,0,0,100,0),c(0,0,0,0,0,200,0),c(0,0,0,0,0,150,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0));
#'adjacency_th(cl)<-rbind(c(0,0,0,0,100,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0));
#'adjacency_fuel(cl)<-rbind(c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,100),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(200,0,0,0,0,0,0));
#' #Let spy wado fuel with variable "P_fuel_plus"
#'adjacencyWatch_fuel(cl)<- rbind(c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0.9),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0));
#'cl<-finalcoordinates(cl)
#'@export

setMethod("adjacencyWatch_fuel<-", signature(object="Sandbox",value="matrix"), function(object, value) {

  errors=character();


  if(dim(value)[1]!=dim(value)[2]){
    msg<-"adjacencyWatch_fuel has to be a quadratic matrix"
    errors<-c(msg,errors)

  }

  k<-length(components(object))
  if((k)*dim(value)[1]>0){
    for(i in seq_len(k)){
      if(any(value[,i]!=0)& is(components(object)[[i]])[1] !="Wado"){
        msg<-paste("component",i,"has to be a wado")
        errors<-c(msg,errors)
      }
    }
  }

  if(any(value>1)|any(value<0)){
    msg<-"values of adjacencyWatch_fuel are not between 0 and 1"
    errors<-c(msg,errors)

  }

  if (length(errors)==0){
    object@adjacencyWatch_fuel <- value
    validObject(object)
  }else{print(errors)}


  return(object)
})
#----adjacencyWatch_fuel_var---------------------------------------
#'adjacencyWatch_fuel_var (Set)
#'
#'Set adjacencyWatch_fuel_var, the matrix  describing  which variable, e.g. P_fuel_plus, of which component(s) is/are spyed by which wado.
#'Following variables are allowed:
#'\itemize{
#'\item P_fuel_plus
#'\item P_fuel_minus
#'}
#'NOTE! The matrix can only be set, if the matrix adjacencyWatch_fuel of the object
#'exists.
#'@importFrom Root adjacencyWatch_fuel_var<-
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
#'maxP_fuel_plus(fuel)<-300
#'minP_fuel_plus(fuel)<-0
#'maxP_fuel_minus(fuel)<-300
#'minP_fuel_minus(fuel)<-0
#'demth<-new.Demth()
#'demel<-new.Demel()
#'wado <- new.Wado()
#'variables(wado)<-c("P_fuel_plus","P_fuel_minus")
#'#Let us connect these things
#'components(cl)<-list(chp,bat1,bat2,fuel,demth,demel,wado)
#'#let fuel deliver fuel  via wado (limited by 200 kW) to chp
#'adjacency_el(cl)<-rbind(c(0,100,100,0,0,100,0),c(0,0,0,0,0,200,0),c(0,0,0,0,0,150,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0));
#'adjacency_th(cl)<-rbind(c(0,0,0,0,100,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0));
#'adjacency_fuel(cl)<-rbind(c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,100),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(200,0,0,0,0,0,0));
#' #Let spy wado fuel with variable "P_fuel_plus"
#'adjacencyWatch_fuel(cl)<- rbind(c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0.9),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0));
#'adjacencyWatch_fuel_var(cl)<- rbind(c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,"P_fuel_plus"),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0));
#'cl<-finalcoordinates(cl)#'library(Sandbox)
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
#'maxP_fuel_plus(fuel)<-300
#'minP_fuel_plus(fuel)<-0
#'maxP_fuel_minus(fuel)<-300
#'minP_fuel_minus(fuel)<-0
#'demth<-new.Demth()
#'demel<-new.Demel()
#'wado <- new.Wado()
#'variables(wado)<-c("P_fuel_plus","P_fuel_minus")
#'#Let us connect these things
#'components(cl)<-list(chp,bat1,bat2,fuel,demth,demel,wado)
#'#let fuel deliver fuel  via wado (limited by 200 kW) to chp
#'adjacency_el(cl)<-rbind(c(0,100,100,0,0,100,0),c(0,0,0,0,0,200,0),c(0,0,0,0,0,150,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0));
#'adjacency_th(cl)<-rbind(c(0,0,0,0,100,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0));
#'adjacency_fuel(cl)<-rbind(c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,100),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(200,0,0,0,0,0,0));
#' #Let spy wado fuel with variable "P_fuel_plus"
#'adjacencyWatch_fuel(cl)<- rbind(c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0.9),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0));
#'adjacencyWatch_fuel_var(cl)<- rbind(c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,"P_fuel_plus"),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0));
#'cl<-finalcoordinates(cl)
#'@export

setMethod("adjacencyWatch_fuel_var<-", signature(object="Sandbox",value="matrix"), function(object, value) {

  errors=character();

  allowed <- c(
    "P_fuel_plus",
    "P_fuel_minus"#,
    #"E_fuel"
  )


  if (all(value[value!=0]%in% allowed) == FALSE) {
    msg <- "not allowed variables. the allowed variables are: P_fuel_plus, P_fuel_minus" #, E_fuel
    errors <- c(errors, msg)
  }
  if(dim(value)[1]!=dim(value)[2]){
    msg<-"adjacencyWatch_fuel_var has to be a quadratic matrix"
    errors<-c(msg,errors)

  }

  k<-length(components(object))
  if((k)*dim(value)[1]>0){
    for(i in seq_len(k)){
      if(any(value[,i]!=0)& is(components(object)[[i]])[1] !="Wado"){
        msg<-paste("component",i,"has to be a wado")
        errors<-c(msg,errors)
      }
    }
  }

  # if(!is(value[value!=0],"character")){
  #   msg<-"values of adjacencyWatch_fuel_var are not characters"
  #   errors<-c(msg,errors)
  #
  # }



  if (length(errors)==0){
    object@adjacencyWatch_fuel_var <- value
    validObject(object)
  }else{print(errors)}


  return(object)
})

#----adjacencyWatch_th---------------------------------------
#'adjacencyWatch_th (Set)
#'
#'Set adjacencyWatch_th, the matrix describing which components are spyed by which watchdog and which components are controlled by which watchdog.
#'If a watchdog is spying a component the value of the matrix has to be one (construction analog to adajency_th)
#'
#'NOTE! The matrix can only be set, if the matrix adjacency_th of the object
#'exists.
#'@importFrom Root adjacencyWatch_th<-
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
#'maxP_th_plus(fuel)<-300
#'minP_th_plus(fuel)<-0
#'maxP_th_minus(fuel)<-300
#'minP_th_minus(fuel)<-0
#'bat1<-new.Bat()
#'maxE_el(bat1)<-100
#'maxP_el_plus(bat1)<-15
#'bat2 <- new.Bat()
#'maxE_el(bat2)<-100
#'maxP_el_plus(bat2)<-15
#'fuel<-new.PubGfuel()
#'maxP_fuel_plus(fuel)<-300
#'minP_fuel_plus(fuel)<-0
#'maxP_fuel_minus(fuel)<-300
#'minP_fuel_minus(fuel)<-0
#'demth<-new.Demth()
#'demel<-new.Demel()
#'wado <- new.Wado()
#'variables(wado)<-c("P_th_plus","P_th_minus")
#'#Let us connect these things
#'components(cl)<-list(chp,bat1,bat2,fuel,demth,demel,wado)
#'#let chp deliver thermal energy via wado (limited by 200 kW) to demth
#'adjacency_el(cl)<-rbind(c(0,100,100,0,0,100,0),c(0,0,0,0,0,200,0),c(0,0,0,0,0,150,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0));
#'adjacency_th(cl)<-rbind(c(0,0,0,0,0,0,200),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,200,0,0));
#'adjacency_fuel(cl)<-rbind(c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(100,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0));
#' #Let spy wado th with variable "P_th_plus"
#'adjacencyWatch_th(cl)<-rbind(c(0,0,0,0,0,0,0.9),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0));
#'cl<-finalcoordinates(cl)
#'coord(cl)
#'@export

setMethod("adjacencyWatch_th<-", signature(object="Sandbox",value="matrix"), function(object, value) {

  errors=character();
  if(dim(value)[1]!=dim(value)[2]){
    msg<-"adjacencyWatch_th has to be a quadratic matrix"
    errors<-c(msg,errors)

  }
  k<-length(components(object))
  if((k)*dim(value)[1]>0){
    for(i in seq_len(k)){
      if(any(value[,i]!=0)& is(components(object)[[i]])[1] !="Wado"){
        msg<-paste("component",i,"has to be a wado")
        errors<-c(msg,errors)
      }
    }
  }
  if(any(value>1)|any(value<0)){
    msg<-"values of adjacencyWatch_th are not between 0 and 1"
    errors<-c(msg,errors)

  }

  if (length(errors)==0){
    object@adjacencyWatch_th <- value
    validObject(object)
  }else{print(errors)}


  return(object)
})


#----adjacencyWatch_th_var---------------------------------------
#'adjacencyWatch_th_var (Set)
#'
#'Set adjacencyWatch_th_var, the matrix  describing  which variable, e.g. P_th_minus, of which component(s) is/are spyed by which wado.
#'
#'#'Following variables are allowed:
#'\itemize{
#'\item P_th_plus
#'\item P_th_minus
#'\item E_th}
#'NOTE! The matrix can only be set, if the matrix adjacencyWatch_th of the object
#'exists.
#'@importFrom Root adjacencyWatch_th_var<-
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
#'maxP_th_plus(fuel)<-300
#'minP_th_plus(fuel)<-0
#'maxP_th_minus(fuel)<-300
#'minP_th_minus(fuel)<-0
#'bat1<-new.Bat()
#'maxE_el(bat1)<-100
#'maxP_el_plus(bat1)<-15
#'bat2 <- new.Bat()
#'maxE_el(bat2)<-100
#'maxP_el_plus(bat2)<-15
#'fuel<-new.PubGfuel()
#'maxP_fuel_plus(fuel)<-300
#'minP_fuel_plus(fuel)<-0
#'maxP_fuel_minus(fuel)<-300
#'minP_fuel_minus(fuel)<-0
#'demth<-new.Demth()
#'demel<-new.Demel()
#'wado <- new.Wado()
#'variables(wado)<-c("P_th_plus","P_th_minus")
#'#Let us connect these things
#'components(cl)<-list(chp,bat1,bat2,fuel,demth,demel,wado)
#'#let chp deliver thermal energy via wado (limited by 200 kW) to demth
#'adjacency_el(cl)<-rbind(c(0,100,100,0,0,100,0),c(0,0,0,0,0,200,0),c(0,0,0,0,0,150,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0));
#'adjacency_th(cl)<-rbind(c(0,0,0,0,0,0,200),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,200,0,0));
#'adjacency_fuel(cl)<-rbind(c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(100,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0));
#' #Let spy wado th with variable "P_th_plus"
#'adjacencyWatch_th(cl)<-rbind(c(0,0,0,0,0,0,0.9),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0));
#'adjacencyWatch_th_var(cl)<- rbind(c(0,0,0,0,0,0,"P_th_plus"),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0));
#'cl<-finalcoordinates(cl)
#'coord(cl)
#'@export

setMethod("adjacencyWatch_th_var<-", signature(object="Sandbox",value="matrix"), function(object, value) {

  errors=character();

  allowed <- c(
    "P_th_plus",
    "P_th_minus",
    "E_th"
  )


  if (all(value[value!=0]%in% allowed) == FALSE) {
    msg <- "not allowed variables. the allowed variables are: P_th_plus, P_th_minus, E_th"
    errors <- c(errors, msg)
  }
  if(dim(value)[1]!=dim(value)[2]){
    msg<-"adjacencyWatch_th_var has to be a quadratic matrix"
    errors<-c(msg,errors)

  }

  k<-length(components(object))
  if((k)*dim(value)[1]>0){
    for(i in seq_len(k)){
      if(any(value[,i]!=0)& is(components(object)[[i]])[1] !="Wado"){
        msg<-paste("component",i,"has to be a wado")
        errors<-c(msg,errors)
      }
    }
  }

  # if(!is(value[value!=0],"character")){
  #   msg<-"values of adjacencyWatch_th_var are not characters"
  #   errors<-c(msg,errors)
  #
  # }



  if (length(errors)==0){
    object@adjacencyWatch_th_var <- value
    validObject(object)
  }else{print(errors)}


  return(object)
})

#----components (set)--------------------------------------
#'components
#'
#'Define the components of a Sandbox
#'
#'@importFrom Root components<-
#'@export
#'@param object  Sandbox
#'@param value A list of Sandboxes or a Root.
#'             If \code{value} is a list, the list of Roots is stored in the slot components.
#'             If \code{value} is a Root, the Root is appended to the list components.
#'@return Sandbox
#'@examples
#'x<-new.Sandbox()
#'name(x)<-"Sandy"
#'timegrid(x)<-rep(15,2)
#'#append a new CHP to the list
#'components(x)<-list(new.CHP(),new.PubGfuel(),new.PubGel(),new.PubGth())
#'compoNames(x)
#'#add new component
#'components(x)<-list(new.CHP())
#'compoNames(x)
#'
#'@details
#'Note: If variables, name and timegrid of a Sandbox are set, the coordinates
#'of the Root are built in the form:
#'\itemize{
#'\item  P_xy_plus (variable of Comp1) and P_xy_minus (variable of Comp2): var1_Comp1_Comp2, var2_Comp1_Comp2,, ...,
#' \item E_xy, Op: var1_Compxy, var2_Compxy,.....
#' }
#'If name of an Sandbox is removed, also the coordinates are removed.

setMethod("components<-", signature(object = "Sandbox"), function(object, value) {

  if (!is.list(value) & !is(value, "Root")) {
    stop("value has to be list or Root")
  }
  if(is(value,"Sandbox")){
    stop("A Sandbox as component is not allowed!!!")
  }
  if(length(components(object))>0){
    warning(paste(name(object), "has already components. The new components are additionally added!!!"))
  }

  v<-length(value)
  for (m in 1: v){
  
    #value can be an Root or an list containg one Root, else value is a list containing Root's
    if(v==1){
      if(is(value,"list")){valuetmp<-value[[1]]
      }else{valuetmp<-value}
    }else{valuetmp<- value[[m]]}

    #each component itself has to be an Root
    if(is(valuetmp, "Root")  & length(valuetmp)>0 ){
      #each component has to a Sandbox or has to have variables
     
      if(length(valuetmp@variables)>0 | is(valuetmp,"Sandbox") ){

        n<-length(object@components)
        object@components[[n + 1]] <- valuetmp
        timegrid(object@components[[n+1]])<-object@timegrid;

      }else{stop(paste0("ERROR!", valuetmp, "is not a Sandbox and variables of the component", valuetmp,  " is missing "))}
    }else{stop(paste0("ERROR!",valuetmp, "is empty or not an Root"))}

  }
  #update the names of the components

  name(object)<-name(object)
  validObject(object)




  return(object)
})

#----name------------------------------
# name (set)
#'
#'Set name of Sandbox
#'@importFrom Root name<-
#'@export
#'@param object A Sandbox
#'@param value A character. Note the name of an Sandbox has to be unique.
#'@return object The new object of Sandbox with set name
#'@examples
#'cl<-new.Sandbox()
#'name(cl)<-"Sandy"
#'@details
#'Note: If variables, name and timegrid of a Sandbox are set, the coordinates of the EP are built in the form name_var1_t1,name_var1_t2,..,name_var2_t1,name_var2_t2....
#'If name of an Sandbox is removed, also the coordinates are removed.

setMethod("name<-", signature(object = "Sandbox",value="character"), function(object,value) {

  object@name <- value

  #renaming the name of the components of the Sandbox
  k<-length(object@components)
  if(k>0){
    for (i in 1:k){

      #building the name of the components recursive
      compClass<-compoClass(object)[1:i];
      j<-length(compClass[compClass==compClass[i]]);

      name(object@components[[i]])<-paste0(object@name,"_",is(object@components[[i]])[1],j);

    }}



  # #generate coordinates if timegrid is defined
  # if(length(value)>0){
  #   if(length(object@timegrid)>0){
  #     object@coord <-coordi(object)}
  #
  #
  # } else{object@coord<- character()}


  if (validObject(object) == TRUE) {
    return(object)
  }

})

#----timegrid----------------------------------------------
#'timegrid (Set)
#'
#'Set the timegrid for a Sandbox. All components get the same timegrid as
#'Sandbox.
#'@importFrom Root timegrid<-
#'@export
#'

setMethod("timegrid<-",signature(object="Sandbox",value="numeric"),function(object,value){

  object@timegrid<-value

  k<-length(object@components);
  #change the timegrid of the components
  if(k>0){
    for(i in 1:k){
      timegrid(object@components[[i]])<-value;
    }
  }

  # if(length(value)>0){
  #   if(length(object@name)>0)
  #   {object@coord <- coordi(object)
  #
  #   }
  # } else{object@coord<- character()}

  if (validObject(object) == TRUE) {
    return(object)
  }
})


#'setPrice_el
#'
#'set the price series. This meant to give good behaviour with length(timegrid),
#'as it cuts off or recycles the other series in adjaPrice_el
#'@param sandbox A Sandbox
#'@param price A numeric. A price vector
#'@param from A character.
#'@param to A character.
#'@export

setPrice_el<-function(sandbox,price,from,to){
  stopifnot(is.numeric(price))
  stopifnot(any(is(sandbox)=="Sandbox"))
  stopifnot(length(price)==length(timegrid(sandbox)))
  stopifnot(is.character(c(from,to)))
  nam<-str_split_fixed(compoNames(sandbox),"_",Inf)[,2]
  stopifnot(all(is.element(c(from,to),nam)))
  k <- length(components(sandbox));
  te <- length(timegrid(sandbox))
  x <- adjacencyPrice_el(sandbox);
  tee <- min(c(dim(x)[3],te));
  x <- x[,,1:tee]
  y <- array(array(x),dim = c(k,k,te),dimnames = list(nam,nam,paste0("time",1:te)));
  y[from,to,]<-price;
  adjacencyPrice_el(sandbox)<-y;
  return(sandbox)

}
