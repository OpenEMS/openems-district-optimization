#----Overview----------------------------------------------
# tau
# Op (value = missing)
# Op (value = numeric)
# Op (value = character)
# P_el_plus (value = missing)
# P_el_plus (value = numeric)
# P_el_plus (value = character)
# P_el_minus (value = missing)
# P_el_minus (value = numeric)
# P_el_minus (value = character)
# P_th_plus (value = missing)
# P_th_plus (value = numeric)
# P_th_plus (value = character)
# p_th_minus (value = missing)
# p_th_minus (value = numeric)
# p_th_minus (value = character)
# P_fuel_plus (value = missing)
# P_fuel_plus (value = numeric)
# P_fuel_plus (value = character)
# P_fuel_minus (value = missing)
# P_fuel_minus (value = numeric)
# P_fuel_minus (value = character)
# E_el (value = missing)
# E_el (value = numeric)
# E_el (value = character)
# E_th (value = missing)
# E_th (value = numeric)
# E_th (value = character)
# E_fuel (value = missing)
# E_th (value = numeric)
# E_th (value = character)
#----------------------------------------------------------
#----tau---------------------------------------------------
#' tau
#'
#'tau gives timegrid as hour blocks
#'
#'@export
#'@param object numeric. A time timegrid: Blocks are measured in minutes
#'@return numeric. The timegrid in hour blocks

setMethod("tau", signature(object = "Root"), function(object) {
  x <- numeric()
  if (length(timegrid(object)) != 0) {
    x <- timegrid(object) / 60
    return(x)
  } else {
    print(paste0("timegrid of object '", name(object), "' has no entries"))
  }
})
#----Op (value = missing)----------------------------------
#'Op
#'
#'Returns a matrix depending on variable "Op" (Root on/off). If varaible is
#'used in object Root the function returns a matrix with an entrie at any
#'position of the col-name of the variable depending on length of timegrid.
#'If variable is not used in object Root the function returns a zero matrix.
#'
#'@export
#'@param object An Root
#'@return matrix with row-length of timegrid and col-length of coordinates of object Root
#'@examples
#'library(Sandbox)
#'library(CHP)
#'library(Demth)
#'library(PubGfuel)
#'library(Demel)
#'library(Bat)
#'cl<-new.Sandbox()
#'name(cl)<-'Sandy'
#'timegrid(cl)<-rep(15,3)
#'chp<-new.CHP()
#'bat1<-new.Bat()
#'bat2<-new.Bat()
#'fuel<-new.PubGfuel()
#'demth<-new.Demth()
#'demel<-new.Demel()
#'#Let us connect these things
#'components(cl)<-list(chp,bat1,bat2,fuel,demth,demel)
#'#connect CHP with both batteries (limited to 100 kW)
#'adjacency_el(cl)<-rbind(c(0,100,100,0,0,100),c(0,0,0,0,0,100),c(0,0,0,0,0,50),c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0));
#'adjacency_th(cl)<-rbind(c(0,0,0,0,100,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0));
#'adjacency_fuel(cl)<-rbind(c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(100,0,0,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0));
#'cl<-finalcoordinates(cl)
#'Op(components(cl)[[1]])
#'
setMethod("Op", signature(object = "Root", value = "missing",steps = "missing"), function(object) {
  co <- coord(object)

  n <- length(co)

  te <- length(object@timegrid)
  name<-  str_split_fixed(object@name,"_",2)[,2]
  where<-grep(paste0("Op_",name, "_"), co)
  u <- paste0( "Op_of_", name,  "_time", 1:te)


  if (length(where)==0 ) {
    id <- matrix(0, te, n)

    colnames(id) <- co

    row.names(id) <- u
    return(id)
  }

  id <- diag(n)
  id<-id[,where];
  #from here it's pretty nasty, but short!!! We sum every te-th line of t(id)
  w<-length(where)/te;
  id<-matrix(id,byrow = T,nrow = w);
  id<-colSums(id);
  id<-matrix(id,byrow = T,ncol=n);
  colnames(id)<-co;
  rownames(id)<-u


  # id <- t(id[, where])
  #
  # row.names(id) <- u
  # colnames(id) <- co



  return(id)


})
#----Op (value = numeric)----------------------------------
#'Op
#'
#'projection matrix on Op of components in Root called by their number in the list components
#'
#'@export
#'@param object Root
#'@return matrix
#'@examples
#'library(Sandbox)
#'library(CHP)
#'library(Demth)
#'library(PubGfuel)
#'library(Demel)
#'library(Bat)
#'cl<-new.Sandbox()
#'name(cl)<-'Sandy'
#'timegrid(cl)<-rep(15,3)
#'chp<-new.CHP()
#'bat1<-new.Bat()
#'bat2<-new.Bat()
#'fuel<-new.PubGfuel()
#'demth<-new.Demth()
#'demel<-new.Demel()
#'#Let us connect these things
#'components(cl)<-list(chp,chp,bat2,fuel,demth,demel)
#'#connect CHP with both batteries (limited to 100 kW)
#'adjacency_el(cl)<-rbind(c(0,0,100,0,0,100),c(0,0,0,0,0,100),c(0,0,0,0,0,50),c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0));
#'adjacency_th(cl)<-rbind(c(0,0,0,0,100,0),c(0,0,0,0,100,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0));
#'adjacency_fuel(cl)<-rbind(c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(100,100,0,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0));
#'cl<-finalcoordinates(cl)
#'Op(cl,c(1))

setMethod("Op",signature(object="Root",value="numeric",steps = "missing"),function(object,value){
  if(any(value%%1!=0)){
    stop("value has to be integer")
  }

  if(any(value>length(components(object)))){
    stop(paste0("value to big, there is/are no",value[value>lenght(components(object))],"th component(s)"))
  }

  co<-coord(object);
  n<-length(co);
  te<-length(timegrid(object));

  if(length(value)==0){
    if(n==0){
      stop("Coordinates are neccessary!!!")
    }
    id<-matrix(0,te,n);
    colnames(id)<-co;
    return(id)
  }


  names<-str_split_fixed(compoNames(object)[value],"_",2)[,2]
  exp<-paste(paste0("Op_",names, "_"), sep="", collapse="|")
  #at which positions are the components with Op?
  where<-grep(exp,co);
  u <- paste0( "Op_of_",paste0(names,collapse = "_and_"), "_time", 1:te)


  if(length(where)==0){
    id<-matrix(0,te,n);
    colnames(id)<-co;
    rownames(id)<-u;
    return(id)
  }


  id<-diag(n);

  id<-id[,where];
  #from here it's pretty nasty, but short!!! We sum every te-th line of t(id)
  w<-length(where)/te;

  id<-matrix(id,byrow = T,nrow = w);
  id<-colSums(id);
  id<-matrix(id,byrow = T,ncol=n);
  colnames(id)<-co;
  rownames(id)<-u

  return(id)
})
#----Op (value = character)--------------------------------
#'Op
#'
#'projection matrix on Op of components in Root called by their name
#'
#'@export
#'@param object Root
#'@return matrix
#'@examples
#'library(Sandbox)
#'library(CHP)
#'library(Demth)
#'library(PubGfuel)
#'library(Demel)
#'library(Bat)
#'cl<-new.Sandbox()
#'name(cl)<-'Sandy'
#'timegrid(cl)<-rep(15,3)
#'chp<-new.CHP()
#'bat1<-new.Bat()
#'bat2<-new.Bat()
#'fuel<-new.PubGfuel()
#'demth<-new.Demth()
#'demel<-new.Demel()
#'#Let us connect these things
#'components(cl)<-list(chp,chp,bat2,fuel,demth,demel)
#'#connect CHP with both batteries (limited to 100 kW)
#'adjacency_el(cl)<-rbind(c(0,0,100,0,0,100),c(0,0,0,0,0,100),c(0,0,0,0,0,50),c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0));
#'adjacency_th(cl)<-rbind(c(0,0,0,0,100,0),c(0,0,0,0,100,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0));
#'adjacency_fuel(cl)<-rbind(c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(100,100,0,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0));
#'cl<-finalcoordinates(cl)
#'# do not remember the name of the component: call compoNames(cl)
#'Op(cl,c("Sandy_CHP1","Sandy_CHP2"))

setMethod("Op",signature(object="Root",value="character",steps = "missing"),function(object,value){

  if(!all(is.element(value,compoNames(object)))){
    stop("value has to be a subvector of names of components")
  }

  co<-coord(object);
  n<-length(co);
  te<-length(timegrid(object));

  names<-str_split_fixed(value,"_",2)[,2]

  exp<-paste(paste0("Op_",names, "_"), sep="", collapse="|")
  #at which positions are the components with Op?
  where<-grep(exp,co);
  u <- paste0( "Op_of_",paste0(names,collapse = "_and_"),  "_time", 1:te)

  if(length(where)==0){
    id<-matrix(0,te,n);
    colnames(id)<-co;
    rownames(id)<-u;
    return(id)
  }


  id<-diag(n);

  id<-id[,where];
  #from here it's pretty nasty, but short!!! We sum every te-th line of t(id)
  w<-length(where)/te;
  id<-matrix(id,byrow = T,nrow = w);
  id<-colSums(id);
  id<-matrix(id,byrow = T,ncol=n);
  colnames(id)<-co;
  rownames(id)<-u;

  return(id)
})

#----Op (value = character)--------------------------------
#'Op
#'
#'projection matrix on Op of components in Root called by their name
#'
#'@export
#'@param object character
#'@return matrix
#'@examples
#'library(Sandbox)
#'library(CHP)
#'library(Demth)
#'library(PubGfuel)
#'library(Demel)
#'library(Bat)
#'cl<-new.Sandbox()
#'name(cl)<-'Sandy'
#'timegrid(cl)<-rep(15,3)
#'chp<-new.CHP()
#'bat1<-new.Bat()
#'bat2<-new.Bat()
#'fuel<-new.PubGfuel()
#'demth<-new.Demth()
#'demel<-new.Demel()
#'#Let us connect these things
#'components(cl)<-list(chp,chp,bat2,fuel,demth,demel)
#'#connect CHP with both batteries (limited to 100 kW)
#'adjacency_el(cl)<-rbind(c(0,0,100,0,0,100),c(0,0,0,0,0,100),c(0,0,0,0,0,50),c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0));
#'adjacency_th(cl)<-rbind(c(0,0,0,0,100,0),c(0,0,0,0,100,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0));
#'adjacency_fuel(cl)<-rbind(c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(100,100,0,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0));
#'cl<-finalcoordinates(cl)
#'# do not remember the name of the component: call compoNames(cl)
#'Op(coord(cl),c("Sandy_CHP1","Sandy_CHP2"))

setMethod("Op",signature(object="character",value="character",steps = "numeric"),function(object,value,steps){
  names<-str_split_fixed(value,"_",2)[,2]
  if(!any(grepl(names,object))){
    stop(paste0(names," has to occure in object"))
  }

  co<-object;
  n<-length(co);
  te<-steps;

  names<-str_split_fixed(value,"_",2)[,2]

  exp<-paste(paste0("Op_",names, "_"), sep="", collapse="|")
  #at which positions are the components with Op?
  where<-grep(exp,co);
  u <- paste0( "Op_of_",paste0(names,collapse = "_and_"),  "_time", 1:te)

  if(length(where)==0){
    id<-matrix(0,te,n);
    colnames(id)<-co;
    rownames(id)<-u;
    return(id)
  }


  id<-diag(n);

  id<-id[,where];
  #from here it's pretty nasty, but short!!! We sum every te-th line of t(id)
  w<-length(where)/te;
  id<-matrix(id,byrow = T,nrow = w);
  id<-colSums(id);
  id<-matrix(id,byrow = T,ncol=n);
  colnames(id)<-co;
  rownames(id)<-u;

  return(id)
})



#----P_el_plus (value = missing)---------------------------
#' P_el_plus
#'
#'Returns a matrix depending on variable "P_el_plus". If varaible is used in
#'object Root the function returns a matrix with an entrie at any position of
#'the col-name of the variable depending on length of timegrid. If variable
#'is not used in object Root the function returns a zero matrix.
#'
#'@export
#'@param object An Root
#'@return matrix with row-length of timegrid and col-length of coordinates of object Root
#'@examples
#'library(Sandbox)
#'library(Bat)
#'library(Demel)
#'cl <- new.Sandbox()
#'name(cl) <- "Sandy"
#'timegrid(cl) <- rep(15,3)
#'bat <- new.Bat()
#'demel <- new.Demel()
#'components(cl) <- list(bat,demel,demel)
#'adjacency_el(cl) <- rbind(c(0,100,100),c(0,0,0),c(0,0,0))
#'cl<-finalcoordinates(cl)
#'P_el_plus(components(cl)[[1]])
setMethod("P_el_plus", signature(object = "Root", value = "missing",steps = "missing"), function(object) {

  co <- coord(object)

  n <- length(co)

  te <- length(object@timegrid)

  name<-  str_split_fixed(object@name,"_",2)[,2]
  where<- grep(paste0("Pel_",name, "_"), co)
  u <- paste0( "Pel_from_", name,  "_time", 1:te)


  if (length(where)==0) {
    id <- matrix(0, te, n)

    colnames(id) <- co

    row.names(id) <- u
    return(id)
  }

  id <- diag(n)
  id<-id[,where];
  #from here it's pretty nasty, but short!!! We sum every te-th line of t(id)
  w<-length(where)/te;
  id<-matrix(id,byrow = T,nrow = w);
  id<-colSums(id);
  id<-matrix(id,byrow = T,ncol=n);
  colnames(id)<-co;
  rownames(id)<-u


  # id <- t(id[, where])
  #
  # colnames(id) <- co
  #
  # row.names(id) <- u

  return(id)

})
#----P_el_plus (value = numeric)---------------------------
#'P_el_plus
#'
#'projection matrix on P_el_plus of components in Root called by their number in the list components
#'
#'@export
#'@param object Root
#'@return matrix
#'@examples
#'library(Sandbox)
#'library(Bat)
#'library(Demel)
#'cl <- new.Sandbox()
#'name(cl) <- "Sandy"
#'timegrid(cl) <- rep(15,3)
#'bat <- new.Bat()
#'demel <- new.Demel()
#'components(cl) <- list(bat,demel,demel)
#'adjacency_el(cl) <- rbind(c(0,100,100),c(0,0,0),c(0,0,0))
#'cl<-finalcoordinates(cl)
#'P_el_plus(cl,1)

setMethod("P_el_plus",signature(object="Root",value="numeric",steps = "missing"),function(object,value){
  if(any(value%%1!=0)){
    stop("value has to be integer")
  }

  if(any(value>length(components(object)))){
    stop(paste0("value to big, there is/are no",value[value>lenght(components(object))],"th component(s)"))
  }

  co<-coord(object);
  n<-length(co);
  te<-length(timegrid(object));
  if(length(value)==0){
    if(n==0){
      stop("Coordinates are neccessary!!!")
    }
    id<-matrix(0,te,n);
    colnames(id)<-co;
    return(id)
  }


  names<-str_split_fixed(compoNames(object)[value],"_",2)[,2]
  exp<-paste(paste0("Pel_",names, "_"), sep="", collapse="|")
  where<-grep(exp,co);
  u <- paste0("Pel_from_",paste0(names,collapse = "_and_"),"_time", 1:te)
  #at which positions are the components with P_el_plus?


  if(length(where)==0){
    id<-matrix(0,te,n);
    colnames(id)<-co;
    rownames(id)<-u
    return(id)
  }

  id<-diag(n);
  colnames(id)<-co;
  id<-id[,where];
  #from here it's pretty nasty, but short!!! We sum every te-th line of t(id)
  w<-length(where)/te;
  id<-matrix(id,byrow = T,nrow = w);
  id<-colSums(id);
  id<-matrix(id,byrow = T,ncol=length(co));
  colnames(id)<-co;
  rownames(id)<-u
  # names<-compoNames(object);
  # names<-names[value];
  # exp<-paste(paste0(names,"_P_el_plus"), sep="", collapse="|")
  # #at which positions are the components with P_el_plus?
  # where<-grep(exp,co);
  #
  # if(length(where)==0){
  #   id<-matrix(0,te,n);
  #   colnames(id)<-co;
  #   return(id)
  # }
  #
  # id<-diag(n);
  # colnames(id)<-co;
  # id<-id[,where];
  # #from here it's pretty nasty, but short!!! We sum every te-th line of t(id)
  # w<-length(where)/te;
  # id<-matrix(id,byrow = T,nrow = w);
  # id<-colSums(id);
  # id<-matrix(id,byrow = T,ncol=length(co));
  # colnames(id)<-co;

  return(id)
})
#----P_el_plus (value = character)-------------------------
#'P_el_plus
#'
#'projection matrix on P_el_plus of components in Root called by their name
#'
#'@export
#'@param object Root
#'@return matrix
#'@examples
#'library(Sandbox)
#'library(Bat)
#'library(Demel)
#'cl <- new.Sandbox()
#'name(cl) <- "Sandy"
#'timegrid(cl) <- rep(15,3)
#'bat <- new.Bat()
#'demel <- new.Demel()
#'components(cl) <- list(bat,demel,demel)
#'adjacency_el(cl) <- rbind(c(0,100,100),c(0,0,0),c(0,0,0))
#'cl<-finalcoordinates(cl)
#'# do not remember the name of the component: call compoNames(cl)
#'P_el_plus(cl,"Sandy_Bat1")
setMethod("P_el_plus",signature(object="Root",value="character",steps = "missing"),function(object,value){

  if(!all(is.element(value,compoNames(object)))){
    stop("value has to be a subvector of names of components")
  }

  co<-coord(object);
  n<-length(co);
  te<-length(timegrid(object));
  names<-str_split_fixed(value,"_",2)[,2]
  exp<-paste(paste0("Pel_",names, "_"), sep="", collapse="|")
  where<-grep(exp,co);
  u <- paste0("Pel_from_",paste0(names,collapse = "_and_"),"_time", 1:te)
  #at which positions are the components with P_el_plus?


  if(length(where)==0){
    id<-matrix(0,te,n);
    colnames(id)<-co;
    rownames(id)<-u
    return(id)
  }

  id<-diag(n);
  colnames(id)<-co;
  id<-id[,where];
  #from here it's pretty nasty, but short!!! We sum every te-th line of t(id)
  w<-length(where)/te;
  id<-matrix(id,byrow = T,nrow = w);
  id<-colSums(id);
  id<-matrix(id,byrow = T,ncol=length(co));
  colnames(id)<-co;
  rownames(id)<-u

  return(id)
  # names<-value;
  # exp<-paste(paste0(names,"_P_el_plus"), sep="", collapse="|")
  # #at which positions are the components with P_el_plus?
  # where<-grep(exp,co);
  # n<-length(co);
  # te<-length(timegrid(object));
  # if(length(where)==0){
  #   id<-matrix(0,te,n);
  #   colnames(id)<-co;
  #   return(id)
  # }
  #
  # id<-diag(n);
  # colnames(id)<-co;
  # id<-id[,where];
  # #from here it's pretty nasty, but short!!! We sum every te-th line of t(id)
  # w<-length(where)/te;
  # id<-matrix(id,byrow = T,nrow = w);
  # id<-colSums(id);
  # id<-matrix(id,byrow = T,ncol=length(co));
  # colnames(id)<-co;
  #
  # return(id)
})



  #----P_el_plus (value = character)-------------------------
  #'P_el_plus
  #'
  #'projection matrix on P_el_plus of components in Root called by their name
  #'
  #'@export
  #'@param object character
  #'@return matrix
  #'@examples
  #'library(Sandbox)
  #'library(Bat)
  #'library(Demel)
  #'cl <- new.Sandbox()
  #'name(cl) <- "Sandy"
  #'timegrid(cl) <- rep(15,3)
  #'bat <- new.Bat()
  #'demel <- new.Demel()
  #'components(cl) <- list(bat,demel,demel)
  #'adjacency_el(cl) <- rbind(c(0,100,100),c(0,0,0),c(0,0,0))
  #'cl<-finalcoordinates(cl)
  #'# do not remember the name of the component: call compoNames(cl)
  #'P_el_plus(coord(cl),"Sandy_Bat1")
  setMethod("P_el_plus",signature(object="character",value="character",steps = "numeric"),function(object,value,steps){

    names<-str_split_fixed(value,"_",2)[,2]
    if(!any(grepl(names,object))){
      stop(paste0(names," has to occure in object"))
    }

    co<-object;
    n<-length(co);
    te<-steps;
    names<-str_split_fixed(value,"_",2)[,2]
    exp<-paste(paste0("Pel_",names, "_"), sep="", collapse="|")
    where<-grep(exp,co);
    u <- paste0("Pel_from_",paste0(names,collapse = "_and_"),"_time", 1:te)
    #at which positions are the components with P_el_plus?


    if(length(where)==0){
      id<-matrix(0,te,n);
      colnames(id)<-co;
      rownames(id)<-u
      return(id)
    }

    id<-diag(n);
    colnames(id)<-co;
    id<-id[,where];
    #from here it's pretty nasty, but short!!! We sum every te-th line of t(id)
    w<-length(where)/te;
    id<-matrix(id,byrow = T,nrow = w);
    id<-colSums(id);
    id<-matrix(id,byrow = T,ncol=length(co));
    colnames(id)<-co;
    rownames(id)<-u

    return(id)
    # names<-value;
    # exp<-paste(paste0(names,"_P_el_plus"), sep="", collapse="|")
    # #at which positions are the components with P_el_plus?
    # where<-grep(exp,co);
    # n<-length(co);
    # te<-length(timegrid(object));
    # if(length(where)==0){
    #   id<-matrix(0,te,n);
    #   colnames(id)<-co;
    #   return(id)
    # }
    #
    # id<-diag(n);
    # colnames(id)<-co;
    # id<-id[,where];
    # #from here it's pretty nasty, but short!!! We sum every te-th line of t(id)
    # w<-length(where)/te;
    # id<-matrix(id,byrow = T,nrow = w);
    # id<-colSums(id);
    # id<-matrix(id,byrow = T,ncol=length(co));
    # colnames(id)<-co;
    #
    # return(id)
  })
#----P_el_minus (value = missing)--------------------------
#'P_el_minus
#'
#'Returns a matrix depending on variable "P_el_minus". If varaible is used
#'in object Root the function returns a matrix with an entrie at any position
#'of the col-name of the variable depending on length of timegrid. If variable
#'is not used in object Root the function returns a zero matrix.
#'
#'@export
#'@param object An Root
#'@return matrix with row-length of timegrid and col-length of coordinates of object Root
#'@examples
#'library(Sandbox)
#'library(Bat)
#'library(Demel)
#'cl <- new.Sandbox()
#'name(cl) <- "Sandy"
#'timegrid(cl) <- rep(15,3)
#'bat <- new.Bat()
#'demel <- new.Demel()
#'components(cl) <- list(bat,bat,demel)
#'adjacency_el(cl) <- rbind(c(0,0,100),c(0,0,100),c(0,0,0))
#'cl<-finalcoordinates(cl)
#'P_el_minus(components(cl)[[3]])
setMethod("P_el_minus", signature(object = "Root", value = "missing",steps = "missing"), function(object) {
  co <- coord(object)

  n <- length(co)

  te <- length(object@timegrid)

  name<-str_split_fixed(object@name,"_",2)[,2]
  exp<-paste(paste0("Pel"), sep="", collapse="|")
  #at which positions are the coordinates of the consuming components?

  where<-co[grep(exp,co)];
  where<-where[grep(paste(name,"_time", sep="", collapse="|"),where)]

  u <- paste0( "Pel_to_", name,  "_time", 1:te)


  if (length(where)==0) {
    id <- matrix(0, te, n)

    colnames(id) <- co

    row.names(id) <- u
    return(id)
  }


  id <- diag(n)
  colnames(id)<-co;
  id<-id[,where];
  #from here it's pretty nasty, but short!!! We sum every te-th line of t(id)
  w<-length(where)/te;
  id<-matrix(id,byrow = T,nrow = w);
  id<-colSums(id);
  id<-matrix(id,byrow = T,ncol=n);
  colnames(id)<-co;
  rownames(id)<-u


  # id <- t(id[, where])
  #
  # colnames(id) <- co
  # row.names(id) <- u

  return(id)

})
#----P_el_minus (value = numeric)--------------------------
#'P_el_minus
#'
#'projection matrix on P_el_minus of components in Root called by their number in the list components
#'
#'@export
#'@param object Root
#'@return matrix
#'@examples
#'library(Sandbox)
#'library(Bat)
#'library(Demel)
#'cl <- new.Sandbox()
#'name(cl) <- "Sandy"
#'timegrid(cl) <- rep(15,3)
#'bat <- new.Bat()
#'demel <- new.Demel()
#'components(cl) <- list(bat,bat,demel)
#'adjacency_el(cl) <- rbind(c(0,0,100),c(0,0,100),c(0,0,0))
#'cl<-finalcoordinates(cl)
#'P_el_minus(cl,3)

setMethod("P_el_minus",signature(object="Root",value="numeric",steps = "missing"),function(object,value){
  if(any(value%%1!=0)){
    stop("value has to be integer")
  }

  if(any(value>length(components(object)))){
    stop(paste0("value to big, there is/are no",value[value>lenght(components(object))],"th component(s)"))
  }

  co<-coord(object);
  n<-length(co);
  te<-length(timegrid(object));
  if(length(value)==0){
    if(n==0){
      stop("Coordinates are neccessary!!!")
    }
    id<-matrix(0,te,n);
    colnames(id)<-co;
    return(id)
  }

  names<-str_split_fixed(compoNames(object)[value],"_",2)[,2]
  exp<-"Pel"
  #at which positions are the coordinates of the consuming components?

  where<-co[grep(exp,co)];
  where<-where[grep(paste(names,"_time", sep="", collapse="|"),where)]
  u <- paste0( "Pel_to_", paste0(names,collapse = "_and_"), "_time", 1:te)


  if(length(where)==0){
    id<-matrix(0,te,n);
    colnames(id)<-co;
    rownames(id)<-u;
    return(id)
  }

  id<-diag(n);
  colnames(id)<-co;
  id<-id[,where];

  #from here it's pretty nasty, but short!!! We sum every te-th line of t(id)
  w<-length(where)/te;
  id<-matrix(id,byrow = T,nrow = w);
  id<-colSums(id);
  id<-matrix(id,byrow = T,ncol=length(co));
  colnames(id)<-co;
  rownames(id)<-u;

  return(id)
  #at which positions are the components with P_el_minus?
  # where<-co[grep(exp,co)];
  # where<-grep(paste(names,"_time", sep="", collapse="|"),where)
  #
  #
  # exp<-paste(paste0(names,"_P_el_minus"), sep="", collapse="|")
  # #at which positions are the components with P_el_minus?
  # where<-grep(exp,co);
  #
  # if(length(where)==0){
  #   id<-matrix(0,te,n);
  #   colnames(id)<-co;
  #   return(id)
  # }
  #
  # id<-diag(n);
  # colnames(id)<-co;
  # id<-id[,where];
  # #from here it's pretty nasty, but short!!! We sum every te-th line of t(id)
  # w<-length(where)/te;
  # id<-matrix(id,byrow = T,nrow = w);
  # id<-colSums(id);
  # id<-matrix(id,byrow = T,ncol=length(co));
  # colnames(id)<-co;
  #
  # return(id)
})
#----P_el_minus (value = character)------------------------
#'P_el_minus
#'
#'projection matrix on P_el_minus of components in Root called by their name
#'
#'@export
#'@param object Root
#'@return matrix
#'@examples
#'library(Sandbox)
#'library(Bat)
#'library(Demel)
#'cl <- new.Sandbox()
#'name(cl) <- "Sandy"
#'timegrid(cl) <- rep(15,3)
#'bat <- new.Bat()
#'demel <- new.Demel()
#'components(cl) <- list(bat,bat,demel)
#'adjacency_el(cl) <- rbind(c(0,0,100),c(0,0,100),c(0,0,0))
#'cl<-finalcoordinates(cl)
#' # do not remember the name of the component call compoNames(cl)
#'P_el_minus(cl,"Sandy_Demel1")

setMethod("P_el_minus",signature(object="Root",value="character",steps = "missing"),function(object,value){

  if(!all(is.element(value,compoNames(object)))){
    stop("value has to be a subvector of names of components")
  }

  co<-coord(object);
  n<-length(co);
  te<-length(timegrid(object));

  names<-str_split_fixed(value,"_",2)[,2]
  exp<-"Pel"
  #at which positions are the coordinates of the consuming components?
  where<-co[grep(exp,co)];
  where<-where[grep(paste(names,"_time", sep="", collapse="|"),where)]
  u <- paste0( "Pel_to_",paste0(names,collapse = "_and_"),  "_time", 1:te)

  if(length(where)==0){
    id<-matrix(0,te,n);
    colnames(id)<-co;
    rownames(id)<-u;
    return(id)
  }

  id<-diag(n);
  colnames(id)<-co;
  id<-id[,where];
  #from here it's pretty nasty, but short!!! We sum every te-th line of t(id)
  w<-length(where)/te;
  id<-matrix(id,byrow = T,nrow = w);
  id<-colSums(id);
  id<-matrix(id,byrow = T,ncol=length(co));
  colnames(id)<-co;
  rownames(id)<-u;

  return(id)
  # exp<-paste(paste0(names,"_P_el_minus"), sep="", collapse="|")
  # #at which positions are the components with P_el_minus?
  # where<-grep(exp,co);
  # n<-length(co);
  # te<-length(timegrid(object));
  # if(length(where)==0){
  #   id<-matrix(0,te,n);
  #   colnames(id)<-co;
  #   return(id)
  # }
  #
  # id<-diag(n);
  # colnames(id)<-co;
  # id<-id[,where];
  # #from here it's pretty nasty, but short!!! We sum every te-th line of t(id)
  # w<-length(where)/te;
  # id<-matrix(id,byrow = T,nrow = w);
  # id<-colSums(id);
  # id<-matrix(id,byrow = T,ncol=length(co));
  # colnames(id)<-co;
  #
  # return(id)
})



    #----P_el_minus (value = character)------------------------
    #'P_el_minus
    #'
    #'projection matrix on P_el_minus of components in Root called by their name
    #'
    #'@export
    #'@param object character
    #'@return matrix
    #'@examples
    #'library(Sandbox)
    #'library(Bat)
    #'library(Demel)
    #'cl <- new.Sandbox()
    #'name(cl) <- "Sandy"
    #'timegrid(cl) <- rep(15,3)
    #'bat <- new.Bat()
    #'demel <- new.Demel()
    #'components(cl) <- list(bat,bat,demel)
    #'adjacency_el(cl) <- rbind(c(0,0,100),c(0,0,100),c(0,0,0))
    #'cl<-finalcoordinates(cl)
    #' # do not remember the name of the component call compoNames(cl)
    #'P_el_minus(coord(cl),"Sandy_Demel1")

    setMethod("P_el_minus",signature(object="character",value="character",steps = "numeric"),function(object,value,steps){

      names<-str_split_fixed(value,"_",2)[,2]
      if(!any(grepl(names,object))){
        stop(paste0(names," has to occure in object"))
      }

      co<-object;
      n<-length(co);
      te<-steps;


      exp<-"Pel"
      #at which positions are the coordinates of the consuming components?
      where<-co[grep(exp,co)];
      where<-where[grep(paste(names,"_time", sep="", collapse="|"),where)]
      u <- paste0( "Pel_to_",paste0(names,collapse = "_and_"),  "_time", 1:te)

      if(length(where)==0){
        id<-matrix(0,te,n);
        colnames(id)<-co;
        rownames(id)<-u;
        return(id)
      }

      id<-diag(n);
      colnames(id)<-co;
      id<-id[,where];
      #from here it's pretty nasty, but short!!! We sum every te-th line of t(id)
      w<-length(where)/te;
      id<-matrix(id,byrow = T,nrow = w);
      id<-colSums(id);
      id<-matrix(id,byrow = T,ncol=length(co));
      colnames(id)<-co;
      rownames(id)<-u;

      return(id)
      # exp<-paste(paste0(names,"_P_el_minus"), sep="", collapse="|")
      # #at which positions are the components with P_el_minus?
      # where<-grep(exp,co);
      # n<-length(co);
      # te<-length(timegrid(object));
      # if(length(where)==0){
      #   id<-matrix(0,te,n);
      #   colnames(id)<-co;
      #   return(id)
      # }
      #
      # id<-diag(n);
      # colnames(id)<-co;
      # id<-id[,where];
      # #from here it's pretty nasty, but short!!! We sum every te-th line of t(id)
      # w<-length(where)/te;
      # id<-matrix(id,byrow = T,nrow = w);
      # id<-colSums(id);
      # id<-matrix(id,byrow = T,ncol=length(co));
      # colnames(id)<-co;
      #
      # return(id)
    })
#----P_th_plus (value = missing)---------------------------
#'P_th_plus
#'
#'Returns a matrix depending on variable "P_th_plus". If varaible is used in
#'object Root the function returns a matrix with an entrie at any position of
#'the col-name of the variable depending on length of timegrid. If variable is
#'not used in object Root the function returns a zero matrix.
#'
#'@export
#'@param object An Root
#'@return matrix with row-length of timegrid and col-length of coordinates of object Root
#'@examples
#'library(Sandbox)
#'library(PubGth)
#'library(Demth)
#'cl <- new.Sandbox()
#'name(cl) <- "Sandy"
#'timegrid(cl) <- rep(15,3)
#'pubth <- new.PubGth()
#'demth <- new.Demth()
#'components(cl) <- list(pubth,pubth,demth)
#'adjacency_th(cl) <- rbind(c(0,1000,100),c(0,0,100),c(0,0,0))
#'cl<-finalcoordinates(cl)
#'P_th_plus(components(cl)[[1]])
#'

setMethod("P_th_plus", signature(object = "Root", value = "missing",steps = "missing"), function(object) {
  co <- coord(object)

  n <- length(co)

  te <- length(object@timegrid)
  name<-  str_split_fixed(object@name,"_",2)[,2]
  where<- grep(paste0("Pth_",name, "_"), co)
  u <- paste0( "Pth_from_", name,  "_time", 1:te)

  if (length(where) == 0 ) {
    id <- matrix(0, te, n)

    colnames(id) <- co

    row.names(id) <- u
    return(id)
  }

  id <- diag(n)

  id<-id[,where];
  #from here it's pretty nasty, but short!!! We sum every te-th line of t(id)
  w<-length(where)/te;
  id<-matrix(id,byrow = T,nrow = w);
  id<-colSums(id);
  id<-matrix(id,byrow = T,ncol=n);
  colnames(id)<-co;
  rownames(id)<-u


  # id <- t(id[, where])
  #
  # row.names(id) <- u
  # colnames(id) <- co



  return(id)

})
#----P_th_plus (value = numeric)---------------------------
#'P_th_plus
#'
#'projection matrix on P_th_plus of components in Root called by their number in the list components
#'
#'@export
#'@param object Root
#'@return matrix
#'@examples
#'library(Sandbox)
#'library(PubGth)
#'library(Demth)
#'cl <- new.Sandbox()
#'name(cl) <- "Sandy"
#'timegrid(cl) <- rep(15,3)
#'pubth <- new.PubGth()
#'demth <- new.Demth()
#'components(cl) <- list(pubth,pubth,demth)
#'adjacency_th(cl) <- rbind(c(0,1000,100),c(0,0,100),c(0,0,0))
#'cl<-finalcoordinates(cl)
#'P_th_plus(cl,1)

setMethod("P_th_plus",signature(object="Root",value="numeric",steps = "missing"),function(object,value){
  if(any(value%%1!=0)){
    stop("value has to be integer")
  }

  if(any(value>length(components(object)))){
    stop(paste0("value to big, there is/are no",value[value>lenght(components(object))],"th component(s)"))
  }

  co<-coord(object);
  n<-length(co);
  te<-length(timegrid(object));
  if(length(value)==0){
    if(n==0){
      stop("Coordinates are neccessary!!!")
    }
    id<-matrix(0,te,n);
    colnames(id)<-co;
    return(id)
  }


  names<-str_split_fixed(compoNames(object)[value],"_",2)[,2]
  exp<-paste(paste0("Pth_",names, "_"), sep="", collapse="|")
  #at which positions are the components with P_th_plus?
  where<-grep(exp,co);
  u <- paste0( "Pth_from_", paste0(names,collapse = "_and_"), "_time", 1:te)

  if(length(where)==0){
    id<-matrix(0,te,n);
    colnames(id)<-co;
    rownames(id)<-u
    return(id)
  }

  id<-diag(n);
  colnames(id)<-co;
  id<-id[,where];
  #from here it's pretty nasty, but short!!! We sum every te-th line of t(id)
  w<-length(where)/te;
  id<-matrix(id,byrow = T,nrow = w);
  id<-colSums(id);
  id<-matrix(id,byrow = T,ncol=length(co));
  colnames(id)<-co;
  rownames(id)<-u

  return(id)

  # co<-coord(object);
  # n<-length(co);
  # te<-length(timegrid(object));
  # if(length(value)==0){
  #   if(n==0){
  #     stop("Coordinates are neccessary!!!")
  #   }
  #   id<-matrix(0,te,n);
  #   colnames(id)<-co;
  #   return(id)
  # }
  #
  # names<-compoNames(object);
  # names<-names[value];
  # exp<-paste(paste0(names,"_P_th_plus"), sep="", collapse="|")
  # #at which positions are the components with P_th_plus?
  # where<-grep(exp,co);
  #
  # if(length(where)==0){
  #   id<-matrix(0,te,n);
  #   colnames(id)<-co;
  #   return(id)
  # }
  #
  # id<-diag(n);
  # colnames(id)<-co;
  # id<-id[,where];
  # #from here it's pretty nasty, but short!!! We sum every te-th line of t(id)
  # w<-length(where)/te;
  # id<-matrix(id,byrow = T,nrow = w);
  # id<-colSums(id);
  # id<-matrix(id,byrow = T,ncol=length(co));
  # colnames(id)<-co;
  #
  # return(id)

})
#----P_th_plus (value = character)-------------------------
#'P_th_plus
#'
#'projection matrix on P_th_plus of components in Root called by their name
#'
#'@export
#'@param object Root
#'@return matrix
#'@examples
#'library(Sandbox)
#'library(PubGth)
#'library(Demth)
#'cl <- new.Sandbox()
#'name(cl) <- "Sandy"
#'timegrid(cl) <- rep(15,3)
#'pubth <- new.PubGth()
#'demth <- new.Demth()
#'components(cl) <- list(pubth,pubth,demth)
#'adjacency_th(cl) <- rbind(c(0,1000,100),c(0,0,100),c(0,0,0))
#'cl<-finalcoordinates(cl)
#'# do not remember the name of the component call compoNames(cl)
#'P_th_plus(cl,"Sandy_PubGth1")
setMethod("P_th_plus",signature(object="Root",value="character",steps = "missing"),function(object,value){

  if(!all(is.element(value,compoNames(object)))){
    stop("value has to be a subvector of names of components")
  }

  co<-coord(object);

  n<-length(co);
  te<-length(timegrid(object));
  names<-str_split_fixed(value,"_",2)[,2]
  exp<-paste(paste0("Pth_",names, "_"), sep="", collapse="|")
  #at which positions are the components with P_th_plus?
  where<-grep(exp,co);
  u <- paste0( "Pth_from_", paste0(names,collapse = "_and_"), "_time", 1:te)

  if(length(where)==0){
    id<-matrix(0,te,n);
    colnames(id)<-co;
    rownames(id)<-u
    return(id)
  }


  id<-diag(n);
  id<-id[,where];
  #from here it's pretty nasty, but short!!! We sum every te-th line of t(id)
  w<-length(where)/te;
  id<-matrix(id,byrow = T,nrow = w);
  id<-colSums(id);
  id<-matrix(id,byrow = T,ncol=length(co));
  colnames(id)<-co;
  rownames(id)<-u

  return(id)

  # co<-coord(object);
  #
  # names<-value;
  # exp<-paste(paste0(names,"_P_th_plus"), sep="", collapse="|")
  # #at which positions are the components with P_th_plus?
  # where<-grep(exp,co);
  # n<-length(co);
  # te<-length(timegrid(object));
  # if(length(where)==0){
  #   id<-matrix(0,te,n);
  #   colnames(id)<-co;
  #   return(id)
  # }
  #
  #
  # id<-diag(n);
  # colnames(id)<-co;
  # id<-id[,where];
  # #from here it's pretty nasty, but short!!! We sum every te-th line of t(id)
  # w<-length(where)/te;
  # id<-matrix(id,byrow = T,nrow = w);
  # id<-colSums(id);
  # id<-matrix(id,byrow = T,ncol=length(co));
  # colnames(id)<-co;
  #
  # return(id)

})


      #----P_th_plus (value = character)-------------------------
      #'P_th_plus
      #'
      #'projection matrix on P_th_plus of components in Root called by their name
      #'
      #'@export
      #'@param object character
      #'@return matrix
      #'@examples
      #'library(Sandbox)
      #'library(PubGth)
      #'library(Demth)
      #'cl <- new.Sandbox()
      #'name(cl) <- "Sandy"
      #'timegrid(cl) <- rep(15,3)
      #'pubth <- new.PubGth()
      #'demth <- new.Demth()
      #'components(cl) <- list(pubth,pubth,demth)
      #'adjacency_th(cl) <- rbind(c(0,1000,100),c(0,0,100),c(0,0,0))
      #'cl<-finalcoordinates(cl)
      #'# do not remember the name of the component call compoNames(cl)
      #'P_th_plus(coord(cl),"Sandy_PubGth1")
      setMethod("P_th_plus",signature(object="character",value="character",steps = "numeric"),function(object,value,steps){

        names<-str_split_fixed(value,"_",2)[,2]
        if(!any(grepl(names,object))){
          stop(paste0(names," has to occure in object"))
        }

        co<-object;

        n<-length(co);
        te<-steps;

        exp<-paste(paste0("Pth_",names, "_"), sep="", collapse="|")
        #at which positions are the components with P_th_plus?
        where<-grep(exp,co);
        u <- paste0( "Pth_from_", paste0(names,collapse = "_and_"), "_time", 1:te)

        if(length(where)==0){
          id<-matrix(0,te,n);
          colnames(id)<-co;
          rownames(id)<-u
          return(id)
        }


        id<-diag(n);
        id<-id[,where];
        #from here it's pretty nasty, but short!!! We sum every te-th line of t(id)
        w<-length(where)/te;
        id<-matrix(id,byrow = T,nrow = w);
        id<-colSums(id);
        id<-matrix(id,byrow = T,ncol=length(co));
        colnames(id)<-co;
        rownames(id)<-u

        return(id)

        # co<-coord(object);
        #
        # names<-value;
        # exp<-paste(paste0(names,"_P_th_plus"), sep="", collapse="|")
        # #at which positions are the components with P_th_plus?
        # where<-grep(exp,co);
        # n<-length(co);
        # te<-length(timegrid(object));
        # if(length(where)==0){
        #   id<-matrix(0,te,n);
        #   colnames(id)<-co;
        #   return(id)
        # }
        #
        #
        # id<-diag(n);
        # colnames(id)<-co;
        # id<-id[,where];
        # #from here it's pretty nasty, but short!!! We sum every te-th line of t(id)
        # w<-length(where)/te;
        # id<-matrix(id,byrow = T,nrow = w);
        # id<-colSums(id);
        # id<-matrix(id,byrow = T,ncol=length(co));
        # colnames(id)<-co;
        #
        # return(id)

      })
#----P_th_minus (value = missing)--------------------------
#'P_th_minus
#'
#'Returns a matrix depending on variable "P_th_minus". If varaible is used in
#'object Root the function returns a matrix with an entrie at any position of
#'the col-name of the variable depending on length of timegrid. If variable is
#'not used in object Root the function returns a zero matrix.
#'
#'@export
#'@param object An Root
#'@return matrix with row-length of timegrid and col-length of coordinates of object Root
#'@examples
#'library(Sandbox)
#'library(PubGth)
#'library(Demth)
#'cl <- new.Sandbox()
#'name(cl) <- "Sandy"
#'timegrid(cl) <- rep(15,3)
#'pubgth <- new.PubGth()
#'demth <- new.Demth()
#'components(cl) <- list(pubgth,pubgth,demth)
#'adjacency_el(cl) <- rbind(c(0,0,100),c(0,0,100),c(0,0,0))
#'cl<-finalcoordinates(cl)
#'P_th_minus(components(cl)[[3]])

setMethod("P_th_minus", signature(object = "Root", value = "missing",steps = "missing"), function(object) {
  co <- coord(object)

  n <- length(co)

  te <- length(object@timegrid)
  name<-str_split_fixed(object@name,"_",2)[,2]
  exp<-"Pth"
  #at which positions are the coordinates of the consuming components?

  where<-co[grep(exp,co)];
  where<-where[grep(paste(name,"_time", sep="", collapse="|"),where)]

  u <-  paste0( "Pth_to_", name,  "_time", 1:te)

  if (length(where) == 0 ) {
    id <- matrix(0, te, n)

    colnames(id) <- co

    row.names(id) <-u
    return(id)
  }




  id <- diag(n)
  colnames(id)<-co;
  id<-id[,where];
  #from here it's pretty nasty, but short!!! We sum every te-th line of t(id)
  w<-length(where)/te;
  id<-matrix(id,byrow = T,nrow = w);
  id<-colSums(id);
  id<-matrix(id,byrow = T,ncol=n);
  colnames(id)<-co;
  rownames(id)<-u


  # id <- t(id[, where])
  #
  # colnames(id) <- co
  #
  # row.names(id) <- u

  return(id)
})
#----P_th_minus (value = numeric)--------------------------
#'P_th_minus
#'
#'projection matrix on P_th_minus of components in Root called by their number in the list components
#'
#'@export
#'@param object Root
#'@return matrix
#'@examples
#'library(Sandbox)
#'library(PubGth)
#'library(Demth)
#'cl <- new.Sandbox()
#'name(cl) <- "Sandy"
#'timegrid(cl) <- rep(15,3)
#'pubgth <- new.PubGth()
#'demth <- new.Demth()
#'components(cl) <- list(pubgth,pubgth,demth,demth)
#'adjacency_th(cl) <- rbind(c(0,0,100,100),c(0,0,100,100),c(0,0,0,0),c(0,0,0,0))
#'cl<-finalcoordinates(cl)
#'P_th_minus(cl,c(3,4))
setMethod("P_th_minus",signature(object="Root",value="numeric",steps = "missing"),function(object,value){
  if(any(value%%1!=0)){
    stop("value has to be integer")
  }

  if(any(value>length(components(object)))){
    stop(paste0("value to big, there is/are no ",value[value>lenght(components(object))],"th component(s)"))
  }
  co<-coord(object);
  n<-length(co);
  te<-length(timegrid(object));
  if(length(value)==0){
    if(n==0){
      stop("Coordinates are neccessary!!!")
    }
    id<-matrix(0,te,n);
    colnames(id)<-co;
    return(id)
  }


  names<-str_split_fixed(compoNames(object)[value],"_",2)[,2]
  exp<-"Pth_"
  #at which positions are the coordinates of the consuming components?
  where<-co[grep(exp,co)];
  where<-where[grep(paste(names,"_time", sep="", collapse="|"),where)]
  u <- paste0( "Pth_to_",paste0(names,collapse = "_and_"), "_time", 1:te)

  if(length(where)==0){
    id<-matrix(0,te,n);
    colnames(id)<-co;
    rownames(id)<-u;
    return(id)
  }

  id<-diag(n);
  colnames(id)<-co;
  id<-id[,where];
  #from here it's pretty nasty, but short!!! We sum every te-th line of t(id)
  w<-length(where)/te;
  id<-matrix(id,byrow = T,nrow = w);
  id<-colSums(id);
  id<-matrix(id,byrow = T,ncol=length(co));
  colnames(id)<-co;
  rownames(id)<-u

  return(id)
  # names<-compoNames(object);
  # names<-names[value];
  # exp<-paste(paste0(names,"_P_th_minus"), sep="", collapse="|")
  # #at which positions are the components with P_th_minus?
  # where<-grep(exp,co);
  #
  # if(length(where)==0){
  #   id<-matrix(0,te,n);
  #   colnames(id)<-co;
  #   return(id)
  # }
  #
  # id<-diag(n);
  # colnames(id)<-co;
  # id<-id[,where];
  # #from here it's pretty nasty, but short!!! We sum every te-th line of t(id)
  # w<-length(where)/te;
  # id<-matrix(id,byrow = T,nrow = w);
  # id<-colSums(id);
  # id<-matrix(id,byrow = T,ncol=length(co));
  # colnames(id)<-co;
  #
  # return(id)
})
#----P_th_minus (value = character)------------------------
#'P_th_minus
#'
#'projection matrix on P_th_minus of components in Root called by their name
#'
#'@export
#'@param object Root
#'@return matrix
#'@examples
#'library(Sandbox)
#'library(PubGth)
#'library(Demth)
#'cl <- new.Sandbox()
#'name(cl) <- "Sandy"
#'timegrid(cl) <- rep(15,3)
#'pubgth <- new.PubGth()
#'demth <- new.Demth()
#'components(cl) <- list(pubgth,pubgth,demth)
#'adjacency_el(cl) <- rbind(c(0,0,100),c(0,0,100),c(0,0,0))
#'cl<-finalcoordinates(cl)
#'# do not remember the name of the component call compoNames(cl)
#'P_th_minus(cl,"Sandy_Demth1")
setMethod("P_th_minus",signature(object="Root",value="character",steps = "missing"),function(object,value){

  if(!all(is.element(value,compoNames(object)))){
    stop("value has to be a subvector of names of components")
  }

  co<-coord(object);
  n<-length(co);
  te<-length(timegrid(object));

  names<-str_split_fixed(value,"_",2)[,2]
  exp<-"Pth_"
  #at which positions are the coordinates of the consuming components?
  where<-co[grep(exp,co)];
  where<-where[grep(paste(names,"_time", sep="", collapse="|"),where)]
  u <- paste0( "Pth_to_",paste0(names,collapse = "_and_"), "_time", 1:te)

  if(length(where)==0){
    id<-matrix(0,te,n);
    colnames(id)<-co;
    return(id)
  }

  id<-diag(n);
  colnames(id)<-co;
  id<-id[,where];
  #from here it's pretty nasty, but short!!! We sum every te-th line of t(id)
  w<-length(where)/te;
  id<-matrix(id,byrow = T,nrow = w);
  id<-colSums(id);
  id<-matrix(id,byrow = T,ncol=length(co));
  colnames(id)<-co;
  rownames(id)<-u

  return(id)
  # names<-value;
  # exp<-paste(paste0(names,"_P_th_minus"), sep="", collapse="|")
  #at which positions are the components with P_th_minus?
  # where<-grep(exp,co);
  # n<-length(co);
  # te<-length(timegrid(object));
  # if(length(where)==0){
  #   id<-matrix(0,te,n);
  #   colnames(id)<-co;
  #   return(id)
  # }
  #
  # id<-diag(n);
  # colnames(id)<-co;
  # id<-id[,where];
  # #from here it's pretty nasty, but short!!! We sum every te-th line of t(id)
  # w<-length(where)/te;
  # id<-matrix(id,byrow = T,nrow = w);
  # id<-colSums(id);
  # id<-matrix(id,byrow = T,ncol=length(co));
  # colnames(id)<-co;
  #
  # return(id)
})

        #----P_th_minus (value = character)------------------------
        #'P_th_minus
        #'
        #'projection matrix on P_th_minus of components in Root called by their name
        #'
        #'@export
        #'@param object character
        #'@return matrix
        #'@examples
        #'library(Sandbox)
        #'library(PubGth)
        #'library(Demth)
        #'cl <- new.Sandbox()
        #'name(cl) <- "Sandy"
        #'timegrid(cl) <- rep(15,3)
        #'pubgth <- new.PubGth()
        #'demth <- new.Demth()
        #'components(cl) <- list(pubgth,pubgth,demth)
        #'adjacency_el(cl) <- rbind(c(0,0,100),c(0,0,100),c(0,0,0))
        #'cl<-finalcoordinates(cl)
        #'# do not remember the name of the component call compoNames(cl)
        #'P_th_minus(coord(cl),"Sandy_Demth1")
        setMethod("P_th_minus",signature(object="character",value="character",steps = "numeric"),function(object,value,steps){

          names<-str_split_fixed(value,"_",2)[,2]
          if(!any(grepl(names,object))){
            stop(paste0(names," has to occure in object"))
          }

          co<-object;
          n<-length(co);
          te<-steps;


          exp<-"Pth_"
          #at which positions are the coordinates of the consuming components?
          where<-co[grep(exp,co)];
          where<-where[grep(paste(names,"_time", sep="", collapse="|"),where)]
          u <- paste0( "Pth_to_",paste0(names,collapse = "_and_"), "_time", 1:te)

          if(length(where)==0){
            id<-matrix(0,te,n);
            colnames(id)<-co;
            return(id)
          }

          id<-diag(n);
          colnames(id)<-co;
          id<-id[,where];
          #from here it's pretty nasty, but short!!! We sum every te-th line of t(id)
          w<-length(where)/te;
          id<-matrix(id,byrow = T,nrow = w);
          id<-colSums(id);
          id<-matrix(id,byrow = T,ncol=length(co));
          colnames(id)<-co;
          rownames(id)<-u

          return(id)
          # names<-value;
          # exp<-paste(paste0(names,"_P_th_minus"), sep="", collapse="|")
          #at which positions are the components with P_th_minus?
          # where<-grep(exp,co);
          # n<-length(co);
          # te<-length(timegrid(object));
          # if(length(where)==0){
          #   id<-matrix(0,te,n);
          #   colnames(id)<-co;
          #   return(id)
          # }
          #
          # id<-diag(n);
          # colnames(id)<-co;
          # id<-id[,where];
          # #from here it's pretty nasty, but short!!! We sum every te-th line of t(id)
          # w<-length(where)/te;
          # id<-matrix(id,byrow = T,nrow = w);
          # id<-colSums(id);
          # id<-matrix(id,byrow = T,ncol=length(co));
          # colnames(id)<-co;
          #
          # return(id)
        })

#----P_fuel_plus (value = missing)-------------------------
#'P_fuel_plus
#'
#'Returns a matrix depending on variable "P_fuel_plus". If varaible is used in
#'object Root the function returns a matrix with an entrie at any position of
#'the col-name of the variable depending on length of timegrid. If variable is
#'not used in object Root the function returns a zero matrix.
#'
#'@export
#'@param object An Root
#'@return matrix with row-length of timegrid and col-length of coordinates of object Root
#'@examples
#'library(Sandbox)
#'library(PubGfuel)
#'library(Demfuel)
#'cl <- new.Sandbox()
#'name(cl) <- "Sandy"
#'timegrid(cl) <- rep(15,3)
#'pubfuel <- new.PubGfuel()
#'demfuel <- new.Demfuel()
#'components(cl) <- list(pubfuel,pubfuel,demfuel)
#'adjacency_fuel(cl) <- rbind(c(0,1000,100),c(0,0,100),c(0,0,0))
#'cl<-finalcoordinates(cl)
#'P_fuel_plus(components(cl)[[1]])
setMethod("P_fuel_plus", signature(object = "Root", value = "missing",steps = "missing"), function(object) {
  co <- coord(object)

  n <- length(co)

  te <- length(object@timegrid)

  name<-  str_split_fixed(object@name,"_",2)[,2]
  where<- grep(paste0("Pfuel_",name, "_"), co)
  u <- paste0( "Pfuel_from_", name,  "_time", 1:te)


  if (length(where)==0) {
    id <- matrix(0, te, n)

    colnames(id) <- co

    row.names(id) <-u
    return(id)
  }


  id <- diag(n)

  id<-id[,where];
  #from here it's pretty nasty, but short!!! We sum every te-th line of t(id)
  w<-length(where)/te;
  id<-matrix(id,byrow = T,nrow = w);
  id<-colSums(id);
  id<-matrix(id,byrow = T,ncol=n);
  colnames(id)<-co;
  rownames(id)<-u


  # id <- t(id[, where])
  #
  # row.names(id) <- u
  # colnames(id) <- co



  return(id)

})
#----P_fuel_plus (value = numeric)-------------------------
#'P_fuel_plus
#'
#'projection matrix on P_fuel_plus of components in Root called by their number in the list components
#'
#'@export
#'@param object Root
#'@return matrix
#'@examples
#'library(Sandbox)
#'library(PubGfuel)
#'library(Demfuel)
#'cl <- new.Sandbox()
#'name(cl) <- "Sandy"
#'timegrid(cl) <- rep(15,3)
#'pubfuel <- new.PubGfuel()
#'demfuel <- new.Demfuel()
#'components(cl) <- list(pubfuel,pubfuel,demfuel)
#'adjacency_fuel(cl) <- rbind(c(0,1000,100),c(0,0,100),c(0,0,0))
#'cl<-finalcoordinates(cl)
#'P_fuel_plus(cl,1)

setMethod("P_fuel_plus",signature(object="Root",value="numeric",steps = "missing"),function(object,value){

  if(any(value%%1!=0)){
    stop("value has to be integer")
  }

  if(any(value>length(components(object)))){
    stop(paste0("value to big, there is/are no",value[value>lenght(components(object))],"th component(s)"))
  }

  co<-coord(object);
  n<-length(co);
  te<-length(timegrid(object));
  if(length(value)==0){
    if(n==0){
      stop("Coordinates are neccessary!!!")
    }
    id<-matrix(0,te,n);
    colnames(id)<-co;
    return(id)
  }

  names<-str_split_fixed(compoNames(object)[value],"_",2)[,2]
  exp<-paste(paste0("Pfuel_",names, "_"), sep="", collapse="|")

  #at which positions are the components with P_fuel_plus?
  where<-grep(exp,co);
  u <- paste0( "Pfuel_from_", paste0(names,collapse = "_and_"), "_time", 1:te)

  if(length(where)==0){
    id<-matrix(0,te,n);
    colnames(id)<-co;
    rownames(id)<-u;
    return(id)
  }


  id<-diag(n);
  id<-id[,where];
  #from here it's pretty nasty, but short!!! We sum every te-th line of t(id)
  w<-length(where)/te;
  id<-matrix(id,byrow = T,nrow = w);
  id<-colSums(id);
  id<-matrix(id,byrow = T,ncol=length(co));
  colnames(id)<-co;
  rownames(id)<-u

  return(id)
  # names<-compoNames(object);
  # names<-names[value];
  # exp<-paste(paste0(names,"_P_fuel_plus"), sep="", collapse="|")
  # #at which positions are the components with P_fuel_plus?
  # where<-grep(exp,co);
  #
  # if(length(where)==0){
  #   id<-matrix(0,te,n);
  #   colnames(id)<-co;
  #   return(id)
  # }
  #
  #
  # id<-diag(n);
  # colnames(id)<-co;
  # id<-id[,where];
  # #from here it's pretty nasty, but short!!! We sum every te-th line of t(id)
  # w<-length(where)/te;
  # id<-matrix(id,byrow = T,nrow = w);
  # id<-colSums(id);
  # id<-matrix(id,byrow = T,ncol=length(co));
  # colnames(id)<-co;
  #
  # return(id)
})
#----P_fuel_plus (value = character)-----------------------
#'P_fuel_plus
#'
#'projection matrix on P_fuel_plus of components in Root called by their name
#'
#'@export
#'@param object Root
#'@return matrix
#'@examples
#'library(Sandbox)
#'library(PubGfuel)
#'library(Demfuel)
#'cl <- new.Sandbox()
#'name(cl) <- "Sandy"
#'timegrid(cl) <- rep(15,3)
#'pubfuel <- new.PubGfuel()
#'demfuel <- new.Demfuel()
#'components(cl) <- list(pubfuel,pubfuel,demfuel)
#'adjacency_fuel(cl) <- rbind(c(0,1000,100),c(0,0,100),c(0,0,0))
#'cl<-finalcoordinates(cl)
#'#do not remember the name of the component: call compoNames(cl)
#'P_fuel_plus(cl,"Sandy_PubGfuel1")

setMethod("P_fuel_plus",signature(object="Root",value="character",steps = "missing"),function(object,value){

  if(!all(is.element(value,compoNames(object)))){
    stop("value has to be a subvector of names of components")
  }

  co<-coord(object);
  n<-length(co);
  te<-length(timegrid(object));

  names<-str_split_fixed(value,"_",2)[,2]
  exp<-paste(paste0("Pfuel_",names, "_"), sep="", collapse="|")

  #at which positions are the components with P_fuel_plus?
  where<-grep(exp,co);


  u <- paste0( "Pfuel_from_",paste0(names,collapse = "_and_"), "_time", 1:te)
  if(length(where)==0){
    id<-matrix(0,te,n);
    colnames(id)<-co;
    rownames(id)<-u;
    return(id)
  }


  id<-diag(n);

  id<-id[,where];
  #from here it's pretty nasty, but short!!! We sum every te-th line of t(id)
  w<-length(where)/te;
  id<-matrix(id,byrow = T,nrow = w);
  id<-colSums(id);
  id<-matrix(id,byrow = T,ncol=length(co));
  colnames(id)<-co;
  rownames(id)<-u
  return(id)
  # names<-value;
  # exp<-paste(paste0(names,"_P_fuel_plus"), sep="", collapse="|")
  # #at which positions are the components with P_fuel_plus?
  # where<-grep(exp,co);
  # n<-length(co);
  # te<-length(timegrid(object));
  # if(length(where)==0){
  #   id<-matrix(0,te,n);
  #   colnames(id)<-co;
  #   return(id)
  # }
  #
  #
  # id<-diag(n);
  # colnames(id)<-co;
  # id<-id[,where];
  # #from here it's pretty nasty, but short!!! We sum every te-th line of t(id)
  # w<-length(where)/te;
  # id<-matrix(id,byrow = T,nrow = w);
  # id<-colSums(id);
  # id<-matrix(id,byrow = T,ncol=length(co));
  # colnames(id)<-co;
  #
  # return(id)
})


          #----P_fuel_plus (value = character)-----------------------
          #'P_fuel_plus
          #'
          #'projection matrix on P_fuel_plus of components in Root called by their name
          #'
          #'@export
          #'@param object character
          #'@return matrix
          #'@examples
          #'library(Sandbox)
          #'library(PubGfuel)
          #'library(Demfuel)
          #'cl <- new.Sandbox()
          #'name(cl) <- "Sandy"
          #'timegrid(cl) <- rep(15,3)
          #'pubfuel <- new.PubGfuel()
          #'demfuel <- new.Demfuel()
          #'components(cl) <- list(pubfuel,pubfuel,demfuel)
          #'adjacency_fuel(cl) <- rbind(c(0,1000,100),c(0,0,100),c(0,0,0))
          #'cl<-finalcoordinates(cl)
          #'#do not remember the name of the component: call compoNames(cl)
          #'P_fuel_plus(cl,"Sandy_PubGfuel1")

          setMethod("P_fuel_plus",signature(object="character",value="character",steps = "numeric"),function(object,value,steps){

            names<-str_split_fixed(value,"_",2)[,2]
            if(!any(grepl(names,object))){
              stop(paste0(names," has to occure in object"))
            }

            co<-object;
            n<-length(co);
            te<-steps;

            names<-str_split_fixed(value,"_",2)[,2]
            exp<-paste(paste0("Pfuel_",names, "_"), sep="", collapse="|")

            #at which positions are the components with P_fuel_plus?
            where<-grep(exp,co);


            u <- paste0( "Pfuel_from_",paste0(names,collapse = "_and_"), "_time", 1:te)
            if(length(where)==0){
              id<-matrix(0,te,n);
              colnames(id)<-co;
              rownames(id)<-u;
              return(id)
            }


            id<-diag(n);

            id<-id[,where];
            #from here it's pretty nasty, but short!!! We sum every te-th line of t(id)
            w<-length(where)/te;
            id<-matrix(id,byrow = T,nrow = w);
            id<-colSums(id);
            id<-matrix(id,byrow = T,ncol=length(co));
            colnames(id)<-co;
            rownames(id)<-u
            return(id)
            # names<-value;
            # exp<-paste(paste0(names,"_P_fuel_plus"), sep="", collapse="|")
            # #at which positions are the components with P_fuel_plus?
            # where<-grep(exp,co);
            # n<-length(co);
            # te<-length(timegrid(object));
            # if(length(where)==0){
            #   id<-matrix(0,te,n);
            #   colnames(id)<-co;
            #   return(id)
            # }
            #
            #
            # id<-diag(n);
            # colnames(id)<-co;
            # id<-id[,where];
            # #from here it's pretty nasty, but short!!! We sum every te-th line of t(id)
            # w<-length(where)/te;
            # id<-matrix(id,byrow = T,nrow = w);
            # id<-colSums(id);
            # id<-matrix(id,byrow = T,ncol=length(co));
            # colnames(id)<-co;
            #
            # return(id)
          })
#----P_fuel_minus (value = missing)------------------------
#'P_fuel_minus
#'
#'Returns a matrix depending on variable "P_fuel_minus". If varaible is used
#'in object Root the function returns a matrix with an entrie at any position
#'of the col-name of the variable depending on length of timegrid. If variable
#'is not used in object Root the function returns a zero matrix.
#'
#'@export
#'@param object An Root
#'@return matrix with row-length of timegrid and col-length of coordinates of object Root
#'@examples
#'library(Sandbox)
#'library(PubGfuel)
#'library(Demfuel)
#'cl <- new.Sandbox()
#'name(cl) <- "Sandy"
#'timegrid(cl) <- rep(15,3)
#'pubfuel <- new.PubGfuel()
#'demfuel <- new.Demfuel()
#'components(cl) <- list(pubfuel,pubfuel,demfuel)
#'adjacency_fuel(cl) <- rbind(c(0,0,100),c(0,0,100),c(0,0,0))
#'cl<-finalcoordinates(cl)
#'P_fuel_minus(components(cl)[[3]])
#'
setMethod("P_fuel_minus", signature(object = "Root", value = "missing",steps = "missing"), function(object) {
  co <- coord(object)

  n <- length(co)

  te <- length(object@timegrid)
  name<-str_split_fixed(object@name,"_",2)[,2]
  exp<-"Pfuel"
  #at which positions are the coordinates of the consuming components?

  where<-co[grep(exp,co)];
  where<-where[grep(paste(name,"_time", sep="", collapse="|"),where)]
  u <- paste0( "Pfuel_to_", name,  "_time", 1:te)


  if (length(where) == 0) {
    id <- matrix(0, te, n)

    colnames(id) <- co

    row.names(id) <- u
    return(id)
  }



  id <- diag(n)
  colnames(id)<-co;

  id<-id[,where];
  #from here it's pretty nasty, but short!!! We sum every te-th line of t(id)
  w<-length(where)/te;
  id<-matrix(id,byrow = T,nrow = w);
  id<-colSums(id);
  id<-matrix(id,byrow = T,ncol=n);
  colnames(id)<-co;
  rownames(id)<-u


  # id <- t(id[, where])
  #
  # row.names(id) <- u
  # colnames(id) <- co


  return(id)

})
#----P_fuel_minus (value = numeric)------------------------
#'P_fuel_minus
#'
#'projection matrix on P_fuel_minus of components in Root called by their number in the list components
#'
#'@export
#'@param object Root
#'@return matrix
#'@examples
#'library(Sandbox)
#'library(PubGfuel)
#'library(Demfuel)
#'cl <- new.Sandbox()
#'name(cl) <- "Sandy"
#'timegrid(cl) <- rep(15,3)
#'pubfuel <- new.PubGfuel()
#'demfuel <- new.Demfuel()
#'components(cl) <- list(pubfuel,pubfuel,demfuel)
#'adjacency_fuel(cl) <- rbind(c(0,0,100),c(0,0,100),c(0,0,0))
#'cl<-finalcoordinates(cl)
#'P_fuel_minus(cl,3)
setMethod("P_fuel_minus",signature(object="Root",value="numeric",steps = "missing"),function(object,value){
  if(any(value%%1!=0)){
    stop("value has to be integer")
  }

  if(any(value>length(components(object)))){
    stop(paste0("value to big, there is/are no ",value[value>lenght(components(object))],"th component(s)"))
  }

  co<-coord(object);
  n<-length(co);
  te<-length(timegrid(object));
  if(length(value)==0){
    if(n==0){
      stop("Coordinates are neccessary!!!")
    }
    id<-matrix(0,te,n);
    colnames(id)<-co;
    return(id)
  }

  names<-str_split_fixed(compoNames(object)[value],"_",2)[,2]
  exp<-"Pfuel_"
  #at which positions are the coordinates of the consuming components?
  where<-co[grep(exp,co)];
  where<-where[grep(paste(names,"_time", sep="", collapse="|"),where)]
  u <- paste0( "Pfuel_to",paste0(names,collapse = "_and_"),  "_time", 1:te)



  if(length(where)==0){
    id<-matrix(0,te,n);
    colnames(id)<-co;
    rownames(id)<-u;
    return(id)
  }

  id<-diag(n);
  colnames(id)<-co;
  id<-id[,where];
  #from here it's pretty nasty, but short!!! We sum every te-th line of t(id)
  w<-length(where)/te;
  id<-matrix(id,byrow = T,nrow = w);
  id<-colSums(id);
  id<-matrix(id,byrow = T,ncol=length(co));
  colnames(id)<-co;
  rownames(id)<-u;

  return(id)
  # names<-compoNames(object);
  # names<-names[value];
  # exp<-paste(paste0(names,"_P_fuel_minus"), sep="", collapse="|")
  # #at which positions are the components with P_fuel_minus?
  # where<-grep(exp,co);
  #
  # if(length(where)==0){
  #   id<-matrix(0,te,n);
  #   colnames(id)<-co;
  #   return(id)
  # }
  #
  # id<-diag(n);
  # colnames(id)<-co;
  # id<-id[,where];
  # #from here it's pretty nasty, but short!!! We sum every te-th line of t(id)
  # w<-length(where)/te;
  # id<-matrix(id,byrow = T,nrow = w);
  # id<-colSums(id);
  # id<-matrix(id,byrow = T,ncol=length(co));
  # colnames(id)<-co;
  #
  # return(id)
})
#----P_fuel_minus (value = character)----------------------
#'P_fuel_minus
#'
#'projection matrix on P_fuel_minus of components in Root called by their name
#'
#'@export
#'@param object Root
#'@return matrix
#'@examples
#'library(Sandbox)
#'library(PubGfuel)
#'library(Demfuel)
#'cl <- new.Sandbox()
#'name(cl) <- "Sandy"
#'timegrid(cl) <- rep(15,3)
#'pubfuel <- new.PubGfuel()
#'demfuel <- new.Demfuel()
#'components(cl) <- list(pubfuel,pubfuel,demfuel)
#'adjacency_fuel(cl) <- rbind(c(0,0,100),c(0,0,100),c(0,0,0))
#'cl<-finalcoordinates(cl)
#' #do not remember the name of the component: call compoNames(cl)
#'P_fuel_minus(cl,"Sandy_Demfuel1")
setMethod("P_fuel_minus",signature(object="Root",value="character",steps = "missing"),function(object,value){

  if(!all(is.element(value,compoNames(object)))){
    stop("value has to be a subvector of names of components")
  }

  co<-coord(object);
  n<-length(co);
  te<-length(timegrid(object));

  names<-str_split_fixed(value,"_",2)[,2];
  exp<-"Pfuel_"
  #at which positions are the coordinates of the consuming components?
  where<-co[grep(exp,co)];
  where<-where[grep(paste(names,"_time", sep="", collapse="|"),where)]
  u <- paste0( "Pfuel_to", paste0(names,collapse = "_and_"), "_time", 1:te)


  if(length(where)==0){
    id<-matrix(0,te,n);
    colnames(id)<-co;
    rownames(id)<-u;
    return(id)
  }

  id<-diag(n);
  colnames(id)<-co;
  id<-id[,where];
  #from here it's pretty nasty, but short!!! We sum every te-th line of t(id)
  w<-length(where)/te;
  id<-matrix(id,byrow = T,nrow = w);
  id<-colSums(id);
  id<-matrix(id,byrow = T,ncol=length(co));
  colnames(id)<-co;
  rownames(id)<-u

  return(id)
  # names<-value;
  # exp<-paste(paste0(names,"_P_fuel_minus"), sep="", collapse="|")
  # #at which positions are the components with P_fuel_minus?
  # where<-grep(exp,co);
  # n<-length(co);
  # te<-length(timegrid(object));
  # if(length(where)==0){
  #   id<-matrix(0,te,n);
  #   colnames(id)<-co;
  #   return(id)
  # }
  #
  # id<-diag(n);
  # colnames(id)<-co;
  # id<-id[,where];
  # #from here it's pretty nasty, but short!!! We sum every te-th line of t(id)
  # w<-length(where)/te;
  # id<-matrix(id,byrow = T,nrow = w);
  # id<-colSums(id);
  # id<-matrix(id,byrow = T,ncol=length(co));
  # colnames(id)<-co;
  #
  # return(id)
})

#----E_el (value = missing)--------------------------------
#'E_el
#'
#'Returns a matrix depending on variable "E_el". If varaible is used in object
#'Root the function returns a matrix with an entrie at any position of the
#'col-name of the variable depending on length of timegrid. If variable is not
#'used in object Root the function returns a zero matrix.
#'
#'@export
#'@param object An Root
#'@return matrix with row-length of timegrid and col-length of coordinates of object Root
#'@examples
#'library(Sandbox)
#'library(Bat)
#'library(PubGel)
#'cl <- new.Sandbox()
#'name(cl) <- "Sandy"
#'timegrid(cl) <- rep(15,3)
#'bat<-new.Bat()
#'pubgel<-new.PubGel()
#'components(cl)<-list(pubgel,bat)
#'adjacency_el(cl)<-rbind(c(100,0),c(0,0))
#'cl<-finalcoordinates(cl)
#'E_el(components(cl)[[2]])
setMethod("E_el", signature(object = "Root", value = "missing",steps = "missing"), function(object) {
  co <- coord(object)

  n <- length(co)

  te <- length(object@timegrid)
  name<-str_split_fixed(name(object),"_",2)[,2]
  exp<-paste(paste0("Eel_",name, "_"), sep="", collapse="|")
  #at which positions are the components with E_el?
  where<-grep(exp,co);
  u <- paste0( "E_el_of_", name,  "_time", 1:te)


  if (length(where)==0) {
    id <- matrix(0, te, n)

    colnames(id) <- co
    rownames(id)<-u

    return(id)
  }



  id <- diag(n)
  id<-id[,where];
  #from here it's pretty nasty, but short!!! We sum every te-th line of t(id)
  w<-length(where)/te;
  id<-matrix(id,byrow = T,nrow = w);
  id<-colSums(id);
  id<-matrix(id,byrow = T,ncol=n);
  colnames(id)<-co;
  rownames(id)<-u


  # id <- t(id[, where])
  #
  # row.names(id) <-u
  # colnames(id) <- co


  return(id)

})
            #----P_fuel_minus (value = character)----------------------
            #'P_fuel_minus
            #'
            #'projection matrix on P_fuel_minus of components in Root called by their name
            #'
            #'@export
            #'@param object character
            #'@return matrix
            #'@examples
            #'library(Sandbox)
            #'library(PubGfuel)
            #'library(Demfuel)
            #'cl <- new.Sandbox()
            #'name(cl) <- "Sandy"
            #'timegrid(cl) <- rep(15,3)
            #'pubfuel <- new.PubGfuel()
            #'demfuel <- new.Demfuel()
            #'components(cl) <- list(pubfuel,pubfuel,demfuel)
            #'adjacency_fuel(cl) <- rbind(c(0,0,100),c(0,0,100),c(0,0,0))
            #'cl<-finalcoordinates(cl)
            #' #do not remember the name of the component: call compoNames(cl)
            #'P_fuel_minus(coord(cl),"Sandy_Demfuel1")
            setMethod("P_fuel_minus",signature(object="character",value="character",steps = "numeric"),function(object,value,steps){
              names<-str_split_fixed(value,"_",2)[,2]
              if(!any(grepl(names,object))){
                stop(paste0(names," has to occure in object"))
              }

              co<-object;
              n<-length(co);
              te<-steps;


              exp<-"Pfuel_"
              #at which positions are the coordinates of the consuming components?
              where<-co[grep(exp,co)];
              where<-where[grep(paste(names,"_time", sep="", collapse="|"),where)]
              u <- paste0( "Pfuel_to", paste0(names,collapse = "_and_"), "_time", 1:te)


              if(length(where)==0){
                id<-matrix(0,te,n);
                colnames(id)<-co;
                rownames(id)<-u;
                return(id)
              }

              id<-diag(n);
              colnames(id)<-co;
              id<-id[,where];
              #from here it's pretty nasty, but short!!! We sum every te-th line of t(id)
              w<-length(where)/te;
              id<-matrix(id,byrow = T,nrow = w);
              id<-colSums(id);
              id<-matrix(id,byrow = T,ncol=length(co));
              colnames(id)<-co;
              rownames(id)<-u

              return(id)
              # names<-value;
              # exp<-paste(paste0(names,"_P_fuel_minus"), sep="", collapse="|")
              # #at which positions are the components with P_fuel_minus?
              # where<-grep(exp,co);
              # n<-length(co);
              # te<-length(timegrid(object));
              # if(length(where)==0){
              #   id<-matrix(0,te,n);
              #   colnames(id)<-co;
              #   return(id)
              # }
              #
              # id<-diag(n);
              # colnames(id)<-co;
              # id<-id[,where];
              # #from here it's pretty nasty, but short!!! We sum every te-th line of t(id)
              # w<-length(where)/te;
              # id<-matrix(id,byrow = T,nrow = w);
              # id<-colSums(id);
              # id<-matrix(id,byrow = T,ncol=length(co));
              # colnames(id)<-co;
              #
              # return(id)
            })#----E_el (value = numeric)--------------------------------
#'E_el
#'
#'projection matrix on E_el of components in Root called by their number in the list components
#'
#'@export
#'@param object Root
#'@return matrix
#'@examples
#'library(Sandbox)
#'library(Bat)
#'library(PubGel)
#'cl <- new.Sandbox()
#'name(cl) <- "Sandy"
#'timegrid(cl) <- rep(15,3)
#'bat<-new.Bat()
#'pubgel<-new.PubGel()
#'components(cl)<-list(pubgel,bat)
#'adjacency_el(cl)<-rbind(c(100,0),c(0,0))
#'cl<-finalcoordinates(cl)
#'E_el(cl,2)

setMethod("E_el",signature(object="Root",value="numeric",steps = "missing"),function(object,value){
  if(length(value)==0){
    if(n==0){
      stop("Coordinates are neccessary!!!")
    }
    id<-matrix(0,te,n);
    colnames(id)<-co;
    return(id)
  }
  if(any(value%%1!=0)){
    stop("value has to be integer")
  }

  if(any(value>length(components(object)))){
    stop(paste0("value to big, there is/are no",value[value>lenght(components(object))],"th component(s)"))
  }

  co<-coord(object);
  n<-length(co);
  te<-length(timegrid(object));


  names<-str_split_fixed(compoNames(object)[value],"_",2)[,2]
  exp<-paste(paste0("Eel_",names, "_"), sep="", collapse="|")
  #at which positions are the components with E_el?
  where<-grep(exp,co);
  u <-   paste0( "Eel_of", paste0(names,collapse = "_and_"), "_time", 1:te)


  if(length(where)==0){
    id<-matrix(0,te,n);
    colnames(id)<-co;
    rownames(id)<-u;
    return(id)
  }

  id<-diag(n);
  id<-id[,where];
  #from here it's pretty nasty, but short!!! We sum every te-th line of t(id)
  w<-length(where)/te;
  id<-matrix(id,byrow = T,nrow = w);
  id<-colSums(id);
  id<-matrix(id,byrow = T,ncol=n);
  colnames(id)<-co;
  rownames(id)<-u

  return(id)
})
#----E_el (value = character)------------------------------
#'E_el
#'
#'projection matrix on E_el of components in Root called by their name
#'
#'@export
#'@param object Root
#'@return matrix
#'@examples
#'library(Sandbox)
#'library(Bat)
#'library(PubGel)
#'cl <- new.Sandbox()
#'name(cl) <- "Sandy"
#'timegrid(cl) <- rep(15,3)
#'bat<-new.Bat()
#'pubgel<-new.PubGel()
#'components(cl)<-list(pubgel,bat)
#'adjacency_el(cl)<-rbind(c(100,0),c(0,0))
#'cl<-finalcoordinates(cl)
#'# do not remember the name of the component: call compoNames(cl)
#'E_el(cl, "Sandy_Bat1")

setMethod("E_el",signature(object="Root",value="character",steps = "missing"),function(object,value){

  if(!all(is.element(value,compoNames(object)))){
    stop("value has to be a subvector of names of components")
  }

  co<-coord(object);



  n<-length(co);
  te<-length(timegrid(object));

  names<-str_split_fixed(value,"_",2)[,2]
  exp<-paste(paste0("Eel_",names, "_"), sep="", collapse="|")
  #at which positions are the components with E_el?
  where<-grep(exp,co);
  u <-   paste0( "Eel_of", paste0(names,collapse = "_and_"), "_time", 1:te)

  if(length(where)==0){
    id<-matrix(0,te,n);
    colnames(id)<-co;
    rownames(id)<-u;
    return(id)
  }


  id<-diag(n);

  id<-id[,where];

  #from here it's pretty nasty, but short!!! We sum every te-th line of t(id)
  w<-length(where)/te;
  id<-matrix(id,byrow = T,nrow = w);
  id<-colSums(id);
  id<-matrix(id,byrow = T,ncol=n);
  colnames(id)<-co;
  rownames(id)<-u;

  return(id)
})


              #----E_el (value = character)------------------------------
              #'E_el
              #'
              #'projection matrix on E_el of components in Root called by their name
              #'
              #'@export
              #'@param object character
              #'@return matrix
              #'@examples
              #'library(Sandbox)
              #'library(Bat)
              #'library(PubGel)
              #'cl <- new.Sandbox()
              #'name(cl) <- "Sandy"
              #'timegrid(cl) <- rep(15,3)
              #'bat<-new.Bat()
              #'pubgel<-new.PubGel()
              #'components(cl)<-list(pubgel,bat)
              #'adjacency_el(cl)<-rbind(c(100,0),c(0,0))
              #'cl<-finalcoordinates(cl)
              #'# do not remember the name of the component: call compoNames(cl)
              #'E_el(coord(cl), "Sandy_Bat1")

              setMethod("E_el",signature(object="character",value="character",steps = "numeric"),function(object,value,steps){
                names<-str_split_fixed(value,"_",2)[,2]
                if(!any(grepl(names,object))){
                  stop(paste0(names," has to occure in object"))
                }

                co<-object;



                n<-length(co);
                te<-steps;


                exp<-paste(paste0("Eel_",names, "_"), sep="", collapse="|")
                #at which positions are the components with E_el?
                where<-grep(exp,co);
                u <-   paste0( "Eel_of", paste0(names,collapse = "_and_"), "_time", 1:te)

                if(length(where)==0){
                  id<-matrix(0,te,n);
                  colnames(id)<-co;
                  rownames(id)<-u;
                  return(id)
                }


                id<-diag(n);

                id<-id[,where];

                #from here it's pretty nasty, but short!!! We sum every te-th line of t(id)
                w<-length(where)/te;
                id<-matrix(id,byrow = T,nrow = w);
                id<-colSums(id);
                id<-matrix(id,byrow = T,ncol=n);
                colnames(id)<-co;
                rownames(id)<-u;

                return(id)
              })
#----E_th (value = missing)--------------------------------
#'E_th
#'
#'Returns a matrix depending on variable "E_th". If varaible is used in object
#'Root the function returns a matrix with an entrie at any position of the
#'col-name of the variable depending on length of timegrid. If variable is not
#'used in object Root the function returns a zero matrix.
#'
#'@export
#'@param object An Root
#'@return matrix with row-length of timegrid and col-length of coordinates of object Root
#'@examples
#'library(Sandbox)
#'library(TS)
#'library(PubGth)
#'cl <- new.Sandbox()
#'name(cl) <- "Sandy"
#'timegrid(cl) <- rep(15,3)
#'ts<-new.TS()
#'pubgth<-new.PubGth()
#'components(cl)<-list(pubgth,ts)
#'adjacency_th(cl)<-rbind(c(100,0),c(0,0))
#'cl<-finalcoordinates(cl)
#'E_th(components(cl)[[2]])

setMethod("E_th", signature(object = "Root", value = "missing",steps = "missing"), function(object) {
  co <- coord(object)

  n <- length(co)

  te <- length(object@timegrid)
  name<-str_split_fixed(name(object),"_",2)[,2]
  exp<-paste(paste0("Eth_",name, "_"), sep="", collapse="|")
  #at which positions are the components with E_el?
  where<-grep(exp,co);
  u <- paste0( "E_th_of_", name,  "_time", 1:te)


  if (length(where) == 0 ) {
    id <- matrix(0, te, n)

    colnames(id) <- co
    rownames(id) <- u

    return(id)
  }


  id <- diag(n)

  id<-id[,where];
  #from here it's pretty nasty, but short!!! We sum every te-th line of t(id)
  w<-length(where)/te;
  id<-matrix(id,byrow = T,nrow = w);
  id<-colSums(id);
  id<-matrix(id,byrow = T,ncol=n);
  colnames(id)<-co;
  rownames(id)<-u

  # id <- t(id[, where])
  #
  # row.names(id) <- u
  # colnames(id) <- co
  #

  return(id)

})
#----E_th (value = numeric)--------------------------------
#'E_th
#'
#'projection matrix on E_th of components in Root called by their number in the list components
#'
#'@export
#'@param object Root
#'@return matrix
#'@examples
#'library(Sandbox)
#'library(TS)
#'library(PubGth)
#'cl <- new.Sandbox()
#'name(cl) <- "Sandy"
#'timegrid(cl) <- rep(15,3)
#'ts<-new.TS()
#'pubgth<-new.PubGth()
#'components(cl)<-list(pubgth,ts)
#'adjacency_th(cl)<-rbind(c(100,0),c(0,0))
#'cl<-finalcoordinates(cl)
#'E_th(cl,2)


setMethod("E_th",signature(object="Root",value="numeric",steps = "missing"),function(object,value){
  if(length(value)==0){
    if(n==0){
      stop("Coordinates are neccessary!!!")
    }
    id<-matrix(0,te,n);
    colnames(id)<-co;
    return(id)
  }
  if(any(value%%1!=0)){
    stop("value has to be integer")
  }

  if(any(value>length(components(object)))){
    stop(paste0("value to big, there is/are no",value[value>lenght(components(object))],"th component(s)"))
  }

  co<-coord(object);
  n<-length(co);
  te<-length(timegrid(object));


  names<-str_split_fixed(compoNames(object)[value],"_",2)[,2]
  exp<-paste(paste0("Eth_",names, "_"), sep="", collapse="|")
  #at which positions are the components with E_th?
  where<-grep(exp,co);
  u <-   paste0( "E_th_of",paste0(names,collapse = "_and_"),  "_time", 1:te)

  if(length(where)==0){
    id<-matrix(0,te,n);
    colnames(id)<-co;
    rownames(id)<-u
    return(id)
  }

  id<-diag(n);
  colnames(id)<-co;
  id<-id[,where];
  #from here it's pretty nasty, but short!!! We sum every te-th line of t(id)
  w<-length(where)/te;
  id<-matrix(id,byrow = T,nrow = w);
  id<-colSums(id);
  id<-matrix(id,byrow = T,ncol=n);
  colnames(id)<-co;
  rownames(id)<-u

  return(id)
})
#----E_th (value = character)------------------------------
#'E_th
#'
#'projection matrix on E_th of components in Root called by their name
#'
#'@export
#'@param object Root
#'@return matrix
#'@examples
#'library(Sandbox)
#'library(TS)
#'library(PubGth)
#'cl <- new.Sandbox()
#'name(cl) <- "Sandy"
#'timegrid(cl) <- rep(15,3)
#'ts<-new.TS()
#'pubgth<-new.PubGth()
#'components(cl)<-list(pubgth,ts)
#'adjacency_th(cl)<-rbind(c(100,0),c(0,0))
#'cl<-finalcoordinates(cl)
#'# do not remember the name of the component: call compoNames(cl)
#'E_th(cl,2)

setMethod("E_th",signature(object="Root",value="character",steps = "missing"),function(object,value){

  if(!all(is.element(value,compoNames(object)))){
    stop("value has to be a subvector of names of components")
  }

  co<-coord(object);
  n<-length(co);
  te<-length(timegrid(object));

  names<-str_split_fixed(value,"_",2)[,2]
  exp<-paste(paste0("Eth_",names, "_"), sep="", collapse="|")
  #at which positions are the components with E_th?
  where<-grep(exp,co);
  u <-   paste0( "E_th_of", paste0(names,collapse = "_and_"), "_time", 1:te)


  if(length(where)==0){
    id<-matrix(0,te,n);
    colnames(id)<-co;
    rownames(id)<-u
    return(id)
  }

  id<-diag(n);
  id<-id[,where];
  #from here it's pretty nasty, but short!!! We sum every te-th line of t(id)
  w<-length(where)/te;
  id<-matrix(id,byrow = T,nrow = w);
  id<-colSums(id);
  id<-matrix(id,byrow = T,ncol=n);
  colnames(id)<-co;
  rownames(id)<-u

  return(id)
})

              #----E_th (value = character)------------------------------
              #'E_th
              #'
              #'projection matrix on E_th of components in Root called by their name
              #'
              #'@export
              #'@param object character
              #'@return matrix
              #'@examples
              #'library(Sandbox)
              #'library(TS)
              #'library(PubGth)
              #'cl <- new.Sandbox()
              #'name(cl) <- "Sandy"
              #'timegrid(cl) <- rep(15,3)
              #'ts<-new.TS()
              #'pubgth<-new.PubGth()
              #'components(cl)<-list(pubgth,ts)
              #'adjacency_th(cl)<-rbind(c(100,0),c(0,0))
              #'cl<-finalcoordinates(cl)
              #'# do not remember the name of the component: call compoNames(cl)
              #'E_th(coord(cl),2)

              setMethod("E_th",signature(object="character",value="character",steps = "numeric"),function(object,value,steps){
                names<-str_split_fixed(value,"_",2)[,2]
                if(!any(grepl(names,object))){
                  stop(paste0(names," has to occure in object"))
                }

                co<-object;
                n<-length(co);
                te<-steps;


                exp<-paste(paste0("Eth_",names, "_"), sep="", collapse="|")
                #at which positions are the components with E_th?
                where<-grep(exp,co);
                u <-   paste0( "E_th_of", paste0(names,collapse = "_and_"), "_time", 1:te)


                if(length(where)==0){
                  id<-matrix(0,te,n);
                  colnames(id)<-co;
                  rownames(id)<-u
                  return(id)
                }

                id<-diag(n);
                id<-id[,where];
                #from here it's pretty nasty, but short!!! We sum every te-th line of t(id)
                w<-length(where)/te;
                id<-matrix(id,byrow = T,nrow = w);
                id<-colSums(id);
                id<-matrix(id,byrow = T,ncol=n);
                colnames(id)<-co;
                rownames(id)<-u

                return(id)
              })
#----E_fuel (value = missing)------------------------------
#'E_fuel
#'
#'Returns a matrix depending on variable "E_fuel". If varaible is used in
#'object Root the function returns a matrix with an entrie at any position of
#'the col-name of the variable depending on length of timegrid. If variable
#'is not used in object Root the function returns a zero matrix.
#'
#'@export
#'@param object An Root
#'@return matrix with row-length of timegrid and col-length of coordinates of object Root

setMethod("E_fuel", signature(object = "Root", value = "missing",steps = "missing"), function(object) {
  co <- coord(object)

  n <- length(co)

  te <- length(object@timegrid)
  name<-str_split_fixed(name(object),"_",2)[,2]
  exp<-paste(paste0("Efuel_",name, "_"), sep="", collapse="|")
  #at which positions are the components with E_el?
  where<-grep(exp,co);

  u <- paste0( "E_fuel_of_", name,  "_time", 1:te)


  if (length(where) == 0 ) {
    id <- matrix(0, te, n)

    colnames(id) <- co
    rownames(id) <- u
    return(id)
  }

  id <- diag(n)

  id<-id[,where];
  #from here it's pretty nasty, but short!!! We sum every te-th line of t(id)
  w<-length(where)/te;
  id<-matrix(id,byrow = T,nrow = w);
  id<-colSums(id);
  id<-matrix(id,byrow = T,ncol=n);
  colnames(id)<-co;
  rownames(id)<-u

  # id <- t(id[, where])
  #
  # row.names(id) <- u
  # colnames(id) <- co



  return(id)

})

#----E_fuel (value = numeric)------------------------------
#'E_fuel
#'
#'projection matrix on E_fuel of components in Root called by their number in the list components
#'
#'@export
#'@param object Root
#'@return matrix

setMethod("E_fuel",signature(object="Root",value="numeric",steps = "missing"),function(object,value){
  if(length(value)==0){
    if(n==0){
      stop("Coordinates are neccessary!!!")
    }
    id<-matrix(0,te,n);
    colnames(id)<-co;
    return(id)
  }
  if(any(value%%1!=0)){
    stop("value has to be integer")
  }

  if(any(value>length(components(object)))){
    stop(paste0("value to big, there is/are no",value[value>lenght(components(object))],"th component(s)"))
  }

  co<-coord(object);
  n<-length(co);
  te<-length(timegrid(object));


  names<-str_split_fixed(compoNames(object)[value],"_",2)[,2]
  exp<-paste(paste0("Efuel_",names, "_"), sep="", collapse="|")
  #at which positions are the components with E_fuel?
  where<-grep(exp,co);
  u <-   paste0( "E_fuel_of", paste0(names,collapse = "_and_"),  "_time", 1:te)

  if(length(where)==0){
    id<-matrix(0,te,n);
    colnames(id)<-co;
    return(id)
  }

  id<-diag(n);
  colnames(id)<-co;
  id<-id[,where];
  #from here it's pretty nasty, but short!!! We sum every te-th line of t(id)
  w<-length(where)/te;
  id<-matrix(id,byrow = T,nrow = w);
  id<-colSums(id);
  id<-matrix(id,byrow = T,ncol=n);
  colnames(id)<-co;
  rownames(id)<-u

  return(id)
})
#----E_fuel (value = character)----------------------------
#'E_fuel
#'
#'projection matrix on E_fuel of components in Root called by their name
#'
#'@export
#'@param object Root
#'@return matrix

setMethod("E_fuel",signature(object="Root",value="character",steps = "missing"),function(object,value){

  if(!all(is.element(value,compoNames(object)))){
    stop("value has to be a subvector of names of components")
  }

  co<-coord(object);
  n<-length(co);
  te<-length(timegrid(object));

  names<-str_split_fixed(value,"_",2)[,2]
  exp<-paste(paste0("Efuel_",names, "_"), sep="", collapse="|")
  #at which positions are the components with E_fuel?
  where<-grep(exp,co);
  u <-   paste0( "E_fuel_of",paste0(names,collapse = "_and_"),  "_time", 1:te)

  if(length(where)==0){
    id<-matrix(0,te,n);
    colnames(id)<-co;
    rownames(id)<-u
    return(id)
  }

  id<-diag(n);
  id<-id[,where];
  #from here it's pretty nasty, but short!!! We sum every te-th line of t(id)
  w<-length(where)/te;
  id<-matrix(id,byrow = T,nrow = w);
  id<-colSums(id);
  id<-matrix(id,byrow = T,ncol=n);
  colnames(id)<-co;
  rownames(id)<-u

  return(id)
})




#----E_fuel (value = character)----------------------------
#'E_fuel
#'
#'projection matrix on E_fuel of components in Root called by their name
#'
#'@export
#'@param object Root
#'@return matrix

setMethod("E_fuel",signature(object="character",value="character",steps = "numeric"),function(object,value,steps){
names<-str_split_fixed(value,"_",2)[,2]
if(!any(grepl(names,object))){
  stop(paste0(names," has to occure in object"))
}


co<-object;
n<-length(co);
te<-steps;

names<-str_split_fixed(value,"_",2)[,2]
exp<-paste(paste0("Efuel_",names, "_"), sep="", collapse="|")
#at which positions are the components with E_fuel?
where<-grep(exp,co);
u <-   paste0( "E_fuel_of",paste0(names,collapse = "_and_"),  "_time", 1:te)

if(length(where)==0){
  id<-matrix(0,te,n);
  colnames(id)<-co;
  rownames(id)<-u
  return(id)
}

id<-diag(n);
id<-id[,where];
#from here it's pretty nasty, but short!!! We sum every te-th line of t(id)
w<-length(where)/te;
id<-matrix(id,byrow = T,nrow = w);
id<-colSums(id);
id<-matrix(id,byrow = T,ncol=n);
colnames(id)<-co;
rownames(id)<-u

return(id)
})






