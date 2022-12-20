###################################################
#Various methods for Sandbox
##################################################
# auto_adjacency
# coordComp
# compoNames
# finalcoordinates
# lineloss
# removeComp
# P_el
# P_el_from
# P_el_to
# P_th
# P_th_from
# P_th_to
# P_fuel
# P_fuel_from
# P_fuel_to
#------------
#---auto_adjacency------
#' auto_adjacency
#'
#' Set the adjacency of el,fuel and th based on the variables of the components
#'
#'@importFrom Root auto_adjacency
#'@export
#'@param object Sandbox
#'@return object with adjacency_el, adjacency_fuel and adjacency_th
setMethod("auto_adjacency", "Sandbox", function(object) {

  coordi <- character()

  #if variables, name, timegrid of the Sandbox available the coordinates of the variables are generated
  #get compoNames
  co<-compoNames(object)
  k<-length(components(object))
  #remove "global name"/prefix of name to get only the unique name of the components, e.g. "Sandbox_CHP1" -> "CHP1"
  comp<-str_split_fixed(co,"_",2)[,2]

  # possible variables of the components
  variables <- c(
    "P_el_plus",
    "P_el_minus",
    "P_th_plus",
    "P_th_minus",
    "P_fuel_plus",
    "P_fuel_minus",
    "E_el",
    "E_th",
    "E_fuel",
    "Op"
  )
  #number of variables
  v<-length(variables)

  # create table with 1 as entry: which component (columns) has which variable (rows)
  tmp<-data.frame(matrix(0,nrow=v,ncol=k))

  #tmp<-data.frame(variable=variables,tmp)
  rownames(tmp)<-variables
  colnames(tmp)<-comp

  #get variables of the components
  for(i in seq_len(k)){
    if(length(variables(components(object)[[i]]))>0){
      tmp[match(variables(components(object)[[i]]),rownames(tmp)),i]<-1
    }else{stop("Components are missing")}
  }
  # subset (sub table) containing only the connecting variables of two componets (P_el_plus,P_el_minus,P_fuel_plus,P_fuel_minus,P_th_plus,P_th_minus)
  tmp1<-tmp[grep("P_",rownames(tmp)),]


  if(dim(adjacency_el(object))[1]==0){
    #get P_el_plus and P_el_minus
    tmpel<-tmp1[grepl("_el",rownames(tmp1)),]
    if(all(tmpel[1,]==0)&any(tmpel[2,]!=0)|any(tmpel[1,]!=0)&all(tmpel[2,]==0) ){
      stop("There is a electrical consumer but no producer or the other way around, i.e. no components has the variable P_el_minus, but at least one component hast the variable P_el_plus or no components has the variable P_el_plus, but at least one component hast the variable P_el_minus " )
    }else{
      #create adjacency_el with unlimited connections (-1)
      adjael<-as.numeric(tmpel[1,])%*%t(as.numeric(tmpel[2,]))*(-1)

      # diagel<-diag(adjael)

      if(sum(adjael)==0 & any(diag(adjael)!=0)){
        stop("There is only one compenent that consumes and produces by itself. If there is a electrical consumer at least one electrical producer is nesseccary and the way around")
      }else{
        #remove self-connections
        diag(adjael)<-0
        rownames(adjael)<-comp
        colnames(adjael)<-comp
        adjacency_el(object)<-adjael
      }}
  }else{print("adjacency_el already exists")}

  if(dim(adjacency_fuel(object))[1]==0){
    #get P_fuel_plus and P_fuel_minus
    tmpfuel<-tmp1[grepl("_fuel",rownames(tmp1)),]
    if(all(tmpfuel[1,]==0)&any(tmpfuel[2,]!=0)|any(tmpfuel[1,]!=0)&all(tmpfuel[2,]==0) ){
      stop("There is a fuel consumer but no producer or the other way around, i.e. no components has the variable P_fuel_minus, but at least one component hast the variable P_fuel_plus or no components has the variable P_fuel_plus, but at least one component hast the variable P_fuel_minus " )
    }else{
      #create adjacency_fuel with unlimited connections (-1)
      adjafuel<-as.numeric(tmpfuel[1,])%*%t(as.numeric(tmpfuel[2,]))*(-1)


      if(sum(adjafuel)==0 & any( diag(adjafuel)!=0)){
        stop("There is only one compenent that consumes and produces by itself. If there is a fuel consumer at least one fuel producer is nesseccary and the way around")
      }else{
        #remove self-connetions
        diag(adjafuel)<-0
        rownames(adjafuel)<-comp
        colnames(adjafuel)<-comp
        adjacency_fuel(object)<-adjafuel
      }
    }
  }else{print("adjacency_fuel already exists")}

  if(dim(adjacency_th(object))[1]==0){
    #get P_th_plus and P_th_minus
    tmpth<-tmp1[grepl("_th",rownames(tmp1)),]
    if(all(tmpth[1,]==0)&any(tmpth[2,]!=0)|any(tmpth[1,]!=0)&all(tmpth[2,]==0) ){
      stop("There is a thermal consumer but no producer or the other way around, i.e. no components has the variable P_th_minus, but at least one component hast the variable P_th_plus or no components has the variable P_th_plus, but at least one component hast the variable P_th_minus " )

    }else{
      #create adjacency_th with unlimited connections (-1)
      adjath<-as.numeric(tmpth[1,])%*%t(as.numeric(tmpth[2,]))*(-1)


      if(sum(adjath)==0 & any( diag(adjath)!=0)){
        stop("There is only one compenent that consumes and produces by itself. If there is a thermal consumer at least one producer is nesseccary and the way around")
      }else{
        #remove self-connection
        diag(adjath)<-0
        rownames(adjath)<-comp
        colnames(adjath)<-comp
        adjacency_th(object)<-adjath
      }}
  }else{print("adjacency_th already exists")}
  return(object)
})

#---coordComp--------------------
#'coordComp
#'
#'Get the coordinates of the components
#'
#'@importFrom Root coordComp
#'@export
#'@param object Sandbox
#'@return character. The coordinates of the components.



setMethod("coordComp", "Sandbox", function(object) {
  return(rapply(components(object),coord))
})
# ---compoNames------------------------------------------------------
#'compoNames
#'
#'Get the names of the components of a Sandbox
#'
#'@importFrom Root compoNames
#'@export
#'@param object A Sandbox
#'@return A character
#'@examples
#'a<-new("Sandbox")
#'#reserve a CHP component
#'componentsDefault(a)<-"CHP";
#'#default values for a. Now we actually have a CHP component
#'a<-default(a)
#'compoNames(a)
#'#Now we rename the CHP
#'#first get the CHP
#'x<-components(a)[[1]]
#'name(x)<-"buddy";
#'components(a)<-list(x)
#'#clear the componentsDefault, otherwise default creates a new CHP the next time
#'componentsDefault(a)<-character();
#'compoNames(a)



setMethod("compoNames", signature(object = "Sandbox"), function(object) {

  #call_func_to_LogFile("LogFile.txt","compoNames",object)
  k<-length(object@components);

  if (k > 0) {
    return(rapply(components(object),name))
  }


})

# ---component------------------------------------------------------
#'component
#'
#'Get a component of a Sandbox by calling it with its name
#'@importFrom Root component
#'@export

#'@examples
#'cl<-default(new("Sandbox",componentsDefault="CHP"))
#'compoNames(cl)
#'component(cl,"Net_CHP1")
setMethod("component", signature(object = "Sandbox", value = "character"), function(object, value) {

  if (!is.element(value, compoNames(object))) {
    stop(paste("No component of name", value, "available"))
  }
  object@components[[which(value == compoNames(object))]]
})




# ---compoClass------------------------------------------------------
#'compoClass
#'
#'Get a character vector with all the classes of the components of a Sandbox
#'
#'@importFrom Root compoClass
#'@export
#'@examples
#'cl<-default(new("Sandbox",componentsDefault="CHP"))
#'compoClass(cl)

setMethod("compoClass", signature(object = "Sandbox"), function(object) {


  return(rapply(object@components,function(z){is(z)[1]}))


})

#@Antonella dieser Top down approach l?sst uns keinerlei Freiheit
#variablen frei an objeten zu definieren.
#die Constraints und variablen die lokal an einem Objekt genutzt
#werden duerfen global nicht interessieren, sie werden einfach eingebunden

#---finalcoordinates----
#'finalcoordinates
#'
#'Set the final coordinates
#'
#'@importFrom Root finalcoordinates
#'@examples
#'cl<-new.Sandbox()
#'cl<-finalcoordinates(cl)
#'@export

setMethod("finalcoordinates", signature(object="Sandbox"), function(object){
  #set final coordinates:
  if(length(timegrid(object))==0){
    stop("Timegrid is necessary")


  }else{
    coordi <- character()

    #if variables, name, timegrid of the Sandbox available the coordinates of the variables are generated
    #get compoNames
    co<-compoNames(object)
    k<-length(components(object))
    #remove "global name"/prefix of name to get only the unique name of the components, e.g. "Sandbox_CHP1" -> "CHP1"
    comp<-str_split_fixed(co,"_",2)[,2]

    # possible variables of the components
    variables <- c(
      "P_el_plus",
      "P_el_minus",
      "P_th_plus",
      "P_th_minus",
      "P_fuel_plus",
      "P_fuel_minus",
      "E_el",
      "E_th",
      "E_fuel",
      "Op"
    )
    #number of variables
    v<-length(variables)

    # create table with 1 as entry: which component (columns) has which variable (rows)
    tmp<-data.frame(matrix(0,nrow=v,ncol=k))

    #tmp<-data.frame(variable=variables,tmp)
    rownames(tmp)<-variables
    colnames(tmp)<-comp

    #get variables of the components
    for(i in seq_len(k)){
      if(length(variables(components(object)[[i]]))>0){
        tmp[match(variables(components(object)[[i]]),rownames(tmp)),i]<-1
      }else{stop("Components are missing")}
    }
    # subset (sub table) containing only the connecting variables of two componets (P_el_plus,P_el_minus,P_fuel_plus,P_fuel_minus,P_th_plus,P_th_minus)
    tmp1<-tmp[grep("P_",rownames(tmp)),]
    # subset containg only the "intern" variables of the components (E_th,E_el, E_fuel, Op)
    tmp2<-tmp[!rownames(tmp)%in%rownames(tmp1),]

    #create adjacency_*  (*=el/fuel/th) if not exsists
    if(dim(adjacency_el(object))[1]==0){
      #get P_el_plus and P_el_minus
      tmpel<-tmp1[grepl("_el",rownames(tmp1)),]
      if(all(tmpel[1,]==0)&any(tmpel[2,]!=0)|any(tmpel[1,]!=0)&all(tmpel[2,]==0) ){
        stop("There is a electrical consumer but no producer or the other way around, i.e. no components has the variable P_el_minus, but at least one component hast the variable P_el_plus or no components has the variable P_el_plus, but at least one component hast the variable P_el_minus " )
      }else{
        #create adjacency_el with unlimited connections (-1)
        adjael<-as.numeric(tmpel[1,])%*%t(as.numeric(tmpel[2,]))*(-1)

        # diagel<-diag(adjael)

        if(sum(adjael)==0 & any(diag(adjael)!=0)){
          stop("There is only one compenent that consumes and produces by itself. If there is a electrical consumer at least one electrical producer is nesseccary and the way around")
        }else{
          #remove self-connections
          diag(adjael)<-0
          rownames(adjael)<-comp
          colnames(adjael)<-comp
          adjacency_el(object)<-adjael
        }}
    }

    if(dim(adjacency_fuel(object))[1]==0){
      #get P_fuel_plus and P_fuel_minus
      tmpfuel<-tmp1[grepl("_fuel",rownames(tmp1)),]
      if(all(tmpfuel[1,]==0)&any(tmpfuel[2,]!=0)|any(tmpfuel[1,]!=0)&all(tmpfuel[2,]==0) ){
        stop("There is a fuel consumer but no producer or the other way around, i.e. no components has the variable P_fuel_minus, but at least one component hast the variable P_fuel_plus or no components has the variable P_fuel_plus, but at least one component hast the variable P_fuel_minus " )
      }else{
        #create adjacency_fuel with unlimited connections (-1)
        adjafuel<-as.numeric(tmpfuel[1,])%*%t(as.numeric(tmpfuel[2,]))*(-1)


        if(sum(adjafuel)==0 & any( diag(adjafuel)!=0)){
          stop("There is only one compenent that consumes and produces by itself. If there is a fuel consumer at least one fuel producer is nesseccary and the way around")
        }else{
          #remove self-connetions
          diag(adjafuel)<-0
          rownames(adjafuel)<-comp
          colnames(adjafuel)<-comp
          adjacency_fuel(object)<-adjafuel
        }
      }
    }

    if(dim(adjacency_th(object))[1]==0){
      #get P_th_plus and P_th_minus
      tmpth<-tmp1[grepl("_th",rownames(tmp1)),]
      if(all(tmpth[1,]==0)&any(tmpth[2,]!=0)|any(tmpth[1,]!=0)&all(tmpth[2,]==0) ){
        stop("There is a thermal consumer but no producer or the other way around, i.e. no components has the variable P_th_minus, but at least one component hast the variable P_th_plus or no components has the variable P_th_plus, but at least one component hast the variable P_th_minus " )

      }else{
        #create adjacency_th with unlimited connections (-1)
        adjath<-as.numeric(tmpth[1,])%*%t(as.numeric(tmpth[2,]))*(-1)


        if(sum(adjath)==0 & any( diag(adjath)!=0)){
          stop("There is only one compenent that consumes and produces by itself. If there is a thermal consumer at least one producer is nesseccary and the way around")
        }else{
          #remove self-connection
          diag(adjath)<-0
          rownames(adjath)<-comp
          colnames(adjath)<-comp
          adjacency_th(object)<-adjath
        }}
    }


    #get the position of the connection of two components
    tmpel<-which(adjacency_el(object)!=0,arr.ind = TRUE)
    #create coordinate: "Pel_Producer_Consumer"
    for(i in seq_len(dim(tmpel)[1])){
      coordi<-c(coordi,paste0("Pel_",rownames(adjacency_el(object ))[tmpel[i,1]],"_",rownames(adjacency_el(object ))[tmpel[i,2]]))
    }


    tmpfuel<-which(adjacency_fuel(object)!=0,arr.ind = TRUE)
    #create coordinate: "Pfuel_Producer_Consumer"
    for(i in seq_len(dim(tmpfuel)[1])){
      coordi<-c(coordi,paste0("Pfuel_",rownames(adjacency_fuel(object))[tmpfuel[i,1]],"_",rownames(adjacency_fuel(object ))[tmpfuel[i,2]]))
    }


    #get the position of the connection of two components
    tmpth<-which(adjacency_th(object)!=0,arr.ind = TRUE)
    #create coordinate: "Pth_Producer_Consumer"
    for(i in seq_len(dim(tmpth)[1])){
      coordi<-c(coordi,paste0("Pth_",rownames(adjacency_th(object ))[tmpth[i,1]],"_",rownames(adjacency_th(object ))[tmpth[i,2]]))
    }

    #create the coordinates of the "intern" variables: e.g. "E_th_Component",

    rownames(tmp2)<-sub("_","",rownames(tmp2))
    if(dim(tmp2)[1]>0){
      for(i in seq_len(dim(tmp2)[1])){
        for(j in seq_len(dim(tmp2)[2])){
          if(tmp2[i,j]>0){
            coordi<-c(coordi,paste0(row.names(tmp2)[i],"_",colnames(tmp2)[j]))
          }
        }

      }}

    #Paste timesteps
    te<-length(timegrid(object))
    coordi<- paste0(t(matrix(rep(coordi, te), nrow = length(coordi))), "_time", seq(te),"_")

    #set slot within sandbox
    object@coord<-coordi
    #set coordinates slots within the components
    for(i in seq_len(k)){
      name<-str_split_fixed(name(components(object)[[i]]),"_",2)[,2]

      object@components[[i]]@coord<-coordi[grepl(name,coordi)]

      if(dim(adjacencyEff_el(object))[1]>0){
        if(any(adjacencyEff_el(object)[,i]>0)){
        object@components[[i]]@linelossel<-adjacencyEff_el(object)[,i]}
      }
      if(dim(adjacencyEff_fuel(object))[1]>0){
        if(any(adjacencyEff_fuel(object)[,i]>0)){
        object@components[[i]]@linelossfuel<-adjacencyEff_fuel(object)[,i]}
      }
      if(dim(adjacencyEff_th(object))[1]>0){
        if(any(adjacencyEff_th(object)[,i]>0)){
        object@components[[i]]@linelossth<-adjacencyEff_th(object)[,i]}}

    }



  }
  return(object)
})

#---lineloss----
#'lineloss
#'
#'Set the lineloss within the components, based on the adjacencyEff_* matrix
#'
#'@export
#'@importFrom Root lineloss
#'@param object A Sandbox
#'@return Sandbox: Components containing linelosses

setMethod("lineloss", signature(object = "Sandbox"), function(object) {
  k<-length(components(object))

  for(i in seq_len(k)){
  if(dim(adjacencyEff_el(object))[1]>0){
    if(any(adjacencyEff_el(object)[,i]>0)){
    object@components[[i]]@linelossel<-adjacencyEff_el(object)[,i]}
  }
  if(dim(adjacencyEff_fuel(object))[1]>0){
    if(any(adjacencyEff_fuel(object)[,i]>0)){
    object@components[[i]]@linelossfuel<-adjacencyEff_fuel(object)[,i]}
  }
  if(dim(adjacencyEff_th(object))[1]>0){
    if(any(adjacencyEff_th(object)[,i]>0)){
    object@components[[i]]@linelossth<-adjacencyEff_th(object)[,i]}}

}
    return(object)

})

#---removeComp---------------
#'removeComp
#'
#'Remove components of a Sandbox
#'@importFrom Root removeComp
#'@export

#'@examples
#'############
#'########## noch zu erstellen!!!!
#'###########
#'


setMethod("removeComp", signature(object = "Sandbox"), function(object,value) {
  if(length(value)>0){
    if(value %in% compoNames(object))
    {

      if(length(value)==1){
        object@components[[which(value == compoNames(object))]]<-NULL

      }else{
        for(i in value){
          object@components[[which(i == compoNames(object))]]<-NULL
        }
      }
      #update coordinates
      name(object)<-name(object)
      return(object)
    }else{print(paste0('Component \'', value,'\' is not an element of the Sandbox'))}

  } else{object@components<-list()
  #update coordinates
  name(object)<-name(object)
  #print(paste0('All components of the Sandbox are removed'))
  return(object)}
}
)
#---P_el--------------------
#'P_el
#'
#'coordinate projection matrix for the (i,j) connection.
#'The matrix has 1 as entry at the coordinate of the connection of two components (for each timestep), else where the matrix is zero.
#'
#'@importFrom Root P_el
#'@param object Sandbox Sandbox
#'@param i numeric. Represents the i-th component of \code{object}
#'@param j numeric. Represents the j-th component of \code{object}
#'@return matrix. Coordinate projection matrix.
#'@examples
#'cl <- new.Sandbox()
#'name(cl) <- "Sandy"
#'timegrid(cl) <- rep(15,3)
#'bat <- new.Bat()
#'demel <- new.Demel()
#'components(cl) <- list(bat,demel)
#'adjacency_el(cl) <- rbind(c(0,100),c(0,0))
#'cl<-finalcoordinates(cl)
#'P_el(cl,1,2)
#'
#'
#'
#'@export
setMethod("P_el", signature(object = "Sandbox", i= "numeric", j= "numeric"), function(object,i, j) {

    k<-length(components(object))
    if(k<i | k<j | k==0){
      stop("There are no components, or i or j is bigger than number of components")
    }else{
   te <- length(object@timegrid)

  co <- coord(object)
  comp<-str_split_fixed(compoNames(object),"_",2)[,2]

  v <- grep(paste0("Pel_",comp[i],"_",comp[j]),co)
  u <- paste0("Pel_from_", comp[i], "_to_", comp[j], "_time", 1:te)

  if (length(v)==0) {
    x <- matrix(0, te, length(co))

    colnames(x) <- co

    row.names(x) <- u

    return(x)
  }
  id <- diag(length(co))

  colnames(id) <- co

  x <- t(id[,v])

  colnames(x) <- co

  rownames(x) <- u

  return(x)
    }

})

#---P_el_from----------------------------------------------------------
#'P_el_from
#'
#'Projection matrix for all the electricity coming from a component i
#'The matrix has 1 as entry at the coordinates of the connections of the component i and the connected consuming components  (for each timestep), else where the matrix is zero.
#'
#'@importFrom Root P_el_from
#'@param object Sandbox Sandbox
#'@param i numeric. Represents the i-th component of \code{object}
#'@return matrix. Coordinate projection matrix.
#'@examples
#'cl <- new.Sandbox()
#'name(cl) <- "Sandy"
#'timegrid(cl) <- rep(15,3)
#'bat <- new.Bat()
#'demel <- new.Demel()
#'components(cl) <- list(bat,demel,demel)
#'adjacency_el(cl) <- rbind(c(0,100,100),c(0,0,0),c(0,0,0))
#'cl<-finalcoordinates(cl)
#'P_el_from(cl,1)
#'
#'
#'
#'@export
setMethod("P_el_from", signature(object = "Sandbox", i= "numeric"), function(object,i) {
  k<-length(components(object))
  if(k<i | k==0){
    stop("There are no components, or i  is bigger than number of components")
  }else{
  te <- length(object@timegrid)

  co <- coord(object)

  comp<-str_split_fixed(compoNames(object)[i],"_",2)[,2]
  v <- grep(paste0("Pel_",comp), co)

  u <- paste0("Pel_from_",comp,"_time", 1:te)

  if (length(v) == 0) {
    x <- matrix(0, te, length(co))

    colnames(x) <- co

    row.names(x) <- u

    return(x)
  }
  id <- diag(length(co))

  colnames(id) <- co

  x <- t(id[, v])

  a <- c(t(x))

  w <- object@adjacency_el[i, ]

  w <- length(which(w != 0))

  if (te * w != length(v)) {
    stop("number of production variables wrong!?")
  }
  a <- matrix(a, byrow = T, nrow = w)

  a <- colSums(a)

  x <- matrix(a, byrow = T, ncol = length(co))

  colnames(x) <- co

  row.names(x) <- u

  return(x)}
})

#---P_el_to----------------------------------------------------------
#'P_el_to
#'
#'Projection matrix for all the electricity coming to a component.
#'#'The matrix has 1 as entry at the coordinates of the connections of the component i and the connected producing components  (for each timestep), else where the matrix is zero.
#'
#'@importFrom Root P_el_to
#'@param object Sandbox Sandbox
#'@param i numeric. Represents the i-th component of \code{object}
#'@return matrix. Coordinate projection matrix.
#'@examples
#'cl <- new.Sandbox()
#'name(cl) <- "Sandy"
#'timegrid(cl) <- rep(15,3)
#'bat <- new.Bat()
#'demel <- new.Demel()
#'components(cl) <- list(bat,bat,demel)
#'adjacency_el(cl) <- rbind(c(0,0,100),c(0,0,100),c(0,0,0))
#'cl<-finalcoordinates(cl)
#'P_el_to(cl,3)
#'
#'
#'
#'@export
setMethod("P_el_to", signature(object = "Sandbox", i= "numeric"), function(object,i) {

  k<-length(components(object))
  if(k<i | k==0){
    stop("There are no components or i is bigger than number of components")
  }else{
  te <- length(object@timegrid)

  co <- coord(object)

  names<-str_split_fixed(compoNames(object)[i],"_",2)[,2]
  exp<-"Pel_"
  #at which positions are the coordinates of the consuming components?
  el<-grep(exp,co);
  compo<-grep(paste0(names,"_time"),co)
  v<- which( el%in% compo)


  #at which positions are the coordinates of the consuming components?
  u <- paste0("Pel_to_",names,"_time", 1:te)

  if (length(v) == 0) {
    x <- matrix(0, te, length(co))

    colnames(x) <- co

    row.names(x) <- u

    return(x)
  }
  id <- diag(length(co))

  colnames(id) <- co

  x <- t(id[, v])

  a <- c(t(x))

  w <- object@adjacency_el[, i]

  w <- length(which(w != 0))

  if (te * w != length(v)) {
    stop("number of production variables wrong!?")
  }
  a <- matrix(a, byrow = T, nrow = w)

  a <- colSums(a)

  x <- matrix(a, byrow = T, ncol = length(co))

  colnames(x) <- co

  row.names(x) <- u

  return(x)}
})


#---P_th----------------------------------------------------------
#'P_th
#'
#'coordinate projection matrix for the (i,j) connection.
#'The matrix has 1 as entry at the coordinate of the connection of two components (for each timestep), else where the matrix is zero.
#'
#'@importFrom Root P_th
#'@param object Sandbox Sandbox
#'@param i numeric. Represents the i-th component of \code{object}
#'@param j numeric. Represents the j-th component of \code{object}
#'@return matrix. Coordinate projection matrix.
#'@examples
#'cl <- new.Sandbox()
#'name(cl) <- "Sandy"
#'timegrid(cl) <- rep(15,3)
#'pubth <- new.PubGth()
#'demth <- new.Demth()
#'components(cl) <- list(pubth,demth)
#'adjacency_th(cl) <- rbind(c(0,100),c(0,0))
#'cl<-finalcoordinates(cl)
#'P_th(cl,1,2)
#'
#'
#'
#'@export
setMethod("P_th", signature(object = "Sandbox", i= "numeric", j= "numeric"), function(object,i, j) {

  k<-length(components(object))
  if(k<i | k<j | k==0){
    stop("There are no components or i or j is bigger than number of components")
  }else{

  te <- length(object@timegrid)

  co <- coord(object)

  comp<-str_split_fixed(compoNames(object),"_",2)[,2]

  v <- grep(paste0("Pth_",comp[i],"_",comp[j]),co)
  u <- paste0("Pth_from_", comp[i], "_to_", comp[j], "_time", 1:te)



  if (length(v)==0) {
    x <- matrix(0, te, length(co))

    colnames(x) <- co

    row.names(x) <- u

    return(x)
  }
  id <- diag(length(co))

  colnames(id) <- co

  x <- t(id[,v])

  colnames(x) <- co

  row.names(x) <- u

  return(x)}

})


#---P_th_from----------------------------------------------------------
#'P_th_from
#'
#'Projection matrix for all the thermal coming from a component.
#'The matrix has 1 as entry at the coordinates of the connections of the component i and the connected consuming components  (for each timestep), else where the matrix is zero.
#'@importFrom Root P_th_from
#'@param object Sandbox Sandbox
#'@param i numeric. Represents the i-th component of \code{object}
#'@return matrix. Coordinate projection matrix.
#'@examples
#'cl <- new.Sandbox()
#'name(cl) <- "Sandy"
#'timegrid(cl) <- rep(15,3)
#'pubth <- new.PubGth()
#'demth <- new.Demth()
#'components(cl) <- list(pubth,pubth,demth)
#'adjacency_th(cl) <- rbind(c(0,1000,100),c(0,0,100),c(0,0,0))
#'cl<-finalcoordinates(cl)
#'P_th_from(cl,1)
#'
#'
#'
#'@export
setMethod("P_th_from", signature(object = "Sandbox", i= "numeric"), function(object,i) {
  k<-length(components(object))
  if(k<i  | k==0){
    stop("There are no components or i  is bigger than number of components")
  }else{

  te <- length(object@timegrid)

  co <- coord(object)

  comp<-str_split_fixed(compoNames(object)[i],"_",2)[,2]
  v <- grep(paste0("Pth_",comp), co)

  u <- paste0("Pth_from_",comp,"_time", 1:te)

  if (length(v) == 0) {
    x <- matrix(0, te, length(co))

    colnames(x) <- co

    row.names(x) <- u

    return(x)
  }
  id <- diag(length(co))

  colnames(id) <- co

  x <- t(id[, v])

  a <- c(t(x))

  w <- object@adjacency_th[i, ]

  w <- length(which(w != 0))

  if (te * w != length(v)) {
    stop("number of production variables wrong!?")
  }
  a <- matrix(a, byrow = T, nrow = w)

  a <- colSums(a)

  x <- matrix(a, byrow = T, ncol = length(co))

  colnames(x) <- co

  row.names(x) <- u

  return(x)}

})

#---P_th_to----------------------------------------------------------
#'P_th_to
#'Projection matrix for all the thermal energy coming to a component.
#'#'The matrix has 1 as entry at the coordinates of the connections of the component i and the connected producing components  (for each timestep), else where the matrix is zero.
#'
#'@importFrom Root P_th_to
#'@param object Sandbox Sandbox
#'@param i numeric. Represents the i-th component of \code{object}
#'@return matrix. Coordinate projection matrix.
#'@examples
#'cl <- new.Sandbox()
#'name(cl) <- "Sandy"
#'timegrid(cl) <- rep(15,3)
#'pubgth <- new.PubGth()
#'demth <- new.Demth()
#'components(cl) <- list(pubgth,pubgth,demth)
#'adjacency_el(cl) <- rbind(c(0,0,100),c(0,0,100),c(0,0,0))
#'cl<-finalcoordinates(cl)
#'P_th_to(cl,3)
#'
#'
#'
#'@export
setMethod("P_th_to", signature(object = "Sandbox", i= "numeric"), function(object,i) {
  k<-length(components(object))
  if(k<i  | k==0){
    stop("There are no components or i is bigger than number of components")
  }else{

  te <- length(object@timegrid)

  co <- coord(object)

  names<-str_split_fixed(compoNames(object)[i],"_",2)[,2]
  exp<-"Pth_"
  #at which positions are the coordinates of the consuming components?
  th<-grep(exp,co);
  compo<-grep(paste0(names,"_time"),co)
  v<- which( th%in% compo)

  u<- paste0("Pth_to_",names, "_time", 1:te)
  if (length(v) == 0) {
    x <- matrix(0, te, length(co))

    colnames(x) <- co

    row.names(x) <- u

    return(x)
  }
  id <- diag(length(co))

  colnames(id) <- co

  x <- t(id[, v])

  a <- c(t(x))

  w <- object@adjacency_th[, i]

  w <- length(which(w != 0))

  if (te * w != length(v)) {
    stop("number of production variables wrong!?")
  }
  a <- matrix(a, byrow = T, nrow = w)

  a <- colSums(a)

  x <- matrix(a, byrow = T, ncol = length(co))

  colnames(x) <- co

  row.names(x) <- u

  return(x)}

})

#---P_fuel----------------------------------------------------------
#'P_fuel
#'
#'projection matrix for all the fuel coming from a component i to j
#'The matrix has 1 as entry at the coordinate of the connection of two components (for each timestep), else where the matrix is zero.
#'
#'@importFrom Root P_fuel
#'@param object Sandbox Sandbox
#'@param i numeric. Represents the i-th component of \code{object}
#'@param j numeric. Represents the j-th component of \code{object}
#'@return matrix. Coordinate projection matrix.
#'@examples
#'cl <- new.Sandbox()
#'name(cl) <- "Sandy"
#'timegrid(cl) <- rep(15,3)
#'pubfuel <- new.PubGfuel()
#'demfuel <- new.Demfuel()
#'components(cl) <- list(pubfuel,demfuel)
#'adjacency_fuel(cl) <- rbind(c(0,100),c(0,0))
#'cl<-finalcoordinates(cl)
#'P_fuel(cl,1,2)
#'
#'
#'
#'@export
setMethod("P_fuel", signature(object = "Sandbox", i= "numeric", j= "numeric"), function(object,i, j) {

  k<-length(components(object))
  if(k<i | k<j | k==0){
    stop("There are no components or i or j is bigger than number of components")
  }else{

  te <- length(object@timegrid)

  co <- coord(object)

  comp<-str_split_fixed(compoNames(object),"_",2)[,2]

  v <- grep(paste0("Pfuel_",comp[i],"_",comp[j]),co)
  u <- paste0("Pfuel_from_", comp[i], "_to_", comp[j], "_time", 1:te)



  if (length(v)==0) {
    x <- matrix(0, te, length(co))

    colnames(x) <- co

    row.names(x) <- u

    return(x)
  }
  id <- diag(length(co))

  colnames(id) <- co

  x <- t(id[,v])

  colnames(x) <- co

  row.names(x) <- u

  return(x)}

})

#---P_fuel_from----------------------------------------------------------
#'P_fuel_from
#'
#'projection matrix for all the fuel coming from a component i
#'
#'The matrix has 1 as entry at the coordinates of the connections of the component i and the connected consuming components  (for each timestep), else where the matrix is zero.
#'
#'@importFrom Root P_fuel_from
#'@param object Sandbox Sandbox
#'@param i numeric. Represents the i-th component of \code{object}
#'@return matrix. Coordinate projection matrix.
#'@examples
#'cl <- new.Sandbox()
#'name(cl) <- "Sandy"
#'timegrid(cl) <- rep(15,3)
#'pubfuel <- new.PubGfuel()
#'demfuel <- new.Demfuel()
#'components(cl) <- list(pubfuel,pubfuel,demfuel)
#'adjacency_fuel(cl) <- rbind(c(0,1000,100),c(0,0,100),c(0,0,0))
#'cl<-finalcoordinates(cl)
#'P_fuel_from(cl,1)
#'
#'
#'
#'@export
setMethod("P_fuel_from", signature(object = "Sandbox", i= "numeric"), function(object,i) {
  k<-length(components(object))
  if(k<i | k==0){
    stop("There are no components or i  is bigger than number of components")
  }else{
  te <- length(object@timegrid)

  co <- coord(object)

  comp<-str_split_fixed(compoNames(object)[i],"_",2)[,2]
  v <- grep(paste0("Pfuel_",comp), co)

  u <- paste0("Pfuel_from_",comp,"_time", 1:te)
  if (length(v) == 0) {
    x <- matrix(0, te, length(co))

    colnames(x) <- co

    row.names(x) <- u

    return(x)
  }
  id <- diag(length(co))

  colnames(id) <- co

  x <- t(id[, v])

  a <- c(t(x))

  w <- object@adjacency_fuel[i, ]

  w <- length(which(w != 0))

  if (te * w != length(v)) {
    stop("number of production variables wrong!?")
  }
  a <- matrix(a, byrow = T, nrow = w)

  a <- colSums(a)

  x <- matrix(a, byrow = T, ncol = length(co))

  colnames(x) <- co

  row.names(x) <- u

  return(x)}

})

#---P_fuel_to----------------------------------------------------------
#'P_fuel_to
#'Projection matrix for all the fuel coming to a component.
#'#'The matrix has 1 as entry at the coordinates of the connections of the component i and the connected producing components  (for each timestep), else where the matrix is zero.
#'
#'@importFrom Root P_fuel_to
#'@param object Sandbox Sandbox
#'@param i numeric. Represents the i-th component of \code{object}
#'@return matrix. Coordinate projection matrix.
#'@examples
#'cl <- new.Sandbox()
#'name(cl) <- "Sandy"
#'timegrid(cl) <- rep(15,3)
#'pubfuel <- new.PubGfuel()
#'demfuel <- new.Demfuel()
#'components(cl) <- list(pubfuel,pubfuel,demfuel)
#'adjacency_fuel(cl) <- rbind(c(0,0,100),c(0,0,100),c(0,0,0))
#'cl<-finalcoordinates(cl)
#'P_th_to(cl,3)
#'
#'
#'
#'@export
setMethod("P_fuel_to", signature(object = "Sandbox", i= "numeric"), function(object,i) {
  k<-length(components(object))
  if(k<i | k==0){
    stop("There are no components or i is bigger than number of components")
  }else{

  te <- length(object@timegrid)

  co <- coord(object)

  names<-str_split_fixed(compoNames(object)[i],"_",2)[,2]
  exp<-"Pfuel_"
  #at which positions are the coordinates of the consuming components?
  fuel<-grep(exp,co);
  compo<-grep(paste0(names,"_time"),co)
  v<- which( fuel%in% compo)

  u <- paste0("Pfuel_to", names, "_time", 1:te)

  if (length(v) == 0) {
    x <- matrix(0, te, length(co))

    colnames(x) <- co

    row.names(x) <- u

    return(x)
  }
  id <- diag(length(co))

  colnames(id) <- co

  x <- t(id[, v])

  a <- c(t(x))

  w <- object@adjacency_fuel[, i]

  w <- length(which(w != 0))

  if (te * w != length(v)) {
    stop("number of production variables wrong!?")
  }
  a <- matrix(a, byrow = T, nrow = w)

  a <- colSums(a)

  x <- matrix(a, byrow = T, ncol = length(co))

  colnames(x) <- co

  row.names(x) <- u

  return(x)}

})

