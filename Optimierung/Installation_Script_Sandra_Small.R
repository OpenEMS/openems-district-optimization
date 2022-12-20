#----Installaton of all packages---------------------------
#'Installation of all packages
#'
#'To be able to use the simulationtool correctly all packages have to be
#'installed. This package will install everything in the correct order.
#'Please install packages 'devtools' and 'roxygen2' first!
#'




### Maybe you have to install this packages first

#install.packages("devtools")

### if there is an error installing devotools install this in linux:

#sudo apt-get install libcurl4-gnutls-dev

### and

#sudo apt-get install libssl-dev

### and

#sudo apt-get update
#sudo apt-get install libxml2-dev
#sudo apt-get install libgit2-dev

### and

#sudo apt-get install -y glpk-utils libglpk-dev
#install.packages("Rglpk")

library(devtools)
directory1 <- getwd()

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Installing sandraModelling
# get working directory
directory <- paste0(directory1,"/sandraFramework/sandraModelling/")

# Building all packages
ListPackage <- list.dirs(directory,full.names=FALSE,recursive = FALSE)
# Build "LM" first
path <- paste0(directory,"LM")
Build_LM <- devtools::build(path)
ListPackage <- ListPackage[ListPackage!="LM"]
# Build "Root" second
path <- paste0(directory,"Root")
Build_Root <- devtools::build(path)
ListPackage <- ListPackage[ListPackage!="Root"]
# "Sandbox" has to be build at the end
ListPackage <- ListPackage[ListPackage!="Sandbox"]
# Build all other packages in directory
ListPackage <- as.list(ListPackage)
ListPackage <- paste0(directory,ListPackage)
ListPackage <- as.list(ListPackage)
Build_Packages <- rapply(ListPackage,devtools::build)
# Build "Sandbox" at the end
path <- paste0(directory,"Sandbox")
Build_Sandbox <- devtools::build(path)

#setwd(directory)
# Installing all packages
# LM first of four
PosSlash <- gregexpr("/", Build_LM)
NumSlash <- length(PosSlash[[1]])
PosCut <- PosSlash[[1]][NumSlash]
Build_LM <- substr(Build_LM, PosCut+1, nchar(Build_LM))
path <- paste0(directory,Build_LM)
install.packages(path, repos = NULL)
# Root second of four
PosSlash <- gregexpr("/", Build_Root)
NumSlash <- length(PosSlash[[1]])
PosCut <- PosSlash[[1]][NumSlash]
Build_Root <- substr(Build_Root, PosCut+1, nchar(Build_Root))
path <- paste0(directory,Build_Root)
install.packages(path, repos = NULL)
# Packages third of four
for (n in 1:length(Build_Packages)) {
  PosSlash <- gregexpr("/", Build_Packages[n])
  NumSlash <- length(PosSlash[[1]])
  PosCut <- PosSlash[[1]][NumSlash]
  Build_Package <- substr(Build_Packages[n], PosCut+1, nchar(Build_Packages[n]))
  path <- paste0(directory,Build_Package)
  install.packages(path, repos = NULL)
} 
# Sandbox fourth of four
PosSlash <- gregexpr("/", Build_Sandbox)
NumSlash <- length(PosSlash[[1]])
PosCut <- PosSlash[[1]][NumSlash]
Build_Sandbox <- substr(Build_Sandbox, PosCut+1, nchar(Build_Sandbox))
path <- paste0(directory,Build_Sandbox)
install.packages(path, repos = NULL)


