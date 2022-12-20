context("minRuntime and minDowntime for Ops variables")

library(Sandbox)
library(CHP)
library(Demth)
library(PubGfuel)
library(PubGel)
library(TS)


testthat::test_that("minRuntime works",{
  grid<-rep(60,3);
  forecast<-data.frame(Pth_Demth1 = 0*c(50,50,50),Price_DA = c(-100,0,-100))
  sandbox<-sandbox_chp(grid,forecast,stable=F)
  sandbox<-finalcoordinates(sandbox)

  object<-component(sandbox,"RoitherBerg_CHP1")
  object@minRuntime<-2
  object@minDowntime<-2
  u<-LM.minRuntime(object)


})

testthat::test_that("minDowntime works",{
  grid<-rep(60,3);
  forecast<-data.frame(Pth_Demth1 = 0*c(50,50,50),Price_DA = c(-100,0,-100))
  sandbox<-sandbox_chp(grid,forecast,stable=F)
  sandbox<-finalcoordinates(sandbox)

  object<-component(sandbox,"RoitherBerg_CHP1")
  object@minRuntime<-2
  object@minDowntime<-2
  u<-LM.minDowntime(object)


})





