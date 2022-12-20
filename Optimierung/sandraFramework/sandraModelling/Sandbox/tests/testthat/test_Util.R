context("Utils to build  schedule and flexa")

library(Sandbox)


test_that("LM.Cheap gives a nice model", {

  coordinates <-c("Op_CHP1_t1_","Op_CHP1_t2_",
                  "Op_CHP2_t1_","Op_CHP2_t2_",
                  "a");
  which_time <-c(F,T);
  schedule <- data.frame(RB_CHP1 = c(10,20),RB_CHP2 = c(7,6));


  model <- LM.Cheap(coordinates,which_time,schedule);
  expect_is(model,"LM");
  expect_equal( sum(abs(model@matrix - cbind(c(0,0),c(1,0),c(0,0),c(0,1),c(0,0)))), 0 );
  expect_equal(model@direction,c("=","="));
  expect_equal(model@vector, c(1,1));
  expect_equal(model@binary,c(T,F,T,F,T))

})

test_that("LM.FlexaSchedule gives a nice model", {

  coordinates <-c("Flex_positive_t2", "Pel_CHP1_t1_","Pel_CHP1_t2_",
                  "Pel_CHP2_t1_","Pel_CHP2_t2_",
                  "a");

  which_time <-c(F,T);
  schedule <- data.frame(RB_CHP1 = c(10,20),RB_CHP2 = c(7,6));
  direction <- "positive"

  model <- LM.FlexaSchedule(coordinates,schedule,which_time,direction);
  expect_error(LM.FlexaSchedule(c("Pel_CHP1_t1_","Pel_CHP1_t2_",
                                  "Pel_CHP2_t1_","Pel_CHP2_t2_",
                                  "a"),schedule,which_time,direction),
               "No Flex coordinates!")
  expect_is(model,"LM");
  expect_equal( sum(abs(model@matrix - t(c(-1,0,1,0,1,0)))), 0 );
  expect_equal(model@direction,c("="));
  expect_equal(abs(as.numeric(model@vector) - 26) == 0,T);

  direction <- "negative"

  model <- LM.FlexaSchedule(coordinates,schedule,which_time,direction);
  expect_is(model,"LM");
  expect_equal( sum(abs(model@matrix - t(c(1,0,1,0,1,0)))), 0 );
  expect_equal(model@direction,c("="));
  expect_equal(abs(as.numeric(model@vector) - 26) == 0,T);

})


test_that("LM.ScheduleFix gives a nice model", {

  coordinates <-c("Pel_CHP1_t1_","Pel_CHP1_t2_",
                  "Pel_CHP2_t1_","Pel_CHP2_t2_",
                  "a");

  which_time <-c(F,T);
  schedule <- data.frame(RB_CHP1 = c(10,20),RB_CHP2 = c(7,6));
  nam <- NULL

  model <- LM.ScheduleFix(coordinates,which_time,schedule);

  expect_is(model,"LM");
  expect_equal( sum(abs(model@matrix - t(c(0,1,0,1,0)))), 0 );
  expect_equal(model@direction,c("="));
  expect_equal(abs(as.numeric(model@vector) - 26) == 0,T);

  nam <- colnames(schedule);
  schedule<-rowSums(schedule);

  model <- LM.ScheduleFix(coordinates,which_time,schedule,nam);

  expect_is(model,"LM");
  expect_equal( sum(abs(model@matrix - t(c(0,1,0,1,0)))), 0 );
  expect_equal(model@direction,c("="));
  expect_equal(abs(as.numeric(model@vector) - 26) == 0,T);


})

test_that("LM.FlexaDom gives a nice model", {

  coordinates <- c("Flex_positive_t2","Flex_bin_t2")
  which_time <- c(F,T)
  flexMax <- 10
  model <- LM.FlexaDom(coordinates,which_time ,flexMax);

  expect_error(LM.FlexaDom("Flex_positive_t2",
                           which_time ,flexMax),"No Flex binary coordinates")
  expect_error(LM.FlexaDom("Flex_bin_t2",
                           which_time ,flexMax),"No Flex coordinates")
  expect_is(model,"LM");
  expect_equal( sum(abs(model@matrix - t(c(1,-10)))), 0 );
  expect_equal(model@direction,c("<="));
  expect_equal(abs(as.numeric(model@vector) - 0) == 0,T);

})

test_that("LM.FlexaMon gives a nice model", {

  coordinates <- c("Flex_positive_t1","Flex_positive_t2")
  which_time <- c(F,T,T)
  flexMax <- 10
  model <- LM.FlexaMon(coordinates,which_time );


  expect_error(LM.FlexaMon("Flex_bin_t2",
                           which_time ),"No Flex coordinates")
  expect_is(model,"LM");
  expect_equal( sum(abs(model@matrix - t(c(-1,1)))), 0 );
  expect_equal(model@direction,c("<="));
  expect_equal(abs(as.numeric(model@vector) - 0) == 0,T);

  which_time <- c(F,T)
  model <- LM.FlexaMon(coordinates,which_time );
  expect_equal( dim(model@matrix)[1] , 0 );


})

test_that("LM.FlexaMonBin gives a nice model", {

  coordinates <- c("Flex_bin_t1","Flex_bin_t2")
  which_time <- c(F,T,T)
  flexMax <- 10
  model <- LM.FlexaMonBin(coordinates,which_time );


  expect_error(LM.FlexaMonBin("Flex_positive_t2",
                           which_time ),"No Flex binary coordinates")
  expect_is(model,"LM");
  expect_equal( sum(abs(model@matrix - t(c(-1,1)))), 0 );
  expect_equal(model@direction,c("<="));
  expect_equal(abs(as.numeric(model@vector) - 0) == 0,T);

  which_time <- c(F,T)
  model <- LM.FlexaMonBin(coordinates,which_time );
  expect_equal( dim(model@matrix)[1] , 0 );


})

test_that("LM.FlexaConst gives a nice model", {

  coordinates <- c("Flex_positive_t2","Flex_positive_t3",
                   "Flex_bin_t2","Flex_bin_t3")
  which_time <- c(F,T,T)
  flexMax <- 10
  model <- LM.FlexaConst(coordinates,which_time ,flexMax);

  expect_error(LM.FlexaConst(c("Flex_positive_t2","Flex_positive_t2"),
                           which_time ,flexMax),"No Flex binary coordinates")
  expect_error(LM.FlexaConst(c("Flex_bin_t2","Flex_bin_t3"),
                           which_time ,flexMax),"No Flex coordinates")
  expect_is(model,"LM");
  expect_equal( sum(abs(model@matrix - t(c(1,-1,0,10)))), 0 );
  expect_equal(model@direction,c("<="));
  expect_equal(abs(as.numeric(model@vector) - 10) == 0,T);

  which_time <- c(F,T)
  model <- LM.FlexaConst(coordinates,which_time ,flexMax);
  expect_equal( dim(model@matrix)[1] , 0 );
})


test_that("LM.Shave gives a nice model", {

  coordinates <-c("P_max-el_CHP1", "P_max-el_CHP2", "Pel_CHP1_t1_","Pel_CHP1_t2_",
                  "Pel_CHP2_t1_","Pel_CHP2_t2_",
                  "a");

  which_time <-c(F,T);
  nam<-c("RB_CHP1","RB_CHP2")


  model <- LM.ShaveDom(coordinates,which_time,nam);

  expect_is(model,"LM");
  expect_equal( sum(abs(model@matrix -
                          rbind(t(c(-1,0,0,1,0,0,0)),
                                t(c(0,-1,0,0,0,1,0))))), 0 );
  expect_equal(model@direction,c("<=","<="));
  expect_equal(sum(abs(as.numeric(model@vector) - c(0,0))) == 0,T);



})
