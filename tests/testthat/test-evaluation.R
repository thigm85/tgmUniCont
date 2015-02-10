context("error metrics")

test_that("computeNegativeAbsoluteError works", {

  pred <- c(3,4,5)
  target <- c(4,4,4)
  weight <- NULL
  
  expect_equal(object = computeNegativeAbsoluteError(prediction = pred, 
                                                     target = target, 
                                                     weight = weight),
               c(-1, 0, -1))
  
  pred <- c(-3,4,5)
  target <- c(4,4,4)
  weight <- c(2,1,1)
  
  expect_equal(object = computeNegativeAbsoluteError(prediction = pred, 
                                                     target = target, 
                                                     weight = weight),
               c(-14, 0, -1))
  
  pred <- c(-3,4,5)
  target <- c(4,4,4)
  weight <- c(0,1,1)
  
  expect_equal(object = computeNegativeAbsoluteError(prediction = pred, 
                                                     target = target, 
                                                     weight = weight),
               c(0, 0, -1))

})

test_that("computeUniContScore works", {

  pred <- c(-3,4,5)
  target <- c(4,4,4)
  weight <- c(2,1,1)
  type <- "negative_square_error"
  
  expect_equal(computeUniContScore(prediction = pred, 
                                   target = target, 
                                   type = type, 
                                   weight = weight),
               c(-98, 0, -1))
  
  pred <- c(-3,4,5)
  target <- c(4,4,4)
  weight <- c(2,1,1)
  type <- "negative_absolute_error"
  
  expect_equal(computeUniContScore(prediction = pred, 
                                   target = target, 
                                   type = type, 
                                   weight = weight),
               c(-14, 0, -1))
  
})