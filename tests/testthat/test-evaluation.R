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

context("calibration plots")

test_that("computeUniContScore works", {
  
  data <- get(load(file = system.file("extdata", "predicted_weighted_dwell", package="tgmUniCont")))
  
  ## with weights
  expected_calibration_object <- get(load(file = system.file("extdata", "predicted_weighted_dwell_calibration_object", package="tgmUniCont")))
  expected_plot_calibration <- get(load(file = system.file("extdata", "predicted_weighted_dwell_plot_calibration", package="tgmUniCont")))
  expected_plot_smooth_calibration <- get(load(file = system.file("extdata", "predicted_weighted_dwell_plot_smooth_calibration", package="tgmUniCont")))
  
  observed <- checkCalibrationBase(x = data$pred, 
                                   y = data$dwell, 
                                   number_bins = 50, 
                                   x_labs = "Predicted", 
                                   y_labs = "Observed", 
                                   weights = data$click_count)
  
  expect_equal(object = observed$calibration_objects, expected_calibration_object)
  expect_equal(object = observed$plot_calibration_points, expected_plot_calibration)
  expect_equal(object = observed$plot_smooth_calibration, expected_plot_smooth_calibration)
  
  ## without weights
  
  expected_calibration_object <- get(load(file = system.file("extdata", "predicted_weighted_dwell_calibration_object_no_weight", package="tgmUniCont")))
  expected_plot_calibration <- get(load(file = system.file("extdata", "predicted_weighted_dwell_plot_calibration_no_weight", package="tgmUniCont")))
  expected_plot_smooth_calibration <- get(load(file = system.file("extdata", "predicted_weighted_dwell_plot_smooth_calibration_no_weight", package="tgmUniCont")))
  
  observed <- checkCalibrationBase(x = data$pred, 
                                   y = data$dwell, 
                                   number_bins = 50, 
                                   x_labs = "Predicted", 
                                   y_labs = "Observed", 
                                   weights = NULL)

  expect_equal(object = observed$calibration_objects, expected_calibration_object)
  expect_equal(object = observed$plot_calibration_points, expected_plot_calibration)
  expect_equal(object = observed$plot_smooth_calibration, expected_plot_smooth_calibration)
  
})  
  
  