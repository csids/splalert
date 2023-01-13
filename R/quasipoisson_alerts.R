create_train_prediction_data <- function(d,
                                         b,
                                         is_day = T){

  if (is_day) {

    date <- unique(d$date)
    prediction_period <- date[(length(date)-365+1):length(date)]
    train_period <- date[(length(date)-365*b):(length(date)-365)]

    d_train <- d[date %in% train_period]
    d_prediction <- d[date %in% prediction_period]


  } else {

    isoyearweek <- unique(d$isoyearweek)
    prediction_period <- isoyearweek[(length(isoyearweek)-52+1):length(isoyearweek)]
    train_period <- isoyearweek[(length(isoyearweek)-52*5):(length(isoyearweek)-(52))]

    d_train <- d[isoyearweek %in% train_period]
    d_prediction <- d[isoyearweek %in% prediction_period]
  }



  return(list(d_train=d_train,d_prediction=d_prediction))
}

#' Quasipoisson Algorithm Create Alerts
#'
#' Description:
#' Applys a surveillance algorithm based on a quasi-poisson regression
#' model to the selected data. The difference from the Farrington algorithm is in how
#' seasonality is accounted for (here it is adjusted for, in Farrington it is removed
#' by design.
#'
#' @param data Training splfmt_rds data object
#' @param reweights Number (greater or equal to 0) of residual reweights adjusting for previous outbreaks (default: 1; 1 reweight)
#' @param remove.highcounts Number between 0 and 1 of fraction of high counts to be removed from prediction, to remove impact of earlier outbreaks (default: 0)
#' @param is_day Significance level for the prediction intervals (default: 0.05)
#' @param s
#' @importFrom glm2 glm2
#' @import stats
#' @import data.table
#' @export


quasipoisson_algorithm_create_alerts <- function( data,
                                                  b = 5,
                                                  offset = F,
                                                  reweights = 1,
                                                  remove.highcounts = 0,
                                                  is_day = T,
                                                  s = 2.58 ) {

  d <- create_train_prediction_data(data,b, is_day)
  d_train <- d$d_train
  d_prediction <- d$d_prediction

  # SET REGRESSION FORMULA:
  if (is_day) {

  regformula <- as.formula(ifelse(offset,
                                  "n ~  offset(log(denom_n)) + isoyear + sin(2 * pi * (isoweek - 1) / 52) + cos(2 * pi * (isoweek - 1) / 52) + holiday + factor(wday)",
                                  "n ~  isoyear + sin(2 * pi * (isoweek - 1) / 52) + cos(2 * pi * (isoweek - 1) / 52) + holiday + factor(wday)"))
  } else {

  regformula <- as.formula(ifelse(offset,
                                  "n ~ offset(log(denom_n)) + isoyear + sin(2 * pi * (isoweek - 1) / 52) + cos(2 * pi * (isoweek - 1) / 52) + holiday",
                                  "n ~ isoyear + sin(2 * pi * (isoweek - 1) / 52) + cos(2 * pi * (isoweek - 1) / 52) + holiday"))
  }

  # If chosen, remove upper given percentage of counts from the prediction:
  if (remove.highcounts > 0) {
    d_train <- d_train[n < quantile(n, (1 - remove.highcounts)), ]
  }

  # FIT QUASI-POISSON REGRESSION MODEL ON THE TRAINING SET:
  normalFunction <- function(regformula, d_train) {
    fit <- glm2::glm2(regformula, data = d_train, family = quasipoisson, na.action = na.omit)
    return(list(fit = fit, failed = !fit$converged))
  }
  exceptionalFunction <- function(err) {
    return(list(fit = NaN, failed = TRUE))
  }
  poisreg <- tryCatch(normalFunction(regformula, d_train), error = exceptionalFunction, warning = exceptionalFunction)
  if (poisreg$failed) {
    d_prediction[, baseline_n_expected := 0.0]
    d_prediction[, baseline_n_predinterval_l4 := 0.0]
    d_prediction[, baseline_n_predinterval_l2 := 0.0]
    d_prediction[, baseline_n_predinterval_u2 := 5.0]
    d_prediction[, baseline_n_predinterval_u4 := 10.0]
    d_prediction[, zscore := 0.0]
    d_prediction[, status := "normal"]
    d_prediction[, failed := TRUE]
  } else {

  # REFIT THE REGRESSION USING RESIDUAL WEIGHTS (TO DOWNWEIGHT PREVIOUS OUTBREAKS):
    d_train[, w_i := 1]
    for (i in sort(1:reweights)) {
      dispersion_parameter <- summary(poisreg$fit)$dispersion
      if (i == 0) {
        break
      }
      try(
        {
          anscombe.res <- anscombe.residuals(poisreg$fit, dispersion_parameter)
          anscombe.res[anscombe.res < s] <- 1
          d_train[, w_i := anscombe.res^(-2)] # The weight
          gamma <- nrow(d_train) / sum(d_train$w_i)
          d_train[, w_i := gamma * w_i] # Makes sum(w_i) = n
          poisreg$fit <- glm2::glm2(regformula, data = d_train, weights = w_i, family = quasipoisson, na.action = na.omit)
          dispersion_parameter <- summary(poisreg$fit)$dispersion
          regression_diagnostics <- extract_diagnostics(poisreg$fit)
          od <- max(1, sum(poisreg$fit$weights * poisreg$fit$residuals^2) / poisreg$fit$df.r)
        },
        TRUE
      )
    }
    # CALCULATE SIGNAL THRESHOLD (prediction interval from Farrington 1996):
    pred <- tryCatch(
      list(
        vals = predict(poisreg$fit, type = "response", se.fit = T, newdata = d_prediction),
        failed = FALSE
      ),
      error = function(err) {
        list(
          vals = NULL,
          failed = TRUE
        )
      }
    )
    if (!pred$failed) if (max(pred$vals$fit) > 1e10) pred$failed <- TRUE

    if (pred$failed) {
      d_prediction[, baseline_n_expected := 0.0]
      d_prediction[, baseline_n_predinterval_l4 := 0.0]
      d_prediction[, baseline_n_predinterval_l2 := 0.0]
      d_prediction[, baseline_n_predinterval_u2 := 5.0]
      d_prediction[, baseline_n_predinterval_u4 := 10.0]
      d_prediction[, zscore := 0.0]
      d_prediction[, status := "normal"]
      d_prediction[, failed := TRUE]
    } else {
      d_prediction[, baseline_n_expected := pred$vals$fit]
      d_prediction[, baseline_n_predinterval_l4 := FarringtonThreshold(pred$vals, phi = dispersion_parameter, z = -4, skewness.transform = "2/3")]
      d_prediction[, baseline_n_predinterval_l2 := FarringtonThreshold(pred$vals, phi = dispersion_parameter, z = -2, skewness.transform = "2/3")]
      d_prediction[, baseline_n_predinterval_u2 := FarringtonThreshold(pred$vals, phi = dispersion_parameter, z = 2, skewness.transform = "2/3")]
      d_prediction[, baseline_n_predinterval_u4 := FarringtonThreshold(pred$vals, phi = dispersion_parameter, z = 4, skewness.transform = "2/3")]
      d_prediction[, zscore := FarringtonZscore(pred$vals, phi = dispersion_parameter, z = 6, skewness.transform = "2/3", y = n)]
      d_prediction[, status := "normal"]
      d_prediction[n > baseline_n_predinterval_u2, status := "medium"]
      d_prediction[n > baseline_n_predinterval_u4, status := "high"]
      d_prediction[, failed := FALSE]
    }
  }

  return(d_prediction)
}







