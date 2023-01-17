
prep_data_farrighton_flexible <- function(data,
                                          date_selected=365,
                                          p = 10,
                                          b = 4,
                                          freq = 365,
                                          w = 3,
                                          weeks_excluded_n = 23) {
        d <- copy(data)

        if (nrow(d[date < max(date)-freq]) < b * freq) {
          print ("b is too large")
          exit()
        }

        if (is.null(weeks_excluded_n)) {
          weeks_excluded_n <- w
        }

        if (freq == 365){
          # w <- w * 7
          weeks_excluded_n <- weeks_excluded_n * 7
        }

        d[ , period_n:= p]

        date_0 <- d[date == max(date)-(date_selected)+1]$date
        freq_reference_dates <- (1:b)*freq
        reference_dates_0 <- as.Date(seq(as.Date(date_0, origin = "1970-01-01"), length.out = (b + 1), by = "-1 year"))
        reference_dates_to_exclude <- c(date_0, date_0 - 1:weeks_excluded_n)

        d <- d[!date>max(reference_dates_0)+w]
        d <- d[!date<min(reference_dates_0)-w]

        reference_dates_1 <- reference_dates_0 - w
        reference_dates_2 <- reference_dates_0 + w


        for (y in 1:b) {

        reference_dates <- seq.Date(reference_dates_1[y], reference_dates_2[y],1)

        series_periods <- rep(1:(p-1),round((freq - ((2*w)+1))/(p-1)))

        if(!is.na(reference_dates_2[y+1])) {

            if (length(d[date > as.Date(reference_dates_2[y+1]) & date < as.Date(reference_dates_1[y])]$date) == length(series_periods) ) {

                series_periods <- sort(series_periods)
                d[date > as.Date(reference_dates_2[y+1]) & date < as.Date(reference_dates_1[y]), period_n:= series_periods]

            } else if (length(d[date > as.Date(reference_dates_2[y+1]) & date < as.Date(reference_dates_1[y])]$date) > length(series_periods) ) {

                diff <- length(d[date > as.Date(reference_dates_2[y+1]) & date < as.Date(reference_dates_1[y])]$date) - length(series_periods)

                series_periods <- sort(series_periods)

                series_periods <- c(series_periods,tail(series_periods, diff))

                d[date > as.Date(reference_dates_2[y+1]) & date < as.Date(reference_dates_1[y]), period_n:= series_periods]

            } else if (length(d[date > as.Date(reference_dates_2[y+1]) & date < as.Date(reference_dates_1[y])]$date) < length(series_periods) ) {

                diff <- length(d[date > as.Date(reference_dates_2[y+1]) & date < as.Date(reference_dates_1[y])]$date) - length(series_periods)
                series_periods <- sort(series_periods)

                series_periods <- tail(series_periods, diff)

                d[date > as.Date(reference_dates_2[y+1]) & date < as.Date(reference_dates_1[y]), period_n:= series_periods]

            }

  }

}

        d <- d[!date >= min(reference_dates_to_exclude)]

        return(d)
}



#' Farrington Flexible algorithm
#'
#' @description
#' This function is based on farringtonFlexible {surveillance} according to the procedure developed by Farrington et al. (1996) and  Noufaily et al. (2012).
#  For each time point it uses a Poisson GLM with overdispersion. Data is simulated using a poisson/negative binomial model as described in
#' Seasonality is adjusted for by including in the model a p-level factor as described in Noufaily et al. (2012)
#' @param data Training splfmt_rds data object
#' @param p Number of levels in the factor for seasonality adjustment
#' @param b number of years to used for baseline
#' @param freq
#' @param w Window's half-size
#' @param weeks_excluded_n Number of past weeks to remove in the calculation
#' @param remove.highcounts
#' @param is_day
#' @param s
#' @export


farrington_algorithm_create_alerts <- function( data,
                                                method = "farrington_flexible",
                                                p = 10,
                                                b = 5,
                                                freq = 365,
                                                w = 3,
                                                weeks_excluded_n = 23,
                                                offset = F,
                                                reweights = 1,
                                                remove.highcounts = 0,
                                                is_day = T,
                                                s = 2.58 ) {

  d_prediction <- create_train_prediction_data(data,b,is_day)$d_prediction
  d_prediction[,period_n:=p]

  for ( j in freq:1) {

    d_train <- prep_data_farrighton_flexible(baseline_with_seasonal_spike_holiday,
                                             date_selected = j,
                                             p = 10,
                                             b = 5,
                                             freq = 365,
                                             w = 3,
                                             weeks_excluded_n = 23)

    date_prediction <- rev(d_prediction$date)[j]


    # SET REGRESSION FORMULA:
    if (is_day) {

      regformula <- as.formula(ifelse(offset,
                                      "n ~  offset(log(denom_n)) + isoyear + as.factor(period_n) + holiday + factor(wday)",
                                      "n ~  isoyear + as.factor(period_n) + holiday + factor(wday)"))
    } else {

      regformula <- as.formula(ifelse(offset,
                                      "n ~ offset(log(denom_n)) + isoyear + as.factor(period_n) + holiday",
                                      "n ~ isoyear + as.factor(period_n) + holiday"))
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
      d_prediction[date == date_prediction, baseline_n_expected := 0.0]
      d_prediction[date == date_prediction, baseline_n_predinterval_l4 := 0.0]
      d_prediction[date == date_prediction, baseline_n_predinterval_l2 := 0.0]
      d_prediction[date == date_prediction, baseline_n_predinterval_u2 := 5.0]
      d_prediction[date == date_prediction, baseline_n_predinterval_u4 := 10.0]
      d_prediction[date == date_prediction, zscore := 0.0]
      d_prediction[date == date_prediction, status := "normal"]
      d_prediction[date == date_prediction, failed := TRUE]
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
          vals = predict(poisreg$fit, type = "response", se.fit = T, newdata = d_prediction[date == date_prediction]),
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
        d_prediction[date == date_prediction, baseline_n_expected := 0.0]
        d_prediction[date == date_prediction, baseline_n_predinterval_l4 := 0.0]
        d_prediction[date == date_prediction, baseline_n_predinterval_l2 := 0.0]
        d_prediction[date == date_prediction, baseline_n_predinterval_u2 := 5.0]
        d_prediction[date == date_prediction, baseline_n_predinterval_u4 := 10.0]
        d_prediction[date == date_prediction, zscore := 0.0]
        d_prediction[date == date_prediction, status := "normal"]
        d_prediction[date == date_prediction, failed := TRUE]
      } else {
        d_prediction[date == date_prediction, baseline_n_expected := pred$vals$fit]
        d_prediction[date == date_prediction, baseline_n_predinterval_l4 := FarringtonThreshold(pred$vals, phi = dispersion_parameter, z = -4, skewness.transform = "2/3")]
        d_prediction[date == date_prediction, baseline_n_predinterval_l2 := FarringtonThreshold(pred$vals, phi = dispersion_parameter, z = -2, skewness.transform = "2/3")]
        d_prediction[date == date_prediction, baseline_n_predinterval_u2 := FarringtonThreshold(pred$vals, phi = dispersion_parameter, z = 2, skewness.transform = "2/3")]
        d_prediction[date == date_prediction, baseline_n_predinterval_u4 := FarringtonThreshold(pred$vals, phi = dispersion_parameter, z = 4, skewness.transform = "2/3")]
        d_prediction[date == date_prediction, zscore := FarringtonZscore(pred$vals, phi = dispersion_parameter, z = 6, skewness.transform = "2/3", y = n)]

      }
    }

  }
  d_prediction[, status := "normal"]
  d_prediction[n > baseline_n_predinterval_u2, status := "medium"]
  d_prediction[n > baseline_n_predinterval_u4, status := "high"]
  d_prediction[, failed := FALSE]

  return(d_prediction)
}
