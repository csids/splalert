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
#' @param data Training data.table
#' @param reweights Number (greater or equal to 0) of residual reweights adjusting for previous outbreaks (default: 1; 1 reweight)
#' @param remove.highcounts Number between 0 and 1 of fraction of high counts to be removed from prediction, to remove impact of earlier outbreaks (default: 0)
#' @param is_day Significance level for the prediction intervals (default: 0.05)
#' @param s
#' @importFrom glm2 glm2
#' @import stats
#' @import data.table
#' @export


# data <- baseline_with_seasonal_spike_holiday

# quasipoisson_algorithm_create_alerts <- function( data,
#                                                   b = 5,
#                                                   offset = F,
#                                                   reweights = 1,
#                                                   remove.highcounts = 0,
#                                                   is_day = T,
#                                                   s = 2.58 ) {
#
#   d <- create_train_prediction_data(data,b, is_day)
#   d_train <- d$d_train
#   d_prediction <- d$d_prediction
#
#   # SET REGRESSION FORMULA:
#   if (is_day) {
#
#   regformula <- as.formula(ifelse(offset,
#                                   "n ~  offset(log(denom_n)) + isoyear + sin(2 * pi * (isoweek - 1) / 52) + cos(2 * pi * (isoweek - 1) / 52) + holiday + factor(wday)",
#                                   "n ~  isoyear + sin(2 * pi * (isoweek - 1) / 52) + cos(2 * pi * (isoweek - 1) / 52) + holiday + factor(wday)"))
#   } else {
#
#   regformula <- as.formula(ifelse(offset,
#                                   "n ~ offset(log(denom_n)) + isoyear + sin(2 * pi * (isoweek - 1) / 52) + cos(2 * pi * (isoweek - 1) / 52) + holiday",
#                                   "n ~ isoyear + sin(2 * pi * (isoweek - 1) / 52) + cos(2 * pi * (isoweek - 1) / 52) + holiday"))
#   }
#
#   # If chosen, remove upper given percentage of counts from the prediction:
#   if (remove.highcounts > 0) {
#     d_train <- d_train[n < quantile(n, (1 - remove.highcounts)), ]
#   }
#
#   # FIT QUASI-POISSON REGRESSION MODEL ON THE TRAINING SET:
#   normalFunction <- function(regformula, d_train) {
#     fit <- glm2::glm2(regformula, data = d_train, family = quasipoisson, na.action = na.omit)
#     return(list(fit = fit, failed = !fit$converged))
#   }
#   exceptionalFunction <- function(err) {
#     return(list(fit = NaN, failed = TRUE))
#   }
#   poisreg <- tryCatch(normalFunction(regformula, d_train), error = exceptionalFunction, warning = exceptionalFunction)
#   if (poisreg$failed) {
#     d_prediction[, baseline_n_expected := 0.0]
#     d_prediction[, baseline_n_predinterval_l4 := 0.0]
#     d_prediction[, baseline_n_predinterval_l2 := 0.0]
#     d_prediction[, baseline_n_predinterval_u2 := 5.0]
#     d_prediction[, baseline_n_predinterval_u4 := 10.0]
#     d_prediction[, zscore := 0.0]
#     d_prediction[, status := "normal"]
#     d_prediction[, failed := TRUE]
#   } else {
#
#   # REFIT THE REGRESSION USING RESIDUAL WEIGHTS (TO DOWNWEIGHT PREVIOUS OUTBREAKS):
#     d_train[, w_i := 1]
#     for (i in sort(1:reweights)) {
#       dispersion_parameter <- summary(poisreg$fit)$dispersion
#       if (i == 0) {
#         break
#       }
#       try(
#         {
#           anscombe.res <- anscombe.residuals(poisreg$fit, dispersion_parameter)
#           anscombe.res[anscombe.res < s] <- 1
#           d_train[, w_i := anscombe.res^(-2)] # The weight
#           gamma <- nrow(d_train) / sum(d_train$w_i)
#           d_train[, w_i := gamma * w_i] # Makes sum(w_i) = n
#           poisreg$fit <- glm2::glm2(regformula, data = d_train, weights = w_i, family = quasipoisson, na.action = na.omit)
#           dispersion_parameter <- summary(poisreg$fit)$dispersion
#           regression_diagnostics <- extract_diagnostics(poisreg$fit)
#           od <- max(1, sum(poisreg$fit$weights * poisreg$fit$residuals^2) / poisreg$fit$df.r)
#         },
#         TRUE
#       )
#     }
#     # CALCULATE SIGNAL THRESHOLD (prediction interval from Farrington 1996):
#     pred <- tryCatch(
#       list(
#         vals = predict(poisreg$fit, type = "response", se.fit = T, newdata = d_prediction),
#         failed = FALSE
#       ),
#       error = function(err) {
#         list(
#           vals = NULL,
#           failed = TRUE
#         )
#       }
#     )
#     if (!pred$failed) if (max(pred$vals$fit) > 1e10) pred$failed <- TRUE
#
#     if (pred$failed) {
#       d_prediction[, baseline_n_expected := 0.0]
#       d_prediction[, baseline_n_predinterval_l4 := 0.0]
#       d_prediction[, baseline_n_predinterval_l2 := 0.0]
#       d_prediction[, baseline_n_predinterval_u2 := 5.0]
#       d_prediction[, baseline_n_predinterval_u4 := 10.0]
#       d_prediction[, zscore := 0.0]
#       d_prediction[, status := "normal"]
#       d_prediction[, failed := TRUE]
#     } else {
#       d_prediction[, baseline_n_expected := pred$vals$fit]
#       d_prediction[, baseline_n_predinterval_l4 := FarringtonThreshold(pred$vals, phi = dispersion_parameter, z = -4, skewness.transform = "2/3")]
#       d_prediction[, baseline_n_predinterval_l2 := FarringtonThreshold(pred$vals, phi = dispersion_parameter, z = -2, skewness.transform = "2/3")]
#       d_prediction[, baseline_n_predinterval_u2 := FarringtonThreshold(pred$vals, phi = dispersion_parameter, z = 2, skewness.transform = "2/3")]
#       d_prediction[, baseline_n_predinterval_u4 := FarringtonThreshold(pred$vals, phi = dispersion_parameter, z = 4, skewness.transform = "2/3")]
#       d_prediction[, zscore := FarringtonZscore(pred$vals, phi = dispersion_parameter, z = 6, skewness.transform = "2/3", y = n)]
#       d_prediction[, status := "normal"]
#       d_prediction[n > baseline_n_predinterval_u2, status := "medium"]
#       d_prediction[n > baseline_n_predinterval_u4, status := "high"]
#       d_prediction[, failed := FALSE]
#     }
#   }
#
#   return(d_prediction)
# }




# farrington_algorithm_create_alerts <- function( data,
#                                                 method = "farrington_flexible",
#                                                 p = 10,
#                                                 b = 5,
#                                                 freq = 365,
#                                                 w = 3,
#                                                 weeks_excluded_n = 23,
#                                                 offset = F,
#                                                 reweights = 1,
#                                                 remove.highcounts = 0,
#                                                 is_day = T,
#                                                 s = 2.58 ) {
#
#   d_prediction <- create_train_prediction_data(data,b,is_day)$d_prediction
#   d_prediction[,period_n:=p]
#
#   for ( j in freq:1) {
#
#               d_train <- prep_data_farrighton_flexible(baseline_with_seasonal_spike_holiday,
#                                        date_selected = j,
#                                        p = 10,
#                                        b = 5,
#                                        freq = 365,
#                                        w = 3,
#                                        weeks_excluded_n = 23)
#
#               date_prediction <- rev(d_prediction$date)[j]
#
#
#               # SET REGRESSION FORMULA:
#               if (is_day) {
#
#                 regformula <- as.formula(ifelse(offset,
#                                                 "n ~  offset(log(denom_n)) + isoyear + as.factor(period_n) + holiday + factor(wday)",
#                                                 "n ~  isoyear + as.factor(period_n) + holiday + factor(wday)"))
#               } else {
#
#                 regformula <- as.formula(ifelse(offset,
#                                                 "n ~ offset(log(denom_n)) + isoyear + as.factor(period_n) + holiday",
#                                                 "n ~ isoyear + as.factor(period_n) + holiday"))
#               }
#
#               # If chosen, remove upper given percentage of counts from the prediction:
#               if (remove.highcounts > 0) {
#                 d_train <- d_train[n < quantile(n, (1 - remove.highcounts)), ]
#               }
#
#               # FIT QUASI-POISSON REGRESSION MODEL ON THE TRAINING SET:
#               normalFunction <- function(regformula, d_train) {
#                 fit <- glm2::glm2(regformula, data = d_train, family = quasipoisson, na.action = na.omit)
#                 return(list(fit = fit, failed = !fit$converged))
#               }
#               exceptionalFunction <- function(err) {
#                 return(list(fit = NaN, failed = TRUE))
#               }
#               poisreg <- tryCatch(normalFunction(regformula, d_train), error = exceptionalFunction, warning = exceptionalFunction)
#               if (poisreg$failed) {
#                 d_prediction[date == date_prediction, baseline_n_expected := 0.0]
#                 d_prediction[date == date_prediction, baseline_n_predinterval_l4 := 0.0]
#                 d_prediction[date == date_prediction, baseline_n_predinterval_l2 := 0.0]
#                 d_prediction[date == date_prediction, baseline_n_predinterval_u2 := 5.0]
#                 d_prediction[date == date_prediction, baseline_n_predinterval_u4 := 10.0]
#                 d_prediction[date == date_prediction, zscore := 0.0]
#                 d_prediction[date == date_prediction, status := "normal"]
#                 d_prediction[date == date_prediction, failed := TRUE]
#               } else {
#
#                 # REFIT THE REGRESSION USING RESIDUAL WEIGHTS (TO DOWNWEIGHT PREVIOUS OUTBREAKS):
#                 d_train[, w_i := 1]
#                 for (i in sort(1:reweights)) {
#                   dispersion_parameter <- summary(poisreg$fit)$dispersion
#                   if (i == 0) {
#                     break
#                   }
#                   try(
#                     {
#                       anscombe.res <- anscombe.residuals(poisreg$fit, dispersion_parameter)
#                       anscombe.res[anscombe.res < s] <- 1
#                       d_train[, w_i := anscombe.res^(-2)] # The weight
#                       gamma <- nrow(d_train) / sum(d_train$w_i)
#                       d_train[, w_i := gamma * w_i] # Makes sum(w_i) = n
#                       poisreg$fit <- glm2::glm2(regformula, data = d_train, weights = w_i, family = quasipoisson, na.action = na.omit)
#                       dispersion_parameter <- summary(poisreg$fit)$dispersion
#                       regression_diagnostics <- extract_diagnostics(poisreg$fit)
#                       od <- max(1, sum(poisreg$fit$weights * poisreg$fit$residuals^2) / poisreg$fit$df.r)
#                     },
#                     TRUE
#                   )
#                 }
#                 # CALCULATE SIGNAL THRESHOLD (prediction interval from Farrington 1996):
#                 pred <- tryCatch(
#                   list(
#                     vals = predict(poisreg$fit, type = "response", se.fit = T, newdata = d_prediction[date == date_prediction]),
#                     failed = FALSE
#                   ),
#                   error = function(err) {
#                     list(
#                       vals = NULL,
#                       failed = TRUE
#                     )
#                   }
#                 )
#                 if (!pred$failed) if (max(pred$vals$fit) > 1e10) pred$failed <- TRUE
#
#                 if (pred$failed) {
#                   d_prediction[date == date_prediction, baseline_n_expected := 0.0]
#                   d_prediction[date == date_prediction, baseline_n_predinterval_l4 := 0.0]
#                   d_prediction[date == date_prediction, baseline_n_predinterval_l2 := 0.0]
#                   d_prediction[date == date_prediction, baseline_n_predinterval_u2 := 5.0]
#                   d_prediction[date == date_prediction, baseline_n_predinterval_u4 := 10.0]
#                   d_prediction[date == date_prediction, zscore := 0.0]
#                   d_prediction[date == date_prediction, status := "normal"]
#                   d_prediction[date == date_prediction, failed := TRUE]
#                 } else {
#                   d_prediction[date == date_prediction, baseline_n_expected := pred$vals$fit]
#                   d_prediction[date == date_prediction, baseline_n_predinterval_l4 := FarringtonThreshold(pred$vals, phi = dispersion_parameter, z = -4, skewness.transform = "2/3")]
#                   d_prediction[date == date_prediction, baseline_n_predinterval_l2 := FarringtonThreshold(pred$vals, phi = dispersion_parameter, z = -2, skewness.transform = "2/3")]
#                   d_prediction[date == date_prediction, baseline_n_predinterval_u2 := FarringtonThreshold(pred$vals, phi = dispersion_parameter, z = 2, skewness.transform = "2/3")]
#                   d_prediction[date == date_prediction, baseline_n_predinterval_u4 := FarringtonThreshold(pred$vals, phi = dispersion_parameter, z = 4, skewness.transform = "2/3")]
#                   d_prediction[date == date_prediction, zscore := FarringtonZscore(pred$vals, phi = dispersion_parameter, z = 6, skewness.transform = "2/3", y = n)]
#
#                 }
#               }
#
#   }
#   d_prediction[, status := "normal"]
#   d_prediction[n > baseline_n_predinterval_u2, status := "medium"]
#   d_prediction[n > baseline_n_predinterval_u4, status := "high"]
#   d_prediction[, failed := FALSE]
#
#   return(d_prediction)
# }



# test <- farrington_algorithm_create_alerts( data,
#                                     method = "farrington_flexible",
#                                     p = 10,
#                                     b = 4,
#                                     freq = 365,
#                                     w = 3,
#                                     weeks_excluded_n = 23,
#                                     offset = F,
#                                     reweights = 1,
#                                     remove.highcounts = 0,
#                                     is_day = T,
#                                     s = 2.58
#                                     )
#
#
#
# # surveillance::farringtonFlexible()
#
# sts <- data
# sts[,state:=1]
# sts <- with(data,
#                    sts(observed = n, state = state, epoch=date,
#                        start = c(2012, 1), frequency = 365))
#
# control <- list(range=(2558:2922),
#                  noPeriods=10,populationOffset=FALSE,
#                  fitFun="algo.farrington.fitGLM.flexible",
#                  b=4,w=3,weightsThreshold=2.58,
#                  pastWeeksNotIncluded=23*7,
#                  pThresholdTrend=1,trend=TRUE,
#                  thresholdMethod="delta",alpha=0.1,
#                 verbose = FALSE)
# #
# # salm1 <- surveillance::farringtonFlexible(sts,control=control)
# #
# # y.max <- max(upperbound(salm1),observed(salm1),upperbound(salm1),na.rm=TRUE)
# #
# # plot(data, ylim=c(0,y.max), main='S. Newport in Germany', legend.opts=NULL)
# #
# #
# # plot(data[2558:2922]$n, ylim=c(0,y.max), main='S. Newport in Germany', legend.opts=NULL)
# # lines(1:(nrow(salm1)+1)-0.5,
# #       c(upperbound(salm1),upperbound(salm1)[nrow(salm1)]),
# #       type="s",col='tomato4',lwd=2)
# #
# #
# # lines(1:(nrow(salm2)+1)-0.5,
# #       c(upperbound(salm2),upperbound(salm2)[nrow(salm2)]),
# #       type="s",col="blueviolet",lwd=2)
# #
# # legend(0, 10, legend=c('Alarm','Upperbound with old options',
# #                        'Upperbound with new options'),
# #        pch=c(24,NA,NA),lty=c(NA,1,1),
# #        bg="white",lwd=c(2,2,2),col=c('red','tomato4',"blueviolet"))
# #
