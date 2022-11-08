
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


# for ( i in freq:1) {
#
#    test <-prep_data_farrighton_flexible(baseline_with_seasonal_spike_holiday,
#                                         date_selected = i,
#                                         p = 10,
#                                         b = 4,
#                                         freq = 365,
#                                         w = 3,
#                                         weeks_excluded_n = 23)
# }
