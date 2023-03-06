toSeason <- function(.date, num = TRUE) {
  md <- as.numeric(paste0(month(.date), stringr::str_pad(day(.date), width = 2, side = "left",pad = "0")))
  seasons <- if (num) c(1,2,3,4) else c("win", "spr","sum","fall")
  md[dplyr::between(md, 101, 320) | dplyr::between(md, 1221, 1231)] <- seasons[[1]]
  md[dplyr::between(md, 321, 620)] <- seasons[[2]]
  md[dplyr::between(md, 621, 920)] <- seasons[[3]]
  md[dplyr::between(md, 921, 1220)] <- seasons[[4]]
  md

}

hmdetc <- function(x) {
  data.frame(
    start = x,
    season = toSeason(x),
    month = month(x),
    day = day(x),
    wewd = wday(x),
    hour = hour(x),
    minute = round(minute(x)/15)*15
  )
}

#' @title Generate 15 min intervals
#' @rdname generate.interval
#' @description Generate 15 minute intervals for a level 2 charger
#' @param start A datetime object, when the session starts
#' @param duration A double, number of hours the session lasts
#' @param model Model to use to generate intervals
#' @export
generate.interval <- function(start, duration, model) {
  UseMethod("generate.interval", model)
}

#' @rdname generate.interval
generate.interval.xgb.Booster <- function(start, duration, model) {

  interval <- seq.POSIXt(start, start + lubridate::as.period(duration, unit = "hours"), by = 15*60)

  startdf <- data.frame(
    int.start = interval,
    minutes.charging = seq(0, duration * 60, by = 15),
    sess.start = c(1, rep(0, length(interval) - 1))) %>%
    mutate(
      month = month(int.start),
      season = toSeason(int.start),
      day = day(int.start),
      hour = hour(int.start),
      minute = minute(int.start),
      wewd = wday(int.start)
    )

  int.data <- xgboost::xgb.DMatrix(data.matrix(startdf[c("minutes.charging",
                                                "sess.start",
                                                "season",
                                                "month",
                                                "day",
                                                "hour",
                                                "minute",
                                                "wewd")]))

  startdf %>%
    mutate(interval.kwh = as.numeric(predict(model, int.data)),
           interval.kwh = ifelse(interval.kwh < 0, 0, interval.kwh))
}

#' @title Generate a level 2 charging Session for home chargers
#' @description Generate a synthetic level 2 EVSE charging session
#' for a home charger. Resulting session comes in 15 minute intervals.
#' @param start A datetime object, when the session starts
#' @param duration A double, number of hours the session lasts
#' @export

gen_l2_sess <- function(start, duration) {
  generate.interval(start = start, duration = duration, model = l2_model)
}

#' @title Generate EV Owners' Level 2 Interval Data
#' @description Generate Level 2 EVSE home charging data for multiple EV owners.
#' @param start A datetime object, minimum time owners could start charging
#' @param end A datetime object, upper bound for when owners can charge. Charge
#' sessions will occur between \code{start} and \code{end}.
#' @param num_customers Numeric for how many electric vehicle owners to include.
#' @param seed Random seed
#' @export

syn_l2_custs <- function(start,
                         end,
                         num_customers = 10,
                         seed = 123) {
  set.seed(123)
  customers <- list()
  for (i in 1:num_customers) {
    range0 <- seq.POSIXt(start,
                         end,
                         by = "15 min")
    ids <- sort(sample(1:length(range0), 2))
    range <- range0[ids[[1]]:ids[[2]]]

    session_starts <- c()
    id <- 1
    weeks <- range[id:(id+671)]

    while (max(weeks, na.rm = TRUE) < max(range)) {
      week_probs <- dplyr::inner_join(hmdetc(weeks), l2_probs)
      week_sess <- sample(week_probs[,"start"], 4, prob = week_probs[,"prob_sess"])
      session_starts <- c(session_starts, week_sess)
      id <- id+672
      weeks <- range[id:(id+671)]
    }
    session_starts <- sort(lubridate::as_datetime(session_starts))
    session_duration0 <- tidyr::replace_na(as.numeric(floor(difftime(lead(session_starts), session_starts, units = "hours"))), sample(1:4, 1))
    session_duration <- purrr::map_int(session_duration0, ~sample(1:min(c(15,.x)), 1))
    # browser()
    sessions0 <- purrr::map2_dfr(session_starts, session_duration, ~gen_l2_sess(.x,.y))
    sessions1 <- dplyr::mutate(padr::pad(sessions0, "15 min"),
                               interval.kwh = tidyr::replace_na(interval.kwh, 0),
                               hour = hour(int.start))
    customers[[i]] <- sessions1
  }

  dplyr::bind_rows(customers, .id = "customer_id")
}
