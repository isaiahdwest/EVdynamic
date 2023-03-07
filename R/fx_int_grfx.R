
#' @title Plot Load Shape Quantiles
#' @param .data Dataframe of interval charging data
#' @param time_col Datetime column in .data
#' @param grp_col Column to group and determine quantiles from
#' @param col Column in .data to aggregate and produce a load profile for,
#' default is \code{kwh}
#' @param .fn Funciton to use to aggregate \code{col}, default is \code{sum}
#' @export
plt_chrg_quant <- function(.data, time_col, grp_col, col = kwh, .fn = sum) {
  tctk <- enquo(time_col)
  varrr <- enquo(col)
  gprrr <- enquo(grp_col)
.data %>%
  dplyr::group_by(!!gprrr) %>%
  dplyr::mutate(study_col = .fn(!!varrr)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(quan = ntile(study_col, 4) %>% as.factor(),
                hour = lubridate::hour(!!tctk)) %>%
  ggplot2::ggplot(
    ggplot2::aes(x = hour, y = !!varrr, color=quan, fill = quan)
  ) +
  ggplot2::stat_summary(geom="ribbon", fun.min = function(x) quantile(x, 0.25), fun.max = function(x) quantile(x, 0.75), alpha = 0.2, show.legend = FALSE) +
  ggplot2::stat_summary(geom="line", fun="mean", alpha = 0.8, show.legend = FALSE)+
  ggplot2::stat_summary(fun = "mean",
               geom = "line",
               ggplot2::aes(group=1),
               size = 1) +
  ggplot2::theme_bw()+
  ggplot2::theme(legend.position = "bottom")
}

#' @title Roll Up kWh
#' @description Aggregate kwh from session or interval, etc. data to a specified
#' level - pass data grouped to the desired level.
#' @param x Grouped data with kwh - note \code{"kwh"} should be a column in the
#' data.
#' @param .fn Aggregating function, default is \code{base::sum()}
#' @param ... Additional arguments to pass to .fn
#' @export
agg_kwh <- function(x, .fn = sum, ...) {
  stopifnot("kwh" %in% names(x))

  x %>%
    summarise(kwh = .fn(kwh,...)) %>%
    ungroup()
}

#' @title Roll Up kW
#' @description Aggregate kW from session or interval, etc. data to a specified
#' level - pass data grouped to the desired level.
#' @param x Grouped data with kwh - note \code{"kW"} should be a column in the
#' data.
#' @param .fn Aggregating function, default is \code{base::mean()}
#' @param ... Additional arguments to pass to .fn
#' @export
agg_kw <- function(x, .fn = mean) {
  stopifnot("kw" %in% names(x))

  x %>%
    summarise(kw = .fn(kwh)) %>%
    ungroup()
}
