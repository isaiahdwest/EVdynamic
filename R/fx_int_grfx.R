
#' @title Plot Load Shape Quantiles
#' @param .data Dataframe of interval charging data
#' @param time_col Datetime column in .data
#' @param grp_col Column to group and determine quantiles from
#' @param col Column in .data to aggregate and produce a load profile for,
#' default is \code{kwh}
#' @param .fn Funciton to use to aggregate \code{col}, default is \code{sum}
#' @export
plt_chrg_quant <- function(.data, time_col, grp_col, load_col = kwh, .fn = sum) {
  tctk <- dplyr::enquo(time_col)
  varrr <- dplyr::enquo(load_col)
  gprrr <- dplyr::enquo(grp_col)
.data %>%
  dplyr::group_by(!!gprrr) %>%
  dplyr::mutate(study_col = .fn(!!varrr)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(quan = dplyr::ntile(study_col, 4) %>% as.factor(),
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


#' @title Plot Load
#' @description Plot an aggregated load profile.
#' @param .data Charging interval data
#' @param time_col Interval time column in \code{.data}
#' @param load_col Column in \code{.data} with charging load
#' @param agg_fn Function to use to aggregate load data, default is \code{mean}
#' @param time_agg What time interval to aggregate to, default is \code{lubridate::hour}
#' @param cf_fn Function to calcualte a confidence interval around the aggregation statistic. Default is \code{utilityR::cf.int}
#' @param ... Additional arguments to pass to \code{plt_load}
plt_load <- function(.data,
                     time_col,
                     load_col,
                     agg_fn = mean,
                     time_agg = lubridate::hour,
                     cf_fn = utilityR::cf.int,
                     ...) {
  time_col <- dplyr::enquo(time_col)
  load_col <- dplyr::enquo(load_col)
  color <-  if("color" %in% names(list(...))) list(...)[["color"]] else "blue"
  alpha <- if("alpha" %in% names(list(...))) list(...)[["alpha"]] else 0.3

  darkColor <- rgb(t(ifelse((col2rgb(color) - 50)/255 <0,0,(col2rgb(color) - 50)/255)))

  .data %>%
    dplyr::group_by(time = time_agg(!!time_col)) %>%
    dplyr::summarise(avg = agg_fn(!!load_col),
                     ci = cf_fn(!!load_col),
                     s1 = avg - ci,
                     s2 = avg + ci) %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot(ggplot2::aes(x = time)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = s1, ymax = s2), alpha = alpha, fill = color) +
    ggplot2::geom_line(ggplot2::aes(y = avg), color = darkColor) +
    ggplot2::theme_bw()
}


