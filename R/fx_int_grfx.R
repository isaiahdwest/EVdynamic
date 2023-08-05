
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
#' @param group_col Default is \code{NULL}. If passed, what column to break
#' load profiles out by, e.g. if a charging customer id column is passed, the output
#' will have a load profile for each customer included.
#' @param agg_fn Function to use to aggregate load data, default is \code{mean}
#' @param time_agg What time interval to aggregate to, default is \code{lubridate::hour}
#' @param plt_cf \code{TRUE}/\code{FALSE} - whether to plot confidence interval
#' @param cf_fn Function to calculate a confidence interval around the aggregation statistic. Default is \code{utilityR::cf.int}
#' @param ... Additional arguments to pass to \code{plt_load}
#' @examples \dontrun{
#'
#' plt_load(l2_custs, time_col = int.start, load_col = interval.kwh / 0.25, group_col = cust)
#'
#' }
#' @export
plt_load <- function(.data,
                     time_col,
                     load_col,
                     group_col = NULL,
                     agg_fn = mean,
                     time_agg = lubridate::hour,
                     plt_cf = FALSE,
                     cf_fn = utilityR::cf.int,
                     peak_hrs = NULL,
                     ...) {
  time_col <- dplyr::enquo(time_col)
  load_col <- dplyr::enquo(load_col)
  group_col <- tryCatch({
    if (is.null(group_col)) NULL else dplyr::enquo(group_col)
  }, error = function(e) {
    dplyr::enquo(group_col)
  })
  grp_colname <- if (is.null(group_col)) NA else dplyr::quo_name(group_col)

  colors <-
    if (is.null(group_col)) {
      if("color" %in% names(list(...))) list(...)[["color"]] else "blue"
    } else {
      if("colors" %in% names(list(...))) list(...)[["colors"]] else ggplot2::scale_color_hue()$palette(nrow(dplyr::distinct(.data, !!group_col)))
    }
  alpha <- if("alpha" %in% names(list(...))) list(...)[["alpha"]] else 0.3

  darkColor <- grDevices::rgb(t(ifelse((col2rgb(colors) - 50)/255 <0,0,(grDevices::col2rgb(colors) - 50)/255)))

  if (is.numeric(.data[[grp_colname]])) {
    .data[[grp_colname]] <- factor(.data[[grp_colname]], levels = as.character(sort((unique(.data[[grp_colname]])))))
  }


  if (is.null(group_col)) {
    .data %>%
      dplyr::group_by(!!group_col, time = time_agg(!!time_col)) %>%
      dplyr::summarise(avg = agg_fn(!!load_col),
                       ci = cf_fn(!!load_col),
                       s1 = avg - ci,
                       s2 = avg + ci) %>%
      dplyr::ungroup() %>%
      ggplot2::ggplot(ggplot2::aes(x = time, group = !!group_col)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = s1, ymax = s2), fill = colors, alpha = alpha) +
      ggplot2::geom_line(ggplot2::aes(y = avg, color = !!group_col)) +
      ggplot2::theme_bw() +
      ggplot2::scale_color_manual(
        values = darkColor
      ) +
      ggplot2::scale_fill_manual(
        values = colors
      )
  } else {
    .data %>%
      dplyr::group_by(!!group_col, time = time_agg(!!time_col)) %>%
      dplyr::summarise(avg = agg_fn(!!load_col),
                       ci = cf_fn(!!load_col),
                       s1 = avg - ci,
                       s2 = avg + ci) %>%
      dplyr::ungroup() %>%
      ggplot2::ggplot(ggplot2::aes(x = time, group = !!group_col)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = s1, ymax = s2, fill = !!group_col), alpha = alpha) +
      ggplot2::geom_line(ggplot2::aes(y = avg, color = !!group_col)) +
      ggplot2::theme_bw() +
      ggplot2::scale_color_manual(
        values = darkColor
      ) +
      ggplot2::scale_fill_manual(
        values = colors
      )
  }
}

#' @title ggplot2 Utility peak hour shading
#' @description Add shading for peak hours to your load profile plots
#' @param start Numeric for the starting hour of peak hours (e.g. \code{12})
#' @param end Numeric for the ending hour, e.g. if peak hours are 12pm - 8pm
#' start would be \code{12} and end would be \code{20}
#' @param color Color to shade with, default is \code{"gray"}
#' @param alpha The alpha to shade with, default is \code{0.2}
#' @examples \dontrun{
#' plt_load(l2_custs[l2_custs$cust == 1, ],
#' time_col = int.start,
#' load_col = interval.kwh / 0.25,
#' cf_fn = function(x) 0) +
#'   labs(x = "Time of Day (Hours)",
#'        y = "Average kW",
#'        title = "Load Profile for a Level 2 Charger") +
#'   plt_pk_hrs(start = 12, end = 20)
#' }
#' @export
plt_pk_hrs <- function(start,
                       end,
                       fill = "gray",
                       alpha = 0.2) {
ggplot2::annotate(geom = "rect",
                      xmin = start,
                      xmax = end,
                      ymin = -Inf,
                      ymax = Inf,
                      fill = fill,
                      alpha = alpha)
}


