#' @title Sessions to Intervals
#' @description Convert PEV charging sessions to interval data.
#' Preferably a session data set with 1 row per charging session.
#' @param .data Session data to convert to intervals
#' @param ... Any unquoted id variables. Examples would be
#'  \code{port_id, charger_id, driver_id}.
#'  @param plug_in The plug in time of the session, should be a datetime column.
#'  @param plug_out The plug out time of the session, should be a datetime column.
#'  @param charge_start The charge start time of the session, should be a datetime column.
#'  @param charge_stop The charge stop time of the session, should be a datetime column.
#'  @param kwh A numeric column representing the energy consumed during the
#'  charging session in kWh.
#'  @param units What kind of interval data to return. The default is minute
#'  interval data, setting to \code{"15 minutes"} returns 15 minute intervals.
#'  @example \dontrun{
#'  sess2int(
#'           session_data1,
#'           cust_id, charger_id, port_id = charger_id,
#'           units = "15 minutes"
#'               )
#'  }
#'
#' @export
#'

sess2int <- function(.data,
                     ...,
                     session_id = session_id,
                     plug_in = plug_in,
                     plug_out = plug_out,
                     charge_start = charge_start,
                     charge_stop = charge_stop,
                     kwh = kwh,
                     units = "mins"
) {

  session_id <- dplyr::enquo(session_id)
  plug_in <- dplyr::enquo(plug_in)
  plug_out = dplyr::enquo(plug_out)
  charge_start <- dplyr::enquo(charge_start)
  charge_stop = dplyr::enquo(charge_stop)
  kwh = dplyr::enquo(kwh)

  out <- .data %>%
    dplyr::distinct(
      ...,
      !!session_id,
      plug_in = lubridate::floor_date(!!plug_in, "mins"),
      plug_out = lubridate::floor_date(!!plug_out, "mins"),
      charge_start = lubridate::floor_date(!!charge_start, "mins"),
      charge_stop = lubridate::floor_date(!!charge_stop, "mins"),
      kwh_min = kwh / as.numeric(difftime(!!charge_stop, !!charge_start, units = "mins"))
    ) %>%
    dplyr::group_by(
      ...,
      !!session_id,
      plug_in,
      plug_out,
      charge_start,
      charge_stop,
      kwh_min) %>%
    dplyr::mutate(interval_start = list(seq.POSIXt(charge_start, charge_stop, "mins"))) %>%
    dplyr::ungroup() %>%
    tidyr::unnest(interval_start) %>%
    dplyr::group_by(..., interval_start) %>%
    dplyr::mutate(kwh_min = mean(kwh_min, na.rm = TRUE)) %>%
    dplyr::filter(plug_in == min(plug_in)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(
      ...,
      !!session_id,
      charge_start,
      charge_stop) %>%
    tidyr::complete(interval_start = seq.POSIXt(min(plug_in), max(plug_out), by = "mins")) %>%
    tidyr::fill(plug_in, plug_out, .direction = "downup") %>%
    dplyr::ungroup() %>%
    dplyr::group_by(...) %>%
    tidyr::complete(interval_start = seq.POSIXt(min(plug_in), max(plug_out), by = "mins")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(kwh = replace(kwh_min, is.infinite(kwh_min) |
                                  is.nan(kwh_min) |
                                  is.na(kwh_min), 0),
                  avg_kw = 60 * kwh) %>%
    dplyr::select(-kwh_min)

  if (units != "mins") {
    out %>%
      dplyr::group_by(...,
                      session_id,
                      interval_start = lubridate::floor_date(interval_start, unit = units),
                      plug_in,
                      plug_out,
                      charge_start,
                      charge_stop) %>%
      dplyr::summarise(minutes_charging = sum(kwh != 0),
                       kwh = sum(kwh),
                       avg_kw = mean(avg_kw)) %>%
      dplyr::ungroup()
  }

}
