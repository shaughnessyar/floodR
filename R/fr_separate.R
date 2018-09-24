#' Hydrograph Separations
#'
#' @description This function takes start and end dates of floods and calculates
#' the baseflow fraction during the flood based on a two end member mixing model.
#'
#' @usage fr_separate(.data, dateVar = NULL, timeVar = NULL, dateTimeVar = NULL,
#'                    tracerVar, fractionVar, starts, ends, hours, ppt, checkOverlap = TRUE)
#'
#' @param .data The working dataframe
#' @param dateVar The variable holding date data
#' @param timeVar The variable holding time data
#' @param dateTimeVar The variable holding time data
#' @param tracerVar The variable holding the tracer data for the mixing model
#' @param fractionVar The name of a new variable to hold the results of the separation
#' @param starts Start times of floods
#' @param ends End times of floods
#' @param hours Number of hours to look back as baseflow conditions for the tracerVar
#' @param ppt The value to use for precipitation in mixing model
#' @param checkOverlap Logical value to check if baseflows overlap
#'
#' @return A dataframe with a new variable holding baseflow frations for each flood with
#' attributes about which baseflow values/times were used in the separations.
#'
#' @seealso \code{\link{selectFloods}} for selecting start and end dates of floods
#'
#' @import rlang
#' @import dplyr
#' @import data.table
#' @import lubridate
#' @import purrr
#'
#' @examples
#' \dontrun{
#' fr_separate(.data = df, dateTimeVar = datetimeQ, tracerVar = SpC, fractionVar = Xb,
#'             starts = selected$start, ends = selected$end, hours = 24, ppt = 54, checkOverlap = T)
#'}
#'
#' @export
fr_separate <- function(.data, dateVar = NULL, timeVar = NULL, dateTimeVar = NULL, tracerVar, fractionVar,
                        starts, ends, hours, ppt, checkOverlap = TRUE){
  paramList <- as.list(match.call())

  if (!is.character(paramList$dateTimeVar)) {
    dt_var <- rlang::enquo(dateTimeVar)
  } else if (is.character(paramList$dateTimeVar)) {
    dt_var <- rlang::quo(!! rlang::sym(dateTimeVar))
  }

  if (!is.character(paramList$dateVar)) {
    d_var <- rlang::enquo(dateVar)
  } else if (is.character(paramList$dateVar)) {
    d_var <- rlang::quo(!! rlang::sym(dateVar))
  }

  if (!is.character(paramList$timeVar)) {
    t_var <- rlang::enquo(timeVar)
  } else if (is.character(paramList$timeVar)) {
    t_var <- rlang::quo(!! rlang::sym(timeVar))
  }

  if (!is.character(paramList$tracerVar)) {
    tcr_var <- rlang::enquo(tracerVar)
  } else if (is.character(paramList$tracerVar)) {
    tcr_var <- rlang::quo(!! rlang::sym(tracerVar))
  }

  frc_var <- rlang::quo_name(rlang::enquo(fractionVar))

  if("dateVar" %in% names(paramList) & "timeVar" %in% names(paramList)){
    .data <- .data %>% mutate(dt = str_c(!!d_var, !!t_var, sep = " "),
                              dt = lubridate::parse_date_time(dt,
                                          orders = c("mdy HMS", "mdy HM", "ymd HMS", "ymd HM")))
  }
  else if("dateTimeVar" %in% names(paramList)){
    .data <- .data %>% mutate(dt = lubridate::parse_date_time(!!dt_var,
                                          orders = c("mdy HMS", "mdy HM", "ymd HMS", "ymd HM")))
  }



  starts <- lubridate::parse_date_time(starts, orders = c("mdy HMS", "mdy HM", "ymd HMS", "ymd HM"))
  ends <- lubridate::parse_date_time(ends, orders = c("mdy HMS", "mdy HM", "ymd HMS", "ymd HM"))
  interv <- lubridate::interval(starts, ends)

  base_vals <- setkey(setDT(.data),"dt")[setkey(setDT(
    data_frame(times = starts - (hours*60*60))),"times"), roll = "nearest"] %>%
    as_tibble() %>% pull(!!tcr_var)

  base_dates <- data_frame(starts = starts - (hours*60*60), base_vals = base_vals)

  if(checkOverlap == TRUE){
    overlap_vec <- map(starts, ~.x %within% interv) %>% purrr::map(~length(which(.x == TRUE)))
    log_check <- overlap_vec %>% unlist() %>% {.>1}
    non_overlaps <- purrr::map_dbl(which(log_check), ~.x - min(.x - which(!log_check)[which(!log_check) < .x]))
    base_vals[which(overlap_vec > 1)] <- base_vals[non_overlaps]
  }

  base_dates <- list(data_frame(base_vals = base_vals), base_dates) %>%
    purrr::reduce(left_join, by = "base_vals") %>%
    pull(starts)

  .data <- .data %>%
    mutate(`:=`(!!frc_var, purrr::map2(interv, base_vals,
                                       ~ifelse(with(.data, dt %within% .x),
                                               with(.data, (!!tcr_var - ppt)/(.y - ppt)), NA)) %>%
                purrr::reduce(dplyr::coalesce))) %>%
    select(-dt)

  attr(.data, "dates") <- paste("dates used for baseflow:",
                                paste(base_dates, collapse = ", "))
  attr(.data, "values") <- paste("values used for baseflow:", paste(base_vals, collapse = ", "))
  attr(.data, "overlaps") <- paste("number of overlaps:",
                            tryCatch(length(which(overlap_vec > 1)), error = function(x) paste("NA")))
  attr(.data, "tracer") <- quo_name(tcr_var)
  attr(.data, "num_floods") <- length(interv)
  return(.data)
}
