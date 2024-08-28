#' Create a tibble with an iCal event.
#'
#' @param start The datetime of when the event starts. Must be `POSIXct` or similar, recommended to use [lubridate::as_datetime()] to correctly format the date.
#' @param end The datetime of when the event ends. See `start` for formatting recommendations.
#' @param title The title shown in the calendar event.
#' @param description A description that is shown in the calendar event.
#'
#' @return A single row of a [tibble::tibble()].
#' @export
#'
#' @examples
#' cal_create_event(
#'   start = lubridate::as_datetime(lubridate::now()),
#'   end = lubridate::now() + lubridate::hours(1),
#'   title = "Meeting",
#'   description = "Discussing the new project"
#' )
cal_create_event <- function(start, end, title, description) {
  checkmate::assert_posixct(start)
  checkmate::assert_scalar(start)
  checkmate::assert_posixct(end)
  checkmate::assert_scalar(end)
  checkmate::assert_character(title)
  checkmate::assert_scalar(title)
  checkmate::assert_character(description)
  checkmate::assert_scalar(description)

  tibble::tibble(
    UID = calendar::ic_guid(),
    DTSTART = start,
    DTEND = end,
    # To show up as the event description
    DESCRIPTION = description,
    # To show up as the event title
    SUMMARY = title,
    # The below are standard iCal fields
    TRANSP = "OPAQUE",
    STATUS = "CONFIRMED",
    SEQUENCE = "0",
    CREATED = cal_timestamp(),
    `LAST-MODIFIED` = cal_timestamp(),
    DTSTAMP = cal_timestamp()
  )
}

cal_timestamp <- function() {
  cal_datetime(lubridate::now())
}

#' Convert a date or datetime object to an iCal timestamp.
#'
#' @param date A datetime object.
#'
#' @return A character vector.
#'
#' @examples
#' cal_datetime(lubridate::now())
cal_datetime <- function(date) {
  lubridate::as_datetime(date, tz = "Europe/Copenhagen")
}

#' Save data as a file in the iCal format.
#'
#' File is saved to `inst/epi-calendar.ics`.
#'
#' @param data The [tibble::tibble()] that contains the events to save in iCal format.
#' @param path The path to the output file.
#'
#' @return A file.
#' @export
#'
write_ical <- function(data, path = NA, header = NULL) {
  if (is.na(path)) {
    path <- rprojroot::find_package_root_file("inst", "calendar.ics")
  }

  data |>
    calendar::ical(ic_attributes = header) |>
    calendar::ic_write(
      file = path
    )
}

#' Read in an iCal file as a tibble.
#'
#' @param path The path to the `.ics` file.
#'
#' @return A [tibble::tibble()].
#' @export
#'
#' @examples
#'
#' read_ical(here::here("inst/calendar.ics"))
#'
read_ical <- function(path) {
  events_as_vector_items <- readr::read_lines(path) |>
    # annoyingly have to do this processing because calendar::ic_read() doesn't work well
    stringr::str_c(collapse = "\n\n") |>
    stringr::str_replace_all("\n ", " ") |>
    stringr::str_split("\n\n") |>
    unlist()

  events_as_tibble <- events_as_vector_items |>
    calendar::ic_list() |>
    purrr::map(calendar::ic_vector) |>
    purrr::map(dplyr::bind_rows) |>
    purrr::list_rbind()

  events_as_tibble |>
    dplyr::mutate(dplyr::across(
      tidyselect::everything(),
      \(x) {
        x <- dplyr::na_if(x, "NATNA")
        dplyr::na_if(x, "NA")
      }
    )) |>
    dplyr::mutate(dplyr::across(
      tidyselect::matches("VALUE=DATE"),
      lubridate::as_date
    )) |>
    dplyr::mutate(dplyr::across(
      tidyselect::matches("^(DTSTART|DTEND|CREATED|LAST-MODIFIED|DTSTAMP)$"),
      cal_datetime
    )) |>
    dplyr::arrange(DTSTART)
}
