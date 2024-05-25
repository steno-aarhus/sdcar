epi_calendar_url <- "https://calendar.google.com/calendar/ical/086okoggkv7c4b0dcbbrj230s8%40group.calendar.google.com/public/basic.ics"

#' Read in a iCal file or URL
#'
#' @param ics A file or URL that is in the `.ics` format.
#'
#' @return A [tibble::tibble()].
#'
cal_read_ical <- function(ics) {
  events_as_vector_items <- readr::read_lines(ics) |>
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
      tidyselect::matches("VALUE=DATE"),
      lubridate::as_date
    )) |>
    dplyr::mutate(dplyr::across(
      tidyselect::matches("^(DTSTART|DTEND)$"),
      lubridate::as_datetime
    )) |>
    dplyr::mutate(
      DTSTART = lubridate::with_tz(lubridate::ymd_hms(DTSTART), "Europe/Copenhagen"),
      DTEND = lubridate::with_tz(lubridate::ymd_hms(DTEND), "Europe/Copenhagen")
    ) |>
    dplyr::arrange(DTSTART)
}

#' Calculate next year.
#'
#' @return A numeric vector for the year.
#'
next_year <- function() {
  lubridate::year(lubridate::today()) + 1
}

#' Add calendar details for Epi Group meetings and standardize to the iCal format.
#'
#' @inheritParams cal_append_current
#'
#' @return A [tibble::tibble()].
#'
cal_set_epi_meeting_details <- function(data) {
  upcoming <- data |>
    dplyr::mutate(
      year = next_year(),
      date = lubridate::ymd(glue::glue("{year}-{month}-{day}"), tz = "Europe/Copenhagen"),
      start_time = "13:00",
      end_time = "15:00"
    )

  upcoming_meetings <- upcoming |>
    dplyr::transmute(
      UID = purrr::map_chr(1:dplyr::n(), ~ calendar::ic_guid()),
      DTSTART = lubridate::ymd_hm(glue::glue("{date} {start_time}"), tz = "Europe/Copenhagen"),
      DTEND = lubridate::ymd_hm(glue::glue("{date} {end_time}"), tz = "Europe/Copenhagen"),
      # To show up as the event description
      DESCRIPTION = "Details about the meeting are found on the Trello board: https://trello.com/b/ipcYGXhC/epidemiology-group",
      # To show up as the event title
      SUMMARY = "Steno Aarhus Epidemiology Group Monthly Meeting"
    )
}

#' Add the current calendar to the upcoming calendar.
#'
#' @param data Data with columns that match standard iCal format.
#' @param calendar_url The URL to the `.ics` file.
#'
#' @return A [tibble::tibble()].
#'
cal_append_current <- function(data, calendar_url = epi_calendar_url) {
  current_calendar <- cal_read_ical(calendar_url) |>
    dplyr::select(DTSTART, DTEND, SUMMARY)

  data |>
    dplyr::anti_join(current_calendar, by = dplyr::join_by(DTSTART, DTEND, SUMMARY))
}

#' Save data as a file in the iCal format.
#'
#' File is saved to `inst/calendar.ics`.
#'
#' @inheritParams cal_append_current
#'
#' @return A file.
#'
cal_write_ical <- function(data) {
  output_ics <- rprojroot::find_package_root_file("inst", "calendar.ics")
  data |>
    calendar::ical() |>
    calendar::ic_write(
      file = output_ics
    )
}

#' Update the Epi Group calendar with upcoming meetings.
#'
#' @param data Upcoming meetings for the Epi Group.
#'
#' @return A `.ics` file of upcoming meetings.
#' @export
#'
update_epi_calendar_meetings <- function(data) {
  data |>
    cal_set_epi_meeting_details() |>
    cal_append_current() |>
    cal_write_ical()
}

# x <- seq(ymd("2010-01-01"),ymd("2015-12-31"),by="1 day")
# x[wday(x,label = TRUE) == "Mon" & day(x) <= 7]
