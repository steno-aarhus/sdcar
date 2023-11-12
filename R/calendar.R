
public_calendar_link <- "https://calendar.google.com/calendar/ical/086okoggkv7c4b0dcbbrj230s8%40group.calendar.google.com/public/basic.ics"

read_ical <- function(ics) {
  readr::read_lines(ics) %>%
    # annoyingly have to do this processing because calendar::ic_read() doesn't work well
    stringr::str_c(collapse = "\n\n") %>%
    stringr::str_replace_all("\n ", " ") %>%
    stringr::str_split("\n\n") %>%
    unlist() %>%
    calendar::ic_list() %>%
    purrr::map(calendar::ic_vector) %>%
    purrr::list_rbind() |>
    dplyr::mutate(dplyr::across(tidyselect::matches("VALUE=DATE"),
                                lubridate::as_date)) %>%
    dplyr::mutate(dplyr::across(tidyselect::matches("^(DTSTART|DTEND)$"),
                                lubridate::as_datetime)) %>%
    dplyr::mutate(
      DTSTART = lubridate::with_tz(lubridate::ymd_hms(DTSTART), "Europe/Copenhagen"),
      DTEND = lubridate::with_tz(lubridate::ymd_hms(DTEND), "Europe/Copenhagen")
    ) %>%
    dplyr::arrange(DTSTART)
}

# library(assertr)
library(tidyverse)
library(lubridate)
library(calendar)
library(glue)

stop("This prevents accidentally sourcing the whole script.")

next_year <- function() {
  lubridate::year(lubridate::today()) + 1
}

meeting_dates_md <- tibble::tribble(
  ~month, ~day,
  "January", 8,
  "February", 5,
  "March", 4,
  "March", 8,
  "May", 6,
  "June", 3,
  "July", 1,
  "September", 2,
  "October", 7,
  "November", 4,
  "December", 2
) %>%
  dplyr::mutate(
    year = next_year(),
    date = lubridate::ymd(glue::glue("{year}-{month}-{day}"), tz = "Europe/Copenhagen"),
    start_time = "13:00",
    end_time = "15:00"
  )

upcoming_meetings <- meeting_dates_md %>%
  dplyr::transmute(
    UID = purrr::map_chr(1:dplyr::n(), ~ calendar::ic_guid()),
    DTSTART = lubridate::ymd_hm(glue::glue("{date} {start_time}"), tz = "Europe/Copenhagen"),
    DTEND = lubridate::ymd_hm(glue::glue("{date} {end_time}"), tz = "Europe/Copenhagen"),
    # To show up as the event description
    DESCRIPTION = "Details about the meeting are found on the Trello board: https://trello.com/b/ipcYGXhC/epidemiology-group",
    # To show up as the event title
    SUMMARY = "Steno Aarhus Epidemiology Group Monthly Meeting"
  )

current_calendar <- read_ical(public_calendar_link) %>%
  dplyr::select(DTSTART, DTEND, SUMMARY)

new_calendar_data <- dplyr::anti_join(upcoming_meetings, current_calendar)

calendar::ic_write(calendar::ical(new_calendar_data),
                   here::here("inst/calendar.ics"))

