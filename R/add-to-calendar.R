# library(assertr)
library(tidyverse)
library(lubridate)
library(calendar)
library(glue)

meeting_dates <- tribble(
        ~month, ~day,
        "January", 3,
        "February", 7,
        "March", 7,
        "April", 4,
        "May", 2,
        "June", 13,
        "September", 5,
        "October", 3,
        "November", 7,
        "December", 5
    ) %>%
    mutate(
        year = "2022",
        date = ymd(glue("{year}-{month}-{day}"), tz = "Europe/Copenhagen"),
        start_time = "13:00",
        end_time = "15:00"
    )

upcoming_meetings <- meeting_dates %>%
      transmute(
          UID = map_chr(1:n(), ~ ic_guid()),
          DTSTART = ymd_hm(glue("{date} {start_time}"), tz = "Europe/Copenhagen"),
          DTEND = ymd_hm(glue("{date} {end_time}"), tz = "Europe/Copenhagen"),
          # To show up as the event description
          DESCRIPTION = "Details about the meeting are found on the Trello board: https://trello.com/b/ipcYGXhC/epidemiology-group",
          # To show up as the event title
          SUMMARY = "Steno Aarhus Epidemiology Group Monthly Meeting"
      )


tidy_ical <- function(path) {
  path |>
    calendar::ic_read() |>
    dplyr::mutate(
      DTSTART = lubridate::with_tz(lubridate::ymd_hms(DTSTART), "Europe/Copenhagen"),
      DTEND = lubridate::with_tz(lubridate::ymd_hms(DTEND), "Europe/Copenhagen")
    ) %>%
    dplyr::arrange(DTSTART) %>%
    dplyr::select(DTSTART, DTEND, SUMMARY)
}

current_calendar <- ic_read(public_calendar_link) %>%
  mutate(
    DTSTART = with_tz(ymd_hms(DTSTART), "Europe/Copenhagen"),
    DTEND = with_tz(ymd_hms(DTEND), "Europe/Copenhagen")
  ) %>%
  arrange(DTSTART) %>%
  select(DTSTART, DTEND, SUMMARY)

new_calendar_data <- anti_join(upcoming_meetings, current_calendar)

calendar::ic_write(
  ic = calendar::ical(new_calendar_data),
  file = fs::path_package("sdcar", "data", "calendar.ics")
)

save_old_calendar <- function() {
public_calendar_link <- "https://calendar.google.com/calendar/ical/086okoggkv7c4b0dcbbrj230s8%40group.calendar.google.com/public/basic.ics"
  ic_read(public_calendar_link) %>%
    mutate(
      DTSTART = with_tz(ymd_hms(DTSTART), "Europe/Copenhagen"),
      DTEND = with_tz(ymd_hms(DTEND), "Europe/Copenhagen")
    ) %>%
    arrange(DTSTART) %>%
    select(DTSTART, DTEND, SUMMARY)
}
