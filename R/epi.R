epi_calendar_url <- "https://calendar.google.com/calendar/ical/086okoggkv7c4b0dcbbrj230s8%40group.calendar.google.com/public/basic.ics"

# Get ---------------------------------------------------------------------

#' Get's the Epi calendar and outputs as a tibble.
#'
#' @param path The path or URL to the file that is in the `.ics` format.
#'
#' @return A [tibble::tibble()].
#' @export
#'
#' @examples
#' \dontrun{
#' epi_get_calendar()
#' }
epi_get_calendar <- function(path = epi_calendar_url) {
  read_ical(path)
}

# Create ------------------------------------------------------------------

#' Add upcoming sessions to the GitHub Project Board as GitHub Issues.
#'
#' @param dates The dates for the upcoming sessions.
#'
#' @return A GitHub JSON response object.
#' @export
#'
epi_create_session_issues <- function(dates) {
  checkmate::assert_posixct(dates)
  purrr::walk(
    dates,
    \(date) {
      gh_create_issue(
        repo = "epi",
        title = stringr::str_c("Session: ", date),
        body = NA,
        assignee = NA,
        template = "upcoming-session",
        label = "sessions",
        project = "3"
      )
      Sys.sleep(1)
    }
  )
}

#' Create default sessions dates for Epi Group meetings.
#'
#' These are standardize to the iCal format. It's important to use the
#' correct timezone for the dates when giving them to this function.
#'
#' @inheritParams cal_create_event
#'
#' @return A [tibble::tibble()].
#'
#' @examples
#' upcoming <- lubridate::as_datetime(c(
#'   "2024-01-06 13:00:00",
#'   "2024-02-03 13:00:00"
#' ), tz = "Europe/Copenhagen")
#'
#' epi_create_events(
#'   upcoming,
#'   upcoming + lubridate::hours(1)
#' )
#'
epi_create_sessions <- function(start, end) {
  purrr::pmap(
    list(start, end),
    \(start, end) cal_create_event(
      start = start,
      end = end,
      title = "Steno Aarhus Epidemiology Group Monthly Session",
      description = "Details about the session are found on the GitHub board: "
    )
  ) |>
    purrr::list_rbind()
}

# Helpers -----------------------------------------------------------------

#' Add new events to the current calendar.
#'
#' Right now is mostly a wrapper around `dplyr::full_join()`.
#'
#' @param current The current calendar.
#' @param new New events to add to the calendar.
#'
#' @return A [tibble::tibble()].
#' @export
#'
epi_add_sessions <- function(current, new) {
  current |>
    dplyr::full_join(new)
}
