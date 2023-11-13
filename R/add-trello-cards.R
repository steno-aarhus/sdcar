epi_board_url <- "https://trello.com/b/ipcYGXhC/epidemiology-group"

#' Authenticate with the Epi Group Trello board.
#'
#' Details about getting Key and OAuth Secret are found on this
#' [website](https://developer.atlassian.com/cloud/trello/guides/rest-api/authorization/).
#' @return A token used to connect and authenticate with Trello.
#'
tr_auth_epi <- function() {
  trello_key <- askpass::askpass("Trello Key.")
  trello_oauth_secret <- askpass::askpass("Trello OAuth Secret.")
  trelloR::get_token(
    "steno-epi-board",
    key = trello_key,
    secret = trello_oauth_secret,
    scope = c("read", "write"),
    expiration = "1day",
    cache = FALSE
  )
}

#' Get the ID for a Trello board label.
#'
#' @param url The board URL.
#' @param label_name The name of the board label you want the ID for.
#' @param token The token obtained from [tr_auth_epi()].
#'
#' @return A character vector of the label ID.
#'
tr_get_board_label_id <- function(url = epi_board_url, label_name = "Meeting", token) {
  trelloR::get_board_labels(url, token = token) |>
    dplyr::filter(name == label_name) |>
    dplyr::pull(id)
}

#' Get the card template to use when making Trello cards.
#'
#' @return A character vector of the card template.
#'
tr_read_card_template <- function() {
  zoom_link <- askpass::askpass("Add Zoom link.")
  zoom_passcode <- askpass::askpass("Add Zoom passcode.")
  template_card_desc <- readr::read_lines(fs::path_package("sdcar", "templates", "trello-meeting-card.md")) |>
    whisker::whisker.render(data = list(
      passcode = zoom_passcode,
      link = zoom_link
    ))
}

#' Get the ID for a Trello board.
#'
#' @param board_name The name of the board you want the ID for.
#' @inheritParams tr_get_board_label_id
#'
#' @return A character vector of the board ID.
#'
tr_get_board_id <- function(url = epi_board_url, board_name = "Upcoming", token) {
  trelloR::get_board_lists(url, token = token) |>
    dplyr::filter(stringr::str_detect(name, board_name)) |>
    dplyr::pull(id)
}

#' Add upcoming meetings to Trello as cards.
#'
#' @param data Data that contains the month and day of the upcoming meetings.
#' @inheritParams tr_get_board_label_id
#'
#' @return Used for the side effect of posting to Trello.
#' @export
#'
update_epi_trello_meetings <- function(data, token) {
  dates <- data |>
    dplyr::mutate(dates = lubridate::ymd(glue::glue("{next_year()}-{month}-{day}"), tz = "Europe/Copenhagen")) |>
    dplyr::pull(dates)

  upcoming_board_id <- tr_get_board_id(token = token)
  template_card <- tr_read_card_template()
  label <- tr_get_board_label_id(token = token)

  purrr::walk(
    dates,
    ~ trelloR::add_card(
      upcoming_board_id,
      body = list(
        name = .x,
        desc = template_card,
        pos = "bottom",
        idLabels = array(label)
      ),
      token = token
    )
  )
}
