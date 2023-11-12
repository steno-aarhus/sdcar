# Comments will be added over time.
library(tidyverse)
library(trelloR)
library(askpass)

stop("This prevents accidentally sourcing the whole script.")

# Details about getting Key and OAuth Secret found here:
# https://developer.atlassian.com/cloud/trello/guides/rest-api/authorization/
trello_key <- askpass("Trello Key.")
trello_oauth_secret <- askpass("Trello OAuth Secret.")

token <- get_token(
    "steno-epi-board",
    key = trello_key,
    secret = trello_oauth_secret,
    scope = c("read", "write"),
    expiration = "1day",
    cache = FALSE
)

board_url <- "https://trello.com/b/ipcYGXhC/epidemiology-group"
upcoming_board_id <- get_board_lists(board_url, token = token) %>%
    filter(str_detect(name, "Upcoming")) %>%
    pull(id)

upcoming_board_cards <- get_board_cards(board_url, token = token) %>%
    filter(idList == upcoming_board_id)

zoom_link <- askpass("Add Zoom link.")
zoom_passcode <- askpass("Add Zoom passcode.")
template_card <- read_lines(here::here("R/card-template.md")) %>%
    whisker::whisker.render(data = list(
        passcode = zoom_passcode,
        link = zoom_link
    )) %>%
    str_c(collapse = "\n")

# Use to check if there are any new additions before updating.
# Just in case you accidentally delete/overwrite something.
# Modify the code below to fit the purposes of the update.
old_content <- upcoming_board_cards
# pull(old_content, desc)

walk(
    upcoming_board_cards$id[-1],
    ~ update_resource(
        "card",
        id = .x,
        body = list(id = .x,
                    desc = template_card),
        token = token
    )
)

upcoming_board_cards %>%
    pull(id) %>%
    map(delete_card, token = token)
