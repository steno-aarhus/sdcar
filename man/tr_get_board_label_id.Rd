% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add-trello-cards.R
\name{tr_get_board_label_id}
\alias{tr_get_board_label_id}
\title{Get the ID for a Trello board label.}
\usage{
tr_get_board_label_id(url = epi_board_url, label_name = "Meeting", token)
}
\arguments{
\item{url}{The board URL.}

\item{label_name}{The name of the board label you want the ID for.}

\item{token}{The token obtained from [tr_auth_epi()].}
}
\value{
A character vector of the label ID.
}
\description{
Get the ID for a Trello board label.
}
