
add_user_to_org <- function(username) {
  checkmate::assert_character(username)
  ghclass::org_invite("steno-aarhus", username)
}

add_user_to_common_docs_team <- function(username) {
  checkmate::assert_character(username)
  return(invisible(list(
    ghclass::org_invite("steno-aarhus", username),
    ghclass::team_invite("steno-aarhus", username, "common-docs-team")
  )))
}

list_github_teams <- function() {
  ghclass::org_teams("steno-aarhus")
}
