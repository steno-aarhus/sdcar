# Get ---------------------------------------------------------------------

gh_get_users <- function() {
  ghclass::org_members("steno-aarhus")
}

gh_get_teams <- function() {
  ghclass::org_teams("steno-aarhus")
}

gh_get_repos <- function() {
  ghclass::org_repos("steno-aarhus") |>
    stringr::str_remove("steno-aarhus/")
}

gh_get_projects <- function() {
  gh_verify()
  projects <- processx::run(
    "gh",
    c(
      "project", "list", "--owner", "steno-aarhus", "--format", "json",
      "--template", "{{range .projects}}{{.number}},{{.title}}\n{{end}}"
    )
  )$stdout
  utils::read.table(
    text = projects,
    sep = ",",
    col.names = c("number", "title"),
    header = FALSE,
    stringsAsFactors = FALSE
  ) |>
    tibble::as_tibble()
}

gh_get_labels <- function(repo) {
  gh_verify()
  repo <- rlang::arg_match(repo, gh_get_repos())
  processx::run(
    "gh",
    c(
      "label",
      "list",
      "--repo", gh_as_repo(repo),
      "--json", "name",
      "-q", ".[].name"
    )
  )$stdout |>
    stringr::str_remove_all("\n$") |>
    stringr::str_split("\n") |>
    unlist()
}

gh_get_issues <- function(repo) {
  repo <- rlang::arg_match(repo, gh_get_repos())
  gh_as_repo(repo) |>
    ghclass::repo_issues()
}

gh_get_issue_templates <- function(repo) {
  repo <- rlang::arg_match(repo, gh_get_repos())
  processx::run(
    "gh",
    c(
      "api",
      glue::glue("/repos/{gh_as_repo(repo)}/git/trees/main?recursive=true"),
      "-q", ".tree[]|.path"
    )
  )$stdout |>
    stringr::str_split("\n") |>
    unlist() |>
    stringr::str_subset("\n$", negate = TRUE) |>
    stringr::str_subset("ISSUE_TEMPLATE/") |>
    fs::path_file() |>
    fs::path_ext_remove()
}

# Add ---------------------------------------------------------------------

gh_add_to_org <- function(username) {
  checkmate::assert_character(username)
  ghclass::org_invite("steno-aarhus", username)
}

gh_add_to_team <- function(username, team) {
  username <- rlang::arg_match(username, gh_get_users())
  team <- rlang::arg_match(team, gh_get_teams())
  ghclass::team_invite("steno-aarhus", username, team)
}

gh_add_to_issue <- function(username, repo, number) {
  username <- rlang::arg_match(username, gh_get_users())
  repo <- rlang::arg_match(repo, gh_get_repos())
  number <- rlang::arg_match(number, gh_get_issues(repo)$number)

  ghclass::issue_edit(
    gh_as_repo(repo),
    number,
    assignees = username
  )
}

# Create ------------------------------------------------------------------

# gh_create_team(name)

#' Create a new GitHub issue on the SDCA GitHub organization.
#'
#' @param repo The name of the repo in the SDCA GitHub organization.
#' @param title The title to give the new issue.
#' @param body The content of the issue itself.
#' @param assignee Who to assign the issue.
#' @param template Which template to use for the issue's body.
#' @param label The label to give the issue.
#' @param project The Project Board to add the issue to.
#'
#' @return A GitHub JSON response object.
#' @export
#'
gh_create_issue <- function(repo, title, body = NA, assignee = NA, template = NA, label = NA, project = NA) {
  repo <- rlang::arg_match(repo, gh_get_repos())
  if (!is.na(assignee)) {
    assignee <- rlang::arg_match(assignee, c(gh_get_users()))
  }
  if (!is.na(template)) {
    template <- rlang::arg_match(template, c(gh_get_issue_templates(repo)))
  }
  if (!is.na(label)) {
    label <- rlang::arg_match(label, c(gh_get_labels(repo)))
  }
  if (!is.na(project)) {
    project <- rlang::arg_match(project, as.character(gh_get_projects()$number))
  }

  arguments <- c(
    "issue",
    "create",
    "--repo", gh_as_repo(repo),
    "--title", title
  )

  extra_arguments <- list(
    "--body" = body,
    "--assignee" = assignee,
    "--template" = template,
    "--label" = label,
    "--project" = project
  ) |>
    purrr::discard(is.na) |>
    purrr::imap(\(x, y) c(y, x)) |>
    unlist(use.names = FALSE)

  processx::run(
    "gh",
    c(
      arguments,
      extra_arguments
    )
  )
}

# Remove ------------------------------------------------------------------

# gh_remove_from_org(username)
# gh_remove_from_team(username, team)

# Helper functions --------------------------------------------------------

gh_verify <- function(call = rlang::caller_env()) {
  output <- processx::run("which", "gh")$status
  if (!length(output)) {
    rlang::abort("Please install the GitHub CLI (gh) before proceeding.",
      call = call
    )
  }
  return(invisible())
}

gh_as_repo <- function(repo) {
  checkmate::assert_character(repo)
  return(glue::glue("steno-aarhus/{repo}"))
}
