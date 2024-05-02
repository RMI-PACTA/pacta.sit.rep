#' Generate a table with R package information
#'
#' @param repo_paths A character vector with GH paths to the R package
#'   repositories
#'
#' @return A tibble with the following columns:
#' * Repo
#' * Lifecycle
#' * Status
#' * Latest_SHA
#' * Maintainer
#'
#' @export
#'
#' @examples
#' generate_package_table(c("RMI-PACTA/r2dii.data", "RMI-PACTA/r2dii.match"))
generate_package_table <- function(repo_paths) {
  out <- tibble::tibble(
    repo = repo_paths,
    lifecycle = purrr::map_chr(repo_paths, table_lifecycle),
    status = purrr::map_chr(repo_paths, table_status),
    sha = purrr::map_chr(repo_paths, table_latest_sha),
    maintainer = purrr::map_chr(repo_paths, table_maintainer)
  )

  dplyr::transmute(
    out,
    Repo = format_repo_v(.data[["repo"]]),
    Lifecycle = format_lifecycle_v(.data[["lifecycle"]]),
    Status = format_status_v(.data[["repo"]], .data[["status"]]),
    Latest_SHA = format_latest_sha_v(.data[["repo"]], .data[["sha"]]),
    Maintainer = format_maintainer_v(.data[["maintainer"]])
  )
}

format_repo <- function(repo) {
  repo_base <- gsub(".*/", "", repo)
  glue::glue("[{repo_base}](https://github.com/{repo})")
}

format_repo_v <- Vectorize(format_repo)

format_lifecycle <- function(lifecycle_badge) {
  if (is.na(lifecycle_badge)) {
    return("No lifecycle badge found.")
  }

  desc <- "![Lifecycle]"
  link <- "https://lifecycle.r-lib.org/articles/stages.html"

  glue::glue("[{desc}({lifecycle_badge})]({link})")
}

format_lifecycle_v <- Vectorize(format_lifecycle)

format_status <- function(repo, r_cmd_check_status) {

  if (is.na(r_cmd_check_status)) {
    return("No R CMD check found.")
  }

  r_cmd_check_status
}

format_status_v <- Vectorize(format_status)

format_latest_sha <- function(repo, sha) {

  if (is.na(sha)) {
    return("`main` branch not found.")
  }

  short_sha <- substr(sha, 1, 7)

  glue::glue(
    "[`{short_sha}`](https://github.com/{repo}/commits/main)"
  )
}

format_latest_sha_v <- Vectorize(format_latest_sha)

format_maintainer <- function(maintainer) {

  if (is.na(maintainer)) {
    return("No maintainer found.")
  }

  glue::glue(
    "[@{maintainer}](https://github.com/{maintainer}/)"
  )
}

format_maintainer_v <- Vectorize(format_maintainer)

table_lifecycle <- function(repo_path) {
  readme <- get_gh_text_file(repo_path, file_path = "README.md")

  if (is.null(readme)) {
    return(NA_character_)
  }

  pattern <- "https://img.shields.io/badge/lifecycle-\\S+.svg"

  lifecycle_badge <- readme[grepl(pattern, readme)][1]
  lifecycle_badge <- gsub(".*(https[^)]*\\.svg).*", "\\1", lifecycle_badge)

  if (length(lifecycle_badge) == 0) {
    return(NA_character_)
  }

  return(lifecycle_badge)
}

table_status <- function(repo_path) {
  readme <- get_gh_text_file(repo_path, file_path = "README.md")

  r_cmd_check_status <- readme[grepl("R.yml|R-CMD-check.yaml", readme)]

  return(r_cmd_check_status)
}

table_latest_sha <- function(repo_path) {
  latest_sha <- fetch_main_sha(repo_path)

  if (is.null(latest_sha)) {
    return(NA_character_)
  }

  return(latest_sha)
}

table_maintainer <- function(repo_path) {
  codeowners <- get_gh_text_file(repo_path, file_path = ".github/CODEOWNERS")
  if (is.null(codeowners)) {
    return(NA_character_)
  }

  maintainer <- codeowners[!grepl("^#", codeowners)]
  maintainer <- maintainer[grepl("^*", maintainer)]
  maintainer <- maintainer[grepl("^.*@.*$", maintainer)]
  maintainer <- gsub("^.*@(.*)$", "\\1", maintainer)

  if (length(maintainer) == 0) {
    return(NA_character_)
  }

  return(maintainer)
}

fetch_main_sha <- function(repo_path) {
  response <- tryCatch(
    gh::gh(
      "/repos/{repo}/branches/main",
      repo = repo_path
    ),
    error = function(cond) return(NULL)
  )

  if (!is.null(response)) {
    return(response$commit$sha)
  }
  return(NULL)
}
