#' Generate a table with R package information
#'
#' @param repo_paths A character vector with GH paths to the R package repositories
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
  tibble::tibble(
    Repo = repo_paths,
    Lifecycle = purrr::map_chr(repo_paths, table_lifecycle),
    Status = purrr::map_chr(repo_paths, table_status),
    Latest_SHA = purrr::map_chr(repo_paths, table_latest_sha),
    Maintainer = purrr::map_chr(repo_paths, table_maintainer)
  )
}

table_lifecycle <- function(repo_path) {
  readme <- fetch_readme(repo_path)

  pattern <- "https://img.shields.io/badge/lifecycle-.*.svg"

  lifecycle_badge <- readme[grepl(pattern, readme)]
  lifecycle_badge <- gsub(".*(https[^)]*\\.svg).*", "\\1", lifecycle_badge)

  return(lifecycle_badge)
}

table_status <- function(repo_path) {
  r_cmd_check_status <-   glue::glue(
    "https://github.com/{repo_path}/actions/workflows/R-CMD-check.yaml/badge.svg"
  )

  return(r_cmd_check_status)
}

table_latest_sha <- function(repo_path) {
  latest_sha <- fetch_main_sha(repo_path)

  return(latest_sha)
}

table_maintainer <- function(repo_path) {
  codeowners <- fetch_codeowners(repo_path)

  maintainer <- codeowners[!grepl("^#", codeowners)]
  maintainer <- maintainer[grepl("^*", maintainer)]
  maintainer <- maintainer[grepl("^.*@.*$", maintainer)]
  maintainer <- gsub("^.*@(.*)$", "\\1", maintainer)

  maintainer <- format_maintainer(maintainer)

  return(maintainer)
}

format_lifecycle <- function(lifecycle_badge) {

}

format_status <- function(r_cmd_check_status) {

}

format_latest_sha <- function(latest_sha) {

}

format_maintainer <- function(maintainer) {
  glue::glue(
    "[@{maintainer}](https://github.com/{maintainer}/)"
  )
}

fetch_codeowners <- function(repo_path) {
  response <- tryCatch(
    gh::gh(
      "/repos/{repo}/contents/.github/CODEOWNERS",
      repo = repo_path,
      ref = "main"
    ),
    error = function(cond) return(NULL)
  )

  if (!is.null(response)) {
    return(readLines(response$download_url))
  }
  return(NULL)
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

fetch_readme <- function(repo_path) {
  response <- tryCatch(
    gh::gh(
      "/repos/{repo}/contents/README.md",
      repo = repo_path
    ),
    error = function(cond) return(NULL)
  )

  if (!is.null(response)) {
    return(readLines(response$download_url))
  }
  return(NULL)
}
