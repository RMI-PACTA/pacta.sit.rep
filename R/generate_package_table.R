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
  out <- tibble::tibble(
    repo = repo_paths,
    lifecycle = purrr::map_chr(repo_paths, table_lifecycle),
    status = purrr::map_chr(repo_paths, table_status),
    sha = purrr::map_chr(repo_paths, table_latest_sha),
    maintainer = purrr::map_chr(repo_paths, table_maintainer)
  )

  dplyr::transmute(
    out,
    Repo = format_repo(repo),
    Lifecycle = format_lifecycle(lifecycle),
    Status = format_status(repo, status),
    Latest_SHA = format_latest_sha(repo, sha),
    Maintainer = format_maintainer(maintainer)
    )
}

format_repo <- function(repo) {
  repo_base <- gsub(".*/", "", repo)
  glue::glue("[{repo_base}](https://github.com/{repo})")
}

format_lifecycle <- function(lifecycle_badge) {
  desc <- "![Lifecycle]"
  link <- "https://lifecycle.r-lib.org/articles/stages.html"

  glue::glue("[{desc}({lifecycle_badge})]({link})")
}

format_status <- function(repo, r_cmd_check_status) {
  desc <- "![R-CMD-check]"
  link <- glue::glue("https://github.com/{repo}/actions/workflows/R-CMD-check.yaml")

  glue::glue("[{desc}({r_cmd_check_status})]({link})")
}

format_latest_sha <- function(repo, sha) {
  short_sha <- substr(sha, 1, 7)

  glue::glue(
    "[`{short_sha}`](https://github.com/{repo}/commits/main)"
  )
}

format_maintainer <- function(maintainer) {
  glue::glue(
    "[@{maintainer}](https://github.com/{maintainer}/)"
  )
}

table_lifecycle <- function(repo_path) {
  readme <- fetch_readme(repo_path)

  pattern <- "https://img.shields.io/badge/lifecycle-\\S+.svg"

  lifecycle_badge <- readme[grepl(pattern, readme)][1]
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

  return(maintainer)
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
