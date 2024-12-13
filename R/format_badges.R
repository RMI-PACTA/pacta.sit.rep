build_markdown_link <- function(display, path) {
  glue::glue("[{display}]({path})")
}

format_name_badge <- function(repo_path) {
  repo_org <- strsplit(repo_path, "/", fixed = TRUE)[[1L]][[1L]]
  repo_name <- strsplit(repo_path, "/", fixed = TRUE)[[1L]][[2L]]

  build_markdown_link(
    display = repo_name,
    path = glue::glue("https://{tolower(repo_org)}.github.io/{repo_name}/")
  )
}

format_lifecycle_badge <- function(lifecycle) {
  stopifnot(
    lifecycle %in% c("experimental", "stable", "deprecated", "superseded")
  )

  lifecycle_badge_url <- switch(
    lifecycle,
    experimental = "https://lifecycle.r-lib.org/reference/figures/lifecycle-experimental.svg", # nolint: line_length_linter
    stable = "https://lifecycle.r-lib.org/reference/figures/lifecycle-stable.svg", # nolint: line_length_linter
    deprecated = "https://lifecycle.r-lib.org/reference/figures/lifecycle-deprecated.svg", # nolint: line_length_linter
    superseded = "https://lifecycle.r-lib.org/reference/figures/lifecycle-superseded.svg" # nolint: line_length_linter
  )

  build_markdown_link(
    display = glue::glue(
      "![]({lifecycle_badge_url})"
    ),
    path = glue::glue(
      "https://lifecycle.r-lib.org/articles/stages.html#{lifecycle}"
    )
  )
}

format_status_badge <- function(repo_path, ci_check) {
  build_markdown_link(
    display = glue::glue(
      "![](https://github.com/{repo_path}/actions/workflows/{ci_check}/badge.svg?branch=main)" # nolint: line_length_linter
    ),
    path = glue::glue(
      "https://github.com/{repo_path}/actions/workflows/{ci_check}?query=branch%3Amain" # nolint: line_length_linter
    )
  )
}

format_coverage_badge <- function(repo_path) {
  build_markdown_link(
    display = glue::glue(
      "![](https://img.shields.io/codecov/c/github/{tolower(repo_path)}/main)"
    ),
    path = glue::glue("https://app.codecov.io/gh/{repo_path}?branch=main")
  )
}

format_version_badge <- function(repo_path) {
  build_markdown_link(
    display = glue::glue(
      "![](https://img.shields.io/github/r-package/v/{tolower(repo_path)}/main?label=version&amp;labelColor=%23444d56&amp;color=%2334d058)" # nolint: line_length_linter
    ),
    path = glue::glue("https://github.com/{repo_path}/blob/main/DESCRIPTION")
  )
}

format_maintainer_badge <- function(repo_path) {
  repo_org <- strsplit(repo_path, "/", fixed = TRUE)[[1L]][[1L]]
  repo_name <- strsplit(repo_path, "/", fixed = TRUE)[[1L]][[2L]]

  build_markdown_link(
    display = glue::glue(
      "![](https://img.shields.io/badge/dynamic/json?label=codeowner&amp;query=codeownerInfo.ownersForFile&amp;url=https%3A%2F%2Fgithub.com%2F{tolower(repo_org)}%2F{tolower(repo_name)}%2Fdeferred-metadata%2Fmain%2F.github%2FCODEOWNERS)" # nolint: line_length_linter
    ),
    path = glue::glue(
      "https://github.com/{repo_path}/blob/main/.github/CODEOWNERS"
    )
  )
}
