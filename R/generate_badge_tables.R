#' Generate a table with R package information
#'
#' @param repo_vectors A list of named character vectors. Each character vector
#'   should contain:
#' * path - the shortpath to the GitHub repository, including organization
#'   (e.g. "RMI-PACTA/r2dii.data")
#' * lifecycle - the lifecycle badge status (e.g. "stable", "experimental")
#' * ci_check - the name of the CI check file (e.g. "R-CMD-check.yaml")
#
#' @return A formatted sitrep tibble.
#'
#' @export
#'
#' @examples
#'
#' repos <- list(
#'   c(
#'     path = "RMI-PACTA/r2dii.data",
#'     lifecycle = "stable",
#'     ci_check = "R-CMD-check.yaml"
#'   ),
#'   c(
#'     path = "RMI-PACTA/r2dii.plot",
#'     lifecycle = "stable",
#'     ci_check = "R.yml"
#'   )
#' )
#'
#' generate_package_table(repos)
generate_package_table <- function(repo_vectors) {
  out <- parse_inputs(repo_vectors)

  out <- dplyr::rowwise(out)
  out <- dplyr::transmute(
    out,
    name = format_name_badge(.data[["path"]]),
    lifecycle = format_lifecycle_badge(.data[["lifecycle"]]),
    status = format_status_badge(.data[["path"]], .data[["ci_check"]]),
    coverage = format_coverage_badge(.data[["path"]]),
    version = format_version_badge(.data[["path"]]),
    maintainer = format_maintainer_badge(.data[["path"]])
  )
  dplyr::ungroup(out)
}

#' Generate a table with workflow information
#'
#' @param repo_vectors A list of named character vectors. Each character vector
#' should contain:
#' * path - the shortpath to the GitHub repository, including organization
#'  (e.g. "RMI-PACTA/workflow.transition.monitor")
#' * lifecycle - the lifecycle badge status (e.g. "stable", "experimental")
#' * ci_check - the name of the CI check file
#'  (e.g. "build-Docker-image-triggers.yml")
#'
#' @return A formatted sitrep tibble.
#'
#' @export
#'
#' @examples
#'
#' repos <- list(
#'   c(
#'     path = "RMI-PACTA/workflow.transition.monitor",
#'     lifecycle = "stable",
#'     ci_check = "build-Docker-image-triggers.yml"
#'   ),
#'   c(
#'     path = "RMI-PACTA/workflow.data.preparation",
#'     lifecycle = "stable",
#'     ci_check = "docker.yml"
#'   )
#' )
#'
#' generate_workflow_table(repos)
generate_workflow_table <- function(repo_vectors) {
  out <- parse_inputs(repo_vectors)

  out <- dplyr::rowwise(out)
  out <- dplyr::transmute(
    out,
    name = format_name_badge(.data[["path"]]),
    lifecycle = format_lifecycle_badge(.data[["lifecycle"]]),
    status = format_status_badge(.data[["path"]], .data[["ci_check"]]),
    maintainer = format_maintainer_badge(.data[["path"]])
  )
  dplyr::ungroup(out)
}

check_inputs <- function(repo_vectors) {
  stopifnot(
    is.list(repo_vectors),
    all(purrr::map_lgl(repo_vectors, is.character)),
    all(purrr::map_lgl(repo_vectors, ~ length(.x) == 3L)),
    all(
      purrr::map_lgl(
        repo_vectors,
        ~ all(names(.x) %in% c("path", "lifecycle", "ci_check"))
      )
    )
  )
}

parse_inputs <- function(repo_vectors) {
  check_inputs(repo_vectors)
  purrr::map_dfr(repo_vectors, ~ tibble::as_tibble(as.list(.)))
}
