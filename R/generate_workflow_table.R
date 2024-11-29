#' Generate a table with workflow information
#'
#' @param repo_paths A character vector with GH paths to the workflow
#'   repositories
#'
#' @return A tibble with the following columns:
#' * Repo
#' * Lifecycle
#' * Docker Status
#' * Latest_SHA
#' * Maintainer
#'
#' @export
#'
#' @examples
#' generate_workflow_table("RMI-PACTA/workflow.transition.monitor")
generate_workflow_table <- function(repo_paths) {
  out <- tibble::tibble(
    repo = repo_paths,
    lifecycle = purrr::map_chr(repo_paths, table_lifecycle),
    docker = purrr::map_chr(repo_paths, table_docker),
    sha = purrr::map_chr(repo_paths, table_latest_sha),
    maintainer = purrr::map_chr(repo_paths, table_maintainer)
  )

  dplyr::transmute(
    out,
    Repo = format_repo_v(.data[["repo"]]),
    Lifecycle = format_lifecycle_v(.data[["lifecycle"]]),
    Docker_Status = format_docker_v(.data[["docker"]]),
    Latest_SHA = format_latest_sha_v(.data[["repo"]], .data[["sha"]]),
    Maintainer = format_maintainer_v(.data[["maintainer"]])
  )
}

format_docker <- function(docker_status) {

  if (is.na(docker_status)) {
    return("No Docker check found.")
  }

  docker_status
}

format_docker_v <- Vectorize(format_docker)

table_docker <- function(repo_path) {
  readme <- get_gh_text_file(repo_path, file_path = "README.md")

  docker_action_names <- "docker.yml|build-Docker-image-triggers.yml"

  docker_status <- readme[grepl(docker_action_names, readme)]

  return(docker_status)
}
