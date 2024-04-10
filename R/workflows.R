get_workflows <- function(repo_fullname) {
  listing <- get_gh_dir_listing(
    repo_fullname,
    dir_path = ".github/workflows" # nolint: non_portable_path
  )

  contents <- list()
  for (i in seq_along(listing)) {
    file_content <- get_gh_text_file(repo_fullname, listing[[i]][["path"]])
    filename <- listing[[i]][["name"]]
    contents[filename] <- list(file_content)
  }
  return(contents)
}
if (requireNamespace("memoise")) {
  get_workflows <- memoise::memoise(get_workflows)
}

parse_workflow <- function(workflow) {
  workflow_list <- yaml::yaml.load(workflow)

  # {yaml} interprets `on:` as `TRUE`
  triggers <- workflow_list[[TRUE]]
  jobs <- workflow_list[["jobs"]]
  rmi_actions <- vapply(
    X = jobs,
    FUN = function(job) {
      if (is.null(job[["uses"]])) {
        return(NA_character_)
      } else {
        out <- grep(
          pattern = "RMI-PACTA/actions", # nolint: non_portable_path
          x = job[["uses"]],
          value = TRUE
        )
        if (length(out) == 0L) {
          return(NA_character_)
        }
        return(out)
      }
    },
    FUN.VALUE = character(1L)
  )
  uses_rmi_actions <- !all(is.null(rmi_actions))
  return(
    list(
      triggers = triggers,
      rmi_actions = rmi_actions,
      uses_rmi_actions = uses_rmi_actions
    )
  )
}

check_workflows <- function(repo_fullname) {
  workflow_definitions <- get_workflows(repo_fullname)
  lapply(
    workflow_definitions,
    parse_workflow
  )
}

workflow_summary <- function(
  repo_fullname,
  expected = c(
    "R.yml",
    "docker.yml"
  )
) {
  workflows <- check_workflows(repo_fullname)
  workflow_summary <- list()
  for (wf in expected) {
    if (is.null(workflows[wf])) {
      is_expected <- FALSE
      details <- NULL
    } else {
      if (
        !("push" %in% names(workflows[[wf]][["triggers"]])) ||
          is.null(workflows[[wf]][["triggers"]][["push"]])
      ) {
        checks_main <- FALSE
      } else {
        checks_main <- "main" %in% workflows[[wf]][["triggers"]][["push"]]
      }
      checks_prs <- "pull_request" %in% names(workflows[[wf]][["triggers"]])
      standard_checks <- checks_main && checks_prs
      uses_correct_rmi_actions <- grepl(
        pattern = wf,
        x = workflows[[wf]][["rmi_actions"]],
        fixed = TRUE
      )
      is_expected <- standard_checks && uses_correct_rmi_actions
      details <- list(
        checks_main = checks_main,
        checks_prs = checks_prs,
        standard_checks = standard_checks,
        uses_correct_rmi_actions = uses_correct_rmi_actions
      )
    }
    workflow_summary[[wf]] <- list(
      standard = is_expected,
      details = details
    )
  }
  workflow_summary[["all_standard"]] <- all(
    vapply(
      X = workflow_summary,
      FUN = function(x) x[["standard"]],
      FUN.VALUE = logical(1L)
    )
  )
  return(workflow_summary)
}
