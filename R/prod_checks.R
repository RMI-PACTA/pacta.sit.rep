prod_checks <- function(repo_json) {
  if (is.null(repo_json[["codeowner"]])) {
    codeowner <- get_codeowner(repo_json[["full_name"]])
    repo_json[["codeowner"]] <- codeowner
  }
  if (is.null(repo_json[["codeowner_errors"]])) {
    codeowner_errors <- get_codeowner_errors(repo_json[["full_name"]])
    repo_json[["codeowner_errors"]] <- codeowner_errors
  }
  if (is.null(repo_json[["gh_deps"]])) {
    gh_deps <- identify_gh_deps(repo_json[["full_name"]])
    repo_json[["gh_deps"]] <- gh_deps
  }
  if (is.null(repo_json[["dep_tree"]])) {
    dep_tree <- identify_if_dep(repo_json[["full_name"]])
    repo_json[["dep_tree"]] <- dep_tree
  }
  if (is.null(repo_json[["lifecycle_badge"]])) {
    lifecycle_badge <- format_lifecycle(
      table_lifecycle(repo_json[["full_name"]])
    )
    repo_json[["lifecycle_badge"]] <- lifecycle_badge
  }
  if (is.null(repo_json[["has_docker"]])) {
    repo_json[["has_docker"]] <- !is.null(
      get_gh_text_file(repo_json[["full_name"]], "Dockerfile")
    )
  }
  if (is.null(repo_json[["has_R"]])) {
    repo_json[["has_R"]] <- !is.null(
      get_gh_dir_listing(repo_json[["full_name"]], "R")
    )
  }
  expected_workflows <- NULL
  if (repo_json[["has_R"]]) {
    expected_workflows <- c(expected_workflows, "R.yml")
  }
  if (repo_json[["has_docker"]]) {
    expected_workflows <- c(expected_workflows, "docker.yml")
  }
  if (is.null(repo_json[["workflows"]])) {
    workflows <- workflow_summary(repo_json[["full_name"]])
    repo_json[["workflows"]] <- workflows
  }
  if (is.null(repo_json[["enabled_rules"]])) {
    enabled_rules <- check_actions_rulesets(repo_json[["full_name"]])
    repo_json[["enabled_rules"]] <- enabled_rules
  }
  return(repo_json)
}
if (requireNamespace("memoise")) {
  prod_checks <- memoise::memoise(prod_checks)
}
