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
    lifecycle_badge <- format_lifecycle(table_lifecycle(repo_json[["full_name"]]))
    repo_json[["lifecycle_badge"]] <- lifecycle_badge
  }
  return(repo_json)
}
if (requireNamespace("memoise")) {
  prod_checks <- memoise::memoise(prod_checks)
}