get_rulesets <- function(repo_fullname) {
  response <- tryCatch(
    expr = {
      gh::gh(
        "/repos/{repo_fullname}/rulesets", # nolint: non_portable_path
        repo_fullname = repo_fullname
      )
    },
    error = function(cond) {
      return(NULL)
    }
  )
  if (is.null(response)) {
    out <- NULL
  } else {
    out <- vapply(
      X = response,
      FUN = function(ruleset) {
        get_ruleset_contents(repo_fullname, ruleset[["id"]])
      },
      FUN.VALUE = list(1L)
    )
  }
  return(out)
}
if (requireNamespace("memoise")) {
  get_rulesets <- memoise::memoise(get_rulesets)
}

get_ruleset_contents <- function(repo_fullname, ruleset_id) {
  response <- tryCatch(
    expr = {
      gh::gh(
        "/repos/{repo_fullname}/rulesets/{ruleset_id}", # nolint: non_portable_path
        repo_fullname = repo_fullname,
        ruleset_id = ruleset_id
      )
    },
    error = function(cond) {
      return(NULL)
    }
  )
  if (is.null(response)) {
    out <- NULL
  } else {
    out <- list(
      list(
        id = response[["id"]],
        name = response[["name"]],
        target = response[["target"]],
        enforcement = response[["enforcement"]],
        conditions = response[["conditions"]],
        rules = response[["rules"]]
      )
    )
  }
  return(out)
}
if (requireNamespace("memoise")) {
  get_ruleset_contents <- memoise::memoise(get_ruleset_contents)
}

check_actions_rulesets <- function(repo_fullname) {
  ruleset_repo <- "RMI-PACTA/actions" # nolint: non_portable_path
  rulesets <- get_rulesets(repo_fullname)
  if (is.null(rulesets)) {
    return(NULL)
  }
  expected_rules <- get_gh_dir_listing(
    repo_fullname = ruleset_repo,
    "rulesets"
  )
  expected <- list()
  for (i in seq_along(expected_rules)) {
    file_content <- get_gh_text_file(
      repo_fullname = ruleset_repo,
      expected_rules[[i]][["path"]]
    )
    filename <- expected_rules[[i]][["name"]]
    expected[filename] <- list(
      jsonlite::fromJSON(
        txt = file_content,
        simplifyDataFrame = FALSE,
        simplifyVector = FALSE
      )
    )
  }
  enabled_rulesets <- vapply(
    X = expected,
    FUN = function(expected_ruleset) {
      any(vapply(
        X = rulesets,
        FUN = function(ruleset) {
          list_is_subset(expected_ruleset, ruleset)
        },
        FUN.VALUE = logical(1L)
      ))
    },
    FUN.VALUE = logical(1L)
  )
  return(as.list(enabled_rulesets))
}
