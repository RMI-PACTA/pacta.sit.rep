identify_gh_deps <- function(repo_fullname) {
  deps <- tryCatch(
    expr = {
      pak::pkg_deps(repo_fullname)
    },
    error = function(e) {
      NULL
    }
  )
  if (is.null(deps)) {
    return(NA_character_)
  }
  gh_deps <- deps[["type"]] == "github"
  gh_dep_refs <- deps[gh_deps, "ref"]
  return(gh_dep_refs)
}
if (requireNamespace("memoise")) {
  identify_gh_deps <- memoise::memoise(identify_gh_deps)
}

identify_if_dep <- function(
  repo_fullname,
  repos_to_check = list(
    prod = c(prod_workflows, r2dii.repos),
    experimental = experimental_workflows
  ),
  return_max = TRUE
) {

  dep_lists <- lapply(repos_to_check, function(repos) {
    deps <- lapply(
      X = repos,
      FUN = identify_gh_deps
    )
    clean_deps <- unique(unlist(deps))
    return(clean_deps)
  })

  dep_levels <- NULL
  for (i in seq_along(dep_lists)) {
    if (
      repo_fullname %in% dep_lists[[i]] ||
        repo_fullname %in% repos_to_check[[i]]
    ) {
      dep_levels <- c(names(dep_lists)[i], dep_levels)
    }
  }
  if (is.null(dep_levels)) {
    dep_levels <- NA_character_
  }

  dep_levels_factor <- factor(
    x = dep_levels,
    levels = c("experimental", "prod"),
    ordered = TRUE
  )

  if (return_max) {
    return(max(dep_levels_factor))
  } else {
    return(dep_levels_factor)
  }
}
