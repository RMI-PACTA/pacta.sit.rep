get_repos <- function(org) {
  repo_fetch <- gh::gh(
    "GET /orgs/{org}/repos", # nolint: non_portable_path
    org = org, per_page  = 20L)
  all_repos <- repo_fetch
  while (
    grepl(
      pattern = "rel=\"next\"",
      x = attr(repo_fetch, "response")[["link"]],
      fixed = TRUE
    )
  ) {
    repo_fetch <- gh::gh_next(repo_fetch)
    all_repos <- c(all_repos, repo_fetch)
  }
  return(all_repos)
}
if (requireNamespace("memoise")) {
  get_repos <- memoise::memoise(get_repos)
}
