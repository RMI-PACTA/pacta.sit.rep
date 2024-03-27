get_gh_text_file <- function(repo_fullname, file_path) {
  response <- tryCatch(
    expr = {
      gh::gh(
        "/repos/{repo_fullname}/contents/{file_path}", # nolint: non_portable_path
        repo_fullname = repo_fullname,
        file_path = file_path
      )
    },
    error = function(cond) return(NULL)
  )
  if (is.null(response)) {
    out <- NULL
  } else if (response[["content"]] == "") {
    # files larger than 1MB are not embedded in content
    out <- readLines(
      response[["download_url"]]
    )
  } else {
    out <- strsplit(
      x = rawToChar(
        base64enc::base64decode(
          response[["content"]]
        )
      ),
      split = "\n",
      fixed = TRUE
    )[[1L]]
  }
  return(out)
}
if (requireNamespace("memoise")) {
  get_gh_text_file <- memoise::memoise(get_gh_text_file)
}

get_gh_dir_listing <- function(repo_fullname, dir_path) {
  response <- tryCatch(
    expr = {
      gh::gh(
        "/repos/{repo_fullname}/contents/{dir_path}", # nolint: non_portable_path
        repo_fullname = repo_fullname,
        dir_path = dir_path
      )
    },
    error = function(cond) return(NULL)
  )
  if (is.null(response)) {
    out <- NULL
  } else {
    out <- lapply(
      response,
      function(x) {
        list(
        name = x[["name"]],
        path = x[["path"]]
        )
      }
    )
  }
  return(out)
}
