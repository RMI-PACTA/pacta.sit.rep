get_codeowner <- function(
    repo_fullname,
    path = ".github/CODEOWNERS", # nolint: non_portable_path
    format = TRUE) {
  content <- get_gh_text_file(repo_fullname, file_path = path)
  if (is.null(content)) {
    return(NULL)
  }
  default_owner <- grep(
    pattern = "^\\s*\\*\\s+@\\S+",
    x = content,
    value = TRUE
  )
  default_owner_clean <- gsub(
    pattern = "^\\s*\\*\\s+@|\\s*$",
    replacement = "",
    x = default_owner
  )
  if (format) {
    default_owner_clean <- paste0("@", default_owner_clean)
  }
  return(default_owner_clean)
}
if (requireNamespace("memoise")) {
  get_codeowner <- memoise::memoise(get_codeowner)
}


get_codeowner_errors <- function(repo_fullname) {
  raw_content <- tryCatch(
    expr = {
      raw_content <- gh::gh(
        "GET /repos/{repo}/codeowners/errors", # nolint: non_portable_path
        repo = repo_fullname
      )
    },
    error = function(e) {
      NA_character_
    }
  )
  if (identical(raw_content, NA_character_)) {
    # Early return
    return(NA_integer_)
  }
  errors <- raw_content[["errors"]]
  return(length(errors))
}
if (requireNamespace("memoise")) {
  get_codeowner_errors <- memoise::memoise(get_codeowner_errors)
}
