get_codeowner <- function(
  repo_fullname,
  path = ".github/CODEOWNERS" # nolint: non_portable_path
) {
  raw_content <- tryCatch(
    expr = {
      raw_content <- gh::gh(
        "GET /repos/{repo}/contents/{path}", # nolint: non_portable_path
        repo = repo_fullname,
        path = path
      )
    },
    error = function(e) {
      NA_character_
    }
  )
  if (identical(raw_content, NA_character_)) {
    # Early return
    return(NA_character_)
  }
  content_b64 <- raw_content[["content"]]
  content_char <- rawToChar(base64enc::base64decode(content_b64))
  content_words <- strsplit(content_char, "\n", fixed = TRUE)[[1L]]
  default_owner <- grep(
    pattern = "^\\s*\\*\\s+@\\S+",
    x = content_words,
    value = TRUE
  )
  default_owner_clean <- gsub(
    pattern = "^\\s*\\*\\s+@|\\s*$",
    replacement = "",
    x = default_owner
  )
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
