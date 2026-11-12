ls_env <- new.env(parent = emptyenv())

#' Set the LimeSurvey Session
#'
#' Stores the instance URL and authentication token reused by the other
#' `ls_*` functions.
#'
#' @param url A character string representing the LimeSurvey instance URL.
#' @param token A character string representing the LimeSurvey session token.
#'
#' @return Invisibly, the session list.
#'
#' @export
ls_set_session <- function(url, token) {
  if (!is.character(url) || length(url) != 1 || is.na(url) || !nzchar(url)) {
    stop("'url' must be a single non-empty character string.", call. = FALSE)
  }

  if (!is.character(token) || length(token) != 1 || is.na(token) || !nzchar(token)) {
    stop("'token' must be a single non-empty character string.", call. = FALSE)
  }

  if (!grepl("/$", url)) {
    url <- paste0(url, "/")
  }

  session <- list(url = url, token = token)
  assign("session", session, envir = ls_env)
  invisible(session)
}

ls_get_session <- function() {
  if (!exists("session", envir = ls_env)) {
    stop("No session set. Call ls_set_session() first.", call. = FALSE)
  }
  get("session", envir = ls_env)
}

#' @importFrom utils modifyList
ls_resolve_session <- function(overrides) {
  session <- ls_get_session()
  unknown <- setdiff(names(overrides), names(session))
  if (length(unknown) > 0) {
    stop("Unknown parameter(s): ", paste(unknown, collapse = ", "), call. = FALSE)
  }
  modifyList(session, overrides)
}
