#' Create or Update a Question Group
#'
#' Creates a new question group, or updates an existing one when `gid` is
#' given, by replaying the admin web UI's question group editor form.
#'
#' @param sid An integer (or integer-coercible) survey id.
#' @param name A character string: the question group's name.
#' @param gid An integer (or integer-coercible) question group id. If
#'   `NULL` (default), a new question group is created; otherwise the
#'   existing group identified by `gid` is updated.
#' @param description A character string: the question group's
#'   description. Default `NULL` (empty).
#' @param language A character string: the language code the `name` and
#'   `description` apply to (default `"en"`).
#' @param condition A character string: the group's relevance equation,
#'   used to make the group conditional on earlier answers (default `1`,
#'   i.e. always shown).
#'
#' @return Invisibly, the integer question group id (`gid`), carrying
#'   `sid` as an attribute so it can be piped straight into
#'   [ls_save_question()] or another [ls_save_group()]
#'   call, e.g. `sid |> ls_save_group(name = "A") |>
#'   ls_save_question(code = "Q1", text = "...")`.
#'
#' @export
ls_save_group <- function(sid, name, gid = NULL, description = NULL, language = "en", condition = 1) {
  sid <- if (!is.null(attr(sid, "sid"))) attr(sid, "sid") else sid
  sid <- ls_validate_id(sid, "sid")
  if (!is.character(name) || length(name) != 1 || is.na(name) || !nzchar(name)) {
    stop("'name' must be a single non-empty character string.", call. = FALSE)
  }
  if (!is.null(gid)) {
    gid <- ls_validate_id(gid, "gid")
  }
  if (!is.character(language) || length(language) != 1 || is.na(language) || !nzchar(language)) {
    stop("'language' must be a single non-empty character string.", call. = FALSE)
  }
  if (length(condition) != 1 || is.na(condition) || !nzchar(trimws(as.character(condition)))) {
    stop("'condition' must be a single non-empty relevance equation.", call. = FALSE)
  }

  csrf_path <- if (is.null(gid)) {
    sprintf("/index.php/questionGroupsAdministration/add?surveyid=%d", sid)
  } else {
    sprintf("/index.php/questionGroupsAdministration/edit/surveyid/%d/gid/%d", sid, gid)
  }
  csrf <- ls_csrf_fetch(csrf_path)

  fields <- list(YII_CSRF_TOKEN = csrf$token, `questionGroup[sid]` = sid)
  if (!is.null(gid)) {
    fields[["questionGroup[gid]"]] <- gid
  }
  fields[[sprintf("questionGroupI10N[%s][group_name]", language)]] <- name
  fields[[sprintf("questionGroupI10N[%s][description]", language)]] <- if (is.null(description)) "" else description
  fields[["questionGroup[randomization_group]"]] <- ""
  fields[["questionGroup[grelevance]"]] <- as.character(condition)

  req <- req_body_form(
    ls_request(
      sprintf("/index.php/questionGroupsAdministration/saveQuestionGroupData/sid/%d", sid),
      csrf_cookie = csrf$cookie
    ),
    !!!fields
  )
  resp <- req_perform(req)

  new_gid <- regmatches_first(resp_url(resp), "gid=([0-9]+)")
  if (is.na(new_gid)) {
    stop("Could not determine the question group id from the response.", call. = FALSE)
  }
  invisible(structure(as.integer(new_gid), sid = sid))
}
