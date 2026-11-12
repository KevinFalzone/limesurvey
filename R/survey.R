#' Create a New LimeSurvey Survey
#'
#' Creates a new survey on the configured LimeSurvey instance by replaying
#' the admin web UI's "Create survey" action, then sets the survey's
#' language, question/group format, and title through the same settings
#' panels the UI itself uses.
#'
#' @param title A character string: the survey's title.
#' @param language A character string: the survey's base language code
#'   (default `"en"`).
#' @param format A character string: one of `"G"` (group by group), `"S"`
#'   (question by question), or `"A"` (all in one page). Default `"G"`.
#'
#' @return Invisibly, the integer survey id (`sid`) of the newly created
#'   survey.
#'
#' @export
ls_create_survey <- function(title, language = "en", format = "G") {
  if (!is.character(title) || length(title) != 1 || is.na(title) || !nzchar(title)) {
    stop("'title' must be a single non-empty character string.", call. = FALSE)
  }
  if (!is.character(language) || length(language) != 1 || is.na(language) || !nzchar(language)) {
    stop("'language' must be a single non-empty character string.", call. = FALSE)
  }
  if (!is.character(format) || length(format) != 1 || !format %in% c("G", "S", "A")) {
    stop("'format' must be one of \"G\", \"S\", or \"A\".", call. = FALSE)
  }

  sid <- ls_new_survey()
  ls_update_general_settings(sid, language = language, format = format)
  ls_update_title(sid, title = title, language = language)

  invisible(sid)
}

#' @importFrom httr2 req_perform resp_url
ls_new_survey <- function() {
  resp <- req_perform(ls_request("/index.php/surveyAdministration/newSurvey"))
  url <- resp_url(resp)
  sid <- regmatches_first(url, "surveyid=([0-9]+)")
  if (is.na(sid)) {
    stop("Could not determine the new survey id from the response.", call. = FALSE)
  }
  as.integer(sid)
}

#' @importFrom httr2 req_body_form req_perform
ls_update_general_settings <- function(sid, language, format) {
  csrf <- ls_csrf_fetch(sprintf(
    "/index.php/surveyAdministration/rendersidemenulink?subaction=generalsettings&surveyid=%d",
    sid
  ))

  req <- req_body_form(
    ls_request(
      "/index.php/admin/database/index/updatesurveylocalesettings_generalsettings",
      csrf_cookie = csrf$cookie
    ),
    YII_CSRF_TOKEN = csrf$token,
    language = language,
    owner_id = -1,
    admin = "",
    adminemail = "",
    bounce_email = "",
    format = format,
    gsid = 1,
    template = "inherit",
    action = "updatesurveylocalesettings_generalsettings",
    sid = sid,
    surveyid = sid,
    responsejson = 1
  )
  invisible(req_perform(req))
}

#' @importFrom httr2 req_body_form req_perform
ls_update_title <- function(sid, title, language) {
  csrf <- ls_csrf_fetch(sprintf(
    "/index.php/surveyAdministration/rendersidemenulink?subaction=surveytexts&surveyid=%d",
    sid
  ))

  fields <- list(
    YII_CSRF_TOKEN = csrf$token,
    action = "updatesurveylocalesettings",
    sid = sid,
    surveyid = sid,
    responsejson = 1
  )
  fields[[paste0("short_title_", language)]] <- title

  req <- req_body_form(
    ls_request(
      "/index.php/admin/database/index/updatesurveylocalesettings",
      csrf_cookie = csrf$cookie
    ),
    !!!fields
  )
  invisible(req_perform(req))
}

#' Delete a LimeSurvey Survey
#'
#' Permanently deletes a survey from the configured LimeSurvey instance by
#' replaying the admin web UI's "Delete" action. This also deletes the
#' survey's questions, groups, and any responses already collected.
#'
#' @param sid An integer (or integer-coercible) survey id.
#'
#' @return Invisibly, the integer survey id that was deleted.
#'
#' @export
ls_delete_survey <- function(sid) {
  sid <- ls_validate_id(sid, "sid")

  csrf <- ls_csrf_fetch("/index.php/surveyAdministration/listsurveys")

  req <- req_body_form(
    ls_request(
      "/index.php/surveyAdministration/delete",
      csrf_cookie = csrf$cookie
    ),
    YII_CSRF_TOKEN = csrf$token,
    iSurveyID = sid,
    delete = "yes"
  )
  req_perform(req)

  invisible(sid)
}

#' Copy a LimeSurvey Survey
#'
#' Copies an existing survey on the configured LimeSurvey instance by
#' replaying the admin web UI's "Copy" action, including its question
#' groups, questions, answer options, quotas, permissions, and resource
#' files.
#'
#' @param sid An integer (or integer-coercible) id of the survey to copy.
#' @param title A character string: the new survey's title. If `NULL`
#'   (default), LimeSurvey assigns `"<original title> - Copy"`.
#'
#' @return Invisibly, the integer survey id (`sid`) of the new copy.
#'
#' @export
ls_copy_survey <- function(sid, title = NULL) {
  sid <- ls_validate_id(sid, "sid")
  if (!is.null(title) && (!is.character(title) || length(title) != 1 || is.na(title) || !nzchar(title))) {
    stop("'title' must be NULL or a single non-empty character string.", call. = FALSE)
  }

  csrf <- ls_csrf_fetch("/index.php/surveyAdministration/listsurveys")

  req <- req_body_form(
    ls_request(
      "/index.php/surveyAdministration/copy",
      csrf_cookie = csrf$cookie
    ),
    YII_CSRF_TOKEN = csrf$token,
    surveyIdToCopy = sid,
    copysurveytitle = if (is.null(title)) "" else title,
    copysurveyid = "",
    copyResourcesAndLinks = 1,
    copySurveyQuotas = 1,
    copySurveyPermissions = 1,
    copyAnswerOptions = 1,
    copySurveyUrlParameters = 1,
    copySurveyConditions = 1,
    copyStartEndDate = 1,
    resetResponseStartId = 1
  )
  resp <- req_perform(req)
  html <- resp_body_html(resp)

  onclick <- html_attr(html_element(html, "input[onclick*=\"iSurveyID=\"]"), "onclick")
  new_sid <- regmatches_first(onclick, "iSurveyID=([0-9]+)")
  if (is.na(new_sid)) {
    stop("Could not determine the copied survey id from the response.", call. = FALSE)
  }

  invisible(as.integer(new_sid))
}

ls_validate_id <- function(x, name = "sid") {
  if (!is.numeric(x) || length(x) != 1 || is.na(x) || x != as.integer(x)) {
    stop(sprintf("'%s' must be a single integer id.", name), call. = FALSE)
  }
  as.integer(x)
}

regmatches_first <- function(x, pattern) {
  m <- regmatches(x, regexpr(pattern, x, perl = TRUE))
  if (length(m) == 0 || !nzchar(m)) {
    return(NA_character_)
  }
  sub(pattern, "\\1", m, perl = TRUE)
}
