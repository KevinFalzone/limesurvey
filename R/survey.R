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
#' @param remove_default_group A logical: whether to delete the "My first
#'   question group" question group LimeSurvey seeds every new survey
#'   with. Default `TRUE`.
#'
#' @return Invisibly, the integer survey id (`sid`) of the newly created
#'   survey.
#'
#' @export
ls_create_survey <- function(
  title,
  language = "en",
  format = "G",
  remove_default_group = TRUE
) {
  if (
    !is.character(title) || length(title) != 1 || is.na(title) || !nzchar(title)
  ) {
    stop("'title' must be a single non-empty character string.", call. = FALSE)
  }
  if (
    !is.character(language) ||
      length(language) != 1 ||
      is.na(language) ||
      !nzchar(language)
  ) {
    stop(
      "'language' must be a single non-empty character string.",
      call. = FALSE
    )
  }
  if (
    !is.character(format) ||
      length(format) != 1 ||
      !format %in% c("G", "S", "A")
  ) {
    stop("'format' must be one of \"G\", \"S\", or \"A\".", call. = FALSE)
  }
  if (!is.logical(remove_default_group) || length(remove_default_group) != 1) {
    stop(
      "'remove_default_group' must be a single logical value.",
      call. = FALSE
    )
  }

  sid <- ls_new_survey()
  ls_update_survey_settings_general(sid, language = language, format = format)
  ls_update_title(sid, title = title, language = language)

  if (isTRUE(remove_default_group)) {
    default_gid <- unique(ls_get_questions(sid)$group_id)
    if (length(default_gid) == 1) {
      ls_delete_group(sid, as.integer(default_gid))
    }
  }

  invisible(sid)
}

#' @importFrom httr2 req_perform resp_url
ls_new_survey <- function() {
  resp <- req_perform(ls_request("/index.php/surveyAdministration/newSurvey"))
  url <- resp_url(resp)
  sid <- regmatches_first(url, "surveyid=([0-9]+)")
  if (is.na(sid)) {
    stop(
      "Could not determine the new survey id from the response.",
      call. = FALSE
    )
  }
  as.integer(sid)
}

#' Update a Survey's General Settings
#'
#' Updates a survey's base language, additional languages, and
#' question/group format, by replaying the admin web UI's "General
#' settings" panel. Can be used right after [ls_create_survey()] or on an
#' already-created survey.
#'
#' @param sid An integer (or integer-coercible) survey id.
#' @param language A character string: the survey's base language code.
#' @param format A character string: one of `"G"` (group by group), `"S"`
#'   (question by question), or `"A"` (all in one page). Default `"G"`.
#' @param additional_languages A character vector of additional language
#'   codes the survey should also be translatable into (e.g. `c("en")`).
#'   Default `NULL` (no additional languages).
#' @param old_additional_languages A character vector of the survey's
#'   *current* additional language codes, if any. LimeSurvey uses this to
#'   detect which languages were removed, so it can clean up their
#'   question texts; pass the survey's existing additional languages here
#'   when it already has some, so none are unintentionally dropped.
#'   Default `NULL` (assumes none yet, e.g. right after
#'   [ls_create_survey()]).
#'
#' @return Invisibly, `sid`.
#'
#' @importFrom httr2 req_body_form req_perform
#'
#' @export
ls_update_survey_settings_general <- function(
  sid,
  language,
  format = "G",
  additional_languages = NULL,
  old_additional_languages = NULL
) {
  sid <- ls_validate_id(sid, "sid")
  if (
    !is.character(language) ||
      length(language) != 1 ||
      is.na(language) ||
      !nzchar(language)
  ) {
    stop(
      "'language' must be a single non-empty character string.",
      call. = FALSE
    )
  }
  if (
    !is.character(format) ||
      length(format) != 1 ||
      !format %in% c("G", "S", "A")
  ) {
    stop("'format' must be one of \"G\", \"S\", or \"A\".", call. = FALSE)
  }
  if (
    !is.null(additional_languages) &&
      (!is.character(additional_languages) || anyNA(additional_languages))
  ) {
    stop(
      "'additional_languages' must be NULL or a character vector.",
      call. = FALSE
    )
  }
  if (
    !is.null(old_additional_languages) &&
      (!is.character(old_additional_languages) ||
        anyNA(old_additional_languages))
  ) {
    stop(
      "'old_additional_languages' must be NULL or a character vector.",
      call. = FALSE
    )
  }

  csrf <- ls_csrf_fetch(sprintf(
    "/index.php/surveyAdministration/rendersidemenulink?subaction=generalsettings&surveyid=%d",
    sid
  ))

  fields <- list(
    YII_CSRF_TOKEN = csrf$token,
    oldlanguages = paste(old_additional_languages, collapse = " "),
    language = language,
    admin = "",
    adminbutton = "N",
    adminemail = "",
    adminemailbutton = "N",
    bounce_email = "",
    bounce_emailbutton = "N",
    gsid = 1,
    format = format,
    template = "inherit",
    action = "updatesurveylocalesettings_generalsettings",
    sid = sid,
    surveyid = sid,
    responsejson = 1
  )
  if (!is.null(additional_languages)) {
    additional_language_fields <- as.list(additional_languages)
    names(additional_language_fields) <- rep(
      "additional_languages[]",
      length(additional_languages)
    )
    fields <- c(fields, additional_language_fields)
  }

  req <- req_body_form(
    ls_request(
      "/index.php/admin/database/index/updatesurveylocalesettings_generalsettings",
      csrf_cookie = csrf$cookie
    ),
    !!!fields
  )
  req_perform(req)

  invisible(sid)
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

#' Update a Survey's Per-Language Texts
#'
#' Updates a survey's title, alias, description, welcome/end messages, end
#' URL, and date/number format, for one or more languages at once, by
#' replaying the admin web UI's "Text elements" panel. LimeSurvey's own
#' form posts these fields for every one of the survey's languages in a
#' single request, so pass every language whose current texts you want to
#' keep; a survey language left out of `texts` may have its texts reset
#' to their format defaults.
#'
#' @param sid An integer (or integer-coercible) survey id.
#' @param texts A named list keyed by language code (e.g.
#'   `list(fr = list(title = "...", welcome = "..."), en = list(title =
#'   "..."))`). Each element is itself a named list with any of: `title`,
#'   `alias`, `description`, `welcome`, `end_text`, `url`,
#'   `url_description` (default `""`), `date_format` (default `"1"`),
#'   `number_format` (default `"0"`).
#'
#' @return Invisibly, `sid`.
#'
#' @importFrom httr2 req_body_form req_perform
#'
#' @export
ls_update_survey_settings_texts <- function(sid, texts) {
  sid <- ls_validate_id(sid, "sid")
  if (
    !is.list(texts) ||
      length(texts) == 0 ||
      is.null(names(texts)) ||
      any(!nzchar(names(texts)))
  ) {
    stop(
      "'texts' must be a non-empty named list, keyed by language code.",
      call. = FALSE
    )
  }
  if (!all(vapply(texts, is.list, logical(1)))) {
    stop("Every element of 'texts' must itself be a list.", call. = FALSE)
  }

  field_defaults <- list(
    title = "",
    alias = "",
    description = "",
    welcome = "",
    end_text = "",
    url = "",
    url_description = "",
    date_format = "1",
    number_format = "0"
  )
  field_suffixes <- c(
    title = "short_title",
    alias = "alias",
    description = "description",
    welcome = "welcome",
    end_text = "endtext",
    url = "url",
    url_description = "urldescrip",
    date_format = "dateformat",
    number_format = "numberformat"
  )

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
  for (language in names(texts)) {
    settings <- modifyList(field_defaults, texts[[language]])
    for (field_name in names(field_defaults)) {
      fields[[sprintf(
        "%s_%s",
        field_suffixes[[field_name]],
        language
      )]] <- settings[[field_name]]
    }
  }

  req <- req_body_form(
    ls_request(
      "/index.php/admin/database/index/updatesurveylocalesettings",
      csrf_cookie = csrf$cookie
    ),
    !!!fields
  )
  req_perform(req)

  invisible(sid)
}

#' Update a Survey's Data Security (GDPR) Settings
#'
#' Updates a survey's data security policy notice visibility and
#' per-language policy label/text/error message, by replaying the admin
#' web UI's "Data security" panel.
#'
#' @param sid An integer (or integer-coercible) survey id.
#' @param show_policy_notice A character string controlling when the
#'   data security policy notice is shown to participants; inspect the
#'   admin UI's "Data security" panel for the current set of options.
#'   Default `"0"`.
#' @param show_token_policy A character string: one of `"I"` (inherit the
#'   global setting), `"Y"`, or `"N"`. Default `"I"`.
#' @param show_register_policy A character string: one of `"I"` (inherit
#'   the global setting), `"Y"`, or `"N"`. Default `"I"`.
#' @param texts A named list keyed by language code (e.g.
#'   `list(fr = list(label = "...", notice = "..."))`). Each element is
#'   itself a named list with any of: `label`, `notice`, `error` (all
#'   default `""`). As with [ls_update_survey_settings_texts()], LimeSurvey's own
#'   form posts these fields for every one of the survey's languages at
#'   once, so pass every language whose current text you want to keep.
#'   Default `list()` (no language-specific text sent).
#'
#' @return Invisibly, `sid`.
#'
#' @importFrom httr2 req_body_form req_perform
#'
#' @export
ls_update_survey_settings_data_security <- function(
  sid,
  show_policy_notice = "0",
  show_token_policy = "I",
  show_register_policy = "I",
  texts = list()
) {
  sid <- ls_validate_id(sid, "sid")
  if (
    !is.character(show_policy_notice) ||
      length(show_policy_notice) != 1 ||
      is.na(show_policy_notice) ||
      !nzchar(show_policy_notice)
  ) {
    stop(
      "'show_policy_notice' must be a single non-empty character string.",
      call. = FALSE
    )
  }
  if (
    !is.character(show_token_policy) ||
      length(show_token_policy) != 1 ||
      !show_token_policy %in% c("I", "Y", "N")
  ) {
    stop(
      "'show_token_policy' must be one of \"I\", \"Y\", or \"N\".",
      call. = FALSE
    )
  }
  if (
    !is.character(show_register_policy) ||
      length(show_register_policy) != 1 ||
      !show_register_policy %in% c("I", "Y", "N")
  ) {
    stop(
      "'show_register_policy' must be one of \"I\", \"Y\", or \"N\".",
      call. = FALSE
    )
  }
  if (!is.list(texts)) {
    stop("'texts' must be a list.", call. = FALSE)
  }
  if (
    length(texts) > 0 &&
      (is.null(names(texts)) || any(!nzchar(names(texts))))
  ) {
    stop(
      "'texts' must be a named list, keyed by language code.",
      call. = FALSE
    )
  }
  if (!all(vapply(texts, is.list, logical(1)))) {
    stop("Every element of 'texts' must itself be a list.", call. = FALSE)
  }

  field_defaults <- list(label = "", notice = "", error = "")
  field_suffixes <- c(
    label = "dataseclabel",
    notice = "datasec",
    error = "datasecerror"
  )

  csrf <- ls_csrf_fetch(sprintf(
    "/index.php/surveyAdministration/rendersidemenulink?subaction=datasecurity&surveyid=%d",
    sid
  ))

  fields <- list(
    YII_CSRF_TOKEN = csrf$token,
    showsurveypolicynotice = show_policy_notice,
    showtokenpolicy = show_token_policy,
    showregisterpolicy = show_register_policy
  )
  for (language in names(texts)) {
    settings <- modifyList(field_defaults, texts[[language]])
    for (field_name in names(field_defaults)) {
      fields[[sprintf(
        "%s_%s",
        field_suffixes[[field_name]],
        language
      )]] <- settings[[field_name]]
    }
  }
  fields$action <- "updatesurveylocalesettings"
  fields$sid <- sid
  fields$surveyid <- sid
  fields$responsejson <- 1

  req <- req_body_form(
    ls_request(
      "/index.php/admin/database/index/updatesurveylocalesettings",
      csrf_cookie = csrf$cookie
    ),
    !!!fields
  )
  req_perform(req)

  invisible(sid)
}

#' Update a Survey's Presentation and Navigation Settings
#'
#' Updates a survey's presentation and navigation options (question
#' index, progress bar, "No answer" handling, page navigation delay,
#' public statistics, etc.), by replaying the admin web UI's
#' "Presentation & navigation" panel.
#'
#' @param sid An integer (or integer-coercible) survey id.
#' @param show_no_answer A character string: one of `"I"` (inherit the
#'   global setting), `"Y"`, or `"N"`. Default `"I"`.
#' @param preselect_no_answer A character string: one of `"I"` (inherit
#'   the global setting), `"Y"`, or `"N"`. Default `"I"`.
#' @param show_x_questions A character string: one of `"I"` (inherit the
#'   global setting), `"Y"`, or `"N"`. Default `"I"`.
#' @param show_group_info A character string: one of `"I"` (inherit the
#'   global setting), `"Y"`, or `"N"`. Default `"I"`.
#' @param show_question_number_code A character string: one of `"I"`
#'   (inherit the global setting), `"Y"`, or `"N"`. Default `"I"`.
#' @param show_welcome A character string: one of `"I"` (inherit the
#'   global setting), `"Y"`, or `"N"`. Default `"I"`.
#' @param show_progress A character string: one of `"I"` (inherit the
#'   global setting), `"Y"`, or `"N"`. Default `"I"`.
#' @param auto_redirect A character string: one of `"I"` (inherit the
#'   global setting), `"Y"`, or `"N"`. Default `"I"`.
#' @param allow_prev A character string: one of `"I"` (inherit the
#'   global setting), `"Y"`, or `"N"`. Default `"I"`.
#' @param print_answers A character string: one of `"I"` (inherit the
#'   global setting), `"Y"`, or `"N"`. Default `"I"`.
#' @param public_statistics A character string: one of `"I"` (inherit
#'   the global setting), `"Y"`, or `"N"`. Default `"I"`.
#' @param public_graphs A character string: one of `"I"` (inherit the
#'   global setting), `"Y"`, or `"N"`. Default `"I"`.
#' @param question_index A character string controlling the question
#'   index/jump menu; inspect the admin UI's "Presentation & navigation"
#'   panel for the current set of options. Default `"0"` (disabled).
#' @param navigation_delay A character string: the page navigation delay
#'   in seconds, only applied when `navigation_delay_custom = "Y"`.
#'   Default `"-1"`.
#' @param navigation_delay_custom A character string: `"Y"` to use
#'   `navigation_delay`, or `"N"` to use the global default delay.
#'   Default `"N"`.
#'
#' @return Invisibly, `sid`.
#'
#' @importFrom httr2 req_body_form req_perform
#'
#' @export
ls_update_survey_settings_presentation <- function(
  sid,
  show_no_answer = "I",
  preselect_no_answer = "I",
  show_x_questions = "I",
  show_group_info = "I",
  show_question_number_code = "I",
  show_welcome = "I",
  show_progress = "I",
  question_index = "0",
  navigation_delay = "-1",
  navigation_delay_custom = "N",
  auto_redirect = "I",
  allow_prev = "I",
  print_answers = "I",
  public_statistics = "I",
  public_graphs = "I"
) {
  sid <- ls_validate_id(sid, "sid")

  iyn_args <- list(
    show_no_answer = show_no_answer,
    preselect_no_answer = preselect_no_answer,
    show_x_questions = show_x_questions,
    show_group_info = show_group_info,
    show_question_number_code = show_question_number_code,
    show_welcome = show_welcome,
    show_progress = show_progress,
    auto_redirect = auto_redirect,
    allow_prev = allow_prev,
    print_answers = print_answers,
    public_statistics = public_statistics,
    public_graphs = public_graphs
  )
  for (arg_name in names(iyn_args)) {
    value <- iyn_args[[arg_name]]
    if (
      !is.character(value) || length(value) != 1 || !value %in% c("I", "Y", "N")
    ) {
      stop(
        sprintf("'%s' must be one of \"I\", \"Y\", or \"N\".", arg_name),
        call. = FALSE
      )
    }
  }
  if (
    !is.character(navigation_delay_custom) ||
      length(navigation_delay_custom) != 1 ||
      !navigation_delay_custom %in% c("Y", "N")
  ) {
    stop("'navigation_delay_custom' must be \"Y\" or \"N\".", call. = FALSE)
  }
  if (
    !is.character(question_index) ||
      length(question_index) != 1 ||
      is.na(question_index) ||
      !nzchar(question_index)
  ) {
    stop(
      "'question_index' must be a single non-empty character string.",
      call. = FALSE
    )
  }
  if (
    !is.character(navigation_delay) ||
      length(navigation_delay) != 1 ||
      is.na(navigation_delay) ||
      !nzchar(navigation_delay)
  ) {
    stop(
      "'navigation_delay' must be a single non-empty character string.",
      call. = FALSE
    )
  }

  csrf <- ls_csrf_fetch(sprintf(
    "/index.php/surveyAdministration/rendersidemenulink?subaction=presentation&surveyid=%d",
    sid
  ))

  req <- req_body_form(
    ls_request(
      "/index.php/admin/database/index/updatesurveylocalesettings",
      csrf_cookie = csrf$cookie
    ),
    YII_CSRF_TOKEN = csrf$token,
    shownoanswer = show_no_answer,
    preselectnoanswer = preselect_no_answer,
    showxquestions = show_x_questions,
    showgroupinfo = show_group_info,
    showqnumcode = show_question_number_code,
    showwelcome = show_welcome,
    showprogress = show_progress,
    questionindex = question_index,
    navigationdelay = navigation_delay,
    navigationdelaybutton = navigation_delay_custom,
    autoredirect = auto_redirect,
    allowprev = allow_prev,
    printanswers = print_answers,
    publicstatistics = public_statistics,
    publicgraphs = public_graphs,
    action = "updatesurveylocalesettings",
    sid = sid,
    surveyid = sid,
    responsejson = 1
  )
  req_perform(req)

  invisible(sid)
}

#' Update a Survey's Participants (Tokens) Settings
#'
#' Updates a survey's participant table options - anonymization, answer
#' persistence across tokens, self-registration, confirmation emails,
#' etc. - by replaying the admin web UI's "Survey participants" panel.
#' Note that `show_token_policy` and `show_register_policy` update the
#' same underlying survey columns as [ls_update_survey_settings_data_security()]; both
#' panels expose them.
#'
#' @param sid An integer (or integer-coercible) survey id.
#' @param anonymized A character string: one of `"I"` (inherit the
#'   global setting), `"Y"`, or `"N"`. Default `"I"`.
#' @param show_token_policy A character string: one of `"I"` (inherit
#'   the global setting), `"Y"`, or `"N"`. Default `"I"`.
#' @param token_answers_persistence A character string: one of `"I"`
#'   (inherit the global setting), `"Y"`, or `"N"`. Default `"I"`.
#' @param allow_edit_after_completion A character string: one of `"I"`
#'   (inherit the global setting), `"Y"`, or `"N"`. Default `"I"`.
#' @param allow_register A character string: one of `"I"` (inherit the
#'   global setting), `"Y"`, or `"N"`. Default `"I"`.
#' @param show_register_policy A character string: one of `"I"` (inherit
#'   the global setting), `"Y"`, or `"N"`. Default `"I"`.
#' @param html_email A character string: one of `"I"` (inherit the
#'   global setting), `"Y"`, or `"N"`. Default `"I"`.
#' @param send_confirmation A character string: one of `"I"` (inherit
#'   the global setting), `"Y"`, or `"N"`. Default `"I"`.
#' @param token_length A character string: the generated token length,
#'   only applied when `token_length_custom = "Y"`. Default `"-1"`.
#' @param token_length_custom A character string: `"Y"` to use
#'   `token_length`, or `"N"` to use the global default length. Default
#'   `"N"`.
#'
#' @return Invisibly, `sid`.
#'
#' @importFrom httr2 req_body_form req_perform
#'
#' @export
ls_update_survey_settings_participants <- function(
  sid,
  anonymized = "I",
  show_token_policy = "I",
  token_answers_persistence = "I",
  allow_edit_after_completion = "I",
  token_length = "-1",
  token_length_custom = "N",
  allow_register = "I",
  show_register_policy = "I",
  html_email = "I",
  send_confirmation = "I"
) {
  sid <- ls_validate_id(sid, "sid")

  iyn_args <- list(
    anonymized = anonymized,
    show_token_policy = show_token_policy,
    token_answers_persistence = token_answers_persistence,
    allow_edit_after_completion = allow_edit_after_completion,
    allow_register = allow_register,
    show_register_policy = show_register_policy,
    html_email = html_email,
    send_confirmation = send_confirmation
  )
  for (arg_name in names(iyn_args)) {
    value <- iyn_args[[arg_name]]
    if (
      !is.character(value) || length(value) != 1 || !value %in% c("I", "Y", "N")
    ) {
      stop(
        sprintf("'%s' must be one of \"I\", \"Y\", or \"N\".", arg_name),
        call. = FALSE
      )
    }
  }
  if (
    !is.character(token_length_custom) ||
      length(token_length_custom) != 1 ||
      !token_length_custom %in% c("Y", "N")
  ) {
    stop("'token_length_custom' must be \"Y\" or \"N\".", call. = FALSE)
  }
  if (
    !is.character(token_length) ||
      length(token_length) != 1 ||
      is.na(token_length) ||
      !nzchar(token_length)
  ) {
    stop(
      "'token_length' must be a single non-empty character string.",
      call. = FALSE
    )
  }

  csrf <- ls_csrf_fetch(sprintf(
    "/index.php/surveyAdministration/rendersidemenulink?subaction=tokens&surveyid=%d",
    sid
  ))

  req <- req_body_form(
    ls_request(
      "/index.php/admin/database/index/updatesurveylocalesettings",
      csrf_cookie = csrf$cookie
    ),
    YII_CSRF_TOKEN = csrf$token,
    anonymized = anonymized,
    showtokenpolicy = show_token_policy,
    tokenanswerspersistence = token_answers_persistence,
    alloweditaftercompletion = allow_edit_after_completion,
    tokenlength = token_length,
    tokenlengthbutton = token_length_custom,
    allowregister = allow_register,
    showregisterpolicy = show_register_policy,
    htmlemail = html_email,
    sendconfirmation = send_confirmation,
    action = "updatesurveylocalesettings",
    sid = sid,
    surveyid = sid,
    responsejson = 1
  )
  req_perform(req)

  invisible(sid)
}

#' Update a Survey's Notification and Data Management Settings
#'
#' Updates a survey's notification and data management options (date
#' stamping, IP address logging/anonymization, referrer URL, response
#' timings, quota-exit saving, assessments, partial-response saving,
#' notification/response email recipients, Google Analytics), by
#' replaying the admin web UI's "Notification & data management" panel.
#'
#' @param sid An integer (or integer-coercible) survey id.
#' @param date_stamp A character string: one of `"I"` (inherit the
#'   global setting), `"Y"`, or `"N"`. Default `"I"`.
#' @param ip_addr A character string: one of `"I"` (inherit the global
#'   setting), `"Y"`, or `"N"`. Default `"I"`.
#' @param ip_anonymize A character string: one of `"I"` (inherit the
#'   global setting), `"Y"`, or `"N"`. Default `"I"`.
#' @param ref_url A character string: one of `"I"` (inherit the global
#'   setting), `"Y"`, or `"N"`. Default `"I"`.
#' @param save_timings A character string: one of `"I"` (inherit the
#'   global setting), `"Y"`, or `"N"`. Default `"I"`.
#' @param save_quota_exit A character string: one of `"I"` (inherit the
#'   global setting), `"Y"`, or `"N"`. Default `"I"`.
#' @param assessments A character string: one of `"I"` (inherit the
#'   global setting), `"Y"`, or `"N"`. Default `"I"`.
#' @param allow_save A character string: one of `"I"` (inherit the
#'   global setting), `"Y"`, or `"N"`. Default `"I"`.
#' @param email_notification_to A character string: the email
#'   address(es) to notify on each response, or `"inherit"` to use the
#'   global default. Default `"inherit"`.
#' @param email_notification_to_custom A character string: `"Y"` to use
#'   `email_notification_to`, or `"N"` to use the global default.
#'   Default `"N"`.
#' @param email_response_to A character string: the email address(es) to
#'   send full response data to, or `"inherit"` to use the global
#'   default. Default `"inherit"`.
#' @param email_response_to_custom A character string: `"Y"` to use
#'   `email_response_to`, or `"N"` to use the global default. Default
#'   `"N"`.
#' @param google_analytics_api_key A character string: one of `"I"`
#'   (inherit the global setting), `"Y"` (use a survey-specific key), or
#'   `"N"` (don't use Google Analytics). Default `"N"`.
#' @param google_analytics_style A character string controlling the
#'   Google Analytics tracking style; inspect the admin UI's
#'   "Notification & data management" panel for the current set of
#'   options. Default `"0"`.
#'
#' @return Invisibly, `sid`.
#'
#' @importFrom httr2 req_body_form req_perform
#'
#' @export
ls_update_survey_settings_notification <- function(
  sid,
  date_stamp = "I",
  ip_addr = "I",
  ip_anonymize = "I",
  ref_url = "I",
  save_timings = "I",
  save_quota_exit = "I",
  assessments = "I",
  allow_save = "I",
  email_notification_to = "inherit",
  email_notification_to_custom = "N",
  email_response_to = "inherit",
  email_response_to_custom = "N",
  google_analytics_api_key = "N",
  google_analytics_style = "0"
) {
  sid <- ls_validate_id(sid, "sid")

  iyn_args <- list(
    date_stamp = date_stamp,
    ip_addr = ip_addr,
    ip_anonymize = ip_anonymize,
    ref_url = ref_url,
    save_timings = save_timings,
    save_quota_exit = save_quota_exit,
    assessments = assessments,
    allow_save = allow_save,
    google_analytics_api_key = google_analytics_api_key
  )
  for (arg_name in names(iyn_args)) {
    value <- iyn_args[[arg_name]]
    if (
      !is.character(value) || length(value) != 1 || !value %in% c("I", "Y", "N")
    ) {
      stop(
        sprintf("'%s' must be one of \"I\", \"Y\", or \"N\".", arg_name),
        call. = FALSE
      )
    }
  }
  yn_args <- list(
    email_notification_to_custom = email_notification_to_custom,
    email_response_to_custom = email_response_to_custom
  )
  for (arg_name in names(yn_args)) {
    value <- yn_args[[arg_name]]
    if (!is.character(value) || length(value) != 1 || !value %in% c("Y", "N")) {
      stop(sprintf("'%s' must be \"Y\" or \"N\".", arg_name), call. = FALSE)
    }
  }
  if (
    !is.character(email_notification_to) ||
      length(email_notification_to) != 1 ||
      is.na(email_notification_to) ||
      !nzchar(email_notification_to)
  ) {
    stop(
      "'email_notification_to' must be a single non-empty character string.",
      call. = FALSE
    )
  }
  if (
    !is.character(email_response_to) ||
      length(email_response_to) != 1 ||
      is.na(email_response_to) ||
      !nzchar(email_response_to)
  ) {
    stop(
      "'email_response_to' must be a single non-empty character string.",
      call. = FALSE
    )
  }
  if (
    !is.character(google_analytics_style) ||
      length(google_analytics_style) != 1 ||
      is.na(google_analytics_style) ||
      !nzchar(google_analytics_style)
  ) {
    stop(
      "'google_analytics_style' must be a single non-empty character string.",
      call. = FALSE
    )
  }

  csrf <- ls_csrf_fetch(sprintf(
    "/index.php/surveyAdministration/rendersidemenulink?subaction=notification&surveyid=%d",
    sid
  ))

  req <- req_body_form(
    ls_request(
      "/index.php/admin/database/index/updatesurveylocalesettings",
      csrf_cookie = csrf$cookie
    ),
    YII_CSRF_TOKEN = csrf$token,
    datestamp = date_stamp,
    ipaddr = ip_addr,
    ipanonymize = ip_anonymize,
    refurl = ref_url,
    savetimings = save_timings,
    savequotaexit = save_quota_exit,
    assessments = assessments,
    allowsave = allow_save,
    emailnotificationto = email_notification_to,
    emailnotificationtobutton = email_notification_to_custom,
    emailresponseto = email_response_to,
    emailresponsetobutton = email_response_to_custom,
    googleanalyticsapikeysetting = google_analytics_api_key,
    googleanalyticsstyle = google_analytics_style,
    action = "updatesurveylocalesettings",
    sid = sid,
    surveyid = sid,
    responsejson = 1
  )
  req_perform(req)

  invisible(sid)
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
  if (
    !is.null(title) &&
      (!is.character(title) ||
        length(title) != 1 ||
        is.na(title) ||
        !nzchar(title))
  ) {
    stop(
      "'title' must be NULL or a single non-empty character string.",
      call. = FALSE
    )
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

  onclick <- html_attr(
    html_element(html, "input[onclick*=\"iSurveyID=\"]"),
    "onclick"
  )
  new_sid <- regmatches_first(onclick, "iSurveyID=([0-9]+)")
  if (is.na(new_sid)) {
    stop(
      "Could not determine the copied survey id from the response.",
      call. = FALSE
    )
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
