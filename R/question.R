#' LimeSurvey Question Types
#'
#' The question type codes and their default theme, as offered by the
#' admin web UI's question type selector. Some type codes support more
#' than one theme (e.g. list-radio vs. image-select); this table only
#' keeps each type's default/canonical theme. Pass `question_theme_name`
#' explicitly to [limesurvey_save_question()] to use a different one.
#'
#' @noRd
limesurvey_question_types <- c(
  ";" = "arrays/texts",
  ":" = "arrays/multiflexi",
  "!" = "list_dropdown",
  "*" = "equation",
  "|" = "file_upload",
  "1" = "arrays/dualscale",
  "5" = "5pointchoice",
  "A" = "arrays/5point",
  "B" = "arrays/10point",
  "C" = "arrays/yesnouncertain",
  "D" = "date",
  "E" = "arrays/increasesamedecrease",
  "F" = "arrays/array",
  "G" = "gender",
  "H" = "arrays/column",
  "I" = "language",
  "K" = "multiplenumeric",
  "L" = "listradio",
  "M" = "multiplechoice",
  "N" = "numerical",
  "O" = "list_with_comment",
  "P" = "multiplechoice_with_comments",
  "Q" = "multipleshorttext",
  "R" = "ranking",
  "S" = "shortfreetext",
  "T" = "longfreetext",
  "U" = "hugefreetext",
  "X" = "boilerplate",
  "Y" = "yesno"
)

#' Create or Update a Question
#'
#' Creates a new question in a question group, or updates an existing one
#' when `qid` is given, by replaying the admin web UI's question editor
#' form.
#'
#' @param sid An integer (or integer-coercible) survey id.
#' @param gid An integer (or integer-coercible) question group id the
#'   question belongs to.
#' @param qid An integer (or integer-coercible) question id. If `NULL`
#'   (default), a new question is created; otherwise the existing
#'   question identified by `qid` is updated.
#' @param code A character string: the question's code (e.g. `"Q1"`).
#' @param type A character string: the LimeSurvey question type code
#'   (default `"T"`, long free text). One of:
#'   * `";"` Array (Texts)
#'   * `":"` Array (Numbers)
#'   * `"!"` List (dropdown)
#'   * `"*"` Equation
#'   * `"|"` File upload
#'   * `"1"` Array dual scale
#'   * `"5"` 5 point choice
#'   * `"A"` Array (5 point choice)
#'   * `"B"` Array (10 point choice)
#'   * `"C"` Array (Yes/Uncertain/No)
#'   * `"D"` Date/Time
#'   * `"E"` Array (Increase/Same/Decrease)
#'   * `"F"` Array
#'   * `"G"` Gender
#'   * `"H"` Array by column
#'   * `"I"` Language switch
#'   * `"K"` Multiple numerical input
#'   * `"L"` List (radio)
#'   * `"M"` Multiple choice
#'   * `"N"` Numerical input
#'   * `"O"` List with comment
#'   * `"P"` Multiple choice with comments
#'   * `"Q"` Multiple short text
#'   * `"R"` Ranking
#'   * `"S"` Short free text
#'   * `"T"` Long free text
#'   * `"U"` Huge free text
#'   * `"X"` Text display (boilerplate)
#'   * `"Y"` Yes/No
#' @param text A character string: the question text.
#' @param question_theme_name A character string: the question theme to
#'   use. If `NULL` (default), the canonical theme for `type` is used;
#'   pass this explicitly to pick an alternate theme for types that offer
#'   more than one (e.g. `"image_select-listradio"` for type `"L"`).
#' @param help A character string: the question's help text. Default
#'   `NULL` (empty).
#' @param language A character string: the language code `text`
#'   and `help` apply to (default `"en"`).
#' @param answers A character vector of answer option texts, in order.
#'   Required for question types that offer a fixed set of choices (e.g.
#'   `"L"` list radio, `"M"` multiple choice, `"O"` list with comment).
#'   Name the elements to set explicit answer codes (e.g.
#'   `c(red = "Rouge", green = "Vert")`); codes must be unique,
#'   alphanumeric, and at most 5 characters. Unnamed elements are coded
#'   `"A1"`, `"A2"`, etc. Default `NULL` (no answer options).
#' @param general_settings A named list mirroring the admin UI's
#'   "General settings" panel. Recognized keys: `mandatory` (one of
#'   `"Y"`, `"S"` soft, or `"N"`, default `"N"`), `relevance` (the
#'   question's relevance equation, default `1`, i.e. always shown),
#'   `encrypted` (`"Y"` or `"N"`, default `"N"`), `preg` (a validation
#'   regular expression). Unrecognized keys are passed through as-is.
#' @param logic,display,input,other,timer,statistics Named lists
#'   mirroring the admin UI's "Advanced settings" accordion panels of the
#'   same name. Available keys vary by `type`; inspect the question
#'   editor in the admin UI to see which ones apply. Nest a list to
#'   target a localized sub-field, e.g.
#'   `logic = list(em_validation_q_tip = list(en = "Hint"))`. Values are
#'   passed through as-is.
#'
#' @return Invisibly, the integer question id (`qid`), carrying `sid`
#'   and `gid` as attributes so it can be piped straight into another
#'   [limesurvey_save_question()] call (same group) or into
#'   [limesurvey_save_group()] (new group, same survey), e.g.
#'   `sid |> limesurvey_save_group(name = "A") |>
#'   limesurvey_save_question(code = "Q1", text = "...") |>
#'   limesurvey_save_question(code = "Q2", text = "...") |>
#'   limesurvey_save_group(name = "B") |>
#'   limesurvey_save_question(code = "Q3", text = "...")`.
#'
#' @export
limesurvey_save_question <- function(sid, gid = NULL, qid = NULL, code, type = "T", text,
                                      question_theme_name = NULL,
                                      help = NULL, language = "en", answers = NULL,
                                      general_settings = list(),
                                      logic = list(), display = list(), input = list(),
                                      other = list(), timer = list(), statistics = list()) {
  if (!is.null(attr(sid, "gid"))) {
    gid <- attr(sid, "gid")
    sid <- attr(sid, "sid")
  } else if (!is.null(attr(sid, "sid"))) {
    gid <- as.integer(sid)
    sid <- attr(sid, "sid")
  }
  sid <- limesurvey_validate_id(sid, "sid")
  gid <- limesurvey_validate_id(gid, "gid")
  if (!is.null(qid)) {
    qid <- limesurvey_validate_id(qid, "qid")
  }
  if (!is.character(code) || length(code) != 1 || is.na(code) || !nzchar(code)) {
    stop("'code' must be a single non-empty character string.", call. = FALSE)
  }
  if (!is.character(text) || length(text) != 1 || is.na(text) || !nzchar(text)) {
    stop("'text' must be a single non-empty character string.", call. = FALSE)
  }
  if (!is.character(type) || length(type) != 1 || is.na(type) || !type %in% names(limesurvey_question_types)) {
    stop(
      "'type' must be one of: ", paste(names(limesurvey_question_types), collapse = ", "),
      call. = FALSE
    )
  }
  if (is.null(question_theme_name)) {
    question_theme_name <- limesurvey_question_types[[type]]
  }

  advanced_settings <- list(
    general_settings = general_settings, logic = logic, display = display,
    input = input, other = other, timer = timer, statistics = statistics
  )
  for (arg_name in names(advanced_settings)) {
    if (!is.list(advanced_settings[[arg_name]])) {
      stop(sprintf("'%s' must be a list.", arg_name), call. = FALSE)
    }
  }

  general_settings <- modifyList(
    list(mandatory = "N", relevance = "1", encrypted = "N"),
    general_settings
  )
  if (!is.null(general_settings$mandatory) && !general_settings$mandatory %in% c("Y", "S", "N")) {
    stop("general_settings$mandatory must be one of \"Y\", \"S\", or \"N\".", call. = FALSE)
  }
  if (!is.null(general_settings$encrypted) && !general_settings$encrypted %in% c("Y", "N")) {
    stop("general_settings$encrypted must be one of \"Y\" or \"N\".", call. = FALSE)
  }
  if (!is.null(general_settings$relevance) &&
        (length(general_settings$relevance) != 1 || is.na(general_settings$relevance) ||
           !nzchar(trimws(as.character(general_settings$relevance))))) {
    stop("general_settings$relevance must be a single non-empty relevance equation.", call. = FALSE)
  }

  answer_codes <- NULL
  if (!is.null(answers)) {
    if (!is.character(answers) || length(answers) == 0 || anyNA(answers) || any(!nzchar(answers))) {
      stop("'answers' must be a non-empty character vector of answer option texts.", call. = FALSE)
    }
    answer_codes <- names(answers)
    if (is.null(answer_codes)) {
      answer_codes <- character(length(answers))
    }
    missing_code <- !nzchar(answer_codes)
    answer_codes[missing_code] <- sprintf("A%d", which(missing_code))
    if (any(!grepl("^[a-zA-Z0-9]{1,5}$", answer_codes))) {
      stop("'answers' codes must be alphanumeric and at most 5 characters.", call. = FALSE)
    }
    if (anyDuplicated(answer_codes) != 0) {
      stop("'answers' codes must be unique.", call. = FALSE)
    }
  }

  csrf_path <- if (is.null(qid)) {
    sprintf("/index.php/questionAdministration/create?surveyid=%d&gid=%d", sid, gid)
  } else {
    sprintf("/index.php/questionAdministration/view?surveyid=%d&gid=%d&qid=%d", sid, gid, qid)
  }
  csrf <- limesurvey_csrf_fetch(csrf_path)

  fields <- list(
    YII_CSRF_TOKEN = csrf$token,
    sid = sid,
    `question[qid]` = if (is.null(qid)) 0 else qid,
    tabOverviewEditor = "overview",
    `question[title]` = code,
    questionTypeSelector = type,
    `question[type]` = type,
    `question[question_theme_name]` = question_theme_name,
    `question[gid]` = gid,
    bFullPOST = 1
  )
  fields[[sprintf("questionI10N[%s][question]", language)]] <- text
  fields[[sprintf("questionI10N[%s][help]", language)]] <- if (is.null(help)) "" else help

  fields <- c(fields, limesurvey_flatten_fields("question", general_settings))
  for (category in c("logic", "display", "input", "other", "timer", "statistics")) {
    settings <- advanced_settings[[category]]
    if (length(settings) > 0) {
      fields <- c(fields, limesurvey_flatten_fields(sprintf("advancedSettings[%s]", category), settings))
    }
  }

  if (!is.null(answers)) {
    keys <- paste0("a", seq_along(answers))
    fields[["answeroptions_sortorder"]] <- paste(keys, collapse = ",")
    for (i in seq_along(answers)) {
      fields[[sprintf("answeroptions[%s][0][code]", keys[i])]] <- answer_codes[i]
      fields[[sprintf("answeroptions[%s][0][answeroptionl10n][%s]", keys[i], language)]] <- answers[[i]]
    }
  }

  req <- req_body_form(
    limesurvey_request("/index.php/questionAdministration/saveQuestionData", csrf_cookie = csrf$cookie),
    !!!fields
  )
  resp <- req_perform(req)

  new_qid <- regmatches_first(resp_url(resp), "questionId=([0-9]+)")
  if (is.na(new_qid)) {
    stop("Could not determine the question id from the response.", call. = FALSE)
  }
  invisible(structure(as.integer(new_qid), sid = sid, gid = gid))
}

#' Flatten a Named List into Bracketed Form Field Names
#'
#' Turns `list(a = 1, b = list(c = 2))` into
#' `list("prefix[a]" = 1, "prefix[b][c]" = 2)`, matching the bracketed
#' array naming LimeSurvey's admin forms use (e.g. `advancedSettings[logic][...]`).
#'
#' @noRd
limesurvey_flatten_fields <- function(prefix, x) {
  fields <- list()
  for (key in names(x)) {
    value <- x[[key]]
    path <- sprintf("%s[%s]", prefix, key)
    if (is.list(value)) {
      fields <- c(fields, limesurvey_flatten_fields(path, value))
    } else {
      fields[[path]] <- value
    }
  }
  fields
}
