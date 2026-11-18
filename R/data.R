utils::globalVariables(c(
  "id",
  "class",
  "group_id",
  "name",
  "path",
  "question_id"
))

#' Carry the Last Non-Empty Value Forward
#'
#' Fills `""` elements with the closest preceding non-`""` value, used to
#' attach each question's enclosing group id (only present on the group's
#' own row in the structure export) to the questions that follow it.
#'
#' @noRd
ls_fill_down <- function(x) {
  for (i in seq_along(x)[-1]) {
    if (x[i] == "") {
      x[i] <- x[i - 1]
    }
  }
  x
}

#' Retrieve Survey Questions from LimeSurvey
#'
#' Retrieves the list of questions for a survey, from the LimeSurvey
#' instance set with [ls_set_session()], by replaying the admin web UI's
#' "Export structure" action.
#'
#' @param sid An integer (or integer-coercible) survey id.
#'
#' @return A tibble with one row per question (including subquestions, e.g.
#'   each item of a multiple choice question), with columns `group_id`,
#'   `id`, `name`, and `path` (a `colselect[]` column identifier, see
#'   [ls_get_data()]): `"Q{id}"` for a question, `"Q{parent_id}_S{id}"` for
#'   a subquestion.
#'
#' @importFrom dplyr filter if_else mutate select
#' @importFrom httr2 req_perform resp_body_string resp_header
#' @importFrom readr read_tsv
#'
#' @export
ls_get_questions <- function(sid) {
  sid <- ls_validate_id(sid, "sid")

  req <- ls_request(
    sprintf(
      "/index.php/admin/export/sa/survey/action/exportstructuretsv/surveyid/%d",
      sid
    ),
    ajax = FALSE,
    csrf_cookie = "YII_CSRF_TOKEN"
  )
  resp <- req_perform(req)

  content_type <- resp_header(resp, "content-type")
  if (!grepl("text/tab-separated-values", content_type, fixed = TRUE)) {
    stop(
      sprintf("Request failed. Not a tsv file (%s).", content_type),
      call. = FALSE
    )
  }

  structure_tsv <- read_tsv(resp_body_string(resp), show_col_types = FALSE) |>
    filter(!is.na(id), class != "A")
  structure_tsv$group_id <- ls_fill_down(ifelse(
    structure_tsv$class == "G",
    structure_tsv$id,
    ""
  ))
  structure_tsv$question_id <- ls_fill_down(ifelse(
    structure_tsv$class == "Q",
    structure_tsv$id,
    ""
  ))

  structure_tsv |>
    filter(class != "G") |>
    mutate(
      path = if_else(
        class == "SQ",
        paste0("Q", question_id, "_S", id),
        paste0("Q", id)
      )
    ) |>
    select(group_id, id, name, path)
}

#' Retrieve Survey Data from LimeSurvey
#'
#' Retrieves survey response data, from the LimeSurvey instance set with
#' [ls_set_session()], by replaying the admin web UI's "Export responses"
#' action.
#'
#' @param sid An integer (or integer-coercible) survey id.
#' @param export_from An integer: the first response number to export.
#' @param export_to An integer: the last response number to export.
#' @param completionstate A character string: one of `"all"`, `"complete"`,
#'   or `"incomplete"`.
#' @param headstyle A character string controlling how questions are
#'   named in the exported columns. One of:
#'   * `"code"` the question code
#'   * `"abbreviated"` the abbreviated question text
#'   * `"full"` the full question text
#'   * `"codetext"` the code and full question text
#' @param columns A character vector of column identifiers to export, each
#'   in the format `"Q{id}"` for a question or `"Q{parent_id}_S{id}"` for a
#'   subquestion. If `NULL` (default), every question is exported; see
#'   [ls_get_questions()] to build a subset.
#' @param language A character string: the language the exported question
#'   labels are rendered in (default `"en"`).
#'
#' @return A tibble of response data, with one row per response and one
#'   column per exported field.
#'
#' @importFrom dplyr pull
#' @importFrom httr2 req_body_form req_perform resp_body_string resp_header
#' @importFrom readr read_csv
#'
#' @export
ls_get_data <- function(
  sid,
  export_from,
  export_to,
  completionstate,
  headstyle,
  columns = NULL,
  language = "en"
) {
  sid <- ls_validate_id(sid, "sid")

  if (
    !is.numeric(export_from) ||
      length(export_from) != 1 ||
      is.na(export_from) ||
      export_from %% 1 != 0 ||
      export_from <= 0
  ) {
    stop("'export_from' must be a single positive integer.", call. = FALSE)
  }
  if (
    !is.numeric(export_to) ||
      length(export_to) != 1 ||
      is.na(export_to) ||
      export_to %% 1 != 0 ||
      export_to <= 0
  ) {
    stop("'export_to' must be a single positive integer.", call. = FALSE)
  }
  if (export_from > export_to) {
    stop(
      "'export_from' must be less than or equal to 'export_to'.",
      call. = FALSE
    )
  }
  if (
    !is.character(completionstate) ||
      length(completionstate) != 1 ||
      !completionstate %in% c("all", "complete", "incomplete")
  ) {
    stop(
      "'completionstate' must be one of \"all\", \"complete\", or \"incomplete\".",
      call. = FALSE
    )
  }
  if (
    !is.character(headstyle) ||
      length(headstyle) != 1 ||
      !headstyle %in% c("code", "abbreviated", "full", "codetext")
  ) {
    stop(
      "'headstyle' must be one of \"code\", \"abbreviated\", \"full\", or \"codetext\".",
      call. = FALSE
    )
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
  if (!is.null(columns)) {
    if (!is.character(columns) || length(columns) == 0) {
      stop(
        "'columns' must be a non-empty character vector of column identifiers, or NULL.",
        call. = FALSE
      )
    }
    if (any(!grepl("^Q\\d+(_S\\d+)?$", columns))) {
      stop(
        paste0(
          "All elements in 'columns' must be in the format \"Q{id}\" or ",
          "\"Q{parent_id}_S{id}\", where id/parent_id are question ids."
        ),
        call. = FALSE
      )
    }
  } else {
    columns <- ls_get_questions(sid) |> pull(path)
  }

  default_columns <- c(
    "id",
    "submitdate",
    "lastpage",
    "startlanguage",
    "seed",
    "startdate",
    "datestamp",
    "ipaddr",
    "refurl"
  )
  names(default_columns) <- rep("colselect[]", length(default_columns))
  names(columns) <- rep("colselect[]", length(columns))

  fields <- c(
    list(
      YII_CSRF_TOKEN = "YII_CSRF_TOKEN",
      type = "csv",
      csvfieldseparator = ",",
      completionstate = completionstate,
      exportlang = language,
      export_from = export_from,
      export_to = export_to,
      answers = "long",
      convertyto = "1",
      convertnto = "2",
      maskequations = "Y",
      headstyle = headstyle,
      striphtmlcode = "1",
      headspacetounderscores = "0",
      abbreviatedtext = "0",
      emcode = "0",
      abbreviatedtextto = "15",
      codetextseparator = ".+",
      sid = sid
    ),
    as.list(default_columns),
    as.list(columns)
  )

  req <- req_body_form(
    ls_request(
      sprintf("/index.php/admin/export/sa/exportresults/surveyid/%d", sid),
      ajax = FALSE,
      csrf_cookie = "YII_CSRF_TOKEN"
    ),
    !!!fields
  )
  resp <- req_perform(req)

  content_type <- resp_header(resp, "content-type")
  if (!grepl("text/comma-separated-values", content_type, fixed = TRUE)) {
    stop(
      sprintf("Request failed. Not a csv file (%s).", content_type),
      call. = FALSE
    )
  }

  read_csv(resp_body_string(resp), show_col_types = FALSE)
}
