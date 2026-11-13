#' Get the Connected LimeSurvey Instance's Version
#'
#' Retrieves the version of the LimeSurvey instance configured through
#' [ls_set_session()] by reading it off the admin footer, where LimeSurvey
#' displays it next to a link to the LimeSurvey Community website (e.g.
#' `"7.9.1+261102"`).
#'
#' @return A character string: the LimeSurvey version.
#'
#' @export
#' @importFrom httr2 req_perform resp_body_html
#' @importFrom rvest html_element html_text2
ls_get_version <- function() {
  resp <- req_perform(ls_request("/index.php/surveyAdministration/listsurveys"))
  html <- resp_body_html(resp)

  footer <- html_element(
    html,
    "div:has(a.ls-link[href*=\"community.limesurvey.org\"])"
  )
  if (is.na(footer)) {
    stop(
      "Could not find the version information (page layout may have changed).",
      call. = FALSE
    )
  }

  version <- regmatches_first(
    html_text2(footer),
    "Version\\s+([0-9A-Za-z.+_-]+)"
  )
  if (is.na(version)) {
    stop(
      "Could not parse the LimeSurvey version (page layout may have changed).",
      call. = FALSE
    )
  }

  version
}
