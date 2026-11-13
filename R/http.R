ls_cookie_name <- "LS-PBAAKGBRRSWIIXON"

#' @importFrom httr2 request req_headers
ls_request <- function(path, ajax = TRUE, csrf_cookie = NULL) {
  session <- ls_get_session()
  base_url <- sub("/$", "", session$url)
  cookie <- paste0(ls_cookie_name, "=", session$token)
  if (!is.null(csrf_cookie)) {
    cookie <- paste0(cookie, "; YII_CSRF_TOKEN=", csrf_cookie)
  }

  req <- request(paste0(base_url, path))
  req <- req_headers(req, Cookie = cookie)
  if (ajax) {
    req <- req_headers(req, "X-Requested-With" = "XMLHttpRequest")
  }
  req
}

#' @importFrom rvest html_element html_attr
ls_csrf_token <- function(html) {
  token <- html_attr(
    html_element(html, "input[name=\"YII_CSRF_TOKEN\"]"),
    "value"
  )
  if (is.na(token) || !nzchar(token)) {
    stop(
      "Could not find the CSRF token (page layout may have changed).",
      call. = FALSE
    )
  }
  token
}

#' Fetch a Form Page's CSRF Token and CSRF Cookie
#'
#' LimeSurvey's forms are protected by Yii2's CSRF double-submit pattern: a
#' hidden `YII_CSRF_TOKEN` form field must be submitted alongside a matching
#' `YII_CSRF_TOKEN` cookie minted on the same response. Both must come from
#' the same request or the submission is rejected with HTTP 400.
#'
#' @noRd
#' @importFrom httr2 req_perform resp_body_html resp_headers
ls_csrf_fetch <- function(path) {
  resp <- req_perform(ls_request(path))
  html <- resp_body_html(resp)

  set_cookie <- resp_headers(resp)[["set-cookie"]]
  if (is.null(set_cookie) || !grepl("YII_CSRF_TOKEN=", set_cookie)) {
    stop("Could not find the CSRF cookie in the response.", call. = FALSE)
  }
  csrf_cookie <- sub("^YII_CSRF_TOKEN=([^;]+);.*$", "\\1", set_cookie)

  list(html = html, token = ls_csrf_token(html), cookie = csrf_cookie)
}
