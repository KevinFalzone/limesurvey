fake_footer_html <- function(footer_text) {
  xml2::read_html(paste0(
    "<html><body><div>",
    "<a class='ls-link' href='https://community.limesurvey.org'>",
    "LimeSurvey Community Edition</a>",
    footer_text,
    "</div></body></html>"
  ))
}

test_that("ls_get_version returns the version string", {
  local_mocked_bindings(
    ls_request = function(...) "dummy-request",
    req_perform = function(...) "dummy-response",
    resp_body_html = function(...) fake_footer_html("Version  7.9.1+261102")
  )

  expect_equal(ls_get_version(), "7.9.1+261102")
})

test_that("ls_get_version errors when the footer cannot be found", {
  local_mocked_bindings(
    ls_request = function(...) "dummy-request",
    req_perform = function(...) "dummy-response",
    resp_body_html = function(...) {
      xml2::read_html("<html><body><div>empty</div></body></html>")
    }
  )

  expect_error(ls_get_version(), "version information")
})

test_that("ls_get_version errors when the version text cannot be parsed", {
  local_mocked_bindings(
    ls_request = function(...) "dummy-request",
    req_perform = function(...) "dummy-response",
    resp_body_html = function(...) fake_footer_html("no version here")
  )

  expect_error(ls_get_version(), "version")
})
