test_that("limesurvey_set_session stores the session", {
  limesurvey_set_session(url = "https://example.com/", token = "t")
  expect_equal(
    limesurvey_get_session(),
    list(url = "https://example.com/", token = "t")
  )
})

test_that("limesurvey_set_session adds a missing trailing slash to url", {
  limesurvey_set_session(url = "https://example.com", token = "t")
  expect_equal(limesurvey_get_session()$url, "https://example.com/")
})

test_that("limesurvey_set_session rejects invalid url", {
  expect_error(limesurvey_set_session(url = "", token = "t"), "url")
  expect_error(limesurvey_set_session(url = NA, token = "t"), "url")
  expect_error(limesurvey_set_session(url = 1, token = "t"), "url")
})

test_that("limesurvey_set_session rejects invalid token", {
  expect_error(limesurvey_set_session(url = "https://example.com/", token = ""), "token")
  expect_error(limesurvey_set_session(url = "https://example.com/", token = NA), "token")
  expect_error(limesurvey_set_session(url = "https://example.com/", token = 1), "token")
})

test_that("limesurvey_get_session errors when no session is set", {
  if (exists("session", envir = limesurvey_env)) {
    rm("session", envir = limesurvey_env)
  }
  expect_error(limesurvey_get_session(), "No session set")
})

test_that("limesurvey_resolve_session rejects unknown overrides", {
  limesurvey_set_session(url = "https://example.com/", token = "t")
  expect_error(
    limesurvey_resolve_session(list(bogus = "x")),
    "Unknown parameter"
  )
})

test_that("limesurvey_resolve_session applies known overrides", {
  limesurvey_set_session(url = "https://example.com/", token = "t")
  resolved <- limesurvey_resolve_session(list(token = "override"))
  expect_equal(resolved, list(url = "https://example.com/", token = "override"))
})
