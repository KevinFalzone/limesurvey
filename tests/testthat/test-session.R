test_that("ls_set_session stores the session", {
  ls_set_session(url = "https://example.com/", token = "t")
  expect_equal(
    ls_get_session(),
    list(url = "https://example.com/", token = "t")
  )
})

test_that("ls_set_session adds a missing trailing slash to url", {
  ls_set_session(url = "https://example.com", token = "t")
  expect_equal(ls_get_session()$url, "https://example.com/")
})

test_that("ls_set_session rejects invalid url", {
  expect_error(ls_set_session(url = "", token = "t"), "url")
  expect_error(ls_set_session(url = NA, token = "t"), "url")
  expect_error(ls_set_session(url = 1, token = "t"), "url")
})

test_that("ls_set_session rejects invalid token", {
  expect_error(
    ls_set_session(url = "https://example.com/", token = ""),
    "token"
  )
  expect_error(
    ls_set_session(url = "https://example.com/", token = NA),
    "token"
  )
  expect_error(ls_set_session(url = "https://example.com/", token = 1), "token")
})

test_that("ls_get_session errors when no session is set", {
  if (exists("session", envir = ls_env)) {
    rm("session", envir = ls_env)
  }
  expect_error(ls_get_session(), "No session set")
})

test_that("ls_resolve_session rejects unknown overrides", {
  ls_set_session(url = "https://example.com/", token = "t")
  expect_error(
    ls_resolve_session(list(bogus = "x")),
    "Unknown parameter"
  )
})

test_that("ls_resolve_session applies known overrides", {
  ls_set_session(url = "https://example.com/", token = "t")
  resolved <- ls_resolve_session(list(token = "override"))
  expect_equal(resolved, list(url = "https://example.com/", token = "override"))
})
