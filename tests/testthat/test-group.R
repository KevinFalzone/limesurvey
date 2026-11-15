test_that("ls_save_group validates 'sid'", {
  expect_error(ls_save_group("1", name = "Group"), "sid")
  expect_error(ls_save_group(NA, name = "Group"), "sid")
})

test_that("ls_save_group validates 'name'", {
  expect_error(ls_save_group(1, name = ""), "name")
  expect_error(ls_save_group(1, name = NA), "name")
  expect_error(ls_save_group(1, name = 1), "name")
})

test_that("ls_save_group validates 'gid'", {
  expect_error(ls_save_group(1, name = "Group", gid = "1"), "gid")
})

test_that("ls_save_group validates 'condition'", {
  expect_error(ls_save_group(1, name = "Group", condition = ""), "condition")
  expect_error(ls_save_group(1, name = "Group", condition = NA), "condition")
  expect_error(
    ls_save_group(1, name = "Group", condition = c("1", "2")),
    "condition"
  )
})

test_that("ls_delete_group validates 'sid'", {
  expect_error(ls_delete_group("1", gid = 1), "sid")
  expect_error(ls_delete_group(NA, gid = 1), "sid")
})

test_that("ls_delete_group validates 'gid'", {
  expect_error(ls_delete_group(1, gid = "1"), "gid")
  expect_error(ls_delete_group(1, gid = NA), "gid")
})

test_that("ls_delete_group posts the expected fields", {
  captured <- new.env()
  local_mocked_bindings(
    ls_csrf_fetch = function(...) list(token = "tok", cookie = "cookie"),
    ls_request = function(...) "dummy-request",
    req_body_form = function(req, ...) {
      captured$fields <- rlang::list2(...)
      req
    },
    req_perform = function(...) "dummy-response"
  )

  ls_delete_group(1, gid = 42)

  expect_equal(captured$fields[["surveyid"]], 1)
  expect_equal(captured$fields[["gid"]], 42)
  expect_equal(captured$fields[["asJson"]], 1)
  expect_equal(captured$fields[["landOnSideMenuTab"]], "structure")
})
