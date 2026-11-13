test_that("ls_fill_down carries the last non-empty value forward", {
  expect_equal(
    ls_fill_down(c("a", "", "", "b", "")),
    c("a", "a", "a", "b", "b")
  )
})

test_that("ls_get_questions validates 'sid'", {
  expect_error(ls_get_questions("1"), "sid")
  expect_error(ls_get_questions(NA), "sid")
  expect_error(ls_get_questions(1.5), "sid")
})

test_that("ls_get_data validates 'sid'", {
  expect_error(
    ls_get_data(
      "1",
      export_from = 1,
      export_to = 10,
      completionstate = "all",
      headstyle = "code"
    ),
    "sid"
  )
})

test_that("ls_get_data validates 'export_from' and 'export_to'", {
  expect_error(
    ls_get_data(
      1,
      export_from = 0,
      export_to = 10,
      completionstate = "all",
      headstyle = "code"
    ),
    "export_from"
  )
  expect_error(
    ls_get_data(
      1,
      export_from = 1,
      export_to = -1,
      completionstate = "all",
      headstyle = "code"
    ),
    "export_to"
  )
  expect_error(
    ls_get_data(
      1,
      export_from = 10,
      export_to = 1,
      completionstate = "all",
      headstyle = "code"
    ),
    "export_from.*export_to"
  )
})

test_that("ls_get_data validates 'completionstate'", {
  expect_error(
    ls_get_data(
      1,
      export_from = 1,
      export_to = 10,
      completionstate = "nope",
      headstyle = "code"
    ),
    "completionstate"
  )
})

test_that("ls_get_data validates 'headstyle'", {
  expect_error(
    ls_get_data(
      1,
      export_from = 1,
      export_to = 10,
      completionstate = "all",
      headstyle = "nope"
    ),
    "headstyle"
  )
})

test_that("ls_get_data validates 'language'", {
  expect_error(
    ls_get_data(
      1,
      export_from = 1,
      export_to = 10,
      completionstate = "all",
      headstyle = "code",
      language = ""
    ),
    "language"
  )
})

test_that("ls_get_data validates 'columns'", {
  expect_error(
    ls_get_data(
      1,
      export_from = 1,
      export_to = 10,
      completionstate = "all",
      headstyle = "code",
      columns = "not-a-path"
    ),
    "SIDXGIDXQID"
  )
  expect_error(
    ls_get_data(
      1,
      export_from = 1,
      export_to = 10,
      completionstate = "all",
      headstyle = "code",
      columns = character(0)
    ),
    "columns"
  )
})
