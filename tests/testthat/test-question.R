test_that("ls_save_question validates 'sid' and 'gid'", {
  expect_error(ls_save_question("1", 1, code = "Q1", text = "Text"), "sid")
  expect_error(ls_save_question(1, "1", code = "Q1", text = "Text"), "gid")
})

test_that("ls_save_question validates 'code'", {
  expect_error(ls_save_question(1, 1, code = "", text = "Text"), "code")
  expect_error(ls_save_question(1, 1, code = NA, text = "Text"), "code")
})

test_that("ls_save_question validates 'text'", {
  expect_error(ls_save_question(1, 1, code = "Q1", text = ""), "text")
  expect_error(ls_save_question(1, 1, code = "Q1", text = NA), "text")
})

test_that("ls_save_question validates 'type'", {
  expect_error(
    ls_save_question(1, 1, code = "Q1", text = "Text", type = "ZZ"),
    "type"
  )
  expect_error(
    ls_save_question(1, 1, code = "Q1", text = "Text", type = NA),
    "type"
  )
})

test_that("ls_save_question validates general_settings/logic/.../statistics are lists", {
  expect_error(
    ls_save_question(1, 1, code = "Q1", text = "Text", general_settings = "N"),
    "general_settings"
  )
  expect_error(
    ls_save_question(1, 1, code = "Q1", text = "Text", logic = "1"),
    "logic"
  )
})

test_that("ls_save_question validates general_settings$mandatory", {
  expect_error(
    ls_save_question(
      1,
      1,
      code = "Q1",
      text = "Text",
      general_settings = list(mandatory = "X")
    ),
    "mandatory"
  )
})

test_that("ls_save_question validates general_settings$relevance", {
  expect_error(
    ls_save_question(
      1,
      1,
      code = "Q1",
      text = "Text",
      general_settings = list(relevance = "")
    ),
    "relevance"
  )
})

test_that("ls_save_question validates general_settings$encrypted", {
  expect_error(
    ls_save_question(
      1,
      1,
      code = "Q1",
      text = "Text",
      general_settings = list(encrypted = "X")
    ),
    "encrypted"
  )
})

test_that("ls_save_question validates 'answers'", {
  expect_error(
    ls_save_question(1, 1, code = "Q1", text = "Text", answers = character(0)),
    "answers"
  )
  expect_error(
    ls_save_question(1, 1, code = "Q1", text = "Text", answers = c("A", NA)),
    "answers"
  )
  expect_error(
    ls_save_question(1, 1, code = "Q1", text = "Text", answers = c("A", "")),
    "answers"
  )
})

test_that("ls_save_question validates 'answers' codes", {
  expect_error(
    ls_save_question(
      1,
      1,
      code = "Q1",
      text = "Text",
      answers = c(TOOLONG = "A")
    ),
    "alphanumeric"
  )
  expect_error(
    ls_save_question(
      1,
      1,
      code = "Q1",
      text = "Text",
      answers = c(`A-1` = "A")
    ),
    "alphanumeric"
  )
  expect_error(
    ls_save_question(
      1,
      1,
      code = "Q1",
      text = "Text",
      answers = c(A1 = "A", A1 = "B")
    ),
    "unique"
  )
})
