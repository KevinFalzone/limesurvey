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

mock_question_save <- function(captured_fields_env, question_id = 42) {
  local_mocked_bindings(
    ls_csrf_fetch = function(...) list(token = "tok", cookie = "cookie"),
    ls_request = function(...) "dummy-request",
    req_body_form = function(req, ...) {
      captured_fields_env$fields <- rlang::list2(...)
      req
    },
    req_perform = function(...) "dummy-response",
    resp_url = function(...) {
      sprintf("https://example.org/?questionId=%d", question_id)
    },
    .env = parent.frame()
  )
}

test_that("ls_save_question posts type 'M' answers as subquestions", {
  captured <- new.env()
  mock_question_save(captured)

  ls_save_question(
    1,
    1,
    code = "Q1",
    type = "M",
    text = "Text",
    answers = c(red = "Rouge", green = "Vert")
  )

  field_names <- names(captured$fields)
  expect_true(any(grepl("^subquestions\\[", field_names)))
  expect_false(any(grepl("^answeroptions\\[", field_names)))
  expect_true(any(
    vapply(
      captured$fields[grepl("\\[code\\]$", field_names)],
      identical,
      logical(1),
      "red"
    )
  ))
})

test_that("ls_save_question still posts type 'L' answers as answeroptions", {
  captured <- new.env()
  mock_question_save(captured)

  ls_save_question(
    1,
    1,
    code = "Q1",
    type = "L",
    text = "Text",
    answers = c(red = "Rouge", green = "Vert")
  )

  field_names <- names(captured$fields)
  expect_true(any(grepl("^answeroptions\\[", field_names)))
  expect_false(any(grepl("^subquestions\\[", field_names)))
})
