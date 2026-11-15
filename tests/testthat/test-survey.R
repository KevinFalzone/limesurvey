test_that("ls_create_survey validates 'title'", {
  expect_error(ls_create_survey(title = ""), "title")
  expect_error(ls_create_survey(title = NA), "title")
  expect_error(ls_create_survey(title = 1), "title")
})

test_that("ls_create_survey validates 'language'", {
  expect_error(ls_create_survey(title = "Survey", language = ""), "language")
  expect_error(ls_create_survey(title = "Survey", language = NA), "language")
})

test_that("ls_create_survey validates 'format'", {
  expect_error(ls_create_survey(title = "Survey", format = "X"), "format")
  expect_error(ls_create_survey(title = "Survey", format = 1), "format")
})

test_that("ls_create_survey validates 'remove_default_group'", {
  expect_error(
    ls_create_survey(title = "Survey", remove_default_group = "yes"),
    "remove_default_group"
  )
  expect_error(
    ls_create_survey(title = "Survey", remove_default_group = c(TRUE, FALSE)),
    "remove_default_group"
  )
})

test_that("ls_create_survey deletes the default question group", {
  deleted_gid <- NULL
  local_mocked_bindings(
    ls_new_survey = function() 1L,
    ls_update_survey_settings_general = function(...) invisible(NULL),
    ls_update_title = function(...) invisible(NULL),
    ls_get_questions = function(sid) {
      data.frame(group_id = "10", id = 100, name = "G1Q1", path = "1X10X100")
    },
    ls_delete_group = function(sid, gid) {
      deleted_gid <<- gid
      invisible(gid)
    }
  )

  ls_create_survey(title = "Survey")

  expect_equal(deleted_gid, 10L)
})

test_that("ls_create_survey skips deletion when remove_default_group is FALSE", {
  called <- FALSE
  local_mocked_bindings(
    ls_new_survey = function() 1L,
    ls_update_survey_settings_general = function(...) invisible(NULL),
    ls_update_title = function(...) invisible(NULL),
    ls_get_questions = function(sid) {
      data.frame(group_id = "10", id = 100, name = "G1Q1", path = "1X10X100")
    },
    ls_delete_group = function(sid, gid) {
      called <<- TRUE
      invisible(gid)
    }
  )

  ls_create_survey(title = "Survey", remove_default_group = FALSE)

  expect_false(called)
})

test_that("ls_create_survey skips deletion when there is no default group", {
  called <- FALSE
  local_mocked_bindings(
    ls_new_survey = function() 1L,
    ls_update_survey_settings_general = function(...) invisible(NULL),
    ls_update_title = function(...) invisible(NULL),
    ls_get_questions = function(sid) {
      data.frame(group_id = character(0), id = integer(0), name = character(0))
    },
    ls_delete_group = function(sid, gid) {
      called <<- TRUE
      invisible(gid)
    }
  )

  ls_create_survey(title = "Survey")

  expect_false(called)
})

test_that("ls_update_survey_settings_general validates 'sid'", {
  expect_error(ls_update_survey_settings_general("1", language = "en"), "sid")
  expect_error(ls_update_survey_settings_general(NA, language = "en"), "sid")
})

test_that("ls_update_survey_settings_general validates 'language'", {
  expect_error(ls_update_survey_settings_general(1, language = ""), "language")
  expect_error(ls_update_survey_settings_general(1, language = NA), "language")
})

test_that("ls_update_survey_settings_general validates 'format'", {
  expect_error(
    ls_update_survey_settings_general(1, language = "en", format = "X"),
    "format"
  )
})

test_that("ls_update_survey_settings_general validates 'additional_languages'", {
  expect_error(
    ls_update_survey_settings_general(
      1,
      language = "en",
      additional_languages = 1
    ),
    "additional_languages"
  )
  expect_error(
    ls_update_survey_settings_general(
      1,
      language = "en",
      additional_languages = NA_character_
    ),
    "additional_languages"
  )
})

test_that("ls_update_survey_settings_general validates 'old_additional_languages'", {
  expect_error(
    ls_update_survey_settings_general(
      1,
      language = "en",
      old_additional_languages = 1
    ),
    "old_additional_languages"
  )
})

test_that("ls_update_survey_settings_general posts additional_languages[] and oldlanguages", {
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

  ls_update_survey_settings_general(
    1,
    language = "en",
    additional_languages = c("fil", "es"),
    old_additional_languages = "fil"
  )

  field_names <- names(captured$fields)
  additional <- unname(unlist(
    captured$fields[field_names == "additional_languages[]"]
  ))

  expect_equal(additional, c("fil", "es"))
  expect_equal(captured$fields[["oldlanguages"]], "fil")
  expect_equal(captured$fields[["language"]], "en")
})

test_that("ls_update_survey_settings_general omits additional_languages[] when NULL", {
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

  ls_update_survey_settings_general(1, language = "en")

  field_names <- names(captured$fields)
  expect_false(any(field_names == "additional_languages[]"))
  expect_equal(captured$fields[["oldlanguages"]], "")
})

test_that("ls_delete_survey validates 'sid'", {
  expect_error(ls_delete_survey("511927"), "sid")
  expect_error(ls_delete_survey(NA), "sid")
  expect_error(ls_delete_survey(1.5), "sid")
  expect_error(ls_delete_survey(c(1, 2)), "sid")
})

test_that("ls_copy_survey validates 'sid'", {
  expect_error(ls_copy_survey("511927"), "sid")
  expect_error(ls_copy_survey(NA), "sid")
  expect_error(ls_copy_survey(1.5), "sid")
})

test_that("ls_copy_survey validates 'title'", {
  expect_error(ls_copy_survey(1, title = ""), "title")
  expect_error(ls_copy_survey(1, title = NA), "title")
  expect_error(ls_copy_survey(1, title = 1), "title")
})
