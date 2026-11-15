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

test_that("ls_update_survey_settings_texts validates 'sid'", {
  expect_error(
    ls_update_survey_settings_texts("1", texts = list(fr = list(title = "T"))),
    "sid"
  )
})

test_that("ls_update_survey_settings_texts validates 'texts'", {
  expect_error(ls_update_survey_settings_texts(1, texts = list()), "texts")
  expect_error(
    ls_update_survey_settings_texts(1, texts = list(title = "T")),
    "texts"
  )
  expect_error(
    ls_update_survey_settings_texts(1, texts = list(fr = "not a list")),
    "list"
  )
})

test_that("ls_update_survey_settings_texts posts fields for every language given", {
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

  ls_update_survey_settings_texts(
    1,
    texts = list(
      en = list(title = "Training request", welcome = "Welcome"),
      fr = list(title = "Training application")
    )
  )

  expect_equal(captured$fields[["short_title_en"]], "Training request")
  expect_equal(captured$fields[["welcome_en"]], "Welcome")
  expect_equal(captured$fields[["dateformat_en"]], "1")
  expect_equal(captured$fields[["numberformat_en"]], "0")
  expect_equal(captured$fields[["short_title_fr"]], "Training application")
  expect_equal(captured$fields[["welcome_fr"]], "")
})

test_that("ls_update_survey_settings_data_security validates 'sid'", {
  expect_error(ls_update_survey_settings_data_security("1"), "sid")
})

test_that("ls_update_survey_settings_data_security validates 'show_token_policy'", {
  expect_error(
    ls_update_survey_settings_data_security(1, show_token_policy = "X"),
    "show_token_policy"
  )
})

test_that("ls_update_survey_settings_data_security validates 'show_register_policy'", {
  expect_error(
    ls_update_survey_settings_data_security(1, show_register_policy = "X"),
    "show_register_policy"
  )
})

test_that("ls_update_survey_settings_data_security validates 'texts'", {
  expect_error(
    ls_update_survey_settings_data_security(1, texts = "not a list"),
    "texts"
  )
  expect_error(
    ls_update_survey_settings_data_security(1, texts = list(label = "x")),
    "texts"
  )
  expect_error(
    ls_update_survey_settings_data_security(1, texts = list(fr = "not a list")),
    "list"
  )
})

test_that("ls_update_survey_settings_data_security posts policy flags and per-language text", {
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

  ls_update_survey_settings_data_security(
    1,
    show_policy_notice = "1",
    show_token_policy = "Y",
    show_register_policy = "N",
    texts = list(en = list(label = "Policy", notice = "Notice text"))
  )

  expect_equal(captured$fields[["showsurveypolicynotice"]], "1")
  expect_equal(captured$fields[["showtokenpolicy"]], "Y")
  expect_equal(captured$fields[["showregisterpolicy"]], "N")
  expect_equal(captured$fields[["dataseclabel_en"]], "Policy")
  expect_equal(captured$fields[["datasec_en"]], "Notice text")
  expect_equal(captured$fields[["datasecerror_en"]], "")
})

test_that("ls_update_survey_settings_presentation validates 'sid'", {
  expect_error(ls_update_survey_settings_presentation("1"), "sid")
})

test_that("ls_update_survey_settings_presentation validates I/Y/N arguments", {
  expect_error(
    ls_update_survey_settings_presentation(1, show_welcome = "X"),
    "show_welcome"
  )
  expect_error(
    ls_update_survey_settings_presentation(1, public_statistics = "X"),
    "public_statistics"
  )
})

test_that("ls_update_survey_settings_presentation validates 'navigation_delay_custom'", {
  expect_error(
    ls_update_survey_settings_presentation(1, navigation_delay_custom = "I"),
    "navigation_delay_custom"
  )
})

test_that("ls_update_survey_settings_presentation validates 'question_index'", {
  expect_error(
    ls_update_survey_settings_presentation(1, question_index = ""),
    "question_index"
  )
  expect_error(
    ls_update_survey_settings_presentation(1, question_index = NA),
    "question_index"
  )
})

test_that("ls_update_survey_settings_presentation validates 'navigation_delay'", {
  expect_error(
    ls_update_survey_settings_presentation(1, navigation_delay = ""),
    "navigation_delay"
  )
})

test_that("ls_update_survey_settings_presentation posts the expected fields", {
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

  ls_update_survey_settings_presentation(
    1,
    show_welcome = "Y",
    question_index = "1",
    navigation_delay = "5",
    navigation_delay_custom = "Y",
    public_statistics = "N"
  )

  expect_equal(captured$fields[["showwelcome"]], "Y")
  expect_equal(captured$fields[["questionindex"]], "1")
  expect_equal(captured$fields[["navigationdelay"]], "5")
  expect_equal(captured$fields[["navigationdelaybutton"]], "Y")
  expect_equal(captured$fields[["publicstatistics"]], "N")
  expect_equal(captured$fields[["shownoanswer"]], "I")
})

test_that("ls_update_survey_settings_participants validates 'sid'", {
  expect_error(ls_update_survey_settings_participants("1"), "sid")
})

test_that("ls_update_survey_settings_participants validates I/Y/N arguments", {
  expect_error(
    ls_update_survey_settings_participants(1, anonymized = "X"),
    "anonymized"
  )
  expect_error(
    ls_update_survey_settings_participants(1, send_confirmation = "X"),
    "send_confirmation"
  )
})

test_that("ls_update_survey_settings_participants validates 'token_length_custom'", {
  expect_error(
    ls_update_survey_settings_participants(1, token_length_custom = "I"),
    "token_length_custom"
  )
})

test_that("ls_update_survey_settings_participants validates 'token_length'", {
  expect_error(
    ls_update_survey_settings_participants(1, token_length = ""),
    "token_length"
  )
})

test_that("ls_update_survey_settings_participants posts the expected fields", {
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

  ls_update_survey_settings_participants(
    1,
    anonymized = "Y",
    token_length = "15",
    token_length_custom = "Y",
    allow_register = "N"
  )

  expect_equal(captured$fields[["anonymized"]], "Y")
  expect_equal(captured$fields[["tokenlength"]], "15")
  expect_equal(captured$fields[["tokenlengthbutton"]], "Y")
  expect_equal(captured$fields[["allowregister"]], "N")
  expect_equal(captured$fields[["showtokenpolicy"]], "I")
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
