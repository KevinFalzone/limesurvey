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
    ls_update_general_settings = function(...) invisible(NULL),
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
    ls_update_general_settings = function(...) invisible(NULL),
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
    ls_update_general_settings = function(...) invisible(NULL),
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
