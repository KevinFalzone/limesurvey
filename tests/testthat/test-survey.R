test_that("limesurvey_create_survey validates 'title'", {
  expect_error(limesurvey_create_survey(title = ""), "title")
  expect_error(limesurvey_create_survey(title = NA), "title")
  expect_error(limesurvey_create_survey(title = 1), "title")
})

test_that("limesurvey_create_survey validates 'language'", {
  expect_error(limesurvey_create_survey(title = "Survey", language = ""), "language")
  expect_error(limesurvey_create_survey(title = "Survey", language = NA), "language")
})

test_that("limesurvey_create_survey validates 'format'", {
  expect_error(limesurvey_create_survey(title = "Survey", format = "X"), "format")
  expect_error(limesurvey_create_survey(title = "Survey", format = 1), "format")
})

test_that("limesurvey_delete_survey validates 'sid'", {
  expect_error(limesurvey_delete_survey("511927"), "sid")
  expect_error(limesurvey_delete_survey(NA), "sid")
  expect_error(limesurvey_delete_survey(1.5), "sid")
  expect_error(limesurvey_delete_survey(c(1, 2)), "sid")
})
