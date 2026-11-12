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
  expect_error(ls_save_group(1, name = "Group", condition = c("1", "2")), "condition")
})
