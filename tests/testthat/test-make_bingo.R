test_that("make_bingo works", {
  test <- make_bingo(letters, save = FALSE, n_cards = 1)

  expect_type(test, "list")

  expect_true(length(test) == 1)


})

test_that("make_bingo saves correctly", {
  test <- make_bingo(letters, save = TRUE, n_cards = 1)

  expect_true(file.exists("bingo_1.pdf"))

  file.remove("bingo_1.pdf")

})
