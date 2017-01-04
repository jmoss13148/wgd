context("Reading State names and Numbers from a text file")

test_that("readEphData has 53 rows and 2 columns",{

  expect_equal(53, nrow(readEphData("williams2005.txt")))
  expect_equal(2, ncol(readEphData("williams2005.txt")))
})
