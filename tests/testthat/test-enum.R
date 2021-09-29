test_that("Check key-value mappings work", {
  library(ospsuite)

  myEnum <- enum(c(a = "b"))

  expect_equal(
    enumPutList("c", "d", myEnum),
    list(a = "b", c = "d")
  )

  expect_equal(
    enumPutList("c", list(12, 2, "a"), myEnum),
    list(a = "b", c = list(12, 2, "a"))
  )

  expect_equal(
    enumPutList("a", list(12, 2, "a"), myEnum, overwrite = TRUE),
    list(a = list(12, 2, "a"))
  )

  expect_error(enumPutList("a", list(12, 2, "a"), myEnum))

  expect_error(enumPutList(c("c", "d", "g"), list(12, 2, "a"), myEnum, overwrite = TRUE))
})
