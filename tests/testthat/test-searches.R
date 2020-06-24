test_that("PLOS Search returns expected tibble", {

  # search term that returns data

  plostest <- plosSearch("microbiome")

  expect_is(plostest, "tbl_df")
  expect_equal(ncol(plostest), 14)


  # search term that does not return data

  plostest2 <- plosSearch("oisdlsduyhfgjsdfljghsklijugh")

  expect_is(plostest2, "tbl_df")
  expect_equal(ncol(plostest2), 0)
  expect_equal(nrow(plostest2), 0)

})
