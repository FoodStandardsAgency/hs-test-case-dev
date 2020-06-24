test_that("crossref returns expected structure", {

  rcf <- rcrossref::cr_works(query = "Down the deep rabbit hole: Untangling deep learning from machine learning and artificial intelligence",
                             select = c("DOI", "type", "title", "author", "container-title", "issue",
                                        "abstract", "issued", "subject"))

  expect_is(rcf, "list")
  expect_named(rcf)
  expect_true("data" %in% names(rcf))
  expect_is(rcf$data, "tbl_df")
  expect_equal(ncol(rcf$data), 9)
})

test_that("fetchFromTitle returns expected output", {

  # when article is present in crossref

  article_present <- fetchFromTitle("Down the deep rabbit hole: Untangling deep learning from machine learning and artificial intelligence")

  expect_is(article_present, "tbl_df")
  expect_gte(nrow(article_present), 1)
  expect_equal(ncol(article_present), 32)

  # when search returns articles but none of them are the right one

  no_article <- fetchFromTitle("rabbit beans oranges")

  expect_is(no_article, "tbl_df")
  expect_gte(nrow(no_article), 0)
  expect_equal(ncol(no_article), 24)

  # when search returns nothing (should be empty tibble)

  no_results <- fetchFromTitle("sodgiushdfuygsdogidfghsiedrtlihsdfg")

  expect_is(no_results, "tbl_df")
  expect_gte(nrow(no_results), 0)
  expect_equal(ncol(no_results), 0)

})

test_that("fetchFromDOI returns expected output", {

  # DOI that exists

  article_present <- fetchFromDOI("10.1371/journal.pone.0090731")

  expect_is(article_present, "tbl_df")
  expect_gte(nrow(article_present), 0)
  expect_equal(ncol(article_present), 26)

  # DOI that does not exist

  no_results <- fetchFromDOI("12345")

  expect_is(no_results, "tbl_df")
  expect_gte(nrow(no_results), 0)
  expect_equal(ncol(no_results), 0)
  expect_warning(fetchFromDOI("12345"), "Resource not found")

})

test_that("crossref cleanup gives desired output", {

  cleandata <- fetchFromDOI("10.1371/journal.pone.0090731") %>% CrossRefCleanup(.)

  expect_is(cleandata, "tbl_df")
  expect_gte(nrow(cleandata), 1)
  expect_equal(ncol(cleandata), 13)

})

test_that("fulltext and pubchunks behave as expected", {

  ftreturn <- fulltext::ft_get("10.1371/journal.pone.0090731") %>%
    fulltext::ft_collect()

  expect_is(ftreturn, "ft_data")

  pc <- ftreturn %>%
    pubchunks::pub_chunks("body") %>%
    pubchunks::pub_tabularize(bind = TRUE)

  expect_is(pc, "data.frame")
  expect_length(pc, 2)
  expect_gte(nrow(pc), 1)

})

test_that("getBody returns expected output", {

  # when it is an article that successfully returns a body

  getbody <- getBody("10.1371/journal.pone.0090731")

  expect_is(getbody, "tbl_df")
  expect_equal(ncol(getbody), 2)
  expect_equal(nrow(getbody), 1)
  expect_equal(names(getbody), c("doi", "body"))
  expect_failure(expect_equal(as.character(getbody$body), NA))

  # when it is not - invalid DOI

  getbody2 <- getBody("10.1371/journal.pone.00907")

  expect_is(getbody, "tbl_df")
  expect_equal(ncol(getbody), 2)
  expect_equal(nrow(getbody), 1)
  expect_equal(names(getbody), c("doi", "body"))

  # when it is not - valid DOI but no access

  getbody3 <- getBody("10.1038/s41467-019-12873-4")

  expect_is(getbody, "tbl_df")
  expect_equal(ncol(getbody), 2)
  expect_equal(nrow(getbody), 1)
  expect_equal(names(getbody), c("doi", "body"))

})

test_that("getAbstract returns expected tibble output", {

  abdois <- c("10.1016/j.molmed.2019.11.005",
            "10.1038/s41563-019-0557-3",
            "10.1016/j.molmed.2019.11.006")

  abs <- purrr::map(abdois, purrr::slowly(getAbstract, rate = purrr::rate_delay(pause = 1)))

  # when DOI is valid and abstract is present

  existsWithAbstract <- abs[[1]]

  expect_is(existsWithAbstract, "tbl_df")
  expect_equal(ncol(existsWithAbstract), 2)
  expect_equal(nrow(existsWithAbstract), 1)

  # when DOI is valid but there is no abstract

  existsWithoutAbstract <- abs[[2]]

  expect_is(existsWithoutAbstract, "tbl_df")
  expect_equal(ncol(existsWithoutAbstract), 2)
  expect_equal(nrow(existsWithoutAbstract), 0)

  # when DOI is not valid

  doesNotExist <- abs[[3]]

  expect_is(doesNotExist, "tbl_df")
  expect_equal(ncol(doesNotExist), 2)
  expect_equal(nrow(doesNotExist), 0)

})
