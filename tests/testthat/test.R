context("Test the basic functionality of the package")

library(dplyr)
library(maps)

setwd(system.file("extdata", package = "fars"))

test_that("fars_read() works correctly", {
  setwd(system.file("extdata", package = "fars"))
  expect_is(fars_read("accident_2015.csv.bz2"), "tbl_df")
  expect_error(fars_read("accident_2012.csv.bz2"))
})

test_that("make_filename()",
          {
            setwd(system.file("extdata", package = "fars"))
            expect_true(make_filename(2015)=="accident_2015.csv.bz2")
            
          }
          )


test_that("fars_read_years()",
          {
            setwd(system.file("extdata", package = "fars"))
            expect_is(fars_read_years(2013:2015),"list")
            
          })


test_that("fars_summarize_years() ", {
  setwd(system.file("extdata", package = "fars"))
  expect_is(fars_summarize_years(2013), "tbl_df")
  expect_equal(names(fars_summarize_years(2013:2014)), c("MONTH", 2013:2014))
  expect_error(fars_summarize_years(2020))
})

test_that("fars_map_state() ", {
  setwd(system.file("extdata", package = "fars"))
  expect_error(fars_map_state(100, 2014))
  expect_error(fars_map_state(1, 2012))
})