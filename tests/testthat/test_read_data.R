library(nails)
context("Reading data")

load("data/column_names.RData")

test_that("single txt-file is read correctly", {
    df <- read_wos_txt("data/crowdsourcing.txt")
    expect_equal("data.frame", class(df))
    expect_equal(60, ncol(df))
    expect_equal(500, nrow(df))
    expect_equal(fields, names(df))
})

test_that("txt-files in folder are read correctly", {
    df <- read_wos_folder("data")
    expect_equal("data.frame", class(df))
    expect_equal(60, ncol(df))
    expect_equal(995, nrow(df))
    expect_equal(fields, names(df))
})

