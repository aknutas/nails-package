library(nails)
context("Reading data")

load(file.path("test_data/fieldtags.RData"))
load(file.path("test_data/column_names.RData"))

test_that("single txt-file is read correctly, fix_names = TRUE", {
    df <- read_wos_txt(file.path("test_data/savedrecs-mactab.txt"), fix_names = TRUE)
    expect_equal("data.frame", class(df))
    expect_equal(60, ncol(df))
    expect_equal(500, nrow(df))
    expect_equal(fields, names(df))
})

test_that("single txt-file is read correctly, fix_names = FALSE", {
    df <- read_wos_txt(file.path("test_data/savedrecs-mactab.txt"), fix_names = FALSE)
    expect_equal("data.frame", class(df))
    expect_equal(60, ncol(df))
    expect_equal(500, nrow(df))
    expect_equal(fieldtags, names(df))
})

test_that("single txt-file is read correctly, new data format", {
    df <- read_wos_txt(file.path("test_data/savedrecs.39.txt"), fix_names = FALSE)
    expect_equal("data.frame", class(df))
    expect_equal(60, ncol(df))
    expect_equal(500, nrow(df))
    expect_equal(fields, names(df))
})

test_that("txt-files in folder are read correctly, fix_names = TRUE", {
    df <- read_wos_folder(file.path("test_data"), fix_names = TRUE)
    expect_equal("data.frame", class(df))
    expect_equal(60, ncol(df))
    expect_equal(995, nrow(df))
    expect_equal(fields, names(df))
})

test_that("txt-files in folder are read correctly, fix_names = FALSE", {
    df <- read_wos_folder(file.path("test_data"), fix_names = FALSE)
    expect_equal("data.frame", class(df))
    expect_equal(60, ncol(df))
    expect_equal(995, nrow(df))
    expect_equal(fieldtags, names(df))
})

test_that("checking data frame works correctly", {
    df <- read_wos_folder(file.path("test_data"), fix_names = FALSE)
    expect_equal(1, check_data(df))
    names(df)[5] <- "WrongName"
    expect_equal(3, check_data(df))

    df <- read_wos_txt(file.path("test_data/crowdsourcing.txt"), fix_names = FALSE)
    expect_equal(1, check_data(df))
    names(df)[5] <- "WrongName"
    expect_equal(3, check_data(df))

    df <- read_wos_folder(file.path("test_data"), fix_names = TRUE)
    expect_equal(2, check_data(df))
    names(df)[5] <- "WrongName"
    expect_equal(3, check_data(df))

    df <- read_wos_txt(file.path("test_data/crowdsourcing.txt"), fix_names = TRUE)
    expect_equal(2, check_data(df))
    names(df)[5] <- "WrongName"
    expect_equal(3, check_data(df))
})
