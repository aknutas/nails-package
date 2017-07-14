library(nails)
context("Reading data")

test_that("single txt-file is read correctly, fix_names = TRUE", {
    df <- read_wos_txt(file.path("test_data/savedrecs-mactab.txt"), fix_names = TRUE)
    expect_equal("data.frame", class(df))
    expect_equal(66, ncol(df))
    expect_equal(100, nrow(df))
})

test_that("single txt-file is read correctly, fix_names = FALSE", {
    df <- read_wos_txt(file.path("test_data/savedrecs-mactab.txt"), fix_names = FALSE)
    expect_equal("data.frame", class(df))
    expect_equal(66, ncol(df))
    expect_equal(100, nrow(df))
})


test_that("txt-files in folder are read correctly, fix_names = TRUE", {
    df <- read_wos_folder(file.path("test_data"), fix_names = TRUE)
    expect_equal("data.frame", class(df))
    expect_equal(66, ncol(df))
    expect_equal(600, nrow(df))
})

test_that("read_wos_data reads a file correctly", {
    df <- read_wos_data(file.path("test_data/savedrecs-mactab.txt"))
    expect_equal("data.frame", class(df))
    expect_equal(66, ncol(df))
    expect_equal(100, nrow(df))
})

test_that("read_wos_data reads a folder correctly", {
    df <- read_wos_data(file.path("test_data"))
    expect_equal("data.frame", class(df))
    expect_equal(66, ncol(df))
    expect_equal(600, nrow(df))
})

# test_that("txt-files in folder are read correctly, fix_names = FALSE", {
#     df <- read_wos_folder(file.path("test_data"), fix_names = FALSE)
#     expect_equal("data.frame", class(df))
#     expect_equal(60, ncol(df))
#     expect_equal(995, nrow(df))
#     expect_equal(fieldtags, names(df))
# })

# test_that("checking data frame works correctly", {
#     df <- read_wos_folder(file.path("test_data"), fix_names = FALSE)
#     expect_equal(1, check_data(df))
#     names(df)[5] <- "WrongName"
#     expect_equal(3, check_data(df))
#
#     df <- read_wos_txt(file.path("test_data/crowdsourcing.txt"), fix_names = FALSE)
#     expect_equal(1, check_data(df))
#     names(df)[5] <- "WrongName"
#     expect_equal(3, check_data(df))
#
#     df <- read_wos_folder(file.path("test_data"), fix_names = TRUE)
#     expect_equal(2, check_data(df))
#     names(df)[5] <- "WrongName"
#     expect_equal(3, check_data(df))
#
#     df <- read_wos_txt(file.path("test_data/crowdsourcing.txt"), fix_names = TRUE)
#     expect_equal(2, check_data(df))
#     names(df)[5] <- "WrongName"
#     expect_equal(3, check_data(df))
# })
