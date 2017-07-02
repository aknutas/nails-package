library(nails)
context("Cleaning data")

a <- c("Fo'o", 'Ba"r', "Baz")
b <- c("Fo'o", 'Ba"r', "Baz")
c <- c("Fo'o", 'Ba"r', "Baz")
df <- data.frame(a, b, c, stringsAsFactors = FALSE)

test_that("quotes are removed", {
    a <- c("Foo", 'Bar', "Baz")
    b <- c("Foo", 'Bar', "Baz")
    c <- c("Fo'o", 'Ba"r', "Baz")
    df_test <- data.frame(a, b, c, stringsAsFactors = FALSE)
    expect_equal(df_test, remove_quotes(df, c("a", "b")))
})

test_that("specified columns are changed to lowercase", {
    a <- c("fo'o", 'ba"r', "baz")
    b <- c("fo'o", 'ba"r', "baz")
    c <- c("Fo'o", 'Ba"r', "Baz")
    df_test <- data.frame(a, b, c, stringsAsFactors = FALSE)
    expect_equal(df_test, change_lowercase(df, c("a", "b")))
})

test_that("specified columns are changed to uppercase", {
    a <- c("FO'O", 'BA"R', "BAZ")
    b <- c("FO'O", 'BA"R', "BAZ")
    c <- c("Fo'o", 'Ba"r', "Baz")
    df_test <- data.frame(a, b, c, stringsAsFactors = FALSE)
    expect_equal(df_test, change_uppercase(df, c("a", "b")))
})

test_that("removing whitespace works", {
    expect_equal("hello world !", trim("  hello world ! "))
})

