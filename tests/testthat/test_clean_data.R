library(nails)
context("Cleaning data")

a <- c("Fo'o", 'Ba"r', "Baz")
b <- c("Fo'o", 'Ba"r', "Baz")
c <- c("Fo'o", 'Ba"r', "Baz")
df_1 <- data.frame(a, b, c, stringsAsFactors = FALSE)

a <- c("Foo", 'Bar', "Baz")
b <- c("Foo", 'Bar', "Baz")
df_2 <- data.frame(a, b, c, stringsAsFactors = FALSE)

a <- c("foo", 'bar', "baz")
b <- c("foo", 'bar', "baz")
df_3 <- data.frame(a, b, c, stringsAsFactors = FALSE)

test_that("quotes are removed", {
    expect_equal(df_2, remove_quotes(df_1, c("a", "b")))
})
