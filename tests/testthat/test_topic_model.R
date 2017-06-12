library(nails)
context("Building topic models")

# Loads test data frame "sublit" into memory
load("data/topicmodeltest_df.RData")

test_that("Topicmodels input data gets cleaned properly", {
    data <- nails::preprocess_literature_for_topicmodeling(sublit)
    expect_is(data, "list")
    # Character vector
    expect_is(data$textlist, "character")
    expect_is(data$doctablewt, "data.frame")
    expect_equal(3, ncol(data$doctablewt))
    expect_equal(50, nrow(data$doctablewt))
    expect_equal(50, length(data$textlist))
})

test_that("stm corpus gets built", {
    data <- nails::preprocess_literature_for_topicmodeling(sublit)
    corpus <- nails:::build_stm_corpus(data$textlist)
    expect_is(corpus, "list")
    expect_equal(50, length(corpus$documents))
})

test_that("topicmodels corpus gets built", {
    data <- nails::preprocess_literature_for_topicmodeling(sublit)
    corpus <- nails:::build_topicmodels_corpus(data$textlist)
    expect_is(corpus, "SimpleCorpus")
    expect_equal(50, length(corpus))
})

test_that("Search optimal K with semantic coherence", {
    data <- nails::preprocess_literature_for_topicmodeling(sublit)
    bestks <- nails::select_optimal_k(data)
    expect_is(bestks, "list")
    expect_is(bestks$bestk, "integer")
    expect_is(bestks$semcohvalues, "data.frame")
    expect_is(bestks$topickest, "searchK")
})

test_that("Full topicmodels process", {
    testlist <- nails::build_topicmodel_from_literature(sublit)
    expect_is(testlist, "list")
    expect_is(testlist$fit, "LDA_Gibbs")
    expect_is(testlist$thetadf, "data.frame")
    expect_is(testlist$doctablewt, "data.frame")
})
