library(nails)
context("Creating a citation network")

# Read and clean test data
df <- read_wos_txt(file.path("test_data/savedrecs-mactab.txt"), fix_names = TRUE)
df <- clean_wos_data(df)

# Pick 3 rows for further processing
df <- df[c(3, 36, 100), ]

# Reference data
number_of_rows <- sum(df$CitedReferenceCount)
reference_1 <- "BARRINGTON L, 2011, ANN GEOPHYS-ITALY, V54, P680, DOI 10.4401/AG-5324"
reference_43 <- "WANG C., 2011, P 17 ACM SIGKDD INT, P448, DOI 10.1145/2020408.2020480"
reference_44 <- "ALDUNCE P, 2014, DISASTER PREV MANAG, V23, P252, DOI 10.1108/DPM-07-2013-0130"
reference_148 <- "ZHANG T., 2009, P 17 ACM INT C MULT, P597, DOI 10.1145/1631272.1631365"

year_1 <- 2011
year_43 <- 2011
year_44 <- 2014
year_148 <- 2009

DOI_1 <- "10.4401/AG-5324"
DOI_43 <- "10.1145/2020408.2020480"
DOI_44 <- "10.1108/DPM-07-2013-0130"
DOI_148 <- "10.1145/1631272.1631365"

nodes_colnames <- c("Id",
                    "YearPublished",
                    "FullReference",
                    "id",
                    "PublicationType",
                    "AuthorFullName",
                    "DocumentTitle",
                    "PublicationName",
                    "BookSeriesTitle",
                    "Language",
                    "DocumentType",
                    "ConferenceTitle",
                    "ConferenceDate",
                    "ConferenceLocation",
                    "ConferenceSponsors",
                    "AuthorKeywords",
                    "SubjectCategory",
                    "TimesCited",
                    "Abstract",
                    "DOI",
                    "Origin",
                    "Label")

citation_network_nodes_colnames <- c(nodes_colnames,
                                     "PageRank",
                                     "InDegree")

edges_id <- c(rep(3, 43), rep(36, 66), rep(100, 40))
edges_DocumentTitle <- c(rep(df$DocumentTitle[1], 43),
                         rep(df$DocumentTitle[2], 66),
                         rep(df$DocumentTitle[3], 40))


test_that("get_reference_list works", {
    rl <- get_reference_list(df)
    # Number of rows
    expect_equal(nrow(rl), number_of_rows)
    # Full reference string
    expect_equal(rl$FullReference[1], reference_1)
    expect_equal(rl$FullReference[43], reference_43)
    expect_equal(rl$FullReference[44], reference_44)
    expect_equal(rl$FullReference[148], reference_148)
    # Year
    expect_equal(rl$ReferenceYear[1], year_1)
    expect_equal(rl$ReferenceYear[43], year_43)
    expect_equal(rl$ReferenceYear[44], year_44)
    expect_equal(rl$ReferenceYear[148], year_148)
    # DOI
    expect_equal(rl$Reference[1], DOI_1)
    expect_equal(rl$Reference[43], DOI_43)
    expect_equal(rl$Reference[44], DOI_44)
    expect_equal(rl$Reference[148], DOI_148)
    # Missing values
    expect_equal(sum(is.na(rl$Reference)), 0)
})

test_that("get_reference_nodes works", {
    nodes <- get_reference_nodes(rl)
    # Number of rows
    expect_equal(nrow(nodes), number_of_rows - sum(duplicated(rl$Reference)))
    # ID
    expect_equal(nodes$Id[1], DOI_1)
    expect_equal(nodes$Id[43], DOI_43)
    expect_equal(nodes$Id[44], DOI_44)
    expect_equal(nodes$Id[147], DOI_148)
    # Year
    expect_equal(nodes$YearPublished[1], year_1)
    expect_equal(nodes$YearPublished[43], year_43)
    expect_equal(nodes$YearPublished[44], year_44)
    expect_equal(nodes$YearPublished[147], year_148)
    # Full reference string
    expect_equal(nodes$FullReference[1], reference_1)
    expect_equal(nodes$FullReference[43], reference_43)
    expect_equal(nodes$FullReference[44], reference_44)
    expect_equal(nodes$FullReference[147], reference_148)
    # Column names
    expect_equal(names(nodes), nodes_colnames)
    # Duplicated values
    expect_equal(sum(duplicated(nodes)), 0)

})

test_that("get_literature_nodes works", {
    nodes_2 <- get_literature_nodes(df)
    # Number of rows
    expect_equal(nrow(nodes_2), nrow(df))
    # ID
    expect_equal(nodes_2$Id, df$DOI)
    # Year
    expect_equal(nodes_2$YearPublished, df$YearPublished)
    # Full reference string
    expect_equal(nodes_2$FullReference, df$ReferenceString)
    # Column names
    expect_equal(names(nodes_2), nodes_colnames)
    # Duplicated values
    expect_equal(sum(duplicated(nodes_2)), 0)
})

test_that("get_citation_nodes works", {
    nodes_3 <- get_citation_nodes(df, rl)
    # Number of rows
    expect_equal(nrow(nodes_3), number_of_rows - sum(duplicated(rl$Reference)) + 3)
    # ID
    expect_equal(nodes_3$Id[1], DOI_1)
    expect_equal(nodes_3$Id[43], DOI_43)
    expect_equal(nodes_3$Id[44], DOI_44)
    expect_equal(nodes_3$Id[147], DOI_148)
    expect_equal(tail(nodes_3$Id, 3), df$DOI)
    # Year
    expect_equal(nodes_3$YearPublished[1], year_1)
    expect_equal(nodes_3$YearPublished[43], year_43)
    expect_equal(nodes_3$YearPublished[44], year_44)
    expect_equal(nodes_3$YearPublished[147], year_148)
    expect_equal(tail(nodes_3$YearPublished, 3), df$YearPublished)
    # Full reference string
    expect_equal(nodes_3$FullReference[1], reference_1)
    expect_equal(nodes_3$FullReference[43], reference_43)
    expect_equal(nodes_3$FullReference[44], reference_44)
    expect_equal(nodes_3$FullReference[147], reference_148)
    expect_equal(tail(nodes_3$FullReference, 3), df$ReferenceString)
    # Column names
    expect_equal(names(nodes_3), nodes_colnames)
    # Duplicated values
    expect_equal(sum(duplicated(nodes_3)), 0)
})

test_that("get_citation_edges works", {
    edges <- get_citation_edges(rl)
    # Number of rows
    expect_equal(nrow(edges), number_of_rows)
    # Source
    expect_equal(edges$Source[1:43], rep(df$DOI[1], 43))
    expect_equal(edges$Source[44:109], rep(df$DOI[2], 66))
    expect_equal(edges$Source[110:149], rep(df$DOI[3], 40))
    # Target
    expect_equal(edges$Target[1], DOI_1)
    expect_equal(edges$Target[43], DOI_43)
    expect_equal(edges$Target[44], DOI_44)
    expect_equal(edges$Target[148], DOI_148)
    # id
    expect_equal(edges$id, edges_id)
    # Year Published
    expect_equal(edges$YearPublished, rep(2017, nrow(edges)))
    # Document title
    expect_equal(edges$DocumentTitle, edges_DocumentTitle)
    # NAs
    expect_equal(sum(is.na(edges$Source)), 0)
    expect_equal(sum(is.na(edges$Target)), 0)
})

test_that("get_citation_network works", {
    citation_network <- get_citation_network(df)
    nodes_3 <- citation_network$citation_nodes
    edges <- citation_network$citation_edges

    # Nodes
    # Number of rows
    expect_equal(nrow(nodes_3), number_of_rows - sum(duplicated(rl$Reference)) + 3)
    # ID
    expect_equal(nodes_3$Id[1], DOI_1)
    expect_equal(nodes_3$Id[43], DOI_43)
    expect_equal(nodes_3$Id[44], DOI_44)
    expect_equal(nodes_3$Id[147], DOI_148)
    expect_equal(tail(nodes_3$Id, 3), df$DOI)
    # Year
    expect_equal(nodes_3$YearPublished[1], year_1)
    expect_equal(nodes_3$YearPublished[43], year_43)
    expect_equal(nodes_3$YearPublished[44], year_44)
    expect_equal(nodes_3$YearPublished[147], year_148)
    expect_equal(tail(nodes_3$YearPublished, 3), df$YearPublished)
    # Full reference string
    expect_equal(nodes_3$FullReference[1], reference_1)
    expect_equal(nodes_3$FullReference[43], reference_43)
    expect_equal(nodes_3$FullReference[44], reference_44)
    expect_equal(nodes_3$FullReference[147], reference_148)
    expect_equal(tail(nodes_3$FullReference, 3), df$ReferenceString)
    # Column names
    expect_equal(names(nodes_3), citation_network_nodes_colnames)
    # Duplicated values
    expect_equal(sum(duplicated(nodes_3)), 0)

    # Edges
    # Number of rows
    expect_equal(nrow(edges), number_of_rows)
    # Source
    expect_equal(edges$Source[1:43], rep(df$DOI[1], 43))
    expect_equal(edges$Source[44:109], rep(df$DOI[2], 66))
    expect_equal(edges$Source[110:149], rep(df$DOI[3], 40))
    # Target
    expect_equal(edges$Target[1], DOI_1)
    expect_equal(edges$Target[43], DOI_43)
    expect_equal(edges$Target[44], DOI_44)
    expect_equal(edges$Target[148], DOI_148)
    # id
    expect_equal(edges$id, edges_id)
    # Year Published
    expect_equal(edges$YearPublished, rep(2017, nrow(edges)))
    # Document title
    expect_equal(edges$DocumentTitle, edges_DocumentTitle)
    # NAs
    expect_equal(sum(is.na(edges$Source)), 0)
    expect_equal(sum(is.na(edges$Target)), 0)
})


