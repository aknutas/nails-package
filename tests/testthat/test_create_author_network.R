library(nails)
context("Creating an author network")

# Read and clean test data
df <- read_wos_data(file.path("test_data"))
df <- clean_wos_data(df)

# Pick 3 rows for further processing
df <- df[c(3, 36, 100, 295), ]

# Reference data
author_id <- c("SACHDEVA, SONYA",
               "MCCAFFREY, SARAH",
               "LOCKE, DEXTER",
               "LUDWIG, THOMAS",
               "KOTTHAUS, CHRISTOPH",
               "REUTER, CHRISTIAN",
               "VAN DONGEN, SOREN",
               "PIPEK, VOLKMAR",
               "RAAD, ELIE",
               "CHBEIR, RICHARD",
               "DIPANDA, ALBERT",
               "RAAD, ELIANA J.",
               "JANAKI, NAFISEH",
               "WHITNEY, JON",
               "JANOWCZYK, ANDREW",
               "MADABHUSHI, ANANT",
               "AVRIL, STEFANIE")
author_label <- author_id
author_freq <- rep(1, 17)
author_timescited <- rep(0, 17)
author_address <- c("US Forest Serv, 1033 Univ Pl,Ste 360, Evanston, IL 60201 USA",
                    "US Forest Serv, 1033 Univ Pl,Ste 360, Evanston, IL 60201 USA",
                    "Clark Univ, Grad Sch Geog, Worcester, MA 01610 USA",
                    "Univ Siegen, Inst Informat Syst, Siegen, Germany",
                    "Univ Siegen, Inst Informat Syst, Siegen, Germany",
                    "Univ Siegen, Inst Informat Syst, Siegen, Germany",
                    "Univ Siegen, Inst Informat Syst, Siegen, Germany",
                    "Univ Siegen, Inst Informat Syst, Siegen, Germany",
                    "Mem Univ Newfoundland, Fac Business, St John, NF A1C 5S7, Canada",
                    "Univ Pau, LIUPPA Lab, Pau, France",
                    "Univ Bourgogne, LE2I Lab, F-21004 Dijon, France",
                    "Univ Bourgogne, LE2I Lab, F-21004 Dijon, France",
                    "Univ Hosp Cleveland Med Ctr, Cleveland, OH USA; Case Western Reserve Univ, Cleveland, OH 44106 USA",
                    "Univ Hosp Cleveland Med Ctr, Cleveland, OH USA; Case Western Reserve Univ, Cleveland, OH 44106 USA",
                    "Univ Hosp Cleveland Med Ctr, Cleveland, OH USA; Case Western Reserve Univ, Cleveland, OH 44106 USA",
                    "Univ Hosp Cleveland Med Ctr, Cleveland, OH USA; Case Western Reserve Univ, Cleveland, OH 44106 USA",
                    "Univ Hosp Cleveland Med Ctr, Cleveland, OH USA; Case Western Reserve Univ, Cleveland, OH 44106 USA")
# author_reprintaddress not tested
# Author email, improve processing!
author_email <- c(rep("sonyasachdeva@fs.fed.us; smccaffrey@fs.fed.us; dexter.locke@gmail.com", 3),
                  rep("thomas.ludwig@uni-siegen.de", 5),
                  rep("eliraad@gmail.com; richard.chbeir@univ-pau.fr; albert.dipanda@u-bourgogne.fr; eliana.raad@u-bourgogne.fr", 4),
                  rep("", 5))
# Author location, improve processing!
author_location <- c(rep("Evanston, IL 60201 USA;Worcester, MA 01610 USA", 3),
                     rep("Siegen, Germany", 5),
                     rep("NF A1C 5S7, Canada;Pau, France;Dijon, France", 4),
                     rep("Cleveland, OH USA;Cleveland, OH 44106 USA", 5 ))

comparison_df <- data.frame(author_id,
                            author_label,
                            author_freq,
                            author_timescited,
                            author_address,
                            author_email,
                            author_location,
                            stringsAsFactors = FALSE)

comparison_df <- comparison_df[with( comparison_df, order(author_id)),]

Source <- c("SACHDEVA, SONYA",
            "SACHDEVA, SONYA",
            "MCCAFFREY, SARAH",
            "LUDWIG, THOMAS",
            "LUDWIG, THOMAS",
            "LUDWIG, THOMAS",
            "LUDWIG, THOMAS",
            "KOTTHAUS, CHRISTOPH",
            "KOTTHAUS, CHRISTOPH",
            "KOTTHAUS, CHRISTOPH",
            "REUTER, CHRISTIAN",
            "REUTER, CHRISTIAN",
            "VAN DONGEN, SOREN",
            "RAAD, ELIE",
            "RAAD, ELIE",
            "RAAD, ELIE",
            "CHBEIR, RICHARD",
            "CHBEIR, RICHARD",
            "DIPANDA, ALBERT",
            "JANAKI, NAFISEH",
            "JANAKI, NAFISEH",
            "JANAKI, NAFISEH",
            "JANAKI, NAFISEH",
            "WHITNEY, JON",
            "WHITNEY, JON",
            "WHITNEY, JON",
            "JANOWCZYK, ANDREW",
            "JANOWCZYK, ANDREW",
            "MADABHUSHI, ANANT")

Target <- c("MCCAFFREY, SARAH",
            "LOCKE, DEXTER",
            "LOCKE, DEXTER",
            "KOTTHAUS, CHRISTOPH",
            "REUTER, CHRISTIAN",
            "VAN DONGEN, SOREN",
            "PIPEK, VOLKMAR",
            "REUTER, CHRISTIAN",
            "VAN DONGEN, SOREN",
            "PIPEK, VOLKMAR",
            "VAN DONGEN, SOREN",
            "PIPEK, VOLKMAR",
            "PIPEK, VOLKMAR",
            "CHBEIR, RICHARD",
            "DIPANDA, ALBERT",
            "RAAD, ELIANA J.",
            "DIPANDA, ALBERT",
            "RAAD, ELIANA J.",
            "RAAD, ELIANA J.",
            "WHITNEY, JON",
            "JANOWCZYK, ANDREW",
            "MADABHUSHI, ANANT",
            "AVRIL, STEFANIE",
            "JANOWCZYK, ANDREW",
            "MADABHUSHI, ANANT",
            "AVRIL, STEFANIE",
            "MADABHUSHI, ANANT",
            "AVRIL, STEFANIE",
            "AVRIL, STEFANIE")

test_that("get_author_nodes works", {
    nodes <- get_author_nodes(df)
    nodes <- nodes[with( nodes, order(Id)), ]
    expect_equal(nodes$Id, comparison_df$author_id)
    expect_equal(nodes$Label, comparison_df$author_label)
    expect_equal(nodes$Freq, comparison_df$author_freq)
    expect_equal(nodes$TotalTimesCited, comparison_df$author_timescited)
    expect_equal(nodes$AuthorAddress, comparison_df$author_address)
    expect_equal(nodes$Location, comparison_df$author_location)
})

test_that("get_author_edges works", {
    edges <- get_author_edges(df)
    expect_equal(Source, edges$Source)
    expect_equal(Target, edges$Target)

})

test_that("get_author_network works", {
    author_network <- get_author_network(df)
    nodes <- author_network$author_nodes
    nodes <- nodes[with( nodes, order(Id)), ]
    edges <- author_network$author_edges

    # Nodes
    expect_equal(nodes$Id, comparison_df$author_id)
    expect_equal(nodes$Label, comparison_df$author_label)
    expect_equal(nodes$Freq, comparison_df$author_freq)
    expect_equal(nodes$TotalTimesCited, comparison_df$author_timescited)
    expect_equal(nodes$AuthorAddress, comparison_df$author_address)
    expect_equal(nodes$Location, comparison_df$author_location)

    # Edges
    expect_equal(Source, edges$Source)
    expect_equal(Target, edges$Target)
})
