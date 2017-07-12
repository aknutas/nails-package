library(nails)
context("Creating an author network")

# Read and clean test data
df <- read_wos_txt(file.path("test_data/savedrecs-mactab.txt"), fix_names = TRUE)
df <- clean_wos_data(df)

# Pick 3 rows for further processing
df <- df[c(3, 36, 100), ]

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
               "RAAD, ELIANA J.")
author_label <- author_id
author_freq <- rep(1, 12)
author_timescited <- rep(0, 12)
author_address <- c("US Forest Serv, 1033 Univ Pl,Ste 360, Evanston, IL 60201 U",
                    "US Forest Serv, 1033 Univ Pl,Ste 360, Evanston, IL 60201 U",
                    "Clark Univ, Grad Sch Geog, Worcester, MA 01610 USA",
                    "Univ Siegen, Inst Informat Syst, Siegen, Germany",
                    "Univ Siegen, Inst Informat Syst, Siegen, Germany",
                    "Univ Siegen, Inst Informat Syst, Siegen, Germany",
                    "Univ Siegen, Inst Informat Syst, Siegen, Germany",
                    "Univ Siegen, Inst Informat Syst, Siegen, Germany",
                    "Mem Univ Newfoundland, Fac Business, St John, NF A1C 5S7, Canada",
                    "Univ Pau, LIUPPA Lab, Pau, France",
                    "Univ Bourgogne, LE2I Lab, F-21004 Dijon, France",
                    "Univ Bourgogne, LE2I Lab, F-21004 Dijon, France")
# author_reprintaddress not tested
# Author email, improve processing!
author_email <- c(rep("sonyasachdeva@fs.fed.us; smccaffrey@fs.fed.us; dexter.locke@gmail.com", 3),
                  rep("thomas.ludwig@uni-siegen.de", 5),
                  rep("eliraad@gmail.com; richard.chbeir@univ-pau.fr; albert.dipanda@u-bourgogne.fr; eliana.raad@u-bourgogne.fr", 4))
# Author location, improve processing!
author_location <- c(rep("Evanston, IL 60201 USA;Worcester, MA 01610 USA", 3),
                     rep("Siegen, Germany", 5),
                     rep("NF A1C 5S7, Canada;Pau, France;Dijon, France", 4))
comparison_df <- data.frame(author_id,
                            author_label,
                            author_freq,
                            author_timescited,
                            author_address,
                            author_email,
                            author_location,
                            stringsAsFactors = FALSE)
comparison_df <- comparison_df[with( comparison_df, order(author_id)),]

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
