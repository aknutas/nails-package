
remove_quotes <- function(df, columns) {
    for (col in columns) {
        df[[col]] <- gsub("'", "", df[[col]])
        df[[col]] <- gsub('"', "", df[[col]])
    }
    return(df)
}

change_lowercase <- function(df, columns) {
    for (col in columns) {
        df[[col]] <- tolower(df[[col]])
    }
    return(df)
}

change_uppercase <- function(df, columns) {
    for (col in columns) {
        df[[col]] <- toupper(df[[col]])
    }
    return(df)
}

clean_wos_data <- function(df) {
    df <- change_lowercase(df, columns = c("AuthorKeywords",
                                           "KeywordsPlus",
                                           "SubjectCategory"))

    df <- change_uppercase(df, columns = c("AuthorFullName",
                                           "CitedReferences",
                                           "DOI"))

    df <- remove_quotes(df, columns = c("AuthorFullName",
                                  "AuthorKeywords",
                                  "KeywordsPlus",
                                  "DocumentTitle",
                                  "SubjectCategory",
                                  "CitedReferences"))

    df$CitedReferences <- gsub("DOI DOI", "DOI", df$CitedReferences)
    df$YearPublished <- as.numeric(df$YearPublished)
    df$TimesCited <- as.numeric(df$TimesCited)
}

# Helper function to construct strings
makeRef <- function(x) {
    refstring <- getName(x)
    if (!is.na(x["YearPublished"])) {
        refstring <- paste(refstring, x["YearPublished"], sep = ", ")
    }
    if (x["SourceAbbreviation"] != "") {
        refstring <- paste(refstring, x["SourceAbbreviation"], sep = ", ")
    }
    if (!is.na(x["Volume"])) {
        refstring <- paste(refstring, ", V", x["Volume"], sep = "")
    }
    if (!is.na(x["BeginningPage"])) {
        refstring <- paste(refstring, ", P", x["BeginningPage"], sep = "")
    }
    if (x["DOI"] != "") {
        refstring <- paste(refstring, ", DOI ", x["DOI"], sep = "")
    }
    return(refstring)
}

# Helper function to extract the name of first author
getName <- function(x) {
    name = NA
    try( {
        names <- unlist(strsplit(x["AuthorFullName"], ";"))
        names <- names[1]
        names <- unlist(strsplit(names, " "))
        name <- names[1]
        name <- gsub(",", "", name)
        if (length(names) > 1) {
            name <- paste(name, substring(names[2], 1, 1))
        }
        if (length(names) > 2) {
            name <- paste(name, substring(names[3], 1, 1), sep = "")
        }
    } )
    return(name)
}
