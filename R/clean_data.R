
#' Helper function to remove leading and trailing whitespace.
#' @param x A string
#' @return A string
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#' Helper function to remove quotation marks from strings.
#' @param df A data frame
#' @param columns A vector of column names to be processed
#' @return A data frame
remove_quotes <- function(df, columns) {
    for (col in columns) {
        df[[col]] <- gsub("'", "", df[[col]])
        df[[col]] <- gsub('"', "", df[[col]])
    }
    return(df)
}

#' Helper function to change strings to lowercase in
#' selected data frame columns.
#' @param df A data frame
#' @param columns A vector of column names to be processes
#' @return A data frame
change_lowercase <- function(df, columns) {
    for (col in columns) {
        df[[col]] <- tolower(df[[col]])
    }
    return(df)
}

#' Helper function to change strings to uppercase in
#' selected data frame columns.
#' @param df A data frame
#' @param columns A vector of column names to be processes
#' @return A data frame
change_uppercase <- function(df, columns) {
    for (col in columns) {
        df[[col]] <- toupper(df[[col]])
    }
    return(df)
}

#' Reprocess data from WOS for further analysis
#' @param df A data frame containing data from WOS.
#' @return A data frame
clean_wos_data <- function(df) {
    data_type <- check_data(df)
    if (data_type == 1) {
        names(df) <- fixed_fieldtags
    }
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
    df$CitedReferenceCount <- as.numeric(df$CitedReferenceCount)

    df$Location <- sapply(df$AuthorAddress, get_location)

    df$ReferenceString <- apply(df, 1, make_reference)
    df <- df[!duplicated(df[, "ReferenceString"]), ]

    df$id <- 1:nrow(df)
    return(df)
}

#' Helper function to construct reference strings
#' @param x A row from a data frame containing cleaned data from WOS
#' @return  A string in same format as references in CitedReferences
make_reference <- function(x) {
    refstring <- get_name(x)
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

#' Helper function to extract the name of first author
#' @param x A string containing full author names
#' @return A string
get_name <- function(x) {
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

#' Helper function to extract DOIs
#' @param x A list of strings containing DOI in second element
#' @return A string cointaining DOI
get_DOI <- function(x) {
    DOI <- NA
    if (length(x) == 2) {
        DOI <- x[2]
    }
    return(DOI)
}

#' Helper function to extract years
#' @param x A list of strings containing year in the second element
#' @return A year number
get_year <- function(x) {
    year <- NA
    if (length(x) > 1) {
        year <- as.numeric(x[2])
    }
    return(year)
}

#' Helper function for extracting countries and cities from AuthorAddress.
#' @param x A string containing AuthorAddress
#' @return A string containing city and country
get_location <- function(x) {
    country <- NA
    city <- NA
    if (x != "") {
        x <- gsub("\\[.*?\\]", "", x)
        x <- unlist(strsplit(x, ";"))
        x <- x[x != " "]
        cities <- sapply(x, function(x) tail(unlist(strsplit(x, ",")), 2))
        city <- apply(cities, 2, function(x) gsub(".*[0-9]+ ", "", x[1]))
        city <- sapply(city, trim)
        #   country <- gsub(" ", "", cities[2, ])
        country <- sapply(cities[2, ], trim)
        location <- paste(paste(city, country, sep = ", "), collapse = ";")
    }
    else {
        location <- NA
    }
    return(location)
}


#' Organise literature data by selected column
#' @param df A data frame containing the literature data
#' @param selection A string with a column name
#' @param split A character used to split selection
#' @return A data frame with literature organised by selected column
arrange_by <- function(df, selection, sep = ";") {
    # Check data

    # Remove NAs
    arranged_df <- df[!is.na(df[, selection]), ]
    # Create data frame: selection split by split, each element on a new row,
    # id copied to new rows
    arranged_df <- splitstackshape::cSplit(arranged_df,
                                         splitCols = selection,
                                         sep = sep, direction = "long")
    # Transform back to a data frame
    arranged_df <- as.data.frame(arranged_df)

    # Change factor back to character
    arranged_df[, selection] <- as.character(arranged_df[, selection])

    # Removing rows with NA as author name created in previous step
    arranged_df <- arranged_df[!is.na(arranged_df[, selection]), ]

    return(arranged_df)
}

