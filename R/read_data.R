
#' Read all .txt-files downloaded from Web of Science within a folder.
#'
#' @param filepath Path to a folder
#' @param fix_names Should field tags be changed to field names?
#' @return A data frame
read_wos_folder <- function(filepath, fix_names = TRUE) {
    all_files <- list.files(filepath, pattern = ".txt", full.names = TRUE)
    df <- plyr::ldply(all_files, read.delim2, fileEncoding="UTF-16",
               quote="", row.names=NULL,
               header = FALSE,
               stringsAsFactors = FALSE)
    colindex <- !is.na(df[1, ])
    df <- df[!duplicated(df), ]
    data_names <- as.character(df[1, colindex])
    df <- df[-1, colindex]
    names(df) <- data_names
    if(fix_names) {
        names(df) <- fix_column_names(names(df))
    }
    return(df)
}


#' Read a .txt-file downloaded from Web of Science.
#'
#' @param filepath Path to a folder
#' @param fix_names Should field tags be changed to field names?
#' @return A data frame
read_wos_txt <- function(filepath, fix_names = TRUE) {
    df <- read.delim2(filepath, fileEncoding="UTF-16",
                      quote="", row.names=NULL,
                      header = FALSE,
                      stringsAsFactors = FALSE)
    colindex <- !is.na(df[1, ])
    data_names <- as.character(df[1, colindex])
    df <- df[-1, colindex]
    names(df) <- data_names
    if (fix_names) {
        names(df) <- fix_column_names(names(df))
    }
    return(df)
}

#' Fix column names of dataset from Web of Science
#'
#' @param column_names Column names
#' @return Fixed column names
fix_column_names <- function(column_names) {
    fields <- fieldtags$field[match(column_names, fieldtags$tag)]
    fields[is.na(fields)] <- column_names[is.na(fields)]
    fields <- gsub(" ", "", fields)

    fields[fields == "KeywordsPlus\xfc\xbe\x8e\x86\x84\xbc"] <- "KeywordsPlus"
    fields[fields == "PublicationType(conference,book,journal,bookinseries,orpatent)"] <- "PublicationType"
    fields[fields == "29-CharacterSourceAbbreviation"] <- "SourceAbbreviation"
    fields[fields == "DigitalObjectIdentifier(DOI)" ] <- "DOI"

    return(fields)
}

#' Check the data frame columns.
#' @param df A data frame
#' @return An integer from 1 to 3
check_data <- function(df) {
    if (isTRUE(all.equal(names(df), original_fieldtags))) {
        return(1)
    }
    if (isTRUE(all.equal(names(df), fixed_fieldtags))) {
        return(2)
    } else {
        return(3)
    }
}
