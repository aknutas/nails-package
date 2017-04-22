
#' Read all .txt-files downloaded from Web of Science within a folder.
#'
#' @param filepath Path to a folder
#' @return A data frame
read_wos_folder <- function(filepath) {
    all_files <- list.files(filepath, pattern = ".txt", full.names = TRUE)
    df <- plyr::ldply(all_files, read.delim2, fileEncoding="UTF-16",
               quote="", row.names=NULL, stringsAsFactors = FALSE)
    df_names <- names(df)[2:length(names(df))]
    df <- df[, 1:(ncol(df) - 1)]
    names(df) <- df_names
    names(df) <- fix_column_names(names(df))
    return(df)
}


#' Read a .txt-file downloaded from Web of Science.
#'
#' @param filepath Path to a folder
#' @return A data frame
read_wos_txt <- function(filepath) {
    df <- read.delim2(filepath, fileEncoding="UTF-16",
                      quote="", row.names=NULL, stringsAsFactors = FALSE)
    df_names <- names(df)[2:length(names(df))]
    df <- df[, 1:(ncol(df) - 1)]
    names(df) <- df_names
    names(df) <- fix_column_names(names(df))
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
