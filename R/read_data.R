
#' Read all .txt-files downloaded from Web of Science within a folder.
#' @param filepath Path to a folder
#' @param fix_names Should field tags be changed to field names?
#' @return A data frame
read_wos_folder <- function(filepath, fix_names = TRUE) {
    all_files <- list.files(filepath, pattern = ".txt", full.names = TRUE)
    data_sample <- read.delim2(all_files[1], fileEncoding = "UTF-16",
                               quote = "", row.names=NULL,
                               header = FALSE,
                               stringsAsFactors = FALSE,
                               nrows = 10)
    colindex <- !is.na(data_sample[1, ])
    data_names <- as.character(data_sample[1, colindex])

    df <- plyr::ldply(all_files, read.delim2, fileEncoding="UTF-16",
               quote="", row.names=NULL,
               header = FALSE,
               stringsAsFactors = FALSE,
               skip = 1)

    df <- df[, colindex]
    names(df) <- data_names
    df <- df[!duplicated(df), ]

    if(fix_names) {
        names(df) <- fix_column_names(names(df))
    }
    return(df)
}


#' Read a .txt-file downloaded from Web of Science.
#' @param filepath Path to a folder
#' @param fix_names Should field tags be changed to field names?
#' @return A data frame
read_wos_txt <- function(filepath, fix_names = TRUE) {
    data_sample <- read.delim2(filepath, fileEncoding = "UTF-16",
                               quote = "", row.names=NULL,
                               header = FALSE,
                               stringsAsFactors = FALSE,
                               nrows = 10)
    colindex <- !is.na(data_sample[1, ])
    data_names <- as.character(data_sample[1, colindex])

    df <- read.delim2(filepath, fileEncoding="UTF-16",
                      quote="", row.names=NULL,
                      header = FALSE,
                      stringsAsFactors = FALSE,
                      skip = 1)

    df <- df[, colindex]
    names(df) <- data_names
    df <- df[!duplicated(df), ]

    if(fix_names) {
        names(df) <- fix_column_names(names(df))
    }
    return(df)
}

#' Read file or folder of files downloaded from Web of Science
#' @param filepath Path to a file or folder containting the data
#' @return A data frame
#' @export
read_wos_data <- function(filepath) {
    file_info <- file.info(filepath)
    df <- NA
    if (file_info$isdir == TRUE) {
        df <- read_wos_folder(filepath)
    }
    if (file_info$isdir == FALSE) {
        df <- read_wos_txt(filepath)
    }
    return(df)
}

#' Fix column names of dataset from Web of Science
#' @param column_names Column names
#' @return Fixed column names
fix_column_names <- function(column_names, fix_format = TRUE) {
    fields <- fieldtags$field[match(column_names, fieldtags$tag)]
    fields[is.na(fields)] <- column_names[is.na(fields)]
    fields <- gsub(" ", "", fields)

    if (fix_format) {
        fields[fields == "KeywordsPlus®"] <- "KeywordsPlus"
        fields[fields ==
            "PublicationType(J=Journal;B=Book;S=Series)"] <- "PublicationType"
        fields[fields ==
            "29-CharacterSourceAbbreviation"] <- "SourceAbbreviation"
        fields[fields == "DigitalObjectIdentifier(DOI)"] <- "DOI"
        fields[fields ==
            "ElectronicInternationalStandardSerialNumber(eISSN)"] <- "eISSN"
        fields[fields ==
            "ORCIDIdentifier(OpenResearcherandContributorID)"] <- "ORCID"
        fields[fields ==
            paste("TotalTimesCited(WebofScienceCore,BIOSISCitationIndex,",
            "andChinese\nScienceCitationDatabase)", sep = "")] <- "TimesCited"
        fields[fields == "UsageCount(Last180Days)"] <- "UsageCountLast180Days"
        fields[fields == "UsageCount(Since2013)"] <- "UsageCountSince2013"
        fields[fields == "BookDigitalObjectIdentifier(DOI)"] <- "BookDOI"
        fields[fields == "E-mailAddress" ] <- "EmailAddress"
    }

    return(fields)
}

#' Check the data frame columns.
#' @param df A data frame
#' @return An integer from 1 to 3
check_data <- function(df) {
    if (length(names(df)[1]) == 2) {
        return(1)
    }
    if (length(names(df)[1]) > 2) {
        return(2)
    } else {
        return(3)
    }
}
