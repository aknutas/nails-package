# These functions can be used to update internal list of fieldtags
# in case Web of Science changes them.

# Url addresses for Web of Science field tag lists

# Primary source for field tags
FIELDTAG_URL_MAIN <- "https://images.webofknowledge.com/WOKRS53B4/help/WOS/hs_wos_fieldtags.html"

# Secondary sources for field tags. These are used for field tags not found in
# the primary source.
FIELDTAG_URL_EXTRA <- "https://images.webofknowledge.com/images/help/WOK/hs_alldb_fieldtags.html"
FIELDTAGS_URL_EXTRA_2 <- "https://images.webofknowledge.com/images/help/WOS/hs_wos_fieldtags.html"

#' Helper function for downloading fieldtags from Web of Science
#' @param url A URL as a string
#' @return A data frame containing fieldtags downloaded from given URL
get_fieldtags <- function(url) {
    # Load web page and read the second table on the page
    url <- RCurl::getURL(url)
    fieldtags <- XML::readHTMLTable(url)[[2]]

    # Fix column names and data types
    names(fieldtags) <- c("tag", "field")
    fieldtags$tag <- as.character(fieldtags$tag)
    fieldtags$field <- as.character(fieldtags$field)

    return(fieldtags)
}

#' Download and merge all fieldtags from Web of Science
#' @param main_url A URL for primary field tags
#' @param extra_url A URL for supplementary field tags. Used for field tags missing in main_url.
#' @param extra_url_2 A URL for supplementary field tags. Used for field tags missing in extra_url.
#' @return A data frame containing all field tags
get_all_fieldtags <- function(main_url, extra_url, extra_url_2) {
    # Get primary field tags
    fieldtags_main <- get_fieldtags(main_url)

     # Get secondary field tags
    fieldtags_extra <- get_fieldtags(extra_url)

    # Get tetriary field tags
    fieldtags_extra_2 <- get_fieldtags(extra_url_2)

    # Merge all field tag lists
    all_fieldtags <- merge(fieldtags_main, fieldtags_extra,
                           by.x = "tag",
                           by.y = "tag",
                           all = TRUE)
    all_fieldtags <- merge(all_fieldtags, fieldtags_extra_2,
                           by.x = "tag",
                           by.y = "tag",
                           all = TRUE)

    # Use secondary field tags if primary is missing
    index <- is.na(all_fieldtags$field.x)
    all_fieldtags$field.x[index] <- all_fieldtags$field.y[index]

    # Use tetriary field tags if primary is still missing
    index <- is.na(all_fieldtags$field.x)
    all_fieldtags$field.x[index] <- all_fieldtags$field[index]

    # Drop unnecessary columns and fix names
    fieldtags <- all_fieldtags[, c("tag", "field.x")]
    names(fieldtags) <- c("tag", "field")

    return(fieldtags)
}

#' Update internally saved field tags
#' @return Announcement that fieldtags have been updated.
update_fieldtags <- function() {
    # Get up to date field tags from Web of Science
    fieldtags <- get_all_fieldtags(FIELDTAG_URL_MAIN,
                                   FIELDTAG_URL_EXTRA,
                                   FIELDTAGS_URL_EXTRA_2)

    # Save new field tags as internal data
    devtools::use_data(fieldtags, internal = TRUE, overwrite = TRUE)

    return("Fieldtags updated!")
}

