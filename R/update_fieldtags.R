
FIELDTAG_URL_MAIN <- "https://images.webofknowledge.com/WOKRS53B4/help/WOS/hs_wos_fieldtags.html"
FIELDTAG_URL_EXTRA <- "https://images.webofknowledge.com/images/help/WOK/hs_alldb_fieldtags.html"
FIELDTAGS_URL_EXTRA_2 <- "https://images.webofknowledge.com/images/help/WOS/hs_wos_fieldtags.html"

#' Helper function for downloading fieldtags from Web of Science
#' @param url A URL as a string
#' @return A data frame containing fieldtags downloaded from given URL
get_fieldtags <- function(url) {
    url <- RCurl::getURL(url)
    fieldtags <- XML::readHTMLTable(url)[[2]]
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
    fieldtags_main <- get_fieldtags(main_url)
    fieldtags_extra <- get_fieldtags(extra_url)
    fieldtags_extra_2 <- get_fieldtags(extra_url_2)
    all_fieldtags <- merge(fieldtags_main, fieldtags_extra,
                           by.x = "tag",
                           by.y = "tag",
                           all = TRUE)
    all_fieldtags <- merge(all_fieldtags, fieldtags_extra_2,
                           by.x = "tag",
                           by.y = "tag",
                           all = TRUE)
    index <- is.na(all_fieldtags$field.x)
    all_fieldtags$field.x[index] <- all_fieldtags$field.y[index]
    index <- is.na(all_fieldtags$field.x)
    all_fieldtags$field.x[index] <- all_fieldtags$field[index]
    fieldtags <- all_fieldtags[, c("tag", "field.x")]
    names(fieldtags) <- c("tag", "field")
    return(fieldtags)
}

update_fieldtags <- function() {
    fieldtags <- get_all_fieldtags(FIELDTAG_URL_MAIN,
                                   FIELDTAG_URL_EXTRA,
                                   FIELDTAGS_URL_EXTRA_2)

    devtools::use_data(fieldtags, internal = TRUE, overwrite = TRUE)
    return("Fieldtags updated!")
}

