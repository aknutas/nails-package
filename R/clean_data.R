
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

    # Subset data
    # POSSIBLE OPTIMIZATION: remove this step!
    arranged_df <- subset(df, select = c(selection, "id"))
    # Remove NAs
    arranged_df <- arranged_df[!is.na(arranged_df[, selection]), ]
    # Create data frame: selection split by split, each element on a new row,
    # id copied to new rows
    arranged_df <- splitstackshape::cSplit(arranged_df,
                                         splitCols = selection,
                                         sep = sep, direction = "long")
    temp <- quote(selection)
    arranged_df <- arranged_df[,  eval(temp):= as.character(get(selection))]
    # Removing rows with NA as author name created in previous step
    arranged_df <- arranged_df[!is.na(arranged_df[, selection]), ]
    # Merge the rest of the data by id
    arranged_df <- merge(arranged_df,
                       df[, !(names(df) %in% c(selection))],
                       by = "id")
    return(arranged_df)
}

#' Helper function for extracting reference lists
#' @param df A data frame containing literature data
#' @return A data frame containing each reference in separate row
get_reference_list <- function(df) {
    rl <- arrange_by(df, "CitedReferences")
    names(rl)[names(rl) == 'CitedReferences'] <- "FullReference"

    year <- strsplit(rl$FullReference, ",")
    rl$ReferenceYear <- sapply(year, get_year)
    references <- strsplit(rl$FullReference, " DOI ")
    rl$Reference <- sapply(references, get_DOI)
    rl$Reference[is.na(rl$Reference)] <- rl$FullReference[is.na(rl$Reference)]
    return(rl)
}

#' Helper function for extracting citation network nodes from reference list
#' @param reference_list A data frame containing reference list
#' @return A data frame containing citation nodes from reference
get_reference_nodes <- function(reference_list) {
    citation_nodes <- data.frame(Id = reference_list$Reference,
                                YearPublished = reference_list$ReferenceYear,
                                FullReference = reference_list$FullReference,
                                id = NA,
                                PublicationType = NA,
                                AuthorFullName = NA,
                                DocumentTitle = NA,
                                PublicationName = NA,
                                BookSeriesTitle = NA,
                                Language = NA,
                                DocumentType = NA,
                                ConferenceTitle = NA,
                                ConferenceDate = NA,
                                ConferenceLocation = NA,
                                ConferenceSponsors = NA,
                                AuthorKeywords = NA,
                                SubjectCategory = NA,
                                TimesCited = NA,
                                Abstract = NA,
                                DOI = NA,
                                Origin = "reference",
                                stringsAsFactors = FALSE)

    index <- duplicated(citation_nodes$Id)
    citation_nodes <- citation_nodes[!index, ]
    return(citation_nodes)
}

#' Helper function for extracting citation network nodes from literature data
#' @param df A data frame containing the literature data
#' @return A data frame containing citation nodes from literature data
get_literature_nodes <- function(df) {
    literature_nodes <- subset(df,
                              select = c(DOI,
                              YearPublished,
                              ReferenceString,
                              id,
                              PublicationType,
                              AuthorFullName,
                              DocumentTitle,
                              PublicationName,
                              BookSeriesTitle,
                              Language,
                              DocumentType,
                              ConferenceTitle,
                              ConferenceDate,
                              ConferenceLocation,
                              ConferenceSponsors,
                              AuthorKeywords,
                              SubjectCategory,
                              TimesCited,
                              Abstract,
                              DOI))

    names(literature_nodes)[c(1, 3, 20)] <- c("Id",
                                            "FullReference",
                                            "DOI")
    index <- literature_nodes$Id == ""
    literature_nodes$Id[index] <- literature_nodes$FullReference[index]
    literature_nodes$Origin <- "literature"

    index <- duplicated(literature_nodes)
    literature_nodes <- literature_nodes[!index, ]
    return(literature_nodes)
}

#' Extract nodes for a citation network from literature data
#' @param df A data frame containing literature data
#' @param reference_list A data frame containing reference list
#' @return A data frame containing nodes for a citation network
get_citation_nodes <- function(df, reference_list) {
    citation_nodes <- get_reference_nodes(reference_list)
    literature_nodes <- get_literature_nodes(df)

    # Remove reference nodes that appear also in literature data
    citation_nodes <- citation_nodes[!(citation_nodes$Id %in%
                                          literature_nodes$Id), ]

    citation_nodes <- rbind(literature_nodes, citation_nodes)
    citation_nodes$Label <- citation_nodes$FullReference

    index <- duplicated(citation_nodes)
    citation_nodes <- citation_nodes[!index, ]
    return(citation_nodes)
}

#' Extract edges for a citation network from reference list
#' @param reference_list A data frame containing reference list
#' @return A data frame containing edges for a citation network
get_citation_edges <- function(reference_list) {
    citation_edges <- data.frame(Source = reference_list$DOI,
                                Target = reference_list$Reference,
                                id = reference_list$id,
                                YearPublished = reference_list$YearPublished,
                                DocumentTitle = reference_list$DocumentTitle,
                                stringsAsFactors = FALSE)
    no_source <- citation_edges$Source == ""
    no_target <- is.na(citation_edges$Target)
    citation_edges$Source[no_source] <- reference_list$ReferenceString[no_source]
    citation_edges$Target[no_target] <- reference_list$FullReference[no_target]

    return(citation_edges)
}


#' Extract nodes for an author network from literature data
#' @param df a data frame containing literature data
#' @return A data frame containing nodes for author network
get_author_nodes <- function(df) {
    authors <- arrange_by(df, "AuthorFullName")

    # Calculating total number of citations for each author
    citation_sums <- aggregate(authors$TimesCited,
                              by = list(AuthorFullName = authors$AuthorFullName),
                              FUN = sum)
    # Fixing column names
    names(citation_sums) <- c("AuthorFullName", "TotalTimesCited")
    authors <- merge(authors, citation_sums,
                     by = "AuthorFullName",
                     all.x = TRUE)

    # Calculating author frequency
    author_table <- as.data.frame(table(authors$AuthorFullName))
    authors <- merge(authors, author_table,
                     by.x = "AuthorFullName",
                     by.y = "Var1",
                     all.x = TRUE)

    names(authors)[names(authors) == "AuthorFullName"] <- "Id"
    authors$Label <- authors$Id

    authors <- subset(authors,
                      select = c(Id,
                                 Label,
                                 Freq,
                                 TotalTimesCited,
                                 AuthorAddress,
                                 ReprintAddress,
                                 # E-mailAddress,   FIX THIS COLNAME!
                                 Location))

    # Remove duplicates
    authors <- authors[!duplicated(authors$Id), ]
    return(authors)
}







# Helper functions for extracting edges

#' Helper function for pasting two nodes together
#' @param node A list containing two author network nodes
#' @return A string containing two nodes separated by ";"
collapse_edge <- function(edge){
    x <- paste(edge, collapse = ';')
}

#' Helper function for creating a node from author names
#' @param x A list of nodes
#' @return A list of edges
create_edges <- function(x){
    edges <- strsplit(x, ';')
    if(length(unlist(edges)) > 1){
        edges <- combn(unlist(edges), 2, simplify = F)
        edges <- lapply(edges, collapse_edge)
    } else{edges <- NA}
    return(edges)
}

#' Extract edges for an author network from literature data
#' @param df a data frame containing literature data
#' @return A data frame containing nodes for an author network
get_author_edges <- function(df) {
    # Create edges
    edges <- lapply(df$AuthorFullName, create_edges)

    # Count the length of edges created from each row
    nodelengths <- sapply(edges, length)

    # Put edges into a data frame
    edges <- unlist(edges)
    edges <- as.data.frame(edges)

    # Split edges to two columns and fix column names
    edges <- as.data.frame(stringr::str_split_fixed(edges$edges, ";", 2))
    names(edges) <- c("Source", "Target")

    # Create id and edge type columns
    edges$id <- rep(df$id, nodelengths)
    edges$Type <- "Undirected"

    # Remove NAs and empty cells from Source and Target columns
    edges <- edges[!is.na(edges$Source), ]
    edges <- edges[!is.na(edges$Target), ]
    edges <- edges[edges$Source != "", ]
    edges <- edges[edges$Target != "", ]

    # Remove leading and trailing whitespace from Sources and Targets
    edges$Source <- trim(edges$Source)
    edges$Target <- trim(edges$Target)

    # Merge with literature
    edges <- merge(edges, subset(df, select = -c(Authors, AuthorFullName)),
                   by.x = "id", by.y = "id")

    # Subset data. Use this to select columns to include in network data
    edges <- subset(edges,
                    select = c("Source",          # Don't change. Gephi need this!
                               "Target",          # Don't change. Gephi need this!
                               "Type",            # Don't change. Gephi need this!
                               "id",              # Don't change. Gephi need this!
                               "YearPublished",   # This and below rows can be changed.
                               "DocumentTitle",
                               "DOI",
                               "TimesCited"))

    return(edges)
}





