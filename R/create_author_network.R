
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
                    select = c("Source",          # Don't change. Gephi needs this!
                               "Target",          # Don't change. Gephi needs this!
                               "Type",            # Don't change. Gephi needs this!
                               "id",              # Don't change. Gephi needs this!
                               "YearPublished",   # This and below rows can be changed.
                               "DocumentTitle",
                               "DOI",
                               "TimesCited"))

    return(edges)
}

