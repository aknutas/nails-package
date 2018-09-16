
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

    # Extract author address
    authors$AuthorAddress <- apply(authors, 1, get_address)

    # Fix column names and add Label column
    names(authors)[names(authors) == "AuthorFullName"] <- "Id"
    authors$Label <- authors$Id

    # Drop unnecessary columns
    authors <- subset(authors,
                      select = c(Id,
                                 Label,
                                 Freq,
                                 TotalTimesCited,
                                 AuthorAddress,
                                 # ReprintAddress, probably not useful
                                 EmailAddress,
                                 Location))

    # Remove duplicates
    authors <- authors[!duplicated(authors$Id), ]

    return(authors)
}

#' Helper function for extracting author address
#' @param df A row from a data frame
#' @return A string containing author's address, if one exists
get_address <- function(df) {
    # Author name for the current row
    author_name <- df["AuthorFullName"]

    # Extract and clean names from AuthorAddress field
    name_list <- unlist(stringr::str_extract_all(df["AuthorAddress"],
                                         "\\[.*?\\]"))

    # If author names are not listed  within square brackets in AuthorAddress,
    # there is only one address, which is extracted.
    # Otherwise, name_list and address_list are processed and
    # names and addresses matched to each other.
    if (length(name_list) == 0) {
        address <- df["AuthorAddress"]
    } else {
        name_list <- gsub("\\[", "", name_list)     # Remove brackets
        name_list <- gsub("\\]", "", name_list)     # Remove brackets
        name_list <- strsplit(name_list, ";")       # Split list of names
        name_list <- lapply(name_list, trim)        # Remove leading &
                                                    # trailing whitespace
        name_list <- lapply(name_list, toupper)     # Change to uppercase

        # Extract and clean addresses
        address_list <- gsub("\\[.*?\\]", "", df["AuthorAddress"])

        # Split list of addresses
        address_list <- unlist(strsplit(address_list, ";"))

        # Remove leading & trailing whitespace
        address_list <- trim(address_list)

        # Match author name to correct address
        address <- NA
        for (i in 1:length(name_list)) {
            if (author_name %in% name_list[[i]]) {
                address <- address_list[i]
            }
        }
    }
    return(address)
}


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

    # Create edges only if more than 1 author
    if(length(unlist(edges)) > 1){
        edges <- combn(unlist(edges), 2, simplify = F)  # Get all author pairs
        edges <- lapply(edges, collapse_edge)           # Simplify author pairs
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
                select = c("Source",        # Don't change. Gephi needs this!
                           "Target",        # Don't change. Gephi needs this!
                           "Type",          # Don't change. Gephi needs this!
                           "id",            # Don't change. Gephi needs this!
                           "YearPublished", # This and below rows can be changed
                           "DocumentTitle",
                           "DOI",
                           "TimesCited"))

    return(edges)
}

#' Extract author network nodes and edges from literature data
#' @param df A data frame containing literature data
#' @param as_igraph Logical: should results be returned as an igraph?
#' @return A list containing nodes and edges in data frames, or an igraph.
#' @export
get_author_network <- function(df, as_igraph = FALSE) {
    author_nodes <- get_author_nodes(df)
    author_edges <- get_author_edges(df)

    # Create igraph for in degree and PageRank calculations
    author_network <- igraph::graph.data.frame(author_edges,
                                                 vertices = author_nodes)
    # Calculate PageRanks
    author_nodes$PageRank <- igraph::page.rank(author_network)$vector
    # Calculate in-degree
    author_nodes$InDegree <- igraph::degree(author_network, mode = "in")

    # By default, return results in a list of two data frames
    if (!as_igraph) {
        author_network <- list(author_nodes = author_nodes,
                                 author_edges = author_edges)
    }
    return(author_network)
}
