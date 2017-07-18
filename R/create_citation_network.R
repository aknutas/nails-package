

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
    citation_nodes$Label <- citation_nodes$FullReference
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
    literature_nodes$Label <- literature_nodes$FullReference

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

    citation_nodes <- rbind(citation_nodes, literature_nodes)

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

#' Extract citation network nodes and edges from literature data
#' @param df A data frame containing literature data
#' @param as_igraph Logical: should results be returned as an igraph?
#' @return A list containing nodes and edges in data frames, or an igraph.
get_citation_network <- function(df, as_igraph = FALSE) {
    # Extract nodes and edges
    reference_list <- get_reference_list(df)
    citation_nodes <- get_citation_nodes(df, reference_list)
    citation_edges <- get_citation_edges(reference_list)

    # Create igraph for in degree and PageRank calculations
    citation_network <- igraph::graph.data.frame(citation_edges,
                                               vertices = citation_nodes)
    # Calculate PageRanks
    citation_nodes$PageRank <- igraph::page.rank(citation_network)$vector
    # Calculate in-degree
    citation_nodes$InDegree <- igraph::degree(citation_network, mode = "in")

    # By default, return results in a list of two data frames
    if (!as_igraph) {
        citation_network <- list(citation_nodes = citation_nodes,
                        citation_edges = citation_edges)
    }
    return(citation_network)
}
