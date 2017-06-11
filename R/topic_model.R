#' Estimates number of topics (K) using the stm library
#'
#' @param literature The cleaned and processed literature dataframe
#' @return A list with vector of words (textlist) and data frame with only abstracts, topics and rowids (doctablewt)
preprocess_literature_for_topicmodeling <- function(literature) {
    if(missing(literature))
        stop("Need valid literature input")

    # Insert rowids for output merging
    literature$topicmodelrowids <-as.numeric(rownames(literature))

    # Subselect dataframe for processing and drop empty rows
    doctablewt <- literature[,c("DocumentTitle", "Abstract", "topicmodelrowids")]
    doctablewt <- doctablewt[!is.na(doctablewt$Abstract), ]
    doctablewt <- doctablewt[!is.na(doctablewt$DocumentTitle), ]

    # Extract and combine topics, abstracts
    data <- paste(doctablewt$DocumentTitle, doctablewt$Abstract, sep = " ")
    return(list(textlist = data, doctablewt = doctablewt))
}

#' Create stm library corpus
#'
#' @param textlist Vector of processed words
#' @return Corpus for stm
build_stm_corpus <- function(textlist) {
    # Prepare documents into corpus
    # Also processes the following by default:
    # Lowercases, removes SMART stopwords, removes numbers, removes punctuation, wordlength min is 3
    processed <- textProcessor(textlist, stem = TRUE)
    out <- prepDocuments(processed$documents, processed$vocab)
    return(out)
}

#' Estimates number of topics (K) using the stm library
#'
#' @param data Data from preprocess_literature_for_topicmodeling()
#' @return A list with topic estimation from stm lib (topicest), semantic coherence values (semcohvalues) and estimate of best K of topics (bestk)
select_optimal_k <- function(data) {
    out <- build_stm_corpus(data$textlist)

    # Estimate number of topics; semantic coherence often good (default method spectral; best compromise and deterministic)
    # Seed set for consitent results
    set.seed(5707363)
    topickest <- searchK(out$documents, out$vocab, K = c(4:12), seed = 5707363)
    semcohsK <- data.frame(topickest$results$K, topickest$results$semcoh)
    colnames(semcohsK)<- c("K","semcohs")

    # Getting the K with highest semantic coherence, setting it as true
    bestpick <- semcohsK[which.max(semcohsK$semcohs),]
    bestK <- as.integer(bestpick$K)
    semcohsK$bestpick <- FALSE
    semcohsK$bestpick[which.max(semcohsK$semcohs)] <- TRUE

    return(list(topicest = topicest, semcohvalues = semcohsK, bestk = bestpick))
}

# Function for converting topicmodels to LDAvis compatible
# Source: https://gist.github.com/a-paxton/a1609f5f772b642027d4
#' Convert the output of a topicmodels Latent Dirichlet Allocation to JSON
#' for use with LDAvis
#'
#' @param fitted Output from a topicmodels \code{LDA} model.
#' @param corpus Corpus object used to create the document term
#' matrix for the \code{LDA} model. This should have been create with
#' the tm package's \code{Corpus} function.
#' @param doc_term The document term matrix used in the \code{LDA}
#' model. This should have been created with the tm package's
#' \code{DocumentTermMatrix} function.
#'
#' @seealso \link{LDAvis}.
#' @export

topicmodels_json_ldavis <- function(fitted, corpus, doc_term){

    # Find required quantities
    phi <- posterior(fitted)$terms %>% as.matrix
    theta <- posterior(fitted)$topics %>% as.matrix
    vocab <- colnames(phi)
    doc_length <- vector()
    for (i in 1:length(corpus)) {
        temp <- paste(corpus[[i]]$content, collapse = ' ')
        doc_length <- c(doc_length, stri_count(temp, regex = '\\S+'))
    }
    temp_frequency <- as.data.frame(as.matrix(doc_term)) # elegant, silenced solution from http://stackoverflow.com/a/18749888/5514568
    freq_matrix <- data.frame(ST = colnames(temp_frequency),
                              Freq = colSums(temp_frequency))
    rm(temp_frequency)

    # Convert to json
    json_lda <- LDAvis::createJSON(phi = phi, theta = theta,
                                   vocab = vocab,
                                   doc.length = doc_length,
                                   term.frequency = freq_matrix$Freq)

    return(json_lda)
}

#' Create topicmodels library corpus
#'
#' @param textlist Vector of texts
#' @return Corpus for topicmodels
build_topicmodels_corpus <- function(textlist) {
    # Create corpus
    abstractCorpus <- Corpus(VectorSource(textlist))

    # Preprocess by lowercasing, removing punctuation, numbers, whitespace and stopwords, and finally stemming
    abstractCorpus <- tm_map(abstractCorpus, content_transformer(tolower))
    abstractCorpus <- tm_map(abstractCorpus, removePunctuation)
    abstractCorpus <- tm_map(abstractCorpus, removeNumbers)
    abstractCorpus <- tm_map(abstractCorpus, stripWhitespace)
    abstractCorpus <- tm_map(abstractCorpus, removeWords, stopwords("SMART"))
    abstractCorpus <- tm_map(abstractCorpus, stemDocument)

    return(abstractCorpus)
}

#' Create topicmodels library DocumentTermMatrix
#'
#' @param abstractCorpus From build_topicmodels_corpus()
#' @return Corpus for topicmodels
build_topicmodels_DTM <- function(abstractCorpus) {
    # Create DTM, minwordlength 3 (like above in stm)
    abstractDTM <- tm::DocumentTermMatrix(abstractCorpus, control = list(minWordLength = 3))

    # Cut documents with no words after filtering
    rowTotals <- apply(abstractDTM , 1, sum)
    # If empty rows, then remove documents from corpus and document-term matrix
    empty.rows <- abstractDTM[rowTotals == 0, ]$dimnames[1][[1]]
    if(!is.null(empty.rows)){
        abstractCorpus <- abstractCorpus[-as.numeric(empty.rows)]
        # doctablewt <- doctablewt[-as.numeric(empty.rows),]
        # Disabled for now, not sure how well the shortcut works (TODO: Testing)
        # abstractDTM <- abstractDTM[rowTotals> 0, ]
        # A two-pass cludge, possibly could be replaced by row abstractDTM <- abstractDTM[rowTotals> 0, ]
        # Enabling a second pass to prevent a discrepancy between corpus, doclist and DTM
        abstractDTM <- tm::DocumentTermMatrix(abstractCorpus, control = list(minWordLength = 3))
    }
}

#' Create a topicmodel from WoS library data frame
#'
#' @param literature Cleaned and processed literature dataframe
#' @param K Predefined K. If null, auto select K with stm library.
#' @return List with topicmodels fit (fit), thetaDF distributions (thetadf) and subset of literature columns with topicmodel columns added (doctablewt)
build_topicmodel_from_literature <- function(literature) {
    if(missing(literature))
        stop("Need valid literature data frame input")

    # Preprocessing literature file
    data <- preprocess_literature_for_topicmodeling(literature)

    if(missing(K)) {
        k_analysis <- select_optimal_k(data)
        K <- k_analysis$bestk
    }

    # Build corpus and DTM
    corpus <- build_topicmodels_corpus(data$textlist)
    abstractDTM <- build_topicmodels_DTM(corpus)

    # If empty rows after DTM processing, remove from doctablewt
    empty.rows <- abstractDTM[rowTotals == 0, ]$dimnames[1][[1]]
    if(!is.null(empty.rows)){
        data$doctablewt <- data$doctablewt[-as.numeric(empty.rows),]
    }

    # Parameters
    # Note: alpha and beta values estimated automatically
    burnin <- 4000
    iter <- 2000 # default value
    thin <- 500
    seed <- 5707363 # random seed
    nstart <- 4 # random starts for model evaluation, best is picked (increase from 1 to 4-6 for final analysis)
    best <- TRUE

    # Latent Dirichlet Allocation
    fit <- LDA(abstractDTM, K, method="Gibbs", control=list(nstart=nstart,
                                                            seed=seed, best=best,
                                                            burnin=burnin, iter=iter,
                                                            thin=thin))

    # Terms for each topic
    topickeywords <- terms(fit, 10)

    # Theta topic probabilities for each document
    thetaDF <- as.data.frame(posterior(fit)$topics)
    # Add top topics for each document, add rowids for future reference
    thetaDF$toptopic <- colnames(thetaDF)[max.col(thetaDF,ties.method="first")]
    thetaDF$topicmodelrowids <- doctablewt$topicmodelrowids

    return(list(fit = fit, thetadf = thetaDF, doctablewt = doctablewt))
}
