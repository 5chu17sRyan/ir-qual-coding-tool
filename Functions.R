# Required Packages:  None
# Input:              responses
# Output:             The responses formatted as a vector
convertResponsesToVector <- function(responses){
  #Convert responses into a vector
  vectorOfResponses <- as.vector(responses)
  
  #Remove blank responses
  vectorOfResponses <- vectorOfResponses[vectorOfResponses != ""]
  
  #Change names
  names(vectorOfResponses) <- c(1:length(vectorOfResponses))
  
  return(vectorOfResponses)
}

##### CATEGORY CREATION FUNCTIONS #####

# Packages Required:  cleanNlP, magrittr, dplyr
# Input:              A vector of documents in a corpus
# Output:             A table of the tokens in the corpus and their characteristics
tokenize <- function(document_vector){
  #Annotate corpus
  annotation <- cleanNLP::cnlp_annotate(document_vector)

  tokens <- annotation$token %>%
    dplyr::filter(upos != "PUNCT") %>% #Remove punctuation
    dplyr::mutate(lemma = tolower(lemma)) #Convert lemmas to lowercase
    
  return(tokens)
}

# Packages Required:  No package but dependent on stopwords.csv
# Input:              Table of tokens
#                     Column of lemmas corresponding to each token
# Output:             Table of tokens with stopwords removed
removeStopwords <- function(tokens, lemmas){
  #Import stopwords
  stopwordsCSV <- read.csv("C:/Users/ryans/OneDrive/Desktop/Spring 2021/Senior Seminar/ir-qual-coding-tool/stopwords.csv", sep="", stringsAsFactors=FALSE)  
  
  #List of stopwords 
  stopwords <- as.character(stopwordsCSV$Stopwords)
  
  #Identify which normalized tokens are stopwords
  isStopword <- lemmas %in% stopwords
  
  #Select tokens which are not stopwords
  nonStopwordTokens <- tokens[!isStopword, ]
  
  return(nonStopwordTokens)
}

# Required Packages:  cleanNLP
# Input:              Table of tokens in a corpus
# Outputs:            The term-frequency inverse document frequency matrix
calculateTFIDF <- function(tokens){
  #Our preference is to never assign less than 5 students to a single group. They become identifiable
  minRespondentsPerCategory <- 5
  
  #Gets the number of responses
  numResponses <- max(as.numeric(tokens$doc_id))
  
  #Calculate tfidf matrix
  tfidf <- cleanNLP::cnlp_utils_tfidf(tokens, 
                                      tf_weight = "lognorm", #lognorm term frequency calculation
                                      idf_weight = "idf", #standard calculation (0 means not important)
                                      min_df = minRespondentsPerCategory/numResponses, #The word should appear in at least 5 responses
                                      max_df = (numResponses-minRespondentsPerCategory)/numResponses) %>% #The word should be absent in at least 5 responses
    as.matrix()
  
  return(tfidf)
}

# Required Packages:  magrittr, dplyr, purrr
# Input:              Term-Frequency Inverse Document Frequency Matrix
# Output:             A list of the most important words in decending order
getTopKeywords <- function(tfidf){
  #Turn tfidf matrix into a dataframe
  tfidf_dataframe <- tfidf %>%
    as.data.frame() %>%
    purrr::map_dbl(max)
  
  #Get the keywords
  keywords <- names(tfidf_dataframe)
  
  #Get tfidf values
  names(tfidf_dataframe) <- NULL
  tfidf_values <- tfidf_dataframe
  
  #Sort the keywords
  tfidf_words_identified <- data.frame(
    keyword = keywords,
    tfidf = tfidf_values
  ) %>%
    dplyr::arrange(desc(tfidf)) #Sort the keywords in descending order based on importance
  
  return(tfidf_words_identified)
}

# Required Packages:  None
# Input:              Principal Components
# Output:             Best Principal Components
getBestPrincipalComponents <- function(principalComponents, optimalNumComponents){
  #Gets the principal components that should be used
  principalComponentsToUse <- principalComponents$x[,1:optimalNumComponents]
  
  return(principalComponentsToUse)
}

# Packages Required:  None
# Input:              Principal Components
# Output:             Number of Principal Components which should be used
determineNumPrincipalComponents <- function(principalComponents){
  #Calculate the percentage of variance explained by each component
  percentage_of_variance <- principalComponents$sdev^2 / sum(principalComponents$sdev^2)
  
  #Calculate the change in the precentage of vairance explained from one component to the next
  numComponents <- length(percentage_of_variance)
  slope <- NA
  for(i in 2:numComponents){
    slope <- c(slope, percentage_of_variance[i] - percentage_of_variance[i-1])
  }
  
  #Calculate when the return (change in percentage of variance per increase in num components) changed the most
  changeInDerivitive <- c(NA, NA)
  for(i in 3:numComponents){
    changeInDerivitive <- c(changeInDerivitive, slope[i] - slope[i-1])
  }
  optimalNumComponents <- which.max(changeInDerivitive)
  
  return(optimalNumComponents)
}

# Required Packages:  magritrr, dplyr
# Input:              Principal Components, Number of Components to Use
# Output:             Matrix of principal components and which words belong to them
getPrincipalComponentLoadings <- function(principalComponents, numComponents){
  #Get the loadings of the the principal components
  loadings <- principalComponents$rotation[,1:numComponents]
  
  #Absolute value of loadings to determine word belongings
  loadingsAbsVal <- abs(loadings)
  
  #Determine which words belongs to which components (Which component are they most heavily loaded onto)
  wordBelongings <- data.frame(
    keywords = row.names(loadings),
    principalComponents = apply(loadingsAbsVal, MARGIN = 1, FUN = which.max)
  )
  
  ## THIS IS INEFFICIENT CODE
  #Determine the direction of the loadings
  pcValues <- NULL
  for(i in 1:nrow(wordBelongings)){
    principalComponent <- wordBelongings$principalComponents[i]
    keyword <- wordBelongings$keywords[i]
    
    pcValue <- loadings[rownames(loadings)==keyword, principalComponent]
    
    pcValues <- rbind(pcValues, pcValue)
  }
  
  wordBelongings$directions <- ifelse(pcValues > 0, "positive", "negative")
  
  #Get rid of the row names
  rownames(wordBelongings) <- NULL
  
  return(wordBelongings)
}

# Required Packages:  bios2mds
# Input:              Principal Components, The number of components to use
# Output:             The clusters each response belongs tos
clusterResponses <- function(principalComponents, numComponents){
  responsesDimReduced <- principalComponents$x[,1:numComponents]
  
  #Determine optimal number of clusters
  num_clusters <- determineNumClusters(responsesDimReduced)
  
  #Run kmeans clustering algorithms
  kmeans <- kmeans(responsesDimReduced, num_clusters)
  
  #Which responses belong to which clusters
  responsesClustered <- kmeans$cluster
  
  return(responsesClustered)
}

# Required Packages:  bios2mds
# Input:              Things to be clustered
# Output:             The optimal number of clusters
determineNumClusters <- function(thingsToBeClustered){
  #Preparation
  set.seed(NULL)
  minClusters <- 2
  maxClusters <- 10
  
  #Calculate silhouette scores for 2-10 clusters
  sil_score <- bios2mds::sil.score(thingsToBeClustered, nb.clus = c(minClusters:maxClusters))
  
  #Calculate the improvement in sil score from one number of clusters to the next
  sil_score_improvement <- NULL
  for(i in minClusters:maxClusters){
    improvement <- 1 - ((1 - sil_score[i]) / (1 - sil_score[i-1]))
    sil_score_improvement <- c(sil_score_improvement, improvement)
  }
  
  #Select the number of clusters which has the maximum sil score improvement
  num_clusters <- which.max(sil_score_improvement)
  
  return(num_clusters)
}

# Required Packages:  magrittr, dplyr
# Input:              Responses, List of clusters corresponding to responses
# Output:             Data Frame of responses and which cluster they belong to
pairResponsesWithClusters <- function(responses, clusters){
  responses <- vectorOfResponses
  clusters <- kmeansClustering
  
  #Turn arguments into data frames
  dfResponses <- as.data.frame(responses)
  dfClusters <- as.data.frame(clusters)
  
  #Merge the data frames
  clusteredResponses <- merge(dfResponses, dfClusters, by = "row.names") %>%
    dplyr::arrange(as.numeric(Row.names)) %>%
    dplyr::select(-c(Row.names))
  
  clusteredResponses$Row.names
  
  return(clusteredResponses)
}

# Required Packages:  magrittr, dplyr
# Input:              Responses and which cluster they belong to
# Output:             A sample of five responses for each cluster
sampleClusterResponses <- function(responseClusters){
  #Determine the number of clusters
  numClusters <- max(responseClusters$clusters)
  
  #Sample 5 responses from each cluster
  sampleResponses <- NULL
  for(i in 1:numClusters){
    clusterI <- responseClusters %>%
      dplyr::filter(clusters == i)
    
    sampleI <- clusterI[sample(nrow(clusterI), 5), ]
    
    sampleResponses <- rbind(sampleResponses, sampleI)
  }
  
  return(sampleResponses)
}

##### SENTIMENT FUNCTIONS #####

# Required Packages:  
# Input:              Survey responses, sentiment calculated for each response
# Output:             Data frame of survey responses and their sentiment coded as positive or negative
encodeSentiment <- function(vectorOfResponses, sentiment){
  #Turn arguments into data frames
  dfResponses <- as.data.frame(vectorOfResponses)
  dfSentiment <- as.data.frame(sentiment$ave_sentiment)
  
  #Code sentiment as positive, negative, or neutral
  encodedSentiment <- ifelse(dfSentiment > 0, "positive", ifelse(dfSentiment < 0, "negative", "neutral"))

  #Combine with responses
  responsesWithSentiment <- cbind(dfResponses, encodedSentiment)

  return(responsesWithSentiment)
}

getExtremeSentiments <- function()