workingDirectory <- getwd()
functionsFilePath <- paste0(workingDirectory, "/Functions.R")
source(functionsFilePath)

# Packages Required:  cleanNLP, magrittr, dplyr, tm, purrr, bios2mds
# Input:              A vector of survey responses
# Output:             A data frame of responses and the categories they respond to,
#                     An RMarkdown file which gives useful information to consider when coding data
createCategories <- function(responses){
  library(magrittr)
  
  #Initialize udpipe english natural language model
  cleanNLP::cnlp_init_udpipe(model_name = "english")
  
  #Convert responses into a vector
  vectorOfResponses <- convertResponsesToVector(responses)

  #Tokenize
  tokens <- tokenize(vectorOfResponses)
  
  #Normalize
  lemmas <- tokens$lemma
  
  #Remove stopwords
  nonStopwordTokens <- removeStopwords(tokens, lemmas)
  
  #Calculate term-frequency inverse document frequency
  tfidf <- calculateTFIDF(nonStopwordTokens)
  
  #Creates principal components
  principalComponents <- prcomp(tfidf)
  
  #Get optimal number of components
  numComponents <- determineNumPrincipalComponents(principalComponents)
  
  #Get optimal components
  principalComponentsToUse <- getBestPrincipalComponents(principalComponents, numComponents)
  
  #k-means clustering
  kmeansClustering <- clusterResponses(principalComponents, numComponents)
  
  #Cluster words
  responseClusters <- pairResponsesWithClusters(vectorOfResponses, kmeansClustering)
  
  return(responseClusters)
}

# Packages Required:  sentimentr, magrittr
# Input:              A vector of survey responses
# Output:             A data frame of responses and their sentiment
#                     An RMarkdown file which gives useful information to consider when coding data
calculateSentiment <- function(responses){
  library(magrittr) # Allows for the pipe operator (%>%)
  
  #Convert responses to a vector
  vectorOfResponses <- convertResponsesToVector(responses)
  
  #Split the corpus into sentences
  sentences <- sentimentr::get_sentences(vectorOfResponses)
  
  #Calculate the sentiment of responses, using sentence level sentiment
  sentiment <- sentimentr::sentiment_by(sentences) 
  
  #Encode responses as positive, negative, or neutral
  encodedSentiment <- encodeSentiment(vectorOfResponses, sentiment)
  
  return(encodedSentiment)
}

# Required Packages:  rmarkdown, cleanNLP, magrittr, dplyr, tm, purrr, bios2mds, sentimentr
# Input:              Survey responses
# Output:             Data frame of coded responses and an html data coding report
codeData <- function(responses){
  ##### Category Creation #####
  library(magrittr)
  
  #Initialize udpipe english natural language model
  cleanNLP::cnlp_init_udpipe(model_name = "english")
  
  #Convert responses into a vector
  vectorOfResponses <- convertResponsesToVector(responses)
  
  #Tokenize
  tokens <- tokenize(vectorOfResponses)
  
  #Normalize
  lemmas <- tokens$lemma
  
  #Remove stopwords
  nonStopwordTokens <- removeStopwords(tokens, lemmas)

  #Calculate term-frequency inverse document frequency
  tfidf <- calculateTFIDF(nonStopwordTokens)

  #Identify most important words
  topKeywords <- getTopKeywords(tfidf)
  topTenKeywords = as.data.frame(topKeywords$keyword[1:10]) %>%
    dplyr::rename(`Top Keywords` = "topKeywords$keyword[1:10]")

  #Creates principal components
  principalComponents <- prcomp(tfidf)

  #Get optimal number of components
  numComponents <- determineNumPrincipalComponents(principalComponents)
  
  #Get optimal components
  principalComponentsToUse <- getBestPrincipalComponents(principalComponents, numComponents)
  
  #Get the principal component loadings
  pcLoadings <- getPrincipalComponentLoadings(principalComponents, numComponents)
  
  #k-means clustering
  kmeansClustering <- clusterResponses(principalComponents, numComponents)
  
  #Cluster responses
  responseClusters <- pairResponsesWithClusters(vectorOfResponses, kmeansClustering)
  
  #Sample 5 responses per cluster
  sampleResponses <- sampleClusterResponses(responseClusters)
  
  ##### Sentiment Assignment #####
  
  #Split the corpus into sentences
  sentences <- sentimentr::get_sentences(vectorOfResponses)
  
  #Calculate the sentiment of responses, using sentence level sentiment
  sentiment <- sentimentr::sentiment_by(sentences) 
  
  #Encode responses as positive, negative, or neutral
  encodedSentiment <- encodeSentiment(vectorOfResponses, sentiment) %>%
    dplyr::rename(responses = "vectorOfResponses", sentiment = "sentiment$ave_sentiment")
  
  #Get 5 most positive and negative responses
  greatestSentiments <- getExtremeSentiments(vectorOfResponses, sentiment) %>%
    dplyr::rename(Sentiment = "sentiment$ave_sentiment", Responses = "vectorOfResponses")

  #Combine category and sentiment coding
  codedData <- merge(responseClusters, encodedSentiment, by = "row.names")

  ##### Report #####
  #Create Rmarkdown Data Cleaning Report
  rmarkdown::render(input = "Report Template.Rmd", 
                    output_file = sprintf("testOutputDoc.html"),
                    params = list(keywords = topTenKeywords,
                                  loadings = pcLoadings,
                                  sampleResponses = sampleResponses,
                                  extremeSentiments = greatestSentiments))
  
  return(codedData)
}
