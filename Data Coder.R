workingDirectory <- getwd()
functionsFilePath <- paste0(workingDirectory, "/Functions.R")
source(functionsFilePath)

#Test Data
student_responses <- test_survey$q25_like_best

# Packages Required:  cleanNLP, magrittr, dplyr, tm, purrr, bios2mds
# Input:              A vector of student_responses
# Output:             A data frame of student_responses and the categories they respond to,
#                     An RMarkdown file which gives useful information to consider when coding data
createCategories <- function(student_responses){
  library(magrittr)
  
  #Initialize udpipe english natural language model
  cleanNLP::cnlp_init_udpipe(model_name = "english")
  
  #Convert responses into a vector
  vectorOfResponses <- as.vector(student_responses)
  
  #Remove blank responses
  vectorOfResponses <- vectorOfResponses[vectorOfResponses != ""]
  
  #Change names
  names(vectorOfResponses) <- c(1:length(vectorOfResponses))

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
  topTenKeywords <- as.data.frame(topKeywords$keyword[1:10])

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
  
  #Cluster words
  responseClusters <- pairResponsesWithClusters(vectorOfResponses, kmeansClustering)
  
  #Sample 5 responses per cluster
  sampleResponses <- sampleClusterResponses(responseClusters)
  
  return(responseClusters)
}


