##### Category Creation #####

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
  numResponses <- max(tokens$doc_id)
  
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
    keyword <- keywords,
    tfidf <- tfidf_values
  ) %>%
    dplyr::arrange(desc(tfidf)) #Sort the keywords in descending order based on importance
  
  #Get the top ten keywords
  topTenKeywords <- tfidf_words_identified$keyword[1:10] %>%
    as.data.frame() %>%
    dplyr::rename(keyword = ".") %>%
    dplyr::filter(!is.na(keyword)) #Remove NA values if there are less than 10 keywords
}

# Required Packages:  None
# Input:              Principal Components
# Output:             Best Principal Components
getBestPrincipalComponents <- function(principalComponents){
  #Determine the optimal number of principal components
  optimalNumComponents <- determineNumPrincipalComponents(principalComponents)
  
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
