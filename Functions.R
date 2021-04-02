##### Category Creation #####

# Packages Required:  cleanNlP, magrittr, dplyr
# Input:              A vector of documents in a corpus
# Output:             A table of the tokens in the corpus and their characteristics
tokenize <- function(document_vector){
  #Annotate corpus
  annotation <- cleanNLP::cnlp_annotate(document_vector)

  tokens <- annotation$token %>%
    dplyr::filter(upos != "PUNCT") #Remove punctuation
  
  return(tokens)
}

# Packages Required:  tm
# Input:              Table of tokens
#                     Column of lemmas corresponding to each token
# Output:             Table of tokens with stopwords removed
removeStopwords <- function(tokens, lemmas){
  #Vector of stopwords
  stopwords <- tm::stopwords(kind = "en")
  
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
                                      idf_weight = "prob", #log odds inverse document frequency calculation
                                      min_df = minRespondentsPerCategory/numResponses, #The word should appear in at least 5 responses
                                      max_df = (numResponses-minRespondentsPerCategory)/numResponses) #The word should be absent in at least 5 responses
  
  return(tfidf)
}

# Required Packages:  magrittr, dplyr
# Input:              Term-Frequency Inverse Document Frequency Matrix
# Output:             A list of the most important words in decending order
getTopKeywords <- function(tfidf){
  #Turn tfidf matrix into a dataframe
  tfidf_dataframe <- tfidf %>%
    apply(MARGIN = 2, FUN = max) %>%
    as.data.frame() %>%
    dplyr::rename(tfidf = ".")
  
  #Get the keywords
  keywords <- rownames(tfidf)
  
  #Add a column identifing keywords
  tfidf_words_identified <- tfidf_dataframe %>%
    dplyr::mutate(keyword = keywords) %>% #Create a column with keyword name
    dplyr::arrange(desc(tfidf)) #Sort the keywords in descending order based on importance
  
  #Get the top ten keywords
  topTenKeywords <- tfidf_words_identified$keyword[1:10] %>%
    as.data.frame() %>%
    rename(keyword = ".") %>%
    filter(!is.na(keyword)) #Remove NA values if there are less than 10 keywords
}