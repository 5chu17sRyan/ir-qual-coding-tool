
workingDirectory <- getwd()
functionsFilePath <- paste0(workingDirectory, "/Data Coder.R")
source(functionsFilePath)

#Test Data
test_survey <- read.csv("C:/Users/ryans/OneDrive/Desktop/Spring 2021/Senior Seminar/ir-qual-coding-tool/Test Data/durhamnc-2011-resident-survey/2011-resident-survey_1.csv", sep=";")
responses <- test_survey$q25_like_best

categories <- createCategories(responses)
head(categories)

sentiment <- calculateSentiment(responses)
head(sentiment)

codedResponses <- codeData(responses)
head(codedResponses)

