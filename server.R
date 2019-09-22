#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Simple prediction function
  simplePredict <- function(tdm,termSize,targetString){
    
    # First parse the last termSize - 1 of the target string
    targetTokens <- strsplit(targetString, " ")
    
    tokenLength <- length(targetTokens[[1]])
    lowerIndexBound <- tokenLength - (termSize-1)
    targetPattern <- NULL
    for(index in 1:tokenLength){
      if(index > lowerIndexBound){
        
        targetPattern <- paste(targetPattern,targetTokens[[1]][index],sep = " ")
      }
    }
    inputSegment <- targetPattern
    targetPattern <- sub("^ ","",targetPattern)
    targetPattern <- paste("^",targetPattern, sep = "")
    
    predictionCandidates <- tdm[grep(targetPattern,tdm)]
    predictionCandidate <- NULL
    if(length(predictionCandidates) == 0){
      return(predictionCandidate)
    }
    else{
      predictionCandidate <-predictionCandidates[[1]]
    }
    
    predictionCandidateTokens <- strsplit(predictionCandidate," ")
    predictionResult <- paste(targetString,predictionCandidateTokens[[1]][length(predictionCandidateTokens[[1]])])
    predictionResult
  }
  
  ## Load all our prediciton models
  print("Loading prediction cache")
  bigramDTM <- get(load("bigram_dtm.rda"))
  trigramDTM <- get(load("trigram_dtm.rda"))
  unigramAssociations <- get(load("unigram_associations.rda"))
  print("Prediction models cache loaded")
  
  # Main prediction function
  predictText <- function(inputString){
    # First detect the token length of the input string
    numberOfInputTokens <-length(strsplit(inputString, " ")[[1]])
    predictionResult <- NULL
    # Attempt the first prediction from bigger grams to smaller ones
    if (numberOfInputTokens >= 3 ){
      predictionResult <- simplePredict(trigramDTM,3,inputString)
    }
    
    if(is.null(predictionResult) && numberOfInputTokens > 0){
      
      print("No match found on trigrams, proceeding with bigrams")
      predictionResult <- simplePredict(bigramDTM,2,inputString)
      if(!is.null(predictionResult)){
        #There was a match, hence we return the value
        return(predictionResult) 
      }
    }
    
    if(is.null(predictionResult) && numberOfInputTokens > 0){
      
      print("No match found on bigrams, proceeding with unigram associations")
      targetTokens <- strsplit(inputString, " ")
      lastInputToken <- targetTokens[[1]][length(targetTokens[[1]])]
      print(lastInputToken)
      predictionCandidate <- as.vector(unigramAssociations$prediction[grep(lastInputToken,unigramAssociations$targetTerm)])[1]
      if(is.na(predictionCandidate))
      {
        #The wasn't a word found as our last prediction attempt, therefore we return a failure message
        return("Failed to predict text")
      }
      
      predictionResult <- paste(inputString,predictionCandidate)
      return(predictionResult)
    }
    
   if(is.null(predictionResult)  && numberOfInputTokens > 0){
     return("Failed to predict text")
   }
    
   #Return the final result
    print(Sys.time()-startTime)
   return(predictionResult)
  }
  
  
  
  output$resultString <- renderText({
    predictText(input$targetString)
  })
  
})
