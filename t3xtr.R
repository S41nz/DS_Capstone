library(tm)

#### Utility methods definition ##########

## Frequency data gram creator
freq_df <- function(tdm){
  freq <- sort(rowSums(as.matrix(tdm)), decreasing=TRUE)
  freq_df <- data.frame(word=names(freq), freq=freq)
  return(freq_df)
}

## Tokenizer for the Bigrams
BigramTokenizer <-
  function(x)
    unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)

## Tokenizer for the Trigrams
TrigramTokenizer <-
  function(x)
    unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)

## Grep based prediction of the text
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
  targetPattern <- sub("^ ","",targetPattern)
  targetPattern <- paste("^",targetPattern, sep = "")
  print(targetPattern)
  
  print(tdm$dimnames$Terms[grep(targetPattern,tdm$dimnames$Terms)])
}
##########################################
# First parse the raw content of the files
newsFile <- readLines("C:\\Users\\psainza\\Documents\\Capstone Project\\final\\en_US\\en_US.news.txt")
blogFile <- readLines("C:\\Users\\psainza\\Documents\\Capstone Project\\final\\en_US\\en_US.blogs.txt")
twitterFile <- readLines("C:\\Users\\psainza\\Documents\\Capstone Project\\final\\en_US\\en_US.twitter.txt")

print("Files loaded")
# Calculate the number of files
newsTotalSize <- length(newsFile)
blogTotalSize <-length(blogFile)
twitterTotalSize <- length(twitterFile)

# Then extract a sample of it, starting with 0.5% for optimization purposes
newsSample <-sample(newsFile,newsTotalSize*0.005)
blogSample <-sample(blogFile,blogTotalSize*0.005)
twitterSample <- sample(twitterFile,twitterTotalSize*0.005)

print("Raw text samples created")
# Create a unified sample vector to create the corpus
unifiedSampleVector <- c(newsSample,blogSample,twitterSample)

targetCorpus <- VCorpus(VectorSource(unifiedSampleVector))
# As a first assumption and for simplicity, lets canonize to lower case
targetCorpus <- tm_map(targetCorpus,content_transformer(tolower))
# Then we remove the punctuation
targetCorpus <-tm_map(targetCorpus,removePunctuation)
# Then remove actual numbers
targetCorpus <-tm_map(targetCorpus,removeNumbers)
# Trim the accidental whitespaces
targetCorpus <- tm_map(targetCorpus,stripWhitespace)

print("Text corpus created")
# Then generate the corresponding Term Matrix Table
targetDTM <- DocumentTermMatrix(targetCorpus)
print("Document Term Matrix created")

# Get the output
inspect(targetDTM)

print("Creating Unigram DTM...")
# Then create the first matrix based on unigrams, lets aim to remove the lower 1% of the curve
unigramDTM <- removeSparseTerms(targetDTM,0.999)
print("Unigram DTM created")
inspect(unigramDTM)

print("Creating the bigram from the original corpus...")
bigramDTM <- removeSparseTerms(TermDocumentMatrix(targetCorpus,control = list(tokenize = BigramTokenizer)),0.9999)
print("Bigram DTM created")
inspect(bigramDTM)

print("Creating the trigram from the original corpus...")
trigramDTM <- removeSparseTerms(TermDocumentMatrix(targetCorpus,control = list(tokenize = TrigramTokenizer)),0.9999)
print("trigram DTM created")
inspect(trigramDTM)

  
# Create the last resource model based on most likely unigram association
associationModel <- data.frame(targetTerm=character(),prediction=character())
names(associationModel) <- c("targetTerm","prediction")

currentTermIndex <- 1
for(currentTerm in unigramDTM$dimnames$Terms){
  # Get the most probable associations based on a threshold of 5%
  currentAssociationResult <- findAssocs(unigramDTM,currentTerm,0.05)
  # Concatenate to our main dataframe
  newAssociationEntry <- data.frame(currentTerm,rownames(as.data.frame(currentAssociationResult[1]))[1])
  names(newAssociationEntry) <- c("targetTerm","prediction")
  
  associationModel <- rbind(associationModel,newAssociationEntry)
  currentTermIndex <- currentTermIndex + 1
  print(paste("Progress:",as.character(currentTermIndex),"of","1826"))
}

#Save the corresponding models
save(trigramDTM,file = "C:\\Users\\psainza\\Documents\\Capstone Project\\App\\T3xtr\\trigram_dtm.rda")
save(bigramDTM,file = "C:\\Users\\psainza\\Documents\\Capstone Project\\App\\T3xtr\\bigram_dtm.rda")
save(associationModel,file = "C:\\Users\\psainza\\Documents\\Capstone Project\\App\\T3xtr\\unigram_associations.rda")
