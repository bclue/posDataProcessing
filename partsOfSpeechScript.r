if(!require(coreNLP)) install.packages('coreNLP', repos = "http://cran.us.r-project.org")

library("coreNLP")
options(java.parameters = "-Xmx6g")

# Initialize Global Variables
parameterFileName <<- "corenlp.properties"
libLoc <<- "C:\\Users\\bclew\\OneDrive\\Documents\\POS Research\\stanford-corenlp-full-2018-10-05"
coreNLPMem <<- "4g"
language <<- "english"
functionWordFile <<- "C:\\Users\\bclew\\OneDrive\\Documents\\POS Research\\functionwords.csv"

getFunctionWords <- function(pennTok) {
  
  functionWords <- vector()
  functionWordList <- read.csv(functionWordFile, header=F)
  
  for (words in pennTok[3]) {
    for(word in words){
      lowerWord <- sapply(word, tolower) 
      if(lowerWord %in% functionWordList[[1]]){
        
        functionWords <- c(functionWords, word)
      } else {
        functionWords <- c(functionWords, "-")
      }
    }
  }
  
  return(functionWords)   
  
} 


getPhraseData <- function(characterParseTrees) {
  
  phraseVector <- vector()
  word = ""
  phrase = ""
  currentPhrase = ""
  firstInstance = FALSE
  depth = 0
  
  for (tree in characterParseTrees){
    for (value in tree){
      if (!(value %in% c("(", ")", " ", "\r", "\n"))){
        word = paste(word, value, sep = "")
        phrase = paste(phrase, value, sep = "")
        
      }
      else if (value == ")"){
        depth = depth - 1
        
        if (word != "") {
          
          word = ""
          phrase = ""
          
          if (firstInstance == TRUE) {
            
            phraseVector <- c(phraseVector, currentPhrase)
            firstInstance = FALSE
            
          }
          else {
            
            phraseVector <- c(phraseVector, "-")
            
          }
          
        }
      }
      else if ((value == " " | value == "\r") & phrase != ""){
        
        if (depth == 3){
          
          
          currentPhrase = phrase
          firstInstance = TRUE
        }
        
        phrase = ""
        word = ""
      }
      
      else if (value == "(") {
        depth = depth + 1
      }
      else {
        word = ""
        phrase = ""
      }
    }
  }
  return(phraseVector)
  
}

batch_generate_csv <- function(fileListPath, outputFilePath, parseTreeOutputFilePath) {
  initCoreNLP(libLoc, type = c(language), parameterFile = parameterFileName, mem = coreNLPMem) 
  conn = file(fileListPath, "r")
  while ( TRUE ) {
    line = readLines(conn, n = 1)
    if ( length(line) == 0 ) {
      break
    }     
    dataList = processCSVData(line) 
    
    csvData <- as.data.frame(dataList[['uniTok']])
    parseTreeData <- dataList[['parseTree']]
    
    csvOutputFile = paste(outputFilePath, tools::file_path_sans_ext(basename(line)), '.csv', sep = "")
    file.create(csvOutputFile)
    csvConn <- file(csvOutputFile)
    
    if(is.null(parseTreeOutputFilePath)) {
      write.csv(csvData, file=csvConn)
    }  else {
      parseTreeOutputFile = paste(parseTreeOutputFilePath, tools::file_path_sans_ext(basename(line)), '.txt', sep = "")
      file.create(parseTreeOutputFile)
      write.csv(csvData, file=csvConn) 
      write(parseTreeData,file=parseTreeOutputFile)
      
    }
    
  }
  close(conn)
  

}

multi_generate_csv <- function(fileListPath, outputFile, parseTreeOutputFile) {
  
  initCoreNLP(libLoc, type = c(language), parameterFile = parameterFileName, mem = coreNLPMem) 
  conn = file(fileListPath, "r")
  
  file.create(outputFile)
  if(!is.null(parseTreeOutputFile)) {
    file.create(parseTreeOutputFile)
  }
  
  id = 0
  sentence = 0
  characterEnd = 0
  i = 0
  while ( TRUE ) {
    line = readLines(conn, n = 1)
    if ( length(line) == 0 ) {
      break
    }
    
    dataList = processCSVData(line) 
    
    csvData <- as.data.frame(dataList[['uniTok']])
    parseTreeData <- dataList[['parseTree']]

    newid = nrow(csvData)
    newSentence = csvData[(nrow(csvData):nrow(csvData)), (1:1)]
    newCharacterEnd = csvData[(nrow(csvData):nrow(csvData)), (6:6)]
    

    rownames(csvData) <- (1 + id):(nrow(csvData) + id)
    
    csvData[(1:nrow(csvData)), (1:1)] <- csvData[(1:nrow(csvData)), (1:1)] + sentence
    csvData[(0:nrow(csvData)), (5:5)] <- csvData[(0:nrow(csvData)), (5:5)] + characterEnd
    csvData[(0:nrow(csvData)), (6:6)] <- csvData[(0:nrow(csvData)), (6:6)] + characterEnd
    
    id = id + newid
    sentence = sentence + newSentence
    characterEnd = characterEnd + newCharacterEnd + 4
    
    if(is.null(parseTreeOutputFile)) {

      if (i == 0){
        
        write.table( csvData,  
                     file=outputFile, 
                     append = T, 
                     sep=',', 
                     row.names=T, 
                     col.names=T )
      } else {
        
        write.table( csvData,  
                     file=outputFile, 
                     append = T, 
                     sep=',', 
                     row.names=T, 
                     col.names=F )
      }
      
    
    }  else {
      if (i == 0){
        
        write.table( csvData,
                     file=outputFile, 
                     append = T, 
                     sep=',', 
                     row.names=T, 
                     col.names=T )
      } else {
        
        write.table( csvData,  
                     file=outputFile, 
                     append = T, 
                     sep=',', 
                     row.names=T, 
                     col.names=F )
      }
      write(parseTreeData,file=parseTreeOutputFile,append=TRUE)
      
    }
    i = i + 1
    
  }
  close(conn)
  
}


# process file (returns vector)

single_generate_csv <- function(file_path, csvOutputFile, parseTreeOutputFile) {
  # load up corenlp
  initCoreNLP(libLoc, type = c(language), parameterFile = parameterFileName, mem = coreNLPMem) 
  file.create(csvOutputFile)
  csvConn <- file(csvOutputFile)
  dataList = processCSVData(file_path) 
  
  csvData <- as.data.frame(dataList[['uniTok']])
  parseTreeData <- dataList[['parseTree']]
  
  if(is.null(parseTreeOutputFile)) {
    write.csv(csvData, file=csvConn)
  }  else {
    file.create(parseTreeOutputFile)
    parseConn <- file(parseTreeOutputFile)
    write.csv(csvData, file=csvConn) 
    write(parseTreeData,file=parseTreeOutputFile)
    
  }
}



processCSVData <- function(filePath) {
  
  # all of the stuff I normally do
  
  result <- annotateFile(filePath, format = "obj")
  
  pennTok <- getToken(result)
  uniTok <- getToken(result)
  parseTree <- getParse(result)
  
  characterParseTrees <- strsplit(parseTree, '')
  functionWords = getFunctionWords(pennTok)
  phrases = getPhraseData(characterParseTrees)
  
  # Add Universal Tags
  
  uniTok <- cbind2(uniTok, universalTagset(uniTok$POS))
  colnames(uniTok)[10] <- "Universal Tags"
  
  # Add Phrase Information
  uniTok <- cbind2(uniTok, phrases)
  uniTok <- cbind2(uniTok, functionWords)
  
  #uniTok <- cbind2(uniTok, wordCountVector)
  
  colnames(uniTok)[11] <- "Phrases"
  colnames(uniTok)[12] <- "Function Words"
  colnames(uniTok)[1] <- "Word ID"
  
  
  uniTok<- uniTok[, -(8:9)]
  

  dataList <- list()
  dataList['uniTok']<-list(uniTok)
  dataList['parseTree']<- toString(parseTree)
  
  return(dataList)
}


main <- function() {
  
  args <- commandArgs(trailingOnly = TRUE)

  if (length(args) < 3 || length(args) > 4) {
    stop("Please provide arguments of the form: {multi|single|batch} inputFilePath outputFile(s)Path [parseTreeFilePath]", call.=FALSE)
  } else {
    processingType <- args[1]
    inputFilePath <- args[2]
    outputFilePath <- args[3]
    
    if (length(args) == 4) {
      parseTreeFilePath <- args[4]
    } else {
      parseTreeFilePath <- NULL
    }
    
    if (processingType == "multi") {
      multi_generate_csv(inputFilePath, outputFilePath, parseTreeFilePath)
    } else if (processingType == "single") {
      single_generate_csv(inputFilePath, outputFilePath, parseTreeFilePath)
    } else if (processingType == "batch") {
      batch_generate_csv(inputFilePath, outputFilePath, parseTreeFilePath)
    } else {
    stop("Please specify whether you are using 'multi', 'single' or 'batch' processing as your first command line arguments", call.=FALSE)
    }
  }
  
  
}

main()
