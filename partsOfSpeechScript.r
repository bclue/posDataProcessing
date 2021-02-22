if(!require(coreNLP)) install.packages('coreNLP', repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")

library("coreNLP")
library("stringr")

options(java.parameters = "-Xmx6g")

# Initialize Global Variables -- EDIT DURING SET UP

# name of properties file
parameterFileName <<- "corenlp.properties"
# file path to your edition of corenlp
libLoc <<- "C:\\Users\\bclew\\OneDrive\\Documents\\POS Research\\stanford-corenlp-full-2018-10-05"
# memory used
coreNLPMem <<- "4g"
# language used in parser
language <<- "english"
# path to your function word file
functionWordFile <<- "C:\\Users\\bclew\\OneDrive\\Documents\\POS Research\\functionwords.csv"


# get function word data for every instance where a part of speech corresponds with a function word in the associated list
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

# get phrase data based on parse tree
getPhraseData <- function(parseTree) {
  
  sentenceTrees <- str_replace_all(parseTree, "[\r\n]" , " ")
  parseTags <- vector()
  
  for (sentence in sentenceTrees) {
    parentheses <- vector()
    parseTag = "-"
    sentence <- strsplit(sentence, " ")
    sentence <- lapply(sentence, function(x){x[!x ==""]})
    
    for (symbol in sentence[[1]]) {
      # add opening parentheses when encountered
      if (str_count(symbol, "\\(") > 0) {
        parentheses <- append(parentheses, "(")
        # if we are three "(" in, we are at a depth of three layers in the parse tree
        if (length(parentheses) == 3) {
          parseTag = substr(symbol, 2, str_length(symbol))
        }
        
      } else if (str_count(symbol, "\\)") > 0) {
        # the apperance of closing parenthesis indicate a word ex "TO)" or "YORK)))" 
        parseTags <- append(parseTags, parseTag)
        parseTag = "-"
        # remove closing parentheses when encountered
        parentheses <- parentheses[1:(length(parentheses)-str_count(symbol, "\\)"))] 
      }
    }
  }
        return(parseTags)

}
 


# turn a batch of seperate texts into csv file (+ and optionally parse tree text files) at once
batchGenerateCSV <- function(fileListPath, outputFilePath, parseTreeOutputFilePath) {
  
  # load in corenlp
  initCoreNLP(libLoc, type = c(language), parameterFile = parameterFileName, mem = coreNLPMem) 
  
  # for each line in initial file, process the text from the listed filepath
  conn = file(fileListPath, "r")
  while ( TRUE ) {

    line = readLines(conn, n = 1)
    if ( length(line) == 0 ) {
      break
    }     
    
    # get csv and parse tree data based on a file path
    dataList = processCSVData(line) 
    csvData <- as.data.frame(dataList[['uniTok']])
    parseTreeData <- dataList[['parseTree']]
    
    # create csv output file--will text initial filename.txt and create filename.csv
    csvOutputFile = paste(outputFilePath, tools::file_path_sans_ext(basename(line)), '.csv', sep = "")
    file.create(csvOutputFile)
    csvConn <- file(csvOutputFile)
    
    # write to file
    if(is.null(parseTreeOutputFilePath)) {
      write.csv(csvData, file=csvConn)
    }  else {
      # create parse tree output file--will create filename.txt and turn it into filename_parse.txt
      parseTreeOutputFile = paste(parseTreeOutputFilePath, tools::file_path_sans_ext(basename(line)), '_parse.txt', sep = "")
      file.create(parseTreeOutputFile)
      write.csv(csvData, file=csvConn) 
      write(parseTreeData,file=parseTreeOutputFile)
    }

  }

  close(conn)
  
}

# combine and process individual file pieces (e.g. chapters) into one output file
# useful when file is too large to process at one time
multiGenerateCSV <- function(fileListPath, outputFile, parseTreeOutputFile) {
  
  # load corenlp
  initCoreNLP(libLoc, type = c(language), parameterFile = parameterFileName, mem = coreNLPMem) 
  
  conn = file(fileListPath, "r")
  # set up output files
  file.create(outputFile)
  if(!is.null(parseTreeOutputFile)) {
    file.create(parseTreeOutputFile)
  }
  
  id = 0
  sentence = 0
  characterEnd = 0
  i = 0
  # go through each line in the initial file, and find the file listed on that line
  while ( TRUE ) {
    line = readLines(conn, n = 1)
    if ( length(line) == 0 ) {
      break
    }
    
    # get the csv and phrase data based on the initial text
    dataList = processCSVData(line) 
    csvData <- as.data.frame(dataList[['uniTok']])
    parseTreeData <- dataList[['parseTree']]
    
    # get the last id, sentence id, and character number
    newid = nrow(csvData)
    newSentence = csvData[(nrow(csvData):nrow(csvData)), (1:1)]
    newCharacterEnd = csvData[(nrow(csvData):nrow(csvData)), (5:5)]
    
    # edit the data that will be appended so that it starts where the previous data ends off
    rownames(csvData) <- (1 + id):(nrow(csvData) + id)
    csvData[(1:nrow(csvData)), (1:1)] <- csvData[(1:nrow(csvData)), (1:1)] + sentence
    csvData[(0:nrow(csvData)), (5:5)] <- csvData[(0:nrow(csvData)), (4:4)] + characterEnd
    csvData[(0:nrow(csvData)), (6:6)] <- csvData[(0:nrow(csvData)), (5:5)] + characterEnd
    
    # increment
    id = id + newid
    sentence = sentence + newSentence
    characterEnd = characterEnd + newCharacterEnd + 4
    
    # write to file
    if(is.null(parseTreeOutputFile)) {
      
      if (i == 0){
        write.table( csvData,  file=outputFile, append = T, sep=',', row.names=T, col.names=T )
      } else {
        write.table( csvData,  file=outputFile, append = T, sep=',', row.names=T, col.names=F )
      }    
      
    }  else {
      if (i == 0){
        write.table( csvData, file=outputFile, append = T, sep=',', row.names=T, col.names=T )
      } else {
        write.table( csvData, file=outputFile, append = T, sep=',', row.names=T, col.names=F )
      }

      write(parseTreeData,file=parseTreeOutputFile,append=TRUE) 
    }

    # move to next line
    i = i + 1
  }

  close(conn)
  
}


# process a single file and generate csv data file (and possibly a parse tree txt file)

singleGenerateCSV <- function(filePath, csvOutputFile, parseTreeOutputFile) {
  # load up corenlp
  initCoreNLP(libLoc, type = c(language), parameterFile = parameterFileName, mem = coreNLPMem) 
  # set up output file
  file.create(csvOutputFile)
  csvConn <- file(csvOutputFile)
  
  # get csv + parse tree data in a list
  dataList = processCSVData(filePath) 
  csvData <- as.data.frame(dataList[['uniTok']])
  parseTreeData <- dataList[['parseTree']]
  
  # write data to file
  if(is.null(parseTreeOutputFile)) {
    write.csv(csvData, file=csvConn)
  }  else {
    file.create(parseTreeOutputFile)
    parseConn <- file(parseTreeOutputFile)
    write.csv(csvData, file=csvConn) 
    write(parseTreeData,file=parseTreeOutputFile)  
  }

}


# get the csv and phrase data based on the initial text
processCSVData <- function(filePath) {
  
  result <- annotateFile(filePath, format = "obj")
  
  pennTok <- getToken(result)
  uniTok <- getToken(result)
  parseTree <- getParse(result)
  
  # get function words and phrase data
  functionWords = getFunctionWords(pennTok)
  phrases = getPhraseData(parseTree)
  
  # add universal tags
  uniTok <- cbind2(uniTok, universalTagset(uniTok$POS))
  colnames(uniTok)[10] <- "Universal Tags"
  
  # add phrase information
  uniTok <- cbind2(uniTok, phrases)
  uniTok <- cbind2(uniTok, functionWords)
  
  # add column names
  colnames(uniTok)[11] <- "Phrases"
  colnames(uniTok)[12] <- "Function Words"
  colnames(uniTok)[0] <- "Word ID"
  
  # remove unneccesary columns
  uniTok<- uniTok[, -(8:9)]
  uniTok<- uniTok[, -(4:4)]
  # package both csv file data and phrase data as a list
  dataList <- list()
  dataList['uniTok']<-list(uniTok)
  dataList['parseTree']<- toString(parseTree)
  
  return(dataList)

}


# Run the script from the command line, taking arguments indicating what type of processing is neccessary
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
      multiGenerateCSV(inputFilePath, outputFilePath, parseTreeFilePath)
    } else if (processingType == "single") {
      singleGenerateCSV(inputFilePath, outputFilePath, parseTreeFilePath)
    } else if (processingType == "batch") {
      batchGenerateCSV(inputFilePath, outputFilePath, parseTreeFilePath)
    } else {
      stop("Please specify whether you are using 'multi', 'single' or 'batch' processing as your first command line arguments", call.=FALSE)
    }

  }
  
}

main()

