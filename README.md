# posDataProcessing
This repository contains a script designed to process text files and generate csv files based on Parts-Of-Speech data. These csv files will be used for further statistical analysis in R, as part of an authorship attribution project.
## How to Set-Up the Script
### Download CoreNLP
The first step is to download the CoreNLP natural language processing software from Stanford. Go to this [website](https://stanfordnlp.github.io/CoreNLP/) and click download. You should get a zipped file. Unzip it in your folder of choice. Note: the current version as of writing this is stanford-corenlp-4.0. The version that I have been using is stanford-corenlp-full-2018-10-05

### Set up Global Variables
There are several global variables at the top of the script that will need to be edited in order to match your host machine's directory set-up. The "libLoc" should be a file path pointing to the version of unzipped CoreNLP that you just downloaded. The "functionWordFile" should be a filepath pointing to a csv list of function words. A list is included in this repository (based on the function words Mosteller-Wallace use in their Federalist Papers study), but can be swapped out depending on your needs. 

## How to Run the Script
### To Process a Single File 
`RScript partsOfSpeechScript.r single INPUT_FILE OUTPUT_FILE [PARSE_OUTPUT_FILE]`
<br>
INPUT_FILE is the filepath to a .txt document to be parsed, OUTPUT_FILE is the filepath to a .csv document to be created, and PARSE_OUTPUT is the filepath to a .txt parse tree file. If no PARSE_OUTPUT_FILE is specified, no file will be created.

### To Process a Batch List of Files 
`RScript partsOfSpeechScript.r batch INPUT_FILE_LIST OUTPUT_DIRECTORY [PARSE_OUTPUT_DIRECTORY]`
<br>
INPUT_FILE_LIST is the filepath to a .txt document that contains a list of documents that you want to process individually. Each document filepath should be each on their own line. In this case, OUTPUT_DIRECTORY and PARSE_OUTPUT_DIRECTORY should not be individual filepaths, but instead the directory that you wish the related documents to be generated in. If the input is myfile.txt the output will by myfile.csv and the parse tree output will be myfile_parse.txt

This option is best used when you have a corpus of shorter texts you would like to process at once. For example, the Federalist Papers.

### To Process a Single File in Parts 
`RScript partsOfSpeechScript.r multi INPUT_FILE_LIST OUTPUT_FILE [PARSE_OUTPUT_FILE]`
<br>
Based on [Stanford's documentation](https://stanfordnlp.github.io/CoreNLP/memory-time.html), the way to correctly parse a longer document is to "process a large file a piece, say a chapter, at a time, not all at once." INPUT_FILE_LIST is the filepath to a .txt document that contains a list of those smaller document chunks that you want to process individually but combine into a single end document. OUTPUT_FILE is the filepath to a .csv document to be created, and PARSE_OUTPUT is the filepath to a .txt parse tree file which will be formed out of the individual parts. Note: when it comes to characters, the script adds four spaces between parts (as that often seemed typical for chapter transitions in the files I was working with)

## Output
The output of the script will be a csv file looking like this:
<br>
	Word ID | Sentence	ID | Word in Sentence ID	| Word	| CharacterOffsetBegin	| CharacterOffsetEnd	| Penn Treebank POS	| Universal POS | Phrases	| Function Words

## Troubleshooting

- If you encounter noisy characters in the script results, it is possible that there is a problem with the encoding of your original text file. For example, if you run this set-up on a Windows machine, it will expect the text file to be Windows encoded rather than Unicode 8. Switch the encoding and run again.
- The output file(s) specified should not be already existing file


