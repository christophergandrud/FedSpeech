######################
# Gather Fed Governor Speeches, Parse HTML/XML
# Christopher Gandrud
# Updated 20 May 2013
######################

library(XML)

#######################
## CSV file with Fed governor speech URLs

SpeechesData <- read.csv("~/Dropbox/Fed_Speeches_Paper/FedSpeech/Data/Raw/FedSpeechesVersionMay2013.csv")

Addresses <- as.character(SpeechesData[, "vars.link"])
x <- 1:length(Addresses)
Combi <- data.frame(x, Addresses, stringsAsFactors = FALSE)

outpathA <- "~/Desktop/FedSpeechIndv/"

for (i in nrow(Combi)){
  TempFile <- paste0(outpathA, "/", i, ".txt")
  URL <- Combi[, 2]
  download.file(URL, destfile = TempFile)
}

#######################
## Open text files, parse individually and remove text of the the speeches

setwd("~/Desktop/FedSpeechIndv/")

    # Create list of text files to parse and extract speech text from
    Files <- list.files(path = "~/Desktop/FedSpeechIndv/", pattern = "*.txt")
    
    # Create object to record empty text files (likely empty due to error in the website download)
    missing <- NULL
    
    # Indicate folder to save cleaned files into
    outpathB <- "~/Desktop/FedSpeechIndvParsed/"

for (i in Files){
  
    # Parse HTML and extract speech text
    marker <- tryCatch(
        unlist(
            xpathSApply(
                doc = htmlParse(
                    file = i), "//p", xmlValue
                )
            ), error = function(e) e
        )

        # Fill object with the file numbers of the empty text files.
        # Then skip if the text file is empty, to prevent the loop from stopping
        if(inherits(marker, "error")){
            missing <- c(missing, i) 
            next
        }     
         
        # Further remove unwanted HTML markup and repeated text
        marker <- gsub("\\n", "", marker) 
        marker <- gsub("Return to top", "", marker)
        marker <- gsub("Return to text", "", marker)
        marker <- gsub("Accessible Version", "", marker)
        marker <- gsub("Accessible version", "", marker)    
        marker <- gsub("Speeches", "", marker) 
        
        # Collapse into a single character string
        marker <- paste(marker, collapse = "") 
        
    # Save as new .txt file
    write(as.character(marker), file = paste(outpathB, "/", "parsed.", i, sep = ""))
}