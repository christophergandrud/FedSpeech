######################
# Gather Fed Governor Speeches, Parse HTML/XML
# Christopher Gandrud
# Updated 20 May 2013
######################

library(RCurl)
library(XML)

#######################
## CSV file with Fed governor speech URLs

addresses <- read.csv("/Users/christophergandrud/Dropbox/Fed_Speeches/Old/fed.hyperlinks/links.full.csv")

for (i in addresses){
    fed.text.Dec <- getURL(i)
}

fed.df <- as.data.frame(fed.text.Dec)

## Save raw HTML to individual text files
outpathA <- "/Users/christophergandrud/Desktop/Fed_Speeches/fed.text.indv.full/"

x <- 1:nrow(fed.df)

for(i in x) {
   write(as.character(fed.df[i,1]), file = paste(outpathA,"/",i,".txt",sep = ""))
}

#######################
## Open text files, parse individually and remove text of the the speeches

setwd("/Users/christophergandrud/Desktop/Fed_Speeches/fed.text.indv/")

    # Create list of text files to parse and extract speech text from
    files <- list.files(path = "/Users/christophergandrud/Desktop/Fed_Speeches/fed.text.indv/", pattern = "*.txt")
    
    # Create object to record empty text files (likely empty due to error in the website download)
    missing <- NULL
    
    # Indicate folder to save cleaned files into
    outpathB <- "/Users/christophergandrud/Desktop/Fed_Speeches/fed.text.parsed/"

for (i in files){
  
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