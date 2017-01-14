library(dplyr)
library(chron)
library(data.table)
library(stringr)
library(RCurl)
library(rjson)
library(urltools)
library(igraph)
library(magrittr)
#library(ApacheLogProcessor)

#install.packages("ApacheLogProcessor")

lct <- Sys.getlocale("LC_TIME"); 
Sys.setlocale("LC_TIME", "C")

# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 500*1024^2)


logsCount <- 0

function(input, output,session) {
  
  #input$ip
  
  # transform raw internal page rank to page rank
  map <- function(x, range = c(0,1), from.range=NA) {
    if(any(is.na(from.range))) from.range <- range(x, na.rm=TRUE)
    
    ## check if all values are the same
    if(!diff(from.range)) return(
      matrix(mean(range), ncol=ncol(x), nrow=nrow(x), 
             dimnames = dimnames(x)))
    
    ## map to [0,1]
    x <- (x-from.range[1])
    x <- x/diff(from.range)
    ## handle single values
    if(diff(from.range) == 0) x <- 0 
    
    ## map from [0,1] to [range]
    if (range[1]>range[2]) x <- 1-x
    x <- x*(abs(diff(range))) + min(range)
    
    x[x<min(range) | x>max(range)] <- NA
    
    x
  }
  
  importLogs <- function(name) {
    
    #if (!exists("DF")) {
    DF <- read.csv2(name, header=TRUE, sep = ",", stringsAsFactors = F, skip=1 )
    
    #get first line to have site url
    website_url <- DF[1,]$Source
    #website_url          <- 'https://data-seo.fr'
    
    ## we keep only link
    DF <- filter(DF,grepl(website_url,Destination) 
                 & !grepl(".jpg",Destination)
                 & !grepl(".png",Destination)
                 & !grepl(".gif",Destination)
                 & !grepl(".pdf",Destination)
                 & Type=="HREF" & Follow=="true") %>%
      select(Source,Destination)
    
    #DF <- as.data.frame(sapply(DF,gsub,pattern=website_url,replacement=""))
    
    ## adapt colnames and rownames
    colnames(DF) <- c("from","to")
    rownames(DF) <- NULL
    #}
    
    # generate graph with data.frame
    graphObject = graph.data.frame(DF)
    
    # calculate pagerank
    urls_pagerank <- page.rank(graphObject, directed= TRUE, damping = 0.85)  %>%
      use_series("vector") %>%
      sort(decreasing = TRUE) %>%
      as.data.frame %>%
      set_colnames("raw.internal.pagerank")
    
    urls_pagerank$Address<-rownames(urls_pagerank)
    rownames(urls_pagerank) <- NULL
    
    urls_pagerank <- mutate(urls_pagerank, internal.pagerank = map(raw.internal.pagerank, c(1,10)))    
    
    
    return(urls_pagerank)
    
  }  
  

  
  
  output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    if (!is.null(input$file1)) {
      
      inFile <- input$file1

      logsSummary <- importLogs(inFile$datapath)
      
      logsCount <- nrow(logsSummary)
      
      #print(logsCount)
      output$caption <- renderText({ paste(logsCount," results ",sep="") })
      
      
      logsSummary
      
    }
    
    
    
  })
  
  
  
  output$downloadData <- downloadHandler(
    filename = "extract.csv",
    content = function(file) {
      
      if (!is.null(input$file1)) {
        
        inFile <- input$file1
        
        
        logsSummary <- importLogs(inFile$datapath)
        
        
        write.csv2(logsSummary,file, row.names = FALSE)
        
        
      }
      
    }
  )
  
}

