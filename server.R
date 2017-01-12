library(dplyr)
library(chron)
library(data.table)
library(stringr)
library(RCurl)
library(rjson)
library(urltools)
#library(ApacheLogProcessor)

#install.packages("ApacheLogProcessor")

lct <- Sys.getlocale("LC_TIME"); 
Sys.setlocale("LC_TIME", "C")

# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 100*1024^2)


logsCount <- 0

function(input, output,session) {
  
  #input$ip
  
  importLogs <- function(name) {
    
    
    if (grepl(".gz",name))
      filename <- gzfile(name) 
    else 
      filename <- name
    
    maxfields <- max(count.fields(filename))
    
    
    
    inputIP <- input$ipbot
    
    inputDate <- input$date
    
    inputUrl <- input$url
    
    inputUseragent <- input$useragent    
    
    if (is.null(inputIP))
      inputIP <- character(0)
    
    # if (is.null(inputDate))
    #   inputDate <- character(0)
    
    # if (is.null(inputUrl))
    #   inputUrl <- character(0)    
    
    
    # update select
    
    updateSelectInput(session, "date",
                      choices = paste0("V",seq_len(maxfields)),
                      selected = inputDate
    )  
    updateSelectInput(session, "url",
                      choices = paste0("V",seq_len(maxfields)),
                      selected = inputUrl
    )      
    updateSelectInput(session, "ipbot",
                      choices = paste0("V",seq_len(maxfields)),
                      selected= inputIP
    )       
    updateSelectInput(session, "useragent",
                      choices = paste0("V",seq_len(maxfields)),
                      selected= inputUseragent
    )      
    
    logs <- read.table(filename, stringsAsFactors=FALSE, 
                       col.names = paste0("V",seq_len(maxfields)), fill = TRUE)
    
    # select only good field
    
    
    # logs <- mutate(logs,V4=paste(V4,V5,sep=""))
    # logs$V5 <- NULL
    
    vField <- c()
    
    #TODO ; si deux column ou plus
    #TODO : find good format
    if (!is.null(inputDate)) {
      
      temp <- gregexpr("[0-9]+", inputDate)
      vDATE <- as.vector(as.numeric(unique(unlist(regmatches(inputDate, temp)))))
      vField <- cbind(vField, vDATE)
      
      if (length(vField)==2) {
        field1 <- vField[1,]
        field2 <- vField[2,]
        
        logs <- mutate(logs,vDate=paste(logs[,field1],logs[,field2],sep=""))
        
        #print(logs$vDate[1])
        
        if (!grepl("-",logs$vDate[1])) {
          #todo : detect format
          logs$vDate <- as.chron(logs$vDate,format="[%d/%b/%Y:%H:%M:%S%z]")
          logs$vDate <- format(logs$vDate,"%d/%m/%Y %H:%M:%S")
          
          #init
          vField <- c()
          vDATE <- c(maxfields+1)
          vField <- cbind(vField, vDATE)
        }
        else {
          logs$vDate <- as.chron(logs$vDate,format="%Y-%m-%d%H:%M:%S")
          logs$vDate <- format(logs$vDate,"%d/%m/%Y %H:%M:%S")
          
          #init
          vField <- c()
          vDATE <- c(maxfields+1)
          vField <- cbind(vField, vDATE)          
        }
        
        
      }
      else
        vField <- cbind(vField, vDATE)
      
      
      
    }
    
    if (!is.null(inputIP)) {
      
      #print("input$ip")      
      
      temp <- gregexpr("[0-9]+", inputIP)
      vIP <- as.vector(as.numeric(unique(unlist(regmatches(inputIP, temp)))))
      vField <- cbind(vField, vIP)   
      
    }
    
    if (!is.null(inputUseragent)) {
      
      #print("input$url")          
      
      temp <- gregexpr("[0-9]+", inputUseragent)
      vUSERAGENT <- as.vector(as.numeric(unique(unlist(regmatches(inputUseragent, temp)))))
      vField <- cbind(vField, vUSERAGENT)      
      
    }    
    
    if (!is.null(inputUrl)) {
      
      #print("input$url")          
      
      temp <- gregexpr("[0-9]+", inputUrl)
      vURL <- as.vector(as.numeric(unique(unlist(regmatches(inputUrl, temp)))))
      vField <- cbind(vField, vURL)      
      
    }
    
    
    if (length(vField)>0)
      logs <- select(logs,vField[1,])
    #logs <- select(logs,vDate2)
    
    #colnames(logs) <- c("ip","date","url")
    
    if (!is.null(logs$vUSERAGENT)) {
      if(input$botmode=="bingbot")
        logs <- filter(logs,grepl("bingbot",vUSERAGENT)) 
      else if(input$botmode=="bingbot")
        logs <- filter(logs,grepl("bingbot",vUSERAGENT)) 
      else if(input$botmode=="baidubot")
        logs <- filter(logs,grepl("Baiduspider",vUSERAGENT)) 
      else if(input$botmode=="yandexbot")
        logs <- filter(logs,grepl("YandexBot",vUSERAGENT))  
      else if(input$botmode=="googlebotmobile") {
        
        logs <- filter(logs,grepl("Googlebot",vUSERAGENT))
        logs_google <- logs   
        
        logs_ip <- group_by(logs_google,vIP) %>%
          summarize(count=n())
        
        logs_ip$count <- NULL
        
        logs_google_mobile <- merge(logs_google, logs_ip, by = "vIP") %>%
          filter(grepl("Mobile",vUSERAGENT))
        
        logs_google_desktop <- merge(logs_google, logs_ip, by = "vIP") %>%
          filter(!grepl("Mobile",vUSERAGENT))
        
        # get all robotstxt
        logs_robotstxt_shared <- filter(logs_google_desktop, grepl("robots.txt",vURL))
        
        logs_mobile_with_robotstxt <- rbind(logs_google_mobile, logs_robotstxt_shared) %>%
          arrange(vIP, vDATE)
        
        logs_mobile_with_robotstxt <- arrange(logs_mobile_with_robotstxt, vIP, vDATE)
        
        lastrobotstxt <- FALSE
        lastip <- ""
        
        logs_mobile_with_robotstxt$keep <- TRUE
        
        for(i in 1:nrow(logs_mobile_with_robotstxt)) {
          
          if(!grepl("robots.txt",logs_mobile_with_robotstxt$vURL[i])) {
            lastrobotstxt <- FALSE
          }  
          
          # detect extra robots.txt
          if (lastrobotstxt==TRUE) {
            logs_mobile_with_robotstxt$keep[i] <- FALSE  
          }
          
          if (logs_mobile_with_robotstxt$vIP[i] != lastip) {
            lastrobotstxt <- FALSE
            lastip <- logs_mobile_with_robotstxt$vIP[i]
          }
          else {
            
            if(grepl("robots.txt",logs_mobile_with_robotstxt$vURL[i])) {
              #print("robots.txt detected ")
              lastrobotstxt <- TRUE
              
              # if last url=robots.txt, we remove
              if (grepl("robots.txt",logs_mobile_with_robotstxt$vURL[i-1])) {
                logs_mobile_with_robotstxt$keep[i-1] <- FALSE 
              }
            }
            
          }
          
          
        }
        
        
        #delete lines where there are too extra robots.txt 
        logs <- logs_mobile_with_robotstxt[logs_mobile_with_robotstxt$keep,] %>%
          select(-keep)
        
        #TODO : apply algo ???
        
        logs <- select(logs, vIP, vDATE, vURL, vUSERAGENT)
        
        # foreach IP
        caseid <- 1
        iptemp <- ""
        
        for (row in 1:nrow(logs)) { 
          
          ip <- logs$vIP[row]
          url <- logs$vURL[row] 
          
          if (grepl("/robots.txt", url)) {
            caseid <- caseid + 1
          }
          
          if (ip!=iptemp) {
            iptemp <- ip
            caseid <- caseid + 1
          }  
          
          # create caseid
          logs$vCASEID[row] <- caseid
          
        }
        
        logs <- select(logs, vIP, vDATE, vCASEID, vURL)
        colnames(logs) <- c('IP','DATE','CASEID','URL')
        
        
      }
      # si google desktop
      # 
      else {
        logs <- filter(logs,grepl("Googlebot",vUSERAGENT))
        logs_google <- logs   
        
        logs_ip <- group_by(logs_google,vIP) %>%
          summarize(count=n())
        
        logs_ip$count <- NULL
        
        logs_google_desktop <- merge(logs_google, logs_ip, by = "vIP") %>%
          filter(!grepl("Mobile",vUSERAGENT))
        
        # get all robotstxt
        logs_robotstxt_shared <- filter(logs_google_desktop, grepl("robots.txt",vURL))
        
        logs <- arrange(logs_google_desktop, vIP, vDATE)
        
        # foreach IP
        caseid <- 1
        iptemp <- ""
        
        for (row in 1:nrow(logs)) { 
          
          ip <- logs$vIP[row]
          url <- logs$vURL[row] 
          
          if (grepl("/robots.txt", url)) {
            caseid <- caseid + 1
          }
          
          if (ip!=iptemp) {
            iptemp <- ip
            caseid <- caseid + 1
          }  
          
          # create caseid
          logs$vCASEID[row] <- caseid
          
        }
        
        logs <- select(logs, vIP, vDATE, vCASEID, vURL)
        colnames(logs) <- c('IP','DATE','CASEID','URL')
        
      }
      
    }
    
    return(logs)
    
  }  
  
  testGoogleIP <- function(ip) {
    
    if(grepl("66.249.",ip)) return(TRUE)
    
    url <- paste("https://api.openresolve.com/reverse/",ip,sep="")
    
    #print(url)
    
    req <-getURL(
      url
      #,httpheader = c(
      #  "accept-encoding" = "gzip"
      #) 
      #, verbose = TRUE
    );
    
    
    result <- fromJSON( req )
    
    if (is.null(result)) return(FALSE)
    if (length(result$AnswerSection)==0) return(FALSE)  
    if (length(result$AnswerSection[[1]]$Target)==0) return(FALSE)  
    
    result <- result$AnswerSection[[1]]$Target
    
    if (grepl("googlebot",result)) return(TRUE)
    else return(FALSE)
    
    
  }  
  
  
  output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    if (!is.null(input$file1)) {
      
      inFile <- input$file1
      
      #print(nrow(inFile))
      
      logsSummary <- NULL
      
      
      for(i in 1:nrow(inFile)) {
        
        #print(input$file1[[i, 'datapath']])
        logs <- importLogs(input$file1[[i, 'datapath']])
        
        if(is.null(logsSummary)) {
          logsSummary <- logs
        }
        else {
          logsSummary <- rbind(logsSummary,logs)
        }
        
        
        
        
      }
      
      logsCount <- nrow(logsSummary)
      
      #print(logsCount)
      output$caption <- renderText({ paste(logsCount," results ",sep="") })
      
      
      head(logsSummary,1000)
      
    }
    
    
    
  })
  
  
  
  output$downloadData <- downloadHandler(
    filename = "extract.csv",
    content = function(file) {
      
      if (!is.null(input$file1)) {
        
        inFile <- input$file1
        
        #print(nrow(inFile))
        
        logsSummary <- NULL
        
        
        for(i in 1:nrow(inFile)) {
          
          #print(input$file1[[i, 'datapath']])
          logs <- importLogs(input$file1[[i, 'datapath']])
          
          if(is.null(logsSummary)) {
            logsSummary <- logs
          }
          else {
            logsSummary <- rbind(logsSummary,logs)
          }
          
          
          
          
        }
        
        
        write.csv2(logsSummary,file,row.names=FALSE)
        
      }
      
    }
  )
  
}

