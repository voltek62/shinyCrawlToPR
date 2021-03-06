#autoinstall packages
packages <- c("igraph", "dplyr", "ggplot2", "magrittr")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

library(igraph)
library(dplyr)
library(ggplot2)
library(magrittr)

#screaming frog :  Bulk Export > All Outlinks from the top menu, and save the CSV file 
file_outlinks         <- 'all_outlinks.csv'
website_url          <- 'https://data-seo.fr'

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

#if (!exists("DF")) {
DF <- read.csv2(file_outlinks, header=TRUE, sep = ",", stringsAsFactors = F, skip=1 )
## we keep only link
DF <- filter(DF,grepl(website_url,Source) & Type=="HREF" & Follow=="true") %>%
  select(Source,Destination)

DF <- as.data.frame(sapply(DF,gsub,pattern=website_url,replacement=""))

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



#install.packages("GGally")
#library(GGally)
#library(network)

#ggnet2(graphObject, color = "party",  alpha = 0.75, size = 4, edge.alpha = 0.5)

