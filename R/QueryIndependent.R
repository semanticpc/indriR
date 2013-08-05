getPageRank <- function(indriIndex){
  pg <- as.numeric(indriIndex$getMetaData( "pagerank"))
  pg[is.na(pg)] <- 0
  return(data.frame(pageRank=pg))
}

getSpamScore <- function(indriIndex){
  spam <- as.numeric(indriIndex$getMetaData( "spam"))
  spam[is.na(spam)] <- 0
  return(data.frame(spam=spam))
}

getEntropy <- function(docTermMatrix){

  docLen <- colSums(docTermMatrix)
  entropy <- c()
  for(i in 1:nrow(docTermMatrix)){
    p_w <- docTermMatrix[i,] / docLen[i]
    entropy <- c(entropy, sum(p_w * log(p_w),na.rm=T))
  }
  return(data.frame(entropy=entropy))
}

#streamLength <- function(indriIndex){
#  docLen <- indriIndex$getDocumentLengths()
#  return(data.frame(docLength=docLen))
#}

getURLFeatures <- function(indriIndex){
  url_list <- indriIndex$getMetaData( "url")
  # Length of Host
  # Length of path (url)
  length <- sapply(url_list, nchar)
  # Depth of URL
  depth <- sapply(url_list, str_count, "/")
  # Digits in Domain
  # Digits in host 
  # Is URL a WikiPage
  return(data.frame(urlLength=length,
                    urlDepth=depth))
}
