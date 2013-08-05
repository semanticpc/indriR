# Query Dependent Features 
scoringStats <- function(indriIndex, field){
  qIndex <- indriIndex$getQueryStats()[,"qIndex"]
  docTermMat <- as.matrix(indriIndex$getTFMatrix(field))
  qTerms <- rownames(indriIndex$getQueryStats())
  termStats <- as.matrix(indriIndex$getTermStats(field)[qTerms, ])
  return(new(ScoringFeatures, docTermMat, termStats, qIndex, rownames(docTermMat)))
}
 

getQueryDependentFeatures <- function(indriIndex, sStats, featureList, field){
  termWeighting <- c('Boolean',
                     'VectorSpace',
                     'BM25',
                     'LMD',
                     'LMJM')
  qDfeatures <- matrix()
  
  for(feature in featureList){
    if(feature %in% termWeighting)
      qDfeatures <- cbind(qDfeatures, do.call(feature, list(indriIndex, field)))
    else
      qDfeatures <- cbind(qDfeatures, do.call(feature, list(sStats, field)))
  }
  return(qDfeatures[, !names(qDfeatures) %in% c("qDfeatures")])
}

numOfQueryTerms <- function(sStats, field){
  res <- data.frame(sStats$numOfQTerms())
  colnames(res) <- c(paste("numOfQueryTerms", field, sep="_"))
  return(res)
}

queryTermRatio <- function(sStats, field){
  res <- data.frame(sStats$ratioOfQTerms())
  colnames(res) <- c(paste("queryTermRatio", field, sep="_"))
  return(res)
}

streamLength <- function(sStats, field){
  res <- data.frame(sStats$getStreamLen())
  colnames(res) <- c(paste("streamLength", field, sep="_"))
  return(res)
}

queryTermFrequencyStats <- function(sStats, field){
  res <- data.frame(sStats$termFrequency("sum"),
                    sStats$termFrequency("min"),
                    sStats$termFrequency("max"),
                    sStats$termFrequency("var"),
                    sStats$termFrequency("mean"))
  
  colnames(res) <- c(paste('termFrequency_sum', field , sep='_'),
                     paste('termFrequency_min', field , sep='_'),
                     paste('termFrequency_max', field , sep='_'),
                     paste('termFrequency_var', field , sep='_'),
                     paste('termFrequency_mean', field , sep='_'))
  return(res)
  
}

queryNormTermFrequencyStats <- function(sStats, field){
  res <- data.frame(sStats$lenNormTermFrequency("sum"),
                    sStats$lenNormTermFrequency("min"),
                    sStats$lenNormTermFrequency("max"),
                    sStats$lenNormTermFrequency("var"),
                    sStats$lenNormTermFrequency("mean"))
  
  colnames(res) <- c(paste('normTF_sum', field , sep='_'),
                      paste('normTF_min', field , sep='_'),
                      paste('normTF_max', field , sep='_'),
                      paste('normTF_var', field , sep='_'),
                      paste('normTF_mean', field , sep='_'))
  return(res)
}

queryTFIDFStats <- function(sStats, field){
  res <- data.frame(TFIDF_sum=sStats$TFIDF("sum"),
                    TFIDF_min=sStats$TFIDF("min"),
                    TFIDF_max=sStats$TFIDF("max"),
                    TFIDF_var=sStats$TFIDF("var"),
                    TFIDF_mean=sStats$TFIDF("mean"))
  
  colnames(res) <- c(paste('TFIDF_sum', field , sep='_'),
                      paste('TFIDF_min', field , sep='_'),
                      paste('TFIDF_max', field , sep='_'),
                      paste('TFIDF_var', field , sep='_'),
                      paste('TFIDF_mean', field , sep='_'))
  return(res)
}

Boolean <- function(indriIndex, field){
  
}

VectorSpace <- function(sStats, field){
  
}



BM25 <- function(indriIndex, field){
  retMethod = "okapi"
  retParams = "k1:1.0,b:0.3"
  indriIndex$setScoringRules(retMethod, retParams)
  res <- data.frame(BM25=indriIndex$runIndriModel())
  colnames(res) <- c(paste('BM25', field , sep='_'))
  return(res)
}

LMD <- function(indriIndex, field){
  retMethod = "dirichlet"
  retParams = "mu:2500"
  index$setScoringRules(retMethod, retParams)
  res <- data.frame(LMD=indriIndex$runIndriModel())
  colnames(res) <- c(paste('LMD', field , sep='_'))
  return(res)
}

LMJM <- function(indriIndex, field){
  retMethod = "jelinek-mercer"
  retParams = "collectionLambda:0.2,documentLambda:0.0"
  index$setScoringRules(retMethod, retParams)
  res <- data.frame(LMJM=indriIndex$runIndriModel())
  colnames(res) <- c(paste('LMJM', field , sep='_'))
  return(res)
}