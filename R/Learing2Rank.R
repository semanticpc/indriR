# Learning to rank -- Train and RunQuery

# runL2RQuery <- function(query, indriIndex, model){
#   
# }
# 
# trainL2RQuery <- function(queries, indriIndex, trainSet){
#   
# }

getFeatures <- function(qno, query, indriIndex){
  
  ##############################################################################
  # Query Independent Features 
  ##############################################################################
  docTermMatrix <- indriIndex$getTFMatrix("all")
  
  features <- getPageRank(indriIndex)
  
  features <- cbind(features, getSpamScore(indriIndex))
  features <- cbind(features, getEntropy(docTermMatrix, docLen))
  features <- cbind(features, getURLFeatures(indriIndex))
  
  ##############################################################################
  # Query Dependent Features 
  ##############################################################################
  fields <- c('all', 'title', 'heading', 'url')
  
  feature_list <- c('numOfQueryTerms',  #Simple Features 
                    'queryTermRatio',

                    'streamLength',
                    #'IDF',
                    
                    #Term Weighting Features
                    'queryTermFrequencyStats',
                    'queryNormTermFrequencyStats',
                    'queryTFIDFStats',
                    
                    #Term Weighting Models
                    #'Boolean',
                    #'VectorSpace',
                    'BM25',
                    'LMD',
                    'LMJM')
  
  for(field in fields){
    sStats <- scoringStats(indriIndex, field)
    features <- cbind(features, getQueryDependentFeatures(indriIndex, 
                                                          sStats, 
                                                          feature_list,
                                                          field))
    
  }
  

  
  # Positional Models Scores  
  #features <- cbind(features, getMRF())
  return(features)
}