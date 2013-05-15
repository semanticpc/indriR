# Term Document Matrix Features

#termDocMatrix <- indriIndex$generateResults( qno, query, docLimit)

getScoringFeatures <- function(indriIndex){
  docTermMat <- indriIndex$getDocTermMatrix("tf")
  termStats <- as.matrix(indriIndex$getTermStats())
  return(new(ScoringFeatures, docTermMat, termStats, rownames(docTermMat)))
}


