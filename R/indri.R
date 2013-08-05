evalqOnLoad({
  # Load Judgment Related Rcpp Modules
  loadModule("Index", TRUE)
  loadModule("ScoringFeatures", TRUE)
  loadModule("GreedyDiversity", TRUE)
})
open_index <- function(path, server=F) {
  index <- new(Index, path, server)
  return(index)
}
test <- function(){
  index <- open_index("/Users/semanticpc/work/data/index/clueweb09_sample")
  index$generateResults( "1", "world war", 1, F)
  index$generateSnippets()
}
# 
# index <- open_index("/Users/semanticpc/work/assessorAgreement/index/trec4")
