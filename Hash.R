require(digest)

hashVect <- function(vector) {
  vector <- lapply(vector,function(x){
    if(!is.na(x)){
      digest(x)
    } else {
      NA
    }
  })
  unlist(vector)
}
