require(digest)

hashVect <- function(vector) {
  vector <- lapply(vector,function(x){
    if(is.na(x) | nchar(x)==0){
      NA
    } else {
      digest(x)
    }
  })
  unlist(vector)
}

hashAndSave <- function(inFname, outFname, cols){
  df <- read.csv(inFname, stringsAsFactors = F)
  for(i in 1:length(cols)){
    df[,cols[i]] <- hashVect(df[,cols[i]])
  }
  write.csv(df,outFname,row.names=F)
}

