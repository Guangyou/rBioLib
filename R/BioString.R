#' match two string vector
#'
#' Search out the index in b for each element in a
#' @param a The query string vector
#' @param b The target string vector
#' @return the index in b for each query element in a
#' @export
matchID<-function(a,b)
{
  cc<-c()
  for(i in a)
  {
    print(i)
    ii<-unique(strsplit(i,";")[[1]])
    bi<-c()
    for(j in b)
    {
      jj<-unique(strsplit(j,";")[[1]])
      bi<-c(bi,length(intersect(ii,jj))/length(union(ii,jj)))
    }
    bj<-which.max(bi)
    if(bi[bj]>=0.5)
    {
      cc<-c(cc,bj)
    }else{
      cc<-c(cc,NA)
    }
  }
  return(cc)
}
