#' save the adjacency matrix to sif network file,
#'
#' save the adjacency matrix using sif network format
#' @param network adjacency matrix
#' @param filepath full path of the networkfile
#' @param mode network type, mode=("undirected","directed")
#' @param diag if self loop is included
#' @param type edge type
#' @return no return
#' @export
saveSif<-function(net,filepath,mode="undirected",diag=FALSE,type="pp")
{
 ID=rownames(net)
 net=c()
 if(mode=="undirected"&!diag)
 {
   for(i in 1:nrow(net))
   {
     for(j in 1:ncol(net))
     {
       if(net[i,j]!=0&i<j&i<nrow(net)&j<nrow(net))
       {
         edge=paste(ID[i]," ",type," ",ID[j],sep="")
         net=c(net,edge)
       }
     }
   }
 }else if(mode=="undirected"&diag)
 {
   for(i in 1:nrow(net))
   {
     for(j in 1:ncol(net))
     {
       if(net[i,j]!=0&i<=j)
       {
         edge=paste(ID[i]," ",type," ",ID[j],sep="")
         net=c(net,edge)
       }
     }
   }
 }else if(mode=="directed"&!diag)
 {
   for(i in 1:nrow(net))
   {
     for(j in 1:ncol(net))
     {
       if(net[i,j]!=0&i!=j)
       {
         edge=paste(ID[i]," ",type," ",ID[j],sep="")
         net=c(net,edge)
       }
     }
   }
 }else if(mode=="directed"&diag)
 {
   for(i in 1:nrow(net))
   {
     for(j in 1:ncol(net))
     {
       if(net[i,j]!=0){
         edge=paste(ID[i]," ",type," ",ID[j],sep="")
         net=c(net,edge)
       }
     }
   }
 }
}
