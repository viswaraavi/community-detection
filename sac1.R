library(igraph)
library(lsa)
graph=read_graph("C:/Users/viswa/Desktop/project/data/1.txt",format= c("edgelist"))
attribute_data <- read.csv("C:/Users/viswa/Desktop/project/data/1.csv",header = TRUE)


#SC1 Algorithm


#Cosine update of the function



cosine_update <- function(k,memebership,values,x)
{
  indices=which(values==memebership)
  sim=0
  for(i in indices)
  {
    sim=sim+cosine(as.numeric(x[k,]),as.numeric(x[i,]))
  }
  
  sim <- sim/length(indices)
  
}


#Phase 1 of Sac1 Algorithm

phase1 <- function(graph,mapped_values=c(1:324),alpha,y=attribute_data){
  for(k in 1:15)
  {
    x=mapped_values
    for(i in 1:vcount(graph))
    {
      
      index <- 0
      max <- 0
      
      n <- neighbors(graph, i)
      for(j in unique(mapped_values[n]))
      {
        
        membership1=mapped_values
        mi=modularity(graph,membership1)
        membership1[i]=j
        ni=modularity(graph,membership1)
        cosine_x <- (1-alpha)*(cosine_update(i,j,mapped_values,y))+(alpha)*(ni-mi)
       
        if(i!=j && cosine_x > max){
          index <- j
          max <- cosine_x
        }
      }
      if(index !=0){
      mapped_values[i] <- index
      }
      
    }
    if(isTRUE(all.equal(x,mapped_values)))
    {
      break
    }
    x=mapped_values
    
  }
  mapped_values  
}

#Phase2 of sac1 algorithm

#By changing the value of alpha we can get different communities
sac1 <- function(alpha){
mapped_communities <- phase1(graph,alpha=alpha,mapped_values = c(1:324))
x=mapped_communities
for(h in 1:15)
{
  g2 <- contract.vertices(graph, mapped_communities)
  g3 <- simplify(g2, remove.multiple = TRUE, remove.loops = TRUE)
  mapped_communities <- phase1(g3,mapped_communities,alpha,attribute_data)
  if(isTRUE(all.equal(x,mapped_communities)))
  {
    break
  }
  x=mapped_communities
}


#writing to file


fileConn<-file("communities.txt","w")

for(i in 1:length(unique(mapped_communities)))
{
  community <- vector("numeric")
  for(j in 1:324)
  {
    if(mapped_communities[j]==unique(mapped_communities)[i]){
      
      community <- append(community,j,after = length(community))
      
    }
  }
  cat(as.character(community), file=fileConn,sep = ",")
  cat("\n", file=fileConn)
}

close(fileConn)

}

args <- commandArgs(trailingOnly = TRUE)

sac1(alpha = as.numeric(args[1]))



