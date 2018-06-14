

library(igraph)
suppressMessages(library(ggplot2))
## artificial scale free network
scaleFree.network <- sample_pa(10000)
## random network
random.network <- erdos.renyi.game(10000,10000, type="gnm") #10,000 vertices or nodes

#Simulate attacks on  network

SimulateAttack_high_Between <- function(network)
{
  tmp.network <- network
  d <- betweenness(tmp.network)
  remove <- which(d==max(d))   # node with highest degree is removed
  tmp.network <- delete.vertices(tmp.network,remove[1])
  tmp.network.com <- components(tmp.network)
  com.size <- max(tmp.network.com$csize)
  com.size.seq <- com.size
  numNodesRemoved = 1
  while(com.size > 1)
  {
    d <- degree(tmp.network)
    remove <- which(d==max(d))
    tmp.network <- delete.vertices(tmp.network,remove[1])
    tmp.network.com <- components(tmp.network)
    com.size <- max(tmp.network.com$csize)
    com.size.seq <- c(com.size.seq,com.size)
    numNodesRemoved = numNodesRemoved + 1
  }
  return(data.frame(fc= c(1:numNodesRemoved)/vcount(network),NG=com.size.seq/vcount(network)))
}

# R function to simulate targeted attack in a network. Node of highest degree is removed. 

SimulateAttack <- function(network)
{
  tmp.network <- network
  d <- degree(tmp.network)
  remove <- which(d==max(d))   # node with highest degree is removed
  tmp.network <- delete.vertices(tmp.network,remove[1])
  tmp.network.com <- components(tmp.network)
  com.size <- max(tmp.network.com$csize)
  com.size.seq <- com.size
  numNodesRemoved = 1
  while(com.size > 1)
  {
    d <- degree(tmp.network)
    remove <- which(d==max(d))
    tmp.network <- delete.vertices(tmp.network,remove[1])
    tmp.network.com <- components(tmp.network)
    com.size <- max(tmp.network.com$csize)
    com.size.seq <- c(com.size.seq,com.size)
    numNodesRemoved = numNodesRemoved + 1
  }
  return(data.frame(fc= c(1:numNodesRemoved)/vcount(network),NG=com.size.seq/vcount(network)))
}

RealFailure <- SimulateAttack_high_Between(scaleFree.network) 
RealAttack <- SimulateAttack(scaleFree.network)

RandomFailure <- SimulateAttack_high_Between(random.network) 
RandomAttack <- SimulateAttack(random.network)

ggplot(RealFailure,aes(x=fc,y=NG)) + geom_point(color='darkblue') +
  ggtitle("Scale Free Network attack") + xlab("fc f raction of nodes removed") + ylab("fc fraction of nodes in giant component") +
  geom_point(data=RandomFailure,aes(x=fc,y=NG),color='darkgreen')


ggplot(RealAttack,aes(x=fc,y=NG)) + geom_point(color='darkblue') +
  ggtitle("Scale Free Network attack") + xlab("fc f raction of nodes removed") + ylab("fc fraction of nodes in giant component") +
  geom_point(data=RandomAttack,aes(x=fc,y=NG),color='darkgreen')

### Q2 

LinearThreshold <- function (g, failed, threshold) {
  V(g)$failed <- 0
  for (v in failed) {
    V(g)[v]$failed <- 1
  }
  any.changes <- T
  while(any.changes) {
    print(c("spread failure ",any.changes))
    any.changes <- F
    for(v in V(g)) {
      if(! V(g)[v]$failed) {
        neighborhood <- neighbors(g, V(g)[v])
        total.neighbors <- length(neighborhood)
        failed.neighbors <- sum(V(g)[neighborhood]$failed)
        if (total.neighbors > 0 &&
            failed.neighbors / total.neighbors >= threshold) {
          V(g)[v]$failed <- T
          any.changes <- T
        }
      }
    }
  }
  return(g)
}

airline <- read_graph("airlines.graphml", format = c("graphml"))
ecount(airline) 
#2101
vcount(airline) 
#235

# Degree centrality
deg_centrality <- degree(airline)  
# 1, 71, 81, 155



l <- layout.fruchterman.reingold(airline)

failed <- c(1)
V(airline)$failed <- 0
for (v in failed) {
  V(airline)[v]$failed <- 1
}
# Visualize network with node falure. One node failed :  (node 1)
V(airline)$color <- ifelse(V(airline)$failed, "green", "blue")
plot(airline, layout=l,main="Before failure propagation")

# Failure propagation with threshhold = 0.5 
airline_new <- LinearThreshold(airline,failed,0.5)

V(airline_new)$color <- ifelse(V(airline_new)$failed, "green", "blue")
plot(airline_new, layout=l,main="Failure propagation-threshold=0.5")

airline_new <- LinearThreshold(airline,failed,0.0125)

V(airline_new)$color <- ifelse(V(airline_new)$failed, "green", "blue")
plot(airline_new, layout=l,main="Failure propagation-threshold=0.0125")
  
airline_new <- LinearThreshold(airline,failed,0.01)

V(airline_new)$color <- ifelse(V(airline_new)$failed, "green", "blue")
plot(airline_new, layout=l,main="Failure propagation-threshold=0.01")


## Node 71
failed <- c(71)
V(airline)$failed <- 0
for (v in failed) {
  V(airline)[v]$failed <- 1
}
#visualize
V(airline)$color <- ifelse(V(airline)$failed, "green", "blue")
plot(airline, layout=l,main="Before failure propagation")

#perform the failure propagation with threshhold = 0.5, begining with only one failed node (node 1)
airline_new <- LinearThreshold(airline,failed,0.5)

V(airline_new)$color <- ifelse(V(airline_new)$failed, "green", "blue")
plot(airline_new, layout=l,main="After failure propagation, threshold=0.5")

#perform the failure propagation with threshhold = 0.3, begining with only one failed node (node 1)
airline_new <- LinearThreshold(airline,failed,0.05)

V(airline_new)$color <- ifelse(V(airline_new)$failed, "green", "blue")
plot(airline_new, layout=l,main="After failure propagation, threshold =0.05")

airline_new <- LinearThreshold(airline,failed,0.075)

V(airline_new)$color <- ifelse(V(airline_new)$failed, "green", "blue")
plot(airline_new, layout=l,main="After failure propagation, threshold=0.075")

############### Airports fail simultaneously  ######################################  

failed <- c(1, 71, 81, 155)
V(airline)$failed <- 0
for (v in failed) {
  V(airline)[v]$failed <- 1
}
#visualize 
V(airline)$color <- ifelse(V(airline)$failed, "green", "blue")
plot(airline, layout=l,main="Before failure propagation")

# Failure propagation with threshhold = 0.5, 

airline_new <- LinearThreshold(airline,failed,0.5)

V(airline_new)$color <- ifelse(V(airline_new)$failed, "green", "blue")
plot(airline_new, layout=l,main="After failure propagation threshold=0.5")

#Failure propagation with threshhold = = 0.3

airline_new <- LinearThreshold(airline,failed,0.05)

V(airline_new)$color <- ifelse(V(airline_new)$failed, "green", "blue")
plot(airline_new, layout=l,main="After failure propagation threshold =0.05")

#All airports fail at threshold = 0.05 



  