library(igraph)
library(acepack)
library(WGCNA)
suppressMessages(library(ggplot2)) 

##### Read data #####
trump <- read.csv("TrumpWorld.csv")
trump_network <- trump[,1:2]
trump_network.2 <- as.undirected(graph.edgelist(as.matrix(trump_network)))


############### Exploring data  #################

# Graph between orgs and/or persons
nrow(trump_network)
length(unique(c(trump_network[,1], trump_network[,2])))

##### Plot Graph ####
par(mar=rep(0,4))
plot(trump_network.2,
     layout=layout.fruchterman.reingold,
     vertex.label=NA,
     vertex.size=2,
     edge.arrow.size=.1
)


#### Hierarchical Agglomerative clustering using Topological Overlap Similarity Matrix  - good to determine small clusters ##  

#get adjacency matrix
adjMat <- as.matrix(igraph::get.adjacency(trump_network.2)) 
diag(adjMat) <- 1
#Compute TOM Similarity
TOMSimilarity_Trump <- WGCNA::TOMsimilarity(adjMat)


#Compute TOM Dissimilarity (need the distance instead of similarity for further clustering
TOMDisimilarity_Trump <- 1- TOMSimilarity_Trump

suppressMessages(library(fastcluster))
h <- fastcluster::hclust(as.dist(TOMDisimilarity_Trump), method="ave")

#plot the hierarchical tree for the Trump network

plot(h)

communities3 <- cutree(h,k=50)
V(trump_network.2)$group <- communities3
V(trump_network.2)$color <- "red"

V(trump_network.2)$color[V(trump_network.2)$group == 50] <- "lightblue"
V(trump_network.2)$color[V(trump_network.2)$group == 50] <- "green" 
plot(trump_network.2,main="agglomerative - 50 communities")


communities <- cutree(h,k=100)
V(trump_network.2)$group <- communities
V(trump_network.2)$color <- "red"

V(trump_network.2)$color[V(trump_network.2)$group == 100] <- "lightblue"
V(trump_network.2)$color[V(trump_network.2)$group == 100] <- "green" 
plot(trump_network.2,main="agglomerative - 100 communities")


######### Hierarchical Divisive clustering using Link betweenness ######


Communities_eb <- cluster_edge_betweenness(trump_network.2,modularity=FALSE) 

plot(as.dendrogram(Communities_eb),
     main="Communities Tree - Edge Betweenness")

# only label vertices with more than 25 edges connecting to them

V(trump_network.2)$label <- ifelse(degree(trump_network.2) > 25, V(g)$name, NA)

# scale vertex size based on node degree
vertex_size <- pmax(1, log2(degree(trump_network.2)))

c2_eb <- cutree(as.hclust(Communities_eb),k=50)
V(trump_network.2)$group <- c2_eb
V(trump_network.2)$color <- "red"
V(trump_network.2)$color[V(trump)$group == 50] <- "lightblue"
plot(trump_network.2,main="Divisive Edge Betweenness - 2 communities")

### Newman Greedy modularity maximization  #### 

adjMat <- as.matrix(igraph::get.adjacency(trump_network.2))
diag(adjMat) <- 1
#Compute TOM Similarity
TOMSimilarity_Trump <- WGCNA::TOMsimilarity(adjMat)

m <- c()
for(k in 1:100) #k is the number of communities
  {
  communities <- cutree(h,k=k)
m <- c(m,modularity(trump_network.2,communities)) }
trump_modularity <- data.frame(k=1:100,modularity = m) 
trump_modularity

ggplot(data=trump_modularity) + geom_point(aes(x=k,y=modularity)) + geom_line(aes(x=k,y=modularity)) 

## The modularity graph looks intersting when running for different values of K. I re-ran the values for up to 100. The graph shows a continuous increase untill k=75

#####  Apply divisive hierarchical clustering (random walk betweeness metric) and compute modularity for each K ## 


Communities_rw <- cluster_walktrap(trump_network.2,modularity=FALSE)

m <- c()
for(k in 1:100) #k is the number of communities 
  {
  c2_rw <- cutree(as.hclust(Communities_eb),k=k)
m <- c(m,modularity(trump_network.2,c2_rw)) }
trump_modularity <- data.frame(k=1:100,modularity = m) 
trump_modularity

ggplot(data=trump_modularity) + geom_point(aes(x=k,y=modularity)) + geom_line(aes(x=k,y=modularity))

#### Graph shows best is k=50

####### Newman’s greedy method #########


newman_trump_com <- cluster_fast_greedy(trump_network.2) 
#obtain modularity
modularity(newman_trump_com)

#membership of each node
newman_trump_com$membership

#size
sizes(newman_trump_com)


l <- layout_with_fr(trump_network.2)

plot(net.bg, layout=l)

#visualization 1
plot(newman_trump_com,trump_network.2,layout=l,main="Newman's method communities")

#visualization 2

V(trump_network.2)$color <- newman_trump_com$membership+1
trump <- set_graph_attr(trump_network.2, "layout", layout_with_fr(trump_network.2)) 

plot(trump, vertex.label.dist=1.5,layout=l,main="Newman's method communities")

################## Louvain method ################## 

louvain_trump_com <- cluster_louvain(trump_network.2) 

#obtain modularity modularity(louvain_trump_com)
modularity(louvain_trump_com)

#membership of each node
louvain_trump_com$membership

l <- layout_with_fr(trump_network.2)

#visualization 1
plot(louvain_trump_com,trump_network.2,
     layout=l,
     main="Louvian method communities")


########### Pick the community with the largest size detected by the above approaches and generate a venn diagram indicating the similarities between the communities. One way to generate venn diagrams is to use gplots #################


x <- which.max(sizes(louvain_trump_com))
subg <- induced.subgraph(trump_network.2, which(membership(louvain_trump_com) == x))

intersection(subg, byname = "auto",
             keep.all.vertices = TRUE)

### For result obtained by Louvain method, for each of the communities with size >= 100, who are the most important (use any centrality metric of your choice) ? Note here that the centrality metric should be calculated on the network that represents the community and not the entire network.


## Communites greater than 100
louvain.100 <- which(sizes(louvain_trump_com) >=100) 
louvain.100


sub_graph <- induced_subgraph(trump_network.2,
                              c(which(sizes(louvain_trump_com) >=100)))


########  For result obtained by Louvain method, create meaningful visualizations of the communities that DONALD J. TRUMP, BETSY DEVOS, and JARED KUSHNER belong to. Do they belong to the same community ? ######## 


#### Find most connected nodes
which.max(degree(trump_network.2))
# Create subnetworks
trump_network.2.d <- decompose(trump_network.2)
# Find largest subnetwork
largest <- which.max(sapply(trump_network.2.d, diameter))
#Plot largest subnetwork
plot(trump_network.2.d[[largest]],
     layout=layout.fruchterman.reingold,
     vertex.label.cex=.5,
     vertex.size=5,
     edge.arrow.size=.1
)





