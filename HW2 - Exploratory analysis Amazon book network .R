library("igraph")
library("poweRlaw")
library("ggplot2")
library(rgexf)
library(dplyr)
library(GGally)
library(network)


data<-read.graph("polbooks.gml",format=c("gml"))


### Plot histogram of node degree (both log and linear scale plots). 
deg <- degree(data, mode="all")
plot(data, vertex.size=deg*3)

hist(deg, breaks=1:vcount(data)-1, main="Histogram of node degree")

plot(degree.histogram, log="y", type='h', lwd=10, lend=2)

###Find out total number of nodes and edges. 
gorder(data)
gsize(data)

# Is the network connected? 

yes
vertex_connectivity(data, source = NULL, target = NULL, checks = TRUE)

### What is the diameter of the network? 
diameter(data, directed = TRUE, unconnected = F, weights = NULL)


### Compute the following centrality metrics for each node to rank the importance of the books: 

# Degree centrality
degree(data, mode="all")
degree <- centr_degree(data, mode="all", normalized=T)

# Eigen vector centrality
eigen_centrality(data, directed=T, weights=NA) 
eigen <- centr_eigen(data, directed=T, normalized=T)

# Alpha centrality

alpha <- alpha_centrality(data, nodes = V(data), alpha = 1, loops = FALSE,exo = 1, weights = NULL,sparse = TRUE)

alpha
# Page rank centrality

page <- page_rank(data)

# Betweeness centrality

between <- centr_betw(data,normalized =T)


# Closeness centrality 


close <-centr_clo(data, mode="all", normalized=T)



### Display the top 10 books ranked based on each of the above metrics. Are the rankings different ? Are there any books that consistently ranked high irrespective of the centrality metrics 

degree_top10 <- data.frame(V(data)$label,degree)
degree_top10 %>% top_n(10)


eigen_top10 <- data.frame(V(data)$label,eigen)
eigen_top10 %>% top_n(10)

alpha_top10 <- data.frame(V(data)$label,alpha)
alpha_10 %>% top_n(10)

page_top10 <- data.frame(V(data)$label,page)
page_10 %>% top_n(10)

between_top10 <- data.frame(V(data)$label,between)
between_10 %>% top_n(10)

close_top10 <- data.frame(V(data)$label,between)
close_10 %>% top_n(10)


###  Generate meaningful visualizations of the network by using a force directed layout, sizing the nodes based on a centrality metric (test with the 6 different metrics), and label the nodes with the title of the book. 

data_with_attributes <- read_graph('polbooks.gml',format="gml")
## Function to wrap long strings
# Source: http://stackoverflow.com/a/7367534/496488

wrap_strings <- function(vector_of_strings,width){
  as.character(sapply(vector_of_strings, FUN=function(x){
    paste(strwrap(x, width=width), collapse="\n")
  }))
}

# Apply the function to wrap the node labels
V(data)$label = wrap_strings(V(data)$label, 12)

## Shrink font
V(data)$label.cex = 0.8

# Function to increase node separation 
# Source: http://stackoverflow.com/a/28722680/496488

layout.by.attr <- function(graph, wc, cluster.strength=1,layout=layout.auto) {  
  g <- graph.edgelist(get.edgelist(graph)) # create a lightweight copy of graph w/o the attributes.
  E(g)$weight <- 1
  
  attr <- cbind(id=1:vcount(g), val=wc)
  g <- g + vertices(unique(attr[,2])) + igraph::edges(unlist(t(attr)), weight=cluster.strength)
  
  l <- layout(g, weights=E(g)$weight)[1:vcount(graph),]
  return(l)
}

set.seed(3)


plot(data, vertex.shape="none", vertex.size=alpha,     
     vertex.label.color=ifelse(V(data)$value=="l", "blue", "red"),layout=layout.kamada.kawai)


plot(data,layout=layout.kamada.kawai,size=eigen, main = "kamada.kawai-layout")

plot(data,layout=layout.kamada.kawai,size=degree, main = "kamada.kawai-layout")

plot(data,layout=layout.kamada.kawai,size=between, main = "kamada.kawai-layout")

plot(data,layout=layout.kamada.kawai,size=close, main = "kamada.kawai-layout")

plot(data,layout=layout.kamada.kawai,size=page, main = "kamada.kawai-layout")


####  By simply observing the visualization How many groups of books can you identify Can you interpret the groups based on ‘kinds of books’ the group contain

# I can really only see two main clusters of different types of books.I was able to notice a difference in the the "types" of books based on the two groups.Judging only by book names and not knowing anything about content, I would say one cluster of books has a more liberal view and the other a more political "right" view. 

  
### 2  Now, you will design a simple recommendation engine to recommend books to a new buyer based on network degree centrality metric. Write an R function which will allow the user to search for books based on title. The function should return a list of recommended titles. 
# The basic premise of the function is listed as follows: 

# The function should first find all the nodes (books) in the network with their titles that partially (suggested R function is ‘grep’) match book title entered by user. Assume that upper and lower case letters are same. Then, extract the sub-network containing only these matched nodes and their neighbors with distance of at most 2 (suggested igraph functions are ‘induced_subgraph’ and ‘neighbors’)

# The function should display the books ranked based on degree centrality metric. Note that the centrality metric should be recalculated using the extracted subnetwork. 

#The function should also visualize a meaningful display of the extracted subnetwork of books. 

#Test your function by querying some example items. Choose items belonging to different groups that you identified from the visualization. Suggested keywords are ‘Democracy’, ‘Bush’, ‘America’, ‘Country’, ‘War’

?induced_subgraph
#vertices
V(data_with_attributes)
#edges
E(data_with_attributes)
vertex <-vertex.attributes(data_with_attributes)
book_title <- 'America'



match <- grep(book_title,vertex(data,book_title),ignore.case = T)

sub <- induced_subgraph(data, match)

plot(sub, vertex.shape="none", vertex.size=alpha,     
     vertex.label.color=ifelse(V(sub)$value=="l", "blue", "red"),layout=layout.kamada.kawai)

sub_degree <- centr_degree(sub, mode="all", normalized=T)
sub_10 <- data.frame(V(sub)$label,sub_degree)
sub_10 %>% top_n(10)




RecommendTitles <- function(data, book_title) 
{
  x <- grep(book_title,vertex(data,book_title),ignore.case = T)
  
  dex <-x
  for (i in x)
    neighbors <- neighbors(data,i)
  dex3  <- c(data,neighbors)
}
index <- unique(dex)

index