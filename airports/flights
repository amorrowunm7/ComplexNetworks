library(igraph)
library(poweRlaw)
library(ggplot2)
library(plyr)
library(geosphere)
data<- read.table("Wiki-Vote.txt",header = FALSE)
wiki <-graph.data.frame(d=data, directed=TRUE)


################# Analysis of Wikipedia voters network ##############

# 1.1.	The number of nodes and edges in the network.

gorder(wiki)
gsize(wiki)

# 1.2.	The number of nodes of zero out-degree.

degree_out <- degree(wiki, v = V(wiki), mode = c("out"),
                     loops = TRUE, normalized = FALSE)

sum(degree_out==0)


# 1.3.	The number of nodes of zero in-degree.


degree_in <- degree(wiki, v = V(wiki), mode = c("in"),
                    loops = TRUE, normalized = FALSE)

sum(degree_in==0)

# 1.4.	The number of nodes with more than 10 outgoing edges (out-degree > 10).

degree_in <- degree(wiki, v = V(wiki), mode = c("out"),
                    loops = TRUE, normalized = FALSE)

sum(degree_out>10)

# 1.5.	The number of nodes with more than 10 incoming edges (in-degree > 10).

degree_in <- degree(wiki, v = V(wiki), mode = c("in"),
                    loops = TRUE, normalized = FALSE)

sum(degree_in>10)

# 1.6.	

wiki.degree <- degree(wiki)

G.degree.histogram <- as.data.frame(table(wiki.degree))

G.degree.histogram[,1] <- as.numeric(G.degree.histogram[,1])


ggplot(G.degree.histogram, aes(x = wiki.degree, y = Freq)) +
  geom_point() +
  scale_x_continuous("Degree\n(nodes with this amount of connections)",
                     breaks = c(1, 3, 10, 30, 100, 300),
                     trans = "log10") +
  scale_y_continuous("Frequency\n(how many of them)",
                     breaks = c(1, 3, 10, 30, 100, 300, 1000),
                     trans = "log10") +
  ggtitle("Degree Distribution (log-log)") +
  theme_bw()


################### Analysis of Airlines network  ###################

## You are given two data frames: Flights.txt, Airports.txt. Airports.txt contains information for each airport (nodes) in the network. The first column is the airport name, second, and third columns are longitudes (x) and latitudes (y) locations of the airports, respectively.  The Flights.txt contains the information of direct flights between two airports. Each row indicates a direct flight between two airports (first two columns).  The third column in the file indicates how much the flight costs

airports <- read.table("airports.txt",header = FALSE)
flights <- read.table("flights.txt",header = FALSE)


colnames(flights) <- c("airport1", "airport2","weight")
colnames(airports) <- c("airport", "long","lat")
head(flights)

airline<-graph.data.frame(d=airports, directed=TRUE)
flight<-graph.data.frame(d=flights, directed=TRUE)




## 4.1 Write an R function Possible Flights, that will find all possible paths (allow at most 3 stops) to go from airport 1 to airport2. Test your function for some example airports in the network


all_paths = all_simple_paths(flight,from="ABQ", to="IAH")
all_paths

path_lengths = sapply( all_paths, function( x ){ length( x ) } )

possible_paths = all_paths[ which( path_lengths <= 4 ) ]

possible_paths

## 4.2 Write an R function CheapestFlight that will find the cheapest possible way (display the path and the cost associated with the path) to go from airport 1 to airport2. Here, assume that you don’t care about the number of stops. You will need to make the weight of an edge as the cost of the flight represented by the edge (given in data frame Flights.txt).  Test your function by using some example airports in the network

CheapestFlight<- distances(flight, v = "ABQ", to = "ABE", mode = "out", weights = flight$weight, algorithm = c("automatic"))

print(CheapestFlight)


## 4.3	Write an R function ShortestFlight that will find the shortest possible way (display the path and the distance associated with the path) to go from airport 1 to airport2. Here assume that you don’t care about the cost of the flight. You will need to make the weight of an edge connecting two airports as the distance between the two airports

flights2 <- flights[,1:2]

head(flights2)
p1 <- merge(flights2,airports, by.x="airport1", by.y="airport")
p2 <- merge(flights2,airports, by.x="airport2", by.y="airport")

head(p1)

p3 <- merge(p1, p2, by=c("airport1","airport2"))
head(p3)

p3$distance<-distHaversine(p3[,3:4], p3[,5:6])


head(p3)

p4 <-graph.data.frame(d=p3,directed=TRUE)

head(p4)

short <- shortest_paths(p4,"ABQ",to="ABE",mode = "out",
               weights = p4$distance,output = c("both"))

print(short)




