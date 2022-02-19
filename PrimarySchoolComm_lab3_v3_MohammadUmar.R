getwd()
# Save the data file to a location on your hard drive and specify the path here (Windows systems use forward slashes)
dir_path <-“~/YourWorkingDirectoryFilePath”
setwd(dir_path)
# clear everything out of memory
rm(list=ls())  

# Load primary school data, contact data
infile_edges<-"Edges_sp_data_school_day_2.csv"
infile_nodes<-"Nodes_sp_data_school_day_2.csv"

## Load package
library(igraph)
edge_frame=read.csv(infile_edges, header = TRUE, sep = ",")
node_frame=read.csv(infile_nodes, header = TRUE, sep = ",")

g_primschool=graph.data.frame(edge_frame, directed = FALSE, vertices= node_frame)

plot(g_primschool)
# Edges
ecount(g_primschool)
## Vertices
vcount(g_primschool)
is.weighted(g_primschool)

V(g_primschool)$name
E(g_primschool)$weight
V(g_primschool)$gender
V(g_primschool)$classname
V(g_primschool)[V(g_primschool)$classname=="1B"]

is.simple(g_primschool)
is.connected(g_primschool)

# http://igraph.wikidot.com/community-detection-in-r
# "The following code snippet performs a Wilcoxon rank-sum test on the "internal" and "external"
# degrees of a community in order to quantify its significance. Let us call the edges within a 
# community "internal" and the edges connecting the vertices of a community with the rest of the graph "external".
# The null hypothesis of the test is that there is no difference between the number of "internal" and "external" edges 
# incident to a vertex of the community. More internal than external edges show that the community is significant; less 
# internal than external edges show that the community is in fact an "anti-community". The p-value of the test performed by 
# this function will be close to zero in both cases; the value of the test statistic tells us whether we have a community or an anti-community."
community.significance.test <- function(graph, vs, ...) {
  if (is.directed(graph)) stop("This method requires an undirected graph")
  subgraph <- induced.subgraph(graph, vs)
  in.degrees <- degree(subgraph)
  # Total degree among nodes in the vs list, minus the degree within the subgraph 
  out.degrees <- degree(graph, vs) - in.degrees
  wilcox.test(in.degrees, out.degrees, ...)
}
################

stud.class <- get.vertex.attribute(g_primschool, "classname")
stud.gender<- get.vertex.attribute(g_primschool, "gender")

# Does edge weight make any difference here?

########### Community detection using the Fast Greedy Algorithm
school_comm_fast <- fastgreedy.community(g_primschool, weights=E(g_primschool)$weight)
school_comm_fast
c.m <- membership(school_comm_fast)
c.m

# Assignment to communities, based on class section or teacher status. This analysis can be extended to gender (see below).
table(c.m, stud.class, useNA = c("no"))
table(c.m, stud.gender, useNA = c("no"))

# Here, we are testing community significance for just two of the communities. 
v_comp1 <- V(g_primschool)[c.m==1]
v_comp2 <- V(g_primschool)[c.m==2]
v_comp3 <- V(g_primschool)[c.m==3]
v_comp4 <- V(g_primschool)[c.m==4]
v_comp5 <- V(g_primschool)[c.m==5]
v_comp6 <- V(g_primschool)[c.m==6]
v_comp7 <- V(g_primschool)[c.m==7]
community.significance.test(g_primschool, v_comp1)
community.significance.test(g_primschool, v_comp2)
community.significance.test(g_primschool, v_comp3)
community.significance.test(g_primschool, v_comp4)
community.significance.test(g_primschool, v_comp5)
community.significance.test(g_primschool, v_comp6)
community.significance.test(g_primschool, v_comp7)

set.seed(123)
# Similar plots for the walktrap, spinglass, and label propagation algorithms for community detection
plot(school_comm_fast,g_primschool, vertex.label= NA, vertex.size=2)
# In the igraph help, online documentation or KC book, students will find the function calls for the walktrap, spinglass, and label propagation algorithms 
# Why are the benefits and drawbacks of the Girvan-Newman algorithm for community detection? Hint: try it in igraph

########### Walktrap Algorithm #################
school_comm_wt <- walktrap.community(g_primschool, modularity=TRUE, weights=E(g_primschool)$weight)
school_comm_wt
c.m_wt <- membership(school_comm_wt)
c.m_wt

table(c.m_wt, stud.class, useNA = c("no"))
table(c.m_wt, stud.gender, useNA = c("no"))
plot(school_comm_wt,g_primschool, vertex.label= NA, vertex.size=2)

################# Spin-glass #############
school_comm_sc <- spinglass.community(g_primschool, spins=10, weights=E(g_primschool)$weight)
school_comm_sc
c.m_sc <- membership(school_comm_sc)
c.m_sc

table(c.m_sc, stud.class, useNA = c("no"))
table(c.m_sc, stud.gender, useNA = c("no"))
plot(school_comm_sc,g_primschool, vertex.label= NA, vertex.size=2)

############## Label propagation ####################
school_comm_lp<-label.propagation.community(g_primschool, weights=E(g_primschool)$weight)
c.m_lp <- membership(school_comm_sc)
c.m_lp

table(c.m_lp, stud.class, useNA = c("no"))
table(c.m_lp, stud.gender, useNA = c("no"))
plot(school_comm_lp, g_primschool, vertex.label= NA, vertex.size=2)
?label.propagation.community
################# Girvan Newman###################

cluster_edge_betweenness(
  g_primschool,
  weights = E(g_primschool)$weight,
  directed = TRUE,
  edge.betweenness = TRUE,
  merges = TRUE,
  bridges = TRUE,
  modularity = TRUE,
  membership = TRUE
)
eb <- cluster_edge_betweenness(g_primschool)
eb
plot(eb, g_primschool, vertex.label= NA, vertex.size=2)


# Consider students in first grade and 5th grade. To what extent does community structure indicate that students segregate by gender in these two grades?
# Use the Fast Greedy algorithm for analysis.
v_grade1students<-V(g_primschool)[V(g_primschool)$classname=="1B" | V(g_primschool)$classname=="1A"]
v_grade5students<-V(g_primschool)[V(g_primschool)$classname=="5B" | V(g_primschool)$classname=="5A"]
v_grade1students
v_grade5students

subgraph_grade1<-induced_subgraph(g_primschool, v_grade1students)
subgraph_grade5<-induced_subgraph(g_primschool, v_grade5students)

stud.class1 <- get.vertex.attribute(subgraph_grade1, "classname")
stud.gender1<- get.vertex.attribute(subgraph_grade1, "gender")
stud.class5 <- get.vertex.attribute(subgraph_grade5, "classname")
stud.gender5<- get.vertex.attribute(subgraph_grade5, "gender")
### Fast Greedy For Grade 1 ####
par(mfrow=c(1,2)) # Run this then the 2 plots needed to be in same graph
school_comm_fast_1 <- fastgreedy.community(subgraph_grade1, weights=E(subgraph_grade1)$weight)
plot(school_comm_fast_1,subgraph_grade1, vertex.label= NA, vertex.size=2)


c.m_1 <- membership(school_comm_fast_1)
c.m_1
table(c.m_1, stud.class1, useNA = c("no"))
table(c.m_1, stud.gender1, useNA = c("no"))

### Fast Greedy for Grade 5 ###
school_comm_fast_5 <- fastgreedy.community(subgraph_grade5, weights=E(subgraph_grade5)$weight)
plot(school_comm_fast_5, subgraph_grade5, vertex.label= NA, vertex.size=2)

c.m_5 <- membership(school_comm_fast_5)
c.m_5
table(c.m_5, stud.class5, useNA = c("no"))
table(c.m_5, stud.gender5, useNA = c("no"))
### As kids become older they segregate more on the basis of gender; makes sense cause they get a sense of gender 

##### Q8 ###
g <- graph.formula(A-B,B-C,C-D, D-E, E-A)
E(g)$sign<-c(+1,-1, -1, +1, 1)
plot(g)

#### Q9#####
h <- graph.formula(A-B,A-C,A-D, B-C, B-D, C-D )
E(h)$sign<-c(+1,1, -1, 1, -1, 1)
plot(h)

