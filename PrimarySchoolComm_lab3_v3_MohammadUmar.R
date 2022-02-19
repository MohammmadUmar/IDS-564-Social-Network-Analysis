getwd()
# Save the data file to a location on your hard drive and specify the path here (Windows systems use forward slashes)
dir_path <-â~/YourWorkingDirectoryFilePathâ
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
#Students will complete tests for the remainder of communities for each algorithm. 

v_comp1_fc <- V(g_primschool)[c.m==1]
v_comp2_fc <- V(g_primschool)[c.m==2]
v_comp3_fc <- V(g_primschool)[c.m==3]
v_comp4_fc <- V(g_primschool)[c.m==4]
v_comp5_fc <- V(g_primschool)[c.m==5]
v_comp6_fc <- V(g_primschool)[c.m==6]
v_comp7_fc <- V(g_primschool)[c.m==7]
community.significance.test(g_primschool, v_comp1_fc)
community.significance.test(g_primschool, v_comp2_fc)
community.significance.test(g_primschool, v_comp3_fc)
community.significance.test(g_primschool, v_comp4_fc)
community.significance.test(g_primschool, v_comp5_fc)
community.significance.test(g_primschool, v_comp6_fc)
community.significance.test(g_primschool, v_comp7_fc)

set.seed(123)
# Students will produce similar plots for the walktrap, spinglass, and label propagation algorithms for community detection
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

v_comp1_wt <- V(g_primschool)[c.m_wt==1]
v_comp2_wt <- V(g_primschool)[c.m_wt==2]
v_comp3_wt <- V(g_primschool)[c.m_wt==3]
v_comp4_wt <- V(g_primschool)[c.m_wt==4]
v_comp5_wt <- V(g_primschool)[c.m_wt==5]
v_comp6_wt <- V(g_primschool)[c.m_wt==6]
v_comp7_wt <- V(g_primschool)[c.m_wt==7]
v_comp8_wt <- V(g_primschool)[c.m_wt==8]
v_comp9_wt <- V(g_primschool)[c.m_wt==9]

community.significance.test(g_primschool, v_comp1_wt)
community.significance.test(g_primschool, v_comp2_wt)
community.significance.test(g_primschool, v_comp3_wt)
community.significance.test(g_primschool, v_comp4_wt)
community.significance.test(g_primschool, v_comp5_wt)
community.significance.test(g_primschool, v_comp6_wt)
community.significance.test(g_primschool, v_comp7_wt)
community.significance.test(g_primschool, v_comp8_wt)
community.significance.test(g_primschool, v_comp9_wt)

################# Spin-glass #############
school_comm_sc <- spinglass.community(g_primschool, spins=10, weights=E(g_primschool)$weight)
school_comm_sc
c.m_sc <- membership(school_comm_sc)
c.m_sc

table(c.m_sc, stud.class, useNA = c("no"))
table(c.m_sc, stud.gender, useNA = c("no"))
plot(school_comm_sc,g_primschool, vertex.label= NA, vertex.size=2)

v_comp1_sc <- V(g_primschool)[c.m_sc==1]
v_comp2_sc <- V(g_primschool)[c.m_sc==2]
v_comp3_sc <- V(g_primschool)[c.m_sc==3]
v_comp4_sc <- V(g_primschool)[c.m_sc==4]
v_comp5_sc <- V(g_primschool)[c.m_sc==5]
v_comp6_sc <- V(g_primschool)[c.m_sc==6]
v_comp7_sc <- V(g_primschool)[c.m_sc==7]
v_comp8_sc <- V(g_primschool)[c.m_sc==8]

community.significance.test(g_primschool, v_comp1_sc)
community.significance.test(g_primschool, v_comp2_sc)
community.significance.test(g_primschool, v_comp3_sc)
community.significance.test(g_primschool, v_comp4_sc)
community.significance.test(g_primschool, v_comp5_sc)
community.significance.test(g_primschool, v_comp6_sc)
community.significance.test(g_primschool, v_comp7_sc)
community.significance.test(g_primschool, v_comp8_sc)

############## Label propagation ####################
school_comm_lp<-label.propagation.community(g_primschool, weights=E(g_primschool)$weight)
c.m_lp <- membership(school_comm_sc)
c.m_lp

table(c.m_lp, stud.class, useNA = c("no"))
table(c.m_lp, stud.gender, useNA = c("no"))
plot(school_comm_lp, g_primschool, vertex.label= NA, vertex.size=2)

v_comp1_lp <- V(g_primschool)[c.m_lp==1]
v_comp2_lp <- V(g_primschool)[c.m_lp==2]
v_comp3_lp <- V(g_primschool)[c.m_lp==3]
v_comp4_lp <- V(g_primschool)[c.m_lp==4]
v_comp5_lp <- V(g_primschool)[c.m_lp==5]
v_comp6_lp <- V(g_primschool)[c.m_lp==6]
v_comp7_lp <- V(g_primschool)[c.m_lp==7]
v_comp8_lp <- V(g_primschool)[c.m_lp==8]

community.significance.test(g_primschool, v_comp1_sc)
community.significance.test(g_primschool, v_comp2_sc)
community.significance.test(g_primschool, v_comp3_sc)
community.significance.test(g_primschool, v_comp4_sc)
community.significance.test(g_primschool, v_comp5_sc)
community.significance.test(g_primschool, v_comp6_sc)
community.significance.test(g_primschool, v_comp7_sc)
community.significance.test(g_primschool, v_comp8_sc)

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
school_comm_gn <- cluster_edge_betweenness(g_primschool, weights=E(g_primschool)$weight)
school_comm_gn
c.m_gn <- membership(school_comm_gn)
c.m_gn

table(c.m_gn, stud.class, useNA = c("no"))
table(c.m_gn, stud.gender, useNA = c("no"))

v_comp1_gn <- V(g_primschool)[c.m_gn==1]
v_comp2_gn <- V(g_primschool)[c.m_gn==2]
v_comp3_gn <- V(g_primschool)[c.m_gn==3]
v_comp4_gn <- V(g_primschool)[c.m_gn==4]
v_comp5_gn <- V(g_primschool)[c.m_gn==5]
v_comp6_gn <- V(g_primschool)[c.m_gn==6]
v_comp7_gn <- V(g_primschool)[c.m_gn==7]
v_comp8_gn <- V(g_primschool)[c.m_gn==8]
v_comp9_gn <- V(g_primschool)[c.m_gn==9]
v_comp10_gn <- V(g_primschool)[c.m_gn==10]
v_comp11_gn <- V(g_primschool)[c.m_gn==11]
v_comp12_gn <- V(g_primschool)[c.m_gn==12]
v_comp13_gn <- V(g_primschool)[c.m_gn==13]
v_comp14_gn <- V(g_primschool)[c.m_gn==14]
v_comp15_gn <- V(g_primschool)[c.m_gn==15]
v_comp16_gn <- V(g_primschool)[c.m_gn==16]
v_comp17_gn <- V(g_primschool)[c.m_gn==17]
v_comp18_gn <- V(g_primschool)[c.m_gn==18]
v_comp19_gn <- V(g_primschool)[c.m_gn==19]
v_comp20_gn <- V(g_primschool)[c.m_gn==20]
v_comp21_gn <- V(g_primschool)[c.m_gn==21]
v_comp22_gn <- V(g_primschool)[c.m_gn==22]
v_comp23_gn <- V(g_primschool)[c.m_gn==23]
v_comp24_gn <- V(g_primschool)[c.m_gn==24]
v_comp25_gn <- V(g_primschool)[c.m_gn==25]
v_comp26_gn <- V(g_primschool)[c.m_gn==26]
v_comp27_gn <- V(g_primschool)[c.m_gn==27]
v_comp28_gn <- V(g_primschool)[c.m_gn==28]
v_comp29_gn <- V(g_primschool)[c.m_gn==29]
v_comp30_gn <- V(g_primschool)[c.m_gn==30]
v_comp31_gn <- V(g_primschool)[c.m_gn==31]
v_comp32_gn <- V(g_primschool)[c.m_gn==32]
v_comp33_gn <- V(g_primschool)[c.m_gn==33]
v_comp34_gn <- V(g_primschool)[c.m_gn==34]
v_comp35_gn <- V(g_primschool)[c.m_gn==35]
v_comp36_gn <- V(g_primschool)[c.m_gn==36]
v_comp37_gn <- V(g_primschool)[c.m_gn==37]
v_comp38_gn <- V(g_primschool)[c.m_gn==38]
v_comp39_gn <- V(g_primschool)[c.m_gn==39]
v_comp40_gn <- V(g_primschool)[c.m_gn==40]
v_comp41_gn <- V(g_primschool)[c.m_gn==41]
v_comp42_gn <- V(g_primschool)[c.m_gn==42]
v_comp43_gn <- V(g_primschool)[c.m_gn==43]
v_comp44_gn <- V(g_primschool)[c.m_gn==44]
v_comp45_gn <- V(g_primschool)[c.m_gn==45]
v_comp46_gn <- V(g_primschool)[c.m_gn==46]
v_comp47_gn <- V(g_primschool)[c.m_gn==47]
v_comp48_gn <- V(g_primschool)[c.m_gn==48]
v_comp49_gn <- V(g_primschool)[c.m_gn==49]
v_comp50_gn <- V(g_primschool)[c.m_gn==50]
v_comp51_gn <- V(g_primschool)[c.m_gn==51]
v_comp52_gn <- V(g_primschool)[c.m_gn==52]
v_comp53_gn <- V(g_primschool)[c.m_gn==53]
v_comp54_gn <- V(g_primschool)[c.m_gn==54]
v_comp55_gn <- V(g_primschool)[c.m_gn==55]
v_comp56_gn <- V(g_primschool)[c.m_gn==56]
v_comp57_gn <- V(g_primschool)[c.m_gn==57]
v_comp58_gn <- V(g_primschool)[c.m_gn==58]
v_comp59_gn <- V(g_primschool)[c.m_gn==59]
v_comp60_gn <- V(g_primschool)[c.m_gn==60]
v_comp61_gn <- V(g_primschool)[c.m_gn==61]
v_comp62_gn <- V(g_primschool)[c.m_gn==62]
v_comp63_gn <- V(g_primschool)[c.m_gn==63]
v_comp64_gn <- V(g_primschool)[c.m_gn==64]
v_comp65_gn <- V(g_primschool)[c.m_gn==65]
v_comp66_gn <- V(g_primschool)[c.m_gn==66]
v_comp67_gn <- V(g_primschool)[c.m_gn==67]
v_comp68_gn <- V(g_primschool)[c.m_gn==68]
v_comp69_gn <- V(g_primschool)[c.m_gn==69]
v_comp70_gn <- V(g_primschool)[c.m_gn==70]
v_comp71_gn <- V(g_primschool)[c.m_gn==71]
v_comp72_gn <- V(g_primschool)[c.m_gn==72]
v_comp73_gn <- V(g_primschool)[c.m_gn==73]
v_comp74_gn <- V(g_primschool)[c.m_gn==74]
v_comp75_gn <- V(g_primschool)[c.m_gn==75]
v_comp76_gn <- V(g_primschool)[c.m_gn==76]
v_comp77_gn <- V(g_primschool)[c.m_gn==77]
v_comp78_gn <- V(g_primschool)[c.m_gn==78]
v_comp79_gn <- V(g_primschool)[c.m_gn==79]
v_comp80_gn <- V(g_primschool)[c.m_gn==80]
v_comp81_gn <- V(g_primschool)[c.m_gn==81]
v_comp82_gn <- V(g_primschool)[c.m_gn==82]
v_comp83_gn <- V(g_primschool)[c.m_gn==83]
v_comp84_gn <- V(g_primschool)[c.m_gn==84]
v_comp85_gn <- V(g_primschool)[c.m_gn==85]
v_comp86_gn <- V(g_primschool)[c.m_gn==86]
v_comp87_gn <- V(g_primschool)[c.m_gn==87]
v_comp88_gn <- V(g_primschool)[c.m_gn==88]
v_comp89_gn <- V(g_primschool)[c.m_gn==89]
v_comp90_gn <- V(g_primschool)[c.m_gn==90]
v_comp91_gn <- V(g_primschool)[c.m_gn==91]
v_comp92_gn <- V(g_primschool)[c.m_gn==92]
v_comp93_gn <- V(g_primschool)[c.m_gn==93]
v_comp94_gn <- V(g_primschool)[c.m_gn==94]
v_comp95_gn <- V(g_primschool)[c.m_gn==95]
v_comp96_gn <- V(g_primschool)[c.m_gn==96]
v_comp97_gn <- V(g_primschool)[c.m_gn==97]
v_comp98_gn <- V(g_primschool)[c.m_gn==98]
v_comp99_gn <- V(g_primschool)[c.m_gn==99]
v_comp100_gn <- V(g_primschool)[c.m_gn==100]

community.significance.test(g_primschool, v_comp1_gn)
community.significance.test(g_primschool, v_comp2_gn)
community.significance.test(g_primschool, v_comp3_gn)
community.significance.test(g_primschool, v_comp4_gn)
community.significance.test(g_primschool, v_comp5_gn)
community.significance.test(g_primschool, v_comp6_gn)
community.significance.test(g_primschool, v_comp7_gn)
community.significance.test(g_primschool, v_comp8_gn)
community.significance.test(g_primschool, v_comp9_gn)
community.significance.test(g_primschool, v_comp10_gn)
community.significance.test(g_primschool, v_comp11_gn)
community.significance.test(g_primschool, v_comp12_gn)
community.significance.test(g_primschool, v_comp13_gn)
community.significance.test(g_primschool, v_comp14_gn)
community.significance.test(g_primschool, v_comp15_gn)
community.significance.test(g_primschool, v_comp16_gn)
community.significance.test(g_primschool, v_comp17_gn)
community.significance.test(g_primschool, v_comp18_gn)
community.significance.test(g_primschool, v_comp19_gn)
community.significance.test(g_primschool, v_comp20_gn)
community.significance.test(g_primschool, v_comp21_gn)
community.significance.test(g_primschool, v_comp22_gn)
community.significance.test(g_primschool, v_comp23_gn)
community.significance.test(g_primschool, v_comp24_gn)
community.significance.test(g_primschool, v_comp25_gn)
community.significance.test(g_primschool, v_comp26_gn)
community.significance.test(g_primschool, v_comp27_gn)
community.significance.test(g_primschool, v_comp28_gn)
community.significance.test(g_primschool, v_comp29_gn)
community.significance.test(g_primschool, v_comp30_gn)
community.significance.test(g_primschool, v_comp31_gn)
community.significance.test(g_primschool, v_comp32_gn)
community.significance.test(g_primschool, v_comp33_gn)
community.significance.test(g_primschool, v_comp34_gn)
community.significance.test(g_primschool, v_comp35_gn)
community.significance.test(g_primschool, v_comp36_gn)
community.significance.test(g_primschool, v_comp37_gn)
community.significance.test(g_primschool, v_comp38_gn)
community.significance.test(g_primschool, v_comp39_gn)
community.significance.test(g_primschool, v_comp40_gn)
community.significance.test(g_primschool, v_comp41_gn)
community.significance.test(g_primschool, v_comp42_gn)
community.significance.test(g_primschool, v_comp43_gn)
community.significance.test(g_primschool, v_comp44_gn)
community.significance.test(g_primschool, v_comp45_gn)
community.significance.test(g_primschool, v_comp46_gn)
community.significance.test(g_primschool, v_comp47_gn)
community.significance.test(g_primschool, v_comp48_gn)
community.significance.test(g_primschool, v_comp49_gn)
community.significance.test(g_primschool, v_comp50_gn)
community.significance.test(g_primschool, v_comp51_gn)
community.significance.test(g_primschool, v_comp51_gn)
community.significance.test(g_primschool, v_comp52_gn)
community.significance.test(g_primschool, v_comp53_gn)
community.significance.test(g_primschool, v_comp54_gn)
community.significance.test(g_primschool, v_comp55_gn)
community.significance.test(g_primschool, v_comp56_gn)
community.significance.test(g_primschool, v_comp57_gn)
community.significance.test(g_primschool, v_comp58_gn)
community.significance.test(g_primschool, v_comp59_gn)
community.significance.test(g_primschool, v_comp60_gn)
community.significance.test(g_primschool, v_comp61_gn)
community.significance.test(g_primschool, v_comp61_gn)
community.significance.test(g_primschool, v_comp62_gn)
community.significance.test(g_primschool, v_comp63_gn)
community.significance.test(g_primschool, v_comp64_gn)
community.significance.test(g_primschool, v_comp65_gn)
community.significance.test(g_primschool, v_comp66_gn)
community.significance.test(g_primschool, v_comp67_gn)
community.significance.test(g_primschool, v_comp68_gn)
community.significance.test(g_primschool, v_comp69_gn)
community.significance.test(g_primschool, v_comp70_gn)
community.significance.test(g_primschool, v_comp71_gn)
community.significance.test(g_primschool, v_comp72_gn)
community.significance.test(g_primschool, v_comp73_gn)
community.significance.test(g_primschool, v_comp74_gn)
community.significance.test(g_primschool, v_comp75_gn)
community.significance.test(g_primschool, v_comp76_gn)
community.significance.test(g_primschool, v_comp77_gn)
community.significance.test(g_primschool, v_comp78_gn)
community.significance.test(g_primschool, v_comp79_gn)
community.significance.test(g_primschool, v_comp80_gn)
community.significance.test(g_primschool, v_comp81_gn)
community.significance.test(g_primschool, v_comp82_gn)
community.significance.test(g_primschool, v_comp83_gn)
community.significance.test(g_primschool, v_comp84_gn)
community.significance.test(g_primschool, v_comp85_gn)
community.significance.test(g_primschool, v_comp86_gn)
community.significance.test(g_primschool, v_comp87_gn)
community.significance.test(g_primschool, v_comp88_gn)
community.significance.test(g_primschool, v_comp89_gn)
community.significance.test(g_primschool, v_comp90_gn)
community.significance.test(g_primschool, v_comp91_gn)
community.significance.test(g_primschool, v_comp92_gn)
community.significance.test(g_primschool, v_comp93_gn)
community.significance.test(g_primschool, v_comp94_gn)
community.significance.test(g_primschool, v_comp95_gn)
community.significance.test(g_primschool, v_comp96_gn)
community.significance.test(g_primschool, v_comp97_gn)
community.significance.test(g_primschool, v_comp98_gn)
community.significance.test(g_primschool, v_comp99_gn)
community.significance.test(g_primschool, v_comp100_gn)

plot(school_comm_gn, g_primschool, vertex.label= NA, vertex.size=2)

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
