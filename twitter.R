library(SocialMediaLab)
library(magrittr)
library(twitteR)
library(igraph)
library(network)
library(networkD3)
######Sentiment Analysis ###
library(tidytext)
library(wordcloud)
library(syuzhet)
library(ggplot2)
library(tm)
library(reshape2)
library(RWeka)


myapikey <- "XXXXX6X2aMv4OxPc9gY5YsXXXX"
myapisecret <- "jXXXXWGbntvtnnwdRh18VlTXFr3W2fSWJLEqeLrf2XXXXXXX"
myaccesstoken <- "48570XXXX-XXXXXDAs8aRzOe2c6XvOZypyphj6p8WBsUTcTvx" 
myaccesstokensecret <- "XXXXXPixZuWxYA1Xy3AMIZTJZWOMCzWVQ9XXXXX" 

myTwitterData <- SocialMediaLab::Authenticate("twitter",
                              apiKey=myapikey,
                              apiSecret=myapisecret,
                              accessToken=myaccesstoken,
                              accessTokenSecret=myaccesstokensecret) %>%
  SocialMediaLab::Collect(searchTerm=c("#Namo"), numTweets=150, writeToFile=FALSE, verbose=TRUE)

View(myTwitterData)
txt=myTwitterData$text
write.csv(txt,"Text.csv")
View(txt)
#sentiment Analysis
mysentiment <- get_nrc_sentiment(myTwitterData$text)
SentimentScores <- data.frame(colSums(mysentiment[,]))
SentimentScores
names(SentimentScores) <- "Score"
SentimentScores <- cbind("sentiment" = rownames(SentimentScores), SentimentScores)
rownames(SentimentScores) <- NULL
#dev.off()
ggplot(data = SentimentScores, aes(x = sentiment, y = Score)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Score") + ggtitle("Total Sentiment Score Based Tweet")

############################
myTwitterData$text <- iconv(myTwitterData$text, to = 'utf-8')

g_twitter_actor <- myTwitterData %>% Create("Actor")
plot(g_twitter_actor)
g_twitter_semantic <- myTwitterData %>% Create("Semantic")
plot(g_twitter_semantic)
g_bimodal_twitter <- myTwitterData %>% Create("Bimodal")
plot(g_bimodal_twitter)
#Storing your social network in graphml format
write.graph(g_bimodal_twitter, "g_bimodal_twitter.graphml", format="graphml")

## Social Network Analysis
#Basic counting
#Numberof nodes
vcount(g_bimodal_twitter)

#Number of edges
ecount(g_bimodal_twitter)

# List of nodes in the network
V(g_bimodal_twitter)

# List of edges in the network
E(g_bimodal_twitter)

# Access to particular node in the network
V(g_bimodal_twitter)[42]

# Access the particular edge in the network
E(g_bimodal_twitter)[1]



## Graph connectivity

# who are the neighbours of node #42?
neighbors(g_bimodal_twitter,42)

#this is not a weakly connected component
is.connected(g_bimodal_twitter, mode="weak")

#information on connected components
cc <- clusters(g_bimodal_twitter)
#which component node is assigned to
# cc$membership
#size of each component
cc$csize

#number of components
cc$no


#subnetwork - giant component
g3 <- induced_subgraph(g_bimodal_twitter, which(cc$membership == which.max(cc$csize)))
#node indegree
degree(g3, mode="in")


#node outdegree
degree(g3, mode="out")


#node indegree, using edge weights
ind <- strength(g3, mode="in")

#top-5 nodes, based on (weighted) indegree
V(g3)[order(ind, decreasing=T)[1:3]]

#closeness centrality
closeness(g3)

#betweenness centrality
betweenness(g3)

#eigenvector centrality
evcent(g3)$vector

# density
graph.density(g3)

# (global) clustering coefficient
# rel. frequency connected triples close to form triangles
transitivity(g3)

# number of dyads with reciprocated (mutual)
# edges/number of dyads with single edge
reciprocity(g3, mode="default")


#total number of reciprocated edges/total number of edges
reciprocity(g3, mode="ratio")


# Find important nodes in the network
pagerank_instagram <- sort(page.rank(g_bimodal_twitter)$vector,decreasing=TRUE)
head(pagerank_instagram,n=3)

# page rank
#  top 10 important terms 
pageRank_semantic <- sort(page.rank(g_twitter_semantic)$vector,decreasing=TRUE)
head(pageRank_semantic,n=10)
tail(pageRank_semantic,n=3)
