install.packages("tm")
install.packages("ggplot2")
install.packages("readr")
install.packages("NLP")
install.packages("SnowballC")

library(ggplot2)
library(readr)
library(NLP)
library(tm)
library(SnowballC)
library(igraph)
library(circlize)
library(wordcloud)

# setwd("~/R studio projects/UB project")
# setwd("~/Documents/curs_bib/") #This is for me

## 1. Corpus (loading data and creating the corpus)

# Load .txt to dataframe
table <- read.delim("posts.txt", sep="\n", quote = "", 
                    row.names = NULL, 
                    stringsAsFactors = FALSE)

# Load data as corpus
# VectorSource() creates character vectors
mydata <- Corpus(VectorSource(table[,1]))

## 2. Preprocessing DATA

# Convert to lower case
mydata <- tm_map(mydata, content_transformer(tolower))

# remove stopwords
mydata <- tm_map(mydata, removeWords, stopwords("english"))

# Remove punctuations and numbers
mydata <- tm_map(mydata, removePunctuation)
mydata <- tm_map(mydata, removeNumbers)

# Stemming (grouping terms with the same root)
mydata <- tm_map(mydata, stemDocument)

## Code trying to join the three dictionaries
## We generate the Document Term Matrix filtering with the Dictionary Diseases, defined manually
diseases<-tolower(c("Allergies", "Alzheimer", "Arthritis", "Bipolar", "Breast", "Celiac",
                     "Fatigue", "Crohn", "Fibrosis", "Thyroid", "Kidney", "Diabetes", "Epilepsy",
                     "Fibromyalgia", "GERD", "Headache", "Heart", "Bowel", "Ostomies", "Hepatitis", "Colitis",
                     "Lupus", "Lyme", "Migraine", "Sclerosis", "Parkinson", "Prostate", "Cancer", "Psoriasis", "Sjogren"))
side_effects<-tolower(as.character(unique(read.csv("meddra_all_se.tsv", sep = "\t")[,6])))
drugs<-tolower(as.character(unique(read.csv("drug_names.tsv", sep="\t")[,2])))
dictionaries<-data.frame("words"=stemDocument(tolower(c(diseases,drugs,side_effects))), "category"=c(rep("diseases",length(diseases)), rep("drugs",length(drugs)), rep("side_effects",length(side_effects))))

dtm_dis<-DocumentTermMatrix(mydata, list(dictionary= stemDocument(c(diseases, drugs, side_effects))))
# We use stemDocument() function to us the root of the words to filter, otherwise they won't coincide with mydata

mt_dtm_dis<-as.matrix(dtm_dis[,findFreqTerms(dtm_dis, 100)]) ## We have to filter for the words that have at least 100 occurrence because if not the size of the matrix is huge
mt_dtm_dis<-mt_dtm_dis[apply(mt_dtm_dis, 1, function(x) !all(x == 0)),] ## We remove the empty documents

# We calculate the number of appearences for every word (counting only one by document)
tmp<-sort(apply(mt_dtm_dis, 2, function(x) length(x[x > 0])), decreasing = T)
df_dtm_dis_freq<-data.frame("words"=names(tmp), "freq"=as.numeric(tmp))

# Ploting some frequency plots
ggplot(df_dtm_dis_freq[1:20,], aes(as.factor(words), freq))+geom_bar(stat="identity")+scale_x_discrete(limits=rev(df_dtm_dis_freq$words[1:20]))+coord_flip()
png("freq_diseases.png", res=900, units="px", width=3000, height = 2000)
ggplot(data=df_dtm_dis_freq[df_dtm_dis_freq$words %in% stemDocument(diseases),][1:10,], aes(as.factor(words), freq))+
  geom_bar(stat="identity", fill="aquamarine", color="black")+
  scale_x_discrete(limits=rev(df_dtm_dis_freq$words[df_dtm_dis_freq$words %in% stemDocument(diseases)][1:10]))+coord_flip()+
  labs(x="", y="Frequency")+
  theme_bw()
dev.off()
png("wordcloud_diseases.png", res=300, units="px", width=3000, height = 3000)
wordcloud(df_dtm_dis_freq$words[df_dtm_dis_freq$words %in% stemDocument(diseases)], df_dtm_dis_freq$freq[df_dtm_dis_freq$words %in% stemDocument(diseases)], 
          random.color = FALSE, colors=colorRampPalette(c("red", "aquamarine"))(200))
dev.off()

png("freq_drugs.png", res=900, units="px", width=3000, height = 2000)
ggplot(data=df_dtm_dis_freq[df_dtm_dis_freq$words %in% stemDocument(drugs),][1:10,], aes(as.factor(words), freq))+
  geom_bar(stat="identity", fill="aquamarine", color="black")+
  scale_x_discrete(limits=rev(df_dtm_dis_freq$words[df_dtm_dis_freq$words %in% stemDocument(drugs)][1:10]))+coord_flip()+
  labs(x="", y="Frequency")+
  theme_bw()
dev.off()
png("wordcloud_drugs.png", res=300, units="px", width=3000, height = 3000)
wordcloud(df_dtm_dis_freq$words[df_dtm_dis_freq$words %in% stemDocument(drugs)], df_dtm_dis_freq$freq[df_dtm_dis_freq$words %in% stemDocument(drugs)], 
          random.color = FALSE, colors=colorRampPalette(c("red", "aquamarine"))(200))
dev.off()

png("freq_symptoms.png", res=900, units="px", width=3000, height = 2000)
ggplot(data=df_dtm_dis_freq[df_dtm_dis_freq$words %in% stemDocument(side_effects),][1:10,], aes(as.factor(words), freq))+
  geom_bar(stat="identity", fill="aquamarine", color="black")+
  scale_x_discrete(limits=rev(df_dtm_dis_freq$words[df_dtm_dis_freq$words %in% stemDocument(side_effects)][1:10]))+coord_flip()+
  labs(x="", y="Frequency")+
  theme_bw()
dev.off()
png("wordcloud_symptoms.png", res=300, units="px", width=3000, height = 3000)
wordcloud(df_dtm_dis_freq$words[df_dtm_dis_freq$words %in% stemDocument(side_effects)][1:100], df_dtm_dis_freq$freq[df_dtm_dis_freq$words %in% stemDocument(side_effects)][1:100], 
          random.color = FALSE, colors=colorRampPalette(c("red", "aquamarine"))(200))
dev.off()


## Create a network from DTM
# We create a weighted edge list
tdm.matrix<-t(mt_dtm_dis)
tdm.matrix[tdm.matrix>=1] <- 1
tdm.matrix <- tdm.matrix %*% t(tdm.matrix)
mat_adja <- as.data.frame(as.table(tdm.matrix))

# We remove self connections and connections without any ocurrence
mat_adja<-mat_adja[mat_adja[,1] != mat_adja[,2],]
mat_adja<-mat_adja[mat_adja[,3] > 0,]

# We remove the duplicated connections (for as both directions are the same: A->B vs B->A)
mat_adja<-mat_adja[duplicated(
  lapply(1:nrow(mat_adja), function(y){
    A <- mat_adja[y, ]
    as.vector(unlist(A[order(A)]))
  })),]

# We remove connections between words from the same dictionary.
mat_adja<-mat_adja[unlist(lapply(1:nrow(mat_adja), function(y){
  dictionaries[mat_adja[y,1],2] != dictionaries[mat_adja[y,2],2]
})),]

# We make a vector with the 10 words of each dictionary that appear most
topcategories<-as.character(df_dtm_dis_freq$words[df_dtm_dis_freq$words %in% stemDocument(tolower(as.character(diseases)))][1:10])
topcategories<-append(topcategories,as.character(df_dtm_dis_freq$words[df_dtm_dis_freq$words %in% stemDocument(tolower(as.character(drugs)))][1:10]))
topcategories<-append(topcategories,as.character(df_dtm_dis_freq$words[df_dtm_dis_freq$words %in% stemDocument(tolower(as.character(side_effects)))][1:10]))

# We create the network graphic from the edge list and we add the weight of each edge
g=graph.edgelist(as.matrix(mat_adja[,1:2]), directed = FALSE)
E(g)$weight=as.numeric(mat_adja[,3])

# We exctract the adjacency matrix and we plot it with chordDiagrasm
adj_graph<-get.adjacency(g, attr='weight', type="lower")
mat<-as.matrix(adj_graph)

# We select the words previosly stored in the "topcategories" vector
mat<-mat[rownames(mat) %in% topcategories,colnames(mat) %in% topcategories]

# All this commands are to sort the words by dictionary and to assign colors to each one that will be used in the chordDiagram
mat_words<-union(colnames(mat), rownames(mat))
tmp_order<-c(which(mat_words %in% stemDocument(tolower(diseases))), which(mat_words %in% stemDocument(tolower(drugs))), which(mat_words %in% stemDocument(tolower(side_effects))))
tmp_color<-vector(length=length(mat_words))
tmp_color[which(mat_words %in% stemDocument(tolower(side_effects)))]<-"IndianRed"
tmp_color[which(mat_words %in% stemDocument(tolower(drugs)))]<-"blue4"
tmp_color[which(mat_words %in% stemDocument(tolower(diseases)))]<-"aquamarine"
mat_words<-mat_words[tmp_order[!duplicated(tmp_order)]]
grid.col<-tmp_color[tmp_order[!duplicated(tmp_order)]]

# Ploting the chordDiagram
png("chordDiagram_top10_dictionaries.png", res=300, units = "px", width = 2000, height = 2000)
chordDiagram(mat, order=mat_words, annotationTrack = "grid", preAllocateTracks = 1, directional = F, grid.col = grid.col)
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1] + .1, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
  circos.axis(h = "top", labels.cex = 0.5, major.tick.percentage = 0.2, sector.index = sector.name, track.index = 2)
}, bg.border = NA)
dev.off()

##Sentimental analysis

library(syuzhet)

mydataCopy <- mydata
#carryout sentiment mining using the get_nrc_sentiment()function log the findings under a variable result
result <- get_nrc_sentiment(as.character(mydataCopy))

#change result from a list to a dataframe and transpose it 
result1<-data.frame(t(result))

#rowSums computes column sums across rows for each level of a grouping variable.
new_result <- data.frame(rowSums(result1))

#name rows and columns of the dataframe
names(new_result)[1] <- "count"
new_result <- cbind("sentiment" = rownames(new_result), new_result)
rownames(new_result) <- NULL

#plot the first 8 rows, distict emotions
qplot(sentiment, data=new_result[1:8,], weight=count, geom="bar",fill=sentiment)+ggtitle("TedTalk Sentiments")+
  theme_bw()+
  theme(axis.text.x = element_text(angle=45, hjust=1))

#plot the positive and negative
qplot(sentiment, data=new_result[9:10,], weight=count, geom="bar",fill=sentiment)+ggtitle("NRC Emoticon Lexicon")+
  theme_bw()

