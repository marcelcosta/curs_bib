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
library(icd)


# setwd("~/R studio projects/UB project")
# setwd("~/Documents/curs_bib/") #This is for me
## 1. Corpus (loading data and creating the corpus)

# Load .txt to dataframe
# table <- read.delim("~/R studio projects/UB project/posts.txt", sep="\n")
table <- read.delim("posts.txt", sep="\n", quote = "", 
                    row.names = NULL, 
                    stringsAsFactors = FALSE)

# A solucionar un error "Warning message: In scan(file = file, what = what, sep = sep, quote = quote, dec = dec, :EOF within quoted string"
# !! Con read.table da 3765 obs (vs. 7167 read.delim)
# Marcel: Solucionado! Ahora da 10822 sin error.

# Load data as corpus
# VectorSource() creates character vectors
mydata <- Corpus(VectorSource(table[,1]))
mydata[[1]]$content
# inspect(mydata)
# S?lo salen n?meros como contenido!!!

## 2. Preprocessing DATA

# Convert to lower case
mydata <- tm_map(mydata, content_transformer(tolower))
#Error de "tm_map : transformation drops documents". See https://stackoverflow.com/questions/51081415/transformation-drops-documents-error-in-r 
## Marcel: no vi que ya lo habías reportado y llegué al mismo artículo de stackoverflow :).

# remove stopwords
mydata <- tm_map(mydata, removeWords, stopwords("english"))
# Connection error 1st time: "In read.dcf(file.path(p, "DESCRIPTION"), c("Package", "Version")) : cannot open compressed file 'C:/Program Files/R/R-3.5.0/library/tm/DESCRIPTION', probable reason 'No such file or directory'"
# Solved deactivating in Tools->General: https://support.rstudio.com/hc/en-us/community/posts/200522573-Can-t-install-packages

# Remove punctuations and numbers
mydata <- tm_map(mydata, removePunctuation)
mydata <- tm_map(mydata, removeNumbers)

# Stemming (grouping terms with the same root)
mydata <- tm_map(mydata, stemDocument)

# Document Term Matrix
dtm <- DocumentTermMatrix(mydata)
inspect(dtm)

mt_dtm<-as.matrix(dtm)
tmp_2<-sort(apply(mt_dtm, 2, function(x) length(x[x > 0])), decreasing = T)
df_dtm_freq<-data.frame("words"=names(tmp_2), "freq"=as.numeric(tmp_2))
ggplot(df_dtm_freq[1:100,], aes(as.factor(words), freq))+geom_bar(stat="identity")+scale_x_discrete(limits=rev(df_dtm_freq[1:100,]$words))+coord_flip()
wordcloud(df_vsel$v_sel, df_vsel$Freq, random.color = FALSE, colors=colorRampPalette(c("red", "green"))(200))

## Marcel:
## We generate the Document Term Matrix filtering with the Dictionary Diseases, defined manually
diseases<-tolower(c("Allergies", "Alzheimer's", "Anxiety", "Panic", "Arthritis", "Breast",
            "Fatigue", "Crohn's", "Cystic", "Fibrosis", "Depression", "Diabetes", "Epilepsy",
            "Fibromyalgia", "GERD", "Reflux)", "Headaches", "Heartburn", "Hepatitis","Irritable", "Bowel",
            "Lupus", "Lyme", "Migraines", "Sclerosis", "Parkinson's", "Prostate", "Cancer"))
dtm_dis<-DocumentTermMatrix(mydata, list(dictionary= stemDocument(diseases)))
# We use stemDocument() function to us the root of the words to filter, otherwise they won't coincide with mydata

# We extract the whole matrix of appearence
mt_dtm_dis<-as.matrix(dtm_dis)

# We calculate the number of appearences for every word (counting only one by document)
tmp<-sort(apply(mt_dtm_dis, 2, function(x) length(x[x > 0])), decreasing = T)
df_dtm_dis_freq<-data.frame("words"=names(tmp), "freq"=as.numeric(tmp))

# Representing sorted frequency
ggplot(df_dtm_dis_freq, aes(as.factor(words), freq))+geom_bar(stat="identity", fill="#66cdaa", color="black")+scale_x_discrete(limits=rev(df_dtm_dis_freq$words))+coord_flip()+
  labs(y="Frequency", x="words")+theme_bw()
wordcloud(df_dtm_dis_freq$words, df_dtm_dis_freq$freq, random.color = FALSE, colors=colorRampPalette(c("red", "blue"))(200))

## Create a network from DTM
# We create a weighted edge list
tdm.matrix<-t(as.matrix(dtm_dis))
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

# We create the network graphic from the edge list and we add the weight of each edge
g=graph.edgelist(as.matrix(mat_adja[,1:2]), directed = FALSE)
E(g)$weight=as.numeric(mat_adja[,3])

# We exctract the adjacency matrix and we plot it with chordDiagrasm
adj_graph<-get.adjacency(g, attr='weight', type="lower")
mat<-as.matrix(adj_graph)
chordDiagram(mat, annotationTrack = "grid", preAllocateTracks = 1, directional = F) #, grid.col = grid.col)
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1] + .1, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
  circos.axis(h = "top", labels.cex = 0.5, major.tick.percentage = 0.2, sector.index = sector.name, track.index = 2)
}, bg.border = NA)

# We save a plot using igraph specifying edge size by its weight # This commands are to plot the png, are not suitable for visualizing inside Rstudio, as letters are very big.
png("test_nw.png", res=300, units="px", width=10000, height = 10000)
plot(g, edge.width=E(g)$weight/50, layout=layout_with_drl, edge.color="black", 
     vertex.size=igraph::degree(g, mode="all")/2, rescale=T, vertex.label.cex=4,
     vertex.color="#ffffcc", vertex.frame.cex=10)
dev.off()

png("test_nw_alllayouts.png", res=96, units="px", width=30000, height = 30000)
par(mfrow=c(4,3))
for (layout in c(layout_nicely, layout_with_dh, layout_with_drl, layout_with_kk, layout_with_fr, layout_with_gem, layout_with_graphopt, layout_with_lgl, layout_with_mds, layout_with_sugiyama)){
  plot(g, edge.width=E(g)$weight/50, layout=layout, edge.color="black", 
       vertex.size=igraph::degree(g, mode="all")/2, rescale=T, vertex.label.cex=4,
       vertex.color="#ffffcc", vertex.frame.cex=10)
}
dev.off()

## I did the same but using the icd9 disease classification as dictionary
icd9<-tolower(unique(icd9cm_hierarchy$major))
dtm_dis<-DocumentTermMatrix(mydata, list(dictionary= stemDocument(icd9), minDocFreq = 50))
# We extract the whole matrix of appearence
mt_dtm_dis<-as.matrix(dtm_dis[,findFreqTerms(dtm_dis, 1)]) ## I have to filter for the words that have at least 1 occurrence because if not the size of the matrix was huge
mt_dtm_dis<-mt_dtm_dis[apply(mt_dtm_dis, 1, function(x) !all(x == 0)),] ## I remove the empty documents


# We calculate the number of appearences for every word (counting only one by document)
tmp<-sort(apply(mt_dtm_dis, 2, function(x) length(x[x > 0])), decreasing = T)
df_dtm_dis_freq<-data.frame("words"=names(tmp), "freq"=as.numeric(tmp))

# Representing sorted frequency
ggplot(df_dtm_dis_freq, aes(as.factor(words), freq))+geom_bar(stat="identity")+scale_x_discrete(limits=rev(df_dtm_dis_freq$words))+coord_flip()
wordcloud(df_dtm_dis_freq$words, df_dtm_dis_freq$freq, random.color = FALSE, colors=colorRampPalette(c("red", "green"))(200))

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

# We create the network graphic from the edge list and we add the weight of each edge
g=graph.edgelist(as.matrix(mat_adja[,1:2]), directed = FALSE)
E(g)$weight=as.numeric(mat_adja[,3])

# We exctract the adjacency matrix and we plot it with chordDiagrasm
mat<-as.matrix(adj_graph)
adj_graph<-get.adjacency(g, attr='weight', type="lower")
chordDiagram(as.matrix(adj_graph), transparency = 0.5)
chordDiagram(mat, annotationTrack = "grid", preAllocateTracks = 1, directional = F) #, grid.col = grid.col)
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1] + .1, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
  circos.axis(h = "top", labels.cex = 0.5, major.tick.percentage = 0.2, sector.index = sector.name, track.index = 2)
}, bg.border = NA)

## Code trying to join the three dictionaries
## We generate the Document Term Matrix filtering with the Dictionary Diseases, defined manually
diseases<-tolower(c("Allergies", "Alzheimer's", "Anxiety", "Panic", "Arthritis", "Breast",
                    "Fatigue", "Crohn's", "Cystic", "Fibrosis", "Depression", "Diabetes", "Epilepsy",
                    "Fibromyalgia", "GERD", "Reflux)", "Headaches", "Heartburn", "Hepatitis","Irritable", "Bowel",
                    "Lupus", "Lyme", "Migraines", "Sclerosis", "Parkinson's", "Prostate", "Cancer"))
side_effects<-tolower(as.character(unique(read.csv("meddra_all_se.tsv", sep = "\t")[,6])))
drugs<-tolower(as.character(unique(read.csv("drug_names.tsv", sep="\t")[,2])))
dictionaries<-data.frame("words"=stemDocument(tolower(c(diseases,drugs,side_effects))), "category"=c(rep("diseases",length(diseases)), rep("drugs",length(drugs)), rep("side_effects",length(side_effects))))

dtm_dis<-DocumentTermMatrix(mydata, list(dictionary= stemDocument(c(diseases, drugs, side_effects))))
# We use stemDocument() function to us the root of the words to filter, otherwise they won't coincide with mydata

mt_dtm_dis<-as.matrix(dtm_dis[,findFreqTerms(dtm_dis, 100)]) ## I have to filter for the words that have at least 100 occurrence because if not the size of the matrix was huge
mt_dtm_dis<-mt_dtm_dis[apply(mt_dtm_dis, 1, function(x) !all(x == 0)),] ## I remove the empty documents

# We calculate the number of appearences for every word (counting only one by document)
tmp<-sort(apply(mt_dtm_dis, 2, function(x) length(x[x > 0])), decreasing = T)
df_dtm_dis_freq<-data.frame("words"=names(tmp), "freq"=as.numeric(tmp))

# Ploting some simple plots
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

ggplot(data=df_dtm_dis_freq[df_dtm_dis_freq$words %in% stemDocument(diseases),][1:50,], aes(as.factor(words), freq))+
  geom_bar(stat="identity")+
  scale_x_discrete(limits=rev(df_dtm_dis_freq$words[df_dtm_dis_freq$words %in% stemDocument(diseases)][1:50]))+coord_flip()
wordcloud(df_dtm_dis_freq$words[1:200], df_dtm_dis_freq$freq[1:200], random.color = FALSE, colors=colorRampPalette(c("red", "green"))(200))


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

# Different ways to select the connectors
# mat<-mat[,colnames(mat) %in% df_dtm_dis_freq$words[df_dtm_dis_freq$words %in% stemDocument(drugs)][1:20]]
# mat<-mat[rownames(mat) %in% df_dtm_dis_freq$words[1:30],colnames(mat) %in% df_dtm_dis_freq$words[1:30]]
# This is one selects the words previosly stored in the "topcategories" vector
mat<-mat[rownames(mat) %in% topcategories,colnames(mat) %in% topcategories]

# All this is to sort the words by dictionary and to assign colors to each one that will be used in the chordDiagram
mat_words<-union(colnames(mat), rownames(mat))
tmp_order<-c(which(mat_words %in% stemDocument(tolower(diseases))), which(mat_words %in% stemDocument(tolower(drugs))), which(mat_words %in% stemDocument(tolower(side_effects))))
tmp_color<-vector(length=length(mat_words))
tmp_color[which(mat_words %in% stemDocument(tolower(side_effects)))]<-"IndianRed"
tmp_color[which(mat_words %in% stemDocument(tolower(drugs)))]<-"blue4"
tmp_color[which(mat_words %in% stemDocument(tolower(diseases)))]<-"aquamarine"
# tmp_color<-c(rep("green",length(mat_words[mat_words %in% stemDocument(tolower(diseases))])), rep("blue",length(mat_words[mat_words %in% stemDocument(tolower(drugs))])), rep("red",length(mat_words[mat_words %in% stemDocument(tolower(side_effects))])))
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
