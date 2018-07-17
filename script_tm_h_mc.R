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

setwd("~/R studio projects/UB project")

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
inspect(mydata)
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
ggplot(df_dtm_dis_freq, aes(as.factor(words), freq))+geom_bar(stat="identity")+scale_x_discrete(limits=rev(df_dtm_dis_freq$words))+coord_flip()

library(wordcloud)
wordcloud(df_dtm_dis_freq$words, df_dtm_dis_freq$freq, random.color = FALSE, colors=colorRampPalette(c("red", "green"))(200))

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
chordDiagram(as.matrix(adj_graph), transparency = 0.5)

# We save a plot using igraph specifying edge size by its weight # This commands are to plot the png, are not suitable for visualizing inside Rstudio, as letters are very big.
png("test_nw.png", res=300, units="px", width=10000, height = 10000)
plot(g, edge.width=E(g)$weight/5, layout=layout_with_kk, edge.color="black", 
     vertex.size=igraph::degree(g, mode="all")/2, rescale=T, vertex.label.cex=4,
     vertex.color="#ffffcc", vertex.frame.cex=10)
dev.off()

