install.packages("tm")
install.packages("ggplot2")
install.packages("readr")
install.packages("NLP")
install.packages("SnowballC")
install.packages("circlize")
install.packages("wordcloud")
library(ggplot2)
library(readr)
library(NLP)
library(tm)
library(SnowballC)
library(igraph)
library(circlize)
library(wordcloud)

##setwd("~/R studio projects/UB project")
setwd("I:\LauroLinuxCompartit\posgrau data science 2018\treball")

## 1. Corpus (loading data and creating the corpus)

# Load .txt to dataframe
# table <- read.delim("~/R studio projects/UB project/posts.txt", sep="\n")
table <- read.delim("file:///I:/LauroLinuxCompartit/posgrau data science 2018/treball/curs_bib-master_MCosta_180718/curs_bib-master/posts.txt", sep="\n", quote = "", 
                    row.names = NULL, 
                    stringsAsFactors = FALSE)

# A solucionar un error "Warning message: In scan(file = file, what = what, sep = sep, quote = quote, dec = dec, :EOF within quoted string"
# !! Con read.table da 3765 obs (vs. 7167 read.delim)
# Marcel: Solucionado! Ahora da 10822 sin error.

# Load data as corpus
# VectorSource() creates character vectors
mydata <- Corpus(VectorSource(table[,1]))
mydata[[1]]$content
summary(mydata)
##inspect(mydata)
# S?lo salen n?meros como contenido!!!

## 2. Preprocessing DATA

# Convert to lower case
mydata <- tm_map(mydata, content_transformer(tolower))
#Error de "tm_map : transformation drops documents". See https://stackoverflow.com/questions/51081415/transformation-drops-documents-error-in-r 
## Marcel: no vi que ya lo habÃ­as reportado y lleguÃ© al mismo artÃ­culo de stackoverflow :).

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
wordcloud(df_dtm_freq$words[1:199], df_dtm_freq$freq[1:199], random.color = FALSE, colors=colorRampPalette(c("red", "green"))(200))


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
wordcloud(df_dtm_dis_freq$words, df_dtm_dis_freq$freq, random.color = FALSE, colors=colorRampPalette(c("red", "green"))(28))

## Create a network from DTM
# We create a weighted edge list
tdm.matrix<-t(as.matrix(dtm_dis))
tdm.matrix[tdm.matrix>=1] <- 1
tdm.matrix <- tdm.matrix %*% t(tdm.matrix)
mat_adja <- as.data.frame(as.table(tdm.matrix))

# We remove self connections and connections without any ocurrence
mat_adja<-mat_adja[mat_adja[,1] != mat_adja[,2],]
mat_adja<-mat_adja[mat_adja[,3] > 0,]

# We create the network graphic from the edge list
g=graph.edgelist(as.matrix(mat_adja[,1:2]), directed = FALSE)
E(g)$weight=as.numeric(mat_adja[,3])

# We exctract the adjacency matrix and we plot it with chordDiagram
adj_graph<-get.adjacency(g, attr='weight', type="lower")
chordDiagram(as.matrix(adj_graph), transparency = 0.5)

# We save a plot using igraph specifying edge size by its weight # This commands are to plot the png, are not suitable for visualizing inside Rstudio, as letters are very big.
png("test_nw.png", res=300, units="px", width=10000, height = 10000)
plot(g, edge.width=E(g)$weight/5, layout=layout_with_kk, edge.color="black", 
     vertex.size=igraph::degree(g, mode="all")/2, rescale=T, vertex.label.cex=4,
     vertex.color="#ffffcc", vertex.frame.cex=10)
dev.off()

#we get dictionaries of drugs and side effects from http://sideeffects.embl.de/
drugs.table <- read.delim("file:///I:/LauroLinuxCompartit/posgrau data science 2018/treball/extres per treball/drug_names.tsv", sep="\t", quote = "", 
                    row.names = NULL, 
                    stringsAsFactors = FALSE)

drugs<-tolower(unique(drugs.table[,2]))
dtm_drg<-DocumentTermMatrix(mydata, list(dictionary= stemDocument(drugs)))
# We extract the whole matrix of appearence
mt_dtm_drg<-as.matrix(dtm_drg)
# We calculate the number of appearences for every word (counting only one by document)
tmp<-sort(apply(mt_dtm_drg, 2, function(x) length(x[x > 0])), decreasing = T)
df_dtm_drg_freq<-data.frame("words"=names(tmp), "freq"=as.numeric(tmp))

# Representing sorted frequency
ggplot(df_dtm_drg_freq, aes(as.factor(words), freq))+geom_bar(stat="identity")+scale_x_discrete(limits=rev(df_dtm_drg_freq$words))+coord_flip()
wordcloud(df_dtm_drg_freq$words, df_dtm_drg_freq$freq, random.color = FALSE, colors=colorRampPalette(c("red", "green"))(28))

#same for side effects
sidef.table <- read.delim("file:///I:/LauroLinuxCompartit/posgrau data science 2018/treball/extres per treball/meddra_all_se.tsv", sep="\t", quote = "", 
                          row.names = NULL, 
                          stringsAsFactors = FALSE)

sidef<-tolower(unique(sidef.table[,6]))
dtm_sef<-DocumentTermMatrix(mydata, list(dictionary= stemDocument(sidef)))
# We extract the whole matrix of appearence
mt_dtm_sef<-as.matrix(dtm_sef)
# We calculate the number of appearences for every word (counting only one by document)
tmp<-sort(apply(mt_dtm_sef, 2, function(x) length(x[x > 0])), decreasing = T)
df_dtm_sef_freq<-data.frame("words"=names(tmp), "freq"=as.numeric(tmp))

# Representing sorted frequency
ggplot(df_dtm_sef_freq, aes(as.factor(words), freq))+geom_bar(stat="identity")+scale_x_discrete(limits=rev(df_dtm_sef_freq$words))+coord_flip()
wordcloud(df_dtm_sef_freq$words, df_dtm_sef_freq$freq, random.color = FALSE, colors=colorRampPalette(c("red", "green"))(200))
wordcloud(df_dtm_sef_freq$words[df_dtm_sef_freq$freq>20], df_dtm_sef_freq$freq[df_dtm_sef_freq$freq>20], random.color = FALSE, colors=colorRampPalette(c("red", "green"))(200))

#explore co-occurrence of disease + drugs + side effects
# by clustering???
hclust(mt_)




> sessionInfo()
R version 3.5.0 (2018-04-23)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 7 x64 (build 7601) Service Pack 1

Matrix products: default

locale:
  [1] LC_COLLATE=Spanish_Spain.1252  LC_CTYPE=Spanish_Spain.1252   
[3] LC_MONETARY=Spanish_Spain.1252 LC_NUMERIC=C                  
[5] LC_TIME=Spanish_Spain.1252    

attached base packages:
  [1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
  [1] wordcloud_2.5      RColorBrewer_1.1-2 tm_0.7-4           circlize_0.4.4    
[5] SnowballC_0.5.1    NLP_0.1-11         igraph_1.2.1       readr_1.1.1       
[9] ggplot2_2.2.1     

loaded via a namespace (and not attached):
  [1] Rcpp_0.12.16        xml2_1.2.0          magrittr_1.5        hms_0.4.2          
[5] munsell_0.4.3       lattice_0.20-35     colorspace_1.3-2    R6_2.2.2           
[9] rlang_0.2.0         plyr_1.8.4          tools_3.5.0         parallel_3.5.0     
[13] grid_3.5.0          gtable_0.2.0        yaml_2.1.19         lazyeval_0.2.1     
[17] tibble_1.4.2        Matrix_1.2-14       GlobalOptions_0.1.0 shape_1.4.4        
[21] slam_0.1-43         labeling_0.3        compiler_3.5.0      pillar_1.2.2       
[25] scales_0.5.0        pkgconfig_2.0.1    
