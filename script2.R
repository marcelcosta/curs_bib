library(wordcloud)
library(ggplot2)
library(igraph)
library(ggraph)
# setwd("~/Documents/curs_bib/") #This is for me
# We define a list of autoimmune diseases
diseases<-c("Allergies", "Alzheimer's", "Anxiety", "Panic", "Arthritis", "Breast",
            "Fatigue", "Crohn's", "Cystic", "Fibrosis", "Depression", "Diabetes", "Epilepsy",
            "Fibromyalgia", "GERD", "Reflux)", "Headaches", "Heartburn", "Hepatitis","Irritable", "Bowel",
            "Lupus", "Lyme", "Migraines", "Sclerosis", "Parkinson's", "Prostate", "Cancer")
diseases<-tolower(diseases)
# We introduce the data
table<-read.table("posts.txt", check.names = FALSE, sep="\n")

# We clean the character data (this can be improved)
cl_table<-apply(table, 1, function(x) gsub("\n", "", x))
cl_table<-sapply(cl_table, function(x) gsub("[0-9|{|}|(|)|*|+|.|?]", "", x))
cl_table<-sapply(cl_table, function(x) gsub("[\\[]", "", x))
cl_table<-sapply(cl_table, function(x) gsub("]", "", x))
cl_table<-sapply(cl_table, tolower) # This is to convert every letter to lower case

# We split the text by words
lcl_table<-sapply(cl_table, function(x) strsplit(x, split=" "))

# We select the words in the vector diseases
sel_table<-lapply(lcl_table, function(x) x[x %in% diseases])
sel_table_short<-sel_table[lapply(sel_table, length) > 0] #We remove the list entries (posts) without any word

# We keep only one appearence by post
sel_table_short<-sapply(sel_table_short, function(x) x[!duplicated(x)])

## Here we will represent individual word frequencies
# We join all appearence in a unique vector (we loose the information by post)
v_sel<-unlist(sel_table_short, use.names=FALSE)

# We summerize in a table each word frequency
df_vsel<-as.data.frame(sort(table(v_sel)))

# We represent this frequencies
ggplot(df_vsel, aes(v_sel, Freq))+geom_bar(stat="identity")+coord_flip()
wordcloud(df_vsel$v_sel, df_vsel$Freq, random.color = FALSE, colors=colorRampPalette(c("red", "green"))(200))


## Here we will represent connections of two words within single posts
# We will create an edgelist for every post and will concatenate them.
mat_adja<-matrix(nrow=0, ncol=2)
for (list in sel_table_short){
  if (length(list) > 1){
    adjacency_temp<-matrix(rep(1, length(list)), length(list), length(list), dimnames=list(list, list))
    mat_adja<-rbind(mat_adja,  get.edgelist(graph.adjacency(adjacency_temp)))
  }
}
# Here we will remove "self connections". Eg: "depression" connected to "depression".
mat_adja<-mat_adja[mat_adja[,1] != mat_adja[,2],]
# This plots the network, but is too complex to understand
plot(graph.data.frame(mat_adja), layout=layout.circle, main="circle")
# We save the igraph
mygraph<-graph.data.frame(mat_adja, directed = FALSE)
# We represent the igraph with the ggraph package in a circular form
ggraph(mygraph, layout = 'dendrogram', circular = TRUE) +
  geom_edge_diagonal() +
  theme_void()
