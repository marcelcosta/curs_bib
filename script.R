# We define a list of autoimmune diseases
diseases<-c("Allergies", "Alzheimer's Disease", "Anxiety - Panic Disorders", "Arthritis", "Breast Cancer",
            "Chronic Fatigue Syndrome", "Crohn's Disease", "Cystic Fibrosis", "Depression", "Diabetes", "Epilepsy",
            "Fibromyalgia", "GERD (Acid Reflux)", "Headaches", "Heartburn", "Hepatitis","Irritable Bowel Syndrome",
            "Lupus", "Lyme Disease", "Migraines", "Multiple Sclerosis", "Parkinson's Disease", "Prostate Cancer")

# We introduce the data
table<-read.table("posts.txt", check.names = FALSE, sep="\n")
# num<-1000
# list<-strsplit(as.character(table[1:num,1]),split=" ")
# df<-as.data.frame(matrix(nrow=max(sapply(list,length)),ncol=num))
# for (elem in 1:num){df[1:length(list[[elem]]),elem]<-as.vector(list[[elem]])}
# v<-c()
# for (elem in 1:num){v<-append(list[[elem]],v)}
# sort(table(v))

# We convert table variable into a one character string. This is just to explore as we will want to mantain posts separaterly.
str<-paste(as.character(table[,1]), collapse="")

# We clean a bit the text
str<-gsub("\n", "", str)
str<-gsub("[0-9|{|}|(|)|*|+|.|?]", "", str)
str<-gsub("[\\[]", "", str)
str<-gsub("]", "", str)

# We split the text by words
strsp<-strsplit(str, split=" ")

# We summarize words by frequency
t_strsp<-table(strsp)

# We sort the data
df<-as.data.frame(sort(t_strsp, decreasing=TRUE))
# df<-as.data.frame(head(sort(t_strsp, decreasing=TRUE), n=300))

# We convert it to factor
df$strsp<-as.factor(df$strsp)

# We select only words that appear in the vector "diseases"
df2<-as.data.frame(matrix(ncol=2, nrow=0))
for (num in 1:length(df$strsp)){
  if (length(grep(paste("\\<",df$strsp[num],"\\>", sep=""), diseases, ignore.case=TRUE))>0){
    print(df$strsp[num])
    df2<-rbind(df2, df[num,])
  }
}
colnames(df2)<-colnames(df)
df2$strsp<-as.factor(df2$strsp)

df3<-df2[length(grep(df$strsp, dict$Title, ignore.case=TRUE)) > 0,]

wordcloud(df2$strsp, df2$Freq, random.color = FALSE, colors=colorRampPalette(c("red", "green"))(200))
ggplot(df2, aes(strsp, Freq))+geom_bar(stat="identity")+coord_flip()+scale_x_discrete(limits=rev(df2$strsp))

