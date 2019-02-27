rm(list=ls()) 
#install.packages("dplyr")
#install.packages("stringr")
#install.packages("tidytext")
#install.packages("tm")
#install.packages("modelr")
#install.packages("ggplot2")
library("dplyr")
library("stringr")
library("tidytext")
library("tm")
library("modelr")
library("ggplot2")
setwd("/Users/manu/Desktop/Fall'18/IST 707/Wk4/")

fed<- read.csv("fedPapers85.csv", header = TRUE, stringsAsFactors = FALSE)


fed_new<-
  mutate(fed,document=as.integer(str_extract(filename,"\\d+")))%>%
  select(-filename)

#head(stopwords("english"))

Dispt<-fed_new[1:11,]
H<-fed_new[12:62,]
M<-fed_new[71:85,]
HM<-fed_new[63:65,]
Jay<-fed_new[66:70,]

H<-H[,-1]

H<-H[,-71]
c<-4
km<- kmeans(H,centers = c)
km
km$iter
colnames(km$centers)<- colnames(H)

for (i in 1:c) { # loop for each cluster
  cat("CLUSTER", i, "\n")
  cat("Top 10 words:\n") # 10 most important terms at the centroid 
  print(head(sort(km$centers[i, ], decreasing = TRUE), n = 10))
  cat("\n")
  cat("Federalist Papers classified: \n") # extract essays classified 
  print(rownames(H)[km$cluster == i])
  cat("\n")
}

##
M<-M[,-1]

M<-M[,-71]
kmm<- kmeans(M,centers = c)
kmm
kmm$iter
colnames(kmm$centers)<- colnames(M)

for (i in 1:c) { # loop for each cluster
  cat("CLUSTER", i, "\n")
  cat("Top 10 words:\n") # 10 most important terms at the centroid 
  print(head(sort(kmm$centers[i, ], decreasing = TRUE), n = 10))
  cat("\n")
  cat("Federalist Papers classified: \n") # extract essays classified 
  print(rownames(M)[kmm$cluster == i])
  cat("\n")
}
##
HM<-HM[,-1]

HM<-HM[,-71]
kmhm<- kmeans(HM,centers = 2)
kmhm
kmhm$iter
colnames(kmhm$centers)<- colnames(HM)

for (i in 1:2) { # loop for each cluster
  cat("CLUSTER", i, "\n")
  cat("Top 10 words:\n") # 10 most important terms at the centroid 
  print(head(sort(kmhm$centers[i, ], decreasing = TRUE), n = 10))
  cat("\n")
  cat("Federalist Papers classified: \n") # extract essays classified 
  print(rownames(HM)[kmhm$cluster == i])
  cat("\n")
}
##
Jay<-Jay[,-1]

Jay<-Jay[,-71]
kmj<- kmeans(Jay,centers = c)
kmj
kmj$iter
colnames(kmj$centers)<- colnames(Jay)

for (i in 1:c) { # loop for each cluster
  cat("CLUSTER", i, "\n")
  cat("Top 10 words:\n") # 10 most important terms at the centroid 
  print(head(sort(kmj$centers[i, ], decreasing = TRUE), n = 10))
  cat("\n")
  cat("Federalist Papers classified: \n") # extract essays classified 
  print(rownames(Jay)[kmj$cluster == i])
  cat("\n")
}

##
Dispt<-Dispt[,-1]

Dispt<-Dispt[,-71]
kmd<- kmeans(Dispt,centers = c)
kmd
kmd$iter
colnames(kmd$centers)<- colnames(Dispt)

for (i in 1:c) { # loop for each cluster
  cat("CLUSTER", i, "\n")
  cat("Top 10 words:\n") # 10 most important terms at the centroid 
  print(head(sort(kmd$centers[i, ], decreasing = TRUE), n = 10))
  cat("\n")
  cat("Federalist Papers classified: \n") # extract essays classified 
  print(rownames(Dispt)[kmd$cluster == i])
  cat("\n")
}

##
Dispt$author<-"NA"
fed_pred<- fed_new %>% 
  filter(.$author=="Hamilton" | .$author=="Madison") %>% 
  mutate(author2=case_when(.$author=='Hamilton'~1, .$author=='Madison'~-1, TRUE~NA_real_))


fed_pred<-fed_pred[,-1]
doc_id<-as.data.frame(fed_pred[,71])
fed_pred<-fed_pred[,-71]
names(fed_pred)[71]<-"author"
auth<-rbind(fed_pred,Dispt)


hm.fit<-lm(author~upon+there+to+the+some+on+will,data=fed_pred)
hm.fit
hm.fitted<-fitted(hm.fit)
hm.fitted

fed_pred<-fed_pred%>%
  add_predictions(hm.fit)%>%
  mutate(prediction = if_else(pred>=0, "Hamilton", "Madison"))

sd(fed_pred$pred)

mean(hm.fitted[fed_pred$author == 1] > 0)
mean(hm.fitted[fed_pred$author == -1] < 0)

pp_dis<-predict(hm.fit, newdata = Dispt)
pp_dis

Dispt<-cbind(Dispt,pp_dis)

Dispt<- Dispt%>%
  mutate(prediction = if_else(pp_dis>=0, "Hamilton", "Madison"))

Dispt<-Dispt[,-71]
Dispt[,71]

##plot
final<-cbind(fed_pred,doc_id)
doc_id_h<- final%>%
  filter(author=="1")

doc_id_m<- final%>%
  filter(author=="-1")

doc_id_dis<- fed_new%>%
  filter(author=="dispt")

plot(doc_id_h[,74], doc_id_h$pred, pch = 15,xlim = c(1, 85), ylim = c(-2, 2), col = "red",xlab = "Federalist Papers", ylab = "Predicted values") 
abline(h = 0, lty = "dashed")
## essays authored by Madison; blue circles 
points(doc_id_m[,74], doc_id_m$pred,pch = 16, col = "blue")
## disputed authorship; black triangles 
points(doc_id_dis[,72], Dispt$pp_dis, pch = 17)

Final_Dispute_results<- cbind(Dispt,doc_id_dis[,72])
Final_Dispute_results[,c(71,72,73)]


###

hfed<-fed_new[,-1]
hfed<-hfed[,c(71,1:70)]
#hfed<-as.data.frame(t(hfed))
row.names(hfed) <- fed$filename
hfed<-hfed[,-1]
hdistance<-dist(hfed)
wordclust<-hclust(hdistance, method = "complete")
wordclust

cluster_no<-cutree(wordclust,4)
cluster_no
plot(wordclust, xlab="document_ids", main="cluster_dendogram")

#table(c, row.names(hfed))

ggplot(fed_new, aes(fed_new$document, cluster_no, color = fed_new$author)) + 
  geom_point(alpha = 0.4, size = 3.5) + geom_point(col = cluster_no) + 
  scale_color_manual(values = c('black', 'red', 'green','blue','pink'))
