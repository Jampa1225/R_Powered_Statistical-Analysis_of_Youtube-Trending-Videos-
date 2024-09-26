---
title: "Youtube new Trending Statistics"
author: "Donyoe"
output: 
  html_document: 
    fig_height: 6
    fig_width: 9
    highlight: tango
    theme: cosmo
    toc: yes
    code_folding: hide
---



```{r setup, include=FALSE}

knitr::opts_chunk$set(

	echo = TRUE,

	message = FALSE,

	warning = FALSE

)

```



![](https://www.seoclerk.com/pics/443105-1Cdpmj1460287445.png)

  

  

  

* **YouTube** is an American video-sharing website headquartered in San Bruno, California. The service was created by three former PayPal employees—Chad Hurley, Steve Chen, and Jawed Karim—in February 2005. Google bought the site in November 2006 for US$1.65 billion; YouTube now operates as one of Google's subsidiaries.





# Loading libraries

```{r}

set.seed(123)

# Data manipulation

library(data.table)

library(dplyr)

library(DT)

# Time manipulation

library(lubridate)

# Visualization

library(ggplot2)

library(plotrix)

library(corrplot)

library(ggdendro)

library(ggrepel)

# Wordcloud

library(wordcloud)

# Text manipulation

library(tidytext)

library(stringr)

library(tm)

library(sentimentr)

library(wordcloud)

library(RSentiment)

```



# Reading and preparing data

```{r}

gb <- tail(fread("../input/GBvideos.csv",encoding = "UTF-8"),20000)

gb[,"Location":="GB"]

fr <- tail(fread("../input/FRvideos.csv",encoding = "UTF-8"),20000)

fr[,"Location":="FR"]

ca <- tail(fread("../input/CAvideos.csv",encoding = "UTF-8"),20000)

ca[,"Location":="CA"]

us <- tail(fread("../input/USvideos.csv",encoding = "UTF-8"),20000)

us[,"Location":="US"]

de <- tail(fread("../input/DEvideos.csv",encoding = "UTF-8"),20000)

de[,"Location":="DE"]



videos <- as.data.table(rbind(gb,fr,ca,us,de))

videos$trending_date <- ydm(videos$trending_date)

videos$publish_time <- ymd(substr(videos$publish_time,start = 1,stop = 10))

videos$dif_days <- videos$trending_date-videos$publish_time

```



> First thing we are going to do is an analysis of the complete dataset, after that we will analyse every region







# First lets see the correlation 

```{r}

corrplot.mixed(corr = cor(videos[,c("category_id","views","likes","dislikes","comment_count"),with=F]))

```



* We can see that between views and likes we have a high correlation, I thought that we will have a similar correlation between views and dislikes, but is almost half of the like correlation.





# Most...{.tabset .tabset-pills}



## Viewed videos

```{r}

mvideo <- videos[,.("Total_Views"=round(max(views,na.rm = T),digits = 2)),by=.(title,thumbnail_link)][order(-Total_Views)]



mvideo %>% 

  mutate(image = paste0('<img width="80%" height="80%" src="', thumbnail_link , '"></img>')) %>% 

  arrange(-Total_Views) %>% 

  top_n(10,wt = Total_Views) %>% 

  select(image, title, Total_Views) %>% 

  datatable(class = "nowrap hover row-border", escape = FALSE, options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))

```





## Liked videos (Absolute)

```{r}

mvideo <- videos[,.("Total_Likes"=round(max(likes,na.rm = T),digits = 2)),by=.(title,thumbnail_link)][order(-Total_Likes)]



mvideo %>% 

  mutate(image = paste0('<img width="80%" height="80%" src="', thumbnail_link , '"></img>')) %>% 

  arrange(-Total_Likes) %>% 

  top_n(10,wt = Total_Likes) %>% 

  select(image, title, Total_Likes) %>% 

  datatable(class = "nowrap hover row-border", escape = FALSE, options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))

```



## Disliked videos (Absolute)

```{r}

mvideo <- videos[,.("Total_Dislikes"=round(max(dislikes,na.rm = T),digits = 2)),by=.(title,thumbnail_link)][order(-Total_Dislikes)]



mvideo %>% 

  mutate(image = paste0('<img width="80%" height="80%" src="', thumbnail_link , '"></img>')) %>% 

  arrange(-Total_Dislikes) %>% 

  top_n(10,wt = Total_Dislikes) %>% 

  select(image, title, Total_Dislikes) %>% 

  datatable(class = "nowrap hover row-border", escape = FALSE, options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))

```





## Commented videos (Absolute)

```{r}

mvideo <- videos[,.("Total_comments"=round(max(comment_count,na.rm = T),digits = 2)),by=.(title,thumbnail_link)][order(-Total_comments)]



mvideo %>% 

  mutate(image = paste0('<img width="80%" height="80%" src="', thumbnail_link , '"></img>')) %>% 

  arrange(-Total_comments) %>% 

  top_n(10,wt = Total_comments) %>% 

  select(image, title, Total_comments) %>% 

  datatable(class = "nowrap hover row-border", escape = FALSE, options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))

```





# Top 10 in percentage{.tabset .tabset-pills}



* Because the absolute number of likes, dislikes and comments didnt show all the information to really know if the video had an impact or not we will see their percentages.



## % Liked videos

```{r}

mvideo <- videos[,.("Percentage_Likes"=round(100*max(likes,na.rm = T)/max(views,na.rm = T),digits = 2)),by=.(title,thumbnail_link)][order(-Percentage_Likes)]



mvideo %>% 

  mutate(image = paste0('<img width="80%" height="80%" src="', thumbnail_link , '"></img>')) %>% 

  arrange(-Percentage_Likes) %>% 

  top_n(10,wt = Percentage_Likes) %>% 

  select(image, title, Percentage_Likes) %>% 

  datatable(class = "nowrap hover row-border", escape = FALSE, options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))

```



## % Disliked videos 

```{r}

mvideo <- videos[,.("Percentage_Dislikes"=round(100*max(dislikes,na.rm = T)/max(views,na.rm = T),digits = 2)),by=.(title,thumbnail_link)][order(-Percentage_Dislikes)]



mvideo %>% 

  mutate(image = paste0('<img width="80%" height="80%" src="', thumbnail_link , '"></img>')) %>% 

  arrange(-Percentage_Dislikes) %>% 

  top_n(10,wt = Percentage_Dislikes) %>% 

  select(image, title, Percentage_Dislikes) %>% 

  datatable(class = "nowrap hover row-border", escape = FALSE, options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))

```





## % Commented videos 

```{r}

mvideo <- videos[,.("Percentage_comments"=round(100*max(comment_count,na.rm = T)/max(views,na.rm = T),digits = 2)),by=.(title,thumbnail_link)][order(-Percentage_comments)]



mvideo %>% 

  mutate(image = paste0('<img width="80%" height="80%" src="', thumbnail_link , '"></img>')) %>% 

  arrange(-Percentage_comments) %>% 

  top_n(10,wt = Percentage_comments) %>% 

  select(image, title, Percentage_comments) %>% 

  datatable(class = "nowrap hover row-border", escape = FALSE, options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))

```



* It looks that the French and German people comments more often than other nationalities









# Top trending Channels in all countries

```{r}

ggplot(videos[,.N,by=channel_title][order(-N)][1:10],aes(reorder(channel_title,-N),N,fill=channel_title))+geom_bar(stat="identity")+geom_label(aes(label=N))+guides(fill="none")+theme(axis.text.x = element_text(angle = 45,hjust = 1))+  labs(caption="Donyoe",title=" Top trending channel titles in all countries")+

xlab(NULL)+ylab(NULL)+coord_flip()

```



# Title Bigrams 

```{r}

biga <- unnest_tokens(videos,bigram, title, token = "ngrams", n = 2)

biga <- as.data.table(biga)



ggplot(biga[,.N,by=bigram][order(-N)][1:19],aes(reorder(bigram,-N),N,fill=bigram))+geom_bar(stat="identity")+geom_label(aes(label=N))+guides(fill="none")+theme(axis.text.x = element_text(angle = 45,hjust = 1))+  labs(caption="Donyoe",title="Top Title bigrams")+xlab(NULL)+ylab(NULL)



```



* There are mainly bigrams relationed to *music*.





# Title wordcloud

```{r include=FALSE}

#Testing a bug

corpus = Corpus(VectorSource(list(sample(videos$title,size=2000))))

corpus = tm_map(corpus, removePunctuation)

corpus = tm_map(corpus, content_transformer(tolower))

corpus = tm_map(corpus, removeNumbers) 

corpus = tm_map(corpus, stripWhitespace)

corpus = tm_map(corpus, removeWords, stopwords('english'))



dtm_eap = DocumentTermMatrix(VCorpus(VectorSource(corpus[[1]]$content)))

freq_eap <- colSums(as.matrix(dtm_eap))



sentiments_eap = calculate_sentiment(names(freq_eap))

sent_video = cbind(sentiments_eap, as.data.frame(freq_eap))

sent_video[contains(match = "uu",vars = sent_video$text),"freq_eap"] <- 0L

```



```{r}

wordcloud(sent_video$text,sent_video$freq, min.freq=5,colors=brewer.pal(6,"Dark2"),random.order = F)

```



* We can see that a lot of the trending videos are music videos.



# Top Category ID

```{r}

ggplot(videos[,.N,by=category_id][order(-N)],aes(reorder(category_id,-N),N,fill=as.factor(category_id)))+geom_bar(stat="identity")+guides(fill="none")+labs(caption="Donyoe",title=" Top Category ID")+

xlab(NULL)+ylab(NULL)

```



# How much time passes between published and trending?

```{r}

ggplot(videos[dif_days<30],aes(as.factor(dif_days),fill=as.factor(dif_days)))+geom_bar()+guides(fill="none")+labs(caption="Donyoe",title=" Time between published and trending",subtitle="In days")+xlab(NULL)+ylab(NULL)

```



* It seems that the videos never trend in the same day it is published.



# Tags wordcloud

```{r include=FALSE}

corpus = Corpus(VectorSource(list(sample(videos$tags,size=2000))))

corpus = tm_map(corpus, removePunctuation)

corpus = tm_map(corpus, content_transformer(tolower))

corpus = tm_map(corpus, removeNumbers) 

corpus = tm_map(corpus, stripWhitespace)

corpus = tm_map(corpus, removeWords, stopwords('english'))



dtm_eap = DocumentTermMatrix(VCorpus(VectorSource(corpus[[1]]$content)))

freq_eap <- colSums(as.matrix(dtm_eap))



sentiments_eap = calculate_sentiment(names(freq_eap))

sent_video = cbind(sentiments_eap, as.data.frame(freq_eap))

sent_video[contains(match = "u067",vars = sent_video$text),"freq_eap"] <- 0L

sent_video[contains(match = "uuu",vars = sent_video$text),"freq_eap"] <- 0L



```



```{r}

wordcloud(sent_video$text,sent_video$freq, min.freq=10,colors=brewer.pal(6,"Dark2"),random.order = F)

```



* [none] is displayed if there are no tags, after none we can see tags as *new*, *iphone*, *episode* and tags related to *music*.



# Views Vs Likes

```{r}

ggplot(videos[,.("views"=max(views),"likes"=max(likes)),by=title],aes(views,likes,colour=likes,size=likes))+geom_jitter()+geom_smooth()+guides(fill="none")+labs(caption="Donyoe",title="Views Vs Likes",subtitle="In days")+theme(legend.position = "none")+geom_text_repel(data=subset(videos[,.("views"=max(views),"likes"=max(likes)),by=title], views > 5e+07),

            aes(views,likes,label=title),check_overlap=T)

```



# Likes Vs Comment

```{r}

ggplot(videos[,.("comment_count"=max(comment_count),"likes"=max(likes)),by=title],aes(comment_count,likes,colour=likes,size=likes))+geom_jitter()+geom_smooth()+guides(fill="none")+labs(caption="Donyoe",title="Views Vs Comment",subtitle="In days")+

  theme(legend.position = "none")+geom_text_repel(data=subset(videos[,.("comment_count"=max(comment_count),"likes"=max(likes)),by=title], likes > 1e+06),

            aes(comment_count,likes,label=title),check_overlap=T)

```



# Sentiment Analysis Description field (Sample)

```{r include=FALSE}

corpus = Corpus(VectorSource(list(sample(videos$description,size=2000))))

corpus = tm_map(corpus, removePunctuation)

corpus = tm_map(corpus, content_transformer(tolower))

corpus = tm_map(corpus, removeNumbers) 

corpus = tm_map(corpus, stripWhitespace)

corpus = tm_map(corpus, removeWords, stopwords('english'))



dtm_eap = DocumentTermMatrix(VCorpus(VectorSource(corpus[[1]]$content)))

freq_eap <- colSums(as.matrix(dtm_eap))



 sentiments <- as.data.table(sentiments_eap)

 sentiments1 <- sentiments[,.N,by=.(sentiment)]

 sentiments1[,"Total":=sum(N)]

 sentiments1 <- sentiments1[,.("Percentage"=100*N/Total),by=.(sentiment)]

```







```{r}

ggplot(sentiments1,aes(x = sentiment,y = Percentage ,fill=sentiment ))+

  geom_bar(stat = "identity") +

  ggtitle("Description Sentiments (Sample)")+xlab("Sentiment")+ylab("% Sentiment")+ 

  theme(axis.text.x = element_text(angle = 45, size=8,hjust = 1))



```



* Here we can see that the sentiments in the description field are basically neutral.





# Sentimentr Analysis

```{r}

sents_eap <- sentiment(videos$description)

sents_eap <- sents_eap[,.("word_count"=sum(word_count),"sentiment"=sum(sentiment)),by=element_id]

ggplot(data=sents_eap)+

  geom_histogram(mapping = aes(x=sentiment),binwidth = .1)+

  theme_bw()+scale_fill_brewer(palette = "Set1")+

  geom_vline(xintercept = 0, color = "coral", size = 1.5, alpha = 0.6, linetype = "longdash") +

  labs(title="Description Score",caption="Donyoe")+coord_cartesian(xlim = c(-4, 4))

```



* We can see that the video description is clearly more posite than negative



# Description Bigrams 

```{r}

biga <- unnest_tokens(videos,bigram, description, token = "ngrams", n = 2)

biga <- as.data.table(biga)



ggplot(biga[,.N,by=bigram][order(-N)][1:19],aes(reorder(bigram,-N),N,fill=bigram))+geom_bar(stat="identity")+geom_label(aes(label=N))+guides(fill="none")+theme(axis.text.x = element_text(angle = 45,hjust = 1))+  labs(caption="Donyoe",title="Top Description bigrams")+xlab(NULL)+ylab(NULL)



```



* Here in the description box we can see that they are mainly shortener links to other sites, the most used link shortener is *bit.ly* followed by *goog.gl*, after the shortener pages there are links to other *youtube* videos, link to sell in *amazon* and after that, links to their social media accounts in *twitter* and *facebook.*





# Cluster

```{r}

videos[,"Percentage_Likes":=round(100*(likes)/sum(as.numeric(views),na.rm = T),digits = 4)]

videos[,"Percentage_Disikes":=round(100*(dislikes)/sum(as.numeric(views),na.rm = T),digits = 4)]

videos[,"Percentage_comments":=round(100*(comment_count)/sum(as.numeric(views),na.rm = T),digits = 4)]

dista <- dist(x = videos)

cluster <- hclust(dista,method = "ward.D")

ggdendrogram(cluster,rotate = T)

clust_cut <- cutree(cluster,3)

videos <- videos[,"Cluster":=clust_cut]





c1 <- apply(videos[Cluster==1,.(views,likes,dislikes,comment_count,Percentage_Likes,Percentage_Disikes,Percentage_comments,as.numeric(dif_days))], 2, function(x) mean(x,na.rm=T))

c2 <- apply(videos[Cluster==2,.(views,likes,dislikes,comment_count,Percentage_Likes,Percentage_Disikes,Percentage_comments,as.numeric(dif_days))], 2, function(x) mean(x,na.rm=T))

c3 <- apply(videos[Cluster==3,.(views,likes,dislikes,comment_count,Percentage_Likes,Percentage_Disikes,Percentage_comments,as.numeric(dif_days))], 2, function(x) mean(x,na.rm=T))



clus <- as.data.table(rbind(c1,c2,c3))

knitr::kable(t(clus),digits=10)



```



* Cluster A : The second in reach the trending page , and medium in views, likes and dislikes.

* Cluster B : The cluster with most views with a significative difference, very liked, disliked and commented as in absolute as in relative units.

* Cluster C : This is a quite weird cluster, they reach the trending page the last with sifnificative difference, they have very little views, likes, dislikes and comments, and is the last in percentage of likes, dislikes and comments.







# Top Countries in Absolute numbers{.tabset .tabset-pills}



## In total number of views

```{r}

ggplot(videos[,.("Total_Views"=max(views)),by=Location],aes(reorder(Location,-Total_Views),Total_Views,fill=Location))+geom_bar(stat="identity")+geom_label(aes(label=Total_Views))+guides(fill="none")+theme(axis.text.x = element_text(angle = 45,hjust = 1))+  labs(caption="Donyoe",title=" Total Views by Countries")+xlab(NULL)+ylab(NULL)

```



* GB is the Country with most viewed videos in the trending field with significative difference with the other countries, almost doubled the second country.



## In total number of likes

```{r}

ggplot(videos[,.("Total_Likes"=max(likes)),by=Location],aes(reorder(Location,-Total_Likes),Total_Likes,fill=Location))+geom_bar(stat="identity")+geom_label(aes(label=Total_Likes))+guides(fill="none")+theme(axis.text.x = element_text(angle = 45,hjust = 1))+  labs(caption="Donyoe",title=" Total number of likes by Countries")+xlab(NULL)+ylab(NULL)

```



## In total number of dislikes

```{r}

ggplot(videos[,.("Total_Dislikes"=max(dislikes)),by=Location],aes(reorder(Location,-Total_Dislikes),Total_Dislikes,fill=Location))+geom_bar(stat="identity")+geom_label(aes(label=Total_Dislikes))+guides(fill="none")+theme(axis.text.x = element_text(angle = 45,hjust = 1))+  labs(caption="Donyoe",title=" Total Dislikes by Countries")+xlab(NULL)+ylab(NULL)

```



## In total number of comments

```{r}

ggplot(videos[,.("Total_Comments"=max(comment_count)),by=Location],aes(reorder(Location,-Total_Comments),Total_Comments,fill=Location))+geom_bar(stat="identity")+geom_label(aes(label=Total_Comments))+guides(fill="none")+theme(axis.text.x = element_text(angle = 45,hjust = 1))+  labs(caption="Donyoe",title=" Total Comments by Countries")+xlab(NULL)+ylab(NULL)

```



# Top Countries in % {.tabset .tabset-pills}



## In % of likes

```{r}

ggplot(videos[,.("MeanPercentage_Likes"=mean(Percentage_Likes,na.rm = T)),by=Location],aes(reorder(Location,-MeanPercentage_Likes),MeanPercentage_Likes,fill=Location))+geom_bar(stat="identity")+geom_label(aes(label=MeanPercentage_Likes))+guides(fill="none")+theme(axis.text.x = element_text(angle = 45,hjust = 1))+  labs(caption="Donyoe",title=" Mean Percentage of likes by Countries")+xlab(NULL)+ylab(NULL)

```



## In % of dislikes

```{r}

ggplot(videos[,.("MeanPercentage_Disikes"=mean(Percentage_Disikes,na.rm = T)),by=Location],aes(reorder(Location,-MeanPercentage_Disikes),MeanPercentage_Disikes,fill=Location))+geom_bar(stat="identity")+geom_label(aes(label=MeanPercentage_Disikes))+guides(fill="none")+theme(axis.text.x = element_text(angle = 45,hjust = 1))+  labs(caption="Donyoe",title=" Mean Percentage of dislikes by Countries")+xlab(NULL)+ylab(NULL)

```



## In % comments

```{r}

ggplot(videos[,.("MeanPercentage_Likes"=mean(Percentage_Likes,na.rm = T)),by=Location],aes(reorder(Location,-MeanPercentage_Likes),MeanPercentage_Likes,fill=Location))+geom_bar(stat="identity")+geom_label(aes(label=MeanPercentage_Likes))+guides(fill="none")+theme(axis.text.x = element_text(angle = 45,hjust = 1))+  labs(caption="Donyoe",title=" Mean Percentage of comments by Countries")+xlab(NULL)+ylab(NULL)

```





# Title length in words

```{r}

videos[,"Word_len":= str_length(title)]

ggplot(videos[,.N,keyby=Word_len],aes(Word_len,N,fill=N))+geom_bar(stat = "identity")+guides(fill="none")+labs(caption="Donyoe",title="Title length in words")+xlab(NULL)+ylab(NULL)



```



# How much time passes between published and trending by countries?

```{r}

ggplot(videos[dif_days<40],aes(as.factor(dif_days),fill=as.factor(dif_days)))+geom_bar()+guides(fill="none")+labs(caption="Donyoe",title=" Time between published and trending by countries",subtitle="In days")+xlab(NULL)+ylab(NULL)+facet_wrap(~Location)

```



* In DE and FR takes a little time to reach the trending page, 1 or 2 days usually. Similar case to CA, but here is higher the 2 day time difference, it takes a little longer to reach the trending page.

* In GB and US the time difference is clearly different, the videos takes in GB usually more than 2 days to reach the trending page, and in the US not seem to have videos that reach the trending page fast.





* Hope you like it!
