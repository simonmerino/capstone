
# Does the link lead to an HTML page describing the exploratory analysis of the training data set?
# Has the data scientist done basic summaries of the three files? Word counts, line counts and basic data tables?
# Has the data scientist made basic plots, such as histograms to illustrate features of the data?
# Was the report written in a brief, concise style, in a way that a non-data scientist manager could appreciate?
  

#Loading libraries
library(ggplot2)
library(tm)
library(NLP)
library(stringr)
library(RWeka)
library(gridExtra)
library(wordcloud)
library(stopwords)
library(quanteda)
library(readtext)
library(data.table)
library(LaF)
library(plyr)

#numsamples<-20000 # esta es la que tengo los rds salvados
#numsamples<-100000
#numsamples<-2000
perc<-0.2

# TWITTER
#Determine lenght to filter
connection <- file("en_US.twitter.txt",open="r")
file_length<-length(readLines(connection))
remove(connection)
#filter
temp<-sample_lines("en_US.twitter.txt", file_length*perc)
write(temp,file="twitter_filtered.txt")


#BLOGS
connection <- file("en_US.blogs.txt",open="r")
file_length<-length(readLines(connection))
remove(connection)
#filter
temp<-sample_lines("en_US.blogs.txt", file_length*perc)
write(temp,file="blogs_filtered.txt")


# NEWS
#Determine lenght to filter
#esto da un castañazo por "final line found"
connection <- file("en_US.news.txt",open="r")
file_length<-length(readLines(connection))
remove(connection)
#filter
temp<-sample_lines("en_US.news.txt", file_length*perc)
write(temp,file="news_filtered.txt")
remove(temp)



# *^**^**^**^**^**^**^**^**^**^**^**^**^**^**^**^**^**^*
#TWITTER SECTION
# new method for building ngrams.
prueba<-readtext("twitter_filtered.txt")
twitterCorp<-quanteda::corpus(prueba)

twitter_1<-tokens(twitterCorp, what = "word", remove_numbers = TRUE,
          remove_punct = TRUE,remove_symbols = TRUE,
           ngrams = 1)

remove(twitterCorp)
#removing stopwords BEFORE creating ngrams as noted in the help of dfm

twitter_1 <- tokens_remove(twitter_1, stopwords("english"))

twitter_2<-tokens(twitter_1, what = "word", remove_numbers = TRUE,
                  remove_punct = TRUE,remove_symbols = TRUE,
                  ngrams = 2)

twitter_3<-tokens(twitter_1, what = "word", remove_numbers = TRUE,
                  remove_punct = TRUE,remove_symbols = TRUE,
                  ngrams = 3)

twitter_4<-tokens(twitter_1, what = "word", remove_numbers = TRUE,
                  remove_punct = TRUE,remove_symbols = TRUE,
                  ngrams = 4)


dfm_1<-dfm(twitter_1)
dfm_2<-dfm(twitter_2)
dfm_3<-dfm(twitter_3)
dfm_4<-dfm(twitter_4)



remove(twitter_1)
remove(twitter_2)
remove(twitter_3)
remove(twitter_4)

#eliminate stop words


dt_twitter_1<-data.table(ngram=featnames(dfm_1),count=colSums(dfm_1), key="ngram")
dt_twitter_2<-data.table(ngram=featnames(dfm_2),count=colSums(dfm_2), key="ngram")
dt_twitter_3<-data.table(ngram=featnames(dfm_3),count=colSums(dfm_3), key="ngram")
dt_twitter_4<-data.table(ngram=featnames(dfm_4),count=colSums(dfm_4), key="ngram")

remove(dfm_1)
remove(dfm_2)
remove(dfm_3)
remove(dfm_4)
# *^**^**^**^**^**^**^**^**^**^**^**^**^**^**^**^**^**^*


# *^**^**^**^**^**^**^**^**^**^**^**^**^**^**^**^**^**^*
#NEWS SECTION
# new method for building ngrams.
prueba<-readtext("news_filtered.txt")
newsCorp<-quanteda::corpus(prueba)

news_1<-tokens(newsCorp, what = "word", remove_numbers = TRUE,
               remove_punct = TRUE,remove_symbols = TRUE,
                  ngrams = 1)

remove(newsCorp)
#removing stopwords BEFORE creating ngrams as noted in the help of dfm

news_1 <- tokens_remove(news_1, stopwords("english"))

news_2<-tokens(news_1, what = "word", remove_numbers = TRUE,
               remove_punct = TRUE,remove_symbols = TRUE,
                  ngrams = 2)

news_3<-tokens(news_1, what = "word", remove_numbers = TRUE,
               remove_punct = TRUE,remove_symbols = TRUE,
                  ngrams = 3)

news_4<-tokens(news_1, what = "word", remove_numbers = TRUE,
               remove_punct = TRUE,remove_symbols = TRUE,
                  ngrams = 4)


dfm_1<-dfm(news_1)
dfm_2<-dfm(news_2)
dfm_3<-dfm(news_3)
dfm_4<-dfm(news_4)



remove(news_1)
remove(news_2)
remove(news_3)
remove(news_4)

#eliminate stop words


dt_news_1<-data.table(ngram=featnames(dfm_1),count=colSums(dfm_1), key="ngram")
dt_news_2<-data.table(ngram=featnames(dfm_2),count=colSums(dfm_2), key="ngram")
dt_news_3<-data.table(ngram=featnames(dfm_3),count=colSums(dfm_3), key="ngram")
dt_news_4<-data.table(ngram=featnames(dfm_4),count=colSums(dfm_4), key="ngram")

remove(dfm_1)
remove(dfm_2)
remove(dfm_3)
remove(dfm_4)
# *^**^**^**^**^**^**^**^**^**^**^**^**^**^**^**^**^**^*

# *^**^**^**^**^**^**^**^**^**^**^**^**^**^**^**^**^**^*
#Blogs SECTION
# new method for building ngrams.
prueba<-readtext("blogs_filtered.txt")
blogsCorp<-quanteda::corpus(prueba)

blogs_1<-tokens(blogsCorp, what = "word", remove_numbers = TRUE,
                remove_punct = TRUE,remove_symbols = TRUE,
               ngrams = 1)

remove(blogsCorp)
#removing stopwords BEFORE creating ngrams as noted in the help of dfm

blogs_1 <- tokens_remove(blogs_1, stopwords("english"))

blogs_2<-tokens(blogs_1, what = "word", remove_numbers = TRUE,
                remove_punct = TRUE,remove_symbols = TRUE,
               ngrams = 2)

blogs_3<-tokens(blogs_1, what = "word", remove_numbers = TRUE,
                remove_punct = TRUE,remove_symbols = TRUE,
               ngrams = 3)

blogs_4<-tokens(blogs_1, what = "word", remove_numbers = TRUE,
                remove_punct = TRUE,remove_symbols = TRUE,
               ngrams = 4)


dfm_1<-dfm(blogs_1)
dfm_2<-dfm(blogs_2)
dfm_3<-dfm(blogs_3)
dfm_4<-dfm(blogs_4)



remove(blogs_1)
remove(blogs_2)
remove(blogs_3)
remove(blogs_4)

#eliminate stop words


dt_blogs_1<-data.table(ngram=featnames(dfm_1),count=colSums(dfm_1), key="ngram")
dt_blogs_2<-data.table(ngram=featnames(dfm_2),count=colSums(dfm_2), key="ngram")
dt_blogs_3<-data.table(ngram=featnames(dfm_3),count=colSums(dfm_3), key="ngram")
dt_blogs_4<-data.table(ngram=featnames(dfm_4),count=colSums(dfm_4), key="ngram")

remove(dfm_1)
remove(dfm_2)
remove(dfm_3)
remove(dfm_4)

# *^**^**^**^**^**^**^**^**^**^**^**^**^**^**^**^**^**^*



#To join datatables, just use rbind

dt1<-rbind(dt_blogs_1,dt_news_1,dt_twitter_1)
dt2<-rbind(dt_blogs_2,dt_news_2,dt_twitter_2)
dt3<-rbind(dt_blogs_3,dt_news_3,dt_twitter_3)
dt4<-rbind(dt_blogs_4,dt_news_4,dt_twitter_4)

#watch out, these will leave duplicated entries!!

remove(dt_blogs_1)
remove(dt_blogs_2)
remove(dt_blogs_3)
remove(dt_blogs_4)

remove(dt_news_1)
remove(dt_news_2)
remove(dt_news_3)
remove(dt_news_4)

remove(dt_twitter_1)
remove(dt_twitter_2)
remove(dt_twitter_3)
remove(dt_twitter_4)


#combine values of the same type
dt1<-ddply(dt1,~ngram,summarise,count=sum(count))
dt2<-ddply(dt2,~ngram,summarise,count=sum(count))
dt3<-ddply(dt3,~ngram,summarise,count=sum(count))
dt4<-ddply(dt4,~ngram,summarise,count=sum(count))




#Utilities

searchpatterns<-function(data,entry){
  #data is a data table with ngram and count as columns
  #entry is the string (with sep="_") that needs to be found in data$ngram
  indexes<-0
  for (i in 1:nrow(data)){
    if (str_detect(data$ngram[i], entry)){
      indexes<-c(indexes,i)
    }
  }
  return(indexes)
  
}

#Reduce size of the model by removing entries

dt1<-dt1[which(dt1$count>2),]
dt2<-dt2[which(dt2$count>2),]
dt3<-dt3[which(dt3$count>2),]
dt4<-dt4[which(dt4$count>2),]

#Convert number of occurences to percentage

temp<-sum(dt1$count)
dt1$perc<-(dt1$count*100)/temp

temp<-sum(dt2$count)
dt2$perc<-(dt2$count*100)/temp

temp<-sum(dt3$count)
dt3$perc<-(dt3$count*100)/temp

temp<-sum(dt4$count)
dt4$perc<-(dt4$count*100)/temp


#Searching algorithm

string<-"i love that song and the music"
#string<-stopwords()



#Searching Algorithm
mostfrequent <-function(string){
  #remove stopwords
  string<-removeWords(string,stopwords("english"))  
  #trim and remove multiple spaces
  string<-gsub("\\s+", " ", str_trim(string))
  string<-gsub(" ","_",string)
  numberofwords<-str_count(string," ")+1
  if (numberofwords>2){
    output<-dt4[searchpatterns(dt4,word(string,start=-3,end=-1,sep="_")),]    
  }
  if (numberofwords>1){
    tempoutput<-dt3[searchpatterns(dt3,word(string,start=-2,end=-1,sep="_")),]    
    output<-rbind(output,tempoutput)
  }
  if (numberofwords>0){
    tempoutput<-dt2[searchpatterns(dt2,word(string,start=-1,end=-1,sep="_")),]    
    output<-rbind(output,tempoutput)
  }
  

#I have rmoved also 2 ocurrences in dt4,dt3 and dt1
  
  return(output[1])
}
#1 Remove stopwords






dt1[searchpatterns(dt1,"!_#4"),]
dt2[searchpatterns(dt2,"case_of"),]
dt3[searchpatterns(dt3,"!_#6"),]
dt4[searchpatterns(dt4,"bouquet_case"),]
