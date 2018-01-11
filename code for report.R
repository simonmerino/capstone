
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




# Twitter File

connection <- file("en_US.twitter.txt",open="r")
twitterLines<-readLines(connection)
twitterCorp <- SimpleCorpus(VectorSource(twitterLines[1:10000]))
close(connection)


#Blogs File
connection <- file("en_US.blogs.txt",open="r")
blogsLines<-readLines(connection)
blogsCorp <- SimpleCorpus(VectorSource(blogsLines[1:10000]))
close(connection)

# News File
connection <- file("en_US.news.txt",open="r")
newslines<-readLines(connection)
newsCorp <- SimpleCorpus(VectorSource(newslines[1:10000]))
close(connection)

#Dataframe for hosting stats about the files

summaryTable<-data.frame(row.names=c("TwitterData","BlogsData","NewsData"))

#Summary of read files

summaryTable<-cbind(summaryTable,c(length(twitterLines),length(blogsLines),length(newslines)))
names(summaryTable)<-"Number_of_lines"

remove(twitterLines)
remove(blogsLines)
remove(newslines)

#Preprocessing
#Let´s eliminate stopwords

twitterCorp<-tm_map(twitterCorp,removeWords,stopwords("english"))
blogsCorp<-tm_map(blogsCorp,removeWords,stopwords("english"))
newsCorp<-tm_map(newsCorp,removeWords,stopwords("english"))


#Now let´s find raw version of every word (stem)

twitterCorp<-tm_map(twitterCorp,stemDocument)
blogsCorp<-tm_map(blogsCorp,stemDocument)
newsCorp<-tm_map(newsCorp,stemDocument)


#Generate TermDocumentMatrix (I think I do not use this)

#tdmTwitter<-TermDocumentMatrix(twitterCorp)
#tdmBlogs<-TermDocumentMatrix(blogsCorp)
#tdmNews<-TermDocumentMatrix(newsCorp)


#1,2 and 3 grams for the 3 corpus under study
twitter_grams_1 <- NGramTokenizer(twitterCorp, Weka_control(min = 1, max = 1))
twitter_grams_2 <- NGramTokenizer(twitterCorp, Weka_control(min = 2, max = 2))
twitter_grams_3 <-NGramTokenizer(twitterCorp, Weka_control(min = 3, max = 3))

blogs_grams_1 <- NGramTokenizer(blogsCorp, Weka_control(min = 1, max = 1))
blogs_grams_2 <- NGramTokenizer(blogsCorp, Weka_control(min = 2, max = 2))
blogs_grams_3 <-NGramTokenizer(blogsCorp, Weka_control(min = 3, max = 3))

news_grams_1 <- NGramTokenizer(newsCorp, Weka_control(min = 1, max = 1))
news_grams_2 <- NGramTokenizer(newsCorp, Weka_control(min = 2, max = 2))
news_grams_3 <-NGramTokenizer(newsCorp, Weka_control(min = 3, max = 3))



#Summarizing the information
summaryTable$Total_number_of_1_grams<-c(length(twitter_grams_1),length(blogs_grams_1),length(news_grams_1))
summaryTable$Total_number_of_2_grams<-c(length(twitter_grams_2),length(blogs_grams_2),length(news_grams_2))
summaryTable$Total_number_of_3_grams<-c(length(twitter_grams_3),length(blogs_grams_3),length(news_grams_3))


#Dealing now with unique 1-2-3 grams

twitter_grams_1<-table(twitter_grams_1)
twitter_grams_2<-table(twitter_grams_2)
twitter_grams_3<-table(twitter_grams_3)

blogs_grams_1<-table(blogs_grams_1)
blogs_grams_2<-table(blogs_grams_2)
blogs_grams_3<-table(blogs_grams_3)

news_grams_1<-table(news_grams_1)
news_grams_2<-table(news_grams_2)
news_grams_3<-table(news_grams_3)

#Summarizing the information
summaryTable$Total_number_of_unique_1_grams<-c(length(twitter_grams_1),length(blogs_grams_1),length(news_grams_1))
summaryTable$Total_number_of_unique_2_grams<-c(length(twitter_grams_2),length(blogs_grams_2),length(news_grams_2))
summaryTable$Total_number_of_unique_3_grams<-c(length(twitter_grams_3),length(blogs_grams_3),length(news_grams_3))



#Bar Char for TOP N most used words for the 3 documents

twitter_grams_1<-as.data.frame(twitter_grams_1)
twitter_grams_2<-as.data.frame(twitter_grams_2)
twitter_grams_3<-as.data.frame(twitter_grams_3)

blogs_grams_1<-as.data.frame(blogs_grams_1)
blogs_grams_2<-as.data.frame(blogs_grams_2)
blogs_grams_3<-as.data.frame(blogs_grams_3)

news_grams_1<-as.data.frame(news_grams_1)
news_grams_2<-as.data.frame(news_grams_2)
news_grams_3<-as.data.frame(news_grams_3)


# combined Word Cloud

#dataframes need to have the same column names

names(blogs_grams_1)[1]<-"gram"
names(twitter_grams_1)[1]<-"gram"
names(news_grams_1)[1]<-"gram"
names(blogs_grams_2)[1]<-"gram"
names(twitter_grams_2)[1]<-"gram"
names(news_grams_2)[1]<-"gram"
names(blogs_grams_3)[1]<-"gram"
names(twitter_grams_3)[1]<-"gram"
names(news_grams_3)[1]<-"gram"


gram1<-rbind(twitter_grams_1,blogs_grams_1,news_grams_1)

wordcloud(gram1$gram, gram1$Freq,min.freq=10,ordered.colors=TRUE,max.words=50)



#Sorting the dataframes in descending order
twitter_grams_1<-twitter_grams_1[order(twitter_grams_1$Freq,decreasing = TRUE),]
twitter_grams_2<-twitter_grams_2[order(twitter_grams_2$Freq,decreasing = TRUE),]
twitter_grams_3<-twitter_grams_3[order(twitter_grams_3$Freq,decreasing = TRUE),]


blogs_grams_1<-blogs_grams_1[order(blogs_grams_1$Freq,decreasing = TRUE),]
blogs_grams_2<-blogs_grams_2[order(blogs_grams_2$Freq,decreasing = TRUE),]
blogs_grams_3<-blogs_grams_3[order(blogs_grams_3$Freq,decreasing = TRUE),]

news_grams_1<-news_grams_1[order(news_grams_1$Freq,decreasing = TRUE),]
news_grams_2<-news_grams_2[order(news_grams_2$Freq,decreasing = TRUE),]
news_grams_3<-news_grams_3[order(news_grams_3$Freq,decreasing = TRUE),]




#Take TOP 30 words

twitter_grams_1<-twitter_grams_1[1:30,]
twitter_grams_2<-twitter_grams_2[1:30,]
twitter_grams_3<-twitter_grams_3[1:30,]

blogs_grams_1<-blogs_grams_1[1:30,]
blogs_grams_2<-blogs_grams_2[1:30,]
blogs_grams_3<-blogs_grams_3[1:30,]


news_grams_1<-news_grams_1[1:30,]
news_grams_2<-news_grams_2[1:30,]
news_grams_3<-news_grams_3[1:30,]




#Plot

t1<-ggplot(twitter_grams_1,aes(x=reorder(gram, -Freq),y=Freq)) + geom_bar(stat="identity", fill = "darkblue", colour = "black")
t1<-t1+theme(axis.text.x = element_text(angle=30, hjust=1))+xlab("Top Ngrams of order 1 in Twitter dataset")

t2<-ggplot(twitter_grams_2,aes(x=reorder(gram, -Freq),y=Freq)) + geom_bar(stat="identity", fill = "darkblue", colour = "black")
t2<-t2+theme(axis.text.x = element_text(angle=30, hjust=1))+xlab("Top Ngrams of order 2 in Twitter dataset")


t3<-ggplot(twitter_grams_3,aes(x=reorder(gram, -Freq),y=Freq)) + geom_bar(stat="identity", fill = "darkblue", colour = "black")
t3<-t3+theme(axis.text.x = element_text(angle=30, hjust=1))+xlab("Top Ngrams of order 3 in Twitter dataset")
grid.arrange(t1,t2,t3,nrow=3)


b1<-ggplot(blogs_grams_1,aes(x=reorder(gram, -Freq),y=Freq)) + geom_bar(stat="identity", fill = "darkblue", colour = "black")
b1<-b1+theme(axis.text.x = element_text(angle=30, hjust=1))+xlab("Top Ngrams of order 1 in Blog dataset")

b2<-ggplot(blogs_grams_2,aes(x=reorder(gram, -Freq),y=Freq)) + geom_bar(stat="identity", fill = "darkblue", colour = "black")
b2<-b2+theme(axis.text.x = element_text(angle=30, hjust=1))+xlab("Top Ngrams of order 2 in Blog dataset")

b3<-ggplot(blogs_grams_3,aes(x=reorder(gram, -Freq),y=Freq)) + geom_bar(stat="identity", fill = "darkblue", colour = "black")
b3<-b3+theme(axis.text.x = element_text(angle=30, hjust=1))+xlab("Top Ngrams of order 3 in Blog dataset")
grid.arrange(b1,b2,b3,nrow=3)


n1<-ggplot(news_grams_1,aes(x=reorder(gram, -Freq),y=Freq)) + geom_bar(stat="identity", fill = "darkblue", colour = "black")
n1<-n1+theme(axis.text.x = element_text(angle=30, hjust=1))+xlab("Top Ngrams of order 1 in News dataset")

n2<-ggplot(news_grams_2,aes(x=reorder(gram, -Freq),y=Freq)) + geom_bar(stat="identity", fill = "darkblue", colour = "black")
n2<-n2+theme(axis.text.x = element_text(angle=30, hjust=1))+xlab("Top Ngrams of order 2 in News dataset")

n3<-ggplot(news_grams_3,aes(x=reorder(gram, -Freq),y=Freq)) + geom_bar(stat="identity", fill = "darkblue", colour = "black")
n3<-n3+theme(axis.text.x = element_text(angle=30, hjust=1))+xlab("Top Ngrams of order 3 in News dataset")

grid.arrange(n1,n2,n3,nrow=3)

#Common words in TOP 30 words

common1grams<-Reduce(intersect,list(twitter_grams_1$gram,news_grams_1$gram, blogs_grams_1$gram))
common2grams<-Reduce(intersect,list(twitter_grams_2$gram,news_grams_2$gram, blogs_grams_2$gram))
common3grams<-Reduce(intersect,list(twitter_grams_3$gram,news_grams_3$gram, blogs_grams_3$gram))

