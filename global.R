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


# First is loading the datasets with the 2,3 and 4 ngrams

load("dt2")
load("dt3")
load("dt4")

#throw away data to increase response speed
dt2<-dt2[which(dt2$count>7),]
dt3<-dt3[which(dt3$count>2),]
dt4<-dt4[which(dt4$count>2),]


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


#Searching Algorithm
mostfrequenttable <-function(string){
  #remove stopwords
  string<-removeWords(string,stopwords("english"))  
  #trim and remove multiple spaces
  string<-gsub("\\s+", " ", str_trim(string))
  string<-tolower(string)
  string<-gsub(" ","_",string)
  numberofwords<-str_count(string,"_")+1
  output<-NULL
  if (numberofwords>2){
    output<-dt4[searchpatterns(dt4,word(string,start=-3,end=-1,sep="_")),]    
  }
  if (numberofwords>1){
    tempoutput<-NULL
    tempoutput<-dt3[searchpatterns(dt3,word(string,start=-2,end=-1,sep="_")),]    
    output<-rbind(output,tempoutput)
  }
  if (numberofwords>0){
    tempoutput<-dt2[searchpatterns(dt2,word(string,start=-1,end=-1,sep="_")),]    
    output<-rbind(output,tempoutput)
  }
  
  #I remove those outputs whose last word is that last word of the search string
  #I this did because of speed reason when originally searching the dataframes
  lastword<-word(string,start=-1,sep="_")
  output<-output[!lastword==word(output$ngram,start=-1,sep="_"),]    
  
  #Finally order (note I have not used percentage)
  output<-output[order(-output$count),]
  return(output)
}

# mostfrequent<-function (string, datos,position){
#   string<-removeWords(string,stopwords("english"))  
#   string<-gsub("\\s+", " ", str_trim(string))
#   string<-tolower(string)
#   lastword<-tolower(word(string,start=-1, sep=" "))
#   datos<-datos[position,1]
#   if (wordposition(datos,lastword)==0){
#     return ("NOT_FOUND")
#   }
#   position<-wordposition(datos,lastword)
#   return(word(datos,start=position+1,end=position+1,sep="_"))
# }

removeinput<-function (string, datos){
  string<-removeWords(string,stopwords("english"))  
  string<-gsub("\\s+", " ", str_trim(string))
  string<-tolower(string)
  lastword<-tolower(word(string,start=-1, sep=" "))
  for (i in 1:nrow(datos)){
    datos_temp<-datos[i,1]  
    if (wordposition(datos_temp,lastword)==0){
      datos[i,1]<-" "
    }else{    
    position<-wordposition(datos_temp,lastword)
  datos[i,1]<-word(datos_temp,start=position+1,end=position+1,sep="_")
  }
  }
  return(datos)
}


wordposition<-function(string,pattern){
  i<-1
  condition<-TRUE
  numberofwords<-str_count(string,"_")+1
  while (condition){
    #protection againt non finding pattern
    nextword<-word(string,start=i,end=i,sep="_")
    if(i>numberofwords){
      return (0)
    }
    else if (is.na(nextword)){
      i<-i+1
    } else if (nextword==pattern){
      condition<-FALSE
    }else{
      i<-i+1  
    }
  }
  return (i)
}
