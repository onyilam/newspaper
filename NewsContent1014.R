library(NLP)
library(tm)
library(rJava)
library(Rwordseg)
library(tmcn)
library(RWeka)


setwd("/Users/onyi/Dropbox/newspaper/dictionary")
options(max.print=1000000)

#reading dictionaries
dictlist<-readLines('dictlist_hklocal.txt')
insertWords(dictlist)
dictlist2<-readLines('dictlist_pol.txt')
insertWords(dictlist2)
dictlist3<-readLines('dictlist_polCN.txt')
insertWords(dictlist3)
dictlist4<-readLines('dictlist_news.txt')
insertWords(dictlist4)
dictlist5<-readLines('lpnewwords3_edit.txt')
insertWords(dictlist5)
dictlist6<-readLines('lpnewwords4_edit.txt')
insertWords(dictlist6)
dictlist7<-readLines('wwnewwords3_edit.txt')
insertWords(dictlist7)
dictlist8<-readLines('wwnewwords4_edit.txt')
insertWords(dictlist8)
dictlist9<-readLines('newwords_edit.txt')
insertWords(dictlist9)
dictlist10<-readLines('newwords2_edit.txt')
insertWords(dictlist10)
dictlist11<-readLines('wwnewwords_edit.txt')
insertWords(dictlist11)
dictlist12<-readLines('wwnewwords2_edit.txt')
insertWords(dictlist12)

skipWords <- function(x) removeWords(x, myStopWords)
removeEngNum <-function(word) {
  gsub("[A-Za-z0-9]", "", word)
}
myStopWords <- c(stopwordsCN(), "湯家驊", "吳靄儀", "陳文敏", "李志喜","這些", "不能","不同","這個","一些","他們", "我們","必須","是否","進一步","基本上","換言之","不論","什麼","沒有","可能","筆者","一下","一些副","大家","第一個","愈來愈","從來不","不論是",
                 "不少","星期一", "星期三", "星期二","星期四","法政","隨筆", "星期五","星期日","星期六","見報","一個","星期","梁家傑","副刊時代","李柱銘","的","會","是","不算","余若薇","甚麼","因為","只不過","那麼","一份","一篇","讀者","覺得","差不多","第一個","第二步",
                 "上星期","一部分","為什麼","大多數","大部分","事實上","最大的","換句話說","一個一個","實際上","問題","越來越","可能性","一系列","文匯", "社評", "記者", "這些", "不能","不同","這個","一些","他們", "我們","必須","是否","進一步","基本上","換言之","不論","什麼","沒有","可能",
                 "不少","星期一", "星期三", "星期二","星期四","評論", "昨日","同時","上半年","不僅","人士","已經","十分","一位","為何","應該","其實","這裏","最近","似乎","究竟","很多","此次","實在","當然","一次","一天","今天","一面","一支","可惜","有人","第一次","怎樣",
                 "文匯報","事實上","的","會","是","不算","下半年","目前","認為","當局","有關","表示","指出","方面","提出","相關","雖然","加上","實際上","問題","本報","近年來","更何况","時代","當中","一點","一件","一句","一種","下去","下來","情況","昨天","萬象","情况","始終")

source("segmentCN2.R")

#read apple newspapers
setwd("/Users/onyi/Dropbox/newspaper/HK_headline/Apple/Quarterly1014")
filelist <- list.files(pattern='^apple.*txt')
Sys.setlocale("LC_ALL","zh_CN.utf-8")
aplist <- lapply(filelist, function(x)read.delim(x,row.names=NULL, fileEncoding="UTF-8",stringsAsFactors=FALSE)) 
aplist2<- lapply(1:length(aplist), function(x)apply( aplist[[x]],2,paste,collapse=" ")) #remove all paragraph breaks, i.e. combine all lines into single paragraph
aplist_corpora <- lapply(1:length(aplist2), function(i) Corpus(VectorSource(aplist2[[i]])))
funcs <- list(removePunctuation, removeNumbers, stripWhitespace, removeEngNum)
aplist_corpora1 <- lapply(1:length(aplist_corpora), function(i) tm_map(aplist_corpora[[i]], FUN = tm_reduce, tmFuns = funcs))
funcs2 <- list(segmentCN2, skipWords)
aplist_corpora2 <- lapply(1:length(aplist_corpora1), function(i) tm_map(aplist_corpora1[[i]], FUN = tm_reduce, tmFuns = funcs2))
clean <- list(PlainTextDocument)
aplist_clean <- lapply(1:length(aplist_corpora2), function(i) tm_map(aplist_corpora2[[i]], FUN = tm_reduce, tmFuns = clean))
aplist_dtms <- lapply(1:length(aplist_clean), function(i) TermDocumentMatrix(aplist_clean[[i]], control = list(wordLengths = c(2,6),
                                                                                                               removePunctuation = TRUE,
                                                                                                               removeNumbers = TRUE,
                                                                                                               stopwords = myStopWords)))
                                                                                                          
#read oriental daily
setwd("/Users/onyi/Dropbox/newspaper/HK_headline/Oriental/Quarterly1014")
filelist <- list.files(pattern='^ori.*txt')
orlist <- lapply(filelist, function(x)read.delim(x,row.names=NULL, fileEncoding="UTF-8",stringsAsFactors=FALSE)) 
orlist2<- lapply(1:length(orlist), function(x)apply(orlist[[x]],2,paste,collapse=" ")) #remove all paragraph breaks, i.e. combine all lines into single paragraph
orlist_corpora <- lapply(1:length(orlist2), function(i) Corpus(VectorSource(orlist2[[i]])))
orlist_corpora1 <- lapply(1:length(orlist_corpora), function(i) tm_map(orlist_corpora[[i]], FUN = tm_reduce, tmFuns = funcs))
orlist_corpora2 <- lapply(1:length(orlist_corpora1), function(i) tm_map(orlist_corpora1[[i]], FUN = tm_reduce, tmFuns = funcs2))
orlist_clean <- lapply(1:length(orlist_corpora2), function(i) tm_map(orlist_corpora2[[i]], FUN = tm_reduce, tmFuns = clean))
orlist_dtms <- lapply(1:length(orlist_clean), function(i) TermDocumentMatrix(orlist_clean[[i]], control = list(wordLengths = c(2,6),
                                                                                                               removePunctuation = TRUE,
                                                                                                               removeNumbers = TRUE,
                                                                                                               stopwords = myStopWords)))


#combine the frequencies of all terms together
dtms_headline <-lapply(  1:length(aplist_dtms), function(i) c(aplist_dtms[[i]],  orlist_dtms[[i]],recursive=T) )

#convert dtms into data.frame (for merge later)
temp<- lapply(1:length(dtms_headline), function(i) inspect( dtms_headline[[i]]) )
temp2<- lapply(1:length(dtms), function(i) as.data.frame(temp[[i]], stringsAsFactors = FALSE) ) 
lapply(1:length(dtms), function(i)  write.csv(temp2[[i]], file=paste("temp", i, ".csv", sep="")))
filelist2 <- list.files(pattern='^temp.*csv')
fileNum<-as.numeric(gsub('^temp([0123456789]*)\\.csv$','\\1',filelist2))
filelist3<-filelist2[order(fileNum)];
FreqTab_HL <- lapply(filelist3, function(x)read.csv(x, header=T)) 
for (i in 1:length(FreqTab)) {
  colnames(FreqTab_HL[[i]])<-c("words","apFreq","orFreq")
}
do.call(file.remove,list(filelist3)) #remove temp file

#merge highest chi-square phrases with the newspaper words
TopBi_HL <- lapply(1: length(TopBi), function(i) merge(FreqTab_HL[[i]], TopBi[[i]], by = "words"))
TopTri_HL <- lapply(1: length(TopTri), function(i) merge(FreqTab_HL[[i]], TopTri[[i]], by = "words"))
TopQuad_HL <- lapply(1: length(TopQuad), function(i) merge(FreqTab_HL[[i]], TopQuad[[i]], by = "words"))

#rank  top phrases by chi-square
TopBi_HL <- lapply(1: length(TopBi), function(i) TopBi_HL[[i]][order(TopBi_HL[[i]]$chi_sq, decreasing=TRUE),])
TopTri_HL <- lapply(1: length(TopTri), function(i) TopTri_HL[[i]][order(TopTri_HL[[i]]$chi_sq, decreasing=TRUE),])
TopQuad_HL <- lapply(1: length(TopQuad), function(i) TopQuad_HL[[i]][order(TopQuad_HL[[i]]$chi_sq, decreasing=TRUE),])
