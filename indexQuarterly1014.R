#calculate the relative frequencies of the terms used by newpapers, ww and lp used by the top phrases

TopBi_HL <- lapply (1: length(TopBi_HL), function(i) head(TopBi_HL[[i]], 100))
TopTri_HL <- lapply (1: length(TopBi_HL), function(i) head(TopTri_HL[[i]], 100))
TopQuad_HL <- lapply (1: length(TopQuad_HL), function(i) head(TopQuad_HL[[i]], 100))

for (i in 1:length(TopBi_HL)) {
  TopBi_HL[[i]]["lpRelFreq"] <- TopBi_HL[[i]]["lpFreq"]/sum(TopBi_HL[[i]]["lpFreq"])
  TopBi_HL[[i]]["wwRelFreq"] <- TopBi_HL[[i]]["wwFreq"]/sum(TopBi_HL[[i]]["wwFreq"])
  TopBi_HL[[i]]["apRelFreq"] <- TopBi_HL[[i]]["apFreq"]/sum(TopBi_HL[[i]]["apFreq"])
  TopBi_HL[[i]]["orRelFreq"] <- TopBi_HL[[i]]["orFreq"]/sum(TopBi_HL[[i]]["orFreq"])
  
  #difference in rel frequency of lp and ww
  TopBi_HL[[i]]["DiffFreq"] <- TopBi_HL[[i]]["lpRelFreq"]-TopBi_HL[[i]]["wwRelFreq"]
}

for (i in 1:length(TopTri_HL)) {
  TopTri_HL[[i]]["lpRelFreq"] <- TopTri_HL[[i]]["lpFreq"]/sum(TopTri_HL[[i]]["lpFreq"])
  TopTri_HL[[i]]["wwRelFreq"] <- TopTri_HL[[i]]["wwFreq"]/sum(TopTri_HL[[i]]["wwFreq"])
  TopTri_HL[[i]]["apRelFreq"] <- TopTri_HL[[i]]["apFreq"]/sum(TopTri_HL[[i]]["apFreq"])
  TopTri_HL[[i]]["orRelFreq"] <- TopTri_HL[[i]]["orFreq"]/sum(TopTri_HL[[i]]["orFreq"])
  
  #difference in rel frequency of lp and ww
  TopTri_HL[[i]]["DiffFreq"] <- TopTri_HL[[i]]["lpRelFreq"]-TopTri_HL[[i]]["wwRelFreq"]
  
}

for (i in 1:length(TopQuad_HL)) {
  TopQuad_HL[[i]]["lpRelFreq"] <- TopQuad_HL[[i]]["lpFreq"]/sum(TopQuad_HL[[i]]["lpFreq"])
  TopQuad_HL[[i]]["wwRelFreq"] <- TopQuad_HL[[i]]["wwFreq"]/sum(TopQuad_HL[[i]]["wwFreq"])
  TopQuad_HL[[i]]["apRelFreq"] <- TopQuad_HL[[i]]["apFreq"]/sum(TopQuad_HL[[i]]["apFreq"])
  TopQuad_HL[[i]]["orRelFreq"] <- TopQuad_HL[[i]]["orFreq"]/sum(TopQuad_HL[[i]]["orFreq"])
  
  #difference in rel frequency of lp and ww
  TopQuad_HL[[i]]["DiffFreq"] <- TopQuad_HL[[i]]["lpRelFreq"]-TopQuad_HL[[i]]["wwRelFreq"]
  
}

##regress relative frequency of the term of each newspaper'son the difference of frequecies between lp and ww
#stack the bigrams, trigrams and quadgrams together

stacked <- lapply(1:length(TopBi_HL), function(i) rbind(TopBi_HL[[i]],TopTri_HL[[i]],TopQuad_HL[[i]]))
stacked <- lapply(1:length(TopBi_HL), function(i) stacked[[i]][order(stacked[[i]]$chi_sq, decreasing=TRUE),])
stacked <- lapply(1:length(TopBi_HL), function(i) head(stacked[[i]],80))


#reg 
betaAp = matrix(NA, nrow = length(TopQuad_HL), ncol = 1) #5 periods of data
betaOr = matrix(NA, nrow = length(TopQuad_HL), ncol = 1) 
for (i in 1:length(stacked) ){
  fit = lm(stacked[[i]]$apRelFreq~stacked[[i]]$DiffFreq)
  coefs <- coef(fit) 
  temp<-t(as.data.frame(coefs))
  colnames(temp) <- c("intercept", "apple")
  temp2<- as.data.frame(temp)
  betaAp[ i , 1 ] <- temp2$apple
}


for (i in 1:length(stacked) ){
  fit = lm(stacked[[i]]$orRelFreq~stacked[[i]]$DiffFreq)
  coefs <- coef(fit) 
  temp<-t(as.data.frame(coefs))
  colnames(temp) <- c("intercept", "oriental")
  temp2<- as.data.frame(temp)
  betaOr[ i , 1 ] <- temp2$oriental
}


#merge betas together

betas<-cbind(betaAp,  betaOr)
beta.df<- as.data.frame(betas)
colnames(beta.df)<-c("Apple","Oriental")


#compute difference between betas
beta.df$diff <- beta.df$Apple - beta.df$Oriental


setwd("/Users/onyi/Dropbox/newspaper")  
AdsTS<-read.csv(file="AdsQuarterly2.csv", header=TRUE, sep=",")
AdsTS<-AdsTS[complete.cases(AdsTS),]
AdsTS$diff<- beta.df$Apple - beta.df$Oriental

AdsTS$yq<- factor(AdsTS$yq, levels=c('2010/1','2010/2', '2010/3',
                                     '2010/4', '2011/1',
                                     '2011/2','2011/3',
                                     '2011/4', '2012/1', '2012/2',
                                     '2012/3','2012/4','2013/1',  '2013/2',
                                     '2013/3','2013/4','2014/1',
                                     '2014/2', '2014/3', '2014/4'   ))

#generate dummy variables for quarter
inds2 <- model.matrix(~ factor(AdsTS$Quarter) - 1) 
colnames(inds2) <- c("q1", "q2","q3","q4")
inds2<-subset(inds2, select=-c(factor(q1)))
AdsTS<-cbind(AdsTS,inds2)

library(forecast)
#generate time series data
AdsByQ <- ts(AdsTS[,2:length(AdsTS)], start = 1, end = 20)
c<-tslm(AppleShare2~diff+q3+q2+q4,AdsByQ)
summary(c)


b<-tslm(AppleShare2~diff+trend+q3+q2+q4,AdsByQ)
summary(b)

a<-tslm(AppleShare2~diff+ApReadShare+q3+q2+q4,AdsByQ)
summary(a)

c2<-tslm(ApShareAll~diff+q3+q2+q4,AdsByQ)
summary(c2)

b2<-tslm(ApShareAll~diff+trend+q3+q2+q4,AdsByQ)
summary(b2)

a2<-tslm(ApShareAll~diff+ApReadShare+q3+q2+q4,AdsByQ)
summary(a2)



library(ggplot2)
ggplot(AdsTS, aes(x=yq, group=1)) + 
#geom_line(aes(y = ApShareAll))+
geom_line(aes(y = diff))+
  theme_bw() +
  ylab("Difference in Slant between \n Apple and Oriental") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  annotate("text", x='2012/3', y=0.6, label= "National Education\n controversies", size=3) + 
  annotate("text", x='2014/4', y=0.65, label= "Umbrella \n Movement", size=3) + 
  annotate("text", x='2014/1', y=0.6, label= "Knife Attack on \n Kevin Lau ", size=3) + 
  annotate("text", x='2014/2', y=0.15, label= "White Papers \n controversies ", size=3) + 
  ylim(-0.1, 0.67)+
  xlab("Year/Quarter")  

ggplot(AdsTS, aes(x=yq, group=1)) + 
  geom_line(aes(y = AppleShare2))+
  theme_bw() +
  ylab("Ad Share of Apple relative to Oriental") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Year/Quarter")  +
  scale_y_continuous(breaks=seq(0.43,.53, 0.01))



#calculate the number of PB and PD phrases
for (i in 1:length(stacked)) {
with(stacked[[i]], c(
       print( sum(lpFreq> wwFreq))
   ))
}