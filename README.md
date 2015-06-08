# Text-mining
options(stringsAsFactors = FALSE)


candidates=c("Romney", "Obama")
pathname="C:/Users/manusharma5/Documents/Speeches"

#Clean
cleancorpus=function(corpus){
  corpus.tmp=tm_map(corpus,removePunctuation)
  corpus.tmp=tm_map(corpus,stripWhitespace)
  corpus.tmp=tm_map(corpus,tolower)
  corpus.tmp=tm_map(corpus,removeWords,stopwords("English"))
  return(corpus.tmp)
}

# Generate TDM
generateTDM=function(candidates,pathname){
  
  s.dir=sprintf("%s/%s",pathname,candidates)
  s.cor=Corpus(DirSource(directory = s.dir,encoding = "UTF8"))
  s.cor.cl=cleancorpus(s.cor)
  s.tdm=TermDocumentMatrix(s.cor.cl)
  s.tdm=removeSparseTerms(s.tdm,0.7)
  result =list(name=candidates,tdm=s.tdm)
  
}
result

#run term document matrix on all candidates
tdm=lapply(candidates,generateTDM,path=pathname)
tdm
View(tdm)

#bind candidate name to term document matrix
data.matrix(tdm[["tdm"]])
str(tdm)
bindcandidateTDM=function(tdm){
  s.mat=t(data.matrix(tdm[["tdm"]]))

s.df=as.data.frame(s.mat,stringsAsFactors=FALSE)
s.df=cbind(s.df,rep(tdm[["name"]],nrow(s.df)))
colnames(s.df)[ncol(s.df)]="targetcandidate"
return(s.df)
}
s.mat
#append candidate field to tdm
candTDM=lapply(tdm,bindcandidateTDM)
str(candTDM)

#rbind candidate tdm
tdm.stack=do.call(rbind.fill, candTDM)
tdm.stack[is.na(tdm.stack)]=0
head(tdm.stack)
nrow(tdm.stack)
ncol(tdm.stack)

#hold out sampling
train.idx=sample(nrow(tdm.stack),ceiling(nrow(tdm.stack)*0.8))
test.idx=(1:nrow(tdm.stack))[-train.idx]
train.idx
test.idx


#extract candidate name

tdm.cand=tdm.stack[,"targetcandidate"]
tdm.stack.nl=tdm.stack[,!colnames(tdm.stack) %in% "targetcandidate"]

#model
knn.pred=knn(tdm.stack.nl[train.idx,],tdm.stack.nl[test.idx,],tdm.cand[train.idx])

#confusion matrix

conf.matrix=table("predictions"=knn.pred,"Actual"=tdm.cand[test.idx])
conf.matrix
#Accuracy

Accuracy=sum(diag(conf.matrix))/length(test.idx)*100
Accuracy

