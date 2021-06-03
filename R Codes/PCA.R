mydata1=read.csv(file.choose(),header = T)
mydata2=na.omit(mydata1)
str(mydata2)
mydata2=mydata2[,-c(1,14,22)]

install.packages(c("FactoMineR","factoextra"))
library(factoextra)
library(FactoMineR)
res.famd=FAMD(mydata2,graph=TRUE)
print(res.famd)

eig.val<-get_eigenvalue(res.famd)
eig.val
fviz_eig(res.famd,addlabels = TRUE,ncp=10,ylim=c(0,10))
fviz_famd_var(res.famd,repel=TRUE)
fviz_contrib(res.famd,"var",axes=1)

quanti.var=get_famd_var(res.famd,"quanti.var")
quanti.var
fviz_famd_var(res.famd,"quanti.var",col.var = "contrib",gradient.cols=c("#00AFBB","#E7B800","#FC4E07"),repel=TRUE)

ind=get_famd_var(res.famd)
ind
fviz_famd_ind(res.famd,col.ind = "cos2",gradient.cols = c("#00AFBB","#E7B800","#FC4E07"),repel=TRUE)

#Principal Component Regression
install.packages("pls")
library(pls)
head(mydata2)
mydata3=mydata2[,-C(19,20,21)]
head(mydata3)
set.seed(2)
pcr.fit=pcr(o3~.,data=mydata2,scale=TRUE,validation="CV")
summary(pcr.fit)
lm(o3~.,data=mydata2)
