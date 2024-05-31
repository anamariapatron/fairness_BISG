# Hierarchical data

# Code to generate multinomial variables (multiclass framework)
msample=function(A,B,C){
  Y=rep(NA,B)
  for(i in 1:B){Y[i]=sample(A,size=1,prob=C[i,])}
  return(Y)
}

# Code to generate n observations of multinomial variables
generate3=function(n,x,pb=c(-2,0)){
  set.seed(x)
  X1=runif(n)
  X2=runif(n)
  X3=runif(n)
  s1=pb[1]+X1+X2
  s2=pb[2]-X1+X2
  P1=exp(s1)/(1+exp(s1)+exp(s2))
  P2=exp(s2)/(1+exp(s1)+exp(s2))
  Y=msample(0:2,n,cbind(1-P1-P2,P1,P2))
  df=data.frame(Y=Y,X1=X1,X2=X2,X3=X3)
  return(df)
}

# Generation of a dataset
pb=c(.31,.42)
# Training
DF1=generate3(1000,1,pb=pb)
# Validation
DF2=generate3(500,2,pb=pb)

# Sort categories by decreasing frequency in training set
modalite=names(sort(table(DF1$Y),decreasing = TRUE)) 
# R will consider the most frequent modality of Y as baseline value ?

# One multinomial model
library(nnet)
reg=multinom(as.factor(Y) ~ ., data = DF1)
summary(reg) # We have the coeff for Y=1 and Y=2
mp1=predict (reg, DF1, "probs")
mp2=predict (reg, DF2, "probs")

# Two binomial models
modalite[1]
# First logreg: Y==1 contre Y!=1
reg1=glm((Y==modalite[1])~.,data=DF1,family=binomial)
# Second logreg: Y==2 contre Y!=2 | Y!=1 -> modèle que sur les données qui ont Y!=1
reg2=glm((Y==modalite[2])~.,data=DF1[-which(DF1$Y==modalite[1]),],family=binomial)
# Training
p11=predict (reg1, newdata=DF1, type="response")
p12=predict (reg2, newdata=DF1, type="response")
# Validation
p21=predict (reg1, newdata=DF2, type="response")
p22=predict (reg2, newdata=DF2, type="response")
# Probabilities
mmp1=cbind(p11,(1-p11)*p12,(1-p11)*(1-p12))
mmp2=cbind(p21,(1-p21)*p22,(1-p21)*(1-p22))
colnames(mmp1)=colnames(mmp2)=modalite


