#������������ �������� �������
library(car)
library(ggplot2)
library(caret)
library(corrgram)
library(gvlma)
library(pastecs)
library(rpart)
library(party)
library(glmnet)
library(boot)
options(digits=2)
set.seed(10)
#sink(file)# ����� � ����
#������ ������������� ���������� �������
v1<-rep(1,10)
v2<-rep(2,10)
v3<-rep(3,10)
v4<-rep(4,10)
v5<-rep(5,10)
v6<-rep(6,10)
v7<-rep(7,10)
v8<-rep(8,10)
v9<-rep(9,10)
v10<-rep(10,10)
row1<-DataD[,1]+v1
row2<-DataD[,2]+v2
row3<-DataD[,3]+v3
row4<-DataD[,4]+v4
row5<-DataD[,5]+v5
row6<-DataD[,6]+v6
row7<-DataD[,7]+v7
row8<-DataD[,8]+v8
row9<-DataD[,9]+v9
row10<-DataD[,10]+v10
Data2F<-cbind(row1,row2,row3,row4,row5,row6,row7,row8,row9,row10,DataD[,11])
Data2F<-as.data.frame(Data2F)
Dvar<-apply(Data2F[,-11],1,var)
Dout<-Data2F[,11]
Data2F<-cbind(Dvar,Dout)
Data2F<-as.data.frame(Data2F)# ������������� �������
#����������� ������--------------------------------------------
plot(Data2F$Dvar,Data2F$Dout,pch = 19, col = "red",
     xlab = "����������� ��������",ylab = "������������")#����������� inst �� out
dotplot(Dout~Dvar, data = Data2F)# ������ �������� � �����������
p1<-ggplot(data = Data30G[c(501:550),],aes(x = c(1:50),y = value))
p1+geom_point(shape = 22,size = 4,fill = "blue")# ���������� out
qplot(inst,value,data = Data30G[-c(501:550),],geom = "boxplot",
      colour = I("blue"),outlier.colour = "red")#��������� ��������
#������ ���������� �������
summary(Data30)
i1<-rnorm(50,mean = 2.88,sd = 1.8 )
i2<-rnorm(50,mean = 8.6,sd = 1.76 )
i3<-rnorm(50,mean = 4.2,sd = 4.98 )
i4<-rnorm(50,mean = 5.4,sd = 1.14 )
i5<-rnorm(50,mean = 2.4,sd = 5.91 )
i6<-rnorm(50,mean = 3.4,sd = 6.26 )
i7<-rnorm(50,mean = 2,sd = 1.41 )
i8<-rnorm(50,mean = 3.4,sd = 4.78 )
i9<-rnorm(50,mean = 1.64,sd = 1.93 )
i10<-rnorm(50,mean = 2.2,sd = 2.43 )
DataN<-as.data.frame(cbind(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10))
DataNorm<-cbind(DataN,Data30[,12])# ������� � ��������� �������������� �������
stat.desc(Data30[,-1],basic = T,desc = T,norm = T,p=0.95)#�������� ����������
stat.desc(DataNorm,basic = T,desc = T,norm = T,p=0.95)#���������� �������������� ������
par(mfrow = c(2,5))# �������� ������������� ����������
qqnorm(Data30$inst1,pch = 16,col = "red", main = " inst1")
qqnorm(Data30$inst2,pch = 16,col = "red", main = " inst2")
qqnorm(Data30$inst3,pch = 16,col = "red", main = " inst3")
qqnorm(Data30$inst4,pch = 16,col = "red", main = " inst4")
qqnorm(Data30$inst5,pch = 16,col = "red", main = " inst5")
qqnorm(Data30$inst6,pch = 16,col = "red", main = " inst6")
qqnorm(Data30$inst7,pch = 16,col = "red", main = " inst7")
qqnorm(Data30$inst8,pch = 16,col = "red", main = " inst8")
qqnorm(Data30$inst9,pch = 16,col = "red", main = " inst9")
qqnorm(Data30$inst10,pch = 16,col = "red", main = " inst10")
par(mfrow = c(1,4))
plot(density(Data30$inst4),lwd = 2, col = "blue",main = "inst4")
plot(density(Data30$inst5),lwd = 2, col = "blue",main = "inst5")
plot(density(Data30$inst6),lwd = 2, col = "blue",main = "inst6")
plot(density(Data30$inst7),lwd = 2, col = "blue",main = "inst7")
par(mfrow = c(1,2))
qqnorm(Data30$out,pch = 16,col = "red", main = "out")
plot(density(Data30$out),lwd = 2, col = "blue",main = "out")
freq<-Data30G[-c(501:550), ]#����� out
freq<-as.data.frame(freq)
qplot(value,data = freq,geom = "freqpoly",binwidth = 0.5,col = "red",
      main = "������� ������������� ������ ���������� inst1-10")#������� ������
corrgram (Data30, order=TRUE, lower.panel=panel.shade,
          upper.panel=panel.pie, text.panel=panel.txt,
          diag.panel=panel.minmax)#������������ �������� ������
#������ ��������������
corrgram (ModelingData[,-1], order=TRUE, lower.panel=panel.shade,
          upper.panel=panel.pie, text.panel=panel.txt,
          diag.panel=panel.minmax)# ��������������������

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)#��������� ��������� ����� ����� �����������
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y)*6)
  txt <- format(c(r, 0.2), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.2/strwidth(txt)
  text(0.55,0.5, txt, cex = cex.cor * r)
}
pairs(Data30[,-1], lower.panel = panel.smooth, upper.panel = panel.cor,
      gap=0, row1attop=FALSE)#��������� �������� ������������ 1.5
#�������������############################################################################
fit2F<-lm(Dout~Dvar,data = Data2F)#������������� ������
summary(fit2F)
confint(fit2F)
par(mfrow = c(2,2))
plot(fit2F)
par(mfrow = c(1,1))
Predfit2F<-predict(fit2F)
plot(Data2F$Dvar,Data2F$Dout,pch = 16,col = "red",
                     xlab = "����������� ��������",ylab = "������������")
abline(fit2F,lwd = 3,col = "blue")#��������� �������������
checkfitlmSimplefit2F<-gvlma(fit2F)#�������� ����� ���������� � �������� ������
summary(checkfitlmSimplefit2F)#���������� � �������� ������� �� ����������� !
#�������� �������������� �������� 
fit2FPoly2<-lm(Dout~poly(Dvar,2),data = Data2F)
fit2FPoly3<-lm(Dout~poly(Dvar,3),data = Data2F)
fit2FPoly4<-lm(Dout~poly(Dvar,4),data = Data2F)
fit2FPoly5<-lm(Dout~poly(Dvar,5),data = Data2F)
fit2FPoly6<-lm(Dout~poly(Dvar,6),data = Data2F)
fit2FPoly7<-lm(Dout~poly(Dvar,7),data = Data2F)
anova(fit2F,fit2FPoly2,fit2FPoly3,fit2FPoly4,fit2FPoly5,fit2FPoly6,fit2FPoly7)#fit2FPoly3-�����
checkfitlmSimple2FPoly3<-gvlma(fit2FPoly3)#�������� ����� ���������� � �������� ������
summary(checkfitlmSimple2FPoly3)#���������� � �������� ������� �� ����������� !
xa<-c(2:7)
ya<-c(0,1.72,0,1.49,0.13,0)
plot(xa,ya,type = "b",pch= 20,lty = 1,lwd = 2,col = "blue",xlab = "������� ��������",ylab = "F ��������")
qqnorm(Data2F$Dvar,pch = 16,col = "red", main = " Dvar")#������������� Dvar
qqnorm(Data2F$Dout,pch = 16,col = "red", main = " Dout")#������������� Dout
summary(fit2FPoly3)#������ �������� 3 �������
#������������� ������������� ������
Data30lm<-lm(out~inst1+inst2+inst3+inst4+inst5+
               inst6+inst7+inst8+inst9+inst10,data = Data30)
summary(Data30lm)
checkfitlmSimpleData30lm<-gvlma(Data30lm)#�������� ����� ���������� � �������� ������
#��������� ��������� ����������
Data30lmStep1<-step(Data30lm, trace = 1)
summary(Data30lmStep1)
Data30lmStep1res<-lm(out ~ inst1 + inst4 + inst5 + inst7 + inst8 + inst9 + 
                       inst10, data = Data30)#��������� �������� ���������� ���������
#��������� ������ �����-����������-----------------------------------
fit2FTrain<-train(Dout~Dvar,data = Data2F, method = 'lm', 
      trControl = trainControl(method = "cv"))
print(fit2FTrain)#�������������-������ ������
Data30lmTrain<-train(out~inst1+inst2+inst3+inst4+inst5+
                    inst6+inst7+inst8+inst9+inst10,data = Data30,method = 'lm', 
                  trControl = trainControl(method = "cv"))
print(Data30lmTrain)#������
Data30lmStep1resTrain<-train(out~inst1+inst4+inst5+
                               inst7+inst8+inst9+inst10,data = Data30,method = 'lm', 
                             trControl = trainControl(method = "cv"))
print(Data30lmStep1resTrain)#�����������
#����� ������������� ���������� fit2FTrain  
fit2FTrainB<-train(Dout~Dvar,data = Data2F, method = 'lm', 
                   trControl = trainControl(method = "boot"))
print(fit2FTrainB)
summary(fit2FTrainB)
ggplot(data = Data2F, aes(x = Dvar, Dout)) + geom_point() +
  geom_smooth(method = "lm", se = T) +
  geom_rug(color = "gray70", sides = "tr") +
  ylab("������������") + xlab("����������� ��������")# ������������� ������ � �������������� �����������
#��������� ������������� ��������� ��� ������ fit2F ����������
boot.fn<- function(data,index) + coefficients(lm(Dout~Dvar,data = Data2F,subset = index))
set.seed(10)
fit2FBoot<-boot(Data2F,boot.fn,1000)
plot(fit2FBoot)  
quantile(fit2FBoot$t, c(0.025, 0.975))#������ � ������� ������� �������������� ���������  
boot.ci(fit2FBoot, type = "bca")#������������� ��������  ���������  
#������������ �������� ������ �����������
set.seed(10) 
ctrl <- gafsControl(functions = mutation, method = "cv",  
                    verbose = FALSE, returnResamp = "final") 
lmProfGafs <- gafs(Data30[,-1], Data30$out,  
                   iters = 10, # 10 ��������� 
                   gafsControl = ctrl) 
print(lmProfGafs) 
lm_gafs.a1 <- lm(out~inst1+inst2+inst3+inst4+inst5+
                   inst6+inst7+inst8+inst9+inst10, 
                 data = Data30) 
GenLm<-train(out~inst2+inst4+inst9+inst10, 
      data = Data30, method = 'lm', 
      trControl = trainControl(method = "cv")) 
print(GenLm)
#��������� ���������
x <- model.matrix(out ~inst1+inst2+inst3+inst4+inst5+
                    inst6+inst7+inst8+inst9+inst10 ,data=Data30)[,-1] 
grid=10^seq(10,-2,length=100)
Data30lmTrainRig <- glmnet(x,Data30[,-1]$out, alpha=0, lambda=grid)   
print(Data30lmTrainRig)
plot(Data30lmTrainRig, xvar =  "lambda", label = TRUE, lwd=2) 
#��������� �����
Data30lmTrainlasso <- glmnet(x,Data30[,-1]$out, alpha=1, lambda=grid)   
print(Data30lmTrainlasso)
plot(Data30lmTrainlasso, xvar =  "lambda", label = TRUE, lwd=2) 
#������������ ����������� ��������� ��������� ��������
set.seed(10)
grid.train = seq(0,10,length=50)
Data30lmTrainRigLO <- train(as.data.frame(x), Data30[,-1]$out, 
                            method='glmnet', 
                            tuneGrid = expand.grid(.lambda = grid.train, .alpha = 0), 
                            trControl = trainControl(method = "LOOCV"))
print(Data30lmTrainRigLO$results)
print(Data30lmTrainRigLO$bestTune)
coef(Data30lmTrainRigLO$finalModel,Data30lmTrainRigLO$bestTune$lambda)
#������������ �����- ���������� ��������� �������� 
Data30lmTrainRigCV <- train(as.data.frame(x), Data30[,-1]$out, 
                            method='glmnet', 
                            tuneGrid = expand.grid(.lambda = grid.train, .alpha = 0), 
                            trControl = trainControl(method = "cv"))
print(Data30lmTrainRigCV$results)
print(Data30lmTrainRigCV$bestTune)
coef(Data30lmTrainRigCV$finalModel,Data30lmTrainRigCV$bestTune$lambda)

#������������ ����������� ��������� �������� �����
Data30lmTrainlassoLO <- train(as.data.frame(x), Data30[,-1]$out, 
                              method='glmnet', 
                              tuneGrid = expand.grid(.lambda = grid.train, .alpha = 1), 
                              trControl = trainControl(method = "LOOCV")) 
print(Data30lmTrainlassoLO$results)
print(Data30lmTrainlassoLO$bestTune)
coef(Data30lmTrainlassoLO$finalModel,Data30lmTrainlassoLO$bestTune$lambda)
coef.cv.glmnet()
#������������ �����- ���������� �������� �����
grid.trainCV = seq(-1,5,length=50) 
Data30lmTrainlassoCV <- train(as.data.frame(x), Data30[,-1]$out, 
                        method='glmnet', 
                        tuneGrid = expand.grid(.lambda = grid.trainCV, .alpha = 1), 
                        trControl = trainControl(method = "cv")) 
print(Data30lmTrainlassoCV$results)
print(Data30lmTrainlassoCV$bestTune)
coef(Data30lmTrainlassoCV$finalModel,Data30lmTrainlassoCV$bestTune$lambda)


