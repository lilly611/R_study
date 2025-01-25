florida <-read.csv("florida.csv")
fit2<-lm(Buchanan00~Perot96,data=florida)
fit2

TSS2<-sum((florida$Buchanan00-mean(florida$Buchanan00))^2)
SSR2<-sum(resid(fit2)^2)
(TSS2-SSR2)/TSS2

R2<-function(fit){
  resid<-resid(fit)
  y<-fitted(fit)+resid
  TSS<-sum((y-mean(y))^2)
  SSR<-sum(resid^2)
  R2<-(TSS-SSR)/TSS
  return(R2)
}
R2(fit2)

fit2summary<-summary(fit2)
fit2summary$r.squared

#R2(fit1)

plot(fitted(fit2),resid(fit2),xlim=c(0,1500),ylim=c(-750,2500),xlab="当てはめ値",ylab="残差")
abline(h=0)

florida$county[resid(fit2)==max(resid(fit2))]

florida.pb<-subset(florida,subset = (county!="PalmBeach"))
fit3<-lm(Buchanan00~Perot96,data=florida.pb)
fit3
R2(fit3)

plot(fitted(fit3),resid(fit3),xlim=c(0,1500),ylim=c(-750,2500),xlab="当てはめ値",ylab="残差",main="パームビーチ郡を除いた残差プロット")
abline(h=0)
plot(florida$Perot96,florida$Buchanan00,xlab = "1966年のペローの得票数",ylab = "2000年のブキャナンの得票数")
abline(fit2,lty="dashed")
abline(fit3)
text(30000,3250,"パームビーチ郡")
text(30000,1500,"パームビーチ郡を含んだ回帰直線")
text(30000,400,"パームビーチ郡を\n除いた回帰直線")


#第14回
women<-read.csv("women.csv")
mean(women$female[women$reserved==1])
mean(women$female[women$reserved==0])
mean(women$water[women$reserved==1]-mean(women$water[women$reserved==0]))
mean(women$irrigation[women$reserved==1]-mean(women$irrigation[women$reserved==0]))
lm(water~reserved,data = women)
lm(irrigation~reserved,data=women)

social<-read.csv("social.csv")
#自分の環境ではコメントアウトしている方じゃないとlevels関数が動かなかった
#social<-read.csv("social.csv",stringsAsFactors = T)
levels(social$messages)
fit<-lm(primary2006~messages,data=social)
fit

social$Control<-ifelse(social$messages=="Control",1,0)
social$Hawthorne<-ifelse(social$messages=="Hawthorne",1,0)
social$Neighbors<-ifelse(social$messages=="Neighbors",1,0)
lm(primary2006~Control+Hawthorne+Neighbors,data=social)
unique.messages<-data.frame(messages=unique(social$messages))
unique.messages
predict(fit,newdata = unique.messages)
tapply(social$primary2006,social$messages,mean)

fit.noint<-lm(primary2006~-1+messages,data = social)
fit.noint
coef(fit)["messagesNeighbors"]-coef(fit)["messagesControl"]

mean(social$primary2006[social$messages=="Neighbors"]-mean(social$primary2006[social$messages=="Control"]))

adjR2<-function(fit){
  resid<-resid(fit)
  y<-fitted(fit)+resid
  n<-length(y)
  TSS.adj<-sum((y-mean(y))^2)/(n-1)
  SSR.adj<-sum(resid^2)/(n-length(coef(fit)))
  R2.adj<-1-SSR.adj/TSS.adj
  return(R2.adj)
}
adjR2(fit)

social.voter<-subset(social,primary2004==1)
ate.voter<-mean(social.voter$primary2006[social.voter$messages=="Neighbors"])-mean(social.voter$primary2006[social.voter$messages=="Control"])
ate.voter

social.nonvoter<-subset(social,primary2004==0)
ate.nonvoter<-mean(social.nonvoter$primary2006[social.nonvoter$messages=="Neighbors"])-mean(social.nonvoter$primary2006[social.nonvoter$messages=="Control"])
ate.nonvoter

ate.voter-ate.nonvoter

social.neighbor<-subset(social,(messages=="Control")|(messages=="Neighbors"))
fit.int<-lm(primary2006~primary2004+messages+primary2004:messages,data=social.neighbor)
fit.int

#公式HPの正誤表によると2006ではなく2008
social.neighbor$age<-2008-social.neighbor$yearofbirth
summary(social.neighbor$age)

fit.age<-lm(primary2006~age*messages,data=social.neighbor)
fit.age

age.neighbor<-data.frame(age=seq(from=25,to=85,by=20),messages="Neighbors")
age.control<-data.frame(age=seq(from=25,to=85,by=20),messages="Control")
ate.age<-predict(fit.age,newdata = age.neighbor)-predict(fit.age,newdata = age.control)
ate.age

fit.age2<-lm(primary2006~age+I(age^2)+messages+age:messages+I(age^2):messages,data = social.neighbor)
fit.age2

yT.hat<-predict(fit.age2,newdata = data.frame(age=25:85,messages="Neighbors"))
yC.hat<-predict(fit.age2,newdata = data.frame(age=25:85,messages="Control"))
plot(x=25:85,y=yT.hat,type = "l",xlim = c(20,90),ylim = c(0,0.5),xlab="年齢",ylab="予測された投票率")
lines(x=25:85,y=yC.hat,lty="dashed")
text(45,0.48,"Neighbors条件")
text(55,0.25,"Control条件")

plot(x=25:85,y=yT.hat-yC.hat,type = "l",xlim = c(20,90),ylim = c(0,0.1),xlab="年齢",ylab="推定された平均トリートメント効果")


MPs<-read.csv("MPs.csv")
MPs.labour<-subset(MPs, subset = (party=="labour"))
MPs.tory<-subset(MPs, subset = (party=="tory"))

labour.fit1<-lm(ln.net~margin,data = MPs.labour[MPs.labour$margin<0,])
labour.fit2<-lm(ln.net~margin,data = MPs.labour[MPs.labour$margin>0,])
tory.fit1<-lm(ln.net~margin,data = MPs.tory[MPs.tory$margin<0,])
tory.fit2<-lm(ln.net~margin,data = MPs.tory[MPs.tory$margin>0,])
y1l.range<-c(min(MPs.labour$margin),0)
y2l.range<-c(0,max(MPs.labour$margin))
y1.labour<-predict(labour.fit1,newdata = data.frame(margin=y1l.range))
y2.labour<-predict(labour.fit2,newdata = data.frame(margin=y2l.range))

y1t.range<-c(min(MPs.tory$margin),0)
y2t.range<-c(0,max(MPs.tory$margin))
y1.tory<-predict(tory.fit1,newdata = data.frame(margin=y1t.range))
y2.tory<-predict(tory.fit2,newdata = data.frame(margin=y2t.range))

plot(MPs.labour$margin,MPs.labour$ln.net,main = "労働党",xlim = c(-0.5,0.5),ylim = c(6,18),xlab = "勝利マージン",ylab = "死亡時純資産の対数")
abline(v=0,lty="dashed")
lines(y1l.range,y1.labour,col="blue")
lines(y2l.range,y1.labour,col="blue")

plot(MPs.tory$margin,MPs.tory$ln.net,main = "保守党",xlim = c(-0.5,0.5),ylim = c(6,18),xlab = "勝利マージン",ylab = "死亡時純資産の対数")
abline(v=0,lty="dashed")
lines(y1t.range,y1.tory,col="blue")
lines(y2t.range,y2.tory,col="blue")

tory.MP<-exp(y2.tory[1])
tory.MP

tory.nonMP<-exp(y1.tory[2])
tory.nonMP

tory.MP-tory.nonMP

tory.fit3<-lm(margin.pre~margin,data = MPs.tory[MPs.tory$margin<0,])
tory.fit4<-lm(margin.pre~margin,data = MPs.tory[MPs.tory$margin>0,])
coef(tory.fit4)[1]-coef(tory.fit3)[1]
