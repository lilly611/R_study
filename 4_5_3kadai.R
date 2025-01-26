#1 仮定：市町村の人口について，しきい値（非連続な点）で追加収入を得るかどうかということ以外が起きていない
# この仮定が成立しないシナリオは，例えば学校を建てるか否かの基準がその人口のしきい値になっている，というようなシナリオがある．
# この例における回帰分断デザインの利点は上に示した因果関係の仮定が満たされやすいことだが，例えば人口が多い市町村としきい値付近でかろうじて追加収入を得た市町村では別の因果関係が働いている可能性があり一般に拡張できるとは言いづらい．

brazil<-read.csv("transfer.csv")
#しきい値10188,13584,16980
threshold1<-10188
threshold2<-13584
threshold3<-16980

#それぞれしきい値に近いものに1のラベル付
brazil$threshold1<- ifelse(brazil$pop82 < (threshold2 + threshold1)/2,1,0)
brazil$threshold2<- ifelse((threshold2 + threshold1)/2 < brazil$pop82 & brazil$pop82 < (threshold3 + threshold2)/2,1,0)
brazil$threshold3<- ifelse((threshold3 + threshold2)/2 < brazil$pop82,1,0)
#一番近いしきい値との差
brazil$dif<- ifelse(brazil$threshold1==1,brazil$pop82-threshold1, ifelse(brazil$threshold2==1,brazil$pop82- threshold2,brazil$pop82- threshold3))
#対応するしきい値で正規化した差
brazil$dif.normalize<- ifelse(brazil$threshold1==1,brazil$dif / threshold1 * 100, ifelse(brazil$threshold2==1,brazil$dif / threshold2 * 100,brazil$dif / threshold3 *100))
#対応するしきい値について，追加収入を得たかどうかラベル付
brazil$get<-ifelse(brazil$dif>0,1,0)
#1つ目のしきい値+-3%以内の部分集合をつくる
threshold1.3pc<-subset(brazil,subset = (abs(dif.normalize) <= 3 & threshold1==1))

#平均教育年数（係数が因果効果）
educ_fit<-lm(educ80 ~ get,data = threshold1.3pc)
educ_fit

#識字率
liter_fit<-lm(literate91~get,data = threshold1.3pc)
liter_fit

#貧困率
pover_fit<-lm(poverty80~get,data = threshold1.3pc)
pover_fit

#人口も絡めて図示
educ_fit2<-lm(educ80~get*pop82,data = threshold1.3pc)
plot(threshold1.3pc$pop82,threshold1.3pc$educ80,main="平均教育年数")
abline(v= threshold1)
y1.range=c(min(threshold1.3pc$pop82))
y1<-predict(educ_fit2,newdata = data.frame(pop82=min(threshold1.3pc$pop82):threshold1,get=0))
lines(min(threshold1.3pc$pop82):threshold1,y1,col="blue")
y2<-predict(educ_fit2,newdata = data.frame(pop82=threshold1:max(threshold1.3pc$pop82),get=1))
lines(threshold1:max(threshold1.3pc$pop82),y2,col="blue")

liter_fit2<-lm(literate91~get*pop82,data = threshold1.3pc)
plot(threshold1.3pc$pop82,threshold1.3pc$literate91,main="識字率")
abline(v= threshold1)
y1.liter<-predict(liter_fit2,newdata = data.frame(pop82=min(threshold1.3pc$pop82):threshold1,get=0))
lines(min(threshold1.3pc$pop82):threshold1,y1.liter,col="blue")
y2.liter<-predict(liter_fit2,newdata = data.frame(pop82=threshold1:max(threshold1.3pc$pop82),get=1))
lines(threshold1:max(threshold1.3pc$pop82),y2.liter,col="blue")

pover_fit2<-lm(poverty80~get*pop82,data = threshold1.3pc)
plot(threshold1.3pc$pop82,threshold1.3pc$poverty80,main="貧困率")
abline(v= threshold1)
y1.pover<-predict(pover_fit2,newdata = data.frame(pop82=min(threshold1.3pc$pop82):threshold1,get=0))
lines(min(threshold1.3pc$pop82):threshold1,y1.pover,col="blue")
y2.pover<-predict(pover_fit2,newdata = data.frame(pop82=threshold1:max(threshold1.3pc$pop82),get=1))
lines(threshold1:max(threshold1.3pc$pop82),y2.pover,col="blue")

#図からしきい値の前後で回帰直線に差があることがわかる。
#1つ目のしきい値関して、追加収入を得るかどうかが平均年数や識字率、貧困率に関わることが示唆される。
#貧困率に関して回帰直線が上向きである．予測の一つとして，
#追加収入を得てかつ人口が少ないと単純に一人あたりに使うことができる資金が増えるため上向きだと考えられるが，
#はじめで示したようにこの回帰直線は図全体（特に図の端）には適用できないことがあることに注意すべきである。