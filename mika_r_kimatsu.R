
##総合演習課題

##Q1(データの取り込みとデータの確認)
#setwd("C:/data/econome3") これシャープ外すの忘れないで！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！

fukuoka_y10_y20<-read.csv("fukuoka_y10_20_v3.csv")

str(fukuoka_y10_y20) #structure(構造)dataframeの詳しい内容
head(fukuoka_y10_y20) #最初の部分
tail(fukuoka_y10_y20) #最後の部分
colnames(fukuoka_y10_y20) #列名
ncol(fukuoka_y10_y20) #列数
nrow(fukuoka_y10_y20) #行数
dim(fukuoka_y10_y20) #dimension(次元)行数、列数

##データが確認できた


##Q2(使用データの抽出)
##Q2-1
d<-subset(fukuoka_y10_y20,fukuoka_y10_y20$m_name!="kitakyushu_c" & fukuoka_y10_y20$m_name != "fukuoka_c")


##Q2-2
subset(lapply(d, class),lapply(d,class)=="character")
##実行結果より，pop_female_over100の項目は数値であることが望ましいのに文字列になっていることがわかる．


##Q2-3
d10<-subset(d,d$year==2010)
d15<-subset(d,d$year==2015)
d20<-subset(d,d$year==2020)


##Q2-4
d_r1<-subset(d,d$region_code4==1)
d_r2<-subset(d,d$region_code4==2)
d_r3<-subset(d,d$region_code4==3)
d_r4<-subset(d,d$region_code4==4)

##Q3(変数の基本統計量の計算)
install.packages("psych")
library(psych)


##Q3-1
#地域の働く力
describe(d20[15:19],na.rm = TRUE)
describe(d15[15:19],na.rm = TRUE)
#地域のにぎわい力
describe(d20[20:24],na.rm = TRUE)
describe(d15[20:24],na.rm = TRUE)
#乳幼児サポート力
describe(d20[25:26],na.rm = TRUE)
describe(d15[25:26],na.rm = TRUE)
#子育て基盤力
describe(d20[29:32],na.rm = TRUE)
describe(d15[29:32],na.rm = TRUE)
#家族・地域の絆力
describe(d20[33:37],na.rm = TRUE)
describe(d15[33:37],na.rm = TRUE)
#結婚・出産関連の変数
which(colnames(d)=="total_fertility_rate")
which(colnames(d)=="marital_rate30_34")
which(colnames(d)=="first_child_tfr30_34")
which(colnames(d)=="second_child_tfr30_34")
which(colnames(d)=="third_child_tfr30_34")

vec_marriage<-c(42,46,145,152,159)

describe(d20[vec_marriage],na.rm = TRUE)
describe(d15[vec_marriage],na.rm = TRUE)

install.packages("tidyverse")
library(tidyverse)
#合計特殊出生率top5
(top_n(d20,5,total_fertility_rate) |> arrange(-total_fertility_rate))[c("m_name","total_fertility_rate")]
#合計特殊出生率worst5
(top_n(d20,5,-total_fertility_rate) |> arrange(total_fertility_rate))[c("m_name","total_fertility_rate")]


##Q3-2
var_list<-c(15:26,29:37,vec_marriage)
change<-drop_na(d20[var_list]-d15[var_list])
region_work_diff<-data.frame(change)
lapply(region_work_diff,mean)


##Q4(グループごとに変数の平均値の計算)
#完全失業率
round(tapply(d$unemp_rate20_34,list(d$region_4,d$year),mean),3)
#市町村民所得
round(tapply(d$resident_income_pc,list(d$region_4,d$year),mean),0)
#合計特殊出生率
round(tapply(d$total_fertility_rate,list(d$region_4,d$year),mean),3)
#有配偶率
round(tapply(d$marital_rate30_34,list(d$region_4,d$year),mean),3)


##Q5(折れ線グラフの作成)
dataframe_total_fertility_rate<-tapply(d$total_fertility_rate,list(d$year,d$region_4),mean)
matplot(dataframe_total_fertility_rate,xaxt="n",type="l",ylab = "合計特殊出生率")
axis(1,1:3,rownames(dataframe_total_fertility_rate))
legend("topright",legend = colnames(dataframe_total_fertility_rate),col = c("black","red","green","blue"),lty = c(1,2,2,2))

dataframe_marital_rate<-tapply(d$marital_rate30_34,list(d$year,d$region_4),mean)
matplot(dataframe_marital_rate,xaxt="n",type="l",ylab = "有配偶率（30～34歳）")
axis(1,1:3,rownames(dataframe_marital_rate))
legend("topright",legend = colnames(dataframe_marital_rate),col = c("black","red","green","blue"),lty = c(1,2,2,2))

##Q6(散布図)
#縦軸を市町村民所得とした
plot(d20$resident_income_pc[d20$region_code4==1],d20$marital_rate30_34[d20$region_code4==1],col=("black"),pch=1,xlab="市町村民所得",ylab = "有配偶率")
points(d20$resident_income_pc[d20$region_code4==2],d20$marital_rate30_34[d20$region_code4==2],col=("red"),pch=2)
points(d20$resident_income_pc[d20$region_code4==3],d20$marital_rate30_34[d20$region_code4==3],col=("green"),pch=3)
points(d20$resident_income_pc[d20$region_code4==4],d20$marital_rate30_34[d20$region_code4==4],col=("blue"),pch=4)
legend("topright",legend = c("北九州","福岡","筑後","筑豊"),col = c("black","red","green","blue"),pch = c(1,2,3,4),cex=0.5)

##Q7(箱ひげ図)
#boxplot(d20$marital_rate30_34[d20$region_code4==1],d20$marital_rate30_34[d20$region_code4==2],d20$marital_rate30_34[d20$region_code4==3],d20$marital_rate30_34[d20$region_code4==4])
boxplot(marital_rate30_34~region_4,data=d20,xlab = "region",ylab = "marital rate")

##Q8(相関係数)
d_r1_20<-subset(d_r1,d_r1$year==2020)
d_r2_20<-subset(d_r2,d_r1$year==2020)
d_r3_20<-subset(d_r3,d_r1$year==2020)
d_r4_20<-subset(d_r4,d_r1$year==2020)

cor_1<-data.frame(cor(d_r1_20[var_list]))
cor_2<-data.frame(cor(d_r2_20[var_list]))
cor_3<-data.frame(cor(d_r3_20[var_list]))
cor_4<-data.frame(cor(d_r4_20[var_list]))
#dataframeのtotal_fertility_rateが求める相関係数の値である。そこから、北九州では新設住宅着工戸数、福岡では有配偶率、筑後は第２子の出生率、筑豊は第１子の出生率が合計特殊出生率と相関が高いことがわかる
#有配偶率も同様にして求められる。dataframeから、北九州は第２子の出生率、福岡では合計特殊出生率、筑後では新設住宅着工戸数、筑豊では市町村民所得が相関が高い


##Q9(重回帰分析と推計後のパラメータの利用)
