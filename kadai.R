#csvの読み込み
fukuoka<-read.csv("./datasets/fukuoka_y10_20_v1.csv")

#必要なデータだけ抽出
total_fertility_rate<-fukuoka[,c("year","m_code","total_fertility_rate")]
total_fertility_rate_2015<-total_fertility_rate[total_fertility_rate$year==2015,]
total_fertility_rate_2020<-total_fertility_rate[total_fertility_rate$year==2020,]
#標準化
total_fertility_rate_2015$standardization<-scale(total_fertility_rate_2015$total_fertility_rate)
total_fertility_rate_2020$standardization<-scale(total_fertility_rate_2020$total_fertility_rate)
#自治体で結合
total_fertility_rate_y15_y20<-merge(total_fertility_rate_2015,total_fertility_rate_2020,by="m_code")



#割合
length(total_fertility_rate_y15_y20$m_code[total_fertility_rate_y15_y20$standardization.x<total_fertility_rate_y15_y20$standardization.y])/length(total_fertility_rate_y15_y20$m_code)
#割合（講義スライドのやり方）（評価式でTRUEorFalseつまり1or0のベクトルを作って平均を取っているから実質割合が求まる 0と1からなるベクトルの和を個数で割っているから）
mean(total_fertility_rate_y15_y20$standardization.x<total_fertility_rate_y15_y20$standardization.y)



#下位25%タイル
mean((total_fertility_rate_y15_y20$standardization.x<total_fertility_rate_y15_y20$standardization.y)[total_fertility_rate_y15_y20$standardization.x<=quantile(total_fertility_rate_y15_y20$standardization.x,0.25)])
#0.7894737

#上位25%タイル
mean((total_fertility_rate_y15_y20$standardization.x<total_fertility_rate_y15_y20$standardization.y)[total_fertility_rate_y15_y20$standardization.x>=quantile(total_fertility_rate_y15_y20$standardization.x,0.75)])
#0.3684211
#確かに下位のほうが上位より大きい割合であることが確認できた


#重回帰
#2010年
reg_2010<-lm(fukuoka$total_fertility_rate[fukuoka$year==2010]~fukuoka$regular_work_ratio[fukuoka$year==2010]+fukuoka$large_retail_store_per10000[fukuoka$year==2010],data <- fukuoka)
summary(reg_2010)
#regular_work_ratioのt値が2.401となった

#2015
reg_2015<-lm(fukuoka$total_fertility_rate[fukuoka$year==2015]~fukuoka$regular_work_ratio[fukuoka$year==2015]+fukuoka$large_retail_store_per10000[fukuoka$year==2015],data <- fukuoka)
summary(reg_2015)

#2020
reg_2020<-lm(fukuoka$total_fertility_rate[fukuoka$year==2020]~fukuoka$regular_work_ratio[fukuoka$year==2020]+fukuoka$large_retail_store_per10000[fukuoka$year==2020],data <- fukuoka)
summary(reg_2020)