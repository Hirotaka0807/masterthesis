##Results#####################################################################################################################################3
library(AER)
library(stargazer)

#Statistics
mean(manusharechange_9500)
mean(manusharechange_0005)
mean(manusharechange_0510)

##表6.1.1 Gross Benchmark##
stargazer(TGresult_9510_2sls_2,TGresult_9510_2sls_4,omit = c("region", "time"),no.space = T, title = "Table 6.1.1 Results of Gross trade case",covariate.labels = "GrossIPWj", type = "html")
summary(TGresult_9510_2sls_4)
stargazer(TGTYPEresult_9510_2sls_4,omit = c("region", "time"),no.space = T, title = "Table 6.1.1 Results of Gross trade case",covariate.labels = c("GrossFNLIPWj","GrossINTIPWj"), type = "html")
summary(TGTYPEresult_9510_2sls_4)
#ダミーだけ盛りと、コントロール含みでいい気がする。
#強めに負。
#コロプレス分析
library(choroplethr)
library(choroplethrAdmin1)

#各期の平均を県ごとに取る？

region <- c("hokkaido","aomori","iwate","miyagi","akita","yamagata","fukushima","ibaraki","tochigi","gunma","saitama","chiba","tokyo","kanagawa","niigata","toyama","ishikawa","fukui","yamanashi","nagano","gifu","shizuoka","aichi","mie","shiga","kyoto","osaka","hyogo","nara","wakayama","tottori","shimane","okayama","hiroshima","yamaguchi","tokushima","kagawa","ehime","kochi","fukuoka","saga","nagasaki","kumamoto","oita","miyazaki","kagoshima","okinawa")
TGrossIPW_plot <- cbind(TGrossIPWj_9500,TGrossIPWj_0005,TGrossIPWj_0510)

meandata <- function(data){
y <- NULL
  for(i in 1:47){
    y <- rbind(y,mean(data[i,]))
  }
return(y)
}
TGrossIPW_mean <- meandata(TGrossIPW_plot)

rownames(TGrossIPW_mean) <- region
TGrossIPW_mean <- data.frame(region=region,value =TGrossIPW_mean)

mean(GrossIPW_plot[1,])
View(GrossIPW_mean)
View(TGrossIPW_mean[order(TGrossIPW_mean$value),])
mean(TGrossIPW_mean$value)

View(TGrossIPW_mean)

#最大：秋田（6.206）、福井、鳥取、長野、福島
#最小：沖縄県（1.869）、北海道、香川、愛知、福岡
#平均値 4.079

mean(manushare_2010 - manushare_1995)

(sum(empi_1995)/sum(workpop1995)-sum(empi_2010)/sum(workpop2010))*100

mean(TGrossIPW_mean$value)
coefficients(TGresult_9510_2sls_4)[2]
summary(TGresult_9510_2sls_4)
View(TGrossIPW_mean)


choro_TGIPW9500 <- admin1_choropleth(country.name = "japan",df = data.frame(region=region,value=GrossIPWj_9500),num_colors = 4,title = "Figure 5.1.1. Gross Import Exposure 1995-2000",legend = "GrossIPW")
choro_TGIPW0005 <- admin1_choropleth(country.name = "japan",df = data.frame(region=region,value=GrossIPWj_0005),num_colors = 4,title = "Figure 5.1.2. Gross Import Exposure 2000-2005",legend = "GrossIPW")
choro_TGIPW0510 <- admin1_choropleth(country.name = "japan",df = data.frame(region=region,value=GrossIPWj_0510),num_colors = 4,title = "Figure 5.1.3. Gross Import Exposure 2005-2010",legend = "GrossIPW")
choro_TGIPW <- admin1_choropleth(country.name = "japan",df = TGrossIPW_mean,num_colors = 4,title = "Figure 6.1.1. Gross Import Exposure 1995-2010",legend = "GrossIPW")

choro_GIPW9500
choro_GIPW0005
choro_GIPW0510
choro_TGIPW

GrossIPW_plot <- cbind(WVACHNIPWj_9500,WVACHNIPWj_0005,WVACHNIPWj_0510)
VAIPW_mean <- meandata(GrossIPW_plot)
choro_VAIPW9500 <- admin1_choropleth(country.name = "japan",df = data.frame(region=region,value=WVACHNIPWj_9500),num_colors = 4,title = "Figure 5.2.1. VA Import Exposure 1995-2000",legend = "VAIPW")
choro_VAIPW0005 <- admin1_choropleth(country.name = "japan",df = data.frame(region=region,value=WVACHNIPWj_0005),num_colors = 4,title = "Figure 5.2.2. VA Import Exposure 2000-2005",legend = "VAIPW")
choro_VAIPW0510 <- admin1_choropleth(country.name = "japan",df = data.frame(region=region,value=WVACHNIPWj_0510),num_colors = 4,title = "Figure 5.2.3. VA Import Exposure 2005-2010",legend = "VAIPW")
choro_VAIPW <- admin1_choropleth(country.name = "japan",df = data.frame(region=region,value=VAIPW_mean),num_colors = 4,title = "Figure 6.2.1. VA Import Exposure 1995-2010",legend = "VAIPW")


ZVAIPW <- (VAIPW_mean-mean(VAIPW_mean))/sd(VAIPW_mean)
ZVAIPW
ZTGrossIPW <- (TGrossIPW_mean$value-mean(TGrossIPW_mean$value))/sd(TGrossIPW_mean$value)

IPWdif <- data.frame(region=region,value=(ZVAIPW-ZTGrossIPW))
IPWdif

choro_IPWdif <- admin1_choropleth(country.name = "japan",df = IPWdif,num_colors = 4,title = "Figure 6.2.2. Exposure Difference between Gross and VA 1995-2010",legend = "GrossIPW-VAIPW")
choro_IPWdif

mean(VAIPW_mean)

choro_VAIPW9500
choro_VAIPW0005
choro_VAIPW

median(GrossIPWj_9500)

#四分位差×効果

#平均×効果

##Gross,VAの比較表##
stargazer(WVACHNresult_9510_2sls_2,WVACHNresult_9510_2sls_4,TGresult_9510_2sls_4,omit = c("region", "time"),no.space = T, title = "Table 6.2.1 Results of VA trade case",covariate.labels = c("VAIPWj","GrossIPWj"), type = "html")

#四分位差×効果
mean(WVACHNIPWj_9510)

#平均×効果

sd(TGrossIPWj_9500)
sd(TGrossIPWj_0005)
sd(TGrossIPWj_0510)
sd(TGrossIPWj_9510)
sd(TGrossIPWFNLj_9500)
sd(TGrossIPWFNLj_0005)
sd(TGrossIPWFNLj_0510)
sd(TGrossIPWFNLj_9510)
sd(TGrossIPWINTj_9500)
sd(TGrossIPWINTj_0005)
sd(TGrossIPWINTj_0510)
sd(TGrossIPWINTj_9510)

mean(WVACHNIPWj_9500)
mean(WVACHNIPWj_0005)
mean(WVACHNIPWj_0510)
mean(WVACHNIPWj_9510)
sd(WVACHNIPWj_9500)
sd(WVACHNIPWj_0005)
sd(WVACHNIPWj_0510)
sd(WVACHNIPWj_9510)

mean(WVACHNFNLIPWj_9500)
mean(WVACHNFNLIPWj_0005)
mean(WVACHNFNLIPWj_0510)
mean(WVACHNFNLIPWj_9510)
sd(WVACHNFNLIPWj_9500)
sd(WVACHNFNLIPWj_0005)
sd(WVACHNFNLIPWj_0510)
sd(WVACHNFNLIPWj_9510)

mean(WVACHNINTIPWj_9500)
mean(WVACHNINTIPWj_0005)
mean(WVACHNINTIPWj_0510)
mean(WVACHNINTIPWj_9510)
sd(WVACHNINTIPWj_9500)
sd(WVACHNINTIPWj_0005)
sd(WVACHNINTIPWj_0510)
sd(WVACHNINTIPWj_9510)


CHNVCHNIPWj_9500
mean(CHNVCHNIPWj_9500)
mean(CHNVCHNIPWj_0005)
mean(CHNVCHNIPWj_0510)
mean(CHNVCHNIPWj_9510)
sd(CHNVCHNIPWj_9500)
sd(CHNVCHNIPWj_0005)
sd(CHNVCHNIPWj_0510)
sd(CHNVCHNIPWj_9510)

mean(OTRVCHNIPWj_9500)
mean(OTRVCHNIPWj_0005)
mean(OTRVCHNIPWj_0510)
mean(OTRVCHNIPWj_9510)
sd(OTRVCHNIPWj_9500)
sd(OTRVCHNIPWj_0005)
sd(OTRVCHNIPWj_0510)
sd(OTRVCHNIPWj_9510)

#TYPEresult
stargazer(WLDVCHNTYPEresult_9510_2sls_2,WLDVCHNTYPEresult_9510_2sls_4,omit = c("region", "time"),no.space = T, title = "Table 5.2 Results of VA trade case",covariate.labels = c("VAFNLIPWj","VAINTIPWj"), type = "html")

##2. CHNVCHNresult
summary(CHNVCHNresult_9510_2sls_4)
stargazer(CHNVCHNresult_9510_2sls_2,CHNVCHNresult_9510_2sls_4,omit = c("region", "time"),no.space = T, title = "Table 5.4 Results of DVA, FVA",covariate.labels = c("DVAIPWj","FVAIPWj"), type = "html")



stargazer(Gresult_9510_2sls_4,CHNVWLDresult_9510_2sls_4)
stargazer(CHNVWLDresult_9510_2sls_1,CHNVWLDresult_9510_2sls_3,CHNVWLDresult_9510_2sls_2,CHNVWLDresult_9510_2sls_4,Gresult_9510_2sls_4)
#1と4だけでいいし、ダミーは全部入れなくていいかも。
stargazer(CHNVWLDresult_9510_2sls_1,CHNVWLDresult_9510_2sls_4,Gresult_9510_2sls_4,omit = "region",no.space = T,title = "表4.1　最終需要に含まれる中国の総付加価値での結果",covariate.labels = c("VAIPWj","GrossIPWj"))
summary(CHNVWLDresult_9510_2sls_4)
summary(Gresult_9510_2sls_4)

#TYPEresult
stargazer(CHNVWLDTYPEresult_9510_2sls_1,CHNVWLDTYPEresult_9510_2sls_4,omit = "region",no.space = T,covariate.labels = c("FNLVAIPWj","INTVAIPWj"))

summary(CHNVWLDTYPEresult_9510_2sls_4)

#CHNCHNresult
stargazer(CHNVCHNresult_9510_2sls_1,CHNVCHNresult_9510_2sls_4,omit = "region",no.space = T,covariate.labels = c("DVAIPWj","FVAIPWj"))
summary(CHNVCHNresult_9510_2sls_4)

summary(CHNVCHNTYPEresult_O_9510_2sls_4)

stargazer(CHNVCHNresult_9510_2sls_1,CHNVCHNresult_9510_2sls_4,omit = "region",no.space = T,covariate.labels = c("DVAIPWj","FVAIPWj"))


#############################################################################
##dependent variables
manushare_1995 <- empi_1995/WorkPop_JAP_1995$workingage*100
manushare_2000 <- empi_2000/WorkPop_JAP_2000$workingage*100
manushare_2005 <- empi_2005/WorkPop_JAP_2005$workingage*100
manushare_2010 <- empi_2010/WorkPop_JAP_2010$workingage*100

empi_1995
WorkPop_JAP_1995
manushare_1995

mean(WorkPop_JAP_2010$workingage)
mean(WorkPop_JAP_2005$workingage)
mean(WorkPop_JAP_2000$workingage)

mean(empi_2010)
mean(empi_2005)
mean(empi_2000)

mean(manushare_2010)
mean(manushare_2005)
mean(manushare_2000)

manusharechange_0510 <- manushare_2010-manushare_2005
manusharechange_0005 <- manushare_2005-manushare_2000
manusharechange_9500 <- manushare_2000-manushare_1995

manusharechange_9510 <- rbind(manusharechange_9500,manusharechange_0005,manusharechange_0510)
manusharechange_9505 <- rbind(manusharechange_9500,manusharechange_0005)
manusharechange_0510 <- rbind(manusharechange_0005,manusharechange_0510)

manusharechange_9510

weight_1995 <- WorkPop_JAP_1995$workingage/sum(WorkPop_JAP_1995$workingage)*100
weight_2000 <- WorkPop_JAP_2000$workingage/sum(WorkPop_JAP_2000$workingage)*100
weight_2005 <- WorkPop_JAP_2005$workingage/sum(WorkPop_JAP_2005$workingage)*100

weight_1995 <- t(weight_1995)
weight_2000 <- t(weight_2000)
weight_2005 <- t(weight_2005)

mean(manusharechange_9500)
mean(manusharechange_0005)
mean(manusharechange_0510)
sd(manusharechange_9500)
sd(manusharechange_0005)
sd(manusharechange_0510)

mean(CHNVCHNIPWj_9500)
mean(CHNVCHNIPWj_0005)
mean(CHNVCHNIPWj_0510)
sd(CHNVCHNIPWj_9500)
sd(CHNVCHNIPWj_0005)
sd(CHNVCHNIPWj_0510)

mean(OTRVCHNIPWj_9500)
mean(OTRVCHNIPWj_0005)
mean(OTRVCHNIPWj_0510)
sd(OTRVCHNIPWj_9500)
sd(OTRVCHNIPWj_0005)
sd(OTRVCHNIPWj_0510)

##Control, weights################################################################
#全期間
womenpart_9510 <- Control$Women
foreign_9510 <- Control$Foreign
college_9510 <- Control$College

region1_9510 <- Control$region1
region2_9510 <- Control$region2
region3_9510 <- Control$region3
region4_9510 <- Control$region4
region5_9510 <- Control$region5
region6_9510 <- Control$region6
region7_9510 <- Control$region7
region8_9510 <- Control$region8

time9500_9510 <- Control$time_95
time0005_9510 <- Control$time_00
time0510_9510 <- Control$time_05

weight_9510 <-rbind(weight_1995,weight_2000,weight_2005)
weight_9510 <- as.vector(weight_9510)

manushare_start_9510 <- rbind(manushare_1995,manushare_2000,manushare_2005)

View(manusharechange_9510)

#前期
womenpart_9505 <- womenpart_9510[1:94]
foreign_9505 <- foreign_9510[1:94]
college_9505 <- college_9510[1:94]

region1_9505 <- region1_9510[1:94]
region2_9505 <- region2_9510[1:94]
region3_9505 <- region3_9510[1:94]
region4_9505 <- region4_9510[1:94]
region5_9505 <- region5_9510[1:94]
region6_9505 <- region6_9510[1:94]
region7_9505 <- region7_9510[1:94]
region8_9505 <- region8_9510[1:94]

time9500_9505 <- time9500_9510[1:94]
time0005_9505 <- time0005_9510[1:94]

weight_9505 <-rbind(weight_1995,weight_2000)
weight_9505 <- as.vector(weight_9505)

manushare_start_9505 <- rbind(manushare_1995,manushare_2000)

#後期
womenpart_0010 <- womenpart_9510[48:141]
foreign_0010 <- foreign_9510[48:141]
college_0010 <- college_9510[48:141]

region1_0010 <- region1_9510[48:141]
region2_0010 <- region2_9510[48:141]
region3_0010 <- region3_9510[48:141]
region4_0010 <- region4_9510[48:141]
region5_0010 <- region5_9510[48:141]
region6_0010 <- region6_9510[48:141]
region7_0010 <- region7_9510[48:141]
region8_0010 <- region8_9510[48:141]

time0005_0010 <- time0005_9510[48:141]
time0510_0010 <- time0510_9510[48:141]

weight_0010 <-rbind(weight_2000,weight_2005)
weight_0010 <- as.vector(weight_0010)

manushare_start_0010 <- rbind(manushare_2000,manushare_2005)

######################################################################################################################################
##IPW(VA)
IPW <- function(importchange,empji,empj,empi){
  IPW <- NULL
  for(i in 1:47){
  Importjperworkeri <- importchange/empi[i,1]
  weight <- empji[i,]/empj
  ipw <- sum(weight*Importjperworkeri)
  IPW <- rbind(IPW,ipw)
  }
  IPWt <<- IPW
  ##定義した変数に代入されて返る。
}

#0. Gross(TiVA)###########
TGrossIPWj_9500 <- IPW(C_ExJPN_9500,empji_1995,empj_1995,empi_1995)
TGrossIPWj_9500 <- matrix(TGrossIPWj_9500,47,1)
rownames(TGrossIPWj_9500) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")
TGrossIPWj_0005 <- IPW(C_ExJPN_0005,empji_2000,empj_2000,empi_2000)
TGrossIPWj_0005 <- matrix(TGrossIPWj_0005,47,1)
rownames(TGrossIPWj_0005) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")
TGrossIPWj_0510 <- IPW(C_ExJPN_0510,empji_2005,empj_2005,empi_2005)
TGrossIPWj_0510 <- matrix(TGrossIPWj_0510,47,1)
rownames(TGrossIPWj_0510) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")
TGrossIPWu_9500 <- IPW(C_ExUSA_9500,empji_1985,empj_1985,empi_1985)
TGrossIPWu_9500 <- matrix(TGrossIPWu_9500,47,1)
rownames(TGrossIPWu_9500) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")
TGrossIPWu_0005 <- IPW(C_ExUSA_0005,empji_1990,empj_1990,empi_1990)
TGrossIPWu_0005 <- matrix(TGrossIPWu_0005,47,1)
rownames(TGrossIPWu_0005) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")
TGrossIPWu_0510 <- IPW(C_ExUSA_0510,empji_1995,empj_1995,empi_1995)
TGrossIPWu_0510 <- matrix(TGrossIPWu_0510,47,1)
rownames(TGrossIPWu_0510) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

TGrossIPWj_9510 <- rbind(TGrossIPWj_9500,TGrossIPWj_0005,TGrossIPWj_0510)
TGrossIPWu_9510 <- rbind(TGrossIPWu_9500,TGrossIPWu_0005,TGrossIPWu_0510)

TGrossIPWFNLj_9500 <- IPW(C_ExJPNFNL_9500,empji_1995,empj_1995,empi_1995)
TGrossIPWFNLj_9500 <- matrix(TGrossIPWFNLj_9500,47,1)
rownames(TGrossIPWFNLj_9500) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")
TGrossIPWFNLj_0005 <- IPW(C_ExJPNFNL_0005,empji_2000,empj_2000,empi_2000)
TGrossIPWFNLj_0005 <- matrix(TGrossIPWFNLj_0005,47,1)
rownames(TGrossIPWFNLj_0005) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")
TGrossIPWFNLj_0510 <- IPW(C_ExJPNFNL_0510,empji_2005,empj_2005,empi_2005)
TGrossIPWFNLj_0510 <- matrix(TGrossIPWFNLj_0510,47,1)
rownames(TGrossIPWFNLj_0510) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

TGrossIPWFNLu_9500 <- IPW(C_ExUSAFNL_9500,empji_1985,empj_1985,empi_1985)
TGrossIPWFNLu_9500 <- matrix(TGrossIPWFNLu_9500,47,1)
rownames(TGrossIPWFNLu_9500) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")
TGrossIPWFNLu_0005 <- IPW(C_ExUSAFNL_0005,empji_1990,empj_1990,empi_1990)
TGrossIPWFNLu_0005 <- matrix(TGrossIPWFNLu_0005,47,1)
rownames(TGrossIPWFNLu_0005) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")
TGrossIPWFNLu_0510 <- IPW(C_ExUSAFNL_0510,empji_1995,empj_1995,empi_1995)
TGrossIPWFNLu_0510 <- matrix(TGrossIPWFNLu_0510,47,1)
rownames(TGrossIPWFNLu_0510) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

TGrossIPWFNLj_9510 <- rbind(TGrossIPWFNLj_9500,TGrossIPWFNLj_0005,TGrossIPWFNLj_0510)
TGrossIPWFNLu_9510 <- rbind(TGrossIPWFNLu_9500,TGrossIPWFNLu_0005,TGrossIPWFNLu_0510)

TGrossIPWINTj_9500 <- IPW(C_ExJPNINT_9500,empji_1995,empj_1995,empi_1995)
TGrossIPWINTj_9500 <- matrix(TGrossIPWINTj_9500,47,1)
rownames(TGrossIPWINTj_9500) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")
TGrossIPWINTj_0005 <- IPW(C_ExJPNINT_0005,empji_2000,empj_2000,empi_2000)
TGrossIPWINTj_0005 <- matrix(TGrossIPWINTj_0005,47,1)
rownames(TGrossIPWINTj_0005) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")
TGrossIPWINTj_0510 <- IPW(C_ExJPNINT_0510,empji_2005,empj_2005,empi_2005)
TGrossIPWINTj_0510 <- matrix(TGrossIPWINTj_0510,47,1)
rownames(TGrossIPWINTj_0510) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

TGrossIPWINTu_9500 <- IPW(C_ExUSAINT_9500,empji_1985,empj_1985,empi_1985)
TGrossIPWINTu_9500 <- matrix(TGrossIPWINTu_9500,47,1)
rownames(TGrossIPWINTu_9500) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")
TGrossIPWINTu_0005 <- IPW(C_ExUSAINT_0005,empji_1990,empj_1990,empi_1990)
TGrossIPWINTu_0005 <- matrix(TGrossIPWINTu_0005,47,1)
rownames(TGrossIPWINTu_0005) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")
TGrossIPWINTu_0510 <- IPW(C_ExUSAINT_0510,empji_1995,empj_1995,empi_1995)
TGrossIPWINTu_0510 <- matrix(TGrossIPWINTu_0510,47,1)
rownames(TGrossIPWINTu_0510) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

TGrossIPWINTj_9510 <- rbind(TGrossIPWINTj_9500,TGrossIPWINTj_0005,TGrossIPWINTj_0510)
TGrossIPWINTu_9510 <- rbind(TGrossIPWINTu_9500,TGrossIPWINTu_0005,TGrossIPWINTu_0510)

#前期
TGresult_9505_2sls_1 <- ivreg(formula = manusharechange_9505 ~ GrossIPWj_9505+manushare_start_9505+time9500_9505|GrossIPWu_9505+manushare_start_9505+time9500_9505,weights = weight_9505)
summary(Gresult_9505_2sls_1)
TGresult_9505_2sls_2 <- ivreg(formula = manusharechange_9505 ~ GrossIPWj_9505+manushare_start_9505+time9500_9505+region1_9505+region2_9505+region3_9505+region4_9505+region5_9505+region6_9505+region8_9505|GrossIPWu_9505+manushare_start_9505+time9500_9505+region1_9505+region2_9505+region3_9505+region4_9505+region5_9505+region6_9505+region8_9505,weights = weight_9505)
summary(Gresult_9505_2sls_2)
TGresult_9505_2sls_3 <- ivreg(formula = manusharechange_9505 ~ GrossIPWj_9505+manushare_start_9505+time_9500+time_0005+womenpart_9505+foreign_9505+college_9505|GrossIPWu_9510+manushare_start_9505+time9500_9505+time0005_9505+womenpart_9505+foreign_9505+college_9505,weights = weight_9510)
summary(Gresult_9505_2sls_3)
TGresult_9505_2sls_4 <- ivreg(formula = manusharechange_9505 ~ GrossIPWj_9505+manushare_start_9505+time9500_9505+womenpart_9505+foreign_9505+college_9505+region1_9505+region2_9505+region3_9505+region4_9505+region5_9505+region6_9505+region8_9505|GrossIPWu_9505+manushare_start_9505+time9500_9505+womenpart_9505+foreign_9505+college_9505+region1_9505+region2_9505+region3_9505+region4_9505+region5_9505+region6_9505+region8_9505,weights = weight_9505)
summary(TGresult_9505_2sls_4)


#追加
TGresult_9510_2sls_1 <- ivreg(formula = manusharechange_9510 ~ TGrossIPWj_9510+manushare_start+time_9500+time_0005|TGrossIPWu_9510+manushare_start+time_9500+time_0005,weights = weight_9510)

head(manusharechange_9510)
summary(TGresult_9510_2sls_1)
#有意に負。＋の効果は持続していない。

TGresult_9510_2sls_2 <- ivreg(formula = manusharechange_9510 ~ TGrossIPWj_9510+manushare_start+time_9500+time_0005+region1+region2+region3+region4+region5+region6+region8|TGrossIPWu_9510+manushare_start+time_9500+time_0005+region1+region2+region3+region4+region5+region6+region8,weights = weight_9510)
summary(TGresult_9510_2sls_2)
#ダミーコントロールでも負。

TGresult_9510_2sls_3 <- ivreg(formula = manusharechange_9510 ~ TGrossIPWj_9510+manushare_start+time_9500+time_0005+womenpart+foreign+college|TGrossIPWu_9510+manushare_start+time_9500+time_0005+womenpart+foreign+college,weights = weight_9510)
summary(TGresult_9510_2sls_3)

TGresult_9510_2sls_4 <- ivreg(formula = manusharechange_9510 ~ TGrossIPWj_9510+manushare_start+time_9500+time_0005+womenpart+foreign+college+region1+region2+region3+region4+region5+region6+region8|TGrossIPWu_9510+manushare_start+time_9500+time_0005+womenpart+foreign+college+region1+region2+region3+region4+region5+region6+region8,weights = weight_9510)
fGross <- lm(TGrossIPWj_9510~TGrossIPWu_9510+manushare_start+time_9500+time_0005+womenpart+foreign+college+region1+region2+region3+region4+region5+region6+region8)
summary(fGross)

summary(TGresult_9510_2sls_4)
#ダミー＋コントロールでも有意に負。何だこれ。

mean(TGrossIPWj_9510)*-0.43




#分割
flmWFNL_ <- lm(TGrossIPWFNLj_9510~TGrossIPWFNLu_9510+manushare_start+time_9500+time_0005,weights = weight_9510)
flmWINT_ <- lm(TGrossIPWINTj_9510~TGrossIPWINTu_9510+manushare_start+time_9500+time_0005,weights = weight_9510)
hatTGrossIPWFNLj_9510 <- predict(flmWFNL_)
hatTGrossIPWINTj_9510 <- predict(flmWINT_)
GTYPEresult_9510_2sls_1 <- lm(manusharechange_9510 ~ hatGrossIPWFNLj_9510+hatGrossIPWINTj_9510+manushare_start+time_9500+time_0005,weights = weight_9510)
summary(GTYPEresult_9510_2sls_1)
  summary(flmWFNL_)

#中間財が有意に正。最終財が有意ではないが負。

CHNVWLDTYPEresult_9510_2sls_2 <- lm(manusharechange_9510 ~ hatCHNVWLDFNLIPWj_9510+hatCHNVWLDINTIPWj_9510+manushare_start+region1+region2+region3+region4+region5+region6+region8)
summary(CHNVWLDTYPEresult_9510_2sls_2)
#ダミーコントロールでも同じ。
CHNVWLDTYPEresult_9510_2sls_3 <- lm(manusharechange_9510 ~ hatCHNVWLDFNLIPWj_9510+hatCHNVWLDINTIPWj_9510+manushare_start+womenpart+foreign+college)
summary(CHNVWLDTYPEresult_9510_2sls_3)
#他のコントロールのみでも。外国人が有意に効いている。

flmWFNL_ <- lm(TGrossIPWFNLj_9510~TGrossIPWFNLu_9510+manushare_start+time_9500+time_0005+womenpart+foreign+college+region1+region2+region3+region4+region5+region6+region8,weights = weight_9510)
flmWINT_ <- lm(TGrossIPWINTj_9510~TGrossIPWINTu_9510+manushare_start+time_9500+time_0005+womenpart+foreign+college+region1+region2+region3+region4+region5+region6+region8,weights = weight_9510)
summary(flmWFNL_)
summary(flmWINT_)

hatTGrossIPWFNLj_9510 <- predict(flmWFNL_)
hatTGrossIPWINTj_9510 <- predict(flmWINT_)

TGTYPEresult_9510_2sls_4 <- lm(manusharechange_9510 ~ hatTGrossIPWFNLj_9510+hatTGrossIPWINTj_9510+manushare_start+time_9500+time_0005+womenpart+foreign+college+region1+region2+region3+region4+region5+region6+region8,weights=weight_9510)
summary(GTYPEresult_9510_2sls_4)

Coef <- GTYPEresult_9510_2sls_4$coefficients
Coef[2]*mean(hatGrossIPWFNLj_9510)
Coef[3]*mean(hatGrossIPWINTj_9510)
#中間財は正だが有意ではない。


#0. Gross(UNCOMTRADE)###########
GrossIPWj_9500 <- IPW(CGImportJPN_9500,empji_1995,empj_1995,empi_1995)
GrossIPWj_9500 <- matrix(GrossIPWj_9500,47,1)
rownames(GrossIPWj_9500) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")
GrossIPWj_0005 <- IPW(CGImportJPN_0005,empji_2000,empj_2000,empi_2000)
GrossIPWj_0005 <- matrix(GrossIPWj_0005,47,1)
rownames(GrossIPWj_0005) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")
GrossIPWj_0510 <- IPW(CGImportJPN_0510,empji_2005,empj_2005,empi_2005)
GrossIPWj_0510 <- matrix(GrossIPWj_0510,47,1)
rownames(GrossIPWj_0510) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")
GrossIPWu_9500 <- IPW(CGImportUSA_9500,empji_1985,empj_1985,empi_1985)
GrossIPWu_9500 <- matrix(GrossIPWu_9500,47,1)
rownames(GrossIPWu_9500) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")
GrossIPWu_0005 <- IPW(CGImportUSA_0005,empji_1990,empj_1990,empi_1990)
GrossIPWu_0005 <- matrix(GrossIPWu_0005,47,1)
rownames(GrossIPWu_0005) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")
GrossIPWu_0510 <- IPW(CGImportUSA_0510,empji_1995,empj_1995,empi_1995)
GrossIPWu_0510 <- matrix(GrossIPWu_0510,47,1)
rownames(GrossIPWu_0510) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

GrossIPWj_9510 <- rbind(GrossIPWj_9500,GrossIPWj_0005,GrossIPWj_0510)
GrossIPWu_9510 <- rbind(GrossIPWu_9500,GrossIPWu_0005,GrossIPWu_0510)

library(AER)

Gresult_9510_2sls_1 <- ivreg(formula = manusharechange_9510 ~ GrossIPWj_9510+manushare_start+time_9500+time_0005|GrossIPWu_9510+manushare_start+time_9500+time_0005,weights = weight_9510)
manusharechange_9510
summary(Gresult_9510_2sls_4)
summary(TGresult_9510_2sls_4)
coefficients(TGresult_9510_2sls_4)[2]*mean(TGrossIPWj_9510)
coefficients(Gresult_9510_2sls_4)[2]*mean(GrossIPWj_9510)
mean(GrossIPWj_9510)
mean(TGrossIPWj_9510)

#有意に負。＋の効果は持続していない。

Gresult_9510_2sls_2 <- ivreg(formula = manusharechange_9510 ~ GrossIPWj_9510+manushare_start+time_9500+time_0005+region1+region2+region3+region4+region5+region6+region8|GrossIPWu_9510+manushare_start+time_9500+time_0005+region1+region2+region3+region4+region5+region6+region8,weights = weight_9510)
summary(Gresult_9510_2sls_2)
#ダミーコントロールでも負。

Gresult_9510_2sls_3 <- ivreg(formula = manusharechange_9510 ~ GrossIPWj_9510+manushare_start+time_9500+time_0005+womenpart+foreign+college|GrossIPWu_9510+manushare_start+time_9500+time_0005+womenpart+foreign+college,weights = weight_9510)
summary(Gresult_9510_2sls_3)

Gresult_9510_2sls_4 <- ivreg(formula = manusharechange_9510 ~ GrossIPWj_9510+manushare_start+time_9500+time_0005+womenpart+foreign+college+region1+region2+region3+region4+region5+region6+region8|GrossIPWu_9510+manushare_start+time_9500+time_0005+womenpart+foreign+college+region1+region2+region3+region4+region5+region6+region8,weights = weight_9510)
summary(Gresult_9510_2sls_4)

#どうやらそういうもんらしい。
#期間の取り方を変えると結果が大きく変わる。
#2001-2007は雇用の改善期だったようだが、期間を2005－2010とかに変えると改善期ではなくなる。

mean(manusharechange_0007)
mean(manusharechange_0005)
mean(manusharechange_0510)

#1.WldVinCHNExJPN####
WVACHNIPWj_9500 <- IPW(C_WldVinCHNExJPN_9500,empji_1995,empj_1995,empi_1995)
WVACHNIPWj_9500 <- matrix(WVACHNIPWj_9500,47,1)
rownames(WVACHNIPWj_9500) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

WVACHNIPWu_9500 <- IPW(C_WldVinCHNExUSA_9500,empji_1985,empj_1985,empi_1985)
WVACHNIPWu_9500 <- matrix(WVACHNIPWu_9500,47,1)
rownames(WVACHNIPWu_9500) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

WVACHNIPWj_0005 <- IPW(C_WldVinCHNExJPN_0005,empji_2000,empj_2000,empi_2000)
WVACHNIPWj_0005 <- matrix(WVACHNIPWj_0005,47,1)
rownames(WVACHNIPWj_0005) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

WVACHNIPWu_0005 <- IPW(C_WldVinCHNExUSA_0005,empji_1990,empj_1990,empi_1990)
WVACHNIPWu_0005 <- matrix(WVACHNIPWu_0005,47,1)
rownames(WVACHNIPWu_0005) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

WVACHNIPWj_0510 <- IPW(C_WldVinCHNExJPN_0510,empji_2005,empj_2005,empi_2005)
WVACHNIPWj_0510 <- matrix(WVACHNIPWj_0510,47,1)
rownames(WVACHNIPWj_0510) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

WVACHNIPWu_0510 <- IPW(C_WldVinCHNExUSA_0510,empji_1995,empj_1995,empi_1995)
WVACHNIPWu_0510 <- matrix(WVACHNIPWu_0510,47,1)
rownames(WVACHNIPWu_0510) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

WVACHNIPWj_9510 <- rbind(WVACHNIPWj_9500,WVACHNIPWj_0005,WVACHNIPWj_0510)
WVACHNIPWu_9510 <- rbind(WVACHNIPWu_9500,WVACHNIPWu_0005,WVACHNIPWu_0510)

##analysis
library(sem)
library(AER)

View(time_9500)

WVACHNresult_9510_2sls_1 <- ivreg(formula = manusharechange_9510 ~ WVACHNIPWj_9510+manushare_start+time_9500+time_0005|WVACHNIPWu_9510+manushare_start+time_9500+time_0005,weights = weight_9510)
summary(WVACHNresult_9510_2sls_1)
#有意に負。
WVACHNresult_9510_2sls_2 <- ivreg(formula = manusharechange_9510 ~WVACHNIPWj_9510+manushare_start+time_9500+time_0005+region1+region2+region3+region4+region5+region6+region8|WVACHNIPWu_9510+manushare_start+time_9500+time_0005+region1+region2+region3+region4+region5+region6+region8,weights = weight_9510)
summary(WVACHNresult_9510_2sls_2)
#ダミーコントロールでも正。
WVACHNresult_9510_2sls_3 <- ivreg(formula = manusharechange_9510 ~WVACHNIPWj_9510+manushare_start+time_9500+time_0005+womenpart+foreign+college|WVACHNIPWu_9510+manushare_start+time_9500+time_0005+womenpart+foreign+college,weights = weight_9510)
summary(WVACHNresult_9510_2sls_3)

WVACHNresult_9510_2sls_4 <- ivreg(formula = manusharechange_9510 ~WVACHNIPWj_9510+manushare_start+time_9500+time_0005+womenpart+foreign+college+region1+region2+region3+region4+region5+region6+region8|WVACHNIPWu_9510+time_9500+time_0005+manushare_start+womenpart+foreign+college+region1+region2+region3+region4+region5+region6+region8,weights = weight_9510)
FVA <- lm(WVACHNFNLIPWj_9510 ~ WVACHNFNLIPWu_9510+manushare_start+time_9500+time_0005+womenpart+foreign+college+region1+region2+region3+region4+region5+region6+region8)
summary(FVA)

summary(WVACHNresult_9510_2sls_4)

mean(WVACHNIPWj_9510)*(-0.54)

stargazer(WVACHNresult_9510_2sls_1,WVACHNresult_9510_2sls_2,WVACHNresult_9510_2sls_3,WVACHNresult_9510_2sls_4,type="html")


#2.CHNVinWldExJPN
CHNVWLDIPWj_9500 <- IPW(C_CHNVinWldExJPN_9500,empji_1995,empj_1995,empi_1995)
CHNVWLDIPWj_9500 <- matrix(CHNVWLDIPWj_9500,47,1)
rownames(CHNVWLDIPWj_9500) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

CHNVWLDIPWu_9500 <- IPW(C_CHNVinWldExUSA_9500,empji_1985,empj_1985,empi_1985)
CHNVWLDIPWu_9500 <- matrix(CHNVWLDIPWu_9500,47,1)
rownames(CHNVWLDIPWu_9500) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

CHNVWLDIPWj_0005 <- IPW(C_CHNVinWldExJPN_0005,empji_2000,empj_2000,empi_2000)
CHNVWLDIPWj_0005 <- matrix(CHNVWLDIPWj_0005,47,1)
rownames(CHNVWLDIPWj_0005) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

CHNVWLDIPWu_0005 <- IPW(C_CHNVinWldExUSA_0005,empji_1990,empj_1990,empi_1990)
CHNVWLDIPWu_0005 <- matrix(CHNVWLDIPWu_0005,47,1)
rownames(CHNVWLDIPWu_0005) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

CHNVWLDIPWj_0510 <- IPW(C_CHNVinWldExJPN_0510,empji_2005,empj_2005,empi_2005)
CHNVWLDIPWj_0510 <- matrix(CHNVWLDIPWj_0510,47,1)
rownames(CHNVWLDIPWj_0510) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

CHNVWLDIPWu_0510 <- IPW(C_CHNVinWldExUSA_0510,empji_1995,empj_1995,empi_1995)
CHNVWLDIPWu_0510 <- matrix(CHNVWLDIPWu_0510,47,1)
rownames(CHNVWLDIPWu_0510) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

CHNVWLDIPWj_9510 <- rbind(CHNVWLDIPWj_9500,CHNVWLDIPWj_0005,CHNVWLDIPWj_0510)
CHNVWLDIPWu_9510 <- rbind(CHNVWLDIPWu_9500,CHNVWLDIPWu_0005,CHNVWLDIPWu_0510)

#Analysis
CHNVWLDresult_9510_2sls_1 <- tsls(formula = manusharechange_9510 ~ CHNVWLDIPWj_9510+manushare_start, instruments = ~CHNVWLDIPWu_9510+manushare_start)
summary(CHNVWLDresult_9510_2sls_1)
#有意に正。非常に強い。
CHNVWLDresult_9510_2sls_2 <- tsls(formula = manusharechange_9510 ~ CHNVWLDIPWj_9510+manushare_start+region1+region2+region3+region4+region5+region6+region8, instruments = ~CHNVWLDIPWu_9510+manushare_start+region1+region2+region3+region4+region5+region6+region8)
summary(CHNVWLDresult_9510_2sls_2)
#ダミーコントロールでも正。
CHNVWLDresult_9510_2sls_3 <- tsls(formula = manusharechange_9510 ~ CHNVWLDIPWj_9510+manushare_start+womenpart+foreign+college, instruments = ~CHNVWLDIPWu_9510+manushare_start+womenpart+foreign+college)
summary(CHNVWLDresult_9510_2sls_3)
#他のコントロールのみでも正。外国人が有意に効いている。
CHNVWLDresult_9510_2sls_4 <- ivreg(formula = manusharechange_9510 ~ CHNVWLDIPWj_9510+manushare_start+womenpart+foreign+college+region1+region2+region3+region4+region5+region6+region8|CHNVWLDIPWu_9510+manushare_start+womenpart+foreign+college+region1+region2+region3+region4+region5+region6+region8)
summary(CHNVWLDresult_9510_2sls_4)
#こちらはこれでも強い正の効果。
mean(CHNVWLDFNLIPWj_9510)*1.82

CHNVWLDresult_9510_2sls_1 <- ivreg(formula = manusharechange_9510 ~CHNVWLDIPWj_9510+manushare_start|CHNVWLDIPWu_9510+manushare_start)
summary(CHNVWLDresult_9510_2sls_1)
#有意に正。非常に強い。
CHNVWLDresult_9510_2sls_2 <- ivreg(formula = manusharechange_9510 ~CHNVWLDIPWj_9510+manushare_start+region1+region2+region3+region4+region5+region6+region8|CHNVWLDIPWu_9510+manushare_start+region1+region2+region3+region4+region5+region6+region8)
summary(CHNVWLDresult_9510_2sls_2)
#ダミーコントロールでも正。
CHNVWLDresult_9510_2sls_3 <- ivreg(formula = manusharechange_9510 ~CHNVWLDIPWj_9510+manushare_start+womenpart+foreign+college|CHNVWLDIPWu_9510+manushare_start+womenpart+foreign+college)
summary(CHNVWLDresult_9510_2sls_3)
#他のコントロールのみでも正。外国人が有意に効いている。
CHNVWLDresult_9510_2sls_4 <- ivreg(formula = manusharechange_9510 ~CHNVWLDIPWj_9510+manushare_start+womenpart+foreign+college+region1+region2+region3+region4+region5+region6+region8|CHNVWLDIPWu_9510+manushare_start+womenpart+foreign+college+region1+region2+region3+region4+region5+region6+region8,weights = weight_9510)
summary(CHNVWLDresult_9510_2sls_4)

stargazer(CHNVWLDresult_9510_2sls_1,CHNVWLDresult_9510_2sls_2,CHNVWLDresult_9510_2sls_3,CHNVWLDresult_9510_2sls_4, type="html")


#3.CHNVinCHNEx
CHNVCHNIPWj_9500 <- IPW(C_CHNVinCHNExJPN_9500,empji_1995,empj_1995,empi_1995)
CHNVCHNIPWj_9500 <- matrix(CHNVCHNIPWj_9500,47,1)
rownames(CHNVCHNIPWj_9500) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

CHNVCHNIPWu_9500 <- IPW(C_CHNVinCHNExUSA_9500,empji_1985,empj_1985,empi_1985)
CHNVCHNIPWu_9500 <- matrix(CHNVCHNIPWu_9500,47,1)
rownames(CHNVCHNIPWu_9500) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

CHNVCHNIPWj_0005 <- IPW(C_CHNVinCHNExJPN_0005,empji_2000,empj_2000,empi_2000)
CHNVCHNIPWj_0005 <- matrix(CHNVCHNIPWj_0005,47,1)
rownames(CHNVCHNIPWj_0005) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

CHNVCHNIPWu_0005 <- IPW(C_CHNVinCHNExUSA_0005,empji_1990,empj_1990,empi_1990)
CHNVCHNIPWu_0005 <- matrix(CHNVCHNIPWu_0005,47,1)
rownames(CHNVCHNIPWu_0005) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

CHNVCHNIPWj_0510 <- IPW(C_CHNVinCHNExJPN_0510,empji_2005,empj_2005,empi_2005)
CHNVCHNIPWj_0510 <- matrix(CHNVCHNIPWj_0510,47,1)
rownames(CHNVCHNIPWj_0510) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

CHNVCHNIPWu_0510 <- IPW(C_CHNVinCHNExUSA_0510,empji_1995,empj_1995,empi_1995)
CHNVCHNIPWu_0510 <- matrix(CHNVCHNIPWu_0510,47,1)
rownames(CHNVCHNIPWu_0510) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

CHNVCHNIPWj_9510 <- rbind(CHNVCHNIPWj_9500,CHNVCHNIPWj_0005,CHNVCHNIPWj_0510)
CHNVCHNIPWu_9510 <- rbind(CHNVCHNIPWu_9500,CHNVCHNIPWu_0005,CHNVCHNIPWu_0510)

OTRVCHNIPWj_9500 <- IPW(C_OTRVinCHNExJPN_9500,empji_1995,empj_1995,empi_1995)
OTRVCHNIPWj_9500 <- matrix(OTRVCHNIPWj_9500,47,1)
rownames(OTRVCHNIPWj_9500) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

OTRVCHNIPWu_9500 <- IPW(C_OTRVinCHNExUSA_9500,empji_1985,empj_1985,empi_1985)
OTRVCHNIPWu_9500 <- matrix(OTRVCHNIPWu_9500,47,1)
rownames(OTRVCHNIPWu_9500) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

OTRVCHNIPWj_0005 <- IPW(C_OTRVinCHNExJPN_0005,empji_2000,empj_2000,empi_2000)
OTRVCHNIPWj_0005 <- matrix(OTRVCHNIPWj_0005,47,1)
rownames(OTRVCHNIPWj_0005) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

OTRVCHNIPWu_0005 <- IPW(C_OTRVinCHNExUSA_0005,empji_1990,empj_1990,empi_1990)
OTRVCHNIPWu_0005 <- matrix(OTRVCHNIPWu_0005,47,1)
rownames(OTRVCHNIPWu_0005) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

OTRVCHNIPWj_0510 <- IPW(C_OTRVinCHNExJPN_0510,empji_2005,empj_2005,empi_2005)
OTRVCHNIPWj_0510 <- matrix(OTRVCHNIPWj_0510,47,1)
rownames(OTRVCHNIPWj_0510) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

OTRVCHNIPWu_0510 <- IPW(C_OTRVinCHNExUSA_0510,empji_1995,empj_1995,empi_1995)
OTRVCHNIPWu_0510 <- matrix(OTRVCHNIPWu_0510,47,1)
rownames(OTRVCHNIPWu_0510) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

OTRVCHNIPWj_9510 <- rbind(OTRVCHNIPWj_9500,OTRVCHNIPWj_0005,OTRVCHNIPWj_0510)
OTRVCHNIPWu_9510 <- rbind(OTRVCHNIPWu_9500,OTRVCHNIPWu_0005,OTRVCHNIPWu_0510)

flm1 <- lm(CHNVCHNIPWj_9510~CHNVCHNIPWu_9510)
flm2 <- lm(OTRVCHNIPWj_9510~OTRVCHNIPWu_9510)

hatCHNVCHNIPWj_9510 <- predict(flm1)
hatOTRVCHNIPWj_9510 <- predict(flm2)

#Analysis
CHNVCHNresult_9510_2sls_1 <- lm(manusharechange_9510 ~ hatCHNVCHNIPWj_9510+hatOTRVCHNIPWj_9510+manushare_start+time_9500+time_0005,weights = weight_9510)
summary(CHNVCHNresult_9510_2sls_1)
#有意に正。非常に強い。
#他国の付加価値がかなりマイナスに効いている。

flm1 <- lm(CHNVCHNIPWj_9510~CHNVCHNIPWu_9510+manushare_start+time_9500+time_0005+region1+region2+region3+region4+region5+region6+region8,weights = weight_9510)
flm2 <- lm(OTRVCHNIPWj_9510~OTRVCHNIPWu_9510+manushare_start+time_9500+time_0005+region1+region2+region3+region4+region5+region6+region8,weights = weight_9510)

hatCHNVCHNIPWj_9510 <- predict(flm1)
hatOTRVCHNIPWj_9510 <- predict(flm2)

CHNVCHNresult_9510_2sls_2 <- lm(manusharechange_9510 ~ hatCHNVCHNIPWj_9510+hatOTRVCHNIPWj_9510+manushare_start+time_9500+time_0005+region1+region2+region3+region4+region5+region6+region8,weights = weight_9510)
summary(CHNVCHNresult_9510_2sls_2)
#ダミーコントロールでも同じ。

CHNVCHNresult_9510_2sls_3 <- lm(manusharechange_9510 ~ hatCHNVCHNIPWj_9510+hatOTRVCHNIPWj_9510+manushare_start+time_9500+time_0005+womenpart+foreign+college,weights = weight_9510)
summary(CHNVCHNresult_9510_2sls_3)
#他のコントロールのみでも正。外国人が有意に効いている。

flm1 <- lm(CHNVCHNIPWj_9510~CHNVCHNIPWu_9510+manushare_start+time_9500+time_0005+womenpart+foreign+college+region1+region2+region3+region4+region5+region6+region8,weights = weight_9510)
flm2 <- lm(OTRVCHNIPWj_9510~OTRVCHNIPWu_9510+manushare_start+time_9500+time_0005+womenpart+foreign+college+region1+region2+region3+region4+region5+region6+region8,weights = weight_9510)

hatCHNVCHNIPWj_9510 <- predict(flm1)
hatOTRVCHNIPWj_9510 <- predict(flm2)

CHNVCHNresult_9510_2sls_4 <- lm(manusharechange_9510 ~ hatCHNVCHNIPWj_9510+hatOTRVCHNIPWj_9510+manushare_start+time_9500+time_0005+womenpart+foreign+college+region1+region2+region3+region4+region5+region6+region8,weights = weight_9510)
summary(CHNVCHNresult_9510_2sls_4)

mean(hatCHNVCHNIPWj_9510)*CHNVCHNresult_9510_2sls_4$coefficients[2]
mean(hatOTRVCHNIPWj_9510)*CHNVCHNresult_9510_2sls_4$coefficients[3]

#入れない場合
CHNVCHN_O_result_9510_2sls_1 <- lm(manusharechange_9510 ~ hatCHNVCHNIPWj_9510+manushare_start)
summary(CHNVCHN_O_result_9510_2sls_1)
#有意に正。非常に強い。

CHNVCHNresult_9510_2sls_2 <- lm(manusharechange_9510 ~ hatCHNVCHNIPWj_9510+hatOTRVCHNIPWj_9510+manushare_start+region1+region2+region3+region4+region5+region6+region8)
summary(CHNVCHNresult_9510_2sls_2)
#ダミーコントロールでも同じ。

CHNVCHNresult_9510_2sls_3 <- lm(manusharechange_9510 ~ hatCHNVCHNIPWj_9510+hatOTRVCHNIPWj_9510+manushare_start+womenpart+foreign+college)
summary(CHNVCHNresult_9510_2sls_3)
#他のコントロールのみでも正。外国人が有意に効いている。

CHNVCHNresult_O_9510_2sls_4 <- lm(manusharechange_9510 ~ hatCHNVCHNIPWj_9510+manushare_start+time_9500+time_0005+womenpart+foreign+college+region1+region2+region3+region4+region5+region6+region8,weights = weight_9510)
summary(CHNVCHNresult_O_9510_2sls_4)
#ほぼ一貫して同じ。


#INT and FNL#################################################################################################################################
#1. WVACHNEx
WVACHNFNLIPWj_9500 <- IPW(C_WldVinCHNExJPN_FNL_9500,empji_1995,empj_1995,empi_1995)
WVACHNFNLIPWj_9500 <- matrix(WVACHNFNLIPWj_9500,47,1)
rownames(WVACHNFNLIPWj_9500) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

WVACHNFNLIPWu_9500 <- IPW(C_WldVinCHNExUSA_FNL_9500,empji_1985,empj_1985,empi_1985)
WVACHNFNLIPWu_9500 <- matrix(WVACHNFNLIPWu_9500,47,1)
rownames(WVACHNFNLIPWu_9500) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

WVACHNFNLIPWj_0005 <- IPW(C_WldVinCHNExJPN_FNL_0005,empji_2000,empj_2000,empi_2000)
WVACHNFNLIPWj_0005 <- matrix(WVACHNFNLIPWj_0005,47,1)
rownames(WVACHNFNLIPWj_0005) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

WVACHNFNLIPWu_0005 <- IPW(C_WldVinCHNExUSA_FNL_0005,empji_1990,empj_1990,empi_1990)
WVACHNFNLIPWu_0005 <- matrix(WVACHNFNLIPWu_0005,47,1)
rownames(WVACHNFNLIPWu_0005) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

WVACHNFNLIPWj_0510 <- IPW(C_WldVinCHNExJPN_FNL_0510,empji_2005,empj_2005,empi_2005)
WVACHNFNLIPWj_0510 <- matrix(WVACHNFNLIPWj_0510,47,1)
rownames(WVACHNFNLIPWj_0510) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

WVACHNFNLIPWu_0510 <- IPW(C_WldVinCHNExUSA_FNL_0510,empji_1995,empj_1995,empi_1995)
WVACHNFNLIPWu_0510 <- matrix(WVACHNFNLIPWu_0510,47,1)
rownames(WVACHNFNLIPWu_0510) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

WVACHNFNLIPWj_9510 <- rbind(WVACHNFNLIPWj_9500,WVACHNFNLIPWj_0005,WVACHNFNLIPWj_0510)
WVACHNFNLIPWu_9510 <- rbind(WVACHNFNLIPWu_9500,WVACHNFNLIPWu_0005,WVACHNFNLIPWu_0510)

WVACHNINTIPWj_9500 <- IPW(C_WldVinCHNExJPN_INT_9500,empji_1995,empj_1995,empi_1995)
WVACHNINTIPWj_9500 <- matrix(WVACHNINTIPWj_9500,47,1)
rownames(WVACHNINTIPWj_9500) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

WVACHNINTIPWu_9500 <- IPW(C_WldVinCHNExUSA_INT_9500,empji_1985,empj_1985,empi_1985)
WVACHNINTIPWu_9500 <- matrix(WVACHNINTIPWu_9500,47,1)
rownames(WVACHNINTIPWu_9500) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

WVACHNINTIPWj_0005 <- IPW(C_WldVinCHNExJPN_INT_0005,empji_2000,empj_2000,empi_2000)
WVACHNINTIPWj_0005 <- matrix(WVACHNINTIPWj_0005,47,1)
rownames(WVACHNINTIPWj_0005) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

WVACHNINTIPWu_0005 <- IPW(C_WldVinCHNExUSA_INT_0005,empji_1990,empj_1990,empi_1990)
WVACHNINTIPWu_0005 <- matrix(WVACHNINTIPWu_0005,47,1)
rownames(WVACHNINTIPWu_0005) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

WVACHNINTIPWj_0510 <- IPW(C_WldVinCHNExJPN_INT_0510,empji_2005,empj_2005,empi_2005)
WVACHNINTIPWj_0510 <- matrix(WVACHNINTIPWj_0510,47,1)
rownames(WVACHNINTIPWj_0510) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

WVACHNINTIPWu_0510 <- IPW(C_WldVinCHNExUSA_INT_0510,empji_1995,empj_1995,empi_1995)
WVACHNINTIPWu_0510 <- matrix(WVACHNINTIPWu_0510,47,1)
rownames(WVACHNINTIPWu_0510) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

WVACHNINTIPWj_9510 <- rbind(WVACHNINTIPWj_9500,WVACHNINTIPWj_0005,WVACHNINTIPWj_0510)
WVACHNINTIPWu_9510 <- rbind(WVACHNINTIPWu_9500,WVACHNINTIPWu_0005,WVACHNINTIPWu_0510)

#Analysis
flmWFNL_ <- lm(WVACHNFNLIPWj_9510~WVACHNFNLIPWu_9510+time_9500+time_0005,weights = weight_9510)
flmWINT_ <- lm(WVACHNINTIPWj_9510~WVACHNINTIPWu_9510+time_9500+time_0005,weights = weight_9510)
hatWVACHNFNLIPWj_9510 <- predict(flmWFNL_)
hatWVACHNINTIPWj_9510 <- predict(flmWINT_)
WLDVCHNTYPEresult_9510_2sls_1 <- lm(manusharechange_9510 ~ hatWVACHNFNLIPWj_9510+hatWVACHNINTIPWj_9510+manushare_start+time_9500+time_0005,weights = weight_9510)
summary(WLDVCHNTYPEresult_9510_2sls_1)

flmWFNL_ <- lm(WVACHNFNLIPWj_9510~WVACHNFNLIPWu_9510+time_9500+time_0005+region1+region2+region3+region4+region5+region6+region8,weights = weight_9510)
flmWINT_ <- lm(WVACHNINTIPWj_9510~WVACHNINTIPWu_9510+time_9500+time_0005+region1+region2+region3+region4+region5+region6+region8,weights = weight_9510)
hatWVACHNFNLIPWj_9510 <- predict(flmWFNL_)
hatWVACHNINTIPWj_9510 <- predict(flmWINT_)
WLDVCHNTYPEresult_9510_2sls_2 <- lm(manusharechange_9510 ~ hatWVACHNFNLIPWj_9510+hatWVACHNINTIPWj_9510+manushare_start+time_9500+time_0005+region1+region2+region3+region4+region5+region6+region8,weights = weight_9510)
summary(WLDVCHNTYPEresult_9510_2sls_2)

WLDVCHNTYPEresult_9510_2sls_3 <- lm(manusharechange_9510 ~ hatWVACHNFNLIPWj_9510+hatWVACHNINTIPWj_9510+manushare_start+time_9500+time_0005+womenpart+foreign+college,weights = weight_9510)
summary(WLDVCHNTYPEresult_9510_2sls_3)

flmWFNL_ <- lm(WVACHNFNLIPWj_9510~WVACHNFNLIPWu_9510+time_9500+time_0005+womenpart+foreign+college+region1+region2+region3+region4+region5+region6+region8,weights = weight_9510)
flmWINT_ <- lm(WVACHNINTIPWj_9510~WVACHNINTIPWu_9510+time_9500+time_0005+womenpart+foreign+college+region1+region2+region3+region4+region5+region6+region8,weights = weight_9510)
hatWVACHNFNLIPWj_9510 <- predict(flmWFNL_)
hatWVACHNINTIPWj_9510 <- predict(flmWINT_)
WLDVCHNTYPEresult_9510_2sls_4 <- lm(manusharechange_9510 ~ hatWVACHNFNLIPWj_9510+hatWVACHNINTIPWj_9510+manushare_start+time_9500+time_0005+womenpart+foreign+college+region1+region2+region3+region4+region5+region6+region8,weights = weight_9510)
summary(WLDVCHNTYPEresult_9510_2sls_4)
Coef <- WLDVCHNTYPEresult_9510_2sls_4$coefficients
Coef
Coef[2]*mean(hatWVACHNFNLIPWj_9510)
Coef[3]*mean(hatWVACHNINTIPWj_9510)

#2. CHNVWLD
CHNVWLDFNLIPWj_9500 <- IPW(C_CHNVinWldExJPN_FNL_9500,empji_1995,empj_1995,empi_1995)
CHNVWLDFNLIPWj_9500 <- matrix(CHNVWLDFNLIPWj_9500,47,1)
rownames(CHNVWLDFNLIPWj_9500) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

CHNVWLDFNLIPWu_9500 <- IPW(C_CHNVinWldExUSA_FNL_9500,empji_1985,empj_1985,empi_1985)
CHNVWLDFNLIPWu_9500 <- matrix(CHNVWLDFNLIPWu_9500,47,1)
rownames(CHNVWLDFNLIPWu_9500) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

CHNVWLDFNLIPWj_0005 <- IPW(C_CHNVinWldExJPN_FNL_0005,empji_2000,empj_2000,empi_2000)
CHNVWLDFNLIPWj_0005 <- matrix(CHNVWLDFNLIPWj_0005,47,1)
rownames(CHNVWLDFNLIPWj_0005) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

CHNVWLDFNLIPWu_0005 <- IPW(C_CHNVinWldExUSA_FNL_0005,empji_1990,empj_1990,empi_1990)
CHNVWLDFNLIPWu_0005 <- matrix(CHNVWLDFNLIPWu_0005,47,1)
rownames(CHNVWLDFNLIPWu_0005) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

CHNVWLDFNLIPWj_0510 <- IPW(C_CHNVinWldExJPN_FNL_0510,empji_2005,empj_2005,empi_2005)
CHNVWLDFNLIPWj_0510 <- matrix(CHNVWLDFNLIPWj_0510,47,1)
rownames(CHNVWLDFNLIPWj_0510) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

CHNVWLDFNLIPWu_0510 <- IPW(C_CHNVinWldExUSA_FNL_0510,empji_1995,empj_1995,empi_1995)
CHNVWLDFNLIPWu_0510 <- matrix(CHNVWLDFNLIPWu_0510,47,1)
rownames(CHNVWLDFNLIPWu_0510) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

CHNVWLDFNLIPWj_9510 <- rbind(CHNVWLDFNLIPWj_9500,CHNVWLDFNLIPWj_0005,CHNVWLDFNLIPWj_0510)
CHNVWLDFNLIPWu_9510 <- rbind(CHNVWLDFNLIPWu_9500,CHNVWLDFNLIPWu_0005,CHNVWLDFNLIPWu_0510)

CHNVWLDINTIPWj_9500 <- IPW(C_CHNVinWldExJPN_INT_9500,empji_1995,empj_1995,empi_1995)
CHNVWLDINTIPWj_9500 <- matrix(CHNVWLDINTIPWj_9500,47,1)
rownames(CHNVWLDINTIPWj_9500) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

CHNVWLDINTIPWu_9500 <- IPW(C_CHNVinWldExUSA_INT_9500,empji_1985,empj_1985,empi_1985)
CHNVWLDINTIPWu_9500 <- matrix(CHNVWLDINTIPWu_9500,47,1)
rownames(CHNVWLDINTIPWu_9500) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

CHNVWLDINTIPWj_0005 <- IPW(C_CHNVinWldExJPN_INT_0005,empji_2000,empj_2000,empi_2000)
CHNVWLDINTIPWj_0005 <- matrix(CHNVWLDINTIPWj_0005,47,1)
rownames(CHNVWLDINTIPWj_0005) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

CHNVWLDINTIPWu_0005 <- IPW(C_CHNVinWldExUSA_INT_0005,empji_1990,empj_1990,empi_1990)
CHNVWLDINTIPWu_0005 <- matrix(CHNVWLDINTIPWu_0005,47,1)
rownames(CHNVWLDINTIPWu_0005) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

CHNVWLDINTIPWj_0510 <- IPW(C_CHNVinWldExJPN_INT_0510,empji_2005,empj_2005,empi_2005)
CHNVWLDINTIPWj_0510 <- matrix(CHNVWLDINTIPWj_0510,47,1)
rownames(CHNVWLDINTIPWj_0510) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

CHNVWLDINTIPWu_0510 <- IPW(C_CHNVinWldExUSA_INT_0510,empji_1995,empj_1995,empi_1995)
CHNVWLDINTIPWu_0510 <- matrix(CHNVWLDINTIPWu_0510,47,1)
rownames(CHNVWLDINTIPWu_0510) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

CHNVWLDINTIPWj_9510 <- rbind(CHNVWLDINTIPWj_9500,CHNVWLDINTIPWj_0005,CHNVWLDINTIPWj_0510)
CHNVWLDINTIPWu_9510 <- rbind(CHNVWLDINTIPWu_9500,CHNVWLDINTIPWu_0005,CHNVWLDINTIPWu_0510)

#Analysis
flmWFNL_ <- lm(CHNVWLDFNLIPWj_9510~CHNVWLDFNLIPWu_9510)
flmWINT_ <- lm(CHNVWLDINTIPWj_9510~CHNVWLDINTIPWu_9510)

hatCHNVWLDFNLIPWj_9510 <- predict(flmWFNL_)
hatCHNVWLDINTIPWj_9510 <- predict(flmWINT_)

CHNVWLDTYPEresult_9510_2sls_1 <- lm(manusharechange_9510 ~ hatCHNVWLDFNLIPWj_9510+hatCHNVWLDINTIPWj_9510+manushare_start)
summary(CHNVWLDTYPEresult_9510_2sls_1)
#中間財が有意に正。最終財が有意ではないが負。
CHNVWLDTYPEresult_9510_2sls_2 <- lm(manusharechange_9510 ~ hatCHNVWLDFNLIPWj_9510+hatCHNVWLDINTIPWj_9510+manushare_start+region1+region2+region3+region4+region5+region6+region8)
summary(CHNVWLDTYPEresult_9510_2sls_2)
#ダミーコントロールでも同じ。
CHNVWLDTYPEresult_9510_2sls_3 <- lm(manusharechange_9510 ~ hatCHNVWLDFNLIPWj_9510+hatCHNVWLDINTIPWj_9510+manushare_start+womenpart+foreign+college)
summary(CHNVWLDTYPEresult_9510_2sls_3)
#他のコントロールのみでも。外国人が有意に効いている。
CHNVWLDTYPEresult_9510_2sls_4 <- lm(manusharechange_9510 ~ hatCHNVWLDFNLIPWj_9510+hatCHNVWLDINTIPWj_9510+manushare_start+womenpart+foreign+college+region1+region2+region3+region4+region5+region6+region8)
summary(CHNVWLDTYPEresult_9510_2sls_4)
#ずっと中間財は正。

#3.CHNVinCHNEx
CHNVCHNFNLIPWj_9500 <- IPW(C_CHNVinCHNExJPN_FNL_9500,empji_1995,empj_1995,empi_1995)
CHNVCHNFNLIPWj_9500 <- matrix(CHNVCHNFNLIPWj_9500,47,1)
rownames(CHNVCHNFNLIPWj_9500) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

CHNVCHNFNLIPWu_9500 <- IPW(C_CHNVinCHNExUSA_FNL_9500,empji_1985,empj_1985,empi_1985)
CHNVCHNFNLIPWu_9500 <- matrix(CHNVCHNFNLIPWu_9500,47,1)
rownames(CHNVCHNFNLIPWu_9500) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

CHNVCHNFNLIPWj_0005 <- IPW(C_CHNVinCHNExJPN_FNL_0005,empji_2000,empj_2000,empi_2000)
CHNVCHNFNLIPWj_0005 <- matrix(CHNVCHNFNLIPWj_0005,47,1)
rownames(CHNVCHNFNLIPWj_0005) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

CHNVCHNFNLIPWu_0005 <- IPW(C_CHNVinCHNExUSA_FNL_0005,empji_1990,empj_1990,empi_1990)
CHNVCHNFNLIPWu_0005 <- matrix(CHNVCHNFNLIPWu_0005,47,1)
rownames(CHNVCHNFNLIPWu_0005) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

CHNVCHNFNLIPWj_0510 <- IPW(C_CHNVinCHNExJPN_FNL_0510,empji_2005,empj_2005,empi_2005)
CHNVCHNFNLIPWj_0510 <- matrix(CHNVCHNFNLIPWj_0510,47,1)
rownames(CHNVCHNFNLIPWj_0510) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

CHNVCHNFNLIPWu_0510 <- IPW(C_CHNVinCHNExUSA_FNL_0510,empji_1995,empj_1995,empi_1995)
CHNVCHNFNLIPWu_0510 <- matrix(CHNVCHNFNLIPWu_0510,47,1)
rownames(CHNVCHNFNLIPWu_0510) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

CHNVCHNFNLIPWj_9510 <- rbind(CHNVCHNFNLIPWj_9500,CHNVCHNFNLIPWj_0005,CHNVCHNFNLIPWj_0510)
CHNVCHNFNLIPWu_9510 <- rbind(CHNVCHNFNLIPWu_9500,CHNVCHNFNLIPWu_0005,CHNVCHNFNLIPWu_0510)

CHNVCHNINTIPWj_9500 <- IPW(C_CHNVinCHNExJPN_INT_9500,empji_1995,empj_1995,empi_1995)
CHNVCHNINTIPWj_9500 <- matrix(CHNVCHNINTIPWj_9500,47,1)
rownames(CHNVCHNINTIPWj_9500) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

CHNVCHNINTIPWu_9500 <- IPW(C_CHNVinCHNExUSA_INT_9500,empji_1985,empj_1985,empi_1985)
CHNVCHNINTIPWu_9500 <- matrix(CHNVCHNINTIPWu_9500,47,1)
rownames(CHNVCHNINTIPWu_9500) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

CHNVCHNINTIPWj_0005 <- IPW(C_CHNVinCHNExJPN_INT_0005,empji_2000,empj_2000,empi_2000)
CHNVCHNINTIPWj_0005 <- matrix(CHNVCHNINTIPWj_0005,47,1)
rownames(CHNVCHNINTIPWj_0005) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

CHNVCHNINTIPWu_0005 <- IPW(C_CHNVinCHNExUSA_INT_0005,empji_1990,empj_1990,empi_1990)
CHNVCHNINTIPWu_0005 <- matrix(CHNVCHNINTIPWu_0005,47,1)
rownames(CHNVCHNINTIPWu_0005) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

CHNVCHNINTIPWj_0510 <- IPW(C_CHNVinCHNExJPN_INT_0510,empji_2005,empj_2005,empi_2005)
CHNVCHNINTIPWj_0510 <- matrix(CHNVCHNINTIPWj_0510,47,1)
rownames(CHNVCHNINTIPWj_0510) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

CHNVCHNINTIPWu_0510 <- IPW(C_CHNVinCHNExUSA_INT_0510,empji_1995,empj_1995,empi_1995)
CHNVCHNINTIPWu_0510 <- matrix(CHNVCHNINTIPWu_0510,47,1)
rownames(CHNVCHNINTIPWu_0510) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

CHNVCHNINTIPWj_9510 <- rbind(CHNVCHNINTIPWj_9500,CHNVCHNINTIPWj_0005,CHNVCHNINTIPWj_0510)
CHNVCHNINTIPWu_9510 <- rbind(CHNVCHNINTIPWu_9500,CHNVCHNINTIPWu_0005,CHNVCHNINTIPWu_0510)

OTRVCHNFNLIPWj_9500 <- IPW(C_OTRVinCHNExJPN_FNL_9500,empji_1995,empj_1995,empi_1995)
OTRVCHNFNLIPWj_9500 <- matrix(OTRVCHNFNLIPWj_9500,47,1)
rownames(OTRVCHNFNLIPWj_9500) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

OTRVCHNFNLIPWu_9500 <- IPW(C_OTRVinCHNExUSA_FNL_9500,empji_1985,empj_1985,empi_1985)
OTRVCHNFNLIPWu_9500 <- matrix(OTRVCHNFNLIPWu_9500,47,1)
rownames(OTRVCHNFNLIPWu_9500) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

OTRVCHNFNLIPWj_0005 <- IPW(C_OTRVinCHNExJPN_FNL_0005,empji_2000,empj_2000,empi_2000)
OTRVCHNFNLIPWj_0005 <- matrix(OTRVCHNFNLIPWj_0005,47,1)
rownames(OTRVCHNFNLIPWj_0005) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

OTRVCHNFNLIPWu_0005 <- IPW(C_OTRVinCHNExUSA_FNL_0005,empji_1990,empj_1990,empi_1990)
OTRVCHNFNLIPWu_0005 <- matrix(OTRVCHNFNLIPWu_0005,47,1)
rownames(OTRVCHNFNLIPWu_0005) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

OTRVCHNFNLIPWj_0510 <- IPW(C_OTRVinCHNExJPN_FNL_0510,empji_2005,empj_2005,empi_2005)
OTRVCHNFNLIPWj_0510 <- matrix(OTRVCHNFNLIPWj_0510,47,1)
rownames(OTRVCHNFNLIPWj_0510) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

OTRVCHNFNLIPWu_0510 <- IPW(C_OTRVinCHNExUSA_FNL_0510,empji_1995,empj_1995,empi_1995)
OTRVCHNFNLIPWu_0510 <- matrix(OTRVCHNFNLIPWu_0510,47,1)
rownames(OTRVCHNFNLIPWu_0510) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

OTRVCHNFNLIPWj_9510 <- rbind(OTRVCHNFNLIPWj_9500,OTRVCHNFNLIPWj_0005,OTRVCHNFNLIPWj_0510)
OTRVCHNFNLIPWu_9510 <- rbind(OTRVCHNFNLIPWu_9500,OTRVCHNFNLIPWu_0005,OTRVCHNFNLIPWu_0510)

OTRVCHNINTIPWj_9500 <- IPW(C_OTRVinCHNExJPN_INT_9500,empji_1995,empj_1995,empi_1995)
OTRVCHNINTIPWj_9500 <- matrix(OTRVCHNINTIPWj_9500,47,1)
rownames(OTRVCHNINTIPWj_9500) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

OTRVCHNINTIPWu_9500 <- IPW(C_OTRVinCHNExUSA_INT_9500,empji_1985,empj_1985,empi_1985)
OTRVCHNINTIPWu_9500 <- matrix(OTRVCHNINTIPWu_9500,47,1)
rownames(OTRVCHNINTIPWu_9500) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

OTRVCHNINTIPWj_0005 <- IPW(C_OTRVinCHNExJPN_INT_0005,empji_2000,empj_2000,empi_2000)
OTRVCHNINTIPWj_0005 <- matrix(OTRVCHNINTIPWj_0005,47,1)
rownames(OTRVCHNINTIPWj_0005) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

OTRVCHNINTIPWu_0005 <- IPW(C_OTRVinCHNExUSA_INT_0005,empji_1990,empj_1990,empi_1990)
OTRVCHNINTIPWu_0005 <- matrix(OTRVCHNINTIPWu_0005,47,1)
rownames(OTRVCHNINTIPWu_0005) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

OTRVCHNINTIPWj_0510 <- IPW(C_OTRVinCHNExJPN_INT_0510,empji_2005,empj_2005,empi_2005)
OTRVCHNINTIPWj_0510 <- matrix(OTRVCHNINTIPWj_0510,47,1)
rownames(OTRVCHNINTIPWj_0510) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

OTRVCHNINTIPWu_0510 <- IPW(C_OTRVinCHNExUSA_INT_0510,empji_1995,empj_1995,empi_1995)
OTRVCHNINTIPWu_0510 <- matrix(OTRVCHNINTIPWu_0510,47,1)
rownames(OTRVCHNINTIPWu_0510) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")

OTRVCHNINTIPWj_9510 <- rbind(OTRVCHNINTIPWj_9500,OTRVCHNINTIPWj_0005,OTRVCHNINTIPWj_0510)
OTRVCHNINTIPWu_9510 <- rbind(OTRVCHNINTIPWu_9500,OTRVCHNINTIPWu_0005,OTRVCHNINTIPWu_0510)

flmWFNL_ <- lm(CHNVCHNFNLIPWj_9510~CHNVCHNFNLIPWu_9510)
flmWINT_ <- lm(CHNVCHNINTIPWj_9510~CHNVCHNINTIPWu_9510)
flmOWFNL_ <- lm(OTRVCHNFNLIPWj_9510~OTRVCHNFNLIPWu_9510)
flmOWINT_ <- lm(OTRVCHNINTIPWj_9510~OTRVCHNINTIPWu_9510)

hatCHNVCHNFNLIPWj_9510 <- predict(flmWFNL_)
hatCHNVCHNINTIPWj_9510 <- predict(flmWINT_)
hatOTRVCHNFNLIPWj_9510 <- predict(flmOWFNL_)
hatOTRVCHNINTIPWj_9510 <- predict(flmOWINT_)

CHNVCHNTYPEresult_9510_2sls_1 <- lm(manusharechange_9510 ~ hatCHNVCHNFNLIPWj_9510+hatCHNVCHNINTIPWj_9510+hatOTRVCHNIPWj_9510+manushare_start+time_9500+time_0005,weights = weight_9510)
summary(CHNVCHNTYPEresult_9510_2sls_1)
#中間財が有意に正。最終財が有意ではないが負。
CHNVCHNTYPEresult_9510_2sls_2 <- lm(manusharechange_9510 ~ hatCHNVCHNFNLIPWj_9510+hatCHNVCHNINTIPWj_9510+hatOTRVCHNIPWj_9510+manushare_start+time_9500+time_0005+region1+region2+region3+region4+region5+region6+region8,weights = weight_9510)
summary(CHNVCHNTYPEresult_9510_2sls_2)
#ダミーコントロールでも同じ。
CHNVCHNTYPEresult_9510_2sls_3 <- lm(manusharechange_9510 ~ hatCHNVCHNFNLIPWj_9510+hatCHNVCHNINTIPWj_9510+hatOTRVCHNIPWj_9510+manushare_start+time_9500+time_0005+womenpart+foreign+college,weights = weight_9510)
summary(CHNVCHNTYPEresult_9510_2sls_3)
#他のコントロールのみでも。外国人が有意に効いている。

flmWFNL_ <- lm(CHNVCHNFNLIPWj_9510~CHNVCHNFNLIPWu_9510+manushare_start+time_9500+time_0005+womenpart+foreign+college+region1+region2+region3+region4+region5+region6+region8,weights = weight_9510)
flmWINT_ <- lm(CHNVCHNINTIPWj_9510~CHNVCHNINTIPWu_9510+manushare_start+time_9500+time_0005+womenpart+foreign+college+region1+region2+region3+region4+region5+region6+region8,weights = weight_9510)
flmOWFNL_ <- lm(OTRVCHNFNLIPWj_9510~OTRVCHNFNLIPWu_9510+manushare_start+time_9500+time_0005+womenpart+foreign+college+region1+region2+region3+region4+region5+region6+region8,weights = weight_9510)
flmOWINT_ <- lm(OTRVCHNINTIPWj_9510~OTRVCHNINTIPWu_9510+manushare_start+time_9500+time_0005+womenpart+foreign+college+region1+region2+region3+region4+region5+region6+region8,weights = weight_9510)

hatCHNVCHNFNLIPWj_9510 <- predict(flmWFNL_)
hatCHNVCHNINTIPWj_9510 <- predict(flmWINT_)
hatOTRVCHNFNLIPWj_9510 <- predict(flmOWFNL_)
hatOTRVCHNINTIPWj_9510 <- predict(flmOWINT_)

CHNVCHNTYPEresult_9510_2sls_4 <- lm(manusharechange_9510 ~ hatCHNVCHNFNLIPWj_9510+hatCHNVCHNINTIPWj_9510+hatOTRVCHNFNLIPWj_9510+hatOTRVCHNINTIPWj_9510+manushare_start+time_9500+time_0005+womenpart+foreign+college+region1+region2+region3+region4+region5+region6+region8,weights = weight_9510)
summary(CHNVCHNTYPEresult_9510_2sls_4)


CHNVCHNTYPEresult_D_9510_2sls_4 <- lm(manusharechange_9510 ~ hatOTRVCHNIPWj_9510+hatCHNVCHNFNLIPWj_9510+hatCHNVCHNINTIPWj_9510+manushare_start+time_9500+time_0005+womenpart+foreign+college+region1+region2+region3+region4+region5+region6+region8,weights = weight_9510)
summary(CHNVCHNTYPEresult_D_9510_2sls_4)

CHNVCHNTYPEresult_O_9510_2sls_4 <- lm(manusharechange_9510 ~ hatOTRVCHNFNLIPWj_9510+hatOTRVCHNINTIPWj_9510+manushare_start+time_9500+time_0005+womenpart+foreign+college+region1+region2+region3+region4+region5+region6+region8,weights = weight_9510)
summary(CHNVCHNTYPEresult_O_9510_2sls_4)

#地域差
orderGIPW9500 <- cbind(TGrossIPWj_9500,order(TGrossIPWj_9500))
View(orderGIPW9500)

orderGIPW <- cbind(TGrossIPW_mean,order(TGrossIPW_mean,na.last = FALSE))
View(orderGIPW)

colnames(empji) <- c(1516,1719,20,2122,23,24,25,26,27,28,29,31,303233,34,35,3637)
rownames(empji) <- c("北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉","東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重","滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口","徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")
View(empji)

empji[5,]
