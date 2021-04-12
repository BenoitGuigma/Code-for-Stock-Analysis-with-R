rm(list=ls())


#PACKAGE INSTALL #######
install.packages("quantmod","xts","rvest","tidyverse","stringr","forcats","lubridate","plotly","dplyr","PerformanceAnalytics")
install.packages("quantmod")
install.packages("xts")
install.packages("rvest")
install.packages("tidyverse")
install.packages("stringr")
install.packages("forcats")
install.packages("lubridate")
install.packages("plotly")
install.packages("dplyr")
install.packages("PerformanceAnalytics")#Package install
 
#LIBRARY CALLING ###########
library(quantmod)
library(xts)
library(rvest)
library(tidyverse)
library(stringr)
library(forcats)
library(lubridate)
library(plotly)
library(dplyr)
library(PerformanceAnalytics)

#loadSymbols(c("BTCUSD=X","AMD"))

stock_values <- oanda.currencies
View(stock_values)
# Fetch all Symbols & store only the tickers to retrieve the data
symbols <- stockSymbols()
#symbols <- symbols[,1]
view(symbols)
#STOCK LOADING ###########
#Loads the company stock using ticker
date_debut <- "2008-08-01"
date_fin <- "2019-01-01"
indice1 <- "NFLX"
#getSymbols(indice1,from=date_debut,to=date_fin)
getSymbols(indice1,from=date_debut)

command1 <- str_c("`",indice1,"`")
indice1 <- eval(parse(text=command1))

indice2 <- "ETH-USD"
#getSymbols(indice2,from=date_debut,to=date_fin)
indice2 <- eval(parse(text = indice2))
getSymbols(indice2,from=date_debut)

command2 <- str_c("`",indice2,"`")
indice2 <- eval(parse(text=command2))

indice3 <- "AVAX-USD"
#getSymbols(indice3,from=date_debut,to=date_fin)
indice3 <- eval(parse(text = indice3))
getSymbols(indice3)#,from=date_debut)

command3 <- str_c("`",indice3,"`")
indice3 <- eval(parse(text=command3))

indice4 <- "ADA-USD"
#getSymbols(indice4,from=date_debut,to=date_fin)
indice4 <- eval(parse(text = indice4))
getSymbols(indice4)#,from=date_debut)

command4 <- str_c("`",indice4,"`")
indice4 <- eval(parse(text=command4))

indice5 <- "LINK-USD"
#getSymbols(indice5,from=date_debut,to=date_fin)  
indice5 <- eval(parse(text = indice5))
getSymbols(indice5)#,from=date_debut)

command5 <- str_c("`",indice5,"`")
indice5 <- eval(parse(text=command5))


#LOG OPERATIONS ON STOCK ##########
#Stock returns in log

indice1_log_returns<-indice1%>%Ad()%>%dailyReturn(type='log')
indice2_log_returns<-indice2%>%Ad()%>%dailyReturn(type='log')
indice3_log_returns<-indice3%>%Ad()%>%dailyReturn(type='log')
indice4_log_returns<-indice4%>%Ad()%>%dailyReturn(type='log')
indice5_log_returns<-indice5%>%Ad()%>%dailyReturn(type='log')

#Mean of log stock returns 

indice1_mean_log<-mean(indice1_log_returns)
indice2_mean_log<-mean(indice2_log_returns)
indice3_mean_log<-mean(indice3_log_returns)
indice4_mean_log<-mean(indice4_log_returns)
indice5_mean_log<-mean(indice5_log_returns)

#round it to 4 decimal places

mean_log<-c(indice1_mean_log,indice2_mean_log,indice3_mean_log,indice4_mean_log,indice5_mean_log)
mean_log<-round(mean_log,4)

#standard deviation of log stock returns

indice1_sd_log<-sd(indice1_log_returns)
indice2_sd_log<-sd(indice2_log_returns)
indice3_sd_log<-sd(indice3_log_returns)
indice4_sd_log<-sd(indice4_log_returns)
indice5_sd_log<-sd(indice5_log_returns)

#round it to 4 decimal places 

sd_log<-c(indice1_sd_log,indice2_sd_log,indice3_sd_log,indice4_sd_log,indice5_sd_log)
sd_log<-round(sd_log,4) 
#CHART SERIES ############
#Use R to observe a stock's performance
#chart components: bollinger bands, % bollinger change, volume, moving average convergence divergence

indice1%>%Ad()%>%chartSeries()
indice1%>%chartSeries(TA='addBBands();addBBands(draw="p");addVo();addMACD()',subset='2017')

indice2%>%Ad()%>%chartSeries()
indice2%>%chartSeries(TA='addBBands();addBBands(draw="p");addVo();addMACD()',subset='2017')

indice3%>%Ad()%>%chartSeries()
indice3%>%chartSeries(TA='addBBands();addBBands(draw="p");addVo();addMACD()',subset='2017')

indice4%>%Ad()%>%chartSeries()
indice4%>%chartSeries(TA='addBBands();addBBands(draw="p");addVo();addMACD()',subset='2017')

indice5%>%Ad()%>%chartSeries(indice5$Close,subset='2017')
indice5_log_returns%>%chartSeries(indice5$Close,subset='2017')
indice5%>%chartSeries(TA='addBBands();addBBands(draw="p");addVo();addMACD()',subset='2018')

print(adf.test(indice5_log_returns)) # if p value<0,05 it's good and stationnary


#adj_indice5 <- str_c("as.numeric(indice5$",names(indice5[,6]),"[length(indice5$",names(indice5[,6]),"),])") 
#price[5]<- eval(parse(text = adj_indice5))

#diff_indice5 <- diff(indice5$eval(parse(text = adj_indice5)),lag=1)
#print(adf.test(diff_indice5[-1]))

#CORRELATION TEST #########
#Checking the correlation of 4 stocks: tesla, facebook, google, amazon

library(PerformanceAnalytics)
data<-cbind(diff(log(Cl(indice1))),diff(log(Cl(indice5))),diff(log(Cl(indice4))),diff(log(Cl(indice2))))
chart.Correlation(data)

 
#SMALL TABLE CREATION #########

 
#create data frame

graphic1<-data.frame(rbind(c("indice1",indice1_mean_log,indice1_sd_log),c("indice2",indice2_mean_log,indice2_sd_log),c("indice3",indice3_mean_log,indice3_sd_log),c("indice4",indice4_mean_log,indice4_sd_log),c("indice5",indice5_mean_log,indice5_sd_log)),stringsAsFactors = FALSE)


graphic1<-data.frame(mean_log,sd_log)
rownames(graphic1)<-c("indice1","indice2","indice3","indice4","indice5")
colnames(graphic1)<-c("Mean_Log_Return", "Sd_Log_Return")

graphic1
#Data frame contains the 4 companies with each company's average log return and standard deviation. 

ok  
#RISK REWARD TABLE ####

#Used plotly to create a visualization of each stock's risk v reward. 
#Risk: standard deviation of log returns
#Reward: mean of log returns

xlab<-list(title="Reward", titlefont=TRUE)
ylab<-list(title="Risk", titlefont=TRUE)

plot_ly(x=graphic1[,1],y=graphic1[,2],text=rownames(graphic1),type='scatter',mode="markers",marker=list(color=c("black","blue","red","grey","green")))%>%layout(title="Risk v Reward",xaxis=xlab,yaxis=ylab)

 
#RANDOM WALK #####
#random walk: Rooted in past performance is not an indicator of future results. Price fluctuations can not be predicted with accuracy


mu<-indice1_mean_log
sig<-indice1_sd_log
testsim<-rep(NA,252*4)

#generate random daily exponent increase rate using AMZN's mean and sd log returns

#one year 252 trading days, simulate for 4 years 
# 4*252 trading days

price<-rep(NA,252*4)

#most recent price
adj_indice <- str_c("as.numeric(indice1$",names(indice1[,6]),"[length(indice1$",names(indice1[,6]),"),])")
adj_indice2 <- str_c("as.numeric(indice1$`",names(indice1[,6]),"`[length(indice1$`",names(indice1[,6]),"`),])") 

price[1]<- eval(parse(text = adj_indice2))

#start simulating prices

for(i in 2:length(testsim)){
  price[i]<-price[i-1]*exp(rnorm(1,mu,sig))
}

random_data<-cbind(price,1:(252*4))
colnames(random_data)<-c("Price","Day")
random_data<-as.data.frame(random_data)

random_data%>%ggplot(aes(Day,Price))+geom_line()+labs(title=" (indice1) price simulation for 4 years")+theme_bw()



#Average stock daily return####



probs<-c(0.005,0.025,0.25,0.5,0.75,0.975,0.995)

AMZN_dist<-indice1_log_returns%>%quantile(probs=probs,na.rm=TRUE)
AMZN_mean<-mean(indice1_log_returns,na.rm=TRUE)
AMZN_sd<-sd(indice1_log_returns,na.rm=TRUE)

AMZN_mean%>%exp() # 1.001271

FB_dist<-indice2_log_returns%>%quantile(probs=probs,na.rm=TRUE)
FB_mean<-mean(indice2_log_returns,na.rm=TRUE)
FB_sd<-sd(indice2_log_returns,na.rm=TRUE)

FB_mean%>%exp() # 1.000963

TSLA_dist<-indice3_log_returns%>%quantile(probs=probs,na.rm=TRUE)
TSLA_mean<-mean(indice3_log_returns,na.rm=TRUE)
TSLA_sd<-sd(indice3_log_returns,na.rm=TRUE)

TSLA_mean%>%exp() # 1.001244

AAPL_dist<-indice4_log_returns%>%quantile(probs=probs,na.rm=TRUE)
AAPL_mean<-mean(indice4_log_returns,na.rm=TRUE)
AAPL_sd<-sd(indice4_log_returns,na.rm=TRUE)

AAPL_mean%>%exp() # 1.001057

GOOGL_dist<-indice5_log_returns%>%quantile(probs=probs,na.rm=TRUE)
GOOGL_mean<-mean(indice5_log_returns,na.rm=TRUE)
GOOGL_sd<-sd(indice5_log_returns,na.rm=TRUE)

GOOGL_mean%>%exp() # 1.000651



#MONTE CARLO#####
#monte carlo simulation: incredibly useful forecasting tool to predict outcomes of events with many random variables


N<-500
mc_matrix<-matrix(nrow=252*4,ncol=N)
mc_matrix[1,1]<-eval(parse(text = adj_indice2))

for(j in 1:ncol(mc_matrix)){
  mc_matrix[1,j]<-eval(parse(text = adj_indice2))
  for(i in 2:nrow(mc_matrix)){
    mc_matrix[i,j]<-mc_matrix[i-1,j]*exp(rnorm(1,mu,sig))
  }
}

name<-str_c("Sim ",seq(1,500))
name<-c("Day",name)

final_mat<-cbind(1:(252*4),mc_matrix)
final_mat<-as.tibble(final_mat)
colnames(final_mat)<-name

dim(final_mat) #1008 501

final_mat%>%gather("Simulation","Price",2:501)%>%ggplot(aes(x=Day,y=Price,Group=Simulation))+geom_line(alpha=0.2)+labs(title=" Stock (indice1): 500 Monte Carlo Simulations for 4 Years")+theme_bw()

#is it likely? Check the confidence interval



final_mat[500,-1]%>%as.numeric()%>%quantile(probs=probs)
