##Install packages##
#install.packages("quantmod")
#install.packages("PerformanceAnalytics")
#install.packages("webr")
#install.packages("openxlsx")
library(quantmod)
library(PerformanceAnalytics)
library(webr)
library(openxlsx)

##Import data Five ETFs##
#selecting the adj close price of each stock

symbols=c("QQQ", "XLK" , "PNQI" , "FDN" , "IYW")		
getSymbols(symbols,from="2015-01-01",to="2019-01-01",periodicity="weekly",
           return.class="ts") 

#Storing the Closing Prices of the ETFs to mydat
mydat<-cbind(QQQ[,6], XLK[,6], PNQI[,6], FDN[,6], IYW[,6])	
colnames(mydat)<-c("QQQ", "XLK" , "PNQI" , "FDN" , "IYW")

#Retrieving S&P500 and Treasury Bill(Risk-Free Return)
symbols=c("^GSPC", "^IRX")	
getSymbols(symbols,from="2015-01-01",to="2019-01-01",periodicity="weekly",
           return.class="xts")

#Retrieving Dow Jowns US Technology Index Data from CSV and Storing it in "DJI"
DJI = read.csv(file="DJUSTC.csv", header=TRUE,  sep=",")

#Storing S&P500 and DJUSTC closing prices in "markdat"
markdat = cbind(GSPC[,6],DJI[,1])
colnames(markdat)<-c( "GSPC", "DJI")

#Retrieving the Tracking Indexes (TI) of the 5 ETFs
DJINET = read.csv(file="DJINET.csv", header=TRUE,  sep=",") #TI for FDN
IXTNTR = read.csv(file="IXTNTR.csv", header=TRUE,  sep=",") #TI for XLK
NDX = read.csv(file="NDX.csv", header=TRUE,  sep=",")       #TI for QQQ
QNET = read.csv(file="QNET.csv", header=TRUE,  sep=",")     #TI for PNQI
#NOTE: ETF "IYW" tracks the DJUSTC index

#Storing Tracking Index Prices in "ETFindex"
ETFindex = cbind(DJINET[,1],IXTNTR[,1],NDX[,1],QNET[,1], DJI[,1])
colnames(ETFindex)<-c("DJINET", "IXTNTR", "NDX", "QNET","DJIX")


###Question A.###
#weekly return of each ETF
wkQQQret = diff(log(mydat[,1]))
wkXLKret = diff(log(mydat[,2]))
wkPNQIret = diff(log(mydat[,3]))
wkFDNret = diff(log(mydat[,4]))
wkIYWret = diff(log(mydat[,5]))

#Storing the Weekly Returns of Each ETF in the variable "ret" 
ret = cbind(wkQQQret,wkXLKret,wkPNQIret,wkFDNret,wkIYWret)
colnames(ret) = c("QQQ", "XLK" , "PNQI" , "FDN" , "IYW" )

#Calculating Weekly Returns of S&P500 and DJUSTC
wkGSPCret = diff(log(markdat[,1]))
wkDJIret = diff(log(markdat[,2]))

#Storing the Weekly Returns of S&P500 and DJUSTC in "markret"
markret = cbind(wkGSPCret,wkDJIret)
colnames(markret) = c("GSPC","DJI")
markret=na.omit(markret)

#Calculating the Weekly Returns of the Tracking Indexes of the 5 ETFs
wkDJINETret = diff(log(ETFindex[,1]))
wkIXTNTRret = diff(log(ETFindex[,2]))
wkNDXret = diff(log(ETFindex[,3]))
wkQNETret = diff(log(ETFindex[,4]))
wkDJIXret = diff(log(ETFindex[,5]))

#Storing the Weekly Returns of the Tracking Indexes in "indexret"
indexret = cbind(wkDJINETret,wkIXTNTRret,wkNDXret,wkQNETret,wkDJIXret)
colnames(indexret) = c("DJINET", "IXTNTR", "NDX", "QNET","DJIX")

# Histogram of Weekly Returns of QQQ ETF
hist(ret[,1],breaks = 50,prob=TRUE,main=paste("Histogram of QQQ"))
curve(dnorm(x,mean = mean(ret[,1]),sd = sd(ret[,1])),add = TRUE,col = "red",lwd = 2)

# Histogram of Weekly Returns of XLK ETF
hist(ret[,2],breaks = 50,prob=TRUE,main=paste("Histogram of XLK"))
curve(dnorm(x,mean = mean(ret[,2]),sd = sd(ret[,2])),add = TRUE,col = "red",lwd = 2)

# Histogram of Weekly Returns of PNQI ETF
hist(ret[,3],breaks = 50,prob=TRUE,main=paste("Histogram of PNQI"))
curve(dnorm(x,mean = mean(ret[,3]),sd = sd(ret[,3])),add = TRUE,col = "red",lwd = 2)

# Histogram of Weekly Returns of FDN ETF
hist(ret[,4],breaks = 50,prob=TRUE,main=paste("Histogram of FDN"))
curve(dnorm(x,mean = mean(ret[,4]),sd = sd(ret[,4])),add = TRUE,col = "red",lwd = 2)

# Histogram of Weekly Returns of IYW ETF
hist(ret[,5],breaks = 50,prob=TRUE,main=paste("Histogram of IYW"))
curve(dnorm(x,mean = mean(ret[,5]),sd = sd(ret[,5])),add = TRUE,col = "red",lwd = 2)

###Question B.###
#Calculating the Variance, Covariance, and Correlation of ETFs' Weekly Returns
var(ret) #variance and covariance
cor(ret) #correlation

#Retrieving Risk free return by using "IRX"
estimate_1 = matrix(0,nrow = 2,ncol = 5,dimnames = list(c("es.mean","es.sd"), c("QQQ", "XLK" , "PNQI" , "FDN" , "IYW" )))
for(i in 1:5)
{
  estimate_1[1,i] = mean(mydat[,i])/5       
  estimate_1[2,i] = sd(mydat[,i])/sqrt(5)
}
estimate_1

#Question B: Relative risks
RR = matrix(0,nrow = 7,ncol = 5,dimnames = list(c("Mean","SD","CV", "VaR", "Kurtosis", "Skewness"), c("QQQ", "XLK" , "PNQI" , "FDN" , "IYW" )))
for(i in 1:5)
{
  RR[1,i] = mean(mydat[,i]) 
  RR[2,i] = sd(mydat[,i])
  RR[3,i] = sd(mydat[,i])/mean(mydat[,i]) 
  RR[4,i] = quantile( ret[,i], 0.05)  
  RR[5,i] = kurtosis(mydat[,i])
  RR[6,i] = skewness(mydat[,i])
}
RR

#Question B: Performance of funds
#Comparing the Returns of ETFs to the 
ret_compar = cbind(wkQQQret, wkPNQIret, wkIYWret, wkXLKret, wkFDNret, wkGSPCret, wkDJIret)
ret_compar = na.omit(ret_compar)

pof = matrix(0,nrow = 1,ncol = 7,dimnames = list(c("mean"), c("QQQret", "XLKret" , "PNQIret" , "FDNret" , "IYWret", "GSPCret", "DJIret" )))
for(i in 1:7)
{
  pof[1,i] = mean(ret_compar[,i])     
}
pof


###Question C.###

#Question C: Tracking Error
t.error = matrix(0,nrow = 1,ncol = 5,dimnames = list(c("TrackingError"), c("QQQ", "XLK" , "PNQI" , "FDN" , "IYW" )))
for(i in 1:5)
{
  t.error[1:1] = sd(ret[,1]-indexret[,3])
  t.error[1:2] = sd(ret[,2]-indexret[,2])
  t.error[1:3] = sd(ret[,3]-indexret[,4])
  t.error[1:4] = sd(ret[,4]-indexret[,1])
  t.error[1:5] = sd(ret[,5]-indexret[,5])
}
t.error

#Hypothesis
QQQttest = t.test(ret[,1], mu = mean(indexret[,3]) , alternative = "greater")
QQQftest = var.test(ret[,1], indexret[,3])
plot(QQQttest)
plot(QQQftest)
XLKttest = t.test(ret[,2], mu = mean(indexret[,2]) , alternative = "greater")
XLKftest = var.test(ret[,2], indexret[,2])
plot(XLKttest)
plot(XLKftest)
PNQIttest = t.test(ret[,3], mu = mean(indexret[,4]) , alternative = "greater")
PNQIftest = var.test(ret[,3], indexret[,4])
plot(PNQIttest)
plot(PNQIftest)
FDNttest = t.test(ret[,4], mu = mean(indexret[,1]) , alternative = "greater")
FDNftest = var.test(ret[,4], indexret[,1])
plot(FDNttest)
plot(FDNftest)
IYWttest = t.test(ret[,5], mu = mean(indexret[,5]) , alternative = "greater")
IYWftest = var.test(ret[,5], indexret[,5])
plot(IYWttest)
plot(IYWftest)


###Question D.###

wkRFret = IRX[-1,6]/52/100
wkRFret
#Excess return
QQQ_ER = ret_compar[,1]-wkRFret
XLK_ER = ret_compar[,2]-wkRFret
PNQI_ER = ret_compar[,3]-wkRFret
FDN_ER = ret_compar[,4]-wkRFret
IYW_ER = ret_compar[,5]-wkRFret
GSPC_ER = ret_compar[,6]-wkRFret
DJUSTC_ER = ret_compar[,7]-wkRFret

#Storing the Excess Returns from above into "Excess_Returns"
Excess_Returns = cbind(QQQ_ER, XLK_ER, PNQI_ER, FDN_ER, IYW_ER, GSPC_ER, DJUSTC_ER)
colnames(Excess_Returns) = c("QQQ_ER", "XLK_ER" , "PNQI_ER" , "FDN_ER" , "IYW_ER", "GSPC_ER", "DJUSTC_ER")

#Using CAPM Single Factor Model to Regress ETFs' Excess Returns on DJUSTC Market Excess Returns

#Model for QQQ ETF
QQQ.model = lm(QQQ_ER~DJUSTC_ER, data = Excess_Returns)
summary(QQQ.model)

#Plotting QQQ ETF Single Factor Model (Regression Plot)
plot.default(x = Excess_Returns$DJUSTC_ER, y = Excess_Returns$QQQ_ER, main = "CAPM for QQQ", 
             xlab = "Market Excess Return", ylab = "QQQ Excess Return")
abline(QQQ.model)
abline(h = 0, v = 0, lty = 3)
QQQ.alpha = coef(summary(QQQ.model))[1,1]
QQQ.beta = coef(summary(QQQ.model))[2,1]
legend("topleft", legend=
         c(paste("alpha =",round(QQQ.alpha,dig=5)),
           paste("beta =",round(QQQ.beta,dig=5))), cex=1, bty="n")

##Model for XLK ETF##
XLK.model = lm(XLK_ER~DJUSTC_ER, data = Excess_Returns)
summary(XLK.model)

#Plotting XLK ETF Single Factor Model (Regression Plot)
plot.default(x = Excess_Returns$DJUSTC_ER, y = Excess_Returns$XLK_ER, main = "CAPM for XLK", 
             xlab = "Market Excess Return", ylab = "XLK Excess Return")
abline(XLK.model)
abline(h = 0, v = 0, lty = 3)
XLK.alpha = coef(summary(XLK.model))[1,1]
XLK.beta = coef(summary(XLK.model))[2,1]
legend("topleft", legend=
         c(paste("alpha =",round(XLK.alpha,dig=5)),
           paste("beta =",round(XLK.beta,dig=5))), cex=1, bty="n")

##Model for PNQI ETF##
PNQI.model = lm(PNQI_ER~DJUSTC_ER, data = Excess_Returns)

#PNQI Model Summary
summary(PNQI.model)

#Plotting PNQI ETF Single Factor Model (Regression Plot)
plot.default(x = Excess_Returns$DJUSTC_ER, y = Excess_Returns$PNQI_ER, main = "CAPM for PNQI", 
             xlab = "Market Excess Return", ylab = "PNQI Excess Return")
abline(PNQI.model)
abline(h = 0, v = 0, lty = 3)
PNQI.alpha = coef(summary(PNQI.model))[1,1]
PNQI.beta = coef(summary(PNQI.model))[2,1]
legend("topleft", legend=
         c(paste("alpha =",round(PNQI.alpha,dig=5)),
           paste("beta =",round(PNQI.beta,dig=5))), cex=1, bty="n")

#Model for FDN ETF
FDN.model = lm(FDN_ER~DJUSTC_ER, data = Excess_Returns)

#FDN Model Summary 
summary(FDN.model)

#Plotting FDN ETF Single Factor Model (Regression Plot)
plot.default(x = Excess_Returns$DJUSTC_ER, y = Excess_Returns$FDN_ER, main = "CAPM for FDN", 
             xlab = "Market Excess Return", ylab = "FDN Excess Return")
abline(FDN.model)
abline(h = 0, v = 0, lty = 3)
FDN.alpha = coef(summary(FDN.model))[1,1]
FDN.beta = coef(summary(FDN.model))[2,1]
legend("topleft", legend=
         c(paste("alpha =",round(FDN.alpha,dig=5)),
           paste("beta =",round(FDN.beta,dig=5))), cex=1, bty="n")

#Model for FDN ETF
IYW.model = lm(IYW_ER~DJUSTC_ER, data = Excess_Returns)

#FDN Model Summary 
summary(IYW.model)

#Plotting FDN Model
plot.default(x = Excess_Returns$DJUSTC_ER, y = Excess_Returns$IYW_ER, main = "CAPM for IYW", 
             xlab = "Market Excess Return", ylab = "IYW Excess Return")
abline(IYW.model)
abline(h = 0, v = 0, lty = 3)
IYW.alpha = coef(summary(IYW.model))[1,1]
IYW.beta = coef(summary(IYW.model))[2,1]
legend("topleft", legend=
         c(paste("alpha =",round(IYW.alpha,dig=5)),
           paste("beta =",round(IYW.beta,dig=5))), cex=1, bty="n")

##Question E. ANOVA ANALYSIS##
QQQ.anova = anova(QQQ.model)
QQQ.summary = summary(QQQ.model)
QQQ.summary
QQQ.anova
write.xlsx(QQQ.anova, "QQQ_anova.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)


plot(QQQ.model, which = 1, main = "ETF: QQQ")
plot(QQQ.model, which = 2, main = "ETF: QQQ")
stand_resid = rstandard(QQQ.model)
hist(stand_resid, main = "QQQ Standardized Residuals")


#ANOVA TEST ON XLK 
XLK.anova = anova(XLK.model)
XLK.anova
write.xlsx(XLK.anova, "XLK.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)
XLK.summary = summary(XLK.model)
XLK.summary


plot(XLK.model, which = 1, main = "ETF: XLK")
plot(XLK.model, which = 2, main = "ETF: XLK")
stand_resid_XLK = rstandard(XLK.model)
hist(stand_resid_XLK, main = "XLK Standardized Residuals")

#ANOVA TEST ON PNQI
PNQI.anova = anova(PNQI.model)
PNQI.anova
write.xlsx(PNQI.anova, "PNQI_anova.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)
PNQI.summary = summary(PNQI.model)
PNQI.summary


plot(PNQI.model, which = 1, main = "ETF: PNQI")
plot(PNQI.model, which = 2, main = "ETF: PNQI")
stand_resid_PNQI = rstandard(PNQI.model)
hist(stand_resid_PNQI, main = "PNQI Standardized Residuals")

#ANOVA TEST ON FDN
FDN.anova = anova(FDN.model)
FDN.anova
write.xlsx(FDN.anova, "FDN_anova.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)
FDN.summary = summary(FDN.model)
FDN.summary

par(mfrow = c(2,2))
plot(FDN.model, which = 1, main = "ETF: FDN")
plot(FDN.model, which = 2, main = "ETF: FDN")
stand_resid_FDN = rstandard(FDN.model)
hist(stand_resid_FDN, main = "FDN Standardized Residuals")

#ANOVA TEST ON IYW
IYW.anova = anova(IYW.model)
IYW.anova
write.xlsx(IYW.anova, "IYW_anova.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)
IYW.summary = summary(IYW.model)
IYW.summary


plot(IYW.model, which = 1, main = "ETF: IYW")
plot(IYW.model, which = 2, main = "ETF: IYW")
stand_resid_IYW = rstandard(IYW.model)
hist(stand_resid_IYW, main = "IYW Standardized Residuals")


#Storing all R-Squared Values to "R_squared_table"
R_squared_table = cbind(QQQ.summary[["adj.r.squared"]], XLK.summary[["adj.r.squared"]],
                        PNQI.summary[["adj.r.squared"]], FDN.summary[["adj.r.squared"]],
                        IYW.summary[["adj.r.squared"]])
colnames(R_squared_table) = c("QQQ", "XLK", "PNQI", "FDN", "IYW")
row.names(R_squared_table) = "R-Square"
R_squared_table