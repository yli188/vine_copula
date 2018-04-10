library(quantmod); library(magrittr); library(rugarch); 
library(VineCopula); library(doParallel); library(iterators);
library(psych);library(dplyr);library(forecast);library(copula);
library(rgl);library(MASS)
options(scipen = 200)
## data acquiring
sp = getSymbols('^GSPC',from = '2006-01-01',to = '2017-12-31',auto.assign = F) %>% Ad()
vox = getSymbols('VOX',from = '2006-01-01',to = '2017-12-31',auto.assign = F) %>% Ad()
xlf = getSymbols('XLF',from = '2006-01-01',to = '2017-12-31',auto.assign = F) %>% Ad()
xlk = getSymbols('XLK',from = '2006-01-01',to = '2017-12-31',auto.assign = F) %>% Ad()
xlb = getSymbols('XLB',from = '2006-01-01',to = '2017-12-31',auto.assign = F) %>% Ad()
xli = getSymbols('XLI',from = '2006-01-01',to = '2017-12-31',auto.assign = F) %>% Ad()
xlu = getSymbols('XLU',from = '2006-01-01',to = '2017-12-31',auto.assign = F) %>% Ad()
xlp = getSymbols('XLP',from = '2006-01-01',to = '2017-12-31',auto.assign = F) %>% Ad()
xly = getSymbols('XLY',from = '2006-01-01',to = '2017-12-31',auto.assign = F) %>% Ad()
xle = getSymbols('XLE',from = '2006-01-01',to = '2017-12-31',auto.assign = F) %>% Ad()
xlv = getSymbols('XLV',from = '2006-01-01',to = '2017-12-31',auto.assign = F) %>% Ad()


all_data = merge(sp,vox,xlf,xlk,xlb,xli,xlu,xlp,xly,xle,xlv)
all_logrt = apply(all_data,MARGIN = 2,FUN = function(x){
  diff(log(x),lag = 1)
})

in_ret_all = all_logrt[1:2659,]
out_ret_all = all_logrt[2660:3019,]

write.csv(all_data,'C:/Users/yl/Desktop/alldata.csv')
## data split
in_sp = sp[1:2660,]; out_sp = sp[2660:3019,]
in_vox = vox[1:2660,]; out_vox = vox[2660:3019,]
in_xlf = xlf[1:2660,]; out_xlf = xlf[2660:3019,]
in_xlk = xlk[1:2660,]; out_xlk = xlk[2660:3019,]
in_xlb = xlb[1:2660,]; out_xlb = xlb[2660:3019,]
in_xli = xli[1:2660,]; out_xli = xli[2660:3019,]
in_xlu = xlu[1:2660,]; out_xlu = xlu[2660:3019,]
in_xlp = xlp[1:2660,]; out_xlp = xlp[2660:3019,]
in_xly = xly[1:2660,]; out_xly = xly[2660:3019,]
in_xle = xle[1:2660,]; out_xle = xle[2660:3019,]
in_xlv = xlv[1:2660,]; out_xlv = xlv[2660:3019,]


## log return
sp_logrt = diff(log(sp),lag = 1)[-1]
vox_logrt = diff(log(vox),lag = 1)[-1]
xlf_logrt = diff(log(xlf),lag = 1)[-1]
xlk_logrt = diff(log(xlk),lag = 1)[-1]
xlb_logrt = diff(log(xlb),lag = 1)[-1]
xli_logrt = diff(log(xli),lag = 1)[-1]
xlu_logrt = diff(log(xlu),lag = 1)[-1]
xlp_logrt = diff(log(xlp),lag = 1)[-1]
xly_logrt = diff(log(xly),lag = 1)[-1]
xle_logrt = diff(log(xle),lag = 1)[-1]
xlv_logrt = diff(log(xlv),lag = 1)[-1]


## in sample return
in_sp_logrt = diff(log(in_sp),lag = 1)[-1]
in_vox_logrt = diff(log(in_vox),lag = 1)[-1]
in_xlf_logrt = diff(log(in_xlf),lag = 1)[-1]
in_xlk_logrt = diff(log(in_xlk),lag = 1)[-1]
in_xlb_logrt = diff(log(in_xlb),lag = 1)[-1]
in_xli_logrt = diff(log(in_xli),lag = 1)[-1]
in_xlu_logrt = diff(log(in_xlu),lag = 1)[-1]
in_xlp_logrt = diff(log(in_xlp),lag = 1)[-1]
in_xly_logrt = diff(log(in_xly),lag = 1)[-1]
in_xle_logrt = diff(log(in_xle),lag = 1)[-1]
in_xlv_logrt = diff(log(in_xlv),lag = 1)[-1]



## out sample return
out_sp_logrt = diff(log(out_sp),lag = 1)[-1]
out_vox_logrt = diff(log(out_vox),lag = 1)[-1]
out_xlf_logrt = diff(log(out_xlf),lag = 1)[-1]
out_xlk_logrt = diff(log(out_xlk),lag = 1)[-1]
out_xlb_logrt = diff(log(out_xlb),lag = 1)[-1]
out_xli_logrt = diff(log(out_xli),lag = 1)[-1]
out_xlu_logrt = diff(log(out_xlu),lag = 1)[-1]
out_xlp_logrt = diff(log(out_xlp),lag = 1)[-1]
out_xly_logrt = diff(log(out_xly),lag = 1)[-1]
out_xle_logrt = diff(log(out_xle),lag = 1)[-1]
out_xlv_logrt = diff(log(out_xlv),lag = 1)[-1]


#in_ret_all = data.frame(in_sp_logrt,in_vox_logrt,in_xlf_logrt,in_xlk_logrt,
                        #in_xlb_logrt,in_xli_logrt,in_xlu_logrt,in_xlp_logrt,
                        #in_xly_logrt,in_xle_logrt,in_xlv_logrt) %>% as.matrix()

#out_ret_all = data.frame(out_sp_logrt,out_vox_logrt,out_xlf_logrt,out_xlk_logrt,
                         #out_xlb_logrt,out_xli_logrt,out_xlu_logrt,out_xlp_logrt,
                         #out_xly_logrt,out_xle_logrt,out_xlv_logrt)



## get optimal p and q
get_pq = function(data,max_p=5,max_q=5){
  final.aic = 0
  final.order = c(0,0,0)
  for(p in 0:max_p) for(q in 0:max_q){
    if(p == 0 && q == 0){
      next
    }
    arimaFit = tryCatch(arima(data,order = c(p,0,q)),
                        error = function(err) FALSE,
                        warning = function(err) FALSE)
    
    if(!is.logical(arimaFit)){
      
      current.aic <- AIC(arimaFit)
      if(current.aic < final.aic){
        
        final.aic <- current.aic
        final.order <- c(p,0,q)
        final.arima <- arima(data,order = final.order)
      }
    }else{
      next
    }
  }
  return(final.order)
}


xixi = apply(in_ret_all,MARGIN = 2,FUN = get_pq)
## function for converting matrics to list
mat2list = function(data){
  return(as.list(as.data.frame(data,stringAsFactors = FALSE)))
}

sp_meanmod = list(armaOrder = c(3,3),include.mean = T)
vox_meanmod = list(armaOrder = c(5,5),include.mean = T)
xlf_meanmod = list(armaOrder = c(5,4),include.mean = T)
xlk_meanmod = list(armaOrder = c(4,5),include.mean = T)
xlb_meanmod = list(armaOrder = c(5,4),include.mean = T)
xli_meanmod = list(armaOrder = c(5,5),include.mean = T)
xlu_meanmod = list(armaOrder = c(3,2),include.mean = T)
xlp_meanmod = list(armaOrder = c(3,5),include.mean = T)
xly_meanmod = list(armaOrder = c(3,3),include.mean = T)
xle_meanmod = list(armaOrder = c(2,3),include.mean = T)
xlv_meanmod = list(armaOrder = c(2,3),include.mean = T)
varmod = list(model = 'eGARCH',garchOrder = c(1,1))


## model specification
sp_uspec = ugarchspec(varmod,mean.model = sp_meanmod,distribution.model = 'sstd')
vox_uspec = ugarchspec(varmod,mean.model = vox_meanmod,distribution.model = 'sstd')
xlf_uspec = ugarchspec(varmod,mean.model = xlf_meanmod,distribution.model = 'sstd')
xlk_uspec = ugarchspec(varmod,mean.model = xlk_meanmod,distribution.model = 'sstd')
xlb_uspec = ugarchspec(varmod,mean.model = xlb_meanmod,distribution.model = 'sstd')
xli_uspec = ugarchspec(varmod,mean.model = xli_meanmod,distribution.model = 'sstd')
xlu_uspec = ugarchspec(varmod,mean.model = xlu_meanmod,distribution.model = 'sstd')
xlp_uspec = ugarchspec(varmod,mean.model = xlp_meanmod,distribution.model = 'sstd')
xly_uspec = ugarchspec(varmod,mean.model = xly_meanmod,distribution.model = 'sstd')
xle_uspec = ugarchspec(varmod,mean.model = xle_meanmod,distribution.model = 'sstd')
xlv_uspec = ugarchspec(varmod,mean.model = xlv_meanmod,distribution.model = 'sstd')


## fit model
fit_sp = ugarchfit(spec = sp_uspec,data = in_sp_logrt,solver = 'hybrid',
                            parallel = T,core = 4)
fit_vox = ugarchfit(spec = vox_uspec,data = in_vox_logrt,solver = 'hybrid',
                            parallel = T,core = 4)
fit_xlf = ugarchfit(spec = xlf_uspec,data = in_xlf_logrt,solver = 'hybrid',
                            parallel = T,core = 4)
fit_xlk = ugarchfit(spec = xlk_uspec,data = in_xlk_logrt,solver = 'hybrid',
                            parallel = T,core = 4)
fit_xlb = ugarchfit(spec = xlb_uspec,data = in_xlb_logrt,solver = 'hybrid',
                            parallel = T,core = 4)
fit_xli = ugarchfit(spec = xli_uspec,data = in_xli_logrt,solver = 'hybrid',
                            parallel = T,core = 4)
fit_xlu = ugarchfit(spec = xlu_uspec,data = in_xlu_logrt,solver = 'hybrid',
                            parallel = T,core = 4)
fit_xlp = ugarchfit(spec = xlp_uspec,data = in_xlp_logrt,solver = 'hybrid',
                            parallel = T,core = 4)
fit_xly = ugarchfit(spec = xly_uspec,data = in_xly_logrt,solver = 'hybrid',
                            parallel = T,core = 4)
fit_xle = ugarchfit(spec = xle_uspec,data = in_xle_logrt,solver = 'hybrid',
                            parallel = T,core = 4)
fit_xlv = ugarchfit(spec = xlv_uspec,data = in_xlv_logrt,solver = 'hybrid',
                            parallel = T,core = 4)


cva = c()
for(i in 0:(length(in_sp_logrt)-10)){
  cva = c(cva,CVaR(xts(zoo(in_sp_logrt[(1+i):(10+i)]))))
}
varr = -fit_sp@fit$sigma[10:2659]
cva= c(0,0,0,0,0,0,0,0,0,cva)

var_cvar = fit_sp@fit$cvar/fit_sp@fit$cvar



cl = makeCluster(4)
## backtest for the prediction
sp_roll = ugarchroll(sp_uspec,sp_logrt,n.start=2659,refit.every = 10,refit.window ='moving',
                     solver = 'hybrid',calculate.VaR = TRUE,VaR.alpha = 0.01,
                     keep.coef = T,cluster = cl)
vox_roll = ugarchroll(vox_uspec,vox_logrt,n.start=2659,refit.every = 10,refit.window ='moving',
                      solver = 'hybrid',calculate.VaR = TRUE,VaR.alpha = 0.01,
                      keep.coef = T,cluster = cl)
xlf_roll = ugarchroll(xlf_uspec,xlf_logrt,n.start=2659,refit.every = 10,refit.window ='moving',
                      solver = 'hybrid',calculate.VaR = TRUE,VaR.alpha = 0.01,
                      keep.coef = T,cluster = cl)
xlk_roll = ugarchroll(xlk_uspec,xlk_logrt,n.start=2659,refit.every = 10,refit.window ='moving',
                      solver = 'hybrid',calculate.VaR = TRUE,VaR.alpha = 0.01,
                      keep.coef = T,cluster = cl)
xlb_roll = ugarchroll(xlb_uspec,xlb_logrt,n.start=2659,refit.every = 10,refit.window ='moving',
                      solver = 'hybrid',calculate.VaR = TRUE,VaR.alpha = 0.01,
                      keep.coef = T,cluster = cl)
xli_roll = ugarchroll(xli_uspec,xli_logrt,n.start=2659,refit.every = 10,refit.window ='moving',
                      solver = 'hybrid',calculate.VaR = TRUE,VaR.alpha = 0.01,
                      keep.coef = T,cluster = cl)
xlu_roll = ugarchroll(xlu_uspec,xlu_logrt,n.start=2659,refit.every = 10,refit.window ='moving',
                      solver = 'hybrid',calculate.VaR = TRUE,VaR.alpha = 0.01,
                      keep.coef = T,cluster = cl)
xlp_roll = ugarchroll(xlp_uspec,xlp_logrt,n.start=2659,refit.every = 10,refit.window ='moving',
                      solver = 'hybrid',calculate.VaR = TRUE,VaR.alpha = 0.01,
                      keep.coef = T,cluster = cl)
xly_roll = ugarchroll(xly_uspec,xly_logrt,n.start=2659,refit.every = 10,refit.window ='moving',
                      solver = 'hybrid',calculate.VaR = TRUE,VaR.alpha = 0.01,
                      keep.coef = T,cluster = cl)
xle_roll = ugarchroll(xle_uspec,xle_logrt,n.start=2659,refit.every = 10,refit.window ='moving',
                      solver = 'hybrid',calculate.VaR = TRUE,VaR.alpha = 0.01,
                      keep.coef = T,cluster = cl)
xlv_roll = ugarchroll(xlv_uspec,xlv_logrt,n.start=2659,refit.every = 10,refit.window ='moving',
                      solver = 'hybrid',calculate.VaR = TRUE,VaR.alpha = 0.01,
                      keep.coef = T,cluster = cl)
stopCluster(cl)

aaa =report(sp_roll)




###################################################################
par(mfrow=c(2,2))
##sp 
sp_fore_var = zoo(sp_roll@forecast$VaR[,1])
index(sp_fore_var) = as.Date(rownames(sp_roll@forecast$VaR))
sp_actual_var = zoo(sp_roll@forecast$VaR[,2])
index(sp_actual_var) = as.Date(rownames(sp_roll@forecast$VaR))
plot(sp_actual_var,type='p',main = 'SP500 1-day ahead VaR at 99% conf level',xlab = 'Date',
     ylab = 'VaR',col='green',pch = 19)
lines(sp_fore_var,col = 2,lwd=2)
legend('bottomright',legend = c('actual','forecast'),col = c('green','red'),lwd=2,lty = c(NA,1),pch = c(19,NA))
##vox
vox_fore_var = zoo(vox_roll@forecast$VaR[,1])
index(vox_fore_var) = as.Date(rownames(vox_roll@forecast$VaR))
vox_actual_var = zoo(vox_roll@forecast$VaR[,2])
index(vox_actual_var) = as.Date(rownames(vox_roll@forecast$VaR))
plot(vox_actual_var,type='p',main = 'VOX one-day ahead VaR at 99% conf level',xlab = 'Date',
     ylab = 'VaR',col='green',pch = 19)
lines(vox_fore_var,col = 2,lwd=2)
legend('bottomright',legend = c('actual','forecast'),col = c('green','red'),lwd=2,lty = c(NA,1),pch = c(19,NA))
##xlf 
xlf_fore_var = zoo(xlf_roll@forecast$VaR[,1])
index(xlf_fore_var) = as.Date(rownames(xlf_roll@forecast$VaR))
xlf_actual_var = zoo(xlf_roll@forecast$VaR[,2])
index(xlf_actual_var) = as.Date(rownames(xlf_roll@forecast$VaR))
plot(xlf_actual_var,type='p',main = 'XLF 1-day ahead VaR at 99% conf level',xlab = 'Date',
     ylab = 'VaR',col='green',pch = 19)
lines(xlf_fore_var,col = 2,lwd=2)
legend('bottomright',legend = c('actual','forecast'),col = c('green','red'),lwd=2,lty = c(NA,1),pch = c(19,NA))
##xlk
xlk_fore_var = zoo(xlk_roll@forecast$VaR[,1])
index(xlk_fore_var) = as.Date(rownames(xlk_roll@forecast$VaR))
xlk_actual_var = zoo(xlk_roll@forecast$VaR[,2])
index(xlk_actual_var) = as.Date(rownames(xlk_roll@forecast$VaR))
plot(xlk_actual_var,type='p',main = 'XLK 1-day ahead VaR at 99% conf level',xlab = 'Date',
     ylab = 'VaR',col='green',pch = 19)
lines(xlk_fore_var,col = 2,lwd=2)
legend('bottomright',legend = c('actual','forecast'),col = c('green','red'),lwd=2,lty = c(NA,1),pch = c(19,NA))

##################################################################
par(mfrow=c(2,2))
##xlb
xlb_fore_var = zoo(xlb_roll@forecast$VaR[,1])
index(xlb_fore_var) = as.Date(rownames(xlb_roll@forecast$VaR))
xlb_actual_var = zoo(xlb_roll@forecast$VaR[,2])
index(xlb_actual_var) = as.Date(rownames(xlb_roll@forecast$VaR))
plot(xlb_actual_var,type='p',main = 'XLB 1-day ahead VaR at 99% conf level',xlab = 'Date',
     ylab = 'VaR',col='green',pch = 19)
lines(xlb_fore_var,col = 2,lwd=2)
legend('bottomright',legend = c('actual','forecast'),col = c('green','red'),lwd=2,lty = c(NA,1),pch = c(19,NA))
##xli
xli_fore_var = zoo(xli_roll@forecast$VaR[,1])
index(xli_fore_var) = as.Date(rownames(xli_roll@forecast$VaR))
xli_actual_var = zoo(xli_roll@forecast$VaR[,2])
index(xli_actual_var) = as.Date(rownames(xli_roll@forecast$VaR))
plot(xli_actual_var,type='p',main = 'XLI 1-day ahead VaR at 99% conf level',xlab = 'Date',
     ylab = 'VaR',col='green',pch = 19)
lines(xli_fore_var,col = 2,lwd=2)
legend('bottomright',legend = c('actual','forecast'),col = c('green','red'),lwd=2,lty = c(NA,1),pch = c(19,NA))
##xlu
xlu_fore_var = zoo(xlu_roll@forecast$VaR[,1])
index(xlu_fore_var) = as.Date(rownames(xlu_roll@forecast$VaR))
xlu_actual_var = zoo(xlu_roll@forecast$VaR[,2])
index(xlu_actual_var) = as.Date(rownames(xlu_roll@forecast$VaR))
plot(xlu_actual_var,type='p',main = 'XLU 1-day ahead VaR at 99% conf level',xlab = 'Date',
     ylab = 'VaR',col='green',pch = 19)
lines(xlu_fore_var,col = 2,lwd=2)
legend('bottomright',legend = c('actual','forecast'),col = c('green','red'),lwd=2,lty = c(NA,1),pch = c(19,NA))
##xlp
xlp_fore_var = zoo(xlp_roll@forecast$VaR[,1])
index(xlp_fore_var) = as.Date(rownames(xlp_roll@forecast$VaR))
xlp_actual_var = zoo(xlp_roll@forecast$VaR[,2])
index(xlp_actual_var) = as.Date(rownames(xlp_roll@forecast$VaR))
plot(xlp_actual_var,type='p',main = 'XLP 1-day ahead VaR at 99% conf level',xlab = 'Date',
     ylab = 'VaR',col='green',pch = 19)
lines(xlp_fore_var,col = 2,lwd=2)
legend('bottomright',legend = c('actual','forecast'),col = c('green','red'),lwd=2,lty = c(NA,1),pch = c(19,NA))
#######################################################################
par(mfrow=c(2,2))
##xly
xly_fore_var = zoo(xly_roll@forecast$VaR[,1])
index(xly_fore_var) = as.Date(rownames(xly_roll@forecast$VaR))
xly_actual_var = zoo(xly_roll@forecast$VaR[,2])
index(xly_actual_var) = as.Date(rownames(xly_roll@forecast$VaR))
plot(xly_actual_var,type='p',main = 'XLY 1-day ahead VaR at 99% conf level',xlab = 'Date',
     ylab = 'VaR',col='green',pch = 19)
lines(xly_fore_var,col = 2,lwd=2)
legend('bottomright',legend = c('actual','forecast'),col = c('green','red'),lwd=2,lty = c(NA,1),pch = c(19,NA))
##xle
xle_fore_var = zoo(xle_roll@forecast$VaR[,1])
index(xle_fore_var) = as.Date(rownames(xle_roll@forecast$VaR))
xle_actual_var = zoo(xle_roll@forecast$VaR[,2])
index(xle_actual_var) = as.Date(rownames(xle_roll@forecast$VaR))
plot(xle_actual_var,type='p',main = 'XLE 1-day ahad VaR at 99%',xlab = 'Date',
     ylab = 'VaR',col='green',pch = 19)
lines(xle_fore_var,col = 2,lwd=2)
legend('bottomright',legend = c('actual','forecast'),col = c('green','red'),lwd=2,lty = c(NA,1),pch = c(19,NA))
##xlv
xlv_fore_var = zoo(xlv_roll@forecast$VaR[,1])
index(xlv_fore_var) = as.Date(rownames(xlv_roll@forecast$VaR))
xlv_actual_var = zoo(xlv_roll@forecast$VaR[,2])
index(xlv_actual_var) = as.Date(rownames(xlv_roll@forecast$VaR))
plot(xlv_actual_var,type='p',main = 'XLV 1-day ahead VaR at 99%',xlab = 'Date',
     ylab = 'VaR',col='green',pch = 19)
lines(xlv_fore_var,col = 2,lwd=2)
legend('bottomright',legend = c('actual','forecast'),col = c('green','red'),lwd=2,lty = c(NA,1),pch = c(19,NA))

# 
# # forecast 
sp_forecast = ugarchforecast(fit_sp,n.ahead = 360)

# sp_forecast

# n.start-->from which period I wanna start the backtest
## if not converge, change n.start for trying



## setting distribution model for each sector,and convert to list
# extract z(standardized residuals)


z_sp = residuals(fit_sp,standardize = T)
z_vox = residuals(fit_vox,standardize = T)
z_xlf = residuals(fit_xlf,standardize = T)
z_xlk = residuals(fit_xlk,standardize = T)
z_xlb = residuals(fit_xlb,standardize = T)
z_xli = residuals(fit_xli,standardize = T)
z_xlu = residuals(fit_xlu,standardize = T)
z_xlp = residuals(fit_xlp,standardize = T)
z_xly = residuals(fit_xly,standardize = T)
z_xle = residuals(fit_xle,standardize = T)
z_xlv = residuals(fit_xlv,standardize = T)


z_all = data.frame(z_sp,z_vox,z_xlf,z_xlk,
                   z_xlb,z_xli,z_xlu,z_xlp,
                   z_xly,z_xle,z_xlv) %>% as.matrix()

cor(z_all,method = 'kendall')
pairs.panels(z_all,method = 'kendall')

#### transformed standardized residuals with uniform distribution  

uniform_all = apply(z_all,MARGIN = 2,pobs)
#uniform_sp = pit(fit_sp)
#uniform_vox = pit(fit_vox)
#uniform_xlf = pit(fit_xlf)
#uniform_xlk = pit(fit_xlk)
#uniform_xlb = pit(fit_xlb)
#uniform_xli = pit(fit_xli)
#uniform_xlu = pit(fit_xlu)
#uniform_xlp = pit(fit_xlp)
#uniform_xly = pit(fit_xly)
#uniform_xle = pit(fit_xle)
#uniform_xlv = pit(fit_xlv)


#uniform_all = data.frame(uniform_sp,uniform_vox,uniform_xlf,
                         #uniform_xlk,uniform_xlb,uniform_xli,
                         #uniform_xlu,uniform_xlp,uniform_xly,
                         #uniform_xle,uniform_xlv) %>% as.matrix()
#colnames(uniform_all) = c('sp500','vox','xlf','xlk','xlb','xli','xlu','xlp','xly','xle','xlv')
#uniform_all[1:10,]
#plot3d(uniform_all[,1],uniform_all[,2],uniform_all[,3],pch=20,col='navyblue')


cor(uniform_all,method = 'kendall')
pairs.panels(uniform_all,method = 'kendall')

#x1 = qt(uniform_all[,1],df = all_par_mat['shape',1])
#x2 = qt(uniform_all[,2],df = all_par_mat['shape',2])
#x3 = qt(uniform_all[,3],df = all_par_mat['shape',3])
#plot3d(x1,x2,x3,pch=20,col='blue')


#df_X = cbind(x1,x2,x3)
#pairs.panels(df_X)


# extract eps(non-standardized residuals)
eps_sp = residuals(fit_sp,standardize = F)
eps_vox = residuals(fit_vox,standardize = F)
eps_xlf = residuals(fit_xlf,standardize = F)
eps_xlk = residuals(fit_xlk,standardize = F)
eps_xlb = residuals(fit_xlb,standardize = F)
eps_xli = residuals(fit_xli,standardize = F)
eps_xlu = residuals(fit_xlu,standardize = F)
eps_xlp = residuals(fit_xlp,standardize = F)
eps_xly = residuals(fit_xly,standardize = F)
eps_xle = residuals(fit_xle,standardize = F)
eps_xlv = residuals(fit_xlv,standardize = F)
eps_all = data.frame(eps_sp,eps_vox,eps_xlf,eps_xlk,
                     eps_xlb,eps_xli,eps_xlu,eps_xlp,
                     eps_xly,eps_xle,eps_xlv) %>% as.matrix()





uniform_eps_all = pnorm(eps_all)
pairs.panels(eps_all)


## extract par
par_extract = function(x){
  return(x@fit$matcoef[,1] * (x@fit$matcoef[,4] <= 0.05))
}

par_name = c('mu','ar1','ar2','ar3','ar4','ar5','ma1','ma2','ma3','ma4','ma5','omega','alpha1','beta1','gamma1','skew','shape')

par_mat2list = function(x){
  if(class(x) == 'list'){
    return(x)
  }
  else{
    return(mat2list(x))
  }
}

par_sp = par_extract(fit_sp) 
par_vox = par_extract(fit_vox)
par_xlf = par_extract(fit_xlf)
par_xlk = par_extract(fit_xlk) 
par_xlb = par_extract(fit_xlb) 
par_xli = par_extract(fit_xli) 
par_xlu = par_extract(fit_xlu) 
par_xlp = par_extract(fit_xlp) 
par_xly = par_extract(fit_xly) 
par_xle = par_extract(fit_xle) 
par_xlv = par_extract(fit_xlv) 



get_par_mat = function(input_par,ret_mat_col){
  temp_mat = matrix(0,nrow = length(par_name),ncol = length(colnames(ret_mat_col)))
  rownames(temp_mat) = par_name
  colnames(temp_mat) = colnames(ret_mat_col)
  for(i in 1:length(par_name)){
    temp_mat[i,] = as.numeric(input_par[par_name[i]])
  }
  temp_mat[is.na(temp_mat)] =0
  return(temp_mat)
}



sp_par_mat = get_par_mat(par_sp,in_sp_logrt)
vox_par_mat = get_par_mat(par_vox,in_vox_logrt)
xlf_par_mat = get_par_mat(par_xlf,in_xlf_logrt)
xlk_par_mat = get_par_mat(par_xlk,in_xlk_logrt)
xlb_par_mat = get_par_mat(par_xlb,in_xlb_logrt)
xli_par_mat = get_par_mat(par_xli,in_xli_logrt)
xlu_par_mat = get_par_mat(par_xlu,in_xlu_logrt)
xlp_par_mat = get_par_mat(par_xlp,in_xlp_logrt)
xly_par_mat = get_par_mat(par_xly,in_xly_logrt)
xle_par_mat = get_par_mat(par_xle,in_xle_logrt)
xlv_par_mat = get_par_mat(par_xlv,in_xlv_logrt)


all_par_mat = cbind(sp_par_mat,vox_par_mat,xlf_par_mat,xlk_par_mat,
                    xlb_par_mat,xli_par_mat,xlu_par_mat,xlp_par_mat,
                    xly_par_mat,xle_par_mat,xlv_par_mat)
all_par_mat[17,11] = coef(fit_xlv)['shape']


#write.csv(all_par_mat,'C:/Users/liyang/Desktop/parameter.csv')
## extract sigma
sig_sp = sigma(fit_sp)
sig_vox = sigma(fit_vox)
sig_xlf = sigma(fit_xlf)
sig_xlk = sigma(fit_xlk)
sig_xlb = sigma(fit_xlb)
sig_xli = sigma(fit_xli)
sig_xlu = sigma(fit_xlu)
sig_xlp = sigma(fit_xlp)
sig_xly = sigma(fit_xly)
sig_xle = sigma(fit_xle)
sig_xlv = sigma(fit_xlv)


sig_all = data.frame(sig_sp,sig_vox,sig_xlf,sig_xlk,
                     sig_xlb,sig_xli,sig_xlu,sig_xlp,
                     sig_xly,sig_xle,sig_xlv) %>% as.matrix()


##all group situation Vine structure data preparation
#emipirical distribution
#pobs_z_all = apply(z_all,MARGIN = 2,FUN = pobs)
#df_pobs_z_all = data.frame(pobs_z_all)
#pobs_eps_all = apply(eps_all,MARGIN = 2,FUN = pobs)
#df_pobs_eps_all = data.frame(pobs_eps_all)





#???????????
fitcop = fitCopula(ellipCopula('normal',dim=11,df.fixed = TRUE),data = uniform_all,method = 'itau')

count_par = function(input_mat){
    num = 0
    for(i in 1:11){
        for(j in 1:11){
          if(input_mat[i,j] != 0){
              num = num + 1
          }  
        }
    }
    return(num)
}






cores = detectCores()
par(mfrow=c(1,1)) 
all_gaussian = RVineStructureSelect(data = uniform_all,
                                    familyset = 1,
                                    progress = F,
                                    type = 'RVine',
                                    treecrit = 'tau',
                                    cores = cores)
AIC_gaus = all_gaussian$AIC
LLH_gaus = all_gaussian$logLik
par_count_gaus = count_par(all_gaussian$par)
res1 = c(par_count_gaus,AIC_gaus,LLH_gaus)



all_tstudent = RVineStructureSelect(data = uniform_all,
                                    familyset = 2,
                                    progress = F,
                                    type = 'RVine',
                                    treecrit = 'tau',
                                    core = cores)
AIC_t = all_tstudent$AIC
LLH_t = all_tstudent$logLik
par_count_t = count_par(all_tstudent$par)   #+ count_par(all_tstudent$par2)  ?
res2 = c(par_count_t,AIC_t,LLH_t)

all_NA = RVineStructureSelect(data = uniform_all,
                                familyset = NA,
                                progress = F,
                                type = 'RVine',
                                treecrit = 'tau',
                                cores = cores)
sim_u = RVineSim(N = dim(in_ret_all)[1],RVM = all_NA)
pairs.panels(sim_u,method = 'kendall')
AIC_NA = all_NA$AIC
LLH_NA = all_NA$logLik
par_count_NA = count_par(all_NA$par) + count_par(all_NA$par2)
res3 = c(par_count_NA,AIC_NA,LLH_NA)

res_all = rbind(res1,res2,res3)
rownames(res_all) = c('Gaussian','t-student','all family')
colnames(res_all) = c('Number of parameters','AIC','LogLik')


##try??
gau.cop = normalCopula(dim = 2)
set.seed(500)
fit2 = fitCopula(gau.cop,uniform_all[,1:2],method = 'ml')
rho2 = coef(fit2)[1]
persp(normalCopula(dim=2,rho2),dCopula)

gau.cop = normalCopula(dim = 11)
fit2 = fitCopula(gau.cop,uniform_all,method = 'ml')
####persp(normalCopula(dim = 11, coef(fit2)[1]),dCopula) #dim = 2
u = rCopula(dim(in_ret_all)[1],normalCopula(dim = 11,coef(fit2)[1]))


t.cop = tCopula(dim= 11)
fit3 = fitCopula(t.cop,uniform_all,method = 'ml')
u2 = rCopula(dim(in_ret_all)[1],tCopula(dim = 11,coef(fit3)[1],df = coef(fit3)[2]))





monte_carlo_copula = function(N = 360,M = 10000,tick){
    store_mat = c()
    for(i in 1:M){
        sim = RVineSim(N = N, RVM = all_NA)
        store_mat = cbind(store_mat,sim[,tick])
    }
    output = rowMeans(store_mat)
    return(output)
}

tkname = c('sp500','vox','xlf','xlk','xlb','xli','xlu','xlp','xly','xle','xlv')
mc_res_mat = c()
for(i in tkname){
    temp = monte_carlo_copula(tick = i)
    mc_res_mat = cbind(mc_res_mat,temp)
}
colnames(mc_res_mat) = tkname
u = mc_res_mat



quantile_trans = function(shape,skew,u){
    return(qdist(distribution = 'sstd',p = u,shape = shape,
                 skew = skew))
}

z_sim_all = c()
for(i in 1:11){
    temp2 = quantile_trans(shape = all_par_mat[17,i],skew = all_par_mat[16,i],u = sim_u[,i])
    z_sim_all = cbind(z_sim_all,temp2)
}


# copula ------------> aram-egarch(1,1)  #test sp500: arma(3,3)-egarch(1,1)
test = exp(t(all_par_mat[c('omega','beta1','gamma1','alpha1'),1,drop = FALSE]) %*%
               rbind(as.numeric(tail(sp_logrt,1)),log((as.numeric(tail(sig_sp,1)))^2),
                     (as.numeric(tail(eps_sp,1))/as.numeric(tail(sig_sp,1))),
                     (abs(as.numeric(tail(eps_sp,1)))/as.numeric(tail(sig_sp,1))))) %>% diag() %>% sqrt() %*%
    as.numeric(tail(z_sim_all,1)[1]) + all_par_mat['mu',1] + all_par_mat['ar1',1]*as.numeric(tail(in_sp_logrt,3)[3]) +
    all_par_mat['ar2',1]*as.numeric(tail(in_sp_logrt,3)[2]) + all_par_mat['ar3',1]*as.numeric(tail(in_sp_logrt,3)[1]) +
    all_par_mat['ma1',1]*as.numeric(tail(eps_all,3)[3,1]) + all_par_mat['ma2',1]*as.numeric(tail(eps_all,3)[2,1]) +
    all_par_mat['ma3',1]*as.numeric(tail(eps_all,3)[1,1])  


t(test)


cal_Xt = function(para_mat = all_par_mat,sig_data = all_sig,eps_data = all_eps,ret_data = all_ret,z_data = all_z){
  Xt = c()
  for(i in 1:11){
    temp = exp(t(para_mat[c('omega','beta1','gamma1','alpha1'),i,drop = FALSE]) %*%
                 rbind(1,log((as.numeric(tail(sig_data,1)))^2),
                       (as.numeric(tail(eps_data,1))/sqrt(as.numeric(tail(sig_data,1))^2)),
                       ((abs(as.numeric(tail(eps_data,1)))/sqrt(as.numeric(tail(sig_data,1))^2))-sqrt(2/pi)))) %>% diag() %>% sqrt() %*%
      as.numeric(tail(z_data[,i],1)) + 
      para_mat['mu',i] + 
      para_mat['ar1',i]*as.numeric(tail(ret_data,5)[5,i]) +
      para_mat['ar2',i]*as.numeric(tail(ret_data,5)[4,i]) +
      para_mat['ar3',i]*as.numeric(tail(ret_data,5)[3,i]) +
      para_mat['ar4',i]*as.numeric(tail(ret_data,5)[2,i]) +
      para_mat['ar5',i]*as.numeric(tail(ret_data,5)[1,i]) +
      para_mat['ma1',i]*as.numeric(tail(eps_data,5)[5,i]) + 
      para_mat['ma2',i]*as.numeric(tail(eps_data,5)[4,i]) +
      para_mat['ma3',i]*as.numeric(tail(eps_data,5)[3,i]) +
      para_mat['ma4',i]*as.numeric(tail(eps_data,5)[2,i]) +
      para_mat['ma5',i]*as.numeric(tail(eps_data,5)[1,i])
    Xt = c(Xt,temp)
  }
  return(Xt)
}
cal_Xt(para_mat = all_par_mat,sig_data = sig_all,eps_data = eps_all,
       ret_data = in_ret_all,z_data = z_sim_all)


ret_data = in_ret_all
ret_data = ret_data[1:2691,]
write.csv(ret_data,'C:/Users/yl/Desktop/1to2691.csv')

for(j in 32:360){
  start = Sys.time()
  
  fit_sp = ugarchfit(spec = sp_uspec,data = ret_data[(1+j):(2658+j),1],solver = 'hybrid',
                     parallel = T,core = 4)
  fit_vox = ugarchfit(spec = vox_uspec,data = ret_data[(1+j):(2658+j),2],solver = 'hybrid',
                      parallel = T,core = 4)
  fit_xlf = ugarchfit(spec = xlf_uspec,data = ret_data[(1+j):(2658+j),3],solver = 'hybrid',
                      parallel = T,core = 4)
  fit_xlk = ugarchfit(spec = xlk_uspec,data = ret_data[(1+j):(2658+j),4],solver = 'hybrid',
                      parallel = T,core = 4)
  fit_xlb = ugarchfit(spec = xlb_uspec,data = ret_data[(1+j):(2658+j),5],solver = 'hybrid',
                      parallel = T,core = 4)
  fit_xli = ugarchfit(spec = xli_uspec,data = ret_data[(1+j):(2658+j),6],solver = 'hybrid',
                      parallel = T,core = 4)
  fit_xlu = ugarchfit(spec = xlu_uspec,data = ret_data[(1+j):(2658+j),7],solver = 'hybrid',
                      parallel = T,core = 4)
  fit_xlp = ugarchfit(spec = xlp_uspec,data = ret_data[(1+j):(2658+j),8],solver = 'hybrid',
                      parallel = T,core = 4)
  fit_xly = ugarchfit(spec = xly_uspec,data = ret_data[(1+j):(2658+j),9],solver = 'hybrid',
                      parallel = T,core = 4)
  fit_xle = ugarchfit(spec = xle_uspec,data = ret_data[(1+j):(2658+j),10],solver = 'hybrid',
                      parallel = T,core = 4)
  fit_xlv = ugarchfit(spec = xlv_uspec,data = ret_data[(1+j):(2658+j),11],solver = 'hybrid',
                      parallel = T,core = 4)
  
  z_sp = residuals(fit_sp,standardize = T)
  z_vox = residuals(fit_vox,standardize = T)
  z_xlf = residuals(fit_xlf,standardize = T)
  z_xlk = residuals(fit_xlk,standardize = T)
  z_xlb = residuals(fit_xlb,standardize = T)
  z_xli = residuals(fit_xli,standardize = T)
  z_xlu = residuals(fit_xlu,standardize = T)
  z_xlp = residuals(fit_xlp,standardize = T)
  z_xly = residuals(fit_xly,standardize = T)
  z_xle = residuals(fit_xle,standardize = T)
  z_xlv = residuals(fit_xlv,standardize = T)
  z_all = data.frame(z_sp,z_vox,z_xlf,z_xlk,
                     z_xlb,z_xli,z_xlu,z_xlp,
                     z_xly,z_xle,z_xlv) %>% as.matrix()
  
  
  uniform_all = apply(z_all,MARGIN =2, pobs)
  
  eps_sp = residuals(fit_sp,standardize = F)
  eps_vox = residuals(fit_vox,standardize = F)
  eps_xlf = residuals(fit_xlf,standardize = F)
  eps_xlk = residuals(fit_xlk,standardize = F)
  eps_xlb = residuals(fit_xlb,standardize = F)
  eps_xli = residuals(fit_xli,standardize = F)
  eps_xlu = residuals(fit_xlu,standardize = F)
  eps_xlp = residuals(fit_xlp,standardize = F)
  eps_xly = residuals(fit_xly,standardize = F)
  eps_xle = residuals(fit_xle,standardize = F)
  eps_xlv = residuals(fit_xlv,standardize = F)
  eps_all = data.frame(eps_sp,eps_vox,eps_xlf,eps_xlk,
                       eps_xlb,eps_xli,eps_xlu,eps_xlp,
                       eps_xly,eps_xle,eps_xlv) %>% as.matrix()
  
  sig_sp = sigma(fit_sp)
  sig_vox = sigma(fit_vox)
  sig_xlf = sigma(fit_xlf)
  sig_xlk = sigma(fit_xlk)
  sig_xlb = sigma(fit_xlb)
  sig_xli = sigma(fit_xli)
  sig_xlu = sigma(fit_xlu)
  sig_xlp = sigma(fit_xlp)
  sig_xly = sigma(fit_xly)
  sig_xle = sigma(fit_xle)
  sig_xlv = sigma(fit_xlv)
  sig_all = data.frame(sig_sp,sig_vox,sig_xlf,sig_xlk,
                       sig_xlb,sig_xli,sig_xlu,sig_xlp,
                       sig_xly,sig_xle,sig_xlv) %>% as.matrix()
  
  
  par_sp = par_extract(fit_sp) 
  par_vox = par_extract(fit_vox)
  par_xlf = par_extract(fit_xlf)
  par_xlk = par_extract(fit_xlk) 
  par_xlb = par_extract(fit_xlb) 
  par_xli = par_extract(fit_xli) 
  par_xlu = par_extract(fit_xlu) 
  par_xlp = par_extract(fit_xlp) 
  par_xly = par_extract(fit_xly) 
  par_xle = par_extract(fit_xle) 
  par_xlv = par_extract(fit_xlv)
  
  sp_par_mat = get_par_mat(par_sp,in_sp_logrt)
  vox_par_mat = get_par_mat(par_vox,in_vox_logrt)
  xlf_par_mat = get_par_mat(par_xlf,in_xlf_logrt)
  xlk_par_mat = get_par_mat(par_xlk,in_xlk_logrt)
  xlb_par_mat = get_par_mat(par_xlb,in_xlb_logrt)
  xli_par_mat = get_par_mat(par_xli,in_xli_logrt)
  xlu_par_mat = get_par_mat(par_xlu,in_xlu_logrt)
  xlp_par_mat = get_par_mat(par_xlp,in_xlp_logrt)
  xly_par_mat = get_par_mat(par_xly,in_xly_logrt)
  xle_par_mat = get_par_mat(par_xle,in_xle_logrt)
  xlv_par_mat = get_par_mat(par_xlv,in_xlv_logrt)
  
  all_par_mat = cbind(sp_par_mat,vox_par_mat,xlf_par_mat,xlk_par_mat,
                      xlb_par_mat,xli_par_mat,xlu_par_mat,xlp_par_mat,
                      xly_par_mat,xle_par_mat,xlv_par_mat)
  all_par_mat[17,11] = coef(fit_xlv)['shape']
  
  all_NA = RVineStructureSelect(data = uniform_all,
                                familyset = NA,
                                progress = F,
                                type = 'RVine',
                                treecrit = 'tau',
                                cores = 4)
  sim_u = RVineSim(N = dim(in_ret_all)[1],RVM = all_NA)
  
  quantile_trans = function(shape,skew,u){
    return(qdist(distribution = 'sstd',p = u,shape = shape,
                 skew = skew))
  }
  
  z_sim_all = c()
  for(k in 1:11){
    temp2 = quantile_trans(shape = all_par_mat[17,k],skew = all_par_mat[16,k],u = sim_u[,k])
    z_sim_all = cbind(z_sim_all,temp2)
  }
  
  new_Xt = cal_Xt(para_mat = all_par_mat,sig_data = sig_all,
                  eps_data = eps_all,ret_data = ret_data[(1+j):(2658+j),],z_data = z_sim_all)
  if(sum(is.na(new_Xt)) > 0){
    new_Xt[is.na(new_Xt)] = 0
  }
  ret_data = rbind(ret_data,new_Xt)
  end = Sys.time()
  print(j)
  print(end - start)
}
    

write.csv(ret_data,'C:/Users/yl/Desktop/360days.csv')
ret_data = read.csv('G:/800/360days.csv',header = T)
ret_data = ret_data[1:3019,2:12]
rownames(ret_data) = rownames(all_logrt)
forecast_data = ret_data[2660:3019,]
rownames(forecast_data) = rownames(out_ret_all)

with_index_forecast = zoo(forecast_data)
index(with_index_forecast) = as.Date(rownames(out_ret_all))

plot(out_ret_all[,1],type='l')
lines(forecast_data[,1],type='l',col='green')

all_data_with_pred = rbind(in_ret_all,forecast_data)
index_with_all = zoo(all_data_with_pred)
index(index_with_all) = as.Date(rownames(all_logrt))

get_pq = function(data,max_p=5,max_q=5){
  final.aic = 0
  final.order = c(0,0,0)
  for(p in 0:max_p) for(q in 0:max_q){
    if(p == 0 && q == 0){
      next
    }
    arimaFit = tryCatch(arima(data,order = c(p,0,q)),
                        error = function(err) FALSE,
                        warning = function(err) FALSE)
    
    if(!is.logical(arimaFit)){
      
      current.aic <- AIC(arimaFit)
      if(current.aic < final.aic){
        
        final.aic <- current.aic
        final.order <- c(p,0,q)
        final.arima <- arima(data,order = final.order)
      }
    }else{
      next
    }
  }
  return(final.order)
}


xixi1 = apply(index_with_all,MARGIN=2,get_pq)
sp_meanmod = list(armaOrder = c(3,3),include.mean = T)
vox_meanmod = list(armaOrder = c(4,2),include.mean = T)
xlf_meanmod = list(armaOrder = c(5,4),include.mean = T)
xlk_meanmod = list(armaOrder = c(2,4),include.mean = T)
xlb_meanmod = list(armaOrder = c(5,3),include.mean = T)
xli_meanmod = list(armaOrder = c(5,5),include.mean = T)
xlu_meanmod = list(armaOrder = c(5,4),include.mean = T)
xlp_meanmod = list(armaOrder = c(5,3),include.mean = T)
xly_meanmod = list(armaOrder = c(3,3),include.mean = T)
xle_meanmod = list(armaOrder = c(2,3),include.mean = T)
xlv_meanmod = list(armaOrder = c(3,4),include.mean = T)
varmod = list(model = 'eGARCH',garchOrder = c(1,1))


## model specification
sp_uspec = ugarchspec(varmod,mean.model = sp_meanmod,distribution.model = 'sstd')
vox_uspec = ugarchspec(varmod,mean.model = vox_meanmod,distribution.model = 'sstd')
xlf_uspec = ugarchspec(varmod,mean.model = xlf_meanmod,distribution.model = 'sstd')
xlk_uspec = ugarchspec(varmod,mean.model = xlk_meanmod,distribution.model = 'sstd')
xlb_uspec = ugarchspec(varmod,mean.model = xlb_meanmod,distribution.model = 'sstd')
xli_uspec = ugarchspec(varmod,mean.model = xli_meanmod,distribution.model = 'sstd')
xlu_uspec = ugarchspec(varmod,mean.model = xlu_meanmod,distribution.model = 'sstd')
xlp_uspec = ugarchspec(varmod,mean.model = xlp_meanmod,distribution.model = 'sstd')
xly_uspec = ugarchspec(varmod,mean.model = xly_meanmod,distribution.model = 'sstd')
xle_uspec = ugarchspec(varmod,mean.model = xle_meanmod,distribution.model = 'sstd')
xlv_uspec = ugarchspec(varmod,mean.model = xlv_meanmod,distribution.model = 'sstd')


copula_sp_logrt = xts(index_with_all[,1])
copula_vox_logrt = xts(index_with_all[,2])
copula_xlf_logrt = xts(index_with_all[,3])
copula_xlk_logrt = xts(index_with_all[,4])
copula_xlb_logrt = xts(index_with_all[,5])
copula_xli_logrt = xts(index_with_all[,6])
copula_xlu_logrt = xts(index_with_all[,7])
copula_xlp_logrt = xts(index_with_all[,8])
copula_xly_logrt = xts(index_with_all[,9])
copula_xle_logrt = xts(index_with_all[,10])
copula_xlv_logrt = xts(index_with_all[,11])

cl = makeCluster(4)

copula_sp_roll = ugarchroll(sp_uspec,copula_sp_logrt,n.start=2659,refit.every = 10,refit.window ='moving',
                            solver = 'hybrid',calculate.VaR = TRUE,VaR.alpha = 0.01,
                            keep.coef = T,cluster = cl)
copula_vox_roll = ugarchroll(vox_uspec,copula_vox_logrt,n.start=2659,refit.every = 10,refit.window ='moving',
                             solver = 'hybrid',calculate.VaR = TRUE,VaR.alpha = 0.01,
                             keep.coef = T,cluster = cl)
copula_xlf_roll = ugarchroll(xlf_uspec,copula_xlf_logrt,n.start=2659,refit.every = 10,refit.window ='moving',
                             solver = 'hybrid',calculate.VaR = TRUE,VaR.alpha = 0.01,
                             keep.coef = T,cluster = cl)
copula_xlk_roll = ugarchroll(xlk_uspec,copula_xlk_logrt,n.start=2659,refit.every = 10,refit.window ='moving',
                             solver = 'hybrid',calculate.VaR = TRUE,VaR.alpha = 0.01,
                             keep.coef = T,cluster = cl)
copula_xlb_roll = ugarchroll(xlb_uspec,copula_xlb_logrt,n.start=2659,refit.every = 10,refit.window ='moving',
                             solver = 'hybrid',calculate.VaR = TRUE,VaR.alpha = 0.01,
                             keep.coef = T,cluster = cl)
copula_xli_roll = ugarchroll(xli_uspec,copula_xli_logrt,n.start=2659,refit.every = 10,refit.window ='moving',
                             solver = 'hybrid',calculate.VaR = TRUE,VaR.alpha = 0.01,
                             keep.coef = T,cluster = cl)
copula_xlu_roll = ugarchroll(xlu_uspec,copula_xlu_logrt,n.start=2659,refit.every = 10,refit.window ='moving',
                             solver = 'hybrid',calculate.VaR = TRUE,VaR.alpha = 0.01,
                             keep.coef = T,cluster = cl)
copula_xlp_roll = ugarchroll(xlp_uspec,copula_xlp_logrt,n.start=2659,refit.every = 10,refit.window ='moving',
                             solver = 'hybrid',calculate.VaR = TRUE,VaR.alpha = 0.01,
                             keep.coef = T,cluster = cl)
copula_xly_roll = ugarchroll(xly_uspec,copula_xly_logrt,n.start=2659,refit.every = 10,refit.window ='moving',
                             solver = 'hybrid',calculate.VaR = TRUE,VaR.alpha = 0.01,
                             keep.coef = T,cluster = cl)
copula_xle_roll = ugarchroll(xle_uspec,copula_xle_logrt,n.start=2659,refit.every = 10,refit.window ='moving',
                             solver = 'hybrid',calculate.VaR = TRUE,VaR.alpha = 0.01,
                             keep.coef = T,cluster = cl)
copula_xlv_roll = ugarchroll(xlv_uspec,copula_xlv_logrt,n.start=2659,refit.every = 10,refit.window ='moving',
                             solver = 'hybrid',calculate.VaR = TRUE,VaR.alpha = 0.01,
                             keep.coef = T,cluster = cl)


par(mfrow=c(1,1))

##sp 
sp_fore_var = zoo(sp_roll@forecast$VaR[,1])
index(sp_fore_var) = as.Date(rownames(sp_roll@forecast$VaR))
sp_copula_fore_var = zoo(copula_sp_roll@forecast$VaR[,1])
index(sp_copula_fore_var) = as.Date(rownames(sp_roll@forecast$VaR))
sp_actual_var = zoo(sp_roll@forecast$VaR[,2])
index(sp_actual_var) = as.Date(rownames(sp_roll@forecast$VaR))
plot(sp_actual_var,type='p',main = 'SP500 1-day ahead VaR at 99% conf level',xlab = 'Date',
     ylab = 'VaR',col='green',pch = 19,ylim=c(min(sp_copula_fore_var),max(sp_actual_var)))
lines(sp_fore_var,col = 2,lwd=2)
lines(sp_copula_fore_var,col='blue',lwd=2)
legend('bottomright',legend = c('actual return','ARMA-EGARCH forecast','Copula-based forecast'),col = c('green','red','blue'),lwd=2,lty = c(NA,1,1),pch = c(19,NA,NA),cex = 0.8)
##vox
vox_fore_var = zoo(vox_roll@forecast$VaR[,1])
index(vox_fore_var) = as.Date(rownames(vox_roll@forecast$VaR))
vox_copula_fore_var = zoo(copula_vox_roll@forecast$VaR[,1])
index(vox_copula_fore_var) = as.Date(rownames(vox_roll@forecast$VaR))
vox_actual_var = zoo(vox_roll@forecast$VaR[,2])
index(vox_actual_var) = as.Date(rownames(vox_roll@forecast$VaR))
plot(vox_actual_var,type='p',main = 'VOX one-day ahead VaR at 99% conf level',xlab = 'Date',
     ylab = 'VaR',col='green',pch = 19,ylim=c(min(vox_copula_fore_var),max(vox_actual_var)))
lines(vox_fore_var,col = 2,lwd=2)
lines(vox_copula_fore_var,col='blue',lwd=2)
legend('bottomright',legend = c('actual return','ARMA-EGARCH forecast','Copula-based forecast'),col = c('green','red','blue'),lwd=2,lty = c(NA,1,1),pch = c(19,NA,NA))
##xlf 
xlf_fore_var = zoo(xlf_roll@forecast$VaR[,1])
index(xlf_fore_var) = as.Date(rownames(xlf_roll@forecast$VaR))
xlf_copula_fore_var = zoo(copula_xlf_roll@forecast$VaR[,1])
index(xlf_copula_fore_var) = as.Date(rownames(xlf_roll@forecast$VaR))
xlf_actual_var = zoo(xlf_roll@forecast$VaR[,2])
index(xlf_actual_var) = as.Date(rownames(xlf_roll@forecast$VaR))
plot(xlf_actual_var,type='p',main = 'XLF 1-day ahead VaR at 99% conf level',xlab = 'Date',
     ylab = 'VaR',col='green',pch = 19,ylim=c(min(xlf_copula_fore_var),max(xlf_actual_var)))
lines(xlf_fore_var,col = 2,lwd=2)
lines(xlf_copula_fore_var,col='blue',lwd =2)
legend('bottomright',legend = c('actual return','ARMA-EGARCH forecast','Copula-based forecast'),col = c('green','red','blue'),lwd=2,lty = c(NA,1,1),pch = c(19,NA,NA))
##xlk
xlk_fore_var = zoo(xlk_roll@forecast$VaR[,1])
index(xlk_fore_var) = as.Date(rownames(xlk_roll@forecast$VaR))
xlk_copula_fore_var = zoo(copula_xlk_roll@forecast$VaR[,1])
index(xlk_copula_fore_var) = as.Date(rownames(xlk_roll@forecast$VaR))
xlk_actual_var = zoo(xlk_roll@forecast$VaR[,2])
index(xlk_actual_var) = as.Date(rownames(xlk_roll@forecast$VaR))
plot(xlk_actual_var,type='p',main = 'XLK 1-day ahead VaR at 99% conf level',xlab = 'Date',
     ylab = 'VaR',col='green',pch = 19,ylim=c(min(xlk_copula_fore_var),max(xlk_actual_var)))
lines(xlk_fore_var,col = 2,lwd=2)
lines(xlk_copula_fore_var,col = 'blue',lwd =2)
legend('bottomright',legend = c('actual return','ARMA-EGARCH forecast','Copula-based forecast'),col = c('green','red','blue'),lwd=2,lty = c(NA,1,1),pch = c(19,NA,NA))
##################################################################
par(mfrow=c(1,1))
##xlb
xlb_fore_var = zoo(xlb_roll@forecast$VaR[,1])
index(xlb_fore_var) = as.Date(rownames(xlb_roll@forecast$VaR))
xlb_copula_fore_var = zoo(copula_xlb_roll@forecast$VaR[,1])
index(xlb_copula_fore_var) = as.Date(rownames(xlb_roll@forecast$VaR))
xlb_actual_var = zoo(xlb_roll@forecast$VaR[,2])
index(xlb_actual_var) = as.Date(rownames(xlb_roll@forecast$VaR))
plot(xlb_actual_var,type='p',main = 'XLB 1-day ahead VaR at 99% conf level',xlab = 'Date',
     ylab = 'VaR',col='green',pch = 19,ylim=c(min(xlb_copula_fore_var),max(xlb_actual_var)))
lines(xlb_fore_var,col = 2,lwd=2)
lines(xlb_copula_fore_var,col = 'blue',lwd =2)
legend('bottomright',legend = c('actual return','ARMA-EGARCH forecast','Copula-based forecast'),col = c('green','red','blue'),lwd=2,lty = c(NA,1,1),pch = c(19,NA,NA))
##xli
xli_fore_var = zoo(xli_roll@forecast$VaR[,1])
index(xli_fore_var) = as.Date(rownames(xli_roll@forecast$VaR))
xli_copula_fore_var = zoo(copula_xli_roll@forecast$VaR[,1])
index(xli_copula_fore_var) = as.Date(rownames(xli_roll@forecast$VaR))
xli_actual_var = zoo(xli_roll@forecast$VaR[,2])
index(xli_actual_var) = as.Date(rownames(xli_roll@forecast$VaR))
plot(xli_actual_var,type='p',main = 'XLI 1-day ahead VaR at 99% conf level',xlab = 'Date',
     ylab = 'VaR',col='green',pch = 19,ylim=c(min(xli_copula_fore_var),max(xli_actual_var)))
lines(xli_fore_var,col = 2,lwd=2)
lines(xli_copula_fore_var,col = 'blue',lwd =2)
legend('bottomright',legend = c('actual return','ARMA-EGARCH forecast','Copula-based forecast'),col = c('green','red','blue'),lwd=2,lty = c(NA,1,1),pch = c(19,NA,NA))
##xlu
xlu_fore_var = zoo(xlu_roll@forecast$VaR[,1])
index(xlu_fore_var) = as.Date(rownames(xlu_roll@forecast$VaR))
xlu_copula_fore_var = zoo(copula_xlu_roll@forecast$VaR[,1])
index(xlu_copula_fore_var) = as.Date(rownames(xlu_roll@forecast$VaR))
xlu_actual_var = zoo(xlu_roll@forecast$VaR[,2])
index(xlu_actual_var) = as.Date(rownames(xlu_roll@forecast$VaR))
plot(xlu_actual_var,type='p',main = 'XLU 1-day ahead VaR at 99% conf level',xlab = 'Date',
     ylab = 'VaR',col='green',pch = 19,ylim=c(min(xlu_copula_fore_var),max(xlu_actual_var)))
lines(xlu_fore_var,col = 2,lwd=2)
lines(xlu_copula_fore_var,col = 'blue',lwd = 2)
legend('bottomright',legend = c('actual return','ARMA-EGARCH forecast','Copula-based forecast'),col = c('green','red','blue'),lwd=2,lty = c(NA,1,1),pch = c(19,NA,NA))
##xlp
xlp_fore_var = zoo(xlp_roll@forecast$VaR[,1])
index(xlp_fore_var) = as.Date(rownames(xlp_roll@forecast$VaR))
xlp_copula_fore_var = zoo(copula_xlp_roll@forecast$VaR[,1])
index(xlp_copula_fore_var) = as.Date(rownames(xlp_roll@forecast$VaR))
xlp_actual_var = zoo(xlp_roll@forecast$VaR[,2])
index(xlp_actual_var) = as.Date(rownames(xlp_roll@forecast$VaR))
plot(xlp_actual_var,type='p',main = 'XLP 1-day ahead VaR at 99% conf level',xlab = 'Date',
     ylab = 'VaR',col='green',pch = 19,ylim=c(min(xlp_copula_fore_var),max(xlp_actual_var)))
lines(xlp_fore_var,col = 2,lwd=2)
lines(xlp_copula_fore_var,col='blue',lwd=2)
legend('bottomright',legend = c('actual return','ARMA-EGARCH forecast','Copula-based forecast'),col = c('green','red','blue'),lwd=2,lty = c(NA,1,1),pch = c(19,NA,NA))
#######################################################################
par(mfrow=c(1,1))
##xly
xly_fore_var = zoo(xly_roll@forecast$VaR[,1])
index(xly_fore_var) = as.Date(rownames(xly_roll@forecast$VaR))
xly_copula_fore_var = zoo(copula_xly_roll@forecast$VaR[,1])
index(xly_copula_fore_var) = as.Date(rownames(xly_roll@forecast$VaR))
xly_actual_var = zoo(xly_roll@forecast$VaR[,2])
index(xly_actual_var) = as.Date(rownames(xly_roll@forecast$VaR))
plot(xly_actual_var,type='p',main = 'XLY 1-day ahead VaR at 99% conf level',xlab = 'Date',
     ylab = 'VaR',col='green',pch = 19,ylim=c(min(xly_copula_fore_var),max(xly_actual_var)))
lines(xly_fore_var,col = 2,lwd=2)
lines(xly_copula_fore_var,col='blue',lwd =2)
legend('bottomright',legend = c('actual return','ARMA-EGARCH forecast','Copula-based forecast'),col = c('green','red','blue'),lwd=2,lty = c(NA,1,1),pch = c(19,NA,NA))
##xle
xle_fore_var = zoo(xle_roll@forecast$VaR[,1])
index(xle_fore_var) = as.Date(rownames(xle_roll@forecast$VaR))
xle_copula_fore_var = zoo(copula_xle_roll@forecast$VaR[,1])
index(xle_copula_fore_var) = as.Date(rownames(xle_roll@forecast$VaR))
xle_actual_var = zoo(xle_roll@forecast$VaR[,2])
index(xle_actual_var) = as.Date(rownames(xle_roll@forecast$VaR))
plot(xle_actual_var,type='p',main = 'XLE 1-day ahad VaR at 99%',xlab = 'Date',
     ylab = 'VaR',col='green',pch = 19,ylim=c(min(xle_copula_fore_var),max(xle_actual_var)))
lines(xle_fore_var,col = 2,lwd=2)
lines(xle_copula_fore_var,col='blue',lwd = 2)
legend('bottomright',legend = c('actual return','ARMA-EGARCH forecast','Copula-based forecast'),col = c('green','red','blue'),lwd=2,lty = c(NA,1,1),pch = c(19,NA,NA))
##xlv
xlv_fore_var = zoo(xlv_roll@forecast$VaR[,1])
index(xlv_fore_var) = as.Date(rownames(xlv_roll@forecast$VaR))
xlv_copula_fore_var = zoo(copula_xlv_roll@forecast$VaR[,1])
index(xlv_copula_fore_var) = as.Date(rownames(xlv_roll@forecast$VaR))
xlv_actual_var = zoo(xlv_roll@forecast$VaR[,2])
index(xlv_actual_var) = as.Date(rownames(xlv_roll@forecast$VaR))
plot(xlv_actual_var,type='p',main = 'XLV 1-day ahead VaR at 99%',xlab = 'Date',
     ylab = 'VaR',col='green',pch = 19,ylim=c(min(xle_copula_fore_var),max(xle_actual_var)))
lines(xlv_fore_var,col = 2,lwd=2)
lines(xlv_copula_fore_var,col = 'blue',lwd =2)
legend('bottomright',legend = c('actual return','ARMA-EGARCH forecast','Copula-based forecast'),col = c('green','red','blue'),lwd=2,lty = c(NA,1,1),pch = c(19,NA,NA))

stopCluster(cl)


ml_result = read.csv('C:/Users/yl/Desktop/ml_result.csv')
ml_result = ml_result[,2:12]
colnames(ml_result) = colnames(in_ret_all)
#ml_result = zoo(ml_result)
rownames(ml_result) = rownames(out_ret_all)
all_ret_ml = rbind(in_ret_all,ml_result)
all_ret_ml = zoo(all_ret_ml)
index(all_ret_ml) = as.Date(rownames(all_logrt))



ml_sp_logrt = xts(all_ret_ml[,1])
ml_vox_logrt = xts(all_ret_ml[,2])
ml_xlf_logrt = xts(all_ret_ml[,3])
ml_xlk_logrt = xts(all_ret_ml[,4])
ml_xlb_logrt = xts(all_ret_ml[,5])
ml_xli_logrt = xts(all_ret_ml[,6])
ml_xlu_logrt = xts(all_ret_ml[,7])
ml_xlp_logrt = xts(all_ret_ml[,8])
ml_xly_logrt = xts(all_ret_ml[,9])
ml_xle_logrt = xts(all_ret_ml[,10])
ml_xlv_logrt = xts(all_ret_ml[,11])



get_pq = function(data,max_p=5,max_q=5){
  final.aic = 0
  final.order = c(0,0,0)
  for(p in 0:max_p) for(q in 0:max_q){
    if(p == 0 && q == 0){
      next
    }
    arimaFit = tryCatch(arima(data,order = c(p,0,q)),
                        error = function(err) FALSE,
                        warning = function(err) FALSE)
    
    if(!is.logical(arimaFit)){
      
      current.aic <- AIC(arimaFit)
      if(current.aic < final.aic){
        
        final.aic <- current.aic
        final.order <- c(p,0,q)
        final.arima <- arima(data,order = final.order)
      }
    }else{
      next
    }
  }
  return(final.order)
}


xixi2 = apply(all_ret_ml,MARGIN=2,get_pq)
sp_meanmod = list(armaOrder = c(5,3),include.mean = T)
vox_meanmod = list(armaOrder = c(4,4),include.mean = T)
xlf_meanmod = list(armaOrder = c(4,5),include.mean = T)
xlk_meanmod = list(armaOrder = c(5,5),include.mean = T)
xlb_meanmod = list(armaOrder = c(4,5),include.mean = T)
xli_meanmod = list(armaOrder = c(4,3),include.mean = T)
xlu_meanmod = list(armaOrder = c(5,5),include.mean = T)
xlp_meanmod = list(armaOrder = c(3,5),include.mean = T)
xly_meanmod = list(armaOrder = c(5,5),include.mean = T)
xle_meanmod = list(armaOrder = c(4,3),include.mean = T)
xlv_meanmod = list(armaOrder = c(5,0),include.mean = T)
varmod = list(model = 'eGARCH',garchOrder = c(1,1))


## model specification
sp_uspec = ugarchspec(varmod,mean.model = sp_meanmod,distribution.model = 'sstd')
vox_uspec = ugarchspec(varmod,mean.model = vox_meanmod,distribution.model = 'sstd')
xlf_uspec = ugarchspec(varmod,mean.model = xlf_meanmod,distribution.model = 'sstd')
xlk_uspec = ugarchspec(varmod,mean.model = xlk_meanmod,distribution.model = 'sstd')
xlb_uspec = ugarchspec(varmod,mean.model = xlb_meanmod,distribution.model = 'sstd')
xli_uspec = ugarchspec(varmod,mean.model = xli_meanmod,distribution.model = 'sstd')
xlu_uspec = ugarchspec(varmod,mean.model = xlu_meanmod,distribution.model = 'sstd')
xlp_uspec = ugarchspec(varmod,mean.model = xlp_meanmod,distribution.model = 'sstd')
xly_uspec = ugarchspec(varmod,mean.model = xly_meanmod,distribution.model = 'sstd')
xle_uspec = ugarchspec(varmod,mean.model = xle_meanmod,distribution.model = 'sstd')
xlv_uspec = ugarchspec(varmod,mean.model = xlv_meanmod,distribution.model = 'sstd')

cl = makeCluster(4)
ml_sp_roll = ugarchroll(sp_uspec,ml_sp_logrt,n.start=2659,refit.every = 10,refit.window ='moving',
                            solver = 'hybrid',calculate.VaR = TRUE,VaR.alpha = 0.01,
                            keep.coef = T,cluster = cl)
ml_vox_roll = ugarchroll(vox_uspec,ml_vox_logrt,n.start=2659,refit.every = 10,refit.window ='moving',
                             solver = 'hybrid',calculate.VaR = TRUE,VaR.alpha = 0.01,
                             keep.coef = T,cluster = cl)
ml_xlf_roll = ugarchroll(xlf_uspec,ml_xlf_logrt,n.start=2659,refit.every = 10,refit.window ='moving',
                             solver = 'hybrid',calculate.VaR = TRUE,VaR.alpha = 0.01,
                             keep.coef = T,cluster = cl)
ml_xlk_roll = ugarchroll(xlk_uspec,ml_xlk_logrt,n.start=2659,refit.every = 10,refit.window ='moving',
                             solver = 'hybrid',calculate.VaR = TRUE,VaR.alpha = 0.01,
                             keep.coef = T,cluster = cl)
ml_xlb_roll = ugarchroll(xlb_uspec,ml_xlb_logrt,n.start=2659,refit.every = 10,refit.window ='moving',
                             solver = 'hybrid',calculate.VaR = TRUE,VaR.alpha = 0.01,
                             keep.coef = T,cluster = cl)
ml_xli_roll = ugarchroll(xli_uspec,ml_xli_logrt,n.start=2659,refit.every = 10,refit.window ='moving',
                             solver = 'hybrid',calculate.VaR = TRUE,VaR.alpha = 0.01,
                             keep.coef = T,cluster = cl)
ml_xlu_roll = ugarchroll(xlu_uspec,ml_xlu_logrt,n.start=2659,refit.every = 10,refit.window ='moving',
                             solver = 'hybrid',calculate.VaR = TRUE,VaR.alpha = 0.01,
                             keep.coef = T,cluster = cl)
ml_xlp_roll = ugarchroll(xlp_uspec,ml_xlp_logrt,n.start=2659,refit.every = 10,refit.window ='moving',
                             solver = 'hybrid',calculate.VaR = TRUE,VaR.alpha = 0.01,
                             keep.coef = T,cluster = cl)
ml_xly_roll = ugarchroll(xly_uspec,ml_xly_logrt,n.start=2659,refit.every = 10,refit.window ='moving',
                             solver = 'hybrid',calculate.VaR = TRUE,VaR.alpha = 0.01,
                             keep.coef = T,cluster = cl)
ml_xle_roll = ugarchroll(xle_uspec,ml_xle_logrt,n.start=2659,refit.every = 10,refit.window ='moving',
                             solver = 'hybrid',calculate.VaR = TRUE,VaR.alpha = 0.01,
                             keep.coef = T,cluster = cl)
ml_xlv_roll = ugarchroll(xlv_uspec,ml_xlv_logrt,n.start=2659,refit.every = 10,refit.window ='moving',
                             solver = 'hybrid',calculate.VaR = TRUE,VaR.alpha = 0.01,
                             keep.coef = T,cluster = cl)



sp_fore_var = zoo(sp_roll@forecast$VaR[,1])
index(sp_fore_var) = as.Date(rownames(sp_roll@forecast$VaR))
sp_copula_fore_var = zoo(copula_sp_roll@forecast$VaR[,1])
index(sp_copula_fore_var) = as.Date(rownames(sp_roll@forecast$VaR))
sp_ml_fore_var = zoo(ml_sp_roll@forecast$VaR[,1])
index(sp_ml_fore_var) = as.Date(rownames(sp_roll@forecast$VaR))
sp_actual_var = zoo(sp_roll@forecast$VaR[,2])
index(sp_actual_var) = as.Date(rownames(sp_roll@forecast$VaR))
plot(sp_actual_var,type='p',main = 'SP500 1-day ahead VaR at 99% conf level',xlab = 'Date',
     ylab = 'VaR',col='green',pch = 19,ylim=c(min(sp_copula_fore_var),max(sp_actual_var)))
lines(sp_fore_var,col = 2,lwd=2)
lines(sp_copula_fore_var,col='blue',lwd=2)
lines(sp_ml_fore_var,col='yellow',lwd=2)
legend('bottomright',legend = c('actual return','ARMA-EGARCH forecast','Copula-based forecast'),col = c('green','red','blue'),lwd=2,lty = c(NA,1,1),pch = c(19,NA,NA),cex = 0.8)

##vox
vox_fore_var = zoo(vox_roll@forecast$VaR[,1])
index(vox_fore_var) = as.Date(rownames(vox_roll@forecast$VaR))
vox_copula_fore_var = zoo(copula_vox_roll@forecast$VaR[,1])
index(vox_copula_fore_var) = as.Date(rownames(vox_roll@forecast$VaR))
vox_ml_fore_var = zoo(ml_vox_roll@forecast$VaR[,1])
index(vox_ml_fore_var) = as.Date(rownames(vox_roll@forecast$VaR))
vox_actual_var = zoo(vox_roll@forecast$VaR[,2])
index(vox_actual_var) = as.Date(rownames(vox_roll@forecast$VaR))

plot(vox_actual_var,type='p',main = 'VOX 1-day ahead VaR at 99% conf level',xlab = 'Date',
     ylab = 'VaR',col='green',pch = 19,ylim=c(min(vox_ml_fore_var),max(vox_actual_var)))
lines(vox_fore_var,col = 2,lwd=2)
lines(vox_copula_fore_var,col='blue',lwd=2)
lines(vox_ml_fore_var,col='pink',lwd=2)
legend('bottom',legend = c('actual return','ARMA-EGARCH','Copula-based','SVM'),col=c('green','red','blue','pink'),cex=0.8,lwd=2)
#xlf
xlf_fore_var = zoo(xlf_roll@forecast$VaR[,1])
index(xlf_fore_var) = as.Date(rownames(xlf_roll@forecast$VaR))
xlf_copula_fore_var = zoo(copula_xlf_roll@forecast$VaR[,1])
index(xlf_copula_fore_var) = as.Date(rownames(xlf_roll@forecast$VaR))
xlf_ml_fore_var = zoo(ml_xlf_roll@forecast$VaR[,1])
index(xlf_ml_fore_var) = as.Date(rownames(xlf_roll@forecast$VaR))
xlf_actual_var = zoo(xlf_roll@forecast$VaR[,2])
index(xlf_actual_var) = as.Date(rownames(xlf_roll@forecast$VaR))

plot(xlf_actual_var,type='p',main = 'XLF 1-day ahead VaR at 99% conf level',xlab = 'Date',
     ylab = 'VaR',col='green',pch = 19,ylim=c(min(xlf_ml_fore_var),max(xlf_ml_fore_var)))
lines(xlf_fore_var,col = 2,lwd=2)
lines(xlf_copula_fore_var,col='blue',lwd=2)
lines(xlf_ml_fore_var,col='pink',lwd=2)
legend('bottom',legend = c('actual return','ARMA-EGARCH','Copula-based','SVM'),col=c('green','red','blue','pink'),cex=0.8,lwd=2)
#xlk
xlk_fore_var = zoo(xlk_roll@forecast$VaR[,1])
index(xlk_fore_var) = as.Date(rownames(xlk_roll@forecast$VaR))
xlk_copula_fore_var = zoo(copula_xlk_roll@forecast$VaR[,1])
index(xlk_copula_fore_var) = as.Date(rownames(xlk_roll@forecast$VaR))
xlk_ml_fore_var = zoo(ml_xlk_roll@forecast$VaR[,1])
index(xlk_ml_fore_var) = as.Date(rownames(xlk_roll@forecast$VaR))
xlk_actual_var = zoo(xlk_roll@forecast$VaR[,2])
index(xlk_actual_var) = as.Date(rownames(xlk_roll@forecast$VaR))

plot(xlk_actual_var,type='p',main = 'XLK 1-day ahead VaR at 99% conf level',xlab = 'Date',
     ylab = 'VaR',col='green',pch = 19,ylim=c(min(xlk_ml_fore_var),max(xlk_actual_var)))
lines(xlk_fore_var,col = 2,lwd=2)
lines(xlk_copula_fore_var,col='blue',lwd=2)
lines(xlk_ml_fore_var,col='pink',lwd=2)
legend('bottom',legend = c('actual return','ARMA-EGARCH','Copula-based','SVM'),col=c('green','red','blue','pink'),cex=0.8,lwd=2)







### financil crisis period 
start_date = as.Date('2007-08-09')  # liquidity crisis
end_date = as.Date('2009-01-01')
fc_ret = in_ret_all[rownames(in_ret_all)>= start_date & rownames(in_ret_all)<=end_date,]
fc_fit_sp = -(ugarchfit(spec = sp_uspec,data = fc_ret[,1],solver = 'hybrid',
                   parallel = T,core = 4)@fit$var)
fc_fit_vox = -(ugarchfit(spec = vox_uspec,data = fc_ret[,2],solver = 'hybrid',
                      parallel = T,core = 4)@fit$var)
fc_fit_xlf = -(ugarchfit(spec = xlf_uspec,data = fc_ret[,3],solver = 'hybrid',
                      parallel = T,core = 4)@fit$var)
fc_fit_xlk = -(ugarchfit(spec = xlk_uspec,data = fc_ret[,4],solver = 'hybrid',
                      parallel = T,core = 4)@fit$var)
fc_fit_xlb = -(ugarchfit(spec = xlb_uspec,data = fc_ret[,5],solver = 'hybrid',
                      parallel = T,core = 4)@fit$var)
fc_fit_xli = -(ugarchfit(spec = xli_uspec,data = fc_ret[,6],solver = 'hybrid',
                      parallel = T,core = 4)@fit$var)
fc_fit_xlu = -(ugarchfit(spec = xlu_uspec,data = fc_ret[,7],solver = 'hybrid',
                      parallel = T,core = 4)@fit$var)
fc_fit_xlp = -(ugarchfit(spec = xlp_uspec,data = fc_ret[,8],solver = 'hybrid',
                      parallel = T,core = 4)@fit$var)
fc_fit_xly = -(ugarchfit(spec = xly_uspec,data = fc_ret[,9],solver = 'hybrid',
                      parallel = T,core = 4)@fit$var)
fc_fit_xle = -(ugarchfit(spec = xle_uspec,data = fc_ret[,10],solver = 'hybrid',
                      parallel = T,core = 4)@fit$var)
fc_fit_xlv = -(ugarchfit(spec = xlv_uspec,data = fc_ret[,11],solver = 'hybrid',
                      parallel = T,core = 4)@fit$var)

fc_var = data.frame(fc_fit_sp,fc_fit_vox,fc_fit_xlf,fc_fit_xlk,
                    fc_fit_xlb,fc_fit_xli,fc_fit_xlu,fc_fit_xlp,
                    fc_fit_xly,fc_fit_xle,fc_fit_xlv) 

vox_var_r = fc_var$fc_fit_vox/fc_var$fc_fit_sp
xlf_var_r = fc_var$fc_fit_xlf/fc_var$fc_fit_sp
xlk_var_r = fc_var$fc_fit_xlk/fc_var$fc_fit_sp
xlb_var_r = fc_var$fc_fit_xlb/fc_var$fc_fit_sp
xli_var_r = fc_var$fc_fit_xli/fc_var$fc_fit_sp
xlu_var_r = fc_var$fc_fit_xlu/fc_var$fc_fit_sp
xlp_var_r = fc_var$fc_fit_xlp/fc_var$fc_fit_sp
xly_var_r = fc_var$fc_fit_xly/fc_var$fc_fit_sp
xle_var_r = fc_var$fc_fit_xle/fc_var$fc_fit_sp
xlv_var_r = fc_var$fc_fit_xlv/fc_var$fc_fit_sp
var_r_df = data.frame(vox_var_r,xlf_var_r,xlk_var_r,xlb_var_r,
                      xli_var_r,xlu_var_r,xlp_var_r,
                      xle_var_r,xlv_var_r)
index(var_r_df) = as.Date(rownames(fc_ret))
mean_var_r = apply(var_r_df,MARGIN = 2, FUN = mean) 
top4 = names(sort(mean_var_r,decreasing = T)[1:4]) # xlf,xle,xlb,vox

zoo_xlf = zoo(xlf_var_r)
index(zoo_xlf) = as.Date(rownames(fc_ret))
zoo_xle = zoo(xle_var_r)
index(zoo_xle) = as.Date(rownames(fc_ret))
zoo_xlb = zoo(xlb_var_r)
index(zoo_xlb) = as.Date(rownames(fc_ret))
zoo_vox = zoo(vox_var_r)
index(zoo_vox) = as.Date(rownames(fc_ret))
par(mfrow=c(1,1))
plot(zoo_xlf,col=1,lty=1,lwd=2,xlab='Date',ylab = 'VaR Ratio',
     main='Top 4 risk contributors during financial crisis',ylim=c(0,9))
lines(zoo_xle,col=2,lty=2,lwd=2)
lines(zoo_xlb,col=3,lty=3,lwd=2)
lines(zoo_vox,col=4,lty=4,lwd=2)
legend('topleft',legend = c('Financial','Energy','Materials','Telecom'),col=c(1,2,3,4),
       lty=c(1,2,3,4),lwd=c(2,2,2,2))
