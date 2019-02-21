require(readxl)
require(curl)
require(ggplot2)
require(rethinking)
require(rstan)
require(bayesplot)
require(quadprog)

anr = read_excel('data/final.xlsx', sheet = 8, col_names = F)
anr.clean = anr[complete.cases(anr[ ,2]),] # if firm name is NA remove
anr.df = anr.clean[0,]
sym = ""# 
for (i in 1:nrow(anr.clean)){
  if (as.character(anr.clean[i,1]) != sym && is.na(as.character(anr.clean[i,1])) == F){
    sym = as.character(anr.clean[i,1])
    print(sym)
    j = i+1
    while (is.na(as.character(anr.clean[j,1])) && as.character(anr.clean[j,2]) != "#N/A Invalid Security"){
      j = j + 1
    }
    anr.clean[i:(j-1),1] = sym
    anr.df = rbind(anr.df, anr.clean[i:(j-1),])
  } 
}

names(anr.df) = c('security','firm','analyst','recommendation','score','score2','target_price','timeframe','date','rank','returns')

sym.list = unlist(lapply(unique(anr.df$security), function(x){strsplit(x,' ')[[1]][1]}))
sym.list.str = paste(sym.list, collapse = "%2C")

req = curl_fetch_memory(paste("https://api.tdameritrade.com/v1/marketdata/quotes?apikey=OFWNX9PJR###&symbol=", sym.list.str, sep = ''))
obj = jsonlite::fromJSON(rawToChar(req$content))

last.price = data.frame(security=unique(anr.df$security), last_price=numeric(length(unique(anr.df$security))), stringsAsFactors = F)
for (i in 1:nrow(last.price)){
  sym = strsplit(last.price$security[i], ' ')[[1]][1]
  last.price$last_price[i] = obj[sym][[1]]$lastPrice
}

anr.df = merge(anr.df, last.price, by='security')
anr.df$expected_return = (as.numeric(anr.df$target_price)-anr.df$last_price)/anr.df$last_price

k = which(!is.na(anr.df$expected_return))
anr.df = anr.df[k,]

ggplot(anr.df, aes(x=expected_return))+geom_histogram(binwidth = 0.01)

anr.df$sec_idx = as.numeric(as.factor(anr.df$security))

anr.df.rep = anr.df[0,]

for (i in unique(anr.df$security)){
  print(i)
  df = anr.df[which(anr.df$security == i),]
  df$rank = as.numeric(df$rank)
  max.rank = max(df$rank, na.rm = T)
  for (j in 1:nrow(df)){
    anr.df.rep = rbind(anr.df.rep, df[j,])
    if (!is.na(df$rank[j]) && max.rank-df$rank[j] > 0){
      for (k in 1:(max.rank-df$rank[j])){
        anr.df.rep = rbind(anr.df.rep, df[j,])
      }
    } 
  }
}

model = stan_model(file = 'model.stan')

J = length(unique(anr.df.rep$security))
N = nrow(anr.df.rep)
data = list(J = J, N = N, returns = anr.df.rep$expected_return, security = anr.df.rep$sec_idx)

model.fit = sampling(model, data=data, iter = 5000, warmup = 1000, chains = 2, cores = 2)
model.map = optimizing(model, data=data, hessian = TRUE)

plot(model.fit, pars = paste("mu[",1:10,"]",sep = ''))
plot(model.fit, pars = paste("sigma[",1:10,"]",sep = ''))

traceplot(model.fit, pars = c("mu"), inc_warmup = TRUE, nrow = 2)
print(model.fit, pars = c("mu", "sigma"))

mcmc_areas(as.matrix(model.fit), pars = paste("mu[",1:10,"]",sep = ''),prob = 0.8)
mcmc_areas(as.matrix(model.fit), pars = paste("sigma[",1:10,"]",sep = ''),prob = 0.8)

mu = as.data.frame(get_posterior_mean(model.fit,par=c("mu")))
mu$sec_idx = 1:nrow(mu)
mu = mu[,c("mean-all chains","sec_idx")]
names(mu)[1] = "mu"
sigma = as.data.frame(get_posterior_mean(model.fit,par=c("sigma")))
sigma$sec_idx = 1:nrow(sigma)
sigma = sigma[,c("mean-all chains","sec_idx")]
names(sigma)[1] = 'sigma'

anr.df.final = data.frame(security=unique(anr.df.rep$security), volatility=numeric(J), volatility_state=integer(J), stringsAsFactors = F)
anr.df.final$sec_idx = as.numeric(as.factor(anr.df.final$security))
anr.df.final$symbol = unlist(lapply(anr.df.final$security, function(x){strsplit(x, ' ')[[1]][1]}))
anr.df.final = merge(anr.df.final, last.price, by='security')
anr.df.final = merge(anr.df.final, mu, by='sec_idx')
anr.df.final = merge(anr.df.final, sigma, by='sec_idx')

for (i in 1:nrow(anr.df.final)){
  sym = anr.df.final$symbol[i]
  anr.df.final$volatility[i] = obj[sym][[1]]$volatility
}

write.csv(anr.df.final, file = 'anr-no-volatility.csv', row.names = F)

anr.df.final$iv = numeric(nrow(anr.df.final))
anr.df.final$strike = numeric(nrow(anr.df.final))
anr.df.final$option_price = numeric(nrow(anr.df.final))
anr.df.final$option = NA

for (i in 1:nrow(anr.df.final)){
  req.options = curl_fetch_memory(paste("https://api.tdameritrade.com/v1/marketdata/chains?apikey=OFWNX9PJR###&symbol=",anr.df.final$symbol[i],"&contractType=CALL&strikeCount=2&range=OTM&fromDate=2019-08-15", sep = ''))
  obj.options = jsonlite::fromJSON(rawToChar(req.options$content))
  
  strike.index = 2
  for (exp.index in 1:length(obj.options$callExpDateMap)){
    if (length(obj.options$callExpDateMap[[exp.index]]) < 2){
      strike.index = 1
    } else {
      strike.index = 2
    }
    if (obj.options$callExpDateMap[[exp.index]][[strike.index]]$volatility[1] != -999){
      anr.df.final$iv[i] = obj.options$callExpDateMap[[exp.index]][[strike.index]]$volatility[1]
      anr.df.final$strike[i] = obj.options$callExpDateMap[[exp.index]][[strike.index]]$strikePrice[1]
      anr.df.final$option_price[i] = obj.options$callExpDateMap[[exp.index]][[strike.index]]$last[1]
      anr.df.final$option[i] = obj.options$callExpDateMap[[exp.index]][[strike.index]]$description[1]
      break()
    }
  }
  print(paste(anr.df.final$symbol[i],anr.df.final$iv[i]))
  Sys.sleep(1)
}

anr.df.final$iv[which(anr.df.final$iv == 0)] = NA

anr.df.final = anr.df.final[complete.cases(anr.df.final[ ,'iv']),]
anr.df.final$iv = anr.df.final$iv/100.0
anr.df.final$position = NA
for (i in 1:nrow(anr.df.final)){
  if (anr.df.final$mu[i]>=0.0){
    anr.df.final$position[i] = 1
  } else {
    anr.df.final$mu[i] = abs(anr.df.final$mu[i])
    anr.df.final$position[i] = -1
  }
}
anr.df.final.filter = anr.df.final[which(anr.df.final$last_price < 50),]
write.csv(anr.df.final.filter, file = 'anr-volatility.csv', row.names = F)

mu = matrix(anr.df.final$mu)
sigma = matrix(anr.df.final$sigma)
iv = matrix(anr.df.final$iv)
S = matrix(0, length(sigma), length(sigma))
diag(S) = sigma
V = S + mu %*% t(mu)

lb = 0
ub = 0.2
uv = 0.3
lambda = 1.4
eta = seq(1.0, 10, by = 0.5)
C1 = numeric(length(eta))
C2 = numeric(length(eta))
for (k in 1:length(eta)){
  wt = getOptWt_Quadprog(mu = mu, V = V, iv = iv, lambda = lambda/eta[k], lb = lb, ub = ub, uv = uv)
  C1[k] = wt %*% mu
  C2[k] = wt %*% V %*% t(wt)
}
Cfun=C1-lambda*C2+lambda*(C1^2)
opteta = eta[which(Cfun==max(Cfun))]
opteta
wts = getOptWt_Quadprog(mu = mu, V = V, iv = iv, lambda = lambda/opteta, lb = lb, ub = ub, uv = uv)

print(paste("E[mu]", wts %*% mu))
print(paste("E[V]", wt %*% V %*% t(wt)))
print(paste("IV:", wts%*%iv))

capital = 5000
for (i in 1:length(wts)){
  if (wts[i] > 0.001){
    print(paste(anr.df.final$security[i], round(wts[i],2), anr.df.final$position[i], anr.df.final$iv[i], anr.df.final$option[i], round(wts[i]*capital)))
  }
}

getOptWt_Quadprog <- function(mu, V, iv, lambda, lb, ub, uv){
  #mu = matrix(anr.df.final$mu)
  #sigma = matrix(anr.df.final$sigma)
  # S = matrix(0, length(sigma), length(sigma))
  # diag(S) = sigma
  # V = S + mu %*% t(mu)
  
  #lambda = 1
  
  m = length(mu)
  f = 0.5 * mu
  H = lambda * V
  A = matrix(-iv, m, 1) #zeros(1, m) inclue iv constraint here
  b = -uv
  Aeq = matrix(1, m, 1) #ones(1, m)
  beq = 1
  
  #Amat = cbind(A, Aeq)
  #bvec = c(b, beq)
  #solve.QP(H, f, Amat, bvec, meq = 1)
  
  Amat = cbind(A, Aeq, diag(m), -diag(m))
  bvec = c(b, beq, rep(lb, m), rep(-ub, m)) #
  wts = solve.QP(H, f, Amat, bvec, meq = 1)
  return(t(matrix(wts$solution)))
}


