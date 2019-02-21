require(readxl)
require(entropy)
require(MCMCpack)
require(scatterplot3d)
require(plotly)
require(quadprog)

parse_rank = function(rank){
  num = substr(rank, 1, nchar(rank)-2)
  return(as.numeric(num))
}

parse_cons = function(cons){
  num = substr(cons, 1, nchar(cons)-1)
  return(as.numeric(num)/100.0)
}



s = read_excel('data/symbol-02122019.xlsx')

files = list.files(path="data/ANR-02122019/")

fdf = data.frame(symbol=character(length(files)), mu=numeric(length(files)), sigma=numeric(length(files)), entropy=numeric(length(files)), position=numeric(length(files)), stringsAsFactors = F)

for (i in 1:length(files)){
  
  p = read_excel(paste('data/ANR-02122019/', files[i], sep = ''), col_names = FALSE)
  
  s = p[1,1]$X__1
  
  cat(s,'\n')
  
  cat(as.character(p[9,1]), as.numeric(p[9,3]),'\n')
  p_last = as.numeric(p[9,3])
  
  df_names = c('firm','analyst','action','recommendation','tgt_px','date','yr_rtn','barr','rank')
  df = p[14:nrow(p),]
  names(df) = df_names
  
  r = (as.numeric(df$tgt_px)-p_last)/p_last
  mu = mean(r, na.rm = TRUE)
  qplot(r)+geom_vline(xintercept = 0.0)
  
  sigma = var(r, na.rm = TRUE)
  
  consensus = unlist(lapply(p[5:7,2]$X__2, parse_cons))
  h = entropy(rep(1/3, 3)) - entropy(consensus)
  
  fdf[i,'symbol'] = s
  fdf[i,'mu'] = abs(mu)
  fdf[i,'sigma'] = sigma
  fdf[i,'entropy'] = h
  if (mu>=0.0){
    fdf[i,'position'] = 1
  } else {
    fdf[i,'position'] = -1
  }
}

View(read_excel(paste('data/ANR-02122019/', files[which(fdf$symbol == "AMRN US Equity")], sep = ''), col_names = FALSE))

write.csv(x = fdf, file = 'anr.csv', row.names = F)

nsim=100000
simdf = data.frame(mu=numeric(nsim), sigma=numeric(nsim), entropy=numeric(nsim), stringsAsFactors = F)
for (i in 1:nsim){
  w = rdirichlet(1, rep(abs(rcauchy(1)),length(files)))
  
  simdf[i,'mu'] = sum(w*fdf$mu)
  simdf[i,'sigma'] = sum(w*fdf$sigma)
  simdf[i,'entropy'] = sum(w*fdf$entropy)
}

ggplot(simdf, aes(x=sigma,y=mu,color=entropy))+geom_point()
ggplot(simdf, aes(x=mu,y=entropy,color=sigma))+geom_point()
plot_ly(simdf, x = ~mu, y = ~sigma, z = ~entropy)







# Get monthly return data from 2012 through 2013
require(quantmod)
myStocks <- c("BOX")
getSymbols(myStocks ,src='yahoo')
returns.df <- lapply(myStocks, function(s) periodReturn(eval(parse(text=s)),period='daily'))

# Plot monthly return data
require(ggplot2)
require(reshape2)
returns2 <- as.data.frame(returns.df)
returns2$date <- row.names(returns2)
returns2 <- melt(returns2, id="date")
ggplot(returns2, aes(x=date,y=value, group=variable)) +
  geom_line(aes(color=variable)) +
  ylab("Daily Return")+ xlab("Date") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(returns2, aes(x=value))+geom_histogram(binwidth = 0.005)

# Compute the average returns and covariance matrix of the return series
r <- matrix(colMeans(data.frame(returns.df)), nrow=1)
C <- cov(data.frame(returns.df))

# Stage and solve the QP
require(quadprog)
A    <- matrix(1,1,10)
A    <- rbind(A, r, diag(10),-diag(10))
f    <- c(1, 0.01, rep(0,10),rep(-1,10))
sol <- solve.QP(Dmat=C, dvec = rep(0,10), Amat=t(A), bvec=f, meq=1)

require(ggplot2)
portfolio <- data.frame(name = myStocks, w = round(sol$solution,3))
ggplot(portfolio, aes(x=name, y=w)) + geom_bar(stat="identity", fill="blue")

library("fPortfolio")
require(ROI)
data(LPP2005.RET)
lppData <- 100 * LPP2005.RET[, 1:6]
r <- mean(lppData)
r

foo <- Q_objective(Q = cov(lppData), L = rep(0, ncol(lppData)))
full_invest <- L_constraint(rep(1, ncol(lppData)), "==", 1)
target_return <- L_constraint(apply(lppData, 2, mean), "==",r)
op <- QP(objective = foo, constraints = rbind(full_invest, target_return))
op

lp  <- OP(objective  =  fdf$mu,
          constraints = rbind(budget_constraint(djia2018),
                              group_constraint(djia2018, index = c(3, 17), dir = "==", rhs = 0.5)),
          maximum = T)
#===========================================

