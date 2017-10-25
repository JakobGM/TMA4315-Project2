myglm <- function(formula, data = list(), contrasts = NULL, ...){
  # Extract model matrix & responses
  mf <- model.frame(formula = formula, data = data)
  X  <- model.matrix(attr(mf, "terms"), data = mf, contrasts.arg = contrasts)
  y  <- model.response(mf)
  terms <- attr(mf, "terms")
  
  #The log-likelihood-function
  ML = function(x){
    MLE = 0
    for(i in 1:length(y)){MLE = MLE +y[i] * t(X[i,]) %*% x-exp(t(X[i,]) %*% x)}
    return(-1*MLE)
  }
  
  #Maximizing the log-likelihood
  MLE = optim(matrix(0,nrow=17,ncol=1),ML,method = "BFGS")
  betahat = MLE$par
  row.names(betahat) = c("Intercept","Home",tippeliga$home[2:16])
  colnames(betahat) = "Estimate"
  
  est <- list(terms = terms, model = mf)
  
  # Store call and formula used
  est$call <- match.call()
  est$formula <- formula
  est$coefficients <- betahat
  
  # Set class name. This is very important!
  class(est) <- 'myglm'
  
  # Return the object with all results
  return(est)
}

#Load data set
filepath <- "https://www.math.ntnu.no/emner/TMA4315/2017h/tippeligaen2016.dat"
tippeliga <- read.table(file = filepath, header = TRUE, colClasses = c("character", "character", "numeric", "numeric"))

#Create reanking from the results
ranking <- function(tippeliga){
scores = data.frame(matrix(0,nrow=16,ncol=1),matrix(0,nrow=16,ncol=1))
row.names(scores) = tippeliga$home[1:16]
colnames(scores) = c("Points","GD")
for(i in 1:240){
  if(tippeliga$yh[i] > tippeliga$ya[i]){
    scores[tippeliga$home[i],1] = scores[tippeliga$home[i],1]+3
  }else if(tippeliga$yh[i] < tippeliga$ya[i]){
    scores[tippeliga$away[i],1] = scores[tippeliga$away[i],1]+3
  }else{
    scores[tippeliga$home[i],1] = scores[tippeliga$home[i],1]+1
    scores[tippeliga$away[i],1] = scores[tippeliga$away[i],1]+1
  }
  scores[tippeliga$home[i],2] = scores[tippeliga$home[i],2] + tippeliga$yh[i]-tippeliga$ya[i]
  scores[tippeliga$away[i],2] = scores[tippeliga$away[i],2] - tippeliga$yh[i]+tippeliga$ya[i]
}
scores = scores[order(-scores$Points,-scores$GD),]
return(scores)
}

#Creating the design matrix
X = matrix(0,nrow=480,ncol=18)
colnames(X) = c("Intercept","Home",tippeliga$home[1:16])
for(i in 1:240){
  #Home:
  X[i,"Home"] = 1
  X[i,"Intercept"] = 1
  X[i,tippeliga$home[i]] = 1
  X[i,tippeliga$away[i]] = -1
  
  #Away:
  X[i+240,"Intercept"] = 1
  X[i+240,tippeliga$home[i]] = -1
  X[i+240,tippeliga$away[i]] = 1
}
X = X[,-3] #remove Aalesund
goals = c(tippeliga$yh,tippeliga$ya)

summary(glm(goals ~ -1 + X, family = poisson(link=log)))

#Ranking the strengths
strengths = myglm(goals ~ -1 + X)$coefficients
strengthsRank = strengths[3:17, , drop= FALSE]
strengthsRank = strengthsRank[order(-strengthsRank[,1]), ,drop=FALSE]


#Simulation
rank = ranking(tippeliga)
rankings = matrix(0,nrow=16,ncol=1000)
row.names(rankings) = row.names(rank)

set.seed(1792)
betahat = myglm(goals ~ -1 + X)$coefficients
for(season in 1:1000){
  tippeliga1 = data.frame(tippeliga$home,tippeliga$away,matrix(0,nrow=240,ncol=1),matrix(0,nrow=240,ncol=1),stringsAsFactors=FALSE)
  colnames(tippeliga1) = c("home","away","yh","ya")
  for(i in 1:240){
    tippeliga1$yh[i] = rpois(1,lambda = exp(t(X[i,]) %*% betahat))
    tippeliga1$ya[i] = rpois(1,lambda = exp(t(X[i+240,]) %*% betahat))
  }
  tippeliga1scores = ranking(tippeliga1)
  for(i in 1:16){
    rankings[row.names(tippeliga1scores[i,1,drop=FALSE]),season] = i
  }
}

#Plot simulated results
g = ggplot(data = melt(rankings),aes(x = value,fill=as.factor(Var1))) + 
  facet_wrap( ~ as.factor(Var1),ncol=4) +
  guides(fill=FALSE) +
  geom_histogram(binwidth=1,center=0,col="white") +
  scale_x_continuous(breaks=1:16,limits = c(0,17)) +
  theme(panel.grid.minor.x  = element_blank(),panel.grid.major.x = element_blank())
g

