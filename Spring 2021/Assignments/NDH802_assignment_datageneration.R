set.seed(2308)

rDirichlet <- function(param){
  nCat <- length(param)
  piDraws <- matrix(NA,nCat,1)
  for (j in 1:nCat){
    piDraws[j] <- rgamma(1,param[j],1)
  }
  piDraws = piDraws/sum(piDraws) # Diving every column of piDraws by the sum of the elements in that column.
  return(piDraws)
}

ncust <- 2706308
cust.df <- data.frame(cust.id=as.factor(sample(1:ncust)))
cust.df$age <- round(rnorm(ncust, mean= 38, sd=5),0)
cust.df$email <- factor(sample(c("yes", "no"), size=ncust, replace=TRUE, prob=c(0.8, 0.2)))
cust.df$member.since <- factor(sample(2015:2021, size=ncust, replace=TRUE, prob=rDirichlet(c(1,1,2,2,3,2,2))))
cust.df$distance.to.store <- exp(rnorm(n=ncust, mean=1, sd=1))
cust.df$distance.to.store <- ifelse(cust.df$distance.to.store > 100, exp(rnorm(n=ncust, mean=1, sd=1.2)),
                                    cust.df$distance.to.store )
cust.df$distance.to.store <- round(cust.df$distance.to.store, 2)
hist(cust.df$distance.to.store, xlim = c(0,100), breaks = 100)

cust.df$store.trans <- rpois(ncust, lambda = 70*exp(rnorm(n=ncust, mean = 0.5/sqrt(cust.df$distance.to.store), sd =0.3)))
hist(cust.df$store.trans, 50)
#cust.df$store.trans <- rnbinom(ncust, size=30, mu=30 / sqrt(cust.df$distance.to.store))
cust.df$store.spend <- 5*exp(rnorm(ncust, mean= 25/sqrt(cust.df$age), sd=0.4)) * cust.df$store.trans
#plot(store.spend ~ age, data = cust.df)

cust.df$online.visits <- rnbinom(ncust,
                                 size = 1,
                                 mu = 30 + ifelse(cust.df$email == "yes", 30, 0) -
                                 0.5*sqrt(cust.df$age))
summary(cust.df$online.visits)
#cust.df$conversion.rate <- rbeta(ncust,3,5)
cust.df$online.trans <- round(cust.df$online.visits * rbeta(ncust,3,5), 0)
cust.df$online.spend <- exp(rnorm(ncust, mean=7, sd=0.3)) * cust.df$online.trans
cust.df$total.spend <- cust.df$online.spend + cust.df$store.spend
#summary(cust.df$total.spend)
cust.df$points <- round((rnorm(ncust, mean = 500, sd = 100) + (cust.df$online.spend*2 + cust.df$store.spend))/100,0)

cust.df$main.store <- ifelse(cust.df$online.trans > cust.df$store.trans, "Online", ifelse(
                        cust.df$distance.to.store < 2.5 & cust.df$store.spend/cust.df$store.trans < 250,
                        sample("Convenient store", prob = mean(rbeta(sum(cust.df$online.trans < cust.df$store.trans),10,2))),
                        "Supermarket"))
prop.table(table(cust.df$main.store))
summary(cust.df)



# cust.df$quantile <- quantile(cust.df$total.spend, prob = seq(0,1,1/6), na.rm = T)
# cust.df$last.trans <- factor(sample(1:12, size=ncust, replace=TRUE, prob=c(0.8, 0.2)))



mod <- lm(total.spend ~ age + distance.to.store + main.store + member.since, data = cust.df)
summary(mod)

#cust.df <- cust.df[!(cust.df$online.trans == 0 & cust.df$store.trans == 0), ]

summary(cust.df)
