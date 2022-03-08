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
cust.df$age <- round(rnorm(ncust, mean = 30, sd=10),0)
cust.df <- cust.df[cust.df$age > 18,]; ncust <- nrow(cust.df)
cust.df$email <- factor(sample(c("yes", "no"), size=ncust, replace=TRUE, prob=c(0.8, 0.2)))
cust.df$member.since <- factor(sample(2019:2021, size=ncust, replace=TRUE, prob=rDirichlet(c(3,2,2))))
cust.df$distance.to.store <- exp(rnorm(n=ncust, mean=1, sd=1))
cust.df$distance.to.store <- ifelse(cust.df$distance.to.store > 100, exp(rnorm(n=ncust, mean=1, sd=1.2)),
                                    cust.df$distance.to.store )
cust.df$distance.to.store <- round(cust.df$distance.to.store, 2)
#hist(cust.df$distance.to.store, xlim = c(0,100), breaks = 100)

cust.df$store.trans <- rpois(ncust, lambda = 70*exp(rnorm(n=ncust, mean = 0.5/sqrt(cust.df$distance.to.store), sd =0.3)))
#hist(cust.df$store.trans, 50)
#cust.df$store.trans <- rnbinom(ncust, size=30, mu=30 / sqrt(cust.df$distance.to.store))
cust.df$store.spend <- round(5*exp(rnorm(ncust, mean= 25/sqrt(cust.df$age), sd=0.4)) * cust.df$store.trans,3)
#plot(store.spend ~ age, data = cust.df)

cust.df$online.visits <- rnbinom(ncust,
                                 size = 1,
                                 mu = 35 + ifelse(cust.df$email == "yes", 30, 0) -
                                 0.3*sqrt(cust.df$age))
summary(cust.df$online.visits)
#cust.df$conversion.rate <- rbeta(ncust,3,5)
cust.df$online.trans <- round(cust.df$online.visits * rbeta(ncust,3,5), 0)
cust.df$online.spend <- round(rnorm(ncust, mean=700, sd=150) * cust.df$online.trans, 3)
cust.df$total.spend <- round(cust.df$online.spend + cust.df$store.spend, 3)
#summary(cust.df$total.spend)
cust.df$points <- round((rnorm(ncust, mean = 500, sd = 50) + (cust.df$online.spend*2 + cust.df$store.spend))/100,0)

cust.df$main.format <- ifelse(cust.df$online.trans > cust.df$store.trans, "Online", ifelse(
                        cust.df$distance.to.store < 2.5 & cust.df$store.spend/cust.df$store.trans < 250,
                        sample("Convenient store", prob = mean(rbeta(sum(cust.df$online.trans < cust.df$store.trans, na.rm = T),10,2))),
                        "Supermarket"))

# cust.df$prob = ifelse(cust.df$age %in% 18:25, rbinom(nrow(cust.df), 1, rbeta(ncust,9,3)),
#                       ifelse(cust.df$age %in% 26:31, rbinom(nrow(cust.df), 1, rbeta(ncust,3,5)),
#                              rbinom(nrow(cust.df), 1, rbeta(ncust,1,9))))

# cust.df$prob = rbinom(nrow(cust.df), 1, rbeta(ncust, 1,10)/)
# hist(cust.df[cust.df$prob == 1, "age"], 50)
# 
# cust.df$student = rep(27, nrow(cust.df))
# for(i in 1:nrow(cust.df)) cust.df[i, "student"] <- rbinom(1,1,cust.df[i, "prob"])
# 
# boxplot(age~prob, cust.df)

prop.table(table(cust.df$main.format))

cust.df <- cust.df[!(cust.df$store.trans > 365*3 | cust.df$online.trans > 365
                     | cust.df$age < 18 | cust.df$points > 6000 | cust.df$total.spend > 320000
                     | cust.df$online.spend < 70),]
cust.df$main.format = ifelse(cust.df$main.format == "Convenient store", "Convenience store", cust.df$main.format)

mod = lm(total.spend ~ distance.to.store + age, data = cust.df)
summary(mod)

cust.df$khang = rep(27, nrow(cust.df))
for(i in 1:nrow(cust.df)){
  cust.df[i, "khang"] <- rbinom(1, 1, rbeta(ncust, shape2 = (cust.df[i,"age"]-5),
                                            shape1=(cust.df[i,"age"]-17)))
}
hist(cust.df[cust.df$khang == 1, "age"], 50)
boxplot(age~khang, cust.df)
  
  
  rbinom(nrow(cust.df), 1, rbeta(ncust, shape2 = cust.df$age-17, shape1=3))
hist(cust.df[cust.df$khang == 1, "age"], 50)

shape2 = exp(cust.df$age-18)

summary(cust.df)
boxplot(total.spend ~ main.format, data = cust.df)


# cust.df$quantile <- quantile(cust.df$total.spend, prob = seq(0,1,1/6), na.rm = T)
# cust.df$last.trans <- factor(sample(1:12, size=ncust, replace=TRUE, prob=c(0.8, 0.2)))



mod <- lm(total.spend ~ age + distance.to.store + main.format + member.since, data = cust.df)
summary(mod)

new_data = cust.df[cust.df$cust.id %in% c("705108","741366", "2042086"), -c(1, 7, 10, 11)]
