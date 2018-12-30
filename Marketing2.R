set.seed(21821)
ncust <- 1000
cust <- data.frame(cust.id = as.factor(1:(ncust)))
cust$age <- rnorm(n = ncust, mean = 35, sd = 5)
cust$credit.score <- rnorm(n = ncust,  mean = 3+cust$age + 620, sd = 50)
cust$email <- factor(sample(c("yes", "no"), size = ncust, replace = TRUE, prob = c(0.8, 0.2)))
cust$dist <- exp(rnorm(n = ncust, mean = 2, sd = 1.2))
summary(cust)
head(cust)

hist(cust$dist)

cust$online_vis <- rnbinom(ncust, size = 0.3, 
                           mu = 15+ ifelse(cust$email =="yes",
                                           15,0)
                           - 0.7 *(cust$age- median(cust$age)))
head(cust$online_vis)
cust$online_trans <- rbinom(ncust, size = cust$online_vis, prob = 0.3)
cust$online_spend <- exp(rnorm(ncust, mean = 3, sd = 0.1)) * cust$online_trans
head(cust$online_spend)

cust$store_trans <- rnbinom(ncust, size = 5, 
                            mu=3/sqrt(cust$dist))
head(cust$store_trans)
cust$store_spend <- exp(rnorm(ncust, mean = 3.5, sd = 0.4)) * cust$dist
head(cust$store_spend)
summary(cust)
sat.overall <- rnorm(ncust, mean = 3.1, sd = 0.7)
summary(sat.overall)
sat.service <- floor(sat.overall + rnorm(ncust, mean = 0.5, sd = 0.4))
sat.selection <- floor(sat.overall + rnorm(ncust, mean = -0.2, sd = 0.6))
summary(cbind(sat.service, sat.selection))
cbind(sat.service, sat.selection)
sat.service[sat.service >5] <- 5
sat.service[sat.service <1] <- 1
sat.selection[sat.selection >5] <- 5
sat.selection[sat.selection <1] <- 1

summary(cbind(sat.service, sat.selection))

no.response <- as.logical(rbinom(ncust, size = 1, prob = cust$age/100))
sat.service[no.response] <- NA
sat.selection[no.response] <- NA 

summary(cbind(sat.service, sat.selection))
cust$sat.service <- sat.service
cust$sat.selection <- sat.selection
summary(cust)
