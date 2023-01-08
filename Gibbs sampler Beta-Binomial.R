#------------------------------
# Gibbs sampling: Beta-Binomial
#------------------------------

# create the Gibbs sampler
Gibbs_sampler <- function(nsim, burn, alpha, beta, n) {
  
  X <- matrix(0, nsim, 2) # empty matrix to record the simulated values
  X[1,1] <- round(n*alpha/(alpha+beta)) # initial numerber of yes voters on subsamples
  X[1,2] <- alpha/(alpha+beta) # initial observation probability p, the proportion of yes voters
  
  for(i in 2:nsim) {
    X[i,1] = rbinom(1, size = n, prob = X[i-1,2]) # sample from the full conditional Binomial distribution
    X[i,2] = rbeta(1, alpha + X[i, 1], beta + n - X[i, 1]) # sample from the full conditional Beta distribution
  }
  
  b <- burn + 1 # record the burn in period (observations to be discarded)
  x <- X[b:nsim, ] 
  
  return(data.frame(x))
}

# The initial estimated percentage that a popular initiative is accepted is 
# 0.44% (44 yes on 100 people surveyed)  
# beta parameters:   alpha = 44; beta = 56; (alpha/(alpha+beta))

alpha = 4.4; beta = 5.6; (alpha/(alpha+beta))
alpha/(alpha+beta) # mean centered on 0.44

(alpha*beta) / ((alpha+beta)^2 * (alpha+beta+1)) # variance (high), reflect high uncertainty that the score will change in the future

# our prior distribution on the proportion of yes voters
p = seq(0,1, length=100)
plot(p, dbeta(p, alpha, beta), ylab="density(p)", xlab = "p", type ="l", col="red",
     main="Beta(4.6, 5.6)")

# run the simulations
set.seed(1986)
random_sample <- Gibbs_sampler(nsim = 11000, burn = 1000,
                               alpha = alpha,
                               beta = beta, n = 100)
random_sample

# extrapolate the result to the total population with a participation rate of 40%
random_sample <- data.frame(yes_voters=random_sample$X1*(8500000*0.4)/100, 
                            simulated_score = random_sample$X2)
random_sample

# probability that the popular initiative is accepted (majority of the voters)
length(which(random_sample$yes_voters > (8500000*0.4)/2 & random_sample$simulated_score > 0.5 )) / (length(random_sample[,1]))

# ploting the results

library(ggplot2)
library(gridExtra)

random_sample2 <- subset(random_sample, random_sample$yes_voters > ((8500000*0.4))/2 & random_sample$simulated_score > 0.5 )
dim(random_sample2)

htop <- ggplot(data=random_sample, aes(x=yes_voters)) + 
  geom_histogram(aes(y=..density..), fill = "grey90", color = "black", binwidth = 0.3) + 
  stat_density(colour = "red3", geom="line", size = 1.2, position="identity", show.legend=FALSE) +
  theme(axis.title.x = element_blank(),
        panel.background = element_blank())

blank <- ggplot() + geom_point(aes(1,1), colour="white") +
  theme(axis.ticks=element_blank(), panel.background=element_blank(), panel.grid=element_blank(),
        axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())

scatter <- ggplot(data=random_sample, aes(x=yes_voters, y=simulated_score)) + 
  geom_point(size = 0.6, pch=3) + 
  geom_smooth(method = "lm",se = FALSE, color="red4") +
  geom_text(x=-2.3, y= 1.9, label="R2 = 0.8053") +
  geom_point(data=random_sample2, 
             aes(x=yes_voters,y=simulated_score), 
             color='red',
             size=1)
theme_light()

hright <- ggplot(data=random_sample, aes(x=simulated_score)) + 
  geom_histogram(aes(y=..density..), fill = "grey90", color = "black", binwidth = 0.05) + 
  stat_density(colour = "red3", geom="line", size = 1.2, position="identity", show.legend=FALSE) +
  coord_flip() + theme(axis.title.y = element_blank(),
                       panel.background = element_blank())

grid.arrange(htop, blank, scatter, hright, ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4))

#----
# end
#----
