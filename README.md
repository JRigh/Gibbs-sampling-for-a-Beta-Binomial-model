# Gibbs-sampling-for-a-Beta-Binomial-model
Gibbs sampling for a Beta Binomial model, with example

# Gibbs sampler with burn in
Gibbs_sampler <- function(N, burn, alpha, beta, n) {
  
  # initialization
  Y <- matrix(0, N, 2) # empty matrix to record the simulated values
  Y[1,1] <- round(n*alpha/(alpha+beta)) # initial numerber of yes voters 
  Y[1,2] <- alpha/(alpha+beta) # initial observation the proportion of yes voters
  
  # run MCMC
  for(i in 2:N) {
    Y[i,1] = rbinom(1, size = n, prob = Y[i-1,2]) # sample from the marginal Binomial dist.
    Y[i,2] = rbeta(1, Y[i, 1] + alpha, n - Y[i, 1] + beta) # sample from marginal BetaBin dist.
  }

  Y <- Y[-sample((1:N), size = burn, replace = FALSE), ] # discard random observation
  
  return(data.frame(y=Y[,1], p=Y[,2]))
}

# The initial estimated percentage that a popular initiative is accepted is 
# 0.44% (44 yes on 1000 people surveyed) about 6% of error  

alpha = 29.92; beta = 38.08
alpha/(alpha+beta) # mean centered on 0.44

(alpha*beta) / ((alpha+beta)^2 * (alpha+beta+1)) # variance 
((alpha*beta) / ((alpha+beta)^2 * (alpha+beta+1)))^0.5 # standard deviation
# more of less 6%

# our prior distribution on the proportion of yes voters
seq=seq(from = 0, to = 1, length = 100)
q=dbeta(seq, 29.92, 38.08)

df=data.frame(seq,q)
ggplot(df, aes(seq)) +              
  geom_line(aes(y=q), colour="red3", size = 1.5) + 
  geom_text(x=0.7, y=5, label="mean = 0.44", size = 6)+
  geom_text(x=0.7, y=4, label="sd = 0.06", size = 6)+
  xlab("p")+ylab("density")+
  xlim(0, 1)+ ylim(0, 7) +
  ggtitle("Beta(29.92, 38.08) prior")

# run the simulations with N=200000 and 50000 randomly discarded observations
set.seed(1986)
random_sample <- Gibbs_sampler(N = 200000, burn = 50000,
                               alpha = alpha,
                               beta = beta, n = 1000)
head(random_sample)
dim(random_sample)

# check posterior means
mean(random_sample$y);mean(random_sample$p)

# posterior 95% credible interval for y and p
quantile(random_sample$y, probs = c(0.025,0.5,0.975))
quantile(random_sample$p, probs = c(0.025,0.5,0.975))

# probability that the popular initiative is accepted (majority of the voters)
length(which(random_sample$y > 500 )) / (length(random_sample$y))

# ploting the results
random_sample2 <- subset(random_sample, random_sample$y > 500 )
dim(random_sample2)

library(ggplot2)
library(gridExtra)

htop <- ggplot(data=random_sample, aes(x=y)) + 
  geom_histogram(aes(y=..density..), fill = "grey90", color = "black", binwidth = 0.3) + 
  stat_density(colour = "red3", geom="line", size = 1.2, position="identity", show.legend=FALSE) +
  theme(axis.title.x = element_blank(),
        panel.background = element_blank())

blank <- ggplot() + geom_point(aes(1,1), colour="white") +
  theme(axis.ticks=element_blank(), panel.background=element_blank(), panel.grid=element_blank(),
        axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())

scatter <- ggplot(data=random_sample, aes(x=y, y=p)) + 
  geom_point(size = 0.6, pch=3) + 
  geom_smooth(method = "lm",se = FALSE, color="red4") +
  geom_text(x=-2.3, y= 1.9, label="R2 = 0.8053") +
  geom_point(data=random_sample2, 
             aes(x=y,y=p), 
             color='red',
             size=1)
theme_light()

hright <- ggplot(data=random_sample, aes(x=p)) + 
  geom_histogram(aes(y=..density..), fill = "grey90", color = "black", binwidth = 0.05) + 
  stat_density(colour = "red3", geom="line", size = 1.2, position="identity", show.legend=FALSE) +
  coord_flip() + theme(axis.title.y = element_blank(),
                       panel.background = element_blank())

grid.arrange(htop, blank, scatter, hright, ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4))

# check for convergence of the chain using traceplots
par(mfrow=c(2,1))
plot(x=1:length(random_sample$y), y=random_sample$y, type="l")
plot(x=1:length(random_sample$p), y=random_sample$p, type="l")

# Bayesian convergence to the joint posterior
y1 <- random_sample$y[1:500]
p1 <- random_sample$p[1:500]

par(mfrow=c(1,1))
plot(y1, p1, type='l', main='Bayesian convergence to the joint posterior distribution', 
     xlab=expression(y[1]), ylab=expression(p[1]))


# Histogram y
qplot(random_sample$y, geom="histogram",
      fill=..count.., bins = length(seq(0.3,1,0.01))) +
  scale_fill_gradient(low="firebrick1", high="firebrick4") + 
  ggtitle("Histogram of simulated number of yes voters on random samples of size n=1000") +
  annotate(geom="text", x=600, y=5000, label="0.1606",
           color="black") +
  theme(plot.title = element_text(size=10),
        axis.text.x = element_text(size = 10),
        axis.title=element_text(size=8)) +
  scale_x_continuous(name = "Number of yes voters") + 
  geom_vline(xintercept=500, color="black", size = 1.2,
             linetype="dashed") 

# Histogram p
qplot(random_sample$p, geom="histogram",
      fill=..count.., bins = length(seq(0.3,1,0.01))) +
  scale_fill_gradient(low="firebrick1", high="firebrick4") + 
  ggtitle("Histogram of simulated number of outcome p on random samples of size n=1000") +
  annotate(geom="text", x=0.7, y=4000, label="0.1551",
           color="black") +
  theme(plot.title = element_text(size=10),
        axis.text.x = element_text(size = 10),
        axis.title=element_text(size=8)) +
  scale_x_continuous(name = "percentage of yes") + 
  geom_vline(xintercept=0.5, color="black", size = 1.2,
             linetype="dashed")
