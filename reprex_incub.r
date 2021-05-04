library(reprex)

set.seed(2718)
data <- matrix(  data = rep(0,80), ncol = 2)

for (i in 1:10) {
  level <- abs(rnorm(1,0,50))
  bias  <- rnorm(1,0,.5)
  data[((i)*4-3):(i*4),] <- rep(c(level,level+bias),4)
  
}
data <- round( data, 1)
colnames(data) <- c("y","x")
data <- as.data.frame(data)


mod <- lm( y ~ x, data)

data_b <- data[c(1,4,7,10),]


modb <- lm( y ~ x, data_b)

summary(mod)

summary(modb)

reprex()