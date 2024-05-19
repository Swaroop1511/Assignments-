inc <- read.csv(file.choose())   # Open Income.csv
head(inc)    # 5 variables
# Lets look around first
qplot( x = inc$Age , y= inc$Income)  # I guess income depends on Age
qplot( x = inc$Education , y= inc$Income) # A little bit on Education
qplot( x = inc$Gender , y= inc$Income)    # not so much on gender

results = lm ( inc$Income~inc$Age+inc$Education+inc$Gender )
results
summary(results)

inc <- read.csv(file.choose())
head(inc)    # 5 variables
# Lets look around first
qplot( x = inc$Age , y= inc$Income)  # I guess income depends on Age
qplot( x = inc$Education , y= inc$Income) # A little bit on Education
qplot( x = inc$Gender , y= inc$Income)    # not so much on gender


results2 = lm ( inc$Income~inc$Age+inc$Education )
results2
summary(results2)


# Generate a linearly relation
x <- runif(75,0,10)           # 75 random numbers of uniform distribution
x <-  sort(x)
y <- 20 + 10*x + rnorm(75,0,10)  # y= 20+10x and a bit of variety
# see it!
plot(x,y)
# Now lets do the regression
lr <- lm(y~x)
lr    # print out the deducted equation
# draw the linear regression line
points( x, lr$coefficients[1] + lr$coefficients[2] * x, type="l", col=4 ) 


x <- runif(75,0,10)  # 75 random numbers of uniform distribution
x<-  sort(x)
y <- 200 + x^3 - 10 * x^2 + x + rnorm(75,0,20)


plot(x,y)


lr <- lm(y~x)
lr

points(x,lr$coefficients[1] + lr$coefficients[2] * x, type="l", col=4 ) 


x <- runif(75,0,10)  # 75 random numbers of uniform distribution
x<-  sort(x)
y <- 200 + x^3 - 10 * x^2 + x + rnorm(75,0,20)

plot(x,y)

lr <- lm(y~x)
lr
poly <- loess(y~x)     # Polynomial regression
fit <- predict(poly)
points(x,fit, type="l", col=2)
points(x,lr$coefficients[1] + lr$coefficients[2] * x, type="l", col=4 )

