house <- read.csv(file.choose())   
head(house)    
qplot( x = house$X506 , y= house$X13)  
qplot( x = house$X13 , y= house$X13) 
qplot( x = house$X.1 , y= house$X13)    

results = lm ( house$X506~house$X13+house$X.1 )
results
summary(results)
x <- runif(75,0,10)  # 75 random numbers of uniform distribution
x<-  sort(x)
y <- 200 + x^3 - 10 * x^2 + x + rnorm(75,0,20)

lr <- lm(y~x)
lr    
# print out the deducted equation
# draw the linear regression line

#points( x, lr$coefficients[1] + lr$coefficients[2] * x, type="l", col=4 ) 

plot(x,y)


lr <- lm(y~x)
lr

points(x,lr$coefficients[1] + lr$coefficients[2] * x, type="l", col=4 ) 

x <- runif(75,0,10)  # 75 random numbers of uniform distribution
x<-  sort(x)
y <- 200 + x^3 - 10 * x^2 + x + rnorm(75,0,20)

plot(x,y)


# Polynomial regression

lr <- lm(y~x)
lr
poly <- loess(y~x)
fit <- predict(poly)
points(x,fit, type="l", col=2)
points(x,lr$coefficients[1] + lr$coefficients[2] * x, type="l", col=4 )

# Logistic regression
house_input  <- read.csv(file.choose()) 
head(house_input)




# Assuming 'house_input' is your data frame and 'Target' is the dependent variable
house_logistic1 <- glm(Target ~ X506 + X13 + X.2, 
                       data = house_input, 
                       family = binomial(link = "logit"))

# Print summary of the model
summary(house_logistic1)

