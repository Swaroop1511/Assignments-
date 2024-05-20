groceries <- read.csv(file.choose())   
head(groceries)
summary(groceries)
qplot( x =groceries$availability , y=groceries$brand)  
qplot( x = groceries$price , y= groceries$brand) 
qplot( x = groceries$currency , y= groceries$brand)    

results = lm ( groceries$brand~groceries$availability+groceries$price+groceries$currency )
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

groceries_input  <- read.csv(file.choose())  
head(groceries_input)
sum(groceries$price)

# Lets look around first at those who actually churned
gr <- groceries_input[groceries_input$price=="1",]
qplot( x = groceries$name  )
qplot( x = groceries$price  ) 
qplot( x = groceries$brand ) 
qplot( x = groceries$availability)    

# Now let's see those who did not churn
gr <- groceries_input[groceries_input$price=="0",]
qplot( x = groceries$name  )
qplot( x = groceries$price  ) 
qplot( x = groceries$brand ) 
qplot( x = groceries$availability)


# Now fit the logistic regression models
groceries_logistic1 <- glm(availability ~ price + currency + brand + avg_rating, 
                           data = groceries_input, family = binomial(link = "logit"))
summary(groceries_logistic1)

groceries_logistic2 <- glm(availability ~ price + avg_rating, 
                           data = groceries_input, family = binomial(link = "logit"))
summary(groceries_logistic2)




