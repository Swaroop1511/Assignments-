churn_input  <- read.csv(file.choose())  # open the Churn.csv file on blackboard
head(churn_input)
sum(churn_input$Churned)

# Lets look around first at those who actually churned
ch <- churn_input[churn_input$Churned=="1",]
qplot( x = ch$Churned_contacts  )
qplot( x = ch$Age  ) 
qplot( x = ch$Married ) 
qplot( x = ch$Cust_years)    

# Now let's see those who did not churn
nch <- churn_input[churn_input$Churned=="0",]
qplot( x = nch$Churned_contacts  )
qplot( x = nch$Age  ) 
qplot( x = nch$Married ) 
qplot( x = nch$Cust_years)


Churn_logistic1 <- glm (Churned~Age + Married + Cust_years + Churned_contacts, 
                        data=churn_input, family=binomial(link="logit"))
Churn_logistic1
summary(Churn_logistic1)  # Eliminate Married and Cust_years



Churn_logistic2 <- glm (Churned~Age + Churned_contacts, 
                        data=churn_input, family=binomial(link="logit"))
Churn_logistic2
summary(Churn_logistic2)

