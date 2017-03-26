# The Cars_mileage  data was downloaded as saved as a CSV file on my laptop.

# Reading the data:
library(readr)
Cars_mileage <- read_csv("C:/My Future and Career/TAMU/Azavea/Cars_mileage.csv")

# Checking the head of the data and the data summary:
head(Cars_mileage)

summary(Cars_mileage)

# By looking at the head and summary, we see that a variable 'horsepower' is of class 'character'
# Coverting 'horsepower' into numerical:

is.numeric(Cars_mileage$horsepower)
Cars_mileage$horsepower= as.numeric(Cars_mileage$horsepower)
is.numeric(Cars_mileage$horsepower)
head(Cars_mileage$horsepower)

# Thus, 'horsepower' is successfully converted into numeric.

# (a) Create a binary variable that represents whether the car's mpg is above or below the 
# dataset's median. Above the median should be represented as 1. 
# Name this variable mpg_binary.

for (i in 1:nrow(Cars_mileage)){if (Cars_mileage$mpg[i]<=median(Cars_mileage$mpg)) {Cars_mileage$mpg_binary[i]=0} else {Cars_mileage$mpg_binary[i]=1}}
# There are many ways to create a variable 'mpg_binary'.  We can write ifelse statement to do the same.

head(Cars_mileage$mpg_binary)

typeof(Cars_mileage$mpg_binary)
# Converting mpg_binary into Factor
Cars_mileage$mpg_binary= as.factor(Cars_mileage$mpg_binary)
summary(Cars_mileage$mpg_binary)

# Since we are splitting at the median mpg value, there should be approximately the same number of 0 and 1. 
# But this is not happening.
nrow(Cars_mileage[which(Cars_mileage$mpg == median(Cars_mileage$mpg)), ])
# Here, we come to know that there are 9 cars which have mpg value equal to the median.


# [b] Which of the other variables seem most likely to be useful in predicting whether a car's mpg is above or below its median? 
# To answer this question, we will use t-test for Numerical Predictors and a Chi-Sq test for categorical predictors.

# We will consider Cylinder, Year and Origin as categorical predictors.
# Thus, converting them into Factors:
Cars_mileage$cylinders= as.factor(Cars_mileage$cylinders)
Cars_mileage$year= as.factor(Cars_mileage$year)
Cars_mileage$origin= as.factor(Cars_mileage$origin)

# Chi-Sq test of 'cylinders'
tbl1 = table(Cars_mileage$cylinders, Cars_mileage$mpg_binary) 
tbl1
chisq.test(tbl1) 
# Here, we can observe that p-value is less than 0.05, thus the predictor 'cylinders' is significant (as we reject null hypothesis)
# Note: There are some cells where the number of observations is less than 5, so to enhance the result, we can combine the cells row-wise or column-wise to increase the number of observations above 5 but the result would still remain same as p value is very much below 0.05

# Chi-Sq test of 'year'
tbl2 = table(Cars_mileage$year, Cars_mileage$mpg_binary) 
tbl2
chisq.test(tbl2) 
# Here, we can observe that p-value is less than 0.05, thus the predictor 'year' is significant (as we reject null hypothesis)

# Chi-Sq test of 'origin'
tbl3 = table(Cars_mileage$origin, Cars_mileage$mpg_binary) 
tbl3
chisq.test(tbl3) 
# Here, we can observe that p-value is less than 0.05, thus the predictor 'year' is significant (as we reject null hypothesis)

# t-test of 'displacement'
t.test(Cars_mileage$displacement ~ Cars_mileage$mpg_binary)
# Here, we can observe that p-value is less than 0.05, thus the predictor 'displacement' is significant (as we reject null hypothesis)

# t-test of 'horsepower'
t.test(Cars_mileage$horsepower ~ Cars_mileage$mpg_binary)
# Here, we can observe that p-value is less than 0.05, thus the predictor 'horsepower' is significant (as we reject null hypothesis)

# t-test of 'weight'
t.test(Cars_mileage$weight ~ Cars_mileage$mpg_binary)
# Here, we can observe that p-value is less than 0.05, thus the predictor 'weight' is significant (as we reject null hypothesis)

# t-test of 'acceleration'
t.test(Cars_mileage$acceleration ~ Cars_mileage$mpg_binary)
# Here, we can observe that p-value is less than 0.05, thus the predictor 'acceleration' is significant (as we reject null hypothesis)

# Visualising distributon of the Factor variables wrt mpg_binary
plot(Cars_mileage$origin,Cars_mileage$mpg_binary, xlab='Origin', ylab='MPG_Binary', col=c('blanchedalmond','azure1'))
mtext('Note: Broader the category, higher the proportion of the data in that category', side=1, line = 4)

plot(Cars_mileage$year,Cars_mileage$mpg_binary, xlab='Year', ylab='MPG_Binary', col=c('blanchedalmond','azure1'))
mtext('Note: Broader the category, higher the proportion of the data in that category', side=1, line = 4)

plot(Cars_mileage$cylinders,Cars_mileage$mpg_binary, xlab='Cylinders', ylab='MPG_Binary', col=c('blanchedalmond','azure1'))
mtext('Note: Broader the category, higher the proportion of the data in that category', side=1, line = 4)
# Note: You can create such beautiful plots using a function ggplot() also!

# Visualising distributon of the Numerical variables wrt mpg_binary
boxplot(displacement~mpg_binary,data=Cars_mileage,xlab="Mpg_Binary", ylab="Displacement", col=c('blanchedalmond','azure1'))
boxplot(horsepower~mpg_binary,data=Cars_mileage,xlab="Mpg_Binary", ylab="Horsepower", col=c('blanchedalmond','azure1'))
boxplot(weight~mpg_binary,data=Cars_mileage,xlab="Mpg_Binary", ylab="Weight", col=c('blanchedalmond','azure1'))
boxplot(displacement~mpg_binary,data=Cars_mileage,xlab="Mpg_Binary", ylab="Displacement", col=c('blanchedalmond','azure1'))

# Note: Looking at the box plots above, we see that Upper and Lower Quartiles are Not overlapping for all predictors.
# This corroborates the fact that all numerical predictors are significant!

# (c) Split the data into a training set and a test set.
# Dropping the rows with missing values
Cars_mileage=na.omit(Cars_mileage)

## We will consider 75% of the sample size into a training data and 25% into a testing data
smp_size <- floor(0.75 * nrow(Cars_mileage))

## setting a rondom seed to make the partition reproductible
set.seed(27)
train_ind <- sample(seq_len(nrow(Cars_mileage)), size = smp_size)

# We will convert some categorical variables to a datatype 'Factor':
Cars_mileage$cylinders=as.factor(Cars_mileage$cylinders)
Cars_mileage$year=as.factor(Cars_mileage$year)
Cars_mileage$origin=as.factor(Cars_mileage$origin)
Cars_mileage$mpg_binary=as.factor(Cars_mileage$mpg_binary)

#Now, defining train, test:
train <- Cars_mileage[train_ind, ]
test <- Cars_mileage[-train_ind, ]

nrow(train) #294
nrow(test) #98
nrow(Cars_mileage) #392


# Firstly, we will drop some columns which are of no use and would otherwise overfit the data
Cars_mileage <- subset(Cars_mileage, select = -c(mpg,name) )
dim(Cars_mileage)


###############Logistic Regression #################
# As the response variable is binary, I will definitely go for Logistic Regression.
# Logistic Regression converts the categorical variables into numerical in order to using a logit function
# So, here, we do not have to manually convert any data type.
# Fitting a Logistic Regression  model on the Train Data

glm.fit =glm ( mpg_binary~cylinders+displacement+horsepower+weight+acceleration+year+origin,
                 data=train , family = binomial )
summary(glm.fit)

#Making a prediction using the model on the test data
glm.probs= predict(glm.fit, test, type="response")

glm.pred=rep (0,98) # creating a vector with all 0
glm.pred[glm.probs >.5]=1 # converting those to 1 whose corresponding probability was predicted above 0.5
table (glm.pred , test$mpg_binary) # creating a confusion matrix
(48+44)/98 # [1] 0.9387755, so the accuracy is good enough!
# Thus, test error for Logistic Regression= 0.0612245 or 6.12%


############### Random Forest #################
# After trying Logistic Regression, I would like to apply Random Forest
# Because, it's one of my favourite method which can be used for both classificatoin as well as regression problems.
# At the value of mtry=2, we get highest accuracy ( I tried varying 'mtry' from 1 to 5)
# Random Forest
library ( randomForest)
set.seed(27)
# Training a model on train data
bag.mpg = randomForest(mpg_binary~., data=train, mtry =2, importance =TRUE)
bag.mpg
# Making the predictions on the test data using the model:
bag.pred = predict (bag.mpg, newdata = test)
table (bag.pred , test$mpg_binary) # creating a confusion matrix
(47+47)/98 # [1] 0.9591837 so the accuracy is good enough and is better than Logistic Regression!
importance(bag.mpg)
# when mtry=4, we get same overall accuracy as when mtry=2. But for mtry=2, True Negative Rate is 100%
# Thus, I would choose mtry=2
# Thus, test error for Random Forest= 0.0408163 or 4.08%

# Thus, we can observe that Random Forest performs better than Logistic Regression.


