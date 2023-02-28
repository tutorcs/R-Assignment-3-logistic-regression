# DUE: FEBRUARY, 8

# I BEG YOU, PAY ATTENTION WHEN READING THE TASKS
# I ALSO BEG YOU TO TRY RUNNING YOUR CODE BEFORE HAND-IN

###################### I #####################
# load data from log_reg_data.txt
# you are supposed to see 500x3 matrix with columns X1, X2, Y. The last one is binary

# 1. plot X1 and X2 against each other using different color depending on Y 
#    (it is better if you use ggplot for future plots)
library(ggplot2)

# 2. In logistic regression the hyperplane given by b0+b1*x1+...+bm*xm=0 is the hyperplane
#    which separates classes. Or mayb just a curved surface if you have terms which interact
# a) try to fit a glm object for just a linear combination of the valiables you have (that is, Y ~ X1 + X2).
#    Remember to use family="binomial". In this case, the model will try to separate classes using a straight line
#
# b) to the scatterplot of colored data points add the line which is supposed to separate them.
#    the line is given by equation b1*x1 + b2*x2 + b0 = 0. So you can express x2 via x1 for example
#    to turn it into a more familiar one, x2 = f(x1) and make a plot using lines() or abline()

# 3. This line isn't very good at separation, is it? Try adding more features, which use interactions between
#    X1 and X2. To avoid recreating them for every model, you can add them right to your data frame
data[['X1^2']] 
data[['X2^2']]
data[['X1*X2']]
data[['X1^3']]
data[['X1^2*X2']]
data[['X1*X2^2']]
data[['X2^3']] 
#    Split your data into train (75%) and validation (25%) sets.Try different combinations of these features, 
#    train several models and choose the best on its accuracy (= % of examples which it predicted correctly)
#    on the validation set.
#    For each combination of features that you try, store validation accuracy and make a plot of the separating curve
#    over the scatter plot with colored datapoints. Given the number of datapoints, curve won't be significantly different...
#    Unless you use too few variables.
#    Here is how you can make this plot:
# model - the glm you fit
cf <- model$coefficients
# use the coef names you actually have in your model or you'll get NA
# Here I assume that the model was using all the features
sep.curve <- function(x1, x2){
  (cf['`X1^3`']*x1^3 + cf['`X1^2*X2`']*x1^2*x1 + cf['`X1*X2^2`']*x1*x2^2 + cf['`X2^3`']*x2^3 +
     cf['`X1^2`']*x1^2 + cf['`X2^2`']*x2^2 + cf['`X1*X2`']*x1*x2 + cf['X1']*x1 + cf['X2']*x2 + cf['(Intercept)'])
}
# you can check if you're getting NAs
sep.curve(1,0.5)
# now, we want to make a curve using implicit equation: f(x1, x2) = 0
x1.plot <- seq(min(X1), max(X2), len = 200)
x2.plot <- seq(min(X2), max(X2), len = 200)
z <- outer(x1.plot, x2.plot, sep.curve)
cr <- contourLines(x1.plot, x2.plot, z, levels = 0)
# plot
ggplot() + geom_point(aes(x=X1, y=X2, col=Y)) + geom_path(aes(x=cr[[1]]$x, y=cr[[1]]$y))
# if you got cr=list(), it means f(x1, x2) is never 0 in your given range. So, all datapoints were classified to
# the same class

# if you are using not all features, formula will be written with quotes like this
model <- glm(Y ~ `X1^2` + `X1*X2`, data=data, family="binomial")

# 4. Which features uses the best model, chosen by accuracy on validation set?

########################### II #######################
# load breast cancer data
library(mclust)
data(wdbc)
# Target column is Diagnosis. You do not need ID column
y <- as.numeric(wdbc$Diagnosis=='M')
x <- wdbc[,-cbind(1,2)]
# 1. Select 20% of data into validation set randomly. The rest is training data.
#    What is the proportion of 1 and 0s in y for validation data? And for training?

# 2. Use prcomp() to obtain principal components of the TRAINING data. Don't forget to standard (scale&center it)
# a) How much variance is explained by 1st component?
# b) how many components are needed to explain 90% of variance in data?

# We also need to be able to transfer new data (for example, validation set) into our principal component space
# To do so, use predict, like:
pc.x.val <- predict(pc, x.val) # pc - result of prcomp()
# you can assert that pc$x == predict(pc, x.train)

# 3. Try fitting a glm model starting with 1st component and adding them one by one. For each number of components
#    find accuracies of predictions on validation set and train sets and store it in a dataframe. 
#    You may want to use a cycle instead of writing the same code 30 times
# this may also help
model <- glm(y.train ~ pc.x.train[, c(1:N)], family="binomial")
predict(model, newdata=pc.x.val[, c(1:N)], type="response")

# 4. Make a plot with 2 lines, where x-axis is number of components, y-axis is accuracy. 
#    The two lines are for accuracy on train and accuracy on validation. After how many components does validation
#    accuracy start to increase too slowly?
