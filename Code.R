# Loading Required Libraries 
load.libraries <- c('gridExtra', 'corrplot', 'cluster', 'ggplot2', 'GGally', 'mice', 'ggfortify', 'stargazer',
                    'tree', 'rpart.plot', 'randomForest', 'gbm', 'dplyr', 'caret', 'reshape2', 'sjPlot')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]
for(libs in install.lib) install.packages(libs, dependences = TRUE)
sapply(load.libraries, require, character = TRUE)

df = read.csv('/Users/mohamaddalati/Desktop/MGSC-661/Final Project/Dataset 5 — Automobile data.csv')
attach(df)

summary(df)

# Data Pre-processing 
# confirming missing values 
colSums(is.na(df)) # no missing values #BEAUTIFULLLL!

# There are no missing values in the data but ? 
colSums(df == '?') # numofdoors, stroke, horsepower, peak, price have ? 

# There are no missing values, Only "?" entries that we have to deal with.
# Only 1 categorical variable (num of doors) has '?' and the rest are numerical variables.  

# Since the number of observations are not high, I will try to avoid dropping the "?" and fill them 
# with a relative value instead. 

# Categorical variable aka number of doors variable  check 
df$num.of.doors = as.factor(df$num.of.doors)
attach(df)
levels(df$num.of.doors)
# df %>% filter(num.of.doors == "?") %>% select(make, body.style)
table(df$make,df$num.of.doors) # mazda and dodge are the ones with the ? 
table(df$body.style,df$num.of.doors) # it's also coming from their sedan styles 
# So for the # of doors variables, both of them are coming from sedan cars of brands mazda and dodge 

red.bold.italic.text <- element_text(face = "bold.italic", color = "red")

plot = ggplot(df, aes(x = num.of.doors))
plot + geom_bar() +  labs(title = "Quantity vs. Number of Doors", x = 'Number of Doors', y = 'Quantity')+
  theme(title = red.bold.italic.text, axis.title = red.bold.italic.text) + 
  theme(plot.title=element_text(hjust=0.5)) 
# bar chart 
# majority of the cars have 4 doors , but that's not enough

# Data Visualization 
red.bold.italic.text <- element_text(face = "bold.italic", color = "red")
# Check connection between body style vs. number of doors 
plot = ggplot(df, aes(x = num.of.doors, y=body.style))
plot + geom_jitter( aes(color=make) ) +
  labs(title = "Style vs. Number of Doors", x = 'Number of Doors', y = 'Style') +
  theme(title = red.bold.italic.text, axis.title = red.bold.italic.text) + 
  theme(plot.title=element_text(hjust=0.5)) + guides(color=guide_legend(title = 'Brands'))

# To exclusively compare between dodge and mazda vehicle make
ggplot(filter(df,make=="mazda" | make=="dodge"), aes(num.of.doors, body.style)) + 
  geom_jitter(aes(color=make), size = 3) + 
  labs(title = "Mazda and Dodge", x = 'Number of Doors', y = 'Vehicle Style') +
  theme(title = red.bold.italic.text, axis.title = red.bold.italic.text) + 
  theme(plot.title=element_text(hjust=0.5)) + guides(color=guide_legend(title = 'Brands'))
# Looks like most sedans are actually with four doors,
# therefore, I will assign 'four' to the two missing '?' entries

df$num.of.doors[df$num.of.doors == '?'] <- 'four'
attach(df)
levels(num.of.doors)


colSums(df == '?') # num of doors shows 0 for '?" entries
summary(df)

# For numerical variables, I will use MICE package to fill them. 

# Numerical Variables Check 
# Set all variables with '?' to NA 
df$bore[df$normalized.losses == "?"] <- NA
df$bore[df$bore == "?"] <- NA
df$stroke[df$stroke == "?"] <- NA
df$horsepower[df$horsepower == "?"] <- NA
df$peak.rpm[df$peak.rpm == "?"] <- NA
df$price[df$price == "?"] <- NA
attach(df)
# Check before converting 
colSums(df == '?') # good 

#convert to numerics
df$normalized.losses<-as.numeric(as.character(df$normalized.losses))
df$bore<-as.numeric(as.character(df$bore))
df$stroke<-as.numeric(as.character(df$stroke))
df$horsepower<-as.numeric(as.character(df$horsepower))
df$peak.rpm<-as.numeric(as.character(df$peak.rpm))
df$price<-as.numeric(as.character(df$price))
summary(df)
attach(df)



# MICE (Multivariate Imputation via Chained Equations)
# https://datascienceplus.com/handling-missing-data-with-mice-package-a-simple-approach/
# Remove categorial variables to allow mice package figure a pattern for numerical ones to be filled 
df.num <- df[sapply(df, is.numeric)]
md.pattern(df.num)
tempData <- mice(df.num, m=1, maxit=5, meth='pmm', seed=500)
summary(tempData)

# Let's check the modified observations
tempData$imp$horsepower
tempData$imp$stroke
tempData$imp$bore
tempData$imp$peak.rpm

xyplot(tempData, horsepower ~ stroke + bore + peak.rpm,pch=18,cex=1)

df.num = complete(tempData, 1)

df.non_num <- df[, -which(names(df) %in% names(df.num))]
df <- cbind(df.num, df.non_num)

# Final check on missing entries
sapply(df, function(x) sum(is.na(x)))
sapply(df, function(x) sum(df == "?"))
# all missing entries have been filled adequately 

# Identify categorical variables 
df$symboling = as.factor(df$symboling)
df$make = as.factor(df$make)
df$fuel.type = as.factor(df$fuel.type)
df$aspiration = as.factor(df$aspiration)
df$num.of.doors = as.factor(df$num.of.doors)
df$fuel.system = as.factor(df$fuel.system)
df$body.style = as.factor(df$body.style)
attach(df)
summary(df)

# Model checks:
# Hetero - No need since it's a classification model

# Outliers 
boxplot(price) # Natural outliers 
boxplot(normalized.losses) # Same
boxplot(curb.weight) # No outliers 
boxplot(wheel.base) # not significant 
boxplot(length) # clear 
boxplot(width) # not significant 
boxplot(height) # clear 
boxplot(engine.size) #

boxplot(stroke)
# The most common designs for engines are two-stroke and four-stroke. 
# Less common designs include five-stroke engines, six-stroke engines and two-and-four stroke engines
# Classification models like decision trees and random forests, are less sensitive to outliers and may not require explicit outlier detection and removal.
# Therefore, I'll simply train the model on the full dataset and let the model itself identify and handle any outliers


# Correlation check 
# Correlation refers to the relationship between two continuous variables, such as the relationship between height and weight. In classification models, the dependent variable is a binary 
# or categorical variable, and the predictor variables can be continuous or categorical. So there is no need to check that. 
# 

# We want to classify the risk factor of cars 
# so our target variable (Y) is symboling, let's rename it to risk factor 
names(df)[names(df) == "symboling"] <- "risk_factor"
attach(df)

df$risk_factor = ifelse(risk_factor == '-2' | risk_factor == '-1' | risk_factor == '0', 0, 1) # 0 represent low risk, 1 represent high risk
df$risk_factor  = as.factor(df$risk_factor)
attach(df)
levels(risk_factor)
table(risk_factor)

##### Done with Data Pre-processing 
legend_title = 'Brand'
plot = ggplot(df, aes(x = reorder(make, make, 
                                  function(x)+length(x))))
plot + geom_bar(fill ='lightblue') +
  labs(title = "Number of Vehicles vs. Brand", x = 'Vehicle brand', y = 'Number of Vehicles') + 
  theme(title = red.bold.italic.text, axis.title = red.bold.italic.text) + 
  theme(plot.title=element_text(hjust=0.5)) + coord_flip() 

####  Data Visualization 
red.bold.italic.text <- element_text(face = "bold.italic", color = "red")

# Check connection between body style vs. number of doors 
plot = ggplot(df, aes(x = num.of.doors, y=body.style))
plot + geom_jitter( aes(color=make) ) +
  labs(title = "Style vs. Number of Doors", x = 'Number of Doors', y = 'Style') +
  theme(title = red.bold.italic.text, axis.title = red.bold.italic.text) + 
  theme(plot.title=element_text(hjust=0.5)) + guides(color=guide_legend(title = 'Brands'))

# cars with 
plot(~df$price+horsepower,ylab="Horsepower",xlab="Price",main="Plot between Price and Horsepower",
     cex=0.8,cex.lab=0.8,cex.main=1.2,pch=20,col.lab="red",col="orange",col.main="red")
abline(lm(df$horsepower~df$price))


ggplot(df,  aes(x=risk_factor, y=height)) +geom_boxplot(outlier.shape = NA) +labs(title = "Height vs. Risk Factor", x = 'Risk', y = 'Height') +
  theme(title = red.bold.italic.text, axis.title = red.bold.italic.text) + 
  theme(plot.title=element_text(hjust=0.5))
ggplot(df, aes(x=risk_factor, y=height))+geom_boxplot(outlier.shape = NA) 
# Shorter cars have higher risk rating. Since they are more like to be sport cars, and more vulnerable in terms it made an accident 


summary(df)

# MODELLING 

#####  Random Forest Classifier 
# I will start with random forest classifier to get the feature importance 
# Random Forest 
classifiedforest=randomForest(risk_factor~normalized.losses + wheel.base+ length + width + 
                                height + curb.weight + engine.size + bore + stroke + compression.ratio
                              + horsepower + peak.rpm + city.mpg + highway.mpg + price + make + fuel.type+
                                aspiration + num.of.doors + body.style + drive.wheels + engine.location + 
                                engine.type + num.of.cylinders + fuel.system, ntree = 500, data = df, 
                              importance = TRUE)
classifiedforest
# find the best feature importance 
# As a brief guideline or rough estimation to help us perform the logistic regression 
importance(classifiedforest) 
stargazer(importance(classifiedforest), type = 'html' )

#### To plot the important variables PRELIMINARY
varImpPlot(classifiedforest)
# RHS Gini captures the purity of the nodes at the end of the tree 
# make scored the highest contribution of purity 



##### Multiple Logistic Regression (MLR) 
df2 = df
attach(df2)

# Calculate the importance of each feature in the random forest model

mlogit = glm(risk_factor ~ make + normalized.losses + wheel.base + width + height + num.of.doors + curb.weight + length + price , family = "binomial")
# since it's showing an error fitting the model, I will do it on a small set just to read the relationship.


mlogit1 = glm(risk_factor ~ make + normalized.losses, family = "binomial") 
summary(mlogit1)

mlogit2 = glm(risk_factor ~ wheel.base + width, family = "binomial", data = df2)# 
summary(mlogit2)


mlogit3 = glm(risk_factor ~ height + num.of.doors , family = "binomial", data = df2)# 
summary(mlogit3)

mlogit4 = glm(risk_factor ~ curb.weight + length + price , family = "binomial", data = df2)# 
summary(mlogit4)



stargazer(mlogit1, type = 'html')
stargazer(mlogit2, type = 'html')
stargazer(mlogit3, type = 'html')
stargazer(mlogit4, type = 'html')

##### Interpretation 


##### Model Viz
#### Classification Tree 
fittedtree=rpart(risk_factor ~  make + normalized.losses + wheel.base + width + height + 
                   num.of.doors + curb.weight + length + price ,control=rpart.control(cp=0.01))

rpart.plot(fittedtree)

# To find the relative error 
printcp(fittedtree)
plotcp(fittedtree)
# Optimal value can't be found since this is the CP determines a split based on the RSS improvement, but as a 
# classification model, there is no RSS 



###### Random Forest Model Tuning
## find the optimal number of trees (ntree)
ntreeSelection = randomForest(risk_factor ~ ., ntree = 500, data = df, importance = TRUE)
ntreeSelection
plot(ntreeSelection)
# The OOB error rate seems constant at 400 trees 

# Tuning Number of variables tried at each split (mtry) 
X = df[ , -1]
y = df[,1]
t <- tuneRF(X, y, 
       stepFactor = 0.5,
       plot = TRUE,
       ntreeTry = 400,
       trace = TRUE,
       improve =  0.05)   # relative improvement in OOB for search to continue, similar to LEARNING RATE
# We can see how the lowest OOB error is = mtry of 10, therefore, let's adjust that 

rf <- randomForest(risk_factor~., data = df, 
                   ntree = 400, 
                   mtry = 10,
                   importance = TRUE, 
                   proximity = TRUE)

rf

# Visualize feature importance using ggplot 
importances <- RFmodel$importance


# Tuning Number of variables tried at each split (mtry) 

X_top = df[ , c(2:7,17,20)]
y = df[,1]
t <- tuneRF(X_top, y, 
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 400,
            trace = TRUE,
            improve =  0.05)
rf


# Random Forest Model
model = randomForest(x = X, y = y, ntree = 400, mtry = 2)

RFmodel1 = randomForest(risk_factor ~ make + normalized.losses + wheel.base + width +
                          height + num.of.doors + length + curb.weight + body.style + bore, ntree = 400, mtry=2, data = df, importance = TRUE)

RFmodel1 # 7.32% 


RFmodel2 = randomForest(risk_factor ~ make + normalized.losses + wheel.base + width +
                          height + num.of.doors + length + curb.weight + body.style, ntree = 400, mtry=2, data = df, importance = TRUE)

RFmodel2 # 7.8% 

RFmodel3 = randomForest(risk_factor ~ make + normalized.losses + wheel.base + width +
                          height + num.of.doors + length + curb.weight, ntree = 400, mtry=2, data = df, importance = TRUE)

RFmodel3 # 8.78%

### For tuning the selected variables since this variable of high criticality 
set.seed(123) # to make it repeatable
test1 = sample(2, nrow(df), replace = TRUE, prob = c(0.7, 0.3))
train = df[test1 == 1, ] # 70% training set
test = df[test1 == 2, ] # 30% test set 

# RF 
set.seed(222)
rf <- randomForest(risk_factor~., data = train)



# Visualize the confusion matrix using ggplot
# Diagonals in the Confusion matrix represent the correct predictions 
# Almost 23 predictions were misclassified. for example: 3 observations that should have been classified as risk factor of 1, was classified as risk factor of 3 instead.
# I will use 3=500 trees with an accuracy rate of (100-OOB, 100-12.68 = 87.32%)
predictions <- predict(RFmodel1, data = df)

# Create a confusion matrix
cm <- table(predicted = predictions, actual = df$risk_factor)
# Plot Confusion Matrix 
library(reshape2)
ggplot(melt(cm), aes(x = actual, y = predicted)) +
  geom_tile(aes(fill = value)) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(x = "Actual Risk Factors", y = "Predicted Risk Factors")


hist(treesize(RFmodel1),
     main = "No. of Nodes for the Trees", 
     col = "green")
# Distribution of # of nodes in each of the 400 trees. 
# Majority of the trees have about 30-35 nodes in them

install.packages("yardstick")
library(ggplot2)
library(yardstick)

# The confusion matrix from a single assessment set (i.e. fold)
cm <- conf_mat(predictions, actual = df$risk_factor )

set.seed(1)
head(df)



# Predictions 
# For predictions we need to dummy-code the categorical variables with many levels (i.e. make, and num of doors )
Preddf = df
Preddf$isaudi = ifelse(make == 'audi', 1, 0)
Preddf$isromero = ifelse(make == 'alfa-romero', 1, 0)
Preddf$isbmw = ifelse(make == 'bmw', 1, 0)
Preddf$ischevrolet = ifelse(make == 'chevrolet', 1, 0)
Preddf$isdodge = ifelse(make == 'dodge', 1, 0)
Preddf$ishonda = ifelse(make == 'honda', 1, 0)
Preddf$isisuzu = ifelse(make == 'isuzu', 1, 0)
Preddf$isjaguar = ifelse(make == 'jaguar', 1, 0)
Preddf$ismazda = ifelse(make == 'mazda', 1, 0)
Preddf$isbenz = ifelse(make == 'benz', 1, 0)
Preddf$ismercury = ifelse(make == 'mercury', 1, 0)
Preddf$ismitsubishi = ifelse(make == 'mitsubishi', 1, 0)
Preddf$isnissan = ifelse(make == 'nissan', 1, 0)
Preddf$ispeugot = ifelse(make == 'peugot', 1, 0)
Preddf$isplymouth = ifelse(make == 'plymouth', 1, 0)
Preddf$isporsche = ifelse(make == 'porsche', 1, 0)
Preddf$isrenault = ifelse(make == 'renault', 1, 0)
Preddf$issaab = ifelse(make == 'saab', 1, 0)
Preddf$issubaru = ifelse(make == 'subaru', 1, 0)
Preddf$istoyota = ifelse(make == 'toyota', 1, 0)
Preddf$isvolkswagen = ifelse(make == 'volkswagen', 1, 0)
Preddf$isvolvo = ifelse(make == 'volvo', 1, 0)
attach(Preddf)


predict(___, data.frame(normalized.losses = 1550 , wheel.base = 61, width = 20, height = 55.5, make = audi, length = 10, curb.weight = 1820, price = 30000), data = Preddf)


#### 
predict(cf5, data.frame(isaudi = 1, normalized.losses = 1550 , wheel.base = 61, width = 20, height = 55.5, length = 10, curb.weight = 1820, price = 30000))
table(price)

predict(cf5,data.frame(make='mazda',normalized.losses=1340.0,wheel.base=60.5,width=70.3,height=55.5,num.of.doors='four', data = Preddf))







# ### QDA 
# library(MASS)
# library(klaR)
# partimat(risk_factor~make + normalized.losses + wheel.base + width + height + num.of.doors, method="qda",image.colors=c("white", "light green", "light blue"), data = df)
# 
# # Probability 
# hists=ggplot(df, aes(x=price))+geom_histogram(bins = 50, fill ='green')+facet_grid(risk_factor)+labs(title = "Frequency vs. Price", x = 'Price', y = 'Count') 
# hists
# 
# myqda = qda(risk_factor~make + normalized.losses + wheel.base + width + height + num.of.doors, data = df)
# myqda
# 
# # C) Plot the Classification regions 
# partimat(relationshipType~age+swipes, method = 'qda', image.colors = 
#            c('light grey', 'light green', 'purple', 'light blue'), 
#          data = relationship)





# Check the relationship between price and risk 
plot = ggplot(df, aes(y=risk_factor, x=price))
scatter=geom_point()
plot+scatter+ labs(title = "Price vs. Risk", x = 'Price', y = 'Risk Factor') +
  theme(title = red.bold.italic.text, axis.title = red.bold.italic.text) + 
  theme(plot.title=element_text(hjust=0.5))
# A lot of cheap cars are considered among the high risk factor, which explains why price was not among the important predictors! 



# Get some ideas 
ggplot(df, aes(x=risk_factor, y=price))+geom_boxplot()+ labs(title = "Price vs. Risk Factor", x = 'Price', y = 'Risk Factor') +
  theme(title = red.bold.italic.text, axis.title = red.bold.italic.text) + 
  theme(plot.title=element_text(hjust=0.5))
# surprised that even the most expensive cars are not the safest! 

# 75% of the observations showing the expensive cars showed are actually in the low risk factor!


#########    PCA 
# Separating categorical and numerical predictors into separate datasets
dfpca = df
attach(dfpca)

df_labels = dfpca[ , c(1)] # only contains the target 
df_vars = dfpca[ , c(2:7,16)] 

pca = prcomp(df_vars, scale=TRUE)
pca

library(ggplot2)
library(GGally)
autoplot(pca, data = df_vars, loadings = TRUE, col = ifelse(dfpca$risk_factor == '0', 'blue', 'green'), loadings.label = TRUE)
# legend("topright", legend = c("Low Risk", "1"), col = c("blue", "green"), pch = 1)
#### ^I'm trying to add a legend but it is not appearing!!!
# blue for low risk , green for high risk


######Percentage of variance explained (PVE) plot
# To find the optimal number of PC 
pve=(pca$sdev^2)/sum(pca$sdev^2)
par(mfrow=c(1,2))
plot(pve, ylim=c(0,1))
plot(cumsum(pve), ylim=c(0,1))
# The first PC always captures the highest PVE!!
# X-axis is the number of PC 




# Additional GENERAL PCA 
# legend("topright", legend = c("Low Risk", "1"), col = c("blue", "green"), pch = 1)
#### ^I'm trying to add a legend but it is not appearing!!!
# blue for low risk , green for high risk
dfpca2 = df
attach(dfpca2)

df_labels2 = dfpca2[ , c(1)] # only contains the target 
df_vars2 <- select(df, normalized.losses, wheel.base, length, width, height, curb.weight, engine.size, horsepower, peak.rpm, city.mpg,
                   highway.mpg, price)

pca2 = prcomp(df_vars2, scale=TRUE)
pca2

autoplot(pca2, data = df_vars, loadings = TRUE, col = ifelse(dfpca$risk_factor == '0', 'blue', 'green'), 
         loadings.label = TRUE, legend = c("Low Risk", "High Risk"), title = "PCA Plot", title.position = "top", 
         title.theme = element_text(face = "bold"), key.theme = element_rect(fill = "white", color = "black"))

# Results are shown in the report 




