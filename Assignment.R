# Load necessary libraries
library(plyr)
library(naniar)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(corrplot)
library(dplyr)
library(rpart)
library(rpart.plot)

# Read the dataset
dataset <- read.csv("train.csv", header = TRUE, stringsAsFactors = TRUE)

# Preview the dataset
head(dataset)
summary(dataset)

# Revalue the 'Dependents' column, combining '3+' into '3'
dataset$Dependents <- revalue(dataset$Dependents, c("3+"="3"))

# Count missing values in each column
tr <- read.csv(file="train.csv", na.strings=c("", "NA"), header=TRUE)
library(plyr)
tr$Dependents <- revalue(tr$Dependents, c("3+"="3"))
sapply(tr, function(x) sum(is.na(x)))

# Visualize missing data pattern
gg_miss_var(tr) +
  theme_minimal() +
  labs(title = "Missing Data Pattern",
       x = "Variables",
       y = "Number of Missing Values")


# Distribution of Loan Amount
hist(dataset$LoanAmount, 
     main="Histogram for Loan Amount", 
     xlab="Loan Amount", 
     border="blue", 
     col="maroon",
     xlim=c(0,700),
     breaks=20)
boxplot(dataset$LoanAmount, col='maroon', xlab = 'Loan Amount', main = 'Box Plot for Loan Amount')

# Distribution of Applicant Income
hist(dataset$ApplicantIncome, 
     main="Histogram for Applicant Income", 
     xlab="Income", 
     border="blue", 
     col="maroon",
     xlim=c(0,80000),
     breaks=50)
boxplot(dataset$ApplicantIncome, col='maroon', xlab = 'Applicant Income', main = 'Box Plot for Applicant Income')

# Density plot of Loan Amount by Education Level
ggplot(data=dataset, aes(x=LoanAmount, fill=Education)) +
  geom_density(alpha=0.6) +
  facet_grid(Education ~ .) +
  scale_fill_manual(values=c("Bachelor"="#1f78b4", "Master"="#33a02c", "Not Graduate"="#e31a1c")) +
  labs(title="Density Plot of Loan Amount by Education Level",
       subtitle="Comparison of Loan Amount Distributions across Different Education Levels",
       x="Loan Amount",
       y="Density",
       fill="Education Level") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face="bold", hjust=0.5, size=20, color="#4e4d4d"),
    plot.subtitle = element_text(hjust=0.5, size=14, color="#4e4d4d"),
    axis.title = element_text(face="bold", color="#4e4d4d"),
    axis.text = element_text(color="#4e4d4d"),
    legend.position = "bottom",
    legend.title = element_text(face="bold", size=12),
    legend.text = element_text(size=10)
  )

# Create individual bar plots for various features
custom_colors <- c("#66c2a5", "#fc8d62")

plot1 <- ggplot(data=dataset, aes(x=Gender, fill=Loan_Status)) +
  geom_bar(position="dodge") +
  scale_fill_manual(values=custom_colors) +
  labs(title="Loan Status by Gender", x="Gender", y="Count") +
  theme_minimal()

plot2 <- ggplot(data=dataset, aes(x=Education, fill=Loan_Status)) +
  geom_bar(position="dodge") +
  scale_fill_manual(values=custom_colors) +
  labs(title="Loan Status by Education", x="Education", y="Count") +
  theme_minimal()

plot3 <- ggplot(data=dataset, aes(x=Married, fill=Loan_Status)) +
  geom_bar(position="dodge") +
  scale_fill_manual(values=custom_colors) +
  labs(title="Loan Status by Married", x="Married", y="Count") +
  theme_minimal()

plot4 <- ggplot(data=dataset, aes(x=Self_Employed, fill=Loan_Status)) +
  geom_bar(position="dodge") +
  scale_fill_manual(values=custom_colors) +
  labs(title="Loan Status by Self Employed", x="Self Employed", y="Count") +
  theme_minimal()

plot5 <- ggplot(data=dataset, aes(x=Property_Area, fill=Loan_Status)) +
  geom_bar(position="dodge") +
  scale_fill_manual(values=custom_colors) +
  labs(title="Loan Status by Property Area", x="Property Area", y="Count") +
  theme_minimal()

plot6 <- ggplot(data=dataset, aes(x=Credit_History, fill=Loan_Status)) +
  geom_bar(position="dodge") +
  scale_fill_manual(values=custom_colors) +
  labs(title="Loan Status by Credit History", x="Credit History", y="Count") +
  theme_minimal()

# Arrange the plots in a grid
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, ncol=3)

# Correlation heatmap
correlation_matrix <- cor(dataset[, sapply(dataset, is.numeric)], use="pairwise.complete.obs")

corrplot(correlation_matrix, method="color", type="lower", 
         tl.col="black", tl.srt=45, 
         col=brewer.pal(n=8, name="RdYlBu"))

# Data cleaning: remove 'Loan_ID' and convert 'Credit_History' to factor
loan_clean <- dataset %>% 
  select(-c(Loan_ID)) %>% 
  mutate(Credit_History = as.factor(Credit_History))

# Count missing values after cleaning
colSums(is.na(loan_clean))

# Filter out incomplete cases
loan_clean <- loan_clean %>% 
  filter(complete.cases(.)) 
colSums(is.na(loan_clean))

# Log transformation of Loan Amount
tr <- loan_clean
tr$LogLoanAmount <- log(tr$LoanAmount)
par(mfrow=c(1,2))
hist(tr$LogLoanAmount,
     main="Histogram for Loan Amount",
     xlab="Loan Amount",
     border="blue",
     col="maroon",
     las=1,
     breaks=20, prob = TRUE)
lines(density(tr$LogLoanAmount), col='black', lwd=3)
boxplot(tr$LogLoanAmount, col='maroon', xlab = 'Income', main = 'Box Plot for Applicant Amount')

# Combine incomes and remove original income columns
tr$Income <- tr$ApplicantIncome + tr$CoapplicantIncome
tr$ApplicantIncome <- NULL
tr$CoapplicantIncome <- NULL

# Log transformation of combined Income
tr$LogIncome <- log(tr$Income)
par(mfrow=c(1,2))
hist(tr$LogIncome,
     main="Histogram for Applicant Income",
     xlab="Income",
     border="blue",
     col="maroon",
     las=1,
     breaks=50, prob = TRUE)
lines(density(tr$LogIncome), col='black', lwd=3)
boxplot(tr$LogIncome, col='maroon', xlab = 'Income', main = 'Box Plot for applicant Income')

# Train-test split
set.seed(42)
sample <- sample.int(n = nrow(tr), size = floor(.70*nrow(tr)), replace = FALSE)
trainnew <- tr[sample, ]
testnew  <- tr[-sample, ]

# Logistic regression model
logistic1 <- glm (Loan_Status ~ Credit_History, data = trainnew, family = binomial)

# Predictions on training set
my_prediction_tr1 <- predict(logistic1, newdata = trainnew, type = "response")
table(trainnew$Loan_Status, my_prediction_tr1 > 0.5)

# Predictions on test set
logistic_test1 <- glm (Loan_Status ~ Credit_History, data = testnew, family = binomial)
my_prediction_te1 <- predict(logistic_test1, newdata = testnew, type = "response")
table(testnew$Loan_Status, my_prediction_te1 > 0.5)



# Load required libraries
library(rpart)
library(rpart.plot)

# Grow tree on training data
dtree <- rpart(Loan_Status ~ Credit_History + Education + Self_Employed + Property_Area + LogLoanAmount +
                 LogIncome, method = "class", data = trainnew, parms = list(split = "information"))

# Display complexity table for the grown tree
dtree$cptable

# Prune the decision tree using the specified complexity parameter
dtree.pruned <- prune(dtree, cp = 0.02290076)

# Plot the pruned decision tree
prp(dtree.pruned, type = 2, extra = 104,
    fallen.leaves = TRUE, main = "Pruned Decision Tree")

# Make predictions on the training data using the pruned tree
dtree.pred <- predict(dtree.pruned, trainnew, type = "class")

# Create a confusion matrix for the training predictions
dtree.perf <- table(trainnew$Loan_Status, dtree.pred,
                    dnn = c("Actual", "Predicted"))
dtree.perf

# Grow tree on test data (not pruned)
dtree_test <- rpart(Loan_Status ~ Credit_History + Education + Self_Employed + Property_Area + LogLoanAmount +
                      LogIncome, method = "class", data = testnew, parms = list(split = "information"))

# Display complexity table for the tree grown on test data
dtree_test$cptable

# Plot the complexity parameter plot for the test tree
plotcp(dtree_test)

# Prune the decision tree on test data using a specific complexity parameter
dtree_test.pruned <- prune(dtree_test, cp = 0.01639344)

# Plot the pruned decision tree for the test data
prp(dtree_test.pruned, type = 2, extra = 104,
    fallen.leaves = TRUE, main = "Pruned Decision Tree (Test Data)")

# Make predictions on the test data using the pruned tree
dtree_test.pred <- predict(dtree_test.pruned, testnew, type = "class")

# Create a confusion matrix for the test predictions
dtree_test.perf <- table(testnew$Loan_Status, dtree_test.pred,
                         dnn = c("Actual", "Predicted"))
dtree_test.perf


# Calculate accuracy for the training data
train_accuracy <- sum(diag(dtree.perf)) / sum(dtree.perf)
train_accuracy

# Calculate accuracy for the test data
test_accuracy <- sum(diag(dtree_test.perf)) / sum(dtree_test.perf)
test_accuracy

# Fit logistic regression model logistic1
logistic1 <- glm(Loan_Status ~ Credit_History, data = trainnew, family = binomial)
# Predictions on training set using logistic1
my_prediction_tr1 <- predict(logistic1, newdata = trainnew, type = "response")

# Create a confusion matrix for training predictions
train_confusion_log1 <- table(trainnew$Loan_Status, my_prediction_tr1 > 0.5)
train_accuracy_log1 <- sum(diag(train_confusion_log1)) / sum(train_confusion_log1)
# Predictions on test set using logistic1
my_prediction_te1 <- predict(logistic1, newdata = testnew, type = "response")

# Create a confusion matrix for test predictions
test_confusion_log1 <- table(testnew$Loan_Status, my_prediction_te1 > 0.5)
test_accuracy_log1 <- sum(diag(test_confusion_log1)) / sum(test_confusion_log1)
cat("Logistic Regression Training Accuracy:", train_accuracy_log1, "\n")
cat("Logistic Regression Test Accuracy:", test_accuracy_log1, "\n")



# Assuming you have already calculated these accuracies
train_accuracy_log1 <- 0.8135135   
test_accuracy_log1 <- 0.8176101 

train_accuracy_tree <-  0.8135135  
test_accuracy_tree <- 0.8616352

# Create a data frame for accuracy comparison
accuracy_data <- data.frame(
  Model = c("Logistic Regression", "Logistic Regression", "Decision Tree", "Decision Tree"),
  Dataset = c("Training", "Test", "Training", "Test"),
  Accuracy = c(train_accuracy_log1, test_accuracy_log1, train_accuracy_tree, test_accuracy_tree)
)

# Load ggplot2 library if not loaded
library(ggplot2)

# Create bar graph
ggplot(accuracy_data, aes(x=Model, y=Accuracy, fill=Dataset)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_y_continuous(labels = scales::percent, limits=c(0, 1)) +  # Display y-axis as percentage with limit up to 90%
  labs(title="Prediction Accuracy by Model and Dataset",
       x="Model",
       y="Accuracy") +
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5, size=16, face="bold"),
        axis.title = element_text(size=12),
        axis.text = element_text(size=10),
        legend.title = element_text(size=12),
        legend.text = element_text(size=10))































