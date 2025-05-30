# Multiple Linear Regression Model
# Make Predictions for each department for May, June, and July

setwd('/Users/RafikaMomin/Desktop') 
library(tseries)
library(forecast)

# Nov 2004 - May 2020
matrix <- read.csv("Matrix.csv", header = F, sep=",")
nrow(matrix)
ncol(matrix)

# V1 - V7: Time,Labels,Placarding Systems,Placards,Packaging,Books,Software
# Jan 2005 - June 2020
sales <- read.csv("Sales.csv", header = F, sep=",")
nrow(sales)
ncol(sales)

install.packages("carData")
library(car)

######Predictions for May #####
# Labels:
label_may_sales <- sales[1:184,2]
may_matrix <- matrix[2:185,]
may_matrix$labelsales <- label_may_sales

nrow(may_matrix)
nrow(may_sales)

labels_fit <- lm(labelsales ~ ., data = may_matrix, singular.ok = TRUE)
# vif(labels_fit) # Variance Inflation Factor
predict(labels_fit, newdata = matrix[c(185),] )

# Placarding Systems:
ps_may_sales <- sales[1:184,3]
may_matrix <- matrix[2:185,]
may_matrix$pssales <- ps_may_sales

nrow(may_matrix)
nrow(may_sales)

ps_fit <- lm(pssales ~ ., data = may_matrix, singular.ok = TRUE)
# vif(labels_fit) # Variance Inflation Factor
predict(ps_fit, newdata = matrix[c(185),] )

# Placards:
p_may_sales <- sales[1:184,4]
may_matrix <- matrix[2:185,]
may_matrix$psales <- p_may_sales

nrow(may_matrix)
nrow(may_sales)

p_fit <- lm(psales ~ ., data = may_matrix, singular.ok = TRUE)
# vif(labels_fit) # Variance Inflation Factor
predict(p_fit, newdata = matrix[c(185),] )

# Packaging
pack_may_sales <- sales[1:184,5]
may_matrix <- matrix[2:185,]
may_matrix$packsales <- pack_may_sales

nrow(may_matrix)
nrow(may_sales)

pack_fit <- lm(packsales ~ ., data = may_matrix, singular.ok = TRUE)
# vif(labels_fit) # Variance Inflation Factor
predict(pack_fit, newdata = matrix[c(185),] )

# Books:
book_may_sales <- sales[1:184,6]
may_matrix <- matrix[2:185,]
may_matrix$booksales <- book_may_sales

nrow(may_matrix)
nrow(may_sales)

book_fit <- lm(booksales ~ ., data = may_matrix, singular.ok = TRUE)
# vif(labels_fit) # Variance Inflation Factor
predict(book_fit, newdata = matrix[c(185),] )

# Software: 
soft_may_sales <- sales[1:184,7]
may_matrix <- matrix[2:185,]
may_matrix$softsales <- soft_may_sales

nrow(may_matrix)
nrow(may_sales)

soft_fit <- lm(softsales ~ ., data = may_matrix, singular.ok = TRUE)
# vif(labels_fit) # Variance Inflation Factor
predict(soft_fit, newdata = matrix[c(185),] )

######Predictions for June #####
# Labels:
label_june_sales <- sales[1:185,2]
june_matrix <- matrix[2:186,]
june_matrix$labelsales <- label_june_sales

nrow(june_matrix)

labels_fit <- lm(labelsales ~ ., data = june_matrix, singular.ok = TRUE)
# vif(labels_fit) # Variance Inflation Factor
predict(labels_fit, newdata = matrix[c(186),] )

# Placarding Systems:
ps_june_sales <- sales[1:185,3]
june_matrix <- matrix[2:186,]
june_matrix$pssales <- ps_june_sales

nrow(june_matrix)

ps_fit <- lm(pssales ~ ., data = june_matrix, singular.ok = TRUE)
# vif(labels_fit) # Variance Inflation Factor
predict(ps_fit, newdata = matrix[c(186),] )

# Placards
plac_june_sales <- sales[1:185,4]
june_matrix <- matrix[2:186,]
june_matrix$placsales <- plac_june_sales

nrow(june_matrix)

plac_fit <- lm(placsales ~ ., data = june_matrix, singular.ok = TRUE)
# vif(labels_fit) # Variance Inflation Factor
predict(plac_fit, newdata = matrix[c(186),] )

# Packaging 
pack_june_sales <- sales[1:185,5]
june_matrix <- matrix[2:186,]
june_matrix$packsales <- pack_june_sales

nrow(june_matrix)

pack_fit <- lm(packsales ~ ., data = june_matrix, singular.ok = TRUE)
# vif(labels_fit) # Variance Inflation Factor
predict(pack_fit, newdata = matrix[c(186),] )

# Books
book_june_sales <- sales[1:185,6]
june_matrix <- matrix[2:186,]
june_matrix$booksales <- book_june_sales

nrow(june_matrix)

book_fit <- lm(booksales ~ ., data = june_matrix, singular.ok = TRUE)
# vif(labels_fit) # Variance Inflation Factor
predict(book_fit, newdata = matrix[c(186),] )

# Software
soft_june_sales <- sales[1:185,7]
june_matrix <- matrix[2:186,]
june_matrix$softsales <- soft_june_sales

nrow(june_matrix)

soft_fit <- lm(softsales ~ ., data = june_matrix, singular.ok = TRUE)
# vif(labels_fit) # Variance Inflation Factor
predict(soft_fit, newdata = matrix[c(186),] )

######Predictions for July #####
# Labels 
label_july_sales <- sales[1:186,2]
july_matrix <- matrix[1:186,]
july_matrix$labelsales <- label_july_sales

nrow(july_matrix)

labels_fit <- lm(labelsales ~ ., data = july_matrix, singular.ok = TRUE)
# vif(labels_fit) # Variance Inflation Factor
predict(labels_fit, newdata = matrix[c(187),] )

# Placarding Systems
plac_july_sales <- sales[1:186,3]
july_matrix <- matrix[1:186,]
july_matrix$placsales <- plac_july_sales

nrow(july_matrix)

plac_fit <- lm(placsales ~ ., data = july_matrix, singular.ok = TRUE)
# vif(labels_fit) # Variance Inflation Factor
predict(plac_fit, newdata = matrix[c(187),] )

# Placards
placards_july_sales <- sales[1:186,4]
july_matrix <- matrix[1:186,]
july_matrix$placardssales <- placards_july_sales

nrow(july_matrix)

placards_fit <- lm(placardssales ~ ., data = july_matrix, singular.ok = TRUE)
# vif(labels_fit) # Variance Inflation Factor
predict(placards_fit, newdata = matrix[c(187),] )

# Packaging
pack_july_sales <- sales[1:186,5]
july_matrix <- matrix[1:186,]
july_matrix$packsales <- pack_july_sales

nrow(july_matrix)

pack_fit <- lm(packsales ~ ., data = july_matrix, singular.ok = TRUE)
# vif(labels_fit) # Variance Inflation Factor
predict(pack_fit, newdata = matrix[c(187),] )

# Books
book_july_sales <- sales[1:186,6]
july_matrix <- matrix[1:186,]
july_matrix$booksales <- book_july_sales

nrow(july_matrix)

book_fit <- lm(booksales ~ ., data = july_matrix, singular.ok = TRUE)
# vif(labels_fit) # Variance Inflation Factor
predict(book_fit, newdata = matrix[c(187),] )

# Software
soft_july_sales <- sales[1:186,7]
july_matrix <- matrix[1:186,]
july_matrix$softsales <- soft_july_sales

nrow(july_matrix)

soft_fit <- lm(softsales ~ ., data = july_matrix, singular.ok = TRUE)
# vif(labels_fit) # Variance Inflation Factor
predict(soft_fit, newdata = matrix[c(187),] )
