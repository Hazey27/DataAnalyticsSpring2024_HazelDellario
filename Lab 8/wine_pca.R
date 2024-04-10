

library("tidyverse")
library("HDclassif")
library("factoextra")
library("caret")

data(wine)

colnames(wine) <- c("Cvs", "Alcohol", "Malic_Acid", "Ash", "Alkalinity_of_Ash", "Magnesium", "Total_Phenols", "Flavanoids", "NonFlavanoid_Phenols", "Proanthocyanins", "Color_Intensity", "Hue", "OD280/OD315_of_Diluted_Wine", "Proline")
wine$Cvs = factor(wine$Cvs, labels = c("C1", "C2", "C3"))

pca = prcomp(wine[ , -1], center = T, scale = T)

fviz_pca_var(pca)
fviz_eig(pca)  # nearly 60% of variance explained by first 2 axes
summary(pca)

fviz_pca_ind(pca)


heatmap(cor(wine[ , -1]))


# Visualizing in ggplot
pca_df = cbind(dim1 = pca$x[ , 1], dim2 = pca$x[ , 2], wine[ , ])

pca_plot = ggplot(pca_df, aes(x = dim1, y = dim2, color = Cvs)) +
  geom_point()

print(pca_plot)


# KNN model
num_obs = length(wine[ , 1])
train_indices = sample.int(num_obs, size = num_obs * 0.7, replace = F)
train_data = wine[train_indices, ]
test_data = wine[-train_indices, ]
# 
 train_scaled = preProcess(x = train_data, method = c("center", "scale"))
# test_scaled = preProcess(x = test_data, method = c("center", "scale"))


ctrl <- trainControl(method = "repeatedcv", repeats = 3, number = 5, classProbs = T)
knnFit <- train(form = Cvs ~ ., data = train_data, method = "knn", trControl = ctrl, tuneLength = 10, preProcess = c("center", "scale")) # 

train_data$preds = predict(knnFit, newdata = train_data[ , -1])
test_data$preds = predict(knnFit, newdata = test_data[ , -1])

confusionMatrix(train_data$Cvs, train_data$preds)
confusionMatrix(test_data$Cvs, test_data$preds)  # 94% accurate

train_data$mismatched[which(train_data$Cvs != train_data$preds)] = TRUE
train_data$mismatched[which(train_data$Cvs == train_data$preds)] = FALSE

test_data$mismatched[which(test_data$Cvs != test_data$preds)] = TRUE
test_data$mismatched[which(test_data$Cvs == test_data$preds)] = FALSE

pca_test = predict(pca, newdata = test_data[ , -c(1, 15, 16)])
pca_test_df = cbind(dim1 = pca_test[ , 1], dim2 = pca_test[ , 2], test_data[ , ])

ggplot(pca_test_df, aes(x = dim1, y = dim2, shape = mismatched, color = Cvs)) +
  geom_point(size = 3) +
  scale_shape_manual(values = c(16, 17)) +
  xlab("Dimension 1 (36.2% of variance)") +
  ylab("Dimension 2 (19.2% of variance)") +
  ggtitle("Test Values and Predictions") +
  theme_bw() +
  labs(shape = "Incorrect Prediction", color = "CVS") +
  theme(plot.title = element_text(hjust = 0.5))

