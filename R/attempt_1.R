# attempt 1
library(plyr)
library(dplyr)
library(magrittr)
library(randomForest)

d_in = read.csv("data/train.csv", as.is = TRUE)

d_store = read.csv("data/store.csv", as.is = TRUE)

d_test = read.csv("data/test.csv", as.is = TRUE)

d_sample = read.csv("data/sample_submission.csv", as.is = TRUE)

# PaulShearer (12 line model script): Geometric mean (.13952)
d_in = d_in[d_in$Sales > 0, ]
preds=c('Store', 'DayOfWeek', 'Promo')
mdl = d_in %>% group_by_(.dots = preds) %>% summarise(PredSales = exp(mean(log(Sales)))) %>% ungroup()
train_pred = d_in %>% left_join(mdl, by = preds) %>% ungroup() %>% mutate(Id = row_number()) %>% select(Id, Sales, PredSales)
train_error = sqrt(mean((train_pred$Sales - train_pred$PredSales)^2, na.rm = TRUE))

pred = d_test %>% left_join(mdl, by = preds) %>% select(Id, PredSales) %>% rename(Sales = PredSales)
pred$Sales[is.na(pred$Sales)] = 0

write.csv(pred, "data/pred_geometric_mean.csv", row.names = F)

#median (.13888)
d_in = d_in[d_in$Sales > 0, ]
preds1=c('Store', 'DayOfWeek', 'Promo')
mdl1 = d_in %>% group_by_(.dots = preds1) %>% summarise(PredSales = median(Sales)) %>% ungroup()
train_pred1 = d_in %>% left_join(mdl1, by = preds1) %>% ungroup() %>% mutate(Id = row_number()) %>% select(Id, Sales, PredSales)
train_error1 = sqrt(mean((train_pred1$Sales - train_pred1$PredSales)^2, na.rm = TRUE))

pred1 = d_test %>% left_join(mdl1, by = preds) %>% select(Id, PredSales) %>% rename(Sales = PredSales)
pred1$Sales[is.na(pred1$Sales)] = 0

write.csv(pred1, "data/pred_median.csv", row.names = F)

# average of the above 2 approaches (.13873)
train_pred_avg = train_pred %>%
  left_join(train_pred1, by = "Id") %>%
  mutate(PredSales = (PredSales.x + PredSales.y)/2) %>%
  select(Id, Sales.x, PredSales)
train_avg_error = sqrt(mean((train_pred_avg$Sales.x - train_pred_avg$PredSales)^2, na.rm = TRUE))



pred_avg = pred %>%
  left_join(pred1, by = "Id") %>%
  mutate(Sales = (Sales.x + Sales.y)/2) %>%
  select(Id, Sales)
train_pred_avg = d_in %>% left_join(mdl1, by = preds1) 
train_error_avg = sqrt(mean((train_pred1$Sales - train_pred1$PredSales)^2, na.rm = TRUE))


# utilizing other variables
write.csv(pred_avg, "data/pred_avg_median_gm.csv", row.names = F)


# michaelpawlus random forest script
train <- merge(d_in, d_store)
test <- merge(d_test, d_store)

train[is.na(train)]   <- 0
test[is.na(test)]   <- 0

train$Date = as.Date(train$Date)
train$month <- as.integer(format(train$Date, "%m"))
train$year <- as.integer(format(train$Date, "%y"))
train$day <- as.integer(format(train$Date, "%d"))
train <- train[,-c(3,8)]

test$Date = as.Date(test$Date)
test$month <- as.integer(format(test$Date, "%m"))
test$year <- as.integer(format(test$Date, "%y"))
test$day <- as.integer(format(test$Date, "%d"))
test <- test[,-c(4,7)]

feature.names <- names(train)[c(1,2,6,8:12,14:19)]
cat("Feature Names\n")
feature.names

for (f in feature.names) {
  if (class(train[[f]])=="character") {
    levels <- unique(c(train[[f]], test[[f]]))
    train[[f]] <- as.integer(factor(train[[f]], levels=levels))
    test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
  }
}

clf <- randomForest(train[,feature.names], 
                    log(train$Sales+1),
                    mtry=5,
                    ntree=50,
                    sampsize=100000,
                    do.trace=TRUE)

cat("model stats\n")
clf
cat("print model\n")
print(clf)
cat("Importance 1\n")
importance(clf)
cat("Permutation Importance Unscaled\n")
importance(clf, type = 1)
cat("GINI Importance\n")
importance(clf, type = 2)
cat("Plot Model\n")
plot(clf)
cat("Plot Importance\n")
plot(importance(clf), lty=2, pch=16)

pred_rf <- exp(predict(clf, test)) -1
pred_rf_out <- data.frame(Id=test$Id, Sales=pred_rf)

write.csv(pred_rf_out, "data/pred_rf.csv", row.names = F)



