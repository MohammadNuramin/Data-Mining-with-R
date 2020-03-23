data <- read.csv("data/zeemee_data.csv")

print(paste("Number of rows:", nrow(data)))
print(paste("Number of columns:", ncol(data)))
# sprintf("Number of rows: %i", nrow(data))
# sprintf("Number of columns: %i", ncol(data))

head(data)

tail(data)

str(data)

sapply(data, class)

sapply(data, typeof)

colSums(is.na(data))

cat_cols <- c('college', 'public_profile_enabled', 'going', 'interested', 'start_term', 'cohort_year', 'created_by_csv', 'high_school', 'transfer_status', 'roommate_match_quiz', 'engaged', 'final_funnel_stage')
num_cols <- c('last_login', 'schools_followed', 'chat_messages_sent', 'chat_viewed', 'videos_liked', 'videos_viewed', 'videos_viewed_unique', 'total_official_videos')

print(paste("Total categorical features:", length(cat_cols)))
print(paste("Total numerical features:", length(num_cols)))

cat_data <- data[, (colnames(data) %in% cat_cols)]

sapply(cat_data, function(x) length(unique(x)))

library(magrittr)
library(plyr)
library(dplyr)
library(purrr)
library(tidyr)

library(ggplot2)
library(grid)
library(gridExtra)

set.seed(1234)
options(warn=-1)

options(repr.plot.width=15, repr.plot.height=25)

count_plots <- function (data, column) {
    ggplot(data, aes_string(x=column)) +
        geom_bar(colour="black", fill="#83ff52") +
        xlab(column) +
        coord_flip()
#         theme(axis.title.y = element_text(size = rel(1.0), angle = 90)) +
#         theme(axis.title.x = element_text(size = rel(1.0), angle = 00))
}

cat_plots <- lapply(colnames(cat_data), count_plots, data=cat_data)

grid.arrange(grobs=cat_plots, ncol=2, as.table=FALSE)



# cat_data %>%
#   gather() %>% 
#   ggplot(aes(value)) +
#     facet_wrap(~ key, scales="free") +
#     geom_bar(colour="black", fill="#DD8888") +
#     coord_flip()

num_data <- data[, (colnames(data) %in% num_cols)]

summary(num_data)

options(repr.plot.width=7, repr.plot.height=12)

dist_plots <- function (data, column) {
    ggplot(data, aes_string(x=column)) +
    geom_density(fill="#FF6666")
}

num_plots <- lapply(colnames(num_data), dist_plots, data=num_data)

grid.arrange(grobs=num_plots, ncol=2, as.table=FALSE)
# num_data %>%
#   gather() %>% 
#   ggplot(aes(value)) +
#     facet_wrap(~ key, scales = "free") +
#     geom_density(fill="#FF6666")

options(repr.plot.width=10, repr.plot.height=15)

dist_plots <- function (data, column) {
    ggplot(data, aes_string(x=column, y=column)) + 
    geom_boxplot(fill="#FF6666") +
    guides(fill=FALSE)
}

num_plots <- lapply(colnames(num_data), dist_plots, data=num_data)

grid.arrange(grobs=num_plots, ncol=2, as.table=FALSE)



# num_data %>%
#   gather() %>% 
#   ggplot(aes(value, value)) +
#     facet_wrap(~ key, scales = "free") +
#     geom_boxplot(fill="#FF6666")

data$final_funnel_stage <- revalue(data$final_funnel_stage, c('Enrolled' = 'Enroll',
                                                              'Deposited'='Enroll',
                                                               'Inquired' = "Doesn't Enroll", 
                                                               'Applied' = "Doesn't Enroll",
                                                               'Accepted' = "Doesn't Enroll",
                                                               'Application_Complete' = "Doesn't Enroll"))

options(repr.plot.width=5, repr.plot.height=5)

data_pie <- data %>% 
  group_by(final_funnel_stage) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(per=n/sum(n)) %>% 
  arrange(desc(final_funnel_stage))

ggplot(data=data_pie)+
  geom_bar(aes(x="", y=per, fill=final_funnel_stage), stat="identity", width = 1)+
  coord_polar("y", start=0, direction = -1)+
  theme_void()+
  geom_text(aes(x=1, y = cumsum(per) - per/2, label=scales::percent(data_pie$per)))

complete_data = na.omit(data)

trainRowCount <- floor(0.8 * nrow(complete_data))
set.seed(1234)

trainIndex <- sample(1:nrow(complete_data), trainRowCount)
train <- complete_data[trainIndex,]
test <- complete_data[-trainIndex,]

print(paste("Shape of training set:", toString(dim(train))))
print(paste("Shape of test set:", toString(dim(test))))

logit_model <- glm(final_funnel_stage ~ college + public_profile_enabled + going + interested + start_term + cohort_year + created_by_csv + last_login + schools_followed + high_school + transfer_status + roommate_match_quiz + chat_messages_sent + chat_viewed + videos_liked + videos_viewed + videos_viewed_unique + total_official_videos + engaged, 
                data=train, 
                family=binomial(link="logit"))

summary(logit_model)

y_pred <- predict(logit_model, test[,-20], type="response")
predictions_logit <- factor(ifelse(y_pred > 0.5, "Enroll", "Doesn't Enroll"))

library(caret)
confusionMatrix(predictions_logit, test$final_funnel_stage, positive = "Enroll")

options(repr.plot.width=5, repr.plot.height=5)

library(pROC)

res.roc <- roc(test$final_funnel_stage, y_pred)
plot.roc(res.roc, print.auc = TRUE, print.thres = "best")

library(randomForest)

features <- c('college', 'public_profile_enabled', 'going', 'interested', 'start_term', 'cohort_year', 
              'created_by_csv', 'last_login', 'schools_followed', 'high_school', 'transfer_status', 
              'roommate_match_quiz', 'chat_messages_sent', 'chat_viewed', 'videos_liked', 'videos_viewed', 
              'videos_viewed_unique', 'total_official_videos', 'engaged')

rf_model <- randomForest(train[features], train$final_funnel_stage)

summary(rf_model)

predictions_rf <- predict(rf_model, test[features])

varImp(rf_model)

varImpPlot(rf_model)

options(repr.plot.width=15, repr.plot.height=7)

var_importance <- data_frame(variable=setdiff(colnames(train), "final_funnel_stage"),
                             importance=as.vector(importance(rf_model)))
var_importance <- arrange(var_importance, desc(importance))
var_importance$variable <- factor(var_importance$variable, levels=var_importance$variable)

p <- ggplot(var_importance, aes(x=variable, weight=importance, fill=variable))
p <- p + geom_bar() + ggtitle("Variable Importance from Random Forest Fit")
p <- p + xlab("Demographic Attribute") + ylab("Variable Importance (Mean Decrease in Gini Index)")
p <- p + scale_fill_discrete(name="Variable Name")
p + theme(axis.text.x=element_blank(),
          axis.text.y=element_text(size=12),
          axis.title=element_text(size=16),
          plot.title=element_text(size=18),
          legend.title=element_text(size=16),
          legend.text=element_text(size=12))

library(caret)

confusionMatrix(predictions_rf, test$final_funnel_stage, positive = "Enroll")

options(repr.plot.width=5, repr.plot.height=5)

library(pROC)

predicted_probabilities <- predict(rf_model, test[features], type = "prob")[,2]

res.roc <- roc(test$final_funnel_stage, predicted_probabilities)
plot.roc(res.roc, print.auc = TRUE, print.thres = "best")

# Extract some interesting results
roc.data <- data_frame(
  thresholds = res.roc$thresholds,
  sensitivity = res.roc$sensitivities,
  specificity = res.roc$specificities
)

# Get the probablity threshold for specificity = 0.6
roc.data %>% filter(specificity >= 0.6)
