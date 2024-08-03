library(pacman)

# Load libraries
p_load(tidyverse, dplyr, ggplot2, partykit,
       lubridate, magrittr, ggthemes, GGally,
       psych, corrplot, partykit, CHAID,
       tidymodels, rsample)
heart_df <- read_csv("/Users/michaelhuynh/Desktop/Programming/Datasets/heart.csv")


# Preliminary View for Data headers and types
View(heart_df)
head(heart_df)
colnames(heart_df)
sapply(heart_df, class)

# Preprocessing
## Find any NAs/missing values
any(is.na(heart_df))
colSums(is.na(heart_df))

# Descriptive Statistics
heart_df %>% 
  select(where(is.numeric)) %>%
  describe()
  
# Age Distribution
heart_df %>% 
  ggplot(aes(x = Age)) +
  geom_histogram(aes(y = ..count..), fill = "blue", color = "black", alpha = 0.7, bins = 30) +
  labs(title = "Age Distribution",
       x = "Age",
       y = "Count") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
# Note 1
## In this geom_bar, we find that there is a strong central tendency of ages from 40 to estimated 60 years among patients

# Facet Distribution Plots
heart_df %>% 
  select(where(is.character)) %>% 
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>% 
  ggplot(aes(x = value, fill = variable)) + 
  facet_wrap( ~ variable, scales = "free") +
  geom_bar() +
  theme_bw() +
  labs(title = "Distribution Plots for Numeric Variables") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  scale_colour_brewer() +
  theme_light() +
  guides(fill = "none")
# Note 1: Asymptomatic chest pain was the most frequent type of chest pain compared to all other types.
# Note 2: More than half of patients did not have exercise-induced angina.
# Note 3: More than half of patients had normal resting ecg levels
# Note 4: The patient population was overwhelmingly male in comparison to the much smaller female population
# Note 5: The majority of patients have Flat/Up st_slope in which Down st_slope patients were rare.

heart_df %>% 
  select(where(is.numeric), -HeartDisease) %>% # Leave out HeartDisease column
  
  # Increases the number of rows while decreasing number of columns 
  # Calling all columns with everything(), we pivot the column names so that they become row names 
  # Creates new column called value that corresponds to original column values
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  # Creates ggplot for new unique row name
  ggplot(aes(x = value, fill = variable)) +
  # Creates multiple plots for each variable.
  # The scales argument being specified as free to scale each plot independently of one another.
  facet_wrap(~ variable, scales = "free") +
  # Creates 30 bins and yields 70% non-transparency, making the plots easier to see
  geom_histogram(bins = 30, alpha = 0.7) +
  labs(title = "Distribution Plots for Numeric Variables") +
  scale_colour_brewer() + 
  theme_light() +
  guides(fill = "none") # Specifies that there are to be no legends
# Cholesterol levels were centrally distributed between 200 and 330 with a mean of 198.8.
# RestingBP levels were centrally distributed between 125 and 140.

# Boxplots
heart_df %>% 
  mutate(HeartDisease = factor(HeartDisease)) %>% # Turns HeartDisease into a factor object
  select(HeartDisease, where(is.numeric)) %>% # Selects only HeartDisease and all variables that are numeric
  pivot_longer(cols = where(is.numeric), names_to = "variable", values_to = "value") %>%  # Shortens number of columns by placing column names into rows under new column "variable" while assigning them their values based on previous structure.
  ggplot(aes(x = HeartDisease, y = value, fill = HeartDisease)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free") +
  labs(x = "Output", y = "Value", title = "Box Plots by Output") +
  scale_colour_brewer() +
  theme_light()

# Correlation Analysis
heart_df %>% 
  select(where(is.numeric)) %>% 
  ggcorr(label = TRUE, label_size = 4)
  
heart_df_factor <- heart_df %>%
  mutate_all(as.factor)
# Out of all variable relationships, Oldpeak (ST Depression) and fasting blood sugars (FastingBS) had the strongest positive correlative effect on suffering heart attacks albeit to a limited degree.
# Out of all variable relationships, MaxHR and Cholesterol had the most negative correlations, meaning that as each of these measures goes up, the likelihood of suffering heart disease went down.


# CHAID Decision Tree
control = chaid_control(maxheight = 6,
                        minbucket = 10)
chaid_model = chaid(HeartDisease ~ .,
                    data = heart_df_factor,
                    control = control)
par(mar = c(5, 4, 4, 2) + 0.1)

# Import CHAID Decision Tree (If it already exists)
chaid_model = readRDS("/Users/michaelhuynh/Desktop/Programming/R/chaid_model.rds")

plot(chaid_model,
     uniform = TRUE,
     gp = gpar(fontsize = 6,
               color = "black",
               lineheight = 0.8))ß

# According to the CHAID decision tree analysis, there is a notable association between asymptomatic chest pain and the Down/Flat slope of the peak exercise ST segment, particularly regarding their relationship with heart disease. 
# Among individuals exhibiting a Down/Flat ST segment slope and reporting asymptomatic chest pain, the analysis reveals a strong relationship between being male and suffering from heart attacks, especially if one has high fasting blood sugar levels.
# Across nearly all categorical attributes, being male had a relationally significant effect on experiencing heart attacks

# Model Analysis
heart_df$HeartDisease = as.factor(heart_df$HeartDisease)

# Data Split (need rsample to run initial_split!)
# This allocates 70% of the data from heart_df_factor to training set
# 30% remaining data is allocated to test set
# strata argument is used to stratify by HeartDisease to ensure that both training and test sets have similar proportions of those with/without heart disease to prevent
heart_split = initial_split(heart_df,
                            prop = 0.7,
                            strata = HeartDisease)

# Create training and test data sets
heart_training <- heart_split %>% 
  training()
heart_test <- heart_split %>% 
  testing()

# Data Preprocessing and modeling
## Creates recipe variable where HeartDisease is dependent variable being affected by all predictors in heart_training dataset.
## step_corr removes all numeric variables that are highly correlated with each other to mitigate multicollinearity
## step_normalize standardizes all numeric variables to have zero mean and unit variance; this is really good for KNN or support vector machines.
## step_dummy converts all nominal (categorical) variables to dummy variables except for HeartDisease as done by -all_outcomes() argument.
heart_recipe <- recipe(HeartDisease ~ .,
                       data = heart_training) %>% 
  step_corr(all_numeric(), threshold = 0.8) %>% 
  step_normalize(all_numeric()) %>% 
  step_dummy(all_nominal(), -all_outcomes())

# Creates heart_recipe_prep as recipe variable that uses heart_training data, cleaned of highly correlated variables and has only standardized variables
# Then it applies prep function on training data to estimate the necessary statistics (mean, standard deviations) to apply the defined transformations
heart_recipe_prep <- heart_recipe %>%
  prep(training = heart_training)

# Takes trained recipe and applies preprocessing steps like removing highly correlated variables above 0.8 threshold, normalizing numeric variables, and applies dummy variables (created in heart_recipe) to the original training dataset.
# Applies preprocessing steps defined in heart_recipe_prep to original training dataset, heart_training
heart_training_prep <- heart_recipe_prep %>%
  bake(new_data = NULL)

# Takes recipe object, heart_recipe_prep, and applies preprocessing steps like removing highly correlated variables above 0.8 threshold, 
# normalizing numeric variables, and applies dummy variables to the heart_test data set
heart_test_prep <- heart_recipe_prep %>%
  bake(new_data = heart_test)

head(heart_training_prep)

# Logistic Model

# Creates logistic regression model and stores to variable glm_model
# set_engine sets computational engine to fit model based on general linear model framework
# set_mode sets mode to classification; this is especially applicable to binary variables.
glm_model <- logistic_reg() %>% 
  set_engine('glm') %>% 
  set_mode('classification')

# Fits logistic regression model to the preprocessed training dataset
glm_fit <- glm_model %>% 
  fit(HeartDisease ~ .,
      data = heart_training_prep)

glm_fit

# Logistic Regression Model Predictions on Test Data

# Applies fitted logistic regression model, glm_fit, to testing data and generates class predictions
glm_class_prediction <- predict(glm_fit,
                                new_data = heart_test_prep,
                                type = "class")

# Applies fitted logistic regression model, glm_fit, to testing data and generates probability predictions
glm_prob_prediction <- predict(glm_fit,
                        new_data = heart_test_prep,
                        type = "prob")

# Combines actual outcomes, class predictions, and probability predictions into one dataframe
glm_results <- heart_test %>% 
  select(HeartDisease) %>% 
  bind_cols(glm_class_prediction, glm_prob_prediction)

# Rename glm_results column
glm_results <- glm_results %>%
  rename(predicted_class = .pred_class, prob_0 = .pred_0, prob_1 = .pred_1)

# Creates combined dataframe that shows predicted vs actual outcomes
glm_results

# Creates confusion matrix that shows predicted vs actual class labels 
confusion_matrix = conf_mat(glm_results, truth = HeartDisease, estimate = predicted_class)
print(confusion_matrix)
# Note: Based on the high volume of true positives/true negatives and the low frequency of false positives/negatives shown in the 
# model, it is reliable for accurately distinguishing between the presence and absence of heart disease.

# Create ROC Curve
roc_curve_data <- glm_results %>%
  roc_curve(truth = HeartDisease, prob_0)

# Display ROC curve data
print(roc_curve_data)

# Plot ROC Curve
roc_curve_data %>% 
  ggplot(aes(x = 1 - specificity, y = sensitivity)) + 
  geom_path() +
  theme_bw() +
  geom_abline(lty = 2, color = "red") +
  labs(title = "ROC Curve",
       x = "False Positive Rate (1 - Specificity)",
       y = "True Positive Rate (Sensitivity)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  
# Based on logarithmic shape of the ROC curve, the model is able to accurately distinguish between groups that are likely to experience heart disease vs not.

heart_tune_model <- decision_tree(cost_complexity = tune(),
                               tree_depth = tune(),
                               min_n = tune()) %>% 
  set_engine('rpart') %>% 
  set_mode('classification')

heart_tune_model

heart_metrics <- metric_set(accuracy, sens, spec, roc_auc)

# Create Workflow
heart_tune_wkfl <- workflow() %>% 
  add_model(heart_tune_model) %>% 
  add_recipe(heart_recipe)

heart_tune_wkfl

heart_grid <- grid_random(parameters(heart_tune_model),
                       size = 7)
heart_grid

heart_folds <- vfold_cv(heart_training, v = 5, strata = HeartDisease)

heart_tuning <- heart_tune_wkfl %>% 
  tune_grid(resamples = heart_folds,
            grid = heart_grid,
            metrics = heart_metrics)

# Show 4 Best Performing Models
heart_tuning %>% 
  show_best(metric = 'roc_auc', n = 4)

best_heart_model <- heart_tuning %>% 
  select_best(metric = 'roc_auc')

# Finalize workflow
final_heart_wkfl <- heart_tune_wkfl %>% 
  finalize_workflow(best_heart_model)
