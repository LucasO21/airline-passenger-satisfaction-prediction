# TOPIC: AIRLINE CUSTOMER SATISFACTION ANALYSIS ----

## Create Directories 
# source("../Functions/Functions_List.R")
# func_create_dir()

# 1.0: SETUP ----

## Libraries ----

## Data Wrangling
library(tidyverse)

## Visualizations 
library(skimr)
library(patchwork)
library(dlookr)
library(correlationfunnel)
library(wesanderson)
library(formattable)
library(DT)

## Modeling
library(recipes)
library(h2o)
library(lime)

## Load Data ----
train_raw_tbl <- read.csv("00_Data/00_Data_Raw/train.csv") %>% as_tibble()
test_raw_tbl <- read.csv("00_Data/00_Data_Raw/test.csv") %>% as_tibble()

# 2.0: DATA UNDERSTANDING ----

## 2.1: Data Quality 
dim(train_raw_tbl)
dim(test_raw_tbl)

train_raw_tbl %>% glimpse()
train_raw_tbl %>% sapply(function(x) sum(is.na(x)))
train_raw_tbl %>% count(satisfaction)

test_raw_tbl %>% glimpse()
test_raw_tbl %>% sapply(function(x) sum(is.na(x)))
test_raw_tbl %>% count(satisfaction)

# Observation: 
 # Column "X" is an index and can be removed. 
 # "Arrival.Delay.in.Minutes" is missing a few values and will be imputed.

# Initial Data Cleaning ----

 # edit "satisfaction" variable & remove "x" variable
train_raw_tbl <- train_raw_tbl %>% 
    setNames(names(.) %>% str_to_lower()) %>% 
    mutate(satisfaction = case_when(
        satisfaction == "neutral or dissatisfied" ~ "not satisfied",
        TRUE ~ "satisfied"
    )) %>% 
    select(-x) %>% 
    mutate_if(is.character, as.factor)

test_raw_tbl <- test_raw_tbl %>% 
    setNames(names(.) %>% str_to_lower()) %>% 
    mutate(satisfaction = case_when(
        satisfaction == "neutral or dissatisfied" ~ "not satisfied",
        TRUE ~ "satisfied"
    )) %>% 
    select(-x) %>% 
    mutate_if(is.character, as.factor)

train_raw_tbl %>% 
    select(gate.location, seat.comfort, food.and.drink, satisfaction) %>% 
    gather(key = key, value = values, (gate.location:food.and.drink)) %>% 
    ggplot(aes(values, fill = satisfaction))+
    geom_bar(position = position_dodge())+
    facet_wrap(~ key, "free")

train_raw_tbl %>% 
    select(gate.location, seat.comfort, food.and.drink, satisfaction) %>% 
    gather(key = key, value = values, (gate.location:food.and.drink)) %>% 
    ggplot(aes(values, fill = satisfaction))+
    geom_density()+
    facet_wrap(~ key, "free")


# Functions ----

# Color Scale Manual
scale_manual_2 <- c("#ce295e", "#476481")
scale_manual_3 <- c("#476481", "#ce295e", "#f57814")

# Function 1: Missing Data
func_plot_missing_data <- function(data){
  
  nrow <- nrow(data)
  
  missing_tbl <- data %>% 
    sapply(function(x) sum(is.na(x))) %>% 
    tidy() %>% 
    mutate(pct = x/nrow) %>% 
    mutate(pct_txt = pct %>% scales::percent(accuracy = .3)) %>% 
    mutate(names = names %>% fct_rev()) %>% 
    rename(missing = x)
  
  missing_tbl %>% 
    ggplot(aes(missing, names))+
    geom_col(fill = "#ce295e")+
    geom_label(aes(label = pct_txt, fill = "#ce295e"))+
    theme_bw()+
    theme(legend.position = "none")
    
  
}

func_plot_missing_data(train_raw_tbl)

# Function 2: Mosiac Plot Function: For Categorical Variables

func_mosaic_plot <- function(data, var_name){
  
  var_name_expr <- rlang::enquo(var_name)
  
  data %>% 
    select(!!var_name_expr, satisfaction) %>% 
    group_by(!!var_name_expr, satisfaction) %>% 
    summarise(count = n()) %>%
    mutate(cut.count = sum(count),
           prop = count/sum(count)) %>%
    ungroup() %>% 
    ggplot(aes(!!var_name_expr, prop, width = cut.count, fill = satisfaction))+
    geom_bar(stat = "identity", position = "fill")+
    scale_y_continuous(labels = scales::percent_format())+
    theme_bw()+
    geom_text(aes(label = scales::percent(prop, accuracy = .1)), position = position_stack(vjust = 0.5), size = 3)+
    scale_fill_manual(values = scale_manual_2)+
    theme(axis.text.x = element_blank())+
    theme(legend.position = "bottom",
          legend.direction = "horizontal",
          legend.title = element_blank(),
          legend.key.size = unit(0.35, "cm"))
}

func_mosaic_plot(data = train_raw_tbl, var_name = on.board.service) + 
  facet_grid(~ on.board.service, scales = "free_x", space = "free_x")+
  func_plot_axis_text_format(facet = TRUE, mosaic = TRUE)

# Function 3: GGPLOT axis edit 
func_plot_axis_text_format <- function(facet = FALSE, mosaic = TRUE){
  
  # format x & y axis
  theme <- theme(axis.text.x = element_text(color = "black", size = 8.5),
                 axis.text.y = element_text(colour = "black", size = 8.5))
  
  # edit strip text if facet is TRUE
  if (facet){
    theme <- theme +
      theme(strip.background = element_rect(fill = "#2c3e50"),
            strip.text = element_text(color = "white"))
    
  } else {
    
    theme <- theme
    
  }
  
  # mosaic plots
  if (mosaic){
    
    theme <- theme +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank())
    
  } else {
    
    theme <- theme
    
  }
  
  return(theme)
  
}

# Function 4: Evaluate Model Performance on Test Set
func_get_model_performance_auc_plot <- function(model_h2o){
  
  perf_h2o <- h2o.performance(model_h2o, newdata = test_h2o)
  
  perf_h2o %>% 
    h2o.metric() %>% 
    as_tibble() %>% 
    mutate(auc = h2o.auc(perf_h2o)) %>% 
    select(tpr, fpr, auc, precision, recall)
}



fs::file_create("02_Functions/functions_airline_passengers_analysis.R")

file_header_text <- str_glue(
  "
  # FUNCTIONS FOR AIRLINE PASSENGER SATISFACTION ANALYSIS
  
  # func_mosaic_plot(): A function to create mosiaic plots to visualize 2 way categorical relationships
  
  # func_plot_axis_text_format(): A function to size and bold x & y axis of ggplots
  
  "
)

write_lines(file_header_text, file = "02_Functions/functions_airline_passengers_analysis.R")

c("func_plot_missing_data", "func_mosaic_plot", "func_plot_axis_text_format", "func_get_model_performance_auc_plot") %>% 
  dump(file = "02_Functions/functions_airline_passengers_analysis.R", append = FALSE)

# Exploratory Data Analysis ----

# proportion of outcome variable
train_raw_tbl %>% 
    count(satisfaction) %>% 
    mutate(pct = n/sum(n))

train_raw_tbl %>% skim()

# distribution of departure delay
train_raw_tbl %>% 
    ggplot(aes(departure.delay.in.minutes, fill = satisfaction))+
    geom_density()

# distribution of arrival delay
train_raw_tbl %>% 
    ggplot(aes(arrival.delay.in.minutes, fill = satisfaction))+
    geom_density()

# distribution of "age"
train_raw_tbl %>% 
    ggplot(aes(age, fill = satisfaction))+
    geom_density(alpha = 0.7)

# distribution of "flight.distance"
train_raw_tbl %>% 
    ggplot(aes(flight.distance, fill = satisfaction))+
    geom_density(alpha = 0.7)


# correlation funnel
correlation_funnel_tbl <- train_raw_tbl %>% 
    select(-id) %>% 
    mutate(arrival.delay.in.minutes = ifelse(is.na(arrival.delay.in.minutes), 0,
                                             arrival.delay.in.minutes)) %>% 
    mutate(across(c(inflight.wifi.service:cleanliness), factor)) %>% 
    binarize()

correlation_funnel_tbl %>% glimpse()

correlation_funnel_plot <- correlation_funnel_tbl %>% 
    correlate(target = satisfaction__satisfied) %>% 
    plot_correlation_funnel()+
    theme_bw()+
    theme(axis.text.x = element_text(face = "bold"),
          axis.text.y = element_text(face = "bold"))

correlation_funnel_plot

saveRDS(object = correlation_funnel_plot, file = "06_Exec_Summary_Plots/corr_funnel.rds")


# ML Modeling ----

# Preprocessing
train_raw_tbl <- train_raw_tbl %>% 
    mutate(across(c(inflight.wifi.service:cleanliness), factor)) %>% 
    mutate(satisfaction = satisfaction %>% fct_relevel(
        "satisfied", "not satisfied"
    ))

test_raw_tbl <- test_raw_tbl %>% 
    mutate(across(c(inflight.wifi.service:cleanliness), factor)) %>% 
    mutate(satisfaction = satisfaction %>% fct_relevel(
        "satisfied", "not satisfied"
    ))

# Recipe
recipe_obj <- recipe(satisfaction ~ ., data = train_raw_tbl) %>% 
    step_rm(id) %>% 
    step_zv(all_predictors()) %>% 
    step_medianimpute(arrival.delay.in.minutes) %>% 
    prep()

recipe_obj

train_tbl <- bake(recipe_obj, new_data = train_raw_tbl)
test_tbl <- bake(recipe_obj, new_data = test_raw_tbl)

# Modeling
h2o.init()

# Split H2o Frame into Training & Validation Sets
h2o_split_obj <- h2o.splitFrame(as.h2o(train_tbl), ratios = c(0.85), seed = 100)
train_h2o <- h2o_split_obj[[1]]
valid_h2o <- h2o_split_obj[[2]]
test_h2o <- as.h2o(test_tbl)

# Assign Roles
y <- "satisfaction"
x <- setdiff(names(train_h2o), y)

# Model Specification
automl_models_h2o <- h2o.automl(
    x = x,
    y = y,
    training_frame = train_h2o,
    validation_frame = valid_h2o,
    max_runtime_secs = 300,
    nfolds = 5
)

# Inspect Model Output 
automl_models_h2o@leaderboard

automl_models_h2o@leader

# Custom Extract Model Function
func_extract_h2o_model_name_by_position <- function(h2o_leaderboard, n = 1,
                                                    verbose = TRUE){
    
    model_name <- h2o_leaderboard %>% 
        as_tibble() %>% 
        slice(n) %>% 
        pull(model_id) 
    
    if (verbose) message(model_name)
    
    return(model_name)
        
}

automl_models_h2o@leaderboard %>% 
    func_extract_h2o_model_name_by_position(5) %>% 
    h2o.getModel()

# Save Top 5 Models
h2o.getModel("XGBoost_grid__1_AutoML_20210531_161514_model_1") %>% 
    h2o.saveModel(path = "05_Models/")

h2o.getModel("StackedEnsemble_BestOfFamily_AutoML_20210531_161514") %>% 
    h2o.saveModel(path = "05_Models/")

h2o.getModel("StackedEnsemble_AllModels_AutoML_20210531_161514") %>% 
    h2o.saveModel(path = "05_Models/")

h2o.getModel("GBM_grid__1_AutoML_20210531_161514_model_1") %>% 
    h2o.saveModel(path = "05_Models/")

h2o.getModel("XGBoost_2_AutoML_20210531_161514") %>% 
    h2o.saveModel(path = "05_Models/")

# Loading Models
h2o.loadModel("05_Models/XGBoost_grid__1_AutoML_20210531_161514_model_1")

# Making Predictions
xgboost_grid_1_automl <- h2o.loadModel("05_Models/XGBoost_grid__1_AutoML_20210531_161514_model_1")

h2o_predictions_tbl <- h2o.predict(object = xgboost_grid_1_automl, newdata = test_h2o) %>% 
    as_tibble()

# Visualizing The Leaderboard ----
top_10_leaderboard_transformed_tbl <- automl_models_h2o@leaderboard %>% 
    as_tibble() %>% 
    mutate(model_type = str_split(model_id, "_", simplify = TRUE) %>% .[,1]) %>% 
    slice(1:5) %>% 
    rownames_to_column() %>% 
    mutate(
        model_id = model_id %>% as.factor %>% reorder(auc),
        model_type = model_type %>% as.factor) %>% 
    select(rowname, model_id, auc, logloss, model_type) %>% 
    gather(key = key, value = value, -c(model_id, model_type, rowname), factor_key = TRUE) %>% 
    mutate(model_id = paste0(rowname, ". ", model_id) %>% as.factor() %>% fct_rev())

top_10_leaderboard_transformed_tbl %>% 
    ggplot(aes(value, model_id))+
    geom_col(width = 0.6, fill = "lightgrey", col = "darkgrey")+
    geom_text(aes(label = value %>% scales::percent(accuracy = .1), hjust = "inward",
                  fontface = "bold"))+
    facet_wrap(~ key, "free_x", ncol = 1)+
    theme_classic()+
    theme(axis.text.x = element_text(size = 10, face = "bold"),
          axis.text.y = element_text(size = 10, face = "bold"),
          strip.text = element_text(size = 10, face = "bold", color = "white"),
          strip.background = element_rect(fill = "gray30"),
          legend.position = "none")
        

top_10_leaderboard_transformed_tbl %>% 
    formattable()
        

top_10_leaderboard_transformed_tbl <- automl_models_h2o@leaderboard %>% 
    as_tibble() %>% 
    slice(1:10) %>% 
    select(model_id, auc, logloss) %>% 
    mutate(auc = round(auc, 3),
           logloss = round(logloss,3)) 

as.datatable(
    caption = "Leaderboard Ranking by AUC & Logloss",
    formattable(top_10_leaderboard_transformed_tbl, list(
    auc = color_tile("transparent", "lightgreen"),
    logloss = color_tile("lightgreen", "transparent")
 )))
   

# Grid Search
gbm_1_h2o

h2o.performance(gbm_1_h2o, newdata = test_h2o)

# Current overall accuracy (pre - grid search) is about 96%. 

?h2o.grid()
gbm_1_h2o@allparameters

# get a list of hyper params for the model
gbm_1_h2o_params <- list(learn_rate = c(0.01, 0.1),
                         max_depth = c(3, 5, 9),
                         sample_rate = c(0.8, 1.0),
                         col_sample_rate = c(0.2, 0.5, 1.0))

# set up and tune grid
gbm_1_grid_01 <- h2o.grid(
    algorithm = "gbm",
    grid_id = "gbm_1_grid_01",
    hyper_params = gbm_1_h2o_params,
        
    # h2o.gbm()
    x = x,
    y = y,
    training_frame = train_h2o,
    validation_frame = valid_h2o,
    nfolds = 5
)

# examine grid search results
gbm_1_grid_01

h2o.getGrid(grid_id = "gbm_1_grid_01", sort_by = "auc", decreasing = TRUE)

# save best model (auc) from grid search
gbm_1_grid_01_model_17 <- h2o.getModel("gbm_1_grid_01_model_17")

# evaluate best model on training and validation set
gbm_1_grid_01_model_17 %>% h2o.auc(train = TRUE, valid = TRUE)

# evaluate best model from grid search on test set
gbm_1_grid_01_model_17 %>% 
    h2o.performance(newdata = test_h2o)

# Overall accuracy (post - grid search) is still about 96%)

# H20 MODEL PERFORMANCE EVALUATION ----
xgboost_grid_1_automl <- h2o.loadModel("")
h2o.auc(xgboost_grid_1_automl, train = TRUE, valid = TRUE, xval = TRUE)

stacked_ensemble_bof <- h2o.loadModel("05_Models/StackedEnsemble_BestOfFamily_AutoML_20210531_161514")

performance_h2o <- h2o.performance(xgboost_grid_1_automl, newdata = test_h2o)

performance_h2o %>% slotNames()
performance_h2o %>% typeof()

performance_h2o@metrics 
h2o.auc(performance_h2o)
h2o.giniCoef(performance_h2o)
h2o.recall(performance_h2o)
h2o.confusionMatrix(performance_h2o)

performance_tbl <- performance_h2o %>% 
    h2o.metric %>% 
    as_tibble()

performance_tbl

xgboost_grid_1_automl

model <- automl_models_h2o@leaderboard %>%
    func_extract_h2o_model_name_by_position(5) %>%
    h2o.getModel()

h2o.performance(model, newdata = test_h2o) %>% h2o.confusionMatrix()
h2o.performance(model, newdata = test_h2o) %>% h2o.recall()

#* NOTE:
#* PRECISION = tp/(tp + fp): Metric for over picking "not satisfied" = 10729 / (10729 + 303)
#* The model correctly predicted "not satisfied" as "not satisfied" 97% of the time
#* 
#* RECALL = tp/(tp + fn): Metric for under picking "satisfied" = 10729 / (10729 + 674)
#* The model correctly predicted "not satisfied" as "not satisfied" 97% of the time
#* 
#* F1 = 2 * (precision * recall) / (precision + recall): Metric for balancing 
#* 2 * (0.9725344 * 0.9408927) / (0.9725344 + 0.9408927) -> Harmonic Mean

# max f1
performance_tbl %>% filter(f1 == max(f1)) # matches calculation above

# visualize precision vs recall

performance_tbl %>% 
    ggplot(aes(threshold))+
    geom_line(aes(y = precision), color = "blue", size = 1)+
    geom_line(aes(y = recall), color = "red", size = 1)+
    geom_vline(xintercept = h2o.find_threshold_by_max_metric(performance_h2o, "f1"))+
    annotate(geom = "label", x = 0.50, y = 0.10,
             label = "max f1 line")+
    theme_bw()+
    labs(title = "Precision vs Recall", y = "value")

# visualize roc curves for multiple models
path <- "05_Models/StackedEnsemble_AllModels_AutoML_20210603_185006"
func_load_model_performance_metrics <- function(path, test_tbl){
    
    model_h2o <- h2o.loadModel(path)
    perf_h2o <- h2o.performance(model_h2o, newdata = test_h2o)
    
    perf_h2o %>% 
        h2o.metric() %>% 
        as_tibble() %>% 
        mutate(auc = h2o.auc(perf_h2o)) %>% 
        select(tpr, fpr, auc)
}

model_metrics_tbl <- fs::dir_info(path = "05_Models/") %>% 
    select(path) %>% 
    mutate(metrics = map(path, func_load_model_performance_metrics, test_h2o)) %>% 
    unnest(cols = c(metrics))

model_metrics_tbl %>% 
    mutate(
        path = str_split(path, pattern = "/", simplify = TRUE)[, 2] %>% as.factor(),
        auc = auc %>% round(5) %>%  as.character() %>% as.factor()) %>%
    ggplot(aes(fpr, tpr, color = path, linetype = auc))+
    geom_line(size = 1)+
    theme_bw()+
    theme(axis.text.x = element_text(size = 10, face = "bold"),
          axis.text.y = element_text(size = 10, face = "bold"),
          legend.position = "bottom", legend.direction = "vertical")+
    labs(title = "ROC Plot", subtitle = "Performance of Top 5 Models")

# visualize precision vs recall
# path <- "05_Models/GBM_grid__1_AutoML_20210531_010440_model_1"

func_load_model_performance_metrics <- function(path, test_tbl){
    
    model_h2o <- h2o.loadModel(path)
    perf_h2o <- h2o.performance(model_h2o, newdata = test_h2o)
    
    perf_h2o %>% 
        h2o.metric() %>% 
        as_tibble() %>% 
        mutate(auc = h2o.auc(perf_h2o)) %>% 
        select(tpr, fpr, auc, precision, recall)
}

model_metrics_tbl <- fs::dir_info(path = "05_Models/") %>% 
    select(path) %>% 
    mutate(metrics = map(path, func_load_model_performance_metrics, test_h2o)) %>% 
    unnest(cols = c(metrics))

model_metrics_tbl %>% 
    mutate(
        path = str_split(path, pattern = "/", simplify = TRUE)[, 2] %>% as.factor(),
        auc = auc %>% round(5) %>%  as.character() %>% as.factor()) %>%
    ggplot(aes(recall, precision, color = path, linetype = auc))+
    geom_line(size = 1)+
    theme_bw()+
    theme(axis.text.x = element_text(size = 10, face = "bold"),
          axis.text.y = element_text(size = 10, face = "bold"),
          legend.position = "bottom", legend.direction = "vertical")+
    labs(title = "Precision vs Recall Plot", subtitle = "Performance of Top 5 Models")

# gain & lift calculation: (manual)
ranked_predictions_tbl <- h2o_predictions_tbl %>% 
    bind_cols(test_tbl) %>% 
    select(predict:satisfied, satisfaction) %>% 
    arrange(desc(not.satisfied))

#* Without a model, we'd only expect the global satisfaction rate for first 10 (57%) or 5.7 people
#* Out of 25,976, we'd expect 25976 * 0.57 = 14806 to not be satisfied 
#* If 14806 people expected to not be satisfied, we just 10 of 25976 or
#* We just gained 14806 or 100% in first 10 cases 

#*******************************************************************************

func_plot_gain <- function(data){
  
  # Gain Calculation
  gain_calc <- data %>% 
    mutate(ntile = ntile(not.satisfied, n = 10)) %>% 
    group_by(ntile) %>% 
    summarise(
      cases = n(),
      responses = sum(satisfaction == "not satisfied")
    ) %>% 
    arrange(desc(ntile)) %>% 
    mutate(group = row_number()) %>% 
    select(group, cases, responses) %>% 
    mutate(
      cumulative_responses = cumsum(responses),
      pct_responses        = responses / sum(responses),
      gain                 = cumsum(pct_responses),
      cumulative_pct_cases = cumsum(cases) / sum(cases),
      lift                 = gain / cumulative_pct_cases,
      gain_baseline        = cumulative_pct_cases,
      lift_baseline        = gain_baseline / cumulative_pct_cases
    )
  
  # Gain Chart
  arrow_types <- expand.grid(
    lineend = c('round', 'butt', 'square'),
    linejoin = c('round', 'mitre', 'bevel'),
    stringsAsFactors = FALSE
  )
  arrow_types <- data.frame(arrow_types, y = 1:9)
  
  gain_chart <- gain_calc %>% 
    
    ggplot(aes(cumulative_data_fraction, value, color = key))+
    geom_line(size = 1.5)+
    theme_bw()+
    theme(legend.position = "bottom")+
    labs(title = "Gain Chart",
         x = "Cumulative Data Fraction", y = "Gain")+
    func_ggplot_axis_format()+
    scale_color_manual(values = c("#2c3e50", "#CE295E"))
}
#*******************************************************************************


  
gain_lift_manual_tbl <- ranked_predictions_tbl %>% 
    mutate(ntile = ntile(not.satisfied, n = 16)) %>% 
    group_by(ntile) %>% 
    summarise(
        cases = n(),
        responses = sum(satisfaction == "not satisfied")
    ) %>% 
    arrange(desc(ntile)) %>% 
    mutate(group = row_number()) %>% 
    select(group, cases, responses) %>% 
    mutate(
        cumulative_responses = cumsum(responses),
        pct_responses        = responses / sum(responses),
        gain                 = cumsum(pct_responses),
        cumulative_pct_cases = cumsum(cases) / sum(cases),
        lift                 = gain / cumulative_pct_cases,
        gain_baseline        = cumulative_pct_cases,
        lift_baseline        = gain_baseline / cumulative_pct_cases
        
    )

gain_lift_manual_tbl %>% 
    select(group, cumulative_pct_cases, gain, gain_baseline) %>% 
    gather(key = key, value = value, gain, gain_baseline ) %>% 
    ggplot(aes(cumulative_pct_cases, value, color = key))+
    geom_line(size = 1.5)


# gain & lift calculation: (h2o)
gain_lift_tbl <- performance_h2o %>% 
    h2o.gainsLift() %>% 
    as_tibble()

gain_lift_tbl %>% glimpse()

gain_transformed_tbl <- gain_lift_tbl %>% 
    select(group, cumulative_data_fraction, cumulative_capture_rate, cumulative_lift) %>% 
    select(-contains("lift")) %>% 
    mutate(baseline = cumulative_data_fraction) %>% 
    rename(gain = cumulative_capture_rate) %>% 
    gather(key = key, value = value, gain, baseline)

# gain & lift calculations
func_ggplot_axis_format <- function(){
    
    theme(axis.text.x = element_text(size = 10, face = "bold"),
          axis.text.y = element_text(size = 10, face = "bold"))
}

arrow_types <- expand.grid(
    lineend = c('round', 'butt', 'square'),
    linejoin = c('round', 'mitre', 'bevel'),
    stringsAsFactors = FALSE
)
arrow_types <- data.frame(arrow_types, y = 1:9)

gain_transformed_tbl %>% 
    ggplot(aes(cumulative_data_fraction, value, color = key))+
    geom_line(size = 1.5)+
    theme_bw()+
    theme(legend.position = "bottom")+
    labs(title = "Gain Chart",
         x = "Cumulative Data Fraction", y = "Gain")+
    func_ggplot_axis_format()+
    scale_color_manual(values = c("#2c3e50", "#CE295E"))+
    geom_segment(aes(
        x = 0.25, y = 0.45, xend = 0.25, yend = 0.27),
        arrow = arrow(length = unit(0.5, "cm")), size = 3, color = "#18bc9c",
        lineend = arrow_types$round, linejoin = arrow_types$round)+
    geom_segment(aes(
        x = 0.25, y = 0.27, xend = 0.25, yend = 0.54),
        arrow = arrow(length = unit(0.5, "cm")), size = 3, color = "#18bc9c",
        lineend = arrow_types$round, linejoin = arrow_types$round)+
    annotate(geom = "text", x = 0.35, y = 0.13, fontface = "bold", size = 7, color = "#2c3e50",
             label = str_glue("Gained
                               From: 25% (baseline)
                              To: 55% (model)"))
    # 
    # geom_segment(aes(
    #     x = 0.25, y = 0.80, xend = 0.25, yend = 0.60),
    #     arrow = arrow(length = unit(0.5, "cm")), size = 4, color = "#CE295E",
    #     lineend = arrow_types$round, linejoin = arrow_types$round)+
    # annotate(geom = "text", x = 0.10, y = 0.75, fontface = "bold", size = 5, color = "#CE295E",
    #          label = str_glue("Strategically, targeting 25% 
    #                           of high probability customers 
    #                           should yield 75% of potential 
    #                           unsatisfied customers"))


lift_transformed_tbl <- gain_lift_tbl %>% 
    select(group, cumulative_data_fraction, cumulative_capture_rate, cumulative_lift) %>% 
    select(-contains("capture")) %>% 
    mutate(baseline = 1) %>% 
    rename(lift = cumulative_lift) %>% 
    gather(key = key, value = value, lift, baseline)

lift_transformed_tbl

lift_transformed_tbl %>% 
    ggplot(aes(cumulative_data_fraction, value, color = key))+
    geom_line(size = 1.5)+
    theme_bw()+
    theme(legend.position = "bottom")+
    labs(title = "Lift Chart",
         x = "Cumulative Data Fraction", y = "Lift")+
    func_ggplot_axis_format()+
    scale_color_manual(values = c("#2c3e50", "#CE295E"))


# MODEL EXPLANATION WITH LIME ----

# load h2o model
automl_leader <- h2o.loadModel("05_Models/StackedEnsemble_BestOfFamily_AutoML_20210531_161514")

# making predictions
predictions_tbl <- automl_leader %>% 
    h2o.predict(newdata = test_h2o) %>% 
    as_tibble() %>% 
    bind_cols(
        test_raw_tbl %>% select(satisfaction, id)
    )

predictions_tbl

test_raw_tbl %>% slice(3) %>% glimpse()

# single explanation
explainer_obj <- train_tbl %>% 
    select(-satisfaction) %>% 
    lime(
        model          = automl_leader,
        bin_continous  = TRUE, # bins numeric features based on quantiles
        n_bins         = 4,
        quantile_bins  = TRUE # puts equal proportions into each bin
    )

explainer_obj    

options(scipen = 999)

explanation <- test_tbl %>% 
    slice(3) %>% 
    select(-satisfaction) %>% 
    lime::explain(
        explainer      = explainer_obj,
        n_labels       = 1,
        n_features     = 10,
        n_permutations = 5000,
        kernel_width   = 0.50,
        feature_select = "lasso_path"
    )

explanation %>% 
    as_tibble() %>% 
    select(feature:prediction)

plot_features(explanation = explanation, ncol = 1)

# multiple explanations

explanation <- test_tbl %>% 
    slice(1:10) %>% 
    select(-satisfaction) %>% 
    lime::explain(
        explainer      = explainer_obj,
        n_labels       = 1,
        n_features     = 10,
        n_permutations = 5000,
        kernel_width   = 0.50,
        feature_select = "lasso_path"
    )

explanation %>% tibble() 

plot_features(explanation, ncol = 3)

plot_explanations(explanation)



















