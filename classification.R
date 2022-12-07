# train random forest classifier to predict social context from Action Unit combinations

library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(rsample)
library(parsnip)
library(recipes)
library(workflows)
library(yardstick)

# MaqFACS data
maqfacs <- read_tsv("maqfacs.txt")
# Action Units
AU <- c("10", "12", "16", "17", "18", "1plus2", "26", "27", "41", "43", "5", "6", "8", "9", "ad185", "ad19", "eau1", "eau2", "eau3", "ad101", "ad59")

# keep clear contexts
dc <-
  maqfacs %>%
  filter(context %in% c("affiliative", "aggressive", "submissive")) %>%
  mutate(across(species, factor, levels = c("rhesus", "barbary", "crested")))

d2 <-
  dc %>%
  select(species, context, all_of(AU)) %>%
  arrange(species, context) %>%
  mutate(across(context, factor)) %>%
  nest(data = -species)

# split training and test data
d3 <-
  d2 %>%
  mutate(
    split = map(data, ~initial_split(.x, strata = context, prop = 0.7)),
    train = map(split, ~training(.x)),
    test =  map(split, ~testing(.x))
  )

# model specification
rf_mod <-
  rand_forest(mode = "classification",
              mtry = floor(sqrt(length(AU))),
              trees = 500L,
              min_n = 10) %>%
  set_engine(engine = "ranger",
             importance = "permutation")

parallel::detectCores(logical = TRUE)
library(doMC)
registerDoMC(cores = 8)

# prepare training set and train model
d4 <-
  d3 %>%
  mutate(
    d_recipe = map(
      train,
      ~recipe(context ~ ., data = .x) %>%
        themis::step_smote(context)
    ),
    d_prep = map(
      d_recipe,
      ~prep(.x)
    ),
    d_wf = map(
      d_prep,
      ~workflow() %>%
        add_recipe(.x) %>%
        add_model(rf_mod)
    ),
    rf_fit_smote = map2(
      train, d_wf,
      ~fit(.y, data = .x)
    )
  )

# predict on test data and get model performance metrics
d5 <-
  d4 %>%
  mutate(
    rf_pred_smote = map2(
      test, rf_fit_smote,
      ~.x %>%
        select(context) %>%
        bind_cols(predict(.y, new_data = .x[, AU]))
    ),
    rf_metrics_smote = map(
      rf_pred_smote,
      ~.x %>%
        metrics(truth = context, estimate = .pred_class)
    ),
    conf_mat_smote = map(
      rf_pred_smote,
      ~conf_mat(.x, context, .pred_class))
  )


# check metrics
d5 %>%
  select(species, rf_metrics_smote) %>%
  unnest(cols = c(species, rf_metrics_smote)) %>%
  filter(.metric == "kap")

# confusion matrix
d5$conf_mat_smote %>%
  set_names(d5$species)

