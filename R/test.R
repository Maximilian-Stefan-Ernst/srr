pacman::p_load(tidyverse, tidymodels, here, magrittr)


# load and prepare data -------------------------------------------------------

df <- FFTrees::heartdisease
df %<>% mutate(diagnosis = as.factor(diagnosis))

part <- initial_split(df, prop = 0.5)
df_train <- training(part)
df_test <- testing(part)


dt_mod <- decision_tree(mode = "classification",
                    tree_depth = 6) %>% 
  set_engine("rpart")

rec <- recipe(diagnosis ~ ., data = df_train)


# decision tree -----------------------------------------------------------

dt_wf <- workflow() %>% 
  add_model(dt_mod) %>% 
  add_recipe(rec)


dt_fit <- fit(dt_wf, df_train)

dt_fit %>% pull_workflow_fit() %>% `$`(., fit) %>% rpart.plot::rpart.plot()

tree <- dt_fit %>% pull_workflow_fit() %>% `$`(., fit)

rec_obj <- rec %>% 
  step_tree2dummy(everything(), options = list(tree = tree)) %>%
  step_rm(-has_role("predictor_treedummy"), -has_role("outcome")) %>% 
  prep(training = df_train)

bake(rec_obj, df_train) %>% View()
