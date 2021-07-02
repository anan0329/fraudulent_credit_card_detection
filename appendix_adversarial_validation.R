library(tidymodels)
tidymodels_prefer(quiet = FALSE)

data <- readRDS("data/processed/pre_processed1.rds") %>% 
    mutate(is_test = if_else(locdt <= 70, 0, 1))

data %>%
    filter(is_test == 1) %>%
    select(bacno) %>%
    unique() %>%
    anti_join(data %>%
                  filter(is_test == 0) %>%
                  select(bacno) %>%
                  unique()) %>% 
    pull() -> new_bacno_list

data %>%
    filter(bacno %in% new_bacno_list) %>%
    mutate(new_user = 1) -> train_ad

'%!in%' <- function(x,y) !('%in%'(x,y))
data %>%
    filter(bacno %!in% new_bacno_list) %>%
    mutate(new_user = 0) -> test_ad

dat <- bind_rows(train_ad, test_ad)
dat$new_user <- factor(dat$new_user)
dat$ecfg <- if_else(dat$ecfg == "N", 0, 1)
dat$flbmk <- if_else(dat$flbmk == "Y", 1, 0, 0) # impute NA as mode N
dat$flg_3dsmk <- if_else(dat$flg_3dsmk == "Y", 1, 0, 0) # impute NA as mode N
dat$insfg <- if_else(dat$insfg == "N", 0, 1)
dat$ovrlt <- if_else(dat$ovrlt == "N", 0, 1)

dat_rec <- recipe(new_user ~ ., data = dat) %>%
    update_role(bacno , new_role = "customer variable") %>% 
    update_role(cano, new_role = "card variable") %>% 
    update_role(fraud_ind, new_role = "subtack target variable") %>% 
    update_role(locdt, new_role = "time variable") %>% 
    update_role(txkey, new_role = "id variable") %>%
    step_zv(all_predictors())

rf_model <- 
    rand_forest(trees = 100, mode = "classification") %>%
    set_engine("ranger")

dat_wflow <- 
    workflow() %>% 
    add_recipe(dat_rec) %>% 
    add_model(rf_model)

dat_fit <- 
    dat_wflow %>% 
    fit(data = dat)

dat_pred <- 
    predict(dat_fit, dat) %>% 
    bind_cols(dat %>% select(new_user, txkey))

f_meas(dat_pred, truth = new_user, estimate = .pred_class)
