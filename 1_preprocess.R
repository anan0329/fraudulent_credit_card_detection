library(data.table)
setDTthreads(6)
library(dtplyr)
library(vroom)
library(tidyverse)

raw_data <- vroom("data/train.csv") %>% 
    mutate(loctm_pad = str_pad(
        loctm %>% as.integer(), width = 6, side = "left", pad = "0"), .before = loctm
    ) %>% 
    select(-loctm) %>% 
    separate(loctm_pad, c(paste0("loctm_", c("hr", "min", "sec"))), sep = c(2,4)) %>% 
    mutate(across(starts_with("loctm_"), as.numeric)) %>% 
    mutate(loctimes = locdt*24*60 + loctm_hr*60 + loctm_min + loctm_sec/60) %>% 
    arrange(bacno, cano, loctimes) %>% 
    select(bacno, cano, locdt, fraud_ind, loctimes, loctm_hr, loctm_min, loctm_sec, everything())
glimpse(raw_data)

data <- lazy_dt(raw_data)
dataTmp <- data %>% 
    mutate(n = row_number()) %>% 
    # select(n, cano, conam, loctimes, stocn, fraud_ind) %>% 
    group_by(cano) %>% 
    mutate(
        # 客戶第一次刷該信用卡
        cano_first_entrance = case_when(seq(n()) == 1 ~ 1,
                                        TRUE ~ 0), 
        # 與首次消費地國別不同
        stocn_dif_first = if_else(first(stocn) != stocn, 1, 0),
        res = conam - lag(conam), 
        # 本次與上次消費時間差
        res_use_interval = case_when(
            seq(n()) == 1 ~ 128271.6, # 第一次刷通常不是盜刷 (128151:1262)
            TRUE ~ round(loctimes - lag(loctimes), 2)
        ),
        amount_check = if_else(conam<=150 | (conam<=1100 & conam>=850), 1, 0),
        amount_t_stat = case_when(
            seq(n()) == 1 ~ 0,
            TRUE ~ (conam - cummean(conam))/sqrt(cumsum(conam^2)/seq(n()) - (cummean(conam))^2 + 1e-5)
        )
    ) %>% 
    as_tibble()

first_fraud_list <- data %>% 
    select(cano, fraud_ind) %>%
    group_by(cano) %>%
    slice(1) %>%
    filter(fraud_ind == 1) %>%
    select(cano) %>% 
    as_tibble()

data %>%
    filter(cano %in% first_fraud_list$cano) %>% 
    summarise(sum = sum(fraud_ind), cnt = length(fraud_ind))

data %>% 
    mutate(first_is_fraud = if_else(cano %in% first_fraud_list$cano == TRUE, 1, 0)) %>%
    group_by(first_is_fraud) %>%
    summarise(mean = mean(fraud_ind), var = var(fraud_ind), bacno_cnt = n_distinct(bacno))

# 與前次交易金額相同
same_amount_forward <- vector("integer", nrow(dataTmp))
i <- 2
while(i <= nrow(dataTmp)){
    if(!is.na(dataTmp$res[i]) & dataTmp$res[i] == 0 ){
        same_amount_forward[i-1] = same_amount_forward[i] = 1
    }else{
        same_amount_forward[i] == 0
    }
    i <- i + 1
}
prop.table(table(dataTmp$fraud_ind, same_amount_forward))

# 與首次消費地國別不同，且非另消費地國別第一次交易，與前次交易時間接近
# Todo

# 與前次交易時間接近，消費地國別不同
# Todo
dataTmp %>% 
    head(1e4) %>% 
    select(bacno, cano, stocn, scity, conam, loctimes, res_use_interval, fraud_ind) %>% 
    group_by(cano) %>% 
    mutate(has_fraud = sum(fraud_ind)) %>% 
    filter(has_fraud > 1) %>% 
    View()

dataTmp %<>% 
    bind_cols(same_amount_forward = same_amount_forward) %>% 
    mutate(
        day_1_7 = factor(locdt %% 7 + 1),
        doubt_same_amount_forward = if_else(same_amount_forward == 1 & (conam<150 | (800<conam & conam<1100)), 1, 0),
        res = NULL
    )

