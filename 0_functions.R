

decompose_dr_xfit_intA <- function(data, formula_treatment, preds_outcome, sl_models) {
  
  # select complete cases
  data <- data %>% na.omit()
  
  # determine counter-factual composition via intervention rule
  composition_cf <- multinom("eseg_class ~ 1", data = data %>% filter(female == 0), family = "binomial", weights = weight)
  
  composition_cf <- as.data.frame(predict(composition_cf, type = "probs")) %>% slice(1)
  
  colnames(composition_cf) <- paste0("counterfact_share_occ_", colnames(composition_cf))
  
  data <- data %>% cross_join(composition_cf)
  
  rm(list = setdiff(ls(), c("data", "formula_treatment", "preds_outcome", "sl_models")))
  
  
  
  # create cross-fitting splits
  split <- split(data, f = rep_len(1:5, nrow(data)))
  
  data_split_1 = rbind(split$`2`, split$`3`, split$`4`, split$`5`)
  data_split_1_heldout = split$`1`
  
  data_split_2 = rbind(split$`1`, split$`3`, split$`4`, split$`5`)
  data_split_2_heldout = split$`2`
  
  data_split_3 = rbind(split$`1`, split$`2`, split$`4`, split$`5`)
  data_split_3_heldout = split$`3`
  
  data_split_4 = rbind(split$`1`, split$`2`, split$`3`, split$`5`)
  data_split_4_heldout = split$`4`
  
  data_split_5 = rbind(split$`1`, split$`2`, split$`3`, split$`4`)
  data_split_5_heldout = split$`5`
  
  rm(split)
  
  
  
  # treatment model
  data_split_1_male <- data_split_1 %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_2_male <- data_split_2 %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_3_male <- data_split_3 %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_4_male <- data_split_4 %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_5_male <- data_split_5 %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  
  data_split_1_female <- data_split_1 %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_2_female <- data_split_2 %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_3_female <- data_split_3 %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_4_female <- data_split_4 %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_5_female <- data_split_5 %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  
  data_split_1_heldout_male <- data_split_1_heldout %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_2_heldout_male <- data_split_2_heldout %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_3_heldout_male <- data_split_3_heldout %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_4_heldout_male <- data_split_4_heldout %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_5_heldout_male <- data_split_5_heldout %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  
  data_split_1_heldout_female <- data_split_1_heldout %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_2_heldout_female <- data_split_2_heldout %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_3_heldout_female <- data_split_3_heldout %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_4_heldout_female <- data_split_4_heldout %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_5_heldout_female <- data_split_5_heldout %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  
  # train
  # men
  model_male_split_1 <- multinom(formula_treatment, data = data_split_1_male, family = "binomial", weights = weight)
  model_male_split_2 <- multinom(formula_treatment, data = data_split_2_male, family = "binomial", weights = weight)
  model_male_split_3 <- multinom(formula_treatment, data = data_split_3_male, family = "binomial", weights = weight)
  model_male_split_4 <- multinom(formula_treatment, data = data_split_4_male, family = "binomial", weights = weight)
  model_male_split_5 <- multinom(formula_treatment, data = data_split_5_male, family = "binomial", weights = weight)
  
  # women
  model_female_split_1 <- multinom(formula_treatment, data = data_split_1_female, family = "binomial", weights = weight)
  model_female_split_2 <- multinom(formula_treatment, data = data_split_2_female, family = "binomial", weights = weight)
  model_female_split_3 <- multinom(formula_treatment, data = data_split_3_female, family = "binomial", weights = weight)
  model_female_split_4 <- multinom(formula_treatment, data = data_split_4_female, family = "binomial", weights = weight)
  model_female_split_5 <- multinom(formula_treatment, data = data_split_5_female, family = "binomial", weights = weight)
  
  # predict using model trained on other split
  # men
  data_split_1_heldout_male <- cbind(data_split_1_heldout_male, as.data.frame(predict(model_male_split_1, newdata = data_split_1_heldout_male, type = "probs")))
  colnames(data_split_1_heldout_male)[32:38] = paste0("prop_occ_", colnames(data_split_1_heldout_male)[32:38])
  
  data_split_2_heldout_male <- cbind(data_split_2_heldout_male, as.data.frame(predict(model_male_split_2, newdata = data_split_2_heldout_male, type = "probs")))
  colnames(data_split_2_heldout_male)[32:38] = paste0("prop_occ_", colnames(data_split_2_heldout_male)[32:38])
  
  data_split_3_heldout_male <- cbind(data_split_3_heldout_male, as.data.frame(predict(model_male_split_3, newdata = data_split_3_heldout_male, type = "probs")))
  colnames(data_split_3_heldout_male)[32:38] = paste0("prop_occ_", colnames(data_split_3_heldout_male)[32:38])
  
  data_split_4_heldout_male <- cbind(data_split_4_heldout_male, as.data.frame(predict(model_male_split_4, newdata = data_split_4_heldout_male, type = "probs")))
  colnames(data_split_4_heldout_male)[32:38] = paste0("prop_occ_", colnames(data_split_4_heldout_male)[32:38])
  
  data_split_5_heldout_male <- cbind(data_split_5_heldout_male, as.data.frame(predict(model_male_split_5, newdata = data_split_5_heldout_male, type = "probs")))
  colnames(data_split_5_heldout_male)[32:38] = paste0("prop_occ_", colnames(data_split_5_heldout_male)[32:38])
  
  # women
  data_split_1_heldout_female <- cbind(data_split_1_heldout_female, as.data.frame(predict(model_female_split_1, newdata = data_split_1_heldout_female, type = "probs")))
  colnames(data_split_1_heldout_female)[32:38] = paste0("prop_occ_", colnames(data_split_1_heldout_female)[32:38])
  
  data_split_2_heldout_female <- cbind(data_split_2_heldout_female, as.data.frame(predict(model_female_split_2, newdata = data_split_2_heldout_female, type = "probs")))
  colnames(data_split_2_heldout_female)[32:38] = paste0("prop_occ_", colnames(data_split_2_heldout_female)[32:38])
  
  data_split_3_heldout_female <- cbind(data_split_3_heldout_female, as.data.frame(predict(model_female_split_3, newdata = data_split_3_heldout_female, type = "probs")))
  colnames(data_split_3_heldout_female)[32:38] = paste0("prop_occ_", colnames(data_split_3_heldout_female)[32:38])
  
  data_split_4_heldout_female <- cbind(data_split_4_heldout_female, as.data.frame(predict(model_female_split_4, newdata = data_split_4_heldout_female, type = "probs")))
  colnames(data_split_4_heldout_female)[32:38] = paste0("prop_occ_", colnames(data_split_4_heldout_female)[32:38])
  
  data_split_5_heldout_female <- cbind(data_split_5_heldout_female, as.data.frame(predict(model_female_split_5, newdata = data_split_5_heldout_female, type = "probs")))
  colnames(data_split_5_heldout_female)[32:38] = paste0("prop_occ_", colnames(data_split_5_heldout_female)[32:38])
  
  # combine
  data_split_1_heldout <- rbind(data_split_1_heldout_male, data_split_1_heldout_female)
  data_split_2_heldout <- rbind(data_split_2_heldout_male, data_split_2_heldout_female)
  data_split_3_heldout <- rbind(data_split_3_heldout_male, data_split_3_heldout_female)
  data_split_4_heldout <- rbind(data_split_4_heldout_male, data_split_4_heldout_female)
  data_split_5_heldout <- rbind(data_split_5_heldout_male, data_split_5_heldout_female)
  
  data_split_1_heldout <- data_split_1_heldout %>% as.data.frame() %>% ungroup()
  data_split_2_heldout <- data_split_2_heldout %>% as.data.frame() %>% ungroup()
  data_split_3_heldout <- data_split_3_heldout %>% as.data.frame() %>% ungroup()
  data_split_4_heldout <- data_split_4_heldout %>% as.data.frame() %>% ungroup()
  data_split_5_heldout <- data_split_5_heldout %>% as.data.frame() %>% ungroup()
  
  
  # train
  # men
  model_male_split_1 <- SuperLearner(Y = data_split_1_male$outcome, X = data_split_1_male %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_1_male$weight, cvControl = list(V = 5))
  model_male_split_2 <- SuperLearner(Y = data_split_2_male$outcome, X = data_split_2_male %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_2_male$weight, cvControl = list(V = 5))
  model_male_split_3 <- SuperLearner(Y = data_split_3_male$outcome, X = data_split_3_male %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_3_male$weight, cvControl = list(V = 5))
  model_male_split_4 <- SuperLearner(Y = data_split_4_male$outcome, X = data_split_4_male %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_4_male$weight, cvControl = list(V = 5))
  model_male_split_5 <- SuperLearner(Y = data_split_5_male$outcome, X = data_split_5_male %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_5_male$weight, cvControl = list(V = 5))
  
  # women
  model_female_split_1 <- SuperLearner(Y = data_split_1_female$outcome, X = data_split_1_female %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_1_female$weight, cvControl = list(V = 5))
  model_female_split_2 <- SuperLearner(Y = data_split_2_female$outcome, X = data_split_2_female %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_2_female$weight, cvControl = list(V = 5))
  model_female_split_3 <- SuperLearner(Y = data_split_3_female$outcome, X = data_split_3_female %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_3_female$weight, cvControl = list(V = 5))
  model_female_split_4 <- SuperLearner(Y = data_split_4_female$outcome, X = data_split_4_female %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_4_female$weight, cvControl = list(V = 5))
  model_female_split_5 <- SuperLearner(Y = data_split_5_female$outcome, X = data_split_5_female %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_5_female$weight, cvControl = list(V = 5))
  
  # predictions
  for (i in sort(unique(data$eseg_class))) {
    
    data_split_1_heldout_male$eseg_class <- factor(i, levels = 1:7)
    data_split_2_heldout_male$eseg_class <- factor(i, levels = 1:7)
    data_split_3_heldout_male$eseg_class <- factor(i, levels = 1:7)
    data_split_4_heldout_male$eseg_class <- factor(i, levels = 1:7)
    data_split_5_heldout_male$eseg_class <- factor(i, levels = 1:7)
    
    data_split_1_heldout_female$eseg_class <- factor(i, levels = 1:7)
    data_split_2_heldout_female$eseg_class <- factor(i, levels = 1:7)
    data_split_3_heldout_female$eseg_class <- factor(i, levels = 1:7)
    data_split_4_heldout_female$eseg_class <- factor(i, levels = 1:7)
    data_split_5_heldout_female$eseg_class <- factor(i, levels = 1:7)
    
    data_split_1_heldout_male$prediction <- as.data.frame(predict(model_male_split_1, newdata = data_split_1_heldout_male %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_2_heldout_male$prediction <- as.data.frame(predict(model_male_split_2, newdata = data_split_2_heldout_male %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_3_heldout_male$prediction <- as.data.frame(predict(model_male_split_3, newdata = data_split_3_heldout_male %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_4_heldout_male$prediction <- as.data.frame(predict(model_male_split_4, newdata = data_split_4_heldout_male %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_5_heldout_male$prediction <- as.data.frame(predict(model_male_split_5, newdata = data_split_5_heldout_male %>% select(all_of(preds_outcome)), type = "response"))$pred
    
    data_split_1_heldout_female$prediction <- as.data.frame(predict(model_female_split_1, newdata = data_split_1_heldout_female %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_2_heldout_female$prediction <- as.data.frame(predict(model_female_split_2, newdata = data_split_2_heldout_female %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_3_heldout_female$prediction <- as.data.frame(predict(model_female_split_3, newdata = data_split_3_heldout_female %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_4_heldout_female$prediction <- as.data.frame(predict(model_female_split_4, newdata = data_split_4_heldout_female %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_5_heldout_female$prediction <- as.data.frame(predict(model_female_split_5, newdata = data_split_5_heldout_female %>% select(all_of(preds_outcome)), type = "response"))$pred
    
    data_split_1_heldout_male <- data_split_1_heldout_male %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_2_heldout_male <- data_split_2_heldout_male %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_3_heldout_male <- data_split_3_heldout_male %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_4_heldout_male <- data_split_4_heldout_male %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_5_heldout_male <- data_split_5_heldout_male %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    
    data_split_1_heldout_female <- data_split_1_heldout_female %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_2_heldout_female <- data_split_2_heldout_female %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_3_heldout_female <- data_split_3_heldout_female %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_4_heldout_female <- data_split_4_heldout_female %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_5_heldout_female <- data_split_5_heldout_female %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    
  }
  
  rm(model_male_split_1, model_male_split_2, model_male_split_3, model_male_split_4, model_male_split_5, model_female_split_1, model_female_split_2, model_female_split_3, model_female_split_4, model_female_split_5)
  
  rm(i)
  
  # combine
  preds_split_1 <- rbind(data_split_1_heldout_male, data_split_1_heldout_female)
  preds_split_2 <- rbind(data_split_2_heldout_male, data_split_2_heldout_female)
  preds_split_3 <- rbind(data_split_3_heldout_male, data_split_3_heldout_female)
  preds_split_4 <- rbind(data_split_4_heldout_male, data_split_4_heldout_female)
  preds_split_5 <- rbind(data_split_5_heldout_male, data_split_5_heldout_female)
  
  preds_split_1 <- preds_split_1 %>% select(orgpid, age, contains("exp_outcome_occ_")) %>% group_by(orgpid, age) %>% slice(1)
  preds_split_2 <- preds_split_2 %>% select(orgpid, age, contains("exp_outcome_occ_")) %>% group_by(orgpid, age) %>% slice(1)
  preds_split_3 <- preds_split_3 %>% select(orgpid, age, contains("exp_outcome_occ_")) %>% group_by(orgpid, age) %>% slice(1)
  preds_split_4 <- preds_split_4 %>% select(orgpid, age, contains("exp_outcome_occ_")) %>% group_by(orgpid, age) %>% slice(1)
  preds_split_5 <- preds_split_5 %>% select(orgpid, age, contains("exp_outcome_occ_")) %>% group_by(orgpid, age) %>% slice(1)
  
  data_split_1_heldout <- data_split_1_heldout %>% left_join(preds_split_1, by = c("orgpid", "age")) %>% ungroup()
  data_split_2_heldout <- data_split_2_heldout %>% left_join(preds_split_2, by = c("orgpid", "age")) %>% ungroup()
  data_split_3_heldout <- data_split_3_heldout %>% left_join(preds_split_3, by = c("orgpid", "age")) %>% ungroup()
  data_split_4_heldout <- data_split_4_heldout %>% left_join(preds_split_4, by = c("orgpid", "age")) %>% ungroup()
  data_split_5_heldout <- data_split_5_heldout %>% left_join(preds_split_5, by = c("orgpid", "age")) %>% ungroup()
  
  
  
  # counterfactual probability of being assigned to the factual occupation
  cf_prob_fc_occ <- data %>% ungroup() %>%
    select(orgpid, age, eseg_class, contains("counterfact_share_occ")) %>%
    pivot_longer(cols = c(4:10), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 23, 23)) %>% 
    filter(eseg_class == occupation) %>%
    rename("cf_prob_fc_occ" = "value") %>%
    select(orgpid, age, cf_prob_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  # inverse of the factual probability of the factual occupation
  inv_fc_prob_fc_occ_split_1 <- data_split_1_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class, contains("prop_occ")) %>%
    pivot_longer(cols = c(4:10), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 10, 10)) %>% 
    filter(eseg_class == occupation) %>%
    mutate(value = 1 / value) %>%
    rename("inv_fc_prob_fc_occ" = "value") %>%
    select(orgpid, age, inv_fc_prob_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup() %>%
    mutate(inv_fc_prob_fc_occ = case_when(inv_fc_prob_fc_occ > quantile(inv_fc_prob_fc_occ, 0.999) ~ quantile(inv_fc_prob_fc_occ, 0.999),
                                          inv_fc_prob_fc_occ < quantile(inv_fc_prob_fc_occ, 0.001) ~ quantile(inv_fc_prob_fc_occ, 0.001),
                                          T ~ inv_fc_prob_fc_occ))
  
  inv_fc_prob_fc_occ_split_2 <- data_split_2_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class, contains("prop_occ")) %>%
    pivot_longer(cols = c(4:10), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 10, 10)) %>% 
    filter(eseg_class == occupation) %>%
    mutate(value = 1 / value) %>%
    rename("inv_fc_prob_fc_occ" = "value") %>%
    select(orgpid, age, inv_fc_prob_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup() %>%
    mutate(inv_fc_prob_fc_occ = case_when(inv_fc_prob_fc_occ > quantile(inv_fc_prob_fc_occ, 0.999) ~ quantile(inv_fc_prob_fc_occ, 0.999),
                                          inv_fc_prob_fc_occ < quantile(inv_fc_prob_fc_occ, 0.001) ~ quantile(inv_fc_prob_fc_occ, 0.001),
                                          T ~ inv_fc_prob_fc_occ))
  
  inv_fc_prob_fc_occ_split_3 <- data_split_3_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class, contains("prop_occ")) %>%
    pivot_longer(cols = c(4:10), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 10, 10)) %>% 
    filter(eseg_class == occupation) %>%
    mutate(value = 1 / value) %>%
    rename("inv_fc_prob_fc_occ" = "value") %>%
    select(orgpid, age, inv_fc_prob_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup() %>%
    mutate(inv_fc_prob_fc_occ = case_when(inv_fc_prob_fc_occ > quantile(inv_fc_prob_fc_occ, 0.999) ~ quantile(inv_fc_prob_fc_occ, 0.999),
                                          inv_fc_prob_fc_occ < quantile(inv_fc_prob_fc_occ, 0.001) ~ quantile(inv_fc_prob_fc_occ, 0.001),
                                          T ~ inv_fc_prob_fc_occ))
  
  inv_fc_prob_fc_occ_split_4 <- data_split_4_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class, contains("prop_occ")) %>%
    pivot_longer(cols = c(4:10), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 10, 10)) %>% 
    filter(eseg_class == occupation) %>%
    mutate(value = 1 / value) %>%
    rename("inv_fc_prob_fc_occ" = "value") %>%
    select(orgpid, age, inv_fc_prob_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup() %>%
    mutate(inv_fc_prob_fc_occ = case_when(inv_fc_prob_fc_occ > quantile(inv_fc_prob_fc_occ, 0.999) ~ quantile(inv_fc_prob_fc_occ, 0.999),
                                          inv_fc_prob_fc_occ < quantile(inv_fc_prob_fc_occ, 0.001) ~ quantile(inv_fc_prob_fc_occ, 0.001),
                                          T ~ inv_fc_prob_fc_occ))
  
  inv_fc_prob_fc_occ_split_5 <- data_split_5_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class, contains("prop_occ")) %>%
    pivot_longer(cols = c(4:10), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 10, 10)) %>% 
    filter(eseg_class == occupation) %>%
    mutate(value = 1 / value) %>%
    rename("inv_fc_prob_fc_occ" = "value") %>%
    select(orgpid, age, inv_fc_prob_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup() %>%
    mutate(inv_fc_prob_fc_occ = case_when(inv_fc_prob_fc_occ > quantile(inv_fc_prob_fc_occ, 0.999) ~ quantile(inv_fc_prob_fc_occ, 0.999),
                                          inv_fc_prob_fc_occ < quantile(inv_fc_prob_fc_occ, 0.001) ~ quantile(inv_fc_prob_fc_occ, 0.001),
                                          T ~ inv_fc_prob_fc_occ))
  
  # outcome predictions
  exp_outcome_fc_occ_split_1 <- data_split_1_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class, contains("exp_outcome_occ")) %>%
    pivot_longer(cols = c(4:10), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 17, 17)) %>% 
    filter(eseg_class == occupation) %>%
    rename("exp_outcome_fc_occ" = "value") %>%
    select(orgpid, age, exp_outcome_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  exp_outcome_fc_occ_split_2 <- data_split_2_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class, contains("exp_outcome_occ")) %>%
    pivot_longer(cols = c(4:10), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 17, 17)) %>% 
    filter(eseg_class == occupation) %>%
    rename("exp_outcome_fc_occ" = "value") %>%
    select(orgpid, age, exp_outcome_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  exp_outcome_fc_occ_split_3 <- data_split_3_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class, contains("exp_outcome_occ")) %>%
    pivot_longer(cols = c(4:10), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 17, 17)) %>% 
    filter(eseg_class == occupation) %>%
    rename("exp_outcome_fc_occ" = "value") %>%
    select(orgpid, age, exp_outcome_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  exp_outcome_fc_occ_split_4 <- data_split_4_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class, contains("exp_outcome_occ")) %>%
    pivot_longer(cols = c(4:10), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 17, 17)) %>% 
    filter(eseg_class == occupation) %>%
    rename("exp_outcome_fc_occ" = "value") %>%
    select(orgpid, age, exp_outcome_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  exp_outcome_fc_occ_split_5 <- data_split_5_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class, contains("exp_outcome_occ")) %>%
    pivot_longer(cols = c(4:10), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 17, 17)) %>% 
    filter(eseg_class == occupation) %>%
    rename("exp_outcome_fc_occ" = "value") %>%
    select(orgpid, age, exp_outcome_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  # create combined weight
  weight_split_1 <- data_split_1_heldout %>% ungroup() %>%
    select(orgpid, age, weight) %>% 
    left_join(cf_prob_fc_occ, by = c("orgpid", "age")) %>%
    left_join(inv_fc_prob_fc_occ_split_1, by = c("orgpid", "age")) %>%
    mutate(combined_weight = weight * cf_prob_fc_occ * inv_fc_prob_fc_occ) %>%
    select(orgpid, age, combined_weight) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  weight_split_2 <- data_split_2_heldout %>% ungroup() %>%
    select(orgpid, age, weight) %>% 
    left_join(cf_prob_fc_occ, by = c("orgpid", "age")) %>%
    left_join(inv_fc_prob_fc_occ_split_2, by = c("orgpid", "age")) %>%
    mutate(combined_weight = weight * cf_prob_fc_occ * inv_fc_prob_fc_occ) %>%
    select(orgpid, age, combined_weight) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  weight_split_3 <- data_split_3_heldout %>% ungroup() %>%
    select(orgpid, age, weight) %>% 
    left_join(cf_prob_fc_occ, by = c("orgpid", "age")) %>%
    left_join(inv_fc_prob_fc_occ_split_3, by = c("orgpid", "age")) %>%
    mutate(combined_weight = weight * cf_prob_fc_occ * inv_fc_prob_fc_occ) %>%
    select(orgpid, age, combined_weight) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  weight_split_4 <- data_split_4_heldout %>% ungroup() %>%
    select(orgpid, age, weight) %>% 
    left_join(cf_prob_fc_occ, by = c("orgpid", "age")) %>%
    left_join(inv_fc_prob_fc_occ_split_4, by = c("orgpid", "age")) %>%
    mutate(combined_weight = weight * cf_prob_fc_occ * inv_fc_prob_fc_occ) %>%
    select(orgpid, age, combined_weight) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  weight_split_5 <- data_split_5_heldout %>% ungroup() %>%
    select(orgpid, age, weight) %>% 
    left_join(cf_prob_fc_occ, by = c("orgpid", "age")) %>%
    left_join(inv_fc_prob_fc_occ_split_5, by = c("orgpid", "age")) %>%
    mutate(combined_weight = weight * cf_prob_fc_occ * inv_fc_prob_fc_occ) %>%
    select(orgpid, age, combined_weight) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  # join
  data_split_1_heldout <- data_split_1_heldout %>% left_join(exp_outcome_fc_occ_split_1, by = c("orgpid", "age")) %>% left_join(weight_split_1, by = c("orgpid", "age")) %>% ungroup()
  data_split_2_heldout <- data_split_2_heldout %>% left_join(exp_outcome_fc_occ_split_2, by = c("orgpid", "age")) %>% left_join(weight_split_2, by = c("orgpid", "age")) %>% ungroup()
  data_split_3_heldout <- data_split_3_heldout %>% left_join(exp_outcome_fc_occ_split_3, by = c("orgpid", "age")) %>% left_join(weight_split_3, by = c("orgpid", "age")) %>% ungroup()
  data_split_4_heldout <- data_split_4_heldout %>% left_join(exp_outcome_fc_occ_split_4, by = c("orgpid", "age")) %>% left_join(weight_split_4, by = c("orgpid", "age")) %>% ungroup()
  data_split_5_heldout <- data_split_5_heldout %>% left_join(exp_outcome_fc_occ_split_5, by = c("orgpid", "age")) %>% left_join(weight_split_5, by = c("orgpid", "age")) %>% ungroup()
  
  # calculate bias correction
  bias_correction_m_split_1 <- data_split_1_heldout %>% filter(female == 0) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_m_split_2 <- data_split_2_heldout %>% filter(female == 0) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_m_split_3 <- data_split_3_heldout %>% filter(female == 0) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_m_split_4 <- data_split_4_heldout %>% filter(female == 0) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_m_split_5 <- data_split_5_heldout %>% filter(female == 0) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_f_split_1 <- data_split_1_heldout %>% filter(female == 1) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_f_split_2 <- data_split_2_heldout %>% filter(female == 1) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_f_split_3 <- data_split_3_heldout %>% filter(female == 1) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_f_split_4 <- data_split_4_heldout %>% filter(female == 1) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_f_split_5 <- data_split_5_heldout %>% filter(female == 1) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_split_1 <- rbind(bias_correction_m_split_1, bias_correction_f_split_1)
  bias_correction_split_2 <- rbind(bias_correction_m_split_2, bias_correction_f_split_2)
  bias_correction_split_3 <- rbind(bias_correction_m_split_3, bias_correction_f_split_3)
  bias_correction_split_4 <- rbind(bias_correction_m_split_4, bias_correction_f_split_4)
  bias_correction_split_5 <- rbind(bias_correction_m_split_5, bias_correction_f_split_5)
  
  # correct bias in outcome predictions
  data_split_1_heldout <- data_split_1_heldout %>% 
    left_join(bias_correction_split_1, by = "female") %>% ungroup() %>%
    mutate_at(vars(contains("exp_outcome_occ_")), ~(. - bias_correction)) %>% 
    mutate_at(vars(contains("exp_outcome_occ_")), ~(case_when(. > 1 ~ 1, . < 0 ~ 0, T ~ .)))
  
  data_split_2_heldout <- data_split_2_heldout %>% 
    left_join(bias_correction_split_2, by = "female") %>% ungroup() %>%
    mutate_at(vars(contains("exp_outcome_occ_")), ~(. - bias_correction)) %>% 
    mutate_at(vars(contains("exp_outcome_occ_")), ~(case_when(. > 1 ~ 1, . < 0 ~ 0, T ~ .)))
  
  data_split_3_heldout <- data_split_3_heldout %>% 
    left_join(bias_correction_split_3, by = "female") %>% ungroup() %>%
    mutate_at(vars(contains("exp_outcome_occ_")), ~(. - bias_correction)) %>% 
    mutate_at(vars(contains("exp_outcome_occ_")), ~(case_when(. > 1 ~ 1, . < 0 ~ 0, T ~ .)))
  
  data_split_4_heldout <- data_split_4_heldout %>% 
    left_join(bias_correction_split_4, by = "female") %>% ungroup() %>%
    mutate_at(vars(contains("exp_outcome_occ_")), ~(. - bias_correction)) %>% 
    mutate_at(vars(contains("exp_outcome_occ_")), ~(case_when(. > 1 ~ 1, . < 0 ~ 0, T ~ .)))
  
  data_split_5_heldout <- data_split_5_heldout %>% 
    left_join(bias_correction_split_5, by = "female") %>% ungroup() %>%
    mutate_at(vars(contains("exp_outcome_occ_")), ~(. - bias_correction)) %>% 
    mutate_at(vars(contains("exp_outcome_occ_")), ~(case_when(. > 1 ~ 1, . < 0 ~ 0, T ~ .)))
  
  # pre-intervention
  levels_pre_female <- data %>% filter(female == 1) %>% ungroup() %>%summarize(exp = weighted.mean(outcome, w = weight) * 100) %>% as.numeric()
  levels_pre_male <- data %>% filter(female == 0) %>% ungroup() %>%summarize(exp = weighted.mean(outcome, w = weight) * 100) %>% as.numeric()
  
  # post-intervention
  # split 1
  levels_post_female_split_1 <- data_split_1_heldout %>% filter(female == 1) %>% mutate(exp = (exp_outcome_occ_1 * counterfact_share_occ_1) + (exp_outcome_occ_2 * counterfact_share_occ_2) + (exp_outcome_occ_3 * counterfact_share_occ_3) + (exp_outcome_occ_4 * counterfact_share_occ_4) + (exp_outcome_occ_5 * counterfact_share_occ_5) + (exp_outcome_occ_6 * counterfact_share_occ_6) + (exp_outcome_occ_7 * counterfact_share_occ_7)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  levels_post_male_split_1 <- data_split_1_heldout %>% filter(female == 0) %>% mutate(exp = (exp_outcome_occ_1 * counterfact_share_occ_1) + (exp_outcome_occ_2 * counterfact_share_occ_2) + (exp_outcome_occ_3 * counterfact_share_occ_3) + (exp_outcome_occ_4 * counterfact_share_occ_4) + (exp_outcome_occ_5 * counterfact_share_occ_5) + (exp_outcome_occ_6 * counterfact_share_occ_6) + (exp_outcome_occ_7 * counterfact_share_occ_7)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  
  # split 2
  levels_post_female_split_2 <- data_split_2_heldout %>% filter(female == 1) %>% mutate(exp = (exp_outcome_occ_1 * counterfact_share_occ_1) + (exp_outcome_occ_2 * counterfact_share_occ_2) + (exp_outcome_occ_3 * counterfact_share_occ_3) + (exp_outcome_occ_4 * counterfact_share_occ_4) + (exp_outcome_occ_5 * counterfact_share_occ_5) + (exp_outcome_occ_6 * counterfact_share_occ_6) + (exp_outcome_occ_7 * counterfact_share_occ_7)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  levels_post_male_split_2 <- data_split_2_heldout %>% filter(female == 0) %>% mutate(exp = (exp_outcome_occ_1 * counterfact_share_occ_1) + (exp_outcome_occ_2 * counterfact_share_occ_2) + (exp_outcome_occ_3 * counterfact_share_occ_3) + (exp_outcome_occ_4 * counterfact_share_occ_4) + (exp_outcome_occ_5 * counterfact_share_occ_5) + (exp_outcome_occ_6 * counterfact_share_occ_6) + (exp_outcome_occ_7 * counterfact_share_occ_7)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  
  # split 3
  levels_post_female_split_3 <- data_split_3_heldout %>% filter(female == 1) %>% mutate(exp = (exp_outcome_occ_1 * counterfact_share_occ_1) + (exp_outcome_occ_2 * counterfact_share_occ_2) + (exp_outcome_occ_3 * counterfact_share_occ_3) + (exp_outcome_occ_4 * counterfact_share_occ_4) + (exp_outcome_occ_5 * counterfact_share_occ_5) + (exp_outcome_occ_6 * counterfact_share_occ_6) + (exp_outcome_occ_7 * counterfact_share_occ_7)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  levels_post_male_split_3 <- data_split_3_heldout %>% filter(female == 0) %>% mutate(exp = (exp_outcome_occ_1 * counterfact_share_occ_1) + (exp_outcome_occ_2 * counterfact_share_occ_2) + (exp_outcome_occ_3 * counterfact_share_occ_3) + (exp_outcome_occ_4 * counterfact_share_occ_4) + (exp_outcome_occ_5 * counterfact_share_occ_5) + (exp_outcome_occ_6 * counterfact_share_occ_6) + (exp_outcome_occ_7 * counterfact_share_occ_7)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  
  # split 4
  levels_post_female_split_4 <- data_split_4_heldout %>% filter(female == 1) %>% mutate(exp = (exp_outcome_occ_1 * counterfact_share_occ_1) + (exp_outcome_occ_2 * counterfact_share_occ_2) + (exp_outcome_occ_3 * counterfact_share_occ_3) + (exp_outcome_occ_4 * counterfact_share_occ_4) + (exp_outcome_occ_5 * counterfact_share_occ_5) + (exp_outcome_occ_6 * counterfact_share_occ_6) + (exp_outcome_occ_7 * counterfact_share_occ_7)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  levels_post_male_split_4 <- data_split_4_heldout %>% filter(female == 0) %>% mutate(exp = (exp_outcome_occ_1 * counterfact_share_occ_1) + (exp_outcome_occ_2 * counterfact_share_occ_2) + (exp_outcome_occ_3 * counterfact_share_occ_3) + (exp_outcome_occ_4 * counterfact_share_occ_4) + (exp_outcome_occ_5 * counterfact_share_occ_5) + (exp_outcome_occ_6 * counterfact_share_occ_6) + (exp_outcome_occ_7 * counterfact_share_occ_7)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  
  # split 5
  levels_post_female_split_5 <- data_split_5_heldout %>% filter(female == 1) %>% mutate(exp = (exp_outcome_occ_1 * counterfact_share_occ_1) + (exp_outcome_occ_2 * counterfact_share_occ_2) + (exp_outcome_occ_3 * counterfact_share_occ_3) + (exp_outcome_occ_4 * counterfact_share_occ_4) + (exp_outcome_occ_5 * counterfact_share_occ_5) + (exp_outcome_occ_6 * counterfact_share_occ_6) + (exp_outcome_occ_7 * counterfact_share_occ_7)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  levels_post_male_split_5 <- data_split_5_heldout %>% filter(female == 0) %>% mutate(exp = (exp_outcome_occ_1 * counterfact_share_occ_1) + (exp_outcome_occ_2 * counterfact_share_occ_2) + (exp_outcome_occ_3 * counterfact_share_occ_3) + (exp_outcome_occ_4 * counterfact_share_occ_4) + (exp_outcome_occ_5 * counterfact_share_occ_5) + (exp_outcome_occ_6 * counterfact_share_occ_6) + (exp_outcome_occ_7 * counterfact_share_occ_7)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  
  # average cross-fits
  levels_post_female <- (levels_post_female_split_1 + levels_post_female_split_2 + levels_post_female_split_3 + levels_post_female_split_4 + levels_post_female_split_5) / 5
  levels_post_male <- (levels_post_male_split_1 + levels_post_male_split_2 + levels_post_male_split_3 + levels_post_male_split_4 + levels_post_male_split_5) / 5
  
  # calculate estimands
  gap_pre <- levels_pre_female - levels_pre_male
  
  gap_post <- levels_post_female - levels_post_male
  
  change <- gap_post - gap_pre
  
  change_pp <- (change / gap_pre) * 100
  
  estimates_men_pre <- data.frame(estimand = "Men (before intervention)", "est" = round(levels_pre_male, 2))
  estimates_women_pre <- data.frame(estimand = "Women (before intervention)", "est" = round(levels_pre_female, 2))
  estimates_men_post <- data.frame(estimand = "Men (after intervention)", "est" = round(levels_post_male, 2))
  estimates_women_post <- data.frame(estimand = "Women (after intervention)", "est" = round(levels_post_female, 2))
  estimates_gap_pre <- data.frame(estimand = "Disparity (before intervention)", "est" = round(gap_pre, 2))
  estimates_gap_post <- data.frame(estimand = "Disparity (after intervention)", "est" = round(gap_post, 2))
  estimates_reduction <- data.frame(estimand = "Change in disparity", "est" = round(change, 2))
  estimates_reduction_pp <- data.frame(estimand = "Change in disparity (pp.)", "est" = round(change_pp, 2))
  
  estimates <- rbind(estimates_men_pre, estimates_women_pre, estimates_men_post, estimates_women_post, estimates_gap_pre, estimates_gap_post, estimates_reduction, estimates_reduction_pp)
  
  rm(list = setdiff(ls(), c("estimates", "data_final", "decompose_cond_dr_xfit", "formula_treatment", "preds_outcome", "sl_models")))
  
  gc()
  
  return(estimates)
  
}


decompose_dr_xfit_intB <- function(data, formula_treatment, preds_outcome, sl_models) {
  
  # select complete cases
  data <- data %>% na.omit()
  
  # determine counter-factual composition via intervention rule
  composition_cf <- multinom("eseg_class ~ 1", data = data %>% filter(female == 1), family = "binomial", weights = weight)
  
  composition_cf <- as.data.frame(predict(composition_cf, type = "probs")) %>% slice(1)
  
  colnames(composition_cf) <- paste0("counterfact_share_occ_", colnames(composition_cf))
  
  data <- data %>% cross_join(composition_cf)
  
  rm(list = setdiff(ls(), c("data", "formula_treatment", "preds_outcome", "sl_models")))
  
  
  
  # create cross-fitting splits
  split <- split(data, f = rep_len(1:5, nrow(data)))
  
  data_split_1 = rbind(split$`2`, split$`3`, split$`4`, split$`5`)
  data_split_1_heldout = split$`1`
  
  data_split_2 = rbind(split$`1`, split$`3`, split$`4`, split$`5`)
  data_split_2_heldout = split$`2`
  
  data_split_3 = rbind(split$`1`, split$`2`, split$`4`, split$`5`)
  data_split_3_heldout = split$`3`
  
  data_split_4 = rbind(split$`1`, split$`2`, split$`3`, split$`5`)
  data_split_4_heldout = split$`4`
  
  data_split_5 = rbind(split$`1`, split$`2`, split$`3`, split$`4`)
  data_split_5_heldout = split$`5`
  
  rm(split)
  
  
  
  # treatment model
  data_split_1_male <- data_split_1 %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_2_male <- data_split_2 %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_3_male <- data_split_3 %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_4_male <- data_split_4 %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_5_male <- data_split_5 %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  
  data_split_1_female <- data_split_1 %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_2_female <- data_split_2 %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_3_female <- data_split_3 %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_4_female <- data_split_4 %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_5_female <- data_split_5 %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  
  data_split_1_heldout_male <- data_split_1_heldout %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_2_heldout_male <- data_split_2_heldout %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_3_heldout_male <- data_split_3_heldout %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_4_heldout_male <- data_split_4_heldout %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_5_heldout_male <- data_split_5_heldout %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  
  data_split_1_heldout_female <- data_split_1_heldout %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_2_heldout_female <- data_split_2_heldout %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_3_heldout_female <- data_split_3_heldout %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_4_heldout_female <- data_split_4_heldout %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_5_heldout_female <- data_split_5_heldout %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  
  # train
  # men
  model_male_split_1 <- multinom(formula_treatment, data = data_split_1_male, family = "binomial", weights = weight)
  model_male_split_2 <- multinom(formula_treatment, data = data_split_2_male, family = "binomial", weights = weight)
  model_male_split_3 <- multinom(formula_treatment, data = data_split_3_male, family = "binomial", weights = weight)
  model_male_split_4 <- multinom(formula_treatment, data = data_split_4_male, family = "binomial", weights = weight)
  model_male_split_5 <- multinom(formula_treatment, data = data_split_5_male, family = "binomial", weights = weight)
  
  # women
  model_female_split_1 <- multinom(formula_treatment, data = data_split_1_female, family = "binomial", weights = weight)
  model_female_split_2 <- multinom(formula_treatment, data = data_split_2_female, family = "binomial", weights = weight)
  model_female_split_3 <- multinom(formula_treatment, data = data_split_3_female, family = "binomial", weights = weight)
  model_female_split_4 <- multinom(formula_treatment, data = data_split_4_female, family = "binomial", weights = weight)
  model_female_split_5 <- multinom(formula_treatment, data = data_split_5_female, family = "binomial", weights = weight)
  
  # predict using model trained on other split
  # men
  data_split_1_heldout_male <- cbind(data_split_1_heldout_male, as.data.frame(predict(model_male_split_1, newdata = data_split_1_heldout_male, type = "probs")))
  colnames(data_split_1_heldout_male)[32:38] = paste0("prop_occ_", colnames(data_split_1_heldout_male)[32:38])
  
  data_split_2_heldout_male <- cbind(data_split_2_heldout_male, as.data.frame(predict(model_male_split_2, newdata = data_split_2_heldout_male, type = "probs")))
  colnames(data_split_2_heldout_male)[32:38] = paste0("prop_occ_", colnames(data_split_2_heldout_male)[32:38])
  
  data_split_3_heldout_male <- cbind(data_split_3_heldout_male, as.data.frame(predict(model_male_split_3, newdata = data_split_3_heldout_male, type = "probs")))
  colnames(data_split_3_heldout_male)[32:38] = paste0("prop_occ_", colnames(data_split_3_heldout_male)[32:38])
  
  data_split_4_heldout_male <- cbind(data_split_4_heldout_male, as.data.frame(predict(model_male_split_4, newdata = data_split_4_heldout_male, type = "probs")))
  colnames(data_split_4_heldout_male)[32:38] = paste0("prop_occ_", colnames(data_split_4_heldout_male)[32:38])
  
  data_split_5_heldout_male <- cbind(data_split_5_heldout_male, as.data.frame(predict(model_male_split_5, newdata = data_split_5_heldout_male, type = "probs")))
  colnames(data_split_5_heldout_male)[32:38] = paste0("prop_occ_", colnames(data_split_5_heldout_male)[32:38])
  
  # women
  data_split_1_heldout_female <- cbind(data_split_1_heldout_female, as.data.frame(predict(model_female_split_1, newdata = data_split_1_heldout_female, type = "probs")))
  colnames(data_split_1_heldout_female)[32:38] = paste0("prop_occ_", colnames(data_split_1_heldout_female)[32:38])
  
  data_split_2_heldout_female <- cbind(data_split_2_heldout_female, as.data.frame(predict(model_female_split_2, newdata = data_split_2_heldout_female, type = "probs")))
  colnames(data_split_2_heldout_female)[32:38] = paste0("prop_occ_", colnames(data_split_2_heldout_female)[32:38])
  
  data_split_3_heldout_female <- cbind(data_split_3_heldout_female, as.data.frame(predict(model_female_split_3, newdata = data_split_3_heldout_female, type = "probs")))
  colnames(data_split_3_heldout_female)[32:38] = paste0("prop_occ_", colnames(data_split_3_heldout_female)[32:38])
  
  data_split_4_heldout_female <- cbind(data_split_4_heldout_female, as.data.frame(predict(model_female_split_4, newdata = data_split_4_heldout_female, type = "probs")))
  colnames(data_split_4_heldout_female)[32:38] = paste0("prop_occ_", colnames(data_split_4_heldout_female)[32:38])
  
  data_split_5_heldout_female <- cbind(data_split_5_heldout_female, as.data.frame(predict(model_female_split_5, newdata = data_split_5_heldout_female, type = "probs")))
  colnames(data_split_5_heldout_female)[32:38] = paste0("prop_occ_", colnames(data_split_5_heldout_female)[32:38])
  
  # combine
  data_split_1_heldout <- rbind(data_split_1_heldout_male, data_split_1_heldout_female)
  data_split_2_heldout <- rbind(data_split_2_heldout_male, data_split_2_heldout_female)
  data_split_3_heldout <- rbind(data_split_3_heldout_male, data_split_3_heldout_female)
  data_split_4_heldout <- rbind(data_split_4_heldout_male, data_split_4_heldout_female)
  data_split_5_heldout <- rbind(data_split_5_heldout_male, data_split_5_heldout_female)
  
  data_split_1_heldout <- data_split_1_heldout %>% as.data.frame() %>% ungroup()
  data_split_2_heldout <- data_split_2_heldout %>% as.data.frame() %>% ungroup()
  data_split_3_heldout <- data_split_3_heldout %>% as.data.frame() %>% ungroup()
  data_split_4_heldout <- data_split_4_heldout %>% as.data.frame() %>% ungroup()
  data_split_5_heldout <- data_split_5_heldout %>% as.data.frame() %>% ungroup()
  
  
  # train
  # men
  model_male_split_1 <- SuperLearner(Y = data_split_1_male$outcome, X = data_split_1_male %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_1_male$weight, cvControl = list(V = 5))
  model_male_split_2 <- SuperLearner(Y = data_split_2_male$outcome, X = data_split_2_male %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_2_male$weight, cvControl = list(V = 5))
  model_male_split_3 <- SuperLearner(Y = data_split_3_male$outcome, X = data_split_3_male %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_3_male$weight, cvControl = list(V = 5))
  model_male_split_4 <- SuperLearner(Y = data_split_4_male$outcome, X = data_split_4_male %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_4_male$weight, cvControl = list(V = 5))
  model_male_split_5 <- SuperLearner(Y = data_split_5_male$outcome, X = data_split_5_male %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_5_male$weight, cvControl = list(V = 5))
  
  # women
  model_female_split_1 <- SuperLearner(Y = data_split_1_female$outcome, X = data_split_1_female %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_1_female$weight, cvControl = list(V = 5))
  model_female_split_2 <- SuperLearner(Y = data_split_2_female$outcome, X = data_split_2_female %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_2_female$weight, cvControl = list(V = 5))
  model_female_split_3 <- SuperLearner(Y = data_split_3_female$outcome, X = data_split_3_female %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_3_female$weight, cvControl = list(V = 5))
  model_female_split_4 <- SuperLearner(Y = data_split_4_female$outcome, X = data_split_4_female %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_4_female$weight, cvControl = list(V = 5))
  model_female_split_5 <- SuperLearner(Y = data_split_5_female$outcome, X = data_split_5_female %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_5_female$weight, cvControl = list(V = 5))
  
  # predictions
  for (i in sort(unique(data$eseg_class))) {
    
    data_split_1_heldout_male$eseg_class <- factor(i, levels = 1:7)
    data_split_2_heldout_male$eseg_class <- factor(i, levels = 1:7)
    data_split_3_heldout_male$eseg_class <- factor(i, levels = 1:7)
    data_split_4_heldout_male$eseg_class <- factor(i, levels = 1:7)
    data_split_5_heldout_male$eseg_class <- factor(i, levels = 1:7)
    
    data_split_1_heldout_female$eseg_class <- factor(i, levels = 1:7)
    data_split_2_heldout_female$eseg_class <- factor(i, levels = 1:7)
    data_split_3_heldout_female$eseg_class <- factor(i, levels = 1:7)
    data_split_4_heldout_female$eseg_class <- factor(i, levels = 1:7)
    data_split_5_heldout_female$eseg_class <- factor(i, levels = 1:7)
    
    data_split_1_heldout_male$prediction <- as.data.frame(predict(model_male_split_1, newdata = data_split_1_heldout_male %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_2_heldout_male$prediction <- as.data.frame(predict(model_male_split_2, newdata = data_split_2_heldout_male %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_3_heldout_male$prediction <- as.data.frame(predict(model_male_split_3, newdata = data_split_3_heldout_male %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_4_heldout_male$prediction <- as.data.frame(predict(model_male_split_4, newdata = data_split_4_heldout_male %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_5_heldout_male$prediction <- as.data.frame(predict(model_male_split_5, newdata = data_split_5_heldout_male %>% select(all_of(preds_outcome)), type = "response"))$pred
    
    data_split_1_heldout_female$prediction <- as.data.frame(predict(model_female_split_1, newdata = data_split_1_heldout_female %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_2_heldout_female$prediction <- as.data.frame(predict(model_female_split_2, newdata = data_split_2_heldout_female %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_3_heldout_female$prediction <- as.data.frame(predict(model_female_split_3, newdata = data_split_3_heldout_female %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_4_heldout_female$prediction <- as.data.frame(predict(model_female_split_4, newdata = data_split_4_heldout_female %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_5_heldout_female$prediction <- as.data.frame(predict(model_female_split_5, newdata = data_split_5_heldout_female %>% select(all_of(preds_outcome)), type = "response"))$pred
    
    data_split_1_heldout_male <- data_split_1_heldout_male %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_2_heldout_male <- data_split_2_heldout_male %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_3_heldout_male <- data_split_3_heldout_male %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_4_heldout_male <- data_split_4_heldout_male %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_5_heldout_male <- data_split_5_heldout_male %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    
    data_split_1_heldout_female <- data_split_1_heldout_female %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_2_heldout_female <- data_split_2_heldout_female %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_3_heldout_female <- data_split_3_heldout_female %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_4_heldout_female <- data_split_4_heldout_female %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_5_heldout_female <- data_split_5_heldout_female %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    
  }
  
  rm(model_male_split_1, model_male_split_2, model_male_split_3, model_male_split_4, model_male_split_5, model_female_split_1, model_female_split_2, model_female_split_3, model_female_split_4, model_female_split_5)
  
  rm(i)
  
  # combine
  preds_split_1 <- rbind(data_split_1_heldout_male, data_split_1_heldout_female)
  preds_split_2 <- rbind(data_split_2_heldout_male, data_split_2_heldout_female)
  preds_split_3 <- rbind(data_split_3_heldout_male, data_split_3_heldout_female)
  preds_split_4 <- rbind(data_split_4_heldout_male, data_split_4_heldout_female)
  preds_split_5 <- rbind(data_split_5_heldout_male, data_split_5_heldout_female)
  
  preds_split_1 <- preds_split_1 %>% select(orgpid, age, contains("exp_outcome_occ_")) %>% group_by(orgpid, age) %>% slice(1)
  preds_split_2 <- preds_split_2 %>% select(orgpid, age, contains("exp_outcome_occ_")) %>% group_by(orgpid, age) %>% slice(1)
  preds_split_3 <- preds_split_3 %>% select(orgpid, age, contains("exp_outcome_occ_")) %>% group_by(orgpid, age) %>% slice(1)
  preds_split_4 <- preds_split_4 %>% select(orgpid, age, contains("exp_outcome_occ_")) %>% group_by(orgpid, age) %>% slice(1)
  preds_split_5 <- preds_split_5 %>% select(orgpid, age, contains("exp_outcome_occ_")) %>% group_by(orgpid, age) %>% slice(1)
  
  data_split_1_heldout <- data_split_1_heldout %>% left_join(preds_split_1, by = c("orgpid", "age")) %>% ungroup()
  data_split_2_heldout <- data_split_2_heldout %>% left_join(preds_split_2, by = c("orgpid", "age")) %>% ungroup()
  data_split_3_heldout <- data_split_3_heldout %>% left_join(preds_split_3, by = c("orgpid", "age")) %>% ungroup()
  data_split_4_heldout <- data_split_4_heldout %>% left_join(preds_split_4, by = c("orgpid", "age")) %>% ungroup()
  data_split_5_heldout <- data_split_5_heldout %>% left_join(preds_split_5, by = c("orgpid", "age")) %>% ungroup()
  
  
  
  # counterfactual probability of being assigned to the factual occupation
  cf_prob_fc_occ <- data %>% ungroup() %>%
    select(orgpid, age, eseg_class, contains("counterfact_share_occ")) %>%
    pivot_longer(cols = c(4:10), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 23, 23)) %>% 
    filter(eseg_class == occupation) %>%
    rename("cf_prob_fc_occ" = "value") %>%
    select(orgpid, age, cf_prob_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  # inverse of the factual probability of the factual occupation
  inv_fc_prob_fc_occ_split_1 <- data_split_1_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class, contains("prop_occ")) %>%
    pivot_longer(cols = c(4:10), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 10, 10)) %>% 
    filter(eseg_class == occupation) %>%
    mutate(value = 1 / value) %>%
    rename("inv_fc_prob_fc_occ" = "value") %>%
    select(orgpid, age, inv_fc_prob_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup() %>%
    mutate(inv_fc_prob_fc_occ = case_when(inv_fc_prob_fc_occ > quantile(inv_fc_prob_fc_occ, 0.999) ~ quantile(inv_fc_prob_fc_occ, 0.999),
                                          inv_fc_prob_fc_occ < quantile(inv_fc_prob_fc_occ, 0.001) ~ quantile(inv_fc_prob_fc_occ, 0.001),
                                          T ~ inv_fc_prob_fc_occ))
  
  inv_fc_prob_fc_occ_split_2 <- data_split_2_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class, contains("prop_occ")) %>%
    pivot_longer(cols = c(4:10), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 10, 10)) %>% 
    filter(eseg_class == occupation) %>%
    mutate(value = 1 / value) %>%
    rename("inv_fc_prob_fc_occ" = "value") %>%
    select(orgpid, age, inv_fc_prob_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup() %>%
    mutate(inv_fc_prob_fc_occ = case_when(inv_fc_prob_fc_occ > quantile(inv_fc_prob_fc_occ, 0.999) ~ quantile(inv_fc_prob_fc_occ, 0.999),
                                          inv_fc_prob_fc_occ < quantile(inv_fc_prob_fc_occ, 0.001) ~ quantile(inv_fc_prob_fc_occ, 0.001),
                                          T ~ inv_fc_prob_fc_occ))
  
  inv_fc_prob_fc_occ_split_3 <- data_split_3_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class, contains("prop_occ")) %>%
    pivot_longer(cols = c(4:10), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 10, 10)) %>% 
    filter(eseg_class == occupation) %>%
    mutate(value = 1 / value) %>%
    rename("inv_fc_prob_fc_occ" = "value") %>%
    select(orgpid, age, inv_fc_prob_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup() %>%
    mutate(inv_fc_prob_fc_occ = case_when(inv_fc_prob_fc_occ > quantile(inv_fc_prob_fc_occ, 0.999) ~ quantile(inv_fc_prob_fc_occ, 0.999),
                                          inv_fc_prob_fc_occ < quantile(inv_fc_prob_fc_occ, 0.001) ~ quantile(inv_fc_prob_fc_occ, 0.001),
                                          T ~ inv_fc_prob_fc_occ))
  
  inv_fc_prob_fc_occ_split_4 <- data_split_4_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class, contains("prop_occ")) %>%
    pivot_longer(cols = c(4:10), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 10, 10)) %>% 
    filter(eseg_class == occupation) %>%
    mutate(value = 1 / value) %>%
    rename("inv_fc_prob_fc_occ" = "value") %>%
    select(orgpid, age, inv_fc_prob_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup() %>%
    mutate(inv_fc_prob_fc_occ = case_when(inv_fc_prob_fc_occ > quantile(inv_fc_prob_fc_occ, 0.999) ~ quantile(inv_fc_prob_fc_occ, 0.999),
                                          inv_fc_prob_fc_occ < quantile(inv_fc_prob_fc_occ, 0.001) ~ quantile(inv_fc_prob_fc_occ, 0.001),
                                          T ~ inv_fc_prob_fc_occ))
  
  inv_fc_prob_fc_occ_split_5 <- data_split_5_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class, contains("prop_occ")) %>%
    pivot_longer(cols = c(4:10), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 10, 10)) %>% 
    filter(eseg_class == occupation) %>%
    mutate(value = 1 / value) %>%
    rename("inv_fc_prob_fc_occ" = "value") %>%
    select(orgpid, age, inv_fc_prob_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup() %>%
    mutate(inv_fc_prob_fc_occ = case_when(inv_fc_prob_fc_occ > quantile(inv_fc_prob_fc_occ, 0.999) ~ quantile(inv_fc_prob_fc_occ, 0.999),
                                          inv_fc_prob_fc_occ < quantile(inv_fc_prob_fc_occ, 0.001) ~ quantile(inv_fc_prob_fc_occ, 0.001),
                                          T ~ inv_fc_prob_fc_occ))
  
  # outcome predictions
  exp_outcome_fc_occ_split_1 <- data_split_1_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class, contains("exp_outcome_occ")) %>%
    pivot_longer(cols = c(4:10), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 17, 17)) %>% 
    filter(eseg_class == occupation) %>%
    rename("exp_outcome_fc_occ" = "value") %>%
    select(orgpid, age, exp_outcome_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  exp_outcome_fc_occ_split_2 <- data_split_2_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class, contains("exp_outcome_occ")) %>%
    pivot_longer(cols = c(4:10), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 17, 17)) %>% 
    filter(eseg_class == occupation) %>%
    rename("exp_outcome_fc_occ" = "value") %>%
    select(orgpid, age, exp_outcome_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  exp_outcome_fc_occ_split_3 <- data_split_3_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class, contains("exp_outcome_occ")) %>%
    pivot_longer(cols = c(4:10), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 17, 17)) %>% 
    filter(eseg_class == occupation) %>%
    rename("exp_outcome_fc_occ" = "value") %>%
    select(orgpid, age, exp_outcome_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  exp_outcome_fc_occ_split_4 <- data_split_4_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class, contains("exp_outcome_occ")) %>%
    pivot_longer(cols = c(4:10), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 17, 17)) %>% 
    filter(eseg_class == occupation) %>%
    rename("exp_outcome_fc_occ" = "value") %>%
    select(orgpid, age, exp_outcome_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  exp_outcome_fc_occ_split_5 <- data_split_5_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class, contains("exp_outcome_occ")) %>%
    pivot_longer(cols = c(4:10), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 17, 17)) %>% 
    filter(eseg_class == occupation) %>%
    rename("exp_outcome_fc_occ" = "value") %>%
    select(orgpid, age, exp_outcome_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  # create combined weight
  weight_split_1 <- data_split_1_heldout %>% ungroup() %>%
    select(orgpid, age, weight) %>% 
    left_join(cf_prob_fc_occ, by = c("orgpid", "age")) %>%
    left_join(inv_fc_prob_fc_occ_split_1, by = c("orgpid", "age")) %>%
    mutate(combined_weight = weight * cf_prob_fc_occ * inv_fc_prob_fc_occ) %>%
    select(orgpid, age, combined_weight) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  weight_split_2 <- data_split_2_heldout %>% ungroup() %>%
    select(orgpid, age, weight) %>% 
    left_join(cf_prob_fc_occ, by = c("orgpid", "age")) %>%
    left_join(inv_fc_prob_fc_occ_split_2, by = c("orgpid", "age")) %>%
    mutate(combined_weight = weight * cf_prob_fc_occ * inv_fc_prob_fc_occ) %>%
    select(orgpid, age, combined_weight) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  weight_split_3 <- data_split_3_heldout %>% ungroup() %>%
    select(orgpid, age, weight) %>% 
    left_join(cf_prob_fc_occ, by = c("orgpid", "age")) %>%
    left_join(inv_fc_prob_fc_occ_split_3, by = c("orgpid", "age")) %>%
    mutate(combined_weight = weight * cf_prob_fc_occ * inv_fc_prob_fc_occ) %>%
    select(orgpid, age, combined_weight) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  weight_split_4 <- data_split_4_heldout %>% ungroup() %>%
    select(orgpid, age, weight) %>% 
    left_join(cf_prob_fc_occ, by = c("orgpid", "age")) %>%
    left_join(inv_fc_prob_fc_occ_split_4, by = c("orgpid", "age")) %>%
    mutate(combined_weight = weight * cf_prob_fc_occ * inv_fc_prob_fc_occ) %>%
    select(orgpid, age, combined_weight) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  weight_split_5 <- data_split_5_heldout %>% ungroup() %>%
    select(orgpid, age, weight) %>% 
    left_join(cf_prob_fc_occ, by = c("orgpid", "age")) %>%
    left_join(inv_fc_prob_fc_occ_split_5, by = c("orgpid", "age")) %>%
    mutate(combined_weight = weight * cf_prob_fc_occ * inv_fc_prob_fc_occ) %>%
    select(orgpid, age, combined_weight) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  # join
  data_split_1_heldout <- data_split_1_heldout %>% left_join(exp_outcome_fc_occ_split_1, by = c("orgpid", "age")) %>% left_join(weight_split_1, by = c("orgpid", "age")) %>% ungroup()
  data_split_2_heldout <- data_split_2_heldout %>% left_join(exp_outcome_fc_occ_split_2, by = c("orgpid", "age")) %>% left_join(weight_split_2, by = c("orgpid", "age")) %>% ungroup()
  data_split_3_heldout <- data_split_3_heldout %>% left_join(exp_outcome_fc_occ_split_3, by = c("orgpid", "age")) %>% left_join(weight_split_3, by = c("orgpid", "age")) %>% ungroup()
  data_split_4_heldout <- data_split_4_heldout %>% left_join(exp_outcome_fc_occ_split_4, by = c("orgpid", "age")) %>% left_join(weight_split_4, by = c("orgpid", "age")) %>% ungroup()
  data_split_5_heldout <- data_split_5_heldout %>% left_join(exp_outcome_fc_occ_split_5, by = c("orgpid", "age")) %>% left_join(weight_split_5, by = c("orgpid", "age")) %>% ungroup()
  
  # calculate bias correction
  bias_correction_m_split_1 <- data_split_1_heldout %>% filter(female == 0) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_m_split_2 <- data_split_2_heldout %>% filter(female == 0) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_m_split_3 <- data_split_3_heldout %>% filter(female == 0) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_m_split_4 <- data_split_4_heldout %>% filter(female == 0) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_m_split_5 <- data_split_5_heldout %>% filter(female == 0) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_f_split_1 <- data_split_1_heldout %>% filter(female == 1) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_f_split_2 <- data_split_2_heldout %>% filter(female == 1) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_f_split_3 <- data_split_3_heldout %>% filter(female == 1) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_f_split_4 <- data_split_4_heldout %>% filter(female == 1) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_f_split_5 <- data_split_5_heldout %>% filter(female == 1) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_split_1 <- rbind(bias_correction_m_split_1, bias_correction_f_split_1)
  bias_correction_split_2 <- rbind(bias_correction_m_split_2, bias_correction_f_split_2)
  bias_correction_split_3 <- rbind(bias_correction_m_split_3, bias_correction_f_split_3)
  bias_correction_split_4 <- rbind(bias_correction_m_split_4, bias_correction_f_split_4)
  bias_correction_split_5 <- rbind(bias_correction_m_split_5, bias_correction_f_split_5)
  
  # correct bias in outcome predictions
  data_split_1_heldout <- data_split_1_heldout %>% 
    left_join(bias_correction_split_1, by = "female") %>% ungroup() %>%
    mutate_at(vars(contains("exp_outcome_occ_")), ~(. - bias_correction)) %>% 
    mutate_at(vars(contains("exp_outcome_occ_")), ~(case_when(. > 1 ~ 1, . < 0 ~ 0, T ~ .)))
  
  data_split_2_heldout <- data_split_2_heldout %>% 
    left_join(bias_correction_split_2, by = "female") %>% ungroup() %>%
    mutate_at(vars(contains("exp_outcome_occ_")), ~(. - bias_correction)) %>% 
    mutate_at(vars(contains("exp_outcome_occ_")), ~(case_when(. > 1 ~ 1, . < 0 ~ 0, T ~ .)))
  
  data_split_3_heldout <- data_split_3_heldout %>% 
    left_join(bias_correction_split_3, by = "female") %>% ungroup() %>%
    mutate_at(vars(contains("exp_outcome_occ_")), ~(. - bias_correction)) %>% 
    mutate_at(vars(contains("exp_outcome_occ_")), ~(case_when(. > 1 ~ 1, . < 0 ~ 0, T ~ .)))
  
  data_split_4_heldout <- data_split_4_heldout %>% 
    left_join(bias_correction_split_4, by = "female") %>% ungroup() %>%
    mutate_at(vars(contains("exp_outcome_occ_")), ~(. - bias_correction)) %>% 
    mutate_at(vars(contains("exp_outcome_occ_")), ~(case_when(. > 1 ~ 1, . < 0 ~ 0, T ~ .)))
  
  data_split_5_heldout <- data_split_5_heldout %>% 
    left_join(bias_correction_split_5, by = "female") %>% ungroup() %>%
    mutate_at(vars(contains("exp_outcome_occ_")), ~(. - bias_correction)) %>% 
    mutate_at(vars(contains("exp_outcome_occ_")), ~(case_when(. > 1 ~ 1, . < 0 ~ 0, T ~ .)))
  
  # pre-intervention
  levels_pre_female <- data %>% filter(female == 1) %>% ungroup() %>%summarize(exp = weighted.mean(outcome, w = weight) * 100) %>% as.numeric()
  levels_pre_male <- data %>% filter(female == 0) %>% ungroup() %>%summarize(exp = weighted.mean(outcome, w = weight) * 100) %>% as.numeric()
  
  # post-intervention
  # split 1
  levels_post_female_split_1 <- data_split_1_heldout %>% filter(female == 1) %>% mutate(exp = (exp_outcome_occ_1 * counterfact_share_occ_1) + (exp_outcome_occ_2 * counterfact_share_occ_2) + (exp_outcome_occ_3 * counterfact_share_occ_3) + (exp_outcome_occ_4 * counterfact_share_occ_4) + (exp_outcome_occ_5 * counterfact_share_occ_5) + (exp_outcome_occ_6 * counterfact_share_occ_6) + (exp_outcome_occ_7 * counterfact_share_occ_7)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  levels_post_male_split_1 <- data_split_1_heldout %>% filter(female == 0) %>% mutate(exp = (exp_outcome_occ_1 * counterfact_share_occ_1) + (exp_outcome_occ_2 * counterfact_share_occ_2) + (exp_outcome_occ_3 * counterfact_share_occ_3) + (exp_outcome_occ_4 * counterfact_share_occ_4) + (exp_outcome_occ_5 * counterfact_share_occ_5) + (exp_outcome_occ_6 * counterfact_share_occ_6) + (exp_outcome_occ_7 * counterfact_share_occ_7)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  
  # split 2
  levels_post_female_split_2 <- data_split_2_heldout %>% filter(female == 1) %>% mutate(exp = (exp_outcome_occ_1 * counterfact_share_occ_1) + (exp_outcome_occ_2 * counterfact_share_occ_2) + (exp_outcome_occ_3 * counterfact_share_occ_3) + (exp_outcome_occ_4 * counterfact_share_occ_4) + (exp_outcome_occ_5 * counterfact_share_occ_5) + (exp_outcome_occ_6 * counterfact_share_occ_6) + (exp_outcome_occ_7 * counterfact_share_occ_7)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  levels_post_male_split_2 <- data_split_2_heldout %>% filter(female == 0) %>% mutate(exp = (exp_outcome_occ_1 * counterfact_share_occ_1) + (exp_outcome_occ_2 * counterfact_share_occ_2) + (exp_outcome_occ_3 * counterfact_share_occ_3) + (exp_outcome_occ_4 * counterfact_share_occ_4) + (exp_outcome_occ_5 * counterfact_share_occ_5) + (exp_outcome_occ_6 * counterfact_share_occ_6) + (exp_outcome_occ_7 * counterfact_share_occ_7)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  
  # split 3
  levels_post_female_split_3 <- data_split_3_heldout %>% filter(female == 1) %>% mutate(exp = (exp_outcome_occ_1 * counterfact_share_occ_1) + (exp_outcome_occ_2 * counterfact_share_occ_2) + (exp_outcome_occ_3 * counterfact_share_occ_3) + (exp_outcome_occ_4 * counterfact_share_occ_4) + (exp_outcome_occ_5 * counterfact_share_occ_5) + (exp_outcome_occ_6 * counterfact_share_occ_6) + (exp_outcome_occ_7 * counterfact_share_occ_7)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  levels_post_male_split_3 <- data_split_3_heldout %>% filter(female == 0) %>% mutate(exp = (exp_outcome_occ_1 * counterfact_share_occ_1) + (exp_outcome_occ_2 * counterfact_share_occ_2) + (exp_outcome_occ_3 * counterfact_share_occ_3) + (exp_outcome_occ_4 * counterfact_share_occ_4) + (exp_outcome_occ_5 * counterfact_share_occ_5) + (exp_outcome_occ_6 * counterfact_share_occ_6) + (exp_outcome_occ_7 * counterfact_share_occ_7)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  
  # split 4
  levels_post_female_split_4 <- data_split_4_heldout %>% filter(female == 1) %>% mutate(exp = (exp_outcome_occ_1 * counterfact_share_occ_1) + (exp_outcome_occ_2 * counterfact_share_occ_2) + (exp_outcome_occ_3 * counterfact_share_occ_3) + (exp_outcome_occ_4 * counterfact_share_occ_4) + (exp_outcome_occ_5 * counterfact_share_occ_5) + (exp_outcome_occ_6 * counterfact_share_occ_6) + (exp_outcome_occ_7 * counterfact_share_occ_7)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  levels_post_male_split_4 <- data_split_4_heldout %>% filter(female == 0) %>% mutate(exp = (exp_outcome_occ_1 * counterfact_share_occ_1) + (exp_outcome_occ_2 * counterfact_share_occ_2) + (exp_outcome_occ_3 * counterfact_share_occ_3) + (exp_outcome_occ_4 * counterfact_share_occ_4) + (exp_outcome_occ_5 * counterfact_share_occ_5) + (exp_outcome_occ_6 * counterfact_share_occ_6) + (exp_outcome_occ_7 * counterfact_share_occ_7)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  
  # split 5
  levels_post_female_split_5 <- data_split_5_heldout %>% filter(female == 1) %>% mutate(exp = (exp_outcome_occ_1 * counterfact_share_occ_1) + (exp_outcome_occ_2 * counterfact_share_occ_2) + (exp_outcome_occ_3 * counterfact_share_occ_3) + (exp_outcome_occ_4 * counterfact_share_occ_4) + (exp_outcome_occ_5 * counterfact_share_occ_5) + (exp_outcome_occ_6 * counterfact_share_occ_6) + (exp_outcome_occ_7 * counterfact_share_occ_7)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  levels_post_male_split_5 <- data_split_5_heldout %>% filter(female == 0) %>% mutate(exp = (exp_outcome_occ_1 * counterfact_share_occ_1) + (exp_outcome_occ_2 * counterfact_share_occ_2) + (exp_outcome_occ_3 * counterfact_share_occ_3) + (exp_outcome_occ_4 * counterfact_share_occ_4) + (exp_outcome_occ_5 * counterfact_share_occ_5) + (exp_outcome_occ_6 * counterfact_share_occ_6) + (exp_outcome_occ_7 * counterfact_share_occ_7)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  
  # average cross-fits
  levels_post_female <- (levels_post_female_split_1 + levels_post_female_split_2 + levels_post_female_split_3 + levels_post_female_split_4 + levels_post_female_split_5) / 5
  levels_post_male <- (levels_post_male_split_1 + levels_post_male_split_2 + levels_post_male_split_3 + levels_post_male_split_4 + levels_post_male_split_5) / 5
  
  # calculate estimands
  gap_pre <- levels_pre_female - levels_pre_male
  
  gap_post <- levels_post_female - levels_post_male
  
  change <- gap_post - gap_pre
  
  change_pp <- (change / gap_pre) * 100
  
  estimates_men_pre <- data.frame(estimand = "Men (before intervention)", "est" = round(levels_pre_male, 2))
  estimates_women_pre <- data.frame(estimand = "Women (before intervention)", "est" = round(levels_pre_female, 2))
  estimates_men_post <- data.frame(estimand = "Men (after intervention)", "est" = round(levels_post_male, 2))
  estimates_women_post <- data.frame(estimand = "Women (after intervention)", "est" = round(levels_post_female, 2))
  estimates_gap_pre <- data.frame(estimand = "Disparity (before intervention)", "est" = round(gap_pre, 2))
  estimates_gap_post <- data.frame(estimand = "Disparity (after intervention)", "est" = round(gap_post, 2))
  estimates_reduction <- data.frame(estimand = "Change in disparity", "est" = round(change, 2))
  estimates_reduction_pp <- data.frame(estimand = "Change in disparity (pp.)", "est" = round(change_pp, 2))
  
  estimates <- rbind(estimates_men_pre, estimates_women_pre, estimates_men_post, estimates_women_post, estimates_gap_pre, estimates_gap_post, estimates_reduction, estimates_reduction_pp)
  
  rm(list = setdiff(ls(), c("estimates", "data_final", "decompose_cond_dr_xfit", "formula_treatment", "preds_outcome", "sl_models")))
  
  gc()
  
  return(estimates)
  
}


decompose_dr_xfit_intC <- function(data, formula_treatment, preds_outcome, sl_models) {
  
  # select complete cases
  data <- data %>% na.omit()
  
  # determine counter-factual composition via intervention rule
  composition_cf <- multinom("eseg_class ~ 1", data = data, family = "binomial", weights = weight)
  
  composition_cf <- as.data.frame(predict(composition_cf, type = "probs"))
  
  colnames(composition_cf) <- paste0("counterfact_share_occ_", colnames(composition_cf))
  
  data <- cbind(data, composition_cf)
  
  rm(list = setdiff(ls(), c("data", "formula_treatment", "preds_outcome", "sl_models")))
  
  
  
  # create cross-fitting splits
  split <- split(data, f = rep_len(1:5, nrow(data)))
  
  data_split_1 = rbind(split$`2`, split$`3`, split$`4`, split$`5`)
  data_split_1_heldout = split$`1`
  
  data_split_2 = rbind(split$`1`, split$`3`, split$`4`, split$`5`)
  data_split_2_heldout = split$`2`
  
  data_split_3 = rbind(split$`1`, split$`2`, split$`4`, split$`5`)
  data_split_3_heldout = split$`3`
  
  data_split_4 = rbind(split$`1`, split$`2`, split$`3`, split$`5`)
  data_split_4_heldout = split$`4`
  
  data_split_5 = rbind(split$`1`, split$`2`, split$`3`, split$`4`)
  data_split_5_heldout = split$`5`
  
  rm(split)
  
  
  
  # treatment model
  data_split_1_male <- data_split_1 %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_2_male <- data_split_2 %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_3_male <- data_split_3 %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_4_male <- data_split_4 %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_5_male <- data_split_5 %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  
  data_split_1_female <- data_split_1 %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_2_female <- data_split_2 %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_3_female <- data_split_3 %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_4_female <- data_split_4 %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_5_female <- data_split_5 %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  
  data_split_1_heldout_male <- data_split_1_heldout %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_2_heldout_male <- data_split_2_heldout %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_3_heldout_male <- data_split_3_heldout %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_4_heldout_male <- data_split_4_heldout %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_5_heldout_male <- data_split_5_heldout %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  
  data_split_1_heldout_female <- data_split_1_heldout %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_2_heldout_female <- data_split_2_heldout %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_3_heldout_female <- data_split_3_heldout %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_4_heldout_female <- data_split_4_heldout %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_5_heldout_female <- data_split_5_heldout %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  
  # train
  # men
  model_male_split_1 <- multinom(formula_treatment, data = data_split_1_male, family = "binomial", weights = weight)
  model_male_split_2 <- multinom(formula_treatment, data = data_split_2_male, family = "binomial", weights = weight)
  model_male_split_3 <- multinom(formula_treatment, data = data_split_3_male, family = "binomial", weights = weight)
  model_male_split_4 <- multinom(formula_treatment, data = data_split_4_male, family = "binomial", weights = weight)
  model_male_split_5 <- multinom(formula_treatment, data = data_split_5_male, family = "binomial", weights = weight)
  
  # women
  model_female_split_1 <- multinom(formula_treatment, data = data_split_1_female, family = "binomial", weights = weight)
  model_female_split_2 <- multinom(formula_treatment, data = data_split_2_female, family = "binomial", weights = weight)
  model_female_split_3 <- multinom(formula_treatment, data = data_split_3_female, family = "binomial", weights = weight)
  model_female_split_4 <- multinom(formula_treatment, data = data_split_4_female, family = "binomial", weights = weight)
  model_female_split_5 <- multinom(formula_treatment, data = data_split_5_female, family = "binomial", weights = weight)
  
  # predict using model trained on other split
  # men
  data_split_1_heldout_male <- cbind(data_split_1_heldout_male, as.data.frame(predict(model_male_split_1, newdata = data_split_1_heldout_male, type = "probs")))
  colnames(data_split_1_heldout_male)[32:38] = paste0("prop_occ_", colnames(data_split_1_heldout_male)[32:38])
  
  data_split_2_heldout_male <- cbind(data_split_2_heldout_male, as.data.frame(predict(model_male_split_2, newdata = data_split_2_heldout_male, type = "probs")))
  colnames(data_split_2_heldout_male)[32:38] = paste0("prop_occ_", colnames(data_split_2_heldout_male)[32:38])
  
  data_split_3_heldout_male <- cbind(data_split_3_heldout_male, as.data.frame(predict(model_male_split_3, newdata = data_split_3_heldout_male, type = "probs")))
  colnames(data_split_3_heldout_male)[32:38] = paste0("prop_occ_", colnames(data_split_3_heldout_male)[32:38])
  
  data_split_4_heldout_male <- cbind(data_split_4_heldout_male, as.data.frame(predict(model_male_split_4, newdata = data_split_4_heldout_male, type = "probs")))
  colnames(data_split_4_heldout_male)[32:38] = paste0("prop_occ_", colnames(data_split_4_heldout_male)[32:38])
  
  data_split_5_heldout_male <- cbind(data_split_5_heldout_male, as.data.frame(predict(model_male_split_5, newdata = data_split_5_heldout_male, type = "probs")))
  colnames(data_split_5_heldout_male)[32:38] = paste0("prop_occ_", colnames(data_split_5_heldout_male)[32:38])
  
  # women
  data_split_1_heldout_female <- cbind(data_split_1_heldout_female, as.data.frame(predict(model_female_split_1, newdata = data_split_1_heldout_female, type = "probs")))
  colnames(data_split_1_heldout_female)[32:38] = paste0("prop_occ_", colnames(data_split_1_heldout_female)[32:38])
  
  data_split_2_heldout_female <- cbind(data_split_2_heldout_female, as.data.frame(predict(model_female_split_2, newdata = data_split_2_heldout_female, type = "probs")))
  colnames(data_split_2_heldout_female)[32:38] = paste0("prop_occ_", colnames(data_split_2_heldout_female)[32:38])
  
  data_split_3_heldout_female <- cbind(data_split_3_heldout_female, as.data.frame(predict(model_female_split_3, newdata = data_split_3_heldout_female, type = "probs")))
  colnames(data_split_3_heldout_female)[32:38] = paste0("prop_occ_", colnames(data_split_3_heldout_female)[32:38])
  
  data_split_4_heldout_female <- cbind(data_split_4_heldout_female, as.data.frame(predict(model_female_split_4, newdata = data_split_4_heldout_female, type = "probs")))
  colnames(data_split_4_heldout_female)[32:38] = paste0("prop_occ_", colnames(data_split_4_heldout_female)[32:38])
  
  data_split_5_heldout_female <- cbind(data_split_5_heldout_female, as.data.frame(predict(model_female_split_5, newdata = data_split_5_heldout_female, type = "probs")))
  colnames(data_split_5_heldout_female)[32:38] = paste0("prop_occ_", colnames(data_split_5_heldout_female)[32:38])
  
  # combine
  data_split_1_heldout <- rbind(data_split_1_heldout_male, data_split_1_heldout_female)
  data_split_2_heldout <- rbind(data_split_2_heldout_male, data_split_2_heldout_female)
  data_split_3_heldout <- rbind(data_split_3_heldout_male, data_split_3_heldout_female)
  data_split_4_heldout <- rbind(data_split_4_heldout_male, data_split_4_heldout_female)
  data_split_5_heldout <- rbind(data_split_5_heldout_male, data_split_5_heldout_female)
  
  data_split_1_heldout <- data_split_1_heldout %>% as.data.frame() %>% ungroup()
  data_split_2_heldout <- data_split_2_heldout %>% as.data.frame() %>% ungroup()
  data_split_3_heldout <- data_split_3_heldout %>% as.data.frame() %>% ungroup()
  data_split_4_heldout <- data_split_4_heldout %>% as.data.frame() %>% ungroup()
  data_split_5_heldout <- data_split_5_heldout %>% as.data.frame() %>% ungroup()
  
  
  # train
  # men
  model_male_split_1 <- SuperLearner(Y = data_split_1_male$outcome, X = data_split_1_male %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_1_male$weight, cvControl = list(V = 5))
  model_male_split_2 <- SuperLearner(Y = data_split_2_male$outcome, X = data_split_2_male %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_2_male$weight, cvControl = list(V = 5))
  model_male_split_3 <- SuperLearner(Y = data_split_3_male$outcome, X = data_split_3_male %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_3_male$weight, cvControl = list(V = 5))
  model_male_split_4 <- SuperLearner(Y = data_split_4_male$outcome, X = data_split_4_male %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_4_male$weight, cvControl = list(V = 5))
  model_male_split_5 <- SuperLearner(Y = data_split_5_male$outcome, X = data_split_5_male %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_5_male$weight, cvControl = list(V = 5))
  
  # women
  model_female_split_1 <- SuperLearner(Y = data_split_1_female$outcome, X = data_split_1_female %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_1_female$weight, cvControl = list(V = 5))
  model_female_split_2 <- SuperLearner(Y = data_split_2_female$outcome, X = data_split_2_female %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_2_female$weight, cvControl = list(V = 5))
  model_female_split_3 <- SuperLearner(Y = data_split_3_female$outcome, X = data_split_3_female %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_3_female$weight, cvControl = list(V = 5))
  model_female_split_4 <- SuperLearner(Y = data_split_4_female$outcome, X = data_split_4_female %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_4_female$weight, cvControl = list(V = 5))
  model_female_split_5 <- SuperLearner(Y = data_split_5_female$outcome, X = data_split_5_female %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_5_female$weight, cvControl = list(V = 5))
  
  # predictions
  for (i in sort(unique(data$eseg_class))) {
    
    data_split_1_heldout_male$eseg_class <- factor(i, levels = 1:7)
    data_split_2_heldout_male$eseg_class <- factor(i, levels = 1:7)
    data_split_3_heldout_male$eseg_class <- factor(i, levels = 1:7)
    data_split_4_heldout_male$eseg_class <- factor(i, levels = 1:7)
    data_split_5_heldout_male$eseg_class <- factor(i, levels = 1:7)
    
    data_split_1_heldout_female$eseg_class <- factor(i, levels = 1:7)
    data_split_2_heldout_female$eseg_class <- factor(i, levels = 1:7)
    data_split_3_heldout_female$eseg_class <- factor(i, levels = 1:7)
    data_split_4_heldout_female$eseg_class <- factor(i, levels = 1:7)
    data_split_5_heldout_female$eseg_class <- factor(i, levels = 1:7)
    
    data_split_1_heldout_male$prediction <- as.data.frame(predict(model_male_split_1, newdata = data_split_1_heldout_male %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_2_heldout_male$prediction <- as.data.frame(predict(model_male_split_2, newdata = data_split_2_heldout_male %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_3_heldout_male$prediction <- as.data.frame(predict(model_male_split_3, newdata = data_split_3_heldout_male %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_4_heldout_male$prediction <- as.data.frame(predict(model_male_split_4, newdata = data_split_4_heldout_male %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_5_heldout_male$prediction <- as.data.frame(predict(model_male_split_5, newdata = data_split_5_heldout_male %>% select(all_of(preds_outcome)), type = "response"))$pred
    
    data_split_1_heldout_female$prediction <- as.data.frame(predict(model_female_split_1, newdata = data_split_1_heldout_female %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_2_heldout_female$prediction <- as.data.frame(predict(model_female_split_2, newdata = data_split_2_heldout_female %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_3_heldout_female$prediction <- as.data.frame(predict(model_female_split_3, newdata = data_split_3_heldout_female %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_4_heldout_female$prediction <- as.data.frame(predict(model_female_split_4, newdata = data_split_4_heldout_female %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_5_heldout_female$prediction <- as.data.frame(predict(model_female_split_5, newdata = data_split_5_heldout_female %>% select(all_of(preds_outcome)), type = "response"))$pred
    
    data_split_1_heldout_male <- data_split_1_heldout_male %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_2_heldout_male <- data_split_2_heldout_male %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_3_heldout_male <- data_split_3_heldout_male %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_4_heldout_male <- data_split_4_heldout_male %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_5_heldout_male <- data_split_5_heldout_male %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    
    data_split_1_heldout_female <- data_split_1_heldout_female %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_2_heldout_female <- data_split_2_heldout_female %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_3_heldout_female <- data_split_3_heldout_female %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_4_heldout_female <- data_split_4_heldout_female %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_5_heldout_female <- data_split_5_heldout_female %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    
  }
  
  rm(model_male_split_1, model_male_split_2, model_male_split_3, model_male_split_4, model_male_split_5, model_female_split_1, model_female_split_2, model_female_split_3, model_female_split_4, model_female_split_5)
  
  rm(i)
  
  # combine
  preds_split_1 <- rbind(data_split_1_heldout_male, data_split_1_heldout_female)
  preds_split_2 <- rbind(data_split_2_heldout_male, data_split_2_heldout_female)
  preds_split_3 <- rbind(data_split_3_heldout_male, data_split_3_heldout_female)
  preds_split_4 <- rbind(data_split_4_heldout_male, data_split_4_heldout_female)
  preds_split_5 <- rbind(data_split_5_heldout_male, data_split_5_heldout_female)
  
  preds_split_1 <- preds_split_1 %>% select(orgpid, age, contains("exp_outcome_occ_")) %>% group_by(orgpid, age) %>% slice(1)
  preds_split_2 <- preds_split_2 %>% select(orgpid, age, contains("exp_outcome_occ_")) %>% group_by(orgpid, age) %>% slice(1)
  preds_split_3 <- preds_split_3 %>% select(orgpid, age, contains("exp_outcome_occ_")) %>% group_by(orgpid, age) %>% slice(1)
  preds_split_4 <- preds_split_4 %>% select(orgpid, age, contains("exp_outcome_occ_")) %>% group_by(orgpid, age) %>% slice(1)
  preds_split_5 <- preds_split_5 %>% select(orgpid, age, contains("exp_outcome_occ_")) %>% group_by(orgpid, age) %>% slice(1)
  
  data_split_1_heldout <- data_split_1_heldout %>% left_join(preds_split_1, by = c("orgpid", "age")) %>% ungroup()
  data_split_2_heldout <- data_split_2_heldout %>% left_join(preds_split_2, by = c("orgpid", "age")) %>% ungroup()
  data_split_3_heldout <- data_split_3_heldout %>% left_join(preds_split_3, by = c("orgpid", "age")) %>% ungroup()
  data_split_4_heldout <- data_split_4_heldout %>% left_join(preds_split_4, by = c("orgpid", "age")) %>% ungroup()
  data_split_5_heldout <- data_split_5_heldout %>% left_join(preds_split_5, by = c("orgpid", "age")) %>% ungroup()
  
  
  
  # counterfactual probability of being assigned to the factual occupation
  cf_prob_fc_occ <- data %>% ungroup() %>%
    select(orgpid, age, eseg_class, contains("counterfact_share_occ")) %>%
    pivot_longer(cols = c(4:10), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 23, 23)) %>% 
    filter(eseg_class == occupation) %>%
    rename("cf_prob_fc_occ" = "value") %>%
    select(orgpid, age, cf_prob_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  # inverse of the factual probability of the factual occupation
  inv_fc_prob_fc_occ_split_1 <- data_split_1_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class, contains("prop_occ")) %>%
    pivot_longer(cols = c(4:10), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 10, 10)) %>% 
    filter(eseg_class == occupation) %>%
    mutate(value = 1 / value) %>%
    rename("inv_fc_prob_fc_occ" = "value") %>%
    select(orgpid, age, inv_fc_prob_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup() %>%
    mutate(inv_fc_prob_fc_occ = case_when(inv_fc_prob_fc_occ > quantile(inv_fc_prob_fc_occ, 0.999) ~ quantile(inv_fc_prob_fc_occ, 0.999),
                                          inv_fc_prob_fc_occ < quantile(inv_fc_prob_fc_occ, 0.001) ~ quantile(inv_fc_prob_fc_occ, 0.001),
                                          T ~ inv_fc_prob_fc_occ))
  
  inv_fc_prob_fc_occ_split_2 <- data_split_2_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class, contains("prop_occ")) %>%
    pivot_longer(cols = c(4:10), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 10, 10)) %>% 
    filter(eseg_class == occupation) %>%
    mutate(value = 1 / value) %>%
    rename("inv_fc_prob_fc_occ" = "value") %>%
    select(orgpid, age, inv_fc_prob_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup() %>%
    mutate(inv_fc_prob_fc_occ = case_when(inv_fc_prob_fc_occ > quantile(inv_fc_prob_fc_occ, 0.999) ~ quantile(inv_fc_prob_fc_occ, 0.999),
                                          inv_fc_prob_fc_occ < quantile(inv_fc_prob_fc_occ, 0.001) ~ quantile(inv_fc_prob_fc_occ, 0.001),
                                          T ~ inv_fc_prob_fc_occ))
  
  inv_fc_prob_fc_occ_split_3 <- data_split_3_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class, contains("prop_occ")) %>%
    pivot_longer(cols = c(4:10), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 10, 10)) %>% 
    filter(eseg_class == occupation) %>%
    mutate(value = 1 / value) %>%
    rename("inv_fc_prob_fc_occ" = "value") %>%
    select(orgpid, age, inv_fc_prob_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup() %>%
    mutate(inv_fc_prob_fc_occ = case_when(inv_fc_prob_fc_occ > quantile(inv_fc_prob_fc_occ, 0.999) ~ quantile(inv_fc_prob_fc_occ, 0.999),
                                          inv_fc_prob_fc_occ < quantile(inv_fc_prob_fc_occ, 0.001) ~ quantile(inv_fc_prob_fc_occ, 0.001),
                                          T ~ inv_fc_prob_fc_occ))
  
  inv_fc_prob_fc_occ_split_4 <- data_split_4_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class, contains("prop_occ")) %>%
    pivot_longer(cols = c(4:10), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 10, 10)) %>% 
    filter(eseg_class == occupation) %>%
    mutate(value = 1 / value) %>%
    rename("inv_fc_prob_fc_occ" = "value") %>%
    select(orgpid, age, inv_fc_prob_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup() %>%
    mutate(inv_fc_prob_fc_occ = case_when(inv_fc_prob_fc_occ > quantile(inv_fc_prob_fc_occ, 0.999) ~ quantile(inv_fc_prob_fc_occ, 0.999),
                                          inv_fc_prob_fc_occ < quantile(inv_fc_prob_fc_occ, 0.001) ~ quantile(inv_fc_prob_fc_occ, 0.001),
                                          T ~ inv_fc_prob_fc_occ))
  
  inv_fc_prob_fc_occ_split_5 <- data_split_5_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class, contains("prop_occ")) %>%
    pivot_longer(cols = c(4:10), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 10, 10)) %>% 
    filter(eseg_class == occupation) %>%
    mutate(value = 1 / value) %>%
    rename("inv_fc_prob_fc_occ" = "value") %>%
    select(orgpid, age, inv_fc_prob_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup() %>%
    mutate(inv_fc_prob_fc_occ = case_when(inv_fc_prob_fc_occ > quantile(inv_fc_prob_fc_occ, 0.999) ~ quantile(inv_fc_prob_fc_occ, 0.999),
                                          inv_fc_prob_fc_occ < quantile(inv_fc_prob_fc_occ, 0.001) ~ quantile(inv_fc_prob_fc_occ, 0.001),
                                          T ~ inv_fc_prob_fc_occ))
  
  # outcome predictions
  exp_outcome_fc_occ_split_1 <- data_split_1_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class, contains("exp_outcome_occ")) %>%
    pivot_longer(cols = c(4:10), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 17, 17)) %>% 
    filter(eseg_class == occupation) %>%
    rename("exp_outcome_fc_occ" = "value") %>%
    select(orgpid, age, exp_outcome_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  exp_outcome_fc_occ_split_2 <- data_split_2_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class, contains("exp_outcome_occ")) %>%
    pivot_longer(cols = c(4:10), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 17, 17)) %>% 
    filter(eseg_class == occupation) %>%
    rename("exp_outcome_fc_occ" = "value") %>%
    select(orgpid, age, exp_outcome_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  exp_outcome_fc_occ_split_3 <- data_split_3_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class, contains("exp_outcome_occ")) %>%
    pivot_longer(cols = c(4:10), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 17, 17)) %>% 
    filter(eseg_class == occupation) %>%
    rename("exp_outcome_fc_occ" = "value") %>%
    select(orgpid, age, exp_outcome_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  exp_outcome_fc_occ_split_4 <- data_split_4_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class, contains("exp_outcome_occ")) %>%
    pivot_longer(cols = c(4:10), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 17, 17)) %>% 
    filter(eseg_class == occupation) %>%
    rename("exp_outcome_fc_occ" = "value") %>%
    select(orgpid, age, exp_outcome_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  exp_outcome_fc_occ_split_5 <- data_split_5_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class, contains("exp_outcome_occ")) %>%
    pivot_longer(cols = c(4:10), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 17, 17)) %>% 
    filter(eseg_class == occupation) %>%
    rename("exp_outcome_fc_occ" = "value") %>%
    select(orgpid, age, exp_outcome_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  # create combined weight
  weight_split_1 <- data_split_1_heldout %>% ungroup() %>%
    select(orgpid, age, weight) %>% 
    left_join(cf_prob_fc_occ, by = c("orgpid", "age")) %>%
    left_join(inv_fc_prob_fc_occ_split_1, by = c("orgpid", "age")) %>%
    mutate(combined_weight = weight * cf_prob_fc_occ * inv_fc_prob_fc_occ) %>%
    select(orgpid, age, combined_weight) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  weight_split_2 <- data_split_2_heldout %>% ungroup() %>%
    select(orgpid, age, weight) %>% 
    left_join(cf_prob_fc_occ, by = c("orgpid", "age")) %>%
    left_join(inv_fc_prob_fc_occ_split_2, by = c("orgpid", "age")) %>%
    mutate(combined_weight = weight * cf_prob_fc_occ * inv_fc_prob_fc_occ) %>%
    select(orgpid, age, combined_weight) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  weight_split_3 <- data_split_3_heldout %>% ungroup() %>%
    select(orgpid, age, weight) %>% 
    left_join(cf_prob_fc_occ, by = c("orgpid", "age")) %>%
    left_join(inv_fc_prob_fc_occ_split_3, by = c("orgpid", "age")) %>%
    mutate(combined_weight = weight * cf_prob_fc_occ * inv_fc_prob_fc_occ) %>%
    select(orgpid, age, combined_weight) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  weight_split_4 <- data_split_4_heldout %>% ungroup() %>%
    select(orgpid, age, weight) %>% 
    left_join(cf_prob_fc_occ, by = c("orgpid", "age")) %>%
    left_join(inv_fc_prob_fc_occ_split_4, by = c("orgpid", "age")) %>%
    mutate(combined_weight = weight * cf_prob_fc_occ * inv_fc_prob_fc_occ) %>%
    select(orgpid, age, combined_weight) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  weight_split_5 <- data_split_5_heldout %>% ungroup() %>%
    select(orgpid, age, weight) %>% 
    left_join(cf_prob_fc_occ, by = c("orgpid", "age")) %>%
    left_join(inv_fc_prob_fc_occ_split_5, by = c("orgpid", "age")) %>%
    mutate(combined_weight = weight * cf_prob_fc_occ * inv_fc_prob_fc_occ) %>%
    select(orgpid, age, combined_weight) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  # join
  data_split_1_heldout <- data_split_1_heldout %>% left_join(exp_outcome_fc_occ_split_1, by = c("orgpid", "age")) %>% left_join(weight_split_1, by = c("orgpid", "age")) %>% ungroup()
  data_split_2_heldout <- data_split_2_heldout %>% left_join(exp_outcome_fc_occ_split_2, by = c("orgpid", "age")) %>% left_join(weight_split_2, by = c("orgpid", "age")) %>% ungroup()
  data_split_3_heldout <- data_split_3_heldout %>% left_join(exp_outcome_fc_occ_split_3, by = c("orgpid", "age")) %>% left_join(weight_split_3, by = c("orgpid", "age")) %>% ungroup()
  data_split_4_heldout <- data_split_4_heldout %>% left_join(exp_outcome_fc_occ_split_4, by = c("orgpid", "age")) %>% left_join(weight_split_4, by = c("orgpid", "age")) %>% ungroup()
  data_split_5_heldout <- data_split_5_heldout %>% left_join(exp_outcome_fc_occ_split_5, by = c("orgpid", "age")) %>% left_join(weight_split_5, by = c("orgpid", "age")) %>% ungroup()
  
  # calculate bias correction
  bias_correction_m_split_1 <- data_split_1_heldout %>% filter(female == 0) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_m_split_2 <- data_split_2_heldout %>% filter(female == 0) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_m_split_3 <- data_split_3_heldout %>% filter(female == 0) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_m_split_4 <- data_split_4_heldout %>% filter(female == 0) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_m_split_5 <- data_split_5_heldout %>% filter(female == 0) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_f_split_1 <- data_split_1_heldout %>% filter(female == 1) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_f_split_2 <- data_split_2_heldout %>% filter(female == 1) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_f_split_3 <- data_split_3_heldout %>% filter(female == 1) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_f_split_4 <- data_split_4_heldout %>% filter(female == 1) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_f_split_5 <- data_split_5_heldout %>% filter(female == 1) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_split_1 <- rbind(bias_correction_m_split_1, bias_correction_f_split_1)
  bias_correction_split_2 <- rbind(bias_correction_m_split_2, bias_correction_f_split_2)
  bias_correction_split_3 <- rbind(bias_correction_m_split_3, bias_correction_f_split_3)
  bias_correction_split_4 <- rbind(bias_correction_m_split_4, bias_correction_f_split_4)
  bias_correction_split_5 <- rbind(bias_correction_m_split_5, bias_correction_f_split_5)
  
  # correct bias in outcome predictions
  data_split_1_heldout <- data_split_1_heldout %>% 
    left_join(bias_correction_split_1, by = "female") %>% ungroup() %>%
    mutate_at(vars(contains("exp_outcome_occ_")), ~(. - bias_correction)) %>% 
    mutate_at(vars(contains("exp_outcome_occ_")), ~(case_when(. > 1 ~ 1, . < 0 ~ 0, T ~ .)))
  
  data_split_2_heldout <- data_split_2_heldout %>% 
    left_join(bias_correction_split_2, by = "female") %>% ungroup() %>%
    mutate_at(vars(contains("exp_outcome_occ_")), ~(. - bias_correction)) %>% 
    mutate_at(vars(contains("exp_outcome_occ_")), ~(case_when(. > 1 ~ 1, . < 0 ~ 0, T ~ .)))
  
  data_split_3_heldout <- data_split_3_heldout %>% 
    left_join(bias_correction_split_3, by = "female") %>% ungroup() %>%
    mutate_at(vars(contains("exp_outcome_occ_")), ~(. - bias_correction)) %>% 
    mutate_at(vars(contains("exp_outcome_occ_")), ~(case_when(. > 1 ~ 1, . < 0 ~ 0, T ~ .)))
  
  data_split_4_heldout <- data_split_4_heldout %>% 
    left_join(bias_correction_split_4, by = "female") %>% ungroup() %>%
    mutate_at(vars(contains("exp_outcome_occ_")), ~(. - bias_correction)) %>% 
    mutate_at(vars(contains("exp_outcome_occ_")), ~(case_when(. > 1 ~ 1, . < 0 ~ 0, T ~ .)))
  
  data_split_5_heldout <- data_split_5_heldout %>% 
    left_join(bias_correction_split_5, by = "female") %>% ungroup() %>%
    mutate_at(vars(contains("exp_outcome_occ_")), ~(. - bias_correction)) %>% 
    mutate_at(vars(contains("exp_outcome_occ_")), ~(case_when(. > 1 ~ 1, . < 0 ~ 0, T ~ .)))
  
  # pre-intervention
  levels_pre_female <- data %>% filter(female == 1) %>% ungroup() %>%summarize(exp = weighted.mean(outcome, w = weight) * 100) %>% as.numeric()
  levels_pre_male <- data %>% filter(female == 0) %>% ungroup() %>%summarize(exp = weighted.mean(outcome, w = weight) * 100) %>% as.numeric()
  
  # post-intervention
  # split 1
  levels_post_female_split_1 <- data_split_1_heldout %>% filter(female == 1) %>% mutate(exp = (exp_outcome_occ_1 * counterfact_share_occ_1) + (exp_outcome_occ_2 * counterfact_share_occ_2) + (exp_outcome_occ_3 * counterfact_share_occ_3) + (exp_outcome_occ_4 * counterfact_share_occ_4) + (exp_outcome_occ_5 * counterfact_share_occ_5) + (exp_outcome_occ_6 * counterfact_share_occ_6) + (exp_outcome_occ_7 * counterfact_share_occ_7)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  levels_post_male_split_1 <- data_split_1_heldout %>% filter(female == 0) %>% mutate(exp = (exp_outcome_occ_1 * counterfact_share_occ_1) + (exp_outcome_occ_2 * counterfact_share_occ_2) + (exp_outcome_occ_3 * counterfact_share_occ_3) + (exp_outcome_occ_4 * counterfact_share_occ_4) + (exp_outcome_occ_5 * counterfact_share_occ_5) + (exp_outcome_occ_6 * counterfact_share_occ_6) + (exp_outcome_occ_7 * counterfact_share_occ_7)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  
  # split 2
  levels_post_female_split_2 <- data_split_2_heldout %>% filter(female == 1) %>% mutate(exp = (exp_outcome_occ_1 * counterfact_share_occ_1) + (exp_outcome_occ_2 * counterfact_share_occ_2) + (exp_outcome_occ_3 * counterfact_share_occ_3) + (exp_outcome_occ_4 * counterfact_share_occ_4) + (exp_outcome_occ_5 * counterfact_share_occ_5) + (exp_outcome_occ_6 * counterfact_share_occ_6) + (exp_outcome_occ_7 * counterfact_share_occ_7)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  levels_post_male_split_2 <- data_split_2_heldout %>% filter(female == 0) %>% mutate(exp = (exp_outcome_occ_1 * counterfact_share_occ_1) + (exp_outcome_occ_2 * counterfact_share_occ_2) + (exp_outcome_occ_3 * counterfact_share_occ_3) + (exp_outcome_occ_4 * counterfact_share_occ_4) + (exp_outcome_occ_5 * counterfact_share_occ_5) + (exp_outcome_occ_6 * counterfact_share_occ_6) + (exp_outcome_occ_7 * counterfact_share_occ_7)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  
  # split 3
  levels_post_female_split_3 <- data_split_3_heldout %>% filter(female == 1) %>% mutate(exp = (exp_outcome_occ_1 * counterfact_share_occ_1) + (exp_outcome_occ_2 * counterfact_share_occ_2) + (exp_outcome_occ_3 * counterfact_share_occ_3) + (exp_outcome_occ_4 * counterfact_share_occ_4) + (exp_outcome_occ_5 * counterfact_share_occ_5) + (exp_outcome_occ_6 * counterfact_share_occ_6) + (exp_outcome_occ_7 * counterfact_share_occ_7)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  levels_post_male_split_3 <- data_split_3_heldout %>% filter(female == 0) %>% mutate(exp = (exp_outcome_occ_1 * counterfact_share_occ_1) + (exp_outcome_occ_2 * counterfact_share_occ_2) + (exp_outcome_occ_3 * counterfact_share_occ_3) + (exp_outcome_occ_4 * counterfact_share_occ_4) + (exp_outcome_occ_5 * counterfact_share_occ_5) + (exp_outcome_occ_6 * counterfact_share_occ_6) + (exp_outcome_occ_7 * counterfact_share_occ_7)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  
  # split 4
  levels_post_female_split_4 <- data_split_4_heldout %>% filter(female == 1) %>% mutate(exp = (exp_outcome_occ_1 * counterfact_share_occ_1) + (exp_outcome_occ_2 * counterfact_share_occ_2) + (exp_outcome_occ_3 * counterfact_share_occ_3) + (exp_outcome_occ_4 * counterfact_share_occ_4) + (exp_outcome_occ_5 * counterfact_share_occ_5) + (exp_outcome_occ_6 * counterfact_share_occ_6) + (exp_outcome_occ_7 * counterfact_share_occ_7)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  levels_post_male_split_4 <- data_split_4_heldout %>% filter(female == 0) %>% mutate(exp = (exp_outcome_occ_1 * counterfact_share_occ_1) + (exp_outcome_occ_2 * counterfact_share_occ_2) + (exp_outcome_occ_3 * counterfact_share_occ_3) + (exp_outcome_occ_4 * counterfact_share_occ_4) + (exp_outcome_occ_5 * counterfact_share_occ_5) + (exp_outcome_occ_6 * counterfact_share_occ_6) + (exp_outcome_occ_7 * counterfact_share_occ_7)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  
  # split 5
  levels_post_female_split_5 <- data_split_5_heldout %>% filter(female == 1) %>% mutate(exp = (exp_outcome_occ_1 * counterfact_share_occ_1) + (exp_outcome_occ_2 * counterfact_share_occ_2) + (exp_outcome_occ_3 * counterfact_share_occ_3) + (exp_outcome_occ_4 * counterfact_share_occ_4) + (exp_outcome_occ_5 * counterfact_share_occ_5) + (exp_outcome_occ_6 * counterfact_share_occ_6) + (exp_outcome_occ_7 * counterfact_share_occ_7)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  levels_post_male_split_5 <- data_split_5_heldout %>% filter(female == 0) %>% mutate(exp = (exp_outcome_occ_1 * counterfact_share_occ_1) + (exp_outcome_occ_2 * counterfact_share_occ_2) + (exp_outcome_occ_3 * counterfact_share_occ_3) + (exp_outcome_occ_4 * counterfact_share_occ_4) + (exp_outcome_occ_5 * counterfact_share_occ_5) + (exp_outcome_occ_6 * counterfact_share_occ_6) + (exp_outcome_occ_7 * counterfact_share_occ_7)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  
  # average cross-fits
  levels_post_female <- (levels_post_female_split_1 + levels_post_female_split_2 + levels_post_female_split_3 + levels_post_female_split_4 + levels_post_female_split_5) / 5
  levels_post_male <- (levels_post_male_split_1 + levels_post_male_split_2 + levels_post_male_split_3 + levels_post_male_split_4 + levels_post_male_split_5) / 5
  
  # calculate estimands
  gap_pre <- levels_pre_female - levels_pre_male
  
  gap_post <- levels_post_female - levels_post_male
  
  change <- gap_post - gap_pre
  
  change_pp <- (change / gap_pre) * 100
  
  estimates_men_pre <- data.frame(estimand = "Men (before intervention)", "est" = round(levels_pre_male, 2))
  estimates_women_pre <- data.frame(estimand = "Women (before intervention)", "est" = round(levels_pre_female, 2))
  estimates_men_post <- data.frame(estimand = "Men (after intervention)", "est" = round(levels_post_male, 2))
  estimates_women_post <- data.frame(estimand = "Women (after intervention)", "est" = round(levels_post_female, 2))
  estimates_gap_pre <- data.frame(estimand = "Disparity (before intervention)", "est" = round(gap_pre, 2))
  estimates_gap_post <- data.frame(estimand = "Disparity (after intervention)", "est" = round(gap_post, 2))
  estimates_reduction <- data.frame(estimand = "Change in disparity", "est" = round(change, 2))
  estimates_reduction_pp <- data.frame(estimand = "Change in disparity (pp.)", "est" = round(change_pp, 2))
  
  estimates <- rbind(estimates_men_pre, estimates_women_pre, estimates_men_post, estimates_women_post, estimates_gap_pre, estimates_gap_post, estimates_reduction, estimates_reduction_pp)
  
  rm(list = setdiff(ls(), c("estimates", "data_final", "decompose_cond_dr_xfit", "formula_treatment", "preds_outcome", "sl_models")))
  
  gc()
  
  return(estimates)
  
}


decompose_dr_xfit_intD <- function(data, formula_treatment, preds_outcome, sl_models) {
  
  # select complete cases
  data <- data %>% na.omit()
  
  # determine counter-factual composition via intervention rule
  composition_cf <- multinom("eseg_class ~ education_level", data = data, family = "binomial", weights = weight)
  
  composition_cf <- as.data.frame(predict(composition_cf, type = "probs"))
  
  colnames(composition_cf) <- paste0("counterfact_share_occ_", colnames(composition_cf))
  
  data <- cbind(data, composition_cf)
  
  rm(list = setdiff(ls(), c("data", "formula_treatment", "preds_outcome", "sl_models")))
  
  
  
  # create cross-fitting splits
  split <- split(data, f = rep_len(1:5, nrow(data)))
  
  data_split_1 = rbind(split$`2`, split$`3`, split$`4`, split$`5`)
  data_split_1_heldout = split$`1`
  
  data_split_2 = rbind(split$`1`, split$`3`, split$`4`, split$`5`)
  data_split_2_heldout = split$`2`
  
  data_split_3 = rbind(split$`1`, split$`2`, split$`4`, split$`5`)
  data_split_3_heldout = split$`3`
  
  data_split_4 = rbind(split$`1`, split$`2`, split$`3`, split$`5`)
  data_split_4_heldout = split$`4`
  
  data_split_5 = rbind(split$`1`, split$`2`, split$`3`, split$`4`)
  data_split_5_heldout = split$`5`
  
  rm(split)
  
  
  
  # treatment model
  data_split_1_male <- data_split_1 %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_2_male <- data_split_2 %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_3_male <- data_split_3 %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_4_male <- data_split_4 %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_5_male <- data_split_5 %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  
  data_split_1_female <- data_split_1 %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_2_female <- data_split_2 %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_3_female <- data_split_3 %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_4_female <- data_split_4 %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_5_female <- data_split_5 %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  
  data_split_1_heldout_male <- data_split_1_heldout %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_2_heldout_male <- data_split_2_heldout %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_3_heldout_male <- data_split_3_heldout %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_4_heldout_male <- data_split_4_heldout %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_5_heldout_male <- data_split_5_heldout %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  
  data_split_1_heldout_female <- data_split_1_heldout %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_2_heldout_female <- data_split_2_heldout %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_3_heldout_female <- data_split_3_heldout %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_4_heldout_female <- data_split_4_heldout %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_split_5_heldout_female <- data_split_5_heldout %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  
  # train
  # men
  model_male_split_1 <- multinom(formula_treatment, data = data_split_1_male, family = "binomial", weights = weight)
  model_male_split_2 <- multinom(formula_treatment, data = data_split_2_male, family = "binomial", weights = weight)
  model_male_split_3 <- multinom(formula_treatment, data = data_split_3_male, family = "binomial", weights = weight)
  model_male_split_4 <- multinom(formula_treatment, data = data_split_4_male, family = "binomial", weights = weight)
  model_male_split_5 <- multinom(formula_treatment, data = data_split_5_male, family = "binomial", weights = weight)
  
  # women
  model_female_split_1 <- multinom(formula_treatment, data = data_split_1_female, family = "binomial", weights = weight)
  model_female_split_2 <- multinom(formula_treatment, data = data_split_2_female, family = "binomial", weights = weight)
  model_female_split_3 <- multinom(formula_treatment, data = data_split_3_female, family = "binomial", weights = weight)
  model_female_split_4 <- multinom(formula_treatment, data = data_split_4_female, family = "binomial", weights = weight)
  model_female_split_5 <- multinom(formula_treatment, data = data_split_5_female, family = "binomial", weights = weight)
  
  # predict using model trained on other split
  # men
  data_split_1_heldout_male <- cbind(data_split_1_heldout_male, as.data.frame(predict(model_male_split_1, newdata = data_split_1_heldout_male, type = "probs")))
  colnames(data_split_1_heldout_male)[32:38] = paste0("prop_occ_", colnames(data_split_1_heldout_male)[32:38])
  
  data_split_2_heldout_male <- cbind(data_split_2_heldout_male, as.data.frame(predict(model_male_split_2, newdata = data_split_2_heldout_male, type = "probs")))
  colnames(data_split_2_heldout_male)[32:38] = paste0("prop_occ_", colnames(data_split_2_heldout_male)[32:38])
  
  data_split_3_heldout_male <- cbind(data_split_3_heldout_male, as.data.frame(predict(model_male_split_3, newdata = data_split_3_heldout_male, type = "probs")))
  colnames(data_split_3_heldout_male)[32:38] = paste0("prop_occ_", colnames(data_split_3_heldout_male)[32:38])
  
  data_split_4_heldout_male <- cbind(data_split_4_heldout_male, as.data.frame(predict(model_male_split_4, newdata = data_split_4_heldout_male, type = "probs")))
  colnames(data_split_4_heldout_male)[32:38] = paste0("prop_occ_", colnames(data_split_4_heldout_male)[32:38])
  
  data_split_5_heldout_male <- cbind(data_split_5_heldout_male, as.data.frame(predict(model_male_split_5, newdata = data_split_5_heldout_male, type = "probs")))
  colnames(data_split_5_heldout_male)[32:38] = paste0("prop_occ_", colnames(data_split_5_heldout_male)[32:38])
  
  # women
  data_split_1_heldout_female <- cbind(data_split_1_heldout_female, as.data.frame(predict(model_female_split_1, newdata = data_split_1_heldout_female, type = "probs")))
  colnames(data_split_1_heldout_female)[32:38] = paste0("prop_occ_", colnames(data_split_1_heldout_female)[32:38])
  
  data_split_2_heldout_female <- cbind(data_split_2_heldout_female, as.data.frame(predict(model_female_split_2, newdata = data_split_2_heldout_female, type = "probs")))
  colnames(data_split_2_heldout_female)[32:38] = paste0("prop_occ_", colnames(data_split_2_heldout_female)[32:38])
  
  data_split_3_heldout_female <- cbind(data_split_3_heldout_female, as.data.frame(predict(model_female_split_3, newdata = data_split_3_heldout_female, type = "probs")))
  colnames(data_split_3_heldout_female)[32:38] = paste0("prop_occ_", colnames(data_split_3_heldout_female)[32:38])
  
  data_split_4_heldout_female <- cbind(data_split_4_heldout_female, as.data.frame(predict(model_female_split_4, newdata = data_split_4_heldout_female, type = "probs")))
  colnames(data_split_4_heldout_female)[32:38] = paste0("prop_occ_", colnames(data_split_4_heldout_female)[32:38])
  
  data_split_5_heldout_female <- cbind(data_split_5_heldout_female, as.data.frame(predict(model_female_split_5, newdata = data_split_5_heldout_female, type = "probs")))
  colnames(data_split_5_heldout_female)[32:38] = paste0("prop_occ_", colnames(data_split_5_heldout_female)[32:38])
  
  # combine
  data_split_1_heldout <- rbind(data_split_1_heldout_male, data_split_1_heldout_female)
  data_split_2_heldout <- rbind(data_split_2_heldout_male, data_split_2_heldout_female)
  data_split_3_heldout <- rbind(data_split_3_heldout_male, data_split_3_heldout_female)
  data_split_4_heldout <- rbind(data_split_4_heldout_male, data_split_4_heldout_female)
  data_split_5_heldout <- rbind(data_split_5_heldout_male, data_split_5_heldout_female)
  
  data_split_1_heldout <- data_split_1_heldout %>% as.data.frame() %>% ungroup()
  data_split_2_heldout <- data_split_2_heldout %>% as.data.frame() %>% ungroup()
  data_split_3_heldout <- data_split_3_heldout %>% as.data.frame() %>% ungroup()
  data_split_4_heldout <- data_split_4_heldout %>% as.data.frame() %>% ungroup()
  data_split_5_heldout <- data_split_5_heldout %>% as.data.frame() %>% ungroup()
  
  
  # train
  # men
  model_male_split_1 <- SuperLearner(Y = data_split_1_male$outcome, X = data_split_1_male %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_1_male$weight, cvControl = list(V = 5))
  model_male_split_2 <- SuperLearner(Y = data_split_2_male$outcome, X = data_split_2_male %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_2_male$weight, cvControl = list(V = 5))
  model_male_split_3 <- SuperLearner(Y = data_split_3_male$outcome, X = data_split_3_male %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_3_male$weight, cvControl = list(V = 5))
  model_male_split_4 <- SuperLearner(Y = data_split_4_male$outcome, X = data_split_4_male %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_4_male$weight, cvControl = list(V = 5))
  model_male_split_5 <- SuperLearner(Y = data_split_5_male$outcome, X = data_split_5_male %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_5_male$weight, cvControl = list(V = 5))
  
  # women
  model_female_split_1 <- SuperLearner(Y = data_split_1_female$outcome, X = data_split_1_female %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_1_female$weight, cvControl = list(V = 5))
  model_female_split_2 <- SuperLearner(Y = data_split_2_female$outcome, X = data_split_2_female %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_2_female$weight, cvControl = list(V = 5))
  model_female_split_3 <- SuperLearner(Y = data_split_3_female$outcome, X = data_split_3_female %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_3_female$weight, cvControl = list(V = 5))
  model_female_split_4 <- SuperLearner(Y = data_split_4_female$outcome, X = data_split_4_female %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_4_female$weight, cvControl = list(V = 5))
  model_female_split_5 <- SuperLearner(Y = data_split_5_female$outcome, X = data_split_5_female %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_5_female$weight, cvControl = list(V = 5))
  
  # predictions
  for (i in sort(unique(data$eseg_class))) {
    
    data_split_1_heldout_male$eseg_class <- factor(i, levels = 1:7)
    data_split_2_heldout_male$eseg_class <- factor(i, levels = 1:7)
    data_split_3_heldout_male$eseg_class <- factor(i, levels = 1:7)
    data_split_4_heldout_male$eseg_class <- factor(i, levels = 1:7)
    data_split_5_heldout_male$eseg_class <- factor(i, levels = 1:7)
    
    data_split_1_heldout_female$eseg_class <- factor(i, levels = 1:7)
    data_split_2_heldout_female$eseg_class <- factor(i, levels = 1:7)
    data_split_3_heldout_female$eseg_class <- factor(i, levels = 1:7)
    data_split_4_heldout_female$eseg_class <- factor(i, levels = 1:7)
    data_split_5_heldout_female$eseg_class <- factor(i, levels = 1:7)
    
    data_split_1_heldout_male$prediction <- as.data.frame(predict(model_male_split_1, newdata = data_split_1_heldout_male %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_2_heldout_male$prediction <- as.data.frame(predict(model_male_split_2, newdata = data_split_2_heldout_male %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_3_heldout_male$prediction <- as.data.frame(predict(model_male_split_3, newdata = data_split_3_heldout_male %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_4_heldout_male$prediction <- as.data.frame(predict(model_male_split_4, newdata = data_split_4_heldout_male %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_5_heldout_male$prediction <- as.data.frame(predict(model_male_split_5, newdata = data_split_5_heldout_male %>% select(all_of(preds_outcome)), type = "response"))$pred
    
    data_split_1_heldout_female$prediction <- as.data.frame(predict(model_female_split_1, newdata = data_split_1_heldout_female %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_2_heldout_female$prediction <- as.data.frame(predict(model_female_split_2, newdata = data_split_2_heldout_female %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_3_heldout_female$prediction <- as.data.frame(predict(model_female_split_3, newdata = data_split_3_heldout_female %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_4_heldout_female$prediction <- as.data.frame(predict(model_female_split_4, newdata = data_split_4_heldout_female %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_5_heldout_female$prediction <- as.data.frame(predict(model_female_split_5, newdata = data_split_5_heldout_female %>% select(all_of(preds_outcome)), type = "response"))$pred
    
    data_split_1_heldout_male <- data_split_1_heldout_male %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_2_heldout_male <- data_split_2_heldout_male %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_3_heldout_male <- data_split_3_heldout_male %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_4_heldout_male <- data_split_4_heldout_male %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_5_heldout_male <- data_split_5_heldout_male %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    
    data_split_1_heldout_female <- data_split_1_heldout_female %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_2_heldout_female <- data_split_2_heldout_female %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_3_heldout_female <- data_split_3_heldout_female %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_4_heldout_female <- data_split_4_heldout_female %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_5_heldout_female <- data_split_5_heldout_female %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    
  }
  
  rm(model_male_split_1, model_male_split_2, model_male_split_3, model_male_split_4, model_male_split_5, model_female_split_1, model_female_split_2, model_female_split_3, model_female_split_4, model_female_split_5)
  
  rm(i)
  
  # combine
  preds_split_1 <- rbind(data_split_1_heldout_male, data_split_1_heldout_female)
  preds_split_2 <- rbind(data_split_2_heldout_male, data_split_2_heldout_female)
  preds_split_3 <- rbind(data_split_3_heldout_male, data_split_3_heldout_female)
  preds_split_4 <- rbind(data_split_4_heldout_male, data_split_4_heldout_female)
  preds_split_5 <- rbind(data_split_5_heldout_male, data_split_5_heldout_female)
  
  preds_split_1 <- preds_split_1 %>% select(orgpid, age, contains("exp_outcome_occ_")) %>% group_by(orgpid, age) %>% slice(1)
  preds_split_2 <- preds_split_2 %>% select(orgpid, age, contains("exp_outcome_occ_")) %>% group_by(orgpid, age) %>% slice(1)
  preds_split_3 <- preds_split_3 %>% select(orgpid, age, contains("exp_outcome_occ_")) %>% group_by(orgpid, age) %>% slice(1)
  preds_split_4 <- preds_split_4 %>% select(orgpid, age, contains("exp_outcome_occ_")) %>% group_by(orgpid, age) %>% slice(1)
  preds_split_5 <- preds_split_5 %>% select(orgpid, age, contains("exp_outcome_occ_")) %>% group_by(orgpid, age) %>% slice(1)
  
  data_split_1_heldout <- data_split_1_heldout %>% left_join(preds_split_1, by = c("orgpid", "age")) %>% ungroup()
  data_split_2_heldout <- data_split_2_heldout %>% left_join(preds_split_2, by = c("orgpid", "age")) %>% ungroup()
  data_split_3_heldout <- data_split_3_heldout %>% left_join(preds_split_3, by = c("orgpid", "age")) %>% ungroup()
  data_split_4_heldout <- data_split_4_heldout %>% left_join(preds_split_4, by = c("orgpid", "age")) %>% ungroup()
  data_split_5_heldout <- data_split_5_heldout %>% left_join(preds_split_5, by = c("orgpid", "age")) %>% ungroup()
  
  
  
  # counterfactual probability of being assigned to the factual occupation
  cf_prob_fc_occ <- data %>% ungroup() %>%
    select(orgpid, age, eseg_class, contains("counterfact_share_occ")) %>%
    pivot_longer(cols = c(4:10), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 23, 23)) %>% 
    filter(eseg_class == occupation) %>%
    rename("cf_prob_fc_occ" = "value") %>%
    select(orgpid, age, cf_prob_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  # inverse of the factual probability of the factual occupation
  inv_fc_prob_fc_occ_split_1 <- data_split_1_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class, contains("prop_occ")) %>%
    pivot_longer(cols = c(4:10), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 10, 10)) %>% 
    filter(eseg_class == occupation) %>%
    mutate(value = 1 / value) %>%
    rename("inv_fc_prob_fc_occ" = "value") %>%
    select(orgpid, age, inv_fc_prob_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup() %>%
    mutate(inv_fc_prob_fc_occ = case_when(inv_fc_prob_fc_occ > quantile(inv_fc_prob_fc_occ, 0.999) ~ quantile(inv_fc_prob_fc_occ, 0.999),
                                          inv_fc_prob_fc_occ < quantile(inv_fc_prob_fc_occ, 0.001) ~ quantile(inv_fc_prob_fc_occ, 0.001),
                                          T ~ inv_fc_prob_fc_occ))
  
  inv_fc_prob_fc_occ_split_2 <- data_split_2_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class, contains("prop_occ")) %>%
    pivot_longer(cols = c(4:10), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 10, 10)) %>% 
    filter(eseg_class == occupation) %>%
    mutate(value = 1 / value) %>%
    rename("inv_fc_prob_fc_occ" = "value") %>%
    select(orgpid, age, inv_fc_prob_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup() %>%
    mutate(inv_fc_prob_fc_occ = case_when(inv_fc_prob_fc_occ > quantile(inv_fc_prob_fc_occ, 0.999) ~ quantile(inv_fc_prob_fc_occ, 0.999),
                                          inv_fc_prob_fc_occ < quantile(inv_fc_prob_fc_occ, 0.001) ~ quantile(inv_fc_prob_fc_occ, 0.001),
                                          T ~ inv_fc_prob_fc_occ))
  
  inv_fc_prob_fc_occ_split_3 <- data_split_3_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class, contains("prop_occ")) %>%
    pivot_longer(cols = c(4:10), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 10, 10)) %>% 
    filter(eseg_class == occupation) %>%
    mutate(value = 1 / value) %>%
    rename("inv_fc_prob_fc_occ" = "value") %>%
    select(orgpid, age, inv_fc_prob_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup() %>%
    mutate(inv_fc_prob_fc_occ = case_when(inv_fc_prob_fc_occ > quantile(inv_fc_prob_fc_occ, 0.999) ~ quantile(inv_fc_prob_fc_occ, 0.999),
                                          inv_fc_prob_fc_occ < quantile(inv_fc_prob_fc_occ, 0.001) ~ quantile(inv_fc_prob_fc_occ, 0.001),
                                          T ~ inv_fc_prob_fc_occ))
  
  inv_fc_prob_fc_occ_split_4 <- data_split_4_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class, contains("prop_occ")) %>%
    pivot_longer(cols = c(4:10), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 10, 10)) %>% 
    filter(eseg_class == occupation) %>%
    mutate(value = 1 / value) %>%
    rename("inv_fc_prob_fc_occ" = "value") %>%
    select(orgpid, age, inv_fc_prob_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup() %>%
    mutate(inv_fc_prob_fc_occ = case_when(inv_fc_prob_fc_occ > quantile(inv_fc_prob_fc_occ, 0.999) ~ quantile(inv_fc_prob_fc_occ, 0.999),
                                          inv_fc_prob_fc_occ < quantile(inv_fc_prob_fc_occ, 0.001) ~ quantile(inv_fc_prob_fc_occ, 0.001),
                                          T ~ inv_fc_prob_fc_occ))
  
  inv_fc_prob_fc_occ_split_5 <- data_split_5_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class, contains("prop_occ")) %>%
    pivot_longer(cols = c(4:10), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 10, 10)) %>% 
    filter(eseg_class == occupation) %>%
    mutate(value = 1 / value) %>%
    rename("inv_fc_prob_fc_occ" = "value") %>%
    select(orgpid, age, inv_fc_prob_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup() %>%
    mutate(inv_fc_prob_fc_occ = case_when(inv_fc_prob_fc_occ > quantile(inv_fc_prob_fc_occ, 0.999) ~ quantile(inv_fc_prob_fc_occ, 0.999),
                                          inv_fc_prob_fc_occ < quantile(inv_fc_prob_fc_occ, 0.001) ~ quantile(inv_fc_prob_fc_occ, 0.001),
                                          T ~ inv_fc_prob_fc_occ))
  
  # outcome predictions
  exp_outcome_fc_occ_split_1 <- data_split_1_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class, contains("exp_outcome_occ")) %>%
    pivot_longer(cols = c(4:10), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 17, 17)) %>% 
    filter(eseg_class == occupation) %>%
    rename("exp_outcome_fc_occ" = "value") %>%
    select(orgpid, age, exp_outcome_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  exp_outcome_fc_occ_split_2 <- data_split_2_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class, contains("exp_outcome_occ")) %>%
    pivot_longer(cols = c(4:10), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 17, 17)) %>% 
    filter(eseg_class == occupation) %>%
    rename("exp_outcome_fc_occ" = "value") %>%
    select(orgpid, age, exp_outcome_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  exp_outcome_fc_occ_split_3 <- data_split_3_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class, contains("exp_outcome_occ")) %>%
    pivot_longer(cols = c(4:10), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 17, 17)) %>% 
    filter(eseg_class == occupation) %>%
    rename("exp_outcome_fc_occ" = "value") %>%
    select(orgpid, age, exp_outcome_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  exp_outcome_fc_occ_split_4 <- data_split_4_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class, contains("exp_outcome_occ")) %>%
    pivot_longer(cols = c(4:10), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 17, 17)) %>% 
    filter(eseg_class == occupation) %>%
    rename("exp_outcome_fc_occ" = "value") %>%
    select(orgpid, age, exp_outcome_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  exp_outcome_fc_occ_split_5 <- data_split_5_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class, contains("exp_outcome_occ")) %>%
    pivot_longer(cols = c(4:10), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 17, 17)) %>% 
    filter(eseg_class == occupation) %>%
    rename("exp_outcome_fc_occ" = "value") %>%
    select(orgpid, age, exp_outcome_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  # create combined weight
  weight_split_1 <- data_split_1_heldout %>% ungroup() %>%
    select(orgpid, age, weight) %>% 
    left_join(cf_prob_fc_occ, by = c("orgpid", "age")) %>%
    left_join(inv_fc_prob_fc_occ_split_1, by = c("orgpid", "age")) %>%
    mutate(combined_weight = weight * cf_prob_fc_occ * inv_fc_prob_fc_occ) %>%
    select(orgpid, age, combined_weight) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  weight_split_2 <- data_split_2_heldout %>% ungroup() %>%
    select(orgpid, age, weight) %>% 
    left_join(cf_prob_fc_occ, by = c("orgpid", "age")) %>%
    left_join(inv_fc_prob_fc_occ_split_2, by = c("orgpid", "age")) %>%
    mutate(combined_weight = weight * cf_prob_fc_occ * inv_fc_prob_fc_occ) %>%
    select(orgpid, age, combined_weight) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  weight_split_3 <- data_split_3_heldout %>% ungroup() %>%
    select(orgpid, age, weight) %>% 
    left_join(cf_prob_fc_occ, by = c("orgpid", "age")) %>%
    left_join(inv_fc_prob_fc_occ_split_3, by = c("orgpid", "age")) %>%
    mutate(combined_weight = weight * cf_prob_fc_occ * inv_fc_prob_fc_occ) %>%
    select(orgpid, age, combined_weight) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  weight_split_4 <- data_split_4_heldout %>% ungroup() %>%
    select(orgpid, age, weight) %>% 
    left_join(cf_prob_fc_occ, by = c("orgpid", "age")) %>%
    left_join(inv_fc_prob_fc_occ_split_4, by = c("orgpid", "age")) %>%
    mutate(combined_weight = weight * cf_prob_fc_occ * inv_fc_prob_fc_occ) %>%
    select(orgpid, age, combined_weight) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  weight_split_5 <- data_split_5_heldout %>% ungroup() %>%
    select(orgpid, age, weight) %>% 
    left_join(cf_prob_fc_occ, by = c("orgpid", "age")) %>%
    left_join(inv_fc_prob_fc_occ_split_5, by = c("orgpid", "age")) %>%
    mutate(combined_weight = weight * cf_prob_fc_occ * inv_fc_prob_fc_occ) %>%
    select(orgpid, age, combined_weight) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  # join
  data_split_1_heldout <- data_split_1_heldout %>% left_join(exp_outcome_fc_occ_split_1, by = c("orgpid", "age")) %>% left_join(weight_split_1, by = c("orgpid", "age")) %>% ungroup()
  data_split_2_heldout <- data_split_2_heldout %>% left_join(exp_outcome_fc_occ_split_2, by = c("orgpid", "age")) %>% left_join(weight_split_2, by = c("orgpid", "age")) %>% ungroup()
  data_split_3_heldout <- data_split_3_heldout %>% left_join(exp_outcome_fc_occ_split_3, by = c("orgpid", "age")) %>% left_join(weight_split_3, by = c("orgpid", "age")) %>% ungroup()
  data_split_4_heldout <- data_split_4_heldout %>% left_join(exp_outcome_fc_occ_split_4, by = c("orgpid", "age")) %>% left_join(weight_split_4, by = c("orgpid", "age")) %>% ungroup()
  data_split_5_heldout <- data_split_5_heldout %>% left_join(exp_outcome_fc_occ_split_5, by = c("orgpid", "age")) %>% left_join(weight_split_5, by = c("orgpid", "age")) %>% ungroup()
  
  # calculate bias correction
  bias_correction_m_split_1 <- data_split_1_heldout %>% filter(female == 0) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_m_split_2 <- data_split_2_heldout %>% filter(female == 0) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_m_split_3 <- data_split_3_heldout %>% filter(female == 0) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_m_split_4 <- data_split_4_heldout %>% filter(female == 0) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_m_split_5 <- data_split_5_heldout %>% filter(female == 0) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_f_split_1 <- data_split_1_heldout %>% filter(female == 1) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_f_split_2 <- data_split_2_heldout %>% filter(female == 1) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_f_split_3 <- data_split_3_heldout %>% filter(female == 1) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_f_split_4 <- data_split_4_heldout %>% filter(female == 1) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_f_split_5 <- data_split_5_heldout %>% filter(female == 1) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_split_1 <- rbind(bias_correction_m_split_1, bias_correction_f_split_1)
  bias_correction_split_2 <- rbind(bias_correction_m_split_2, bias_correction_f_split_2)
  bias_correction_split_3 <- rbind(bias_correction_m_split_3, bias_correction_f_split_3)
  bias_correction_split_4 <- rbind(bias_correction_m_split_4, bias_correction_f_split_4)
  bias_correction_split_5 <- rbind(bias_correction_m_split_5, bias_correction_f_split_5)
  
  # correct bias in outcome predictions
  data_split_1_heldout <- data_split_1_heldout %>% 
    left_join(bias_correction_split_1, by = "female") %>% ungroup() %>%
    mutate_at(vars(contains("exp_outcome_occ_")), ~(. - bias_correction)) %>% 
    mutate_at(vars(contains("exp_outcome_occ_")), ~(case_when(. > 1 ~ 1, . < 0 ~ 0, T ~ .)))
  
  data_split_2_heldout <- data_split_2_heldout %>% 
    left_join(bias_correction_split_2, by = "female") %>% ungroup() %>%
    mutate_at(vars(contains("exp_outcome_occ_")), ~(. - bias_correction)) %>% 
    mutate_at(vars(contains("exp_outcome_occ_")), ~(case_when(. > 1 ~ 1, . < 0 ~ 0, T ~ .)))
  
  data_split_3_heldout <- data_split_3_heldout %>% 
    left_join(bias_correction_split_3, by = "female") %>% ungroup() %>%
    mutate_at(vars(contains("exp_outcome_occ_")), ~(. - bias_correction)) %>% 
    mutate_at(vars(contains("exp_outcome_occ_")), ~(case_when(. > 1 ~ 1, . < 0 ~ 0, T ~ .)))
  
  data_split_4_heldout <- data_split_4_heldout %>% 
    left_join(bias_correction_split_4, by = "female") %>% ungroup() %>%
    mutate_at(vars(contains("exp_outcome_occ_")), ~(. - bias_correction)) %>% 
    mutate_at(vars(contains("exp_outcome_occ_")), ~(case_when(. > 1 ~ 1, . < 0 ~ 0, T ~ .)))
  
  data_split_5_heldout <- data_split_5_heldout %>% 
    left_join(bias_correction_split_5, by = "female") %>% ungroup() %>%
    mutate_at(vars(contains("exp_outcome_occ_")), ~(. - bias_correction)) %>% 
    mutate_at(vars(contains("exp_outcome_occ_")), ~(case_when(. > 1 ~ 1, . < 0 ~ 0, T ~ .)))
  
  # pre-intervention
  levels_pre_female <- data %>% filter(female == 1) %>% ungroup() %>%summarize(exp = weighted.mean(outcome, w = weight) * 100) %>% as.numeric()
  levels_pre_male <- data %>% filter(female == 0) %>% ungroup() %>%summarize(exp = weighted.mean(outcome, w = weight) * 100) %>% as.numeric()
  
  # post-intervention
  # split 1
  levels_post_female_split_1 <- data_split_1_heldout %>% filter(female == 1) %>% mutate(exp = (exp_outcome_occ_1 * counterfact_share_occ_1) + (exp_outcome_occ_2 * counterfact_share_occ_2) + (exp_outcome_occ_3 * counterfact_share_occ_3) + (exp_outcome_occ_4 * counterfact_share_occ_4) + (exp_outcome_occ_5 * counterfact_share_occ_5) + (exp_outcome_occ_6 * counterfact_share_occ_6) + (exp_outcome_occ_7 * counterfact_share_occ_7)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  levels_post_male_split_1 <- data_split_1_heldout %>% filter(female == 0) %>% mutate(exp = (exp_outcome_occ_1 * counterfact_share_occ_1) + (exp_outcome_occ_2 * counterfact_share_occ_2) + (exp_outcome_occ_3 * counterfact_share_occ_3) + (exp_outcome_occ_4 * counterfact_share_occ_4) + (exp_outcome_occ_5 * counterfact_share_occ_5) + (exp_outcome_occ_6 * counterfact_share_occ_6) + (exp_outcome_occ_7 * counterfact_share_occ_7)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  
  # split 2
  levels_post_female_split_2 <- data_split_2_heldout %>% filter(female == 1) %>% mutate(exp = (exp_outcome_occ_1 * counterfact_share_occ_1) + (exp_outcome_occ_2 * counterfact_share_occ_2) + (exp_outcome_occ_3 * counterfact_share_occ_3) + (exp_outcome_occ_4 * counterfact_share_occ_4) + (exp_outcome_occ_5 * counterfact_share_occ_5) + (exp_outcome_occ_6 * counterfact_share_occ_6) + (exp_outcome_occ_7 * counterfact_share_occ_7)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  levels_post_male_split_2 <- data_split_2_heldout %>% filter(female == 0) %>% mutate(exp = (exp_outcome_occ_1 * counterfact_share_occ_1) + (exp_outcome_occ_2 * counterfact_share_occ_2) + (exp_outcome_occ_3 * counterfact_share_occ_3) + (exp_outcome_occ_4 * counterfact_share_occ_4) + (exp_outcome_occ_5 * counterfact_share_occ_5) + (exp_outcome_occ_6 * counterfact_share_occ_6) + (exp_outcome_occ_7 * counterfact_share_occ_7)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  
  # split 3
  levels_post_female_split_3 <- data_split_3_heldout %>% filter(female == 1) %>% mutate(exp = (exp_outcome_occ_1 * counterfact_share_occ_1) + (exp_outcome_occ_2 * counterfact_share_occ_2) + (exp_outcome_occ_3 * counterfact_share_occ_3) + (exp_outcome_occ_4 * counterfact_share_occ_4) + (exp_outcome_occ_5 * counterfact_share_occ_5) + (exp_outcome_occ_6 * counterfact_share_occ_6) + (exp_outcome_occ_7 * counterfact_share_occ_7)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  levels_post_male_split_3 <- data_split_3_heldout %>% filter(female == 0) %>% mutate(exp = (exp_outcome_occ_1 * counterfact_share_occ_1) + (exp_outcome_occ_2 * counterfact_share_occ_2) + (exp_outcome_occ_3 * counterfact_share_occ_3) + (exp_outcome_occ_4 * counterfact_share_occ_4) + (exp_outcome_occ_5 * counterfact_share_occ_5) + (exp_outcome_occ_6 * counterfact_share_occ_6) + (exp_outcome_occ_7 * counterfact_share_occ_7)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  
  # split 4
  levels_post_female_split_4 <- data_split_4_heldout %>% filter(female == 1) %>% mutate(exp = (exp_outcome_occ_1 * counterfact_share_occ_1) + (exp_outcome_occ_2 * counterfact_share_occ_2) + (exp_outcome_occ_3 * counterfact_share_occ_3) + (exp_outcome_occ_4 * counterfact_share_occ_4) + (exp_outcome_occ_5 * counterfact_share_occ_5) + (exp_outcome_occ_6 * counterfact_share_occ_6) + (exp_outcome_occ_7 * counterfact_share_occ_7)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  levels_post_male_split_4 <- data_split_4_heldout %>% filter(female == 0) %>% mutate(exp = (exp_outcome_occ_1 * counterfact_share_occ_1) + (exp_outcome_occ_2 * counterfact_share_occ_2) + (exp_outcome_occ_3 * counterfact_share_occ_3) + (exp_outcome_occ_4 * counterfact_share_occ_4) + (exp_outcome_occ_5 * counterfact_share_occ_5) + (exp_outcome_occ_6 * counterfact_share_occ_6) + (exp_outcome_occ_7 * counterfact_share_occ_7)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  
  # split 5
  levels_post_female_split_5 <- data_split_5_heldout %>% filter(female == 1) %>% mutate(exp = (exp_outcome_occ_1 * counterfact_share_occ_1) + (exp_outcome_occ_2 * counterfact_share_occ_2) + (exp_outcome_occ_3 * counterfact_share_occ_3) + (exp_outcome_occ_4 * counterfact_share_occ_4) + (exp_outcome_occ_5 * counterfact_share_occ_5) + (exp_outcome_occ_6 * counterfact_share_occ_6) + (exp_outcome_occ_7 * counterfact_share_occ_7)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  levels_post_male_split_5 <- data_split_5_heldout %>% filter(female == 0) %>% mutate(exp = (exp_outcome_occ_1 * counterfact_share_occ_1) + (exp_outcome_occ_2 * counterfact_share_occ_2) + (exp_outcome_occ_3 * counterfact_share_occ_3) + (exp_outcome_occ_4 * counterfact_share_occ_4) + (exp_outcome_occ_5 * counterfact_share_occ_5) + (exp_outcome_occ_6 * counterfact_share_occ_6) + (exp_outcome_occ_7 * counterfact_share_occ_7)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  
  # average cross-fits
  levels_post_female <- (levels_post_female_split_1 + levels_post_female_split_2 + levels_post_female_split_3 + levels_post_female_split_4 + levels_post_female_split_5) / 5
  levels_post_male <- (levels_post_male_split_1 + levels_post_male_split_2 + levels_post_male_split_3 + levels_post_male_split_4 + levels_post_male_split_5) / 5
  
  # calculate estimands
  gap_pre <- levels_pre_female - levels_pre_male
  
  gap_post <- levels_post_female - levels_post_male
  
  change <- gap_post - gap_pre
  
  change_pp <- (change / gap_pre) * 100
  
  estimates_men_pre <- data.frame(estimand = "Men (before intervention)", "est" = round(levels_pre_male, 2))
  estimates_women_pre <- data.frame(estimand = "Women (before intervention)", "est" = round(levels_pre_female, 2))
  estimates_men_post <- data.frame(estimand = "Men (after intervention)", "est" = round(levels_post_male, 2))
  estimates_women_post <- data.frame(estimand = "Women (after intervention)", "est" = round(levels_post_female, 2))
  estimates_gap_pre <- data.frame(estimand = "Disparity (before intervention)", "est" = round(gap_pre, 2))
  estimates_gap_post <- data.frame(estimand = "Disparity (after intervention)", "est" = round(gap_post, 2))
  estimates_reduction <- data.frame(estimand = "Change in disparity", "est" = round(change, 2))
  estimates_reduction_pp <- data.frame(estimand = "Change in disparity (pp.)", "est" = round(change_pp, 2))
  
  estimates <- rbind(estimates_men_pre, estimates_women_pre, estimates_men_post, estimates_women_post, estimates_gap_pre, estimates_gap_post, estimates_reduction, estimates_reduction_pp)
  
  rm(list = setdiff(ls(), c("estimates", "data_final", "decompose_cond_dr_xfit", "formula_treatment", "preds_outcome", "sl_models")))
  
  gc()
  
  return(estimates)
  
}





decompose_dr_xfit_intA_detailed <- function(data, formula_treatment, preds_outcome, sl_models) {
  
  # select complete cases
  data <- data %>% na.omit()
  
  # determine counter-factual composition via intervention rule
  composition_cf <- multinom("eseg_class_2 ~ 1", data = data %>% filter(female == 0), family = "binomial", weights = weight)
  
  composition_cf <- as.data.frame(predict(composition_cf, type = "probs")) %>% slice(1)
  
  colnames(composition_cf) <- paste0("counterfact_share_occ_", colnames(composition_cf))
  
  data <- data %>% cross_join(composition_cf)
  
  rm(list = setdiff(ls(), c("data", "formula_treatment", "preds_outcome", "sl_models")))
  
  
  
  # create cross-fitting splits
  split <- split(data, f = rep_len(1:5, nrow(data)))
  
  data_split_1 = rbind(split$`2`, split$`3`, split$`4`, split$`5`)
  data_split_1_heldout = split$`1`
  
  data_split_2 = rbind(split$`1`, split$`3`, split$`4`, split$`5`)
  data_split_2_heldout = split$`2`
  
  data_split_3 = rbind(split$`1`, split$`2`, split$`4`, split$`5`)
  data_split_3_heldout = split$`3`
  
  data_split_4 = rbind(split$`1`, split$`2`, split$`3`, split$`5`)
  data_split_4_heldout = split$`4`
  
  data_split_5 = rbind(split$`1`, split$`2`, split$`3`, split$`4`)
  data_split_5_heldout = split$`5`
  
  rm(split)
  
  
  
  # treatment model
  data_split_1_male <- data_split_1 %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_2_male <- data_split_2 %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_3_male <- data_split_3 %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_4_male <- data_split_4 %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_5_male <- data_split_5 %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  
  data_split_1_female <- data_split_1 %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_2_female <- data_split_2 %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_3_female <- data_split_3 %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_4_female <- data_split_4 %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_5_female <- data_split_5 %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  
  data_split_1_heldout_male <- data_split_1_heldout %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_2_heldout_male <- data_split_2_heldout %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_3_heldout_male <- data_split_3_heldout %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_4_heldout_male <- data_split_4_heldout %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_5_heldout_male <- data_split_5_heldout %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  
  data_split_1_heldout_female <- data_split_1_heldout %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_2_heldout_female <- data_split_2_heldout %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_3_heldout_female <- data_split_3_heldout %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_4_heldout_female <- data_split_4_heldout %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_5_heldout_female <- data_split_5_heldout %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  
  # train
  # men
  model_male_split_1 <- multinom(formula_treatment, data = data_split_1_male, family = "binomial", weights = weight)
  model_male_split_2 <- multinom(formula_treatment, data = data_split_2_male, family = "binomial", weights = weight)
  model_male_split_3 <- multinom(formula_treatment, data = data_split_3_male, family = "binomial", weights = weight)
  model_male_split_4 <- multinom(formula_treatment, data = data_split_4_male, family = "binomial", weights = weight)
  model_male_split_5 <- multinom(formula_treatment, data = data_split_5_male, family = "binomial", weights = weight)
  
  # women
  model_female_split_1 <- multinom(formula_treatment, data = data_split_1_female, family = "binomial", weights = weight)
  model_female_split_2 <- multinom(formula_treatment, data = data_split_2_female, family = "binomial", weights = weight)
  model_female_split_3 <- multinom(formula_treatment, data = data_split_3_female, family = "binomial", weights = weight)
  model_female_split_4 <- multinom(formula_treatment, data = data_split_4_female, family = "binomial", weights = weight)
  model_female_split_5 <- multinom(formula_treatment, data = data_split_5_female, family = "binomial", weights = weight)
  
  # predict using model trained on other split
  # men
  data_split_1_heldout_male <- cbind(data_split_1_heldout_male, as.data.frame(predict(model_male_split_1, newdata = data_split_1_heldout_male, type = "probs")))
  colnames(data_split_1_heldout_male)[58:86] = paste0("prop_occ_", colnames(data_split_1_heldout_male)[58:86])
  
  data_split_2_heldout_male <- cbind(data_split_2_heldout_male, as.data.frame(predict(model_male_split_2, newdata = data_split_2_heldout_male, type = "probs")))
  colnames(data_split_2_heldout_male)[58:86] = paste0("prop_occ_", colnames(data_split_2_heldout_male)[58:86])
  
  data_split_3_heldout_male <- cbind(data_split_3_heldout_male, as.data.frame(predict(model_male_split_3, newdata = data_split_3_heldout_male, type = "probs")))
  colnames(data_split_3_heldout_male)[58:86] = paste0("prop_occ_", colnames(data_split_3_heldout_male)[58:86])
  
  data_split_4_heldout_male <- cbind(data_split_4_heldout_male, as.data.frame(predict(model_male_split_4, newdata = data_split_4_heldout_male, type = "probs")))
  colnames(data_split_4_heldout_male)[58:86] = paste0("prop_occ_", colnames(data_split_4_heldout_male)[58:86])
  
  data_split_5_heldout_male <- cbind(data_split_5_heldout_male, as.data.frame(predict(model_male_split_5, newdata = data_split_5_heldout_male, type = "probs")))
  colnames(data_split_5_heldout_male)[58:86] = paste0("prop_occ_", colnames(data_split_5_heldout_male)[58:86])
  
  # women
  data_split_1_heldout_female <- cbind(data_split_1_heldout_female, as.data.frame(predict(model_female_split_1, newdata = data_split_1_heldout_female, type = "probs")))
  colnames(data_split_1_heldout_female)[58:86] = paste0("prop_occ_", colnames(data_split_1_heldout_female)[58:86])
  
  data_split_2_heldout_female <- cbind(data_split_2_heldout_female, as.data.frame(predict(model_female_split_2, newdata = data_split_2_heldout_female, type = "probs")))
  colnames(data_split_2_heldout_female)[58:86] = paste0("prop_occ_", colnames(data_split_2_heldout_female)[58:86])
  
  data_split_3_heldout_female <- cbind(data_split_3_heldout_female, as.data.frame(predict(model_female_split_3, newdata = data_split_3_heldout_female, type = "probs")))
  colnames(data_split_3_heldout_female)[58:86] = paste0("prop_occ_", colnames(data_split_3_heldout_female)[58:86])
  
  data_split_4_heldout_female <- cbind(data_split_4_heldout_female, as.data.frame(predict(model_female_split_4, newdata = data_split_4_heldout_female, type = "probs")))
  colnames(data_split_4_heldout_female)[58:86] = paste0("prop_occ_", colnames(data_split_4_heldout_female)[58:86])
  
  data_split_5_heldout_female <- cbind(data_split_5_heldout_female, as.data.frame(predict(model_female_split_5, newdata = data_split_5_heldout_female, type = "probs")))
  colnames(data_split_5_heldout_female)[58:86] = paste0("prop_occ_", colnames(data_split_5_heldout_female)[58:86])
  
  # combine
  data_split_1_heldout <- rbind(data_split_1_heldout_male, data_split_1_heldout_female)
  data_split_2_heldout <- rbind(data_split_2_heldout_male, data_split_2_heldout_female)
  data_split_3_heldout <- rbind(data_split_3_heldout_male, data_split_3_heldout_female)
  data_split_4_heldout <- rbind(data_split_4_heldout_male, data_split_4_heldout_female)
  data_split_5_heldout <- rbind(data_split_5_heldout_male, data_split_5_heldout_female)
  
  data_split_1_heldout <- data_split_1_heldout %>% as.data.frame() %>% ungroup()
  data_split_2_heldout <- data_split_2_heldout %>% as.data.frame() %>% ungroup()
  data_split_3_heldout <- data_split_3_heldout %>% as.data.frame() %>% ungroup()
  data_split_4_heldout <- data_split_4_heldout %>% as.data.frame() %>% ungroup()
  data_split_5_heldout <- data_split_5_heldout %>% as.data.frame() %>% ungroup()
    
  
  # train
  # men
  model_male_split_1 <- SuperLearner(Y = data_split_1_male$outcome, X = data_split_1_male %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_1_male$weight, cvControl = list(V = 5))
  model_male_split_2 <- SuperLearner(Y = data_split_2_male$outcome, X = data_split_2_male %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_2_male$weight, cvControl = list(V = 5))
  model_male_split_3 <- SuperLearner(Y = data_split_3_male$outcome, X = data_split_3_male %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_3_male$weight, cvControl = list(V = 5))
  model_male_split_4 <- SuperLearner(Y = data_split_4_male$outcome, X = data_split_4_male %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_4_male$weight, cvControl = list(V = 5))
  model_male_split_5 <- SuperLearner(Y = data_split_5_male$outcome, X = data_split_5_male %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_5_male$weight, cvControl = list(V = 5))
  
  # women
  model_female_split_1 <- SuperLearner(Y = data_split_1_female$outcome, X = data_split_1_female %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_1_female$weight, cvControl = list(V = 5))
  model_female_split_2 <- SuperLearner(Y = data_split_2_female$outcome, X = data_split_2_female %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_2_female$weight, cvControl = list(V = 5))
  model_female_split_3 <- SuperLearner(Y = data_split_3_female$outcome, X = data_split_3_female %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_3_female$weight, cvControl = list(V = 5))
  model_female_split_4 <- SuperLearner(Y = data_split_4_female$outcome, X = data_split_4_female %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_4_female$weight, cvControl = list(V = 5))
  model_female_split_5 <- SuperLearner(Y = data_split_5_female$outcome, X = data_split_5_female %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_5_female$weight, cvControl = list(V = 5))
  
  # predictions
  for (i in levels(data$eseg_class_2)) {
    
    data_split_1_heldout_male$eseg_class_2 <- factor(i, levels = levels(data$eseg_class_2))
    data_split_2_heldout_male$eseg_class_2 <- factor(i, levels = levels(data$eseg_class_2))
    data_split_3_heldout_male$eseg_class_2 <- factor(i, levels = levels(data$eseg_class_2))
    data_split_4_heldout_male$eseg_class_2 <- factor(i, levels = levels(data$eseg_class_2))
    data_split_5_heldout_male$eseg_class_2 <- factor(i, levels = levels(data$eseg_class_2))
    
    data_split_1_heldout_female$eseg_class_2 <- factor(i, levels = levels(data$eseg_class_2))
    data_split_2_heldout_female$eseg_class_2 <- factor(i, levels = levels(data$eseg_class_2))
    data_split_3_heldout_female$eseg_class_2 <- factor(i, levels = levels(data$eseg_class_2))
    data_split_4_heldout_female$eseg_class_2 <- factor(i, levels = levels(data$eseg_class_2))
    data_split_5_heldout_female$eseg_class_2 <- factor(i, levels = levels(data$eseg_class_2))
    
    data_split_1_heldout_male$prediction <- as.data.frame(predict(model_male_split_1, newdata = data_split_1_heldout_male %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_2_heldout_male$prediction <- as.data.frame(predict(model_male_split_2, newdata = data_split_2_heldout_male %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_3_heldout_male$prediction <- as.data.frame(predict(model_male_split_3, newdata = data_split_3_heldout_male %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_4_heldout_male$prediction <- as.data.frame(predict(model_male_split_4, newdata = data_split_4_heldout_male %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_5_heldout_male$prediction <- as.data.frame(predict(model_male_split_5, newdata = data_split_5_heldout_male %>% select(all_of(preds_outcome)), type = "response"))$pred
    
    data_split_1_heldout_female$prediction <- as.data.frame(predict(model_female_split_1, newdata = data_split_1_heldout_female %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_2_heldout_female$prediction <- as.data.frame(predict(model_female_split_2, newdata = data_split_2_heldout_female %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_3_heldout_female$prediction <- as.data.frame(predict(model_female_split_3, newdata = data_split_3_heldout_female %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_4_heldout_female$prediction <- as.data.frame(predict(model_female_split_4, newdata = data_split_4_heldout_female %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_5_heldout_female$prediction <- as.data.frame(predict(model_female_split_5, newdata = data_split_5_heldout_female %>% select(all_of(preds_outcome)), type = "response"))$pred
    
    data_split_1_heldout_male <- data_split_1_heldout_male %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_2_heldout_male <- data_split_2_heldout_male %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_3_heldout_male <- data_split_3_heldout_male %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_4_heldout_male <- data_split_4_heldout_male %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_5_heldout_male <- data_split_5_heldout_male %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    
    data_split_1_heldout_female <- data_split_1_heldout_female %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_2_heldout_female <- data_split_2_heldout_female %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_3_heldout_female <- data_split_3_heldout_female %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_4_heldout_female <- data_split_4_heldout_female %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_5_heldout_female <- data_split_5_heldout_female %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    
    print(i)
    
  }
  
  rm(model_male_split_1, model_male_split_2, model_male_split_3, model_male_split_4, model_male_split_5, model_female_split_1, model_female_split_2, model_female_split_3, model_female_split_4, model_female_split_5)
  
  rm(i)
  
  # combine
  preds_split_1 <- rbind(data_split_1_heldout_male, data_split_1_heldout_female)
  preds_split_2 <- rbind(data_split_2_heldout_male, data_split_2_heldout_female)
  preds_split_3 <- rbind(data_split_3_heldout_male, data_split_3_heldout_female)
  preds_split_4 <- rbind(data_split_4_heldout_male, data_split_4_heldout_female)
  preds_split_5 <- rbind(data_split_5_heldout_male, data_split_5_heldout_female)
  
  preds_split_1 <- preds_split_1 %>% select(orgpid, age, contains("exp_outcome_occ_")) %>% group_by(orgpid, age) %>% slice(1)
  preds_split_2 <- preds_split_2 %>% select(orgpid, age, contains("exp_outcome_occ_")) %>% group_by(orgpid, age) %>% slice(1)
  preds_split_3 <- preds_split_3 %>% select(orgpid, age, contains("exp_outcome_occ_")) %>% group_by(orgpid, age) %>% slice(1)
  preds_split_4 <- preds_split_4 %>% select(orgpid, age, contains("exp_outcome_occ_")) %>% group_by(orgpid, age) %>% slice(1)
  preds_split_5 <- preds_split_5 %>% select(orgpid, age, contains("exp_outcome_occ_")) %>% group_by(orgpid, age) %>% slice(1)
  
  data_split_1_heldout <- data_split_1_heldout %>% left_join(preds_split_1, by = c("orgpid", "age")) %>% ungroup()
  data_split_2_heldout <- data_split_2_heldout %>% left_join(preds_split_2, by = c("orgpid", "age")) %>% ungroup()
  data_split_3_heldout <- data_split_3_heldout %>% left_join(preds_split_3, by = c("orgpid", "age")) %>% ungroup()
  data_split_4_heldout <- data_split_4_heldout %>% left_join(preds_split_4, by = c("orgpid", "age")) %>% ungroup()
  data_split_5_heldout <- data_split_5_heldout %>% left_join(preds_split_5, by = c("orgpid", "age")) %>% ungroup()
  
  
  
  # counterfactual probability of being assigned to the factual occupation
  cf_prob_fc_occ <- data %>% ungroup() %>%
    select(orgpid, age, eseg_class_2, contains("counterfact_share_occ")) %>%
    pivot_longer(cols = c(4:32), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 23, 24)) %>% 
    filter(eseg_class_2 == occupation) %>%
    rename("cf_prob_fc_occ" = "value") %>%
    select(orgpid, age, cf_prob_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  # inverse of the factual probability of the factual occupation
  inv_fc_prob_fc_occ_split_1 <- data_split_1_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class_2, contains("prop_occ")) %>%
    pivot_longer(cols = c(4:32), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 10, 11)) %>% 
    filter(eseg_class_2 == occupation) %>%
    mutate(value = 1 / value) %>%
    rename("inv_fc_prob_fc_occ" = "value") %>%
    select(orgpid, age, inv_fc_prob_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup() %>%
    mutate(inv_fc_prob_fc_occ = case_when(inv_fc_prob_fc_occ > quantile(inv_fc_prob_fc_occ, 0.999) ~ quantile(inv_fc_prob_fc_occ, 0.999),
                                          inv_fc_prob_fc_occ < quantile(inv_fc_prob_fc_occ, 0.001) ~ quantile(inv_fc_prob_fc_occ, 0.001),
                                          T ~ inv_fc_prob_fc_occ))
  
  inv_fc_prob_fc_occ_split_2 <- data_split_2_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class_2, contains("prop_occ")) %>%
    pivot_longer(cols = c(4:32), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 10, 11)) %>% 
    filter(eseg_class_2 == occupation) %>%
    mutate(value = 1 / value) %>%
    rename("inv_fc_prob_fc_occ" = "value") %>%
    select(orgpid, age, inv_fc_prob_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup() %>%
    mutate(inv_fc_prob_fc_occ = case_when(inv_fc_prob_fc_occ > quantile(inv_fc_prob_fc_occ, 0.999) ~ quantile(inv_fc_prob_fc_occ, 0.999),
                                          inv_fc_prob_fc_occ < quantile(inv_fc_prob_fc_occ, 0.001) ~ quantile(inv_fc_prob_fc_occ, 0.001),
                                          T ~ inv_fc_prob_fc_occ))
  
  inv_fc_prob_fc_occ_split_3 <- data_split_3_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class_2, contains("prop_occ")) %>%
    pivot_longer(cols = c(4:32), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 10, 11)) %>% 
    filter(eseg_class_2 == occupation) %>%
    mutate(value = 1 / value) %>%
    rename("inv_fc_prob_fc_occ" = "value") %>%
    select(orgpid, age, inv_fc_prob_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup() %>%
    mutate(inv_fc_prob_fc_occ = case_when(inv_fc_prob_fc_occ > quantile(inv_fc_prob_fc_occ, 0.999) ~ quantile(inv_fc_prob_fc_occ, 0.999),
                                          inv_fc_prob_fc_occ < quantile(inv_fc_prob_fc_occ, 0.001) ~ quantile(inv_fc_prob_fc_occ, 0.001),
                                          T ~ inv_fc_prob_fc_occ))
  
  inv_fc_prob_fc_occ_split_4 <- data_split_4_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class_2, contains("prop_occ")) %>%
    pivot_longer(cols = c(4:32), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 10, 11)) %>% 
    filter(eseg_class_2 == occupation) %>%
    mutate(value = 1 / value) %>%
    rename("inv_fc_prob_fc_occ" = "value") %>%
    select(orgpid, age, inv_fc_prob_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup() %>%
    mutate(inv_fc_prob_fc_occ = case_when(inv_fc_prob_fc_occ > quantile(inv_fc_prob_fc_occ, 0.999) ~ quantile(inv_fc_prob_fc_occ, 0.999),
                                          inv_fc_prob_fc_occ < quantile(inv_fc_prob_fc_occ, 0.001) ~ quantile(inv_fc_prob_fc_occ, 0.001),
                                          T ~ inv_fc_prob_fc_occ))
  
  inv_fc_prob_fc_occ_split_5 <- data_split_5_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class_2, contains("prop_occ")) %>%
    pivot_longer(cols = c(4:32), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 10, 11)) %>% 
    filter(eseg_class_2 == occupation) %>%
    mutate(value = 1 / value) %>%
    rename("inv_fc_prob_fc_occ" = "value") %>%
    select(orgpid, age, inv_fc_prob_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup() %>%
    mutate(inv_fc_prob_fc_occ = case_when(inv_fc_prob_fc_occ > quantile(inv_fc_prob_fc_occ, 0.999) ~ quantile(inv_fc_prob_fc_occ, 0.999),
                                          inv_fc_prob_fc_occ < quantile(inv_fc_prob_fc_occ, 0.001) ~ quantile(inv_fc_prob_fc_occ, 0.001),
                                          T ~ inv_fc_prob_fc_occ))
  
  # outcome predictions
  exp_outcome_fc_occ_split_1 <- data_split_1_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class_2, contains("exp_outcome_occ")) %>%
    pivot_longer(cols = c(4:32), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 17, 18)) %>% 
    filter(eseg_class_2 == occupation) %>%
    rename("exp_outcome_fc_occ" = "value") %>%
    select(orgpid, age, exp_outcome_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  exp_outcome_fc_occ_split_2 <- data_split_2_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class_2, contains("exp_outcome_occ")) %>%
    pivot_longer(cols = c(4:32), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 17, 18)) %>% 
    filter(eseg_class_2 == occupation) %>%
    rename("exp_outcome_fc_occ" = "value") %>%
    select(orgpid, age, exp_outcome_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  exp_outcome_fc_occ_split_3 <- data_split_3_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class_2, contains("exp_outcome_occ")) %>%
    pivot_longer(cols = c(4:32), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 17, 18)) %>% 
    filter(eseg_class_2 == occupation) %>%
    rename("exp_outcome_fc_occ" = "value") %>%
    select(orgpid, age, exp_outcome_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  exp_outcome_fc_occ_split_4 <- data_split_4_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class_2, contains("exp_outcome_occ")) %>%
    pivot_longer(cols = c(4:32), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 17, 18)) %>% 
    filter(eseg_class_2 == occupation) %>%
    rename("exp_outcome_fc_occ" = "value") %>%
    select(orgpid, age, exp_outcome_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  exp_outcome_fc_occ_split_5 <- data_split_5_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class_2, contains("exp_outcome_occ")) %>%
    pivot_longer(cols = c(4:32), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 17, 18)) %>% 
    filter(eseg_class_2 == occupation) %>%
    rename("exp_outcome_fc_occ" = "value") %>%
    select(orgpid, age, exp_outcome_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  # create combined weight
  weight_split_1 <- data_split_1_heldout %>% ungroup() %>%
    select(orgpid, age, weight) %>% 
    left_join(cf_prob_fc_occ, by = c("orgpid", "age")) %>%
    left_join(inv_fc_prob_fc_occ_split_1, by = c("orgpid", "age")) %>%
    mutate(combined_weight = weight * cf_prob_fc_occ * inv_fc_prob_fc_occ) %>%
    select(orgpid, age, combined_weight) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  weight_split_2 <- data_split_2_heldout %>% ungroup() %>%
    select(orgpid, age, weight) %>% 
    left_join(cf_prob_fc_occ, by = c("orgpid", "age")) %>%
    left_join(inv_fc_prob_fc_occ_split_2, by = c("orgpid", "age")) %>%
    mutate(combined_weight = weight * cf_prob_fc_occ * inv_fc_prob_fc_occ) %>%
    select(orgpid, age, combined_weight) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  weight_split_3 <- data_split_3_heldout %>% ungroup() %>%
    select(orgpid, age, weight) %>% 
    left_join(cf_prob_fc_occ, by = c("orgpid", "age")) %>%
    left_join(inv_fc_prob_fc_occ_split_3, by = c("orgpid", "age")) %>%
    mutate(combined_weight = weight * cf_prob_fc_occ * inv_fc_prob_fc_occ) %>%
    select(orgpid, age, combined_weight) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  weight_split_4 <- data_split_4_heldout %>% ungroup() %>%
    select(orgpid, age, weight) %>% 
    left_join(cf_prob_fc_occ, by = c("orgpid", "age")) %>%
    left_join(inv_fc_prob_fc_occ_split_4, by = c("orgpid", "age")) %>%
    mutate(combined_weight = weight * cf_prob_fc_occ * inv_fc_prob_fc_occ) %>%
    select(orgpid, age, combined_weight) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  weight_split_5 <- data_split_5_heldout %>% ungroup() %>%
    select(orgpid, age, weight) %>% 
    left_join(cf_prob_fc_occ, by = c("orgpid", "age")) %>%
    left_join(inv_fc_prob_fc_occ_split_5, by = c("orgpid", "age")) %>%
    mutate(combined_weight = weight * cf_prob_fc_occ * inv_fc_prob_fc_occ) %>%
    select(orgpid, age, combined_weight) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  # join
  data_split_1_heldout <- data_split_1_heldout %>% left_join(exp_outcome_fc_occ_split_1, by = c("orgpid", "age")) %>% left_join(weight_split_1, by = c("orgpid", "age")) %>% ungroup()
  data_split_2_heldout <- data_split_2_heldout %>% left_join(exp_outcome_fc_occ_split_2, by = c("orgpid", "age")) %>% left_join(weight_split_2, by = c("orgpid", "age")) %>% ungroup()
  data_split_3_heldout <- data_split_3_heldout %>% left_join(exp_outcome_fc_occ_split_3, by = c("orgpid", "age")) %>% left_join(weight_split_3, by = c("orgpid", "age")) %>% ungroup()
  data_split_4_heldout <- data_split_4_heldout %>% left_join(exp_outcome_fc_occ_split_4, by = c("orgpid", "age")) %>% left_join(weight_split_4, by = c("orgpid", "age")) %>% ungroup()
  data_split_5_heldout <- data_split_5_heldout %>% left_join(exp_outcome_fc_occ_split_5, by = c("orgpid", "age")) %>% left_join(weight_split_5, by = c("orgpid", "age")) %>% ungroup()
  
  # calculate bias correction
  bias_correction_m_split_1 <- data_split_1_heldout %>% filter(female == 0) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_m_split_2 <- data_split_2_heldout %>% filter(female == 0) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_m_split_3 <- data_split_3_heldout %>% filter(female == 0) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_m_split_4 <- data_split_4_heldout %>% filter(female == 0) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_m_split_5 <- data_split_5_heldout %>% filter(female == 0) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_f_split_1 <- data_split_1_heldout %>% filter(female == 1) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_f_split_2 <- data_split_2_heldout %>% filter(female == 1) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_f_split_3 <- data_split_3_heldout %>% filter(female == 1) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_f_split_4 <- data_split_4_heldout %>% filter(female == 1) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_f_split_5 <- data_split_5_heldout %>% filter(female == 1) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_split_1 <- rbind(bias_correction_m_split_1, bias_correction_f_split_1)
  bias_correction_split_2 <- rbind(bias_correction_m_split_2, bias_correction_f_split_2)
  bias_correction_split_3 <- rbind(bias_correction_m_split_3, bias_correction_f_split_3)
  bias_correction_split_4 <- rbind(bias_correction_m_split_4, bias_correction_f_split_4)
  bias_correction_split_5 <- rbind(bias_correction_m_split_5, bias_correction_f_split_5)
  
  # correct bias in outcome predictions
  data_split_1_heldout <- data_split_1_heldout %>% 
    left_join(bias_correction_split_1, by = "female") %>% ungroup() %>%
    mutate_at(vars(contains("exp_outcome_occ_")), ~(. - bias_correction)) %>% 
    mutate_at(vars(contains("exp_outcome_occ_")), ~(case_when(. > 1 ~ 1, . < 0 ~ 0, T ~ .)))
  
  data_split_2_heldout <- data_split_2_heldout %>% 
    left_join(bias_correction_split_2, by = "female") %>% ungroup() %>%
    mutate_at(vars(contains("exp_outcome_occ_")), ~(. - bias_correction)) %>% 
    mutate_at(vars(contains("exp_outcome_occ_")), ~(case_when(. > 1 ~ 1, . < 0 ~ 0, T ~ .)))
  
  data_split_3_heldout <- data_split_3_heldout %>% 
    left_join(bias_correction_split_3, by = "female") %>% ungroup() %>%
    mutate_at(vars(contains("exp_outcome_occ_")), ~(. - bias_correction)) %>% 
    mutate_at(vars(contains("exp_outcome_occ_")), ~(case_when(. > 1 ~ 1, . < 0 ~ 0, T ~ .)))
  
  data_split_4_heldout <- data_split_4_heldout %>% 
    left_join(bias_correction_split_4, by = "female") %>% ungroup() %>%
    mutate_at(vars(contains("exp_outcome_occ_")), ~(. - bias_correction)) %>% 
    mutate_at(vars(contains("exp_outcome_occ_")), ~(case_when(. > 1 ~ 1, . < 0 ~ 0, T ~ .)))
  
  data_split_5_heldout <- data_split_5_heldout %>% 
    left_join(bias_correction_split_5, by = "female") %>% ungroup() %>%
    mutate_at(vars(contains("exp_outcome_occ_")), ~(. - bias_correction)) %>% 
    mutate_at(vars(contains("exp_outcome_occ_")), ~(case_when(. > 1 ~ 1, . < 0 ~ 0, T ~ .)))
  
  # pre-intervention
  levels_pre_female <- data %>% filter(female == 1) %>% ungroup() %>%summarize(exp = weighted.mean(outcome, w = weight) * 100) %>% as.numeric()
  levels_pre_male <- data %>% filter(female == 0) %>% ungroup() %>%summarize(exp = weighted.mean(outcome, w = weight) * 100) %>% as.numeric()
  
  # post-intervention
  # split 1
  levels_post_female_split_1 <- data_split_1_heldout %>% filter(female == 1) %>% mutate(exp = (exp_outcome_occ_24 * counterfact_share_occ_24) + (exp_outcome_occ_33 * counterfact_share_occ_33) + (exp_outcome_occ_51 * counterfact_share_occ_51) + (exp_outcome_occ_25 * counterfact_share_occ_25) + (exp_outcome_occ_71 * counterfact_share_occ_71) + (exp_outcome_occ_21 * counterfact_share_occ_21) + (exp_outcome_occ_23 * counterfact_share_occ_23) + (exp_outcome_occ_42 * counterfact_share_occ_42) + (exp_outcome_occ_65 * counterfact_share_occ_65) + (exp_outcome_occ_61 * counterfact_share_occ_61) + (exp_outcome_occ_72 * counterfact_share_occ_72) + (exp_outcome_occ_13 * counterfact_share_occ_13) + (exp_outcome_occ_63 * counterfact_share_occ_63) + (exp_outcome_occ_34 * counterfact_share_occ_34) + (exp_outcome_occ_22 * counterfact_share_occ_22) + (exp_outcome_occ_31 * counterfact_share_occ_31) + (exp_outcome_occ_64 * counterfact_share_occ_64) + (exp_outcome_occ_73 * counterfact_share_occ_73) + (exp_outcome_occ_62 * counterfact_share_occ_62) + (exp_outcome_occ_54 * counterfact_share_occ_54) + (exp_outcome_occ_74 * counterfact_share_occ_74) + (exp_outcome_occ_11 * counterfact_share_occ_11) + (exp_outcome_occ_32 * counterfact_share_occ_32) + (exp_outcome_occ_43 * counterfact_share_occ_43) + (exp_outcome_occ_52 * counterfact_share_occ_52) + (exp_outcome_occ_53 * counterfact_share_occ_53) + (exp_outcome_occ_14 * counterfact_share_occ_14) + (exp_outcome_occ_12 * counterfact_share_occ_12) + (exp_outcome_occ_41 * counterfact_share_occ_41)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  levels_post_male_split_1 <- data_split_1_heldout %>% filter(female == 0) %>% mutate(exp = (exp_outcome_occ_24 * counterfact_share_occ_24) + (exp_outcome_occ_33 * counterfact_share_occ_33) + (exp_outcome_occ_51 * counterfact_share_occ_51) + (exp_outcome_occ_25 * counterfact_share_occ_25) + (exp_outcome_occ_71 * counterfact_share_occ_71) + (exp_outcome_occ_21 * counterfact_share_occ_21) + (exp_outcome_occ_23 * counterfact_share_occ_23) + (exp_outcome_occ_42 * counterfact_share_occ_42) + (exp_outcome_occ_65 * counterfact_share_occ_65) + (exp_outcome_occ_61 * counterfact_share_occ_61) + (exp_outcome_occ_72 * counterfact_share_occ_72) + (exp_outcome_occ_13 * counterfact_share_occ_13) + (exp_outcome_occ_63 * counterfact_share_occ_63) + (exp_outcome_occ_34 * counterfact_share_occ_34) + (exp_outcome_occ_22 * counterfact_share_occ_22) + (exp_outcome_occ_31 * counterfact_share_occ_31) + (exp_outcome_occ_64 * counterfact_share_occ_64) + (exp_outcome_occ_73 * counterfact_share_occ_73) + (exp_outcome_occ_62 * counterfact_share_occ_62) + (exp_outcome_occ_54 * counterfact_share_occ_54) + (exp_outcome_occ_74 * counterfact_share_occ_74) + (exp_outcome_occ_11 * counterfact_share_occ_11) + (exp_outcome_occ_32 * counterfact_share_occ_32) + (exp_outcome_occ_43 * counterfact_share_occ_43) + (exp_outcome_occ_52 * counterfact_share_occ_52) + (exp_outcome_occ_53 * counterfact_share_occ_53) + (exp_outcome_occ_14 * counterfact_share_occ_14) + (exp_outcome_occ_12 * counterfact_share_occ_12) + (exp_outcome_occ_41 * counterfact_share_occ_41)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  
  # split 2
  levels_post_female_split_2 <- data_split_2_heldout %>% filter(female == 1) %>% mutate(exp = (exp_outcome_occ_24 * counterfact_share_occ_24) + (exp_outcome_occ_33 * counterfact_share_occ_33) + (exp_outcome_occ_51 * counterfact_share_occ_51) + (exp_outcome_occ_25 * counterfact_share_occ_25) + (exp_outcome_occ_71 * counterfact_share_occ_71) + (exp_outcome_occ_21 * counterfact_share_occ_21) + (exp_outcome_occ_23 * counterfact_share_occ_23) + (exp_outcome_occ_42 * counterfact_share_occ_42) + (exp_outcome_occ_65 * counterfact_share_occ_65) + (exp_outcome_occ_61 * counterfact_share_occ_61) + (exp_outcome_occ_72 * counterfact_share_occ_72) + (exp_outcome_occ_13 * counterfact_share_occ_13) + (exp_outcome_occ_63 * counterfact_share_occ_63) + (exp_outcome_occ_34 * counterfact_share_occ_34) + (exp_outcome_occ_22 * counterfact_share_occ_22) + (exp_outcome_occ_31 * counterfact_share_occ_31) + (exp_outcome_occ_64 * counterfact_share_occ_64) + (exp_outcome_occ_73 * counterfact_share_occ_73) + (exp_outcome_occ_62 * counterfact_share_occ_62) + (exp_outcome_occ_54 * counterfact_share_occ_54) + (exp_outcome_occ_74 * counterfact_share_occ_74) + (exp_outcome_occ_11 * counterfact_share_occ_11) + (exp_outcome_occ_32 * counterfact_share_occ_32) + (exp_outcome_occ_43 * counterfact_share_occ_43) + (exp_outcome_occ_52 * counterfact_share_occ_52) + (exp_outcome_occ_53 * counterfact_share_occ_53) + (exp_outcome_occ_14 * counterfact_share_occ_14) + (exp_outcome_occ_12 * counterfact_share_occ_12) + (exp_outcome_occ_41 * counterfact_share_occ_41)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  levels_post_male_split_2 <- data_split_2_heldout %>% filter(female == 0) %>% mutate(exp = (exp_outcome_occ_24 * counterfact_share_occ_24) + (exp_outcome_occ_33 * counterfact_share_occ_33) + (exp_outcome_occ_51 * counterfact_share_occ_51) + (exp_outcome_occ_25 * counterfact_share_occ_25) + (exp_outcome_occ_71 * counterfact_share_occ_71) + (exp_outcome_occ_21 * counterfact_share_occ_21) + (exp_outcome_occ_23 * counterfact_share_occ_23) + (exp_outcome_occ_42 * counterfact_share_occ_42) + (exp_outcome_occ_65 * counterfact_share_occ_65) + (exp_outcome_occ_61 * counterfact_share_occ_61) + (exp_outcome_occ_72 * counterfact_share_occ_72) + (exp_outcome_occ_13 * counterfact_share_occ_13) + (exp_outcome_occ_63 * counterfact_share_occ_63) + (exp_outcome_occ_34 * counterfact_share_occ_34) + (exp_outcome_occ_22 * counterfact_share_occ_22) + (exp_outcome_occ_31 * counterfact_share_occ_31) + (exp_outcome_occ_64 * counterfact_share_occ_64) + (exp_outcome_occ_73 * counterfact_share_occ_73) + (exp_outcome_occ_62 * counterfact_share_occ_62) + (exp_outcome_occ_54 * counterfact_share_occ_54) + (exp_outcome_occ_74 * counterfact_share_occ_74) + (exp_outcome_occ_11 * counterfact_share_occ_11) + (exp_outcome_occ_32 * counterfact_share_occ_32) + (exp_outcome_occ_43 * counterfact_share_occ_43) + (exp_outcome_occ_52 * counterfact_share_occ_52) + (exp_outcome_occ_53 * counterfact_share_occ_53) + (exp_outcome_occ_14 * counterfact_share_occ_14) + (exp_outcome_occ_12 * counterfact_share_occ_12) + (exp_outcome_occ_41 * counterfact_share_occ_41)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  
  # split 3
  levels_post_female_split_3 <- data_split_3_heldout %>% filter(female == 1) %>% mutate(exp = (exp_outcome_occ_24 * counterfact_share_occ_24) + (exp_outcome_occ_33 * counterfact_share_occ_33) + (exp_outcome_occ_51 * counterfact_share_occ_51) + (exp_outcome_occ_25 * counterfact_share_occ_25) + (exp_outcome_occ_71 * counterfact_share_occ_71) + (exp_outcome_occ_21 * counterfact_share_occ_21) + (exp_outcome_occ_23 * counterfact_share_occ_23) + (exp_outcome_occ_42 * counterfact_share_occ_42) + (exp_outcome_occ_65 * counterfact_share_occ_65) + (exp_outcome_occ_61 * counterfact_share_occ_61) + (exp_outcome_occ_72 * counterfact_share_occ_72) + (exp_outcome_occ_13 * counterfact_share_occ_13) + (exp_outcome_occ_63 * counterfact_share_occ_63) + (exp_outcome_occ_34 * counterfact_share_occ_34) + (exp_outcome_occ_22 * counterfact_share_occ_22) + (exp_outcome_occ_31 * counterfact_share_occ_31) + (exp_outcome_occ_64 * counterfact_share_occ_64) + (exp_outcome_occ_73 * counterfact_share_occ_73) + (exp_outcome_occ_62 * counterfact_share_occ_62) + (exp_outcome_occ_54 * counterfact_share_occ_54) + (exp_outcome_occ_74 * counterfact_share_occ_74) + (exp_outcome_occ_11 * counterfact_share_occ_11) + (exp_outcome_occ_32 * counterfact_share_occ_32) + (exp_outcome_occ_43 * counterfact_share_occ_43) + (exp_outcome_occ_52 * counterfact_share_occ_52) + (exp_outcome_occ_53 * counterfact_share_occ_53) + (exp_outcome_occ_14 * counterfact_share_occ_14) + (exp_outcome_occ_12 * counterfact_share_occ_12) + (exp_outcome_occ_41 * counterfact_share_occ_41)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  levels_post_male_split_3 <- data_split_3_heldout %>% filter(female == 0) %>% mutate(exp = (exp_outcome_occ_24 * counterfact_share_occ_24) + (exp_outcome_occ_33 * counterfact_share_occ_33) + (exp_outcome_occ_51 * counterfact_share_occ_51) + (exp_outcome_occ_25 * counterfact_share_occ_25) + (exp_outcome_occ_71 * counterfact_share_occ_71) + (exp_outcome_occ_21 * counterfact_share_occ_21) + (exp_outcome_occ_23 * counterfact_share_occ_23) + (exp_outcome_occ_42 * counterfact_share_occ_42) + (exp_outcome_occ_65 * counterfact_share_occ_65) + (exp_outcome_occ_61 * counterfact_share_occ_61) + (exp_outcome_occ_72 * counterfact_share_occ_72) + (exp_outcome_occ_13 * counterfact_share_occ_13) + (exp_outcome_occ_63 * counterfact_share_occ_63) + (exp_outcome_occ_34 * counterfact_share_occ_34) + (exp_outcome_occ_22 * counterfact_share_occ_22) + (exp_outcome_occ_31 * counterfact_share_occ_31) + (exp_outcome_occ_64 * counterfact_share_occ_64) + (exp_outcome_occ_73 * counterfact_share_occ_73) + (exp_outcome_occ_62 * counterfact_share_occ_62) + (exp_outcome_occ_54 * counterfact_share_occ_54) + (exp_outcome_occ_74 * counterfact_share_occ_74) + (exp_outcome_occ_11 * counterfact_share_occ_11) + (exp_outcome_occ_32 * counterfact_share_occ_32) + (exp_outcome_occ_43 * counterfact_share_occ_43) + (exp_outcome_occ_52 * counterfact_share_occ_52) + (exp_outcome_occ_53 * counterfact_share_occ_53) + (exp_outcome_occ_14 * counterfact_share_occ_14) + (exp_outcome_occ_12 * counterfact_share_occ_12) + (exp_outcome_occ_41 * counterfact_share_occ_41)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  
  # split 4
  levels_post_female_split_4 <- data_split_4_heldout %>% filter(female == 1) %>% mutate(exp = (exp_outcome_occ_24 * counterfact_share_occ_24) + (exp_outcome_occ_33 * counterfact_share_occ_33) + (exp_outcome_occ_51 * counterfact_share_occ_51) + (exp_outcome_occ_25 * counterfact_share_occ_25) + (exp_outcome_occ_71 * counterfact_share_occ_71) + (exp_outcome_occ_21 * counterfact_share_occ_21) + (exp_outcome_occ_23 * counterfact_share_occ_23) + (exp_outcome_occ_42 * counterfact_share_occ_42) + (exp_outcome_occ_65 * counterfact_share_occ_65) + (exp_outcome_occ_61 * counterfact_share_occ_61) + (exp_outcome_occ_72 * counterfact_share_occ_72) + (exp_outcome_occ_13 * counterfact_share_occ_13) + (exp_outcome_occ_63 * counterfact_share_occ_63) + (exp_outcome_occ_34 * counterfact_share_occ_34) + (exp_outcome_occ_22 * counterfact_share_occ_22) + (exp_outcome_occ_31 * counterfact_share_occ_31) + (exp_outcome_occ_64 * counterfact_share_occ_64) + (exp_outcome_occ_73 * counterfact_share_occ_73) + (exp_outcome_occ_62 * counterfact_share_occ_62) + (exp_outcome_occ_54 * counterfact_share_occ_54) + (exp_outcome_occ_74 * counterfact_share_occ_74) + (exp_outcome_occ_11 * counterfact_share_occ_11) + (exp_outcome_occ_32 * counterfact_share_occ_32) + (exp_outcome_occ_43 * counterfact_share_occ_43) + (exp_outcome_occ_52 * counterfact_share_occ_52) + (exp_outcome_occ_53 * counterfact_share_occ_53) + (exp_outcome_occ_14 * counterfact_share_occ_14) + (exp_outcome_occ_12 * counterfact_share_occ_12) + (exp_outcome_occ_41 * counterfact_share_occ_41)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  levels_post_male_split_4 <- data_split_4_heldout %>% filter(female == 0) %>% mutate(exp = (exp_outcome_occ_24 * counterfact_share_occ_24) + (exp_outcome_occ_33 * counterfact_share_occ_33) + (exp_outcome_occ_51 * counterfact_share_occ_51) + (exp_outcome_occ_25 * counterfact_share_occ_25) + (exp_outcome_occ_71 * counterfact_share_occ_71) + (exp_outcome_occ_21 * counterfact_share_occ_21) + (exp_outcome_occ_23 * counterfact_share_occ_23) + (exp_outcome_occ_42 * counterfact_share_occ_42) + (exp_outcome_occ_65 * counterfact_share_occ_65) + (exp_outcome_occ_61 * counterfact_share_occ_61) + (exp_outcome_occ_72 * counterfact_share_occ_72) + (exp_outcome_occ_13 * counterfact_share_occ_13) + (exp_outcome_occ_63 * counterfact_share_occ_63) + (exp_outcome_occ_34 * counterfact_share_occ_34) + (exp_outcome_occ_22 * counterfact_share_occ_22) + (exp_outcome_occ_31 * counterfact_share_occ_31) + (exp_outcome_occ_64 * counterfact_share_occ_64) + (exp_outcome_occ_73 * counterfact_share_occ_73) + (exp_outcome_occ_62 * counterfact_share_occ_62) + (exp_outcome_occ_54 * counterfact_share_occ_54) + (exp_outcome_occ_74 * counterfact_share_occ_74) + (exp_outcome_occ_11 * counterfact_share_occ_11) + (exp_outcome_occ_32 * counterfact_share_occ_32) + (exp_outcome_occ_43 * counterfact_share_occ_43) + (exp_outcome_occ_52 * counterfact_share_occ_52) + (exp_outcome_occ_53 * counterfact_share_occ_53) + (exp_outcome_occ_14 * counterfact_share_occ_14) + (exp_outcome_occ_12 * counterfact_share_occ_12) + (exp_outcome_occ_41 * counterfact_share_occ_41)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  
  # split 5
  levels_post_female_split_5 <- data_split_5_heldout %>% filter(female == 1) %>% mutate(exp = (exp_outcome_occ_24 * counterfact_share_occ_24) + (exp_outcome_occ_33 * counterfact_share_occ_33) + (exp_outcome_occ_51 * counterfact_share_occ_51) + (exp_outcome_occ_25 * counterfact_share_occ_25) + (exp_outcome_occ_71 * counterfact_share_occ_71) + (exp_outcome_occ_21 * counterfact_share_occ_21) + (exp_outcome_occ_23 * counterfact_share_occ_23) + (exp_outcome_occ_42 * counterfact_share_occ_42) + (exp_outcome_occ_65 * counterfact_share_occ_65) + (exp_outcome_occ_61 * counterfact_share_occ_61) + (exp_outcome_occ_72 * counterfact_share_occ_72) + (exp_outcome_occ_13 * counterfact_share_occ_13) + (exp_outcome_occ_63 * counterfact_share_occ_63) + (exp_outcome_occ_34 * counterfact_share_occ_34) + (exp_outcome_occ_22 * counterfact_share_occ_22) + (exp_outcome_occ_31 * counterfact_share_occ_31) + (exp_outcome_occ_64 * counterfact_share_occ_64) + (exp_outcome_occ_73 * counterfact_share_occ_73) + (exp_outcome_occ_62 * counterfact_share_occ_62) + (exp_outcome_occ_54 * counterfact_share_occ_54) + (exp_outcome_occ_74 * counterfact_share_occ_74) + (exp_outcome_occ_11 * counterfact_share_occ_11) + (exp_outcome_occ_32 * counterfact_share_occ_32) + (exp_outcome_occ_43 * counterfact_share_occ_43) + (exp_outcome_occ_52 * counterfact_share_occ_52) + (exp_outcome_occ_53 * counterfact_share_occ_53) + (exp_outcome_occ_14 * counterfact_share_occ_14) + (exp_outcome_occ_12 * counterfact_share_occ_12) + (exp_outcome_occ_41 * counterfact_share_occ_41)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  levels_post_male_split_5 <- data_split_5_heldout %>% filter(female == 0) %>% mutate(exp = (exp_outcome_occ_24 * counterfact_share_occ_24) + (exp_outcome_occ_33 * counterfact_share_occ_33) + (exp_outcome_occ_51 * counterfact_share_occ_51) + (exp_outcome_occ_25 * counterfact_share_occ_25) + (exp_outcome_occ_71 * counterfact_share_occ_71) + (exp_outcome_occ_21 * counterfact_share_occ_21) + (exp_outcome_occ_23 * counterfact_share_occ_23) + (exp_outcome_occ_42 * counterfact_share_occ_42) + (exp_outcome_occ_65 * counterfact_share_occ_65) + (exp_outcome_occ_61 * counterfact_share_occ_61) + (exp_outcome_occ_72 * counterfact_share_occ_72) + (exp_outcome_occ_13 * counterfact_share_occ_13) + (exp_outcome_occ_63 * counterfact_share_occ_63) + (exp_outcome_occ_34 * counterfact_share_occ_34) + (exp_outcome_occ_22 * counterfact_share_occ_22) + (exp_outcome_occ_31 * counterfact_share_occ_31) + (exp_outcome_occ_64 * counterfact_share_occ_64) + (exp_outcome_occ_73 * counterfact_share_occ_73) + (exp_outcome_occ_62 * counterfact_share_occ_62) + (exp_outcome_occ_54 * counterfact_share_occ_54) + (exp_outcome_occ_74 * counterfact_share_occ_74) + (exp_outcome_occ_11 * counterfact_share_occ_11) + (exp_outcome_occ_32 * counterfact_share_occ_32) + (exp_outcome_occ_43 * counterfact_share_occ_43) + (exp_outcome_occ_52 * counterfact_share_occ_52) + (exp_outcome_occ_53 * counterfact_share_occ_53) + (exp_outcome_occ_14 * counterfact_share_occ_14) + (exp_outcome_occ_12 * counterfact_share_occ_12) + (exp_outcome_occ_41 * counterfact_share_occ_41)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  
  # average cross-fits
  levels_post_female <- (levels_post_female_split_1 + levels_post_female_split_2 + levels_post_female_split_3 + levels_post_female_split_4 + levels_post_female_split_5) / 5
  levels_post_male <- (levels_post_male_split_1 + levels_post_male_split_2 + levels_post_male_split_3 + levels_post_male_split_4 + levels_post_male_split_5) / 5
  
  # calculate estimands
  gap_pre <- levels_pre_female - levels_pre_male
  
  gap_post <- levels_post_female - levels_post_male
  
  change <- gap_post - gap_pre
  
  change_pp <- (change / gap_pre) * 100
  
  estimates_men_pre <- data.frame(estimand = "Men (before intervention)", "est" = round(levels_pre_male, 2))
  estimates_women_pre <- data.frame(estimand = "Women (before intervention)", "est" = round(levels_pre_female, 2))
  estimates_men_post <- data.frame(estimand = "Men (after intervention)", "est" = round(levels_post_male, 2))
  estimates_women_post <- data.frame(estimand = "Women (after intervention)", "est" = round(levels_post_female, 2))
  estimates_gap_pre <- data.frame(estimand = "Disparity (before intervention)", "est" = round(gap_pre, 2))
  estimates_gap_post <- data.frame(estimand = "Disparity (after intervention)", "est" = round(gap_post, 2))
  estimates_reduction <- data.frame(estimand = "Change in disparity", "est" = round(change, 2))
  estimates_reduction_pp <- data.frame(estimand = "Change in disparity (pp.)", "est" = round(change_pp, 2))
  
  estimates <- rbind(estimates_men_pre, estimates_women_pre, estimates_men_post, estimates_women_post, estimates_gap_pre, estimates_gap_post, estimates_reduction, estimates_reduction_pp)
  
  rm(list = setdiff(ls(), c("estimates", "data_final", "decompose_cond_dr_xfit", "formula_treatment", "preds_outcome", "sl_models")))
  
  gc()
  
  return(estimates)
  
}


decompose_dr_xfit_intB_detailed <- function(data, formula_treatment, preds_outcome, sl_models) {
  
  # select complete cases
  data <- data %>% na.omit()
  
  # determine counter-factual composition via intervention rule
  composition_cf <- multinom("eseg_class_2 ~ 1", data = data %>% filter(female == 1), family = "binomial", weights = weight)
  
  composition_cf <- as.data.frame(predict(composition_cf, type = "probs")) %>% slice(1)
  
  colnames(composition_cf) <- paste0("counterfact_share_occ_", colnames(composition_cf))
  
  data <- data %>% cross_join(composition_cf)
  
  rm(list = setdiff(ls(), c("data", "formula_treatment", "preds_outcome", "sl_models")))
  
  
  
  # create cross-fitting splits
  split <- split(data, f = rep_len(1:5, nrow(data)))
  
  data_split_1 = rbind(split$`2`, split$`3`, split$`4`, split$`5`)
  data_split_1_heldout = split$`1`
  
  data_split_2 = rbind(split$`1`, split$`3`, split$`4`, split$`5`)
  data_split_2_heldout = split$`2`
  
  data_split_3 = rbind(split$`1`, split$`2`, split$`4`, split$`5`)
  data_split_3_heldout = split$`3`
  
  data_split_4 = rbind(split$`1`, split$`2`, split$`3`, split$`5`)
  data_split_4_heldout = split$`4`
  
  data_split_5 = rbind(split$`1`, split$`2`, split$`3`, split$`4`)
  data_split_5_heldout = split$`5`
  
  rm(split)
  
  
  
  # treatment model
  data_split_1_male <- data_split_1 %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_2_male <- data_split_2 %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_3_male <- data_split_3 %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_4_male <- data_split_4 %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_5_male <- data_split_5 %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  
  data_split_1_female <- data_split_1 %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_2_female <- data_split_2 %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_3_female <- data_split_3 %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_4_female <- data_split_4 %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_5_female <- data_split_5 %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  
  data_split_1_heldout_male <- data_split_1_heldout %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_2_heldout_male <- data_split_2_heldout %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_3_heldout_male <- data_split_3_heldout %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_4_heldout_male <- data_split_4_heldout %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_5_heldout_male <- data_split_5_heldout %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  
  data_split_1_heldout_female <- data_split_1_heldout %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_2_heldout_female <- data_split_2_heldout %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_3_heldout_female <- data_split_3_heldout %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_4_heldout_female <- data_split_4_heldout %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_5_heldout_female <- data_split_5_heldout %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  
  # train
  # men
  model_male_split_1 <- multinom(formula_treatment, data = data_split_1_male, family = "binomial", weights = weight)
  model_male_split_2 <- multinom(formula_treatment, data = data_split_2_male, family = "binomial", weights = weight)
  model_male_split_3 <- multinom(formula_treatment, data = data_split_3_male, family = "binomial", weights = weight)
  model_male_split_4 <- multinom(formula_treatment, data = data_split_4_male, family = "binomial", weights = weight)
  model_male_split_5 <- multinom(formula_treatment, data = data_split_5_male, family = "binomial", weights = weight)
  
  # women
  model_female_split_1 <- multinom(formula_treatment, data = data_split_1_female, family = "binomial", weights = weight)
  model_female_split_2 <- multinom(formula_treatment, data = data_split_2_female, family = "binomial", weights = weight)
  model_female_split_3 <- multinom(formula_treatment, data = data_split_3_female, family = "binomial", weights = weight)
  model_female_split_4 <- multinom(formula_treatment, data = data_split_4_female, family = "binomial", weights = weight)
  model_female_split_5 <- multinom(formula_treatment, data = data_split_5_female, family = "binomial", weights = weight)
  
  # predict using model trained on other split
  # men
  data_split_1_heldout_male <- cbind(data_split_1_heldout_male, as.data.frame(predict(model_male_split_1, newdata = data_split_1_heldout_male, type = "probs")))
  colnames(data_split_1_heldout_male)[58:86] = paste0("prop_occ_", colnames(data_split_1_heldout_male)[58:86])
  
  data_split_2_heldout_male <- cbind(data_split_2_heldout_male, as.data.frame(predict(model_male_split_2, newdata = data_split_2_heldout_male, type = "probs")))
  colnames(data_split_2_heldout_male)[58:86] = paste0("prop_occ_", colnames(data_split_2_heldout_male)[58:86])
  
  data_split_3_heldout_male <- cbind(data_split_3_heldout_male, as.data.frame(predict(model_male_split_3, newdata = data_split_3_heldout_male, type = "probs")))
  colnames(data_split_3_heldout_male)[58:86] = paste0("prop_occ_", colnames(data_split_3_heldout_male)[58:86])
  
  data_split_4_heldout_male <- cbind(data_split_4_heldout_male, as.data.frame(predict(model_male_split_4, newdata = data_split_4_heldout_male, type = "probs")))
  colnames(data_split_4_heldout_male)[58:86] = paste0("prop_occ_", colnames(data_split_4_heldout_male)[58:86])
  
  data_split_5_heldout_male <- cbind(data_split_5_heldout_male, as.data.frame(predict(model_male_split_5, newdata = data_split_5_heldout_male, type = "probs")))
  colnames(data_split_5_heldout_male)[58:86] = paste0("prop_occ_", colnames(data_split_5_heldout_male)[58:86])
  
  # women
  data_split_1_heldout_female <- cbind(data_split_1_heldout_female, as.data.frame(predict(model_female_split_1, newdata = data_split_1_heldout_female, type = "probs")))
  colnames(data_split_1_heldout_female)[58:86] = paste0("prop_occ_", colnames(data_split_1_heldout_female)[58:86])
  
  data_split_2_heldout_female <- cbind(data_split_2_heldout_female, as.data.frame(predict(model_female_split_2, newdata = data_split_2_heldout_female, type = "probs")))
  colnames(data_split_2_heldout_female)[58:86] = paste0("prop_occ_", colnames(data_split_2_heldout_female)[58:86])
  
  data_split_3_heldout_female <- cbind(data_split_3_heldout_female, as.data.frame(predict(model_female_split_3, newdata = data_split_3_heldout_female, type = "probs")))
  colnames(data_split_3_heldout_female)[58:86] = paste0("prop_occ_", colnames(data_split_3_heldout_female)[58:86])
  
  data_split_4_heldout_female <- cbind(data_split_4_heldout_female, as.data.frame(predict(model_female_split_4, newdata = data_split_4_heldout_female, type = "probs")))
  colnames(data_split_4_heldout_female)[58:86] = paste0("prop_occ_", colnames(data_split_4_heldout_female)[58:86])
  
  data_split_5_heldout_female <- cbind(data_split_5_heldout_female, as.data.frame(predict(model_female_split_5, newdata = data_split_5_heldout_female, type = "probs")))
  colnames(data_split_5_heldout_female)[58:86] = paste0("prop_occ_", colnames(data_split_5_heldout_female)[58:86])
  
  # combine
  data_split_1_heldout <- rbind(data_split_1_heldout_male, data_split_1_heldout_female)
  data_split_2_heldout <- rbind(data_split_2_heldout_male, data_split_2_heldout_female)
  data_split_3_heldout <- rbind(data_split_3_heldout_male, data_split_3_heldout_female)
  data_split_4_heldout <- rbind(data_split_4_heldout_male, data_split_4_heldout_female)
  data_split_5_heldout <- rbind(data_split_5_heldout_male, data_split_5_heldout_female)
  
  data_split_1_heldout <- data_split_1_heldout %>% as.data.frame() %>% ungroup()
  data_split_2_heldout <- data_split_2_heldout %>% as.data.frame() %>% ungroup()
  data_split_3_heldout <- data_split_3_heldout %>% as.data.frame() %>% ungroup()
  data_split_4_heldout <- data_split_4_heldout %>% as.data.frame() %>% ungroup()
  data_split_5_heldout <- data_split_5_heldout %>% as.data.frame() %>% ungroup()
  
  
  # train
  # men
  model_male_split_1 <- SuperLearner(Y = data_split_1_male$outcome, X = data_split_1_male %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_1_male$weight, cvControl = list(V = 5))
  model_male_split_2 <- SuperLearner(Y = data_split_2_male$outcome, X = data_split_2_male %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_2_male$weight, cvControl = list(V = 5))
  model_male_split_3 <- SuperLearner(Y = data_split_3_male$outcome, X = data_split_3_male %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_3_male$weight, cvControl = list(V = 5))
  model_male_split_4 <- SuperLearner(Y = data_split_4_male$outcome, X = data_split_4_male %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_4_male$weight, cvControl = list(V = 5))
  model_male_split_5 <- SuperLearner(Y = data_split_5_male$outcome, X = data_split_5_male %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_5_male$weight, cvControl = list(V = 5))
  
  # women
  model_female_split_1 <- SuperLearner(Y = data_split_1_female$outcome, X = data_split_1_female %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_1_female$weight, cvControl = list(V = 5))
  model_female_split_2 <- SuperLearner(Y = data_split_2_female$outcome, X = data_split_2_female %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_2_female$weight, cvControl = list(V = 5))
  model_female_split_3 <- SuperLearner(Y = data_split_3_female$outcome, X = data_split_3_female %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_3_female$weight, cvControl = list(V = 5))
  model_female_split_4 <- SuperLearner(Y = data_split_4_female$outcome, X = data_split_4_female %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_4_female$weight, cvControl = list(V = 5))
  model_female_split_5 <- SuperLearner(Y = data_split_5_female$outcome, X = data_split_5_female %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_5_female$weight, cvControl = list(V = 5))
  
  # predictions
  for (i in levels(data$eseg_class_2)) {
    
    data_split_1_heldout_male$eseg_class_2 <- factor(i, levels = levels(data$eseg_class_2))
    data_split_2_heldout_male$eseg_class_2 <- factor(i, levels = levels(data$eseg_class_2))
    data_split_3_heldout_male$eseg_class_2 <- factor(i, levels = levels(data$eseg_class_2))
    data_split_4_heldout_male$eseg_class_2 <- factor(i, levels = levels(data$eseg_class_2))
    data_split_5_heldout_male$eseg_class_2 <- factor(i, levels = levels(data$eseg_class_2))
    
    data_split_1_heldout_female$eseg_class_2 <- factor(i, levels = levels(data$eseg_class_2))
    data_split_2_heldout_female$eseg_class_2 <- factor(i, levels = levels(data$eseg_class_2))
    data_split_3_heldout_female$eseg_class_2 <- factor(i, levels = levels(data$eseg_class_2))
    data_split_4_heldout_female$eseg_class_2 <- factor(i, levels = levels(data$eseg_class_2))
    data_split_5_heldout_female$eseg_class_2 <- factor(i, levels = levels(data$eseg_class_2))
    
    data_split_1_heldout_male$prediction <- as.data.frame(predict(model_male_split_1, newdata = data_split_1_heldout_male %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_2_heldout_male$prediction <- as.data.frame(predict(model_male_split_2, newdata = data_split_2_heldout_male %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_3_heldout_male$prediction <- as.data.frame(predict(model_male_split_3, newdata = data_split_3_heldout_male %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_4_heldout_male$prediction <- as.data.frame(predict(model_male_split_4, newdata = data_split_4_heldout_male %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_5_heldout_male$prediction <- as.data.frame(predict(model_male_split_5, newdata = data_split_5_heldout_male %>% select(all_of(preds_outcome)), type = "response"))$pred
    
    data_split_1_heldout_female$prediction <- as.data.frame(predict(model_female_split_1, newdata = data_split_1_heldout_female %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_2_heldout_female$prediction <- as.data.frame(predict(model_female_split_2, newdata = data_split_2_heldout_female %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_3_heldout_female$prediction <- as.data.frame(predict(model_female_split_3, newdata = data_split_3_heldout_female %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_4_heldout_female$prediction <- as.data.frame(predict(model_female_split_4, newdata = data_split_4_heldout_female %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_5_heldout_female$prediction <- as.data.frame(predict(model_female_split_5, newdata = data_split_5_heldout_female %>% select(all_of(preds_outcome)), type = "response"))$pred
    
    data_split_1_heldout_male <- data_split_1_heldout_male %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_2_heldout_male <- data_split_2_heldout_male %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_3_heldout_male <- data_split_3_heldout_male %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_4_heldout_male <- data_split_4_heldout_male %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_5_heldout_male <- data_split_5_heldout_male %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    
    data_split_1_heldout_female <- data_split_1_heldout_female %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_2_heldout_female <- data_split_2_heldout_female %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_3_heldout_female <- data_split_3_heldout_female %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_4_heldout_female <- data_split_4_heldout_female %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_5_heldout_female <- data_split_5_heldout_female %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    
    print(i)
    
  }
  
  rm(model_male_split_1, model_male_split_2, model_male_split_3, model_male_split_4, model_male_split_5, model_female_split_1, model_female_split_2, model_female_split_3, model_female_split_4, model_female_split_5)
  
  rm(i)
  
  # combine
  preds_split_1 <- rbind(data_split_1_heldout_male, data_split_1_heldout_female)
  preds_split_2 <- rbind(data_split_2_heldout_male, data_split_2_heldout_female)
  preds_split_3 <- rbind(data_split_3_heldout_male, data_split_3_heldout_female)
  preds_split_4 <- rbind(data_split_4_heldout_male, data_split_4_heldout_female)
  preds_split_5 <- rbind(data_split_5_heldout_male, data_split_5_heldout_female)
  
  preds_split_1 <- preds_split_1 %>% select(orgpid, age, contains("exp_outcome_occ_")) %>% group_by(orgpid, age) %>% slice(1)
  preds_split_2 <- preds_split_2 %>% select(orgpid, age, contains("exp_outcome_occ_")) %>% group_by(orgpid, age) %>% slice(1)
  preds_split_3 <- preds_split_3 %>% select(orgpid, age, contains("exp_outcome_occ_")) %>% group_by(orgpid, age) %>% slice(1)
  preds_split_4 <- preds_split_4 %>% select(orgpid, age, contains("exp_outcome_occ_")) %>% group_by(orgpid, age) %>% slice(1)
  preds_split_5 <- preds_split_5 %>% select(orgpid, age, contains("exp_outcome_occ_")) %>% group_by(orgpid, age) %>% slice(1)
  
  data_split_1_heldout <- data_split_1_heldout %>% left_join(preds_split_1, by = c("orgpid", "age")) %>% ungroup()
  data_split_2_heldout <- data_split_2_heldout %>% left_join(preds_split_2, by = c("orgpid", "age")) %>% ungroup()
  data_split_3_heldout <- data_split_3_heldout %>% left_join(preds_split_3, by = c("orgpid", "age")) %>% ungroup()
  data_split_4_heldout <- data_split_4_heldout %>% left_join(preds_split_4, by = c("orgpid", "age")) %>% ungroup()
  data_split_5_heldout <- data_split_5_heldout %>% left_join(preds_split_5, by = c("orgpid", "age")) %>% ungroup()
  
  
  
  # counterfactual probability of being assigned to the factual occupation
  cf_prob_fc_occ <- data %>% ungroup() %>%
    select(orgpid, age, eseg_class_2, contains("counterfact_share_occ")) %>%
    pivot_longer(cols = c(4:32), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 23, 24)) %>% 
    filter(eseg_class_2 == occupation) %>%
    rename("cf_prob_fc_occ" = "value") %>%
    select(orgpid, age, cf_prob_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  # inverse of the factual probability of the factual occupation
  inv_fc_prob_fc_occ_split_1 <- data_split_1_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class_2, contains("prop_occ")) %>%
    pivot_longer(cols = c(4:32), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 10, 11)) %>% 
    filter(eseg_class_2 == occupation) %>%
    mutate(value = 1 / value) %>%
    rename("inv_fc_prob_fc_occ" = "value") %>%
    select(orgpid, age, inv_fc_prob_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup() %>%
    mutate(inv_fc_prob_fc_occ = case_when(inv_fc_prob_fc_occ > quantile(inv_fc_prob_fc_occ, 0.999) ~ quantile(inv_fc_prob_fc_occ, 0.999),
                                          inv_fc_prob_fc_occ < quantile(inv_fc_prob_fc_occ, 0.001) ~ quantile(inv_fc_prob_fc_occ, 0.001),
                                          T ~ inv_fc_prob_fc_occ))
  
  inv_fc_prob_fc_occ_split_2 <- data_split_2_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class_2, contains("prop_occ")) %>%
    pivot_longer(cols = c(4:32), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 10, 11)) %>% 
    filter(eseg_class_2 == occupation) %>%
    mutate(value = 1 / value) %>%
    rename("inv_fc_prob_fc_occ" = "value") %>%
    select(orgpid, age, inv_fc_prob_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup() %>%
    mutate(inv_fc_prob_fc_occ = case_when(inv_fc_prob_fc_occ > quantile(inv_fc_prob_fc_occ, 0.999) ~ quantile(inv_fc_prob_fc_occ, 0.999),
                                          inv_fc_prob_fc_occ < quantile(inv_fc_prob_fc_occ, 0.001) ~ quantile(inv_fc_prob_fc_occ, 0.001),
                                          T ~ inv_fc_prob_fc_occ))
  
  inv_fc_prob_fc_occ_split_3 <- data_split_3_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class_2, contains("prop_occ")) %>%
    pivot_longer(cols = c(4:32), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 10, 11)) %>% 
    filter(eseg_class_2 == occupation) %>%
    mutate(value = 1 / value) %>%
    rename("inv_fc_prob_fc_occ" = "value") %>%
    select(orgpid, age, inv_fc_prob_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup() %>%
    mutate(inv_fc_prob_fc_occ = case_when(inv_fc_prob_fc_occ > quantile(inv_fc_prob_fc_occ, 0.999) ~ quantile(inv_fc_prob_fc_occ, 0.999),
                                          inv_fc_prob_fc_occ < quantile(inv_fc_prob_fc_occ, 0.001) ~ quantile(inv_fc_prob_fc_occ, 0.001),
                                          T ~ inv_fc_prob_fc_occ))
  
  inv_fc_prob_fc_occ_split_4 <- data_split_4_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class_2, contains("prop_occ")) %>%
    pivot_longer(cols = c(4:32), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 10, 11)) %>% 
    filter(eseg_class_2 == occupation) %>%
    mutate(value = 1 / value) %>%
    rename("inv_fc_prob_fc_occ" = "value") %>%
    select(orgpid, age, inv_fc_prob_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup() %>%
    mutate(inv_fc_prob_fc_occ = case_when(inv_fc_prob_fc_occ > quantile(inv_fc_prob_fc_occ, 0.999) ~ quantile(inv_fc_prob_fc_occ, 0.999),
                                          inv_fc_prob_fc_occ < quantile(inv_fc_prob_fc_occ, 0.001) ~ quantile(inv_fc_prob_fc_occ, 0.001),
                                          T ~ inv_fc_prob_fc_occ))
  
  inv_fc_prob_fc_occ_split_5 <- data_split_5_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class_2, contains("prop_occ")) %>%
    pivot_longer(cols = c(4:32), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 10, 11)) %>% 
    filter(eseg_class_2 == occupation) %>%
    mutate(value = 1 / value) %>%
    rename("inv_fc_prob_fc_occ" = "value") %>%
    select(orgpid, age, inv_fc_prob_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup() %>%
    mutate(inv_fc_prob_fc_occ = case_when(inv_fc_prob_fc_occ > quantile(inv_fc_prob_fc_occ, 0.999) ~ quantile(inv_fc_prob_fc_occ, 0.999),
                                          inv_fc_prob_fc_occ < quantile(inv_fc_prob_fc_occ, 0.001) ~ quantile(inv_fc_prob_fc_occ, 0.001),
                                          T ~ inv_fc_prob_fc_occ))
  
  # outcome predictions
  exp_outcome_fc_occ_split_1 <- data_split_1_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class_2, contains("exp_outcome_occ")) %>%
    pivot_longer(cols = c(4:32), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 17, 18)) %>% 
    filter(eseg_class_2 == occupation) %>%
    rename("exp_outcome_fc_occ" = "value") %>%
    select(orgpid, age, exp_outcome_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  exp_outcome_fc_occ_split_2 <- data_split_2_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class_2, contains("exp_outcome_occ")) %>%
    pivot_longer(cols = c(4:32), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 17, 18)) %>% 
    filter(eseg_class_2 == occupation) %>%
    rename("exp_outcome_fc_occ" = "value") %>%
    select(orgpid, age, exp_outcome_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  exp_outcome_fc_occ_split_3 <- data_split_3_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class_2, contains("exp_outcome_occ")) %>%
    pivot_longer(cols = c(4:32), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 17, 18)) %>% 
    filter(eseg_class_2 == occupation) %>%
    rename("exp_outcome_fc_occ" = "value") %>%
    select(orgpid, age, exp_outcome_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  exp_outcome_fc_occ_split_4 <- data_split_4_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class_2, contains("exp_outcome_occ")) %>%
    pivot_longer(cols = c(4:32), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 17, 18)) %>% 
    filter(eseg_class_2 == occupation) %>%
    rename("exp_outcome_fc_occ" = "value") %>%
    select(orgpid, age, exp_outcome_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  exp_outcome_fc_occ_split_5 <- data_split_5_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class_2, contains("exp_outcome_occ")) %>%
    pivot_longer(cols = c(4:32), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 17, 18)) %>% 
    filter(eseg_class_2 == occupation) %>%
    rename("exp_outcome_fc_occ" = "value") %>%
    select(orgpid, age, exp_outcome_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  # create combined weight
  weight_split_1 <- data_split_1_heldout %>% ungroup() %>%
    select(orgpid, age, weight) %>% 
    left_join(cf_prob_fc_occ, by = c("orgpid", "age")) %>%
    left_join(inv_fc_prob_fc_occ_split_1, by = c("orgpid", "age")) %>%
    mutate(combined_weight = weight * cf_prob_fc_occ * inv_fc_prob_fc_occ) %>%
    select(orgpid, age, combined_weight) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  weight_split_2 <- data_split_2_heldout %>% ungroup() %>%
    select(orgpid, age, weight) %>% 
    left_join(cf_prob_fc_occ, by = c("orgpid", "age")) %>%
    left_join(inv_fc_prob_fc_occ_split_2, by = c("orgpid", "age")) %>%
    mutate(combined_weight = weight * cf_prob_fc_occ * inv_fc_prob_fc_occ) %>%
    select(orgpid, age, combined_weight) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  weight_split_3 <- data_split_3_heldout %>% ungroup() %>%
    select(orgpid, age, weight) %>% 
    left_join(cf_prob_fc_occ, by = c("orgpid", "age")) %>%
    left_join(inv_fc_prob_fc_occ_split_3, by = c("orgpid", "age")) %>%
    mutate(combined_weight = weight * cf_prob_fc_occ * inv_fc_prob_fc_occ) %>%
    select(orgpid, age, combined_weight) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  weight_split_4 <- data_split_4_heldout %>% ungroup() %>%
    select(orgpid, age, weight) %>% 
    left_join(cf_prob_fc_occ, by = c("orgpid", "age")) %>%
    left_join(inv_fc_prob_fc_occ_split_4, by = c("orgpid", "age")) %>%
    mutate(combined_weight = weight * cf_prob_fc_occ * inv_fc_prob_fc_occ) %>%
    select(orgpid, age, combined_weight) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  weight_split_5 <- data_split_5_heldout %>% ungroup() %>%
    select(orgpid, age, weight) %>% 
    left_join(cf_prob_fc_occ, by = c("orgpid", "age")) %>%
    left_join(inv_fc_prob_fc_occ_split_5, by = c("orgpid", "age")) %>%
    mutate(combined_weight = weight * cf_prob_fc_occ * inv_fc_prob_fc_occ) %>%
    select(orgpid, age, combined_weight) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  # join
  data_split_1_heldout <- data_split_1_heldout %>% left_join(exp_outcome_fc_occ_split_1, by = c("orgpid", "age")) %>% left_join(weight_split_1, by = c("orgpid", "age")) %>% ungroup()
  data_split_2_heldout <- data_split_2_heldout %>% left_join(exp_outcome_fc_occ_split_2, by = c("orgpid", "age")) %>% left_join(weight_split_2, by = c("orgpid", "age")) %>% ungroup()
  data_split_3_heldout <- data_split_3_heldout %>% left_join(exp_outcome_fc_occ_split_3, by = c("orgpid", "age")) %>% left_join(weight_split_3, by = c("orgpid", "age")) %>% ungroup()
  data_split_4_heldout <- data_split_4_heldout %>% left_join(exp_outcome_fc_occ_split_4, by = c("orgpid", "age")) %>% left_join(weight_split_4, by = c("orgpid", "age")) %>% ungroup()
  data_split_5_heldout <- data_split_5_heldout %>% left_join(exp_outcome_fc_occ_split_5, by = c("orgpid", "age")) %>% left_join(weight_split_5, by = c("orgpid", "age")) %>% ungroup()
  
  # calculate bias correction
  bias_correction_m_split_1 <- data_split_1_heldout %>% filter(female == 0) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_m_split_2 <- data_split_2_heldout %>% filter(female == 0) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_m_split_3 <- data_split_3_heldout %>% filter(female == 0) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_m_split_4 <- data_split_4_heldout %>% filter(female == 0) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_m_split_5 <- data_split_5_heldout %>% filter(female == 0) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_f_split_1 <- data_split_1_heldout %>% filter(female == 1) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_f_split_2 <- data_split_2_heldout %>% filter(female == 1) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_f_split_3 <- data_split_3_heldout %>% filter(female == 1) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_f_split_4 <- data_split_4_heldout %>% filter(female == 1) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_f_split_5 <- data_split_5_heldout %>% filter(female == 1) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_split_1 <- rbind(bias_correction_m_split_1, bias_correction_f_split_1)
  bias_correction_split_2 <- rbind(bias_correction_m_split_2, bias_correction_f_split_2)
  bias_correction_split_3 <- rbind(bias_correction_m_split_3, bias_correction_f_split_3)
  bias_correction_split_4 <- rbind(bias_correction_m_split_4, bias_correction_f_split_4)
  bias_correction_split_5 <- rbind(bias_correction_m_split_5, bias_correction_f_split_5)
  
  # correct bias in outcome predictions
  data_split_1_heldout <- data_split_1_heldout %>% 
    left_join(bias_correction_split_1, by = "female") %>% ungroup() %>%
    mutate_at(vars(contains("exp_outcome_occ_")), ~(. - bias_correction)) %>% 
    mutate_at(vars(contains("exp_outcome_occ_")), ~(case_when(. > 1 ~ 1, . < 0 ~ 0, T ~ .)))
  
  data_split_2_heldout <- data_split_2_heldout %>% 
    left_join(bias_correction_split_2, by = "female") %>% ungroup() %>%
    mutate_at(vars(contains("exp_outcome_occ_")), ~(. - bias_correction)) %>% 
    mutate_at(vars(contains("exp_outcome_occ_")), ~(case_when(. > 1 ~ 1, . < 0 ~ 0, T ~ .)))
  
  data_split_3_heldout <- data_split_3_heldout %>% 
    left_join(bias_correction_split_3, by = "female") %>% ungroup() %>%
    mutate_at(vars(contains("exp_outcome_occ_")), ~(. - bias_correction)) %>% 
    mutate_at(vars(contains("exp_outcome_occ_")), ~(case_when(. > 1 ~ 1, . < 0 ~ 0, T ~ .)))
  
  data_split_4_heldout <- data_split_4_heldout %>% 
    left_join(bias_correction_split_4, by = "female") %>% ungroup() %>%
    mutate_at(vars(contains("exp_outcome_occ_")), ~(. - bias_correction)) %>% 
    mutate_at(vars(contains("exp_outcome_occ_")), ~(case_when(. > 1 ~ 1, . < 0 ~ 0, T ~ .)))
  
  data_split_5_heldout <- data_split_5_heldout %>% 
    left_join(bias_correction_split_5, by = "female") %>% ungroup() %>%
    mutate_at(vars(contains("exp_outcome_occ_")), ~(. - bias_correction)) %>% 
    mutate_at(vars(contains("exp_outcome_occ_")), ~(case_when(. > 1 ~ 1, . < 0 ~ 0, T ~ .)))
  
  # pre-intervention
  levels_pre_female <- data %>% filter(female == 1) %>% ungroup() %>%summarize(exp = weighted.mean(outcome, w = weight) * 100) %>% as.numeric()
  levels_pre_male <- data %>% filter(female == 0) %>% ungroup() %>%summarize(exp = weighted.mean(outcome, w = weight) * 100) %>% as.numeric()
  
  # post-intervention
  # split 1
  levels_post_female_split_1 <- data_split_1_heldout %>% filter(female == 1) %>% mutate(exp = (exp_outcome_occ_24 * counterfact_share_occ_24) + (exp_outcome_occ_33 * counterfact_share_occ_33) + (exp_outcome_occ_51 * counterfact_share_occ_51) + (exp_outcome_occ_25 * counterfact_share_occ_25) + (exp_outcome_occ_71 * counterfact_share_occ_71) + (exp_outcome_occ_21 * counterfact_share_occ_21) + (exp_outcome_occ_23 * counterfact_share_occ_23) + (exp_outcome_occ_42 * counterfact_share_occ_42) + (exp_outcome_occ_65 * counterfact_share_occ_65) + (exp_outcome_occ_61 * counterfact_share_occ_61) + (exp_outcome_occ_72 * counterfact_share_occ_72) + (exp_outcome_occ_13 * counterfact_share_occ_13) + (exp_outcome_occ_63 * counterfact_share_occ_63) + (exp_outcome_occ_34 * counterfact_share_occ_34) + (exp_outcome_occ_22 * counterfact_share_occ_22) + (exp_outcome_occ_31 * counterfact_share_occ_31) + (exp_outcome_occ_64 * counterfact_share_occ_64) + (exp_outcome_occ_73 * counterfact_share_occ_73) + (exp_outcome_occ_62 * counterfact_share_occ_62) + (exp_outcome_occ_54 * counterfact_share_occ_54) + (exp_outcome_occ_74 * counterfact_share_occ_74) + (exp_outcome_occ_11 * counterfact_share_occ_11) + (exp_outcome_occ_32 * counterfact_share_occ_32) + (exp_outcome_occ_43 * counterfact_share_occ_43) + (exp_outcome_occ_52 * counterfact_share_occ_52) + (exp_outcome_occ_53 * counterfact_share_occ_53) + (exp_outcome_occ_14 * counterfact_share_occ_14) + (exp_outcome_occ_12 * counterfact_share_occ_12) + (exp_outcome_occ_41 * counterfact_share_occ_41)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  levels_post_male_split_1 <- data_split_1_heldout %>% filter(female == 0) %>% mutate(exp = (exp_outcome_occ_24 * counterfact_share_occ_24) + (exp_outcome_occ_33 * counterfact_share_occ_33) + (exp_outcome_occ_51 * counterfact_share_occ_51) + (exp_outcome_occ_25 * counterfact_share_occ_25) + (exp_outcome_occ_71 * counterfact_share_occ_71) + (exp_outcome_occ_21 * counterfact_share_occ_21) + (exp_outcome_occ_23 * counterfact_share_occ_23) + (exp_outcome_occ_42 * counterfact_share_occ_42) + (exp_outcome_occ_65 * counterfact_share_occ_65) + (exp_outcome_occ_61 * counterfact_share_occ_61) + (exp_outcome_occ_72 * counterfact_share_occ_72) + (exp_outcome_occ_13 * counterfact_share_occ_13) + (exp_outcome_occ_63 * counterfact_share_occ_63) + (exp_outcome_occ_34 * counterfact_share_occ_34) + (exp_outcome_occ_22 * counterfact_share_occ_22) + (exp_outcome_occ_31 * counterfact_share_occ_31) + (exp_outcome_occ_64 * counterfact_share_occ_64) + (exp_outcome_occ_73 * counterfact_share_occ_73) + (exp_outcome_occ_62 * counterfact_share_occ_62) + (exp_outcome_occ_54 * counterfact_share_occ_54) + (exp_outcome_occ_74 * counterfact_share_occ_74) + (exp_outcome_occ_11 * counterfact_share_occ_11) + (exp_outcome_occ_32 * counterfact_share_occ_32) + (exp_outcome_occ_43 * counterfact_share_occ_43) + (exp_outcome_occ_52 * counterfact_share_occ_52) + (exp_outcome_occ_53 * counterfact_share_occ_53) + (exp_outcome_occ_14 * counterfact_share_occ_14) + (exp_outcome_occ_12 * counterfact_share_occ_12) + (exp_outcome_occ_41 * counterfact_share_occ_41)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  
  # split 2
  levels_post_female_split_2 <- data_split_2_heldout %>% filter(female == 1) %>% mutate(exp = (exp_outcome_occ_24 * counterfact_share_occ_24) + (exp_outcome_occ_33 * counterfact_share_occ_33) + (exp_outcome_occ_51 * counterfact_share_occ_51) + (exp_outcome_occ_25 * counterfact_share_occ_25) + (exp_outcome_occ_71 * counterfact_share_occ_71) + (exp_outcome_occ_21 * counterfact_share_occ_21) + (exp_outcome_occ_23 * counterfact_share_occ_23) + (exp_outcome_occ_42 * counterfact_share_occ_42) + (exp_outcome_occ_65 * counterfact_share_occ_65) + (exp_outcome_occ_61 * counterfact_share_occ_61) + (exp_outcome_occ_72 * counterfact_share_occ_72) + (exp_outcome_occ_13 * counterfact_share_occ_13) + (exp_outcome_occ_63 * counterfact_share_occ_63) + (exp_outcome_occ_34 * counterfact_share_occ_34) + (exp_outcome_occ_22 * counterfact_share_occ_22) + (exp_outcome_occ_31 * counterfact_share_occ_31) + (exp_outcome_occ_64 * counterfact_share_occ_64) + (exp_outcome_occ_73 * counterfact_share_occ_73) + (exp_outcome_occ_62 * counterfact_share_occ_62) + (exp_outcome_occ_54 * counterfact_share_occ_54) + (exp_outcome_occ_74 * counterfact_share_occ_74) + (exp_outcome_occ_11 * counterfact_share_occ_11) + (exp_outcome_occ_32 * counterfact_share_occ_32) + (exp_outcome_occ_43 * counterfact_share_occ_43) + (exp_outcome_occ_52 * counterfact_share_occ_52) + (exp_outcome_occ_53 * counterfact_share_occ_53) + (exp_outcome_occ_14 * counterfact_share_occ_14) + (exp_outcome_occ_12 * counterfact_share_occ_12) + (exp_outcome_occ_41 * counterfact_share_occ_41)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  levels_post_male_split_2 <- data_split_2_heldout %>% filter(female == 0) %>% mutate(exp = (exp_outcome_occ_24 * counterfact_share_occ_24) + (exp_outcome_occ_33 * counterfact_share_occ_33) + (exp_outcome_occ_51 * counterfact_share_occ_51) + (exp_outcome_occ_25 * counterfact_share_occ_25) + (exp_outcome_occ_71 * counterfact_share_occ_71) + (exp_outcome_occ_21 * counterfact_share_occ_21) + (exp_outcome_occ_23 * counterfact_share_occ_23) + (exp_outcome_occ_42 * counterfact_share_occ_42) + (exp_outcome_occ_65 * counterfact_share_occ_65) + (exp_outcome_occ_61 * counterfact_share_occ_61) + (exp_outcome_occ_72 * counterfact_share_occ_72) + (exp_outcome_occ_13 * counterfact_share_occ_13) + (exp_outcome_occ_63 * counterfact_share_occ_63) + (exp_outcome_occ_34 * counterfact_share_occ_34) + (exp_outcome_occ_22 * counterfact_share_occ_22) + (exp_outcome_occ_31 * counterfact_share_occ_31) + (exp_outcome_occ_64 * counterfact_share_occ_64) + (exp_outcome_occ_73 * counterfact_share_occ_73) + (exp_outcome_occ_62 * counterfact_share_occ_62) + (exp_outcome_occ_54 * counterfact_share_occ_54) + (exp_outcome_occ_74 * counterfact_share_occ_74) + (exp_outcome_occ_11 * counterfact_share_occ_11) + (exp_outcome_occ_32 * counterfact_share_occ_32) + (exp_outcome_occ_43 * counterfact_share_occ_43) + (exp_outcome_occ_52 * counterfact_share_occ_52) + (exp_outcome_occ_53 * counterfact_share_occ_53) + (exp_outcome_occ_14 * counterfact_share_occ_14) + (exp_outcome_occ_12 * counterfact_share_occ_12) + (exp_outcome_occ_41 * counterfact_share_occ_41)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  
  # split 3
  levels_post_female_split_3 <- data_split_3_heldout %>% filter(female == 1) %>% mutate(exp = (exp_outcome_occ_24 * counterfact_share_occ_24) + (exp_outcome_occ_33 * counterfact_share_occ_33) + (exp_outcome_occ_51 * counterfact_share_occ_51) + (exp_outcome_occ_25 * counterfact_share_occ_25) + (exp_outcome_occ_71 * counterfact_share_occ_71) + (exp_outcome_occ_21 * counterfact_share_occ_21) + (exp_outcome_occ_23 * counterfact_share_occ_23) + (exp_outcome_occ_42 * counterfact_share_occ_42) + (exp_outcome_occ_65 * counterfact_share_occ_65) + (exp_outcome_occ_61 * counterfact_share_occ_61) + (exp_outcome_occ_72 * counterfact_share_occ_72) + (exp_outcome_occ_13 * counterfact_share_occ_13) + (exp_outcome_occ_63 * counterfact_share_occ_63) + (exp_outcome_occ_34 * counterfact_share_occ_34) + (exp_outcome_occ_22 * counterfact_share_occ_22) + (exp_outcome_occ_31 * counterfact_share_occ_31) + (exp_outcome_occ_64 * counterfact_share_occ_64) + (exp_outcome_occ_73 * counterfact_share_occ_73) + (exp_outcome_occ_62 * counterfact_share_occ_62) + (exp_outcome_occ_54 * counterfact_share_occ_54) + (exp_outcome_occ_74 * counterfact_share_occ_74) + (exp_outcome_occ_11 * counterfact_share_occ_11) + (exp_outcome_occ_32 * counterfact_share_occ_32) + (exp_outcome_occ_43 * counterfact_share_occ_43) + (exp_outcome_occ_52 * counterfact_share_occ_52) + (exp_outcome_occ_53 * counterfact_share_occ_53) + (exp_outcome_occ_14 * counterfact_share_occ_14) + (exp_outcome_occ_12 * counterfact_share_occ_12) + (exp_outcome_occ_41 * counterfact_share_occ_41)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  levels_post_male_split_3 <- data_split_3_heldout %>% filter(female == 0) %>% mutate(exp = (exp_outcome_occ_24 * counterfact_share_occ_24) + (exp_outcome_occ_33 * counterfact_share_occ_33) + (exp_outcome_occ_51 * counterfact_share_occ_51) + (exp_outcome_occ_25 * counterfact_share_occ_25) + (exp_outcome_occ_71 * counterfact_share_occ_71) + (exp_outcome_occ_21 * counterfact_share_occ_21) + (exp_outcome_occ_23 * counterfact_share_occ_23) + (exp_outcome_occ_42 * counterfact_share_occ_42) + (exp_outcome_occ_65 * counterfact_share_occ_65) + (exp_outcome_occ_61 * counterfact_share_occ_61) + (exp_outcome_occ_72 * counterfact_share_occ_72) + (exp_outcome_occ_13 * counterfact_share_occ_13) + (exp_outcome_occ_63 * counterfact_share_occ_63) + (exp_outcome_occ_34 * counterfact_share_occ_34) + (exp_outcome_occ_22 * counterfact_share_occ_22) + (exp_outcome_occ_31 * counterfact_share_occ_31) + (exp_outcome_occ_64 * counterfact_share_occ_64) + (exp_outcome_occ_73 * counterfact_share_occ_73) + (exp_outcome_occ_62 * counterfact_share_occ_62) + (exp_outcome_occ_54 * counterfact_share_occ_54) + (exp_outcome_occ_74 * counterfact_share_occ_74) + (exp_outcome_occ_11 * counterfact_share_occ_11) + (exp_outcome_occ_32 * counterfact_share_occ_32) + (exp_outcome_occ_43 * counterfact_share_occ_43) + (exp_outcome_occ_52 * counterfact_share_occ_52) + (exp_outcome_occ_53 * counterfact_share_occ_53) + (exp_outcome_occ_14 * counterfact_share_occ_14) + (exp_outcome_occ_12 * counterfact_share_occ_12) + (exp_outcome_occ_41 * counterfact_share_occ_41)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  
  # split 4
  levels_post_female_split_4 <- data_split_4_heldout %>% filter(female == 1) %>% mutate(exp = (exp_outcome_occ_24 * counterfact_share_occ_24) + (exp_outcome_occ_33 * counterfact_share_occ_33) + (exp_outcome_occ_51 * counterfact_share_occ_51) + (exp_outcome_occ_25 * counterfact_share_occ_25) + (exp_outcome_occ_71 * counterfact_share_occ_71) + (exp_outcome_occ_21 * counterfact_share_occ_21) + (exp_outcome_occ_23 * counterfact_share_occ_23) + (exp_outcome_occ_42 * counterfact_share_occ_42) + (exp_outcome_occ_65 * counterfact_share_occ_65) + (exp_outcome_occ_61 * counterfact_share_occ_61) + (exp_outcome_occ_72 * counterfact_share_occ_72) + (exp_outcome_occ_13 * counterfact_share_occ_13) + (exp_outcome_occ_63 * counterfact_share_occ_63) + (exp_outcome_occ_34 * counterfact_share_occ_34) + (exp_outcome_occ_22 * counterfact_share_occ_22) + (exp_outcome_occ_31 * counterfact_share_occ_31) + (exp_outcome_occ_64 * counterfact_share_occ_64) + (exp_outcome_occ_73 * counterfact_share_occ_73) + (exp_outcome_occ_62 * counterfact_share_occ_62) + (exp_outcome_occ_54 * counterfact_share_occ_54) + (exp_outcome_occ_74 * counterfact_share_occ_74) + (exp_outcome_occ_11 * counterfact_share_occ_11) + (exp_outcome_occ_32 * counterfact_share_occ_32) + (exp_outcome_occ_43 * counterfact_share_occ_43) + (exp_outcome_occ_52 * counterfact_share_occ_52) + (exp_outcome_occ_53 * counterfact_share_occ_53) + (exp_outcome_occ_14 * counterfact_share_occ_14) + (exp_outcome_occ_12 * counterfact_share_occ_12) + (exp_outcome_occ_41 * counterfact_share_occ_41)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  levels_post_male_split_4 <- data_split_4_heldout %>% filter(female == 0) %>% mutate(exp = (exp_outcome_occ_24 * counterfact_share_occ_24) + (exp_outcome_occ_33 * counterfact_share_occ_33) + (exp_outcome_occ_51 * counterfact_share_occ_51) + (exp_outcome_occ_25 * counterfact_share_occ_25) + (exp_outcome_occ_71 * counterfact_share_occ_71) + (exp_outcome_occ_21 * counterfact_share_occ_21) + (exp_outcome_occ_23 * counterfact_share_occ_23) + (exp_outcome_occ_42 * counterfact_share_occ_42) + (exp_outcome_occ_65 * counterfact_share_occ_65) + (exp_outcome_occ_61 * counterfact_share_occ_61) + (exp_outcome_occ_72 * counterfact_share_occ_72) + (exp_outcome_occ_13 * counterfact_share_occ_13) + (exp_outcome_occ_63 * counterfact_share_occ_63) + (exp_outcome_occ_34 * counterfact_share_occ_34) + (exp_outcome_occ_22 * counterfact_share_occ_22) + (exp_outcome_occ_31 * counterfact_share_occ_31) + (exp_outcome_occ_64 * counterfact_share_occ_64) + (exp_outcome_occ_73 * counterfact_share_occ_73) + (exp_outcome_occ_62 * counterfact_share_occ_62) + (exp_outcome_occ_54 * counterfact_share_occ_54) + (exp_outcome_occ_74 * counterfact_share_occ_74) + (exp_outcome_occ_11 * counterfact_share_occ_11) + (exp_outcome_occ_32 * counterfact_share_occ_32) + (exp_outcome_occ_43 * counterfact_share_occ_43) + (exp_outcome_occ_52 * counterfact_share_occ_52) + (exp_outcome_occ_53 * counterfact_share_occ_53) + (exp_outcome_occ_14 * counterfact_share_occ_14) + (exp_outcome_occ_12 * counterfact_share_occ_12) + (exp_outcome_occ_41 * counterfact_share_occ_41)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  
  # split 5
  levels_post_female_split_5 <- data_split_5_heldout %>% filter(female == 1) %>% mutate(exp = (exp_outcome_occ_24 * counterfact_share_occ_24) + (exp_outcome_occ_33 * counterfact_share_occ_33) + (exp_outcome_occ_51 * counterfact_share_occ_51) + (exp_outcome_occ_25 * counterfact_share_occ_25) + (exp_outcome_occ_71 * counterfact_share_occ_71) + (exp_outcome_occ_21 * counterfact_share_occ_21) + (exp_outcome_occ_23 * counterfact_share_occ_23) + (exp_outcome_occ_42 * counterfact_share_occ_42) + (exp_outcome_occ_65 * counterfact_share_occ_65) + (exp_outcome_occ_61 * counterfact_share_occ_61) + (exp_outcome_occ_72 * counterfact_share_occ_72) + (exp_outcome_occ_13 * counterfact_share_occ_13) + (exp_outcome_occ_63 * counterfact_share_occ_63) + (exp_outcome_occ_34 * counterfact_share_occ_34) + (exp_outcome_occ_22 * counterfact_share_occ_22) + (exp_outcome_occ_31 * counterfact_share_occ_31) + (exp_outcome_occ_64 * counterfact_share_occ_64) + (exp_outcome_occ_73 * counterfact_share_occ_73) + (exp_outcome_occ_62 * counterfact_share_occ_62) + (exp_outcome_occ_54 * counterfact_share_occ_54) + (exp_outcome_occ_74 * counterfact_share_occ_74) + (exp_outcome_occ_11 * counterfact_share_occ_11) + (exp_outcome_occ_32 * counterfact_share_occ_32) + (exp_outcome_occ_43 * counterfact_share_occ_43) + (exp_outcome_occ_52 * counterfact_share_occ_52) + (exp_outcome_occ_53 * counterfact_share_occ_53) + (exp_outcome_occ_14 * counterfact_share_occ_14) + (exp_outcome_occ_12 * counterfact_share_occ_12) + (exp_outcome_occ_41 * counterfact_share_occ_41)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  levels_post_male_split_5 <- data_split_5_heldout %>% filter(female == 0) %>% mutate(exp = (exp_outcome_occ_24 * counterfact_share_occ_24) + (exp_outcome_occ_33 * counterfact_share_occ_33) + (exp_outcome_occ_51 * counterfact_share_occ_51) + (exp_outcome_occ_25 * counterfact_share_occ_25) + (exp_outcome_occ_71 * counterfact_share_occ_71) + (exp_outcome_occ_21 * counterfact_share_occ_21) + (exp_outcome_occ_23 * counterfact_share_occ_23) + (exp_outcome_occ_42 * counterfact_share_occ_42) + (exp_outcome_occ_65 * counterfact_share_occ_65) + (exp_outcome_occ_61 * counterfact_share_occ_61) + (exp_outcome_occ_72 * counterfact_share_occ_72) + (exp_outcome_occ_13 * counterfact_share_occ_13) + (exp_outcome_occ_63 * counterfact_share_occ_63) + (exp_outcome_occ_34 * counterfact_share_occ_34) + (exp_outcome_occ_22 * counterfact_share_occ_22) + (exp_outcome_occ_31 * counterfact_share_occ_31) + (exp_outcome_occ_64 * counterfact_share_occ_64) + (exp_outcome_occ_73 * counterfact_share_occ_73) + (exp_outcome_occ_62 * counterfact_share_occ_62) + (exp_outcome_occ_54 * counterfact_share_occ_54) + (exp_outcome_occ_74 * counterfact_share_occ_74) + (exp_outcome_occ_11 * counterfact_share_occ_11) + (exp_outcome_occ_32 * counterfact_share_occ_32) + (exp_outcome_occ_43 * counterfact_share_occ_43) + (exp_outcome_occ_52 * counterfact_share_occ_52) + (exp_outcome_occ_53 * counterfact_share_occ_53) + (exp_outcome_occ_14 * counterfact_share_occ_14) + (exp_outcome_occ_12 * counterfact_share_occ_12) + (exp_outcome_occ_41 * counterfact_share_occ_41)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  
  # average cross-fits
  levels_post_female <- (levels_post_female_split_1 + levels_post_female_split_2 + levels_post_female_split_3 + levels_post_female_split_4 + levels_post_female_split_5) / 5
  levels_post_male <- (levels_post_male_split_1 + levels_post_male_split_2 + levels_post_male_split_3 + levels_post_male_split_4 + levels_post_male_split_5) / 5
  
  # calculate estimands
  gap_pre <- levels_pre_female - levels_pre_male
  
  gap_post <- levels_post_female - levels_post_male
  
  change <- gap_post - gap_pre
  
  change_pp <- (change / gap_pre) * 100
  
  estimates_men_pre <- data.frame(estimand = "Men (before intervention)", "est" = round(levels_pre_male, 2))
  estimates_women_pre <- data.frame(estimand = "Women (before intervention)", "est" = round(levels_pre_female, 2))
  estimates_men_post <- data.frame(estimand = "Men (after intervention)", "est" = round(levels_post_male, 2))
  estimates_women_post <- data.frame(estimand = "Women (after intervention)", "est" = round(levels_post_female, 2))
  estimates_gap_pre <- data.frame(estimand = "Disparity (before intervention)", "est" = round(gap_pre, 2))
  estimates_gap_post <- data.frame(estimand = "Disparity (after intervention)", "est" = round(gap_post, 2))
  estimates_reduction <- data.frame(estimand = "Change in disparity", "est" = round(change, 2))
  estimates_reduction_pp <- data.frame(estimand = "Change in disparity (pp.)", "est" = round(change_pp, 2))
  
  estimates <- rbind(estimates_men_pre, estimates_women_pre, estimates_men_post, estimates_women_post, estimates_gap_pre, estimates_gap_post, estimates_reduction, estimates_reduction_pp)
  
  rm(list = setdiff(ls(), c("estimates", "data_final", "decompose_cond_dr_xfit", "formula_treatment", "preds_outcome", "sl_models")))
  
  gc()
  
  return(estimates)
  
}


decompose_dr_xfit_intC_detailed <- function(data, formula_treatment, preds_outcome, sl_models) {
  
  # select complete cases
  data <- data %>% na.omit()
  
  # determine counter-factual composition via intervention rule
  composition_cf <- multinom("eseg_class_2 ~ 1", data = data, family = "binomial", weights = weight)
  
  composition_cf <- as.data.frame(predict(composition_cf, type = "probs")) %>% slice(1)
  
  colnames(composition_cf) <- paste0("counterfact_share_occ_", colnames(composition_cf))
  
  data <- data %>% cross_join(composition_cf)
  
  rm(list = setdiff(ls(), c("data", "formula_treatment", "preds_outcome", "sl_models")))
  
  
  
  # create cross-fitting splits
  split <- split(data, f = rep_len(1:5, nrow(data)))
  
  data_split_1 = rbind(split$`2`, split$`3`, split$`4`, split$`5`)
  data_split_1_heldout = split$`1`
  
  data_split_2 = rbind(split$`1`, split$`3`, split$`4`, split$`5`)
  data_split_2_heldout = split$`2`
  
  data_split_3 = rbind(split$`1`, split$`2`, split$`4`, split$`5`)
  data_split_3_heldout = split$`3`
  
  data_split_4 = rbind(split$`1`, split$`2`, split$`3`, split$`5`)
  data_split_4_heldout = split$`4`
  
  data_split_5 = rbind(split$`1`, split$`2`, split$`3`, split$`4`)
  data_split_5_heldout = split$`5`
  
  rm(split)
  
  
  
  # treatment model
  data_split_1_male <- data_split_1 %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_2_male <- data_split_2 %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_3_male <- data_split_3 %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_4_male <- data_split_4 %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_5_male <- data_split_5 %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  
  data_split_1_female <- data_split_1 %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_2_female <- data_split_2 %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_3_female <- data_split_3 %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_4_female <- data_split_4 %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_5_female <- data_split_5 %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  
  data_split_1_heldout_male <- data_split_1_heldout %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_2_heldout_male <- data_split_2_heldout %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_3_heldout_male <- data_split_3_heldout %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_4_heldout_male <- data_split_4_heldout %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_5_heldout_male <- data_split_5_heldout %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  
  data_split_1_heldout_female <- data_split_1_heldout %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_2_heldout_female <- data_split_2_heldout %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_3_heldout_female <- data_split_3_heldout %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_4_heldout_female <- data_split_4_heldout %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_5_heldout_female <- data_split_5_heldout %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  
  # train
  # men
  model_male_split_1 <- multinom(formula_treatment, data = data_split_1_male, family = "binomial", weights = weight)
  model_male_split_2 <- multinom(formula_treatment, data = data_split_2_male, family = "binomial", weights = weight)
  model_male_split_3 <- multinom(formula_treatment, data = data_split_3_male, family = "binomial", weights = weight)
  model_male_split_4 <- multinom(formula_treatment, data = data_split_4_male, family = "binomial", weights = weight)
  model_male_split_5 <- multinom(formula_treatment, data = data_split_5_male, family = "binomial", weights = weight)
  
  # women
  model_female_split_1 <- multinom(formula_treatment, data = data_split_1_female, family = "binomial", weights = weight)
  model_female_split_2 <- multinom(formula_treatment, data = data_split_2_female, family = "binomial", weights = weight)
  model_female_split_3 <- multinom(formula_treatment, data = data_split_3_female, family = "binomial", weights = weight)
  model_female_split_4 <- multinom(formula_treatment, data = data_split_4_female, family = "binomial", weights = weight)
  model_female_split_5 <- multinom(formula_treatment, data = data_split_5_female, family = "binomial", weights = weight)
  
  # predict using model trained on other split
  # men
  data_split_1_heldout_male <- cbind(data_split_1_heldout_male, as.data.frame(predict(model_male_split_1, newdata = data_split_1_heldout_male, type = "probs")))
  colnames(data_split_1_heldout_male)[58:86] = paste0("prop_occ_", colnames(data_split_1_heldout_male)[58:86])
  
  data_split_2_heldout_male <- cbind(data_split_2_heldout_male, as.data.frame(predict(model_male_split_2, newdata = data_split_2_heldout_male, type = "probs")))
  colnames(data_split_2_heldout_male)[58:86] = paste0("prop_occ_", colnames(data_split_2_heldout_male)[58:86])
  
  data_split_3_heldout_male <- cbind(data_split_3_heldout_male, as.data.frame(predict(model_male_split_3, newdata = data_split_3_heldout_male, type = "probs")))
  colnames(data_split_3_heldout_male)[58:86] = paste0("prop_occ_", colnames(data_split_3_heldout_male)[58:86])
  
  data_split_4_heldout_male <- cbind(data_split_4_heldout_male, as.data.frame(predict(model_male_split_4, newdata = data_split_4_heldout_male, type = "probs")))
  colnames(data_split_4_heldout_male)[58:86] = paste0("prop_occ_", colnames(data_split_4_heldout_male)[58:86])
  
  data_split_5_heldout_male <- cbind(data_split_5_heldout_male, as.data.frame(predict(model_male_split_5, newdata = data_split_5_heldout_male, type = "probs")))
  colnames(data_split_5_heldout_male)[58:86] = paste0("prop_occ_", colnames(data_split_5_heldout_male)[58:86])
  
  # women
  data_split_1_heldout_female <- cbind(data_split_1_heldout_female, as.data.frame(predict(model_female_split_1, newdata = data_split_1_heldout_female, type = "probs")))
  colnames(data_split_1_heldout_female)[58:86] = paste0("prop_occ_", colnames(data_split_1_heldout_female)[58:86])
  
  data_split_2_heldout_female <- cbind(data_split_2_heldout_female, as.data.frame(predict(model_female_split_2, newdata = data_split_2_heldout_female, type = "probs")))
  colnames(data_split_2_heldout_female)[58:86] = paste0("prop_occ_", colnames(data_split_2_heldout_female)[58:86])
  
  data_split_3_heldout_female <- cbind(data_split_3_heldout_female, as.data.frame(predict(model_female_split_3, newdata = data_split_3_heldout_female, type = "probs")))
  colnames(data_split_3_heldout_female)[58:86] = paste0("prop_occ_", colnames(data_split_3_heldout_female)[58:86])
  
  data_split_4_heldout_female <- cbind(data_split_4_heldout_female, as.data.frame(predict(model_female_split_4, newdata = data_split_4_heldout_female, type = "probs")))
  colnames(data_split_4_heldout_female)[58:86] = paste0("prop_occ_", colnames(data_split_4_heldout_female)[58:86])
  
  data_split_5_heldout_female <- cbind(data_split_5_heldout_female, as.data.frame(predict(model_female_split_5, newdata = data_split_5_heldout_female, type = "probs")))
  colnames(data_split_5_heldout_female)[58:86] = paste0("prop_occ_", colnames(data_split_5_heldout_female)[58:86])
  
  # combine
  data_split_1_heldout <- rbind(data_split_1_heldout_male, data_split_1_heldout_female)
  data_split_2_heldout <- rbind(data_split_2_heldout_male, data_split_2_heldout_female)
  data_split_3_heldout <- rbind(data_split_3_heldout_male, data_split_3_heldout_female)
  data_split_4_heldout <- rbind(data_split_4_heldout_male, data_split_4_heldout_female)
  data_split_5_heldout <- rbind(data_split_5_heldout_male, data_split_5_heldout_female)
  
  data_split_1_heldout <- data_split_1_heldout %>% as.data.frame() %>% ungroup()
  data_split_2_heldout <- data_split_2_heldout %>% as.data.frame() %>% ungroup()
  data_split_3_heldout <- data_split_3_heldout %>% as.data.frame() %>% ungroup()
  data_split_4_heldout <- data_split_4_heldout %>% as.data.frame() %>% ungroup()
  data_split_5_heldout <- data_split_5_heldout %>% as.data.frame() %>% ungroup()
  
  
  # train
  # men
  model_male_split_1 <- SuperLearner(Y = data_split_1_male$outcome, X = data_split_1_male %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_1_male$weight, cvControl = list(V = 5))
  model_male_split_2 <- SuperLearner(Y = data_split_2_male$outcome, X = data_split_2_male %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_2_male$weight, cvControl = list(V = 5))
  model_male_split_3 <- SuperLearner(Y = data_split_3_male$outcome, X = data_split_3_male %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_3_male$weight, cvControl = list(V = 5))
  model_male_split_4 <- SuperLearner(Y = data_split_4_male$outcome, X = data_split_4_male %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_4_male$weight, cvControl = list(V = 5))
  model_male_split_5 <- SuperLearner(Y = data_split_5_male$outcome, X = data_split_5_male %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_5_male$weight, cvControl = list(V = 5))
  
  # women
  model_female_split_1 <- SuperLearner(Y = data_split_1_female$outcome, X = data_split_1_female %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_1_female$weight, cvControl = list(V = 5))
  model_female_split_2 <- SuperLearner(Y = data_split_2_female$outcome, X = data_split_2_female %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_2_female$weight, cvControl = list(V = 5))
  model_female_split_3 <- SuperLearner(Y = data_split_3_female$outcome, X = data_split_3_female %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_3_female$weight, cvControl = list(V = 5))
  model_female_split_4 <- SuperLearner(Y = data_split_4_female$outcome, X = data_split_4_female %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_4_female$weight, cvControl = list(V = 5))
  model_female_split_5 <- SuperLearner(Y = data_split_5_female$outcome, X = data_split_5_female %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_5_female$weight, cvControl = list(V = 5))
  
  # predictions
  for (i in levels(data$eseg_class_2)) {
    
    data_split_1_heldout_male$eseg_class_2 <- factor(i, levels = levels(data$eseg_class_2))
    data_split_2_heldout_male$eseg_class_2 <- factor(i, levels = levels(data$eseg_class_2))
    data_split_3_heldout_male$eseg_class_2 <- factor(i, levels = levels(data$eseg_class_2))
    data_split_4_heldout_male$eseg_class_2 <- factor(i, levels = levels(data$eseg_class_2))
    data_split_5_heldout_male$eseg_class_2 <- factor(i, levels = levels(data$eseg_class_2))
    
    data_split_1_heldout_female$eseg_class_2 <- factor(i, levels = levels(data$eseg_class_2))
    data_split_2_heldout_female$eseg_class_2 <- factor(i, levels = levels(data$eseg_class_2))
    data_split_3_heldout_female$eseg_class_2 <- factor(i, levels = levels(data$eseg_class_2))
    data_split_4_heldout_female$eseg_class_2 <- factor(i, levels = levels(data$eseg_class_2))
    data_split_5_heldout_female$eseg_class_2 <- factor(i, levels = levels(data$eseg_class_2))
    
    data_split_1_heldout_male$prediction <- as.data.frame(predict(model_male_split_1, newdata = data_split_1_heldout_male %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_2_heldout_male$prediction <- as.data.frame(predict(model_male_split_2, newdata = data_split_2_heldout_male %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_3_heldout_male$prediction <- as.data.frame(predict(model_male_split_3, newdata = data_split_3_heldout_male %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_4_heldout_male$prediction <- as.data.frame(predict(model_male_split_4, newdata = data_split_4_heldout_male %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_5_heldout_male$prediction <- as.data.frame(predict(model_male_split_5, newdata = data_split_5_heldout_male %>% select(all_of(preds_outcome)), type = "response"))$pred
    
    data_split_1_heldout_female$prediction <- as.data.frame(predict(model_female_split_1, newdata = data_split_1_heldout_female %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_2_heldout_female$prediction <- as.data.frame(predict(model_female_split_2, newdata = data_split_2_heldout_female %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_3_heldout_female$prediction <- as.data.frame(predict(model_female_split_3, newdata = data_split_3_heldout_female %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_4_heldout_female$prediction <- as.data.frame(predict(model_female_split_4, newdata = data_split_4_heldout_female %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_5_heldout_female$prediction <- as.data.frame(predict(model_female_split_5, newdata = data_split_5_heldout_female %>% select(all_of(preds_outcome)), type = "response"))$pred
    
    data_split_1_heldout_male <- data_split_1_heldout_male %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_2_heldout_male <- data_split_2_heldout_male %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_3_heldout_male <- data_split_3_heldout_male %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_4_heldout_male <- data_split_4_heldout_male %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_5_heldout_male <- data_split_5_heldout_male %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    
    data_split_1_heldout_female <- data_split_1_heldout_female %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_2_heldout_female <- data_split_2_heldout_female %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_3_heldout_female <- data_split_3_heldout_female %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_4_heldout_female <- data_split_4_heldout_female %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_5_heldout_female <- data_split_5_heldout_female %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    
    print(i)
    
  }
  
  rm(model_male_split_1, model_male_split_2, model_male_split_3, model_male_split_4, model_male_split_5, model_female_split_1, model_female_split_2, model_female_split_3, model_female_split_4, model_female_split_5)
  
  rm(i)
  
  # combine
  preds_split_1 <- rbind(data_split_1_heldout_male, data_split_1_heldout_female)
  preds_split_2 <- rbind(data_split_2_heldout_male, data_split_2_heldout_female)
  preds_split_3 <- rbind(data_split_3_heldout_male, data_split_3_heldout_female)
  preds_split_4 <- rbind(data_split_4_heldout_male, data_split_4_heldout_female)
  preds_split_5 <- rbind(data_split_5_heldout_male, data_split_5_heldout_female)
  
  preds_split_1 <- preds_split_1 %>% select(orgpid, age, contains("exp_outcome_occ_")) %>% group_by(orgpid, age) %>% slice(1)
  preds_split_2 <- preds_split_2 %>% select(orgpid, age, contains("exp_outcome_occ_")) %>% group_by(orgpid, age) %>% slice(1)
  preds_split_3 <- preds_split_3 %>% select(orgpid, age, contains("exp_outcome_occ_")) %>% group_by(orgpid, age) %>% slice(1)
  preds_split_4 <- preds_split_4 %>% select(orgpid, age, contains("exp_outcome_occ_")) %>% group_by(orgpid, age) %>% slice(1)
  preds_split_5 <- preds_split_5 %>% select(orgpid, age, contains("exp_outcome_occ_")) %>% group_by(orgpid, age) %>% slice(1)
  
  data_split_1_heldout <- data_split_1_heldout %>% left_join(preds_split_1, by = c("orgpid", "age")) %>% ungroup()
  data_split_2_heldout <- data_split_2_heldout %>% left_join(preds_split_2, by = c("orgpid", "age")) %>% ungroup()
  data_split_3_heldout <- data_split_3_heldout %>% left_join(preds_split_3, by = c("orgpid", "age")) %>% ungroup()
  data_split_4_heldout <- data_split_4_heldout %>% left_join(preds_split_4, by = c("orgpid", "age")) %>% ungroup()
  data_split_5_heldout <- data_split_5_heldout %>% left_join(preds_split_5, by = c("orgpid", "age")) %>% ungroup()
  
  
  
  # counterfactual probability of being assigned to the factual occupation
  cf_prob_fc_occ <- data %>% ungroup() %>%
    select(orgpid, age, eseg_class_2, contains("counterfact_share_occ")) %>%
    pivot_longer(cols = c(4:32), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 23, 24)) %>% 
    filter(eseg_class_2 == occupation) %>%
    rename("cf_prob_fc_occ" = "value") %>%
    select(orgpid, age, cf_prob_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  # inverse of the factual probability of the factual occupation
  inv_fc_prob_fc_occ_split_1 <- data_split_1_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class_2, contains("prop_occ")) %>%
    pivot_longer(cols = c(4:32), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 10, 11)) %>% 
    filter(eseg_class_2 == occupation) %>%
    mutate(value = 1 / value) %>%
    rename("inv_fc_prob_fc_occ" = "value") %>%
    select(orgpid, age, inv_fc_prob_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup() %>%
    mutate(inv_fc_prob_fc_occ = case_when(inv_fc_prob_fc_occ > quantile(inv_fc_prob_fc_occ, 0.999) ~ quantile(inv_fc_prob_fc_occ, 0.999),
                                          inv_fc_prob_fc_occ < quantile(inv_fc_prob_fc_occ, 0.001) ~ quantile(inv_fc_prob_fc_occ, 0.001),
                                          T ~ inv_fc_prob_fc_occ))
  
  inv_fc_prob_fc_occ_split_2 <- data_split_2_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class_2, contains("prop_occ")) %>%
    pivot_longer(cols = c(4:32), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 10, 11)) %>% 
    filter(eseg_class_2 == occupation) %>%
    mutate(value = 1 / value) %>%
    rename("inv_fc_prob_fc_occ" = "value") %>%
    select(orgpid, age, inv_fc_prob_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup() %>%
    mutate(inv_fc_prob_fc_occ = case_when(inv_fc_prob_fc_occ > quantile(inv_fc_prob_fc_occ, 0.999) ~ quantile(inv_fc_prob_fc_occ, 0.999),
                                          inv_fc_prob_fc_occ < quantile(inv_fc_prob_fc_occ, 0.001) ~ quantile(inv_fc_prob_fc_occ, 0.001),
                                          T ~ inv_fc_prob_fc_occ))
  
  inv_fc_prob_fc_occ_split_3 <- data_split_3_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class_2, contains("prop_occ")) %>%
    pivot_longer(cols = c(4:32), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 10, 11)) %>% 
    filter(eseg_class_2 == occupation) %>%
    mutate(value = 1 / value) %>%
    rename("inv_fc_prob_fc_occ" = "value") %>%
    select(orgpid, age, inv_fc_prob_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup() %>%
    mutate(inv_fc_prob_fc_occ = case_when(inv_fc_prob_fc_occ > quantile(inv_fc_prob_fc_occ, 0.999) ~ quantile(inv_fc_prob_fc_occ, 0.999),
                                          inv_fc_prob_fc_occ < quantile(inv_fc_prob_fc_occ, 0.001) ~ quantile(inv_fc_prob_fc_occ, 0.001),
                                          T ~ inv_fc_prob_fc_occ))
  
  inv_fc_prob_fc_occ_split_4 <- data_split_4_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class_2, contains("prop_occ")) %>%
    pivot_longer(cols = c(4:32), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 10, 11)) %>% 
    filter(eseg_class_2 == occupation) %>%
    mutate(value = 1 / value) %>%
    rename("inv_fc_prob_fc_occ" = "value") %>%
    select(orgpid, age, inv_fc_prob_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup() %>%
    mutate(inv_fc_prob_fc_occ = case_when(inv_fc_prob_fc_occ > quantile(inv_fc_prob_fc_occ, 0.999) ~ quantile(inv_fc_prob_fc_occ, 0.999),
                                          inv_fc_prob_fc_occ < quantile(inv_fc_prob_fc_occ, 0.001) ~ quantile(inv_fc_prob_fc_occ, 0.001),
                                          T ~ inv_fc_prob_fc_occ))
  
  inv_fc_prob_fc_occ_split_5 <- data_split_5_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class_2, contains("prop_occ")) %>%
    pivot_longer(cols = c(4:32), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 10, 11)) %>% 
    filter(eseg_class_2 == occupation) %>%
    mutate(value = 1 / value) %>%
    rename("inv_fc_prob_fc_occ" = "value") %>%
    select(orgpid, age, inv_fc_prob_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup() %>%
    mutate(inv_fc_prob_fc_occ = case_when(inv_fc_prob_fc_occ > quantile(inv_fc_prob_fc_occ, 0.999) ~ quantile(inv_fc_prob_fc_occ, 0.999),
                                          inv_fc_prob_fc_occ < quantile(inv_fc_prob_fc_occ, 0.001) ~ quantile(inv_fc_prob_fc_occ, 0.001),
                                          T ~ inv_fc_prob_fc_occ))
  
  # outcome predictions
  exp_outcome_fc_occ_split_1 <- data_split_1_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class_2, contains("exp_outcome_occ")) %>%
    pivot_longer(cols = c(4:32), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 17, 18)) %>% 
    filter(eseg_class_2 == occupation) %>%
    rename("exp_outcome_fc_occ" = "value") %>%
    select(orgpid, age, exp_outcome_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  exp_outcome_fc_occ_split_2 <- data_split_2_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class_2, contains("exp_outcome_occ")) %>%
    pivot_longer(cols = c(4:32), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 17, 18)) %>% 
    filter(eseg_class_2 == occupation) %>%
    rename("exp_outcome_fc_occ" = "value") %>%
    select(orgpid, age, exp_outcome_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  exp_outcome_fc_occ_split_3 <- data_split_3_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class_2, contains("exp_outcome_occ")) %>%
    pivot_longer(cols = c(4:32), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 17, 18)) %>% 
    filter(eseg_class_2 == occupation) %>%
    rename("exp_outcome_fc_occ" = "value") %>%
    select(orgpid, age, exp_outcome_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  exp_outcome_fc_occ_split_4 <- data_split_4_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class_2, contains("exp_outcome_occ")) %>%
    pivot_longer(cols = c(4:32), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 17, 18)) %>% 
    filter(eseg_class_2 == occupation) %>%
    rename("exp_outcome_fc_occ" = "value") %>%
    select(orgpid, age, exp_outcome_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  exp_outcome_fc_occ_split_5 <- data_split_5_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class_2, contains("exp_outcome_occ")) %>%
    pivot_longer(cols = c(4:32), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 17, 18)) %>% 
    filter(eseg_class_2 == occupation) %>%
    rename("exp_outcome_fc_occ" = "value") %>%
    select(orgpid, age, exp_outcome_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  # create combined weight
  weight_split_1 <- data_split_1_heldout %>% ungroup() %>%
    select(orgpid, age, weight) %>% 
    left_join(cf_prob_fc_occ, by = c("orgpid", "age")) %>%
    left_join(inv_fc_prob_fc_occ_split_1, by = c("orgpid", "age")) %>%
    mutate(combined_weight = weight * cf_prob_fc_occ * inv_fc_prob_fc_occ) %>%
    select(orgpid, age, combined_weight) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  weight_split_2 <- data_split_2_heldout %>% ungroup() %>%
    select(orgpid, age, weight) %>% 
    left_join(cf_prob_fc_occ, by = c("orgpid", "age")) %>%
    left_join(inv_fc_prob_fc_occ_split_2, by = c("orgpid", "age")) %>%
    mutate(combined_weight = weight * cf_prob_fc_occ * inv_fc_prob_fc_occ) %>%
    select(orgpid, age, combined_weight) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  weight_split_3 <- data_split_3_heldout %>% ungroup() %>%
    select(orgpid, age, weight) %>% 
    left_join(cf_prob_fc_occ, by = c("orgpid", "age")) %>%
    left_join(inv_fc_prob_fc_occ_split_3, by = c("orgpid", "age")) %>%
    mutate(combined_weight = weight * cf_prob_fc_occ * inv_fc_prob_fc_occ) %>%
    select(orgpid, age, combined_weight) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  weight_split_4 <- data_split_4_heldout %>% ungroup() %>%
    select(orgpid, age, weight) %>% 
    left_join(cf_prob_fc_occ, by = c("orgpid", "age")) %>%
    left_join(inv_fc_prob_fc_occ_split_4, by = c("orgpid", "age")) %>%
    mutate(combined_weight = weight * cf_prob_fc_occ * inv_fc_prob_fc_occ) %>%
    select(orgpid, age, combined_weight) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  weight_split_5 <- data_split_5_heldout %>% ungroup() %>%
    select(orgpid, age, weight) %>% 
    left_join(cf_prob_fc_occ, by = c("orgpid", "age")) %>%
    left_join(inv_fc_prob_fc_occ_split_5, by = c("orgpid", "age")) %>%
    mutate(combined_weight = weight * cf_prob_fc_occ * inv_fc_prob_fc_occ) %>%
    select(orgpid, age, combined_weight) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  # join
  data_split_1_heldout <- data_split_1_heldout %>% left_join(exp_outcome_fc_occ_split_1, by = c("orgpid", "age")) %>% left_join(weight_split_1, by = c("orgpid", "age")) %>% ungroup()
  data_split_2_heldout <- data_split_2_heldout %>% left_join(exp_outcome_fc_occ_split_2, by = c("orgpid", "age")) %>% left_join(weight_split_2, by = c("orgpid", "age")) %>% ungroup()
  data_split_3_heldout <- data_split_3_heldout %>% left_join(exp_outcome_fc_occ_split_3, by = c("orgpid", "age")) %>% left_join(weight_split_3, by = c("orgpid", "age")) %>% ungroup()
  data_split_4_heldout <- data_split_4_heldout %>% left_join(exp_outcome_fc_occ_split_4, by = c("orgpid", "age")) %>% left_join(weight_split_4, by = c("orgpid", "age")) %>% ungroup()
  data_split_5_heldout <- data_split_5_heldout %>% left_join(exp_outcome_fc_occ_split_5, by = c("orgpid", "age")) %>% left_join(weight_split_5, by = c("orgpid", "age")) %>% ungroup()
  
  # calculate bias correction
  bias_correction_m_split_1 <- data_split_1_heldout %>% filter(female == 0) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_m_split_2 <- data_split_2_heldout %>% filter(female == 0) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_m_split_3 <- data_split_3_heldout %>% filter(female == 0) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_m_split_4 <- data_split_4_heldout %>% filter(female == 0) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_m_split_5 <- data_split_5_heldout %>% filter(female == 0) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_f_split_1 <- data_split_1_heldout %>% filter(female == 1) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_f_split_2 <- data_split_2_heldout %>% filter(female == 1) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_f_split_3 <- data_split_3_heldout %>% filter(female == 1) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_f_split_4 <- data_split_4_heldout %>% filter(female == 1) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_f_split_5 <- data_split_5_heldout %>% filter(female == 1) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_split_1 <- rbind(bias_correction_m_split_1, bias_correction_f_split_1)
  bias_correction_split_2 <- rbind(bias_correction_m_split_2, bias_correction_f_split_2)
  bias_correction_split_3 <- rbind(bias_correction_m_split_3, bias_correction_f_split_3)
  bias_correction_split_4 <- rbind(bias_correction_m_split_4, bias_correction_f_split_4)
  bias_correction_split_5 <- rbind(bias_correction_m_split_5, bias_correction_f_split_5)
  
  # correct bias in outcome predictions
  data_split_1_heldout <- data_split_1_heldout %>% 
    left_join(bias_correction_split_1, by = "female") %>% ungroup() %>%
    mutate_at(vars(contains("exp_outcome_occ_")), ~(. - bias_correction)) %>% 
    mutate_at(vars(contains("exp_outcome_occ_")), ~(case_when(. > 1 ~ 1, . < 0 ~ 0, T ~ .)))
  
  data_split_2_heldout <- data_split_2_heldout %>% 
    left_join(bias_correction_split_2, by = "female") %>% ungroup() %>%
    mutate_at(vars(contains("exp_outcome_occ_")), ~(. - bias_correction)) %>% 
    mutate_at(vars(contains("exp_outcome_occ_")), ~(case_when(. > 1 ~ 1, . < 0 ~ 0, T ~ .)))
  
  data_split_3_heldout <- data_split_3_heldout %>% 
    left_join(bias_correction_split_3, by = "female") %>% ungroup() %>%
    mutate_at(vars(contains("exp_outcome_occ_")), ~(. - bias_correction)) %>% 
    mutate_at(vars(contains("exp_outcome_occ_")), ~(case_when(. > 1 ~ 1, . < 0 ~ 0, T ~ .)))
  
  data_split_4_heldout <- data_split_4_heldout %>% 
    left_join(bias_correction_split_4, by = "female") %>% ungroup() %>%
    mutate_at(vars(contains("exp_outcome_occ_")), ~(. - bias_correction)) %>% 
    mutate_at(vars(contains("exp_outcome_occ_")), ~(case_when(. > 1 ~ 1, . < 0 ~ 0, T ~ .)))
  
  data_split_5_heldout <- data_split_5_heldout %>% 
    left_join(bias_correction_split_5, by = "female") %>% ungroup() %>%
    mutate_at(vars(contains("exp_outcome_occ_")), ~(. - bias_correction)) %>% 
    mutate_at(vars(contains("exp_outcome_occ_")), ~(case_when(. > 1 ~ 1, . < 0 ~ 0, T ~ .)))
  
  # pre-intervention
  levels_pre_female <- data %>% filter(female == 1) %>% ungroup() %>%summarize(exp = weighted.mean(outcome, w = weight) * 100) %>% as.numeric()
  levels_pre_male <- data %>% filter(female == 0) %>% ungroup() %>%summarize(exp = weighted.mean(outcome, w = weight) * 100) %>% as.numeric()
  
  # post-intervention
  # split 1
  levels_post_female_split_1 <- data_split_1_heldout %>% filter(female == 1) %>% mutate(exp = (exp_outcome_occ_24 * counterfact_share_occ_24) + (exp_outcome_occ_33 * counterfact_share_occ_33) + (exp_outcome_occ_51 * counterfact_share_occ_51) + (exp_outcome_occ_25 * counterfact_share_occ_25) + (exp_outcome_occ_71 * counterfact_share_occ_71) + (exp_outcome_occ_21 * counterfact_share_occ_21) + (exp_outcome_occ_23 * counterfact_share_occ_23) + (exp_outcome_occ_42 * counterfact_share_occ_42) + (exp_outcome_occ_65 * counterfact_share_occ_65) + (exp_outcome_occ_61 * counterfact_share_occ_61) + (exp_outcome_occ_72 * counterfact_share_occ_72) + (exp_outcome_occ_13 * counterfact_share_occ_13) + (exp_outcome_occ_63 * counterfact_share_occ_63) + (exp_outcome_occ_34 * counterfact_share_occ_34) + (exp_outcome_occ_22 * counterfact_share_occ_22) + (exp_outcome_occ_31 * counterfact_share_occ_31) + (exp_outcome_occ_64 * counterfact_share_occ_64) + (exp_outcome_occ_73 * counterfact_share_occ_73) + (exp_outcome_occ_62 * counterfact_share_occ_62) + (exp_outcome_occ_54 * counterfact_share_occ_54) + (exp_outcome_occ_74 * counterfact_share_occ_74) + (exp_outcome_occ_11 * counterfact_share_occ_11) + (exp_outcome_occ_32 * counterfact_share_occ_32) + (exp_outcome_occ_43 * counterfact_share_occ_43) + (exp_outcome_occ_52 * counterfact_share_occ_52) + (exp_outcome_occ_53 * counterfact_share_occ_53) + (exp_outcome_occ_14 * counterfact_share_occ_14) + (exp_outcome_occ_12 * counterfact_share_occ_12) + (exp_outcome_occ_41 * counterfact_share_occ_41)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  levels_post_male_split_1 <- data_split_1_heldout %>% filter(female == 0) %>% mutate(exp = (exp_outcome_occ_24 * counterfact_share_occ_24) + (exp_outcome_occ_33 * counterfact_share_occ_33) + (exp_outcome_occ_51 * counterfact_share_occ_51) + (exp_outcome_occ_25 * counterfact_share_occ_25) + (exp_outcome_occ_71 * counterfact_share_occ_71) + (exp_outcome_occ_21 * counterfact_share_occ_21) + (exp_outcome_occ_23 * counterfact_share_occ_23) + (exp_outcome_occ_42 * counterfact_share_occ_42) + (exp_outcome_occ_65 * counterfact_share_occ_65) + (exp_outcome_occ_61 * counterfact_share_occ_61) + (exp_outcome_occ_72 * counterfact_share_occ_72) + (exp_outcome_occ_13 * counterfact_share_occ_13) + (exp_outcome_occ_63 * counterfact_share_occ_63) + (exp_outcome_occ_34 * counterfact_share_occ_34) + (exp_outcome_occ_22 * counterfact_share_occ_22) + (exp_outcome_occ_31 * counterfact_share_occ_31) + (exp_outcome_occ_64 * counterfact_share_occ_64) + (exp_outcome_occ_73 * counterfact_share_occ_73) + (exp_outcome_occ_62 * counterfact_share_occ_62) + (exp_outcome_occ_54 * counterfact_share_occ_54) + (exp_outcome_occ_74 * counterfact_share_occ_74) + (exp_outcome_occ_11 * counterfact_share_occ_11) + (exp_outcome_occ_32 * counterfact_share_occ_32) + (exp_outcome_occ_43 * counterfact_share_occ_43) + (exp_outcome_occ_52 * counterfact_share_occ_52) + (exp_outcome_occ_53 * counterfact_share_occ_53) + (exp_outcome_occ_14 * counterfact_share_occ_14) + (exp_outcome_occ_12 * counterfact_share_occ_12) + (exp_outcome_occ_41 * counterfact_share_occ_41)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  
  # split 2
  levels_post_female_split_2 <- data_split_2_heldout %>% filter(female == 1) %>% mutate(exp = (exp_outcome_occ_24 * counterfact_share_occ_24) + (exp_outcome_occ_33 * counterfact_share_occ_33) + (exp_outcome_occ_51 * counterfact_share_occ_51) + (exp_outcome_occ_25 * counterfact_share_occ_25) + (exp_outcome_occ_71 * counterfact_share_occ_71) + (exp_outcome_occ_21 * counterfact_share_occ_21) + (exp_outcome_occ_23 * counterfact_share_occ_23) + (exp_outcome_occ_42 * counterfact_share_occ_42) + (exp_outcome_occ_65 * counterfact_share_occ_65) + (exp_outcome_occ_61 * counterfact_share_occ_61) + (exp_outcome_occ_72 * counterfact_share_occ_72) + (exp_outcome_occ_13 * counterfact_share_occ_13) + (exp_outcome_occ_63 * counterfact_share_occ_63) + (exp_outcome_occ_34 * counterfact_share_occ_34) + (exp_outcome_occ_22 * counterfact_share_occ_22) + (exp_outcome_occ_31 * counterfact_share_occ_31) + (exp_outcome_occ_64 * counterfact_share_occ_64) + (exp_outcome_occ_73 * counterfact_share_occ_73) + (exp_outcome_occ_62 * counterfact_share_occ_62) + (exp_outcome_occ_54 * counterfact_share_occ_54) + (exp_outcome_occ_74 * counterfact_share_occ_74) + (exp_outcome_occ_11 * counterfact_share_occ_11) + (exp_outcome_occ_32 * counterfact_share_occ_32) + (exp_outcome_occ_43 * counterfact_share_occ_43) + (exp_outcome_occ_52 * counterfact_share_occ_52) + (exp_outcome_occ_53 * counterfact_share_occ_53) + (exp_outcome_occ_14 * counterfact_share_occ_14) + (exp_outcome_occ_12 * counterfact_share_occ_12) + (exp_outcome_occ_41 * counterfact_share_occ_41)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  levels_post_male_split_2 <- data_split_2_heldout %>% filter(female == 0) %>% mutate(exp = (exp_outcome_occ_24 * counterfact_share_occ_24) + (exp_outcome_occ_33 * counterfact_share_occ_33) + (exp_outcome_occ_51 * counterfact_share_occ_51) + (exp_outcome_occ_25 * counterfact_share_occ_25) + (exp_outcome_occ_71 * counterfact_share_occ_71) + (exp_outcome_occ_21 * counterfact_share_occ_21) + (exp_outcome_occ_23 * counterfact_share_occ_23) + (exp_outcome_occ_42 * counterfact_share_occ_42) + (exp_outcome_occ_65 * counterfact_share_occ_65) + (exp_outcome_occ_61 * counterfact_share_occ_61) + (exp_outcome_occ_72 * counterfact_share_occ_72) + (exp_outcome_occ_13 * counterfact_share_occ_13) + (exp_outcome_occ_63 * counterfact_share_occ_63) + (exp_outcome_occ_34 * counterfact_share_occ_34) + (exp_outcome_occ_22 * counterfact_share_occ_22) + (exp_outcome_occ_31 * counterfact_share_occ_31) + (exp_outcome_occ_64 * counterfact_share_occ_64) + (exp_outcome_occ_73 * counterfact_share_occ_73) + (exp_outcome_occ_62 * counterfact_share_occ_62) + (exp_outcome_occ_54 * counterfact_share_occ_54) + (exp_outcome_occ_74 * counterfact_share_occ_74) + (exp_outcome_occ_11 * counterfact_share_occ_11) + (exp_outcome_occ_32 * counterfact_share_occ_32) + (exp_outcome_occ_43 * counterfact_share_occ_43) + (exp_outcome_occ_52 * counterfact_share_occ_52) + (exp_outcome_occ_53 * counterfact_share_occ_53) + (exp_outcome_occ_14 * counterfact_share_occ_14) + (exp_outcome_occ_12 * counterfact_share_occ_12) + (exp_outcome_occ_41 * counterfact_share_occ_41)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  
  # split 3
  levels_post_female_split_3 <- data_split_3_heldout %>% filter(female == 1) %>% mutate(exp = (exp_outcome_occ_24 * counterfact_share_occ_24) + (exp_outcome_occ_33 * counterfact_share_occ_33) + (exp_outcome_occ_51 * counterfact_share_occ_51) + (exp_outcome_occ_25 * counterfact_share_occ_25) + (exp_outcome_occ_71 * counterfact_share_occ_71) + (exp_outcome_occ_21 * counterfact_share_occ_21) + (exp_outcome_occ_23 * counterfact_share_occ_23) + (exp_outcome_occ_42 * counterfact_share_occ_42) + (exp_outcome_occ_65 * counterfact_share_occ_65) + (exp_outcome_occ_61 * counterfact_share_occ_61) + (exp_outcome_occ_72 * counterfact_share_occ_72) + (exp_outcome_occ_13 * counterfact_share_occ_13) + (exp_outcome_occ_63 * counterfact_share_occ_63) + (exp_outcome_occ_34 * counterfact_share_occ_34) + (exp_outcome_occ_22 * counterfact_share_occ_22) + (exp_outcome_occ_31 * counterfact_share_occ_31) + (exp_outcome_occ_64 * counterfact_share_occ_64) + (exp_outcome_occ_73 * counterfact_share_occ_73) + (exp_outcome_occ_62 * counterfact_share_occ_62) + (exp_outcome_occ_54 * counterfact_share_occ_54) + (exp_outcome_occ_74 * counterfact_share_occ_74) + (exp_outcome_occ_11 * counterfact_share_occ_11) + (exp_outcome_occ_32 * counterfact_share_occ_32) + (exp_outcome_occ_43 * counterfact_share_occ_43) + (exp_outcome_occ_52 * counterfact_share_occ_52) + (exp_outcome_occ_53 * counterfact_share_occ_53) + (exp_outcome_occ_14 * counterfact_share_occ_14) + (exp_outcome_occ_12 * counterfact_share_occ_12) + (exp_outcome_occ_41 * counterfact_share_occ_41)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  levels_post_male_split_3 <- data_split_3_heldout %>% filter(female == 0) %>% mutate(exp = (exp_outcome_occ_24 * counterfact_share_occ_24) + (exp_outcome_occ_33 * counterfact_share_occ_33) + (exp_outcome_occ_51 * counterfact_share_occ_51) + (exp_outcome_occ_25 * counterfact_share_occ_25) + (exp_outcome_occ_71 * counterfact_share_occ_71) + (exp_outcome_occ_21 * counterfact_share_occ_21) + (exp_outcome_occ_23 * counterfact_share_occ_23) + (exp_outcome_occ_42 * counterfact_share_occ_42) + (exp_outcome_occ_65 * counterfact_share_occ_65) + (exp_outcome_occ_61 * counterfact_share_occ_61) + (exp_outcome_occ_72 * counterfact_share_occ_72) + (exp_outcome_occ_13 * counterfact_share_occ_13) + (exp_outcome_occ_63 * counterfact_share_occ_63) + (exp_outcome_occ_34 * counterfact_share_occ_34) + (exp_outcome_occ_22 * counterfact_share_occ_22) + (exp_outcome_occ_31 * counterfact_share_occ_31) + (exp_outcome_occ_64 * counterfact_share_occ_64) + (exp_outcome_occ_73 * counterfact_share_occ_73) + (exp_outcome_occ_62 * counterfact_share_occ_62) + (exp_outcome_occ_54 * counterfact_share_occ_54) + (exp_outcome_occ_74 * counterfact_share_occ_74) + (exp_outcome_occ_11 * counterfact_share_occ_11) + (exp_outcome_occ_32 * counterfact_share_occ_32) + (exp_outcome_occ_43 * counterfact_share_occ_43) + (exp_outcome_occ_52 * counterfact_share_occ_52) + (exp_outcome_occ_53 * counterfact_share_occ_53) + (exp_outcome_occ_14 * counterfact_share_occ_14) + (exp_outcome_occ_12 * counterfact_share_occ_12) + (exp_outcome_occ_41 * counterfact_share_occ_41)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  
  # split 4
  levels_post_female_split_4 <- data_split_4_heldout %>% filter(female == 1) %>% mutate(exp = (exp_outcome_occ_24 * counterfact_share_occ_24) + (exp_outcome_occ_33 * counterfact_share_occ_33) + (exp_outcome_occ_51 * counterfact_share_occ_51) + (exp_outcome_occ_25 * counterfact_share_occ_25) + (exp_outcome_occ_71 * counterfact_share_occ_71) + (exp_outcome_occ_21 * counterfact_share_occ_21) + (exp_outcome_occ_23 * counterfact_share_occ_23) + (exp_outcome_occ_42 * counterfact_share_occ_42) + (exp_outcome_occ_65 * counterfact_share_occ_65) + (exp_outcome_occ_61 * counterfact_share_occ_61) + (exp_outcome_occ_72 * counterfact_share_occ_72) + (exp_outcome_occ_13 * counterfact_share_occ_13) + (exp_outcome_occ_63 * counterfact_share_occ_63) + (exp_outcome_occ_34 * counterfact_share_occ_34) + (exp_outcome_occ_22 * counterfact_share_occ_22) + (exp_outcome_occ_31 * counterfact_share_occ_31) + (exp_outcome_occ_64 * counterfact_share_occ_64) + (exp_outcome_occ_73 * counterfact_share_occ_73) + (exp_outcome_occ_62 * counterfact_share_occ_62) + (exp_outcome_occ_54 * counterfact_share_occ_54) + (exp_outcome_occ_74 * counterfact_share_occ_74) + (exp_outcome_occ_11 * counterfact_share_occ_11) + (exp_outcome_occ_32 * counterfact_share_occ_32) + (exp_outcome_occ_43 * counterfact_share_occ_43) + (exp_outcome_occ_52 * counterfact_share_occ_52) + (exp_outcome_occ_53 * counterfact_share_occ_53) + (exp_outcome_occ_14 * counterfact_share_occ_14) + (exp_outcome_occ_12 * counterfact_share_occ_12) + (exp_outcome_occ_41 * counterfact_share_occ_41)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  levels_post_male_split_4 <- data_split_4_heldout %>% filter(female == 0) %>% mutate(exp = (exp_outcome_occ_24 * counterfact_share_occ_24) + (exp_outcome_occ_33 * counterfact_share_occ_33) + (exp_outcome_occ_51 * counterfact_share_occ_51) + (exp_outcome_occ_25 * counterfact_share_occ_25) + (exp_outcome_occ_71 * counterfact_share_occ_71) + (exp_outcome_occ_21 * counterfact_share_occ_21) + (exp_outcome_occ_23 * counterfact_share_occ_23) + (exp_outcome_occ_42 * counterfact_share_occ_42) + (exp_outcome_occ_65 * counterfact_share_occ_65) + (exp_outcome_occ_61 * counterfact_share_occ_61) + (exp_outcome_occ_72 * counterfact_share_occ_72) + (exp_outcome_occ_13 * counterfact_share_occ_13) + (exp_outcome_occ_63 * counterfact_share_occ_63) + (exp_outcome_occ_34 * counterfact_share_occ_34) + (exp_outcome_occ_22 * counterfact_share_occ_22) + (exp_outcome_occ_31 * counterfact_share_occ_31) + (exp_outcome_occ_64 * counterfact_share_occ_64) + (exp_outcome_occ_73 * counterfact_share_occ_73) + (exp_outcome_occ_62 * counterfact_share_occ_62) + (exp_outcome_occ_54 * counterfact_share_occ_54) + (exp_outcome_occ_74 * counterfact_share_occ_74) + (exp_outcome_occ_11 * counterfact_share_occ_11) + (exp_outcome_occ_32 * counterfact_share_occ_32) + (exp_outcome_occ_43 * counterfact_share_occ_43) + (exp_outcome_occ_52 * counterfact_share_occ_52) + (exp_outcome_occ_53 * counterfact_share_occ_53) + (exp_outcome_occ_14 * counterfact_share_occ_14) + (exp_outcome_occ_12 * counterfact_share_occ_12) + (exp_outcome_occ_41 * counterfact_share_occ_41)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  
  # split 5
  levels_post_female_split_5 <- data_split_5_heldout %>% filter(female == 1) %>% mutate(exp = (exp_outcome_occ_24 * counterfact_share_occ_24) + (exp_outcome_occ_33 * counterfact_share_occ_33) + (exp_outcome_occ_51 * counterfact_share_occ_51) + (exp_outcome_occ_25 * counterfact_share_occ_25) + (exp_outcome_occ_71 * counterfact_share_occ_71) + (exp_outcome_occ_21 * counterfact_share_occ_21) + (exp_outcome_occ_23 * counterfact_share_occ_23) + (exp_outcome_occ_42 * counterfact_share_occ_42) + (exp_outcome_occ_65 * counterfact_share_occ_65) + (exp_outcome_occ_61 * counterfact_share_occ_61) + (exp_outcome_occ_72 * counterfact_share_occ_72) + (exp_outcome_occ_13 * counterfact_share_occ_13) + (exp_outcome_occ_63 * counterfact_share_occ_63) + (exp_outcome_occ_34 * counterfact_share_occ_34) + (exp_outcome_occ_22 * counterfact_share_occ_22) + (exp_outcome_occ_31 * counterfact_share_occ_31) + (exp_outcome_occ_64 * counterfact_share_occ_64) + (exp_outcome_occ_73 * counterfact_share_occ_73) + (exp_outcome_occ_62 * counterfact_share_occ_62) + (exp_outcome_occ_54 * counterfact_share_occ_54) + (exp_outcome_occ_74 * counterfact_share_occ_74) + (exp_outcome_occ_11 * counterfact_share_occ_11) + (exp_outcome_occ_32 * counterfact_share_occ_32) + (exp_outcome_occ_43 * counterfact_share_occ_43) + (exp_outcome_occ_52 * counterfact_share_occ_52) + (exp_outcome_occ_53 * counterfact_share_occ_53) + (exp_outcome_occ_14 * counterfact_share_occ_14) + (exp_outcome_occ_12 * counterfact_share_occ_12) + (exp_outcome_occ_41 * counterfact_share_occ_41)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  levels_post_male_split_5 <- data_split_5_heldout %>% filter(female == 0) %>% mutate(exp = (exp_outcome_occ_24 * counterfact_share_occ_24) + (exp_outcome_occ_33 * counterfact_share_occ_33) + (exp_outcome_occ_51 * counterfact_share_occ_51) + (exp_outcome_occ_25 * counterfact_share_occ_25) + (exp_outcome_occ_71 * counterfact_share_occ_71) + (exp_outcome_occ_21 * counterfact_share_occ_21) + (exp_outcome_occ_23 * counterfact_share_occ_23) + (exp_outcome_occ_42 * counterfact_share_occ_42) + (exp_outcome_occ_65 * counterfact_share_occ_65) + (exp_outcome_occ_61 * counterfact_share_occ_61) + (exp_outcome_occ_72 * counterfact_share_occ_72) + (exp_outcome_occ_13 * counterfact_share_occ_13) + (exp_outcome_occ_63 * counterfact_share_occ_63) + (exp_outcome_occ_34 * counterfact_share_occ_34) + (exp_outcome_occ_22 * counterfact_share_occ_22) + (exp_outcome_occ_31 * counterfact_share_occ_31) + (exp_outcome_occ_64 * counterfact_share_occ_64) + (exp_outcome_occ_73 * counterfact_share_occ_73) + (exp_outcome_occ_62 * counterfact_share_occ_62) + (exp_outcome_occ_54 * counterfact_share_occ_54) + (exp_outcome_occ_74 * counterfact_share_occ_74) + (exp_outcome_occ_11 * counterfact_share_occ_11) + (exp_outcome_occ_32 * counterfact_share_occ_32) + (exp_outcome_occ_43 * counterfact_share_occ_43) + (exp_outcome_occ_52 * counterfact_share_occ_52) + (exp_outcome_occ_53 * counterfact_share_occ_53) + (exp_outcome_occ_14 * counterfact_share_occ_14) + (exp_outcome_occ_12 * counterfact_share_occ_12) + (exp_outcome_occ_41 * counterfact_share_occ_41)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  
  # average cross-fits
  levels_post_female <- (levels_post_female_split_1 + levels_post_female_split_2 + levels_post_female_split_3 + levels_post_female_split_4 + levels_post_female_split_5) / 5
  levels_post_male <- (levels_post_male_split_1 + levels_post_male_split_2 + levels_post_male_split_3 + levels_post_male_split_4 + levels_post_male_split_5) / 5
  
  # calculate estimands
  gap_pre <- levels_pre_female - levels_pre_male
  
  gap_post <- levels_post_female - levels_post_male
  
  change <- gap_post - gap_pre
  
  change_pp <- (change / gap_pre) * 100
  
  estimates_men_pre <- data.frame(estimand = "Men (before intervention)", "est" = round(levels_pre_male, 2))
  estimates_women_pre <- data.frame(estimand = "Women (before intervention)", "est" = round(levels_pre_female, 2))
  estimates_men_post <- data.frame(estimand = "Men (after intervention)", "est" = round(levels_post_male, 2))
  estimates_women_post <- data.frame(estimand = "Women (after intervention)", "est" = round(levels_post_female, 2))
  estimates_gap_pre <- data.frame(estimand = "Disparity (before intervention)", "est" = round(gap_pre, 2))
  estimates_gap_post <- data.frame(estimand = "Disparity (after intervention)", "est" = round(gap_post, 2))
  estimates_reduction <- data.frame(estimand = "Change in disparity", "est" = round(change, 2))
  estimates_reduction_pp <- data.frame(estimand = "Change in disparity (pp.)", "est" = round(change_pp, 2))
  
  estimates <- rbind(estimates_men_pre, estimates_women_pre, estimates_men_post, estimates_women_post, estimates_gap_pre, estimates_gap_post, estimates_reduction, estimates_reduction_pp)
  
  rm(list = setdiff(ls(), c("estimates", "data_final", "decompose_cond_dr_xfit", "formula_treatment", "preds_outcome", "sl_models")))
  
  gc()
  
  return(estimates)
  
}


decompose_dr_xfit_intD_detailed <- function(data, formula_treatment, preds_outcome, sl_models) {
  
  # select complete cases
  data <- data %>% na.omit()
  
  # determine counter-factual composition via intervention rule
  composition_cf <- multinom("eseg_class_2 ~ education_level", data = data, family = "binomial", weights = weight)
  
  composition_cf <- as.data.frame(predict(composition_cf, type = "probs")) %>% slice(1)
  
  colnames(composition_cf) <- paste0("counterfact_share_occ_", colnames(composition_cf))
  
  data <- data %>% cross_join(composition_cf)
  
  rm(list = setdiff(ls(), c("data", "formula_treatment", "preds_outcome", "sl_models")))
  
  
  
  # create cross-fitting splits
  split <- split(data, f = rep_len(1:5, nrow(data)))
  
  data_split_1 = rbind(split$`2`, split$`3`, split$`4`, split$`5`)
  data_split_1_heldout = split$`1`
  
  data_split_2 = rbind(split$`1`, split$`3`, split$`4`, split$`5`)
  data_split_2_heldout = split$`2`
  
  data_split_3 = rbind(split$`1`, split$`2`, split$`4`, split$`5`)
  data_split_3_heldout = split$`3`
  
  data_split_4 = rbind(split$`1`, split$`2`, split$`3`, split$`5`)
  data_split_4_heldout = split$`4`
  
  data_split_5 = rbind(split$`1`, split$`2`, split$`3`, split$`4`)
  data_split_5_heldout = split$`5`
  
  rm(split)
  
  
  
  # treatment model
  data_split_1_male <- data_split_1 %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_2_male <- data_split_2 %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_3_male <- data_split_3 %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_4_male <- data_split_4 %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_5_male <- data_split_5 %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  
  data_split_1_female <- data_split_1 %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_2_female <- data_split_2 %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_3_female <- data_split_3 %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_4_female <- data_split_4 %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_5_female <- data_split_5 %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  
  data_split_1_heldout_male <- data_split_1_heldout %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_2_heldout_male <- data_split_2_heldout %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_3_heldout_male <- data_split_3_heldout %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_4_heldout_male <- data_split_4_heldout %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_5_heldout_male <- data_split_5_heldout %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  
  data_split_1_heldout_female <- data_split_1_heldout %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_2_heldout_female <- data_split_2_heldout %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_3_heldout_female <- data_split_3_heldout %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_4_heldout_female <- data_split_4_heldout %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  data_split_5_heldout_female <- data_split_5_heldout %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class_2 = factor(eseg_class_2, levels = levels(data$eseg_class_2)))
  
  # train
  # men
  model_male_split_1 <- multinom(formula_treatment, data = data_split_1_male, family = "binomial", weights = weight)
  model_male_split_2 <- multinom(formula_treatment, data = data_split_2_male, family = "binomial", weights = weight)
  model_male_split_3 <- multinom(formula_treatment, data = data_split_3_male, family = "binomial", weights = weight)
  model_male_split_4 <- multinom(formula_treatment, data = data_split_4_male, family = "binomial", weights = weight)
  model_male_split_5 <- multinom(formula_treatment, data = data_split_5_male, family = "binomial", weights = weight)
  
  # women
  model_female_split_1 <- multinom(formula_treatment, data = data_split_1_female, family = "binomial", weights = weight)
  model_female_split_2 <- multinom(formula_treatment, data = data_split_2_female, family = "binomial", weights = weight)
  model_female_split_3 <- multinom(formula_treatment, data = data_split_3_female, family = "binomial", weights = weight)
  model_female_split_4 <- multinom(formula_treatment, data = data_split_4_female, family = "binomial", weights = weight)
  model_female_split_5 <- multinom(formula_treatment, data = data_split_5_female, family = "binomial", weights = weight)
  
  # predict using model trained on other split
  # men
  data_split_1_heldout_male <- cbind(data_split_1_heldout_male, as.data.frame(predict(model_male_split_1, newdata = data_split_1_heldout_male, type = "probs")))
  colnames(data_split_1_heldout_male)[58:86] = paste0("prop_occ_", colnames(data_split_1_heldout_male)[58:86])
  
  data_split_2_heldout_male <- cbind(data_split_2_heldout_male, as.data.frame(predict(model_male_split_2, newdata = data_split_2_heldout_male, type = "probs")))
  colnames(data_split_2_heldout_male)[58:86] = paste0("prop_occ_", colnames(data_split_2_heldout_male)[58:86])
  
  data_split_3_heldout_male <- cbind(data_split_3_heldout_male, as.data.frame(predict(model_male_split_3, newdata = data_split_3_heldout_male, type = "probs")))
  colnames(data_split_3_heldout_male)[58:86] = paste0("prop_occ_", colnames(data_split_3_heldout_male)[58:86])
  
  data_split_4_heldout_male <- cbind(data_split_4_heldout_male, as.data.frame(predict(model_male_split_4, newdata = data_split_4_heldout_male, type = "probs")))
  colnames(data_split_4_heldout_male)[58:86] = paste0("prop_occ_", colnames(data_split_4_heldout_male)[58:86])
  
  data_split_5_heldout_male <- cbind(data_split_5_heldout_male, as.data.frame(predict(model_male_split_5, newdata = data_split_5_heldout_male, type = "probs")))
  colnames(data_split_5_heldout_male)[58:86] = paste0("prop_occ_", colnames(data_split_5_heldout_male)[58:86])
  
  # women
  data_split_1_heldout_female <- cbind(data_split_1_heldout_female, as.data.frame(predict(model_female_split_1, newdata = data_split_1_heldout_female, type = "probs")))
  colnames(data_split_1_heldout_female)[58:86] = paste0("prop_occ_", colnames(data_split_1_heldout_female)[58:86])
  
  data_split_2_heldout_female <- cbind(data_split_2_heldout_female, as.data.frame(predict(model_female_split_2, newdata = data_split_2_heldout_female, type = "probs")))
  colnames(data_split_2_heldout_female)[58:86] = paste0("prop_occ_", colnames(data_split_2_heldout_female)[58:86])
  
  data_split_3_heldout_female <- cbind(data_split_3_heldout_female, as.data.frame(predict(model_female_split_3, newdata = data_split_3_heldout_female, type = "probs")))
  colnames(data_split_3_heldout_female)[58:86] = paste0("prop_occ_", colnames(data_split_3_heldout_female)[58:86])
  
  data_split_4_heldout_female <- cbind(data_split_4_heldout_female, as.data.frame(predict(model_female_split_4, newdata = data_split_4_heldout_female, type = "probs")))
  colnames(data_split_4_heldout_female)[58:86] = paste0("prop_occ_", colnames(data_split_4_heldout_female)[58:86])
  
  data_split_5_heldout_female <- cbind(data_split_5_heldout_female, as.data.frame(predict(model_female_split_5, newdata = data_split_5_heldout_female, type = "probs")))
  colnames(data_split_5_heldout_female)[58:86] = paste0("prop_occ_", colnames(data_split_5_heldout_female)[58:86])
  
  # combine
  data_split_1_heldout <- rbind(data_split_1_heldout_male, data_split_1_heldout_female)
  data_split_2_heldout <- rbind(data_split_2_heldout_male, data_split_2_heldout_female)
  data_split_3_heldout <- rbind(data_split_3_heldout_male, data_split_3_heldout_female)
  data_split_4_heldout <- rbind(data_split_4_heldout_male, data_split_4_heldout_female)
  data_split_5_heldout <- rbind(data_split_5_heldout_male, data_split_5_heldout_female)
  
  data_split_1_heldout <- data_split_1_heldout %>% as.data.frame() %>% ungroup()
  data_split_2_heldout <- data_split_2_heldout %>% as.data.frame() %>% ungroup()
  data_split_3_heldout <- data_split_3_heldout %>% as.data.frame() %>% ungroup()
  data_split_4_heldout <- data_split_4_heldout %>% as.data.frame() %>% ungroup()
  data_split_5_heldout <- data_split_5_heldout %>% as.data.frame() %>% ungroup()
  
  
  # train
  # men
  model_male_split_1 <- SuperLearner(Y = data_split_1_male$outcome, X = data_split_1_male %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_1_male$weight, cvControl = list(V = 5))
  model_male_split_2 <- SuperLearner(Y = data_split_2_male$outcome, X = data_split_2_male %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_2_male$weight, cvControl = list(V = 5))
  model_male_split_3 <- SuperLearner(Y = data_split_3_male$outcome, X = data_split_3_male %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_3_male$weight, cvControl = list(V = 5))
  model_male_split_4 <- SuperLearner(Y = data_split_4_male$outcome, X = data_split_4_male %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_4_male$weight, cvControl = list(V = 5))
  model_male_split_5 <- SuperLearner(Y = data_split_5_male$outcome, X = data_split_5_male %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_5_male$weight, cvControl = list(V = 5))
  
  # women
  model_female_split_1 <- SuperLearner(Y = data_split_1_female$outcome, X = data_split_1_female %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_1_female$weight, cvControl = list(V = 5))
  model_female_split_2 <- SuperLearner(Y = data_split_2_female$outcome, X = data_split_2_female %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_2_female$weight, cvControl = list(V = 5))
  model_female_split_3 <- SuperLearner(Y = data_split_3_female$outcome, X = data_split_3_female %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_3_female$weight, cvControl = list(V = 5))
  model_female_split_4 <- SuperLearner(Y = data_split_4_female$outcome, X = data_split_4_female %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_4_female$weight, cvControl = list(V = 5))
  model_female_split_5 <- SuperLearner(Y = data_split_5_female$outcome, X = data_split_5_female %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_5_female$weight, cvControl = list(V = 5))
  
  # predictions
  for (i in levels(data$eseg_class_2)) {
    
    data_split_1_heldout_male$eseg_class_2 <- factor(i, levels = levels(data$eseg_class_2))
    data_split_2_heldout_male$eseg_class_2 <- factor(i, levels = levels(data$eseg_class_2))
    data_split_3_heldout_male$eseg_class_2 <- factor(i, levels = levels(data$eseg_class_2))
    data_split_4_heldout_male$eseg_class_2 <- factor(i, levels = levels(data$eseg_class_2))
    data_split_5_heldout_male$eseg_class_2 <- factor(i, levels = levels(data$eseg_class_2))
    
    data_split_1_heldout_female$eseg_class_2 <- factor(i, levels = levels(data$eseg_class_2))
    data_split_2_heldout_female$eseg_class_2 <- factor(i, levels = levels(data$eseg_class_2))
    data_split_3_heldout_female$eseg_class_2 <- factor(i, levels = levels(data$eseg_class_2))
    data_split_4_heldout_female$eseg_class_2 <- factor(i, levels = levels(data$eseg_class_2))
    data_split_5_heldout_female$eseg_class_2 <- factor(i, levels = levels(data$eseg_class_2))
    
    data_split_1_heldout_male$prediction <- as.data.frame(predict(model_male_split_1, newdata = data_split_1_heldout_male %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_2_heldout_male$prediction <- as.data.frame(predict(model_male_split_2, newdata = data_split_2_heldout_male %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_3_heldout_male$prediction <- as.data.frame(predict(model_male_split_3, newdata = data_split_3_heldout_male %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_4_heldout_male$prediction <- as.data.frame(predict(model_male_split_4, newdata = data_split_4_heldout_male %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_5_heldout_male$prediction <- as.data.frame(predict(model_male_split_5, newdata = data_split_5_heldout_male %>% select(all_of(preds_outcome)), type = "response"))$pred
    
    data_split_1_heldout_female$prediction <- as.data.frame(predict(model_female_split_1, newdata = data_split_1_heldout_female %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_2_heldout_female$prediction <- as.data.frame(predict(model_female_split_2, newdata = data_split_2_heldout_female %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_3_heldout_female$prediction <- as.data.frame(predict(model_female_split_3, newdata = data_split_3_heldout_female %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_4_heldout_female$prediction <- as.data.frame(predict(model_female_split_4, newdata = data_split_4_heldout_female %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_5_heldout_female$prediction <- as.data.frame(predict(model_female_split_5, newdata = data_split_5_heldout_female %>% select(all_of(preds_outcome)), type = "response"))$pred
    
    data_split_1_heldout_male <- data_split_1_heldout_male %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_2_heldout_male <- data_split_2_heldout_male %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_3_heldout_male <- data_split_3_heldout_male %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_4_heldout_male <- data_split_4_heldout_male %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_5_heldout_male <- data_split_5_heldout_male %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    
    data_split_1_heldout_female <- data_split_1_heldout_female %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_2_heldout_female <- data_split_2_heldout_female %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_3_heldout_female <- data_split_3_heldout_female %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_4_heldout_female <- data_split_4_heldout_female %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_5_heldout_female <- data_split_5_heldout_female %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    
    print(i)
    
  }
  
  rm(model_male_split_1, model_male_split_2, model_male_split_3, model_male_split_4, model_male_split_5, model_female_split_1, model_female_split_2, model_female_split_3, model_female_split_4, model_female_split_5)
  
  rm(i)
  
  # combine
  preds_split_1 <- rbind(data_split_1_heldout_male, data_split_1_heldout_female)
  preds_split_2 <- rbind(data_split_2_heldout_male, data_split_2_heldout_female)
  preds_split_3 <- rbind(data_split_3_heldout_male, data_split_3_heldout_female)
  preds_split_4 <- rbind(data_split_4_heldout_male, data_split_4_heldout_female)
  preds_split_5 <- rbind(data_split_5_heldout_male, data_split_5_heldout_female)
  
  preds_split_1 <- preds_split_1 %>% select(orgpid, age, contains("exp_outcome_occ_")) %>% group_by(orgpid, age) %>% slice(1)
  preds_split_2 <- preds_split_2 %>% select(orgpid, age, contains("exp_outcome_occ_")) %>% group_by(orgpid, age) %>% slice(1)
  preds_split_3 <- preds_split_3 %>% select(orgpid, age, contains("exp_outcome_occ_")) %>% group_by(orgpid, age) %>% slice(1)
  preds_split_4 <- preds_split_4 %>% select(orgpid, age, contains("exp_outcome_occ_")) %>% group_by(orgpid, age) %>% slice(1)
  preds_split_5 <- preds_split_5 %>% select(orgpid, age, contains("exp_outcome_occ_")) %>% group_by(orgpid, age) %>% slice(1)
  
  data_split_1_heldout <- data_split_1_heldout %>% left_join(preds_split_1, by = c("orgpid", "age")) %>% ungroup()
  data_split_2_heldout <- data_split_2_heldout %>% left_join(preds_split_2, by = c("orgpid", "age")) %>% ungroup()
  data_split_3_heldout <- data_split_3_heldout %>% left_join(preds_split_3, by = c("orgpid", "age")) %>% ungroup()
  data_split_4_heldout <- data_split_4_heldout %>% left_join(preds_split_4, by = c("orgpid", "age")) %>% ungroup()
  data_split_5_heldout <- data_split_5_heldout %>% left_join(preds_split_5, by = c("orgpid", "age")) %>% ungroup()
  
  
  
  # counterfactual probability of being assigned to the factual occupation
  cf_prob_fc_occ <- data %>% ungroup() %>%
    select(orgpid, age, eseg_class_2, contains("counterfact_share_occ")) %>%
    pivot_longer(cols = c(4:32), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 23, 24)) %>% 
    filter(eseg_class_2 == occupation) %>%
    rename("cf_prob_fc_occ" = "value") %>%
    select(orgpid, age, cf_prob_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  # inverse of the factual probability of the factual occupation
  inv_fc_prob_fc_occ_split_1 <- data_split_1_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class_2, contains("prop_occ")) %>%
    pivot_longer(cols = c(4:32), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 10, 11)) %>% 
    filter(eseg_class_2 == occupation) %>%
    mutate(value = 1 / value) %>%
    rename("inv_fc_prob_fc_occ" = "value") %>%
    select(orgpid, age, inv_fc_prob_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup() %>%
    mutate(inv_fc_prob_fc_occ = case_when(inv_fc_prob_fc_occ > quantile(inv_fc_prob_fc_occ, 0.999) ~ quantile(inv_fc_prob_fc_occ, 0.999),
                                          inv_fc_prob_fc_occ < quantile(inv_fc_prob_fc_occ, 0.001) ~ quantile(inv_fc_prob_fc_occ, 0.001),
                                          T ~ inv_fc_prob_fc_occ))
  
  inv_fc_prob_fc_occ_split_2 <- data_split_2_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class_2, contains("prop_occ")) %>%
    pivot_longer(cols = c(4:32), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 10, 11)) %>% 
    filter(eseg_class_2 == occupation) %>%
    mutate(value = 1 / value) %>%
    rename("inv_fc_prob_fc_occ" = "value") %>%
    select(orgpid, age, inv_fc_prob_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup() %>%
    mutate(inv_fc_prob_fc_occ = case_when(inv_fc_prob_fc_occ > quantile(inv_fc_prob_fc_occ, 0.999) ~ quantile(inv_fc_prob_fc_occ, 0.999),
                                          inv_fc_prob_fc_occ < quantile(inv_fc_prob_fc_occ, 0.001) ~ quantile(inv_fc_prob_fc_occ, 0.001),
                                          T ~ inv_fc_prob_fc_occ))
  
  inv_fc_prob_fc_occ_split_3 <- data_split_3_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class_2, contains("prop_occ")) %>%
    pivot_longer(cols = c(4:32), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 10, 11)) %>% 
    filter(eseg_class_2 == occupation) %>%
    mutate(value = 1 / value) %>%
    rename("inv_fc_prob_fc_occ" = "value") %>%
    select(orgpid, age, inv_fc_prob_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup() %>%
    mutate(inv_fc_prob_fc_occ = case_when(inv_fc_prob_fc_occ > quantile(inv_fc_prob_fc_occ, 0.999) ~ quantile(inv_fc_prob_fc_occ, 0.999),
                                          inv_fc_prob_fc_occ < quantile(inv_fc_prob_fc_occ, 0.001) ~ quantile(inv_fc_prob_fc_occ, 0.001),
                                          T ~ inv_fc_prob_fc_occ))
  
  inv_fc_prob_fc_occ_split_4 <- data_split_4_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class_2, contains("prop_occ")) %>%
    pivot_longer(cols = c(4:32), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 10, 11)) %>% 
    filter(eseg_class_2 == occupation) %>%
    mutate(value = 1 / value) %>%
    rename("inv_fc_prob_fc_occ" = "value") %>%
    select(orgpid, age, inv_fc_prob_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup() %>%
    mutate(inv_fc_prob_fc_occ = case_when(inv_fc_prob_fc_occ > quantile(inv_fc_prob_fc_occ, 0.999) ~ quantile(inv_fc_prob_fc_occ, 0.999),
                                          inv_fc_prob_fc_occ < quantile(inv_fc_prob_fc_occ, 0.001) ~ quantile(inv_fc_prob_fc_occ, 0.001),
                                          T ~ inv_fc_prob_fc_occ))
  
  inv_fc_prob_fc_occ_split_5 <- data_split_5_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class_2, contains("prop_occ")) %>%
    pivot_longer(cols = c(4:32), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 10, 11)) %>% 
    filter(eseg_class_2 == occupation) %>%
    mutate(value = 1 / value) %>%
    rename("inv_fc_prob_fc_occ" = "value") %>%
    select(orgpid, age, inv_fc_prob_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup() %>%
    mutate(inv_fc_prob_fc_occ = case_when(inv_fc_prob_fc_occ > quantile(inv_fc_prob_fc_occ, 0.999) ~ quantile(inv_fc_prob_fc_occ, 0.999),
                                          inv_fc_prob_fc_occ < quantile(inv_fc_prob_fc_occ, 0.001) ~ quantile(inv_fc_prob_fc_occ, 0.001),
                                          T ~ inv_fc_prob_fc_occ))
  
  # outcome predictions
  exp_outcome_fc_occ_split_1 <- data_split_1_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class_2, contains("exp_outcome_occ")) %>%
    pivot_longer(cols = c(4:32), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 17, 18)) %>% 
    filter(eseg_class_2 == occupation) %>%
    rename("exp_outcome_fc_occ" = "value") %>%
    select(orgpid, age, exp_outcome_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  exp_outcome_fc_occ_split_2 <- data_split_2_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class_2, contains("exp_outcome_occ")) %>%
    pivot_longer(cols = c(4:32), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 17, 18)) %>% 
    filter(eseg_class_2 == occupation) %>%
    rename("exp_outcome_fc_occ" = "value") %>%
    select(orgpid, age, exp_outcome_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  exp_outcome_fc_occ_split_3 <- data_split_3_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class_2, contains("exp_outcome_occ")) %>%
    pivot_longer(cols = c(4:32), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 17, 18)) %>% 
    filter(eseg_class_2 == occupation) %>%
    rename("exp_outcome_fc_occ" = "value") %>%
    select(orgpid, age, exp_outcome_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  exp_outcome_fc_occ_split_4 <- data_split_4_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class_2, contains("exp_outcome_occ")) %>%
    pivot_longer(cols = c(4:32), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 17, 18)) %>% 
    filter(eseg_class_2 == occupation) %>%
    rename("exp_outcome_fc_occ" = "value") %>%
    select(orgpid, age, exp_outcome_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  exp_outcome_fc_occ_split_5 <- data_split_5_heldout %>% ungroup() %>%
    select(orgpid, age, eseg_class_2, contains("exp_outcome_occ")) %>%
    pivot_longer(cols = c(4:32), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 17, 18)) %>% 
    filter(eseg_class_2 == occupation) %>%
    rename("exp_outcome_fc_occ" = "value") %>%
    select(orgpid, age, exp_outcome_fc_occ) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  # create combined weight
  weight_split_1 <- data_split_1_heldout %>% ungroup() %>%
    select(orgpid, age, weight) %>% 
    left_join(cf_prob_fc_occ, by = c("orgpid", "age")) %>%
    left_join(inv_fc_prob_fc_occ_split_1, by = c("orgpid", "age")) %>%
    mutate(combined_weight = weight * cf_prob_fc_occ * inv_fc_prob_fc_occ) %>%
    select(orgpid, age, combined_weight) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  weight_split_2 <- data_split_2_heldout %>% ungroup() %>%
    select(orgpid, age, weight) %>% 
    left_join(cf_prob_fc_occ, by = c("orgpid", "age")) %>%
    left_join(inv_fc_prob_fc_occ_split_2, by = c("orgpid", "age")) %>%
    mutate(combined_weight = weight * cf_prob_fc_occ * inv_fc_prob_fc_occ) %>%
    select(orgpid, age, combined_weight) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  weight_split_3 <- data_split_3_heldout %>% ungroup() %>%
    select(orgpid, age, weight) %>% 
    left_join(cf_prob_fc_occ, by = c("orgpid", "age")) %>%
    left_join(inv_fc_prob_fc_occ_split_3, by = c("orgpid", "age")) %>%
    mutate(combined_weight = weight * cf_prob_fc_occ * inv_fc_prob_fc_occ) %>%
    select(orgpid, age, combined_weight) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  weight_split_4 <- data_split_4_heldout %>% ungroup() %>%
    select(orgpid, age, weight) %>% 
    left_join(cf_prob_fc_occ, by = c("orgpid", "age")) %>%
    left_join(inv_fc_prob_fc_occ_split_4, by = c("orgpid", "age")) %>%
    mutate(combined_weight = weight * cf_prob_fc_occ * inv_fc_prob_fc_occ) %>%
    select(orgpid, age, combined_weight) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  weight_split_5 <- data_split_5_heldout %>% ungroup() %>%
    select(orgpid, age, weight) %>% 
    left_join(cf_prob_fc_occ, by = c("orgpid", "age")) %>%
    left_join(inv_fc_prob_fc_occ_split_5, by = c("orgpid", "age")) %>%
    mutate(combined_weight = weight * cf_prob_fc_occ * inv_fc_prob_fc_occ) %>%
    select(orgpid, age, combined_weight) %>%
    group_by(orgpid, age) %>% slice(1) %>% ungroup()
  
  # join
  data_split_1_heldout <- data_split_1_heldout %>% left_join(exp_outcome_fc_occ_split_1, by = c("orgpid", "age")) %>% left_join(weight_split_1, by = c("orgpid", "age")) %>% ungroup()
  data_split_2_heldout <- data_split_2_heldout %>% left_join(exp_outcome_fc_occ_split_2, by = c("orgpid", "age")) %>% left_join(weight_split_2, by = c("orgpid", "age")) %>% ungroup()
  data_split_3_heldout <- data_split_3_heldout %>% left_join(exp_outcome_fc_occ_split_3, by = c("orgpid", "age")) %>% left_join(weight_split_3, by = c("orgpid", "age")) %>% ungroup()
  data_split_4_heldout <- data_split_4_heldout %>% left_join(exp_outcome_fc_occ_split_4, by = c("orgpid", "age")) %>% left_join(weight_split_4, by = c("orgpid", "age")) %>% ungroup()
  data_split_5_heldout <- data_split_5_heldout %>% left_join(exp_outcome_fc_occ_split_5, by = c("orgpid", "age")) %>% left_join(weight_split_5, by = c("orgpid", "age")) %>% ungroup()
  
  # calculate bias correction
  bias_correction_m_split_1 <- data_split_1_heldout %>% filter(female == 0) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_m_split_2 <- data_split_2_heldout %>% filter(female == 0) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_m_split_3 <- data_split_3_heldout %>% filter(female == 0) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_m_split_4 <- data_split_4_heldout %>% filter(female == 0) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_m_split_5 <- data_split_5_heldout %>% filter(female == 0) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_f_split_1 <- data_split_1_heldout %>% filter(female == 1) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_f_split_2 <- data_split_2_heldout %>% filter(female == 1) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_f_split_3 <- data_split_3_heldout %>% filter(female == 1) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_f_split_4 <- data_split_4_heldout %>% filter(female == 1) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_f_split_5 <- data_split_5_heldout %>% filter(female == 1) %>% ungroup() %>%
    summarise(female = female, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight)) %>% unique()
  
  bias_correction_split_1 <- rbind(bias_correction_m_split_1, bias_correction_f_split_1)
  bias_correction_split_2 <- rbind(bias_correction_m_split_2, bias_correction_f_split_2)
  bias_correction_split_3 <- rbind(bias_correction_m_split_3, bias_correction_f_split_3)
  bias_correction_split_4 <- rbind(bias_correction_m_split_4, bias_correction_f_split_4)
  bias_correction_split_5 <- rbind(bias_correction_m_split_5, bias_correction_f_split_5)
  
  # correct bias in outcome predictions
  data_split_1_heldout <- data_split_1_heldout %>% 
    left_join(bias_correction_split_1, by = "female") %>% ungroup() %>%
    mutate_at(vars(contains("exp_outcome_occ_")), ~(. - bias_correction)) %>% 
    mutate_at(vars(contains("exp_outcome_occ_")), ~(case_when(. > 1 ~ 1, . < 0 ~ 0, T ~ .)))
  
  data_split_2_heldout <- data_split_2_heldout %>% 
    left_join(bias_correction_split_2, by = "female") %>% ungroup() %>%
    mutate_at(vars(contains("exp_outcome_occ_")), ~(. - bias_correction)) %>% 
    mutate_at(vars(contains("exp_outcome_occ_")), ~(case_when(. > 1 ~ 1, . < 0 ~ 0, T ~ .)))
  
  data_split_3_heldout <- data_split_3_heldout %>% 
    left_join(bias_correction_split_3, by = "female") %>% ungroup() %>%
    mutate_at(vars(contains("exp_outcome_occ_")), ~(. - bias_correction)) %>% 
    mutate_at(vars(contains("exp_outcome_occ_")), ~(case_when(. > 1 ~ 1, . < 0 ~ 0, T ~ .)))
  
  data_split_4_heldout <- data_split_4_heldout %>% 
    left_join(bias_correction_split_4, by = "female") %>% ungroup() %>%
    mutate_at(vars(contains("exp_outcome_occ_")), ~(. - bias_correction)) %>% 
    mutate_at(vars(contains("exp_outcome_occ_")), ~(case_when(. > 1 ~ 1, . < 0 ~ 0, T ~ .)))
  
  data_split_5_heldout <- data_split_5_heldout %>% 
    left_join(bias_correction_split_5, by = "female") %>% ungroup() %>%
    mutate_at(vars(contains("exp_outcome_occ_")), ~(. - bias_correction)) %>% 
    mutate_at(vars(contains("exp_outcome_occ_")), ~(case_when(. > 1 ~ 1, . < 0 ~ 0, T ~ .)))
  
  # pre-intervention
  levels_pre_female <- data %>% filter(female == 1) %>% ungroup() %>%summarize(exp = weighted.mean(outcome, w = weight) * 100) %>% as.numeric()
  levels_pre_male <- data %>% filter(female == 0) %>% ungroup() %>%summarize(exp = weighted.mean(outcome, w = weight) * 100) %>% as.numeric()
  
  # post-intervention
  # split 1
  levels_post_female_split_1 <- data_split_1_heldout %>% filter(female == 1) %>% mutate(exp = (exp_outcome_occ_24 * counterfact_share_occ_24) + (exp_outcome_occ_33 * counterfact_share_occ_33) + (exp_outcome_occ_51 * counterfact_share_occ_51) + (exp_outcome_occ_25 * counterfact_share_occ_25) + (exp_outcome_occ_71 * counterfact_share_occ_71) + (exp_outcome_occ_21 * counterfact_share_occ_21) + (exp_outcome_occ_23 * counterfact_share_occ_23) + (exp_outcome_occ_42 * counterfact_share_occ_42) + (exp_outcome_occ_65 * counterfact_share_occ_65) + (exp_outcome_occ_61 * counterfact_share_occ_61) + (exp_outcome_occ_72 * counterfact_share_occ_72) + (exp_outcome_occ_13 * counterfact_share_occ_13) + (exp_outcome_occ_63 * counterfact_share_occ_63) + (exp_outcome_occ_34 * counterfact_share_occ_34) + (exp_outcome_occ_22 * counterfact_share_occ_22) + (exp_outcome_occ_31 * counterfact_share_occ_31) + (exp_outcome_occ_64 * counterfact_share_occ_64) + (exp_outcome_occ_73 * counterfact_share_occ_73) + (exp_outcome_occ_62 * counterfact_share_occ_62) + (exp_outcome_occ_54 * counterfact_share_occ_54) + (exp_outcome_occ_74 * counterfact_share_occ_74) + (exp_outcome_occ_11 * counterfact_share_occ_11) + (exp_outcome_occ_32 * counterfact_share_occ_32) + (exp_outcome_occ_43 * counterfact_share_occ_43) + (exp_outcome_occ_52 * counterfact_share_occ_52) + (exp_outcome_occ_53 * counterfact_share_occ_53) + (exp_outcome_occ_14 * counterfact_share_occ_14) + (exp_outcome_occ_12 * counterfact_share_occ_12) + (exp_outcome_occ_41 * counterfact_share_occ_41)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  levels_post_male_split_1 <- data_split_1_heldout %>% filter(female == 0) %>% mutate(exp = (exp_outcome_occ_24 * counterfact_share_occ_24) + (exp_outcome_occ_33 * counterfact_share_occ_33) + (exp_outcome_occ_51 * counterfact_share_occ_51) + (exp_outcome_occ_25 * counterfact_share_occ_25) + (exp_outcome_occ_71 * counterfact_share_occ_71) + (exp_outcome_occ_21 * counterfact_share_occ_21) + (exp_outcome_occ_23 * counterfact_share_occ_23) + (exp_outcome_occ_42 * counterfact_share_occ_42) + (exp_outcome_occ_65 * counterfact_share_occ_65) + (exp_outcome_occ_61 * counterfact_share_occ_61) + (exp_outcome_occ_72 * counterfact_share_occ_72) + (exp_outcome_occ_13 * counterfact_share_occ_13) + (exp_outcome_occ_63 * counterfact_share_occ_63) + (exp_outcome_occ_34 * counterfact_share_occ_34) + (exp_outcome_occ_22 * counterfact_share_occ_22) + (exp_outcome_occ_31 * counterfact_share_occ_31) + (exp_outcome_occ_64 * counterfact_share_occ_64) + (exp_outcome_occ_73 * counterfact_share_occ_73) + (exp_outcome_occ_62 * counterfact_share_occ_62) + (exp_outcome_occ_54 * counterfact_share_occ_54) + (exp_outcome_occ_74 * counterfact_share_occ_74) + (exp_outcome_occ_11 * counterfact_share_occ_11) + (exp_outcome_occ_32 * counterfact_share_occ_32) + (exp_outcome_occ_43 * counterfact_share_occ_43) + (exp_outcome_occ_52 * counterfact_share_occ_52) + (exp_outcome_occ_53 * counterfact_share_occ_53) + (exp_outcome_occ_14 * counterfact_share_occ_14) + (exp_outcome_occ_12 * counterfact_share_occ_12) + (exp_outcome_occ_41 * counterfact_share_occ_41)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  
  # split 2
  levels_post_female_split_2 <- data_split_2_heldout %>% filter(female == 1) %>% mutate(exp = (exp_outcome_occ_24 * counterfact_share_occ_24) + (exp_outcome_occ_33 * counterfact_share_occ_33) + (exp_outcome_occ_51 * counterfact_share_occ_51) + (exp_outcome_occ_25 * counterfact_share_occ_25) + (exp_outcome_occ_71 * counterfact_share_occ_71) + (exp_outcome_occ_21 * counterfact_share_occ_21) + (exp_outcome_occ_23 * counterfact_share_occ_23) + (exp_outcome_occ_42 * counterfact_share_occ_42) + (exp_outcome_occ_65 * counterfact_share_occ_65) + (exp_outcome_occ_61 * counterfact_share_occ_61) + (exp_outcome_occ_72 * counterfact_share_occ_72) + (exp_outcome_occ_13 * counterfact_share_occ_13) + (exp_outcome_occ_63 * counterfact_share_occ_63) + (exp_outcome_occ_34 * counterfact_share_occ_34) + (exp_outcome_occ_22 * counterfact_share_occ_22) + (exp_outcome_occ_31 * counterfact_share_occ_31) + (exp_outcome_occ_64 * counterfact_share_occ_64) + (exp_outcome_occ_73 * counterfact_share_occ_73) + (exp_outcome_occ_62 * counterfact_share_occ_62) + (exp_outcome_occ_54 * counterfact_share_occ_54) + (exp_outcome_occ_74 * counterfact_share_occ_74) + (exp_outcome_occ_11 * counterfact_share_occ_11) + (exp_outcome_occ_32 * counterfact_share_occ_32) + (exp_outcome_occ_43 * counterfact_share_occ_43) + (exp_outcome_occ_52 * counterfact_share_occ_52) + (exp_outcome_occ_53 * counterfact_share_occ_53) + (exp_outcome_occ_14 * counterfact_share_occ_14) + (exp_outcome_occ_12 * counterfact_share_occ_12) + (exp_outcome_occ_41 * counterfact_share_occ_41)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  levels_post_male_split_2 <- data_split_2_heldout %>% filter(female == 0) %>% mutate(exp = (exp_outcome_occ_24 * counterfact_share_occ_24) + (exp_outcome_occ_33 * counterfact_share_occ_33) + (exp_outcome_occ_51 * counterfact_share_occ_51) + (exp_outcome_occ_25 * counterfact_share_occ_25) + (exp_outcome_occ_71 * counterfact_share_occ_71) + (exp_outcome_occ_21 * counterfact_share_occ_21) + (exp_outcome_occ_23 * counterfact_share_occ_23) + (exp_outcome_occ_42 * counterfact_share_occ_42) + (exp_outcome_occ_65 * counterfact_share_occ_65) + (exp_outcome_occ_61 * counterfact_share_occ_61) + (exp_outcome_occ_72 * counterfact_share_occ_72) + (exp_outcome_occ_13 * counterfact_share_occ_13) + (exp_outcome_occ_63 * counterfact_share_occ_63) + (exp_outcome_occ_34 * counterfact_share_occ_34) + (exp_outcome_occ_22 * counterfact_share_occ_22) + (exp_outcome_occ_31 * counterfact_share_occ_31) + (exp_outcome_occ_64 * counterfact_share_occ_64) + (exp_outcome_occ_73 * counterfact_share_occ_73) + (exp_outcome_occ_62 * counterfact_share_occ_62) + (exp_outcome_occ_54 * counterfact_share_occ_54) + (exp_outcome_occ_74 * counterfact_share_occ_74) + (exp_outcome_occ_11 * counterfact_share_occ_11) + (exp_outcome_occ_32 * counterfact_share_occ_32) + (exp_outcome_occ_43 * counterfact_share_occ_43) + (exp_outcome_occ_52 * counterfact_share_occ_52) + (exp_outcome_occ_53 * counterfact_share_occ_53) + (exp_outcome_occ_14 * counterfact_share_occ_14) + (exp_outcome_occ_12 * counterfact_share_occ_12) + (exp_outcome_occ_41 * counterfact_share_occ_41)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  
  # split 3
  levels_post_female_split_3 <- data_split_3_heldout %>% filter(female == 1) %>% mutate(exp = (exp_outcome_occ_24 * counterfact_share_occ_24) + (exp_outcome_occ_33 * counterfact_share_occ_33) + (exp_outcome_occ_51 * counterfact_share_occ_51) + (exp_outcome_occ_25 * counterfact_share_occ_25) + (exp_outcome_occ_71 * counterfact_share_occ_71) + (exp_outcome_occ_21 * counterfact_share_occ_21) + (exp_outcome_occ_23 * counterfact_share_occ_23) + (exp_outcome_occ_42 * counterfact_share_occ_42) + (exp_outcome_occ_65 * counterfact_share_occ_65) + (exp_outcome_occ_61 * counterfact_share_occ_61) + (exp_outcome_occ_72 * counterfact_share_occ_72) + (exp_outcome_occ_13 * counterfact_share_occ_13) + (exp_outcome_occ_63 * counterfact_share_occ_63) + (exp_outcome_occ_34 * counterfact_share_occ_34) + (exp_outcome_occ_22 * counterfact_share_occ_22) + (exp_outcome_occ_31 * counterfact_share_occ_31) + (exp_outcome_occ_64 * counterfact_share_occ_64) + (exp_outcome_occ_73 * counterfact_share_occ_73) + (exp_outcome_occ_62 * counterfact_share_occ_62) + (exp_outcome_occ_54 * counterfact_share_occ_54) + (exp_outcome_occ_74 * counterfact_share_occ_74) + (exp_outcome_occ_11 * counterfact_share_occ_11) + (exp_outcome_occ_32 * counterfact_share_occ_32) + (exp_outcome_occ_43 * counterfact_share_occ_43) + (exp_outcome_occ_52 * counterfact_share_occ_52) + (exp_outcome_occ_53 * counterfact_share_occ_53) + (exp_outcome_occ_14 * counterfact_share_occ_14) + (exp_outcome_occ_12 * counterfact_share_occ_12) + (exp_outcome_occ_41 * counterfact_share_occ_41)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  levels_post_male_split_3 <- data_split_3_heldout %>% filter(female == 0) %>% mutate(exp = (exp_outcome_occ_24 * counterfact_share_occ_24) + (exp_outcome_occ_33 * counterfact_share_occ_33) + (exp_outcome_occ_51 * counterfact_share_occ_51) + (exp_outcome_occ_25 * counterfact_share_occ_25) + (exp_outcome_occ_71 * counterfact_share_occ_71) + (exp_outcome_occ_21 * counterfact_share_occ_21) + (exp_outcome_occ_23 * counterfact_share_occ_23) + (exp_outcome_occ_42 * counterfact_share_occ_42) + (exp_outcome_occ_65 * counterfact_share_occ_65) + (exp_outcome_occ_61 * counterfact_share_occ_61) + (exp_outcome_occ_72 * counterfact_share_occ_72) + (exp_outcome_occ_13 * counterfact_share_occ_13) + (exp_outcome_occ_63 * counterfact_share_occ_63) + (exp_outcome_occ_34 * counterfact_share_occ_34) + (exp_outcome_occ_22 * counterfact_share_occ_22) + (exp_outcome_occ_31 * counterfact_share_occ_31) + (exp_outcome_occ_64 * counterfact_share_occ_64) + (exp_outcome_occ_73 * counterfact_share_occ_73) + (exp_outcome_occ_62 * counterfact_share_occ_62) + (exp_outcome_occ_54 * counterfact_share_occ_54) + (exp_outcome_occ_74 * counterfact_share_occ_74) + (exp_outcome_occ_11 * counterfact_share_occ_11) + (exp_outcome_occ_32 * counterfact_share_occ_32) + (exp_outcome_occ_43 * counterfact_share_occ_43) + (exp_outcome_occ_52 * counterfact_share_occ_52) + (exp_outcome_occ_53 * counterfact_share_occ_53) + (exp_outcome_occ_14 * counterfact_share_occ_14) + (exp_outcome_occ_12 * counterfact_share_occ_12) + (exp_outcome_occ_41 * counterfact_share_occ_41)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  
  # split 4
  levels_post_female_split_4 <- data_split_4_heldout %>% filter(female == 1) %>% mutate(exp = (exp_outcome_occ_24 * counterfact_share_occ_24) + (exp_outcome_occ_33 * counterfact_share_occ_33) + (exp_outcome_occ_51 * counterfact_share_occ_51) + (exp_outcome_occ_25 * counterfact_share_occ_25) + (exp_outcome_occ_71 * counterfact_share_occ_71) + (exp_outcome_occ_21 * counterfact_share_occ_21) + (exp_outcome_occ_23 * counterfact_share_occ_23) + (exp_outcome_occ_42 * counterfact_share_occ_42) + (exp_outcome_occ_65 * counterfact_share_occ_65) + (exp_outcome_occ_61 * counterfact_share_occ_61) + (exp_outcome_occ_72 * counterfact_share_occ_72) + (exp_outcome_occ_13 * counterfact_share_occ_13) + (exp_outcome_occ_63 * counterfact_share_occ_63) + (exp_outcome_occ_34 * counterfact_share_occ_34) + (exp_outcome_occ_22 * counterfact_share_occ_22) + (exp_outcome_occ_31 * counterfact_share_occ_31) + (exp_outcome_occ_64 * counterfact_share_occ_64) + (exp_outcome_occ_73 * counterfact_share_occ_73) + (exp_outcome_occ_62 * counterfact_share_occ_62) + (exp_outcome_occ_54 * counterfact_share_occ_54) + (exp_outcome_occ_74 * counterfact_share_occ_74) + (exp_outcome_occ_11 * counterfact_share_occ_11) + (exp_outcome_occ_32 * counterfact_share_occ_32) + (exp_outcome_occ_43 * counterfact_share_occ_43) + (exp_outcome_occ_52 * counterfact_share_occ_52) + (exp_outcome_occ_53 * counterfact_share_occ_53) + (exp_outcome_occ_14 * counterfact_share_occ_14) + (exp_outcome_occ_12 * counterfact_share_occ_12) + (exp_outcome_occ_41 * counterfact_share_occ_41)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  levels_post_male_split_4 <- data_split_4_heldout %>% filter(female == 0) %>% mutate(exp = (exp_outcome_occ_24 * counterfact_share_occ_24) + (exp_outcome_occ_33 * counterfact_share_occ_33) + (exp_outcome_occ_51 * counterfact_share_occ_51) + (exp_outcome_occ_25 * counterfact_share_occ_25) + (exp_outcome_occ_71 * counterfact_share_occ_71) + (exp_outcome_occ_21 * counterfact_share_occ_21) + (exp_outcome_occ_23 * counterfact_share_occ_23) + (exp_outcome_occ_42 * counterfact_share_occ_42) + (exp_outcome_occ_65 * counterfact_share_occ_65) + (exp_outcome_occ_61 * counterfact_share_occ_61) + (exp_outcome_occ_72 * counterfact_share_occ_72) + (exp_outcome_occ_13 * counterfact_share_occ_13) + (exp_outcome_occ_63 * counterfact_share_occ_63) + (exp_outcome_occ_34 * counterfact_share_occ_34) + (exp_outcome_occ_22 * counterfact_share_occ_22) + (exp_outcome_occ_31 * counterfact_share_occ_31) + (exp_outcome_occ_64 * counterfact_share_occ_64) + (exp_outcome_occ_73 * counterfact_share_occ_73) + (exp_outcome_occ_62 * counterfact_share_occ_62) + (exp_outcome_occ_54 * counterfact_share_occ_54) + (exp_outcome_occ_74 * counterfact_share_occ_74) + (exp_outcome_occ_11 * counterfact_share_occ_11) + (exp_outcome_occ_32 * counterfact_share_occ_32) + (exp_outcome_occ_43 * counterfact_share_occ_43) + (exp_outcome_occ_52 * counterfact_share_occ_52) + (exp_outcome_occ_53 * counterfact_share_occ_53) + (exp_outcome_occ_14 * counterfact_share_occ_14) + (exp_outcome_occ_12 * counterfact_share_occ_12) + (exp_outcome_occ_41 * counterfact_share_occ_41)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  
  # split 5
  levels_post_female_split_5 <- data_split_5_heldout %>% filter(female == 1) %>% mutate(exp = (exp_outcome_occ_24 * counterfact_share_occ_24) + (exp_outcome_occ_33 * counterfact_share_occ_33) + (exp_outcome_occ_51 * counterfact_share_occ_51) + (exp_outcome_occ_25 * counterfact_share_occ_25) + (exp_outcome_occ_71 * counterfact_share_occ_71) + (exp_outcome_occ_21 * counterfact_share_occ_21) + (exp_outcome_occ_23 * counterfact_share_occ_23) + (exp_outcome_occ_42 * counterfact_share_occ_42) + (exp_outcome_occ_65 * counterfact_share_occ_65) + (exp_outcome_occ_61 * counterfact_share_occ_61) + (exp_outcome_occ_72 * counterfact_share_occ_72) + (exp_outcome_occ_13 * counterfact_share_occ_13) + (exp_outcome_occ_63 * counterfact_share_occ_63) + (exp_outcome_occ_34 * counterfact_share_occ_34) + (exp_outcome_occ_22 * counterfact_share_occ_22) + (exp_outcome_occ_31 * counterfact_share_occ_31) + (exp_outcome_occ_64 * counterfact_share_occ_64) + (exp_outcome_occ_73 * counterfact_share_occ_73) + (exp_outcome_occ_62 * counterfact_share_occ_62) + (exp_outcome_occ_54 * counterfact_share_occ_54) + (exp_outcome_occ_74 * counterfact_share_occ_74) + (exp_outcome_occ_11 * counterfact_share_occ_11) + (exp_outcome_occ_32 * counterfact_share_occ_32) + (exp_outcome_occ_43 * counterfact_share_occ_43) + (exp_outcome_occ_52 * counterfact_share_occ_52) + (exp_outcome_occ_53 * counterfact_share_occ_53) + (exp_outcome_occ_14 * counterfact_share_occ_14) + (exp_outcome_occ_12 * counterfact_share_occ_12) + (exp_outcome_occ_41 * counterfact_share_occ_41)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  levels_post_male_split_5 <- data_split_5_heldout %>% filter(female == 0) %>% mutate(exp = (exp_outcome_occ_24 * counterfact_share_occ_24) + (exp_outcome_occ_33 * counterfact_share_occ_33) + (exp_outcome_occ_51 * counterfact_share_occ_51) + (exp_outcome_occ_25 * counterfact_share_occ_25) + (exp_outcome_occ_71 * counterfact_share_occ_71) + (exp_outcome_occ_21 * counterfact_share_occ_21) + (exp_outcome_occ_23 * counterfact_share_occ_23) + (exp_outcome_occ_42 * counterfact_share_occ_42) + (exp_outcome_occ_65 * counterfact_share_occ_65) + (exp_outcome_occ_61 * counterfact_share_occ_61) + (exp_outcome_occ_72 * counterfact_share_occ_72) + (exp_outcome_occ_13 * counterfact_share_occ_13) + (exp_outcome_occ_63 * counterfact_share_occ_63) + (exp_outcome_occ_34 * counterfact_share_occ_34) + (exp_outcome_occ_22 * counterfact_share_occ_22) + (exp_outcome_occ_31 * counterfact_share_occ_31) + (exp_outcome_occ_64 * counterfact_share_occ_64) + (exp_outcome_occ_73 * counterfact_share_occ_73) + (exp_outcome_occ_62 * counterfact_share_occ_62) + (exp_outcome_occ_54 * counterfact_share_occ_54) + (exp_outcome_occ_74 * counterfact_share_occ_74) + (exp_outcome_occ_11 * counterfact_share_occ_11) + (exp_outcome_occ_32 * counterfact_share_occ_32) + (exp_outcome_occ_43 * counterfact_share_occ_43) + (exp_outcome_occ_52 * counterfact_share_occ_52) + (exp_outcome_occ_53 * counterfact_share_occ_53) + (exp_outcome_occ_14 * counterfact_share_occ_14) + (exp_outcome_occ_12 * counterfact_share_occ_12) + (exp_outcome_occ_41 * counterfact_share_occ_41)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  
  # average cross-fits
  levels_post_female <- (levels_post_female_split_1 + levels_post_female_split_2 + levels_post_female_split_3 + levels_post_female_split_4 + levels_post_female_split_5) / 5
  levels_post_male <- (levels_post_male_split_1 + levels_post_male_split_2 + levels_post_male_split_3 + levels_post_male_split_4 + levels_post_male_split_5) / 5
  
  # calculate estimands
  gap_pre <- levels_pre_female - levels_pre_male
  
  gap_post <- levels_post_female - levels_post_male
  
  change <- gap_post - gap_pre
  
  change_pp <- (change / gap_pre) * 100
  
  estimates_men_pre <- data.frame(estimand = "Men (before intervention)", "est" = round(levels_pre_male, 2))
  estimates_women_pre <- data.frame(estimand = "Women (before intervention)", "est" = round(levels_pre_female, 2))
  estimates_men_post <- data.frame(estimand = "Men (after intervention)", "est" = round(levels_post_male, 2))
  estimates_women_post <- data.frame(estimand = "Women (after intervention)", "est" = round(levels_post_female, 2))
  estimates_gap_pre <- data.frame(estimand = "Disparity (before intervention)", "est" = round(gap_pre, 2))
  estimates_gap_post <- data.frame(estimand = "Disparity (after intervention)", "est" = round(gap_post, 2))
  estimates_reduction <- data.frame(estimand = "Change in disparity", "est" = round(change, 2))
  estimates_reduction_pp <- data.frame(estimand = "Change in disparity (pp.)", "est" = round(change_pp, 2))
  
  estimates <- rbind(estimates_men_pre, estimates_women_pre, estimates_men_post, estimates_women_post, estimates_gap_pre, estimates_gap_post, estimates_reduction, estimates_reduction_pp)
  
  rm(list = setdiff(ls(), c("estimates", "data_final", "decompose_cond_dr_xfit", "formula_treatment", "preds_outcome", "sl_models")))
  
  gc()
  
  return(estimates)
  
}





decompose_sr_intA <- function(data, formula_treatment, preds_outcome, sl_models) {
  
  # select complete cases
  data <- data %>% na.omit()
  
  # determine counter-factual composition via intervention rule
  composition_cf <- multinom("eseg_class ~ 1", data = data %>% filter(female == 0), family = "binomial", weights = weight)
  
  composition_cf <- as.data.frame(predict(composition_cf, type = "probs"))
  
  colnames(composition_cf) <- paste0("counterfact_share_occ_", colnames(composition_cf))
  
  data <- cbind(data, composition_cf)
  
  rm(composition_cf)
  
  
  
  # treatment model
  data_male <- data %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_female <- data %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  
  # train
  model_male <- multinom(formula_treatment, data = data_male, family = "binomial", weights = weight)
  model_female <- multinom(formula_treatment, data = data_female, family = "binomial", weights = weight)
  
  # predict
  data_male <- cbind(data_male, as.data.frame(predict(model_male, newdata = data_male, type = "probs")))
  colnames(data_male)[32:38] = paste0("prop_occ_", colnames(data_male)[32:38])
  
  data_female <- cbind(data_female, as.data.frame(predict(model_female, newdata = data_female, type = "probs")))
  colnames(data_female)[32:38] = paste0("prop_occ_", colnames(data_female)[32:38])
  
  # combine
  data <- rbind(data_male, data_female)
  
  rm(data_male, data_female)
  
  data_male <- data %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_female <- data %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  
  # train
  model_male <- SuperLearner(Y = data_male$outcome, X = data_male %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_male$weight)
  model_female <- SuperLearner(Y = data_female$outcome, X = data_female %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_female$weight)
  
  # predictions
  for (i in sort(unique(data$eseg_class))) {
    
    data_male$eseg_class <- factor(i, levels = 1:7)
    data_female$eseg_class <- factor(i, levels = 1:7)
    
    data_male$prediction <- as.data.frame(predict(model_male, newdata = data_male, type = "response"))$pred
    data_female$prediction <- as.data.frame(predict(model_female, newdata = data_female, type = "response"))$pred
    
    data_male <- data_male %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_female <- data_female %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    
  }
  
  # combine
  preds <- rbind(data_male, data_female)
  
  preds <- preds %>% select(orgpid, age, contains("exp_outcome_occ_")) %>% group_by(orgpid, age) %>% slice(1)
  
  data <- data %>% left_join(preds, by = c("orgpid", "age"))
  
  rm(data_male, data_female, preds)
  
  # pre-intervention
  levels_pre_female <- data %>% filter(female == 1) %>% summarize(exp = weighted.mean(outcome, w = weight) * 100) %>% as.numeric()
  levels_pre_male <- data %>% filter(female == 0) %>% summarize(exp = weighted.mean(outcome, w = weight) * 100) %>% as.numeric()
  
  # post-intervention
  levels_post_female <- data %>% filter(female == 1) %>% mutate(exp = (exp_outcome_occ_1 * counterfact_share_occ_1) + (exp_outcome_occ_2 * counterfact_share_occ_2) + (exp_outcome_occ_3 * counterfact_share_occ_3) + (exp_outcome_occ_4 * counterfact_share_occ_4) + (exp_outcome_occ_5 * counterfact_share_occ_5) + (exp_outcome_occ_6 * counterfact_share_occ_6) + (exp_outcome_occ_7 * counterfact_share_occ_7)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  levels_post_male <- data %>% filter(female == 0) %>% mutate(exp = (exp_outcome_occ_1 * counterfact_share_occ_1) + (exp_outcome_occ_2 * counterfact_share_occ_2) + (exp_outcome_occ_3 * counterfact_share_occ_3) + (exp_outcome_occ_4 * counterfact_share_occ_4) + (exp_outcome_occ_5 * counterfact_share_occ_5) + (exp_outcome_occ_6 * counterfact_share_occ_6) + (exp_outcome_occ_7 * counterfact_share_occ_7)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  
  rm(data)
  
  # calculate estimands
  gap_pre <- levels_pre_female - levels_pre_male
  
  gap_post <- levels_post_female - levels_post_male
  
  change <- gap_post - gap_pre
  
  change_pp <- (change / gap_pre) * 100
  
  estimates_men_pre <- data.frame(estimand = "Men (before intervention)", "est" = round(levels_pre_male, 2))
  estimates_women_pre <- data.frame(estimand = "Women (before intervention)", "est" = round(levels_pre_female, 2))
  estimates_men_post <- data.frame(estimand = "Men (after intervention)", "est" = round(levels_post_male, 2))
  estimates_women_post <- data.frame(estimand = "Women (after intervention)", "est" = round(levels_post_female, 2))
  estimates_gap_pre <- data.frame(estimand = "Disparity (before intervention)", "est" = round(gap_pre, 2))
  estimates_gap_post <- data.frame(estimand = "Disparity (after intervention)", "est" = round(gap_post, 2))
  estimates_reduction <- data.frame(estimand = "Change in disparity", "est" = round(change, 2))
  estimates_reduction_pp <- data.frame(estimand = "Change in disparity (pp.)", "est" = round(change_pp, 2))
  
  estimates <- rbind(estimates_men_pre, estimates_women_pre, estimates_men_post, estimates_women_post, estimates_gap_pre, estimates_gap_post, estimates_reduction, estimates_reduction_pp)
  
  return(estimates)
  
}





decompose_sr_intB <- function(data, formula_treatment, preds_outcome, sl_models) {
  
  # select complete cases
  data <- data %>% na.omit()
  
  # determine counter-factual composition via intervention rule
  composition_cf <- multinom("eseg_class ~ 1", data = data %>% filter(female == 1), family = "binomial", weights = weight)
  
  composition_cf <- as.data.frame(predict(composition_cf, type = "probs"))
  
  colnames(composition_cf) <- paste0("counterfact_share_occ_", colnames(composition_cf))
  
  data <- cbind(data, composition_cf)
  
  rm(composition_cf)
  
  
  
  # treatment model
  data_male <- data %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_female <- data %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  
  # train
  model_male <- multinom(formula_treatment, data = data_male, family = "binomial", weights = weight)
  model_female <- multinom(formula_treatment, data = data_female, family = "binomial", weights = weight)
  
  # predict
  data_male <- cbind(data_male, as.data.frame(predict(model_male, newdata = data_male, type = "probs")))
  colnames(data_male)[32:38] = paste0("prop_occ_", colnames(data_male)[32:38])
  
  data_female <- cbind(data_female, as.data.frame(predict(model_female, newdata = data_female, type = "probs")))
  colnames(data_female)[32:38] = paste0("prop_occ_", colnames(data_female)[32:38])
  
  # combine
  data <- rbind(data_male, data_female)
  
  rm(data_male, data_female)
  
  data_male <- data %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_female <- data %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  
  # train
  model_male <- SuperLearner(Y = data_male$outcome, X = data_male %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_male$weight)
  model_female <- SuperLearner(Y = data_female$outcome, X = data_female %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_female$weight)
  
  # predictions
  for (i in sort(unique(data$eseg_class))) {
    
    data_male$eseg_class <- factor(i, levels = 1:7)
    data_female$eseg_class <- factor(i, levels = 1:7)
    
    data_male$prediction <- as.data.frame(predict(model_male, newdata = data_male, type = "response"))$pred
    data_female$prediction <- as.data.frame(predict(model_female, newdata = data_female, type = "response"))$pred
    
    data_male <- data_male %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_female <- data_female %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    
  }
  
  # combine
  preds <- rbind(data_male, data_female)
  
  preds <- preds %>% select(orgpid, age, contains("exp_outcome_occ_")) %>% group_by(orgpid, age) %>% slice(1)
  
  data <- data %>% left_join(preds, by = c("orgpid", "age"))
  
  rm(data_male, data_female, preds)
  
  # pre-intervention
  levels_pre_female <- data %>% filter(female == 1) %>% summarize(exp = weighted.mean(outcome, w = weight) * 100) %>% as.numeric()
  levels_pre_male <- data %>% filter(female == 0) %>% summarize(exp = weighted.mean(outcome, w = weight) * 100) %>% as.numeric()
  
  # post-intervention
  levels_post_female <- data %>% filter(female == 1) %>% mutate(exp = (exp_outcome_occ_1 * counterfact_share_occ_1) + (exp_outcome_occ_2 * counterfact_share_occ_2) + (exp_outcome_occ_3 * counterfact_share_occ_3) + (exp_outcome_occ_4 * counterfact_share_occ_4) + (exp_outcome_occ_5 * counterfact_share_occ_5) + (exp_outcome_occ_6 * counterfact_share_occ_6) + (exp_outcome_occ_7 * counterfact_share_occ_7)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  levels_post_male <- data %>% filter(female == 0) %>% mutate(exp = (exp_outcome_occ_1 * counterfact_share_occ_1) + (exp_outcome_occ_2 * counterfact_share_occ_2) + (exp_outcome_occ_3 * counterfact_share_occ_3) + (exp_outcome_occ_4 * counterfact_share_occ_4) + (exp_outcome_occ_5 * counterfact_share_occ_5) + (exp_outcome_occ_6 * counterfact_share_occ_6) + (exp_outcome_occ_7 * counterfact_share_occ_7)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  
  rm(data)
  
  # calculate estimands
  gap_pre <- levels_pre_female - levels_pre_male
  
  gap_post <- levels_post_female - levels_post_male
  
  change <- gap_post - gap_pre
  
  change_pp <- (change / gap_pre) * 100
  
  estimates_men_pre <- data.frame(estimand = "Men (before intervention)", "est" = round(levels_pre_male, 2))
  estimates_women_pre <- data.frame(estimand = "Women (before intervention)", "est" = round(levels_pre_female, 2))
  estimates_men_post <- data.frame(estimand = "Men (after intervention)", "est" = round(levels_post_male, 2))
  estimates_women_post <- data.frame(estimand = "Women (after intervention)", "est" = round(levels_post_female, 2))
  estimates_gap_pre <- data.frame(estimand = "Disparity (before intervention)", "est" = round(gap_pre, 2))
  estimates_gap_post <- data.frame(estimand = "Disparity (after intervention)", "est" = round(gap_post, 2))
  estimates_reduction <- data.frame(estimand = "Change in disparity", "est" = round(change, 2))
  estimates_reduction_pp <- data.frame(estimand = "Change in disparity (pp.)", "est" = round(change_pp, 2))
  
  estimates <- rbind(estimates_men_pre, estimates_women_pre, estimates_men_post, estimates_women_post, estimates_gap_pre, estimates_gap_post, estimates_reduction, estimates_reduction_pp)
  
  return(estimates)
  
}





decompose_sr_intC <- function(data, formula_treatment, preds_outcome, sl_models) {
  
  # select complete cases
  data <- data %>% na.omit()
  
  # determine counter-factual composition via intervention rule
  composition_cf <- multinom("eseg_class ~ 1", data = data, family = "binomial", weights = weight)
  
  composition_cf <- as.data.frame(predict(composition_cf, type = "probs"))
  
  colnames(composition_cf) <- paste0("counterfact_share_occ_", colnames(composition_cf))
  
  data <- cbind(data, composition_cf)
  
  rm(composition_cf)
  
  
  
  # treatment model
  data_male <- data %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_female <- data %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  
  # train
  model_male <- multinom(formula_treatment, data = data_male, family = "binomial", weights = weight)
  model_female <- multinom(formula_treatment, data = data_female, family = "binomial", weights = weight)
  
  # predict
  data_male <- cbind(data_male, as.data.frame(predict(model_male, newdata = data_male, type = "probs")))
  colnames(data_male)[32:38] = paste0("prop_occ_", colnames(data_male)[32:38])
  
  data_female <- cbind(data_female, as.data.frame(predict(model_female, newdata = data_female, type = "probs")))
  colnames(data_female)[32:38] = paste0("prop_occ_", colnames(data_female)[32:38])
  
  # combine
  data <- rbind(data_male, data_female)
  
  rm(data_male, data_female)
  
  data_male <- data %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_female <- data %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  
  # train
  model_male <- SuperLearner(Y = data_male$outcome, X = data_male %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_male$weight)
  model_female <- SuperLearner(Y = data_female$outcome, X = data_female %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_female$weight)
  
  # predictions
  for (i in sort(unique(data$eseg_class))) {
    
    data_male$eseg_class <- factor(i, levels = 1:7)
    data_female$eseg_class <- factor(i, levels = 1:7)
    
    data_male$prediction <- as.data.frame(predict(model_male, newdata = data_male, type = "response"))$pred
    data_female$prediction <- as.data.frame(predict(model_female, newdata = data_female, type = "response"))$pred
    
    data_male <- data_male %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_female <- data_female %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    
  }
  
  # combine
  preds <- rbind(data_male, data_female)
  
  preds <- preds %>% select(orgpid, age, contains("exp_outcome_occ_")) %>% group_by(orgpid, age) %>% slice(1)
  
  data <- data %>% left_join(preds, by = c("orgpid", "age"))
  
  rm(data_male, data_female, preds)
  
  # pre-intervention
  levels_pre_female <- data %>% filter(female == 1) %>% summarize(exp = weighted.mean(outcome, w = weight) * 100) %>% as.numeric()
  levels_pre_male <- data %>% filter(female == 0) %>% summarize(exp = weighted.mean(outcome, w = weight) * 100) %>% as.numeric()
  
  # post-intervention
  levels_post_female <- data %>% filter(female == 1) %>% mutate(exp = (exp_outcome_occ_1 * counterfact_share_occ_1) + (exp_outcome_occ_2 * counterfact_share_occ_2) + (exp_outcome_occ_3 * counterfact_share_occ_3) + (exp_outcome_occ_4 * counterfact_share_occ_4) + (exp_outcome_occ_5 * counterfact_share_occ_5) + (exp_outcome_occ_6 * counterfact_share_occ_6) + (exp_outcome_occ_7 * counterfact_share_occ_7)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  levels_post_male <- data %>% filter(female == 0) %>% mutate(exp = (exp_outcome_occ_1 * counterfact_share_occ_1) + (exp_outcome_occ_2 * counterfact_share_occ_2) + (exp_outcome_occ_3 * counterfact_share_occ_3) + (exp_outcome_occ_4 * counterfact_share_occ_4) + (exp_outcome_occ_5 * counterfact_share_occ_5) + (exp_outcome_occ_6 * counterfact_share_occ_6) + (exp_outcome_occ_7 * counterfact_share_occ_7)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  
  rm(data)
  
  # calculate estimands
  gap_pre <- levels_pre_female - levels_pre_male
  
  gap_post <- levels_post_female - levels_post_male
  
  change <- gap_post - gap_pre
  
  change_pp <- (change / gap_pre) * 100
  
  estimates_men_pre <- data.frame(estimand = "Men (before intervention)", "est" = round(levels_pre_male, 2))
  estimates_women_pre <- data.frame(estimand = "Women (before intervention)", "est" = round(levels_pre_female, 2))
  estimates_men_post <- data.frame(estimand = "Men (after intervention)", "est" = round(levels_post_male, 2))
  estimates_women_post <- data.frame(estimand = "Women (after intervention)", "est" = round(levels_post_female, 2))
  estimates_gap_pre <- data.frame(estimand = "Disparity (before intervention)", "est" = round(gap_pre, 2))
  estimates_gap_post <- data.frame(estimand = "Disparity (after intervention)", "est" = round(gap_post, 2))
  estimates_reduction <- data.frame(estimand = "Change in disparity", "est" = round(change, 2))
  estimates_reduction_pp <- data.frame(estimand = "Change in disparity (pp.)", "est" = round(change_pp, 2))
  
  estimates <- rbind(estimates_men_pre, estimates_women_pre, estimates_men_post, estimates_women_post, estimates_gap_pre, estimates_gap_post, estimates_reduction, estimates_reduction_pp)
  
  return(estimates)
  
}





decompose_sr_intD <- function(data, formula_treatment, preds_outcome, sl_models) {
  
  # select complete cases
  data <- data %>% na.omit()
  
  # determine counter-factual composition via intervention rule
  composition_cf <- multinom("eseg_class ~ education_level", data = data, family = "binomial", weights = weight)
  
  composition_cf <- as.data.frame(predict(composition_cf, type = "probs"))
  
  colnames(composition_cf) <- paste0("counterfact_share_occ_", colnames(composition_cf))
  
  data <- cbind(data, composition_cf)
  
  rm(composition_cf)
  
  
  
  # treatment model
  data_male <- data %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_female <- data %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  
  # train
  model_male <- multinom(formula_treatment, data = data_male, family = "binomial", weights = weight)
  model_female <- multinom(formula_treatment, data = data_female, family = "binomial", weights = weight)
  
  # predict
  data_male <- cbind(data_male, as.data.frame(predict(model_male, newdata = data_male, type = "probs")))
  colnames(data_male)[32:38] = paste0("prop_occ_", colnames(data_male)[32:38])
  
  data_female <- cbind(data_female, as.data.frame(predict(model_female, newdata = data_female, type = "probs")))
  colnames(data_female)[32:38] = paste0("prop_occ_", colnames(data_female)[32:38])
  
  # combine
  data <- rbind(data_male, data_female)
  
  rm(data_male, data_female)
  
  data_male <- data %>% filter(female == 0) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  data_female <- data %>% filter(female == 1) %>% ungroup() %>% mutate(eseg_class = factor(eseg_class, levels = 1:7))
  
  # train
  model_male <- SuperLearner(Y = data_male$outcome, X = data_male %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_male$weight)
  model_female <- SuperLearner(Y = data_female$outcome, X = data_female %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_female$weight)
  
  # predictions
  for (i in sort(unique(data$eseg_class))) {
    
    data_male$eseg_class <- factor(i, levels = 1:7)
    data_female$eseg_class <- factor(i, levels = 1:7)
    
    data_male$prediction <- as.data.frame(predict(model_male, newdata = data_male, type = "response"))$pred
    data_female$prediction <- as.data.frame(predict(model_female, newdata = data_female, type = "response"))$pred
    
    data_male <- data_male %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_female <- data_female %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    
  }
  
  # combine
  preds <- rbind(data_male, data_female)
  
  preds <- preds %>% select(orgpid, age, contains("exp_outcome_occ_")) %>% group_by(orgpid, age) %>% slice(1)
  
  data <- data %>% left_join(preds, by = c("orgpid", "age"))
  
  rm(data_male, data_female, preds)
  
  # pre-intervention
  levels_pre_female <- data %>% filter(female == 1) %>% summarize(exp = weighted.mean(outcome, w = weight) * 100) %>% as.numeric()
  levels_pre_male <- data %>% filter(female == 0) %>% summarize(exp = weighted.mean(outcome, w = weight) * 100) %>% as.numeric()
  
  # post-intervention
  levels_post_female <- data %>% filter(female == 1) %>% mutate(exp = (exp_outcome_occ_1 * counterfact_share_occ_1) + (exp_outcome_occ_2 * counterfact_share_occ_2) + (exp_outcome_occ_3 * counterfact_share_occ_3) + (exp_outcome_occ_4 * counterfact_share_occ_4) + (exp_outcome_occ_5 * counterfact_share_occ_5) + (exp_outcome_occ_6 * counterfact_share_occ_6) + (exp_outcome_occ_7 * counterfact_share_occ_7)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  levels_post_male <- data %>% filter(female == 0) %>% mutate(exp = (exp_outcome_occ_1 * counterfact_share_occ_1) + (exp_outcome_occ_2 * counterfact_share_occ_2) + (exp_outcome_occ_3 * counterfact_share_occ_3) + (exp_outcome_occ_4 * counterfact_share_occ_4) + (exp_outcome_occ_5 * counterfact_share_occ_5) + (exp_outcome_occ_6 * counterfact_share_occ_6) + (exp_outcome_occ_7 * counterfact_share_occ_7)) %>% summarize(weighted.mean(exp, w = weight) * 100) %>% as.numeric()
  
  rm(data)
  
  # calculate estimands
  gap_pre <- levels_pre_female - levels_pre_male
  
  gap_post <- levels_post_female - levels_post_male
  
  change <- gap_post - gap_pre
  
  change_pp <- (change / gap_pre) * 100
  
  estimates_men_pre <- data.frame(estimand = "Men (before intervention)", "est" = round(levels_pre_male, 2))
  estimates_women_pre <- data.frame(estimand = "Women (before intervention)", "est" = round(levels_pre_female, 2))
  estimates_men_post <- data.frame(estimand = "Men (after intervention)", "est" = round(levels_post_male, 2))
  estimates_women_post <- data.frame(estimand = "Women (after intervention)", "est" = round(levels_post_female, 2))
  estimates_gap_pre <- data.frame(estimand = "Disparity (before intervention)", "est" = round(gap_pre, 2))
  estimates_gap_post <- data.frame(estimand = "Disparity (after intervention)", "est" = round(gap_post, 2))
  estimates_reduction <- data.frame(estimand = "Change in disparity", "est" = round(change, 2))
  estimates_reduction_pp <- data.frame(estimand = "Change in disparity (pp.)", "est" = round(change_pp, 2))
  
  estimates <- rbind(estimates_men_pre, estimates_women_pre, estimates_men_post, estimates_women_post, estimates_gap_pre, estimates_gap_post, estimates_reduction, estimates_reduction_pp)
  
  return(estimates)
  
}






get_results <- function(decomp_results) {
  
  results <- outputs %>%
    group_by(estimand) %>%
    summarise(
      se = sd(est),
      est = mean(est),
      conf_lower_999 = est - 3.29 * se, conf_higher_999 = est + 3.29 * se,
      conf_lower_99 = est - 2.58 * se, conf_higher_99 = est + 2.58 * se,
      conf_lower_95 = est - 1.96 * se, conf_higher_95 = est + 1.96 * se,
      conf_lower_90 = est - 1.64 * se, conf_higher_90 = est + 1.64 * se,
      sig = case_when(
        conf_lower_999 > 0 & conf_higher_999 > 0 | conf_lower_999 < 0 & conf_higher_999 < 0  ~ "***",
        conf_lower_99 > 0 & conf_higher_99 > 0 | conf_lower_99 < 0 & conf_higher_99 < 0  ~ "***",
        conf_lower_95 > 0 & conf_higher_95 > 0 | conf_lower_95 < 0 & conf_higher_95 < 0  ~ "**",
        conf_lower_90 > 0 & conf_higher_90 > 0 | conf_lower_90 < 0 & conf_higher_90 < 0  ~ "*",
        T ~ "")) %>%
    select(estimand, est, se, sig, conf_lower_999, conf_higher_999, conf_lower_99, conf_higher_99, conf_lower_95, conf_higher_95, conf_lower_90, conf_higher_90)
  
  return(results)
  
}


