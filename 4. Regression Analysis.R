# ============================================================
# 03_lda_to_regression_and_inference_final.R
# Full pipeline: topic grouping → FE regressions → inference →
# marginal effects plot → exports.
# Revised grouping: Final 8 mutually exclusive themes (K=15)
# ============================================================

# Libraries
library(tidyverse)
library(tidytext)
library(topicmodels)
library(fixest)
library(readxl)
library(writexl)
library(modelsummary)
library(ggplot2)
library(marginaleffects)
library(broom)

# ---------------------------
# 0. Read independent variables
# ---------------------------
indvars <- read_excel("Independent Variables.xlsx") %>%
  rename_with(~ tolower(.x)) %>%
  mutate(
    agency = as.character(agency),
    year = as.integer(year)
  )

# ---------------------------
# 1. Verify lda_model and its K
# ---------------------------
if (!exists("lda_model")) stop("lda_model not found in environment. Load or fit an LDA model first.")
k_model <- tryCatch(lda_model@k, error = function(e) NA_integer_)
if (is.na(k_model)) stop("Cannot read lda_model@k; ensure lda_model is a topicmodels::LDA object.")
message("LDA model detected with K = ", k_model)

# Set expected K 
expected_k <- 15L
if (k_model != expected_k) {
  stop("LDA model K (", k_model, ") does not equal expected K = ", expected_k,
       ". Refit with k = ", expected_k, " or update topic_groups accordingly.")
}

# ---------------------------
# 2. Extract gamma and pivot wide
# ---------------------------
gamma_raw <- tidy(lda_model, matrix = "gamma")   # document, topic, gamma

gamma_wide <- gamma_raw %>%
  pivot_wider(
    names_from = topic,
    values_from = gamma,
    names_prefix = "topic_"
  )

# ---------------------------
# 3. Define revised topic groups (FINAL CLEANED MODEL K = 15)
# ---------------------------
topic_groups_revised <- list(
  lgu_share = c(11),                                  # lgus, local, assistance
  procurement_share = c(4, 5),                        # procurement, delivery, items, contract, equipment, facilities
  accounting_and_cash_share = c(2, 7, 8),             # account, ppe, books, inventory, gam, bank, cash
  aging_balances_share = c(3, 9),                     # million, billion, years, due, prior, transfers, advances
  programs_and_policies_share = c(6, 12),             # program, beneficiaries, policies, plans, utilization
  project_impl_share = c(1),                          # contractors, infrastructure, construction, completed
  asset_mgmt_share = c(13),                           # transportation, motor, vehicles
  performance_and_resources_share = c(10, 14, 15)     # physical, target, mfo, percentage, resources, personnel
)

sum_topics <- function(df, topic_indices) {
  cols <- paste0("topic_", topic_indices)
  missing_cols <- setdiff(cols, names(df))
  if (length(missing_cols) > 0) df[missing_cols] <- 0
  rowSums(df[cols], na.rm = TRUE)
}

# ---------------------------
# 4. Create grouped shares (using revised topic groups)
# ---------------------------
gamma_grouped <- gamma_wide %>%
  mutate(
    lgu_share = sum_topics(., topic_groups_revised$lgu_share),
    procurement_share = sum_topics(., topic_groups_revised$procurement_share),
    accounting_and_cash_share = sum_topics(., topic_groups_revised$accounting_and_cash_share),
    aging_balances_share = sum_topics(., topic_groups_revised$aging_balances_share),
    programs_and_policies_share = sum_topics(., topic_groups_revised$programs_and_policies_share),
    project_impl_share = sum_topics(., topic_groups_revised$project_impl_share),
    asset_mgmt_share = sum_topics(., topic_groups_revised$asset_mgmt_share),
    performance_and_resources_share = sum_topics(., topic_groups_revised$performance_and_resources_share)
  )

# Quick sanity: grouped shares should be between 0 and 1
share_cols_temp <- grep("_share$", names(gamma_grouped), value = TRUE)
if (length(share_cols_temp) > 0) {
  max_vals <- sapply(gamma_grouped[share_cols_temp], function(x) max(x, na.rm = TRUE))
  if (any(max_vals > 1 + 1e-8)) {
    warning("One or more grouped shares exceed 1. Check topic grouping or gamma extraction.")
  }
}

# ---------------------------
# 5. Align metadata identifier and merge
# ---------------------------
if (!exists("metadata")) stop("metadata object not found in environment. Load metadata before running this script.")

# Normalize metadata id column
if ("doc_id" %in% names(metadata)) {
  metadata <- metadata %>% rename(document = doc_id)
}

# Decide join column and diagnostics
if ("document" %in% names(metadata)) {
  join_col <- "document"
  missing_in_meta <- sum(!gamma_grouped$document %in% metadata$document)
  message("Documents missing in metadata (document): ", missing_in_meta)
} else if ("filename" %in% names(metadata) && all(gamma_grouped$document %in% metadata$filename)) {
  gamma_grouped <- gamma_grouped %>% rename(filename = document)
  join_col <- "filename"
  missing_in_meta <- sum(!gamma_grouped$filename %in% metadata$filename)
  message("Documents missing in metadata (filename): ", missing_in_meta)
} else {
  stop("Metadata join column mismatch: ensure metadata has doc_id or filename matching gamma IDs.")
}

n_before <- nrow(gamma_grouped)
regression_ready <- gamma_grouped %>%
  left_join(metadata, by = join_col)
n_after <- nrow(regression_ready)
message("Rows before join: ", n_before, " after join: ", n_after)

# Report missing budgets/personnel after join
message("Rows with missing budget: ", sum(is.na(regression_ready$budget)))
message("Rows with missing personnel: ", sum(is.na(regression_ready$personnel)))

# ---------------------------
# 6. Merge independent variables by agency + year (if not already present)
# ---------------------------
regression_ready <- regression_ready %>%
  mutate(agency = as.character(agency), year = as.integer(year)) %>%
  left_join(indvars, by = c("agency", "year"))

message("After merging indvars - missing budget: ", sum(is.na(regression_ready$budget)))
message("After merging indvars - missing personnel: ", sum(is.na(regression_ready$personnel)))

# ---------------------------
# 7. Data checks and transformations (Log–Log Setup)
# ---------------------------
if (all(is.na(regression_ready$budget))) stop("All budget values are missing. Check Independent Variables.xlsx and join keys.")
if (all(is.na(regression_ready$personnel))) stop("All personnel values are missing. Check Independent Variables.xlsx and join keys.")

# Winsorize log(budget) at 1% and 99% on the log scale
qlog <- quantile(ifelse(regression_ready$budget > 0, log(regression_ready$budget), NA_real_), probs = c(0.01, 0.99), na.rm = TRUE)

regression_ready <- regression_ready %>%
  mutate(
    log_budget = if_else(budget > 0, log(budget), NA_real_),
    log_budget_w = if_else(is.na(log_budget), NA_real_, pmin(pmax(log_budget, qlog[1]), qlog[2]))
  )

# Handle personnel zeros: use log(personnel + 1) if zeros exist, otherwise log(personnel)
zero_personnel <- sum(regression_ready$personnel == 0, na.rm = TRUE)
if (zero_personnel > 0) {
  message("Found ", zero_personnel, " rows with personnel == 0. Using log(personnel + 1) to retain rows.")
  regression_ready <- regression_ready %>%
    mutate(log_personnel = log(personnel + 1))
} else {
  regression_ready <- regression_ready %>%
    mutate(log_personnel = if_else(personnel > 0, log(personnel), NA_real_))
}

# Quick summaries
message("Summary of log_budget_w:")
print(summary(regression_ready$log_budget_w))
message("Summary of log_personnel:")
print(summary(regression_ready$log_personnel))

# ---------------------------
# 8. Check grouped shares distribution in regression_ready
# ---------------------------
share_cols <- grep("_share$", names(regression_ready), value = TRUE)
if (length(share_cols) > 0) {
  share_stats <- regression_ready %>%
    summarise(across(all_of(share_cols), list(min = ~min(.x, na.rm=TRUE), mean = ~mean(.x, na.rm=TRUE), max = ~max(.x, na.rm=TRUE))))
  print(share_stats)
}

# ---------------------------
# 9. Estimate fixed-effects models with fixest
# ---------------------------
themes <- c(
  "lgu_share",
  "procurement_share",
  "accounting_and_cash_share",
  "aging_balances_share",
  "programs_and_policies_share",
  "project_impl_share",
  "asset_mgmt_share",
  "performance_and_resources_share"
)

models <- list()
for (th in themes) {
  if (!th %in% names(regression_ready)) {
    warning("Theme ", th, " not found in regression_ready; skipping.")
    next
  }
  f <- as.formula(paste0(th, " ~ log_budget_w + log_personnel | agency + year"))
  models[[th]] <- feols(f, data = regression_ready, cluster = ~agency)
}

# Print clustered summaries to console
invisible(lapply(models, function(m) {
  if (!is.null(m)) print(summary(m, se = "cluster"))
}))

# ---------------------------
# 10. Marginal Effects with 95% CI (Inference) - robust block
# ---------------------------
if (!"procurement_share" %in% names(regression_ready)) stop("procurement_share not found in regression_ready; cannot produce marginal effects plot.")

# Fit or reuse ols_inference_model
if (!exists("ols_inference_model") || !inherits(ols_inference_model, "lm")) {
  message("Fitting ols_inference_model for inference (lm with factor FEs).")
  ols_inference_model <- lm(
    procurement_share ~ log_budget_w + log_personnel + factor(agency) + factor(year),
    data = regression_ready
  )
}

# Representative conditioning value for log_personnel
mean_log_personnel <- mean(regression_ready$log_personnel, na.rm = TRUE)
if (is.na(mean_log_personnel)) stop("log_personnel is all NA. Check personnel values and transformation.")

# Build newdata grid for prediction
newdata_grid <- tibble(
  log_budget_w = seq(min(regression_ready$log_budget_w, na.rm = TRUE),
                     max(regression_ready$log_budget_w, na.rm = TRUE),
                     length.out = 200),
  log_personnel = mean_log_personnel,
  agency = regression_ready$agency[1],
  year = regression_ready$year[1]
)

if (nrow(newdata_grid) == 0) stop("newdata_grid has zero rows; cannot predict. Check log_budget_w range.")

# Predict with lm and get confidence intervals
pred_mat <- predict(ols_inference_model, newdata = newdata_grid, interval = "confidence", level = 0.95)
if (!is.matrix(pred_mat) || ncol(pred_mat) < 3) stop("predict returned unexpected result; cannot build CI.")

newdata_grid <- newdata_grid %>%
  mutate(fit = pred_mat[, "fit"], lwr = pred_mat[, "lwr"], upr = pred_mat[, "upr"])

# Plot and save
p <- ggplot(newdata_grid, aes(x = log_budget_w, y = fit)) +
  geom_line(color = "#2c7fb8", linewidth = 1) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.15, fill = "#2c7fb8") +
  labs(
    x = "Log Budget (winsorized)",
    y = "Predicted Procurement Share",
    title = "Predicted Procurement Share (95% CI)",
    subtitle = paste0("Conditioned on mean log_personnel = ", round(mean_log_personnel, 3),
                      " | Reference agency: ", regression_ready$agency[1],
                      " | Reference year: ", regression_ready$year[1])
  ) +
  theme_minimal()

ggsave("budget_procurement_marginal_effect.png", plot = p, width = 8, height = 5, dpi = 300)
message("Saved budget_procurement_marginal_effect.png")

# ---------------------------
# 11. Robustness: fractional logit (quasi-binomial) for procurement_share
# ---------------------------
frac_model <- tryCatch(
  glm(procurement_share ~ log_budget_w + log_personnel + factor(agency) + factor(year),
      data = regression_ready, family = quasibinomial(link = "logit")),
  error = function(e) { message("Fractional logit failed: ", e$message); NULL }
)
if (!is.null(frac_model)) summary(frac_model)

# ---------------------------
# 12. Export regression table and data
# ---------------------------
model_list <- models[themes]
model_list <- model_list[!sapply(model_list, is.null)]
if (length(model_list) > 0) {
  modelsummary(model_list, output = "regression_table.docx")
} else {
  message("No models to export to regression_table.docx")
}

agency_means <- regression_ready %>%
  group_by(agency) %>%
  summarise(across(ends_with("_share"), mean, na.rm = TRUE)) %>%
  arrange(desc(procurement_share))

regression_ready_export <- regression_ready %>%
  mutate(across(where(is.list), ~ map_chr(., ~ paste(.x, collapse = "; ")))) %>%
  mutate(across(where(is.factor), as.character))

write_xlsx(
  list(
    regression_ready = regression_ready_export,
    agency_means = agency_means
  ),
  path = "lda_regression_final_outputs.xlsx"
)

saveRDS(models, file = "lda_models_list.rds")

# ---------------------------
# 13. Final console summary
# ---------------------------
message("Script finished. Outputs created (if steps succeeded):")
message("- regression_table.docx")
message("- budget_procurement_marginal_effect.png")
message("- lda_regression_final_outputs.xlsx")
message("- lda_models_list.rds")

# ---------------------------
# Additional robustness checks (repeatable)
# ---------------------------

# 1. Fractional logit robustness (procurement)
frac_proc <- glm(procurement_share ~ log_budget_w + log_personnel + factor(agency) + factor(year),
                 data = regression_ready, family = quasibinomial(link = "logit"))
summary(frac_proc)

# 2. Delta method for 10% budget effect with clustered SE (feols)
if ("procurement_share" %in% names(models)) {
  m <- models[["procurement_share"]]
  if (!is.null(m)) {
    beta <- coef(m)["log_budget_w"]
    V <- vcov(m, cluster = "agency")
    delta_log <- log(1.10)
    effect <- beta * delta_log
    se_effect <- sqrt((delta_log^2) * V["log_budget_w","log_budget_w"])
    print(c(effect = effect, se = se_effect, lwr = effect - 1.96*se_effect, upr = effect + 1.96*se_effect))
  }
}

# 3. Leave-one-agency-out sensitivity (procurement)
agencies <- unique(regression_ready$agency)
loo <- map_dfr(agencies, function(a) {
  d2 <- filter(regression_ready, agency != a)
  m2 <- feols(procurement_share ~ log_budget_w + log_personnel | agency + year, data = d2, cluster = ~agency)
  tibble(agency_left_out = a, beta = coef(m2)["log_budget_w"])
})
print(loo %>% arrange(beta))
