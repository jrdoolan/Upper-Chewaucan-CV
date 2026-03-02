# ============================================================
# Upper Chewaucan Watershed Restoration — CV Analysis
# EC434 | Joseph Doolan | 2024
# ============================================================
# Place survey data at: data/survey_responses.xlsx
# Figures will be saved to: figures/
# ============================================================

# ---- 0. Setup -----------------------------------------------

if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(tidyverse, ggplot2, cowplot, patchwork, readxl,
       dplyr, tidyr, stringr, modelsummary, mfx, stargazer)

mytheme <- theme_minimal() +
  theme(
    plot.title  = element_text(size = 12),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    axis.text.x  = element_text(size = 10),
    axis.text.y  = element_text(size = 10)
  )

saveplot <- function(name) {
  ggsave(paste0("figures/", name, ".png"), width = 10, height = 8, dpi = 100)
}

color_1  <- "dodgerblue4"
color_2  <- "red2"
color_2b <- "lightpink"
alpha_1  <- 1
alpha_2  <- 0.5
alpha_3  <- 0.2

# ---- 1. Load Data -------------------------------------------

survey_results <- read_excel("data/survey_responses.xlsx")

# ---- 2. Pivot to Long Format --------------------------------

survey_long <- survey_results %>%
  pivot_longer(
    cols      = starts_with("Would you be willing to pay an additional"),
    names_to  = "bid_question",
    values_to = "response"
  ) %>%
  filter(!is.na(response))

# ---- 3. Clean Bids & Outcome --------------------------------

long <- survey_long %>%
  mutate(
    bid = case_when(
      str_detect(bid_question, "10") ~ 10,
      str_detect(bid_question, "15") ~ 15,
      str_detect(bid_question, "20") ~ 20,
      str_detect(bid_question, "30") ~ 30,
      str_detect(bid_question, "45") ~ 45,
      TRUE ~ NA_real_
    ),
    accept = if_else(response == "Yes", 1L, 0L)
  ) %>%
  filter(!is.na(bid))

# ---- 4. Clean Covariates ------------------------------------

long <- long %>%
  mutate(
    income = factor(
      `What is your annual household income?`,
      ordered = TRUE,
      levels = c(
        "Less than $25,000",
        "$25,000-$50,000",
        "$50,000–$100,000",
        "$100,000–$150,000",
        "More than $150,000"
      )
    )
  )

# ---- 5. Binary Variables & Protest Zeros --------------------

protest_zero_responses <- c(
  "You cannot afford to pay more taxes",
  "You do not support the program",
  "i don't want to pay more taxes, especially when I don't know wtf this is"
)

long <- long %>%
  mutate(
    oregon_fulltime = if_else(`Do you live in Oregon full-time?` == "Yes", 1L, 0L),
    south_central_travel = if_else(
      `Have you traveled around South-Central Oregon? (ex. Lake County, Crater Lake National Park, the Upper Klamath National Wildlife Refuge, the Cascade-Siskiyou National Monument, etc.)` == "Yes",
      1L, 0L
    ),
    visit_parks  = if_else(`Do you visit Oregon rivers, lakes, or parks?` == "Yes", 1L, 0L),
    protest_zero = if_else(
      `Did you reply \u201cNo\u201d because\u2026` %in% protest_zero_responses,
      1L, 0L
    )
  )

# ---- 6. Build Model Dataset ---------------------------------

model_data <- long %>%
  filter(!is.na(accept), !is.na(bid), !is.na(income)) %>%
  dplyr::select(accept, bid, income, oregon_fulltime,
                south_central_travel, protest_zero, visit_parks) %>%
  mutate(
    income3bin = case_when(
      income %in% c("Less than $25,000", "$25,000-$50,000")       ~ "Low",
      income %in% c("$50,000–$100,000", "$100,000–$150,000")      ~ "Middle",
      income == "More than $150,000"                               ~ "High",
      TRUE ~ NA_character_
    ),
    income3bin = factor(income3bin, levels = c("Low", "Middle", "High"))
  )

glimpse(model_data)

# ---- 7. Logit Regression (Full Sample) ----------------------

logit_cv <- glm(
  accept ~ bid + income3bin + oregon_fulltime + south_central_travel + visit_parks,
  data   = model_data,
  family = binomial
)

summary(logit_cv)

# ---- 8. Coefficient Labels ----------------------------------

coef_labels <- c(
  "(Intercept)"          = "Constant",
  "bid"                  = "Bid ($)",
  "income3binMiddle"     = "Income: Middle ($50k–$150k)",
  "income3binHigh"       = "Income: High (>$150k)",
  "oregon_fulltime"      = "Lives in Oregon full-time",
  "south_central_travel" = "Traveled in South-Central Oregon",
  "visit_parks"          = "Visits Rivers Lakes or Parks"
)

# ---- 9. Regression Table ------------------------------------

modelsummary(
  logit_cv,
  coef_map  = coef_labels,
  statistic = "({p.value})",
  estimate  = "{estimate}",
  title     = "Effects from Logit Model (glm)",
  output    = "markdown"
)

# ---- 10. Marginal Effects (Full Sample) ---------------------

logit_cv_margin <- logitmfx(
  accept ~ bid + income3bin + oregon_fulltime + south_central_travel + visit_parks,
  data = model_data
)

mfx_table <- as.data.frame(logit_cv_margin$mfxest)
mfx_table$Variable <- rownames(mfx_table)
mfx_table <- mfx_table[, c("Variable", "dF/dx", "Std. Err.", "z", "P>|z|")]

tidy_mfx <- data.frame(
  term      = mfx_table$Variable,
  estimate  = mfx_table$`dF/dx`,
  std.error = mfx_table$`Std. Err.`,
  statistic = mfx_table$z,
  p.value   = mfx_table$`P>|z|`
)

logit_mfx_ms <- list(tidy = tidy_mfx, glance = data.frame(N = nrow(model_data)))
class(logit_mfx_ms) <- "modelsummary_list"

modelsummary(
  logit_mfx_ms,
  estimate  = "{estimate}",
  statistic = "({std.error})",
  coef_map  = coef_labels,
  gof_omit  = ".*",
  title     = "Marginal Effects from Logit Model (logitmfx)",
  output    = "markdown"
)

# ---- 11. Mean WTP Calculation (Full Sample) -----------------

logit_coef <- coef(logit_cv)
X_mean     <- colMeans(model.matrix(logit_cv))

beta0 <- logit_coef["(Intercept)"]
beta1 <- logit_coef["bid"]

control_names <- setdiff(names(logit_coef), c("(Intercept)", "bid"))
control_coefs <- logit_coef[control_names]
bar           <- X_mean[control_names]

wtp_mean <- -(beta0 + sum(control_coefs * bar)) / beta1
cat("Mean WTP (full sample): $", round(wtp_mean, 2), "\n")

# ---- 12. Protest Zero Adjustment ----------------------------

logit_cv_protest <- glm(
  accept ~ bid + income3bin + oregon_fulltime + south_central_travel + visit_parks,
  data   = model_data %>% filter(protest_zero == 0),
  family = binomial
)

modelsummary(
  logit_cv_protest,
  coef_map  = coef_labels,
  statistic = "({p.value})",
  estimate  = "{estimate}",
  title     = "Effects from Logit Model (glm) — Protest Zeros Removed",
  output    = "markdown"
)

# Marginal effects — protest zeros removed
logit_cv_margin_p <- logitmfx(
  accept ~ bid + income3bin + oregon_fulltime + south_central_travel + visit_parks,
  data = model_data %>% filter(protest_zero == 0)
)

mfx_table_p <- as.data.frame(logit_cv_margin_p$mfxest)
mfx_table_p$Variable <- rownames(mfx_table_p)
mfx_table_p <- mfx_table_p[, c("Variable", "dF/dx", "Std. Err.", "z", "P>|z|")]

tidy_mfx_p <- data.frame(
  term      = mfx_table_p$Variable,
  estimate  = mfx_table_p$`dF/dx`,
  std.error = mfx_table_p$`Std. Err.`,
  statistic = mfx_table_p$z,
  p.value   = mfx_table_p$`P>|z|`
)

logit_mfx_ms_p <- list(tidy = tidy_mfx_p, glance = data.frame(N = nrow(model_data)))
class(logit_mfx_ms_p) <- "modelsummary_list"

modelsummary(
  logit_mfx_ms_p,
  estimate  = "{estimate}",
  statistic = "({std.error})",
  coef_map  = coef_labels,
  gof_omit  = ".*",
  title     = "Marginal Effects from Logit Model (logitmfx) — Protest Zeros Removed",
  output    = "markdown"
)

# WTP — protest zeros removed
logit_coef_protest <- coef(logit_cv_protest)
X_mean_protest     <- colMeans(model.matrix(logit_cv_protest))

beta0_protest <- logit_coef_protest["(Intercept)"]
beta1_protest <- logit_coef_protest["bid"]

control_names_protest <- setdiff(names(logit_coef_protest), c("(Intercept)", "bid"))
control_coefs_protest <- logit_coef_protest[control_names_protest]
bar_protest           <- X_mean_protest[control_names_protest]

wtp_mean_protest <- -(beta0_protest + sum(control_coefs_protest * bar_protest)) / beta1_protest
cat("Mean WTP (protest zeros removed): $", round(wtp_mean_protest, 2), "\n")

# ---- 13. Income x Accept Crosstab --------------------------
with(model_data, table(income, accept))
