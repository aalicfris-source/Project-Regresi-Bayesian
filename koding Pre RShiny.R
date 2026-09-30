# ==============================================================================
# PRECOMPUTE.R - Pelatihan Model & Pre-computation Data
# ==============================================================================
library(readxl)
library(psych)
library(lmtest)
library(dplyr)
library(tidyr)
library(ggplot2)
library(brms)
library(car) 
library(Metrics)
library(randomForest)
library(e1071)

SEED <- 2024
set.seed(SEED)
options(mc.cores = parallel::detectCores())

# ------------------------------------------------------------------------------
# LANGKAH 1: BACA & OLAH DATA
# ------------------------------------------------------------------------------
kapal_raw <- read_excel(file.choose())
kapal <- kapal_raw[, 1:7]
names(kapal) <- c("LCB", "PC", "LDR", "BDR", "LBR", "Fn", "r")
kapal <- kapal %>% mutate(across(everything(), as.numeric)) %>% drop_na()

# Ringkasan Statistik & Tes Diagnostik OLS Awal
stat_desc <- describe(kapal)[, c("mean", "sd", "median", "min", "max", "skew", "kurtosis")]
spearman_cor <- cor(kapal, use = "complete.obs", method = "spearman")

model_ols_init <- lm(r ~ LCB + PC + LDR + BDR + LBR + Fn, data = kapal)
vif_init <- car::vif(model_ols_init)
bp_test <- lmtest::bptest(model_ols_init)

# ------------------------------------------------------------------------------
# LANGKAH 2: EDA & TRANSFORMASI DATA
# ------------------------------------------------------------------------------
kapal$log1p_r <- log1p(kapal$r)
kapal$Fn2     <- kapal$Fn^2

skew_r    <- e1071::skewness(kapal$r)
skew_logr <- e1071::skewness(kapal$log1p_r)

cor_fn_r    <- cor(kapal$Fn, kapal$r)
cor_fn_logr <- cor(kapal$Fn, kapal$log1p_r)
cor_fn2_logr<- cor(kapal$Fn2, kapal$log1p_r)

kor_r    <- sapply(kapal[c("LCB","PC","LDR","BDR","LBR")], function(x) cor(x, kapal$r))
kor_logr <- sapply(kapal[c("LCB","PC","LDR","BDR","LBR")], function(x) cor(x, kapal$log1p_r))

# Standardisasi Prediktor
kapal_std <- kapal %>%
  mutate(across(c(LCB, PC, LDR, BDR, LBR), ~ as.numeric(scale(.)))) 

formula_model <- log1p_r ~ poly(Fn, 2) + LCB + PC + LDR + BDR + LBR

# Split Train (80%) - Test (20%)
set.seed(SEED)
idx_train <- sample(seq_len(nrow(kapal_std)), size = floor(0.8 * nrow(kapal_std)))
train     <- kapal_std[idx_train, ]
test      <- kapal_std[-idx_train, ]

# ------------------------------------------------------------------------------
# LANGKAH 3: FIT MODEL BAYESIAN
# ------------------------------------------------------------------------------
prior_utama <- c(
  prior(normal(0, 2.5), class = "b"),
  prior(normal(0, 5),   class = "Intercept"),
  prior(exponential(1), class = "sigma")
)

fit_bayes <- brm(
  formula_model, data = train, family = gaussian(),
  prior = prior_utama,
  chains = 4, iter = 4000, warmup = 1000, seed = SEED,
  control = list(adapt_delta = 0.95), refresh = 0
)

# ------------------------------------------------------------------------------
# LANGKAH 4, 5, 6: BASELINE OLS, RANDOM FOREST, & EVALUASI
# ------------------------------------------------------------------------------
model_ols <- lm(formula_model, data = train)

set.seed(SEED)
model_rf_tr <- randomForest(r ~ Fn + LCB + PC + LDR + BDR + LBR,
                            data = kapal[idx_train, ], ntree = 500,
                            importance = TRUE)

pred_ols   <- expm1(predict(model_ols, newdata = test))
pred_bayes <- expm1(colMeans(posterior_predict(fit_bayes, newdata = test)))
pred_rf    <- predict(model_rf_tr, newdata = kapal[-idx_train, ])
aktual     <- kapal$r[-idx_train]

mape_val <- function(a, p) mean(abs((a - p) / a)) * 100
r2_val   <- function(a, p) 1 - sum((a - p)^2) / sum((a - mean(a))^2)

evaluasi_test <- data.frame(
  Model = c("OLS (baseline)", "Regresi Bayesian", "Random Forest (benchmark)"),
  RMSE  = c(rmse(aktual, pred_ols), rmse(aktual, pred_bayes), rmse(aktual, pred_rf)),
  MAE   = c(mae(aktual, pred_ols),  mae(aktual, pred_bayes),  mae(aktual, pred_rf)),
  MAPE  = c(mape_val(aktual, pred_ols), mape_val(aktual, pred_bayes), mape_val(aktual, pred_rf)),
  R2    = c(r2_val(aktual, pred_ols), r2_val(aktual, pred_bayes), r2_val(aktual, pred_rf))
)

# ------------------------------------------------------------------------------
# SIMPAN DATA UNTUK SHINY (MENAMBAHKAN kapal_raw AGAR TIDAK NULL)
# ------------------------------------------------------------------------------
precomputed_data <- list(
  kapal_raw     = kapal_raw, # <-- DITAMBAHKAN AGAR TIDAK ERROR
  kapal         = kapal,
  kapal_std     = kapal_std,
  train         = train,
  test          = test,
  stat_desc     = stat_desc,
  spearman_cor  = spearman_cor,
  vif_init      = vif_init,
  bp_test       = bp_test,
  skew_r        = skew_r,
  skew_logr     = skew_logr,
  cor_fn_r      = cor_fn_r,
  cor_fn_logr   = cor_fn_logr,
  cor_fn2_logr  = cor_fn2_logr,
  kor_r         = kor_r,
  kor_logr      = kor_logr,
  fit_bayes     = fit_bayes,
  model_ols     = model_ols,
  model_rf_tr   = model_rf_tr,
  evaluasi_test = evaluasi_test,
  pred_ols      = pred_ols,
  pred_bayes    = pred_bayes,
  pred_rf       = pred_rf,
  aktual        = aktual
)

saveRDS(precomputed_data, "precomputed_bayes_model.rds")
cat("Pre-computation selesai! File 'precomputed_bayes_model.rds' berhasil di-update.\n")