## =====================================================================
## 01_PRE_KOMPUTASI.R (Versi Lengkap & Komprehensif)
## =====================================================================

DATA_PATH  <- NULL    # NULL = pilih file via jendela dialog (file.choose())
OUT_DIR    <- "hasil_pre"
SEED       <- 1234
N_CHAIN    <- 4
N_ITER     <- 4000
N_WARMUP   <- 2000
N_CORES    <- 4

dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(OUT_DIR, "model"), showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(readxl); library(psych); library(lmtest); library(dplyr)
  library(tidyr); library(ggplot2); library(brms); library(loo); library(car)
  library(moments); library(Metrics); library(posterior); library(randomForest)
})

set.seed(SEED)

if (is.null(DATA_PATH)) DATA_PATH <- file.choose()
kapal_raw <- read_excel(DATA_PATH)
kapal <- kapal_raw[, 1:7]
names(kapal) <- c("LCB", "PC", "LDR", "BDR", "LBR", "Fn", "r")
kapal <- kapal %>% mutate(across(everything(), as.numeric))
kapal <- kapal %>% mutate(ID_kapal = interaction(LCB, PC, LDR, BDR, LBR, drop = TRUE))

pred_vars <- c("LCB", "PC", "LDR", "BDR", "LBR", "Fn")
all_vars  <- c(pred_vars, "r")

n_obs     <- nrow(kapal)
n_desain  <- nlevels(kapal$ID_kapal)

# 1. EDA, Statistik, Missing Value, & Outlier
sd_raw    <- describe(kapal %>% dplyr::select(all_of(all_vars)))
stat_desc <- data.frame(Variabel = rownames(sd_raw), round(sd_raw[, c("n", "mean", "sd", "median", "min", "max", "skew", "kurtosis")], 3), row.names = NULL)
spearman  <- round(cor(kapal[, all_vars], use = "complete.obs", method = "spearman"), 3)

n_missing   <- colSums(is.na(kapal[, all_vars]))
missing_tab <- data.frame(Variabel = names(n_missing), Missing_Values = n_missing, row.names = NULL)

outlier_tab <- do.call(rbind, lapply(all_vars, function(v) {
  x   <- kapal[[v]]
  q1  <- quantile(x, 0.25, na.rm = TRUE)
  q3  <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  n_out <- sum(x < (q1 - 1.5 * iqr) | x > (q3 + 1.5 * iqr), na.rm = TRUE)
  data.frame(Variabel = v, Jumlah_Outlier = n_out, Persen = round(100 * n_out / length(x), 2))
}))

# Multikolinearitas (VIF) & Heteroskedastisitas (Breusch-Pagan)
model_uji <- lm(r ~ LCB + PC + LDR + BDR + LBR + Fn, data = kapal)
vif_all   <- data.frame(Variabel = names(vif(model_uji)), VIF = round(as.numeric(vif(model_uji)), 2))
bp_all    <- bptest(model_uji)
bp_all_df <- data.frame(Uji = "Breusch-Pagan (Heteroskedastisitas)", Statistik = round(unname(bp_all$statistic), 4), df = unname(bp_all$parameter), p_value = round(bp_all$p.value, 4))

# Pembuatan Plot EDA (Histogram, Boxplot, Scatterplot)
p_hist_data <- kapal %>% pivot_longer(all_of(all_vars), names_to = "Variabel", values_to = "Nilai")
plot_hist <- ggplot(p_hist_data, aes(x = Nilai)) +
  geom_histogram(aes(y = after_stat(density)), bins = 20, fill = "#0dcaf0", color = "#1b263b", alpha = 0.7) +
  geom_density(color = "#ff6b6b", linewidth = 1) +
  facet_wrap(~Variabel, scales = "free") +
  theme_minimal(base_size = 11) +
  theme(panel.background = element_rect(fill = "#1b263b", color = NA), plot.background = element_rect(fill = "#1b263b", color = NA), text = element_text(color = "#e0e1dd"), axis.text = element_text(color = "#e0e1dd"), strip.text = element_text(color = "#0dcaf0", face = "bold"))

plot_box <- ggplot(p_hist_data, aes(x = Variabel, y = Nilai)) +
  geom_boxplot(fill = "#20c997", color = "#1b263b", alpha = 0.8, outlier.color = "#ff6b6b") +
  facet_wrap(~Variabel, scales = "free") +
  theme_minimal(base_size = 11) +
  theme(panel.background = element_rect(fill = "#1b263b", color = NA), plot.background = element_rect(fill = "#1b263b", color = NA), text = element_text(color = "#e0e1dd"), axis.text = element_text(color = "#e0e1dd"), strip.text = element_text(color = "#0dcaf0", face = "bold"))

p_scatter_data <- kapal %>% pivot_longer(all_of(pred_vars), names_to = "Prediktor", values_to = "Nilai_Prediktor")
plot_scatter <- ggplot(p_scatter_data, aes(x = Nilai_Prediktor, y = r)) +
  geom_point(color = "#0dcaf0", alpha = 0.6, size = 1.5) +
  geom_smooth(method = "loess", se = FALSE, color = "#ff6b6b", linewidth = 1) +
  facet_wrap(~Prediktor, scales = "free") +
  theme_minimal(base_size = 11) +
  theme(panel.background = element_rect(fill = "#1b263b", color = NA), plot.background = element_rect(fill = "#1b263b", color = NA), text = element_text(color = "#e0e1dd"), axis.text = element_text(color = "#e0e1dd"), strip.text = element_text(color = "#0dcaf0", face = "bold")) +
  labs(title = "Scatterplot Hambatan Sisa (r) terhadap Prediktor", x = "Nilai Prediktor", y = "r")

# Standardisasi & Split Train-Test
skala <- data.frame(Variabel = pred_vars, mean = sapply(kapal[pred_vars], mean), sd = sapply(kapal[pred_vars], sd), min = sapply(kapal[pred_vars], min), max = sapply(kapal[pred_vars], max), row.names = NULL)
kapal_std <- kapal %>% mutate(across(all_of(pred_vars), ~ as.numeric(scale(.))))
formula_model <- r ~ Fn + LCB + PC + LDR + BDR + LBR

set.seed(SEED)
ID_kapal_unik <- unique(kapal_std$ID_kapal)
Data_Train_ID <- sample(ID_kapal_unik, size = floor(0.80 * length(ID_kapal_unik)))
idx_Train <- kapal$ID_kapal %in% Data_Train_ID
idx_Test  <- !idx_Train
Train     <- kapal_std[idx_Train, ]
Test      <- kapal_std[idx_Test, ]

# Baseline OLS
model_ols <- lm(formula_model, data = Train)
sum_ols   <- summary(model_ols)
ols_koef  <- data.frame(Variabel = rownames(sum_ols$coefficients), sum_ols$coefficients, row.names = NULL)
names(ols_koef) <- c("Variabel", "Estimate", "Std_Error", "t_value", "p_value")

# 2. Regresi Bayesian (4 Model Lengkap)
diagnosis_bayesian <- data.frame(
  Variabel = c("LCB", "PC", "LDR", "BDR", "LBR", "Fn", "r"),
  Permasalahan = c("Terdapat outlier", "Normal", "Multikolinearitas tinggi", "Outlier & Multikolinearitas", "Multikolinearitas tinggi", "Nonlinear terhadap r", "Skewness & Heteroskedastisitas"),
  Penanganan = c(rep("Standardisasi + Regularizing prior", 5), "Spline s(Fn)", "Student-t + sigma ~ s(Fn)")
)

fit_brm <- function(formula, family, prior, nama, adapt_delta = 0.95, max_treedepth = 15, save_all = FALSE) {
  brm(formula, data = Train, family = family, prior = prior, chains = N_CHAIN, iter = N_ITER, warmup = N_WARMUP, cores = N_CORES, seed = SEED,
      save_pars = if (save_all) save_pars(all = TRUE) else save_pars(),
      control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth), file = file.path(OUT_DIR, "model", nama), file_refit = "on_change")
}

model_student <- fit_brm(r ~ LCB + PC + LDR + BDR + LBR + Fn + (1 | ID_kapal), family = student(), 
                         prior = c(prior(normal(0, 1), class = "b"), prior(student_t(3, 0, 10), class = "Intercept"), prior(exponential(1), class = "sigma"), prior(gamma(2, 0.1), class = "nu"), prior(exponential(1), class = "sd")), nama = "m_student")

model_spline <- fit_brm(r ~ LCB + PC + LDR + BDR + LBR + s(Fn) + (1 | ID_kapal), family = student(), 
                        prior = c(prior(normal(0, 1), class = "b"), prior(student_t(3, 0, 10), class = "Intercept"), prior(exponential(1), class = "sigma"), prior(gamma(2, 0.1), class = "nu"), prior(exponential(1), class = "sd"), prior(exponential(1), class = "sds")), nama = "m_spline", max_treedepth = 12)

model_hetero <- fit_brm(bf(r ~ LCB + PC + LDR + BDR + LBR + s(Fn) + (1 | ID_kapal), sigma ~ s(Fn)), family = student(), 
                        prior = c(prior(normal(0, 1), class = "b"), prior(student_t(3, 0, 10), class = "Intercept"), prior(exponential(1), class = "sd"), prior(exponential(1), class = "sds"), prior(gamma(2, 0.1), class = "nu"), prior(student_t(3, 0, 2.5), class = "Intercept", dpar = "sigma"), prior(exponential(1), class = "sds", dpar = "sigma")), nama = "m_hetero")

model_skew <- fit_brm(bf(r ~ LCB + PC + LDR + BDR + LBR + s(Fn) + (1 | ID_kapal), sigma ~ s(Fn)), family = skew_normal(), 
                      prior = c(prior(normal(0, 1), class = "b"), prior(student_t(3, 0, 10), class = "Intercept"), prior(exponential(1), class = "sd"), prior(exponential(1), class = "sds"), prior(normal(0, 2), class = "alpha"), prior(student_t(3, 0, 2.5), class = "Intercept", dpar = "sigma"), prior(exponential(1), class = "sds", dpar = "sigma")), nama = "m_skew", adapt_delta = 0.99, save_all = TRUE)

model_gab <- model_hetero

# Perbandingan LOO ke-4 Model Bayesian
loo_models <- list(Student = loo(model_student), Spline = loo(model_spline), Hetero = loo(model_gab), Skew = loo(model_skew))
loo_cmp <- loo_compare(loo_models)
loo_tab <- data.frame(Model = rownames(loo_cmp), as.data.frame(unclass(loo_cmp)), row.names = NULL)

ps_all <- as.data.frame(posterior_summary(model_gab))
ps_all$Parameter <- rownames(ps_all)
post_summary <- ps_all[grepl("^(b_|bs_|sds_|sd_|nu$)", ps_all$Parameter), c("Parameter", "Estimate", "Est.Error", "Q2.5", "Q97.5")]
rownames(post_summary) <- NULL

dr <- as_draws_df(model_gab)
vars_utama <- grep("^(b_|bs_|sds_|sd_|nu$)", posterior::variables(dr), value = TRUE)
rhat_max <- max(summarise_draws(subset_draws(dr, variable = vars_utama), rhat = posterior::rhat)$rhat, na.rm = TRUE)

# 3. Random Forest Benchmark
set.seed(SEED)
model_rf_tr <- randomForest(r ~ Fn + LCB + PC + LDR + BDR + LBR, data = kapal[idx_Train, ], ntree = 500, importance = TRUE)
saveRDS(model_rf_tr, file.path(OUT_DIR, "model_rf.rds"))
rf_oob_r2 <- model_rf_tr$rsq[length(model_rf_tr$rsq)]

# 4. Evaluasi Test Set
pred_ols   <- predict(model_ols, newdata = Test)
pred_bayes <- colMeans(posterior_epred(model_gab, newdata = Test, allow_new_levels = TRUE))
pred_rf    <- predict(model_rf_tr, newdata = kapal[idx_Test, ])
aktual     <- Test$r

mape    <- function(a, p) mean(abs((a - p) / a)) * 100
smape_p <- function(a, p) mean(2 * abs(a - p) / (abs(a) + abs(p))) * 100
r2_test <- function(a, p) 1 - sum((a - p)^2) / sum((a - mean(a))^2)

evaluasi_test <- data.frame(
  Model = c("OLS (baseline)", "Regresi Bayesian", "Random Forest"),
  RMSE  = c(Metrics::rmse(aktual, pred_ols), Metrics::rmse(aktual, pred_bayes), Metrics::rmse(aktual, pred_rf)),
  MAE   = c(Metrics::mae(aktual, pred_ols),  Metrics::mae(aktual, pred_bayes),  Metrics::mae(aktual, pred_rf)),
  MAPE  = c(mape(aktual, pred_ols),   mape(aktual, pred_bayes),   mape(aktual, pred_rf)),
  sMAPE = c(smape_p(aktual, pred_ols), smape_p(aktual, pred_bayes), smape_p(aktual, pred_rf)),
  R2    = c(r2_test(aktual, pred_ols), r2_test(aktual, pred_bayes), r2_test(aktual, pred_rf))
)

ypred_draws <- posterior_predict(model_gab, newdata = Test, allow_new_levels = TRUE)
lo <- apply(ypred_draws, 2, quantile, probs = 0.025)
hi <- apply(ypred_draws, 2, quantile, probs = 0.975)
cakupan95 <- mean(aktual >= lo & aktual <= hi) * 100
pred_test <- data.frame(idx = seq_along(aktual), Aktual = aktual, OLS = as.numeric(pred_ols), Bayesian = as.numeric(pred_bayes), RF = as.numeric(pred_rf))

hasil <- list(
  meta = list(n_obs = n_obs, n_desain = n_desain, rhat_max = rhat_max, rf_oob_r2 = rf_oob_r2, cakupan95 = cakupan95),
  skala = skala,
  eda = list(
    stat_desc = stat_desc, spearman = spearman, missing = missing_tab, outlier = outlier_tab,
    vif = vif_all, bp = bp_all_df, plot_hist = plot_hist, plot_box = plot_box, plot_scatter = plot_scatter
  ),
  ols = list(koef = ols_koef),
  bayes = list(diagnosis = diagnosis_bayesian, loo = loo_tab, post_summary = post_summary),
  evaluasi = evaluasi_test, pred_test = pred_test
)

saveRDS(hasil, file.path(OUT_DIR, "hasil_dashboard.rds"), compress = "xz")
cat("Pre-komputasi lengkap berhasil disimpan!\n")