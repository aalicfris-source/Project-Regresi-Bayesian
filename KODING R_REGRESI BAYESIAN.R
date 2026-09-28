# PROJECT REGRESI BAYESIAN
# Pengaruh Angka Kecepatan Froude terhadap Hambatan Kapal Yacht 
# dengan Pengendalian Parameter Geometri Kapal : Pendekatan Regresi Bayesian
  
## 1. Packages yang dibutuhkan
library(readxl)
library(psych)
library(lmtest)
library(dplyr)
library(tidyr)
library(ggplot2)
library(GGally)
library(brms)
library(bayesplot)
library(loo)
library(car) 
library(Metrics)
library(randomForest)

SEED <- 2024
set.seed(SEED)
options(mc.cores = parallel::detectCores())

###############################################################################
## LANGKAH 1: BACA DOKUMENTASI DATA
###############################################################################
kapal_raw <- read_excel("C:/Users/ASUS/Downloads/data.xlsx")
kapal <- kapal_raw[, 1:7]
names(kapal) <- c("LCB", "PC", "LDR", "BDR", "LBR", "Fn", "r")
kapal <- kapal %>% mutate(across(everything(), as.numeric)) %>% drop_na()
head(kapal)

cat("Jumlah observasi:", nrow(kapal), "\n")
cat("Jumlah desain lambung unik:", nrow(distinct(kapal, LCB, PC, LDR, BDR, LBR)), "\n")

# Jumlah data dan kolom
dim(kapal)
# Tipe data
sapply(kapal,class)
# Untuk cek missing data
colSums(is.na(kapal))

# Ringkasan Statistik Detail (Mean, SD, Median, Min, Max, Skew, Kurtosis)
cat("\n--- Statistik Deskriptif ---\n")
stat_desc <- describe(kapal)
print(round(stat_desc[, c("mean", "sd", "median", "min", "max", "skew", "kurtosis")], 3))

# Matriks Korelasi Antar-Variabel (Bivariat)
cat("\n--- MATRIKS KORELASI (SPEARMAN) ---\n")
spearman<-cor(kapal,use="complete.obs",
              method="spearman")
print(spearman)

# Visualisasi Grafik Distribusi Data Mentah
par(mfrow = c(2, 2)) # Membagi jendela plot menjadi 2x2

# Histogram Target Hambatan Kapal (r)
hist(kapal$r, main = "Distribusi Target (r)", xlab = "r", 
     col = "blue", border = "black",freq=FALSE)
lines(density(kapal$r), col = "red", lwd = 2)
hist(kapal$LCB, main = "LCB", xlab = "LCB", 
     col = "blue", border = "black",freq=FALSE)
lines(density(kapal$LCB), col = "red", lwd = 2)
hist(kapal$PC, main = "PC", xlab = "PC", 
     col = "blue", border = "black",freq=FALSE)
lines(density(kapal$PC), col = "red", lwd = 2)
hist(kapal$LDR, main = "LDR", xlab = "LDR", 
     col = "blue", border = "black",freq=FALSE)
lines(density(kapal$LDR), col = "red", lwd = 2)
hist(kapal$BDR, main = "BDR", xlab = "BDR", 
     col = "blue", border = "black",freq=FALSE)
lines(density(kapal$BDR), col = "red", lwd = 2)
hist(kapal$LBR, main = "LBR", xlab = "LBR", 
     col = "blue", border = "black",freq=FALSE)
lines(density(kapal$LBR), col = "red", lwd = 2)
hist(kapal$Fn, main = "Fn", xlab = "Fn", 
     col = "blue", border = "black",freq=FALSE)
lines(density(kapal$Fn), col = "red", lwd = 2)

# Boxplot Target Hambatan Kapal (Cek Outlier)
par(mfrow = c(2, 2))
boxplot(kapal$r, main = "Boxplot r", ylab = "r", 
        col = "orange", vertical = TRUE)
boxplot(kapal$LCB, main = "Boxplot LCB", ylab = "LCB", 
        col = "orange", vertical = TRUE)
boxplot(kapal$PC, main = "Boxplot PC", ylab = "PC", 
        col = "orange", vertical = TRUE)
boxplot(kapal$LDR, main = "Boxplot LDR", ylab = "LDR", 
        col = "orange", vertical = TRUE)
boxplot(kapal$BDR, main = "Boxplot BDR", ylab = "BDR", 
        col = "orange", vertical = TRUE)
boxplot(kapal$LBR, main = "Boxplot LBR", ylab = "LBR", 
        col = "orange", vertical = TRUE)
boxplot(kapal$Fn, main = "Boxplot fn", ylab = "fn", 
        col = "orange", vertical = TRUE)

# multikolinearitas
model<-lm(r~LCB+PC+LDR+BDR+LBR+Fn,
          data=kapal)
multi<-vif(model)
print(multi)

# heteroskedastisitas
hetero<-lm(r~LCB+PC+LDR+BDR+LBR+Fn,data=kapal)
bptest(hetero)

# Scatterplot r terhadap LCB,PC,LDR,BDR,LBR,fn
par(mfrow = c(2, 2))
plot(kapal$LCB, kapal$r, main = "LCB vs r", 
     xlab = "LCB", ylab = "r", pch = 19, col = "blue")
plot(kapal$PC, kapal$r, main = "PC vs r", 
     xlab = "PC", ylab = "r", pch = 19, col = "blue")
plot(kapal$LDR, kapal$r, main = "LDR vs r", 
     xlab = "LDR", ylab = "r", pch = 19, col = "blue")
plot(kapal$BDR, kapal$r, main = "BDR vs r", 
     xlab = "BDR", ylab = "r", pch = 19, col = "blue")
plot(kapal$LBR, kapal$r, main = "LBR vs Hr", 
     xlab = "LBR", ylab = "r", pch = 19, col = "blue")
plot(kapal$Fn, kapal$r, main = "fn vs r", 
     xlab = "fn", ylab = "r", pch = 19, col = "blue")

###############################################################################
## LANGKAH 2: EDA TERARAH PADA TARGET DAN PREDIKTOR (fn)
###############################################################################
# (a) Distribusi target r -> sangat menceng, motivasi transformasi log1p
p_hist_r <- ggplot(kapal, aes(r)) +
  geom_histogram(bins = 30, fill = "steelblue") +
  labs(title = "Distribusi hambatan sisa (r)", x = "r", y = "Frekuensi") +
  theme_minimal()
p_hist_logr <- ggplot(kapal, aes(log1p(r))) +
  geom_histogram(bins = 30, fill = "darkorange") +
  labs(title = "Distribusi log1p(r)", x = "log1p(r)", y = "Frekuensi") +
  theme_minimal()
gridExtra::grid.arrange(p_hist_r, p_hist_logr, ncol = 2)

cat("Skewness r        :", round(e1071::skewness(kapal$r), 3), "\n")
cat("Skewness log1p(r) :", round(e1071::skewness(log1p(kapal$r)), 3), "\n")

# (b) Hubungan r (dan log1p(r)) dengan Fn -> pola nonlinear, menguat setelah log
p_r_fn <- ggplot(kapal, aes(Fn, r)) +
  geom_point(alpha = 0.5) + geom_smooth(se = FALSE, colour = "red") +
  labs(title = "r vs Fn (skala asli)") + theme_minimal()
p_logr_fn <- ggplot(kapal, aes(Fn, log1p(r))) +
  geom_point(alpha = 0.5) + geom_smooth(se = FALSE, colour = "red") +
  labs(title = "log1p(r) vs Fn") + theme_minimal()
p_logr_fn2 <- ggplot(kapal, aes(Fn^2, log1p(r))) +
  geom_point(alpha = 0.5) + geom_smooth(se = FALSE, colour = "red") +
  labs(title = "log1p(r) vs Fn^2") + theme_minimal()
gridExtra::grid.arrange(p_r_fn, p_logr_fn, p_logr_fn2, ncol = 3)

kapal$log1p_r<-log1p(kapal$r)
kapal$Fn2<-kapal$Fn^2
head(kapal)

cat("Korelasi Fn dengan r        :", round(cor(kapal$Fn, kapal$r), 3), "\n")
cat("Korelasi Fn dengan log1p(r) :", round(cor(kapal$Fn, log1p(kapal$r)), 3), "\n")
cat("Korelasi Fn2 dengan log1p(r) :", round(cor(kapal$Fn2, log1p(kapal$r)), 3), "\n")

# (c) Hubungan lima variabel geometri (LCB, PC, LDR, BDR, LBR) terhadap target (r)
kor_r <- sapply(kapal[c("LCB","PC","LDR","BDR","LBR")],
                      function(x) cor(x,kapal$r ))
kor_logr <- sapply(kapal[c("LCB","PC","LDR","BDR","LBR")],
                      function(x) cor(x, log1p(kapal$r)))
cat("Korelasi variabel prediktor dengan r:\n"); print(round(kor_r, 3))
cat("Korelasi variabel prediktor dengan log1p(r):\n"); print(round(kor_logr, 3))
# --> Kesimpulan EDA: Fn adalah prediktor utama yang jauh lebih kuat
#                     Variabel geometri (LCB, PC, LDR, BDR, LBR) jauh lebih lemah
#                     Target dipakai dalam skala log1p; Fn dimasukkan sebagai polinomial
#                     ortogonal derajat 2 untuk pola polinomial yang linier

###############################################################################
## LANGKAH 3: FIT REGRESI BAYESIAN 
###############################################################################
# Standardisasi variabel prediktor agar koefisien sebanding & prior lebih mudah
# diinterpretasikan 
kapal_std <- kapal %>%
  mutate(across(c(LCB, PC, LDR, BDR, LBR), ~ as.numeric(scale(.)))) 

formula_model <- log1p_r ~ poly(Fn, 2) + LCB + PC + LDR + BDR + LBR
head(kapal_std)

# Split train-test (80:20)
set.seed(SEED)
idx_train <- sample(seq_len(nrow(kapal_std)), size = floor(0.8 * nrow(kapal_std)))
train <- kapal_std[idx_train, ]
test  <- kapal_std[-idx_train, ]
c(Train = nrow(train), Test = nrow(test), Total = nrow(kapal_std))

# Model regresi bayesian pada data train
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

cat("\n=== Regresi Bayesian: ringkasan posterior ===\n")
print(summary(fit_bayes))
plot(fit_bayes)                     # trace plot & densitas posterior (cek MCMC)

# Validasi model Bayesian: posterior predictive checking (MCMC/komputasi)
pp_check(fit_bayes, ndraws = 100) +
  labs(title = "Posterior Predictive Check: y (data) vs y_rep (replikasi)")

# Diagnostik MCMC: R-hat dan effective sample size
rhat_vals <- brms::rhat(fit_bayes)
cat("R-hat maksimum (target <= 1.01) :", round(max(rhat_vals, na.rm = TRUE), 4), "\n")
mcmc_trace(fit_bayes, regex_pars = c("b_Intercept", "b_poly(Fn, 3)1"))

# Residual Bayesian (rata-rata posterior residual)
resid_bayes <- residuals(fit_bayes)[, "Estimate"]
qqnorm(resid_bayes, main = "QQ-Plot Residual Model Bayesian"); qqline(resid_bayes)

# Sensitivity analysis: ganti prior, cek kestabilan estimasi posterior
prior_sempit <- c(prior(normal(0, 0.5), class = "b"))
prior_lebar  <- c(prior(normal(0, 10),  class = "b"))

fit_prior_sempit <- update(fit_bayes, prior = prior_sempit, refresh = 0)
fit_prior_lebar  <- update(fit_bayes, prior = prior_lebar,  refresh = 0)

sensitivitas <- bind_rows(
  cbind(prior = "Normal(0, 0.5) - sempit", as.data.frame(fixef(fit_prior_sempit))),
  cbind(prior = "Normal(0, 2.5) - utama",  as.data.frame(fixef(fit_bayes))),
  cbind(prior = "Normal(0, 10) - lebar",   as.data.frame(fixef(fit_prior_lebar)))
)
cat("\n=== Analisis sensitivitas prior (estimasi koefisien Fn linear) ===\n")
print(sensitivitas %>% filter(grepl("poly", rownames(sensitivitas))))

###############################################################################
## LANGKAH 4: SATU BASELINE REGRESI SEDERHANA [OLS]
## RESIDUAL, INFLUENTIAL OBSERVATIONS, MULTIKOLINEARITAS
###############################################################################
# Baseline: regresi linear klasik (OLS) 
model_ols <- lm(formula_model, data = train)
cat("\n=== Baseline OLS ===\n")
print(summary(model_ols))

# Residual model OLS (baseline)
par(mfrow = c(2, 2)); plot(model_ols); par(mfrow = c(1, 1))

# Influential observations (Cook's distance, dari model OLS)
cooksd <- cooks.distance(model_ols)
batas_cook <- 4 / nrow(kapal_std)
obs_berpengaruh <- which(cooksd > batas_cook)
cat("Jumlah observasi berpengaruh (Cook's D >", round(batas_cook, 4), "):",
    length(obs_berpengaruh), "dari", nrow(kapal_std), "\n")
plot(cooksd, type = "h", main = "Cook's Distance", ylab = "Cook's D")
abline(h = batas_cook, col = "red", lty = 2)

# Multikolinearitas (VIF) -- poly() ortogonal menekan VIF antar suku Fn
cat("\n=== Variance Inflation Factor (VIF) ===\n")
print(car::vif(model_ols))

###############################################################################
## LANGKAH 5: BACKMARK RANDOM FOREST 
###############################################################################
set.seed(SEED)
model_rf_tr <- randomForest(r ~ Fn + LCB + PC + LDR + BDR + LBR,
                            data = kapal[idx_train, ], ntree = 500,
                            importance = TRUE)
summary(model_rf_tr)

# Ringkasan hasil model pada data train
cat("\n[1] OLS (baseline)\n")
cat("    Formula :", deparse(formula_model), "\n")
cat("    Metode  : lm() - Ordinary Least Squares\n")
cat("    R2 (train)      :", round(summary(model_ols)$r.squared, 4), "\n")
cat("    R2 adjusted     :", round(summary(model_ols)$adj.r.squared, 4), "\n")

cat("\n[2] Regresi Bayesian\n")
cat("    Formula :", deparse(formula_model), "\n")
cat("    Metode  : brm() - MCMC (Stan/NUTS), 4 chains x 4000 iter (1000 warmup)\n")
rhat_tr <- brms::rhat(fit_bayes)
cat("    R-hat maksimum (target <= 1.01) :", round(max(rhat_tr, na.rm = TRUE), 4), "\n")

cat("\n[3] Random Forest (benchmark)\n")
cat("    Formula :", deparse(formula(model_rf_tr)), "\n")
cat("    Metode  : randomForest(), ntree = 500\n")
cat("    % Var explained (train, OOB) :", round(model_rf_tr$rsq[length(model_rf_tr$rsq)] * 100, 2), "%\n")


###############################################################################
## LANGKAH 6: Evaluasi Model Data Test
###############################################################################
# Evaluasi pada data test (kembalikan ke skala r asli: expm1)
pred_ols   <- expm1(predict(model_ols, newdata = test))
pred_bayes <- expm1(colMeans(posterior_predict(fit_bayes, newdata = test)))
pred_rf    <- predict(model_rf_tr, newdata = kapal[-idx_train, ])
aktual     <- kapal$r[-idx_train]

mape <- function(a, p) mean(abs((a - p) / a)) * 100
r2_test <- function(a, p) 1 - sum((a - p)^2) / sum((a - mean(a))^2)

evaluasi_test <- data.frame(
  Model = c("OLS (baseline)", "Regresi Bayesian", "Random Forest (benchmark)"),
  RMSE  = c(rmse(aktual, pred_ols), rmse(aktual, pred_bayes), rmse(aktual, pred_rf)),
  MAE   = c(mae(aktual, pred_ols),  mae(aktual, pred_bayes),  mae(aktual, pred_rf)),
  MAPE  = c(mape(aktual, pred_ols), mape(aktual, pred_bayes), mape(aktual, pred_rf)),
  R2    = c(r2_test(aktual, pred_ols), r2_test(aktual, pred_bayes), r2_test(aktual, pred_rf))
)
cat("\n=== Evaluasi lengkap pada data TEST (20%, n =", nrow(test), ") ===\n")
print(evaluasi_test, digits = 4)


# Cross-validation Bayesian: LOO (leave-one-out, komputasi via PSIS)
loo_bayes <- loo(fit_bayes)
print(loo_bayes)

###############################################################################
## LANGKAH 7: Visualisasi
###############################################################################
# Plot garis: urutan observasi test vs nilai (aktual + 3 model)
df_pred_long <- data.frame(
  idx = rep(seq_len(nrow(test)), 4),
  nilai = c(aktual, pred_ols, pred_bayes, pred_rf),
  Seri = factor(
    rep(c("Aktual", "OLS", "Regresi Bayesian", "Random Forest"), each = nrow(test)),
    levels = c("Aktual", "OLS", "Regresi Bayesian", "Random Forest")
  )
)
p_line <- ggplot(df_pred_long, aes(x = idx, y = nilai, color = Seri,
                                   linewidth = Seri, alpha = Seri)) +
  geom_line() +
  scale_linewidth_manual(values = c(Aktual = 1.1, OLS = 0.6, `Regresi Bayesian` = 0.6, `Random Forest` = 0.6)) +
  scale_alpha_manual(values = c(Aktual = 1, OLS = 0.85, `Regresi Bayesian` = 0.85, `Random Forest` = 0.85)) +
  scale_color_manual(values = c(Aktual = "black", OLS = "blue",
                                `Regresi Bayesian` = "red", `Random Forest` = "green")) +
  labs(title = "Aktual vs Prediksi pada Data Test",
       subtitle = "Urutan observasi test (bukan diurutkan berdasarkan waktu/Fn)",
       x = "Indeks observasi (data test)", y = "Hambatan sisa (r)") +
  theme_minimal(base_size = 12)
print(p_line)

# Scatter plot aktual vs prediksi per model + garis referensi 45 derajat
df_scatter <- data.frame(
  aktual = rep(aktual, 3),
  prediksi = c(pred_ols, pred_bayes, pred_rf),
  Model = factor(rep(c("OLS", "Regresi Bayesian", "Random Forest"), each = length(aktual)),
                 levels = c("OLS", "Regresi Bayesian", "Random Forest"))
)
p_scatter <- ggplot(df_scatter, aes(x = aktual, y = prediksi, color = Model)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
  facet_wrap(~Model) +
  scale_color_manual(values = c(OLS = "blue", `Regresi Bayesian` = "red", `Random Forest` = "green")) +
  labs(title = "Aktual vs Prediksi per Model (Data Test)",
       subtitle = "Garis putus-putus = prediksi sempurna (aktual = prediksi)",
       x = "Nilai aktual (r)", y = "Nilai prediksi (r)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")
print(p_scatter)

# Visualisasi Prior,Likelihood,Posterior
x <- seq(-5, 15, length.out = 1000)

# Menghitung Kurva Teoritis (Prior Normal(0, 2.5), Likelihood Data, & Posterior)
df_plot <- data.frame(
  x = rep(x, 3),
  Density = c(
    dnorm(x, mean = 0, sd = 2.5),       # Prior: Weakly Informative Normal(0, 2.5)
    dnorm(x, mean = 6, sd = 1.2),       # Likelihood / Observasi Data
    dnorm(x, mean = 4.9, sd = 0.7)      # Posterior (Update)
  ),
  Komponen = factor(
    rep(c("Prior", "Likelihood (Data)", "Posterior"), each = length(x)),
    levels = c("Prior", "Likelihood (Data)", "Posterior")
  )
)

# Plot dalam Satu Gambar
ggplot(df_plot, aes(x = x, y = Density, color = Komponen, fill = Komponen)) +
  geom_line(size = 1.2) +
  geom_area(alpha = 0.25, position = "identity") +
  scale_color_manual(values = c("Prior" = "orange", "Likelihood (Data)" = "blue", "Posterior" = "green")) +
  scale_fill_manual(values = c("Prior" = "orange", "Likelihood (Data)" = "blue", "Posterior" = "green")) +
  labs(
    title = "Regresi Bayesian: Prior, Likelihood, dan Posterior",
    subtitle = "Prior Weakly-Informative Normal(0, 2.5) dengan Likelihood Gaussian",
    x = "Nilai Parameter / Variabel Respon",
    y = "Densitas"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "top",
    legend.title = element_blank()
  )
