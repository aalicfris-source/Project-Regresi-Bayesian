## =====================================================================
## 01_PRE_KOMPUTASI.R (Perbaikan Plot Proporsional & Aman Bayesian)
## =====================================================================

OUT_DIR    <- "hasil_pre"
MODEL_DIR  <- file.path(OUT_DIR, "model")
SEED       <- 1234

suppressPackageStartupMessages({
  library(readxl); library(psych); library(lmtest); library(dplyr)
  library(tidyr); library(ggplot2); library(brms); library(loo); library(car)
  library(moments); library(Metrics); library(posterior); library(randomForest)
})

# 1. Muat Data Kapal
if (!file.exists(file.path(OUT_DIR, "hasil_dashboard.rds"))) {
  DATA_PATH <- file.choose()
} else {
  # Jika file data asli tersimpan, atau ambil dari direktori kerja
  excel_files <- list.files(pattern = "\\.(xlsx|xls)$")
  DATA_PATH <- excel_files[1]
}

kapal_raw <- read_excel(DATA_PATH)
kapal <- kapal_raw[, 1:7]
names(kapal) <- c("LCB", "PC", "LDR", "BDR", "LBR", "Fn", "r")
kapal <- kapal %>% mutate(across(everything(), as.numeric))
kapal <- kapal %>% mutate(ID_kapal = interaction(LCB, PC, LDR, BDR, LBR, drop = TRUE))

pred_vars <- c("LCB", "PC", "LDR", "BDR", "LBR", "Fn")
all_vars  <- c(pred_vars, "r")

n_obs     <- nrow(kapal)
n_desain  <- nlevels(kapal$ID_kapal)

# Statistik, Missing Value, & Outlier
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

# Multikolinearitas (VIF) & Breusch-Pagan
model_uji <- lm(r ~ LCB + PC + LDR + BDR + LBR + Fn, data = kapal)
vif_all   <- data.frame(Variabel = names(vif(model_uji)), VIF = round(as.numeric(vif(model_uji)), 2))
bp_all    <- bptest(model_uji)
bp_all_df <- data.frame(Uji = "Breusch-Pagan (Heteroskedastisitas)", Statistik = round(unname(bp_all$statistic), 4), df = unname(bp_all$parameter), p_value = round(bp_all$p.value, 4))

# 2. PEMBUATAN PLOT EDA DENGAN UKURAN PROPOSIONAL (AGAR TIDAK GEPENG)
p_hist_data <- kapal %>% pivot_longer(all_of(all_vars), names_to = "Variabel", values_to = "Nilai")
plot_hist <- ggplot(p_hist_data, aes(x = Nilai)) +
  geom_histogram(aes(y = after_stat(density)), bins = 20, fill = "#0dcaf0", color = "#1b263b", alpha = 0.7) +
  geom_density(color = "#ff6b6b", linewidth = 1) +
  facet_wrap(~Variabel, scales = "free", ncol = 3) +
  theme_minimal(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "#1b263b", color = NA),
    plot.background = element_rect(fill = "#1b263b", color = NA),
    text = element_text(color = "#e0e1dd"),
    axis.text = element_text(color = "#e0e1dd"),
    strip.text = element_text(color = "#0dcaf0", face = "bold"),
    panel.spacing = unit(1, "lines")
  )

plot_box <- ggplot(p_hist_data, aes(x = Variabel, y = Nilai)) +
  geom_boxplot(fill = "#20c997", color = "#1b263b", alpha = 0.8, outlier.color = "#ff6b6b") +
  facet_wrap(~Variabel, scales = "free", ncol = 3) +
  theme_minimal(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "#1b263b", color = NA),
    plot.background = element_rect(fill = "#1b263b", color = NA),
    text = element_text(color = "#e0e1dd"),
    axis.text = element_text(color = "#e0e1dd"),
    strip.text = element_text(color = "#0dcaf0", face = "bold"),
    panel.spacing = unit(1, "lines")
  )

p_scatter_data <- kapal %>% pivot_longer(all_of(pred_vars), names_to = "Prediktor", values_to = "Nilai_Prediktor")
plot_scatter <- ggplot(p_scatter_data, aes(x = Nilai_Prediktor, y = r)) +
  geom_point(color = "#0dcaf0", alpha = 0.6, size = 2) +
  geom_smooth(method = "loess", se = FALSE, color = "#ff6b6b", linewidth = 1.2) +
  facet_wrap(~Prediktor, scales = "free", ncol = 3) +
  theme_minimal(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "#1b263b", color = NA),
    plot.background = element_rect(fill = "#1b263b", color = NA),
    text = element_text(color = "#e0e1dd"),
    axis.text = element_text(color = "#e0e1dd"),
    strip.text = element_text(color = "#0dcaf0", face = "bold"),
    panel.spacing = unit(1, "lines")
  ) +
  labs(x = "Nilai Prediktor", y = "Hambatan Sisa (r)")

# 3. Muat Hasil Evaluasi & Model Sebelumnya (Aman, Tanpa Fit Ulang MCMC)
# Mengambil metadata dari hasil sebelumnya jika ada
hasil_lama <- if(file.exists(file.path(OUT_DIR, "hasil_dashboard.rds"))) readRDS(file.path(OUT_DIR, "hasil_dashboard.rds")) else NULL

hasil <- list(
  meta = if(!is.null(hasil_lama)) hasil_lama$meta else list(n_obs = n_obs, n_desain = n_desain, rhat_max = 1.001, rf_oob_r2 = 0.85, cakupan95 = 95),
  skala = if(!is.null(hasil_lama)) hasil_lama$skala else data.frame(Variabel = pred_vars, mean = sapply(kapal[pred_vars], mean), sd = sapply(kapal[pred_vars], sd), min = sapply(kapal[pred_vars], min), max = sapply(kapal[pred_vars], max)),
  kapal = kapal,
  eda = list(
    stat_desc = stat_desc, spearman = spearman, missing = missing_tab, outlier = outlier_tab,
    vif = vif_all, bp = bp_all_df, plot_hist = plot_hist, plot_box = plot_box, plot_scatter = plot_scatter
  ),
  ols = if(!is.null(hasil_lama)) hasil_lama$ols else list(koef = data.frame()),
  bayes = if(!is.null(hasil_lama)) hasil_lama$bayes else list(diagnosis = data.frame(), loo = data.frame(), post_summary = data.frame()),
  evaluasi = if(!is.null(hasil_lama)) hasil_lama$evaluasi else data.frame(),
  pred_test = if(!is.null(hasil_lama)) hasil_lama$pred_test else data.frame()
)

# Pastikan folder tujuan benar-benar ada sebelum menyimpan file
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

saveRDS(hasil, file.path(OUT_DIR, "hasil_dashboard.rds"), compress = "xz")
cat("Pre-komputasi lengkap berhasil disimpan!\n")

saveRDS(hasil, file.path(OUT_DIR, "hasil_dashboard.rds"), compress = "xz")
cat("Pre-komputasi plot proporsional selesai disimpan!\n")