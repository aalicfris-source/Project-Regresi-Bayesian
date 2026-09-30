# ⚡ Pengaruh Angka Kecepatan Froude terhadap Hambatan Sisa (Residuary Resistance) Kapal Yacht dengan Pengendalian Geometri Kapal melalui Pendekatan Regresi Bayesian

> **Mata Kuliah:** Analisis Regresi Tingkat Lanjut  
> **Program Studi:** Magister Statistika Terapan, Departemen Statistika  
> **Fakultas:** Matematika dan Ilmu Pengetahuan Alam (FMIPA), Universitas Padjadjaran  
> **Dosen Pengampu:** Dr. I Gede Nyoman Mindra Jaya  
> **Penyusun:**  
> 1. **Hennyda Laura Br Tarigan** (NPM: `140720260015`)  
> 2. **Yiska Friscilla** (NPM: `140720260014`)  

---

## 📌 1. Deskripsi Proyek

Menerapkan Regresi Bayesian pada konteks hambatan hidrodinamika kapal. Fokusnya adalah statistical reasoning, komputasi reproducible, validasi, dashboard, dan komunikasi ilmiah (*Appliances*) berbasis data kapal yatch *Yacht Hydrodynamics* dari dataset **UCI Machine Learning Repository** (308 jumlah observasi data dengan 22 desain geometri lambung kapal dan 14 tingkat kecepatan angka).

Tantangan utama data ini adalah:
1. Variabel target (r) memiliki sebaran yang **tidak simetris (right-skewed)** dan terdapat **outlier**.
2. Terdapat **heteroskedastisitas (varians yang tidak konstan)** pada data variabel target (r).
3. ⁠Terdapat **multikolinearitas** pada variabel target (r) dan dari 6 variabel prediktor terdapat 3 yang memiliki multikolinearitas.
4. ⁠Pada variabel prediktor (Fn) terdapat p**ola polinomial yang non linear**.

Untuk mengatasi tantangan tersebut, diterapkan:
Dari tantangan data tersebut maka dilakukan penanganan dengan pendekatan **bayesian** dengan pemilihan **likelihood gaussian normal**, **prior weakly informative**, kemudian melakukan **evaluasi diagnosis konvergensi MCMC** serta melakukan e**valuasi posterior** menggunakan **Leave One Out Cross Validation(LOO CV)**.

---

## 📊 2. Ringkasan Hasil Evaluasi Model

Pemodelan dilakukan menggunakan **Regresi Bayesian** sebagai model utama, **OLS** sebagai *baseline*, dan **Random Forest** sebagai *benchmark*. Model dilatih pada *data train* dan dievaluasi pada *data test* menggunakan rasio pemisahan (*train-test split*) sebesar **80:20**.

| Model Evaluasi | RMSE | MAE | MAPE (%) | $R^2$ Score |
| :--- | :---: | :---: | :---: | :---: |
| 1. Baseline OLS | 1.730 | 0.8677 | 26.99 | 0.9888 |
| **2. Regresi Bayesian (Utama)** | **1.725** | **0.8751** | **27.31** | **0.9889** |
| 3. Random Forest (AI Benchmark) | 4.384 | 2.8463 | 743.74 | 0.9282 |

---

## 🗂️ 3. Struktur File dan Direktori

```text
📁 Project Upload/
├── 📄 README.md                                            # Dokumentasi utama proyek
├── 📄 data_kapal.xls                                       # Data dictionary ringkas
├── 📄 KODING Rstudio REGRESI BAYESIAN.R                    # Skrip komputasi R lengkap (Analisi Regresi Bayesian)
├── 📄 Analisis Awal_Regresi Bayesian_Kelompok 12.doc       # Analisis Awal_Regresi Bayesian
├── 📄 koding Pre RShiny.R                                  # Pre Koding Rshiny
├── 📄 koding app Rshiny.R                                  # Skrip otomatisasi deploy ke ShinyApps.io
├── 📄 HASIL REGRESI BAYESIAN.ppt                           # Skrip otomatisasi deploy ke ShinyApps.io
└── 📄 LAPORAN HASIL PROJECT_REGRESI BAYESIAN.pdf           # Laporan lengkap proyek
```

---

## 🚀 4. Panduan Menjalankan Proyek

### A. Prasyarat Paket R (*Prerequisites*)
Sebelum menjalankan proyek, pastikan seluruh paket R yang diperlukan sudah terpasang di lingkungan R/RStudio Anda:

```r
install.packages(c(
  "shiny", "bslib", "ggplot2", "dplyr", "tidyr", "readxl", 
  "gridExtra", "Metrics", "tibble", "brms", "bayesplot", 
  "psych", "lmtest", "car", "randomForest", "e1071", "rsconnect"
))
```

### B. Menjalankan Skrip Pre-Computation & Aplikasi Shiny Secara Lokal
Proyek ini menggunakan metode dua tahap eksekusi: pengolahan data & pemodelan awal (`Pre.R`), dilanjutkan dengan menjalankan dashboard Shiny (`app.R`).

1. **Jalankan Skrip Pre-Computation:**  
   Pastikan direktori kerja (*working directory*) sudah diarahkan ke folder proyek, lalu jalankan skrip `Pre.R` untuk melatih model dan menghasilkan file `precomputed_bayes_model.rds`:

   ```r
   # Set direktori kerja (sesuaikan dengan lokasi folder Anda)
   setwd("path/to/your/project")

   # Jalankan skrip pemodelan
   source("Pre.R")
   ```

2. **Jalankan Dashboard Shiny:**  
   Setelah file `precomputed_bayes_model.rds` berhasil dibuat, jalankan aplikasi Shiny melalui skrip `app.R`:

   ```r
   library(shiny)

   # Menjalankan aplikasi Shiny
   runApp("app.R")
   ```

### C. Deploy ke ShinyApps.io
Untuk mempublikasikan aplikasi ke layanan [ShinyApps.io](https://www.shinyapps.io/), jalankan perintah berikut di konsol RStudio Anda:

```r
library(rsconnect)

# Melakukan deployment aplikasi
rsconnect::deployApp(
  appDir        = getwd(),
  appPrimaryDoc = "app.R",
  appName       = "bayesian-yacht-hydrodynamics",
  appFiles      = c("app.R", "precomputed_bayes_model.rds"),
  lint          = FALSE,
  forceUpdate   = TRUE
)
```
🌐 **Live Demo Dashboard:**  
[https://regresibayesian.shinyapps.io/Bayesian7/) .

---

## 🔬 5. Fitur Interaktif Dashboard
### 🟢 TAB 1: Eksplorasi Data Raw
Tab ini difokuskan pada analisis eksplorasi data mentah sebelum dilakukan transformasi data:
* **Kartu Metrik Ringkas:**
  * Menampilkan jumlah total observasi.
  * Menampilkan jumlah desain lambung kapal yang unik.
  * Menampilkan jumlah variabel dalam dataset.
  * Dilengkapi tombol *Upload File* untuk memuat file dataset Excel (`.xlsx`).
* **Tabel Ringkasan Statistik Detail & Diagnostik:**
  * **Statistik Deskriptif:** Menampilkan nilai *mean*, *standard deviation* (sd), *median*, *minimum*, dan *maximum*.
  * **Uji Multikolinearitas:** Pengujian *Variance Inflation Factor* (VIF).
  * **Uji Heteroskedastisitas:** Pengujian *Breusch-Pagan Test*.
  * **Analisis Missing Data:** Tabel identifikasi nilai yang hilang (*missing values*) per variabel.
* **Matriks Korelasi & Distribusi Visual:**
  * Matriks Korelasi Spearman antar variabel.
  * Visualisasi *Boxplot Distribution* untuk melihat variasi data dan pencilan (*outliers*).
  * Plot Histogram distribusi untuk setiap variabel.
  * *Scatterplot* hubungan antara hambatan kapal ($r$) terhadap variabel-variabel prediktor.

### 🟡 TAB 2: EDA Terarah & Transformasi
Tab ini memuat analisis eksplorasi data tingkat lanjut serta proses transformasi variabel untuk optimalisasi model:
* **Perbandingan Distribusi Target:**
  * Visualisasi histogram perbandingan dan pengujian *skewness* (kemiringan) antara variabel target skala asli ($r$) vs hasil transformasi logaritma ($\log(r)$).
* **Analisis Hubungan Non-Linier:**
  * Perhitungan nilai korelasi antara: $Fn$ vs $r$, $Fn$ vs $\log(r)$, dan $Fn^2$ vs $\log(r)$.
  * Visualisasi plot hubungan $r$ dan $\log(r)$ terhadap *Froude Number* ($Fn$) dan $Fn^2$.
* **Geometri & Standarisasi:**
  * Tabel korelasi variabel geometri kapal terhadap $r$ dan $\log(r)$.
  * Tampilan *output* teks *preview tibble* dari hasil standarisasi variabel-variabel prediktor.

### 🔵 TAB 3: Modeling & Evaluasi
Tab ini menyajikan hasil pemodelan prediktif dan evaluasi komparatif dari tiga metode (**OLS**, **Regresi Bayesian**, dan **Random Forest**):
* **Kartu Metrik Model:**
  * Informasi nilai Bayesian $\hat{R}$ (*R-Hat*) untuk mengecek konvergensi MCMC.
  * Informasi proporsi pembagian data: *Data Train* (80%) dan *Data Test* (20%).
* **Evaluasi Performa Model:**
  * Tabel komparasi metrik evaluasi pada *data test*: *Root Mean Square Error* (RMSE), *Mean Absolute Error* (MAE), *Mean Absolute Percentage Error* (MAPE), dan *Coefficient of Determination* ($R^2$).
* **Visualisasi Bayesian & Diagnostik:**
  * Plot kurva distribusi *Prior*, *Likelihood*, dan *Posterior*.
  * Plot *Posterior Predictive Check* ($y$ vs $y_{rep}$).
  * Plot *Normal QQ-Plot* untuk analisis residual Bayesian.
* **Perbandingan Prediksi Model:**
  * *Line plot* perbandingan nilai aktual vs nilai hasil prediksi dari ketiga model pada *data test*.
  * *Scatter plot* kesesuaian nilai aktual vs prediksi untuk masing-masing model.

---

## 📚 6. Referensi Ilmiah

1. Gelman, A., Carlin, J. B., Stern, H. S., Dunson, D. B., Vehtari, A., & Rubin, D. B. (2013). 
Bayesian data analysis (3rd ed.). CRC Press.

2. Gerritsma, J., Onnink, R., & Versluis, A. (1981). Geometry, resistance and stability of the 
Delft systematic yacht hull series. International Shipbuilding Progress, 28(328), 276–297. 
https://doi.org/10.3233/ISP-1981-2832801

3. Tew, S. Y., Boley, M., & Schmidt, D. F. (2023). Bayes beats cross validation: Fast and 
accurate ridge regression via expectation maximization. Advances in Neural Information 
Processing Systems, 36.
