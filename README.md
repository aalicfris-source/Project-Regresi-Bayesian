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

# Penerapan Regresi Bayesian pada Konteks Hambatan Hidrodinamika Kapal

Menerapkan Regresi Bayesian pada konteks hambatan hidrodinamika kapal. Fokusnya adalah statistical reasoning, komputasi reproducible, validasi, dashboard, dan komunikasi ilmiah (Appliances) berbasis data kapal yatch Yacht Hydrodynamics dari dataset UCI Machine Learning Repository (308 jumlah observasi data dengan 22 desain geometri lambung kapal dan 14 tingkat kecepatan angka).

## Tantangan Utama Data

Tantangan utama data ini adalah:
1. Variabel target (r) memiliki sebaran yang tidak simetris (right-skewed) dan terdapat outlier.
2. Terdapat heteroskedastisitas (varians yang tidak konstan) pada data variabel target (r).
3. Terdapat multikolinearitas pada variabel target (r) dan dari 6 variabel prediktor terdapat 3 yang memiliki multikolinearitas.
4. Pada variabel prediktor (Fn) terdapat pola polinomial yang non linear.

## Penanganan dan Pendekatan Model Bayesian

Untuk mengatasi tantangan tersebut, diterapkan: Dari tantangan data tersebut maka dilakukan penanganan dengan pendekatan bayesian dengan pemilihan Empat model Bayesian dibangun pada data train menggunakan paket brms (MCMC NUTS, 4 rantai, 4000 iterasi dengan 2000 warmup, total 8000 sampel posterior), semuanya dengan efek acak (1 | ID_kapal): 

* **Model Student-t:** efek linear untuk seluruh prediktor termasuk Fn, dengan likelihood Student-t. 
* **Model Spline:** Fn dimodelkan dengan spline s(Fn), likelihood Student-t. 
* **Model Heteroskedastis:** spline s(Fn) pada rata-rata dan pada parameter sigma (sigma ~ s(Fn)), likelihood Student-t. 
* **Model Skew-normal:** struktur sama dengan model heteroskedastis, tetapi dengan likelihood skew-normal. 

Prior yang digunakan bersifat *weakly informative*, yaitu koefisien (b) ~ Normal(0, 1), intercept ~ Student-t(3, 0, 10), simpangan baku efek acak (sd) dan spline (sds) ~ Exponential(1), derajat kebebasan nu ~ Gamma(2, 0,1), serta pada sigma digunakan intercept ~ Student-t(3, 0, 2,5) dan sds ~ Exponential(1)*.

---

## 📊 2. Ringkasan Hasil Evaluasi Model

Pemodelan dilakukan menggunakan **Regresi Bayesian** sebagai model utama, **OLS** sebagai *baseline*, dan **Random Forest** sebagai *benchmark*. Model dilatih pada *data train* dan dievaluasi pada *data test* menggunakan rasio pemisahan (*train-test split*) sebesar **80:20**.

| Model Evaluasi | RMSE | MAE | MAPE (%) | sMAPE (%) | $R^2$ Score |
| :--- | :---: | :---: | :---: | :---: | :---: |
| 1. OLS (*baseline*) | 8,586 | 6,916 | 1559,51 | 100,14 | 0,6659 |
| **2. Regresi Bayesian (Utama)** | **5,918** | **2,299** | **27,51** | **20,16** | **0,8413** |
| 3. Random Forest (*benchmark*) | 3,228 | 2,208 | 472,18 | 60,86 | 0,9528 |

## Catatan
nilai minimum r adalah 0,01 sehingga MAPE sangat sensitif terhadap nilai r yang
kecil. Karena itu sMAPE, RMSE, dan MAE digunakan sebagai pelengkap.

---

## 🗂️ 3. Struktur File dan Direktori

```text
📁 Project Upload/
├── 📄 README.md                                                                # Dokumentasi utama proyek
├── 📄 data_kapal.xls                                                           # Data dictionary ringkas
├── 📄 KODING Rstudio REGRESI BAYESIAN.R                                        # Skrip komputasi R lengkap
├── 📄 Koding R_Bayesian_Revisi.R                                               # Skrip komputasi R lengkap Revisi 
├── 📄 Analisis Awal_Regresi Bayesian_Kelompok 12.doc                           # Analisis Awal_Regresi Bayesian
├── 📄 Analisis Lengkap_Regresi Bayesian_Kelompok 12.html                       # Analisis Lengkap_Regresi Bayesian
├── 📄 Rmarkdown-Regresi-Bayesian_Revisi                                        # Analisis Lengkap_Regresi Bayesian
├── 📄 koding Pre RShiny.R                                                      # Pre Koding Rshiny
├── 📄 koding app Rshiny.R                                                      # Skrip otomatisasi deploy ke ShinyApps.io
├── 📄 HASIL REGRESI BAYESIAN.ppt                                               # Skrip otomatisasi deploy ke ShinyApps.io
├── 📄 LAPORAN HASIL PROJECT_REGRESI BAYESIAN.pdf                               # Laporan lengkap proyek 
└── 📄 Laporan_Hasil_Project_Regresi_Bayesian_Kelompok-12(Revisi).pdf           # Laporan lengkap proyek Revisi
```

---

## 🚀 Panduan Menjalankan Proyek

### **A. Prasyarat Paket R (Prerequisites)**
Sebelum menjalankan proyek, pastikan seluruh paket R yang diperlukan telah terpasang:

```r
install.packages(c(
  "shiny", "bslib", "ggplot2", "dplyr", "tidyr", "readxl", 
  "psych", "lmtest", "car", "randomForest", "brms", "loo", 
  "Metrics", "posterior", "DT", "rsconnect"
))
```

---

### **B. Menjalankan Pre-Komputasi & Aplikasi Secara Lokal**

Proyek ini menggunakan alur eksekusi dua tahap: **Pengolahan Data & Pemodelan Pre-komputasi** (`pree.R`), dilanjutkan dengan **Dashboard Shiny** (`app.R`).

#### **1. Jalankan Skrip Pre-Komputasi (`pree.R`)**
Buka RStudio, arahkan *working directory* ke folder proyek, lalu jalankan skrip pre-komputasi untuk melatih model Bayesian dan memproses data EDA. File hasil komputasi akan disimpan secara otomatis di dalam direktori `hasil_pre/hasil_dashboard.rds`.

```r
# Set direktori kerja ke lokasi proyek Anda
setwd("path/to/your/project")

# Jalankan skrip pemodelan & analisis EDA
source("pree.R")
```

> **Catatan:** Skrip `pree(2).R` dapat digunakan jika Anda ingin memperbarui visualisasi plot EDA secara proporsional tanpa melakukan pemodelan ulang MCMC.

#### **2. Jalankan Dashboard Shiny (`app.R`)**
Setelah proses pre-komputasi selesai dan folder `hasil_pre/` terbentuk, jalankan dashboard dengan perintah berikut:

```r
library(shiny)

# Menjalankan aplikasi
runApp("app.R")
```

---

### **C. Deploy ke ShinyApps.io**

Untuk mempublikasikan aplikasi ke layanan **ShinyApps.io**, pastikan folder `hasil_pre/` beserta file `.rds` di dalamnya terikut dalam proses *deployment*:

```r
library(rsconnect)

# Melakukan deployment aplikasi ke ShinyApps.io
rsconnect::deployApp(
  appDir        = getwd(),
  appPrimaryDoc = "app.R",
  appName       = "bayesian-yacht-hydrodynamics",
  appFiles      = c(
    "app.R",
    "hasil_pre/hasil_dashboard.rds",
    "hasil_pre/model/m_hetero.rds",
    "hasil_pre/model_rf.rds"
  ),
  lint          = FALSE,
  forceUpdate   = TRUE
)
```

---

## 🌐 Live Demo Dashboard

Akses aplikasi interaktif yang telah di-deploy secara langsung melalui tautan berikut:  
👉 [https://regresibayesian.shinyapps.io/RegresiBayesian//)

---

## 🔬 5. Fitur Interaktif Dashboard
## 📌 Deskripsi Tab Dashboard

### **Tab 1: EDA (Exploratory Data Analysis)**
* **Eksplorasi Deskriptif & Distribusi:** Menampilkan tabel statistik deskriptif, pemeriksaan *missing values*, analisis *outlier* (IQR), dan matriks korelasi Spearman.
* **Uji Asumsi Klasik:** Diagnostik multikolinearitas (VIF) dan uji heteroskedastisitas Breusch-Pagan.
* **Visualisasi Interaktif:** Histogram dengan Kurva Densitas, *Boxplot Distribution*, dan *Scatterplot* hubungan prediktor terhadap hambatan kapal ($r$).

### **Tab 2: Bayesian & Evaluasi Model**
* **Diagnosis & Penanganan Bayesian:** Pemetaan tantangan data (skewness, heteroskedastisitas, nonlinearitas $Fn$) terhadap penanganan statistik Bayesian.
* **Model Comparison (LOO-CV):** Perbandingan performa *Leave-One-Out Cross-Validation* dari 4 variasi model Bayesian (`m_student`, `m_spline`, `m_hetero`, dan `m_skew`).
* **Ringkasan Parameter Posterior:** Tabel estimasi parameter posterior, kesalahan standar, dan interval kredibel 95%.
* **Evaluasi & Perbandingan Model:** Tabel metrik performa (*RMSE, MAE, MAPE, sMAPE, R²*) antara **OLS Baseline**, **Regresi Bayesian**, dan **Random Forest Benchmark**, dilengkapi dengan plot perbandingan prediksi pada *Test Set*.

### **Tab 3: Simulasi & Prediksi**
* **Panel Parameter Desain Interaktif:** Pengaturan dinamis nilai variabel prediktor geometri lambung (`LCB`, `PC`, `LDR`, `BDR`, `LBR`) dan tingkat kecepatan (`Fn`).
* **Kalkulasi Real-time:** Estimasi prediksi nilai hambatan sisa kapal secara instan menggunakan sampel posterior model Bayesian (lengkap dengan *95% Credible Interval*) serta estimasi *Random Forest*.

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
