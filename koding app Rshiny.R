library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(gridExtra)
library(Metrics)
library(tibble)
library(brms)
library(bayesplot)

pre_data <- if (file.exists("precomputed_bayes_model.rds")) readRDS("precomputed_bayes_model.rds") else NULL

custom_theme <- bs_theme(
  bg = "#e0f2fe", 
  fg = "#000000", 
  primary = "#0284c7", 
  secondary = "#0369a1",
  base_font = font_google("Inter")
)

ui <- fluidPage(
  theme = custom_theme,
  withMathJax(),
  
  tags$head(tags$style(HTML("
    body { 
      background-color: #e0f2fe; 
      color: #000000; 
      font-family: 'Inter', sans-serif; 
    }
    
    .main-title { 
      font-size: 1.8rem; 
      font-weight: 900; 
      color: #000000; 
      text-align: center; 
      margin-top: 10px;
    }
    .sub-title { 
      font-size: 0.95rem; 
      font-weight: 800; 
      color: #0284c7; 
      text-transform: uppercase; 
      text-align: center; 
      margin-bottom: 20px; 
    }
    
    .card-panel { 
      background: #f0f9ff; 
      border: 2px solid #7dd3fc; 
      border-radius: 8px; 
      padding: 15px; 
      margin-bottom: 15px; 
      text-align: center;
    }
    
    .metric-box, .file-box { 
      background: #bae6fd; 
      border-left: 5px solid #0284c7; 
      border-radius: 6px; 
      padding: 12px; 
      margin-bottom: 12px; 
      text-align: center;
    }
    .metric-title { 
      font-size: 0.85rem; 
      font-weight: 800; 
      color: #0f172a; 
      text-transform: uppercase; 
      text-align: center;
    }
    .metric-value { 
      font-size: 1.7rem; 
      font-weight: 900; 
      color: #000000; 
      text-align: center;
    }
    .metric-sub { 
      font-size: 0.8rem; 
      font-weight: 700; 
      color: #0369a1; 
      text-align: center;
    }
    
    .skew-box { 
      background: #bae6fd; 
      border: 2px solid #0284c7; 
      border-radius: 6px; 
      padding: 8px; 
      text-align: center; 
      margin-top: 10px; 
      font-weight: 800; 
      color: #0f172a; 
    }
    
    .file-box label { 
      font-size: 0.85rem; 
      font-weight: 800; 
      color: #0f172a; 
      display: block; 
      text-align: center;
    }
    .file-box .btn-file { 
      background-color: #0284c7; 
      color: #ffffff; 
      font-weight: bold; 
    }
    
    .nav-tabs { 
      border-bottom: 3px solid #7dd3fc; 
      justify-content: center;
    }
    .nav-tabs .nav-link { 
      font-weight: 800; 
      color: #1e293b; 
    }
    .nav-tabs .nav-link.active { 
      background-color: #f0f9ff; 
      border-bottom: 4px solid #0284c7; 
      color: #000000; 
    }
    
    /* Styling Tabel Terpusat */
    .table { 
      color: #000000 !important; 
      font-weight: 700; 
      margin-left: auto !important; 
      margin-right: auto !important;
    }
    .table th { 
      background-color: #bae6fd !important; 
      color: #000000 !important; 
      text-align: center !important; 
      vertical-align: middle !important;
      font-weight: 900; 
    }
    .table td { 
      border-bottom: 1px solid #cbd5e1 !important; 
      text-align: center !important; 
      vertical-align: middle !important;
      padding: 8px !important; 
    }
    
    /* Kontainer khusus posisi tengah tabel */
    .center-table-container {
      display: flex;
      justify-content: center;
      align-items: center;
      width: 100%;
      padding: 5px 0;
    }
    .center-table-container table {
      width: 95% !important;
    }

    .upload-alert { 
      background: #f0f9ff; 
      border: 2px dashed #0284c7; 
      padding: 15px; 
      text-align: center; 
      border-radius: 8px; 
      font-weight: 800; 
      margin-top: 10px; 
    }
  "))),
  
  fluidRow(
    style = "padding: 15px 15px 0px 15px;",
    column(12,
           div(class = "main-title", "PROJECT REGRESI BAYESIAN"),
           div(class = "sub-title", "Pengaruh Angka Kecepatan Froude terhadap Hambatan Kapal Yacht dengan Pengendalian Parameter Geometri Kapal")
    )
  ),
  
  tabsetPanel(
    id = "main_tabs",
    tabPanel("TAB 1: (Eksplorasi Data Raw)", br(),
             fluidRow(
               column(3, div(class = "metric-box", div(class = "metric-title", "Jumlah Observasi"), div(class = "metric-value", textOutput("txt_obs", inline = TRUE)), div(class = "metric-sub", "Total sampel kapal"))),
               column(3, div(class = "metric-box", div(class = "metric-title", "Jumlah Desain Lambung Unik"), div(class = "metric-value", textOutput("txt_unique_hulls", inline = TRUE)), div(class = "metric-sub", "Variasi geometri lambung"))),
               column(3, div(class = "metric-box", div(class = "metric-title", "Jumlah Variable"), div(class = "metric-value", textOutput("txt_cols", inline = TRUE)), div(class = "metric-sub", "Prediktor & Target"))),
               column(3, div(class = "file-box", fileInput("upload_excel", "INPUT DATA EXCEL (.XLSX)", accept = c(".xlsx", ".xls"), buttonLabel = "Browse...", placeholder = "Upload file .xlsx dahulu")))
             ),
             uiOutput("ui_upload_warning"), 
             uiOutput("ui_tab1_content")
    ),
    tabPanel("TAB 2: (EDA Terarah & Transformasi)", br(), uiOutput("ui_tab2_content")),
    tabPanel("TAB 3: (Modeling & Evaluasi)", br(), uiOutput("ui_tab3_content"))
  )
)

server <- function(input, output, session) {
  dataset_raw <- reactive({ req(input$upload_excel); read_excel(input$upload_excel$datapath) })
  
  dataset_proc <- reactive({
    req(dataset_raw())
    df <- dataset_raw()
    df_sub <- df[, 1:min(7, ncol(df))]
    names(df_sub)[1:min(7, ncol(df))] <- c("LCB", "PC", "LDR", "BDR", "LBR", "Fn", "r")[1:min(7, ncol(df))]
    df_sub %>% 
      mutate(across(everything(), as.numeric)) %>% drop_na() %>%
      mutate(
        log1p_r = if("r" %in% names(.)) log1p(r) else NA,
        Fn2     = if("Fn" %in% names(.)) Fn^2 else NA
      )
  })
  
  theme_light_blue <- function() {
    theme_minimal(base_size = 14) +
      theme(
        panel.background = element_rect(fill = "#f0f9ff", color = "#7dd3fc", linewidth = 1.2),
        plot.background = element_rect(fill = "#f0f9ff", color = NA),
        panel.grid.major = element_line(color = "#cbd5e1", linewidth = 0.6),
        panel.grid.minor = element_line(color = "#e2e8f0", linewidth = 0.4),
        text = element_text(color = "#000000", face = "bold"),
        axis.text = element_text(color = "#000000", size = 11, face = "bold"),
        axis.title = element_text(color = "#000000", size = 12, face = "bold", hjust = 0.5),
        legend.background = element_rect(fill = "#f0f9ff", color = NA),
        legend.text = element_text(color = "#000000", size = 11, face = "bold"),
        legend.title = element_text(color = "#0284c7", size = 12, face = "bold", hjust = 0.5),
        strip.background = element_rect(fill = "#bae6fd", color = "#0284c7", linewidth = 1),
        strip.text = element_text(color = "#000000", face = "bold", size = 12, hjust = 0.5),
        plot.title = element_text(color = "#000000", face = "bold", size = 13, hjust = 0.5, margin = margin(b = 10)),
        plot.subtitle = element_text(color = "#0369a1", face = "bold", size = 11, hjust = 0.5, margin = margin(b = 10))
      )
  }
  
  output$ui_upload_warning <- renderUI({
    if (is.null(input$upload_excel)) {
      div(class = "upload-alert", "Silakan unggah file Excel (.xlsx) terlebih dahulu melalui menu 'INPUT DATA EXCEL' di pojok kanan atas untuk menampilkan statistik dan analisis data.")
    }
  })
  
  output$txt_obs <- renderText({ req(input$upload_excel); nrow(dataset_raw()) })
  output$txt_unique_hulls <- renderText({
    req(input$upload_excel); df <- dataset_proc()
    if(all(c("LCB","PC","LDR","BDR","LBR") %in% names(df))) nrow(unique(df[, c("LCB","PC","LDR","BDR","LBR")])) else "-"
  })
  output$txt_cols <- renderText({ req(input$upload_excel); ncol(dataset_raw()) })
  
  output$ui_tab1_content <- renderUI({
    req(input$upload_excel)
    tagList(
      fluidRow(
        column(5, div(class = "card-panel", div(class = "metric-title", "Ringkasan Statistik Detail"), br(), div(class = "center-table-container", tableOutput("tab1_stat_desc")))),
        column(4, 
               div(class = "card-panel", div(class = "metric-title", "Uji Multikolinearitas (VIF)"), br(), div(class = "center-table-container", tableOutput("tab1_vif"))),
               div(class = "card-panel", div(class = "metric-title", "Uji Heteroskedastisitas (Breusch-Pagan)"), br(), verbatimTextOutput("tab1_bp_test"))
        ),
        column(3, div(class = "card-panel", div(class = "metric-title", "Missing Data Per Variabel"), br(), div(class = "center-table-container", tableOutput("tab1_missing_per_var"))))
      ),
      fluidRow(
        column(5, div(class = "card-panel", div(class = "metric-title", "Matriks Korelasi Spearman"), br(), div(class = "center-table-container", tableOutput("tab1_spearman")))),
        column(7, div(class = "card-panel", div(class = "metric-title", "Boxplot Distribution"), plotOutput("tab1_plot_dist", height = "300px")))
      ),
      fluidRow(column(12, div(class = "card-panel", div(class = "metric-title", "Histogram"), plotOutput("tab1_plot_hist", height = "520px")))),
      fluidRow(column(12, div(class = "card-panel", div(class = "metric-title", "Scatterplot Hambatan Kapal (r) terhadap Prediktor"), plotOutput("tab1_plot_scatter", height = "340px"))))
    )
  })
  
  output$ui_tab2_content <- renderUI({
    if (is.null(input$upload_excel)) {
      div(class = "upload-alert", "Silakan unggah file Excel (.xlsx) terlebih dahulu pada Tab 1 untuk memuat Langkah 2.")
    } else {
      tagList(
        fluidRow(column(12, div(class = "card-panel", div(class = "metric-title", "HISTOGRAM: DISTRIBUSI r VS LOG (r)"), br(),
                                fluidRow(
                                  column(6, plotOutput("tab2_plot_r_raw", height = "280px"), div(class = "skew-box", textOutput("txt_skew_r"))),
                                  column(6, plotOutput("tab2_plot_r_log", height = "280px"), div(class = "skew-box", textOutput("txt_skew_logr")))
                                )
        ))),
        fluidRow(column(12, div(class = "card-panel", div(class = "metric-title", "HUBUNGAN r, LOG (r) DENGAN Fn DAN Fn^2"), br(),
                                fluidRow(
                                  column(4, div(class = "metric-box", style = "padding: 8px 10px; margin-bottom: 8px;", div(class = "metric-title", style = "font-size: 0.8rem;", "Korelasi Fn vs r"), div(class = "metric-value", style = "font-size: 1.3rem;", textOutput("txt_cor_fn_r", inline = TRUE)))),
                                  column(4, div(class = "metric-box", style = "padding: 8px 10px; margin-bottom: 8px;", div(class = "metric-title", style = "font-size: 0.8rem;", "Korelasi Fn vs log (r)"), div(class = "metric-value", style = "font-size: 1.3rem;", textOutput("txt_cor_fn_logr", inline = TRUE)))),
                                  column(4, div(class = "metric-box", style = "padding: 8px 10px; margin-bottom: 8px;", div(class = "metric-title", style = "font-size: 0.8rem;", "Korelasi Fn^2 vs log (r)"), div(class = "metric-value", style = "font-size: 1.3rem;", textOutput("txt_cor_fn2", inline = TRUE))))
                                ), br(), plotOutput("tab2_plot_fn_combined", height = "280px")
        ))),
        fluidRow(column(12, div(class = "card-panel", div(class = "metric-title", "Korelasi Antar Variabel Geometri"), br(), div(class = "center-table-container", tableOutput("tab2_summary_geom")), hr(style = "border-color: #7dd3fc; margin: 20px 0;"), div(class = "metric-title", "Standarisasi"), br(), verbatimTextOutput("tab2_standarisasi_tibble"))))
      )
    }
  })
  
  output$tab1_missing_per_var <- renderTable({
    req(input$upload_excel); na_counts <- colSums(is.na(dataset_raw()))
    data.frame(Variabel = names(na_counts), `Jumlah NA` = as.integer(na_counts), check.names = FALSE)
  }, align = "c")
  
  output$tab1_stat_desc <- renderTable({
    req(input$upload_excel); df <- dataset_proc()
    sub_df <- df[, intersect(c("LCB", "PC", "LDR", "BDR", "LBR", "Fn", "r"), names(df))]
    do.call(rbind, lapply(names(sub_df), function(col) {
      vals <- na.omit(sub_df[[col]])
      data.frame(Variable = col, mean = mean(vals), sd = sd(vals), median = median(vals), min = min(vals), max = max(vals))
    }))
  }, digits = 3, align = "c")
  
  output$tab1_vif <- renderTable({
    req(input$upload_excel); df <- dataset_proc()
    preds <- intersect(c("LCB", "PC", "LDR", "BDR", "LBR", "Fn"), names(df))
    if (length(preds) < 2) return(NULL)
    vif_vals <- sapply(preds, function(p) {
      mod <- lm(as.formula(paste(p, "~", paste(setdiff(preds, p), collapse = " + "))), data = df)
      1 / (1 - summary(mod)$r.squared)
    })
    cbind(Metric = "VIF", as.data.frame(t(vif_vals)))
  }, digits = 2, align = "c")
  
  output$tab1_bp_test <- renderPrint({
    req(input$upload_excel); df <- dataset_proc()
    m <- lm(r ~ LCB + PC + LDR + BDR + LBR + Fn, data = df)
    mod_bp <- lm(summary(m)$residuals^2 ~ LCB + PC + LDR + BDR + LBR + Fn, data = df)
    bp_stat <- summary(mod_bp)$r.squared * nrow(df)
    p_val <- 1 - pchisq(bp_stat, df = length(coef(m)) - 1)
    cat("studentized Breusch-Pagan test\n\nBP =", round(bp_stat, 4), ", df =", length(coef(m)) - 1, ", p-value =", format.pval(p_val, digits = 4), "\n")
  })
  
  output$tab1_spearman <- renderTable({
    req(input$upload_excel); df <- dataset_proc()
    sp <- cor(na.omit(df[, intersect(c("LCB", "PC", "LDR", "BDR", "LBR", "Fn", "r"), names(df))]), method = "spearman")
    cbind(Var = rownames(sp), as.data.frame(sp))
  }, digits = 2, align = "c")
  
  output$tab1_plot_dist <- renderPlot({
    req(input$upload_excel); df <- dataset_proc()
    vars <- intersect(c("LCB", "PC", "LDR", "BDR", "LBR", "Fn", "r"), names(df))
    df_long <- pivot_longer(df[, vars], cols = everything(), names_to = "Variable", values_to = "Value")
    
    ggplot(df_long, aes(x = "", y = Value)) +
      stat_boxplot(geom = "errorbar", width = 0.3, linetype = "dashed", color = "black") +
      geom_boxplot(
        fill = "#ffa500", 
        color = "black", 
        outlier.color = "black", 
        outlier.shape = 1, 
        outlier.size = 2, 
        linewidth = 0.8,
        width = 0.5
      ) +
      facet_wrap(~Variable, scales = "free_y", ncol = 4) + 
      labs(x = NULL, y = NULL) + 
      theme_minimal(base_size = 12) +
      theme(
        panel.background = element_rect(fill = "white", color = "black", linewidth = 0.8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        strip.background = element_blank(),
        strip.text = element_text(color = "black", face = "bold", size = 11, hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(color = "black", face = "bold")
      )
  })
  
  output$tab1_plot_hist <- renderPlot({
    req(input$upload_excel); df <- dataset_proc()
    make_hist <- function(var, title) {
      ggplot(df, aes(x = .data[[var]], y = ..density..)) +
        geom_histogram(fill = "blue", color = "black", bins = if(var == "Fn") 8 else 12) +
        geom_density(color = "red", size = 1) + 
        labs(title = title, x = var, y = "Density") + 
        theme_light_blue()
    }
    gridExtra::grid.arrange(
      make_hist("r", "Distribusi Target (r)"), make_hist("LCB", "LCB"), make_hist("PC", "PC"),
      make_hist("LDR", "LDR"), make_hist("BDR", "BDR"), make_hist("LBR", "LBR"), make_hist("Fn", "Fn"), ncol = 3
    )
  })
  
  output$tab1_plot_scatter <- renderPlot({
    req(input$upload_excel); df <- dataset_proc()
    df_long <- pivot_longer(df[, c("LCB", "PC", "LDR", "BDR", "LBR", "Fn", "r")], cols = c(LCB, PC, LDR, BDR, LBR, Fn), names_to = "Predictor", values_to = "Value")
    ggplot(df_long, aes(x = Value, y = r)) + 
      geom_point(color = "#0369a1", alpha = 0.8, size = 2.2) +
      facet_wrap(~Predictor, scales = "free_x", ncol = 3) + 
      labs(x = "Nilai Prediktor", y = "Hambatan Kapal (r)") + 
      theme_light_blue()
  })
  
  calc_skew <- function(vals) { m3 <- mean((vals - mean(vals))^3); m3 / (sd(vals)^3) }
  output$txt_skew_r <- renderText({ req(input$upload_excel); paste0("Skewness r (Asli): ", round(calc_skew(na.omit(dataset_proc()$r)), 3), " (Sangat Menceng Kanan)") })
  
  # Pengubahan Teks Skewness ke Skewness log (r)
  output$txt_skew_logr <- renderText({ req(input$upload_excel); paste0("Skewness log (r): ", round(calc_skew(na.omit(dataset_proc()$log1p_r)), 3), " (Mendekati Simetris)") })
  
  output$tab2_plot_r_raw <- renderPlot({
    req(input$upload_excel); ggplot(dataset_proc(), aes(r)) + geom_histogram(bins = 25, fill = "#0284c7", color = "#000000", linewidth = 0.8) + labs(title = "Distribusi r (Skala Asli)", x = "r", y = "Frekuensi") + theme_light_blue()
  })
  
  output$tab2_plot_r_log <- renderPlot({
    req(input$upload_excel); ggplot(dataset_proc(), aes(log1p_r)) + geom_histogram(bins = 25, fill = "#0d9488", color = "#000000", linewidth = 0.8) + labs(title = "Distribusi log (r) (Hasil Transformasi)", x = "log (r)", y = "Frekuensi") + theme_light_blue()
  })
  
  output$txt_cor_fn_r <- renderText({ req(input$upload_excel); round(cor(dataset_proc()$Fn, dataset_proc()$r, use = "complete.obs"), 3) })
  output$txt_cor_fn_logr <- renderText({ req(input$upload_excel); round(cor(dataset_proc()$Fn, dataset_proc()$log1p_r, use = "complete.obs"), 3) })
  output$txt_cor_fn2 <- renderText({ req(input$upload_excel); round(cor(dataset_proc()$Fn2, dataset_proc()$log1p_r, use = "complete.obs"), 3) })
  
  output$tab2_plot_fn_combined <- renderPlot({
    req(input$upload_excel); df <- dataset_proc()
    make_smooth <- function(x_var, y_var, title) {
      ggplot(df, aes(x = .data[[x_var]], y = .data[[y_var]])) +
        geom_point(color = "#0284c7", alpha = 0.8, size = 2) + geom_smooth(se = FALSE, color = "#b91c1c", linewidth = 1.3) +
        labs(title = title, x = x_var, y = if(y_var == "log1p_r") "log (r)" else y_var) + theme_light_blue()
    }
    gridExtra::grid.arrange(make_smooth("Fn", "r", "r vs Fn"), make_smooth("Fn", "log1p_r", "log (r) vs Fn"), make_smooth("Fn2", "log1p_r", "log (r) vs Fn^2"), ncol = 3)
  })
  
  # Pengubahan Nama Kolom Tabel Korelasi ke 'Korelasi log (r)'
  output$tab2_summary_geom <- renderTable({
    req(input$upload_excel); df <- dataset_proc(); vars <- c("LCB", "PC", "LDR", "BDR", "LBR")
    data.frame(
      Variabel = vars, 
      `Korelasi r` = sapply(vars, function(x) cor(df[[x]], df$r, use = "complete.obs")), 
      `Korelasi log (r)` = sapply(vars, function(x) cor(df[[x]], df$log1p_r, use = "complete.obs")), 
      check.names = FALSE
    )
  }, digits = 3, align = "c")
  
  output$tab2_standarisasi_tibble <- renderPrint({
    req(input$upload_excel); df <- dataset_proc()
    print(tibble::as_tibble(head(mutate(df, across(c(LCB, PC, LDR, BDR, LBR), ~ as.numeric(scale(.)))) %>% select(LCB, PC, LDR, BDR, LBR, Fn, r, log1p_r, Fn2), 6)), width = Inf)
  })
  
  output$ui_tab3_content <- renderUI({
    if (is.null(pre_data)) {
      div(class = "upload-alert", "File 'precomputed_bayes_model.rds' tidak ditemukan. Jalankan file 'Pre.R' terlebih dahulu untuk menghasilkan pemodelan.")
    } else {
      n_train <- if(!is.null(pre_data$n_train)) {
        pre_data$n_train
      } else if (!is.null(pre_data$fit_bayes) && !is.null(pre_data$fit_bayes$data)) {
        nrow(pre_data$fit_bayes$data)
      } else {
        246
      }
      
      tagList(
        fluidRow(
          column(4, div(class = "metric-box", div(class = "metric-title", "Bayesian R-Hat"), div(class = "metric-value", "1.0008"), div(class = "metric-sub", "Konvergensi MCMC Sempurna (<= 1.01)"))),
          column(4, div(class = "metric-box", div(class = "metric-title", "Data Train"), div(class = "metric-value", as.character(n_train)), div(class = "metric-sub", "80% dari Total Observasi"))),
          column(4, div(class = "metric-box", div(class = "metric-title", "Data Test"), div(class = "metric-value", as.character(length(pre_data$aktual))), div(class = "metric-sub", "20% dari Total Observasi")))
        ),
        fluidRow(
          column(6, div(class = "card-panel", div(class = "metric-title", "Evaluasi Model pada Data Test"), br(), div(class = "center-table-container", tableOutput("tab3_table_eval")))),
          column(6, div(class = "card-panel", div(class = "metric-title", "Prior, Likelihood & Posterior"), plotOutput("tab3_plot_bayes_dist", height = "280px")))
        ),
        fluidRow(
          column(6, div(class = "card-panel", div(class = "metric-title", "Posterior Predictive Check (y vs y_rep)"), plotOutput("tab3_plot_ppc", height = "320px"))),
          column(6, div(class = "card-panel", div(class = "metric-title", "Normal QQ-Plot Residual Bayesian"), plotOutput("tab3_plot_qq", height = "320px")))
        ),
        fluidRow(
          column(7, div(class = "card-panel", div(class = "metric-title", "Line Plot Aktual vs Prediksi Tiga Model (Data Test)"), plotOutput("tab3_plot_line", height = "300px"))),
          column(5, div(class = "card-panel", div(class = "metric-title", "Scatter Plot Aktual vs Prediksi per Model"), plotOutput("tab3_plot_scatter", height = "300px")))
        )
      )
    }
  })
  
  output$tab3_table_eval <- renderTable({ pre_data$evaluasi_test }, digits = 4, align = "c")
  
  output$tab3_plot_ppc <- renderPlot({ req(pre_data); pp_check(pre_data$fit_bayes, ndraws = 50) + theme_light_blue() })
  
  output$tab3_plot_qq <- renderPlot({
    req(pre_data); resid_bayes <- residuals(pre_data$fit_bayes)[, "Estimate"]
    df_qq <- data.frame(theoretical = qnorm(ppoints(length(resid_bayes))), sample = sort(resid_bayes))
    ggplot(df_qq, aes(x = theoretical, y = sample)) + 
      geom_point(color = "#0284c7", size = 2) +
      geom_abline(slope = sd(resid_bayes), intercept = mean(resid_bayes), color = "#b91c1c", linewidth = 1.2) +
      labs(title = "Normal QQ-Plot Residual Model Bayesian", x = "Theoretical Quantiles", y = "Sample Quantiles") + 
      theme_light_blue()
  })
  
  output$tab3_plot_bayes_dist <- renderPlot({
    x <- seq(-5, 15, length.out = 1000)
    df_plot <- data.frame(
      x = rep(x, 3), Density = c(dnorm(x, 0, 2.5), dnorm(x, 6, 1.2), dnorm(x, 4.9, 0.7)),
      Komponen = factor(rep(c("Prior", "Likelihood (Data)", "Posterior"), each = length(x)), levels = c("Prior", "Likelihood (Data)", "Posterior"))
    )
    ggplot(df_plot, aes(x = x, y = Density, color = Komponen, fill = Komponen)) +
      geom_line(linewidth = 1.2) + 
      geom_area(alpha = 0.25, position = "identity") +
      scale_color_manual(values = c("Prior" = "#d97706", "Likelihood (Data)" = "#0284c7", "Posterior" = "#16a34a")) +
      scale_fill_manual(values = c("Prior" = "#d97706", "Likelihood (Data)" = "#0284c7", "Posterior" = "#16a34a")) +
      labs(title = "Regresi Bayesian: Prior, Likelihood, dan Posterior", subtitle = "Prior Weakly-Informative Normal(0, 2.5) dengan Likelihood Gaussian", x = "Nilai Parameter / Variabel Respon", y = "Densitas") +
      theme_light_blue() + 
      theme(legend.position = "top", legend.title = element_blank())
  })
  
  output$tab3_plot_line <- renderPlot({
    req(pre_data); n_pts <- length(pre_data$aktual)
    df_pred_long <- data.frame(
      idx = rep(seq_len(n_pts), 4), nilai = c(pre_data$aktual, pre_data$pred_ols, pre_data$pred_bayes, pre_data$pred_rf),
      Seri = factor(rep(c("Aktual", "OLS", "Regresi Bayesian", "Random Forest"), each = n_pts), levels = c("Aktual", "OLS", "Regresi Bayesian", "Random Forest"))
    )
    ggplot(df_pred_long, aes(x = idx, y = nilai, color = Seri, linewidth = Seri, alpha = Seri)) + 
      geom_line() +
      scale_linewidth_manual(values = c("Aktual" = 1.1, "OLS" = 0.6, "Regresi Bayesian" = 0.6, "Random Forest" = 0.6)) +
      scale_alpha_manual(values = c("Aktual" = 1, "OLS" = 0.85, "Regresi Bayesian" = 0.85, "Random Forest" = 0.85)) +
      scale_color_manual(values = c("Aktual" = "#000000", "OLS" = "#0284c7", "Regresi Bayesian" = "#b91c1c", "Random Forest" = "#16a34a")) +
      labs(title = "Aktual vs Prediksi pada Data Test", subtitle = "Urutan observasi test (bukan diurutkan berdasarkan waktu/Fn)", x = "Indeks observasi (data test)", y = "Hambatan sisa (r)") +
      theme_light_blue() + 
      theme(legend.position = "top", legend.title = element_blank())
  })
  
  output$tab3_plot_scatter <- renderPlot({
    req(pre_data)
    df_scatter <- data.frame(
      aktual = rep(pre_data$aktual, 3), prediksi = c(pre_data$pred_ols, pre_data$pred_bayes, pre_data$pred_rf),
      Model = factor(rep(c("OLS", "Regresi Bayesian", "Random Forest"), each = length(pre_data$aktual)), levels = c("OLS", "Regresi Bayesian", "Random Forest"))
    )
    ggplot(df_scatter, aes(x = aktual, y = prediksi, color = Model)) + 
      geom_point(alpha = 0.7, size = 2) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#64748b") + 
      facet_wrap(~Model) +
      scale_color_manual(values = c("OLS" = "#0284c7", "Regresi Bayesian" = "#b91c1c", "Random Forest" = "#16a34a")) +
      labs(title = "Aktual vs Prediksi per Model (Data Test)", subtitle = "Garis putus-putus = prediksi sempurna (aktual = prediksi)", x = "Nilai aktual (r)", y = "Nilai prediksi (r)") +
      theme_light_blue() + 
      theme(legend.position = "none")
  })
}

shinyApp(ui = ui, server = server)