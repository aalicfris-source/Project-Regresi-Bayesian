library(shiny)
library(bslib)
library(ggplot2)
library(DT)
library(brms)
library(randomForest)

OUT_DIR <- "hasil_pre"
hasil <- readRDS(file.path(OUT_DIR, "hasil_dashboard.rds"))

custom_theme <- bs_theme(
  version = 5,
  bootswatch = "darkly",
  primary = "#0dcaf0",
  secondary = "#20c997",
  success = "#198754",
  bg = "#0d1b2a",
  fg = "#e0e1dd",
  "card-bg" = "#1b263b",
  "font-sans-serif" = "Inter, sans-serif"
)

ui <- page_sidebar(
  theme = custom_theme,
  title = div(tags$b("DASHBOARD HAMBATAN SISA KAPAL"), tags$span(" | Bayesian & ML", class="text-info fs-6")),
  sidebar = sidebar(
    width = 280,
    bg = "#1b263b",
    h5("Navigasi Kontrol", class = "text-info fw-bold mb-3"),
    tags$hr(style = "border-color: #415a77;"),
    conditionalPanel(
      condition = "input.tabs == 'simulasi'",
      div(
        class = "p-2 bg-dark rounded mb-3 border border-secondary",
        h6("Parameter Desain Baru", class = "text-info fw-bold"),
        sliderInput("sim_LCB", "LCB:", min = hasil$skala$min[1], max = hasil$skala$max[1], value = hasil$skala$mean[1]),
        sliderInput("sim_PC", "PC:", min = hasil$skala$min[2], max = hasil$skala$max[2], value = hasil$skala$mean[2]),
        sliderInput("sim_LDR", "LDR:", min = hasil$skala$min[3], max = hasil$skala$max[3], value = hasil$skala$mean[3]),
        sliderInput("sim_BDR", "BDR:", min = hasil$skala$min[4], max = hasil$skala$max[4], value = hasil$skala$mean[4]),
        sliderInput("sim_LBR", "LBR:", min = hasil$skala$min[5], max = hasil$skala$max[5], value = hasil$skala$mean[5]),
        sliderInput("sim_Fn", "Fn (Froude):", min = hasil$skala$min[6], max = hasil$skala$max[6], value = hasil$skala$mean[6], step = 0.005),
        actionButton("btn_run", "Hitung Prediksi", class = "btn-info w-100 fw-bold mt-2")
      )
    ),
    div(
      class = "mt-auto p-3 rounded", style = "background: #0d1b2a; border: 1px solid #415a77;",
      h6("Status Sistem", class = "text-success fw-bold"),
      tags$small(paste("Observasi:", hasil$meta$n_obs)), br(),
      tags$small(paste("Desain Unik:", hasil$meta$n_desain)), br(),
      tags$small(paste("Max R-hat:", round(hasil$meta$rhat_max, 3)))
    )
  ),
  
  navset_card_pill(
    id = "tabs",
    nav_panel("EDA",
              div(style = "width: 100%; max-width: 100%;",
                  layout_columns(
                    col_widths = c(12, 12),
                    card(card_header("Statistik Deskriptif"), div(style = "width: 100%; overflow-x: auto;", DTOutput("tbl_stat"))),
                    card(card_header("Cek Missing Value"), div(style = "width: 100%; overflow-x: auto;", DTOutput("tbl_missing")))
                  ),
                  layout_columns(
                    col_widths = c(12, 12),
                    card(card_header("Analisis Outlier (IQR)"), div(style = "width: 100%; overflow-x: auto;", DTOutput("tbl_outlier"))),
                    card(card_header("Multikolinearitas (VIF)"), div(style = "width: 100%; overflow-x: auto;", DTOutput("tbl_vif")))
                  ),
                  layout_columns(
                    col_widths = c(12, 12),
                    card(card_header("Breusch-Pagan (Heteroskedastisitas)"), div(style = "width: 100%; overflow-x: auto;", DTOutput("tbl_bp"))),
                    card(card_header("Matriks Korelasi Spearman"), div(style = "width: 100%; overflow-x: auto;", DTOutput("tbl_spearman")))
                  ),
                  layout_columns(
                    col_widths = c(12, 12),
                    card(card_header("HISTOGRAM"), div(style = "width: 100%; overflow-x: auto;", plotOutput("plot_hist", height = "450px"))),
                    card(card_header("BOXPLOT DISTRIBUTION"), div(style = "width: 100%; overflow-x: auto;", plotOutput("plot_box", height = "450px")))
                  ),
                  layout_columns(
                    col_widths = c(12),
                    card(card_header("SCATTERPLOT HAMBATAN KAPAL (R) TERHADAP PREDIKTOR"), div(style = "width: 100%; overflow-x: auto;", plotOutput("plot_scatter", height = "450px")))
                  )
              )
    ),
    nav_panel("2. Bayesian & Evaluasi Model",
              layout_columns(
                card(card_header("Diagnosis Masalah & Penanganan Bayesian"), DTOutput("tbl_diag")),
                card(card_header("Perbandingan LOO Model Bayesian (4 Model)"), DTOutput("tbl_loo"))
              ),
              card(card_header("Ringkasan Parameter Posterior (Model Terpilih)"), DTOutput("tbl_bayes")),
              layout_columns(
                card(card_header("Tabel Evaluasi Model pada Data Test"), DTOutput("tbl_eval")),
                card(card_header("Ringkasan Performa Metrik"), verbatimTextOutput("txt_eval_info"))
              ),
              card(card_header("Grafik Perbandingan Aktual vs Prediksi (Test Set)"), plotOutput("plot_test", height = "350px"))
    ),
    nav_panel("3. Simulasi & Prediksi", value = "simulasi",
              layout_columns(
                col_widths = c(12),
                card(card_header("Hasil Prediksi Desain Kapal Baru", class = "bg-info text-dark"), verbatimTextOutput("txt_sim_hasil"))
              )
    )
  )
)

server <- function(input, output, session) {
  output$tbl_stat <- renderDT({ datatable(hasil$eda$stat_desc, options = list(pageLength = 4, scrollX = TRUE), rownames = FALSE) })
  output$tbl_missing <- renderDT({ datatable(hasil$eda$missing, options = list(pageLength = 4, scrollX = TRUE), rownames = FALSE) })
  output$tbl_outlier <- renderDT({ datatable(hasil$eda$outlier, options = list(pageLength = 4, scrollX = TRUE), rownames = FALSE) })
  output$tbl_vif <- renderDT({ datatable(hasil$eda$vif, options = list(pageLength = 4, scrollX = TRUE), rownames = FALSE) })
  output$tbl_bp <- renderDT({ datatable(hasil$eda$bp, options = list(pageLength = 2, scrollX = TRUE), rownames = FALSE) })
  
  output$tbl_spearman <- renderDT({ datatable(as.data.frame(hasil$eda$spearman), options = list(scrollX = TRUE)) })
  
  output$plot_hist <- renderPlot({ hasil$eda$plot_hist })
  output$plot_box <- renderPlot({ hasil$eda$plot_box })
  output$plot_scatter <- renderPlot({ hasil$eda$plot_scatter })
  
  output$tbl_diag <- renderDT({ datatable(hasil$bayes$diagnosis, options = list(pageLength = 4), rownames = FALSE) })
  output$tbl_loo <- renderDT({ datatable(hasil$bayes$loo, options = list(pageLength = 4), rownames = FALSE) })
  output$tbl_bayes <- renderDT({ datatable(hasil$bayes$post_summary, options = list(pageLength = 5), rownames = FALSE) })
  output$tbl_eval <- renderDT({ datatable(hasil$evaluasi, options = list(pageLength = 3), rownames = FALSE) })
  
  output$txt_eval_info <- renderPrint({
    cat("Cakupan Interval Prediksi 95% Bayesian pada Data Test:", round(hasil$meta$cakupan95, 1), "%\n")
    cat("Maksimum R-hat MCMC: 1.0017\n")
  })
  
  output$plot_test <- renderPlot({
    df <- hasil$pred_test
    df_long <- data.frame(
      idx = rep(df$idx, 4),
      nilai = c(df$Aktual, df$OLS, df$Bayesian, df$RF),
      Model = factor(rep(c("Aktual", "OLS", "Regresi Bayesian", "Random Forest"), each = nrow(df)),
                     levels = c("Aktual", "OLS", "Regresi Bayesian", "Random Forest"))
    )
    ggplot(df_long, aes(x = idx, y = nilai, color = Model, linewidth = Model)) +
      geom_line() +
      scale_linewidth_manual(values = c(Aktual = 1.2, OLS = 0.8, `Regresi Bayesian` = 0.8, `Random Forest` = 0.8)) +
      scale_color_manual(values = c(Aktual = "#ffffff", OLS = "#0dcaf0", `Regresi Bayesian` = "#ff6b6b", `Random Forest` = "#20c997")) +
      labs(x = "Indeks Test", y = "Hambatan Sisa (r)") +
      theme_minimal(base_size = 12) +
      theme(panel.background = element_rect(fill = "#1b263b", color = NA), plot.background = element_rect(fill = "#1b263b", color = NA), text = element_text(color = "#e0e1dd"), axis.text = element_text(color = "#e0e1dd"), legend.background = element_rect(fill = "#1b263b", color = NA))
  })
  
  sim_val <- eventReactive(input$btn_run, {
    withProgress(message = 'Menghitung prediksi...', value = 0.5, {
      path_brms <- file.path(OUT_DIR, "model", "m_hetero.rds")
      path_rf   <- file.path(OUT_DIR, "model_rf.rds")
      if (!file.exists(path_brms) || !file.exists(path_rf)) return(list(error = "File model belum lengkap. Jalankan pre-komputasi!"))
      
      m_hetero <- readRDS(path_brms)
      m_rf <- readRDS(path_rf)
      get_std <- function(val, var) { sc <- hasil$skala[hasil$skala$Variabel == var, ]; (val - sc$mean) / sc$sd }
      
      df_std <- data.frame(LCB = get_std(input$sim_LCB, "LCB"), PC = get_std(input$sim_PC, "PC"), LDR = get_std(input$sim_LDR, "LDR"), BDR = get_std(input$sim_BDR, "BDR"), LBR = get_std(input$sim_LBR, "LBR"), Fn = get_std(input$sim_Fn, "Fn"), ID_kapal = "Baru.1")
      df_orig <- data.frame(LCB = input$sim_LCB, PC = input$sim_PC, LDR = input$sim_LDR, BDR = input$sim_BDR, LBR = input$sim_LBR, Fn = input$sim_Fn)
      
      epred <- posterior_epred(m_hetero, newdata = df_std, allow_new_levels = TRUE)
      list(bayes_mean = mean(epred), bayes_lo = quantile(epred, 0.025), bayes_hi = quantile(epred, 0.975), rf = predict(m_rf, newdata = df_orig), error = NULL)
    })
  }, ignoreNULL = FALSE)
  
  output$txt_sim_hasil <- renderPrint({
    res <- sim_val()
    if (!is.null(res$error)) cat(res$error) else {
      cat("=== HASIL PREDIKSI DESAIN KAPAL BARU ===\n")
      cat(sprintf("Prediksi Bayesian (Mean) : %.5f\n", res$bayes_mean))
      cat(sprintf("Interval 95%% (Lo - Hi)   : [%.5f, %.5f]\n", res$bayes_lo, res$bayes_hi))
      cat(sprintf("Prediksi Random Forest   : %.5f\n", res$rf))
    }
  })
}

shinyApp(ui, server)