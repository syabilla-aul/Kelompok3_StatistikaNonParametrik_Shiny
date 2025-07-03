# app.R

library(shiny)
library(ggplot2)
library(tseries)

# UI
ui <- navbarPage("Uji Statistik Nonparametrik",
                 tabPanel("Dashboard",
                          fluidPage(style = "background-color: #e6f2ff; padding: 20px;",
                                    h2("Aplikasi Uji Statistik Nonparametrik"),
                                    p("Aplikasi ini dirancang untuk membantu pengguna melakukan uji-uji statistik nonparametrik berdasarkan kasus dan jenis data."),
                                    h4("Fitur Utama:"),
                                    tags$ul(
                                      tags$li("Kasus satu sampel"),
                                      tags$li("Kasus dua sampel berhubungan"),
                                      tags$li("Kasus dua sampel independen"),
                                      tags$li("Jenis data: Nominal dan Ordinal")
                                    ),
                                    h4("Catatan Penting:"),
                                    p(strong("🔸 Aplikasi ini hanya berlaku untuk sampel kecil, yaitu jumlah data tidak boleh melebihi 30 pengamatan.")),
                                    h4("Petunjuk Penggunaan:"),
                                    p("1. Pilih jenis kasus yang ingin diuji."),
                                    p("2. Pilih jenis data (nominal atau ordinal)."),
                                    p("3. Pilih metode uji (jika tersedia), unggah data, dan jalankan uji.")
                          )
                 ),
                 
                 # Kasus Satu Sampel
                 tabPanel("Kasus Satu Sampel",
                          fluidPage(style = "background-color: #f0fff0; padding: 20px;",
                                    sidebarLayout(
                                      sidebarPanel(
                                        radioButtons("jenis_data_satu", "Pilih Jenis Data:",
                                                     choices = c("Nominal", "Ordinal")),
                                        uiOutput("uji_pilihan_satu"),
                                        fileInput("data_satu", "Unggah Data (.csv)"),
                                        actionButton("ok_satu", "OK")
                                      ),
                                      mainPanel(
                                        verbatimTextOutput("hasil_uji_satu"),
                                        plotOutput("plot_satu")
                                      )
                                    )
                          )
                 ),
                 
                 # Kasus Dua Sampel Berhubungan
                 tabPanel("Kasus Dua Sampel Berhubungan",
                          fluidPage(style = "background-color: #fff5e6; padding: 20px;",
                                    sidebarLayout(
                                      sidebarPanel(
                                        radioButtons("jenis_data_berhubungan", "Jenis Data", choices = c("Nominal", "Ordinal")),
                                        uiOutput("uji_berhubungan"),
                                        fileInput("data_dua_berhubungan", "Upload Data CSV"),
                                        actionButton("ok_berhubungan", "OK")
                                      ),
                                      mainPanel(
                                        verbatimTextOutput("hasil_uji_berhubungan"),
                                        plotOutput("plot_berhubungan")
                                      )
                                    )
                          )
                 ),
                 
                 # Kasus Dua Sampel Independen
                 tabPanel("Kasus Dua Sampel Independen",
                          fluidPage(style = "background-color: #f9f2f4; padding: 20px;",
                                    sidebarLayout(
                                      sidebarPanel(
                                        radioButtons("jenis_data_indep", "Pilih Jenis Data:",
                                                     choices = c("Nominal", "Ordinal")),
                                        uiOutput("uji_pilihan_indep"),
                                        fileInput("data_indep", "Unggah Data (.csv)"),
                                        actionButton("ok_indep", "OK")
                                      ),
                                      mainPanel(
                                        verbatimTextOutput("hasil_uji_indep"),
                                        plotOutput("plot_indep")
                                      )
                                    )
                          )
                 )
)

# Server 
server <- function(input, output, session) {
  dataInputSatu <- reactive({
    req(input$data_satu)
    df <- read.csv(input$data_satu$datapath)
    if (nrow(df) > 30) return(NULL)
    return(df)
  })
  
  observeEvent(input$jenis_data_satu, {
    if (input$jenis_data_satu == "Nominal") {
      output$uji_pilihan_satu <- renderUI({
        selectInput("uji_nominal_satu", "Pilih Uji", 
                    choices = c("Uji Binomial" = "binomial", "Uji Chi-Square" = "chisq"))
      })
    } else {
      output$uji_pilihan_satu <- renderUI({
        strong("Uji yang digunakan: Run Test")
      })
    }
  })
  
  observeEvent(input$ok_satu, {
    df <- dataInputSatu()
    req(df)
    
    # ==== UJI BINOMIAL UNTUK NOMINAL, SATU SAMPEL ====
    if (input$jenis_data_satu == "Nominal" && input$uji_nominal_satu == "binomial") {
      if (ncol(df) != 1) {
        output$hasil_uji_satu <- renderPrint({
          cat("❌ Data untuk uji binomial harus hanya satu kolom.")
        })
        output$plot_satu <- renderPlot(NULL)
        return()
      }
      tbl <- table(df[[1]])
      if (length(tbl) != 2) {
        output$hasil_uji_satu <- renderPrint({
          cat("❌ Uji binomial hanya berlaku jika terdapat dua kategori dalam data.")
        })
        output$plot_satu <- renderPlot(NULL)
        return()
      }
      x <- as.vector(tbl)[1]
      n <- sum(tbl)
      test_result <- binom.test(x, n)
      
      output$hasil_uji_satu <- renderPrint({
        cat("\nData yang digunakan:\n")
        print(df)
        cat("\nJenis Uji: Binomial Test\n")
        cat("Hipotesis:\nH0: Proporsi sama dengan 0.5\nH1: Proporsi tidak sama dengan 0.5\n")
        cat("\nNilai Statistik Uji (p): ", round(test_result$statistic, 4), "\n")
        cat("p-value: ", format.pval(test_result$p.value), "\n")
        if (test_result$p.value < 0.05) {
          cat("Tolak H0\nKesimpulan: Proporsi berbeda signifikan dari 0.5\n")
        } else {
          cat("Gagal tolak H0\nKesimpulan: Tidak cukup bukti proporsi berbeda dari 0.5\n")
        }
      })
      
      output$plot_satu <- renderPlot({
        bar_data <- as.data.frame(tbl)
        ggplot(bar_data, aes(x = Var1, y = Freq, fill = Var1)) +
          geom_bar(stat = "identity") +
          labs(x = "Kategori", y = "Frekuensi", title = "Visualisasi Data - Binomial") +
          theme_minimal()
      })
    }
    
    # ==== UJI CHI-SQUARE UNTUK NOMINAL, SATU SAMPEL ====
    if (input$jenis_data_satu == "Nominal" && input$uji_nominal_satu == "chisq") {
      if (ncol(df) != 1) {
        output$hasil_uji_satu <- renderPrint({
          cat("❌ Data untuk uji chi-square harus satu kolom.")
        })
        output$plot_satu <- renderPlot(NULL)
        return()
      }
      tbl <- table(df[[1]])
      test_result <- chisq.test(tbl)
      
      output$hasil_uji_satu <- renderPrint({
        cat("\nData yang digunakan:\n")
        print(df)
        cat("\nJenis Uji: Chi-Square Goodness of Fit\n")
        cat("Hipotesis:\nH0: Data terdistribusi merata\nH1: Data tidak terdistribusi merata\n")
        cat("\nNilai Statistik Uji: ", round(test_result$statistic, 4), "\n")
        cat("p-value: ", format.pval(test_result$p.value), "\n")
        if (test_result$p.value < 0.05) {
          cat("Tolak H0\nKesimpulan: Distribusi tidak merata\n")
        } else {
          cat("Gagal tolak H0\nKesimpulan: Tidak cukup bukti distribusi tidak merata\n")
        }
      })
      
      output$plot_satu <- renderPlot({
        bar_data <- as.data.frame(tbl)
        ggplot(bar_data, aes(x = Var1, y = Freq, fill = Var1)) +
          geom_bar(stat = "identity") +
          labs(x = "Kategori", y = "Frekuensi", title = "Visualisasi - Chi-Square One Sample") +
          theme_minimal()
      })
    }
    
    # ==== UJI RUN UNTUK ORDINAL, SATU SAMPEL ====
    if (input$jenis_data_satu == "Ordinal") {
      if (ncol(df) != 1 || !is.numeric(df[[1]])) {
        output$hasil_uji_satu <- renderPrint({
          cat("❌ Data untuk uji run harus satu kolom numerik.")
        })
        output$plot_satu <- renderPlot(NULL)
        return()
      }
      
      nilai <- df[[1]]
      median_val <- median(nilai)
      kategori <- ifelse(nilai > median_val, "above", "below")
      kategori <- factor(kategori)
      
      test_result <- runs.test(kategori)
      
      output$hasil_uji_satu <- renderPrint({
        cat("\nData yang digunakan:\n")
        print(df)
        cat("\nJenis Uji: Run Test\n")
        cat("Hipotesis:\nH0: Data acak\nH1: Data tidak acak\n")
        cat("\nNilai Statistik Uji (Z): ", round(test_result$statistic, 4), "\n")
        cat("p-value: ", format.pval(test_result$p.value), "\n")
        if (test_result$p.value < 0.05) {
          cat("Tolak H0\nKesimpulan: Data menunjukkan pola (tidak acak)\n")
        } else {
          cat("Gagal tolak H0\nKesimpulan: Data bersifat acak\n")
        }
      })
      
      output$plot_satu <- renderPlot({
        ggplot(data.frame(index = 1:length(nilai), nilai = nilai), aes(x = index, y = nilai)) +
          geom_point(color = "steelblue", size = 3) +
          geom_line(color = "steelblue") +
          labs(x = "Urutan", y = "Nilai", title = "Visualisasi Run Test - Scatter Plot") +
          theme_minimal()
      })
    }
  })
  # SERVER: Kasus Dua Sampel Berhubungan
  dataInputBerhubungan <- reactive({
    req(input$data_dua_berhubungan)
    df <- read.csv(input$data_dua_berhubungan$datapath)
    if (nrow(df) > 30) return(NULL)
    return(df)
  })
  
  output$uji_berhubungan <- renderUI({
    if (input$jenis_data_berhubungan == "Ordinal") {
      selectInput("uji_ordinal_berhubungan", "Pilih Uji", 
                  choices = c("Uji Sign" = "sign", "Uji Wilcoxon" = "wilcoxon"))
    } else {
      strong("Uji yang digunakan: McNemar Test")
    }
  })
  
  observeEvent(input$ok_berhubungan, {
    df <- dataInputBerhubungan()
    req(df)
    
    if (ncol(df) < 2) {
      output$hasil_uji_berhubungan <- renderPrint({
        cat("❌ Data harus memiliki minimal dua kolom.")
      })
      output$plot_berhubungan <- renderPlot(NULL)
      return()
    }
    
    x <- df[[1]]
    y <- df[[2]]
    
    # ==== McNemar Test (Nominal, Dua Sampel Berhubungan) ====
    if (input$jenis_data_berhubungan == "Nominal") {
      tbl <- table(x, y)
      if (all(dim(tbl) == c(2, 2))) {
        test_result <- mcnemar.test(tbl)
        output$hasil_uji_berhubungan <- renderPrint({
          cat("
Data yang digunakan:
")
          print(df)
          cat("
Jenis Uji: McNemar Test
")
          cat("Hipotesis:
H0: Tidak ada perbedaan proporsi
H1: Ada perbedaan proporsi
")
          cat("
Statistik Uji: ", round(test_result$statistic, 4), "
")
          cat("p-value: ", format.pval(test_result$p.value), "
")
          if (test_result$p.value < 0.05) {
            cat("Tolak H0
Kesimpulan: Terdapat perbedaan signifikan
")
          } else {
            cat("Gagal tolak H0
Kesimpulan: Tidak ada bukti perbedaan signifikan
")
          }
        })
        output$plot_berhubungan <- renderPlot({
          ggplot(df, aes(x = x, fill = y)) +
            geom_bar(position = "dodge") +
            labs(title = "Visualisasi McNemar", x = "X", y = "Jumlah") +
            theme_minimal()
        })
      } else {
        output$hasil_uji_berhubungan <- renderPrint({
          cat("❌ McNemar hanya untuk tabel 2x2.")
        })
        output$plot_berhubungan <- renderPlot(NULL)
      }
    }
    
    # ==== Sign Test (Ordinal) ====
    if (input$jenis_data_berhubungan == "Ordinal" && input$uji_ordinal_berhubungan == "sign") {
      x <- suppressWarnings(as.numeric(as.character(x)))
      y <- suppressWarnings(as.numeric(as.character(y)))
      
      if (any(is.na(x)) || any(is.na(y))) {
        output$hasil_uji_berhubungan <- renderPrint({
          cat("❌ Data harus berupa angka (ordinal bersifat numerik).")
        })
        output$plot_berhubungan <- renderPlot(NULL)
        return()
      }
      
      diff <- x - y
      signs <- sum(diff > 0)
      n <- sum(diff != 0)
      test_result <- binom.test(signs, n)
      
      output$hasil_uji_berhubungan <- renderPrint({
        cat("\nData yang digunakan:\n")
        print(df)
        cat("\nJenis Uji: Sign Test\n")
        cat("Hipotesis:\nH0: Median perbedaan = 0\nH1: Median perbedaan ≠ 0\n")
        cat("\nJumlah data: ", n, ", Jumlah tanda positif: ", signs, "\n")
        cat("p-value: ", format.pval(test_result$p.value), "\n")
        if (test_result$p.value < 0.05) {
          cat("Tolak H0\nKesimpulan: Terdapat perbedaan signifikan\n")
        } else {
          cat("Gagal tolak H0\nKesimpulan: Tidak ada bukti perbedaan signifikan\n")
        }
      })
      output$plot_berhubungan <- renderPlot({
        ggplot(data.frame(diff = diff), aes(x = factor(1), y = diff)) +
          geom_boxplot(fill = "lightblue") +
          labs(title = "Sign Test - Boxplot Perbedaan", x = "", y = "X - Y") +
          theme_minimal()
      })
    }
    
    # ==== Wilcoxon Test (Ordinal) ====
    if (input$jenis_data_berhubungan == "Ordinal" && input$uji_ordinal_berhubungan == "wilcoxon") {
      x <- suppressWarnings(as.numeric(as.character(x)))
      y <- suppressWarnings(as.numeric(as.character(y)))
      
      if (any(is.na(x)) || any(is.na(y))) {
        output$hasil_uji_berhubungan <- renderPrint({
          cat("❌ Data harus berupa angka (ordinal bersifat numerik).")
        })
        output$plot_berhubungan <- renderPlot(NULL)
        return()
      }
      
      test_result <- wilcox.test(x, y, paired = TRUE)
      output$hasil_uji_berhubungan <- renderPrint({
        cat("\nData yang digunakan:\n")
        print(df)
        cat("\nJenis Uji: Wilcoxon Signed-Rank Test\n")
        cat("Hipotesis:\nH0: Tidak ada perbedaan peringkat\nH1: Ada perbedaan peringkat\n")
        cat("\nStatistik Uji: ", round(test_result$statistic, 4), "\n")
        cat("p-value: ", format.pval(test_result$p.value), "\n")
        if (test_result$p.value < 0.05) {
          cat("Tolak H0\nKesimpulan: Terdapat perbedaan signifikan\n")
        } else {
          cat("Gagal tolak H0\nKesimpulan: Tidak ada bukti perbedaan signifikan\n")
        }
      })
      output$plot_berhubungan <- renderPlot({
        ggplot(data.frame(x = x, y = y), aes(x = x, y = y)) +
          geom_point(color = "steelblue") +
          geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
          labs(title = "Wilcoxon - Scatter Plot X vs Y", x = "X", y = "Y") +
          theme_minimal()
      })
    }
  })
  
  # SERVER: Kasus Dua Sampel Independen
  dataInputIndep <- reactive({
    req(input$data_indep)
    df <- read.csv(input$data_indep$datapath)
    if (nrow(df) > 30) return(NULL)
    return(df)
  })
  
  observeEvent(input$jenis_data_indep, {
    if (input$jenis_data_indep == "Nominal") {
      output$uji_pilihan_indep <- renderUI({
        selectInput("uji_nominal_indep", "Pilih Uji", 
                    choices = c("Chi-Square" = "chisq", "Fisher’s Exact" = "fisher"))
      })
    } else {
      output$uji_pilihan_indep <- renderUI({
        selectInput("uji_ordinal_indep", "Pilih Uji", 
                    choices = c("Median Test" = "median", 
                                "Mann–Whitney U Test" = "mannwhitney",
                                "Kolmogorov–Smirnov Test" = "ks",
                                "Wald–Wolfowitz Runs Test" = "waldwolfowitz"))
      })
    }
  })
  
  observeEvent(input$ok_indep, {
    df <- dataInputIndep()
    req(df)
    
    # ==== UJI CHI-SQUARE UNTUK NOMINAL, DUA SAMPEL INDEPENDEN ====
    if (input$jenis_data_indep == "Nominal" && input$uji_nominal_indep == "chisq") {
      if (ncol(df) != 2) {
        output$hasil_uji_indep <- renderPrint({
          cat("❌ Data harus terdiri dari dua kolom kategori.")
        })
        output$plot_indep <- renderPlot(NULL)
        return()
      }
      
      tbl <- table(df[[1]], df[[2]])
      test_result <- chisq.test(tbl)
      
      output$hasil_uji_indep <- renderPrint({
        cat("\nData yang digunakan:\n")
        print(df)
        cat("\nJenis Uji: Chi-Square\n")
        cat("Hipotesis:\nH0: Tidak ada asosiasi antara dua variabel\nH1: Ada asosiasi antara dua variabel\n")
        cat("\nNilai Statistik Uji: ", round(test_result$statistic, 4), "\n")
        cat("p-value: ", format.pval(test_result$p.value), "\n")
        if (test_result$p.value < 0.05) {
          cat("Tolak H0\nKesimpulan: Ada asosiasi signifikan antara dua variabel.\n")
        } else {
          cat("Gagal tolak H0\nKesimpulan: Tidak ada asosiasi signifikan.\n")
        }
      })
      
      output$plot_indep <- renderPlot({
        ggplot(df, aes(x = df[[1]], fill = df[[2]])) +
          geom_bar(position = "dodge") +
          labs(x = names(df)[1], fill = names(df)[2],
               title = "Chi-Square Test - Bar Plot") +
          theme_minimal()
      })
    }
    
    # ==== UJI FISHER UNTUK NOMINAL, DUA SAMPEL INDEPENDEN ====
    if (input$jenis_data_indep == "Nominal" && input$uji_nominal_indep == "fisher") {
      if (ncol(df) != 2) {
        output$hasil_uji_indep <- renderPrint({
          cat("❌ Data harus terdiri dari dua kolom kategori.")
        })
        output$plot_indep <- renderPlot(NULL)
        return()
      }
      
      tbl <- table(df[[1]], df[[2]])
      test_result <- fisher.test(tbl)
      
      output$hasil_uji_indep <- renderPrint({
        cat("\nData yang digunakan:\n")
        print(df)
        cat("\nJenis Uji: Fisher’s Exact Test\n")
        cat("Hipotesis:\nH0: Tidak ada asosiasi antara dua variabel\nH1: Ada asosiasi antara dua variabel\n")
        cat("\nNilai Statistik Uji: Tidak relevan\n")
        cat("p-value: ", format.pval(test_result$p.value), "\n")
        if (test_result$p.value < 0.05) {
          cat("Tolak H0\nKesimpulan: Ada asosiasi signifikan antara dua variabel.\n")
        } else {
          cat("Gagal tolak H0\nKesimpulan: Tidak ada asosiasi signifikan.\n")
        }
      })
      
      output$plot_indep <- renderPlot({
        ggplot(df, aes(x = df[[1]], fill = df[[2]])) +
          geom_bar(position = "dodge") +
          labs(x = names(df)[1], fill = names(df)[2],
               title = "Fisher’s Exact Test - Bar Plot") +
          theme_minimal()
      })
    }
    
    # ==== UJI MEDIAN TEST UNTUK ORDINAL, DUA SAMPEL INDEPENDEN ====
    if (input$jenis_data_indep == "Ordinal" && input$uji_ordinal_indep == "median") {
      if (ncol(df) != 2) {
        output$hasil_uji_indep <- renderPrint({
          cat("❌ Data harus terdiri dari dua kolom (x dan y).")
        })
        output$plot_indep <- renderPlot(NULL)
        return()
      }
      x <- suppressWarnings(as.numeric(as.character(x)))
      y <- suppressWarnings(as.numeric(as.character(y)))
      
      if (any(is.na(x)) || any(is.na(y))) {
        output$hasil_uji_indep <- renderPrint({
          cat("❌ Data harus berupa angka (ordinal bersifat numerik).")
        })
        output$plot_indep <- renderPlot(NULL)
        return()
      }
      median_global <- median(c(x, y))
      tab <- table(Group = rep(c("x", "y"), c(length(x), length(y))),
                   AboveMedian = c(x > median_global, y > median_global))
      test_result <- chisq.test(tab)
      data_combined <- data.frame(nilai = c(x, y), grup = factor(rep(c("x", "y"), c(length(x), length(y)))))
      
      output$hasil_uji_indep <- renderPrint({
        cat("\nData yang digunakan:\n")
        print(df)
        cat("\nJenis Uji: Median Test\n")
        cat("Hipotesis:\nH0: Median dua grup sama\nH1: Median dua grup berbeda\n")
        cat("\nNilai Statistik Uji: ", round(test_result$statistic, 4), "\n")
        cat("p-value: ", format.pval(test_result$p.value), "\n")
        if (test_result$p.value < 0.05) {
          cat("Tolak H0\nKesimpulan: Median berbeda signifikan.\n")
        } else {
          cat("Gagal tolak H0\nKesimpulan: Median tidak berbeda signifikan.\n")
        }
      })
      
      output$plot_indep <- renderPlot({
        ggplot(data_combined, aes(x = grup, y = nilai, fill = grup)) +
          geom_boxplot() +
          theme_minimal() +
          labs(x = "Grup", y = "Nilai", title = "Visualisasi Data - Median Test")
      })
    }
    
    # ==== UJI MANN-WHITNEY U UNTUK ORDINAL, DUA SAMPEL INDEPENDEN ====
    if (input$jenis_data_indep == "Ordinal" && input$uji_ordinal_indep == "mannwhitney") {
      if (ncol(df) != 2) {
        output$hasil_uji_indep <- renderPrint({
          cat("❌ Data harus terdiri dari dua kolom (x dan y).")
        })
        output$plot_indep <- renderPlot(NULL)
        return()
      }
      x <- suppressWarnings(as.numeric(as.character(x)))
      y <- suppressWarnings(as.numeric(as.character(y)))
      
      if (any(is.na(x)) || any(is.na(y))) {
        output$hasil_uji_indep <- renderPrint({
          cat("❌ Data harus berupa angka (ordinal bersifat numerik).")
        })
        output$plot_indep <- renderPlot(NULL)
        return()
      }
      test_result <- wilcox.test(x, y, exact = FALSE)
      data_combined <- data.frame(nilai = c(x, y),
                                  grup = factor(rep(c("x", "y"), times = c(length(x), length(y)))))
      output$hasil_uji_indep <- renderPrint({
        cat("\nData yang digunakan:\n")
        print(df)
        cat("\nJenis Uji: Mann–Whitney U Test\n")
        cat("Hipotesis:\nH0: Distribusi kedua kelompok sama\nH1: Distribusi kedua kelompok berbeda\n")
        cat("\nNilai Statistik Uji (W): ", round(test_result$statistic, 4), "\n")
        cat("p-value: ", format.pval(test_result$p.value), "\n")
        if (test_result$p.value < 0.05) {
          cat("Tolak H0\nKesimpulan: Terdapat perbedaan distribusi.\n")
        } else {
          cat("Gagal tolak H0\nKesimpulan: Tidak terdapat perbedaan signifikan.\n")
        }
      })
      output$plot_indep <- renderPlot({
        ggplot(data_combined, aes(x = grup, y = nilai, fill = grup)) +
          geom_boxplot() +
          theme_minimal() +
          labs(x = "Grup", y = "Nilai", title = "Visualisasi Data - Mann–Whitney U Test")
      })
    }
    
    # ==== UJI KOLMOGOROV–SMIRNOV UNTUK ORDINAL, DUA SAMPEL INDEPENDEN ====
    if (input$jenis_data_indep == "Ordinal" && input$uji_ordinal_indep == "ks") {
      if (ncol(df) != 2) {
        output$hasil_uji_indep <- renderPrint({
          cat("❌ Data harus terdiri dari dua kolom (x dan y).")
        })
        output$plot_indep <- renderPlot(NULL)
        return()
      }
      x <- suppressWarnings(as.numeric(as.character(x)))
      y <- suppressWarnings(as.numeric(as.character(y)))
      
      if (any(is.na(x)) || any(is.na(y))) {
        output$hasil_uji_indep <- renderPrint({
          cat("❌ Data harus berupa angka (ordinal bersifat numerik).")
        })
        output$plot_indep <- renderPlot(NULL)
        return()
      }
      test_result <- ks.test(x, y)
      data_combined <- data.frame(nilai = c(x, y),
                                  grup = factor(rep(c("x", "y"), times = c(length(x), length(y)))))
      output$hasil_uji_indep <- renderPrint({
        cat("\nData yang digunakan:\n")
        print(df)
        cat("\nJenis Uji: Kolmogorov–Smirnov Test\n")
        cat("Hipotesis:\nH0: Distribusi dua sampel sama\nH1: Distribusi dua sampel berbeda\n")
        cat("\nNilai Statistik Uji (D): ", round(test_result$statistic, 4), "\n")
        cat("p-value: ", format.pval(test_result$p.value), "\n")
        if (test_result$p.value < 0.05) {
          cat("Tolak H0\nKesimpulan: Distribusi berbeda secara signifikan.\n")
        } else {
          cat("Gagal tolak H0\nKesimpulan: Tidak ada perbedaan distribusi signifikan.\n")
        }
      })
      output$plot_indep <- renderPlot({
        ggplot(data_combined, aes(x = nilai, color = grup)) +
          stat_ecdf(geom = "step") +
          theme_minimal() +
          labs(x = "Nilai", y = "F(x)", title = "ECDF - Kolmogorov–Smirnov Test")
      })
    }
    
    # ==== UJI WALD–WOLFOWITZ RUNS UNTUK ORDINAL, DUA SAMPEL INDEPENDEN ====
    if (input$jenis_data_indep == "Ordinal" && input$uji_ordinal_indep == "waldwolfowitz") {
      if (ncol(df) != 2) {
        output$hasil_uji_indep <- renderPrint({
          cat("❌ Data harus terdiri dari dua kolom (x dan y).")
        })
        output$plot_indep <- renderPlot(NULL)
        return()
      }
      x <- suppressWarnings(as.numeric(as.character(x)))
      y <- suppressWarnings(as.numeric(as.character(y)))
      
      if (any(is.na(x)) || any(is.na(y))) {
        output$hasil_uji_indep <- renderPrint({
          cat("❌ Data harus berupa angka (ordinal bersifat numerik).")
        })
        output$plot_indep <- renderPlot(NULL)
        return()
      }
      z <- c(x, y)
      group <- c(rep(0, length(x)), rep(1, length(y)))
      ordered_group <- group[order(z)]
      test_result <- runs.test(as.factor(ordered_group))
      data_combined <- data.frame(nilai = z,
                                  grup = factor(group, labels = c("x", "y")))
      output$hasil_uji_indep <- renderPrint({
        cat("\nData yang digunakan:\n")
        print(df)
        cat("\nJenis Uji: Wald–Wolfowitz Runs Test\n")
        cat("Hipotesis:\nH0: Kedua kelompok berasal dari populasi yang sama\nH1: Kedua kelompok berasal dari populasi yang berbeda\n")
        cat("\nNilai Statistik Uji (Z): ", round(test_result$statistic, 4), "\n")
        cat("p-value: ", format.pval(test_result$p.value), "\n")
        if (test_result$p.value < 0.05) {
          cat("Tolak H0\nKesimpulan: Ada perbedaan signifikan antara dua kelompok.\n")
        } else {
          cat("Gagal tolak H0\nKesimpulan: Tidak terdapat perbedaan signifikan antara dua kelompok.\n")
        }
      })
      output$plot_indep <- renderPlot({
        ggplot(data_combined, aes(x = reorder(grup, nilai), y = nilai, fill = grup)) +
          geom_boxplot() +
          theme_minimal() +
          labs(x = "Grup", y = "Nilai", title = "Visualisasi Data - Wald–Wolfowitz Test")
      })
    }
  })
}

# Run App
shinyApp(ui = ui, server = server)

