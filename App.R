library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)
library(readr)
library(shinycssloaders)
library(leaflet)
library(sf) 
library(htmlwidgets)
library(webshot)
library(lmtest)
library(car)
library(here)

# Load data
sovi_data <- read_csv(here("Data", "sovi_data.csv"))
peta_provinsi <- readRDS(here("Data", "peta_kabupaten_simplified.rds"))

# Mengubah kolom kodeprkab dari character menjadi numeric
peta_provinsi <- peta_provinsi %>%
  mutate(kodeprkab = as.numeric(kodeprkab))

prov_names <- unique(na.omit(peta_provinsi$nmprov))
pilihan_provinsi <- c("Seluruh Indonesia", prov_names)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Dashboard Analisis Statistik STIS - Komputasi Statistik"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Beranda", tabName = "beranda", icon = icon("home")),
      menuItem("Manajemen Data", tabName = "manajemen", icon = icon("database")),
      menuItem("Eksplorasi Data", tabName = "eksplorasi_utama", icon = icon("chart-bar"), 
               menuSubItem("Analisis Umum", tabName = "eksplorasi", icon = icon("chart-pie")), 
               menuSubItem("Pemetaan Spasial", tabName = "pemetaan", icon = icon("map-marked-alt")) 
      ),
      menuItem("Uji Asumsi & Normalitas", tabName = "asumsi", icon = icon("check-circle")),
      menuItem("Statistik Inferensia", tabName = "inferensia", icon = icon("calculator"),
               menuSubItem("Uji Rata-rata", tabName = "uji_rata"),
               menuSubItem("Uji Proporsi & Varians", tabName = "uji_prop"),
               menuSubItem("ANOVA", tabName = "anova")
      ),
      menuItem("Regresi Linear Berganda", tabName = "regresi", icon = icon("line-chart"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
      .content-wrapper, .right-side {
        background-color: #f4f4f4;
      }
      .box {
        border-radius: 5px;
      }
      .nav-tabs-custom > .nav-tabs > li.active {
        border-top-color: #3c8dbc;
      }
      .placeholder-message {
        text-align: center;
        color: #666;
        font-style: italic;
        padding: 20px;
        background-color: #f9f9f9;
        border: 1px dashed #ccc;
        border-radius: 5px;
      }
      .boundary-input {
        margin-bottom: 10px;
      }
      .category-input {
        margin-bottom: 8px;
      }
      .category-section {
        background-color: #f8f9fa;
        padding: 10px;
        border-radius: 5px;
        margin: 10px 0;
      }
      .boundary-section {
        background-color: #fff3cd;
        padding: 10px;
        border-radius: 5px;
        margin: 10px 0;
        border-left: 4px solid #ffc107;
      }
    "))
    ),
    
    tabItems(
      # Tab Beranda
      tabItem(tabName = "beranda",
              fluidRow(
                box(
                  title = "Selamat Datang di Dashboard Analisis Statistik", 
                  status = "primary", 
                  solidHeader = TRUE, 
                  width = 12,
                  h3("Analisis Statistik Kerentanan Sosial di 511 Kabupaten/Kota Indonesia"),
                  hr(),
                  
                  h4("Metadata Dataset:"),
                  hr(),
                  h4("Definisi Variabel"),
                  
                  fluidRow(
                    column(6,
                           tags$h5(tags$b("Karakteristik Demografi")),
                           tags$ul(
                             tags$li(tags$b("CHILDREN"), ": Persentase penduduk berusia di bawah lima tahun."),
                             tags$li(tags$b("FEMALE"), ": Persentase penduduk perempuan."),
                             tags$li(tags$b("ELDERLY"), ": Persentase penduduk berusia 65 tahun ke atas."),
                             tags$li(tags$b("FAMILYSIZE"), ": Jumlah rata-rata anggota dalam satu rumah tangga."),
                             tags$li(tags$b("GROWTH"), ": Persentase perubahan (pertumbuhan) penduduk."),
                             tags$li(tags$b("POPULATION"), ": Jumlah total penduduk.")
                           ),
                           tags$h5(tags$b("Perumahan dan Infrastruktur")),
                           tags$ul(
                             tags$li(tags$b("NOELECTRIC"), ": Persentase rumah tangga tanpa akses listrik untuk penerangan."),
                             tags$li(tags$b("RENTED"), ": Persentase rumah tangga yang menyewa rumah."),
                             tags$li(tags$b("NOSEWER"), ": Persentase rumah tangga yang tidak memiliki sistem drainase."),
                             tags$li(tags$b("TAPWATER"), ": Persentase rumah tangga yang menggunakan air ledeng/pipa.")
                           )
                    ),
                    column(6,
                           tags$h5(tags$b("Kondisi Sosial Ekonomi")),
                           tags$ul(
                             tags$li(tags$b("FHEAD"), ": Persentase rumah tangga dengan kepala rumah tangga perempuan."),
                             tags$li(tags$b("POVERTY"), ": Persentase penduduk miskin."),
                             tags$li(tags$b("LOWEDU"), ": Persentase penduduk berusia 15+ dengan pendidikan rendah."),
                             tags$li(tags$b("ILLITERATE"), ": Persentase penduduk yang buta huruf (tidak bisa membaca dan menulis).")
                           ),
                           tags$h5(tags$b("Kerentanan terhadap Bencana")),
                           tags$ul(
                             tags$li(tags$b("NOTRAINING"), ": Persentase rumah tangga yang tidak pernah mendapat pelatihan kebencanaan."),
                             tags$li(tags$b("DPRONE"), ": Persentase rumah tangga yang tinggal di daerah rawan bencana.")
                           )
                    )
                  ),
                  
                  tags$ul(
                    tags$li("Sumber Data: https://raw.githubusercontent.com/bmlmcmc\naspaclust/main/data/sovi_data.csv"),
                    tags$li("Matriks Jarak: https://raw.githubusercontent.com/bmlmcmc\naspaclust/main/data/distance.csv"),
                    tags$li("Referensi Metadata: https://www.sciencedirect.com/science/article/pii/S2352340921010180")
                  ),
                  
                  h4("Fitur Dashboard:"),
                  tags$ul(
                    tags$li("Eksplorasi Data: Statistik deskriptif, visualisasi, dan pemetaan"),
                    tags$li("Manajemen Data: Preprocessing, transformasi, dan penanganan outlier"),
                    tags$li("Uji Asumsi: Normalitas dan homogenitas"),
                    tags$li("Statistik Inferensia: Uji hipotesis berbagai parameter"),
                    tags$li("Regresi Linear Berganda: Analisis regresi dengan uji asumsi")
                  ),
                  
                  br(),
                  p("Dikembangkan untuk mata kuliah Komputasi Statistik - Politeknik Statistika STIS"),
                  p("Dosen Pengampu: Robert Kurniawan, Sukim, Yuliagnis Transver Wijaya")
                )
              ),
              
              fluidRow(
                valueBoxOutput("total_obs"),
                valueBoxOutput("total_vars"),
                valueBoxOutput("missing_data")
              )
      ),
      
      # Tab Eksplorasi Data
      tabItem(tabName = "eksplorasi",
              fluidRow(
                box(
                  title = "Pilih Variabel untuk Eksplorasi", 
                  status = "primary", 
                  solidHeader = TRUE, 
                  width = 6,
                  selectInput("var_eksplorasi", "Pilih Variabel:", 
                              choices = NULL),
                  checkboxInput("show_outliers", "Tampilkan Outliers", value = TRUE),
                  hr(), 
                  h4("Pilih Tipe Grafik Eksplorasi:"),
                  checkboxGroupInput(
                    "plot_types",
                    label = NULL, 
                    choices = c(
                      "Histogram" = "histogram",
                      "Boxplot" = "boxplot",
                      "Plot Kepadatan (Density Plot)" = "density",
                      "Scatter Plot (vs. Indeks Data)" = "scatter",
                      "Bar Plot (untuk Variabel Kategorik)" = "bar"
                    ),
                    selected = NULL
                  ),
                  hr(),
                  downloadButton("unduh_laporan_eksplorasi", "Unduh Laporan (PDF) 📝", class = "btn-primary"),
                  p("Laporan akan berisi statistik dan semua plot yang Anda pilih.", style = "font-size:12px; font-style:italic; margin-top:10px;")
                ),
                
                box(
                  title = "Statistik Deskriptif", 
                  status = "info", 
                  solidHeader = TRUE, 
                  width = 6,
                  withSpinner(tableOutput("desc_stats_table"))
                )
              ),
              
              fluidRow(
               
                conditionalPanel(
                  condition = "input.plot_types.includes('histogram')",
                  box(
                    title = tags$span(
                      "Histogram",
                      downloadButton("download_plot_hist", "PNG", class="btn-xs pull-right")
                    ),
                    status = "success", 
                    solidHeader = TRUE, 
                    width = 6,
                    withSpinner(plotlyOutput("histogram")), 
                    hr(), 
                    h4("Interpretasi:"), 
                    withSpinner(uiOutput("histogram_interpretation")) 
                  )
                ),
                
                conditionalPanel(
                  condition = "input.plot_types.includes('boxplot')",
                  box(
                    title = tags$span(
                      "Boxplot",
                      downloadButton("download_plot_box", "PNG", class="btn-xs pull-right")
                    ),
                    status = "success", 
                    solidHeader = TRUE, 
                    width = 6,
                    withSpinner(plotlyOutput("boxplot")),
                    hr(), 
                    h4("Interpretasi:"),
                    withSpinner(uiOutput("boxplot_interpretation")) 
                  )
                ),
                
                fluidRow(
                  conditionalPanel(
                    condition = "input.plot_types.includes('density')",
                    box(
                      title = tags$span(
                        "Plot kepadatan",
                        downloadButton("download_plot_density", "PNG", class="btn-xs pull-right"),
                      ),
                      
                      status = "success",
                      solidHeader = TRUE,
                      width = 6,
                      withSpinner(plotlyOutput("density_plot")),
                      hr(), 
                      h4("Interpretasi:"), 
                      withSpinner(uiOutput("density_interpretation")) 
                    )
                  ),
                  conditionalPanel(
                    condition = "input.plot_types.includes('scatter')",
                    box(
                      title = tags$span(
                        "Scatter Plot",
                        downloadButton("download_plot_scatter", "PNG", class="btn-xs pull-right"),
                      ),
                      
                      status = "success",
                      solidHeader = TRUE,
                      width = 6,
                      withSpinner(plotlyOutput("scatter_plot")),
                      hr(), 
                      h4("Interpretasi:"), 
                      withSpinner(uiOutput("scatter_interpretation")) 
                    )
                  )
                ),
                
                fluidRow(
                  conditionalPanel(
                    condition = "input.plot_types.includes('bar')",
                    box(
                      title = tags$span(
                        "Bar Plot (Frekuensi)",
                        downloadButton("download_plot_bar", "PNG", class="btn-xs pull-right"),
                      ),
                      
                      status = "success",
                      solidHeader = TRUE,
                      width = 6,
                      withSpinner(plotlyOutput("bar_plot")),
                      hr(), 
                      h4("Interpretasi:"), 
                      withSpinner(uiOutput("bar_plot_interpretation")) 
                    )
                  )
                )
                
              ),
              
              fluidRow(
                box(
                  title = "Tabel Data", 
                  status = "primary", 
                  solidHeader = TRUE, 
                  width = 12,
                  withSpinner(DT::dataTableOutput("data_table"))
                )
              )
      ),
      
      
      tabItem(tabName = "manajemen",
              h2("Manajemen dan Preprocessing Data"),
              p("Gunakan fitur di bawah ini untuk mengubah variabel numerik menjadi kategorik atau melakukan transformasi data."),
              
              fluidRow(
                box(
                  title = "1. Pengaturan Kategorisasi",
                  status = "warning", solidHeader = TRUE, width = 4,
                  p("Pilih variabel dan tentukan jumlah kategori yang diinginkan."),
                  selectInput("var_to_cut", "Pilih Variabel Numerik:", choices = NULL),
                  numericInput("num_bins_manual", "Jumlah Kategori:", value = 3, min = 2, max = 10),
                  actionButton("cut_data_manual", "Buat Kategori", icon = icon("cut"), class = "btn-warning", width = "100%")
                ),
                box(
                  title = "2. Tentukan Batas Interval dan Nama Kategori",
                  status = "warning", solidHeader = TRUE, width = 8,
                  p(tags$b("Masukkan semua batas interval secara manual,"), tags$i("dari nilai terkecil hingga terbesar.")),
                  uiOutput("manual_interval_inputs")
                )
              ),
              fluidRow(
                box(
                  title = "3. Hasil dan Interpretasi",
                  status = "success", solidHeader = TRUE, width = 12,
                  withSpinner(uiOutput("interpretation_cut_manual"))
                )
              ),
              
              hr(),

              fluidRow(
                box(
                  title = "Transformasi Data (Penanganan Outlier)",
                  status = "danger", solidHeader = TRUE, width = 12,
                  collapsible = TRUE, collapsed = FALSE,
                  h4("Transformasi untuk Menangani Outlier"),
                  selectInput("var_transformasi", "Pilih Variabel untuk Transformasi:",
                              choices = c("Pilih Variabel..." = "")),
                  selectInput("jenis_transformasi", "Jenis Transformasi:",
                              choices = list("Pilih Transformasi..." = "",
                                             "Log Natural" = "log", 
                                             "1/x (Inverse)" = "inverse", 
                                             "1/x² (Inverse Square)" = "inverse_square")),
                  textInput("nama_var_transformasi", "Nama Variabel Hasil Transformasi:", 
                            placeholder = "contoh: income_log (Jangan ada spasi!!!)"),
                  actionButton("transformasi_btn", "Lakukan Transformasi", class = "btn-danger"),
                  hr(),
                  h5("Status Transformasi:"),
                  verbatimTextOutput("status_transformasi")
                )
              ),

              fluidRow(
                box(
                  title = "Preview Data Hasil Preprocessing",
                  status = "info", solidHeader = TRUE, width = 12,
                  fluidRow(
                    column(6, 
                           radioButtons("show_records", "Jumlah Record yang Ditampilkan:",
                                        choices = list("10" = 10, "100" = 100, "1000" = 1000, "Semua" = -1),
                                        selected = 10, inline = TRUE)
                    ),
                    column(6,
                           downloadButton("download_data", "Download Data Hasil Preprocessing", 
                                          class = "btn-success", style = "margin-top: 25px;")
                    )
                  ),
                  hr(),
                  withSpinner(DT::dataTableOutput("preview_data"))
                )
              )
      ),
      
      # Tab Uji Asumsi dan Normalitas
      tabItem(tabName = "asumsi",
              
              fluidRow(
                box(width = 12, status = "primary", solidHeader = TRUE,
                    title = "Unduh Laporan",
                    downloadButton("unduh_laporan_asumsi", "Unduh Laporan Uji Asumsi (PDF) 📄", class = "btn-primary")
                )
              ),
          
              fluidRow(
                box(
                  title = "Pengaturan Uji Normalitas", 
                  status = "primary", 
                  solidHeader = TRUE, 
                  width = 4,
                  h4("Uji Normalitas"),
                  selectInput("var_normalitas", "Pilih Variabel untuk Uji Normalitas:",
                              choices = c("Pilih Variabel..." = "")),
                  actionButton("uji_normalitas_btn", "Uji Normalitas", class = "btn-primary")
                ),
                
                box(
                  title = "Hasil Uji Normalitas", 
                  status = "success", 
                  solidHeader = TRUE, 
                  width = 8,
                  conditionalPanel(
                    condition = "input.var_normalitas == '' || input.var_normalitas == null",
                    div(class = "placeholder-message", 
                        h4("Silakan pilih variabel untuk uji normalitas terlebih dahulu"))
                  ),
                  conditionalPanel(
                    condition = "input.var_normalitas != '' && input.var_normalitas != null",
                    withSpinner(verbatimTextOutput("hasil_normalitas")),
                    hr(), 
                    h4("Interpretasi Hasil:"), 
                    withSpinner(uiOutput("interpretasi_normalitas")) 
                  )
                )
              ),
              
              # Q-Q PLOT TERPISAH
              fluidRow(
                box(
                  title = tags$span(
                    "Q-Q Plot untuk Uji Normalitas", 
                    downloadButton("download_plot_qq", "PNG", class="btn-xs pull-right", style="margin-top: -5px;")
                  ),
                  
                  status = "success", 
                  solidHeader = TRUE, 
                  width = 12,
                  conditionalPanel(
                    condition = "input.var_normalitas == '' || input.var_normalitas == null",
                    div(class = "placeholder-message", 
                        h4("Q-Q Plot akan muncul setelah memilih variabel dan menjalankan uji normalitas"))
                  ),
                  conditionalPanel(
                    condition = "input.var_normalitas != '' && input.var_normalitas != null",
                    withSpinner(plotOutput("qq_plot"))
                  )
                )
              ),
            
              fluidRow(
                box(
                  title = "Pengaturan Uji Homogenitas", 
                  status = "info", 
                  solidHeader = TRUE, 
                  width = 4,
                  h4("Uji Homogenitas Varians"),
                  selectInput("var_homogenitas", "Pilih Variabel untuk Uji Homogenitas:",
                              choices = c("Pilih Variabel..." = "")),
                  selectInput("group_var", "Pilih Variabel Pengelompokan:",
                              choices = c("Pilih Variabel..." = "")),
                  actionButton("uji_homogenitas_btn", "Uji Homogenitas", class = "btn-info")
                ),
                
                box(
                  title = "Hasil Uji Homogenitas", 
                  status = "warning", 
                  solidHeader = TRUE, 
                  width = 8,
                  conditionalPanel(
                    condition = "input.var_homogenitas == '' || input.group_var == '' || input.var_homogenitas == null || input.group_var == null",
                    div(class = "placeholder-message", 
                        h4("Silakan pilih variabel numerik dan variabel pengelompokan untuk uji homogenitas"))
                  ),
                  conditionalPanel(
                    condition = "input.var_homogenitas != '' && input.group_var != '' && input.var_homogenitas != null && input.group_var != null",
                    withSpinner(verbatimTextOutput("hasil_homogenitas")),
                    hr(),
                    h4("Interpretasi Hasil:"),
                    withSpinner(uiOutput("interpretasi_homogenitas"))
                  )
                )
              )
      ),
      
      # Tab Uji Rata-rata
      tabItem(tabName = "uji_rata",
              
              fluidRow(
                box(width = 12, status = "primary", solidHeader = TRUE,
                    title = "Unduh Laporan",
                    downloadButton("unduh_laporan_uji_rata", "Unduh Laporan Uji Rata-rata (PDF) 📝", class = "btn-primary")
                )
              ),
              
              fluidRow(
                box(
                  title = "Uji Rata-rata Satu Sampel", 
                  status = "primary", 
                  solidHeader = TRUE, 
                  width = 6,
                  selectInput("var_one_sample", "Pilih Variabel:",
                              choices = c("Pilih Variabel..." = "")),
                  numericInput("mu_test", "Nilai μ₀ untuk diuji:", value = 0),
                  actionButton("one_sample_btn", "Uji One Sample t-test", class = "btn-primary"),
                  hr(),
                  conditionalPanel(
                    condition = "input.var_one_sample == '' || input.var_one_sample == null",
                    div(class = "placeholder-message", "Silakan pilih variabel terlebih dahulu")
                  ),
                  conditionalPanel(
                    condition = "input.var_one_sample != '' && input.var_one_sample != null",
                    withSpinner(verbatimTextOutput("one_sample_result")),
                    hr(),
                    h4("Interpretasi:"),
                    withSpinner(uiOutput("one_sample_interp")) 
                  )
                ),
                
                box(
                  title = "Uji Rata-rata Dua Sampel", 
                  status = "info", 
                  solidHeader = TRUE, 
                  width = 6,
                  selectInput("var_two_sample", "Pilih Variabel Numerik:",
                              choices = c("Pilih Variabel..." = "")),
                  selectInput("group_two_sample", "Pilih Variabel Pengelompokan:",
                              choices = c("Pilih Variabel..." = "")),
                  checkboxInput("equal_var", "Asumsi Varians Sama", value = TRUE),
                  actionButton("two_sample_btn", "Uji Two Sample t-test", class = "btn-info"),
                  hr(),
                  conditionalPanel(
                    condition = "input.var_two_sample == '' || input.group_two_sample == '' || input.var_two_sample == null || input.group_two_sample == null",
                    div(class = "placeholder-message", "Silakan pilih kedua variabel terlebih dahulu")
                  ),
                  conditionalPanel(
                    condition = "input.var_two_sample != '' && input.group_two_sample != '' && input.var_two_sample != null && input.group_two_sample != null",
                    withSpinner(verbatimTextOutput("two_sample_result")),
                    hr(),
                    h4("Interpretasi:"),
                    withSpinner(uiOutput("two_sample_interp"))
                  )
                )
              )
      ),
      
      # Tab Uji Proporsi & Varians
      tabItem(tabName = "uji_prop",
              
              fluidRow(
                box(width = 12, status = "primary", solidHeader = TRUE,
                    title = "Unduh Laporan",
                    downloadButton("unduh_laporan_uji_prop_var", "Unduh Laporan Uji Proporsi & Varians (PDF) 📝", class = "btn-primary")
                )
              ),
              
              fluidRow(
                box(
                  title = "Uji Proporsi", 
                  status = "primary", 
                  solidHeader = TRUE, 
                  width = 6,
                  selectInput("var_proporsi", "Pilih Variabel Kategorik:",
                              choices = c("Pilih Variabel..." = "")),
                  numericInput("p_test", "Proporsi yang diuji (p₀):", value = 0.5, min = 0, max = 1),
                  actionButton("prop_test_btn", "Uji Proporsi", class = "btn-primary"),
                  hr(),
                  conditionalPanel(
                    condition = "input.var_proporsi == '' || input.var_proporsi == null",
                    div(class = "placeholder-message", "Silakan pilih variabel terlebih dahulu")
                  ),
                  conditionalPanel(
                    condition = "input.var_proporsi != '' && input.var_proporsi != null",
                    withSpinner(verbatimTextOutput("prop_test_result")),
                    hr(),
                    h4("Interpretasi:"),
                    withSpinner(uiOutput("prop_test_interp"))
                  )
                ),
                
                box(
                  title = "Uji Varians", 
                  status = "success", 
                  solidHeader = TRUE, 
                  width = 6,
                  selectInput("var_variance", "Pilih Variabel untuk Uji Varians:",
                              choices = c("Pilih Variabel..." = "")),
                  numericInput("sigma_test", "Varians yang diuji (σ²₀):", value = 1),
                  actionButton("var_test_btn", "Uji Varians Satu Sampel", class = "btn-success"),
                  hr(),
                  selectInput("group_var_test", "Pilih Variabel Pengelompokan (untuk uji 2 sampel):",
                              choices = c("Pilih Variabel..." = "")),
                  actionButton("var_test_two_btn", "Uji Varians Dua Sampel", class = "btn-warning"),
                  hr(),
                  conditionalPanel(
                    condition = "input.var_variance == '' || input.var_variance == null",
                    div(class = "placeholder-message", "Silakan pilih variabel terlebih dahulu")
                  ),
                  conditionalPanel(
                    condition = "input.var_variance != '' && input.var_variance != null",
                    withSpinner(verbatimTextOutput("var_test_result")),
                    hr(),
                    h4("Interpretasi:"),
                    withSpinner(uiOutput("var_test_interp"))
                  )
                )
              )
      ),
      
      # Tab ANOVA
      tabItem(tabName = "anova",
              
              fluidRow(
                box(width = 12, status = "primary", solidHeader = TRUE,
                    title = "Unduh Laporan",
                    downloadButton("unduh_laporan_anova", "Unduh Laporan ANOVA (PDF) 📝", class = "btn-primary")
                )
              ),
              
              fluidRow(
                box(
                  title = "ANOVA Satu Arah", 
                  status = "primary", 
                  solidHeader = TRUE, 
                  width = 6,
                  selectInput("var_anova_dep", "Variabel Dependen (Numerik):",
                              choices = c("Pilih Variabel..." = "")),
                  selectInput("var_anova_indep", "Variabel Independen (Faktor):",
                              choices = c("Pilih Variabel..." = "")),
                  actionButton("anova_one_btn", "ANOVA Satu Arah", class = "btn-primary"),
                  hr(),
                  conditionalPanel(
                    condition = "input.var_anova_dep == '' || input.var_anova_indep == '' || input.var_anova_dep == null || input.var_anova_indep == null",
                    div(class = "placeholder-message", "Silakan pilih kedua variabel terlebih dahulu")
                  ),
                  conditionalPanel(
                    condition = "input.var_anova_dep != '' && input.var_anova_indep != '' && input.var_anova_dep != null && input.var_anova_indep != null",
                    withSpinner(verbatimTextOutput("anova_one_result")),
                    hr(),
                    h4("Interpretasi:"),
                    withSpinner(uiOutput("anova_one_interp")),
                    downloadButton("download_plot_anova", "PNG", class="btn-xs pull-right"),
                  )
                ),
                
                box(
                  title = "ANOVA Dua Arah", 
                  status = "info", 
                  solidHeader = TRUE, 
                  width = 6,
                  selectInput("var_anova2_dep", "Variabel Dependen:",
                              choices = c("Pilih Variabel..." = "")),
                  selectInput("var_anova2_factor1", "Faktor 1:",
                              choices = c("Pilih Variabel..." = "")),
                  selectInput("var_anova2_factor2", "Faktor 2:",
                              choices = c("Pilih Variabel..." = "")),
                  checkboxInput("interaction", "Sertakan Interaksi", value = TRUE),
                  actionButton("anova_two_btn", "ANOVA Dua Arah", class = "btn-info"),
                  hr(),
                  conditionalPanel(
                    condition = "input.var_anova2_dep == '' || input.var_anova2_factor1 == '' || input.var_anova2_factor2 == '' || input.var_anova2_dep == null || input.var_anova2_factor1 == null || input.var_anova2_factor2 == null",
                    div(class = "placeholder-message", "Silakan pilih semua variabel terlebih dahulu")
                  ),
                  conditionalPanel(
                    condition = "input.var_anova2_dep != '' && input.var_anova2_factor1 != '' && input.var_anova2_factor2 != '' && input.var_anova2_dep != null && input.var_anova2_factor1 != null && input.var_anova2_factor2 != null",
                    withSpinner(verbatimTextOutput("anova_two_result")),
                    hr(),
                    h4("Interpretasi:"),
                    withSpinner(uiOutput("anova_two_interp"))
                  )
                )
              )
      ),
      
      # Tab Regresi Linear Berganda
      tabItem(tabName = "regresi",
              fluidRow(
                box(
                  title = "Pengaturan Model Regresi", 
                  status = "primary", 
                  solidHeader = TRUE, 
                  width = 4,
                  selectInput("var_dependent", "Variabel Dependen (Y):",
                              choices = c("Pilih Variabel..." = "")),
                  checkboxGroupInput("var_independent", "Variabel Independen (X):",
                                     choices = NULL),
                  actionButton("run_regression", "Jalankan Regresi", class = "btn-primary"),
                  hr(),
                  h4("Uji Asumsi Regresi"),
                  actionButton("uji_asumsi_reg", "Uji Asumsi Regresi", class = "btn-warning"),
                  hr(),
                  h6("Jalankan Uji Regressi dahulu untuk mengunduh laporan"),
                  downloadButton("unduh_laporan_regresi", "Unduh Laporan Lengkap (PDF) 📝", class="btn-success")
                ),
                
                box(
                  title = "Hasil Regresi Linear Berganda", 
                  status = "success", 
                  solidHeader = TRUE, 
                  width = 8,
                  conditionalPanel(
                    condition = "input.var_dependent == '' || input.var_dependent == null || !input.var_independent || input.var_independent.length == 0",
                    div(class = "placeholder-message", 
                        h4("Silakan pilih variabel dependen dan minimal satu variabel independen"))
                  ),
                  conditionalPanel(
                    condition = "input.var_dependent != '' && input.var_dependent != null && input.var_independent && input.var_independent.length > 0",
                    withSpinner(verbatimTextOutput("regression_summary")),
                    hr(),
                    h4("Interpretasi:"),
                    withSpinner(uiOutput("regression_interpretation"))
                  )
                )
              ),
              
              fluidRow(
                box(
                  
                  title = tags$span(
                    "Uji Asumsi Regresi", 
                    downloadButton("download_plot_reg_asumsi", "PNG", class="btn-xs pull-right", style="margin-top: -5px;")
                  ),
                  
                  status = "warning", 
                  solidHeader = TRUE, 
                  width = 12,
                  conditionalPanel(
                    condition = "!output.regression_summary",
                    div(class = "placeholder-message", 
                        h4("Silakan jalankan regresi terlebih dahulu sebelum uji asumsi"))
                  ),
                  conditionalPanel(
                    condition = "output.regression_summary",
                    withSpinner(plotOutput("assumption_plots")),
                    withSpinner(verbatimTextOutput("assumption_tests")),
                    hr(), 
                    h4("Interpretasi Asumsi:"),
                    withSpinner(uiOutput("interpretation_assumption_tests"))
                  )
                )
              )
      ),
      
      # TAB BARU UNTUK PEMETAAN
      tabItem(tabName = "pemetaan",
              fluidRow(
                box(
                  title = "Pengaturan Peta",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 3,
                  
                  selectInput("var_map", "Pilih Variabel untuk Dipetakan:",
                              choices = NULL), 
                  selectInput("pilih_provinsi", "Filter Berdasarkan Provinsi:",
                              choices = pilihan_provinsi),
                  
                  downloadButton("download_peta", "Unduh Peta (PNG)", class = "btn-success"),
                  
                  hr(),
                  p("Butuh waktu untuk melakukan rendering."),
                  p("Peta ini akan menampilkan sebaran variabel yang Anda pilih berdasarkan provinsi.Warna yang lebih gelap menunjukkan nilai yang lebih tinggi.")
                ),
                box(
                  title = "Peta Sebaran Variabel",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 9,
                  
                  # Output untuk peta leaflet
                  withSpinner(leafletOutput("peta_sebaran", height = "600px"))
                )
              )
      )
    )
  )
)

# Server 
server <- function(input, output, session) {
  
  values <- reactiveValues(
    processed_data = sovi_data,
    original_data = sovi_data,
    regression_model = NULL
  )
  
  norm_test_results <- reactiveVal(NULL)
  homog_test_results <- reactiveVal(NULL)
  one_sample_t_result <- reactiveVal(NULL)
  two_sample_t_result <- reactiveVal(NULL)
  prop_test_result_val <- reactiveVal(NULL)
  var_test_result_val <- reactiveVal(NULL)
  anova_one_result_val <- reactiveVal(NULL)
  anova_two_result_val <- reactiveVal(NULL)
  reg_assumption_results <- reactiveVal(NULL)
  categorization_details <- reactiveVal(NULL)
  
  observe({
    numeric_vars <- names(select_if(values$processed_data, is.numeric))
    all_vars <- names(values$processed_data)
    categorical_vars <- names(select_if(values$processed_data, function(x) !is.numeric(x) || length(unique(x)) <= 10))
    updateSelectInput(session, "var_eksplorasi", choices = all_vars)
    
    updateSelectInput(session, "var_map", 
                      choices = c("Pilih Variabel..." = "", numeric_vars))
    
    
    updateSelectInput(session, "var_normalitas", 
                      choices = c("Pilih Variabel..." = "", numeric_vars))
    updateSelectInput(session, "var_homogenitas", 
                      choices = c("Pilih Variabel..." = "", numeric_vars))
    updateSelectInput(session, "group_var", 
                      choices = c("Pilih Variabel..." = "", all_vars))
    updateSelectInput(session, "var_one_sample", 
                      choices = c("Pilih Variabel..." = "", numeric_vars))
    updateSelectInput(session, "var_two_sample", 
                      choices = c("Pilih Variabel..." = "", numeric_vars))
    updateSelectInput(session, "group_two_sample", 
                      choices = c("Pilih Variabel..." = "", all_vars))
    updateSelectInput(session, "var_proporsi", 
                      choices = c("Pilih Variabel..." = "", categorical_vars))
    updateSelectInput(session, "var_variance", 
                      choices = c("Pilih Variabel..." = "", numeric_vars))
    updateSelectInput(session, "group_var_test", 
                      choices = c("Pilih Variabel..." = "", all_vars))
    updateSelectInput(session, "var_anova_dep", 
                      choices = c("Pilih Variabel..." = "", numeric_vars))
    updateSelectInput(session, "var_anova_indep", 
                      choices = c("Pilih Variabel..." = "", all_vars))
    updateSelectInput(session, "var_anova2_dep", 
                      choices = c("Pilih Variabel..." = "", numeric_vars))
    updateSelectInput(session, "var_anova2_factor1", 
                      choices = c("Pilih Variabel..." = "", all_vars))
    updateSelectInput(session, "var_anova2_factor2", 
                      choices = c("Pilih Variabel..." = "", all_vars))
    updateSelectInput(session, "var_dependent", 
                      choices = c("Pilih Variabel..." = "", numeric_vars))
    updateCheckboxGroupInput(session, "var_independent", 
                             choices = numeric_vars)
  })
  
  # Tab Beranda 
  output$total_obs <- renderValueBox({
    valueBox(
      value = nrow(values$processed_data),
      subtitle = "Total Observasi",
      icon = icon("list"),
      color = "blue"
    )
  })
  
  output$total_vars <- renderValueBox({
    valueBox(
      value = ncol(values$processed_data),
      subtitle = "Total Variabel",
      icon = icon("columns"),
      color = "green"
    )
  })
  
  output$missing_data <- renderValueBox({
    valueBox(
      value = sum(is.na(values$processed_data)),
      subtitle = "Missing Values",
      icon = icon("exclamation-triangle"),
      color = "yellow"
    )
  })
  
  # Tab Eksplorasi Data
  output$desc_stats_table <- renderTable({
    req(input$var_eksplorasi)
    var_data <- values$processed_data[[input$var_eksplorasi]]
    
    if (is.null(var_data) || input$var_eksplorasi == "") return(NULL)
  
    if(is.numeric(var_data)) {
      stats <- data.frame(
        Statistik = c("Jumlah Observasi (N)", 
                      "Rata-rata (Mean)", 
                      "Standar Deviasi", 
                      "Nilai Minimum", 
                      "Kuartil 1 (Q1)", 
                      "Median", 
                      "Kuartil 3 (Q3)", 
                      "Nilai Maksimum"),
        Nilai = c(length(na.omit(var_data)),
                  mean(var_data, na.rm = TRUE),
                  sd(var_data, na.rm = TRUE),
                  min(var_data, na.rm = TRUE),
                  quantile(var_data, 0.25, na.rm = TRUE),
                  median(var_data, na.rm = TRUE),
                  quantile(var_data, 0.75, na.rm = TRUE),
                  max(var_data, na.rm = TRUE)
        )
      )
      stats$Nilai <- round(stats$Nilai, 4)
      return(stats)
      
    } else {
      freq_table <- as.data.frame(table(var_data, useNA = "ifany"))
      colnames(freq_table) <- c("Kategori", "Frekuensi")
      return(freq_table)
    }
  }, striped = TRUE, hover = TRUE, bordered = TRUE, align = 'lc')
  
  plot_hist_reactive <- reactive({
    req(input$var_eksplorasi, is.numeric(values$processed_data[[input$var_eksplorasi]]))
    ggplot(values$processed_data, aes_string(x = input$var_eksplorasi)) +
      geom_histogram(bins = 30, fill = "steelblue", alpha = 0.8) + theme_minimal() + 
      labs(title = paste("Histogram:", input$var_eksplorasi))
  })
  
  
  output$histogram_interpretation <- renderUI({
    req(input$var_eksplorasi)
    var_data <- values$processed_data[[input$var_eksplorasi]]
    
    if (!is.numeric(var_data)) {
      return(tags$p("Interpretasi histogram hanya tersedia untuk variabel numerik."))
    }
    
    var_data <- na.omit(var_data)
    mean_val <- mean(var_data)
    median_val <- median(var_data)
    sd_val <- sd(var_data)
    skewness <- 3 * (mean_val - median_val) / sd_val
    
    if (abs(skewness) < 0.5) {
      tags$p("Distribusi terlihat ", tags$b("hampir simetris"), ". Nilai rata-rata (", round(mean_val, 2), ") dan median (", round(median_val, 2), ") memiliki nilai yang hampir sama.")
    } else if (skewness > 0.5) {
      tags$p("Distribusi terlihat ", tags$b("menjulur ke kanan (positive skew)"), ". Ini menandakan sebagian besar data terkonsentrasi pada nilai-nilai yang lebih rendah, dengan beberapa nilai tinggi sebagai pencilan. Rata-rata (", round(mean_val, 2), ") lebih besar dari median (", round(median_val, 2), ").")
    } else {
      tags$p("Distribusi terlihat ", tags$b("menjulur ke kiri (negative skew)"), ". Ini menandakan sebagian besar data terkonsentrasi pada nilai-nilai yang lebih tinggi. Rata-rata (", round(mean_val, 2), ") lebih kecil dari median (", round(median_val, 2), ").")
    }
  })
  
  plot_box_reactive <- reactive({
    req(input$var_eksplorasi, is.numeric(values$processed_data[[input$var_eksplorasi]]))
    ggplot(values$processed_data, aes_string(y = input$var_eksplorasi)) +
      geom_boxplot(fill = "lightblue", alpha = 0.8) + theme_minimal() + 
      labs(title = paste("Boxplot:", input$var_eksplorasi))
  })
 
  output$boxplot_interpretation <- renderUI({
    req(input$var_eksplorasi)
    var_data <- values$processed_data[[input$var_eksplorasi]]
    
    if (!is.numeric(var_data)) {
      return(tags$p("Interpretasi boxplot hanya tersedia untuk variabel numerik."))
    }
    
    var_data <- na.omit(var_data)
    q1 <- quantile(var_data, 0.25)
    median_val <- median(var_data)
    q3 <- quantile(var_data, 0.75)
    iqr <- q3 - q1
    lower_bound <- q1 - 1.5 * iqr
    upper_bound <- q3 + 1.5 * iqr
    outliers <- var_data[var_data < lower_bound | var_data > upper_bound]

    outlier_text <- if (length(outliers) == 0) {
      "Berdasarkan metode IQR, tidak terdeteksi adanya outlier yang signifikan."
    } else {
      tagList("Terdeteksi ", tags$b(length(outliers)), " titik data yang berpotensi menjadi outlier. Nilai-nilai ini berada di luar rentang wajar data (di luar 'kumis' boxplot).")
    }
    
    tagList(
      tags$ul(
       
        tags$li("Garis tengah di dalam kotak (median) berada pada nilai ", tags$b(round(median_val, 2)), ". Ini adalah nilai tengah dari data."),
        tags$li("50% dari data berada di dalam kotak (rentang interkuartil), yaitu antara ", tags$b(round(q1, 2)), " (Kuartil 1) dan ", tags$b(round(q3, 2)), " (Kuartil 3)."),
        tags$li(outlier_text)
      )
    )
  })
  

  plot_density_reactive <- reactive({
    req(input$var_eksplorasi, is.numeric(values$processed_data[[input$var_eksplorasi]]))
    ggplot(values$processed_data, aes_string(x = input$var_eksplorasi)) +
      geom_density(fill = "salmon", alpha = 0.7) + theme_minimal() +
      labs(title = paste("Plot Kepadatan:", input$var_eksplorasi))
  })

  output$density_interpretation <- renderUI({
    req(input$var_eksplorasi)
    var_data <- values$processed_data[[input$var_eksplorasi]]
    if (!is.numeric(var_data)) {
      return(tags$p("Plot kepadatan hanya tersedia untuk variabel numerik."))
    }
    
    density_info <- density(na.omit(var_data))
    peak_val <- density_info$x[which.max(density_info$y)]
    
 
    tags$p(
      "Plot kepadatan ini menunjukkan di mana konsentrasi nilai tertinggi berada. Puncak kurva (modus) mengindikasikan nilai yang paling sering muncul, yaitu di sekitar ",
      tags$b(round(peak_val, 2)), "."
    )
  })
  
  # Scatter Plot
  plot_scatter_reactive <- reactive({
    req(input$var_eksplorasi, is.numeric(values$processed_data[[input$var_eksplorasi]]))
    plot_data <- values$processed_data %>% select(all_of(input$var_eksplorasi)) %>%
      filter(!is.na(.)) %>% mutate(index = row_number())
    ggplot(plot_data, aes_string(x = "index", y = input$var_eksplorasi)) +
      geom_point(alpha = 0.6, color = "purple") + theme_minimal() +
      labs(title = paste("Scatter Plot:", input$var_eksplorasi), x = "Indeks Observasi")
  })
  
 
  output$scatter_interpretation <- renderUI({
    req(input$var_eksplorasi)
    var_data <- values$processed_data[[input$var_eksplorasi]]
    if (!is.numeric(var_data)) {
      return(tags$p("Scatter plot hanya tersedia untuk variabel numerik."))
    }
    
    tags$p(
      "Scatter plot ini memvisualisasikan sebaran setiap titik data terhadap indeks (urutan) datanya. Plot ini dapat membantu melihat adanya tren, pola, atau outlier seiring berjalannya observasi."
    )
  })
  
  # Bar Plot
  plot_bar_reactive <- reactive({
    req(input$var_eksplorasi)
    plot_data <- as.data.frame(table(values$processed_data[[input$var_eksplorasi]]))
    colnames(plot_data) <- c("Kategori", "Frekuensi")
    ggplot(plot_data, aes(x = Kategori, y = Frekuensi, fill = Kategori)) +
      geom_bar(stat = "identity", alpha = 0.8) + theme_minimal() +
      labs(title = paste("Bar Plot:", input$var_eksplorasi)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # 5. Interpretasi untuk Bar Plot 
  output$bar_plot_interpretation <- renderUI({
    req(input$var_eksplorasi)
    var_data <- values$processed_data[[input$var_eksplorasi]]
  
    if (is.numeric(var_data) && length(unique(na.omit(var_data))) >= 20) {
      return(tags$p("Bar plot tidak direkomendasikan untuk variabel numerik dengan banyak nilai unik."))
    }
    
    freq_table <- sort(table(na.omit(var_data)), decreasing = TRUE)
    
    if(nrow(freq_table) == 0) {
      return(tags$p("Tidak ada data untuk ditampilkan."))
    }
    
    most_frequent_cat <- names(freq_table)[1]
    
 
    tags$p(
      "Plot ini menunjukkan frekuensi (jumlah kemunculan) dari setiap kategori. Kategori yang paling sering muncul adalah ",
      tags$b(most_frequent_cat), " dengan total ", tags$b(freq_table[1]), " observasi."
    )
  })
  
  output$data_table <- DT::renderDataTable({
    DT::datatable(values$processed_data, options = list(scrollX = TRUE, pageLength = 10))
  })
  
 
  output$preview_data <- DT::renderDataTable({
    data_to_show <- values$processed_data
    
    if(input$show_records == -1) {
      page_length <- nrow(data_to_show)
      length_menu <- list(c(-1, 10, 25, 50, 100), c('All', '10', '25', '50', '100'))
    } else {
      page_length <- as.numeric(input$show_records)
      length_menu <- list(c(10, 25, 50, 100, -1), c('10', '25', '50', '100', 'All'))
    }
    
    DT::datatable(data_to_show, 
                  options = list(
                    scrollX = TRUE, 
                    pageLength = page_length,
                    lengthMenu = length_menu,
                    dom = 'Blfrtip'
                  ))
  })
  
  
  # --- LOGIKA BARU UNTUK KATEGORISASI MANUAL ---
  
  message_reactive_manual <- reactiveVal()
 
  observe({
    req(values$processed_data)
    numeric_vars <- names(select_if(values$processed_data, is.numeric))
    
  
    selected_var_cut <- isolate(input$var_to_cut)
    selected_var_transform <- isolate(input$var_transformasi)
    
    updateSelectInput(session, "var_to_cut", choices = numeric_vars, selected = selected_var_cut)
    updateSelectInput(session, "var_transformasi", choices = numeric_vars, selected = selected_var_transform)
  })
  
  
  output$manual_interval_inputs <- renderUI({
    req(input$num_bins_manual, input$var_to_cut) 
    num_categories <- as.integer(input$num_bins_manual)
    req(num_categories >= 2)
    
    num_breaks <- num_categories + 1
   
    var_data <- values$processed_data[[input$var_to_cut]]
    data_summary <- if(!is.null(var_data) && is.numeric(var_data)) {
      summary_vals <- summary(na.omit(var_data))
      paste0("Info Variabel '", input$var_to_cut, "': Min = ", round(summary_vals[[1]], 2), 
             ", Median = ", round(summary_vals[[3]], 2),
             ", Max = ", round(summary_vals[[6]], 2))
    } else {
      "Pilih variabel numerik untuk melihat info rentang data."
    }
  
    boundary_inputs <- lapply(1:num_breaks, function(i) {
      label <- if (i == 1) "Batas Paling Awal (misal: 0)" else paste("Batas ke-", i)
      numericInput(paste0("manual_break_", i), label, value = NA) 
    })
 
    label_inputs <- lapply(1:num_categories, function(i) {
      textInput(paste0("manual_label_", i), paste("Nama Kategori", i), value = paste0("Kelompok ", i))
    })
    
    tagList(
      div(class = "category-section",
          p(tags$i(data_summary)),
          p(tags$b("Instruksi:"), "Masukkan semua batas interval secara manual, dari nilai terkecil hingga terbesar. Anda bebas menentukan nilai di luar rentang data (misal: 0 atau 1000) untuk membuat kategori yang sesuai.")
      ),
      fluidRow(
        column(6, 
               div(class = "boundary-section",
                   h5(tags$b("Definisikan Batas-Batas Interval")), 
                   boundary_inputs
               )
        ),
        column(6, 
               div(class = "category-section",
                   h5(tags$b("Definisikan Nama Kategori")), 
                   label_inputs
               )
        )
      )
    )
  })
 
  observeEvent(input$cut_data_manual, {
    req(input$var_to_cut, input$num_bins_manual)
    
    var_name <- input$var_to_cut
    num_categories <- as.integer(input$num_bins_manual)
    
    user_breaks <- sapply(1:(num_categories + 1), function(i) input[[paste0("manual_break_", i)]])
    user_labels <- sapply(1:num_categories, function(i) {
      label <- input[[paste0("manual_label_", i)]]
      if (is.null(label) || label == "") paste0("Kategori ", i) else label
    })
 
    if (any(is.null(user_breaks)) || any(is.na(user_breaks)) || is.unsorted(user_breaks, na.rm = TRUE)) {
      showNotification("Error: Semua batas interval harus diisi dan diurutkan dari terkecil ke terbesar.", type = "error")
      categorization_details(NULL) 
      return()
    }
    
    current_data <- values$processed_data
    cut_var_name <- paste0("Kategori_", var_name)
  
    cut_result <- cut(
      current_data[[var_name]],
      breaks = user_breaks,
      labels = user_labels,
      right = TRUE,
      include.lowest = TRUE
    )
    
    current_data[[cut_var_name]] <- cut_result
    values$processed_data <- current_data
   
    categorization_details(list(
      var_name = var_name,
      new_var_name = cut_var_name,
      breaks = user_breaks,
      labels = user_labels
    ))
    
    showNotification("Kategorisasi berhasil!", type = "message")
  })

  output$interpretation_cut_manual <- renderUI({
    req(categorization_details()) 
    details <- categorization_details()
    num_categories <- length(details$labels)
    range_list <- lapply(1:num_categories, function(i) {
      lower_bound <- round(details$breaks[i], 2)
      upper_bound <- round(details$breaks[i + 1], 2)
      
      interval_str <- if (i == 1) {
        paste0("[", lower_bound, ", ", upper_bound, "]")
      } else {
        paste0("(", lower_bound, ", ", upper_bound, "]")
      }
      
      tags$li(tags$b(details$labels[i]), ": mencakup nilai dalam rentang ", tags$code(interval_str))
    })

    tagList(
      h4("Interpretasi Proses Kategorisasi Berhasil"),
      p("Variabel ", tags$strong(details$var_name), " telah berhasil dibagi menjadi ", tags$strong(num_categories), " kelompok."),
      p("Kolom baru bernama ", tags$strong(details$new_var_name), " telah ditambahkan ke dalam dataset."),
      p("Rincian kategori:"),
      tags$ul(range_list)
    )
  })
 
  observeEvent(input$transformasi_btn, {
    req(input$var_transformasi, input$jenis_transformasi, input$nama_var_transformasi)
    
    tryCatch({
      if(input$var_transformasi == "" || input$jenis_transformasi == "" || input$nama_var_transformasi == "") {
        showNotification("Mohon lengkapi semua field!", type = "error")
        return()
      }
      
      var_data <- values$processed_data[[input$var_transformasi]]
      
      if(is.numeric(var_data)) {
        transformed_data <- switch(input$jenis_transformasi,
                                   "log" = {
                                     min_val <- min(var_data, na.rm = TRUE)
                                     if(min_val <= 0) {
                                       shifted_data <- var_data - min_val + 1
                                       log(shifted_data)
                                     } else {
                                       log(var_data)
                                     }
                                   },
                                   "inverse" = {
                                     ifelse(abs(var_data) < 1e-10, NA, 1 / var_data)
                                   },
                                   "inverse_square" = {
                                     ifelse(abs(var_data) < 1e-10, NA, 1 / (var_data^2))
                                   }
        )
        
        values$processed_data[[input$nama_var_transformasi]] <- transformed_data
        
        output$status_transformasi <- renderText({
          n_na <- sum(is.na(transformed_data))
          n_total <- length(transformed_data)
          paste("✓ Berhasil membuat variabel:", input$nama_var_transformasi, 
                "\n  Total observasi:", n_total,
                "\n  Missing values:", n_na,
                "\n  Valid values:", n_total - n_na)
        })
        
        showNotification("Transformasi berhasil! Periksa tabel 'Preview Data' untuk melihat variabel baru.", type = "success")
      } else {
        showNotification("Variabel harus berupa data numerik!", type = "error")
      }
    }, error = function(e) {
      showNotification(paste("Error dalam transformasi:", e$message), type = "error")
    })
  })
 
  output$download_peta <- downloadHandler(
    filename = function() {
      paste0("peta_sebaran_", input$var_map, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      data_peta <- data_peta_reaktif()
      pal <- colorNumeric(
        palette = "YlOrRd",
        domain = data_peta[[input$var_map]],
        na.color = "#bdbdbd"
      )
      
      peta_statis <- leaflet(data = data_peta) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = 118, lat = -2, zoom = 5) %>%
        addPolygons(
          fillColor = ~pal(get(input$var_map)),
          weight = 1.5,
          opacity = 1,
          color = "black",
          dashArray = "1",
          fillOpacity = 0.7
        ) %>%
        addLegend(
          pal = pal,
          values = ~get(input$var_map),
          opacity = 0.7,
          title = input$var_map,
          position = "bottomright"
        )
      htmlwidgets::saveWidget(peta_statis, "temp_peta.html", selfcontained = FALSE)
      webshot2::webshot(
        url = "temp_peta.html",
        file = file,
        vwidth = 1200, 
        vheight = 900, 
        delay = 2 
      )
    }
  )
  
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste("processed_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(values$processed_data, file, row.names = FALSE)
    }
  )
  
  # Uji Normalitas
  observeEvent(input$uji_normalitas_btn, {
    req(input$var_normalitas)
    
    if(input$var_normalitas == "") {
      showNotification("Pilih variabel terlebih dahulu!", type = "error")
      return()
    }
    
    var_data <- values$processed_data[[input$var_normalitas]]
    var_data <- na.omit(var_data)
    
   
    if(is.numeric(var_data) && length(var_data) > 3) {
      shapiro_res <- if(length(var_data) <= 5000) shapiro.test(var_data) else NULL
      ks_res <- ks.test(var_data, "pnorm", mean(var_data), sd(var_data))
      norm_test_results(list(shapiro = shapiro_res, ks = ks_res))
    } else {
      norm_test_results(NULL)
    }
    
    output$hasil_normalitas <- renderPrint({
    
      cat("UJI NORMALITAS\n")
      results <- norm_test_results()
      if(!is.null(results)) {
        if(!is.null(results$shapiro)){
          cat("1. SHAPIRO-WILK TEST:\n")
          print(results$shapiro)
        }
        cat("\n2. KOLMOGOROV-SMIRNOV TEST:\n")
        print(results$ks)
      } else {
        cat("Error: Variabel harus numerik dengan minimal 4 observasi!")
      }
    })
  })
  
  plot_qq_reactive <- reactive({
    req(norm_test_results())
    var_data <- na.omit(values$processed_data[[input$var_normalitas]])
    ggplot(data.frame(x = var_data), aes(sample = x)) +
      stat_qq() + stat_qq_line(color = "red", linetype="dashed") +
      labs(title = paste("Q-Q Plot:", input$var_normalitas)) + theme_minimal()
  })
  
 
  output$interpretasi_normalitas <- renderUI({
    req(norm_test_results())
    results <- norm_test_results()
    alpha <- 0.05
    
  
    shapiro_interp <- if(!is.null(results$shapiro)) {
      p_val <- results$shapiro$p.value
      kesimpulan <- if (p_val > alpha) {
        tagList("data terdistribusi ", tags$b("NORMAL"), ".")
      } else {
        tagList("data terdistribusi ", tags$b("TIDAK NORMAL"), ".")
      }
      tags$li("Uji Shapiro-Wilk: Dengan p-value sebesar ", tags$b(round(p_val, 4)), ", dapat disimpulkan bahwa ", kesimpulan)
    }
   
    ks_p_val <- results$ks$p.value
    ks_kesimpulan <- if (ks_p_val > alpha) {
      tagList("data terdistribusi ", tags$b("NORMAL"), ".")
    } else {
      tagList("data terdistribusi ", tags$b("TIDAK NORMAL"), ".")
    }
    
    tagList(
      tags$ul(
        shapiro_interp,
        tags$li("Uji Kolmogorov-Smirnov: Dengan p-value sebesar ", tags$b(round(ks_p_val, 4)), ", dapat disimpulkan bahwa ", ks_kesimpulan),
        tags$li("Q-Q Plot: Jika titik-titik data menyebar di sekitar garis diagonal merah, maka asumsi normalitas terpenuhi. Jika menyebar jauh, maka asumsi tidak terpenuhi.")
      )
    )
  })
  
  # Uji Homogenitas
  observeEvent(input$uji_homogenitas_btn, {
    req(input$var_homogenitas, input$group_var)
    
    var_data <- values$processed_data[[input$var_homogenitas]]
    group_data <- values$processed_data[[input$group_var]]
    
   
    complete_cases <- complete.cases(var_data, group_data)
    var_data <- var_data[complete_cases]
    group_data <- as.factor(group_data[complete_cases])
    
   
    group_data <- droplevels(group_data)
    
   
    if (length(levels(group_data)) < 2) {
      showNotification("Error: Variabel grup harus memiliki minimal 2 kelompok data.", type = "error")
      return() 
    }
  
    if (any(table(group_data) < 2)) {
      showNotification("Error: Setiap kelompok harus memiliki minimal 2 observasi.", type = "error")
      return() 
    }
  
    bartlett_res <- bartlett.test(var_data ~ group_data)
    fligner_res <- fligner.test(var_data ~ group_data)
    homog_test_results(list(bartlett = bartlett_res, fligner = fligner_res))
    
    output$hasil_homogenitas <- renderPrint({
      results <- homog_test_results()
      if(!is.null(results)){
        cat("BARTLETT TEST:\n")
        print(results$bartlett)
        cat("\nFLIGNER-KILLEEN TEST (Robust):\n")
        print(results$fligner)
      } else {
        cat("Error: Pastikan variabel numerik dan variabel pengelompokan memiliki minimal 2 kelompok!")
      }
    })
  })
 
  output$interpretasi_homogenitas <- renderUI({
    req(homog_test_results())
    results <- homog_test_results()
    alpha <- 0.05
    
  
    p_val_bartlett <- results$bartlett$p.value
    kesimpulan_bartlett <- if(p_val_bartlett > alpha) {
      tagList("varians antar kelompok bersifat ", tags$b("HOMOGEN"), " (sama).")
    } else {
      tagList("varians antar kelompok bersifat ", tags$b("HETEROGEN"), " (berbeda).")
    }
    
 
    p_val_fligner <- results$fligner$p.value
    kesimpulan_fligner <- if(p_val_fligner > alpha) {
      tagList("varians antar kelompok bersifat ", tags$b("HOMOGEN"), " (sama).")
    } else {
      tagList("varians antar kelompok bersifat ", tags$b("HETEROGEN"), " (berbeda).")
    }
    
    tagList(
      tags$p("Hipotesis Nol (H0) untuk uji ini adalah varians antar kelompok sama (homogen)."),
      tags$ul(
        tags$li("Uji Bartlett: Dengan p-value sebesar ", tags$b(round(p_val_bartlett, 4)), ", disimpulkan bahwa ", kesimpulan_bartlett),
        tags$li("Uji Fligner-Killeen (lebih robust terhadap non-normalitas): Dengan p-value sebesar ", tags$b(round(p_val_fligner, 4)), ", disimpulkan bahwa ", kesimpulan_fligner)
      )
    )
  })
  
  
  # Uji One Sample t-test
  observeEvent(input$one_sample_btn, {
    req(input$var_one_sample)
    var_data <- values$processed_data[[input$var_one_sample]]
    var_data <- na.omit(var_data)
    
    if(is.numeric(var_data) && length(var_data) > 1) {
      result <- t.test(var_data, mu = input$mu_test)
      one_sample_t_result(result)
    }
  })
  
  output$one_sample_result <- renderPrint({
    req(one_sample_t_result())
    one_sample_t_result()
  })
  
  output$one_sample_interp <- renderUI({
    req(one_sample_t_result())
    result <- one_sample_t_result()
    p_val <- result$p.value
    
    kesimpulan <- if(p_val < 0.05) {
      tagList("adalah ", tags$b("berbeda secara signifikan"), " dari ", input$mu_test, " (karena p-value < 0.05).")
    } else {
      tagList("adalah ", tags$b("tidak berbeda secara signifikan"), " dari ", input$mu_test, " (karena p-value >= 0.05).")
    }
    
    tags$p("Rata-rata sampel untuk ", tags$b(input$var_one_sample), " adalah ", tags$b(round(result$estimate, 3)), ". Berdasarkan hasil uji, dapat disimpulkan bahwa rata-rata populasi ", kesimpulan)
  })
  
  # Uji Two Sample t-test
  observeEvent(input$two_sample_btn, {
    req(input$var_two_sample, input$group_two_sample)
    var_data <- values$processed_data[[input$var_two_sample]]
    group_data <- as.factor(values$processed_data[[input$group_two_sample]])
    
    if(is.numeric(var_data) && length(levels(group_data)) == 2) {
      result <- t.test(var_data ~ group_data, var.equal = input$equal_var)
      two_sample_t_result(result)
    }
  })
  
  output$two_sample_result <- renderPrint({
    req(two_sample_t_result())
    two_sample_t_result()
  })
  
  output$two_sample_interp <- renderUI({
    req(two_sample_t_result())
    result <- two_sample_t_result()
    p_val <- result$p.value
    
    kesimpulan <- if(p_val < 0.05) {
      tags$b("terdapat perbedaan rata-rata yang signifikan")
    } else {
      tags$b("tidak terdapat perbedaan rata-rata yang signifikan")
    }
    
    tags$p("Berdasarkan hasil uji, ", kesimpulan, " antara dua kelompok (p-value = ", round(p_val, 4), "). Rata-rata untuk kedua kelompok adalah ", round(result$estimate[1], 3), " dan ", round(result$estimate[2], 3), ".")
  })
  
  # UJI PROPORSI
  observeEvent(input$prop_test_btn, {
    req(input$var_proporsi)
    var_data <- na.omit(values$processed_data[[input$var_proporsi]])
    
    if(length(unique(var_data)) == 2) {
      success_count <- sum(var_data == unique(var_data)[1])
      total_count <- length(var_data)
      result <- prop.test(success_count, total_count, p = input$p_test)
      prop_test_result_val(list(test = result, category = unique(var_data)[1]))
    }
  })
  
  output$prop_test_result <- renderPrint({
    req(prop_test_result_val())
    prop_test_result_val()$test
  })
  
  output$prop_test_interp <- renderUI({
    req(prop_test_result_val())
    result <- prop_test_result_val()$test
    cat <- prop_test_result_val()$category
    p_val <- result$p.value
    
    kesimpulan <- if(p_val < 0.05) {
      tagList("berbeda secara signifikan dari ", input$p_test)
    } else {
      tagList("tidak berbeda secara signifikan dari ", input$p_test)
    }
    
    tags$p("Proporsi sampel untuk kategori '", tags$b(cat), "' adalah ", tags$b(round(result$estimate, 3)), ". Hasil uji menunjukkan bahwa proporsi ini ", kesimpulan, " (p-value = ", round(p_val, 4), ").")
  })
  
  # UJI VARIANS
  observeEvent(input$var_test_btn, {
    req(input$var_variance)
    var_data <- na.omit(values$processed_data[[input$var_variance]])
    
    if(is.numeric(var_data) && length(var_data) > 1) {
      n <- length(var_data)
      sample_var <- var(var_data)
      chi_stat <- (n - 1) * sample_var / input$sigma_test
      p_value <- 2 * min(pchisq(chi_stat, n - 1), 1 - pchisq(chi_stat, n - 1))
      var_test_result_val(list(type = "one_sample", p.value = p_value, estimate = sample_var, sigma = input$sigma_test))
    }
  })
  
  observeEvent(input$var_test_two_btn, {
    req(input$var_variance, input$group_var_test)
    var_data <- values$processed_data[[input$var_variance]]
    group_data <- as.factor(values$processed_data[[input$group_var_test]])
    
    if(is.numeric(var_data) && length(levels(group_data)) == 2) {
      result <- var.test(var_data ~ group_data)
      var_test_result_val(list(type = "two_sample", test = result))
    }
  })
  
  output$var_test_result <- renderPrint({
    req(var_test_result_val())
    res <- var_test_result_val()
    if(res$type == "one_sample") {
      cat("One Sample Chi-squared test for variance\n")
      cat("sample variance:", res$estimate, "\n")
      cat("p-value:", res$p.value, "\n")
    } else {
      res$test
    }
  })
  
  output$var_test_interp <- renderUI({
    req(var_test_result_val())
    res <- var_test_result_val()
    
    if(res$type == "one_sample") {
      p_val <- res$p.value
      kesimpulan <- if(p_val < 0.05) "berbeda signifikan" else "tidak berbeda signifikan"
      tags$p("Varians sampel (", tags$b(round(res$estimate, 3)), ") ditemukan ", tags$b(kesimpulan), " dari nilai hipotesis ", res$sigma, " (p-value = ", round(p_val, 4), ").")
    } else {
      p_val <- res$test$p.value
      kesimpulan <- if(p_val < 0.05) "terdapat perbedaan varians yang signifikan" else "tidak terdapat perbedaan varians yang signifikan"
      tags$p("Berdasarkan F-test, ", tags$b(kesimpulan), " antara kedua kelompok (p-value = ", round(p_val, 4), ").")
    }
  })
  
  # ANOVA SATU ARAH
  observeEvent(input$anova_one_btn, {
    req(input$var_anova_dep, input$var_anova_indep)
    dep_data <- values$processed_data[[input$var_anova_dep]]
    indep_data <- as.factor(values$processed_data[[input$var_anova_indep]])
    
    if(is.numeric(dep_data) && length(levels(indep_data)) >= 2) {
      anova_result <- aov(dep_data ~ indep_data)
      anova_one_result_val(summary(anova_result))
    }
  })
  
  output$anova_one_result <- renderPrint({
    req(anova_one_result_val())
    anova_one_result_val()
  })
  
  output$anova_one_interp <- renderUI({
   
    req(anova_one_result_val())

    summary_aov <- anova_one_result_val()
  
    p_val <- summary_aov[[1]][["Pr(>F)"]][1]
    
    req(p_val)
    
    kesimpulan <- if (p_val < 0.05) {
      "terdapat perbedaan rata-rata yang signifikan"
    } else {
      "tidak ada perbedaan rata-rata yang signifikan"
    }
    
    tags$p(
      "Hasil ANOVA menunjukkan bahwa ", tags$b(kesimpulan),
      " pada variabel ", tags$b(isolate(input$var_anova_dep)),
      " di antara kelompok-kelompok ", tags$b(isolate(input$var_anova_indep)),
      " (p-value = ", round(p_val, 4), ")."
    )
  })
  
  # ANOVA DUA ARAH
  observeEvent(input$anova_two_btn, {
    req(input$var_anova2_dep, input$var_anova2_factor1, input$var_anova2_factor2)
    dep_data <- values$processed_data[[input$var_anova2_dep]]
    factor1 <- as.factor(values$processed_data[[input$var_anova2_factor1]])
    factor2 <- as.factor(values$processed_data[[input$var_anova2_factor2]])
    
    if(is.numeric(dep_data) && length(levels(factor1)) >= 2 && length(levels(factor2)) >= 2) {
      formula <- if(input$interaction) dep_data ~ factor1 * factor2 else {dep_data ~ factor1 + factor2}
      anova_result <- aov(formula)
      anova_two_result_val(summary(anova_result))
    }
  })
  
  output$anova_two_result <- renderPrint({
    req(anova_two_result_val())
    anova_two_result_val()
  })
  
  output$anova_two_interp <- renderUI({
    req(anova_two_result_val())
    summary_res <- anova_two_result_val()[[1]]
    p_vals <- summary_res[["Pr(>F)"]]
    
    interp_f1 <- if(p_vals[1] < 0.05) "berpengaruh signifikan" else "tidak berpengaruh signifikan"
    interp_f2 <- if(p_vals[2] < 0.05) "berpengaruh signifikan" else "tidak berpengaruh signifikan"
    
    li_f1 <- tags$li("Faktor ", tags$b(input$var_anova2_factor1), " ", tags$b(interp_f1), " (p-value = ", round(p_vals[1], 4), ").")
    li_f2 <- tags$li("Faktor ", tags$b(input$var_anova2_factor2), " ", tags$b(interp_f2), " (p-value = ", round(p_vals[2], 4), ").")
    
    li_int <- if(input$interaction && !is.na(p_vals[3])) {
      interp_int <- if(p_vals[3] < 0.05) "terdapat efek interaksi yang signifikan" else "tidak terdapat efek interaksi yang signifikan"
      tags$li(tags$b(interp_int), " antara kedua faktor (p-value = ", round(p_vals[3], 4), ").")
    }
    
    tagList(tags$ul(li_f1, li_f2, li_int))
  })
  

  observeEvent(input$pilih_provinsi, {
    req(input$pilih_provinsi) 
    
    proxy <- leafletProxy("peta_sebaran")
    
    if (input$pilih_provinsi == "Seluruh Indonesia") {
     
      proxy %>% setView(lng = 118, lat = -2, zoom = 5)
    } else {
   
      batas_wilayah <- peta_provinsi %>%
        filter(nmprov == input$pilih_provinsi) %>%
        st_bbox() %>%
        as.list()
      
      proxy %>% fitBounds(
        lng1 = batas_wilayah$xmin,
        lat1 = batas_wilayah$ymin,
        lng2 = batas_wilayah$xmax,
        lat2 = batas_wilayah$ymax
      )
    }
  })
  
  output$histogram    <- renderPlotly({ ggplotly(plot_hist_reactive()) })
  output$boxplot      <- renderPlotly({ ggplotly(plot_box_reactive()) })
  output$density_plot <- renderPlotly({ ggplotly(plot_density_reactive()) })
  output$scatter_plot <- renderPlotly({ ggplotly(plot_scatter_reactive()) })
  output$bar_plot     <- renderPlotly({ ggplotly(plot_bar_reactive()) })
  output$qq_plot      <- renderPlot({ plot_qq_reactive() })
  output$assumption_plots <- renderPlot({
    req(values$regression_model); par(mfrow = c(2, 2)); plot(values$regression_model)
  })
  
  output$download_plot_hist <- downloadHandler(
    filename = function() { paste0("histogram_", input$var_eksplorasi, ".png") },
    content = function(file) { ggsave(file, plot = plot_hist_reactive(), device = "png") }
  )
  output$download_plot_box <- downloadHandler(
    filename = function() { paste0("boxplot_", input$var_eksplorasi, ".png") },
    content = function(file) { ggsave(file, plot = plot_box_reactive(), device = "png") }
  )
  output$download_plot_density <- downloadHandler(
    filename = function() { paste0("density_", input$var_eksplorasi, ".png") },
    content = function(file) { ggsave(file, plot = plot_density_reactive(), device = "png") }
  )
  output$download_plot_scatter <- downloadHandler(
    filename = function() { paste0("scatter_", input$var_eksplorasi, ".png") },
    content = function(file) { ggsave(file, plot = plot_scatter_reactive(), device = "png") }
  )
  output$download_plot_bar <- downloadHandler(
    filename = function() { paste0("barplot_", input$var_eksplorasi, ".png") },
    content = function(file) { ggsave(file, plot = plot_bar_reactive(), device = "png") }
  )
  output$download_plot_qq <- downloadHandler(
    filename = function() { paste0("qqplot_", input$var_normalitas, ".png") },
    content = function(file) { ggsave(file, plot = plot_qq_reactive(), device = "png") }
  )
  output$download_plot_anova <- downloadHandler(
    filename = function() { paste0("anova_plot_", input$var_anova_dep, ".png") },
    content = function(file) { ggsave(file, plot = plot_anova_reactive(), device = "png") }
  )
  output$download_plot_reg_asumsi <- downloadHandler(
    filename = "plot_asumsi_regresi.png",
    content = function(file) {
      png(file, width = 800, height = 800, res = 100)
      par(mfrow = c(2, 2)); plot(values$regression_model); dev.off()
    }
  )
  
  # REGRESI LINEAR BERGANDA - FIXED
  observeEvent(input$run_regression, {
    req(input$var_dependent, input$var_independent)
    
    if(input$var_dependent == "" || length(input$var_independent) == 0) {
      showNotification("Pilih variabel dependen dan minimal satu variabel independen!", type = "error")
      return()
    }
    
    output$regression_summary <- renderPrint({
      
      dep_var <- values$processed_data[[input$var_dependent]]
      indep_vars <- values$processed_data[input$var_independent]
      
      reg_data <- data.frame(
        dependent = dep_var,
        indep_vars
      )
   
      reg_data <- reg_data[complete.cases(reg_data), ]
      
      if(nrow(reg_data) > length(input$var_independent) + 1) {
      
        formula_str <- paste("dependent ~", paste(input$var_independent, collapse = " + "))
        
     
        reg_model <- lm(as.formula(formula_str), data = reg_data)
        values$regression_model <- reg_model
        
        cat("REGRESI LINEAR BERGANDA\n")
        cat("=======================\n")
        cat("Variabel Dependen:", input$var_dependent, "\n")
        cat("Variabel Independen:", paste(input$var_independent, collapse = ", "), "\n")
        cat("Jumlah observasi:", nrow(reg_data), "\n\n")
        
       
        print(summary(reg_model))
        
      } else {
        cat("Error: Tidak cukup data untuk analisis regresi!")
      }
    })
    
    output$regression_interpretation <- renderUI({
      req(values$regression_model)
      model <- values$regression_model
      summary_model <- summary(model)
      
      # 1. Uji F (Signifikansi Model)
      f_stat <- summary_model$fstatistic
      f_p_value <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)
      model_sig_text <- if(f_p_value < 0.05) {
        tagList("Model ini ", tags$b("signifikan secara statistik"), " (p-value < 0.05), yang berarti setidaknya satu variabel independen memiliki hubungan dengan variabel dependen.")
      } else {
        tagList("Model ini ", tags$b("tidak signifikan secara statistik"), " (p-value >= 0.05), yang berarti secara keseluruhan model ini tidak dapat menjelaskan variasi variabel dependen dengan baik.")
      }
      
      # 2. R-squared (Kebaikan Model)
      r_sq_text <- tagList("Nilai ", tags$b("Adjusted R-squared"), " adalah ", tags$b(round(summary_model$adj.r.squared, 4)), ". Ini berarti sekitar ", tags$b(paste0(round(summary_model$adj.r.squared * 100, 2), "%")), " variasi pada variabel dependen dapat dijelaskan oleh variabel-variabel independen dalam model.")
      
      # 3. Koefisien
      coeffs <- summary_model$coefficients
      coeff_list <- lapply(1:nrow(coeffs), function(i) {
        var_name <- rownames(coeffs)[i]
        estimate <- round(coeffs[i, 1], 4)
        p_val <- coeffs[i, 4]
        
        significance <- if(p_val < 0.05) tags$b("signifikan") else "tidak signifikan"
        
        if(var_name == "(Intercept)") {
          tags$li(tags$b("Intercept:"), " Nilai estimasi adalah ", tags$b(estimate), ". Ini adalah nilai prediksi variabel dependen ketika semua variabel independen bernilai nol.")
        } else {
          direction <- if(estimate > 0) "meningkatkan" else "menurunkan"
          tags$li(tags$b(var_name), ": Variabel ini ", significance, " (p-value = ", round(p_val, 4), "). Setiap kenaikan 1 unit pada ", var_name, " akan ", tags$b(direction), " nilai variabel dependen sebesar ", tags$b(abs(estimate)), " unit, dengan asumsi variabel lain konstan.")
        }
      })
      
      tagList(
        h4("1. Signifikansi Model (Uji-F)"),
        p(model_sig_text),
        h4("2. Kebaikan Model (Goodness of Fit)"),
        p(r_sq_text),
        h4("3. Interpretasi Koefisien"),
        tags$ul(coeff_list)
      )
    })
  })
  
 
  data_peta_reaktif <- reactive({
    req(input$var_map)
    
  
    peta_terfilter <- if (!is.null(input$pilih_provinsi) && input$pilih_provinsi != "Seluruh Indonesia") {
      peta_provinsi %>% filter(nmprov == input$pilih_provinsi)
    } else {
      peta_provinsi
    }
    
 
    left_join(
      peta_terfilter,
      values$processed_data,
      by = c("kodeprkab" = "DISTRICTCODE")
    )
  })
  

  output$peta_sebaran <- renderLeaflet({
    data_peta <- data_peta_reaktif()
    req(input$var_map, input$var_map %in% names(data_peta))
    if (!is.numeric(data_peta[[input$var_map]])) return(NULL)
    
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = data_peta[[input$var_map]],
      na.color = "#bdbdbd"
    )
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%s: %s",
      data_peta$nmkab,
      input$var_map,
      format(data_peta[[input$var_map]], big.mark = ",", decimal.mark = ".", nsmall = 2)
    ) %>% lapply(htmltools::HTML)
    
  
    leaflet(data = data_peta, options = leafletOptions(preferCanvas = TRUE)) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 118, lat = -2, zoom = 5) %>%
      addPolygons(
        fillColor = ~pal(get(input$var_map)),
        weight = 1.5,                     
        opacity = 1,
        color = "black",                  
        dashArray = "1",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions( 
          weight = 3,                     
          color = "#FFFFFF",              
          dashArray = "",
          fillOpacity = 0.9,             
          bringToFront = TRUE             
        ),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")
      ) %>%
      addLegend(
        pal = pal,
        values = ~get(input$var_map),
        opacity = 0.7,
        title = input$var_map,
        position = "bottomright"
      )
  })
  
  # UJI ASUMSI REGRESI 
  observeEvent(input$uji_asumsi_reg, {
    req(values$regression_model)
    
    model <- values$regression_model
    residuals <- residuals(model)
    
   
    shapiro_res <- if(length(residuals) <= 5000) shapiro.test(residuals) else NULL
    
    bp_res <- tryCatch({
      lmtest::bptest(model)
    }, error = function(e) { NULL })
    
 
    dw_res <- tryCatch({
      lmtest::dwtest(model)
    }, error = function(e) { NULL })
    
   
    vif_res <- if(length(coef(model)) > 2) {
      tryCatch({
        car::vif(model)
      }, error = function(e) { NULL })
    } else { NULL }
    
  
    reg_assumption_results(list(
      shapiro = shapiro_res,
      bptest = bp_res,
      dwtest = dw_res,
      vif = vif_res
    ))
    
    output$assumption_plots <- renderPlot({
      par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
      plot(model)
    })
    
    output$assumption_tests <- renderPrint({
      req(reg_assumption_results())
      res <- reg_assumption_results()
      
      cat("1. UJI NORMALITAS RESIDUAL (Shapiro-Wilk)\n")
      if(!is.null(res$shapiro)) print(res$shapiro) else cat("Jumlah observasi > 5000, uji Shapiro-Wilk dilewati.\n")
      
      cat("\n2. UJI HOMOSKEDASTISITAS (Breusch-Pagan)\n")
      if(!is.null(res$bptest)) print(res$bptest) else cat("Uji gagal dijalankan.\n")
      
      cat("\n3. UJI AUTOKORELASI (Durbin-Watson)\n")
      if(!is.null(res$dwtest)) print(res$dwtest) else cat("Uji gagal dijalankan.\n")
      
      cat("\n4. UJI MULTIKOLINEARITAS (VIF)\n")
      if(!is.null(res$vif)) print(res$vif) else cat("Tidak ada atau hanya ada satu variabel independen.\n")
    })
  })
  
  # Interpretasi dinamis untuk Uji Asumsi Regresi
  output$interpretation_assumption_tests <- renderUI({
    req(reg_assumption_results())
    res <- reg_assumption_results()
    alpha <- 0.05
    
    # 1. Interpretasi Normalitas
    interp_norm <- if(!is.null(res$shapiro)) {
      p_val <- res$shapiro$p.value
      kesimpulan <- if(p_val > alpha) tagList("berdistribusi ", tags$b("NORMAL"), ". Asumsi terpenuhi.") else tagList("berdistribusi ", tags$b("TIDAK NORMAL"), ". Asumsi terlanggar.")
      tags$li("Normalitas Residual:", " P-value dari uji Shapiro-Wilk adalah ", tags$b(round(p_val, 4)), ". Ini berarti residual ", kesimpulan)
    }
    
    # 2. Interpretasi Homoskedastisitas
    interp_homosked <- if(!is.null(res$bptest)) {
      p_val <- res$bptest$p.value
      kesimpulan <- if(p_val > alpha) tagList("bersifat ", tags$b("HOMOSKEDASTIS"), " (varians konstan). Asumsi terpenuhi.") else tagList("bersifat ", tags$b("HETEROSKEDASTIS"), " (varians tidak konstan). Asumsi terlanggar.")
      tags$li("Homoskedastisitas:", " P-value dari uji Breusch-Pagan adalah ", tags$b(round(p_val, 4)), ". Ini berarti residual ", kesimpulan)
    }
    
    # 3. Interpretasi Autokorelasi
    interp_auto <- if(!is.null(res$dwtest)) {
      p_val <- res$dwtest$p.value
      dw_stat <- res$dwtest$statistic
      kesimpulan <- if(p_val < alpha) tags$b("TERDAPAT AUTOKORELASI") else tags$b("TIDAK TERDAPAT AUTOKORELASI")
      tags$li("Independensi Residual:", " Statistik Durbin-Watson adalah ", tags$b(round(dw_stat, 3)), ". Hasil uji menunjukkan bahwa ", kesimpulan, " dalam residual (p-value = ", round(p_val, 4), ").")
    }
    
    # 4. Interpretasi Multikolinearitas
    interp_vif <- if(!is.null(res$vif)) {
      vif_values <- res$vif
      is_high_vif <- any(vif_values > 10)
      kesimpulan <- if(is_high_vif) {
        high_vif_vars <- names(vif_values[vif_values > 10])
        tagList(tags$b("TERDAPAT MASALAH MULTIKOLINEARITAS"), " serius karena ada nilai VIF > 10 pada variabel: ", tags$b(paste(high_vif_vars, collapse = ", ")), ".")
      } else {
        tagList(tags$b("TIDAK ADA MASALAH MULTIKOLINEARITAS"), " yang serius (semua nilai VIF < 10).")
      }
      tags$li("Multikolinearitas:", " ", kesimpulan)
    }
    
    tagList(
      tags$ul(
        interp_norm,
        interp_homosked,
        interp_auto,
        interp_vif
      )
    )
  })
  

  plot_anova_reactive <- reactive({
    req(input$var_anova_dep, input$var_anova_indep)
    ggplot(values$processed_data, aes_string(x = input$var_anova_indep, y = input$var_anova_dep, fill = input$var_anova_indep)) +
      geom_boxplot() +
      theme_minimal() +
      labs(title = paste("Perbandingan", input$var_anova_dep, "berdasarkan", input$var_anova_indep))
  })
  
  output$anova_plot <- renderPlot({
    plot_anova_reactive()
  })
  
  output$unduh_laporan_regresi <- downloadHandler(
    filename = function() {
      paste0("Laporan-Regresi-", input$var_dependent, "-", Sys.Date(), ".pdf")
    },
    content = function(file) {
      req(values$regression_model, message = "Jalankan model regresi terlebih dahulu.")
      
   
      showNotification("Menyiapkan laporan Regresi lengkap...", duration = 15, type = "message")
      
    
      temp_report <- file.path(tempdir(), "laporan_regresi.Rmd")
      file.copy("laporan_regresi.Rmd", temp_report, overwrite = TRUE)
      

      model <- values$regression_model
      asumsi_res <- reg_assumption_results()
    
      interp_reg_text <- NULL
      if (!is.null(model)) {
        summary_model <- summary(model)
        
        # a. Interpretasi Uji F (Signifikansi Model)
        f_stat <- summary_model$fstatistic
        f_p_value <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)
        model_sig_text <- if(f_p_value < 0.05) {
          "Model ini signifikan secara statistik (p-value Uji F < 0.05), yang berarti secara bersama-sama, variabel independen mampu menjelaskan variasi variabel dependen."
        } else {
          "Model ini tidak signifikan secara statistik (p-value Uji F >= 0.05). Secara keseluruhan model ini tidak dapat menjelaskan variasi variabel dependen dengan baik."
        }
        
        # b. Interpretasi R-squared
        r_sq_text <- paste0(
          "Nilai Adjusted R-squared adalah ", round(summary_model$adj.r.squared, 4), 
          ". Ini berarti sekitar ", round(summary_model$adj.r.squared * 100, 2), 
          "% variasi pada variabel dependen dapat dijelaskan oleh variabel-variabel independen dalam model."
        )
        
        # c. Interpretasi Koefisien (Uji t)
        coeffs <- summary_model$coefficients
        coeff_text_lines <- sapply(1:nrow(coeffs), function(i) {
          var_name <- rownames(coeffs)[i]
          estimate <- round(coeffs[i, 1], 4)
          p_val <- coeffs[i, 4]
          significance <- if(p_val < 0.05) "signifikan" else "tidak signifikan"
          
          if(var_name == "(Intercept)") {
            paste0("- Intercept: Nilai estimasi adalah ", estimate, ". Ini adalah nilai prediksi '", isolate(input$var_dependent), "' ketika semua variabel independen bernilai nol.")
          } else {
            direction <- if(estimate > 0) "meningkatkan" else "menurunkan"
            paste0("- ", var_name, ": Variabel ini berpengaruh ", significance, " (p-value = ", round(p_val, 4), "). Setiap kenaikan 1 unit pada ", var_name, " akan ", direction, " nilai '", isolate(input$var_dependent), "' sebesar ", abs(estimate), " unit, dengan asumsi variabel lain konstan.")
          }
        })
        full_coeff_text <- paste(coeff_text_lines, collapse = "\n")
        
        interp_reg_text <- paste(
          "1. Signifikansi Model (Uji-F):\n", model_sig_text, "\n\n",
          "2. Kebaikan Model (Goodness of Fit):\n", r_sq_text, "\n\n",
          "3. Interpretasi Koefisien (Uji-t):\n", full_coeff_text
        )
      }
   
      interp_asumsi_text <- "Uji asumsi belum dijalankan." 
      if(!is.null(asumsi_res)) {
        alpha <- 0.05
        
        # a. Interpretasi Normalitas
        interp_norm <- if(!is.null(asumsi_res$shapiro)) {
          p_val <- asumsi_res$shapiro$p.value
          kesimpulan <- if(p_val > alpha) "NORMAL. Asumsi terpenuhi." else "TIDAK NORMAL. Asumsi terlanggar."
          paste0("- Normalitas Residual (Shapiro-Wilk): P-value = ", round(p_val, 4), ". Residual berdistribusi ", kesimpulan)
        } else {"- Uji Normalitas tidak dijalankan (observasi > 5000)."}
        
        # b. Interpretasi Homoskedastisitas
        interp_homosked <- if(!is.null(asumsi_res$bptest)) {
          p_val <- asumsi_res$bptest$p.value
          kesimpulan <- if(p_val > alpha) "HOMOSKEDASTIS (varians konstan). Asumsi terpenuhi." else "HETEROSKEDASTIS (varians tidak konstan). Asumsi terlanggar."
          paste0("- Homoskedastisitas (Breusch-Pagan): P-value = ", round(p_val, 4), ". Varians residual bersifat ", kesimpulan)
        } else {"- Uji Homoskedastisitas gagal dijalankan."}
        
        # c. Interpretasi Autokorelasi
        interp_auto <- if(!is.null(asumsi_res$dwtest)) {
          p_val <- asumsi_res$dwtest$p.value
          dw_stat <- asumsi_res$dwtest$statistic
          kesimpulan <- if(p_val < alpha) "TERDAPAT AUTOKORELASI." else "TIDAK ADA AUTOKORELASI."
          paste0("- Independensi Residual (Durbin-Watson): Statistik DW = ", round(dw_stat, 3), ". ", kesimpulan, " (p-value = ", round(p_val, 4), ").")
        } else {"- Uji Autokorelasi gagal dijalankan."}
        
        # d. Interpretasi Multikolinearitas
        interp_vif <- if(!is.null(asumsi_res$vif)) {
          vif_values <- asumsi_res$vif
          is_high_vif <- any(vif_values > 10)
          kesimpulan <- if(is_high_vif) "TERDAPAT masalah multikolinearitas serius (ada VIF > 10)." else "TIDAK ADA masalah multikolinearitas serius (semua VIF < 10)."
          paste0("- Multikolinearitas (VIF): ", kesimpulan)
        } else {"- Uji Multikolinearitas tidak dijalankan (hanya ada satu variabel independen)."}
        
        # Gabungkan semua interpretasi asumsi
        interp_asumsi_text <- paste(interp_norm, interp_homosked, interp_auto, interp_vif, sep = "\n")
      }
 
      params_list <- list(
        dep_var = isolate(input$var_dependent),
        indep_vars = isolate(input$var_independent),
        hasil_regresi = if(!is.null(model)) paste(capture.output(summary(model)), collapse="\n") else NULL,
        interpretasi_regresi = interp_reg_text,
        hasil_asumsi_reg = if(!is.null(asumsi_res)) paste(capture.output(asumsi_res), collapse="\n") else NULL,
        interpretasi_asumsi_reg = interp_asumsi_text,
        plot_asumsi_reg = model 
      )
      
   
      rmarkdown::render(
        temp_report, 
        output_file = file, 
        params = params_list, 
        envir = new.env(parent = globalenv())
      )
    }
  )
  
  output$unduh_laporan_anova <- downloadHandler(
    filename = function() { paste0("Laporan-ANOVA-", Sys.Date(), ".pdf") },
    content = function(file) {
      showNotification("Menyiapkan laporan ANOVA...", type = "message")
      temp_report <- file.path(tempdir(), "laporan_anova.Rmd")
      file.copy("laporan_anova.Rmd", temp_report, overwrite = TRUE)
  
      interp_anova_one_text <- NULL
      if (!is.null(anova_one_result_val())) {
        summary_res <- anova_one_result_val()
        p_val <- summary_res[[1]][["Pr(>F)"]][1]
        kesimpulan <- if (p_val < 0.05) "terdapat perbedaan rata-rata yang signifikan" else "tidak ada perbedaan rata-rata yang signifikan"
        interp_anova_one_text <- paste0(
          "Hasil ANOVA menunjukkan bahwa ", kesimpulan, 
          " pada variabel ", isolate(input$var_anova_dep), 
          " di antara kelompok-kelompok ", isolate(input$var_anova_indep), 
          " (p-value = ", round(p_val, 4), ")."
        )
      }
      
 
      interp_anova_two_text <- NULL
      if (!is.null(anova_two_result_val())) {
        summary_res <- anova_two_result_val()[[1]]
        p_vals <- summary_res[["Pr(>F)"]]
        
        interp_f1 <- if(p_vals[1] < 0.05) "berpengaruh signifikan" else "tidak berpengaruh signifikan"
        interp_f2 <- if(p_vals[2] < 0.05) "berpengaruh signifikan" else "tidak berpengaruh signifikan"
        
        text_f1 <- paste0("Faktor ", isolate(input$var_anova2_factor1), " ", interp_f1, " (p-value = ", round(p_vals[1], 4), ").")
        text_f2 <- paste0("Faktor ", isolate(input$var_anova2_factor2), " ", interp_f2, " (p-value = ", round(p_vals[2], 4), ").")
        
        text_int <- ""
        if(isolate(input$interaction) && !is.na(p_vals[3])) {
          interp_int <- if(p_vals[3] < 0.05) "terdapat efek interaksi yang signifikan" else "tidak terdapat efek interaksi yang signifikan"
          text_int <- paste0(interp_int, " antara kedua faktor (p-value = ", round(p_vals[3], 4), ").")
        }
        interp_anova_two_text <- paste(text_f1, text_f2, text_int, sep = "\n")
      }
      
      params_list <- list(
        hasil_anova_one = if(!is.null(anova_one_result_val())) paste(capture.output(anova_one_result_val()), collapse="\n") else NULL,
        interp_anova_one = interp_anova_one_text,
        plot_anova = if(!is.null(anova_one_result_val())) plot_anova_reactive() else NULL,
        hasil_anova_two = if(!is.null(anova_two_result_val())) paste(capture.output(anova_two_result_val()), collapse="\n") else NULL,
        interp_anova_two = interp_anova_two_text
      )
      
      rmarkdown::render(temp_report, output_file = file, params = params_list, envir = new.env(parent = globalenv()))
    }
  )
 
  output$unduh_laporan_uji_prop_var <- downloadHandler(
    filename = function() { paste0("Laporan-Uji-Prop-Var-", Sys.Date(), ".pdf") },
    content = function(file) {
      showNotification("Menyiapkan laporan Uji Proporsi & Varians...", type = "message")
      temp_report <- file.path(tempdir(), "laporan_uji_prop_var.Rmd")
      file.copy("laporan_uji_prop_var.Rmd", temp_report, overwrite = TRUE)
      
      prop_res <- prop_test_result_val()
      var_res <- var_test_result_val()
      
      interp_prop_text <- NULL
      if(!is.null(prop_res)){
        interp_prop_text <- as.character(renderUI({ prop_test_interp() })())
      }
      
      interp_var_text <- NULL
      if(!is.null(var_res)){
        interp_var_text <- as.character(renderUI({ var_test_interp() })())
      }
      
      params_list <- list(
        hasil_prop = if(!is.null(prop_res)) paste(capture.output(prop_res), collapse = "\n") else NULL,
        interp_prop = interp_prop_text,
        hasil_var = if(!is.null(var_res)) paste(capture.output(var_res), collapse = "\n") else NULL,
        interp_var = interp_var_text
      )
      
      rmarkdown::render(temp_report, output_file = file, params = params_list, envir = new.env(parent = globalenv()))
    }
  )
 
  output$unduh_laporan_uji_rata <- downloadHandler(
    filename = function() { paste0("Laporan-Uji-Rata-", Sys.Date(), ".pdf") },
    content = function(file) {
      showNotification("Menyiapkan laporan Uji Rata-rata...", type = "message")
      temp_report <- file.path(tempdir(), "laporan_uji_rata.Rmd")
      file.copy("laporan_uji_rata.Rmd", temp_report, overwrite = TRUE)
      
    
      one_sample_res <- one_sample_t_result()
      two_sample_res <- two_sample_t_result()
      
   
      interp_one_sample_text <- NULL
      if(!is.null(one_sample_res)){
        p_val <- one_sample_res$p.value
        kesimpulan <- if(p_val < 0.05) "berbeda secara signifikan" else "tidak berbeda secara signifikan"
        interp_one_sample_text <- paste0("Dengan p-value ", round(p_val, 4), ", rata-rata populasi disimpulkan ", kesimpulan, " dari ", one_sample_res$null.value, ".")
      }
      
      interp_two_sample_text <- NULL
      if(!is.null(two_sample_res)){
        p_val <- two_sample_res$p.value
        kesimpulan <- if(p_val < 0.05) "terdapat perbedaan rata-rata yang signifikan" else "tidak terdapat perbedaan rata-rata yang signifikan"
        interp_two_sample_text <- paste0("Dengan p-value ", round(p_val, 4), ", disimpulkan ", kesimpulan, " antara kedua kelompok.")
      }
      
      params_list <- list(
        hasil_one_sample = if(!is.null(one_sample_res)) paste(capture.output(one_sample_res), collapse="\n") else NULL,
        interp_one_sample = interp_one_sample_text,
        hasil_two_sample = if(!is.null(two_sample_res)) paste(capture.output(two_sample_res), collapse="\n") else NULL,
        interp_two_sample = interp_two_sample_text
      )
      
      rmarkdown::render(temp_report, output_file = file, params = params_list, envir = new.env(parent = globalenv()))
    }
  )
 
  output$unduh_laporan_asumsi <- downloadHandler(
    filename = function() { paste0("Laporan-Asumsi-", Sys.Date(), ".pdf") },
    content = function(file) {
      showNotification("Menyiapkan laporan Uji Asumsi...", type = "message")
      
      temp_report <- file.path(tempdir(), "laporan_asumsi.Rmd")
      file.copy("laporan_asumsi.Rmd", temp_report, overwrite = TRUE)
      
  
      norm_res <- norm_test_results()
      homog_res <- homog_test_results()
    
      interp_norm_text <- NULL
      if(!is.null(norm_res)) {
        p_val_shapiro <- norm_res$shapiro$p.value
        kesimpulan <- if (p_val_shapiro > 0.05) "data terdistribusi NORMAL." else "data terdistribusi TIDAK NORMAL."
        interp_norm_text <- paste("Berdasarkan uji Shapiro-Wilk (p-value =", round(p_val_shapiro, 4), "), disimpulkan", kesimpulan)
      }
      
      interp_homog_text <- NULL
      if(!is.null(homog_res)) {
        p_val_bartlett <- homog_res$bartlett$p.value
        kesimpulan <- if (p_val_bartlett > 0.05) "varians antar kelompok bersifat HOMOGEN." else "varians antar kelompok bersifat HETEROGEN."
        interp_homog_text <- paste("Berdasarkan uji Bartlett (p-value =", round(p_val_bartlett, 4), "), disimpulkan", kesimpulan)
      }
      
      params_list <- list(
        var_normalitas = input$var_normalitas,
        hasil_normalitas = if(!is.null(norm_res)) paste(capture.output(norm_res), collapse = "\n") else NULL,
        interpretasi_normalitas = interp_norm_text,
        plot_qq = if(!is.null(norm_res)) plot_qq_reactive() else NULL,
        
        var_homogenitas = input$var_homogenitas,
        grup_homogenitas = input$group_var,
        hasil_homogenitas = if(!is.null(homog_res)) paste(capture.output(homog_res), collapse = "\n") else NULL,
        interpretasi_homogenitas = interp_homog_text
      )
      
      rmarkdown::render(temp_report, output_file = file, params = params_list, envir = new.env(parent = globalenv()))
    }
  )

  output$unduh_laporan_eksplorasi <- downloadHandler(
    filename = function() {
      paste0("Laporan-Eksplorasi-", input$var_eksplorasi, "-", Sys.Date(), ".pdf")
    },
    content = function(file) {
      
 
      req(input$var_eksplorasi, message = "Silakan pilih variabel terlebih dahulu.")
      
      showNotification("Menyiapkan laporan, mohon tunggu...", duration = 10, type = "message")
      
      temp_report <- file.path(tempdir(), "laporan_eksplorasi.Rmd")
      file.copy("laporan_eksplorasi.Rmd", temp_report, overwrite = TRUE)
  
      var_data_report <- values$processed_data[[input$var_eksplorasi]]
      
  
      tabel_stats <- NULL
      if(is.numeric(var_data_report)) {
        desc <- summary(var_data_report)
        tabel_stats <- as.data.frame(t(as.matrix(desc)))
      } else {
        tabel_stats <- as.data.frame(table(var_data_report))
        colnames(tabel_stats) <- c("Kategori", "Frekuensi")
      }
   
      interp_hist_text <- NULL
      interp_box_text <- NULL
      
      if (is.numeric(var_data_report)) {
 
        clean_data <- na.omit(var_data_report)
        mean_val <- mean(clean_data)
        median_val <- median(clean_data)
        sd_val <- sd(clean_data)
        skewness <- if(sd_val > 0) { 3 * (mean_val - median_val) / sd_val } else { 0 }
        
        if (abs(skewness) < 0.5) { 
          interp_hist_text <- "Distribusi terlihat hampir simetris."
        } else if (skewness > 0.5) { 
          interp_hist_text <- "Distribusi menjulur ke kanan (positive skew)."
        } else { 
          interp_hist_text <- "Distribusi menjulur ke kiri (negative skew)."
        }
        
  
        q1 <- quantile(clean_data, 0.25)
        q3 <- quantile(clean_data, 0.75)
        interp_box_text <- paste0("Median data berada di ", round(median_val, 2), 
                                  ". 50% data berada di antara ", round(q1, 2), " dan ", round(q3, 2), ".")
      }
     
      params_list <- list(
        var_pilihan = input$var_eksplorasi,
        data_tabel = tabel_stats,
        
        plot_hist = if ('histogram' %in% input$plot_types && is.numeric(var_data_report)) plot_hist_reactive() else NULL,
        interpretasi_hist = interp_hist_text,
        
        plot_box = if ('boxplot' %in% input$plot_types && is.numeric(var_data_report)) plot_box_reactive() else NULL,
        interpretasi_box = interp_box_text,
        
        plot_density = if ('density' %in% input$plot_types && is.numeric(var_data_report)) plot_density_reactive() else NULL,
        interpretasi_density = "Plot kepadatan menunjukkan estimasi distribusi probabilitas dari variabel.",
        
        plot_scatter = if ('scatter' %in% input$plot_types && is.numeric(var_data_report)) plot_scatter_reactive() else NULL,
        interpretasi_scatter = "Scatter plot memvisualisasikan setiap nilai data terhadap indeksnya.",
        
        plot_bar = if ('bar' %in% input$plot_types && !is.numeric(var_data_report)) plot_bar_reactive() else NULL,
        interpretasi_bar = "Bar plot menunjukkan frekuensi untuk setiap kategori."
      )
      
      rmarkdown::render(temp_report, output_file = file, params = params_list, envir = new.env(parent = globalenv()))
    }
  )
  
}

# Run the application
shinyApp(ui = ui, server = server)



