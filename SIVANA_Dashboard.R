# SIVANA - Social Index Vulnerability Analysis Navigator Application
# Comprehensive R Shiny Dashboard for SOVI Data Analysis
# Created for Social Vulnerability Index Analysis

# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(plotly)
library(leaflet)
library(sf)
library(dplyr)
library(corrplot)
library(psych)
library(car)
library(nortest)
library(flextable)
library(officer)
library(downloadthis)
library(shinyWidgets)
library(shinycssloaders)
library(htmlwidgets)
library(RColorBrewer)
library(viridis)
library(reshape2)
library(broom)

# Data loading and preparation
load_data <- function() {
  # Load CSV data
  sovi_data <- read.csv("C:/Users/HARRY PENTALEON/Downloads/sovi_data.csv", stringsAsFactors = FALSE)
  
  # Load shapefile data if available
  shp_data <- NULL
  tryCatch({
    if (file.exists("C:/Users/HARRY PENTALEON/Downloads/sovi_administrasi_kabupaten.shp")) {
      shp_data <- st_read("C:/Users/HARRY PENTALEON/Downloads/sovi_administrasi_kabupaten.shp", quiet = TRUE)
    }
  }, error = function(e) {
    print("Shapefile not found or error loading shapefile")
  })
  
  return(list(csv = sovi_data, shp = shp_data))
}

# Initialize data
data_list <- load_data()
sovi_data <- data_list$csv
shp_data <- data_list$shp

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "SIVANA - Social Vulnerability Analysis Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("🏠 Beranda", tabName = "beranda", icon = icon("home")),
      menuItem("🔧 Manajemen Data", tabName = "manajemen", icon = icon("database")),
      menuItem("📊 Eksplorasi Data", tabName = "eksplorasi", icon = icon("chart-line")),
      menuItem("✅ Uji Asumsi", tabName = "asumsi", icon = icon("check-circle")),
      menuItem("📈 Statistik Inferensia I", tabName = "inferensia1", icon = icon("calculator")),
      menuItem("📉 Statistik Inferensia II", tabName = "inferensia2", icon = icon("chart-bar")),
      menuItem("🎯 Regresi Linear", tabName = "regresi", icon = icon("line-chart"))
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
      "))
    ),
    
    tabItems(
      # BERANDA TAB
      tabItem(tabName = "beranda",
        fluidRow(
          box(
            title = "Selamat Datang di SIVANA Dashboard", status = "primary", solidHeader = TRUE, width = 12,
            h3("Social Index Vulnerability Analysis Navigator Application"),
            br(),
            h4("📖 Tentang Dashboard"),
            p("SIVANA adalah dashboard komprehensif untuk analisis Social Vulnerability Index (SOVI) yang dirancang khusus untuk menganalisis data kerentanan sosial di Indonesia. Dashboard ini menyediakan berbagai fitur analisis statistik dan visualisasi data yang mendalam."),
            
            h4("🎯 Fitur Utama:"),
            tags$ul(
              tags$li("Manajemen data dengan kategorisasi variabel kontinyu"),
              tags$li("Eksplorasi data dengan statistik deskriptif dan visualisasi"),
              tags$li("Pemetaan geografis menggunakan data shapefile"),
              tags$li("Uji asumsi normalitas dan homogenitas"),
              tags$li("Analisis statistik inferensia (uji proporsi, varians, dan rata-rata)"),
              tags$li("Analisis ANOVA satu arah dan dua arah"),
              tags$li("Regresi linear berganda dengan uji asumsi"),
              tags$li("Download hasil analisis dalam berbagai format")
            ),
            
            h4("📊 Metadata Dataset:"),
            p("Dataset ini berisi indikator kerentanan sosial untuk berbagai kabupaten di Indonesia, meliputi:"),
            tags$ul(
              tags$li("CHILDREN: Persentase anak-anak dalam populasi"),
              tags$li("FEMALE: Persentase perempuan dalam populasi"),
              tags$li("ELDERLY: Persentase lansia dalam populasi"),
              tags$li("FHEAD: Persentase kepala keluarga perempuan"),
              tags$li("FAMILYSIZE: Rata-rata ukuran keluarga"),
              tags$li("NOELECTRIC: Persentase rumah tanpa listrik"),
              tags$li("LOWEDU: Persentase penduduk berpendidikan rendah"),
              tags$li("GROWTH: Tingkat pertumbuhan populasi"),
              tags$li("POVERTY: Persentase kemiskinan"),
              tags$li("ILLITERATE: Persentase buta huruf"),
              tags$li("NOTRAINING: Persentase tanpa pelatihan"),
              tags$li("DPRONE: Kerentanan terhadap bencana"),
              tags$li("RENTED: Persentase rumah sewa"),
              tags$li("NOSEWER: Persentase tanpa sistem pembuangan"),
              tags$li("TAPWATER: Persentase akses air bersih"),
              tags$li("POPULATION: Jumlah populasi")
            ),
            
            h4("🔗 Referensi:"),
            p("Dataset ini mengacu pada penelitian Social Vulnerability Index yang dipublikasikan di:"),
            a("https://www.sciencedirect.com/science/article/pii/S2352340921010180", 
              href = "https://www.sciencedirect.com/science/article/pii/S2352340921010180", 
              target = "_blank"),
            
            br(), br(),
            h4("👥 Tim Pengembang:"),
            p("Dashboard ini dikembangkan sebagai alat analisis komprehensif untuk penelitian kerentanan sosial di Indonesia."),
            
            br(),
            div(style = "text-align: center;",
                h5("Mulai eksplorasi data dengan memilih menu di sidebar kiri!")
            )
          )
        )
      ),
      
      # MANAJEMEN DATA TAB
      tabItem(tabName = "manajemen",
        fluidRow(
          box(
            title = "Manajemen dan Transformasi Data", status = "primary", solidHeader = TRUE, width = 12,
            tabsetPanel(
              tabPanel("Data Asli",
                h4("Dataset SOVI Original"),
                withSpinner(DT::dataTableOutput("original_data")),
                br(),
                h5("Ringkasan Dataset:"),
                verbatimTextOutput("data_summary")
              ),
              
              tabPanel("Kategorisasi Data",
                h4("Transformasi Data Kontinyu ke Kategorik"),
                fluidRow(
                  column(4,
                    selectInput("var_to_categorize", "Pilih Variabel untuk Dikategorikan:",
                               choices = NULL),
                    numericInput("n_categories", "Jumlah Kategori:", value = 3, min = 2, max = 5),
                    radioButtons("categorize_method", "Metode Kategorisasi:",
                               choices = list("Quantile" = "quantile", 
                                            "Equal Width" = "width",
                                            "K-means" = "kmeans")),
                    actionButton("categorize_btn", "Kategorikan", class = "btn-primary"),
                    br(), br(),
                    h5("Variabel Kategorik yang Telah Dibuat:"),
                    verbatimTextOutput("categorized_vars_list"),
                    conditionalPanel(
                      condition = "output.has_categorized_vars",
                      br(),
                      selectInput("var_to_remove", "Pilih Variabel untuk Dihapus:", choices = NULL),
                      actionButton("remove_var_btn", "Hapus Variabel", class = "btn-danger btn-sm")
                    )
                  ),
                  column(8,
                    h5("Preview Kategorisasi:"),
                    withSpinner(plotOutput("categorization_plot")),
                    br(),
                    withSpinner(DT::dataTableOutput("categorized_preview"))
                  )
                )
              ),
              
              tabPanel("Data Hasil Transformasi",
                h4("Dataset dengan Variabel Kategorik Baru"),
                withSpinner(DT::dataTableOutput("transformed_data")),
                br(),
                downloadButton("download_transformed", "Download Data Transformasi", class = "btn-success")
              )
            )
          )
        )
      ),
      
      # EKSPLORASI DATA TAB
      tabItem(tabName = "eksplorasi",
        fluidRow(
          box(
            title = "Eksplorasi dan Visualisasi Data", status = "primary", solidHeader = TRUE, width = 12,
            tabsetPanel(
              tabPanel("Statistik Deskriptif",
                fluidRow(
                  column(6,
                    h4("Statistik Deskriptif Variabel Numerik"),
                    withSpinner(DT::dataTableOutput("descriptive_stats"))
                  ),
                  column(6,
                    h4("Distribusi Variabel"),
                    selectInput("desc_var", "Pilih Variabel:", choices = NULL),
                    withSpinner(plotlyOutput("distribution_plot"))
                  )
                ),
                br(),
                h4("Interpretasi Statistik Deskriptif:"),
                verbatimTextOutput("descriptive_interpretation")
              ),
              
              tabPanel("Visualisasi Data",
                fluidRow(
                  column(4,
                    selectInput("plot_type", "Jenis Plot:",
                               choices = list("Histogram" = "hist",
                                            "Box Plot" = "box",
                                            "Scatter Plot" = "scatter",
                                            "Correlation Matrix" = "corr")),
                    conditionalPanel(
                      condition = "input.plot_type == 'scatter'",
                      selectInput("x_var", "Variabel X:", choices = NULL),
                      selectInput("y_var", "Variabel Y:", choices = NULL)
                    ),
                    conditionalPanel(
                      condition = "input.plot_type != 'corr'",
                      selectInput("viz_var", "Variabel:", choices = NULL)
                    )
                  ),
                  column(8,
                    withSpinner(plotlyOutput("visualization_plot", height = "500px"))
                  )
                ),
                br(),
                h4("Interpretasi Visualisasi:"),
                verbatimTextOutput("visualization_interpretation")
              ),
              
              tabPanel("Peta Geografis",
                conditionalPanel(
                  condition = "output.shp_available",
                  fluidRow(
                    column(3,
                      selectInput("map_var", "Variabel untuk Peta:", choices = NULL),
                      selectInput("color_palette", "Palet Warna:",
                                 choices = list("Viridis" = "viridis",
                                              "Blues" = "Blues",
                                              "Reds" = "Reds",
                                              "YlOrRd" = "YlOrRd"))
                    ),
                    column(9,
                      withSpinner(leafletOutput("geographic_map", height = "600px"))
                    )
                  ),
                  br(),
                  h4("Interpretasi Peta:"),
                  verbatimTextOutput("map_interpretation")
                ),
                conditionalPanel(
                  condition = "!output.shp_available",
                  div(class = "alert alert-warning",
                      h4("Data Shapefile Tidak Tersedia"),
                      p("File shapefile tidak ditemukan atau tidak dapat dimuat. Fitur peta geografis tidak tersedia."))
                )
              )
            )
          )
        )
      ),
      
      # UJI ASUMSI TAB
      tabItem(tabName = "asumsi",
        fluidRow(
          box(
            title = "Uji Asumsi Data", status = "primary", solidHeader = TRUE, width = 12,
            tabsetPanel(
              tabPanel("Uji Normalitas",
                fluidRow(
                  column(4,
                    selectInput("norm_var", "Pilih Variabel:", choices = NULL),
                    radioButtons("norm_test", "Jenis Uji:",
                               choices = list("Shapiro-Wilk" = "shapiro",
                                            "Anderson-Darling" = "ad",
                                            "Kolmogorov-Smirnov" = "ks")),
                    actionButton("run_normality", "Jalankan Uji", class = "btn-primary")
                  ),
                  column(8,
                    h5("Hasil Uji Normalitas:"),
                    verbatimTextOutput("normality_result"),
                    br(),
                    withSpinner(plotOutput("normality_plot"))
                  )
                ),
                br(),
                h4("Interpretasi Uji Normalitas:"),
                verbatimTextOutput("normality_interpretation")
              ),
              
              tabPanel("Uji Homogenitas",
                fluidRow(
                  column(4,
                    selectInput("homo_var", "Variabel Numerik:", choices = NULL),
                    selectInput("group_var", "Variabel Grouping:", choices = NULL),
                    radioButtons("homo_test", "Jenis Uji:",
                               choices = list("Levene Test" = "levene",
                                            "Bartlett Test" = "bartlett")),
                    actionButton("run_homogeneity", "Jalankan Uji", class = "btn-primary")
                  ),
                  column(8,
                    h5("Hasil Uji Homogenitas:"),
                    verbatimTextOutput("homogeneity_result"),
                    br(),
                    withSpinner(plotOutput("homogeneity_plot"))
                  )
                ),
                br(),
                h4("Interpretasi Uji Homogenitas:"),
                verbatimTextOutput("homogeneity_interpretation")
              )
            )
          )
        )
      ),
      
      # STATISTIK INFERENSIA I TAB
      tabItem(tabName = "inferensia1",
        fluidRow(
          box(
            title = "Statistik Inferensia - Uji Proporsi dan Varians", status = "primary", solidHeader = TRUE, width = 12,
            tabsetPanel(
              tabPanel("Uji Proporsi",
                fluidRow(
                  column(4,
                    h5("Uji Proporsi Satu Sampel"),
                    selectInput("prop_var", "Variabel:", choices = NULL),
                    numericInput("prop_value", "Nilai untuk Proporsi:", value = 0.5, min = 0, max = 1, step = 0.1),
                    numericInput("prop_test_value", "Proporsi yang Diuji:", value = 0.5, min = 0, max = 1, step = 0.01),
                    actionButton("run_prop_test", "Jalankan Uji Proporsi", class = "btn-primary")
                  ),
                  column(8,
                    h5("Hasil Uji Proporsi:"),
                    verbatimTextOutput("proportion_result"),
                    br(),
                    withSpinner(plotOutput("proportion_plot"))
                  )
                ),
                br(),
                h4("Interpretasi Uji Proporsi:"),
                verbatimTextOutput("proportion_interpretation")
              ),
              
              tabPanel("Uji Varians",
                fluidRow(
                  column(4,
                    radioButtons("var_test_type", "Jenis Uji Varians:",
                               choices = list("Satu Kelompok" = "one",
                                            "Dua Kelompok" = "two")),
                    selectInput("var_test_var", "Variabel:", choices = NULL),
                    conditionalPanel(
                      condition = "input.var_test_type == 'two'",
                      selectInput("var_group_var", "Variabel Grouping:", choices = NULL)
                    ),
                    conditionalPanel(
                      condition = "input.var_test_type == 'one'",
                      numericInput("test_variance", "Varians yang Diuji:", value = 1, min = 0)
                    ),
                    actionButton("run_var_test", "Jalankan Uji Varians", class = "btn-primary")
                  ),
                  column(8,
                    h5("Hasil Uji Varians:"),
                    verbatimTextOutput("variance_result"),
                    br(),
                    withSpinner(plotOutput("variance_plot"))
                  )
                ),
                br(),
                h4("Interpretasi Uji Varians:"),
                verbatimTextOutput("variance_interpretation")
              )
            )
          )
        )
      ),
      
      # STATISTIK INFERENSIA II TAB
      tabItem(tabName = "inferensia2",
        fluidRow(
          box(
            title = "Statistik Inferensia - ANOVA", status = "primary", solidHeader = TRUE, width = 12,
            tabsetPanel(
              tabPanel("ANOVA Satu Arah",
                fluidRow(
                  column(4,
                    selectInput("anova1_dep", "Variabel Dependen:", choices = NULL),
                    selectInput("anova1_indep", "Variabel Independen:", choices = NULL),
                    actionButton("run_anova1", "Jalankan ANOVA Satu Arah", class = "btn-primary"),
                    br(), br(),
                    conditionalPanel(
                      condition = "output.anova1_significant",
                      actionButton("run_posthoc", "Uji Post-hoc (Tukey)", class = "btn-warning")
                    )
                  ),
                  column(8,
                    h5("Hasil ANOVA Satu Arah:"),
                    verbatimTextOutput("anova1_result"),
                    br(),
                    withSpinner(plotOutput("anova1_plot")),
                    br(),
                    conditionalPanel(
                      condition = "output.anova1_significant",
                      h5("Hasil Uji Post-hoc:"),
                      verbatimTextOutput("posthoc_result")
                    )
                  )
                ),
                br(),
                h4("Interpretasi ANOVA Satu Arah:"),
                verbatimTextOutput("anova1_interpretation")
              ),
              
              tabPanel("ANOVA Dua Arah",
                fluidRow(
                  column(4,
                    selectInput("anova2_dep", "Variabel Dependen:", choices = NULL),
                    selectInput("anova2_indep1", "Faktor 1:", choices = NULL),
                    selectInput("anova2_indep2", "Faktor 2:", choices = NULL),
                    checkboxInput("include_interaction", "Sertakan Interaksi", value = TRUE),
                    actionButton("run_anova2", "Jalankan ANOVA Dua Arah", class = "btn-primary")
                  ),
                  column(8,
                    h5("Hasil ANOVA Dua Arah:"),
                    verbatimTextOutput("anova2_result"),
                    br(),
                    withSpinner(plotOutput("anova2_plot"))
                  )
                ),
                br(),
                h4("Interpretasi ANOVA Dua Arah:"),
                verbatimTextOutput("anova2_interpretation")
              )
            )
          )
        )
      ),
      
      # REGRESI LINEAR TAB
      tabItem(tabName = "regresi",
        fluidRow(
          box(
            title = "Analisis Regresi Linear Berganda", status = "primary", solidHeader = TRUE, width = 12,
            tabsetPanel(
              tabPanel("Model Regresi",
                fluidRow(
                  column(4,
                    selectInput("reg_dependent", "Variabel Dependen:", choices = NULL),
                    checkboxGroupInput("reg_independent", "Variabel Independen:", choices = NULL),
                    actionButton("run_regression", "Jalankan Regresi", class = "btn-primary"),
                    br(), br(),
                    conditionalPanel(
                      condition = "output.regression_run",
                      h5("Opsi Tambahan:"),
                      checkboxInput("show_residuals", "Tampilkan Plot Residual", value = TRUE),
                      checkboxInput("show_influence", "Tampilkan Analisis Influential Points", value = FALSE)
                    )
                  ),
                  column(8,
                    h5("Hasil Regresi Linear Berganda:"),
                    verbatimTextOutput("regression_summary"),
                    br(),
                    conditionalPanel(
                      condition = "input.show_residuals && output.regression_run",
                      withSpinner(plotOutput("regression_plots"))
                    )
                  )
                ),
                br(),
                h4("Interpretasi Model Regresi:"),
                verbatimTextOutput("regression_interpretation")
              ),
              
              tabPanel("Uji Asumsi Regresi",
                conditionalPanel(
                  condition = "output.regression_run",
                  fluidRow(
                    column(6,
                      h5("Uji Normalitas Residual:"),
                      verbatimTextOutput("reg_normality_test"),
                      br(),
                      h5("Uji Multikolinearitas (VIF):"),
                      verbatimTextOutput("vif_test")
                    ),
                    column(6,
                      h5("Uji Heteroskedastisitas:"),
                      verbatimTextOutput("hetero_test"),
                      br(),
                      h5("Uji Autokorelasi (Durbin-Watson):"),
                      verbatimTextOutput("autocorr_test")
                    )
                  ),
                  br(),
                  withSpinner(plotOutput("assumption_plots")),
                  br(),
                  h4("Interpretasi Uji Asumsi:"),
                  verbatimTextOutput("assumption_interpretation")
                ),
                conditionalPanel(
                  condition = "!output.regression_run",
                  div(class = "alert alert-info",
                      h4("Jalankan Model Regresi Terlebih Dahulu"),
                      p("Silakan jalankan analisis regresi pada tab 'Model Regresi' untuk melihat uji asumsi."))
                )
              )
            )
          )
        )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  # Reactive values
  values <- reactiveValues(
    transformed_data = NULL,
    current_regression = NULL,
    categorized_vars = character(0)
  )
  
  # Initialize data
  observe({
    # Initialize transformed data
    if (is.null(values$transformed_data)) {
      values$transformed_data <- sovi_data
    }
    
    # Update choices for various inputs
    numeric_vars <- names(values$transformed_data)[sapply(values$transformed_data, is.numeric)]
    all_vars <- names(values$transformed_data)
    
    updateSelectInput(session, "var_to_categorize", choices = numeric_vars)
    updateSelectInput(session, "desc_var", choices = numeric_vars)
    updateSelectInput(session, "viz_var", choices = numeric_vars)
    updateSelectInput(session, "x_var", choices = numeric_vars)
    updateSelectInput(session, "y_var", choices = numeric_vars)
    updateSelectInput(session, "map_var", choices = numeric_vars)
    updateSelectInput(session, "norm_var", choices = numeric_vars)
    updateSelectInput(session, "homo_var", choices = numeric_vars)
    updateSelectInput(session, "prop_var", choices = numeric_vars)
    updateSelectInput(session, "var_test_var", choices = numeric_vars)
    updateSelectInput(session, "anova1_dep", choices = numeric_vars)
    updateSelectInput(session, "anova2_dep", choices = numeric_vars)
    updateSelectInput(session, "reg_dependent", choices = numeric_vars)
    updateCheckboxGroupInput(session, "reg_independent", choices = numeric_vars)
    
    # Update categorical variable choices
    cat_vars <- names(values$transformed_data)[sapply(values$transformed_data, function(x) is.factor(x) || is.character(x))]
    updateSelectInput(session, "group_var", choices = cat_vars)
    updateSelectInput(session, "var_group_var", choices = cat_vars)
    updateSelectInput(session, "anova1_indep", choices = cat_vars)
    updateSelectInput(session, "anova2_indep1", choices = cat_vars)
    updateSelectInput(session, "anova2_indep2", choices = cat_vars)
  })
  
  # Check if shapefile is available
  output$shp_available <- reactive({
    return(!is.null(data_list$shp))
  })
  outputOptions(output, "shp_available", suspendWhenHidden = FALSE)
  
  # BERANDA - No specific server logic needed
  
  # MANAJEMEN DATA
  output$original_data <- DT::renderDataTable({
    DT::datatable(sovi_data, options = list(scrollX = TRUE, pageLength = 10))
  })
  
  output$data_summary <- renderPrint({
    summary(sovi_data)
  })
  
  # Categorization logic
  observeEvent(input$categorize_btn, {
    req(input$var_to_categorize, input$n_categories, input$categorize_method)
    
    var_data <- values$transformed_data[[input$var_to_categorize]]
    
    if (input$categorize_method == "quantile") {
      breaks <- quantile(var_data, probs = seq(0, 1, length.out = input$n_categories + 1), na.rm = TRUE)
      categories <- cut(var_data, breaks = breaks, include.lowest = TRUE, 
                       labels = paste0("Q", 1:input$n_categories))
    } else if (input$categorize_method == "width") {
      categories <- cut(var_data, breaks = input$n_categories, 
                       labels = paste0("Cat", 1:input$n_categories))
    } else if (input$categorize_method == "kmeans") {
      km <- kmeans(var_data[!is.na(var_data)], centers = input$n_categories)
      categories <- rep(NA, length(var_data))
      categories[!is.na(var_data)] <- paste0("Cluster", km$cluster)
      categories <- factor(categories)
    }
    
    # Generate unique variable name
    base_name <- paste0(input$var_to_categorize, "_cat")
    counter <- 1
    new_var_name <- base_name
    
    # Check if variable name already exists and create unique name
    while (new_var_name %in% names(values$transformed_data)) {
      counter <- counter + 1
      new_var_name <- paste0(base_name, "_", counter)
    }
    
    # Add categorized variable to transformed data
    values$transformed_data[[new_var_name]] <- categories
    
    # Track categorized variables
    values$categorized_vars <- c(values$categorized_vars, new_var_name)
    
    # Update group variable choices
    cat_vars <- names(values$transformed_data)[sapply(values$transformed_data, function(x) is.factor(x) || is.character(x))]
    updateSelectInput(session, "group_var", choices = cat_vars)
    updateSelectInput(session, "var_group_var", choices = cat_vars)
    updateSelectInput(session, "anova1_indep", choices = cat_vars)
    updateSelectInput(session, "anova2_indep1", choices = cat_vars)
    updateSelectInput(session, "anova2_indep2", choices = cat_vars)
    
    # Show success message
    showNotification(
      paste("Variabel", new_var_name, "berhasil dibuat!"),
      type = "message",
      duration = 3
    )
  })
  
  # Display categorized variables list
  output$categorized_vars_list <- renderText({
    if (length(values$categorized_vars) == 0) {
      return("Belum ada variabel kategorik yang dibuat.")
    } else {
      paste("Variabel yang telah dikategorikan:\n", paste(values$categorized_vars, collapse = "\n"))
    }
  })
  
  # Check if there are categorized variables
  output$has_categorized_vars <- reactive({
    return(length(values$categorized_vars) > 0)
  })
  outputOptions(output, "has_categorized_vars", suspendWhenHidden = FALSE)
  
  # Update remove variable choices
  observe({
    updateSelectInput(session, "var_to_remove", choices = values$categorized_vars)
  })
  
  # Remove categorized variable
  observeEvent(input$remove_var_btn, {
    req(input$var_to_remove)
    
    # Remove from transformed data
    values$transformed_data[[input$var_to_remove]] <- NULL
    
    # Remove from categorized vars list
    values$categorized_vars <- values$categorized_vars[values$categorized_vars != input$var_to_remove]
    
    # Update choices
    cat_vars <- names(values$transformed_data)[sapply(values$transformed_data, function(x) is.factor(x) || is.character(x))]
    updateSelectInput(session, "group_var", choices = cat_vars)
    updateSelectInput(session, "var_group_var", choices = cat_vars)
    updateSelectInput(session, "anova1_indep", choices = cat_vars)
    updateSelectInput(session, "anova2_indep1", choices = cat_vars)
    updateSelectInput(session, "anova2_indep2", choices = cat_vars)
    
    # Show success message
    showNotification(
      paste("Variabel", input$var_to_remove, "berhasil dihapus!"),
      type = "warning",
      duration = 3
    )
  })
  
  output$categorization_plot <- renderPlot({
    req(input$var_to_categorize)
    
    if (!is.null(values$transformed_data)) {
      # Find the most recent categorized version of this variable
      var_cats <- values$categorized_vars[grepl(paste0("^", input$var_to_categorize, "_cat"), values$categorized_vars)]
      
      if (length(var_cats) > 0) {
        # Use the most recent categorization
        latest_cat <- var_cats[length(var_cats)]
        original_var <- values$transformed_data[[input$var_to_categorize]]
        cat_var <- values$transformed_data[[latest_cat]]
        
        df_plot <- data.frame(
          original = original_var,
          category = cat_var
        )
        
        ggplot(df_plot, aes(x = category, y = original, fill = category)) +
          geom_boxplot() +
          labs(title = paste("Kategorisasi Terbaru:", latest_cat),
               x = "Kategori", y = "Nilai Original") +
          theme_minimal() +
          theme(legend.position = "none")
      } else {
        # Show distribution of original variable if no categorization exists yet
        original_var <- values$transformed_data[[input$var_to_categorize]]
        
        ggplot(data.frame(var = original_var), aes(x = var)) +
          geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
          labs(title = paste("Distribusi", input$var_to_categorize, "(Belum Dikategorikan)"),
               x = input$var_to_categorize, y = "Frekuensi") +
          theme_minimal()
      }
    }
  })
  
  output$categorized_preview <- DT::renderDataTable({
    req(input$var_to_categorize)
    
    if (!is.null(values$transformed_data)) {
      # Find all categorized versions of this variable
      var_cats <- values$categorized_vars[grepl(paste0("^", input$var_to_categorize, "_cat"), values$categorized_vars)]
      
      if (length(var_cats) > 0) {
        # Show original variable and all its categorized versions
        cols_to_show <- c(input$var_to_categorize, var_cats)
        preview_data <- values$transformed_data[, cols_to_show, drop = FALSE]
        DT::datatable(preview_data, options = list(pageLength = 5, scrollX = TRUE))
      } else {
        # Show only original variable if no categorization exists
        preview_data <- values$transformed_data[, input$var_to_categorize, drop = FALSE]
        DT::datatable(preview_data, options = list(pageLength = 5))
      }
    }
  })
  
  output$transformed_data <- DT::renderDataTable({
    req(values$transformed_data)
    DT::datatable(values$transformed_data, options = list(scrollX = TRUE, pageLength = 10))
  })
  
  # EKSPLORASI DATA
  output$descriptive_stats <- DT::renderDataTable({
    numeric_data <- values$transformed_data[sapply(values$transformed_data, is.numeric)]
    desc_stats <- describe(numeric_data)[, c("n", "mean", "sd", "median", "min", "max", "skew", "kurtosis")]
    desc_stats <- round(desc_stats, 3)
    DT::datatable(desc_stats, options = list(pageLength = 15))
  })
  
  output$distribution_plot <- renderPlotly({
    req(input$desc_var)
    
    p <- ggplot(sovi_data, aes_string(x = input$desc_var)) +
      geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
      geom_density(aes(y = ..density.. * nrow(sovi_data) * diff(range(sovi_data[[input$desc_var]], na.rm = TRUE))/30), 
                   color = "red", size = 1) +
      labs(title = paste("Distribusi", input$desc_var),
           x = input$desc_var, y = "Frekuensi") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$descriptive_interpretation <- renderText({
    req(input$desc_var)
    
    var_data <- sovi_data[[input$desc_var]]
    mean_val <- mean(var_data, na.rm = TRUE)
    median_val <- median(var_data, na.rm = TRUE)
    sd_val <- sd(var_data, na.rm = TRUE)
    skew_val <- psych::skew(var_data, na.rm = TRUE)
    
    interpretation <- paste(
      "INTERPRETASI STATISTIK DESKRIPTIF:\n\n",
      sprintf("Variabel %s memiliki rata-rata %.3f dengan standar deviasi %.3f.", input$desc_var, mean_val, sd_val),
      sprintf("Nilai median adalah %.3f.", median_val),
      if (abs(skew_val) < 0.5) {
        "Distribusi data relatif simetris (normal)."
      } else if (skew_val > 0.5) {
        "Distribusi data condong ke kanan (positively skewed)."
      } else {
        "Distribusi data condong ke kiri (negatively skewed)."
      },
      if (mean_val > median_val) {
        "Rata-rata lebih besar dari median, mengindikasikan adanya outlier tinggi."
      } else if (mean_val < median_val) {
        "Rata-rata lebih kecil dari median, mengindikasikan adanya outlier rendah."
      } else {
        "Rata-rata dan median hampir sama, menunjukkan distribusi yang relatif simetris."
      }
    )
    
    return(interpretation)
  })
  
  output$visualization_plot <- renderPlotly({
    req(input$plot_type)
    
    if (input$plot_type == "hist") {
      req(input$viz_var)
      p <- ggplot(sovi_data, aes_string(x = input$viz_var)) +
        geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
        labs(title = paste("Histogram", input$viz_var)) +
        theme_minimal()
      
    } else if (input$plot_type == "box") {
      req(input$viz_var)
      p <- ggplot(sovi_data, aes_string(y = input$viz_var)) +
        geom_boxplot(fill = "lightblue") +
        labs(title = paste("Box Plot", input$viz_var)) +
        theme_minimal()
      
    } else if (input$plot_type == "scatter") {
      req(input$x_var, input$y_var)
      p <- ggplot(sovi_data, aes_string(x = input$x_var, y = input$y_var)) +
        geom_point(alpha = 0.6, color = "steelblue") +
        geom_smooth(method = "lm", color = "red") +
        labs(title = paste("Scatter Plot:", input$x_var, "vs", input$y_var)) +
        theme_minimal()
      
    } else if (input$plot_type == "corr") {
      numeric_data <- sovi_data[sapply(sovi_data, is.numeric)]
      cor_matrix <- cor(numeric_data, use = "complete.obs")
      
      # Convert correlation matrix to long format
      cor_df <- expand.grid(Var1 = rownames(cor_matrix), Var2 = colnames(cor_matrix))
      cor_df$value <- as.vector(cor_matrix)
      
      p <- ggplot(cor_df, aes(Var1, Var2, fill = value)) +
        geom_tile() +
        scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = "Correlation Matrix", fill = "Correlation")
    }
    
    ggplotly(p)
  })
  
  output$visualization_interpretation <- renderText({
    req(input$plot_type)
    
    interpretation <- switch(input$plot_type,
      "hist" = paste("Histogram menunjukkan distribusi frekuensi dari variabel", input$viz_var, 
                     ". Pola distribusi dapat mengindikasikan normalitas data dan keberadaan outlier."),
      "box" = paste("Box plot menampilkan ringkasan lima angka dari variabel", input$viz_var,
                    ". Kotak menunjukkan kuartil 1, median, dan kuartil 3, sedangkan titik-titik di luar whiskers adalah outlier."),
      "scatter" = {
        req(input$x_var, input$y_var)
        cor_val <- cor(sovi_data[[input$x_var]], sovi_data[[input$y_var]], use = "complete.obs")
        paste("Scatter plot menunjukkan hubungan antara", input$x_var, "dan", input$y_var,
              sprintf(". Korelasi antara kedua variabel adalah %.3f.", cor_val),
              if (abs(cor_val) > 0.7) "Terdapat korelasi yang kuat." 
              else if (abs(cor_val) > 0.3) "Terdapat korelasi yang moderat."
              else "Korelasi lemah atau tidak ada korelasi linear.")
      },
      "corr" = "Matrix korelasi menunjukkan kekuatan hubungan linear antara semua pasangan variabel numerik. Warna biru menunjukkan korelasi negatif, merah menunjukkan korelasi positif, dan putih menunjukkan tidak ada korelasi."
    )
    
    return(paste("INTERPRETASI VISUALISASI:\n\n", interpretation))
  })
  
  # Geographic Map
  output$geographic_map <- renderLeaflet({
    req(input$map_var, data_list$shp)
    
    # Merge shapefile with data if needed
    map_data <- data_list$shp
    if ("DISTRICTCODE" %in% names(sovi_data) && "DISTRICTCODE" %in% names(map_data)) {
      map_data <- merge(map_data, sovi_data, by = "DISTRICTCODE", all.x = TRUE)
    }
    
    # Create color palette
    pal <- colorNumeric(palette = input$color_palette, domain = map_data[[input$map_var]], na.color = "transparent")
    
    leaflet(map_data) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(get(input$map_var)),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        popup = ~paste0("District: ", DISTRICTCODE, "<br>", input$map_var, ": ", get(input$map_var))
      ) %>%
      addLegend(pal = pal, values = ~get(input$map_var), opacity = 0.7, title = input$map_var, position = "bottomright")
  })
  
  output$map_interpretation <- renderText({
    req(input$map_var)
    
    interpretation <- paste(
      "INTERPRETASI PETA GEOGRAFIS:\n\n",
      sprintf("Peta menampilkan distribusi spasial variabel %s di berbagai kabupaten.", input$map_var),
      "Warna yang lebih gelap menunjukkan nilai yang lebih tinggi, sedangkan warna yang lebih terang menunjukkan nilai yang lebih rendah.",
      "Pola spasial dapat mengindikasikan clustering geografis dari variabel kerentanan sosial.",
      "Area dengan warna serupa yang berdekatan secara geografis menunjukkan adanya autokorelasi spasial."
    )
    
    return(interpretation)
  })
  
  # UJI ASUMSI
  # Normality Test
  observeEvent(input$run_normality, {
    req(input$norm_var, input$norm_test)
    
    var_data <- sovi_data[[input$norm_var]]
    var_data <- var_data[!is.na(var_data)]
    
    if (input$norm_test == "shapiro") {
      if (length(var_data) <= 5000) {
        test_result <- shapiro.test(var_data)
      } else {
        test_result <- list(method = "Shapiro-Wilk", 
                           data.name = input$norm_var,
                           p.value = NA,
                           statistic = NA,
                           note = "Sample size too large for Shapiro-Wilk test")
      }
    } else if (input$norm_test == "ad") {
      test_result <- nortest::ad.test(var_data)
    } else if (input$norm_test == "ks") {
      test_result <- nortest::lillie.test(var_data)
    }
    
    output$normality_result <- renderPrint({
      if (!is.na(test_result$p.value)) {
        test_result
      } else {
        cat("Note:", test_result$note)
      }
    })
    
    output$normality_plot <- renderPlot({
      par(mfrow = c(1, 2))
      
      # Histogram with normal curve
      hist(var_data, breaks = 30, freq = FALSE, main = paste("Histogram of", input$norm_var),
           xlab = input$norm_var, col = "lightblue")
      curve(dnorm(x, mean = mean(var_data), sd = sd(var_data)), add = TRUE, col = "red", lwd = 2)
      
      # Q-Q plot
      qqnorm(var_data, main = paste("Q-Q Plot of", input$norm_var))
      qqline(var_data, col = "red", lwd = 2)
    })
    
    output$normality_interpretation <- renderText({
      if (!is.na(test_result$p.value)) {
        interpretation <- paste(
          "INTERPRETASI UJI NORMALITAS:\n\n",
          sprintf("Uji %s untuk variabel %s menghasilkan p-value = %.6f", 
                  test_result$method, input$norm_var, test_result$p.value),
          if (test_result$p.value > 0.05) {
            "Dengan alpha = 0.05, kita GAGAL MENOLAK hipotesis nul. Data mengikuti distribusi normal."
          } else {
            "Dengan alpha = 0.05, kita MENOLAK hipotesis nul. Data TIDAK mengikuti distribusi normal."
          },
          "\nHipotesis:",
          "H0: Data mengikuti distribusi normal",
          "H1: Data tidak mengikuti distribusi normal",
          "\nPlot Q-Q dapat digunakan untuk validasi visual. Jika titik-titik mengikuti garis diagonal, data cenderung normal."
        )
      } else {
        interpretation <- "Uji normalitas tidak dapat dilakukan karena ukuran sampel terlalu besar untuk uji Shapiro-Wilk."
      }
      
      return(interpretation)
    })
  })
  
  # Homogeneity Test
  observeEvent(input$run_homogeneity, {
    req(input$homo_var, input$group_var, input$homo_test)
    
    if (!input$group_var %in% names(values$transformed_data)) {
      return()
    }
    
    test_data <- data.frame(
      var = values$transformed_data[[input$homo_var]],
      group = values$transformed_data[[input$group_var]]
    )
    test_data <- test_data[complete.cases(test_data), ]
    
    if (input$homo_test == "levene") {
      test_result <- car::leveneTest(var ~ group, data = test_data)
    } else if (input$homo_test == "bartlett") {
      test_result <- bartlett.test(var ~ group, data = test_data)
    }
    
    output$homogeneity_result <- renderPrint({
      test_result
    })
    
    output$homogeneity_plot <- renderPlot({
      ggplot(test_data, aes(x = group, y = var, fill = group)) +
        geom_boxplot() +
        labs(title = paste("Homogeneity Test:", input$homo_var, "by", input$group_var),
             x = input$group_var, y = input$homo_var) +
        theme_minimal() +
        theme(legend.position = "none")
    })
    
    output$homogeneity_interpretation <- renderText({
      p_val <- if (input$homo_test == "levene") test_result$`Pr(>F)`[1] else test_result$p.value
      
      interpretation <- paste(
        "INTERPRETASI UJI HOMOGENITAS:\n\n",
        sprintf("Uji %s untuk homogenitas varians antara kelompok menghasilkan p-value = %.6f", 
                ifelse(input$homo_test == "levene", "Levene", "Bartlett"), p_val),
        if (p_val > 0.05) {
          "Dengan alpha = 0.05, kita GAGAL MENOLAK hipotesis nul. Varians antar kelompok adalah homogen (sama)."
        } else {
          "Dengan alpha = 0.05, kita MENOLAK hipotesis nul. Varians antar kelompok TIDAK homogen (berbeda)."
        },
        "\nHipotesis:",
        "H0: Varians antar kelompok adalah sama (homogen)",
        "H1: Varians antar kelompok tidak sama (heterogen)",
        "\nBox plot dapat digunakan untuk validasi visual. Jika tinggi kotak-kotak relatif sama, varians cenderung homogen."
      )
      
      return(interpretation)
    })
  })
  
  # STATISTIK INFERENSIA I
  # Proportion Test
  observeEvent(input$run_prop_test, {
    req(input$prop_var, input$prop_value, input$prop_test_value)
    
    var_data <- sovi_data[[input$prop_var]]
    successes <- sum(var_data >= input$prop_value, na.rm = TRUE)
    n <- sum(!is.na(var_data))
    
    test_result <- prop.test(successes, n, p = input$prop_test_value)
    
    output$proportion_result <- renderPrint({
      test_result
    })
    
    output$proportion_plot <- renderPlot({
      observed_prop <- successes / n
      
      # Create visualization
      df_plot <- data.frame(
        Type = c("Observed", "Expected"),
        Proportion = c(observed_prop, input$prop_test_value)
      )
      
      ggplot(df_plot, aes(x = Type, y = Proportion, fill = Type)) +
        geom_bar(stat = "identity", alpha = 0.7) +
        geom_text(aes(label = round(Proportion, 3)), vjust = -0.5) +
        labs(title = "Proportion Test Results",
             y = "Proportion") +
        theme_minimal() +
        scale_fill_brewer(palette = "Set2")
    })
    
    output$proportion_interpretation <- renderText({
      observed_prop <- successes / n
      
      interpretation <- paste(
        "INTERPRETASI UJI PROPORSI:\n\n",
        sprintf("Dari %d observasi, %d memiliki nilai >= %.2f, sehingga proporsi observasi = %.4f", 
                n, successes, input$prop_value, observed_prop),
        sprintf("Uji proporsi satu sampel dengan proporsi yang diuji = %.4f menghasilkan p-value = %.6f", 
                input$prop_test_value, test_result$p.value),
        if (test_result$p.value > 0.05) {
          sprintf("Dengan alpha = 0.05, kita GAGAL MENOLAK hipotesis nul. Proporsi populasi sama dengan %.4f.", input$prop_test_value)
        } else {
          sprintf("Dengan alpha = 0.05, kita MENOLAK hipotesis nul. Proporsi populasi TIDAK sama dengan %.4f.", input$prop_test_value)
        },
        sprintf("Interval kepercayaan 95%% untuk proporsi: [%.4f, %.4f]", 
                test_result$conf.int[1], test_result$conf.int[2])
      )
      
      return(interpretation)
    })
  })
  
  # Variance Test
  observeEvent(input$run_var_test, {
    req(input$var_test_var, input$var_test_type)
    
    var_data <- values$transformed_data[[input$var_test_var]]
    
    if (input$var_test_type == "one") {
      req(input$test_variance)
      # Chi-square test for single variance
      n <- sum(!is.na(var_data))
      sample_var <- var(var_data, na.rm = TRUE)
      chi_stat <- (n - 1) * sample_var / input$test_variance
      p_value <- 2 * min(pchisq(chi_stat, n - 1), 1 - pchisq(chi_stat, n - 1))
      
      test_result <- list(
        statistic = chi_stat,
        p.value = p_value,
        parameter = n - 1,
        method = "Chi-square test for variance",
        sample.var = sample_var,
        test.var = input$test_variance
      )
      
    } else {
      req(input$var_group_var)
      if (!input$var_group_var %in% names(values$transformed_data)) {
        return()
      }
      
      test_data <- data.frame(
        var = var_data,
        group = values$transformed_data[[input$var_group_var]]
      )
      test_data <- test_data[complete.cases(test_data), ]
      
      groups <- split(test_data$var, test_data$group)
      if (length(groups) == 2) {
        test_result <- var.test(groups[[1]], groups[[2]])
      } else {
        test_result <- bartlett.test(var ~ group, data = test_data)
      }
    }
    
    output$variance_result <- renderPrint({
      test_result
    })
    
    output$variance_plot <- renderPlot({
      if (input$var_test_type == "one") {
        # Histogram with variance information
        hist(var_data, breaks = 30, main = paste("Distribution of", input$var_test_var),
             xlab = input$var_test_var, col = "lightblue")
        abline(v = mean(var_data, na.rm = TRUE), col = "red", lwd = 2, lty = 2)
        legend("topright", legend = c("Mean", paste("Sample Var =", round(var(var_data, na.rm = TRUE), 3))),
               col = c("red", "black"), lty = c(2, 1))
      } else {
        # Box plot for two or more groups
        test_data <- data.frame(
          var = var_data,
          group = values$transformed_data[[input$var_group_var]]
        )
        test_data <- test_data[complete.cases(test_data), ]
        
        ggplot(test_data, aes(x = group, y = var, fill = group)) +
          geom_boxplot() +
          labs(title = paste("Variance Test:", input$var_test_var, "by", input$var_group_var),
               x = input$var_group_var, y = input$var_test_var) +
          theme_minimal() +
          theme(legend.position = "none")
      }
    })
    
    output$variance_interpretation <- renderText({
      if (input$var_test_type == "one") {
        interpretation <- paste(
          "INTERPRETASI UJI VARIANS SATU KELOMPOK:\n\n",
          sprintf("Uji Chi-square untuk varians dengan H0: σ² = %.4f menghasilkan p-value = %.6f", 
                  input$test_variance, test_result$p.value),
          sprintf("Varians sampel = %.4f", test_result$sample.var),
          if (test_result$p.value > 0.05) {
            sprintf("Dengan alpha = 0.05, kita GAGAL MENOLAK hipotesis nul. Varians populasi sama dengan %.4f.", input$test_variance)
          } else {
            sprintf("Dengan alpha = 0.05, kita MENOLAK hipotesis nul. Varians populasi TIDAK sama dengan %.4f.", input$test_variance)
          }
        )
      } else {
        interpretation <- paste(
          "INTERPRETASI UJI VARIANS DUA KELOMPOK ATAU LEBIH:\n\n",
          sprintf("Uji untuk kesamaan varians antar kelompok menghasilkan p-value = %.6f", test_result$p.value),
          if (test_result$p.value > 0.05) {
            "Dengan alpha = 0.05, kita GAGAL MENOLAK hipotesis nul. Varians antar kelompok adalah sama."
          } else {
            "Dengan alpha = 0.05, kita MENOLAK hipotesis nul. Varians antar kelompok TIDAK sama."
          }
        )
      }
      
      return(interpretation)
    })
  })
  
  # STATISTIK INFERENSIA II
  # ANOVA One-way
  observeEvent(input$run_anova1, {
    req(input$anova1_dep, input$anova1_indep)
    
    if (!input$anova1_indep %in% names(values$transformed_data)) {
      return()
    }
    
    anova_data <- data.frame(
      dependent = values$transformed_data[[input$anova1_dep]],
      independent = values$transformed_data[[input$anova1_indep]]
    )
    anova_data <- anova_data[complete.cases(anova_data), ]
    
    anova_result <- aov(dependent ~ independent, data = anova_data)
    anova_summary <- summary(anova_result)
    
    values$anova1_result <- anova_result
    values$anova1_data <- anova_data
    
    output$anova1_result <- renderPrint({
      anova_summary
    })
    
    output$anova1_plot <- renderPlot({
      ggplot(anova_data, aes(x = independent, y = dependent, fill = independent)) +
        geom_boxplot() +
        geom_point(position = position_jitter(width = 0.2), alpha = 0.5) +
        stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "red") +
        labs(title = paste("ANOVA:", input$anova1_dep, "by", input$anova1_indep),
             x = input$anova1_indep, y = input$anova1_dep) +
        theme_minimal() +
        theme(legend.position = "none")
    })
    
    # Check if result is significant for post-hoc test
    p_value <- anova_summary[[1]]$`Pr(>F)`[1]
    output$anova1_significant <- reactive({
      return(p_value < 0.05)
    })
    outputOptions(output, "anova1_significant", suspendWhenHidden = FALSE)
    
    output$anova1_interpretation <- renderText({
      f_stat <- anova_summary[[1]]$`F value`[1]
      df1 <- anova_summary[[1]]$Df[1]
      df2 <- anova_summary[[1]]$Df[2]
      
      interpretation <- paste(
        "INTERPRETASI ANOVA SATU ARAH:\n\n",
        sprintf("F(%d, %d) = %.4f, p-value = %.6f", df1, df2, f_stat, p_value),
        if (p_value > 0.05) {
          "Dengan alpha = 0.05, kita GAGAL MENOLAK hipotesis nul. Tidak ada perbedaan rata-rata yang signifikan antar kelompok."
        } else {
          "Dengan alpha = 0.05, kita MENOLAK hipotesis nul. Terdapat perbedaan rata-rata yang signifikan antar kelompok."
        },
        "\nHipotesis:",
        "H0: μ1 = μ2 = μ3 = ... (semua rata-rata kelompok sama)",
        "H1: Minimal ada satu rata-rata kelompok yang berbeda",
        if (p_value < 0.05) {
          "\nKarena hasil signifikan, disarankan untuk melakukan uji post-hoc untuk mengetahui kelompok mana yang berbeda."
        } else {
          ""
        }
      )
      
      return(interpretation)
    })
  })
  
  # Post-hoc test
  observeEvent(input$run_posthoc, {
    req(values$anova1_result, values$anova1_data)
    
    posthoc_result <- TukeyHSD(values$anova1_result)
    
    output$posthoc_result <- renderPrint({
      posthoc_result
    })
  })
  
  # ANOVA Two-way
  observeEvent(input$run_anova2, {
    req(input$anova2_dep, input$anova2_indep1, input$anova2_indep2)
    
    if (!input$anova2_indep1 %in% names(values$transformed_data) || 
        !input$anova2_indep2 %in% names(values$transformed_data)) {
      return()
    }
    
    anova_data <- data.frame(
      dependent = values$transformed_data[[input$anova2_dep]],
      factor1 = values$transformed_data[[input$anova2_indep1]],
      factor2 = values$transformed_data[[input$anova2_indep2]]
    )
    anova_data <- anova_data[complete.cases(anova_data), ]
    
    if (input$include_interaction) {
      formula_str <- "dependent ~ factor1 * factor2"
    } else {
      formula_str <- "dependent ~ factor1 + factor2"
    }
    
    anova_result <- aov(as.formula(formula_str), data = anova_data)
    anova_summary <- summary(anova_result)
    
    output$anova2_result <- renderPrint({
      anova_summary
    })
    
    output$anova2_plot <- renderPlot({
      if (input$include_interaction) {
        ggplot(anova_data, aes(x = factor1, y = dependent, fill = factor2)) +
          geom_boxplot(position = position_dodge()) +
          labs(title = paste("Two-way ANOVA with Interaction:", input$anova2_dep),
               x = input$anova2_indep1, y = input$anova2_dep, fill = input$anova2_indep2) +
          theme_minimal()
      } else {
        p1 <- ggplot(anova_data, aes(x = factor1, y = dependent, fill = factor1)) +
          geom_boxplot() +
          labs(title = paste("Effect of", input$anova2_indep1), x = input$anova2_indep1, y = input$anova2_dep) +
          theme_minimal() + theme(legend.position = "none")
        
        p2 <- ggplot(anova_data, aes(x = factor2, y = dependent, fill = factor2)) +
          geom_boxplot() +
          labs(title = paste("Effect of", input$anova2_indep2), x = input$anova2_indep2, y = input$anova2_dep) +
          theme_minimal() + theme(legend.position = "none")
        
        gridExtra::grid.arrange(p1, p2, ncol = 2)
      }
    })
    
    output$anova2_interpretation <- renderText({
      interpretation <- "INTERPRETASI ANOVA DUA ARAH:\n\n"
      
      for (i in 1:nrow(anova_summary[[1]])) {
        if (!is.na(anova_summary[[1]]$`Pr(>F)`[i])) {
          effect_name <- rownames(anova_summary[[1]])[i]
          f_stat <- anova_summary[[1]]$`F value`[i]
          p_val <- anova_summary[[1]]$`Pr(>F)`[i]
          
          interpretation <- paste(interpretation,
            sprintf("Efek %s: F = %.4f, p-value = %.6f", effect_name, f_stat, p_val),
            if (p_val > 0.05) {
              " - Tidak signifikan\n"
            } else {
              " - Signifikan\n"
            }
          )
        }
      }
      
      interpretation <- paste(interpretation,
        "\nHipotesis untuk setiap efek:",
        "H0: Tidak ada efek dari faktor tersebut",
        "H1: Ada efek dari faktor tersebut"
      )
      
      return(interpretation)
    })
  })
  
  # REGRESI LINEAR
  observeEvent(input$run_regression, {
    req(input$reg_dependent, input$reg_independent)
    
    if (length(input$reg_independent) == 0) {
      return()
    }
    
    # Prepare data
    reg_vars <- c(input$reg_dependent, input$reg_independent)
    reg_data <- values$transformed_data[, reg_vars]
    reg_data <- reg_data[complete.cases(reg_data), ]
    
    # Create formula
    formula_str <- paste(input$reg_dependent, "~", paste(input$reg_independent, collapse = " + "))
    
    # Run regression
    reg_model <- lm(as.formula(formula_str), data = reg_data)
    values$current_regression <- reg_model
    values$reg_data <- reg_data
    
    output$regression_summary <- renderPrint({
      summary(reg_model)
    })
    
    output$regression_plots <- renderPlot({
      par(mfrow = c(2, 2))
      plot(reg_model)
    })
    
    # Indicate regression has been run
    output$regression_run <- reactive({
      return(!is.null(values$current_regression))
    })
    outputOptions(output, "regression_run", suspendWhenHidden = FALSE)
    
    output$regression_interpretation <- renderText({
      reg_summary <- summary(reg_model)
      r_squared <- reg_summary$r.squared
      adj_r_squared <- reg_summary$adj.r.squared
      f_stat <- reg_summary$fstatistic[1]
      p_value <- pf(f_stat, reg_summary$fstatistic[2], reg_summary$fstatistic[3], lower.tail = FALSE)
      
      interpretation <- paste(
        "INTERPRETASI REGRESI LINEAR BERGANDA:\n\n",
        sprintf("Model: %s", formula_str),
        sprintf("R-squared: %.4f (%.2f%% variasi dalam %s dijelaskan oleh model)", 
                r_squared, r_squared * 100, input$reg_dependent),
        sprintf("Adjusted R-squared: %.4f", adj_r_squared),
        sprintf("F-statistic: %.4f, p-value: %.6f", f_stat, p_value),
        if (p_value < 0.05) {
          "Model secara keseluruhan SIGNIFIKAN."
        } else {
          "Model secara keseluruhan TIDAK SIGNIFIKAN."
        },
        "\nKoefisien Regresi:"
      )
      
      coeffs <- reg_summary$coefficients
      for (i in 1:nrow(coeffs)) {
        var_name <- rownames(coeffs)[i]
        coeff <- coeffs[i, 1]
        p_val <- coeffs[i, 4]
        
        interpretation <- paste(interpretation,
          sprintf("%s: β = %.4f, p = %.4f %s", 
                  var_name, coeff, p_val,
                  ifelse(p_val < 0.05, "(signifikan)", "(tidak signifikan)"))
        )
      }
      
      return(interpretation)
    })
  })
  
  # Regression Assumptions
  observe({
    req(values$current_regression)
    
    reg_model <- values$current_regression
    
    # Normality test of residuals
    residuals <- residuals(reg_model)
    shapiro_test <- shapiro.test(residuals)
    
    output$reg_normality_test <- renderPrint({
      cat("Uji Normalitas Residual (Shapiro-Wilk):\n")
      shapiro_test
    })
    
    # VIF test for multicollinearity
    if (length(input$reg_independent) > 1) {
      vif_values <- car::vif(reg_model)
      output$vif_test <- renderPrint({
        cat("Variance Inflation Factor (VIF):\n")
        print(vif_values)
        cat("\nInterpretasi VIF:\n")
        cat("VIF < 5: Tidak ada multikolinearitas yang serius\n")
        cat("VIF 5-10: Multikolinearitas moderat\n")
        cat("VIF > 10: Multikolinearitas yang serius\n")
      })
    } else {
      output$vif_test <- renderText("VIF tidak dapat dihitung untuk regresi dengan satu variabel independen.")
    }
    
    # Heteroscedasticity test (Breusch-Pagan)
    bp_test <- car::ncvTest(reg_model)
    output$hetero_test <- renderPrint({
      cat("Uji Heteroskedastisitas (Breusch-Pagan):\n")
      bp_test
    })
    
    # Autocorrelation test (Durbin-Watson)
    dw_test <- car::durbinWatsonTest(reg_model)
    output$autocorr_test <- renderPrint({
      cat("Uji Autokorelasi (Durbin-Watson):\n")
      dw_test
    })
    
    # Assumption plots
    output$assumption_plots <- renderPlot({
      par(mfrow = c(2, 2))
      
      # Residuals vs Fitted
      plot(reg_model, which = 1, main = "Residuals vs Fitted")
      
      # Normal Q-Q plot
      plot(reg_model, which = 2, main = "Normal Q-Q")
      
      # Scale-Location plot
      plot(reg_model, which = 3, main = "Scale-Location")
      
      # Cook's distance
      plot(reg_model, which = 4, main = "Cook's Distance")
    })
    
    output$assumption_interpretation <- renderText({
      interpretation <- paste(
        "INTERPRETASI UJI ASUMSI REGRESI:\n\n",
        "1. NORMALITAS RESIDUAL:",
        sprintf("   Shapiro-Wilk test: p = %.6f", shapiro_test$p.value),
        if (shapiro_test$p.value > 0.05) {
          "   Residual mengikuti distribusi normal (asumsi terpenuhi)."
        } else {
          "   Residual TIDAK mengikuti distribusi normal (asumsi dilanggar)."
        },
        "\n2. MULTIKOLINEARITAS:",
        if (length(input$reg_independent) > 1) {
          if (any(car::vif(reg_model) > 10)) {
            "   Terdapat multikolinearitas yang serius (VIF > 10)."
          } else if (any(car::vif(reg_model) > 5)) {
            "   Terdapat multikolinearitas moderat (VIF 5-10)."
          } else {
            "   Tidak ada multikolinearitas yang serius (VIF < 5)."
          }
        } else {
          "   Tidak dapat dievaluasi untuk satu variabel independen."
        },
        "\n3. HETEROSKEDASTISITAS:",
        sprintf("   Breusch-Pagan test: p = %.6f", bp_test$p),
        if (bp_test$p > 0.05) {
          "   Varians residual homogen (asumsi terpenuhi)."
        } else {
          "   Terdapat heteroskedastisitas (asumsi dilanggar)."
        },
        "\n4. AUTOKORELASI:",
        sprintf("   Durbin-Watson statistic: %.4f", dw_test$dw),
        if (dw_test$p > 0.05) {
          "   Tidak ada autokorelasi (asumsi terpenuhi)."
        } else {
          "   Terdapat autokorelasi (asumsi dilanggar)."
        }
      )
      
      return(interpretation)
    })
  })
  
  # Download handlers
  output$download_transformed <- downloadHandler(
    filename = function() {
      paste("SOVI_transformed_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(values$transformed_data, file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)