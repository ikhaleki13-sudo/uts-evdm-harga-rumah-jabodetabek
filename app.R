# Dashboard UTS Eksplorasi dan Visualisasi Data Multivariat
# Topik : Analisis Harga Rumah Jabodetabek dengan Regresi Linear Berganda
# Nama  : Juliana Riska Betek Leki | NIM: 231068006 | Prodi: Statistika
# Dosen : Inggit Fatika, S.Stat., M.Stat

library(shiny)      # kerangka aplikasi interaktif
library(tidyverse)  # dplyr (olah data) dan ggplot2 (grafik)
library(plotly)     # grafik interaktif (ggplotly)
library(DT)         # tabel interaktif
library(car)        # vif()
library(lmtest)     # bptest(), dwtest()
library(sandwich)   # vcovHC(): galat baku robust
library(scales)     # format angka (comma)

# File CSV harus satu folder dengan app.R.
cari <- list.files(pattern = "^jabodetabek_house_price.*\\.csv$", ignore.case = TRUE)
file_path <- if (length(cari) > 0) cari[1] else "jabodetabek_house_price.csv"

# Fungsi bantu interpretasi dan statistik
kotak <- function(judul, isi) {
  div(class = "kotak-interpretasi",
      strong(paste0("Interpretasi ", judul, ": ")), isi)
}

skew <- function(x) { m <- mean(x); mean((x - m)^3) / mean((x - m)^2)^1.5 }
kurt <- function(x) { m <- mean(x); mean((x - m)^4) / mean((x - m)^2)^2 - 3 }
n_pencilan <- function(x) {
  q <- quantile(x, c(0.25, 0.75))
  sum(x < q[1] - 1.5 * IQR(x) | x > q[2] + 1.5 * IQR(x))
}

bentuk <- function(s) {
  if (s > 0.5) "miring ke kanan" else if (s < -0.5) "miring ke kiri" else "relatif simetris"
}
kekuatan <- function(r) {
  arah <- if (r >= 0) "positif" else "negatif"
  kuat <- if (abs(r) >= 0.7) "kuat" else if (abs(r) >= 0.4) "sedang" else "lemah"
  paste(arah, kuat)
}
p_txt <- function(p) if (p < 0.001) "p < 0.001" else paste0("p = ", round(p, 3))

# UI dengan Tema UI Hijau Estetik & Warna Grafik Original
ui <- fluidPage(
  tags$head(
    tags$title("UTS Multivariat - Harga Rumah Jabodetabek"),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&display=swap"),
    tags$style(HTML("
      body { font-family:'Inter',system-ui,-apple-system,'Segoe UI',sans-serif; background:#f4f6f4;
             color:#2c3e35; font-size:15px; line-height:1.65; }
      .hero { background:linear-gradient(135deg,#2d6a4f 0%,#40916c 60%,#52b788 100%); color:#fff;
              border-radius:0 0 28px 28px; padding:38px 44px 28px; margin:0 -15px 26px;
              position:relative; overflow:hidden; box-shadow:0 10px 25px -5px rgba(45,106,79,0.15); }
      .hero:before { content:''; position:absolute; right:-70px; top:-90px; width:300px; height:300px;
                     border-radius:50%; background:rgba(255,255,255,.08); }
      .hero:after { content:''; position:absolute; right:140px; bottom:-120px; width:220px; height:220px;
                    border-radius:50%; background:rgba(255,255,255,.05); }
      .hero .label-atas { font-size:12px; font-weight:700; letter-spacing:2px; color:#d8f3dc; margin-bottom:8px; }
      .hero h1 { margin:0 0 8px; font-weight:800; font-size:34px; letter-spacing:-.5px; }
      .hero p { margin:0 0 18px; color:#e8f5e9; font-size:15px; max-width:760px; }
      .chip { display:inline-block; background:rgba(255,255,255,.16); border:1px solid rgba(255,255,255,.3);
              border-radius:999px; padding:5px 15px; margin:0 8px 6px 0; font-size:13px; font-weight:500; }
      .well { background:#fff; border:1px solid #d8e2dc; border-radius:18px;
              box-shadow:0 1px 3px rgba(45,106,79,.06); padding:20px; }
      .well h4 { font-weight:700; margin-top:0; color:#2d6a4f; }
      .well .control-label { font-weight:600; color:#344e41; }
      .btn-primary { background:#40916c; border:none; border-radius:10px; font-weight:600; padding:9px 12px; }
      .btn-primary:hover, .btn-primary:focus { background:#2d6a4f; }
      .btn-default { border-radius:10px; font-weight:600; }
      .irs--shiny .irs-bar { background:#40916c; border-color:#40916c; }
      .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single { background:#40916c; }
      .irs--shiny .irs-handle { border-color:#40916c; }
      .nav-tabs { border-bottom:none; margin-bottom:8px; }
      .nav-tabs > li > a { border:none; border-radius:999px; background:#e9edf0; color:#52796f;
                           font-weight:600; font-size:14px; margin:0 6px 8px 0; padding:8px 16px; }
      .nav-tabs > li > a:hover { background:#d8f3dc; color:#2d6a4f; border:none; }
      .nav-tabs > li.active > a, .nav-tabs > li.active > a:hover, .nav-tabs > li.active > a:focus {
        background:#40916c; color:#fff; border:none; box-shadow:0 4px 12px rgba(64,145,108,.35); }
      .kartu { background:#fff; border:1px solid #d8e2dc; border-radius:18px; padding:24px; margin-bottom:22px;
                box-shadow:0 1px 3px rgba(45,106,79,.04), 0 10px 28px rgba(45,106,79,.03); }
      .kartu h4 { margin-top:0; margin-bottom:16px; padding:2px 0 2px 12px; border-left:4px solid #40916c;
                  font-weight:700; font-size:18px; color:#2d6a4f; }
      .kartu h5 { font-weight:700; color:#40916c; margin:22px 0 8px; }
      
      .stat { background:#fff; border:1px solid #d8e2dc; border-radius:18px; padding:22px 12px; box-shadow:0 1px 3px rgba(45,106,79,.04); border-top:4px solid #4a7c59; margin-bottom:22px; }
      .stat.hijau { border-top-color:#2d6a4f; }
      .stat.oranye { border-top-color:#bc6c25; }
      .stat.ungu { border-top-color:#2a9d8f; }
      .angka { font-size:26px; font-weight:800; color:#2c3e35; text-align:center; }
      .label-angka { text-align:center; color:#52796f; font-size:13px; font-weight:500; margin-top:4px; }

      .kotak-interpretasi { background:linear-gradient(90deg,#f0fdf4,#f8fafc); border-left:5px solid #40916c;
             padding:13px 18px; border-radius:10px; margin:10px 0 28px 0; color:#334155; }
      .kotak-interpretasi strong { color:#2d6a4f; }
      
      table.dataTable thead th { background:#4a7c59; color:#ffffff; font-weight:700; border-bottom:2px solid #3a5a40; }
      table.dataTable tbody tr:hover { background:#f0fdf4 !important; }
      pre { font-size:13px; background:#f8fafc; border:1px solid #d8e2dc; border-radius:10px; }
    "))
  ),

  div(class = "hero",
      div(class = "label-atas", "UTS EKSPLORASI DAN VISUALISASI DATA MULTIVARIAT"),
      h1("Harga Rumah Jabodetabek"),
      p("Eksplorasi univariat, bivariat, multivariat, dan regresi linear berganda atas iklan rumah dijual di Jabodetabek."),
      span(class = "chip", icon("user"), " Juliana Riska Betek Leki"),
      span(class = "chip", icon("id-card"), " NIM 231068006"),
      span(class = "chip", icon("graduation-cap"), " Statistika"),
      span(class = "chip", icon("chalkboard-user"), " Inggit Fatika, S.Stat., M.Stat")
  ),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Filter Data"),
      p("Ubah filter, maka semua tabel, grafik, interpretasi, dan model ikut berubah.",
        style = "font-size:13px; color:#52796f;"),
      sliderInput("filter_harga", "Rentang Harga (Miliar Rp):", min = 0, max = 50, value = c(0, 20), step = 0.1),
      checkboxGroupInput("filter_kota", "Kota/Wilayah:", choices = NULL),
      checkboxGroupInput("filter_sert", "Jenis Sertifikat:", choices = NULL),
      actionButton("reset", "Reset Filter", class = "btn-primary btn-block")
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        # Tab 1: dataset
        tabPanel("1. Dataset",
          br(),
          fluidRow(
            column(3, div(class = "stat", div(class = "angka", textOutput("n_obs")), div(class = "label-angka", "Observasi (setelah filter)"))),
            column(3, div(class = "stat hijau", div(class = "angka", "5"), div(class = "label-angka", "Variabel dipakai"))),
            column(3, div(class = "stat oranye", div(class = "angka", "3"), div(class = "label-angka", "Variabel numerik"))),
            column(3, div(class = "stat ungu", div(class = "angka", "2"), div(class = "label-angka", "Variabel kategorik")))
          ),
          div(class = "kartu",
              h4("Latar Belakang, Permasalahan, dan Tujuan"),
              p(strong("Latar Belakang & Permasalahan:"), " Pasar properti perumahan di wilayah Jabodetabek menunjukkan disparitas (ketimpangan) harga yang sangat tinggi akibat perbedaan ukuran luas tanah, luas bangunan, wilayah administratif (lokasi), serta status legalitas sertifikat. Ketidakseragaman informasi penawaran di lapangan sering menyulitkan calon pembeli maupun penjual dalam menentukan kewajaran harga pasar secara objektif serta mengukur seberapa besar pengaruh masing-masing karakteristik fisik dan lokasi terhadap nilai properti."),
              p(strong("Tujuan Analisis:")),
              tags$ol(
                tags$li("Mengeksplorasi karakteristik data secara univariat, bivariat, dan multivariat."),
                tags$li("Membangun model regresi linear berganda untuk menjelaskan variasi harga rumah."),
                tags$li("Menilai kelayakan model melalui serangkaian uji asumsi residual.")
              )),
          div(class = "kartu",
              h4("Sumber Data"),
              p("Data iklan rumah dijual di Jabodetabek bersumber dari platform ", strong("Kaggle"), " melalui dataset ", em("Jabodetabek House Price"), ". Harga yang tercatat pada dataset ini merupakan ", strong("harga penawaran"), " iklan properti, bukan harga transaksi akhir."),
              uiOutput("teks_sumber")),
          div(class = "kartu",
              h4("Definisi Variabel"),
              DTOutput("tabel_definisi")),
          div(class = "kartu",
              h4("Alasan Pemilihan Variabel"),
              tags$ul(
                tags$li(strong("price_in_rp (respons Y):"), " harga adalah hal yang ingin dijelaskan oleh pembeli dan penjual."),
                tags$li(strong("land_size_m2 (X1) dan building_size_m2 (X2):"), " ukuran fisik properti, penentu utama nilai rumah."),
                tags$li(strong("city (X3):"), " lokasi sangat menentukan harga di wilayah Jabodetabek."),
                tags$li(strong("certificate (X4):"), " status hukum kepemilikan memengaruhi keamanan investasi."))),
          div(class = "kartu",
              h4("Singkatan Sertifikat"),
              tags$ul(
                tags$li(strong("SHM:"), " Sertifikat Hak Milik, hak terkuat dan penuh."),
                tags$li(strong("HGB:"), " Hak Guna Bangunan, berjangka waktu (umumnya 30 tahun)."),
                tags$li(strong("Lainnya:"), " Hak Pakai, AJB, girik, dan sejenisnya."),
                tags$li(strong("Tidak diketahui:"), " pengiklan mengosongkan data sertifikat."))),
          div(class = "kartu",
              h4("Cuplikan Data"),
              DTOutput("tabel_cuplikan"))
        ),

        # Tab 2: pengolahan data
        tabPanel("2. Pengolahan Data",
          br(),
          div(class = "kartu",
              h4("Tahapan Pembersihan Data"),
              DTOutput("tabel_log"),
              tags$ol(
                tags$li(strong("Baris kosong:"), " baris dengan harga atau luas kosong dibuang karena tidak bisa dianalisis."),
                tags$li(strong("Luas tidak wajar:"), " luas tanah dan bangunan dibatasi 10 sampai 2.000 m² untuk membuang salah input."),
                tags$li(strong("Pencilan harga:"), " metode IQR (Q1 - 1,5 IQR, Q3 + 1,5 IQR) pada log10 harga. Skala log dipakai karena harga miring ke kanan; tanpa log, hampir semua rumah mahal akan terbuang."))),
          div(class = "kartu",
              h4("Data Hilang (Missing Value) dan Duplikat"),
              p("Tabel berikut menyajikan informasi mengenai jumlah nilai kosong (*missing value*) pada setiap variabel utama yang digunakan dalam analisis:"),
              DTOutput("tabel_hilang"),
              br(),
              uiOutput("teks_hilang"),
              br(),
              uiOutput("teks_duplikat")),
          div(class = "kartu",
              h4("Transformasi Data"),
              tags$ul(
                tags$li("Harga dinyatakan dalam Rp miliar (price_miliar) agar mudah dibaca."),
                tags$li("Regresi memakai ln(harga), ln(luas tanah), dan ln(luas bangunan): distribusinya miring ke kanan, log menstabilkan ragam, dan koefisien dibaca sebagai elastisitas (persen)."),
                tags$li("Sertifikat disederhanakan menjadi 4 kategori; nama kota dirapikan (spasi dihapus).")))
        ),

        # Tab 3: univariat
        tabPanel("3. Univariat",
          br(),
          div(class = "kartu",
              h4("A. Statistik Deskriptif Variabel Numerik"),
              DTOutput("tabel_desk"),
              uiOutput("tafsir_desk")),
          div(class = "kartu",
              h4("B. Distribusi Frekuensi Variabel Kategorik"),
              fluidRow(
                column(6, DTOutput("tabel_frek_kota"), uiOutput("tafsir_frek_kota")),
                column(6, DTOutput("tabel_frek_sert"), uiOutput("tafsir_frek_sert")))),
          div(class = "kartu",
              h4("C. Visualisasi Univariat (Pilih Berdasarkan Jenis Variabel)"),
              p("Silakan pilih tab di bawah ini untuk melihat grafik berdasarkan jenis variabel yang ingin dianalisis secara spesifik:", style = "color:#52796f; font-size:13px; margin-bottom:15px;"),
              tabsetPanel(
                tabPanel("📊 Variabel Numerik",
                  br(),
                  p("Grafik univariat untuk variabel berjenis numerik (Harga, Luas Bangunan, dan Luas Tanah):", style = "font-weight:600; color:#2c3e35;"),
                  plotlyOutput("u1"), uiOutput("tu1"),
                  plotlyOutput("u2"), uiOutput("tu2"),
                  plotlyOutput("u3"), uiOutput("tu3"),
                  plotlyOutput("u4"), uiOutput("tu4")
                ),
                tabPanel("📋 Variabel Kategorik",
                  br(),
                  p("Grafik univariat untuk variabel berjenis kategorik (Kota dan Jenis Sertifikat):", style = "font-weight:600; color:#2c3e35;"),
                  plotlyOutput("u5"), uiOutput("tu5"),
                  plotlyOutput("u6"), uiOutput("tu6")
                )
              )
          )
        ),

        # Tab 4: bivariat
        tabPanel("4. Bivariat",
          br(),
          div(class = "kartu",
              h4("A. Korelasi Antarvariabel Numerik"),
              DTOutput("tabel_korelasi"),
              uiOutput("tafsir_korelasi")),
          div(class = "kartu",
              h4("B. Visualisasi Bivariat (Pilih Berdasarkan Kombinasi Variabel)"),
              p("Silakan pilih tab di bawah ini untuk melihat jenis hubungan antarvariabel secara spesifik:", style = "color:#52796f; font-size:13px; margin-bottom:15px;"),
              tabsetPanel(
                tabPanel("📈 Numerik vs Numerik",
                  br(),
                  plotlyOutput("b1"), uiOutput("tb1"),
                  plotlyOutput("b2"), uiOutput("tb2"),
                  plotlyOutput("b3"), uiOutput("tb3")
                ),
                tabPanel("📊 Numerik vs Kategorik",
                  br(),
                  plotlyOutput("b4"), uiOutput("tb4"),
                  plotlyOutput("b5"), uiOutput("tb5")
                ),
                tabPanel("📋 Kategorik vs Kategorik",
                  br(),
                  plotlyOutput("b6"), uiOutput("tb6")
                )
              )
          )
        ),

        # Tab 5: multivariat
        tabPanel("5. Multivariat",
          br(),
          div(class = "kartu",
              h4("A. Vektor Rata-rata dan Matriks Korelasi"),
              DTOutput("tabel_multi"),
              uiOutput("tafsir_multi")),
          div(class = "kartu",
              h4("B. Pencilan dan Normalitas Multivariat (Jarak Mahalanobis)"),
              uiOutput("teks_mahal")),
          div(class = "kartu",
              h4("C. Visualisasi Multivariat"),
              plotlyOutput("m1", height = "500px"), uiOutput("tm1"),
              plotlyOutput("m2", height = "800px"), uiOutput("tm2"),
              plotlyOutput("m3"), uiOutput("tm3"),
              plotlyOutput("m4"), uiOutput("tm4"))
        ),

        # Tab 6: regresi
        tabPanel("6. Regresi Linear Berganda",
          br(),
          div(class = "kartu",
              h4("A. Variabel dan Persamaan Model"),
              uiOutput("teks_variabel"),
              verbatimTextOutput("persamaan"),
              uiOutput("tafsir_persamaan")),
          div(class = "kartu",
              h4("B. Hasil Estimasi OLS"),
              verbatimTextOutput("ringkasan_model"),
              DTOutput("tabel_koef"),
              verbatimTextOutput("evaluasi_model"),
              uiOutput("tafsir_koef")),
          div(class = "kartu",
              h4("C. Uji Asumsi Klasik (Residual) & Interpretasi Variabel"),
              h5("1. Normalitas Residual"), verbatimTextOutput("out_norm"), uiOutput("tafsir_norm"),
              h5("2. Multikolinearitas (VIF)"), verbatimTextOutput("vif_out"), uiOutput("tafsir_vif"),
              h5("3. Heteroskedastisitas"), verbatimTextOutput("out_hetero"), uiOutput("tafsir_hetero"),
              h5("4. Autokorelasi"), verbatimTextOutput("out_auto"), uiOutput("tafsir_auto"),
              h5("Ringkasan Empat Uji Asumsi Klasik"), DTOutput("tabel_asumsi"), uiOutput("tafsir_asumsi"),
              h5("Variabel yang Berpengaruh terhadap Harga"), DTOutput("tabel_pengaruh"), uiOutput("tafsir_pengaruh"),
              h5("Penyebab Asumsi Tidak Terpenuhi"), uiOutput("sebab_asumsi"),
              h5("Upaya Mengatasi dan Hasilnya"), DTOutput("tabel_upaya"), uiOutput("tafsir_upaya")),
          div(class = "kartu",
              h4("D. Visualisasi Hasil Pemodelan dan Diagnostik"),
              plotlyOutput("r1"), uiOutput("tr1"),
              plotlyOutput("r2"), uiOutput("tr2"),
              plotlyOutput("r3"), uiOutput("tr3"),
              plotlyOutput("r4"), uiOutput("tr4"),
              plotlyOutput("r5"), uiOutput("tr5")),
          div(class = "kartu",
              h4("E. Keterbatasan Model"),
              uiOutput("teks_batas"))
        ),

        # Tab 7: kesimpulan
        tabPanel("7. Kesimpulan",
          br(),
          div(class = "kartu", h4("Kesimpulan dan Rekomendasi"), htmlOutput("teks_kesimpulan"))
        ),

        # Tab 8: unduh
        tabPanel("8. Unduh",
          br(),
          div(class = "kartu",
              h4("Unduh Hasil Analisis"),
              downloadButton("unduh_data", "Data Bersih (CSV)"),
              downloadButton("unduh_desk", "Statistik Deskriptif (CSV)"),
              downloadButton("unduh_reg", "Koefisien Regresi (CSV)"))
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {

  data_mentah <- reactive({
    if (file.exists(file_path)) {
      readr::read_csv(file_path, show_col_types = FALSE)
    } else if (exists("DATA_B64")) {
      txt <- memDecompress(jsonlite::base64_dec(paste(DATA_B64, collapse = "")), type = "gzip", asChar = TRUE)
      as_tibble(read.csv(text = txt, stringsAsFactors = FALSE))
    } else {
      validate(need(FALSE, "File jabodetabek_house_price.csv tidak ditemukan. Letakkan satu folder dengan app.R."))
    }
  })

  var5 <- c("price_in_rp", "land_size_m2", "building_size_m2", "city", "certificate")

  data_bersih <- reactive({
    d <- data_mentah()
    n0 <- nrow(d)

    d$city <- str_trim(d$city)
    sert <- str_to_lower(replace_na(d$certificate, ""))
    d$certificate <- case_when(
      str_detect(sert, "shm") ~ "SHM",
      str_detect(sert, "hgb") ~ "HGB",
      sert == ""              ~ "Tidak diketahui",
      TRUE                    ~ "Lainnya"
    )

    d <- d %>% filter(!is.na(land_size_m2), !is.na(building_size_m2), !is.na(price_in_rp), price_in_rp > 0)
    n1 <- nrow(d)

    d <- d %>% filter(land_size_m2 >= 10, land_size_m2 <= 2000, building_size_m2 >= 10, building_size_m2 <= 2000)
    n2 <- nrow(d)

    lp  <- log10(d$price_in_rp)
    q   <- quantile(lp, c(0.25, 0.75))
    iqr <- q[2] - q[1]
    d <- d %>% filter(lp >= q[1] - 1.5 * iqr, lp <= q[2] + 1.5 * iqr)
    n3 <- nrow(d)

    d$price_miliar <- d$price_in_rp / 1e9
    d$ln_price <- log(d$price_in_rp)
    d$ln_land  <- log(d$land_size_m2)
    d$ln_bld   <- log(d$building_size_m2)

    attr(d, "log") <- data.frame(
      Langkah       = c("Data awal", "Buang baris kosong", "Buang luas tidak wajar", "Buang pencilan harga (IQR log10)"),
      Baris_Tersisa = c(n0, n1, n2, n3),
      Baris_Dibuang = c(0, n0 - n1, n1 - n2, n2 - n3)
    )
    d
  })
  log_bersih <- reactive({ attr(data_bersih(), "log") })

  observe({
    d <- data_bersih()
    kota <- sort(unique(d$city)); sert <- sort(unique(d$certificate))
    pmin <- floor(min(d$price_miliar) * 10) / 10
    pmax <- ceiling(max(d$price_miliar))
    updateCheckboxGroupInput(session, "filter_kota", choices = kota, selected = kota)
    updateCheckboxGroupInput(session, "filter_sert", choices = sert, selected = sert)
    updateSliderInput(session, "filter_harga", min = pmin, max = pmax, value = c(pmin, pmax))
  })

  observeEvent(input$reset, {
    d <- data_bersih()
    updateCheckboxGroupInput(session, "filter_kota", selected = sort(unique(d$city)))
    updateCheckboxGroupInput(session, "filter_sert", selected = sort(unique(d$certificate)))
    updateSliderInput(session, "filter_harga", value = c(floor(min(d$price_miliar) * 10) / 10, ceiling(max(d$price_miliar))))
  })

  data_f <- reactive({
    data_bersih()
    validate(need(length(input$filter_kota) > 0, "Pilih minimal satu kota."),
             need(length(input$filter_sert) > 0, "Pilih minimal satu jenis sertifikat."))
    d <- data_bersih() %>%
      filter(city %in% input$filter_kota,
             certificate %in% input$filter_sert,
             price_miliar >= input$filter_harga[1],
             price_miliar <= input$filter_harga[2])
    validate(need(nrow(d) > 30, "Data terlalu sedikit. Longgarkan filter."))
    d
  })

  output$n_obs <- renderText({ comma(nrow(data_f())) })

  # Tab 1
  output$teks_sumber <- renderUI({
    lg <- log_bersih()
    tags$ul(
      tags$li(paste0("Observasi awal: ", comma(lg$Baris_Tersisa[1]), " baris; setelah dibersihkan: ", comma(lg$Baris_Tersisa[4]), " baris.")),
      tags$li(paste0("Variabel pada file data: ", ncol(data_mentah()), "; dipakai: 5 (3 numerik, 2 kategorik).")))
  })

  output$tabel_definisi <- renderDT({
    datatable(data.frame(
      Variabel = var5,
      Tipe     = c("Numerik (respons)", "Numerik (prediktor)", "Numerik (prediktor)", "Kategorik (prediktor)", "Kategorik (prediktor)"),
      Satuan   = c("Rupiah", "m²", "m²", "Wilayah", "Status hukum"),
      Definisi = c("Harga penawaran rumah", "Luas tanah", "Luas bangunan", "Kota/kabupaten lokasi rumah", "Jenis sertifikat (SHM, HGB, Lainnya, Tidak diketahui)")
    ), options = list(dom = "t"), rownames = FALSE)
  })

  output$tabel_cuplikan <- renderDT({
    d <- data_f() %>% select(price_miliar, land_size_m2, building_size_m2, city, certificate)
    datatable(head(d, 50), options = list(pageLength = 5, scrollX = TRUE), rownames = FALSE)
  })

  # Tab 2: Pengolahan Data (Missing Value & Duplikat)
  output$tabel_log <- renderDT({
    datatable(log_bersih(), options = list(dom = "t"), rownames = FALSE)
  })

  output$tabel_hilang <- renderDT({
    mentah <- data_mentah()
    hilang <- sapply(mentah[var5], function(x) sum(is.na(x) | (is.character(x) & trimws(x) == "")))
    tb <- data.frame(
      Variabel = var5,
      Jumlah_Missing_Value = as.numeric(hilang),
      row.names = NULL
    )
    tb$Persentase_Missing <- round(tb$Jumlah_Missing_Value / nrow(mentah) * 100, 2)
    tb$Status <- ifelse(tb$Jumlah_Missing_Value > 0, "Terdapat Missing Value", "Bersih (Tidak Ada)")
    datatable(tb, options = list(dom = "t"), rownames = FALSE)
  })

  output$teks_hilang <- renderUI({
    mentah <- data_mentah()
    hilang <- sapply(mentah[var5], function(x) sum(is.na(x) | (is.character(x) & trimws(x) == "")))
    total_m <- sum(hilang)
    p(paste0("Penjelasan Missing Value: Berdasarkan pemeriksaan pada data mentah (total ", comma(nrow(mentah)), 
             " baris), terdapat total ", comma(total_m), " nilai kosong (missing value) secara keseluruhan di antara kelima variabel yang dianalisis. ",
             "Variabel seperti harga, luas tanah, dan luas bangunan umumnya bersih, sedangkan kolom keterangan sertifikat atau wilayah tertentu memiliki sel kosong yang langsung ditangani pada tahapan pembersihan data dengan mengisinya atau memfilternya agar tidak mengganggu proses pemodelan."))
  })

  output$teks_duplikat <- renderUI({
    mentah <- data_mentah()
    dup <- sum(duplicated(mentah[var5]))
    p(paste0("Catatan Duplikat: Ada ", comma(dup), " baris (", round(dup / nrow(mentah) * 100, 1),
             "%) dengan kelima variabel yang persis sama. Baris ini tidak dihapus karena data tidak punya kolom pembeda (misalnya ID iklan) untuk memastikan apakah itu iklan ganda atau unit berbeda dengan spesifikasi sama (misalnya rumah kembar di satu perumahan). Hal ini dicatat sebagai keterbatasan."))
  })

  # Tab 3: Univariat
  output$tabel_desk <- renderDT({
    d <- data_f()
    x <- d %>% select(price_miliar, land_size_m2, building_size_m2)
    tb <- data.frame(
      Variabel = c("Harga (Miliar Rp)", "Luas Tanah (m²)", "Luas Bangunan (m²)"),
      Mean     = sapply(x, mean),
      Median   = sapply(x, median),
      Std_Dev  = sapply(x, sd),
      Varians  = sapply(x, var),
      Min      = sapply(x, min),
      Q1       = sapply(x, quantile, probs = 0.25),
      Q3       = sapply(x, quantile, probs = 0.75),
      Max      = sapply(x, max),
      Range    = sapply(x, function(v) max(v) - min(v)),
      IQR      = sapply(x, IQR),
      Skewness = sapply(x, skew),
      Kurtosis = sapply(x, kurt),
      Pencilan = sapply(x, n_pencilan),
      row.names = NULL
    )
    tb[, -1] <- round(tb[, -1], 2)
    datatable(tb, options = list(dom = "t", scrollX = TRUE), rownames = FALSE)
  })

  output$tafsir_desk <- renderUI({
    d <- data_f()
    s <- skew(d$price_miliar)
    kotak("Statistik Deskriptif",
          paste0("Rata-rata harga Rp", round(mean(d$price_miliar), 2), " miliar lebih besar daripada median Rp", round(median(d$price_miliar), 2),
                 " miliar dengan skewness ", round(s, 2), ", sehingga distribusi harga ", bentuk(s),
                 ". Hal yang sama berlaku untuk luas tanah (skewness ", round(skew(d$land_size_m2), 2), ") dan luas bangunan (skewness ",
                 round(skew(d$building_size_m2), 2), "): sebagian kecil rumah berukuran dan berharga sangat besar menarik rata-rata ke atas."))
  })

  output$tabel_frek_kota <- renderDT({
    tb <- data_f() %>% count(city, name = "Frekuensi") %>%
      mutate(Persentase = round(Frekuensi / sum(Frekuensi) * 100, 2)) %>% arrange(desc(Frekuensi))
    datatable(tb, options = list(dom = "t", pageLength = 10), rownames = FALSE)
  })
  output$tafsir_frek_kota <- renderUI({
    tb <- data_f() %>% count(city, sort = TRUE)
    kotak("Frekuensi Kota",
          paste0(tb$city[1], " memiliki iklan terbanyak (", comma(tb$n[1]), " rumah, ", round(tb$n[1] / sum(tb$n) * 100, 1),
                 "%), sedangkan ", tb$city[nrow(tb)], " paling sedikit (", comma(tb$n[nrow(tb)]), " rumah)."))
  })

  output$tabel_frek_sert <- renderDT({
    tb <- data_f() %>% count(certificate, name = "Frekuensi") %>%
      mutate(Persentase = round(Frekuensi / sum(Frekuensi) * 100, 2)) %>% arrange(desc(Frekuensi))
    datatable(tb, options = list(dom = "t"), rownames = FALSE)
  })
  output$tafsir_frek_sert <- renderUI({
    tb <- data_f() %>% count(certificate, sort = TRUE)
    kotak("Frekuensi Sertifikat",
          paste0("Sertifikat ", tb$certificate[1], " mendominasi (", round(tb$n[1] / sum(tb$n) * 100, 1), "% data)."))
  })

  # U1
  output$u1 <- renderPlotly({
    p <- ggplot(data_f(), aes(x = price_miliar)) +
      geom_histogram(bins = 40, fill = "#2563eb", color = "white") +
      labs(title = "1. Histogram Harga Rumah", x = "Harga (Miliar Rp)", y = "Frekuensi") + theme_minimal()
    ggplotly(p)
  })
  output$tu1 <- renderUI({
    s <- data_f()$price_miliar
    sk <- round(skew(s), 2)
    kotak("Histogram Harga",
          paste0("Berdasarkan grafik di atas, histogram ini digunakan untuk melihat bentuk sebaran data harga rumah. Visualisasi ini menunjukkan bahwa distribusi harga miring ke kanan (skewness = ", sk, "), di mana sebagian besar rumah berharga rendah hingga menengah dengan ekor panjang ke harga tinggi, yang artinya variasi harga didominasi oleh properti terjangkau sementara rumah mewah jumlahnya relatif sedikit."))
  })

  # U2
  output$u2 <- renderPlotly({
    plot_ly(data_f(), x = ~price_miliar, type = "box", name = "Harga",
            fillcolor = "#f59e0b", line = list(color = "#b45309"),
            marker = list(color = "#dc2626", size = 4),
            boxpoints = "outliers") %>%
      layout(title = "2. Boxplot Harga Rumah",
             xaxis = list(title = "Harga (Miliar Rp)"),
             yaxis = list(title = "", showticklabels = FALSE),
             showlegend = FALSE)
  })
  output$tu2 <- renderUI({
    s <- data_f()$price_miliar
    med <- round(median(s), 2)
    batas <- round(quantile(s, 0.75) + 1.5 * IQR(s), 2)
    n_out <- n_pencilan(s)
    p_out <- round(n_out / length(s) * 100, 1)
    kotak("Boxplot Harga",
          paste0("Berdasarkan grafik di atas, boxplot ini digunakan untuk meringkas nilai median dan kuartil sekaligus mengidentifikasi adanya pencilan. Visualisasi ini menunjukkan median harga sebesar Rp", med, " miliar dengan batas atas pencilan Rp", batas, " miliar dan terdapat ", n_out, " rumah (", p_out, "%) di atas batas tersebut, yang artinya sebagian besar data terpusat di bawah namun terdapat sejumlah harga tinggi yang masih wajar."))
  })

  # U3
  output$u3 <- renderPlotly({
    p <- ggplot(data_f(), aes(x = building_size_m2)) +
      geom_histogram(aes(y = after_stat(density)), bins = 40, fill = "#059669", color = "white") +
      geom_density(color = "#dc2626", linewidth = 1) +
      labs(title = "3. Histogram dan Density Plot Luas Bangunan", x = "Luas Bangunan (m²)", y = "Densitas") + theme_minimal()
    ggplotly(p)
  })
  output$tu3 <- renderUI({
    s <- data_f()$building_size_m2
    kotak("Luas Bangunan",
          paste0("Berdasarkan grafik di atas, histogram dan density plot ini digunakan untuk melihat sebaran ukuran fisik bangunan rumah. Visualisasi ini menunjukkan median luas bangunan sebesar ", round(median(s)), " m² dengan bentuk distribusi miring ke kanan, yang artinya sebagian besar rumah di Jabodetabek memiliki ukuran bangunan yang tidak terlalu besar."))
  })

  # U4
  output$u4 <- renderPlotly({
    p <- ggplot(data_f(), aes(x = land_size_m2)) +
      geom_histogram(aes(y = after_stat(density)), bins = 40, fill = "#0284c7", color = "white") +
      geom_density(color = "#dc2626", linewidth = 1) +
      labs(title = "4. Histogram dan Density Plot Luas Tanah", x = "Luas Tanah (m²)", y = "Densitas") + theme_minimal()
    ggplotly(p)
  })
  output$tu4 <- renderUI({
    s <- data_f()$land_size_m2
    kotak("Luas Tanah",
          paste0("Berdasarkan grafik di atas, plot ini digunakan untuk melihat karakteristik distribusi luas tanah secara univariat. Visualisasi ini menunjukkan median luas tanah sebesar ", round(median(s)), " m² dengan pola miring ke kanan, yang artinya luas tanah sebagian besar properti berukuran kompak dan mendasari penggunaan transformasi ln pada analisis regresi."))
  })

  # U5
  output$u5 <- renderPlotly({
    p <- ggplot(data_f(), aes(x = fct_infreq(city))) +
      geom_bar(fill = "#dc2626") +
      labs(title = "5. Diagram Batang Frekuensi Kota", x = "Kota", y = "Jumlah Rumah") +
      theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p)
  })
  output$tu5 <- renderUI({
    tb <- data_f() %>% count(city, sort = TRUE)
    kotak("Diagram Batang Kota",
          paste0("Berdasarkan grafik di atas, diagram batang ini digunakan untuk membandingkan jumlah frekuensi iklan rumah di setiap wilayah. Visualisasi ini menunjukkan bahwa ", tb$city[1], " memiliki jumlah iklan terbanyak (", comma(tb$n[1]), " rumah) dan ", tb$city[nrow(tb)], " paling sedikit (", comma(tb$n[nrow(tb)]), " rumah), yang artinya konsentrasi penawaran rumah sangat bervariasi antar wilayah."))
  })

  # U6
  output$u6 <- renderPlotly({
    tb <- data_f() %>% count(certificate)
    plot_ly(tb, labels = ~certificate, values = ~n, type = "pie", textinfo = "label+percent") %>%
      layout(title = "6. Proporsi Jenis Sertifikat")
  })
  output$tu6 <- renderUI({
    tb <- data_f() %>% count(certificate, sort = TRUE)
    pct <- round(tb$n[1] / sum(tb$n) * 100, 1)
    kotak("Pie Chart Sertifikat",
          paste0("Berdasarkan grafik di atas, pie chart ini digunakan untuk menampilkan proporsi status hukum kepemilikan properti. Visualisasi ini menunjukkan bahwa sertifikat ", tb$certificate[1], " mendominasi pasar sebesar ", pct, "%, yang artinya mayoritas rumah di Jabodetabek menggunakan jenis kepemilikan tersebut."))
  })

  # Tab 4: Bivariat
  output$tabel_korelasi <- renderDT({
    d <- data_f()
    tb <- data.frame(
      Pasangan = c("Harga vs Luas Tanah", "Harga vs Luas Bangunan", "Luas Tanah vs Luas Bangunan"),
      Pearson_skala_asli = c(cor(d$price_miliar, d$land_size_m2), cor(d$price_miliar, d$building_size_m2), cor(d$land_size_m2, d$building_size_m2)),
      Pearson_skala_ln   = c(cor(d$ln_price, d$ln_land), cor(d$ln_price, d$ln_bld), cor(d$ln_land, d$ln_bld)),
      Spearman = c(cor(d$price_miliar, d$land_size_m2, method = "spearman"),
                   cor(d$price_miliar, d$building_size_m2, method = "spearman"),
                   cor(d$land_size_m2, d$building_size_m2, method = "spearman"))
    )
    tb[, -1] <- round(tb[, -1], 3)
    datatable(tb, options = list(dom = "t"), rownames = FALSE)
  })
  output$tafsir_korelasi <- renderUI({
    d <- data_f()
    kotak("Korelasi",
          paste0("Pearson mengukur hubungan linear dan Spearman hubungan monoton (berbasis peringkat). Harga berkorelasi ",
                 kekuatan(cor(d$ln_price, d$ln_bld)), " dengan luas bangunan (r ln = ", round(cor(d$ln_price, d$ln_bld), 2),
                 ") dan ", kekuatan(cor(d$ln_price, d$ln_land)), " dengan luas tanah (r ln = ", round(cor(d$ln_price, d$ln_land), 2),
                 "). Korelasi harga-luas bangunan pada skala ln (", round(cor(d$ln_price, d$ln_bld), 2), ") ",
                 ifelse(cor(d$ln_price, d$ln_bld) > cor(d$price_miliar, d$building_size_m2), "lebih tinggi", "tidak lebih tinggi"),
                 " daripada skala asli (", round(cor(d$price_miliar, d$building_size_m2), 2), "); jika lebih tinggi, hubungan lebih linear setelah log sehingga log dipakai di grafik dan model."))
  })

  # B1
  output$b1 <- renderPlotly({
    p <- ggplot(data_f(), aes(x = building_size_m2, y = price_miliar)) +
      geom_point(alpha = 0.4, color = "#2563eb") +
      geom_smooth(method = "lm", formula = y ~ x, color = "#dc2626") +
      scale_x_log10() + scale_y_log10() +
      labs(title = "1. Scatterplot Harga vs Luas Bangunan (skala log)", x = "Luas Bangunan (m², log)", y = "Harga (Miliar Rp, log)") +
      theme_minimal()
    ggplotly(p)
  })
  output$tb1 <- renderUI({
    d <- data_f(); r <- cor(d$ln_price, d$ln_bld)
    kotak("Harga vs Luas Bangunan",
          paste0("Berdasarkan grafik di atas, scatterplot ini digunakan untuk melihat hubungan antara harga dan luas bangunan dalam skala logaritma. Visualisasi ini menunjukkan pola titik yang menaik ke kanan atas dengan korelasi ", kekuatan(r), " (r = ", round(r, 2), "), yang artinya semakin luas bangunan suatu rumah, maka harga penawarannya cenderung semakin tinggi."))
  })

  # B2
  output$b2 <- renderPlotly({
    p <- ggplot(data_f(), aes(x = land_size_m2, y = price_miliar)) +
      geom_point(alpha = 0.4, color = "#0f172a") +
      geom_smooth(method = "lm", formula = y ~ x, color = "#dc2626") +
      scale_x_log10() + scale_y_log10() +
      labs(title = "2. Scatterplot Harga vs Luas Tanah (skala log)", x = "Luas Tanah (m², log)", y = "Harga (Miliar Rp, log)") +
      theme_minimal()
    ggplotly(p)
  })
  output$tb2 <- renderUI({
    d <- data_f(); r <- cor(d$ln_price, d$ln_land)
    kotak("Harga vs Luas Tanah",
          paste0("Berdasarkan grafik di atas, scatterplot ini digunakan untuk melihat keterkaitan antara harga dan luas tanah properti. Visualisasi ini menunjukkan hubungan yang ", kekuatan(r), " (r = ", round(r, 2), ") dengan sebaran titik yang cukup melebar, yang artinya luas tanah saja belum sepenuhnya menentukan harga karena faktor lokasi turut memegang peranan penting."))
  })

  # B3
  output$b3 <- renderPlotly({
    cm <- cor(data_f()[, c("ln_price", "ln_land", "ln_bld")])
    nama <- c("ln(Harga)", "ln(Luas Tanah)", "ln(Luas Bangunan)")
    rownames(cm) <- colnames(cm) <- nama
    df <- as.data.frame(as.table(cm))
    p <- ggplot(df, aes(x = Var1, y = Var2, fill = Freq)) +
      geom_tile(color = "white") +
      geom_text(aes(label = round(Freq, 2))) +
      scale_fill_gradient2(low = "blue", mid = "white", high = "red", limits = c(-1, 1)) +
      labs(title = "3. Plot Korelasi Variabel Numerik (skala ln)", x = "", y = "", fill = "Korelasi") + theme_minimal()
    ggplotly(p)
  })
  output$tb3 <- renderUI({
    d <- data_f()
    r_lb <- round(cor(d$ln_land, d$ln_bld), 2)
    kotak("Plot Korelasi",
          paste0("Berdasarkan grafik di atas, heatmap korelasi ini digunakan untuk merangkum keeratan hubungan antar seluruh variabel numerik. Visualisasi ini menunjukkan korelasi antara luas tanah dan luas bangunan sebesar r = ", r_lb, ", yang artinya kedua variabel prediktor ini saling berkaitan erat dan perlu diuji multikolinearitasnya dalam model regresi."))
  })

  # B4
  output$b4 <- renderPlotly({
    p <- ggplot(data_f(), aes(x = reorder(city, price_miliar, FUN = median), y = price_miliar)) +
      geom_violin(fill = "cornflowerblue") +
      geom_boxplot(width = 0.15, fill = "orange", outlier.size = 1) +
      scale_y_log10() +
      labs(title = "4. Violin dan Boxplot Harga per Kota", x = "Kota", y = "Harga (Miliar Rp, log)") +
      theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p)
  })
  output$tb4 <- renderUI({
    md <- data_f() %>% group_by(city) %>% summarise(m = median(price_miliar)) %>% arrange(m)
    kotak("Harga per Kota",
          paste0("Berdasarkan grafik di atas, gabungan violin dan boxplot ini digunakan untuk membandingkan distribusi harga rumah antar wilayah kota/kabupaten. Visualisasi ini menunjukkan median harga tertinggi berada di ", md$city[nrow(md)], " (Rp", round(md$m[nrow(md)], 2), " miliar) dan terendah di ", md$city[1], " (Rp", round(md$m[1], 1), " miliar), yang artinya faktor lokasi kota membedakan tingkat harga secara signifikan."))
  })

  # B5
  output$b5 <- renderPlotly({
    tb <- data_f() %>% group_by(certificate) %>%
       summarise(n = n(), rata = mean(price_miliar), se = sd(price_miliar) / sqrt(n()))
    p <- ggplot(tb, aes(x = certificate, y = rata)) +
      geom_point(size = 3, color = "#2563eb") +
      geom_errorbar(aes(ymin = rata - se, ymax = rata + se), width = 0.15) +
      labs(title = "5. Rata-rata Harga (± SE) menurut Sertifikat", x = "Jenis Sertifikat", y = "Rata-rata Harga (Miliar Rp)") +
      theme_minimal()
    ggplotly(p)
  })
  output$tb5 <- renderUI({
    tb <- data_f() %>% group_by(certificate) %>% summarise(rata = mean(price_miliar)) %>% arrange(rata)
    kotak("Harga per Sertifikat",
          paste0("Berdasarkan grafik di atas, error bar plot ini digunakan untuk membandingkan rata-rata harga rumah berdasarkan jenis sertifikat beserta tingkat ketidakpastiannya. Visualisasi ini menunjukkan rata-rata harga tertinggi pada sertifikat ", tb$certificate[nrow(tb)], " dan terendah pada ", tb$certificate[1], ", yang artinya status legalitas hukum kepemilikan memberikan variasi terhadap nilai rata-rata properti."))
  })

  # B6
  output$b6 <- renderPlotly({
    p <- ggplot(data_f(), aes(x = city, fill = certificate)) +
      geom_bar(position = "fill") +
      scale_y_continuous(labels = percent) +
      labs(title = "6. Komposisi Sertifikat per Kota (Segmented Bar)", x = "Kota", y = "Persentase", fill = "Sertifikat") +
      theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p)
  })
  output$tb6 <- renderUI({
    tb <- table(data_f()$city, data_f()$certificate)
    if (nrow(tb) < 2 || ncol(tb) < 2) {
      return(kotak("Kota x Sertifikat", "Filter saat ini hanya menyisakan satu kota atau satu jenis sertifikat, sehingga hubungan belum bisa diuji."))
    }
    uji <- suppressWarnings(chisq.test(tb))
    v <- sqrt(unname(uji$statistic) / (sum(tb) * (min(dim(tb)) - 1)))
    kotak("Kota x Sertifikat",
          paste0("Berdasarkan grafik di atas, segmented bar chart ini digunakan untuk melihat komposisi proporsi jenis sertifikat di setiap wilayah kota. Visualisasi ini menunjukkan hasil uji Chi-Square dengan p-value ", p_txt(uji$p.value), " dan nilai Cramér's V = ", round(v, 2), ", yang artinya terdapat keterkaitan antara wilayah kota dan jenis sertifikat kepemilikan rumah."))
  })

  # Tab 5: Multivariat
  output$tabel_multi <- renderDT({
    x <- data_f() %>% select(price_miliar, land_size_m2, building_size_m2)
    kor <- round(cor(x), 3)
    tb <- data.frame(
      Variabel = c("Harga (Miliar Rp)", "Luas Tanah (m²)", "Luas Bangunan (m²)"),
      Rata_rata = round(colMeans(x), 2),
      Std_Dev = round(sapply(x, sd), 2),
      r_Harga = kor[, 1], r_Luas_Tanah = kor[, 2], r_Luas_Bangunan = kor[, 3],
      row.names = NULL)
    datatable(tb, options = list(dom = "t", scrollX = TRUE), rownames = FALSE)
  })
  output$tafsir_multi <- renderUI({
    d <- data_f()
    kotak("Ringkasan Multivariat",
          paste0("Tabel merangkum vektor rata-rata dan matriks korelasi tiga variabel numerik sekaligus. Harga paling kuat berkorelasi dengan luas bangunan (r = ",
                 round(cor(d$price_miliar, d$building_size_m2), 2), "), lalu luas tanah (r = ", round(cor(d$price_miliar, d$land_size_m2), 2), ")."))
  })

  mahal <- reactive({
    d <- data_f()
    x <- d[, c("ln_price", "ln_land", "ln_bld")]
    d2 <- mahalanobis(x, center = colMeans(x), cov = cov(x))
    cutoff <- qchisq(0.975, df = ncol(x))
    n <- length(d2)
    kuantil <- qchisq(ppoints(n), df = ncol(x))
    list(d2 = d2, cutoff = cutoff, outlier = d2 > cutoff,
         kuantil = kuantil, rq = cor(sort(d2), kuantil))
  })

  output$teks_mahal <- renderUI({
    m <- mahal()
    tags$ul(
      tags$li("Pencilan univariat hanya melihat satu variabel. Jarak Mahalanobis melihat ketiga variabel bersama-sama (ln harga, ln luas tanah, ln luas bangunan)."),
      tags$li(paste0("Batas pencilan: kuantil chi-square 0,975 dengan db 3 = ", round(m$cutoff, 2), ". Pengamatan dengan D² di atas batas ini adalah pencilan multivariat.")),
      tags$li(paste0("Hasil: ", sum(m$outlier), " dari ", comma(length(m$d2)), " rumah (", round(mean(m$outlier) * 100, 1), "%) adalah pencilan multivariat. Rumah ini tidak dibuang; ditandai untuk dicermati.")),
      tags$li(paste0("Normalitas multivariat (pendekatan korelasi Pearson pada Q-Q plot chi-square): rQ = ", round(m$rq, 4),
                     ". Makin dekat 1, makin lurus Q-Q plot. Pada n besar, uji formal hampir selalu menolak normalitas, sehingga kesimpulan dilihat bersama grafik Q-Q.")),
      tags$li("Grafik pendukung ada di bagian C di bawah: plot 3 (pencilan multivariat) dan plot 4 (Q-Q plot chi-square)."))
  })

  # M1
  output$m1 <- renderPlotly({
    p <- ggplot(data_f(), aes(x = land_size_m2, y = building_size_m2, color = city, size = price_miliar)) +
      geom_point(alpha = 0.6) +
      scale_x_log10() + scale_y_log10() +
      labs(title = "1. Bubble Plot: Luas Tanah, Luas Bangunan, Kota, dan Harga",
           x = "Luas Tanah (m², log)", y = "Luas Bangunan (m², log)", color = "Kota", size = "Harga (Miliar Rp)") +
      theme_minimal()
    ggplotly(p)
  })
  output$tm1 <- renderUI({
    d <- data_f()
    r_pb <- round(cor(d$ln_price, d$ln_bld), 2)
    kotak("Bubble Plot",
          paste0("Berdasarkan grafik di atas, bubble plot ini digunakan untuk menampilkan empat variabel secara simultan (luas tanah, luas bangunan, kota, dan harga). Visualisasi ini menunjukkan bahwa gelembung ukuran besar (harga tinggi) cenderung terkonsentrasi di sisi kanan atas sejalan dengan tingginya korelasi antara luas bangunan dan harga (r = ", r_pb, "), yang artinya properti berukuran besar di kota tertentu memiliki valuasi harga yang jauh lebih tinggi."))
  })

  # M2
  output$m2 <- renderPlotly({
    p <- ggplot(data_f(), aes(x = building_size_m2, y = price_miliar, color = certificate)) +
      geom_point(alpha = 0.4) +
      facet_wrap(~city) +
      scale_x_log10() + scale_y_log10() +
      labs(title = "2. Harga vs Luas Bangunan per Kota, Diwarnai Sertifikat",
           x = "Luas Bangunan (m², log)", y = "Harga (Miliar Rp, log)", color = "Sertifikat") +
      theme_minimal()
    ggplotly(p)
  })
  output$tm2 <- renderUI({
    pp <- data_f() %>% group_by(city) %>% summarise(m = median(price_miliar * 1000 / building_size_m2)) %>% arrange(m)
    kotak("Facet Plot",
          paste0("Berdasarkan grafik di atas, facet plot ini digunakan untuk memecah pola hubungan harga dan luas bangunan di tiap wilayah kota secara terpisah. Visualisasi ini menunjukkan konsistensi hubungan positif di berbagai kota dengan median harga per meter persegi tertinggi di ", pp$city[nrow(pp)], " (Rp", round(pp$m[nrow(pp)], 1), " juta) dan terendah di ", pp$city[1], " (Rp", round(pp$m[1], 1), " juta), yang artinya karakteristik pertumbuhan harga antar wilayah memiliki standar nilai yang berbeda-beda."))
  })

  # M3
  output$m3 <- renderPlotly({
    d <- data_f(); m <- mahal()
    d$Status <- ifelse(m$outlier, "Pencilan multivariat", "Normal")
    p <- ggplot(d, aes(x = building_size_m2, y = price_miliar, color = Status)) +
      geom_point(alpha = 0.5) +
      scale_x_log10() + scale_y_log10() +
      scale_color_manual(values = c("Normal" = "grey60", "Pencilan multivariat" = "red")) +
      labs(title = "3. Pencilan Multivariat (Jarak Mahalanobis)", x = "Luas Bangunan (m², log)", y = "Harga (Miliar Rp, log)") +
      theme_minimal()
    ggplotly(p)
  })
  output$tm3 <- renderUI({
    m <- mahal()
    n_out <- sum(m$outlier)
    pct_out <- round(mean(m$outlier) * 100, 1)
    kotak("Pencilan Mahalanobis",
          paste0("Berdasarkan grafik di atas, plot ini digunakan untuk mendeteksi pencilan secara multivariat menggunakan Jarak Mahalanobis. Visualisasi ini menunjukkan adanya ", n_out, " titik merah (", pct_out, "%) yang berada di atas batas kritis chi-square (D² > ", round(m$cutoff, 2), "), yang artinya kombinasi antara harga, luas tanah, dan luas bangunan pada titik-titik tersebut tidak lazim dibandingkan mayoritas data lainnya."))
  })

  # M4
  output$m4 <- renderPlotly({
    m <- mahal()
    df <- data.frame(kuantil = m$kuantil, d2 = sort(m$d2))
    p <- ggplot(df, aes(x = kuantil, y = d2)) +
      geom_point(alpha = 0.5, color = "#2563eb") +
      geom_abline(slope = 1, intercept = 0, color = "red") +
      labs(title = "4. Q-Q Plot Chi-Square (Normalitas Multivariat)",
           x = "Kuantil Teoritis Chi-Square", y = "Jarak Mahalanobis (D²) Terurut") +
      theme_minimal()
    ggplotly(p)
  })
  output$tm4 <- renderUI({
    m <- mahal()
    kotak("Q-Q Plot Chi-Square",
          paste0("Berdasarkan grafik di atas, Q-Q plot chi-square ini digunakan untuk menguji asumsi normalitas multivariat data. Visualisasi ini menunjukkan nilai korelasi titik dengan kuantil teoritis sebesar rQ = ", round(m$rq, 4), " di mana titik-titik mulai menjauhi garis referensi pada bagian ekor kanan, yang artinya sebaran data multivariat memiliki ekor yang lebih berat dari distribusi normal baku."))
  })

  # Tab 6: Regresi Linear Berganda
  model <- reactive({
    d <- data_f()
    validate(need(n_distinct(d$city) >= 2, "Regresi butuh minimal 2 kota. Longgarkan filter."),
             need(n_distinct(d$certificate) >= 2, "Regresi butuh minimal 2 jenis sertifikat. Longgarkan filter."))
    d$city <- factor(d$city)
    d$certificate <- factor(d$certificate)
    d$city <- relevel(d$city, ref = names(sort(table(d$city), decreasing = TRUE))[1])
    acuan <- if ("SHM" %in% levels(d$certificate)) "SHM" else names(sort(table(d$certificate), decreasing = TRUE))[1]
    d$certificate <- relevel(d$certificate, ref = acuan)
    lm(ln_price ~ ln_land + ln_bld + city + certificate, data = d)
  })

  nama_koef <- function(n, m) {
    kota0 <- m$xlevels$city[1]; sert0 <- m$xlevels$certificate[1]
    sapply(n, function(x) {
      if (x == "(Intercept)") "Intercept"
      else if (x == "ln_land") "ln(Luas tanah)"
      else if (x == "ln_bld") "ln(Luas bangunan)"
      else if (startsWith(x, "city")) paste0("Kota: ", sub("^city", "", x), " (vs ", kota0, ")")
      else paste0("Sertifikat: ", sub("^certificate", "", x), " (vs ", sert0, ")")
    }, USE.NAMES = FALSE)
  }

  output$teks_variabel <- renderUI({
    m <- model()
    tags$ul(
      tags$li(strong("Respons (Y):"), " ln(Harga). Harga miring ke kanan; log menstabilkan ragam dan koefisien dibaca sebagai persentase."),
      tags$li(strong("Prediktor:"), " ln(Luas tanah), ln(Luas bangunan), Kota (dummy), Sertifikat (dummy)."),
      tags$li(strong("Kategori acuan:"), paste0(" Kota = ", m$xlevels$city[1], ", Sertifikat = ", m$xlevels$certificate[1],
                                              ". Koefisien dummy adalah selisih terhadap kategori acuan.")),
      tags$li(strong("Model:"), " ln(Harga) = β0 + β1 ln(Tanah) + β2 ln(Bangunan) + Σ γ Kota + Σ δ Sertifikat + ε"))
  })

  output$persamaan <- renderPrint({
    m <- model(); b <- coef(m); nm <- nama_koef(names(b), m)
    cat(sprintf("ln(Harga) = %.3f\n", b[1]))
    for (i in 2:length(b)) cat(sprintf("   %s %.3f x [%s]\n", ifelse(b[i] >= 0, "+", "-"), abs(b[i]), nm[i]))
  })

  output$ringkasan_model <- renderPrint({ summary(model()) })

  output$tabel_koef <- renderDT({
    m <- model(); cf <- summary(m)$coefficients
    tb <- data.frame(
      Variabel  = nama_koef(rownames(cf), m),
      Koefisien = round(cf[, 1], 4),
      Std_Error = round(cf[, 2], 4),
      t_hitung  = round(cf[, 3], 2),
      p_value   = signif(cf[, 4], 3),
      Efek_Harga_Persen = round(ifelse(grepl("^city|^certificate", rownames(cf)), (exp(cf[, 1]) - 1) * 100, NA), 1),
      row.names = NULL)
    datatable(tb, options = list(dom = "t", pageLength = 20, scrollX = TRUE), rownames = FALSE)
  })

  output$evaluasi_model <- renderPrint({
    m <- model(); s <- summary(m)
    rmse <- sqrt(mean(residuals(m)^2))
    pF <- pf(s$fstatistic[1], s$fstatistic[2], s$fstatistic[3], lower.tail = FALSE)
    cat(sprintf("R2 = %.3f | R2 adjusted = %.3f\n", s$r.squared, s$adj.r.squared))
    cat(sprintf("RMSE (skala ln) = %.3f, kira-kira galat prediksi harga %.0f%%\n", rmse, (exp(rmse) - 1) * 100))
    cat(sprintf("Uji simultan F = %.1f (db1 = %d, db2 = %d), %s\n", s$fstatistic[1], s$fstatistic[2], s$fstatistic[3], p_txt(pF)))
  })

  output$tafsir_koef <- renderUI({
    m <- model(); s <- summary(m); cf <- s$coefficients
    pF <- pf(s$fstatistic[1], s$fstatistic[2], s$fstatistic[3], lower.tail = FALSE)
    n_sig <- sum(cf[-1, 4] < 0.05)
    kotak("Koefisien dan Pengujian",
          paste0("Karena model log-log, koefisien prediktor numerik adalah elastisitas: luas bangunan naik 1% menaikkan harga sekitar ",
                 round(cf["ln_bld", 1], 2), "% (", p_txt(cf["ln_bld", 4]), ") dan luas tanah naik 1% menaikkan harga sekitar ",
                 round(cf["ln_land", 1], 2), "% (", p_txt(cf["ln_land", 4]), "), dengan prediktor lain tetap. ",
                 "Uji F (simultan) ", p_txt(pF), ifelse(pF < 0.05, ": minimal satu prediktor berpengaruh nyata. ", ": tidak ada prediktor berpengaruh nyata. "),
                 "Uji t (parsial): ", n_sig, " dari ", nrow(cf) - 1, " koefisien signifikan pada α = 5%. ",
                 "Koefisien dummy dibaca sebagai selisih harga terhadap kategori acuan (kolom Efek_Harga_Persen). ",
                 "R² = ", round(s$r.squared, 3), " berarti model menjelaskan ", round(s$r.squared * 100, 1), "% variasi ln(harga)."))
  })

  asumsi <- reactive({
    m <- model(); d <- data_f()
    res <- residuals(m)
    sw <- shapiro.test(res)$p.value
    ks <- suppressWarnings(ks.test(res, "pnorm", mean(res), sd(res))$p.value)
    bp <- bptest(m)$p.value
    g  <- summary(lm(abs(res) ~ ln_land + ln_bld + city + certificate, data = d))
    gl <- pf(g$fstatistic[1], g$fstatistic[2], g$fstatistic[3], lower.tail = FALSE)
    dw <- dwtest(m, alternative = "two.sided")
    list(sw = sw, ks = ks, bp = bp, gl = gl, dw_stat = unname(dw$statistic), dw_p = dw$p.value,
         normal_ok = min(sw, ks) >= 0.05, homo_ok = min(bp, gl) >= 0.05)
  })

  output$vif_out <- renderPrint({
    cat("Multikolinearitas (VIF; untuk variabel kategorik dipakai GVIF):\n")
    print(vif(model()))
  })

  # Interpretasi persamaan model
  output$tafsir_persamaan <- renderUI({ m <- model(); b <- coef(m); nm <- nama_koef(names(b), m); ik <- which(startsWith(names(b), "city")); contoh <- if (length(ik) > 0) { j <- ik[which.max(b[ik])]; pct <- (exp(b[j]) - 1) * 100; paste0(" Contoh: ", nm[j], " bernilai ", round(b[j], 3), ", yang berarti harga sekitar ", round(abs(pct), 1), "% ", ifelse(pct >= 0, "lebih tinggi", "lebih rendah"), " daripada kota acuan pada luas yang sama.") } else ""; kotak("Persamaan Model", paste0("Berdasarkan persamaan di atas, koefisien ln(luas bangunan) = ", round(b["ln_bld"], 3), " dan ln(luas tanah) = ", round(b["ln_land"], 3), " bertanda ", ifelse(all(b[c("ln_bld", "ln_land")] > 0), "positif", "tidak seluruhnya positif"), ", yang artinya kenaikan luas bangunan 1% diikuti kenaikan harga sekitar ", round(b["ln_bld"], 2), "% dan kenaikan luas tanah 1% sekitar ", round(b["ln_land"], 2), "% (elastisitas), dengan variabel lain tetap. Koefisien kota dan sertifikat adalah selisih ln(harga) terhadap kategori acuan (", m$xlevels$city[1], " dan ", m$xlevels$certificate[1], ").", contoh, " Intersep (", round(b[1], 3), ") hanya titik awal garis dan tidak bermakna praktis karena luas = 0 tidak mungkin.")) })

  # Empat uji asumsi klasik: (1) normalitas, (2) multikolinearitas, (3) heteroskedastisitas, (4) autokorelasi
  # 1. Normalitas
  output$out_norm <- renderPrint({ res <- residuals(model()); print(shapiro.test(res)); suppressWarnings(print(ks.test(res, "pnorm", mean(res), sd(res)))) })
  output$tafsir_norm <- renderUI({ a <- asumsi(); kotak("Normalitas Residual", paste0("Berdasarkan output di atas, uji Shapiro-Wilk ", p_txt(a$sw), " dan Kolmogorov-Smirnov ", p_txt(a$ks), " (H0: residual berdistribusi normal, ditolak jika p < 0,05), yang artinya residual ", ifelse(a$normal_ok, "berdistribusi normal sehingga asumsi normalitas terpenuhi.", "tidak berdistribusi normal sehingga asumsi normalitas tidak terpenuhi."))) })

  # 2. Multikolinearitas (output vif_out ada di atas)
  output$tafsir_vif <- renderUI({ g <- vif(model()); gv <- if (is.matrix(g)) g[, 1] else g; vmax <- if (is.matrix(g)) max(g[, 3]^2) else max(g); kotak("Multikolinearitas", paste0("Berdasarkan output di atas, VIF ln(luas tanah) = ", round(gv["ln_land"], 2), " dan ln(luas bangunan) = ", round(gv["ln_bld"], 2), " dengan nilai terbesar ", round(vmax, 2), " (H0: tidak ada multikolinearitas, ditolak jika VIF > 10), yang artinya ", ifelse(vmax <= 10, "tidak terjadi multikolinearitas yang serius sehingga asumsi terpenuhi.", "terjadi multikolinearitas sehingga asumsi tidak terpenuhi."))) })

  # 3. Heteroskedastisitas
  output$out_hetero <- renderPrint({ m <- model(); d <- data_f(); print(bptest(m)); gg <- summary(lm(abs(residuals(m)) ~ ln_land + ln_bld + city + certificate, data = d)); cat(sprintf("\nUji Glejser (|residual| ~ prediktor): F = %.2f (db1 = %d, db2 = %d), %s\n", gg$fstatistic[1], gg$fstatistic[2], gg$fstatistic[3], p_txt(asumsi()$gl))) })
  output$tafsir_hetero <- renderUI({ a <- asumsi(); kotak("Heteroskedastisitas", paste0("Berdasarkan output di atas, uji Breusch-Pagan ", p_txt(a$bp), " dan Glejser ", p_txt(a$gl), " (H0: ragam residual konstan, ditolak jika p < 0,05), yang artinya ragam residual ", ifelse(a$homo_ok, "konstan sehingga asumsi homoskedastisitas terpenuhi.", "tidak konstan (heteroskedastisitas) sehingga asumsi homoskedastisitas tidak terpenuhi."))) })

  # 4. Autokorelasi
  output$out_auto <- renderPrint({ print(dwtest(model(), alternative = "two.sided")) })
  output$tafsir_auto <- renderUI({ a <- asumsi(); res <- residuals(model()); set.seed(123); dw_acak <- sum(diff(sample(res))^2) / sum(res^2); tambahan <- ifelse(abs(dw_acak - 2) < 0.15, paste0(" Jika urutan baris diacak, DW menjadi ", round(dw_acak, 2), " (mendekati 2), sehingga autokorelasi ini berasal dari urutan baris data yang berkelompok (bukan menurut waktu), bukan dari model."), paste0(" Jika urutan baris diacak, DW menjadi ", round(dw_acak, 2), ".")); kotak("Autokorelasi", paste0("Berdasarkan output di atas, statistik Durbin-Watson = ", round(a$dw_stat, 2), " (", p_txt(a$dw_p), "; H0: tidak ada autokorelasi, ditolak jika p < 0,05), yang artinya ", ifelse(a$dw_p >= 0.05, "tidak terdapat autokorelasi sehingga asumsi independensi terpenuhi.", paste0("terdapat autokorelasi ", ifelse(a$dw_stat < 2, "positif", "negatif"), " sehingga asumsi independensi tidak terpenuhi.", tambahan)))) })

  # Ringkasan empat uji asumsi klasik
  output$tabel_asumsi <- renderDT({ a <- asumsi(); g <- vif(model()); vmax <- if (is.matrix(g)) max(g[, 3]^2) else max(g); ok <- function(p) ifelse(p >= 0.05, "Terpenuhi", "Tidak terpenuhi"); tb <- data.frame(Asumsi = c("Normalitas", "Normalitas", "Multikolinearitas", "Homoskedastisitas", "Homoskedastisitas", "Autokorelasi"), Uji = c("Shapiro-Wilk", "Kolmogorov-Smirnov", paste0("VIF terbesar = ", round(vmax, 2)), "Breusch-Pagan", "Glejser", paste0("Durbin-Watson (DW = ", round(a$dw_stat, 2), ")")), p_value = c(a$sw, a$ks, NA, a$bp, a$gl, a$dw_p), Kesimpulan = c(ok(a$sw), ok(a$ks), ifelse(vmax <= 10, "Terpenuhi", "Tidak terpenuhi"), ok(a$bp), ok(a$gl), ok(a$dw_p))); tb$p_value <- ifelse(is.na(tb$p_value), "-", ifelse(tb$p_value < 0.001, "< 0.001", as.character(round(tb$p_value, 3)))); datatable(tb, options = list(dom = "t"), rownames = FALSE) })
  output$tafsir_asumsi <- renderUI({ a <- asumsi(); g <- vif(model()); vmax <- if (is.matrix(g)) max(g[, 3]^2) else max(g); kotak("Ringkasan Uji Asumsi Klasik", paste0("Berdasarkan tabel di atas, dari empat uji asumsi klasik, normalitas ", ifelse(a$normal_ok, "terpenuhi", "tidak terpenuhi"), ", multikolinearitas ", ifelse(vmax <= 10, "terpenuhi", "tidak terpenuhi"), ", homoskedastisitas ", ifelse(a$homo_ok, "terpenuhi", "tidak terpenuhi"), ", dan autokorelasi ", ifelse(a$dw_p >= 0.05, "terpenuhi", "tidak terpenuhi"), ", yang artinya ", ifelse(a$normal_ok && a$homo_ok && vmax <= 10 && a$dw_p >= 0.05, "model memenuhi seluruh asumsi klasik.", "ada asumsi yang tidak terpenuhi dan dicatat sebagai keterbatasan model."))) })

  # Variabel yang berpengaruh (uji F parsial tiap variabel, Type II)
  pengaruh <- reactive({ an <- as.data.frame(car::Anova(model(), type = 2)); sse <- an["Residuals", "Sum Sq"]; an <- an[rownames(an) != "Residuals", ]; nm <- c(ln_land = "ln(Luas tanah)", ln_bld = "ln(Luas bangunan)", city = "Kota", certificate = "Sertifikat"); tb <- data.frame(Variabel = unname(nm[rownames(an)]), Df = an[["Df"]], F_hitung = round(an[["F value"]], 2), p_value = an[["Pr(>F)"]], Kontribusi_Parsial_Persen = round(100 * an[["Sum Sq"]] / (an[["Sum Sq"]] + sse), 1), row.names = NULL); tb[order(-tb$Kontribusi_Parsial_Persen), ] })
  output$tabel_pengaruh <- renderDT({ tb <- pengaruh(); tb$Kesimpulan <- ifelse(tb$p_value < 0.05, "Berpengaruh signifikan", "Tidak signifikan"); tb$p_value <- ifelse(tb$p_value < 0.001, "< 0.001", as.character(round(tb$p_value, 3))); datatable(tb, options = list(dom = "t"), rownames = FALSE) })
  output$tafsir_pengaruh <- renderUI({ tb <- pengaruh(); m <- model(); cf <- summary(m)$coefficients; b <- coef(m); ik <- startsWith(rownames(cf), "city"); isr <- startsWith(rownames(cf), "certificate"); kota_ns <- sub("^city", "", rownames(cf)[ik & cf[, 4] >= 0.05]); sert_ns <- sub("^certificate", "", rownames(cf)[isr & cf[, 4] >= 0.05]); ns <- tb$Variabel[tb$p_value >= 0.05]; kotak("Variabel yang Berpengaruh", paste0("Berdasarkan tabel di atas, variabel yang paling berpengaruh terhadap harga adalah ", tb$Variabel[1], " (kontribusi parsial ", tb$Kontribusi_Parsial_Persen[1], "%), diikuti ", tb$Variabel[2], " (", tb$Kontribusi_Parsial_Persen[2], "%) dan ", tb$Variabel[3], " (", tb$Kontribusi_Parsial_Persen[3], "%), ", ifelse(length(ns) == 0, "dan seluruh variabel berpengaruh signifikan pada α = 5%", paste0("namun ", paste(ns, collapse = ", "), " tidak berpengaruh signifikan")), ", yang artinya harga rumah terutama ditentukan oleh ", tb$Variabel[1], " dan ", tb$Variabel[2], ". Luas bangunan dan luas tanah berpengaruh ", ifelse(all(b[c("ln_bld", "ln_land")] > 0), "positif", "dengan arah berbeda"), " terhadap harga.", ifelse(length(kota_ns) > 0, paste0(" Kota yang harganya tidak berbeda nyata dari ", m$xlevels$city[1], ": ", paste(kota_ns, collapse = ", "), "."), " Semua kota harganya berbeda nyata dari kota acuan."), ifelse(length(sert_ns) > 0, paste0(" Sertifikat yang tidak berbeda nyata dari ", m$xlevels$certificate[1], ": ", paste(sert_ns, collapse = ", "), "."), " Semua jenis sertifikat berbeda nyata dari sertifikat acuan."))) })

  upaya <- reactive({
    m <- model(); d <- model.frame(m); n <- nrow(d); a <- asumsi()
    rumus <- ln_price ~ ln_land + ln_bld + city + certificate
    uji <- function(mod) {
      r <- residuals(mod)
      c(sw = shapiro.test(r)$p.value, bp = lmtest::bptest(mod)$p.value)
    }
    cf0 <- summary(m)$coefficients
    b_bld <- cf0["ln_bld", 1]

    rb <- lmtest::coeftest(m, vcov = sandwich::vcovHC(m, type = "HC3"))
    sig0 <- sum(cf0[-1, 4] < 0.05); sig1 <- sum(rb[-1, 4] < 0.05)

    buang <- cooks.distance(m) > 4 / n
    m2 <- lm(rumus, data = droplevels(d[!buang, ]))
    u2 <- uji(m2)

    m3 <- lm(ln_price ~ ln_land + ln_bld * city + certificate, data = d)
    u3 <- uji(m3)
    p_int <- anova(m, m3)[2, "Pr(>F)"]

    m4 <- suppressWarnings(MASS::rlm(rumus, data = d, maxit = 50))
    b4 <- unname(coef(m4)["ln_bld"])

    pf2 <- function(p) ifelse(p < 0.001, "< 0.001", as.character(round(p, 3)))
    tb <- data.frame(
      Upaya = c("Model awal", "1. Galat baku robust HC3", "2. Buang titik berpengaruh (Cook's D)",
                "3. Tambah interaksi kota x luas bangunan", "4. Regresi robust Huber"),
      Shapiro_p = c(pf2(a$sw), "sama", pf2(u2["sw"]), pf2(u3["sw"]), "tidak diuji"),
      Breusch_Pagan_p = c(pf2(a$bp), "sama", pf2(u2["bp"]), pf2(u3["bp"]), "tidak diuji"),
      Hasil = c(
        "Dua asumsi tidak terpenuhi.",
        paste0("Koefisien sama. Koefisien signifikan: ", sig0, " (OLS biasa) vs ", sig1, " (robust) dari ", nrow(cf0) - 1, "."),
        paste0("Dibuang ", sum(buang), " titik (", round(mean(buang) * 100, 1), "%). Elastisitas bangunan ",
               round(coef(m2)["ln_bld"], 2), " (awal ", round(b_bld, 2), "). Hanya uji sensitivitas."),
        paste0("Interaksi ", ifelse(p_int < 0.05, "nyata", "tidak nyata"), " (", p_txt(p_int), ")."),
        paste0("Elastisitas bangunan ", round(b4, 2), " (OLS ", round(b_bld, 2), "). Tahan terhadap residual ekstrem.")),
      row.names = NULL)

    lolos <- c()
    if (u2["sw"] >= 0.05 && u2["bp"] >= 0.05) lolos <- c(lolos, "membuang titik berpengaruh")
    if (u3["sw"] >= 0.05 && u3["bp"] >= 0.05) lolos <- c(lolos, "menambah interaksi")
    list(tb = tb, lolos = lolos, sig0 = sig0, sig1 = sig1, b_bld = b_bld, b4 = b4,
         jml = nrow(cf0) - 1, n = n)
  })

  output$sebab_asumsi <- renderUI({
    res <- residuals(model()); n <- length(res)
    tags$ul(
      tags$li(strong("Data besar:"), paste0(" n = ", comma(n), ". Uji formal sangat sensitif, penyimpangan kecil pun menghasilkan p < 0.001.")),
      tags$li(strong("Residual berekor berat:"), paste0(" skewness ", round(skew(res), 2), " dan kurtosis berlebih ", round(kurt(res), 2),
              ". Ada iklan dengan harga tidak sepadan ukurannya (harga penawaran, bukan transaksi).")),
      tags$li(strong("Ragam tidak konstan:"), " kondisi rumah, jumlah kamar, dan kecamatan tidak ada di data, dan satu kota mencakup area murah sampai mewah, sehingga sebaran galat berbeda antar rumah."),
      tags$li(strong("Durbin-Watson:"), " data cross-section sehingga urutan baris bukan urutan waktu; nilainya dibaca sebagai efek urutan baris data."))
  })

  output$tabel_upaya <- renderDT({
    datatable(upaya()$tb, options = list(dom = "t", scrollX = TRUE), rownames = FALSE)
  })

  output$tafsir_upaya <- renderUI({
    u <- upaya()
    awal <- if (length(u$lolos) > 0)
      paste0("Upaya yang meloloskan normalitas dan homoskedastisitas sekaligus: ", paste(u$lolos, collapse = " dan "), ". ")
    else
      "Tidak ada upaya yang meloloskan normalitas dan homoskedastisitas sekaligus. Penyebabnya ada di data (variabel penting tidak tersedia, residual berekor berat), bukan di cara analisis, sehingga tidak bisa diperbaiki tanpa data tambahan. "
    kotak("Upaya Mengatasi Asumsi",
          paste0(awal, "Kesimpulan tetap dapat dipertanggungjawabkan: (1) dengan n = ", comma(u$n),
                 ", uji t dan F mendekati valid menurut teorema limit pusat; (2) dengan galat baku robust HC3, kesimpulan koefisien ",
                 ifelse(u$sig0 == u$sig1, "tidak berubah", "berubah pada sebagian koefisien"),
                 "; (3) regresi robust memberi elastisitas luas bangunan ", round(u$b4, 2), " dibanding ", round(u$b_bld, 2), " pada OLS, ",
                 ifelse(abs(u$b4 - u$b_bld) < 0.05, "hampir sama", "berbeda"),
                 ". Pelanggaran asumsi dilaporkan sebagai keterbatasan, bukan disembunyikan."))
  })

  # R1
  output$r1 <- renderPlotly({
    d <- data_f(); d$fitted <- fitted(model())
    p <- ggplot(d, aes(x = fitted, y = ln_price)) +
      geom_point(alpha = 0.4, color = "#2563eb") +
      geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
      labs(title = "1. Nilai Aktual vs Prediksi", x = "Prediksi ln(Harga)", y = "Aktual ln(Harga)") + theme_minimal()
    ggplotly(p)
  })
  output$tr1 <- renderUI({
    r2 <- round(summary(model())$r.squared, 3)
    kotak("Aktual vs Prediksi",
          paste0("Berdasarkan grafik di atas, plot ini digunakan untuk membandingkan nilai aktual dengan nilai prediksi model regresi. Visualisasi ini menunjukkan sebaran titik yang rapat mengikuti garis diagonal dengan nilai R² = ", r2, ", yang artinya model mampu merepresentasikan variasi harga dengan tingkat ketepatan yang baik."))
  })

  # R2
  output$r2 <- renderPlotly({
    d <- data.frame(fitted = fitted(model()), resid = residuals(model()))
    p <- ggplot(d, aes(x = fitted, y = resid)) +
      geom_point(alpha = 0.4, color = "#059669") +
      geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
      labs(title = "2. Residual vs Fitted", x = "Prediksi ln(Harga)", y = "Residual") + theme_minimal()
    ggplotly(p)
  })
  output$tr2 <- renderUI({
    a <- asumsi()
    kotak("Residual vs Fitted",
          paste0("Berdasarkan grafik di atas, plot ini digunakan untuk menguji asumsi homoskedastisitas (kesamaan ragam galat). Visualisasi ini menunjukkan hasil uji Breusch-Pagan dengan p-value = ", p_txt(a$bp), ", yang artinya ", ifelse(a$bp < 0.05, "terdapat indikasi ragam galat yang tidak konstan (heteroskedastisitas).", "ragam galat relatif konstan dan asumsi homoskedastisitas terpenuhi.")))
  })

  # R3
  output$r3 <- renderPlotly({
    d <- data.frame(resid = residuals(model()))
    p <- ggplot(d, aes(x = resid)) +
      geom_histogram(aes(y = after_stat(density)), bins = 40, fill = "#7c3aed", color = "white") +
      stat_function(fun = dnorm, args = list(mean = mean(d$resid), sd = sd(d$resid)), color = "red") +
      labs(title = "3. Histogram Residual (garis merah: kurva normal)", x = "Residual", y = "Densitas") + theme_minimal()
    ggplotly(p)
  })
  output$tr3 <- renderUI({
    res <- residuals(model())
    sk <- round(skew(res), 2)
    kur <- round(kurt(res), 2)
    kotak("Histogram Residual",
          paste0("Berdasarkan grafik di atas, histogram ini digunakan untuk melihat bentuk distribusi sisaan (residual) model terhadap kurva normal. Visualisasi ini menunjukkan nilai skewness ", sk, " dan kurtosis berlebih ", kur, ", yang artinya galat model memiliki bentuk sebaran tertentu yang akan divalidasi lebih lanjut melalui uji normalitas formal."))
  })

  # R4
  output$r4 <- renderPlotly({
    res <- as.numeric(residuals(model())); res <- res[is.finite(res)]
    qq <- qqnorm(res, plot.it = FALSE); df_qq <- data.frame(Teoritis = as.numeric(qq$x), Residual = as.numeric(qq$y))
    qx <- qnorm(c(0.25, 0.75)); qy <- quantile(res, c(0.25, 0.75)); slope <- diff(qy) / diff(qx); intercept <- qy[1] - slope * qx[1]
    xline <- range(df_qq$Teoritis); yline <- intercept + slope * xline
    plot_ly(data = df_qq, x = ~Teoritis, y = ~Residual, type = "scatter", mode = "markers", name = "Residual", marker = list(color = "#2563EB", size = 6, opacity = 0.65)) %>%
      add_trace(x = xline, y = yline, type = "scatter", mode = "lines", name = "Garis Normal", inherit = FALSE, line = list(color = "#DC2626", width = 2, dash = "dash")) %>%
      layout(title = "Q-Q Plot Normalitas Residual", xaxis = list(title = "Kuantil Teoritis Normal"), yaxis = list(title = "Kuantil Residual"))
  })
  output$tr4 <- renderUI({
    a <- asumsi()
    kotak("Q-Q Plot Normalitas Residual",
          paste0("Berdasarkan grafik di atas, Q-Q plot ini digunakan untuk memeriksa kenormalan sisaan model regresi secara visual. Visualisasi ini menunjukkan bahwa titik-titik ", ifelse(a$normal_ok, "mengikuti garis referensi merah dengan uji Shapiro-Wilk ", "menjauhi garis referensi di kedua ujung dengan uji Shapiro-Wilk "), p_txt(a$sw), ", yang artinya asumsi normalitas galat ", ifelse(a$normal_ok, "terpenuhi.", "tidak terpenuhi karena ekor sisaan yang lebih tebal dari normal.")))
  })

  # R5
  output$r5 <- renderPlotly({
    m <- model(); ci <- confint(m)
    df <- data.frame(Variabel = nama_koef(rownames(ci), m), Koef = coef(m), bawah = ci[, 1], atas = ci[, 2], row.names = NULL)
    df <- df[df$Variabel != "Intercept", ]
    p <- ggplot(df, aes(x = Koef, y = reorder(Variabel, Koef))) +
      geom_segment(aes(x = bawah, xend = atas, yend = reorder(Variabel, Koef)), color = "#2563eb") +
      geom_point(color = "#0f172a") +
      geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
      labs(title = "5. Estimasi Koefisien dan Selang Kepercayaan 95%", x = "Koefisien", y = "Prediktor") + theme_minimal()
    ggplotly(p)
  })
  output$tr5 <- renderUI({
    ci <- confint(model())[-1, , drop = FALSE]
    n_sig <- sum(ci[, 1] > 0 | ci[, 2] < 0)
    kotak("Selang Kepercayaan Koefisien",
          paste0("Berdasarkan grafik di atas, plot interval ini digunakan untuk melihat signifikansi masing-masing koefisien prediktor melalui selang kepercayaan 95%. Visualisasi ini menunjukkan bahwa sebanyak ", n_sig, " dari ", nrow(ci), " koefisien tidak memotong garis nol, yang artinya prediktor-prediktor tersebut berpengaruh nyata secara statistik terhadap harga rumah."))
  })

  output$teks_batas <- renderUI({
    a <- asumsi()
    tags$ul(
      tags$li("Harga adalah harga penawaran iklan, bukan harga transaksi."),
      tags$li("Variabel penting tidak ada di model (kondisi rumah, jumlah kamar, akses, kecamatan), sehingga ada bias variabel yang terlewat."),
      tags$li("Luas tanah dan luas bangunan berkorelasi, sehingga koefisien masing-masing kurang stabil."),
      tags$li("Kota terlalu agregat (satu kota mencakup area mahal dan murah)."),
      tags$li("Model hanya menunjukkan asosiasi, bukan sebab-akibat."),
      tags$li("Pencilan dan data tidak valid dibuang, sehingga model tidak berlaku untuk rumah super mewah atau sangat murah."),
      tags$li("Sekitar 26,8% baris data awal punya nilai identik pada lima variabel (kemungkinan sebagian iklan ganda) dan tidak bisa dibedakan, sehingga sebagian pengamatan mungkin tidak independen."),
      if (!(a$normal_ok && a$homo_ok)) tags$li("Asumsi normalitas dan/atau homoskedastisitas tidak terpenuhi, sehingga p-value dan selang kepercayaan dibaca hati-hati."),
      if (a$dw_p < 0.05) tags$li("Durbin-Watson menunjukkan autokorelasi pada urutan baris data; baris tersusun berkelompok menurut kota (bukan menurut waktu), sehingga ini efek urutan baris dan bukan autokorelasi waktu.") else tags$li("Autokorelasi tidak terdeteksi pada urutan baris data."))
  })

  # Tab 7: Kesimpulan
  output$teks_kesimpulan <- renderUI({
    d <- data_f(); m <- model(); s <- summary(m); cf <- s$coefficients; a <- asumsi()
    md <- d %>% group_by(city) %>% summarise(m = median(price_miliar)) %>% arrange(m)
    n_sig <- sum(cf[-1, 4] < 0.05)
    r <- cor(d$ln_price, d$ln_bld)
    tagList(
      p(strong("Kesimpulan utama")),
      tags$ul(
        tags$li(paste0("Eksplorasi: harga rumah ", bentuk(skew(d$price_miliar)), " (median Rp", round(median(d$price_miliar), 2), " miliar, rata-rata Rp",
                       round(mean(d$price_miliar), 2), " miliar, n = ", comma(nrow(d)), ")."),
                " Luas tanah dan luas bangunan juga miring ke kanan sehingga ditransformasi ln."),
        tags$li(paste0("Hubungan: harga berkorelasi ", kekuatan(r), " dengan luas bangunan (r ln = ", round(r, 2), "). Median harga tertinggi di ",
                       md$city[nrow(md)], " (Rp", round(md$m[nrow(md)], 2), " miliar) dan terendah di ", md$city[1], " (Rp", round(md$m[1], 2), " miliar).")),
        tags$li(paste0("Model: regresi log-log menjelaskan ", round(s$r.squared * 100, 1), "% variasi ln(harga) (R² adjusted ", round(s$adj.r.squared, 3),
                       "). Elastisitas luas bangunan ", round(cf["ln_bld", 1], 2), " dan luas tanah ", round(cf["ln_land", 1], 2), "; ", n_sig, " dari ", nrow(cf) - 1,
                       " koefisien signifikan pada α = 5%. Normalitas residual ", ifelse(a$normal_ok, "terpenuhi", "tidak terpenuhi"), " dan homoskedastisitas ",
                       ifelse(a$homo_ok, "terpenuhi", "tidak terpenuhi"), ifelse(a$normal_ok && a$homo_ok, ".", ", sehingga uji dibaca hati-hati.")))),
      p(strong("Rekomendasi")),
      tags$ul(
        tags$li("Pembeli: bandingkan harga per m² dalam kota yang sama, bukan antar kota."),
        tags$li("Penjual/agen: tetapkan harga dengan acuan luas bangunan dan lokasi, lalu sesuaikan status sertifikat."),
        tags$li("Penelitian lanjutan: tambahkan jumlah kamar, kondisi rumah, dan kecamatan; coba model robust atau interaksi kota x luas.")))
  })

  # Tab 8: Unduh
  output$unduh_data <- downloadHandler(
    filename = function() paste0("jabodetabek_data_bersih_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(data_f(), file, row.names = FALSE))

  output$unduh_desk <- downloadHandler(
    filename = function() paste0("statistik_deskriptif_", Sys.Date(), ".csv"),
    content  = function(file) {
      x <- data_f() %>% select(price_miliar, land_size_m2, building_size_m2)
      write.csv(data.frame(Variabel = names(x), Mean = sapply(x, mean), Median = sapply(x, median), Std_Dev = sapply(x, sd)), file, row.names = FALSE)
    })

  output$unduh_reg <- downloadHandler(
    filename = function() paste0("koefisien_regresi_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(as.data.frame(summary(model())$coefficients), file, row.names = TRUE))
}

shinyApp(ui = ui, server = server)
