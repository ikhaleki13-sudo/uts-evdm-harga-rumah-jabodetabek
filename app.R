# Dashboard UTS Eksplorasi dan Visualisasi Data Multivariat
# Topik : Analisis Harga Rumah Jabodetabek dengan Regresi Linear Berganda
# Nama  : Juliana Riska Betek Leki | NIM: 231068006 | Prodi: Statistika
# Dosen : Inggit Fatika, S.Stat., M.Stat
#
# Struktur kode mengikuti urutan soal UTS:
#   0. Persiapan: paket, data, fungsi bantu
#   1. Data: sumber, variabel, pengolahan data
#   2. Univariat: tabel deskriptif dan 6 grafik
#   3. Bivariat: korelasi dan 6 grafik
#   4. Multivariat: Mahalanobis, normalitas multivariat, 4 grafik
#   5. Regresi: model OLS, uji, asumsi residual, 5 grafik
#   6. Kesimpulan dan unduhan

library(shiny)      # kerangka aplikasi interaktif
library(tidyverse)  # dplyr (olah data) dan ggplot2 (grafik)
library(plotly)     # grafik interaktif (ggplotly)
library(DT)         # tabel interaktif
library(car)        # vif()
library(lmtest)     # bptest(), dwtest()
library(sandwich)   # vcovHC(): galat baku robust
library(scales)     # format angka (comma)

# File CSV harus satu folder dengan app.R.
# Nama boleh "jabodetabek_house_price.csv" atau "jabodetabek_house_price (1).csv".
cari <- list.files(pattern = "^jabodetabek_house_price.*\\.csv$", ignore.case = TRUE)
file_path <- if (length(cari) > 0) cari[1] else "jabodetabek_house_price.csv"

# OPSIONAL: jika kamu punya blok DATA_B64 <- c(...) dari versi lama,
# tempel di sini. Dipakai hanya bila file CSV tidak ditemukan.

# Fungsi bantu

# Kotak interpretasi di bawah setiap tabel dan grafik
kotak <- function(judul, isi) {
  div(class = "kotak-interpretasi",
      strong(paste0("Interpretasi ", judul, ": ")), isi)
}

# Skewness dan kurtosis berlebih (rumus momen)
skew <- function(x) { m <- mean(x); mean((x - m)^3) / mean((x - m)^2)^1.5 }
kurt <- function(x) { m <- mean(x); mean((x - m)^4) / mean((x - m)^2)^2 - 3 }

# Jumlah pencilan menurut aturan boxplot
n_pencilan <- function(x) {
  q <- quantile(x, c(0.25, 0.75))
  sum(x < q[1] - 1.5 * IQR(x) | x > q[2] + 1.5 * IQR(x))
}

# Kata penjelas bentuk distribusi dan kekuatan korelasi
bentuk <- function(s) {
  if (s > 0.5) "miring ke kanan" else if (s < -0.5) "miring ke kiri" else "relatif simetris"
}
kekuatan <- function(r) {
  arah <- if (r >= 0) "positif" else "negatif"
  kuat <- if (abs(r) >= 0.7) "kuat" else if (abs(r) >= 0.4) "sedang" else "lemah"
  paste(arah, kuat)
}

# Penulisan p-value
p_txt <- function(p) if (p < 0.001) "p < 0.001" else paste0("p = ", round(p, 3))

# UI

ui <- fluidPage(
  tags$head(
    tags$title("UTS Multivariat - Harga Rumah Jabodetabek"),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&display=swap"),
    tags$style(HTML("
      body { font-family:'Inter',system-ui,-apple-system,'Segoe UI',sans-serif; background:#f1f5f9;
             color:#1e293b; font-size:15px; line-height:1.65; }
      .hero { background:linear-gradient(135deg,#0f172a 0%,#1e3a8a 55%,#2563eb 100%); color:#fff;
              border-radius:0 0 28px 28px; padding:38px 44px 28px; margin:0 -15px 26px;
              position:relative; overflow:hidden; }
      .hero:before { content:''; position:absolute; right:-70px; top:-90px; width:300px; height:300px;
                     border-radius:50%; background:rgba(255,255,255,.07); }
      .hero:after { content:''; position:absolute; right:140px; bottom:-120px; width:220px; height:220px;
                    border-radius:50%; background:rgba(255,255,255,.05); }
      .hero .label-atas { font-size:12px; font-weight:700; letter-spacing:2px; color:#93c5fd; margin-bottom:8px; }
      .hero h1 { margin:0 0 8px; font-weight:800; font-size:34px; letter-spacing:-.5px; }
      .hero p { margin:0 0 18px; color:#cbd5e1; font-size:15px; max-width:760px; }
      .chip { display:inline-block; background:rgba(255,255,255,.14); border:1px solid rgba(255,255,255,.25);
              border-radius:999px; padding:5px 15px; margin:0 8px 6px 0; font-size:13px; font-weight:500; }
      .well { background:#fff; border:1px solid #e2e8f0; border-radius:18px;
              box-shadow:0 1px 3px rgba(15,23,42,.06); padding:20px; }
      .well h4 { font-weight:700; margin-top:0; }
      .well .control-label { font-weight:600; color:#334155; }
      .btn-primary { background:#2563eb; border:none; border-radius:10px; font-weight:600; padding:9px 12px; }
      .btn-primary:hover, .btn-primary:focus { background:#1d4ed8; }
      .btn-default { border-radius:10px; font-weight:600; }
      .irs--shiny .irs-bar { background:#2563eb; border-color:#2563eb; }
      .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single { background:#2563eb; }
      .irs--shiny .irs-handle { border-color:#2563eb; }
      .nav-tabs { border-bottom:none; margin-bottom:8px; }
      .nav-tabs > li > a { border:none; border-radius:999px; background:#e2e8f0; color:#475569;
                           font-weight:600; font-size:14px; margin:0 6px 8px 0; padding:8px 16px; }
      .nav-tabs > li > a:hover { background:#dbeafe; color:#1d4ed8; border:none; }
      .nav-tabs > li.active > a, .nav-tabs > li.active > a:hover, .nav-tabs > li.active > a:focus {
        background:#2563eb; color:#fff; border:none; box-shadow:0 4px 12px rgba(37,99,235,.35); }
      .kartu { background:#fff; border:1px solid #e2e8f0; border-radius:18px; padding:24px; margin-bottom:22px;
               box-shadow:0 1px 3px rgba(15,23,42,.05), 0 10px 28px rgba(15,23,42,.04); }
      .kartu h4 { margin-top:0; margin-bottom:16px; padding:2px 0 2px 12px; border-left:4px solid #2563eb;
                  font-weight:700; font-size:18px; }
      .kartu h5 { font-weight:700; color:#2563eb; margin:22px 0 8px; }
      .stat { text-align:center; padding:20px 12px; border-top:4px solid #2563eb; }
      .stat.hijau { border-top-color:#059669; }
      .stat.oranye { border-top-color:#f59e0b; }
      .stat.ungu { border-top-color:#7c3aed; }
      .stat .ikon { font-size:20px; color:#94a3b8; margin-bottom:4px; }
      .angka { font-size:26px; font-weight:800; color:#2563eb; text-align:center; }
      .label-angka { text-align:center; color:#64748b; font-size:13px; font-weight:500; }
      .kotak-interpretasi { background:linear-gradient(90deg,#eff6ff,#f8fafc); border-left:5px solid #2563eb;
             padding:13px 18px; border-radius:10px; margin:10px 0 28px 0; color:#334155; }
      .kotak-interpretasi strong { color:#1d4ed8; }
      table.dataTable thead th { background:#f8fafc; color:#334155; font-weight:700; border-bottom:2px solid #e2e8f0; }
      table.dataTable tbody tr:hover { background:#eff6ff !important; }
      pre { font-size:13px; background:#f8fafc; border:1px solid #e2e8f0; border-radius:10px; }
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
    # Sidebar: filter yang mengubah seluruh hasil
    sidebarPanel(
      width = 3,
      h4("Filter Data"),
      p("Ubah filter, maka semua tabel, grafik, interpretasi, dan model ikut berubah.",
        style = "font-size:13px; color:#64748b;"),
      sliderInput("filter_harga", "Rentang Harga (Miliar Rp):", min = 0, max = 50, value = c(0, 20), step = 0.1),
      checkboxGroupInput("filter_kota", "Kota/Wilayah:", choices = NULL),
      checkboxGroupInput("filter_sert", "Jenis Sertifikat:", choices = NULL),
      actionButton("reset", "Reset Filter", class = "btn-primary btn-block")
    ),

    mainPanel(
      width = 9,
      tabsetPanel(

        # Tab 1: permasalahan dan dataset (soal no. 1)
        tabPanel("1. Dataset",
          br(),
          fluidRow(
            column(3, div(class = "kartu", div(class = "angka", textOutput("n_obs")), div(class = "label-angka", "Observasi (setelah filter)"))),
            column(3, div(class = "kartu", div(class = "angka", "5"), div(class = "label-angka", "Variabel dipakai"))),
            column(3, div(class = "kartu", div(class = "angka", "3"), div(class = "label-angka", "Variabel numerik"))),
            column(3, div(class = "kartu", div(class = "angka", "2"), div(class = "label-angka", "Variabel kategorik")))
          ),
          div(class = "kartu",
              h4("Latar Belakang dan Tujuan"),
              p("Harga rumah di Jabodetabek sangat bervariasi menurut ukuran tanah, ukuran bangunan, lokasi, dan status sertifikat. Pembeli dan penjual membutuhkan gambaran faktor apa yang berkaitan dengan harga."),
              p(strong("Tujuan: "), "(1) mengeksplorasi karakteristik data secara univariat, bivariat, dan multivariat; (2) membangun model regresi linear berganda untuk menjelaskan harga rumah; (3) menilai kelayakan model melalui uji asumsi residual.")),
          div(class = "kartu",
              h4("Sumber Data"),
              p("Data iklan rumah dijual di Jabodetabek dari ", strong("Rumah123.com"), " (hasil scraping, dataset Kaggle ", em("Jabodetabek House Price"), "). Harga adalah ", strong("harga penawaran"), " iklan, bukan harga transaksi."),
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

        # Tab 2: pengolahan data (soal B.6)
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
              h4("Data Hilang dan Duplikat"),
              DTOutput("tabel_hilang"),
              uiOutput("teks_duplikat")),
          div(class = "kartu",
              h4("Transformasi Data"),
              tags$ul(
                tags$li("Harga dinyatakan dalam Rp miliar (price_miliar) agar mudah dibaca."),
                tags$li("Regresi memakai ln(harga), ln(luas tanah), dan ln(luas bangunan): distribusinya miring ke kanan, log menstabilkan ragam, dan koefisien dibaca sebagai elastisitas (persen)."),
                tags$li("Sertifikat disederhanakan menjadi 4 kategori; nama kota dirapikan (spasi dihapus).")))
        ),

        # Tab 3: univariat (soal no. 2)
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
              h4("C. Visualisasi Univariat"),
              plotlyOutput("u1"), uiOutput("tu1"),
              plotlyOutput("u2"), uiOutput("tu2"),
              plotlyOutput("u3"), uiOutput("tu3"),
              plotlyOutput("u4"), uiOutput("tu4"),
              plotlyOutput("u5"), uiOutput("tu5"),
              plotlyOutput("u6"), uiOutput("tu6"))
        ),

        # Tab 4: bivariat (soal no. 3)
        tabPanel("4. Bivariat",
          br(),
          div(class = "kartu",
              h4("A. Korelasi Antarvariabel Numerik"),
              DTOutput("tabel_korelasi"),
              uiOutput("tafsir_korelasi")),
          div(class = "kartu",
              h4("B. Visualisasi Bivariat"),
              h5("Numerik vs Numerik"),
              plotlyOutput("b1"), uiOutput("tb1"),
              plotlyOutput("b2"), uiOutput("tb2"),
              plotlyOutput("b3"), uiOutput("tb3"),
              h5("Numerik vs Kategorik"),
              plotlyOutput("b4"), uiOutput("tb4"),
              plotlyOutput("b5"), uiOutput("tb5"),
              h5("Kategorik vs Kategorik"),
              plotlyOutput("b6"), uiOutput("tb6"))
        ),

        # Tab 5: multivariat (soal no. 4)
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

        # Tab 6: regresi (soal no. 5)
        tabPanel("6. Regresi Linear Berganda",
          br(),
          div(class = "kartu",
              h4("A. Variabel dan Persamaan Model"),
              uiOutput("teks_variabel"),
              verbatimTextOutput("persamaan")),
          div(class = "kartu",
              h4("B. Hasil Estimasi OLS"),
              verbatimTextOutput("ringkasan_model"),
              DTOutput("tabel_koef"),
              verbatimTextOutput("evaluasi_model"),
              uiOutput("tafsir_koef")),
          div(class = "kartu",
              h4("C. Uji Asumsi Residual"),
              verbatimTextOutput("vif_out"),
              DTOutput("tabel_asumsi"),
              uiOutput("tafsir_asumsi"),
              h5("Penyebab Asumsi Tidak Terpenuhi"),
              uiOutput("sebab_asumsi"),
              h5("Upaya Mengatasi dan Hasilnya"),
              DTOutput("tabel_upaya"),
              uiOutput("tafsir_upaya")),
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

  # 1. Membaca data: CSV di folder app.R, atau DATA_B64 jika ada
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

  # 2. Pembersihan data: rapikan, buang kosong, buang luas tak wajar, buang pencilan
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

  # Isi pilihan filter dari data
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

  # Data setelah filter: dipakai oleh semua grafik dan model
  data_f <- reactive({
    data_bersih()  # jika CSV tidak ditemukan, pesan file muncul lebih dulu
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

  # Tab 1: dataset
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

  # Tab 2: pengolahan data
  output$tabel_log <- renderDT({
    datatable(log_bersih(), options = list(dom = "t"), rownames = FALSE)
  })

  output$tabel_hilang <- renderDT({
    mentah <- data_mentah()
    hilang <- sapply(mentah[var5], function(x) sum(is.na(x) | (is.character(x) & trimws(x) == "")))
    tb <- data.frame(Variabel = var5, Jumlah_Hilang = as.numeric(hilang), row.names = NULL)
    tb$Persen_Hilang <- round(tb$Jumlah_Hilang / nrow(mentah) * 100, 2)
    datatable(tb, options = list(dom = "t"), rownames = FALSE)
  })

  output$teks_duplikat <- renderUI({
    mentah <- data_mentah()
    dup <- sum(duplicated(mentah[var5]))
    p(paste0("Ada ", comma(dup), " baris (", round(dup / nrow(mentah) * 100, 1),
             "%) dengan kelima variabel yang persis sama. Baris ini tidak dihapus karena data tidak punya kolom pembeda (misalnya ID iklan) untuk memastikan apakah itu iklan ganda atau unit berbeda dengan spesifikasi sama (misalnya rumah kembar di satu perumahan). Hal ini dicatat sebagai keterbatasan. Sertifikat kosong diberi kategori 'Tidak diketahui'."))
  })

  # Tab 3: univariat
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

  # U1: histogram harga
  output$u1 <- renderPlotly({
    p <- ggplot(data_f(), aes(x = price_miliar)) +
      geom_histogram(bins = 40, fill = "#2563eb", color = "white") +
      labs(title = "1. Histogram Harga Rumah", x = "Harga (Miliar Rp)", y = "Frekuensi") + theme_minimal()
    ggplotly(p)
  })
  output$tu1 <- renderUI({
    s <- data_f()$price_miliar
    kotak("Histogram Harga",
          paste0("Histogram dipilih untuk melihat bentuk distribusi variabel numerik. Distribusi harga ", bentuk(skew(s)),
                 " (skewness ", round(skew(s), 2), ")", ifelse(skew(s) > 0.5, ": kebanyakan rumah berharga rendah sampai menengah, dengan ekor panjang ke harga tinggi.", ".")))
  })

  # U2: boxplot harga
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
    batas <- quantile(s, 0.75) + 1.5 * IQR(s)
    kotak("Boxplot Harga",
          paste0("Boxplot dipilih untuk meringkas median dan kuartil sekaligus mengidentifikasi pencilan. Median Rp", round(median(s), 2),
                 " miliar; batas atas pencilan (Q3 + 1,5 IQR) Rp", round(batas, 2), " miliar, dan ada ", n_pencilan(s),
                 " rumah (", round(n_pencilan(s) / length(s) * 100, 1), "%) di atasnya. Pencilan ekstrem sudah dibuang saat pembersihan (IQR log10), jadi sisanya adalah harga tinggi yang masih wajar."))
  })

  # U3: histogram dan density luas bangunan
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
          paste0("Density plot (garis merah) menampilkan distribusi yang dihaluskan. Median ", round(median(s)), " m², rata-rata ", round(mean(s)),
                 " m², distribusi ", bentuk(skew(s)), " dengan ", n_pencilan(s), " pencilan menurut aturan IQR."))
  })

  # U4: histogram dan density luas tanah
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
          paste0("Median ", round(median(s)), " m², rata-rata ", round(mean(s)), " m², distribusi ", bentuk(skew(s)),
                 " dengan ", n_pencilan(s), " pencilan IQR. Distribusi miring ke kanan menjadi alasan memakai ln(luas tanah) pada regresi."))
  })

  # U5: diagram batang kota
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
          paste0("Diagram batang dipilih untuk membandingkan frekuensi antar kategori, diurutkan dari terbanyak. ", tb$city[1], " terbanyak (",
                 comma(tb$n[1]), " rumah) dan ", tb$city[nrow(tb)], " paling sedikit (", comma(tb$n[nrow(tb)]), ")."))
  })

  # U6: pie chart sertifikat
  output$u6 <- renderPlotly({
    tb <- data_f() %>% count(certificate)
    plot_ly(tb, labels = ~certificate, values = ~n, type = "pie", textinfo = "label+percent") %>%
      layout(title = "6. Proporsi Jenis Sertifikat")
  })
  output$tu6 <- renderUI({
    tb <- data_f() %>% count(certificate, sort = TRUE)
    kotak("Pie Chart Sertifikat",
          paste0("Pie chart dipilih untuk menampilkan proporsi tiap kategori terhadap total. Sertifikat ", tb$certificate[1],
                 " adalah kategori dominan (", round(tb$n[1] / sum(tb$n) * 100, 1), "%)."))
  })

  # Tab 4: bivariat
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

  # B1: scatter harga vs luas bangunan
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
          paste0("Scatterplot dengan garis linear dipilih untuk dua variabel numerik. Titik naik ke kanan atas: hubungan ", kekuatan(r),
                 " (r = ", round(r, 2), " pada skala log); rumah yang bangunannya lebih luas cenderung berharga lebih tinggi."))
  })

  # B2: scatter harga vs luas tanah
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
          paste0("Hubungan ", kekuatan(r), " (r = ", round(r, 2), " pada skala log). Sebaran titik lebih lebar dibanding luas bangunan, artinya luas tanah saja kurang cukup menjelaskan harga; lokasi ikut berperan."))
  })

  # B3: plot korelasi
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
    kotak("Plot Korelasi",
          paste0("Plot korelasi meringkas semua pasangan variabel numerik; makin merah makin kuat positif, makin biru makin kuat negatif. Luas tanah dan luas bangunan berkorelasi r = ",
                 round(cor(d$ln_land, d$ln_bld), 2), ", sehingga multikolinearitasnya dicek pada regresi (VIF)."))
  })

  # B4: violin dan boxplot harga per kota
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
          paste0("Gabungan violin (bentuk sebaran) dan boxplot (median, kuartil) dipilih untuk membandingkan numerik antar kelompok. Median harga tertinggi di ",
                 md$city[nrow(md)], " (Rp", round(md$m[nrow(md)], 2), " miliar) dan terendah di ", md$city[1], " (Rp", round(md$m[1], 2),
                 " miliar), sehingga lokasi membedakan harga dengan jelas."))
  })

  # B5: rata-rata harga dan SE per sertifikat
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
          paste0("Plot rata-rata dengan standard error dipilih untuk membandingkan rata-rata antar kelompok beserta ketidakpastiannya. Rata-rata tertinggi pada sertifikat ",
                 tb$certificate[nrow(tb)], " (Rp", round(tb$rata[nrow(tb)], 2), " miliar) dan terendah pada ", tb$certificate[1],
                 " (Rp", round(tb$rata[1], 2), " miliar). Batang error yang tidak saling tumpang tindih menandakan perbedaan yang jelas."))
  })

  # B6: segmented bar kota x sertifikat
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
          paste0("Segmented bar dipilih untuk membandingkan komposisi dua variabel kategorik. Uji chi-square ", p_txt(uji$p.value),
                 ifelse(uji$p.value < 0.05, " (ada hubungan antara kota dan jenis sertifikat)", " (tidak ada hubungan nyata)"),
                 ", Cramér's V = ", round(v, 2), ifelse(v < 0.1, " (lemah)", ifelse(v < 0.3, " (sedang)", " (kuat)")),
                 ". Sebagian sel berisi sedikit rumah sehingga uji dibaca hati-hati."))
  })

  # Tab 5: multivariat
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

  # Jarak Mahalanobis pada ln(harga), ln(luas tanah), ln(luas bangunan).
  # Batas pencilan: kuantil chi-square 0,975 dengan db = 3.
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
                     ". Makin dekat 1, makin lurus Q-Q plot. Pada n besar, uji formal hampir selalu menolak normalitas, sehingga kesimpulan dilihat bersama grafik Q-Q.")))
  })

  # M1: bubble plot
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
    kotak("Bubble Plot",
          paste0("Bubble plot memuat empat variabel sekaligus (sumbu X dan Y = luas, warna = kota, ukuran = harga). Luas tanah dan bangunan berkorelasi r = ",
                 round(cor(d$ln_land, d$ln_bld), 2), " (skala ln), dan ln(harga) berkorelasi r = ", round(cor(d$ln_price, d$ln_bld), 2), " dengan ln(luas bangunan), sehingga gelembung besar (harga tinggi) cenderung berada di sisi kanan atas."))
  })

  # M2: facet scatter per kota
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
    rr <- data_f() %>% group_by(city) %>% summarise(r = suppressWarnings(cor(ln_price, ln_bld)))
    pola <- if (all(rr$r > 0, na.rm = TRUE)) paste0("korelasi ln(harga) dan ln(luas bangunan) positif di semua kota (r = ", round(min(rr$r, na.rm = TRUE), 2), " sampai ", round(max(rr$r, na.rm = TRUE), 2), ").") else "arah hubungan tidak sama di semua kota."
    kotak("Facet Plot",
          paste0("Facet memecah grafik per kota agar hubungan harga dan luas bangunan terlihat di tiap wilayah, dengan warna sertifikat sebagai variabel ketiga. Median harga per m² bangunan tertinggi di ",
                 pp$city[nrow(pp)], " (Rp", round(pp$m[nrow(pp)], 1), " juta) dan terendah di ", pp$city[1], " (Rp", round(pp$m[1], 1),
                 " juta); ", pola))
  })

  # M3: pencilan multivariat
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
    kotak("Pencilan Mahalanobis",
          paste0("Titik merah adalah rumah yang kombinasi harga, luas tanah, dan luas bangunannya tidak lazim (D² > ", round(m$cutoff, 2),
                 "): ", sum(m$outlier), " rumah (", round(mean(m$outlier) * 100, 1),
                 "%). Rumah seperti ini perlu dicek apakah salah input atau memang unit istimewa (misalnya harga tidak sepadan dengan ukurannya)."))
  })

  # M4: Q-Q plot chi-square
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
          paste0("Jika data normal multivariat, titik mengikuti garis lurus. Korelasi titik dengan kuantil teoritis rQ = ", round(m$rq, 4),
                 ifelse(m$rq > 0.99, ", titik hampir lurus.", ", titik menjauhi garis di ujung kanan (ekor lebih berat dari normal), sehingga data tidak sepenuhnya normal multivariat."),
                 " Normalitas multivariat tidak disyaratkan untuk OLS, hanya untuk analisis seperti PCA, MANOVA, atau clustering."))
  })

  # Tab 6: regresi linear berganda
  model <- reactive({
    d <- data_f()
    validate(need(n_distinct(d$city) >= 2, "Regresi butuh minimal 2 kota. Longgarkan filter."),
             need(n_distinct(d$certificate) >= 2, "Regresi butuh minimal 2 jenis sertifikat. Longgarkan filter."))
    d$city <- factor(d$city)
    d$certificate <- factor(d$certificate)
    # Kategori acuan: kota paling banyak, sertifikat SHM
    d$city <- relevel(d$city, ref = names(sort(table(d$city), decreasing = TRUE))[1])
    acuan <- if ("SHM" %in% levels(d$certificate)) "SHM" else names(sort(table(d$certificate), decreasing = TRUE))[1]
    d$certificate <- relevel(d$certificate, ref = acuan)
    lm(ln_price ~ ln_land + ln_bld + city + certificate, data = d)
  })

  # Mengubah nama koefisien R menjadi nama yang mudah dibaca
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

  # Uji asumsi: normalitas, homoskedastisitas, independensi
  asumsi <- reactive({
    m <- model(); d <- data_f()
    res <- residuals(m)
    sw <- shapiro.test(res)$p.value
    ks <- suppressWarnings(ks.test(res, "pnorm", mean(res), sd(res))$p.value)
    bp <- bptest(m)$p.value
    g  <- summary(lm(abs(res) ~ ln_land + ln_bld + city + certificate, data = d))
    gl <- pf(g$fstatistic[1], g$fstatistic[2], g$fstatistic[3], lower.tail = FALSE)
    # Durbin-Watson hanya informatif karena data cross-section
    dw <- dwtest(m, alternative = "two.sided")
    list(sw = sw, ks = ks, bp = bp, gl = gl, dw_stat = unname(dw$statistic), dw_p = dw$p.value,
         normal_ok = min(sw, ks) >= 0.05, homo_ok = min(bp, gl) >= 0.05)
  })

  output$vif_out <- renderPrint({
    cat("Multikolinearitas (VIF; untuk variabel kategorik dipakai GVIF):\n")
    print(vif(model()))
  })

  output$tabel_asumsi <- renderDT({
    a <- asumsi()
    ok <- function(p) ifelse(p >= 0.05, "Terpenuhi", "Tidak terpenuhi")
    tb <- data.frame(
      Asumsi = c("Normalitas residual", "Normalitas residual", "Homoskedastisitas", "Homoskedastisitas", "Independensi (informatif)"),
      Uji    = c("Shapiro-Wilk", "Kolmogorov-Smirnov", "Breusch-Pagan", "Glejser", paste0("Durbin-Watson (DW = ", round(a$dw_stat, 2), ")")),
      p_value = c(a$sw, a$ks, a$bp, a$gl, a$dw_p),
      Kesimpulan = c(ok(a$sw), ok(a$ks), ok(a$bp), ok(a$gl), "Tidak dijadikan kesimpulan"))
    tb$p_value <- ifelse(tb$p_value < 0.001, "< 0.001", as.character(round(tb$p_value, 3)))
    datatable(tb, options = list(dom = "t"), rownames = FALSE)
  })

  output$tafsir_asumsi <- renderUI({
    a <- asumsi(); g <- vif(model())
    vmax <- if (is.matrix(g)) max(g[, 3]^2) else max(g)
    kotak("Uji Asumsi",
          paste0("Multikolinearitas ", ifelse(vmax < 10, "tidak bermasalah", "perlu diperhatikan"), " (VIF terbesar ", round(vmax, 2), ", batas umum 10). ",
                 "Normalitas residual ", ifelse(a$normal_ok, "terpenuhi", "tidak terpenuhi"), " (Shapiro-Wilk ", p_txt(a$sw), "). ",
                 "Homoskedastisitas ", ifelse(a$homo_ok, "terpenuhi", "tidak terpenuhi"), " (Breusch-Pagan ", p_txt(a$bp), "; Glejser ", p_txt(a$gl), "). ",
                 "Autokorelasi: Durbin-Watson ditampilkan hanya sebagai informasi karena data cross-section; urutan baris iklan tidak bermakna waktu, jadi nilainya tidak dijadikan dasar kesimpulan. ",
                 ifelse(a$normal_ok && a$homo_ok, "",
                        "Karena ada asumsi yang tidak terpenuhi, p-value dan selang kepercayaan dibaca hati-hati; uji formal juga sangat sensitif pada data besar sehingga grafik residual perlu dilihat bersamanya.")))
  })

  # Upaya mengatasi pelanggaran asumsi: dihitung ulang dari data aktif
  upaya <- reactive({
    m <- model(); d <- model.frame(m); n <- nrow(d); a <- asumsi()
    rumus <- ln_price ~ ln_land + ln_bld + city + certificate
    uji <- function(mod) {
      r <- residuals(mod)
      c(sw = shapiro.test(r)$p.value, bp = lmtest::bptest(mod)$p.value)
    }
    cf0 <- summary(m)$coefficients
    b_bld <- cf0["ln_bld", 1]

    # Upaya 1: galat baku robust HC3
    rb <- lmtest::coeftest(m, vcov = sandwich::vcovHC(m, type = "HC3"))
    sig0 <- sum(cf0[-1, 4] < 0.05); sig1 <- sum(rb[-1, 4] < 0.05)

    # Upaya 2: buang titik berpengaruh (Cook's D > 4/n)
    buang <- cooks.distance(m) > 4 / n
    m2 <- lm(rumus, data = droplevels(d[!buang, ]))
    u2 <- uji(m2)

    # Upaya 3: tambah interaksi kota x ln(luas bangunan)
    m3 <- lm(ln_price ~ ln_land + ln_bld * city + certificate, data = d)
    u3 <- uji(m3)
    p_int <- anova(m, m3)[2, "Pr(>F)"]

    # Upaya 4: regresi robust Huber
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
      tags$li(strong("Durbin-Watson:"), " tidak relevan karena data cross-section, urutan baris tidak bermakna waktu."))
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

  # R1: aktual vs prediksi
  output$r1 <- renderPlotly({
    d <- data_f(); d$fitted <- fitted(model())
    p <- ggplot(d, aes(x = fitted, y = ln_price)) +
      geom_point(alpha = 0.4, color = "#2563eb") +
      geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
      labs(title = "1. Nilai Aktual vs Prediksi", x = "Prediksi ln(Harga)", y = "Aktual ln(Harga)") + theme_minimal()
    ggplotly(p)
  })
  output$tr1 <- renderUI({
    kotak("Aktual vs Prediksi",
          paste0("Titik yang rapat di sekitar garis diagonal berarti prediksi mendekati nilai aktual (R² = ", round(summary(model())$r.squared, 3),
                 "). Sebaran yang tersisa menunjukkan galat yang belum dijelaskan model."))
  })

  # R2: residual vs fitted
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
          paste0("Plot ini dipakai memeriksa ragam galat. Jika titik membentuk pita acak di sekitar nol, ragam konstan. Breusch-Pagan ", p_txt(a$bp),
                 ifelse(a$bp < 0.05, ": ada indikasi ragam galat tidak konstan (heteroskedastisitas).", ": tidak ada indikasi heteroskedastisitas.")))
  })

  # R3: histogram residual
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
    kotak("Histogram Residual",
          paste0("Histogram dibandingkan dengan kurva normal untuk menilai kenormalan galat. Skewness ", round(skew(res), 2), ", kurtosis berlebih ", round(kurt(res), 2),
                 ifelse(kurt(res) > 1, ": ekor lebih tebal dari normal (ada residual ekstrem).", ": bentuk mendekati lonceng.")))
  })

  # R4: Q-Q plot residual
  output$r4 <- renderPlotly({
    d <- data.frame(resid = residuals(model()))
    p <- ggplot(d, aes(sample = resid)) +
      stat_qq(alpha = 0.5, color = "#0f172a") + stat_qq_line(color = "red") +
      labs(title = "4. Q-Q Plot Residual", x = "Kuantil Teoritis Normal", y = "Kuantil Residual") + theme_minimal()
    ggplotly(p)
  })
  output$tr4 <- renderUI({
    a <- asumsi()
    kotak("Q-Q Plot Residual",
          paste0("Jika residual normal, titik mengikuti garis merah. Shapiro-Wilk ", p_txt(a$sw), ", Kolmogorov-Smirnov ", p_txt(a$ks), "; titik ",
                 ifelse(a$normal_ok, "mengikuti garis sehingga residual mendekati normal.", "menjauhi garis di kedua ujung, tanda ekor distribusi lebih berat dari normal.")))
  })

  # R5: selang kepercayaan koefisien
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
          paste0("Titik adalah estimasi, garis adalah selang kepercayaan 95%. ", n_sig, " dari ", nrow(ci),
                 " koefisien tidak memotong garis nol (signifikan pada α = 5%); selang yang memotong nol berarti prediktor itu tidak berbeda nyata dari nol."))
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
      tags$li("Autokorelasi tidak dijadikan kesimpulan karena data cross-section."))
  })

  # Tab 7: kesimpulan (angka dihitung dari data aktif)
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

  # Tab 8: unduh
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


