# Dashboard Analisis Harga Rumah Jabodetabek

Aplikasi R Shiny untuk Ujian Tengah Semester Eksplorasi dan Visualisasi Data Multivariat (Gasal 2026/2027).
Aplikasi menyajikan eksplorasi univariat, bivariat, multivariat, dan regresi linear berganda atas data iklan rumah dijual di Jabodetabek.

## Identitas

- Nama: Juliana Riska Betek Leki
- NIM: 231068006
- Program Studi: Statistika
- Dosen: Inggit Fatika, S.Stat., M.Stat

## Permasalahan dan Tujuan

Harga rumah di Jabodetabek bervariasi menurut ukuran tanah, ukuran bangunan, lokasi, dan status sertifikat. Tujuan analisis:

1. Mengeksplorasi karakteristik data secara univariat, bivariat, dan multivariat.
2. Membangun model regresi linear berganda untuk menjelaskan harga rumah.
3. Menilai kelayakan model melalui uji asumsi residual.

## Dataset

- Sumber: Kaggle, *Jabodetabek House Price* (data iklan dari Rumah123.com hasil scraping).
- File: `jabodetabek_house_price.csv`
- Harga adalah harga penawaran iklan, bukan harga transaksi.

Variabel yang dipakai:

| Variabel | Tipe | Peran | Keterangan |
|---|---|---|---|
| price_in_rp | Numerik | Respons (Y) | Harga penawaran rumah (Rupiah) |
| land_size_m2 | Numerik | Prediktor | Luas tanah (m²) |
| building_size_m2 | Numerik | Prediktor | Luas bangunan (m²) |
| city | Kategorik | Prediktor | Kota/kabupaten lokasi rumah |
| certificate | Kategorik | Prediktor | Jenis sertifikat (SHM, HGB, Lainnya, Tidak diketahui) |

## Pengolahan Data

1. Baris dengan harga atau luas kosong dibuang.
2. Luas tanah dan bangunan dibatasi 10 sampai 2.000 m² untuk membuang salah input.
3. Pencilan harga dibuang dengan aturan IQR pada log10 harga.
4. Sertifikat disederhanakan menjadi empat kategori; nama kota dirapikan.
5. Transformasi: harga dalam Rp miliar untuk tampilan, dan ln(harga), ln(luas tanah), ln(luas bangunan) untuk regresi.
6. Baris duplikat dicatat tetapi tidak dihapus karena tidak ada kolom pembeda.

## Isi Aplikasi

| Tab | Isi |
|---|---|
| 1. Dataset | Latar belakang, sumber, definisi variabel, alasan pemilihan variabel |
| 2. Pengolahan Data | Tahapan pembersihan, data hilang dan duplikat, transformasi |
| 3. Univariat | Statistik deskriptif dan 6 grafik beserta interpretasi |
| 4. Bivariat | Korelasi dan 6 grafik (numerik-numerik, numerik-kategorik, kategorik-kategorik) |
| 5. Multivariat | Jarak Mahalanobis, Q-Q plot chi-square, bubble plot, facet plot |
| 6. Regresi Linear Berganda | Model log-log, uji F dan t, R², RMSE, VIF, uji asumsi residual, upaya mengatasi pelanggaran asumsi, 5 grafik, keterbatasan |
| 7. Kesimpulan | Kesimpulan dan rekomendasi yang dihitung dari data aktif |
| 8. Unduh | Data bersih, statistik deskriptif, dan koefisien regresi (CSV) |

Filter di sidebar (rentang harga, kota, jenis sertifikat) mengubah seluruh tabel, grafik, interpretasi, dan model.

## Model Regresi

ln(Harga) = β0 + β1 ln(Luas Tanah) + β2 ln(Luas Bangunan) + Σ γ Kota + Σ δ Sertifikat + ε

Karena model log-log, koefisien luas tanah dan luas bangunan dibaca sebagai elastisitas.
Uji asumsi: Shapiro-Wilk, Kolmogorov-Smirnov, Breusch-Pagan, Glejser, VIF, dan Durbin-Watson (informatif, data cross-section).
Bila asumsi tidak terpenuhi, aplikasi menampilkan penyebabnya dan hasil beberapa upaya (galat baku robust HC3, uji sensitivitas Cook's D, interaksi kota x luas bangunan, regresi robust Huber).

## Cara Menjalankan

1. Pasang R dan RStudio.
2. Pasang paket sekali saja:

```r
install.packages(c("shiny", "tidyverse", "plotly", "DT", "car",
                   "lmtest", "sandwich", "scales", "MASS", "readr"))
```

3. Taruh `app.R` dan `jabodetabek_house_price.csv` di folder yang sama.
4. Buka `app.R` di RStudio lalu klik **Run App**, atau jalankan:

```r
shiny::runApp("folder-tempat-app.R")
```

## Keterbatasan

- Harga adalah harga penawaran, bukan harga transaksi.
- Variabel penting seperti kondisi rumah, jumlah kamar, dan kecamatan tidak ada di model.
- Luas tanah dan luas bangunan berkorelasi sehingga koefisien kurang stabil.
- Model menunjukkan asosiasi, bukan sebab-akibat.
- Asumsi normalitas dan homoskedastisitas tidak terpenuhi pada data ini; dilaporkan dan diperiksa dengan metode robust.
