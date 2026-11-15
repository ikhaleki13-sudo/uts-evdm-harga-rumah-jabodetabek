# Dashboard Analisis Harga Rumah Jabodetabek

Aplikasi R Shiny untuk Ujian Tengah Semester Eksplorasi dan Visualisasi Data Multivariat (Gasal 2026/2027).
Aplikasi menyajikan eksplorasi univariat, bivariat, multivariat, dan regresi linear berganda atas data iklan rumah dijual di Jabodetabek.

## Identitas

- Nama: Juliana Riska Betek Leki
- NIM: 231068006
- Program Studi: Statistika
- Dosen: Inggit Fatika, S.Stat., M.Stat

## Permasalahan dan Tujuan

**Permasalahan:**
Pasar properti perumahan di wilayah Jabodetabek menunjukkan disparitas (ketimpangan) harga yang sangat tinggi akibat perbedaan ukuran luas tanah, luas bangunan, wilayah administratif (lokasi), serta status legalitas sertifikat. Ketidakseragaman informasi penawaran di lapangan sering menyulitkan calon pembeli maupun penjual dalam menentukan kewajaran harga pasar secara objektif serta mengukur seberapa besar pengaruh masing-masing karakteristik fisik dan lokasi terhadap nilai properti.

**Tujuan Analisis:**
1. Mengeksplorasi karakteristik data secara univariat, bivariat, dan multivariat.
2. Membangun model regresi linear berganda untuk menjelaskan variasi harga rumah.
3. Menilai kelayakan model melalui serangkaian uji asumsi residual.

## Dataset

- Sumber: Platform **Kaggle**, dataset *Jabodetabek House Price*.
- File: `jabodetabek_house_price.csv`
- Harga yang tercatat adalah harga penawaran iklan properti, bukan harga transaksi akhir.

Variabel yang dipakai:

| Variabel | Tipe | Peran | Keterangan |
|---|---|---|---|
| `price_in_rp` | Numerik | Respons (Y) | Harga penawaran rumah (Rupiah) |
| `land_size_m2` | Numerik | Prediktor | Luas tanah (m²) |
| `building_size_m2` | Numerik | Prediktor | Luas bangunan (m²) |
| `city` | Kategorik | Prediktor | Kota/kabupaten lokasi rumah |
| `certificate` | Kategorik | Prediktor | Jenis sertifikat (SHM, HGB, Lainnya, Tidak diketahui) |

## Pengolahan Data

1. Baris dengan harga atau luas kosong dibuang.
2. Luas tanah dan bangunan dibatasi 10 sampai 2.000 m² untuk membuang salah input.
3. Pencilan harga dibuang dengan aturan IQR pada log10 harga.
4. Sertifikat disederhanakan menjadi empat kategori; nama kota dirapikan.
5. Transformasi: harga dalam Rp miliar untuk tampilan, dan `ln(harga)`, `ln(luas tanah)`, `ln(luas bangunan)` untuk regresi.
6. Pemeriksaan *missing value* disajikan dalam tabel ringkasan terpisah; baris duplikat dicatat tetapi tidak dihapus karena tidak ada kolom pembeda.

## Isi Aplikasi

| Tab | Isi |
|---|---|
| 1. Dataset | Latar belakang & permasalahan, sumber data (Kaggle), definisi variabel, alasan pemilihan variabel, cuplikan data |
| 2. Pengolahan Data | Tahapan pembersihan, tabel *missing value* dan catatan duplikat, transformasi data |
| 3. Univariat | Statistik deskriptif, distribusi frekuensi, dan 6 grafik (terbagi ke dalam sub-tab Variabel Numerik & Variabel Kategorik) beserta interpretasi |
| 4. Bivariat | Korelasi (Pearson & Spearman) dan 6 grafik (terbagi ke dalam sub-tab Numerik vs Numerik, Numerik vs Kategorik, Kategorik vs Kategorik) |
| 5. Multivariat | Vektor rata-rata & matriks korelasi, deteksi pencilan Jarak Mahalanobis, Q-Q plot chi-square, bubble plot, facet plot |
| 6. Regresi Linear Berganda | Model log-log, uji F dan t, R², RMSE, VIF, uji asumsi residual, penyebab & upaya mengatasi pelanggaran asumsi, 5 grafik diagnostik, keterbatasan |
| 7. Kesimpulan | Kesimpulan dan rekomendasi yang dihitung otomatis dari data aktif |
| 8. Unduh | Data bersih, statistik deskriptif, dan koefisien regresi (CSV) |

Filter di sidebar (rentang harga, kota, jenis sertifikat) mengubah seluruh tabel, grafik, interpretasi, dan model secara dinamis.

## Model Regresi

$$\ln(\text{Harga}) = \beta_0 + \beta_1 \ln(\text{Luas Tanah}) + \beta_2 \ln(\text{Luas Bangunan}) + \sum \gamma \text{Kota} + \sum \delta \text{Sertifikat} + \varepsilon$$

Karena model log-log, koefisien luas tanah dan luas bangunan dibaca sebagai elastisitas.
Uji asumsi: Shapiro-Wilk, Kolmogorov-Smirnov, Breusch-Pagan, Glejser, VIF, dan Durbin-Watson (informatif, data cross-section).
Bila asumsi tidak terpenuhi, aplikasi menampilkan penyebabnya dan hasil beberapa upaya (galat baku robust HC3, uji sensitivitas Cook's D, interaksi kota × luas bangunan, regresi robust Huber).

## Cara Menjalankan

1. Pasang R dan RStudio.
2. Pasang paket sekali saja:

```r
install.packages(c("shiny", "tidyverse", "plotly", "DT", "car",
                   "lmtest", "sandwich", "scales", "MASS", "readr"))
