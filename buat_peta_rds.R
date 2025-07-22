# === File: buat_peta_rds.R ===

# 1. Muat library yang diperlukan
library(sf)
library(rmapshaper)
library(here)

# 2. Baca file peta asli Anda
# Ganti nama file jika perlu (misal, jika Anda menggunakan Shapefile)
cat("Membaca file peta asli...\n")
peta_asli <- sf::st_read(here("Data", "Kabupaten Indonesia.geojson"))

# Cetak ukuran file asli untuk perbandingan
print(paste("Ukuran objek asli di memori:", format(object.size(peta_asli), units = "Mb")))

# 3. Lakukan simplifikasi geometri
# 'keep = 0.05' berarti kita hanya mempertahankan 5% dari detail geometri.
# Ini akan mengurangi ukuran file secara drastis tanpa mengubah bentuk peta secara signifikan.
cat("\nMelakukan simplifikasi geometri...\n")
peta_sederhana <- ms_simplify(peta_asli, keep = 0.05, keep_shapes = TRUE)

# Cetak ukuran file setelah disederhanakan
print(paste("Ukuran objek setelah simplifikasi:", format(object.size(peta_sederhana), units = "Mb")))

# 4. Simpan objek yang sudah ringan ini sebagai file .rds
# File inilah yang akan Anda gunakan di aplikasi Shiny.
cat("\nMenyimpan hasil ke file .rds...\n")
saveRDS(peta_sederhana, file = here("Data", "peta_kabupaten_simplified.rds"))

cat("\nProses Selesai! ✅\n")
cat("File 'peta_kabupaten_simplified.rds' telah dibuat di dalam folder Data Anda.\n")