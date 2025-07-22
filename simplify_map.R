# === File: simplify_map.R ===

library(sf)
library(rmapshaper)
library(here)

# 1. Baca file GeoJSON asli yang besar
original_map <- sf::st_read(here("Data", "Kabupaten Indonesia.geojson"))
print(paste("Ukuran file asli:", format(object.size(original_map), units = "Mb")))

# 2. Lakukan simplifikasi
# 'keep_proportion' berarti kita ingin menjaga 5% dari detail asli.
# Angka ini bisa Anda sesuaikan (misal: 0.01 untuk lebih simpel, 0.1 untuk lebih detail).
# 0.05 (5%) biasanya sudah sangat bagus.
simplified_map <- ms_simplify(original_map, keep = 0.05)
print(paste("Ukuran file setelah simplifikasi:", format(object.size(simplified_map), units = "Mb")))

# 3. Simpan peta yang sudah disederhanakan ke file baru
# Gunakan file BARU ini di aplikasi Shiny Anda.
sf::st_write(simplified_map, here("Data", "Kabupaten_Indonesia_simplified.geojson"), delete_dsn = TRUE)

# 4. (SANGAT DIREKOMENDASIKAN) Simpan juga sebagai file .rds untuk loading super cepat
# Daripada membaca GeoJSON setiap kali, baca file .rds ini di app Shiny Anda.
saveRDS(simplified_map, file = here("Data", "peta_simplified.rds"))

print("Proses selesai! Gunakan 'Kabupaten_Indonesia_simplified.geojson' atau 'peta_simplified.rds' di aplikasi Anda.")