# Load required libraries
library(mclust)
library(e1071)
library(MASS)

#Data Input
library(readxl)
Data1<- read_excel("skripsi project/DATA SAMUDRA INDONESIA/Jurnal Sopus/Pertamina FS Sains 2024/Filedata Indeks Standar Pencemaran Udara (ISPU) Tahun 2022.xlsx", 
                   sheet = "Filedata Indeks Standar Pencema")
View(Data1)

#Kandungan Udara
pm_10<-c(Data1$pm_10)
Pm_2.5<-c(Data1$Pm_2.5)
so2<-c(Data1$so2)
co<-c(Data1$o3)
o3<-c(Data1$o3)
no2<-c(Data1$no2)

# Fungsi keanggotaan untuk kategori "Baik"
fuzzy_Baik <- function(x) {
  ifelse(x <= 25, 1, ifelse(x <= 50, (50 - x) / (50 - 25), 0))
}

# Fungsi keanggotaan untuk kategori "Sedang"
fuzzy_Sedang <- function(x) {
  ifelse(x <= 25, 0, ifelse(x <= 50, (x - 25) / (50 - 25), ifelse(x <= 100, (100 - x) / (100 - 50), 0)))
}

# Fungsi keanggotaan untuk kategori "Tidak Sehat"
fuzzy_TidakSehat <- function(x) {
  ifelse(x <= 50, 0, ifelse(x <= 100, (x - 50) / (100 - 50), ifelse(x <= 200, (200 - x) / (200 - 100), 0)))
}

# Fungsi keanggotaan untuk kategori "Sangat Tidak Sehat"
fuzzy_SangatTidakSehat <- function(x) {
  ifelse(x <= 100, 0, ifelse(x <= 200, (x - 100) / (200 - 100), ifelse(x <= 300, (300 - x) / (300 - 200), 0)))
}

# Fungsi keanggotaan untuk kategori "Berbahaya"
fuzzy_Berbahaya <- function(x) {
  ifelse(x <= 200, 0, ifelse(x <= 300, (x - 200) / (300 - 200), 1))
}

# Fungsi untuk fuzzifikasi
fuzzifikasi <- function(data) {
  categories <- c("Baik", "Sedang", "Tidak Sehat", "Sangat Tidak Sehat", "Berbahaya")
  membership_degrees <- matrix(0, nrow = nrow(data), ncol = length(categories))
  
  # Iterasi melalui setiap parameter kandungan udara
  for (i in 1:ncol(data)) {
    # Hitung derajat keanggotaan untuk setiap kategori
    for (j in 1:length(categories)) {
      fuzzy_function <- switch(categories[j],
                               "Baik" = fuzzy_Baik,
                               "Sedang" = fuzzy_Sedang,
                               "Tidak Sehat" = fuzzy_TidakSehat,
                               "Sangat Tidak Sehat" = fuzzy_SangatTidakSehat,
                               "Berbahaya" = fuzzy_Berbahaya)
      membership_degrees[, j] <- pmax(membership_degrees[, j], fuzzy_function(data[, i]))
    }
  }
  
  # Kategorikan berdasarkan derajat keanggotaan tertinggi
  categorized <- apply(membership_degrees, 1, function(x) {
    max_index <- which.max(x)
    if (max_index == 0) {
      return("Tidak Dikategorikan")
    } else {
      return(categories[max_index])
    }
  })
  
  return(categorized)
}

# Contoh data PM10, Pm_2.5, so2, co, o3, no2
pm10 <- Data1$pm_10
pm25 <- Data1$Pm_2.5
so2 <- Data1$so2
co <- Data1$co
o3 <- Data1$o3
no2 <- Data1$no2

# Gabungkan data menjadi satu dataframe
data_udara <- data.frame(pm10, pm25, so2, co, o3, no2)

# Hapus baris dengan nilai kosong (NA)
data_udara <- na.omit(data_udara)

# Fuzzifikasi dan kategorisasi
hasil_kategorisasi <- fuzzifikasi(data_udara)

# Tampilkan hasil kategorisasi
print("Hasil Kategorisasi:")
print(hasil_kategorisasi)

