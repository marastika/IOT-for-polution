
library(e1071)

# Set seed for reproducibility
set.seed(123)

# Jumlah sampel untuk masing-masing skenario
n1 <- 1000
n2 <- 10000
n3 <- 100000

# Skenario n1
CO_n1 <- round(runif(n1, min = 0, max = 500),0)  # Example range for CO
CO2_n1 <- round(runif(n1, min = 0, max = 500),0)  # Example range for CO2

# Skenario n2
CO_n2 <- round(runif(n2, min = 0, max = 500),0)  # Example range for CO
CO2_n2 <- round(runif(n2, min = 0, max = 500),0)  # Example range for CO2

# Skenario n3
CO_n3 <- round(runif(n3, min = 0, max = 500),0)  # Example range for CO
CO2_n3 <- round(runif(n3, min = 0, max = 500),0)  # Example range for CO2

# Combine data into a list or dataframe for each scenario
data_n1 <- data.frame(CO_n1,CO2_n1)
data_n2 <- data.frame(CO_n2, CO2_n2)
data_n3 <- data.frame(CO_n3, CO2_n3)

# Fungsi fuzzyfikasi untuk mengklasifikasikan nilai ISPU ke dalam kategori kualitas udara
fuzzyfikasi_ispu <- function(ispu_value) {
  if (ispu_value >= 0 && ispu_value <= 50) {
    return("Baik")
  } else if (ispu_value >= 51 && ispu_value <= 100) {
    return("Sedang")
  } else if (ispu_value >= 101 && ispu_value <= 199) {
    return("Tidak Sehat")
  } else if (ispu_value >= 200 && ispu_value <= 299) {
    return("Sangat Tidak Sehat")
  } else if (ispu_value >= 300) {
    return("Berbahaya")
  } else {
    return(NA) # Nilai di luar rentang yang valid
  }
}

# Apply fuzzyfikasi to each column n1
data_n1$Kategori_CO <- sapply(data_n1$CO_n1, fuzzyfikasi_ispu)
data_n1$Kategori_CO2 <- sapply(data_n1$CO2_n1, fuzzyfikasi_ispu)


# Apply fuzzyfikasi to each column n2
data_n2$Kategori_CO <- sapply(data_n2$CO_n2, fuzzyfikasi_ispu)
data_n2$Kategori_CO2 <- sapply(data_n2$CO2_n2, fuzzyfikasi_ispu)

# Apply fuzzyfikasi to each column n3
data_n3$Kategori_CO <- sapply(data_n3$CO_n3, fuzzyfikasi_ispu)
data_n3$Kategori_CO2 <- sapply(data_n3$CO2_n3, fuzzyfikasi_ispu)


#---------------------------------Bayes N1---------------------------------------------------#
# Hitung jumlah sampel untuk setiap kategori kualitas udara (Baik, Sedang, Tidak Sehat, Sangat Tidak Sehat, Berbahaya)
count_Baik <- sum(data_n3$Kategori_CO == "Baik")
count_Sedang <- sum(data_n3$Kategori_CO == "Sedang")
count_Tidak_Sehat <- sum(data_n3$Kategori_CO == "Tidak Sehat")
count_Sangat_Tidak_Sehat <- sum(data_n3$Kategori_CO == "Sangat Tidak Sehat")
count_Berbahaya <- sum(data_n3$Kategori_CO == "Berbahaya")

# Hitung total jumlah sampel
total_samples <- nrow(data_n3)

# Hitung probabilitas prior untuk setiap kategori kualitas udara
prior_Baik <- count_Baik / total_samples
prior_Sedang <- count_Sedang / total_samples
prior_Tidak_Sehat <- count_Tidak_Sehat / total_samples
prior_Sangat_Tidak_Sehat <- count_Sangat_Tidak_Sehat / total_samples
prior_Berbahaya <- count_Berbahaya / total_samples

# Output hasil
cat("Probabilitas prior untuk kategori Baik:", prior_Baik, "\n")
cat("Probabilitas prior untuk kategori Sedang:", prior_Sedang, "\n")
cat("Probabilitas prior untuk kategori Tidak Sehat:", prior_Tidak_Sehat, "\n")
cat("Probabilitas prior untuk kategori Sangat Tidak Sehat:", prior_Sangat_Tidak_Sehat, "\n")
cat("Probabilitas prior untuk kategori Berbahaya:", prior_Berbahaya, "\n")

