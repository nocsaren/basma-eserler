library(tidyverse)

read_data <- read.table("data.txt", 
                        sep = "\t", 
                        fill = TRUE , 
                        blank.lines.skip = TRUE, 
                        fileEncoding= "windows-1254", 
                        quote = "", 
                        header = TRUE,
                        strip.white = TRUE,
                        na.strings = "",
                        col.names = c("ESER", "YAZAR", "EDIT", "YER", 
                                      "HICRI", "RUMI", "MILADI", 
                                      "MATBAA", "YAYINEVI", "SERI", 
                                      "BASKI", "ESERAD2", "CILT",
                                      "SAYFA", "KATALOG", "KATNO", 
                                      "KATALOG2","KATNO2", "INFO"))

# standardize year formats
read_data$MILADI <- as.numeric(gsub("[^0-9]", "", read_data$MILADI))
read_data$MILADI <- as.numeric(substr(read_data$MILADI, 1, 4))

read_data$HICRI <- as.numeric(gsub("[^0-9]", "", read_data$HICRI))
read_data$HICRI <- as.numeric(substr(read_data$HICRI, 1, 4))

read_data$RUMI <- as.numeric(gsub("[^0-9]", "", read_data$RUMI))
read_data$RUMI <- as.numeric(substr(read_data$RUMI, 1, 4))

#HİCRİ YILIN MİLADİ YILA ÇEVRİLMESİ
#Hicri yılı 33'e bölünüz. 1420 : 33 = 43.03 (=43) (A sayısı)
#A sayısını hicri yıldan çıkarınız. 1420 - 43 = 1377 (B sayısı)
#B sayısını 622 ile toplayınız. 1377 + 622 = 1999


# MİLADİ YILIN HİCRİ YILA ÇEVRİLMESİ
# Miladi yıldan 621 rakamını çıkarınız. 1999 - 621 = 1378 (A sayısı)
# A sayısını 33'e bölünüz. 1378 : 33 = 41.75 (=42) (B sayısı)
# A sayısını B sayısı ile toplayınız. 1378 + 42 = 1420




# calculate calendar cols
for (i in 1:nrow(read_data)) {
  if ((is.na(read_data$HICRI[i]) + is.na(read_data$RUMI[i])) == 1) {
    if (!is.na(read_data$RUMI[i]) && read_data$RUMI[i] > 1500 && is.na(read_data$MILADI[i])){
      read_data$MILADI[i] <- read_data$RUMI[i]
      read_data$RUMI[i] <- read_data$MILADI[i] - 584
      read_data$HICRI[i] <- read_data$MILADI[i] - 584
    }
    else if (!is.na(read_data$HICRI[i]) && read_data$HICRI[i] > 1500 && is.na(read_data$MILADI[i])) {
      read_data$MILADI[i] <- read_data$HICRI[i]
      read_data$HICRI[i] <- ((read_data$MILADI[i] - 621) %/% 33) + (read_data$MILADI[i] - 621)
      read_data$RUMI[i] <- read_data$MILADI[i] - 584
    } 
    else if (is.na(read_data$MILADI[i])) {
      read_data$MILADI[i] <- (read_data$HICRI[i] - (read_data$HICRI[i] %/% 33)) + 622
    }
    else if (is.na(read_data$HICRI[i])) {
      read_data$HICRI[i] <- read_data$RUMI[i]
      }
    else {
      read_data$RUMI[i] <- read_data$HICRI[i]
      }
    }
  else if (is.na(read_data$HICRI[i])) {
    read_data$HICRI[i] <- ((read_data$MILADI[i] - 621) %/% 33) + (read_data$MILADI[i] - 621)
    read_data$RUMI[i] <- ((read_data$MILADI[i] - 621) %/% 33) + (read_data$MILADI[i] - 621)
    }
  else if (is.na(read_data$RUMI[i])) {
    read_data$HICRI[i] <- ((read_data$MILADI[i] - 621) %/% 33) + (read_data$MILADI[i] - 621)
    read_data$RUMI[i] <- read_data$MILADI[i] - 584
  }
}

ggplot(read_data)+
  geom_histogram(mapping = aes(x=MILADI), na.rm = TRUE, stat = "count")

