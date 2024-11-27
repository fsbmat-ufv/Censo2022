.rs.restartR()
rm(list = ls())#Apagar tudo que esta na memoria do R
cat("\014")#Limpar o console
#install.packages("tidyverse")
library("tidyverse")
library("data.table")
library("abjutils")
#setwd("~/Github/DadosCenso2022")
url <- "~/Github/DadosCenso2022/Agregados_por_municipios_alfabetizacao_BR.csv"
dt <- fread(url,
            encoding = "Latin-1",
            sep = ";",
            header = TRUE)
#str(dt)
dt$NM_MUN <- str_trim(dt$NM_MUN)
dt$NM_MUN <- str_to_upper(dt$NM_MUN)
dt$NM_MUN <- rm_accent(dt$NM_MUN)

