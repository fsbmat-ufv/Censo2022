#.rs.restartR()
rm(list = ls())#Apagar tudo que esta na memoria do R
cat("\014")#Limpar o console
#install.packages("tidyverse")
library("tidyverse")
library("data.table")
library("abjutils")
#setwd("~/Github/DadosCenso2022")
url <- "~/GitHub/Censo2022/DicionariosDados/Agregados_por_municipios_alfabetizacao_BR.csv"
dt <- fread(url,
            encoding = "Latin-1",
            sep = ";",
            header = TRUE)
dt <- dt %>% select(c(1, 3:15, 107:119))
names(dt)
#dt$NM_MUN <- str_trim(dt$NM_MUN) #Remove espacos em Branco
#dt$NM_MUN <- str_to_upper(dt$NM_MUN) #Caixa Alta
#dt$NM_MUN <- rm_accent(dt$NM_MUN) #Remove acentos
#Coluna 65 anos ou mais por cidade

dt <- dt %>%
        mutate(V06599 = rowSums(select(., V00654, V00655, V00656)))
#Coluna 65 anos ou mais por cidade de alfabetizados
dt <- dt %>%
        mutate(V07599 = rowSums(select(., V00758, V00759, V00760)))

#Taxa de Analfabetismo por Cidade por faixa etaria

dt$alf1519 <- round(100*((dt$V00644-dt$V00748)/dt$V00644), digits = 1)
dt$alf2024 <- round(100*((dt$V00645-dt$V00748)/dt$V00645), digits = 1)
dt$alf2529 <- round(100*((dt$V00646-dt$V00748)/dt$V00646), digits = 1)
dt$alf3034 <- round(100*((dt$V00647-dt$V00748)/dt$V00647), digits = 1)
dt$alf3539 <- round(100*((dt$V00648-dt$V00748)/dt$V00648), digits = 1)
dt$alf4044 <- round(100*((dt$V00649-dt$V00748)/dt$V00649), digits = 1)
dt$alf4549 <- round(100*((dt$V00650-dt$V00748)/dt$V00650), digits = 1)
dt$alf5054 <- round(100*((dt$V00651-dt$V00748)/dt$V00651), digits = 1)
dt$alf5559 <- round(100*((dt$V00652-dt$V00748)/dt$V00652), digits = 1)
dt$alf6064 <- round(100*((dt$V00653-dt$V00748)/dt$V00653), digits = 1)
dt$alf6599 <- round(100*((dt$V06599-dt$V07599)/dt$V06599), digits = 1)

#Taxa de Analfabetismo no Brasil por faixa etaria

#TotBr1519 <- round(100*(sum(dt$V00644)-sum(dt$V00748))/sum(dt$V00644), digits = 1)
#TotBr2024 <- round(100*(sum(dt$V00645)-sum(dt$V00749))/sum(dt$V00645), digits = 1)
#TotBr2529 <- round(100*(sum(dt$V00646)-sum(dt$V00750))/sum(dt$V00646), digits = 1)
#TotBr3034 <- round(100*(sum(dt$V00647)-sum(dt$V00751))/sum(dt$V00647), digits = 1)
#TotBr3539 <- round(100*(sum(dt$V00648)-sum(dt$V00752))/sum(dt$V00648), digits = 1)
#TotBr4044 <- round(100*(sum(dt$V00649)-sum(dt$V00753))/sum(dt$V00649), digits = 1)
#TotBr4549 <- round(100*(sum(dt$V00650)-sum(dt$V00754))/sum(dt$V00650), digits = 1)
#TotBr5054 <- round(100*(sum(dt$V00651)-sum(dt$V00755))/sum(dt$V00651), digits = 1)
#TotBr5559 <- round(100*(sum(dt$V00652)-sum(dt$V00756))/sum(dt$V00652), digits = 1)
#TotBr6064 <- round(100*(sum(dt$V00653)-sum(dt$V00757))/sum(dt$V00653), digits = 1)
#TotBr6599 <- round(100*(sum(dt$V06599)-sum(dt$V07599))/sum(dt$V06599), digits = 1)
#TotBr6599 <- round(100*(sum(c(dt$V00654, dt$V00655, dt$V00656))-sum(c(dt$V00758, dt$V00759, dt$V00760)))/sum(c(dt$V00654, dt$V00655, dt$V00656)), digits = 1)

##################################################################################
library(geobr)
# carregando shape files de todos municipios do Brasil
mun <- read_municipality(code_muni="all", year=2022)
###############Juntando os bancos de dados
dt <- left_join(mun, dt, by= c("code_muni" = "CD_MUN"))
rm(mun)
ggplot() + geom_sf(data=dt, aes(fill= alf1519))

library(leaflet)
library(sf)
library(htmltools)  # Necessário para usar `HTML()`
# Corrigir o CRS para WGS84
dt <- dt %>%
        st_transform(crs = 4326)  # EPSG code for WGS84
# Criar o mapa no Leaflet
leaflet(data = dt) %>%
        addTiles() %>%  # Adiciona um mapa base
        addPolygons(
                fillColor = ~colorNumeric(palette = "YlGnBu", domain = dt$alf1519)(alf1519),
                weight = 0.5,  # Espessura das bordas
                opacity = 1,
                color = "gray",  # Cor da borda
                dashArray = "3",  # Linhas das bordas
                fillOpacity = 0.7,  # Transparência do preenchimento
                highlight = highlightOptions(weight = 2,
                                             color = "#666",
                                             dashArray = "",
                                             fillOpacity = 0.9,
                                             bringToFront = TRUE),
                label = ~lapply(paste0(
                        "<strong>Município:</strong> ", name_muni,
                        "<br><strong>Valor alf1519:</strong> ", round(alf1519, 2)
                ), HTML)
        ) %>%
        addLegend(pal = colorNumeric(palette = "YlGnBu", domain = dt$alf1519),
                  values = ~alf1519,
                  opacity = 0.7,
                  title = "alf1519",
                  position = "bottomright")
