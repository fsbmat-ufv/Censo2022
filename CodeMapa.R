#.rs.restartR()
rm(list = ls())#Apagar tudo que esta na memoria do R
cat("\014")#Limpar o console
library("tidyverse")
library("data.table")
library("abjutils")
library("geobr")
library("sf")
library("leaflet")
library("htmltools")  # Necessário para usar `HTML()`
library("RColorBrewer")
display.brewer.all()
url <- "Dados/Populacao_residente_por_situacao_do_domicilio_municipios.csv"


dt <- fread(url,
            encoding = "Latin-1",
            sep = ";",
            header = TRUE,
            dec = ",")
head(dt)
# Remove os pontos de milhares na coluna "URBANO" e converte para numérico
dt$URBANO <- as.numeric(gsub("\\.", "", dt$URBANO))
dt$RURAL <- as.numeric(gsub("\\.", "", dt$RURAL))
dt$TOTAL <- as.numeric(gsub("\\.", "", dt$TOTAL))
# carregando shape files de todos municipios do Brasil
mun <- read_municipality(code_muni="all", year=2022)
###############Juntando os bancos de dados
dt <- left_join(mun, dt, by= c("code_muni" = "COD_MUN"))
rm(mun)
dt$Tx <- dt$RURAL/max(dt$RURAL)
#dt$URBANO <- as.numeric(as.character(dt$URBANO))
# Corrigir o CRS para WGS84
dt <- dt %>%
        st_transform(crs = 4326)  # EPSG code for WGS84
# Criar o mapa no Leaflet
leaflet(data = dt) %>%
        addTiles()%>%
        setView(lng = -60.7, lat = -15, zoom = 4) %>%  # Adiciona um mapa base
        addPolygons(
                fillColor = ~colorNumeric(palette = "Reds", domain = dt$Tx)(Tx),
                weight = 0.5,  # Espessura das bordas
                opacity = 2,
                color = "gray",  # Cor da borda
                dashArray = "3",  # Linhas das bordas
                #fillOpacity = 0.7,  # Transparência do preenchimento
                highlight = highlightOptions(weight = 2,
                                             color = "#666",
                                             dashArray = "",
                                             #fillOpacity = 0.9,
                                             bringToFront = TRUE),
                label = ~lapply(paste0(
                        "<strong>Município:</strong> ", name_muni,
                        "<br><strong>Estado:</strong> ",name_state,
                        "<br><strong>Pop. Urbana:</strong> ", URBANO,
                        "<br><strong>Pop. Rural:</strong> ", RURAL,
                        "<br><strong>Pop. Total:</strong> ", TOTAL), 
                        HTML)
        ) %>%
        addLegend(pal = colorNumeric(palette = "Reds", domain = dt$RURAL),
                  values = ~RURAL,
                  opacity = 0.7,
                  title = "População Rural",
                  position = "bottomright")#%>%
        #addLegend(pal = colorNumeric(palette = "Reds", domain = dt$URBANO),
        #          values = ~URBANO,
        #          opacity = 0.7,
        #          title = "População Urbana",
        #          position = "bottomright")
