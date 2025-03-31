#.rs.restartR()
rm(list = ls())#Apagar tudo que esta na memoria do R
cat("\014")#Limpar o console
#install.packages("tidyverse")
library("tidyverse")
library("data.table")
library("abjutils")
#setwd("~/Github/DadosCenso2022")
#url <- "https://raw.githack.com/fsbmat-ufv/Censo2022/refs/heads/main/Dados/Agregados_por_municipios_alfabetizacao_BR.csv"
#url <- "~/GitHub/Censo2022/Dados/Agregados_por_setores_alfabetizacao_BR.csv"
url <- "Dados/Populacao_residente_por_situacao_do_domicilio_municipios.csv"
library(data.table)

arquivo <- "Dados/RO/Amostra_Domicilios_11.txt"

# Vetores widths e colnames (inserir os acima aqui)
widths <- c(
        2, 5, 13, 8, 16, 1, 2, 3, 2, 1, 2, 2, 1, 6, 9, 1, 2, 3, 2, 3,
        1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2,
        1, 1, 7, 10, 8, 9, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,1)

col_names <- c(
        "V0001", "V0002", "V0011", "V0300", "V0010", "V1001", "V1002", "V1003", "V1004", "V1006",
        "V4001", "V4002", "V0201", "V2011", "V2012", "V0202", "V0203", "V6203", "V0204", "V6204",
        "V0205", "V0206", "V0207", "V0208", "V0209", "V0210", "V0211", "V0212", "V0213", "V0214",
        "V0215", "V0216", "V0217", "V0218", "V0219", "V0220", "V0221", "V0222", "V0301", "V0401",
        "V0402", "V0701", "V6529", "V6530", "V6531", "V6532", "V6600", "V6210", "M0201", "M2011",
        "M0202", "M0203", "M0204", "M0205", "M0206", "M0207", "M0208", "M0209", "M0210", "M0211",
        "M0212", "M0213", "M0214", "M0215", "M0216", "M0217", "M0218", "M0219", "M0220", "M0221",
        "M0222", "M0301", "M0401", "M0402", "M0701", "V1005")
length(col_names)

# Leitura do arquivo posicional

dados <- readr::read_fwf(
        file = arquivo,
        fwf_widths(widths, col_names = col_names),  # <- aqui sim colocamos os nomes
        col_types = cols(.default = "c"),           # lê tudo como caractere para evitar problemas
        progress = TRUE
)


sum(widths)
linha <- readLines(arquivo, n = 1)
nchar(linha)
# Converter para data.table (opcional, mas útil!)
dados <- as.data.table(dados)
print(dados)

# Visualize as primeiras linhas
print(head(dados))
linhas <- readLines(arquivo, n = 5)
cat(linhas, sep = "\n")

dt <- fread(url,
            encoding = "Latin-1",
            sep = ";",
            header = TRUE)

#dt <- dt %>% select(c(1, 3:15, 107:119))
#names(dt)
#dt$NM_MUN <- str_trim(dt$NM_MUN) #Remove espacos em Branco
#dt$NM_MUN <- str_to_upper(dt$NM_MUN) #Caixa Alta
#dt$NM_MUN <- rm_accent(dt$NM_MUN) #Remove acentos
#Coluna Total Brasil

dt <- dt %>%
        mutate(Total = rowSums(select(., V00644:V00656)))
Total <- dt %>% select(Total) %>% summarise(sum(Total))
#Coluna Total de Alfabetizados do Brasil

dt <- dt %>%
        mutate(Alfab = rowSums(select(., V00748:V00760)))
Alfab <- dt %>% select(Alfab) %>% summarise(sum(Alfab))

TXTotalNAlf <- round(((Total-Alfab)/Total)*100, digits = 2)


#Coluna 65 anos ou mais por cidade

dt <- dt %>%
        mutate(V06599 = rowSums(select(., V00654, V00655, V00656)))

#Coluna 65 anos ou mais por cidade de alfabetizados
dt <- dt %>%
        mutate(V07599 = rowSums(select(., V00758, V00759, V00760)))

#Taxa de Analfabetismo por Cidade por faixa etaria
# dt[dt == 'X'] <- 0
# str(dt$V00644)
# dt$V00644 <- as.numeric(as.character(dt$V00644))
Total1519 <- sum(dt$V00644)
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

#Totais por cidade do Brasil

dt <- dt %>%
        mutate(Total = rowSums(select(., 3:15)))
dt <- dt %>%
        mutate(Alfabetizados = rowSums(select(., 16:28)))
dt <- dt %>% 
        mutate(NAlfabetizados = Total - Alfabetizados)
dt$TXAnalf <- round(100*((dt$Total-dt$Alfabetizados)/dt$Total), digits = 5)
#Taxa de Analfabetismo no Brasil por faixa etaria

TotBr1519 <- round(100*(sum(dt$V00644)-sum(dt$V00748))/sum(dt$V00644), digits = 1)
TotBr2024 <- round(100*(sum(dt$V00645)-sum(dt$V00749))/sum(dt$V00645), digits = 1)
TotBr2529 <- round(100*(sum(dt$V00646)-sum(dt$V00750))/sum(dt$V00646), digits = 1)
TotBr3034 <- round(100*(sum(dt$V00647)-sum(dt$V00751))/sum(dt$V00647), digits = 1)
TotBr3539 <- round(100*(sum(dt$V00648)-sum(dt$V00752))/sum(dt$V00648), digits = 1)
TotBr4044 <- round(100*(sum(dt$V00649)-sum(dt$V00753))/sum(dt$V00649), digits = 1)
TotBr4549 <- round(100*(sum(dt$V00650)-sum(dt$V00754))/sum(dt$V00650), digits = 1)
TotBr5054 <- round(100*(sum(dt$V00651)-sum(dt$V00755))/sum(dt$V00651), digits = 1)
TotBr5559 <- round(100*(sum(dt$V00652)-sum(dt$V00756))/sum(dt$V00652), digits = 1)
TotBr6064 <- round(100*(sum(dt$V00653)-sum(dt$V00757))/sum(dt$V00653), digits = 1)
TotBr6599 <- round(100*(sum(dt$V06599)-sum(dt$V07599))/sum(dt$V06599), digits = 1)
#TotBr6599 <- round(100*(sum(c(dt$V00654, dt$V00655, dt$V00656))-sum(c(dt$V00758, dt$V00759, dt$V00760)))/sum(c(dt$V00654, dt$V00655, dt$V00656)), digits = 1)

# Criar o data.frame
resultado <- data.frame(
        "15 a 19 anos" = TotBr1519,
        "20 a 24 anos" = TotBr2024,
        "25 a 29 anos" = TotBr2529,
        "30 a 34 anos" = TotBr3034,
        "35 a 39 anos" = TotBr3539,
        "40 a 44 anos" = TotBr4044,
        "45 a 49 anos" = TotBr4549,
        "50 a 54 anos" = TotBr5054,
        "55 a 59 anos" = TotBr5559,
        "60 a 64 anos" = TotBr6064,
        "65 ou mais"  = TotBr6599,
        row.names = "Censo 2022", # Define o nome da linha como 'Censo 2022'
        check.names = FALSE
)


# Exibir o resultado
htmlTable::htmlTable(resultado, caption = "Taxa de Não Alfabetizados")

#url2 <- "~/GitHub/Censo2022/Dados/Tab1_1_1_2010.csv"
url2 <- "https://raw.githack.com/fsbmat-ufv/Censo2022/refs/heads/main/Dados/Tab1_1_1_2010.csv"
# Passo 1: Pegue o cabeçalho correto
colunas <- names(fread(url2, encoding = "Latin-1", sep = ";", header = TRUE, nrows = 1))

# Passo 2: Leia o arquivo, pulando as 6 primeiras linhas
dt2010 <- fread(url2, 
                encoding = "Latin-1", 
                sep = ";", 
                header = FALSE,
                dec = ",",
                skip = 7)

# Passo 3: Atribua os nomes de coluna corretos ao data.table
setnames(dt2010, colunas)
dt2010[, Idade := trimws(Idade)]
Tot10Br1519 <- 100*dt2010[Idade == "15 a 19 anos", NuncaFreq]/dt2010[Idade == "15 a 19 anos", Total]
Tot10Br2024 <- 100*dt2010[Idade == "20 a 24 anos", NuncaFreq]/dt2010[Idade == "20 a 24 anos", Total]
Tot10Br2529 <- 100*dt2010[Idade == "25 a 29 anos", NuncaFreq]/dt2010[Idade == "25 a 29 anos", Total]
Tot10Br3034 <- 100*dt2010[Idade == "30 a 39 anos", NuncaFreq]/dt2010[Idade == "30 a 39 anos", Total]
Tot10Br4044 <- 100*dt2010[Idade == "40 a 49 anos", NuncaFreq]/dt2010[Idade == "40 a 49 anos", Total]
Tot10Br5054 <- 100*dt2010[Idade == "50 a 59 anos", NuncaFreq]/dt2010[Idade == "50 a 59 anos", Total]
Tot10Br6064 <- 100*dt2010[Idade == "60 anos ou mais", NuncaFreq]/dt2010[Idade == "60 anos ou mais", Total]


# Passo 3: Atribua os nomes de coluna corretos ao data.table
setnames(dt2010, colunas)
dt2010[, Idade := trimws(Idade)]
Tot10Br1519 <- round(100*dt2010[Idade == "15 a 19 anos", NuncaFreq]/dt2010[Idade == "15 a 19 anos", Total], digits = 2)
Tot10Br2024 <- round(100*dt2010[Idade == "20 a 24 anos", NuncaFreq]/dt2010[Idade == "20 a 24 anos", Total], digits = 2)
Tot10Br2529 <- round(100*dt2010[Idade == "25 a 29 anos", NuncaFreq]/dt2010[Idade == "25 a 29 anos", Total], digits = 2)
Tot10Br3039 <- round(100*dt2010[Idade == "30 a 39 anos", NuncaFreq]/dt2010[Idade == "30 a 39 anos", Total], digits = 2)
Tot10Br4049 <- round(100*dt2010[Idade == "40 a 49 anos", NuncaFreq]/dt2010[Idade == "40 a 49 anos", Total], digits = 2)
Tot10Br5059 <- round(100*dt2010[Idade == "50 a 59 anos", NuncaFreq]/dt2010[Idade == "50 a 59 anos", Total], digits = 2)
Tot10Br6099 <- round(100*dt2010[Idade == "60 anos ou mais", NuncaFreq]/dt2010[Idade == "60 anos ou mais", Total], digits = 2)

resultado2 <- data.frame(
        "15 a 19 anos" = Tot10Br1519,
        "20 a 24 anos" = Tot10Br2024,
        "25 a 29 anos" = Tot10Br2529,
        "30 a 39 anos" = Tot10Br3039,
        "40 a 49 anos" = Tot10Br4049,
        "50 a 59 anos" = Tot10Br5059,
        "60 ou mais anos" = Tot10Br6099,
        row.names = "Censo 2010", # Define o nome da linha como 'Censo 2022'
        check.names = FALSE
)

# Exibir o resultado
htmlTable::htmlTable(resultado2, caption = "Taxa de Não Alfabetizados")

# Dados
dados <- data.frame(
        faixa_etaria = c("15 a 19 anos", "20 a 24 anos", "25 a 29 anos", "30 a 34 anos", "35 a 39 anos",
                         "40 a 44 anos", "45 a 49 anos", "50 a 54 anos", "55 a 59 anos", 
                         "60 a 64 anos", "65 anos ou mais"),
        ano_2000 = c(5.0, 6.7, 8.0, 9.7, 10.8, 12.4, 15.7, 20.3, 25.5, 29.1, 38.0),
        ano_2010 = c(as.numeric(unlist(resultado2))[1:3],rep(0,8)),
        ano_2022 = as.numeric(unlist(resultado))
)

# Transformando os dados para o formato longo
dados_long <- dados %>%
        tidyr::pivot_longer(cols = starts_with("ano"), 
                            names_to = "ano", 
                            values_to = "taxa_analfabetismo") %>%
        mutate(ano = gsub("ano_", "", ano))  # Remove o prefixo "ano_"

# Criando o gráfico
ggplot(dados_long, aes(x = faixa_etaria, y = taxa_analfabetismo, fill = ano)) +
        geom_bar(stat = "identity", position = position_dodge()) +
        geom_text(
                aes(label = taxa_analfabetismo),
                position = position_dodge(width = 0.9),
                vjust = -0.5, # Ajuste para posicionar acima das barras
                size = 3
        ) +
        scale_fill_manual(values = c("2000" = "#1f78b4", "2010" = "#a6cee3", "2022" = "#ffbf00")) +
        labs(
                title = "Taxa de analfabetismo das pessoas de 15 anos ou mais de idade,\nsegundo os grupos de idade - Brasil - 2000/2022",
                x = NULL,
                y = "%",
                fill = "Ano"
        ) +
        theme_minimal() +
        theme(
                plot.title = element_text(hjust = 0.5, size = 14),
                axis.text.x = element_text(angle = 45, hjust = 1)
        )

#Taxa de analfabetismo das pessoas de 15 anos ou mais de idade, segundo a cor ou raca
#Coluna Totais Brancos

dt <- dt %>%
        mutate(Branca = rowSums(select(., 
                                       V00657, 
                                       V00662, 
                                       V00667, 
                                       V00672, 
                                       V00677, 
                                       V00682, 
                                       V00687, 
                                       V00692, 
                                       V00697, 
                                       V00702, 
                                       V00707, 
                                       V00712, 
                                       V00717)))
#Coluna Totais Brancos Alfabetizados
dt <- dt %>%
        mutate(BrancaAlf = rowSums(select(., 
                                       V00761, 
                                       V00766, 
                                       V00771, 
                                       V00776, 
                                       V00781, 
                                       V00786, 
                                       V00791, 
                                       V00796, 
                                       V00801, 
                                       V00806, 
                                       V00811, 
                                       V00816, 
                                       V00821)))

#Coluna Totais Pretos

dt <- dt %>%
        mutate(Preto = rowSums(select(., 
                                       V00658, 
                                       V00663, 
                                       V00668, 
                                       V00673, 
                                       V00678, 
                                       V00683, 
                                       V00688, 
                                       V00693, 
                                       V00698, 
                                       V00703, 
                                       V00708, 
                                       V00713, 
                                       V00718)))
#Coluna Totais Preto Alfabetizados
dt <- dt %>%
        mutate(PretoAlf = rowSums(select(., 
                                          V00762, 
                                          V00767, 
                                          V00772, 
                                          V00777, 
                                          V00782, 
                                          V00787, 
                                          V00792, 
                                          V00797, 
                                          V00802, 
                                          V00807, 
                                          V00812, 
                                          V00817, 
                                          V00822)))

#Coluna Totais Amarelos

dt <- dt %>%
        mutate(Amarelo = rowSums(select(., 
                                      V00659, 
                                      V00664, 
                                      V00669, 
                                      V00674, 
                                      V00679, 
                                      V00684, 
                                      V00689, 
                                      V00694, 
                                      V00699, 
                                      V00704, 
                                      V00709, 
                                      V00714, 
                                      V00719)))
#Coluna Totais Amarelos Alfabetizados
dt <- dt %>%
        mutate(AmareloAlf = rowSums(select(., 
                                         V00763, 
                                         V00768, 
                                         V00773, 
                                         V00778, 
                                         V00783, 
                                         V00788, 
                                         V00793, 
                                         V00798, 
                                         V00803, 
                                         V00808, 
                                         V00813, 
                                         V00818, 
                                         V00823)))

#Coluna Totais Pardos

dt <- dt %>%
        mutate(Pardos = rowSums(select(., 
                                        V00660, 
                                        V00665, 
                                        V00670, 
                                        V00675, 
                                        V00680, 
                                        V00685, 
                                        V00690, 
                                        V00695, 
                                        V00700, 
                                        V00705, 
                                        V00710, 
                                        V00715, 
                                        V00720)))
#Coluna Totais Pardos Alfabetizados
dt <- dt %>%
        mutate(PardosAlf = rowSums(select(., 
                                           V00764, 
                                           V00769, 
                                           V00774, 
                                           V00779, 
                                           V00784, 
                                           V00789, 
                                           V00794, 
                                           V00799, 
                                           V00804, 
                                           V00809, 
                                           V00814, 
                                           V00819, 
                                           V00824)))

#Coluna Totais Indigenas

dt <- dt %>%
        mutate(Indigenas = rowSums(select(., 
                                       V00661, 
                                       V00666, 
                                       V00671, 
                                       V00676, 
                                       V00681, 
                                       V00686, 
                                       V00691, 
                                       V00696, 
                                       V00701, 
                                       V00706, 
                                       V00711, 
                                       V00716, 
                                       V00721)))
#Coluna Totais Indigenas Alfabetizados
dt <- dt %>%
        mutate(IndigenasAlf = rowSums(select(., 
                                          V00765, 
                                          V00770, 
                                          V00775, 
                                          V00780, 
                                          V00785, 
                                          V00790, 
                                          V00795, 
                                          V00800, 
                                          V00805, 
                                          V00810, 
                                          V00815, 
                                          V00820, 
                                          V00825)))

#Taxa Brancos nao alfabetizados
TxBrBran      <- round(100*(sum(dt$Branca)-sum(dt$BrancaAlf))/sum(dt$Branca), digits = 1)
TxBrPret      <- round(100*(sum(dt$Preto)-sum(dt$PretoAlf))/sum(dt$Preto), digits = 1)
TxBrAmarelo   <- round(100*(sum(dt$Amarelo)-sum(dt$AmareloAlf))/sum(dt$Amarelo), digits = 1)
TxBrPardos    <- round(100*(sum(dt$Pardos)-sum(dt$PardosAlf))/sum(dt$Pardos), digits = 1)
TxBrIndigenas <- round(100*(sum(dt$Indigenas)-sum(dt$IndigenasAlf))/sum(dt$Indigenas), digits = 1)

cor <- c(TxBrBran, TxBrPret, TxBrAmarelo, TxBrPardos, TxBrIndigenas)


# Dados
dados <- data.frame(
        Descricao = c("Total", "Branca", "Preta", "Amarela", "Parda",
                         "Indígena"),
        ano_2010 = c(9.6,5.9,14.4,8.7,13,23.3),
        ano_2022 = as.numeric(c(TXTotalNAlf,cor)))

# Garantindo a ordem específica da coluna Descricao
dados$Descricao <- factor(dados$Descricao, levels = c("Total", "Branca", "Preta", "Amarela", "Parda", "Indígena"))

# Transformando os dados para o formato longo
dados_long <- dados %>%
        tidyr::pivot_longer(cols = starts_with("ano"), 
                            names_to = "ano", 
                            values_to = "taxa_analfabetismo") %>%
        mutate(ano = gsub("ano_", "", ano))  # Remove o prefixo "ano_"

# Criando o gráfico
ggplot(dados_long, aes(x = Descricao, y = taxa_analfabetismo, fill = ano)) +
        geom_bar(stat = "identity", position = position_dodge()) +
        geom_text(
                aes(label = taxa_analfabetismo),
                position = position_dodge(width = 0.9),
                vjust = -0.5, # Ajuste para posicionar acima das barras
                size = 3
        ) +
        scale_fill_manual(values = c("2010" = "#1f78b4", "2022" = "#a6cee3")) +
        labs(
                title = "Taxa de analfabetismo das pessoas de 15 anos ou mais de idade,\nsegundo os grupos de Raça ou Cor - Brasil - 2010/2022",
                x = NULL,
                y = "%",
                fill = "Ano"
        ) +
        theme_minimal() +
        theme(
                plot.title = element_text(hjust = 0.5, size = 14),
                axis.text.x = element_text(angle = 45, hjust = 1)
        )

##################################################################################
library(geobr)
library(sf)
# carregando shape files de todos municipios do Brasil
mun <- read_municipality(code_muni="all", year=2022)
#dt$COD_MUN <- as.numeric(as.character(dt$COD_MUN))
###############Juntando os bancos de dados
dt <- left_join(mun, dt, by= c("code_muni" = "COD_MUN"))
rm(mun)

analf <- dt %>% 
        st_drop_geometry() %>% 
        select(4,50) %>% 
        group_by(abbrev_state) %>% 
        summarise(Tx=sum(dt$TXAnalf)/n(), quant=n()) %>% 
        arrange(Tx)

TotEst <- dt %>% 
        st_drop_geometry() %>% 
        group_by(name_state) %>% 
        summarise(Alf1519=sum(V00644))

#ggplot() + geom_sf(data=dt, aes(fill= alf1519))

library(leaflet)

library(htmltools)  # Necessário para usar `HTML()`
# Corrigir o CRS para WGS84
dt <- dt %>%
        st_transform(crs = 4326)  # EPSG code for WGS84
# Criar o mapa no Leaflet
leaflet(data = dt) %>%
        addTiles()%>%
        setView(lng = -60.7, lat = -15, zoom = 4) %>%  # Adiciona um mapa base
        addPolygons(
                fillColor = ~colorNumeric(palette = "YlGnBu", domain = dt$alf1519)(alf1519),
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
                        "<br><strong>Valor alf1519:</strong> ", round(alf1519, 2)
                ), HTML)
        ) %>%
        addLegend(pal = colorNumeric(palette = "YlGnBu", domain = dt$alf1519),
                  values = ~alf1519,
                  opacity = 0.7,
                  title = "Taxa de Analfabetismo",
                  position = "bottomright")
