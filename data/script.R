library(covid19br)
library(tidyverse)
library(rvest)
library(leaflet)
library(brazilmaps)
library(sf)
library(rio)
library(readxl)
library(htmlwidgets)


cidades <- downloadCovid19("cities")

cidades <- cidades %>%
  filter(state == "SE" & city_code != 280000,
         date== max(date)) %>% 
  select(city, city_code, date, newCases, newDeaths, accumCases, accumDeaths, pop) %>% 
  mutate(mortes_100k_hab = accumDeaths/pop*10^5,
         casos_100k_hab = accumCases/pop*10^5)



################# DOWNLOAD ################# 
## Dados da vacinação:  https://opendatasus.saude.gov.br/dataset/covid-19-vacinacao

site <- read_html("https://opendatasus.saude.gov.br/dataset/covid-19-vacinacao/resource/a5f0bb2a-f6c2-4f28-b3da-bc79462c3774")

link_SE <- site %>% html_nodes(xpath="//a[contains(text(), 'Dados SE -')]") %>% html_attr("href")
destinos <- c("vacina_SE_1.csv", 
              "vacina_SE_2.csv",
              "vacina_SE_3.csv",
              "vacina_SE_4.csv",
              "vacina_SE_5.csv")

Map(function(u, d) download.file(u, d, mode="wb"), link_SE, destinos)
################# DOWNLOAD ################# 


# Lendo arquivos da vacina: library(rio)
vacinas <- import_list(dir(pattern = ".csv"), rbind = TRUE, encoding = "UTF-8")


## Apagando os arquivos baixados            
file.remove(c('vacina_SE_1.csv', 'vacina_SE_2.csv', 'vacina_SE_3.csv', 'vacina_SE_4.csv', 'vacina_SE_5.csv'))
#################################


vacinas <- vacinas %>% 
  select(city_code=estabelecimento_municipio_codigo, vacina=vacina_descricao_dose, data=vacina_dataAplicacao)

municipios = cidades %>% 
  select(city, city_code)

vacinas <- left_join(vacinas, municipios, by ="city_code")

vacinas <- vacinas %>% 
  select(-city_code) %>% 
  filter(vacina != "")
  
tipo_vacina <- read_excel("data/tipo_vacina.xlsx")

vacinas <- left_join(vacinas, tipo_vacina, by ="vacina")

vacinas <- vacinas %>% 
    select(data, city, vacina=vacina_ajustada)

vacinas_aplicadas <- vacinas %>% 
  group_by(city) %>%
  summarise(
    vacinas_aplicadas = n(),
    vacinados_1dose = sum(vacina == "1ª dose"),
   # vacinados_2dose = sum(vacina == "2ª dose"),
   # vacinados_doseunica = sum(vacina == "Dose única"),
   # vacinados_1doseoudoseunica = sum(vacina == "1ª dose" | vacina == "Dose única"),
    vacinados_2doseoudoseunica = sum(vacina == "2ª dose" | vacina == "Dose única"),
    vacinados_3dose = sum(vacina == "3ª dose"),
    vacinados_4dose = sum(vacina == "4ª dose"),
    vacinados_5dose = sum(vacina == "5ª dose")
    )
    

#Left join pra levar dados da vacina pra o banco geral, com base no nome dos Municípios
Sergipe <- left_join(cidades, vacinas_aplicadas, by = c("city" = "city"))

# Criar proporcoes de vacinados
Sergipe <- Sergipe %>% 
  mutate(prop_1dose = round(vacinados_1dose/pop*100,2),
         prop_2doseoudoseunica = round(vacinados_2doseoudoseunica/pop*100,2),
         prop_3dose = round(vacinados_3dose/pop*100,2),
         prop_4dose = round(vacinados_4dose/pop*100,2),
         prop_5dose = round(vacinados_5dose/pop*100,2),
         Rank_casos = round(rank(desc(accumCases))),
         Rank_casos_100k_hab = round(rank(desc(casos_100k_hab))),
         Rank_novos_casos = ifelse(newCases>0, round(rank(desc(newCases))), "-"),
         Rank_mortes = round(rank(desc(accumDeaths))),
         Rank_mortes_100k_hab = round(rank(desc(mortes_100k_hab))),
         Rank_novas_mortes = ifelse(newDeaths>0, round(rank(desc(newDeaths))), "-"),
         Rank_vacinas_aplicadas = round(rank(desc(vacinas_aplicadas))),
         Rank_prop_1dose = round(rank(desc(prop_1dose))),
         Rank_prop_2doseoudoseunica = round(rank(desc(prop_2doseoudoseunica))),
         Rank_prop_3dose = round(rank(desc(prop_3dose))),
         Rank_prop_4dose = round(rank(desc(prop_4dose))),
         Rank_prop_5dose = round(rank(desc(prop_5dose)))
  )



Sergipe$city_code <- as.character(Sergipe$city_code)
# pegando as geometrias das cidades de Sergipe (28)
shp <- get_brmap("City", geo.filter = list(State = 28))
shp$City <- as.character(shp$City)
shp$City <- substr(shp$City, 1, nchar(shp$City)-1)


# definindo que o dataframe contem dados geometricos
shp_sf <- st_as_sf(shp)%>%
  st_transform(4326)
#unindo os dados de COVID-19 com as geometrias das cidades.
shp_sf <- shp_sf %>% filter(City %in% Sergipe$city_code)
shp_sf <- left_join(shp_sf,Sergipe, by = c("City" = "city_code"))

## define cores para cada conjunto numerico
pal <- colorNumeric(palette = "Reds", domain = shp_sf$prop_2doseoudoseunica)


# heatmap dos vacinados com pelo menos a segunda dose ou dose única
mapa_vacina <- leaflet(shp_sf, options = leafletOptions(attributionControl=FALSE)) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = shp_sf,
              smoothFactor = 0.5,
              fillOpacity = 0.5,
              weight = 0.5,
              color = ~pal(prop_2doseoudoseunica),
              opacity = 0.8,
              highlightOptions = highlightOptions(color = "black",
                                                  weight = 2,
                                                  bringToFront = TRUE),
              popup = ~paste0(sep = " ",
                              "<b>Município: </b>", city, "<br>",
                              "<b>Vacinas aplicadas: </b>", format(vacinas_aplicadas, big.mark='.', decimal.mark=',', scientific=FALSE), '&nbsp&nbsp&nbsp','(',Rank_vacinas_aplicadas, '°&#41', "<br>",
                              "<b>Vacinados 1ª dose: </b>", format(prop_1dose, big.mark='.', decimal.mark=',', scientific=FALSE), '%&nbsp&nbsp&nbsp','(',Rank_prop_1dose, '°&#41', "<br>",
                              "<b>Vacinados 2ª dose ou dose única: </b>", format(prop_2doseoudoseunica, big.mark='.', decimal.mark=',', scientific=FALSE), '%&nbsp&nbsp&nbsp','(',Rank_prop_2doseoudoseunica, '°&#41', "<br>",
                              "<b>Vacinados 3ª dose: </b>", format(prop_3dose, big.mark='.', decimal.mark=',', scientific=FALSE), '%&nbsp&nbsp&nbsp','(',Rank_prop_3dose, '°&#41', "<br>",
                              "<b>Vacinados 4ª dose: </b>", format(prop_4dose,  big.mark='.', decimal.mark=',', scientific=FALSE), '%&nbsp&nbsp&nbsp','(',Rank_prop_4dose, '°&#41', "<br>",
                              "<b>Vacinados 5ª dose: </b>", format(prop_5dose, big.mark='.', decimal.mark=',', scientific=FALSE), '%&nbsp&nbsp&nbsp','(',Rank_prop_5dose, '°&#41', "<br>",
                              "<b>Casos confirmados: </b>", format(accumCases,  big.mark='.', decimal.mark=',', scientific=FALSE), '&nbsp&nbsp&nbsp','(',Rank_casos, '°&#41', "<br>",
                              "<b>Casos por 100 mil habitantes: </b>", format(round(casos_100k_hab,2),  big.mark='.', decimal.mark=',', scientific=FALSE), '&nbsp&nbsp&nbsp','(',Rank_casos_100k_hab, '°&#41', "<br>",
                              "<b>Novos casos: </b>", format(newCases,  big.mark='.', decimal.mark=',', scientific=FALSE), '&nbsp&nbsp&nbsp','(',Rank_novos_casos, ifelse(newCases>0,'°&#41','&#41'), "<br>",
                              "<b>Mortes: </b>", format(accumDeaths,  big.mark='.', decimal.mark=',', scientific=FALSE), '&nbsp&nbsp&nbsp','(',Rank_mortes, '°&#41', "<br>",
                              "<b>Mortes por 100 mil habitantes: </b>", format(round(mortes_100k_hab,2),  big.mark='.', decimal.mark=',', scientific=FALSE), '&nbsp&nbsp&nbsp','(',Rank_mortes_100k_hab, '°&#41', "<br>",
                              "<b>Novas mortes: </b>", format(newDeaths,  big.mark='.', decimal.mark=',', scientific=FALSE), '&nbsp&nbsp&nbsp','(',Rank_novas_mortes, ifelse(newDeaths>0,'°&#41','&#41'), "<br>"),
              label = ~city) %>% 
  addLegend("bottomright",
            title = "Proporção de vacinados <br>2ª dose ou dose única<br>", 
            pal = pal, 
            values = ~prop_2doseoudoseunica, 
            opacity = 0.8)

    
htmlwidgets::saveWidget(mapa_vacina, 'data/mapa_vacina.html')  
saveRDS(vacinas, 'data/vacinas.rds')
saveRDS(vacinas_aplicadas, 'data/vacinas_aplicadas.rds')
saveRDS(Sergipe, 'data/Sergipe.rds')  
    
