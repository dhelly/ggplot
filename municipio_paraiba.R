#criando o mapa municipal

#carregando as bibliotecas
library(geobr)
library(ggplot2)
library(readxl)
library(dplyr)
library(RColorBrewer)
library(ggspatial)

#vamos baixar os dados do municipio da paraiba
?read_municipality
municipios.pb <- read_municipality(code_muni = 'PB')
pb <- read_state(code_state = 'PB')
covid.pb <- read.csv('covid_20200727.txt', sep = ";", header = F, col.names = c('name_muni', 'total_casos'))
#verificando o tipo de dado
class(municipios.pb)

### lendo o arquivo xls com os dados do idh
list.files()
dados <- read_excel('atlas2013_dadosbrutos_pt.xlsx', sheet = 2)

#filtrando os dados para o ano de 2010 e para a PB
work <- filter(dados, ANO == 2010 & UF == 25)

#usando o pipe
work <- read_excel('atlas2013_dadosbrutos_pt.xlsx', sheet = 2) %>%
  filter(ANO == 2010 & UF == 25)

#filtrando colunas
select(work, c(Codmun7, IDHM))

#usando o pipe
work <- read_excel('atlas2013_dadosbrutos_pt.xlsx', sheet = 2) %>%
  filter(ANO == 2010 & UF == 25) %>%
  select(Codmun7, IDHM)

## Juntando os dados com base em colunas de referência

#renomenando o nome da tabale para ficarem compatível
rename(work, code_muni=Codmun7)

#usando o pipe
work <- read_excel('atlas2013_dadosbrutos_pt.xlsx', sheet = 2) %>%
  filter(ANO == 2010 & UF == 25) %>%
  select(Codmun7, IDHM) %>%
  rename(code_muni = Codmun7)

#juntando os dados
completo <- left_join(municipios.pb, work , by = 'code_muni') %>%
  left_join(covid.pb, by = 'name_muni') %>%
  mutate_if(is.numeric, coalesce, 0)

#plotantdo o mapa por codigo do municipio
ggplot(completo) +
  geom_sf(aes(fill=total_casos, col=total_casos)) +
  scale_color_gradientn(colours = c("blue", "white", "red")) +
  scale_fill_gradientn(colours = c("blue", "white", "red"))

# mapa 2
brewer.pal(9, 'Spectral')
ggplot(completo) +
  geom_sf(aes(fill=IDHM, col=IDHM)) +
  geom_sf(data = pb, fill = 'transparent') +
  scale_color_gradientn(colours = brewer.pal(9, 'Spectral')) +
  scale_fill_gradientn(colours = brewer.pal(9, 'Spectral')) +
  theme_minimal() +
  annotation_scale(location='bl') +
  annotation_north_arrow(location='tl',
                         style= north_arrow_nautical(),
                         width = unit(1.8, 'cm'),
                         height = unit(1.8, 'cm')) +
  labs(title = "IDH Médio da Paraiba")

ggsave(plot = covid.pb.plot, filename = "idhm.pb.png")

# mapa 2
brewer.pal(9, 'Spectral')

covid.pb.plot <- ggplot(completo) +
  geom_sf(aes(fill=total_casos, col=total_casos)) +
  geom_sf(data = pb, fill = 'transparent') +
  scale_color_gradientn(colours = brewer.pal(9, 'Spectral')) +
  scale_fill_gradientn(colours = brewer.pal(9, 'Spectral')) +
  theme_minimal() +
  annotation_scale(location='bl') +
  annotation_north_arrow(location='tl',
                         style= north_arrow_nautical(),
                         width = unit(1.8, 'cm'),
                         height = unit(1.8, 'cm')) +
  labs(title = "Total Casos de Covid Por Município da Paraiba")

ggsave(plot = covid.pb.plot, filename = "covid.pb.png")
