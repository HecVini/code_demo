# Credits: Vinicius Hector

#### 1. Packages and Directories ####
library(tidyverse) #Main FUNCTIONALITIES
library(lubridate) #To easily work with dates
library(janitor) #To clean databases
library(data.table) #To use fread() function
library(openxlsx) #To work with .xlsx
library(countrycode) #To deal with countries
library(ipeadatar) #Brazilian Economic Reseatch Bureau API
library(zoo) #To deal with timeseries
library(tidylog) #To see what is going on with the data while cleaning
library(httr) 
library(geobr) #Brazilian geographic data
library(textclean) #To easily clean text
library(ggh4x) #ggplot2 extension
library(anytime) #To deal with hours and minutes 
library(geogrid) #To make the hexmap
library(tmap)
library(cartogram) #Plot the dorling cartogram
library(sf) #Geospatial data 


datasets_directory = '/Users/hec_vini/OneDrive - Fundacao Getulio Vargas - FGV/gv_agro/datasets/'
project_directory = '/Users/hec_vini/Library/CloudStorage/OneDrive-Personal/GitHub Repo/HexmapEleicoes2022/'
#Just set your directories over there

'%!in%' = function(x,y)!('%in%'(x,y))
ColorPallete = 'https://coolors.co/04479f-1368aa-4091c9-9dcee2-fedfd4-f0876a-f36e53-f15041-e72923-b60217'

#### 2. Download Original Data ####
municipalities_codes.R = read.xlsx(paste0(datasets_directory,'divisao_territorial_brasileira_v2020.xlsx'),1) %>% tibble() %>% clean_names()
states_codes.R = read.xlsx(paste0(datasets_directory,'divisao_territorial_brasileira_v2020.xlsx'),3) %>% tibble() %>% clean_names()
immediate_region_codes.R = read.xlsx(paste0(datasets_directory,'divisao_territorial_brasileira_v2020.xlsx'),1) %>% tibble() %>% clean_names()
intermediate_region_codes.R = read.xlsx(paste0(datasets_directory,'divisao_territorial_brasileira_v2020.xlsx'),1) %>% tibble() %>% clean_names()
PresidentialResults.R = read.xlsx(paste0(project_directory,'VotacaoPresidente_Municipios_1T2022.xlsx'),1) %>% tibble() %>% clean_names()

immediate_region_shapes.R = read_immediate_region(code_immediate = 'all')
intermediate_region_shapes.R = read_intermediate_region(code_intermediate = 'all')
states_shapes.R = read_state(code_state = 'all')

#### 3. Clean Data ####
### 3.1 Brazilian Regionalization ###
#Datasets with cities, states and regions names and IDs, according to IBGE.
states_codes = states_codes.R %>% setnames(c('id_state','code_state'))
states_codes[28,1] = 1
states_codes[28,2] = 'BR'

municipalities_codes = municipalities_codes.R %>% subset(select = c(codigo_municipio_completo,nome_municipio,uf))
colnames(municipalities_codes) = c('id_municipality','name_municipality','id_state')
municipalities_codes[,c(1,3)] = lapply(municipalities_codes[,c(1,3)], function(x) as.numeric(as.character(x)))
municipalities_codes = right_join(municipalities_codes,states_codes, by = 'id_state')

immediate_region_codes = immediate_region_codes.R %>% 
  subset(select = c(regiao_geografica_imediata,nome_regiao_geografica_imediata,codigo_municipio_completo,uf)) %>%
  setnames(c('id_immediate_region','name_immediate_region','id_municipality','id_state'))
immediate_region_codes[,c(1,3,4)] = lapply(immediate_region_codes[,c(1,3,4)], function(x) as.numeric(as.character(x)))
immediate_region_codes = right_join(immediate_region_codes,states_codes, by = 'id_state')
immediate_region_codes = immediate_region_codes %>% subset(select = -code_state)

intermediate_region_codes = intermediate_region_codes.R %>% 
  subset(select = c(regiao_geografica_intermediaria,nome_regiao_geografica_intermediaria,codigo_municipio_completo,uf)) %>%
  setnames(c('id_intermediate_region','name_intermediate_region','id_municipality','id_state'))
intermediate_region_codes[,c(1,3,4)] = lapply(intermediate_region_codes[,c(1,3,4)], function(x) as.numeric(as.character(x)))
intermediate_region_codes = right_join(intermediate_region_codes,states_codes, by = 'id_state')
intermediate_region_codes = intermediate_region_codes %>% subset(select = -code_state) 

# Polygonals, set with {geobr}
immediate_region_shapes = immediate_region_shapes.R %>% tibble()
immediate_region_shapes = immediate_region_shapes %>% subset(select = c(code_immediate,geom))
immediate_region_shapes = immediate_region_shapes %>% setnames(c('id_immediate_region','geom'))

intermediate_region_shapes = intermediate_region_shapes.R %>% tibble()
intermediate_region_shapes = intermediate_region_shapes %>% subset(select = c(code_intermediate,geom))
intermediate_region_shapes = intermediate_region_shapes %>% setnames(c('id_intermediate_region','geom'))

states_shapes = states_shapes.R %>% tibble()
states_shapes = states_shapes %>% subset(select = c(code_state,geom))
states_shapes = states_shapes %>% setnames(c('id_state','geom'))


## 2.2 Voting Data by Munnicipality ##
PresidentialResults.R = read.xlsx(paste0(project_directory,'VotacaoPresidente_Municipios_1T2022.xlsx'),1) %>% tibble() %>% clean_names()
PresidentialResults = PresidentialResults.R %>% subset(select = c(ibge7,validos,x13:x80))
colnames(PresidentialResults)[c(1,2)] = c('id_municipality','valid_votes')
PresidentialResults[] = lapply(PresidentialResults[], function(x) as.numeric(as.character(x)))
PresidentialResults = PresidentialResults %>% drop_na(id_municipality)
PresidentialResults = PresidentialResults %>% gather(key = 'party', value = 'votes',x13:x80, factor_key=FALSE)
PresidentialResults = PresidentialResults %>% arrange(id_municipality,party) 
PresidentialResults = PresidentialResults %>% 
  mutate(party = case_when(party == 'x13' ~ 'Lula', party == 'x22' ~ 'Bolsonaro', TRUE ~ 'Others'))
PresidentialResults = PresidentialResults %>% group_by(id_municipality,party) %>%
  summarise(valid_votes = valid_votes, votes = sum(votes)) %>% ungroup() %>% unique()
PresidentialResults = left_join(PresidentialResults,immediate_region_codes, by = 'id_municipality')
PresidentialResults = PresidentialResults %>% 
  subset(select = c('id_immediate_region','id_municipality','id_state','valid_votes','party','votes'))
PresidentialResults = PresidentialResults %>% arrange(id_immediate_region,id_municipality,party)
PresidentialResults = PresidentialResults %>% group_by(id_immediate_region,party) %>%
  summarise(valid_votes = sum(valid_votes), votes = sum(votes)) %>% ungroup() %>% unique()
PresidentialResults = PresidentialResults %>% group_by(id_immediate_region) %>%
  mutate(voting_share = (votes/valid_votes)*100) %>% ungroup()
PresidentialResults = PresidentialResults %>% mutate(valid_votes = valid_votes/10^3, votes = votes/10^3)
PresidentialResults = PresidentialResults %>% subset(select = -votes)
PresidentialResults = PresidentialResults %>% pivot_wider(names_from = 'party', values_from = 'voting_share')
PresidentialResults = PresidentialResults %>% 
  mutate(winner_margin = case_when(Bolsonaro > Lula & Bolsonaro < 50 ~ 'B1',
                                   Bolsonaro >= 50 & Bolsonaro < 60 ~ 'B2',
                                   Bolsonaro >= 60 & Bolsonaro < 70 ~ 'B3',
                                   Bolsonaro >= 70 ~ 'B4',
                                   Lula > Bolsonaro & Lula < 50 ~ 'L1',
                                   Lula >= 50 & Lula < 60 ~ 'L2',
                                   Lula >= 60 & Lula < 70 ~ 'L3',
                                   Lula >= 70 ~ 'L4',
                                   Bolsonaro > Lula & Bolsonaro < 50 ~ 'B1'))
# B1 = Bolsonaro won, but got less than 50% of the valid votes, ..., L4 = Lula won with over 70% of the votes.

#### 3. Make the Map ####
## 3.1 Add the polygonals ##
PresidentialResultsMap = left_join(PresidentialResults,immediate_region_shapes, by = 'id_immediate_region') #Merge immediate regions polygonals with voting data
PresidentialResultsMap = PresidentialResultsMap %>% mutate(id_state = str_extract(id_immediate_region, "^.{2}")) #get state IDs
PresidentialResultsMap = PresidentialResultsMap %>% subset(select = c(id_immediate_region,id_state,2:7)) #Reorder

## 3.4 Map 3: Cartogram ##
# Check this out: https://r-charts.com/spatial/cartogram-ggplot2/
PresidentialResultsShapefile = PresidentialResultsMap %>% as.data.frame() %>% st_as_sf() #tibble to sf object
PresidentialResultsCartogram = st_transform(PresidentialResultsShapefile, 3857) #3857 = mercator projection

PresidentialResultsMap3 = 
  cartogram_dorling(PresidentialResultsCartogram, weight = "valid_votes",k = 1.5,m_weight = 1) 
#Set dorling map. the higher k, the closer the circles will be.
#Dots weighted by valid votes

ElectoralResultsCartogram = 
  ggplot() + 
  geom_sf(PresidentialResultsMap3, mapping = aes(geometry = geom, fill = winner_margin),color = '#000000',size = .5) +
  geom_sf(states_shapes,mapping = aes(geometry = geom,fill = NA),color = '#343a40') +
  scale_fill_manual(values = c('B1' = '#9dcee2','B2' = '#4091c9','B3' = '#1368aa','B4' = '#023047',
                               'L1' = '#ff8182','L2' = '#ff0101','L3' = '#c00000','L4' = '#800000')) +
  theme(plot.title = element_text(hjust = 0.5, colour = '#333132', size = 32, margin = unit(c(0.5,0,0,0),'cm')),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        legend.background = element_blank(),
        legend.position = 'none')
ggsave(filename = 'ElectoralResultsCartogram.png',plot = ElectoralResultsCartogram, device = 'png', 
       width = 19.32, height = 12, path = project_directory) 
	   


