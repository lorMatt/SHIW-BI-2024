packages <- c('ggbump','tidyverse','laeken','MetBrewer','sf','ggiraph')
install.packages(setdiff(packages, rownames(installed.packages()))) 
rm(packages)
library(ggbump)
library(tidyverse)
library(laeken)
library(MetBrewer)
library(sf)
library(ggiraph)
# import pdata
pdata <- readRDS('SHIWpdata')
# Inequality indexes -----------------------------------------------------------
## Gini - Income ----
### Over regions by year
as_tibble(gini(pdata$eqhincome,
     weights = pdata$peso,
     years = pdata$anno,
     breakdown = pdata$ireg,
     na.rm = T
)[['valueByStratum']]) -> giniInc

giniInc$stratum <- gsub(' - ', '-', giniInc$stratum)
### ranking
giniInc |> 
  group_by(year) |> 
  mutate(
    rank = rank(value)
  ) -> giniInc
### rounding off value
giniInc$value <- round(giniInc$value, 2)

### ranking5
giniInc$rank5 <- case_match(giniInc$rank,
                            1:4 ~ 1,
                            5:8 ~ 2,
                            9:12 ~ 3,
                            13:16 ~ 4,
                            17:20 ~ 5
                            )
### Total by year
as_tibble_col(gini(pdata$eqhincome,
     weights = pdata$peso,
     years = pdata$anno,
     na.rm = T
)[["value"]]) -> giniIncTot

giniIncTot$stratum <- 'Italia'
giniIncTot$year <- c(2000, 2002, 2004, 2008, 2010, 2012, 2014, 2016, 2020)
giniIncTot$rank <- NA

giniInc <- rbind(giniInc, giniIncTot)
rm(giniIncTot)


## Gini - wealth ----
### Over regions by year
as_tibble(gini(pdata$pcwealth,
               weights = pdata$peso,
               years = pdata$anno,
               breakdown = pdata$ireg,
               na.rm = T
)[['valueByStratum']]) -> giniW

giniW$stratum <- gsub(' - ', '-', giniW$stratum)

### ranking
giniW |> 
  group_by(year) |> 
  mutate(
    rank = rank(value)
  ) -> giniW
### ranking5
giniW$rank5 <- case_match(giniW$rank,
                            1:4 ~ 1,
                            5:8 ~ 2,
                            9:12 ~ 3,
                            13:16 ~ 4,
                            17:20 ~ 5
)

### rounding off value
giniW$value <- round(giniW$value, 2)

### Total by year
as_tibble_col(gini(pdata$pcwealth,
                   weights = pdata$peso,
                   years = pdata$anno,
                   na.rm = T
)[["value"]]) -> giniWTot

giniWTot$stratum <- 'Italia'
giniWTot$year <- c(2000, 2002, 2004, 2008, 2010, 2012, 2014, 2016, 2020)
giniWTot$rank <- NA

giniW <- rbind(giniW, giniWTot)
rm(giniWTot)

## Export backup .csv
write_csv(giniInc, file = 'geoData/giniInc.csv')
write_csv(giniW, file = 'geoData/giniW.csv')



# Poverty  ---------------------------------------------------------------------

## Head count ----
### Over region by year
pdata |> 
  group_by(anno, ireg) |> 
  count(pov) -> pov
pdata |> 
  group_by(anno, ireg) |> 
  count(!pov) -> pov$'!pov'
pov |> 
  mutate('!pov' = `!pov`$n) |> 
  filter(pov == 1) |> 
  mutate(pov = n) |> 
  select(!n) -> pov

pov |> 
  mutate(hCount = pov/(sum(pov, `!pov`))) -> pov

### Ranking
pov |> 
  group_by(anno) |> 
  mutate(
    rankHCount = 21-rank(hCount)
  ) -> pov

## Poverty intensity ----
### povLine
pdata |> 
  group_by(anno) |> 
  summarise(povLine = weightedMedian(eqhincome,
                                     weights = peso)*0.6) -> povLine
pov <- left_join(pov, povLine)
rm(povLine)

### Poverty gap
avPoor <- pdata |>
  filter(pov == 1) |> 
  group_by(anno, ireg) |>
  summarise(avPoor = weightedMean(eqhincome, weights = peso))
pov <- left_join(pov, avPoor)
rm(avPoor)
pov$povGapIndex <- pov$hCount * (pov$povLine - pov$avPoor)/pov$povLine

### Ranking
pov |> 
  group_by(anno) |> 
  mutate(
    rankPovGap = 21-rank(povGapIndex)
  ) -> pov

## LPM risk of being in poverty ----
pdata <- within(pdata, cfedu <- relevel(factor(cfedu), ref = 'Specializzazione post-laurea'))
pdata <- within(pdata, cfsex <- relevel(factor(cfsex), ref = 'Maschile'))
pdataReg <- pdata |> 
  filter(anno == 2020)

table(pdataReg$pov, useNA = 'always')
### Regression models
lpm0 <- lm(pov ~ factor(cfedu),
   weights = pesopop,
   data = pdataReg)
lpm1 <- lm(pov ~ factor(cfedu) + factor(cfsex) + factor(cfclass),
           weights = pesopop,
           data = pdataReg)

summary(lpm0)
summary(lpm1)

### Harvesting results
lpm0results <- as_tibble(summary(lpm0)[["coefficients"]])
lpm0results <- cbind(lpm0results, confint(lpm0, level=0.95)) #adding CIs

lpm1results <- as_tibble(summary(lpm1)[["coefficients"]])
lpm1results <- cbind(lpm1results, confint(lpm1, level=0.95)) #adding CIs
lpm0results <- lpm0results %>% setNames(paste0('0.', names(.)))

### Cleaning results
lpm0results <- lpm0results |> 
  rownames_to_column(var = 'reg')
lpm1results <- lpm1results |> 
  rownames_to_column(var = 'reg')

lpmresults <- full_join(lpm0results, lpm1results)
rm(lpm0results, lpm1results, lpm0, lpm1)

lpmresults['reg'][lpmresults['reg'] == '(Intercept)'] <- ')Intercept'

lpmresults <- lpmresults |> 
  mutate(reg = str_split_fixed(reg, fixed(')'), n = Inf)) 

lpmresults <- within(lpmresults, reg <- reg[,2])

lpmresults$'0.star' <- ifelse(lpmresults$`0.Pr(>|t|)` <= 0.001, '***',
                            ifelse(lpmresults$`0.Pr(>|t|)` <= 0.01, '**',
                                   ifelse(lpmresults$`0.Pr(>|t|)` <= 0.05, '*',
                                          ifelse(lpmresults$`0.Pr(>|t|)` <= 0.1, '.', ''))))

lpmresults$star<- ifelse(lpmresults$`Pr(>|t|)` <= 0.001, '***',
                              ifelse(lpmresults$`Pr(>|t|)` <= 0.01, '**',
                                     ifelse(lpmresults$`Pr(>|t|)` <= 0.05, '*',
                                            ifelse(lpmresults$`Pr(>|t|)` <= 0.1, '.', ''))))

lpmresults <- lpmresults |> 
  relocate('0.star', .before = Estimate)

lpmresults$vars <- c('Intercetta', 'Titolo di studio', 'Titolo di studio', 'Titolo di studio', 'Titolo di studio', 'Titolo di studio', 'Sesso', 'Settore economico dell\'occupazione principale', 'Settore economico dell\'occupazione principale', 'Settore economico dell\'occupazione principale', 'Settore economico dell\'occupazione principale', 'Settore economico dell\'occupazione principale', 'Settore economico dell\'occupazione principale', 'Settore economico dell\'occupazione principale', 'Settore economico dell\'occupazione principale', 'Settore economico dell\'occupazione principale')
# Dataviz ----------------------------------------------------------------------

## Inequality ----

### Income ----
#### Gini index map ----
##### import sf
regMap <- read_sf('~/Documents/Progetti/RxR/Osservatorio/Data Analysis/SHIW-BI 2024/geoData/ISTAT - confini amministrativi/Reg01012024_g/Reg01012024_g_WGS84.shp')

##### merge Gini tibbles with sf data
incMap <- left_join(giniInc, regMap, by = join_by(stratum == DEN_REG))

##### ggiraph ready map
incGiniGG <- incMap |> 
  filter(year == 2000 | year == 2010 |year == 2020) |> 
  drop_na(rank) |> 
  ggplot() +
  geom_sf_interactive(aes(geometry = geometry, fill = value, data_id = stratum, tooltip = value), colour = 'black') +
  facet_wrap(vars(year)) +
  labs(x = NULL, y = NULL,
       title = 'Indice di Gini per regione',
       subtitle = 'Reddito familiare equivalente',
       caption = 'Dati: Banca d\'Italia. Elaborazione di Lorenzo Mattioli - Una Regione per Restare') +
  theme_minimal(base_family = 'Helvetica') +
  scale_fill_viridis_c(direction = -1, limits = c(15, 40), option = 'mako') +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 15,
                                  hjust = .5),
        plot.subtitle = element_text(hjust = .5),
        plot.caption = element_text(size = 9,
                                    hjust = .5))


##### interactive map
girafe(ggobj = incGiniGG,
       width_svg = 13,
       options = list(
         opts_hover(css = ''), ## CSS code of line we're hovering over
         opts_hover_inv(css = "opacity:0.3;"), ## CSS code of all other lines
         opts_tooltip(css = "background-color:white;
                      color:black;
                      font-family:Helvetica;
                      font-style:empty;
                      padding:8px;
                      border-radius:10px;",
                      use_cursor_pos = T),
         opts_toolbar(position = 'bottomright')
       ))

#### Gini index ranking bump ----
##### static
giniInc |> 
  filter(year == 2000 | year == 2004 | year == 2008 | year == 2012 | year == 2016 | year == 2020) |> 
  drop_na(rank) |> 
  ggplot(aes(x = year, y = rank, data_id = stratum)) +
  geom_bump(linewidth = 0.6, color = 'gray90',
            data = ~. |> filter(stratum != 'Umbria')) +
  geom_bump(aes(colour = stratum), linewidth = 0.8,
            data = ~. |> filter(stratum == 'Umbria')) +
  geom_point(color = 'gray90',
             data = ~. |> filter(stratum != 'Umbria'),
             size = 4) +
  geom_point(aes(colour = stratum),
             data = ~. |> filter(stratum == 'Umbria'),
             size = 4) +
  geom_point(color = 'white', size = 2) +
  geom_text_interactive(aes(label = stratum, group = stratum), colour = 'gray90', x = 2021, hjust = 0, size = 3.5, family = 'Helvetica',
                        data = ~. |> filter(year == 2020)) +
  geom_text(aes(label = stratum, group = stratum), colour = 'black', x = 2021, hjust = 0,, size = 3.5, family = 'Helvetica',
            data = ~. |> filter(year == 2020 & stratum == 'Umbria')) +
  scale_color_viridis_d(option = 'mako') +
  scale_x_continuous(limits = c(2000, 2024) ,expand = c(0.01, 0), breaks=c(2000, 2004, 2008, 2012, 2016, 2020)) +
  scale_y_reverse(expand = c(0.02, 0), breaks = c(5, 10, 15, 20)) +
  labs(x = NULL, y = NULL,
       title = 'Classifica delle regioni italiane per indice di Gini',
       subtitle = 'Reddito equivalente familiare',
       caption = 'Dati: Banca d\'Italia. Elaborazione di Lorenzo Mattioli - Una Regione per Restare') +
  theme_minimal(base_family = 'Helvetica') +
  theme(
    legend.position = 'none',
    panel.grid = element_blank(),
    plot.title = element_text(size = 15,
                              hjust = .5),
    plot.subtitle = element_text(hjust = .5),
    plot.caption = element_text(size = 8,
                                hjust = .5)
  )
ggsave('img/plots/incGiniRank.jpeg', width = 10, height = 5)


### Wealth ----
#### Gini index map ----
wMap <- left_join(giniW, regMap, by = join_by(stratum == DEN_REG))

##### ggiraph ready map
wGiniGG <- wMap |> 
  filter(year == 2000 | year == 2010 |year == 2020) |> 
  drop_na(rank) |> 
  ggplot() +
  geom_sf_interactive(aes(geometry = geometry, fill = value, data_id = stratum, tooltip = value), colour = 'black') +
  facet_wrap(vars(year)) +
  labs(x = NULL, y = NULL,
       title = 'Indice di Gini per regione',
       subtitle = 'Ricchezza',
       caption = 'Dati: Banca d\'Italia. Elaborazione di Lorenzo Mattioli - Una Regione per Restare') +
  theme_minimal(base_family = 'Helvetica') +
  scale_fill_viridis_c(direction = -1, limits = c(38, 73), option = 'mako') +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 15,
                                  hjust = .5),
        plot.subtitle = element_text(hjust = .5),
        plot.caption = element_text(size = 9,
                                    hjust = .5))


##### interactive map
girafe(ggobj = wGiniGG,
       width_svg = 13,
       options = list(
         opts_hover(css = ''), ## CSS code of line we're hovering over
         opts_hover_inv(css = "opacity:0.3;"), ## CSS code of all other lines
         opts_tooltip(css = "background-color:white;
                      color:black;
                      font-family:Helvetica;
                      font-style:empty;
                      padding:8px;
                      border-radius:10px;",
                      use_cursor_pos = T),
         opts_toolbar(position = 'bottomright')
       ))

#### Gini index ranking bump ----
giniW |> 
  filter(year == 2000 | year == 2004 | year == 2008 | year == 2012 | year == 2016 | year == 2020) |> 
  drop_na(rank) |> 
  ggplot(aes(x = year, y = rank, data_id = stratum)) +
  geom_bump(linewidth = 0.6, color = 'gray90',
            data = ~. |> filter(stratum != 'Umbria')) +
  geom_bump(aes(colour = stratum), linewidth = 0.8,
            data = ~. |> filter(stratum == 'Umbria')) +
  geom_point(color = 'gray90',
             data = ~. |> filter(stratum != 'Umbria'),
             size = 4) +
  geom_point(aes(colour = stratum),
             data = ~. |> filter(stratum == 'Umbria'),
             size = 4) +
  geom_point(color = 'white', size = 2) +
  geom_text_interactive(aes(label = stratum, group = stratum), colour = 'gray90', x = 2021, hjust = 0, size = 3.5, family = 'Helvetica',
            data = ~. |> filter(year == 2020)) +
  geom_text(aes(label = stratum, group = stratum), colour = 'black', x = 2021, hjust = 0,, size = 3.5, family = 'Helvetica',
            data = ~. |> filter(year == 2020 & stratum == 'Umbria')) +
  scale_color_viridis_d(option = 'mako') +
  scale_x_continuous(limits = c(2000, 2024) ,expand = c(0.01, 0), breaks=c(2000, 2004, 2008, 2012, 2016, 2020)) +
  scale_y_reverse(expand = c(0.02, 0), breaks = c(5, 10, 15, 20)) +
  labs(x = NULL, y = NULL,
       title = 'Classifica delle regioni italiane per indice di Gini',
       subtitle = 'Ricchezza',
       caption = 'Dati: Banca d\'Italia. Elaborazione di Lorenzo Mattioli - Una Regione per Restare') +
  theme_minimal(base_family = 'Helvetica') +
  theme(
    legend.position = 'none',
    panel.grid = element_blank(),
    plot.title = element_text(size = 15,
                              hjust = .5),
    plot.subtitle = element_text(hjust = .5),
    plot.caption = element_text(size = 8,
                                hjust = .5)
  )
ggsave('img/plots/wGiniRank.jpeg', width = 10, height = 5)

## Poverty ----

### Headcount ----
#### Map ----
pov$ireg <- gsub(' - ', '-', pov$ireg)
povMap <- left_join(pov, regMap, by = join_by(ireg == DEN_REG))

##### ggiraph ready map
povMap$hCount <- round(povMap$hCount*100, 2)

povhGG <- povMap |> 
  filter(anno == 2000 | anno == 2010 |anno == 2020) |>
  ggplot() +
  geom_sf_interactive(aes(geometry = geometry, fill = hCount, data_id = ireg, tooltip = hCount), colour = 'black') +
  facet_wrap(vars(anno)) +
  labs(x = NULL, y = NULL,
       title = 'Indice di povertà per regione',
       caption = 'Dati: Banca d\'Italia. Elaborazione di Lorenzo Mattioli - Una Regione per Restare') +
  theme_minimal(base_family = 'Helvetica') +
  scale_fill_viridis_c(direction = -1, limits = c(0, 51), option = 'inferno') +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 15,
                                  hjust = .5),
        plot.subtitle = element_text(hjust = .5),
        plot.caption = element_text(size = 9,
                                    hjust = .5))


##### interactive map
girafe(ggobj = povhGG,
       width_svg = 13,
       options = list(
         opts_hover(css = ''), ## CSS code of line we're hovering over
         opts_hover_inv(css = "opacity:0.3;"), ## CSS code of all other lines
         opts_tooltip(css = "background-color:white;
                      color:black;
                      font-family:Helvetica;
                      font-style:empty;
                      padding:8px;
                      border-radius:10px;",
                      use_cursor_pos = T),
         opts_toolbar(position = 'bottomright')
       ))

#### Ranking  bump ----
pov |> 
  filter(anno == 2000 | anno == 2004 | anno == 2008 | anno == 2012 | anno == 2016 | anno == 2020) |> 
  ggplot(aes(x = anno, y = rankHCount, group = ireg, data_id = ireg)) +
    geom_bump(linewidth = 0.6, color = "gray90", smooth = 6) +
    geom_bump(aes(colour = ireg), linewidth = 0.8, smooth = 6,
                data = ~. |> filter(ireg == 'Umbria')) +
    geom_point(color = "gray90", size = 4) +
    geom_point(aes(colour = ireg),
               data = ~. |> filter(ireg == 'Umbria'),
               size = 4) +
    geom_point(color = 'white', size = 2) +
    geom_text(aes(label = ireg, group = ireg), colour = 'gray90', x = 2021, hjust = 0, size = 3.5, family = 'Helvetica',
              data = ~. |> filter(anno == 2020)) +
    geom_text(aes(label = ireg, group = ireg), colour = 'black', x = 2021, hjust = 0,, size = 3.5, family = 'Helvetica',
              data = ~. |> filter(anno == 2020 & ireg == 'Umbria')) +
    scale_color_manual(values = met.brewer('Degas')) +
    scale_x_continuous(limits = c(2000, 2030) ,expand = c(0.01, 0), breaks=c(2000, 2010, 2020)) +
    scale_y_reverse(expand = c(0.02, 0), breaks = c(1, 5, 10, 15, 20)) +
    labs(x = NULL, y = NULL,
         title = 'Classifica delle regioni italiane per tasso di povertà',
         subtitle = 'Dalla più povera alla meno povera',
         caption = 'Dati: Banca d\'Italia. Elaborazione di Lorenzo Mattioli - Una Regione per Restare') +
    theme_minimal(base_family = 'Helvetica') +
    theme(
      legend.position = 'none',
      panel.grid = element_blank(),
      plot.title = element_text(size = 15,
                           hjust = .5),
      plot.subtitle = element_text(hjust = .5),
      plot.caption = element_text(size = 8,
                                  hjust = .5)
    )

ggsave('img/plots/hCountRank.jpeg', width = 10, height = 5)

### Intensity ----

#### Map ----
##### ggiraph ready map
povMap$povGapIndex <- round(povMap$povGapIndex*100, 2)

povhGG <- povMap |> 
  filter(anno == 2000 | anno == 2010 |anno == 2020) |>
  ggplot() +
  geom_sf_interactive(aes(geometry = geometry, fill = povGapIndex, data_id = ireg, tooltip = povGapIndex), colour = 'black') +
  facet_wrap(vars(anno)) +
  labs(x = NULL, y = NULL,
       title = 'Indice d\'intensità della povertà per regione',
       caption = 'Dati: Banca d\'Italia. Elaborazione di Lorenzo Mattioli - Una Regione per Restare') +
  theme_minimal(base_family = 'Helvetica') +
  scale_fill_viridis_c(direction = -1, limits = c(0, 21), option = 'inferno') +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 15,
                                  hjust = .5),
        plot.subtitle = element_text(hjust = .5),
        plot.caption = element_text(size = 9,
                                    hjust = .5))


##### interactive map
girafe(ggobj = povhGG,
       width_svg = 13,
       options = list(
         opts_hover(css = ''), ## CSS code of line we're hovering over
         opts_hover_inv(css = "opacity:0.3;"), ## CSS code of all other lines
         opts_tooltip(css = "background-color:white;
                      color:black;
                      font-family:Helvetica;
                      font-style:empty;
                      padding:8px;
                      border-radius:10px;",
                      use_cursor_pos = T),
         opts_toolbar(position = 'bottomright')
       ))

#### Ranking bump ----
pov |> 
  filter(anno == 2000 | anno == 2004 | anno == 2008 | anno == 2012 | anno == 2016 | anno == 2020) |> 
  ggplot(aes(x = anno, y = rankPovGap, group = ireg)) +
  geom_bump(linewidth = 0.6, color = "gray90", smooth = 6) +
  geom_bump(aes(colour = ireg), linewidth = 0.8, smooth = 6,
            data = ~. |> filter(ireg == 'Umbria')) +
  geom_point(color = "gray90", size = 4) +
  geom_point(aes(colour = ireg),
             data = ~. |> filter(ireg == 'Umbria'),
             size = 4) +
  geom_point(color = 'white', size = 2) +
  geom_text(aes(label = ireg, group = ireg), colour = 'gray90', x = 2021, hjust = 0, size = 3.5, family = 'Helvetica',
            data = ~. |> filter(anno == 2020)) +
  geom_text(aes(label = ireg, group = ireg), colour = 'black', x = 2021, hjust = 0,, size = 3.5, family = 'Helvetica',
            data = ~. |> filter(anno == 2020 & ireg == 'Umbria')) +
  scale_color_manual(values = met.brewer('Degas')) +
  scale_x_continuous(limits = c(2000, 2030) ,expand = c(0.01, 0), breaks=c(2000, 2010, 2020)) +
  scale_y_reverse(expand = c(0.02, 0), breaks = c(1, 5, 10, 15, 20)) +
  labs(x = NULL, y = NULL,
       title = 'Classifica delle regioni italiane per poverty intensity',
       subtitle = 'Dalla più povera alla meno povera',
       caption = 'Dati: Banca d\'Italia. Elaborazione di Lorenzo Mattioli - Una Regione per Restare') +
  theme_minimal(base_family = 'Helvetica') +
  theme(
    legend.position = 'none',
    panel.grid = element_blank(),
    plot.title = element_text(size = 15,
                              hjust = .5),
    plot.subtitle = element_text(hjust = .5),
    plot.caption = element_text(size = 8,
                                hjust = .5)
  )

ggsave('img/plots/povGapRank.jpeg', width = 10, height = 5)

table(pdataReg$cfedu)
#### LPM margins plot ----
lpmresults$vars <- factor(lpmresults$vars, levels=unique(lpmresults$vars))
lpmresults$roundEst <- round(lpmresults$Estimate, 2)

gglpm <- lpmresults |> 
  filter(vars == 'Titolo di studio') |> 
  ggplot(aes(x=Estimate, y=reg, colour = vars, data_id = roundEst, tooltip = roundEst)) +
  geom_vline(xintercept = 0,
             linewidth = 0.4,
             color = 'gray70') +
  geom_linerange_interactive(aes(xmin=`2.5 %`,xmax=`97.5 %`), linewidth = .6) +
  geom_point_interactive(size = 4) +
  geom_point(colour = 'white', size = 2) +
  geom_point_interactive(aes(x = `2.5 %`), shape = '|', size = 4) +
  geom_point_interactive(aes(x = `97.5 %`), shape = '|', size = 4) +
  labs(x = NULL, y = NULL,
       title = 'Probabilità di incidenza della povertà relativa in base al titolo di studio del capofamiglia',
       subtitle = 'Categoria di riferimento: Specializzazione post-laurea',
       caption = 'Dati: Banca d\'Italia. Elaborazione di Lorenzo Mattioli - Una Regione per Restare') +
  scale_color_manual(values = met.brewer('Degas')) +
  theme_minimal(base_family = 'Helvetica') +
  theme(panel.grid = element_line(),
        legend.position = 'none',
        plot.title = element_text(size = 15,
                                  hjust = 1),
        plot.subtitle = element_text(hjust = -.065),
        plot.caption = element_text(size = 8,
                                    hjust = .5))

girafe(ggobj = gglpm,
       width_svg = 9,
       options = list(
         opts_hover(css = ''), ## CSS code of line we're hovering over
         opts_hover_inv(css = "opacity:0.3;"), ## CSS code of all other lines
         opts_tooltip(css = "background-color:white;
                      color:black;
                      font-family:Helvetica;
                      font-style:empty;
                      padding:8px;
                      border-radius:10px;",
                      use_cursor_pos = T),
         opts_toolbar(position = 'bottomright')))

       