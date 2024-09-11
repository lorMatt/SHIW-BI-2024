library(tidyr)
library(haven)
library(dplyr)
library(expss)
library(laeken)




# setwd("/Users/lorenzomattioli/Documents/Progetti/RxR/Osservatorio/Data Analysis/SHIW-BI-2024")


# Import data Bank of Italy (.dta) -------------------------------------------
defl <- read_dta("SHIW-STATA/defl.dta") #delfattori
peso <- read_dta("SHIW-STATA/peso.dta") #pesi campionari (storici)

rper <- read_dta("SHIW-STATA/rper.dta") #reddito personale
rfam <- read_dta("SHIW-STATA/rfam.dta") #reddito familiare
cons <- read_dta("SHIW-STATA/cons.dta") #consumo familiare
ricf <- read_dta("SHIW-STATA/ricf.dta") #ricchezza familiare


comp <- read_dta("SHIW-STATA/comp.dta") #composizione familiare (+loc)
fami <- read_dta("SHIW-STATA/fami.dta") #consumi familiari (+titolo di studio)


# Dataset building -------------------------------------------------------------
## household data
hdata <- full_join(cons, ricf)
hdata <- full_join(hdata, defl)
hdata <- full_join(hdata, peso)
hdata <- full_join(hdata, rfam)
hdata <- full_join(hdata, fami)

## personal data
pdata <- full_join(hdata, comp)
pdata |> 
  relocate(nord, .after = nquest) -> pdata

## Year subset
pdata |> filter(anno == 2000 | anno == 2002 | anno == 2004 | anno == 2008 |
                  anno == 2010 | anno == 2012 | anno == 2014 | anno == 2016 |
                  anno == 2020) -> pdata
## removing source data objects
rm(comp, cons, defl, peso, ricf, rper, rfam, fami)


# Labeling and cleaning -------------------------------------------------------

# Geographical variables ----

## Region
var_lab(pdata$ireg) = "Region"
mutate(pdata,
       ireg = dplyr::recode(ireg,
                            '1' = 'Piemonte',
                            '2' = 'Val d’Aosta',
                            '3' = 'Lombardia',
                            '4' = 'Trentino - Alto Adige',
                            '5' = 'Veneto',
                            '6' = 'Friuli - Venezia Giulia',
                            '7' = 'Liguria',
                            '8' = 'Emilia - Romagna' ,
                            '9' = 'Toscana',
                            '10' = 'Umbria',
                            '11' = 'Marche',
                            '12' = 'Lazio',
                            '13' = 'Abruzzo',
                            '14' = 'Molise',
                            '15' = 'Campania',
                            '16' = 'Puglia',
                            '17' = 'Basilicata',
                            '18' = 'Calabria',
                            '19' = 'Sicilia',
                            '20' = 'Sardegna'
       )) -> pdata

## Area5
var_lab(pdata$area5) = 'Area5'
mutate(pdata,
       area5 = dplyr::recode(area5,
                             '1' = 'Nordovest',
                             '2' = 'Nordest',
                             '3' = 'Centro',
                             '4' = 'Sud',
                             '5' = 'Isole'
       )) -> pdata

# Socio-demographic variables (Eurostat HH head) ----

## Educational level
pdata$cfedu <- ifelse(pdata$cfeur == 1, pdata$studio, 0) # HH head's educational attainment

pdata |> 
  group_by(nquest, anno) |> 
  mutate(cfedu = max(cfedu)) -> pdata # extending HH head's eduAtt to other HH members

pdata |> 
  select(cfedu, cfedu, cfeur, studio) |> 
  print(n = 50) # checking for mistakes

var_lab(pdata$cfedu) = 'Livello di istruzione del capofamiglia Eurostat'

## Sex
pdata$cfsex <- ifelse(pdata$cfeur == 1, pdata$sesso, 0) ## HH head's sex

pdata |> 
  group_by(anno, nquest) |> 
  mutate(cfsex = max(cfsex)) -> pdata

pdata |> 
  select(cfeur, cfsex)

var_lab(pdata$cfsex) = 'Sesso del capofamiglia Eurostat'

## Occupational class
pdata$cfclass <- ifelse(pdata$cfeur == 1, pdata$qualp10, 0)

pdata |> 
  group_by(nquest, anno) |> 
  mutate(cfclass = max(cfclass)) -> pdata

var_lab(pdata$cfclass) = 'HH head occupational class'

val_lab(pdata$cfclass) = num_lab('
                                 1  operaio o posizione similare
                                 2  impiegato o insegnante
                                 3  impiegato direttivo / quadro
                                 4  dirigente
                                 5  libero professionista
                                 6  imprenditore individuale
                                 7  lavoratore autonomo
                                 8  titolare o coadiuvante di impresa familiare
                                 9  socio o gestore di società
                                10  IN CONDIZIONE NON PROFESSIONALE')

# Target variables ----

# Income ----

## Household equivalent disposable income
pdata$eqhincome <- pdata$y/pdata$nequ #OCSE scale

# Wealth

## Per capita household net wealth 
pdata$pcwealth <- pdata$w/pdata$ncomp

## Per capita household gross financial wealth
pdata$pcfinass <- pdata$af/pdata$ncomp

# inflation adjustment and final labeling
pdata$eqhincome <-pdata$eqhincome*pdata$defl
var_lab(pdata$eqhincome) = 'Reddito disponibile equivalente netto'

pdata$pcwealth <- pdata$pcwealth*pdata$defl
var_lab(pdata$pcwealth) = 'Ricchezza netta pro capite'

pdata$pcfinass <- pdata$pcfinass*pdata$defl
var_lab(pdata$pcfinass) = 'Attività finanziarie pro capite'

# Poverty ----

## Poverty line (60% median income)
pdata |> 
  group_by(anno) |> 
  summarise(povLine = weightedMedian(eqhincome,
                                     weights = pesopop)*0.6) -> povLine
pdata <- left_join(povLine, pdata, by = join_by(anno))
rm(povLine)

## Head count (logical poor = 1, not poor = 0)
table(pdata$povLine, pdata$anno)
pdata$pov <- ifelse(pdata$eqhincome <= pdata$povLine, 1, 0)


# Save pdata
saveRDS(pdata, file = 'SHIWpdata')