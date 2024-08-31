library(tidyr)
library(haven)
library(dplyr)

setwd("/Users/lorenzomattioli/Documents/Progetti/RxR/Osservatorio/Data Analysis/SHIW-BI-2024")


# Importo dati Banca d'Italia (.dta)

# Importo dati Banca d'Italia (.dta) -------------------------------------------
defl <- read_dta("SHIW-STATA/defl.dta") #delfattori
peso <- read_dta("SHIW-STATA/peso.dta") #pesi campionari (storici)

rper <- read_dta("SHIW-STATA/rper.dta") #reddito personale
cons <- read_dta("SHIW-STATA/cons.dta") #consumo familiare
ricf <- read_dta("SHIW-STATA/ricf.dta") #ricchezza familiare

comp <- read_dta("SHIW-STATA/comp.dta") #composizione familiare (+loc)


# Merge ------------------------------------------------------------------------
## household data
hdata <- full_join(cons, ricf)
hdata <- full_join(hdata, defl)
hdata <- full_join(hdata, peso)

## personal data
pdata <- full_join(hdata, comp)
pdata <- full_join(pdata, rper, by = join_by(nquest, nord, anno))
pdata |> 
  relocate(nord, .after = nquest) -> pdata

## removing source data objects
rm(comp, cons, defl, peso, ricf, rper)
# Labelling and cleaning -------------------------------------------------------

