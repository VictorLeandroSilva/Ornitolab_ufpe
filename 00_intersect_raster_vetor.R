###############################################################
## descri��o: Script para sobreposi��o entre vetores/raster ###
## Autor: Victor Leandro-Silva ################################
## Data: 26/06/2021 ###########################################
###############################################################

# main dir
setwd("C:/Users/Victor Leandro/Google Drive/Meus Trabalhos/Naka/Stephannie")

#pacotes 
library(tidyverse)
library(sf)
library(raster)

#ler unidades de conserva��o
setwd("C:/Users/Victor Leandro/Google Drive/Meus Trabalhos/Naka/Stephannie") #diret�rio das unidades de conserva��o
dir()

#ler com metadados completos (importnate para dados sobre as UCs)
UCs <- sf::st_read("adasdas.shp", quiet = TRUE)
plot(UCs)

#ler apenas poligno 
Ucs_2 <- raster::shapefile("Ucs_br.shp")
plot(Ucs_2)

# ler shape da esp�cie
setwd("C:/Users/Victor Leandro/Google Drive/Meus Trabalhos/Naka/Stephannie/Shapes") # diret�rio com shape por esp�cie
getwd()
dir()

sp <- raster::shapefile("Agelaioides fringillarius.shp")
area_total <- (raster::area(sp))/10000000 #multiplicar por 10000000 para dar em Km2

#crop da distribui��o pelas Ucs (Ucs onde a esp�ice pode ocorrer)
Dis_uc <- raster::intersect(Ucs_2, sp)
Dis_uc
plot(Dis_uc)
area_uc <- raster::area(Dis_uc) #Vai dar a area de cada unidade de conserva��o que a esp�cie ocorre
area_sp_uc <- (sum(area_uc))/10000000 #multiplicar por 10000000 para dar em Km2

# ler raster (modelo da esp�cie)
#dir
setwd("C:/Users/Victor Leandro/Google Drive/Meus Trabalhos/Naka/Stephannie/Binarios")
dir()

sp_bin <- raster::raster("Agelaioides_fringillarius.gri")
plot(sp_bin)

# retirar �reas com adequabilidade mais n�o pertencente a esp�cie
# Aqui, usei um shape com todos os estados do Brasil do nordeste e mais alguns adjacentes
# Todas distribui��es est�o dentro desse espa�o geogr�fico
setwd("C:/Users/Victor Leandro/Google Drive/Meus Trabalhos/Naka/Stephannie")
dir()

estados <- raster::shapefile("estados.shp")
plot(estados)

# Aqui fazemos o ajuste da area do binario
sp_crop <- raster::crop(sp_bin, estados)
plot(sp_crop)

# Area total do bin�rio
area_mod <- data.frame(tapply(area(sp_crop), sp_crop[], sum))
area_sp_mod <- area_mod[2,1] # como ele calcula a area de 1 (valor de presen�a) e 0 (valor de ausencia), 
                             # precisamos pegar apenas o valor de 1

# transformar os bin�rios em poligno
sp_bin_poly <- raster::rasterToPolygons(sp_crop, fun = function(x){x>0}, na.rm = T)
plot(sp_bin_poly)

#Area dentro das unidade de conserva��o
Dis_mod_uc <- raster::intersect(Ucs_2, sp_bin_poly)
plot(Dis_mod_uc)

#area das UCs que s�o areas de potencial ocorrencia da esp�cie
#consequentimente, area protegida para a esp�cie
area_sp_uc <- (sum(area(Dis_mod_uc)))/10000000

# -----------------------------------------------------------------------------
# Juntando os dois metodos para mapear 

# intersect from raster package
pi <- raster::intersect(sp, sp_bin_poly)
plot(pi)

#area total
area_h1 <- (sum(raster::area(pi)))/10000000

var <- raster::intersect(Ucs_2,pi)
plot(var)

area_h1_uc <- (sum(raster::area(var)))/10000000

# -------------------------------------------------------------------------------
#juntando tudo num csv

setwd("C:/Users/Victor Leandro/Google Drive/Meus Trabalhos/Naka/Stephannie/csvs")
dir()

Taxa <- "A. fringilatus"
area_total # area total shape
area_sp_uc # area shape nas Ucs
area_sp_mod # area total do modelo
area_sp_uc # area modelo nas Ucs
area_h1 # area da inter entre shape e modelo
area_h1_uc # area da interse��o entre shape e modelo nas Ucs

data_area <- data.frame(Taxa,area_total,area_sp_uc,area_sp_mod,area_sp_uc,area_h1,area_h1_uc)

# ------------------------------------------------------------------------------------
#juntando todos os Csvs

setwd("C:/Users/Victor Leandro/Google Drive/Meus Trabalhos/Naka/Stephannie/csvs")
getwd()
dir()

occ.sps <- list.files(pattern="csv")
splist <-unlist(lapply(occ.sps, FUN = strsplit, split=("\\.csv")))
i <- 1

for (i in 1:12) {
  occ.semdup <- read.csv(occ.sps[i], sep = c(","," "))
  write.table(occ.semdup, "Todas.csv", append = T, sep = c(";", ",", " "), row.names = F, col.names = F)
}

