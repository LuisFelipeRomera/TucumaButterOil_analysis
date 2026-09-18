setwd("C:/Users/luisr/Documentos/Documents/Documentos - Copia/Projeto_TucTuc/
      TucumaButterOil_analysis")
dataTucTuc <- read.csv("NanoTucumaTucuma.csv", header = TRUE, sep = ",")

#pacote Mixexp
install.packages("mixexp")
library(mixexp)

#pacote performance
library(performance)

#Pacote Dharma
library(DHARMa)

?MixModel

#Abrir tabela em janela lateral
View(dataTucTuc)

mixvars <- c("Tucuma", "Tucuma.1", "Brijo10")

#PDI

#modelo de Scheffe linear

lin <- MixModel(
  dataTucTuc,
  "PDI",
  mixvars,
  1
)

summary(lin)
anova(lin)

par(mfrow = c(2, 2))
plot(lin)

check_model(lin)

#modelo de Scheffe quadratico

quad <- MixModel(
  dataTucTuc,
  "PDI",
  mixvars,
  2
)

summary(quad)
anova(quad)


plot(quad)
check_model(quad)
plot(simulateResiduals(quad))

anova(lin,quad)

#modelo cubico especial

cub <- MixModel(
  dataTucTuc,
  "PDI",
  mixvars,
  3
)

summary(cub)
anova(cub)
anova(quad,cub)

plot(cub)

#Grafico ternario

?ModelPlot

terGraf <- ModelPlot(
  model = quad,
  dimensions = list(x1 = "Tucuma",
                    x2 = "Tucuma.1",
                    x3 = "Brijo10"),
  lims = c(0.165, 0.5, 0.165, 0.5, 0.165, 0.5835),
  constraints = TRUE,
  contour = TRUE,
  at = c(0.0, 0.1, 0.2, 0.4, 0.6, 0.8),
  pseudo = TRUE,
  fill = TRUE,
  #color.palette = ,
  main = "TucTuc PDI",
  axislabs = c("Butter", "Oil", "Surfactant"),
  axislab.offset = 0.1,
  cornerlabs = c("", "", ""),
  grid = TRUE
)

??color.palette

?`palette`

palette.pals()

par(mfrow = c(1, 1))

?ModelEff

ModelEff(
  nfac = 3,
  mod = 2,
  nproc = 3,
  dir = 2,
  ufunc = quad,
  dimensions = mixvars,
  lc = c(0.1665, 0.1665, 0.1665),
  uc = c(0.5, 0.5, 0.5835)
  )
