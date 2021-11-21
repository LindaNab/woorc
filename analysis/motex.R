#==================================#
# woorc                            #
# Motivating example               #
#                                  #
#                                  #
# 20211119 lindanab4@gmail.com     #
#==================================#

# Data: https://datadryad.org/stash/dataset/doi:10.5061/dryad.ck501
# Paper: https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0189254
# Original rq: Irisin, physical activity and fitness status in healthy humans:
# No association under resting conditions in a cross-sectional study

# Here, we'll be aiming to estimate the association between active energy expen
# diture and percentage lean body mass

# 0. load libraries
library(haven)
library(dplyr)
library(mecor)

# 1. load data and select columns needed for analysis
data <-
  read.csv2(file = "./input/dataset_irisin_PA.csv")

data <-
  data %>%
  mutate(MET = Total.MET..MET.min.week.,
         weight = body.weight..kg.,
         AEE = AEE..kcal.d.,
         LBM = LBM....)
data <-
  data %>%
  select(MET, weight, AEE, LBM, sex)
data$MET_day <- data$MET / 7
data <-
  data %>%
  mutate(MET_kcal_day = (((MET_day * 3.5 * weight) / 200)))
data$MET_kcal_day_1000 <- data$MET_kcal_day  / 1000
data$AEE_1000 <- data$AEE / 1000
# some AEE are missing, do not include these rows
data <-
  data %>%
  filter(!is.na(AEE))
anyNA(data)

# 2. correlation between AEE Actiheart and questionnaire
with(data, cor.test(MET_kcal_day_1000, AEE_1000))

# make plot for paper
pdf("./results/corrAEE.pdf",
    width = 3, height = 3,
    pointsize = 8/0.75)
par(
  mar = c(4.25, 5, 2, 1.25),
  xpd = NA,
  cex = 0.75
)
plot(   0,
        type = "n",
        xaxt = "n",
        yaxt = "n",
        yaxs = "i",
        xaxs = "i",
        frame.plot = F,
        ann = F,
        xlim = c(0, 6),
        ylim = c(0, 6),
        asp = 1
)
with(data, points(MET_kcal_day_1000,
           AEE_1000))
axis(
  1,
  at = seq(from = 0, to = 6, by = 1),
  labels = seq(from = 0, to = 6, by = 1),
  las = 1
)
mtext("AEE Questionnaire (1000 kcal/day)",
      side = 1,
      line = 3,
      cex = 0.75)
axis(
  2,
  at = seq(from = 0, to = 6, by = 1),
  labels = seq(from = 0, to = 6, by = 1),
  las = 1
)
mtext("AEE Actiheart\U00AE (1000 kcal/day)",
      side = 2,
      line = 3,
      cex = 0.75)
dev.off()

# 3. association between lbm and aee
# using reference measure
corfit <- lm(LBM ~ AEE_1000 + sex, data = data)
corfit %>% summary()
confint(corfit)

# using proxy measure
naivefit <- lm(LBM ~ MET_kcal_day_1000 + sex, data = data)
naivefit %>% summary()
confint(naivefit)

# correct for measurement error in proxy measure using regcal
set.seed(20211101)
mefit <-
  mecor(LBM ~ MeasError(MET_kcal_day_1000, reference = AEE_1000) + sex,
        data = data, B = 999) %>% summary()
mefit

# 4. dgm for simulation study
hist(data$AEE_1000)
mean(data$AEE_1000, na.rm = TRUE)
var(data$AEE_1000, na.rm = TRUE)
