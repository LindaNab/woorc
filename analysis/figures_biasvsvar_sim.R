#############################################################
## SIMSTUDY OF BIAS VS VARIANCE TRADEOFF
##
## This script creates the figures of the bias vs var tradeoff
## using the output of the simulation study
## lindanab4@gmail.com - 20211121
#############################################################

##############################
# 0 - Load libraries --------
##############################
# DGM:
# AEE ~ N(1, 0.25), AEE*|AEE ~ N(AEE, \tau^2),
# LBM|AEE ~ N(80 + beta * AEE, \sigma^2)
data(summary_biasvsvar)
summary <- summary_biasvsvar
summary %>%
  filter(nobs == 20 & reliability == 0.3 & method == "mecor.delta") %>%
  select(mse, mse_mcse)
summary %>%
  filter(nobs == 20 & reliability == 0.3 & method == "uncor") %>%
  select(mse, mse_mcse)
summary %>%
  filter(!(nobs == 20 & reliability == 0.3)) %>%
  select(mse_mcse) %>% pull() %>% max()

##############################
# 1 - Functions of mse -------
##############################
# reliability = var(x) / var(x_star)
# var(x_star) = var(x) / reliability
mse_uncor <- function(sigma, nobs, reliability, beta){
  (sigma / (nobs * (0.25 / reliability))) + ((1 - reliability) * beta) ^ 2
}
mse_regcal <- function(sigma, nobs, reliability){
  (sigma / (nobs * (0.25 / reliability))) /
    reliability ^ 2
}
nobs <- seq(from = 20, to = 100, by = 1)

##############################
# 2 - Figures, sigma^2 = 25---
##############################
summary_sigma25_rel03 <-
  summary %>%
  filter(sigma_sq == 25 & reliability == 0.3)
mse_uncor_calc1 <- mse_uncor(sigma = 25, nobs = nobs,
                             reliability = 0.3, beta = 3)
mse_regcal_calc1 <- mse_regcal(sigma = 25, nobs = nobs,
                               reliability = 0.3)
pdf("./results/ressim_mse_sigma25_rel03.pdf",
    width = 2.1, height = 2.1,
    pointsize = 8/0.75)
par(
  mar = c(4.25, 5, 3, 2.25),
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
        xlim = c(20, 100),
        ylim = c(0, 20),
        asp = 80 / 20
)
lines(nobs, mse_uncor_calc1, lty = 5, col = "darkgray")
lines(nobs, mse_regcal_calc1, lty = 5, col = "black")
summary_sigma25_rel03 %>%
  filter(method == "uncor") %>%
  with(., lines(nobs, mse, pch = 19, lty = 1, col = "darkgray", type = "b"))
summary_sigma25_rel03 %>%
  filter(method == "mecor.delta" & nobs != 20) %>%
  with(., lines(nobs, mse, pch = 19, lty = 1, col = "black", type = "b"))
axis(
  1,
  at = seq(from = 20, to = 100, by = 10),
  labels = seq(from = 20, to = 100, by = 10),
  las = 1
)
mtext("No. of Observations",
      side = 1,
      line = 3,
      cex = 0.75)
axis(
  2,
  at = seq(from = 0, to = 20, by = 5),
  labels = seq(from = 0, to = 20, by = 5),
  las = 1
)
mtext("Mean Squared Error",
      side = 2,
      line = 3,
      cex = 0.75)
legend("topright",
       legend = c("OLS", "RC"),
       lty = c(1, 1),
       col = c("darkgray", "black"),
       title = expression(underline(Estimator)))
mtext(paste0("REV = 25, Reliability = 0.3"),
      side = 3,
      line = 1,
      cex = 0.75)
di <- dev.size("in")
x <- grconvertX(c(0, di[1]), from = "in", to = "user")
y <- grconvertY(c(0, di[2]), from = "in", to = "user")
fig <- par("fig")
x <- x[1] + (x[2] - x[1]) * fig[1:2]
y <- y[1] + (y[2] - y[1]) * fig[3:4]
x <- x[1] + strwidth("A)")
y <- y[2] - strheight("A)") * 1.5
text(x, y, "A)", cex = 1)
dev.off()

summary_sigma25_rel06 <-
  summary %>%
  filter(sigma_sq == 25 & reliability == 0.6)
mse_uncor_calc2 <- mse_uncor(sigma = 25, nobs = nobs,
                             reliability = 0.6, beta = 3)
mse_regcal_calc2 <- mse_regcal(sigma = 25, nobs = nobs,
                               reliability = 0.6)
pdf("./results/ressim_mse_sigma25_rel06.pdf",
    width = 2.1, height = 2.1,
    pointsize = 8/0.75)
par(
  mar = c(4.25, 5, 3, 2.25),
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
        xlim = c(20, 100),
        ylim = c(0, 20),
        asp = 80 / 20
)
lines(nobs, mse_uncor_calc2, lty = 5, col = "darkgray")
lines(nobs, mse_regcal_calc2, lty = 5, col = "black")
summary_sigma25_rel06 %>%
  filter(method == "uncor") %>%
  with(., lines(nobs, mse, pch = 19, lty = 1, col = "darkgray", type = "b"))
summary_sigma25_rel06 %>%
  filter(method == "mecor.delta") %>%
  with(., lines(nobs, mse, pch = 19, lty = 1, col = "black", type = "b"))
axis(
  1,
  at = seq(from = 20, to = 100, by = 10),
  labels = seq(from = 20, to = 100, by = 10),
  las = 1
)
mtext("No. of Observations",
      side = 1,
      line = 3,
      cex = 0.75)
axis(
  2,
  at = seq(from = 0, to = 20, by = 5),
  labels = seq(from = 0, to = 20, by = 5),
  las = 1
)
mtext("Mean Squared Error",
      side = 2,
      line = 3,
      cex = 0.75)
mtext(paste0("REV = 25, Reliability = 0.6"),
      side = 3,
      line = 1,
      cex = 0.75)
di <- dev.size("in")
x <- grconvertX(c(0, di[1]), from = "in", to = "user")
y <- grconvertY(c(0, di[2]), from = "in", to = "user")
fig <- par("fig")
x <- x[1] + (x[2] - x[1]) * fig[1:2]
y <- y[1] + (y[2] - y[1]) * fig[3:4]
x <- x[1] + strwidth("B)")
y <- y[2] - strheight("B)") * 1.5
text(x, y, "B)", cex = 1)
dev.off()

summary_sigma25_rel09 <-
  summary %>%
  filter(sigma_sq == 25 & reliability == 0.9)
mse_uncor_calc3 <- mse_uncor(sigma = 25, nobs = nobs,
                             reliability = 0.9, beta = 3)
mse_regcal_calc3 <- mse_regcal(sigma = 25, nobs = nobs,
                               reliability = 0.9)
pdf("./results/ressim_mse_sigma25_rel09.pdf",
    width = 2.1, height = 2.1,
    pointsize = 8/0.75)
par(
  mar = c(4.25, 5, 3, 2.25),
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
        xlim = c(20, 100),
        ylim = c(0, 20),
        asp = 80 / 20
)
lines(nobs, mse_uncor_calc3, lty = 5, col = "darkgray")
lines(nobs, mse_regcal_calc3, lty = 5, col = "black")
summary_sigma25_rel09 %>%
  filter(method == "uncor") %>%
  with(., lines(nobs, mse, pch = 19, lty = 1, col = "darkgray", type = "b"))
summary_sigma25_rel09 %>%
  filter(method == "mecor.delta") %>%
  with(., lines(nobs, mse, pch = 19, lty = 1, col = "black", type = "b"))
axis(
  1,
  at = seq(from = 20, to = 100, by = 10),
  labels = seq(from = 20, to = 100, by = 10),
  las = 1
)
mtext("No. of Observations",
      side = 1,
      line = 3,
      cex = 0.75)
axis(
  2,
  at = seq(from = 0, to = 20, by = 5),
  labels = seq(from = 0, to = 20, by = 5),
  las = 1
)
mtext("Mean Squared Error",
      side = 2,
      line = 3,
      cex = 0.75)
mtext(paste0("REV = 25, Reliability = 0.9"),
      side = 3,
      line = 1,
      cex = 0.75)
di <- dev.size("in")
x <- grconvertX(c(0, di[1]), from = "in", to = "user")
y <- grconvertY(c(0, di[2]), from = "in", to = "user")
fig <- par("fig")
x <- x[1] + (x[2] - x[1]) * fig[1:2]
y <- y[1] + (y[2] - y[1]) * fig[3:4]
x <- x[1] + strwidth("C)")
y <- y[2] - strheight("C)") * 1.5
text(x, y, "C)", cex = 1)
dev.off()

##############################
# 3 - Figures, sigma^2 = 5----
##############################
summary_sigma5_rel03 <-
  summary %>%
  filter(sigma_sq == 5 & reliability == 0.3)
mse_uncor_calc1 <- mse_uncor(sigma = 5, nobs = nobs,
                             reliability = 0.3, beta = 3)
mse_regcal_calc1 <- mse_regcal(sigma = 5, nobs = nobs,
                               reliability = 0.3)
pdf("./results/ressim_mse_sigma5_rel03.pdf",
    width = 2.1, height = 2.1,
    pointsize = 8/0.75)
par(
  mar = c(4.25, 5, 3, 2.25),
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
        xlim = c(20, 100),
        ylim = c(0, 20),
        asp = 80 / 20
)
lines(nobs, mse_uncor_calc1, lty = 5, col = "darkgray")
lines(nobs, mse_regcal_calc1, lty = 5, col = "black")
summary_sigma5_rel03 %>%
  filter(method == "uncor") %>%
  with(., lines(nobs, mse, pch = 19, lty = 1, col = "darkgray", type = "b"))
summary_sigma5_rel03 %>%
  filter(method == "mecor.delta" & nobs != 20) %>%
  with(., lines(nobs, mse, pch = 19, lty = 1, col = "black", type = "b"))
axis(
  1,
  at = seq(from = 20, to = 100, by = 10),
  labels = seq(from = 20, to = 100, by = 10),
  las = 1
)
mtext("No. of Observations",
      side = 1,
      line = 3,
      cex = 0.75)
axis(
  2,
  at = seq(from = 0, to = 20, by = 5),
  labels = seq(from = 0, to = 20, by = 5),
  las = 1
)
mtext("Mean Squared Error",
      side = 2,
      line = 3,
      cex = 0.75)
mtext(paste0("REV = 5, Reliability = 0.3"),
      side = 3,
      line = 1,
      cex = 0.75)
di <- dev.size("in")
x <- grconvertX(c(0, di[1]), from = "in", to = "user")
y <- grconvertY(c(0, di[2]), from = "in", to = "user")
fig <- par("fig")
x <- x[1] + (x[2] - x[1]) * fig[1:2]
y <- y[1] + (y[2] - y[1]) * fig[3:4]
x <- x[1] + strwidth("D)")
y <- y[2] - strheight("D)") * 1.5
text(x, y, "D)", cex = 1)
dev.off()

summary_sigma5_rel06 <-
  summary %>%
  filter(sigma_sq == 5 & reliability == 0.6)
mse_uncor_calc2 <- mse_uncor(sigma = 5, nobs = nobs,
                             reliability = 0.6, beta = 3)
mse_regcal_calc2 <- mse_regcal(sigma = 5, nobs = nobs,
                               reliability = 0.6)
pdf("./results/ressim_mse_sigma5_rel06.pdf",
    width = 2.1, height = 2.1,
    pointsize = 8/0.75)
par(
  mar = c(4.25, 5, 3, 2.25),
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
        xlim = c(20, 100),
        ylim = c(0, 20),
        asp = 80 / 20
)
lines(nobs, mse_uncor_calc2, lty = 5, col = "darkgray")
lines(nobs, mse_regcal_calc2, lty = 5, col = "black")
summary_sigma5_rel06 %>%
  filter(method == "uncor") %>%
  with(., lines(nobs, mse, pch = 19, lty = 1, col = "darkgray", type = "b"))
summary_sigma5_rel06 %>%
  filter(method == "mecor.delta") %>%
  with(., lines(nobs, mse, pch = 19, lty = 1, col = "black", type = "b"))
axis(
  1,
  at = seq(from = 20, to = 100, by = 10),
  labels = seq(from = 20, to = 100, by = 10),
  las = 1
)
mtext("No. of Observations",
      side = 1,
      line = 3,
      cex = 0.75)
axis(
  2,
  at = seq(from = 0, to = 20, by = 5),
  labels = seq(from = 0, to = 20, by = 5),
  las = 1
)
mtext("Mean Squared Error",
      side = 2,
      line = 3,
      cex = 0.75)
mtext(paste0("REV = 5, Reliability = 0.6"),
      side = 3,
      line = 1,
      cex = 0.75)
di <- dev.size("in")
x <- grconvertX(c(0, di[1]), from = "in", to = "user")
y <- grconvertY(c(0, di[2]), from = "in", to = "user")
fig <- par("fig")
x <- x[1] + (x[2] - x[1]) * fig[1:2]
y <- y[1] + (y[2] - y[1]) * fig[3:4]
x <- x[1] + strwidth("E)")
y <- y[2] - strheight("E)") * 1.5
text(x, y, "E)", cex = 1)
dev.off()

summary_sigma5_rel09 <-
  summary %>%
  filter(sigma_sq == 5 & reliability == 0.9)
mse_uncor_calc3 <- mse_uncor(sigma = 5, nobs = nobs,
                             reliability = 0.9, beta = 3)
mse_regcal_calc3 <- mse_regcal(sigma = 5, nobs = nobs,
                               reliability = 0.9)
pdf("./results/ressim_mse_sigma5_rel09.pdf",
    width = 2.1, height = 2.1,
    pointsize = 8/0.75)
par(
  mar = c(4.25, 5, 3, 2.25),
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
        xlim = c(20, 100),
        ylim = c(0, 20),
        asp = 80 / 20
)
lines(nobs, mse_uncor_calc3, lty = 5, col = "darkgray")
lines(nobs, mse_regcal_calc3a, lty = 5, col = "black")
summary_sigma5_rel09 %>%
  filter(method == "uncor") %>%
  with(., lines(nobs, mse, pch = 19, lty = 1, col = "darkgray", type = "b"))
summary_sigma5_rel09 %>%
  filter(method == "mecor.delta") %>%
  with(., lines(nobs, mse, pch = 19, lty = 1, col = "black", type = "b"))
axis(
  1,
  at = seq(from = 20, to = 100, by = 10),
  labels = seq(from = 20, to = 100, by = 10),
  las = 1
)
mtext("No. of Observations",
      side = 1,
      line = 3,
      cex = 0.75)
axis(
  2,
  at = seq(from = 0, to = 20, by = 5),
  labels = seq(from = 0, to = 20, by = 5),
  las = 1
)
mtext("Mean Squared Error",
      side = 2,
      line = 3,
      cex = 0.75)
mtext(paste0("REV = 5, Reliability = 0.9"),
      side = 3,
      line = 1,
      cex = 0.75)
di <- dev.size("in")
x <- grconvertX(c(0, di[1]), from = "in", to = "user")
y <- grconvertY(c(0, di[2]), from = "in", to = "user")
fig <- par("fig")
x <- x[1] + (x[2] - x[1]) * fig[1:2]
y <- y[1] + (y[2] - y[1]) * fig[3:4]
x <- x[1] + strwidth("F)")
y <- y[2] - strheight("F)") * 1.5
text(x, y, "F)", cex = 1)
dev.off()
