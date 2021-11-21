#############################################################
## THEORETICAL BIAS VS VARIANCE TRADEOFF
##
## This script creates the figures of the bias vs var tradeoff
## using the theoretical derivation of Carroll et al. 2006
## lindanab4@gmail.com - 20211105
#############################################################

##############################
# 0 - Load libraries --------
##############################
# DGM:
# AEE ~ N(1, 0.25), AEE*|AEE ~ N(AEE, \tau^2),
# LBM|AEE ~ N(80 + beta * AEE, \sigma^2)

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

##############################
# 2 - Figures, sigma^2 = 25---
##############################
nobs <- seq(from = 20, to = 100, by = 1)
mse_uncor_calc1 <- mse_uncor(sigma = 25, nobs = nobs,
                             reliability = 0.3, beta = 3)
mse_regcal_calc1 <- mse_regcal(sigma = 25, nobs = nobs,
                               reliability = 0.3)
pdf("./results/carroll_mse_sigma25_rel03.pdf",
    width = 3, height = 3,
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
lines(nobs, mse_uncor_calc1)
lines(nobs, mse_regcal_calc1, lty = 2)
axis(
  1,
  at = seq(from = 20, to = 100, by = 10),
  labels = seq(from = 20, to = 100, by = 10),
  las = 1
)
mtext("Number of Observations",
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
       legend = c("Uncorrected", "Regression Calibration"),
       lty = c(1, 2),
       title = expression(underline(Estimator)))
mtext(paste0("Residual Error Variance = 25, Reliability = 0.3"),
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

mse_uncor_calc2 <- mse_uncor(sigma = 25, nobs = nobs,
                             reliability = 0.6, beta = 3)
mse_regcal_calc2 <- mse_regcal(sigma = 25, nobs = nobs,
                               reliability = 0.6)
pdf("./results/carroll_mse_sigma25_rel06.pdf",
    width = 3, height = 3,
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
lines(nobs, mse_uncor_calc2)
lines(nobs, mse_regcal_calc2, lty = 2)
axis(
  1,
  at = seq(from = 20, to = 100, by = 10),
  labels = seq(from = 20, to = 100, by = 10),
  las = 1
)
mtext("Number of Observations",
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
mtext(paste0("Residual Error Variance = 25, Reliability = 0.6"),
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

mse_uncor_calc3 <- mse_uncor(sigma = 25, nobs = nobs,
                             reliability = 0.9, beta = 3)
mse_regcal_calc3 <- mse_regcal(sigma = 25, nobs = nobs,
                               reliability = 0.9)
pdf("./results/carroll_mse_sigma25_rel09.pdf",
    width = 3, height = 3,
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
lines(nobs, mse_uncor_calc3)
lines(nobs, mse_regcal_calc3, lty = 2)
axis(
  1,
  at = seq(from = 20, to = 100, by = 10),
  labels = seq(from = 20, to = 100, by = 10),
  las = 1
)
mtext("Number of Observations",
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
mtext(paste0("Residual Error Variance = 25, Reliability = 0.9"),
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
nobs <- seq(from = 20, to = 100, by = 1)
mse_uncor_calc1 <- mse_uncor(sigma = 5, nobs = nobs,
                             reliability = 0.3, beta = 3)
mse_regcal_calc1 <- mse_regcal(sigma = 5, nobs = nobs,
                               reliability = 0.3)
pdf("./results/carroll_mse_sigma5_rel03.pdf",
    width = 3, height = 3,
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
lines(nobs, mse_uncor_calc1)
lines(nobs, mse_regcal_calc1, lty = 2)
axis(
  1,
  at = seq(from = 20, to = 100, by = 10),
  labels = seq(from = 20, to = 100, by = 10),
  las = 1
)
mtext("Number of Observations",
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
mtext(paste0("Residual Error Variance = 5, Reliability = 0.3"),
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

mse_uncor_calc2 <- mse_uncor(sigma = 5, nobs = nobs,
                             reliability = 0.6, beta = 3)
mse_regcal_calc2 <- mse_regcal(sigma = 5, nobs = nobs,
                               reliability = 0.6)
pdf("./results/carroll_mse_sigma5_rel06.pdf",
    width = 3, height = 3,
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
lines(nobs, mse_uncor_calc2)
lines(nobs, mse_regcal_calc2, lty = 2)
axis(
  1,
  at = seq(from = 20, to = 100, by = 10),
  labels = seq(from = 20, to = 100, by = 10),
  las = 1
)
mtext("Number of Observations",
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
mtext(paste0("Residual Error Variance = 5, Reliability = 0.6"),
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

mse_uncor_calc3 <- mse_uncor(sigma = 5, nobs = nobs,
                             reliability = 0.9, beta = 3)
mse_regcal_calc3 <- mse_regcal(sigma = 5, nobs = nobs,
                               reliability = 0.9)
pdf("./results/carroll_mse_sigma5_rel09.pdf",
    width = 3, height = 3,
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
lines(nobs, mse_uncor_calc3)
lines(nobs, mse_regcal_calc3, lty = 2)
axis(
  1,
  at = seq(from = 20, to = 100, by = 10),
  labels = seq(from = 20, to = 100, by = 10),
  las = 1
)
mtext("Number of Observations",
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
mtext(paste0("Residual Error Variance = 5, Reliability = 0.9"),
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


