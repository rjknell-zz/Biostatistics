set.seed(12)

Control <- round(rnorm(10, mean = 13, sd = 2), 1)
Treatment <- round(rnorm(10, mean = 15.7, sd = 2), 1)

Treatment[4] <- 10.2

Group <- rep(c("Control", "Treatment"), each = 10)
Leaf_mass <- c(Control, Treatment)

boxplot(Leaf_mass ~ Group, xlab = "", ylab = "Leaf mass (g)",
        col = "aquamarine4")


library(gplots)
plotmeans(Leaf_mass ~ Group,
          n.label = FALSE,
          connect = FALSE,
          pch = 16,
          cex = 1.8,
          xlab = "",
          ylab = "Leaf mass (g)",
          col = "aquamarine4",
          barcol = "aquamarine4")

2*(1-pt(2.84, df = 18))


t.test(Leaf_mass ~ Group, var.equal = TRUE)




X1 <- seq(-4, 4, length = 200)
Y1 <- dt(X1, 18)
plot(
  X1,
  Y1,
  type = "n",
  xlab = "x",
  ylab = "P(x)",
  ylim = c(0, 0.42),
  main = "t-distribution on 18 df"
)

x0 <- min(which(X1 >= qt(0.005, 18)))
x1 <- min(which(X1 >= qt(0.025, 18)))
x2 <- min(which(X1 >= qt(0.1666, 18)))
x3 <- max(which(X1 <= qt(0.8333, 18)))
x4 <- max(which(X1 <= qt(0.975, 18)))
x5 <- max(which(X1 <= qt(0.995, 18)))

polygon(
  x = c(X1[c(1, 1:x0, x0)]),
  y = c(0, Y1[1:x0], 0),
  col = "#3182bd",
  border = NA
)
polygon(
  x = c(X1[c(x0, x0:x1, x1)]),
  y = c(0, Y1[x0:x1], 0),
  col = "#9ecae1",
  border = NA
)
polygon(
  x = c(X1[c(x1, x1:x2, x2)]),
  y = c(0, Y1[x1:x2], 0),
  col = "#deebf7",
  border = NA
)
polygon(
  x = c(X1[c(x2, x2:x3, x3)]),
  y = c(0, Y1[x2:x3], 0),
  col = "white",
  border = NA
)
polygon(
  x = c(X1[c(x3, x3:x4, x4)]),
  y = c(0, Y1[x3:x4], 0),
  col = "#deebf7",
  border = NA
)
polygon(
  x = c(X1[c(x4, x4:x5, x5)]),
  y = c(0, Y1[x4:x5], 0),
  col = "#9ecae1",
  border = NA
)
polygon(
  x = c(X1[c(x5, x5:200, 200)]),
  y = c(0, Y1[x5:200], 0),
  col = "#3182bd",
  border = NA
)

points(X1, Y1, type = "l")

text(0, Y1[x2] + 0.01, "66% of values", cex = 0.8)
lines(c(X1[x2], X1[x3]), c(Y1[x2] - 0.001, Y1[x3] - 0.001))

text(0, Y1[x1] + 0.01, "95% of values", cex = 0.8)
lines(c(X1[x1], X1[x4]), c(Y1[x1] - 0.001, Y1[x4] - 0.001))

text(0, Y1[x0] + 0.01, "99% of values", cex = 0.8)
lines(c(X1[x0], X1[x5]), c(Y1[x0] - 0.001, Y1[x5] - 0.001))

# arrows(2.816, 0.04, 2.816, dt(2.816, 18), length = 0.05)
#
# text(2.816, 0.06, "Location of\n our calculated\n value of t", cex = 0.7)
