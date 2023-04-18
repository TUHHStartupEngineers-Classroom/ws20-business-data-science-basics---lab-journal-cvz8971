D <- 1000
K <- 5
h <- 0.25
Q <- sqrt(2*D*K/h)
hallo <- mtcars
eoq <- function(D = 1000) {
  K <- 5
  h <- 0.25
  sqrt(2*D*K/h)
}
eoq()

dice <- 1:6
wurf1 <- sample(dice, size = 1)
wurf2 <- sample(dice, size = 2, replace = TRUE)
wurf1
wurf2
sum(wurf2)

# with default values
wuerfeln <- function(seiten = 6, wuerfel = 1)  {
  sample(1:seiten, size = wuerfel, replace = TRUE)
}
wuerfeln(6, 2)