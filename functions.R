z <- function(x) {
  y <- if (x > 3) {
    10
  } else {
    2
  }
  return(y)
}

g <- function(l) {
  while (l >= 8 & l <= 15) {
    print(l)
    coin <- rbinom(1, 1, 0.5)
    if (coin == 1) {
      l <- l + 1
    } else {
      l <- l - 1
    }
  }
}

adder <- function(d, f) {
  return(d + f)
}

make.power <- function(n) {
  pow <- function(x) {
    x ^ n
  }
  pow
}

h <- function(x, y = NULL, d = 3L) {
  z <- cbind(x, d)
  if(!is.null(y))
    z <- z + y
  else
    z <- z + f
  g <- x + y / z
  if(d == 3L)
    return(g)
  g <- g + 10
  g
}

distr1 <- function(seed1, amount = 100) {
  set.seed(seed1)
  x <- rnorm(amount)
  e <- rnorm(amount, 0, 2)
  y <- 0.5 + 2 * x + e
  print(summary(y))
  plot(x, y)
}

distr2 <- function(seed2, amount = 100) {
  set.seed(seed2)
  x <- rbinom(amount, 1, 0.5)
  e <- rnorm(amount, 0, 2)
  y <- 0.5 + 2 * x + e
  print(summary(y))
  plot(x, y)
}

distr3 <- function(seed3, amount = 100) {
  set.seed(seed3)
  x <- rnorm(amount)
  log.mu <- 0.5 + 0.3 * x
  y <- rpois(amount, exp(log.mu))
  print(summary(y))
  plot(x, y)
}