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