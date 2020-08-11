complete <- function(directory, id = 1:332) {
  full_path <- paste(getwd(), '/', directory, '/', sep = '')
  total = matrix(ncol = 2)
  colnames(total) <- c('id', 'nobs')
  for(i in id) {
    file = paste(full_path, sprintf('%03d', i), '.csv', sep = '')
    f <- read.csv(file)
    total <- rbind(total, c(i, sum(complete.cases(f))))
  }
  total <- total[-1,]
  total
}