corr <- function(directory, threshold = 0) {
  full_path <- paste(getwd(), '/', directory, '/', sep = '')
  total = matrix(ncol = 2)
  colnames(total) <- c('sulfate', 'nitrate')
    #list representing amount of complete cases in each file
    amount_of_complete <- complete(directory)
    #print(typeof(amount_of_complete))
    monitor_numbers <- sprintf('%03d', which(amount_of_complete[ ,'nobs'] > threshold))
    for(number in monitor_numbers) {
      file = paste(full_path, number, '.csv', sep = '')
      f <- read.csv(file)
      pollutants <- data.frame(f$sulfate, f$nitrate)
      colnames(pollutants) <- c('sulfate', 'nitrate')
      total <- rbind(total, pollutants)
    }
  plot(total$sulfate, total$nitrate)
  cor(total$sulfate, total$nitrate, use = 'complete.obs')
}


