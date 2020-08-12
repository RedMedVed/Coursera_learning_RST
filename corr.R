corr <- function(directory, threshold = 0) {
  full_path <- paste(getwd(), '/', directory, '/', sep = '')
  total = c()
  #colnames(total) <- c('sulfate', 'nitrate')
    #list representing amount of complete cases in each file
    amount_of_complete <- complete(directory)
    #print(typeof(amount_of_complete))
    monitor_numbers <- sprintf('%03d', which(amount_of_complete[ ,'nobs'] > threshold))
    for(number in monitor_numbers) {
      file = paste(full_path, number, '.csv', sep = '')
      f <- read.csv(file)
      monitor_cor <- cor(f$sulfate, f$nitrate, use = 'complete.obs')
      #print(monitor_cor)
      total <- rbind(total, monitor_cor)
    }
  total
}


