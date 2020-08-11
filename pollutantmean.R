pollutantmean <- function(directory, pollutant, id = 1:332) {
  if(pollutant %in% c('sulfate', 'nitrate')) {
    full_path <- paste(getwd(), '/', directory, '/', sep = '')
    total = 0
    len = 0
    for(i in id) {
      file = paste(full_path, sprintf('%03d', i), '.csv', sep = '')
      f <- read.csv(file)
      total <- total + sum(f[[pollutant]], na.rm = TRUE)
      len <- len + length(f[[pollutant]][!is.na(f[[pollutant]])])
    }
  } else {print('No such pollutant, try again (sulfate or nitrate)')}
  total / len
}