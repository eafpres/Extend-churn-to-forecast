#
# anonymize data for publication
#
  library(data.table)
  library(digest)
#  
# code sourced from 
# https://github.com/jangorecki/jangorecki.github.io/blob/master/_posts/2014-11-07-Data-Anonymization-in-R.md
#
  anonymize <- function(x, algo="crc32"){
    unq_hashes <- vapply(unique(x), function(object) digest(object, algo=algo), FUN.VALUE="", USE.NAMES=TRUE)
    unname(unq_hashes[x])
  }
#  
  data <- read.csv("20180119_cust_churn_data.csv")
  temp_data <- as.data.table(data)
  temp_data$cust_no <- ceiling(((data$cust_no * 3 + 10) / 2) * 10)
  temp_data$cust_order <- ceiling(((data$cust_order * 4 + 18) / 3) * 10)
  cols_to_mask <- colnames(temp_data)[c(1, 2, 10, 14, 15, 16, 17)]
  temp_data[, cols_to_mask := lapply(.SD, anonymize),
            .SDcols = cols_to_mask,
            with = FALSE]
  write.csv(temp_data, "20180119_cust_churn_data_anon.csv")
  