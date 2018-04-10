#
# analyze customer data
#
# clear the environment
#
  rm(list=ls())
#
# required packages
#
# dplyr is used to create summary data
#    
  library(dplyr)
#
# reshape is used to pivot data
#
  library(reshape2)
#
# zoo is used to smooth data
#
  library(zoo)
#  
# euclidean distance function (vectorized)
#
  euc_dist_v <- function(point, centers_matrix) {
    dist <- matrix(0, nrow = 1, ncol = nrow(centers_matrix))
    dist <- t(t(centers_matrix[, 1:ncol(centers_matrix)]) - 
                point[, 1:length(point)])
    dist <- t(sqrt(rowSums(dist * dist)))
    return(dist)
  }
#
# function to find medoid in cluster i from hclust
#
  clust.centroid <- function(i, data_source, clusters_found) {
    indices <- (clusters_found == i)
    colMeans(data_source[indices, ])
  }
#
  graphics.off() 
#
  min_date <- as.numeric(as.Date("2016-06-15"))
  max_date <- as.numeric(as.Date("2016-11-15"))
  max_val_date <- as.numeric(as.Date("2016-11-15"))
#
# set threshold to decide which items are "recent" when 
# subsetting / filtering such as buidling the item list for
# creating dummy variables
#
  recency_cutoff <- 184
#
# get the order data
#
  raw_data <- read.csv("cust_churn_data.csv", 
                       colClasses = c("Item.Number" = "character"),
                       header = TRUE,
                       stringsAsFactors = FALSE)
#
# clean
#
  date_system_offset <- 
    as.numeric(as.Date("1970-01-01") - as.Date("1900-01-01"))
#
  bad_create_dates <- 
    nrow(raw_data[raw_data$create_date == "#VALUE!", ])
  if (bad_create_dates > 0) {
    raw_data[raw_data$create_date == "#VALUE!", ]$create_date <- 
      as.character(min_date - 365 + date_system_offset)
    raw_data$create_date <- as.numeric(raw_data$create_date)
  }
  bad_order_dates <- 
    nrow(raw_data[raw_data$order_date == "#VALUE!", ])
  if (bad_order_dates > 0) {
    raw_data[raw_data$order_date == "#VALUE!", ]$order_date <- 
      as.character(min_date - 365 + date_system_offset)
    raw_data$order_date <- as.numeric(raw_data$order_date)
  }
  bad_ship_dates <- 
    nrow(raw_data[raw_data$ship_date == "#VALUE!", ])
  if (bad_ship_dates > 0) {
    raw_data[raw_data$ship_date == "#VALUE!", ]$ship_date <- 
      as.character(min_date - 365 + date_system_offset)
    raw_data$ship_date <- as.numeric(raw_data$ship_date)
  }
  bad_request_dates <- 
    nrow(raw_data[raw_data$request_date == "#VALUE!", ])
  if (bad_request_dates > 0) {
    raw_data[raw_data$request_date == "#VALUE!", ]$request_date <- 
      as.character(min_date - 365 + date_system_offset)
    raw_data$request_date <- as.numeric(raw_data$request_date)
  }
  warning("There were ", bad_create_dates, " bad create dates\n")
  warning("There were ", bad_order_dates, " bad order dates\n")
  warning("There were ", bad_ship_dates, " bad ship dates\n")
  warning("There were ", bad_request_dates, " bad request dates\n")
#
# subset the data into desired date range
#
  raw_data$create_date <- as.Date(raw_data$create_date,
                                  origin = "1900-01-01")
  raw_data$order_date <- as.Date(raw_data$order_date,
                                 origin = "1900-01-01")
  raw_data$ship_date <- as.Date(raw_data$ship_date,
                                origin = "1900-01-01")
  raw_data$request_date <- as.Date(raw_data$request_date,
                                   origin = "1900-01-01")
#
# before we trim off older data, find the earliest order by customer
# to be used in determining the customer longevity
#
  melted_earliest_order <- melt(raw_data, id = "cust_no",
                            measure.vars = "order_date")
  cust_first_dates <- as.Date(acast(melted_earliest_order, 
                              cust_no ~ variable,
                              min),
                        origin = "1970-01-01")
  cust_rows <- rownames(cust_first_dates)
  cust_first_dates <- as.Date(as.character(cust_first_dates), origin = "1970-01-01")
  cust_firsts <- data.frame("cust_no" = cust_rows, 
                            "cust_first" = cust_first_dates)
#
# remove unwanted line items and drop older data
#
  raw_data_subset_raw <-
    raw_data[((raw_data$order_date >= min_date) &
                (substr(raw_data$fam_series, 1, 6) != "Sample") &
                (substr(raw_data$fam_series, 1, 7) != "Unknown") &
                (substr(raw_data$fam_series, 1, 8) != "Warranty")), ]
#
# generate list of item numbers and how many times ordered and most recent order
# note the use of "length" as the default function when we are not really
# aggregating, just counting--this avoids a warning message
#
  item_no_melt <- melt(raw_data_subset_raw, 
                       id = "Item.Number", 
                       measure.vars = "Item.Number")
  item_frequency <- acast(item_no_melt, 
                          Item.Number ~ variable, length)
  item_list <- 
    data.frame("Item_Number" = 
                 character(length = length(item_frequency)), 
               "Number_of_Orders" = 
                 integer(length = length(item_frequency)),
               "Most_recent_order" = 
                 as.Date(integer(length = length(item_frequency)),
                                             origin = "1970-01-01"))
  item_list[, 1] <- rownames(item_frequency)
  item_list[, 2] <- item_frequency[, ]
  item_no_melt <- melt(raw_data_subset_raw, 
                       id = "Item.Number", 
                       measure.vars = "order_date")
  item_recency <- acast(item_no_melt, 
                        Item.Number ~ variable, max)
  item_list[, 3] <- as.Date(item_recency[, ], origin = "1970-01-01")
  index <- order(item_list[,2], decreasing = TRUE)
  item_list <- item_list[index, ]
#
# subset item_list for more frequently ordered items and recent items
#
  item_list_subset <-
    item_list[((item_list$Number_of_Orders > 100) &
                 (item_list$Most_recent_order > max_date - recency_cutoff)), ]
#
# calcualte days late as another predictor
#
  days_late <- matrix(nrow = nrow(raw_data_subset_raw), ncol = 1)
  days_late <- 
    as.integer(raw_data_subset_raw$ship_date - 
               raw_data_subset_raw$request_date)
  raw_data_subset_raw <- cbind(raw_data_subset_raw, days_late)
#
# method options
#
  kmeans_algorithms = c("Hartigan-Wong", "Lloyd", "MacQueen")
  hclust_algorithms <- c("ward.D", "ward.D2", 
                         "single", "complete", 
                         "average", "mcquitty", 
                         "median", "centroid")
  dist_methods <- c("euclidean", "euclidean^2", "maximum", 
                    "manhattan", "canberra", 
                    "binary", "minkowski")
#
# override defaults if desired here
#  
#  kmeans_algorithms <- "Lloyd"
#  hclust_algorithms <- c("ward.D", "ward.D2")
#  dist_methods <- c("euclidean", "manhattan")
#
# plots_verbose determines if more detailed plot output should be used
#
  plots_verbose <- TRUE
  time_stamp <- paste0(as.character(Sys.time(), "%Y-%m-%d-%H-%M-%s"))
  pdf(file = paste0(time_stamp, "charts.pdf"), onefile = TRUE)
#  
  clusters <- seq(20, 30, 10)
  splits <- rbind(c(0.60, 0.20, 0.20), c(0, 0, 0))
  colnames(splits) <- c("train", "test", "validation")
#  
  all_methods <- c(kmeans_algorithms, hclust_algorithms)
  all_permutations <- character()
  for (i in 1:length(all_methods)) {
    for (j in 1:length(clusters)) {
      if (all_methods[i] %in% kmeans_algorithms) {
        all_permutations <- c(all_permutations,
                              paste0(all_methods[i], 
                                     "_",
                                     clusters[j]))
      } else {
        for (k in 1:length(dist_methods)) {
          all_permutations <- c(all_permutations,
                                paste0(all_methods[i],
                                       "_",
                                       dist_methods[k],
                                       "_",
                                       clusters[j]))
        }
      }
    }
  }
#
  test_cumulative_table <- data.frame(character(),
                                      character(),
                                      integer(),
                                      numeric(),
                                      integer(),
                                      numeric(),
                                      numeric(),
                                      numeric(),
                                      numeric(),
                                      numeric())
  colnames(test_cumulative_table) <- c("Data", "Algorithm", "ID", "split", 
                                       "clusters", "date", "Precision", 
                                       "Recall", "Accuracy", "F1")
#
  val_cumulative_table <- data.frame(character(),
                                     character(),
                                      integer(),
                                      numeric(),
                                      integer(),
                                      numeric(),
                                      numeric(),
                                      numeric(),
                                      numeric(),
                                      numeric())
  colnames(val_cumulative_table) <- c("Data", "Algorithm", "ID", "split", 
                                       "clusters", "date", "Precision", 
                                       "Recall", "Accuracy", "F1")
#
# define "will reorder by" cutoffs for binning results
#
  will_reorder_by_cutoffs <- seq(0, 91, 1)
  will_reorder_centers <- numeric()
#
  for (i in 1:(length(will_reorder_by_cutoffs) - 1)) {
    will_reorder_centers[i] <- 
      mean(c(will_reorder_by_cutoffs[i], 
             will_reorder_by_cutoffs[i + 1]))
  }
# 
# over ride the centers as test
#
  will_reorder_centers <- 
    will_reorder_by_cutoffs[2:length(will_reorder_by_cutoffs)]
#  
  for (split_values in 1:nrow(splits)) {
#  
# create train / test / validation split on subsetted data
# use fixed seed so we can reproduce--can be randomized later
# first split the validation off from the subset
#
    set.seed(2018)
    train_size <- ceiling(splits[split_values, 1] * nrow(raw_data_subset_raw))
    test_size <- ceiling(splits[split_values, 2] * nrow(raw_data_subset_raw))
    val_size <- nrow(raw_data_subset_raw) - train_size - test_size
    if (val_size < splits[split_values, 2] * nrow(raw_data_subset_raw) - 2) {
      warning("validation set size doesn't match split values\n")
    }
#
    cust_ids <- unique(raw_data_subset_raw$cust_no)
    train_cust_ids <- sample(cust_ids, 
                             size = splits[split_values, 1] * length(cust_ids))
    test_cust_ids <- sample(cust_ids[!(cust_ids %in% train_cust_ids)],
                            size = splits[split_values, 2] * length(cust_ids))
    train_data_subset <- 
      raw_data_subset_raw[raw_data_subset_raw$cust_no %in% 
                            train_cust_ids &
                            raw_data_subset_raw$order_date > min_date &
                            raw_data_subset_raw$order_date <= max_date, ]
    test_data_subset <- 
      raw_data_subset_raw[raw_data_subset_raw$cust_no %in%
                            test_cust_ids &
                            raw_data_subset_raw$order_date > min_date &
                            raw_data_subset_raw$order_date <= max_date, ]
    val_data_subset <- 
      raw_data_subset_raw[!(raw_data_subset_raw$cust_no %in% train_cust_ids) &
                            !(raw_data_subset_raw$cust_no %in% test_cust_ids) & 
                            raw_data_subset_raw$order_date > min_date &
                            raw_data_subset_raw$order_date <= max_date, ]
#
# get the next order beyond the end of each data set for each customer
#
    beyond_max_date <- as.integer(max(raw_data$order_date) + 1)
    max_act_test_date <- max(test_data_subset$order_date)
    melted_orders_test <- 
      melt(raw_data, id = "cust_no", measure.vars = "order_date")
#
# stip off any customers not in test set and filter only
# dates that are beyond the max date of the test set
#
    melted_orders_test <-
      melted_orders_test[melted_orders_test$cust_no %in% 
                           test_data_subset$cust_no &
                           melted_orders_test$value >
                           max_act_test_date, ]
#
# now pad the melted data with an entry beyond the max date
# for every customer, to deal with any customers in the
# test data that do not have a future order, by setting 
# those to the beyond_max_date so we can detect them later
#
    test_rows <- length(unique(test_data_subset$cust_no))
    test_melt_pad <- 
      data.frame("cust_no" = as.integer(unique(test_data_subset$cust_no)), 
                 "variable" = as.factor(as.character(rep("order_date", test_rows))), 
                 "value" = as.Date(as.integer(rep(beyond_max_date, test_rows)),
                 origin = "1970-01-01"))
    melted_orders_test <- rbind(melted_orders_test, test_melt_pad)
    next_orders_test <- 
      as.Date(acast(melted_orders_test, 
                    cust_no ~ variable, min), origin = "1970-01-01")
#
# set the lines that had no future order to NA
#    
    next_orders_test[next_orders_test == beyond_max_date] <- NA
#    
    max_act_val_date <- max(val_data_subset$order_date)
    melted_orders_val <- 
      melt(raw_data, id = "cust_no", measure.vars = "order_date")
#
# stip off any customers not in test set and filter only
# dates that are beyond the max date of the test set
#
    melted_orders_val <-
      melted_orders_val[melted_orders_val$cust_no %in% 
                           val_data_subset$cust_no &
                           melted_orders_val$value >
                           max_act_val_date, ]
#
# now pad the melted data with an entry beyond the max date
# for every customer, to deal with any customers in the
# val data that do not have a future order, by setting 
# those to the beyond_max_date so we can detect them later
#
    val_rows <- length(unique(val_data_subset$cust_no))
    val_melt_pad <- 
      data.frame("cust_no" = as.integer(unique(val_data_subset$cust_no)), 
                 "variable" = as.factor(as.character(rep("order_date", val_rows))), 
                 "value" = as.Date(as.integer(rep(beyond_max_date, val_rows)),
                                   origin = "1970-01-01"))
    melted_orders_val <- rbind(melted_orders_val, val_melt_pad)
    next_orders_val <- 
      as.Date(acast(melted_orders_val, 
                    cust_no ~ variable, min), origin = "1970-01-01")
#
# set the lines that had no future order to NA
#    
    next_orders_val[next_orders_val == beyond_max_date] <- NA
                           
#
# drop items with very few data only from the training data
#
    melted_cleaning_data <- 
      melt(train_data_subset, id = "fam_series",
           measure.vars = "order_date")
    melted_cleaning_data$value <- as.numeric(1)
    family_counts <- acast(melted_cleaning_data,
                           fam_series ~ variable,
                           sum)
    family_cutoff <- max(family_counts)
    family_names <- rownames(family_counts)
    family_counts <- 
      as.data.frame(cbind(family_names, family_counts))
    family_counts[, 1] <- as.character(family_counts[, 1])
    family_counts[, 2] <- as.numeric(as.character(family_counts[, 2]))
    colnames(family_counts) <- c("family", "no_of_item")
    remove_families <- 
      family_names[which(family_counts[, 2] < 
                           (0.1 * family_cutoff))]
    train_data_subset <- 
      train_data_subset[!(train_data_subset$fam_series %in% 
                            remove_families), ]
#
# transform training data for analyses
#
    melted_train_data <- melt(train_data_subset, id = "cust_no",
                              measure.vars = "order_date")
    ordered_mtd <- 
      melted_train_data[order(melted_train_data$value, decreasing = FALSE), ]
    if (plots_verbose == TRUE) {
      par(mfrow = c(3, 3))
      for (i in 1:length(unique(ordered_mtd$cust_no))) {
        if (nrow(ordered_mtd[ordered_mtd$cust_no == 
                             unique(ordered_mtd$cust_no)[i], ]) > 75) {
          hist(as.numeric(diff(ordered_mtd[ordered_mtd$cust_no == 
               unique(ordered_mtd$cust_no[i]), ]$value)),
               breaks = 
                 seq(as.numeric(min(diff(ordered_mtd[ordered_mtd$cust_no ==
                     unique(ordered_mtd$cust_no[i]), ]$value))),
                     as.numeric(max(diff(ordered_mtd[ordered_mtd$cust_no ==
                     unique(ordered_mtd$cust_no[i]), ]$value))) +
                     1 - (as.numeric(max(diff(ordered_mtd[ordered_mtd$cust_no ==
                     unique(ordered_mtd$cust_no[i]), ]$value))) %% 1),
                     1),
               xlim = c(0, 30),
               col = "lightblue",
               xlab = "time between orders (days)",
               main = paste0("Reordering distribution\n",
                             "customer number ",
                             ordered_mtd$cust_no[i]),
               cex.main = 0.75)
        }
      }
    }
    par(mfrow = c(1, 1))
    cust_start <- as.Date(acast(melted_train_data, 
                                cust_no ~ variable,
                                min),
                          origin = "1970-01-01")
    cust_ids <- rownames(cust_start)
    frequency <- melted_train_data
    frequency$value <- 1
    frequencies <- as.numeric(acast(frequency,
                                    cust_no ~ variable,
                                    sum))
     cust_end <- as.Date(acast(melted_train_data,
                              cust_no ~ variable,
                              max),
                        origin = "1970-01-01")
    cust_first <- 
      cust_firsts[cust_firsts$cust_no %in% 
                    train_data_subset$cust_no, 2]
    cust_longevity <- 
      as.numeric(as.Date(max_date, 
                         origin = "1970-01-01") - cust_first)
#
# calcualte average time between orders in the data set (using dplyr)
# if no prior order is found, set the time to the overall average
#
    ave_reorder <- melted_train_data %>% 
      group_by(cust_no) %>% 
      arrange(value) %>% 
      summarize(ave = as.numeric(mean(diff(value))))
    ave_reorder$ave[is.na(ave_reorder$ave)] <- 
      mean(ave_reorder$ave, na.rm = TRUE)
    colnames(ave_reorder) <- c("cust_no", "ave_reorder")
    ave_reorder <- ave_reorder[, 2]
#
# calcualte standard deviation of time between orders in the data set
# (using dplyr) if no prior order is found, set the time to the 
# overall average
#
    sd_reorder <- melted_train_data %>% 
      group_by(cust_no) %>% 
      arrange(value) %>% 
      summarize(ave = as.numeric(sd(diff(value))))
    sd_reorder$ave[is.na(sd_reorder$ave)] <- 
      mean(sd_reorder$ave, na.rm = TRUE)
    colnames(sd_reorder) <- c("cust_no", "sd_reorder")
    sd_reorder <- sd_reorder[, 2]
#    
    melted_train_data <- melt(train_data_subset, id = "cust_no",
                              measure.vars = "days_late")
    average_late <- (as.integer(acast(melted_train_data,
                                      cust_no ~ variable,
                                      mean)))
    melted_train_data <- melt(train_data_subset, id = "cust_no",
                              measure.vars = "value")
    value_per_order <- (as.numeric(acast(melted_train_data,
                                         cust_no ~ variable,
                                         mean)))
    value_sds <- as.numeric(acast(melted_train_data,
                                  cust_no ~ variable,
                                  sd))
    value_sds[is.na(value_sds)] <- 0
    train_cust_summary <- data.frame(cust_ids,
                                     cust_start,
                                     cust_end,
                                     frequencies,
                                     value_per_order,
                                     value_sds,
                                     average_late,
                                     recency = 0,
                                     ave_reorder,
                                     sd_reorder,
                                     next_order = 0,
                                     cust_longevity,
                                     last_series = "",
                                     last_item = "",
                                     stringsAsFactors = FALSE)
    train_cust_summary$recency <- 
      as.integer(as.Date(max_date, origin = "1970-01-01") - 
                   cust_end)
#
# clean off any negative order values
#
    train_cust_summary <- 
      train_cust_summary[train_cust_summary$value_per_order > 0, ]
#
# populate the last ordered series for each instance
#
    for (i in 1:nrow(train_cust_summary)) {
      train_cust_summary[i, "last_series"] <- 
        train_data_subset[((train_data_subset$cust_no == 
                 train_cust_summary$cust_ids[i]) & 
                 (train_data_subset$order_date == 
                 train_cust_summary$cust_end[i])), ][1, "fam_series"]
    }
#
# create dummy variables from the last series purchased
#
    last_series_factors <- 
      factor(train_cust_summary$last_series)
    series_dummies <- model.matrix(~last_series_factors)
    for (i in 1:nrow(series_dummies)) {
      row_sum <- 
        sum(series_dummies[i, 2:ncol(series_dummies)])
      if(row_sum == 0) {
        series_dummies[i, 1] <- 1
      } else {
        series_dummies[i, 1] <- 0
      }
    }
    dummies_temp1 <- 
      as.data.frame(matrix(nrow = 
                             nrow(train_cust_summary), ncol = 1))
    dropped_dummies <- numeric()
    for (i in 1:ncol(series_dummies)) {
      if (mean(series_dummies[, i]) != 0) {
        dummies_temp1 <- 
          cbind(dummies_temp1, series_dummies[, i])
      } else {
        dropped_dummies <- c(dropped_dummies, i)
      }
    }
    series_dummies <- dummies_temp1[, 2:ncol(dummies_temp1)]
    fac_melt <- melt(last_series_factors, 
                     id = "value", 
                     measure = "")
    fac_names <- rownames(acast(fac_melt,value ~ ""))
    if (length(dropped_dummies) > 0) {
      fac_names <- fac_names[-dropped_dummies]
    }
    colnames(series_dummies) <- fac_names
    train_cust_summary$last_series <- NULL
    train_cust_summary <- 
      cbind(train_cust_summary, series_dummies)
#
# create dummy variables from last item numbers ordered
#
    for (i in 1:nrow(train_cust_summary)) {
      train_cust_summary[i, "last_item"] <- 
       train_data_subset[((train_data_subset$cust_no == 
                 train_cust_summary$cust_ids[i]) & 
                 (train_data_subset$order_date == 
                 train_cust_summary$cust_end[i])), ][1, "Item.Number"]
      if (!(train_cust_summary[i, ]$last_item %in% 
            item_list$Item_Number)) {
        train_cust_summary[i, ]$last_item <- "other"
      }
    }
    item_no_factors <- factor(train_cust_summary$last_item)
    items_dummies <- 
      model.matrix(train_cust_summary$last_item ~ item_no_factors)
    for (i in 1:nrow(items_dummies)) {
      row_sum <- sum(items_dummies[i, 2:ncol(items_dummies)])
      if(row_sum == 0) {
        items_dummies[i, 1] <- 1
      } else {
        items_dummies[i, 1] <- 0
      }
    }
    dummies_temp2 <- 
      as.data.frame(matrix(nrow = 
                             nrow(train_cust_summary), ncol = 1))
    dropped_dummies <- numeric()
    for (i in 1:ncol(items_dummies)) {
      if (mean(items_dummies[, i]) != 0) {
        dummies_temp2 <- 
          cbind(dummies_temp2, items_dummies[, i])
      } else {
        dropped_dummies <- c(dropped_dummies, i)
      }
    }
    items_dummies <- dummies_temp2[, 2:ncol(dummies_temp2)]
    fac_melt <- melt(item_no_factors, 
                     id = "value", 
                     measure = "")
    fac_names <- rownames(acast(fac_melt,value ~ ""))
    if (length(dropped_dummies) > 0) {
      fac_names <- fac_names[-dropped_dummies]
    }
    colnames(items_dummies) <- fac_names
    train_cust_summary$last_item <- NULL
    train_cust_summary <- 
      cbind(train_cust_summary, items_dummies)   
#
# save a copy of the data beore scaling
#
    train_cust_summary_raw <- train_cust_summary
#
    if (plots_verbose == TRUE) {
      par(mfrow = c(3, 2))
    }
    if (plots_verbose == TRUE) {
      longevity_hist_cutoff <- 2100
      longevity_hist_bin_size <- 30
      hist(train_cust_summary$cust_longevity[
        train_cust_summary$cust_longevity <= longevity_hist_cutoff &
          train_cust_summary$cust_longevity > 0],
        breaks = seq(0, 
                     longevity_hist_cutoff +
                       longevity_hist_bin_size -
                       longevity_hist_cutoff %% longevity_hist_bin_size,
                     longevity_hist_bin_size),
        col = "lightblue",
        main = paste0("Training Data\n",
                      "Longevity\n",
                      "(earliest order, days before ", 
                      as.Date(max_date, origin = "1970-01-01"), ")"),
        cex.main = 0.75,
        ylab = "count",
        xlab = "days")
    }
#    
    if (plots_verbose == TRUE) {
      recency_hist_cutoff <- 730
      recency_hist_bins <- 35
      hist(train_cust_summary$recency[
        train_cust_summary$recency <= 
          recency_hist_cutoff],
        breaks = 
          seq(0, 
              recency_hist_cutoff + 
                ceiling(recency_hist_cutoff / recency_hist_bins),
              ceiling(recency_hist_cutoff / recency_hist_bins)),
        col = "lightblue",
        main = paste0("Training data\n",
                      "Recency\n",
                      "(last order, days before ", 
                      as.Date(max_date, origin = "1970-01-01"), ")"),
        cex.main = 0.75,
        ylab = "count",
        xlab = "days")
    }
#
    reorder_hist_cutoff <- 75
    reorder_hist_bins <- 75
    temp_hist <- hist(train_cust_summary_raw$ave_reorder[
      train_cust_summary_raw$ave_reorder <= 
        reorder_hist_cutoff],
      breaks = 
        seq(0, 
            ceiling(reorder_hist_cutoff + 
              ceiling(reorder_hist_cutoff / reorder_hist_bins)),
            ceiling(reorder_hist_cutoff / reorder_hist_bins)),
      col = "lightblue",
      main = paste0("Training data\n",
                    "Reorder\n",
                    "(days between orders)"),
      cex.main = 0.75,
      ylab = "count",
      xlab = "days",
      plot = TRUE)
#
# store the value histogram counts for the training data
#
    train_reorder_hist_act <- 
      data.frame("day" = numeric(length = length(temp_hist$breaks) - 1),
                 "prob" = numeric(length = length(temp_hist$counts)))
    train_reorder_hist_act$day <- 
      temp_hist$breaks[2:length(temp_hist$breaks)] -
      (temp_hist$breaks[2] - temp_hist$breaks[1]) / 2
    train_reorder_hist_act$prob <- temp_hist$counts / max(temp_hist$counts)
#  
    if (plots_verbose == TRUE) {
      freq_hist_cutoff <- 20
      freq_hist_bins <- 35
      hist(train_cust_summary$frequencies[
        train_cust_summary$frequencies <= 
          freq_hist_cutoff],
        breaks = 
          seq(0, 
              freq_hist_cutoff + 
                ceiling(freq_hist_cutoff / freq_hist_bins),
              ceiling(freq_hist_cutoff / freq_hist_bins)),
        col = "lightblue",
        main = paste0("Training data\n",
                      "Frequency of Orders\n",
                      "(# of orders since ",
                      min_date, " < ", 
                      freq_hist_cutoff, ")"),
        cex.main = 0.75,
        ylab = "count",
        xlab = "# of orders")
    }
#    
    value_hist_cutoff <- 50000
    value_hist_bins <- 35
    temp_hist <- hist(train_cust_summary$value_per_order[
      train_cust_summary$value_per_order <= 
        value_hist_cutoff],
         breaks = 
           seq(0, 
               value_hist_cutoff + 
                 ceiling(value_hist_cutoff / value_hist_bins),
               ceiling(value_hist_cutoff / value_hist_bins)),
         col = "lightblue",
         main = paste0("Training Data\n",
                       "Avg. Value of Orders\n",
                       "(Avg. cust. order < $",
                       value_hist_cutoff, ")"),
         cex.main = 0.75,
         ylab = "count",
         xlab = "Avg. order value ($)",
      plot = TRUE)
    par(mfrow = c(1, 1))
#
# store the value histogram counts for the training data
#
    train_value_hist_act <- 
      data.frame("value" = numeric(length = length(temp_hist$breaks) - 1),
                 "prob" = numeric(length = length(temp_hist$counts)))
    train_value_hist_act$value <- 
      temp_hist$breaks[2:length(temp_hist$breaks)] -
      (temp_hist$breaks[2] - temp_hist$breaks[1]) / 2
    train_value_hist_act$prob <- temp_hist$counts / max(temp_hist$counts)
#
# assign row & column names to training data
# 
    rownames(train_cust_summary) <- 
      train_cust_summary$cust_ids
#
# transform data to numeric to use in clustering models
#
    train_cust_summary$cust_ids <- 
      as.numeric(train_cust_summary$cust_ids)
# 
    train_cust_summary$cust_start <- 
      as.numeric(train_cust_summary$cust_start)
    train_cust_summary$cust_end <- 
      as.numeric(train_cust_summary$cust_end)
#    
# scale data to use in clustering models
#
    train_cust_summary$cust_ids <-
      scale(train_cust_summary$cust_ids)
    ids_center <-
      attr(train_cust_summary$cust_ids, 
           "scaled:center")
    ids_scale <-
      attr(train_cust_summary$cust_ids, 
           "scaled:scale")
#  
    train_cust_summary$cust_start <-
      scale(train_cust_summary$cust_start)
    start_center <-
      attr(train_cust_summary$cust_start, 
           "scaled:center")
    start_scale <-
      attr(train_cust_summary$cust_start, 
           "scaled:scale")
#
    train_cust_summary$cust_end <-
      scale(train_cust_summary$cust_end)
    end_center <- 
      attr(train_cust_summary$cust_end, 
           "scaled:center")
    end_scale <-
      attr(train_cust_summary$cust_end, 
           "scaled:scale")
#  
    train_cust_summary$frequencies <- 
      scale(train_cust_summary$frequencies)
    freq_center <- 
      attr(train_cust_summary$frequencies, 
           "scaled:center")
    freq_scale <- 
      attr(train_cust_summary$frequencies, 
           "scaled:scale")
#
    train_cust_summary$average_late <-
      scale(train_cust_summary$average_late)
    lateness_center <- 
      attr(train_cust_summary$average_late, 
           "scaled:center")
    lateness_scale <-
      attr(train_cust_summary$average_late, 
           "scaled:scale")
#
    train_cust_summary$value_per_order <- 
      scale(train_cust_summary$value_per_order)
    value_center <- 
      attr(train_cust_summary$value_per_order, 
           "scaled:center")
    value_scale <- 
      attr(train_cust_summary$value_per_order, 
           "scaled:scale")
#
    train_cust_summary$value_sds <- 
      scale(train_cust_summary$value_sds)
    value_sds_center <- 
      attr(train_cust_summary$value_sds, 
           "scaled:center")
    value_sds_scale <- 
      attr(train_cust_summary$value_sds, 
           "scaled:scale")
#
    train_cust_summary$recency <- 
      scale(train_cust_summary$recency)
    recency_center <- 
      attr(train_cust_summary$recency, 
           "scaled:center")
    recency_scale <- 
      attr(train_cust_summary$recency, 
           "scaled:scale")
#
    train_cust_summary$ave_reorder <- 
      scale(train_cust_summary$ave_reorder)
    reorder_center <- 
      attr(train_cust_summary$ave_reorder, 
           "scaled:center")
    reorder_scale <- 
      attr(train_cust_summary$ave_reorder, 
           "scaled:scale")
#
    train_cust_summary$sd_reorder <- 
      scale(train_cust_summary$sd_reorder)
    sd_reorder_center <- 
      attr(train_cust_summary$sd_reorder, 
           "scaled:center")
    sd_reorder_scale <- 
      attr(train_cust_summary$sd_reorder, 
           "scaled:scale")  
#
    train_cust_summary$cust_longevity <- 
      scale(train_cust_summary$cust_longevity)
    longevity_center <- 
      attr(train_cust_summary$cust_longevity, 
           "scaled:center")
    longevity_scale <- 
      attr(train_cust_summary$cust_longevity, 
           "scaled:scale")    
#
# transform test data for analyses
#
    melted_test_data <- 
      melt(test_data_subset, id = "cust_no",
           measure.vars = "order_date")
    cust_start <- as.Date(acast(melted_test_data, 
                                cust_no ~ variable,
                                min),
                          origin = "1970-01-01")
    cust_ids <- rownames(cust_start)
    frequency <- melted_test_data
    frequency$value <- 1
    frequencies <- as.numeric(acast(frequency,
                                    cust_no ~ variable,
                                    sum))
    cust_end <- as.Date(acast(melted_test_data,
                              cust_no ~ variable,
                              max),
                        origin = "1970-01-01")
    cust_first <- 
      cust_firsts[cust_firsts$cust_no %in% 
                    test_data_subset$cust_no, 2]
    cust_longevity <- 
      as.numeric(as.Date(max_date, 
                         origin = "1970-01-01") - cust_first)
#
# calcualte average time between orders in the data set (using dplyr)
# if no prior order is found, set the time to verall average
#
    ave_reorder <- melted_test_data %>% 
      group_by(cust_no) %>% 
      arrange(value) %>% 
      summarize(ave = as.numeric(mean(diff(value))))
    ave_reorder$ave[is.na(ave_reorder$ave)] <- 
      mean(ave_reorder$ave, na.rm = TRUE)
    colnames(ave_reorder) <- c("cust_no", "ave_reorder")
    ave_reorder <- ave_reorder[, 2]
#
# calcualte standard difference of time between orders in the data set
# (using dplyr)if no prior order is found, set the time to the 
# overall average
#
    sd_reorder <- melted_test_data %>% 
      group_by(cust_no) %>% 
      arrange(value) %>% 
      summarize(ave = as.numeric(sd(diff(value))))
    sd_reorder$ave[is.na(sd_reorder$ave)] <- 
      mean(sd_reorder$ave, na.rm = TRUE)
    colnames(sd_reorder) <- c("cust_no", "sd_reorder")
    sd_reorder <- sd_reorder[, 2]    
#  
    test_max_date <- as.Date(max(test_data_subset$order_date),
                             origin = "1970-01-01")
    melted_test_data <- 
      melt(test_data_subset, id = "cust_no",
           measure.vars = "days_late")
    average_late <- 
      (as.integer(acast(melted_test_data,
                        cust_no ~ variable,
                        mean)))
    melted_test_data <- 
      melt(test_data_subset, id = "cust_no",
           measure.vars = "value")
    value_per_order <- 
      (as.numeric(acast(melted_test_data,
                        cust_no ~ variable,
                        mean)))
    value_sds <- as.numeric(acast(melted_test_data,
                                  cust_no ~ variable,
                                  sd))
    value_sds[is.na(value_sds)] <- 0
    test_cust_summary <- data.frame(cust_ids,
                                    cust_start,
                                    cust_end,
                                    frequencies,
                                    value_per_order,
                                    value_sds,
                                    average_late,
                                    recency = 0,
                                    ave_reorder,
                                    sd_reorder,
                                    next_order = next_orders_test,
                                    cust_longevity,
                                    last_series = "",
                                    last_item = "",
                                    stringsAsFactors = FALSE)
    test_cust_summary$recency <- 
      as.integer(test_max_date - cust_end)
#
# clean off any negative order values
#
    test_cust_summary <- 
      test_cust_summary[test_cust_summary$value_per_order > 0, ]
#
# populate the last ordered series for each instance
#
    for (i in 1:nrow(test_cust_summary)) {
      test_cust_summary[i, "last_series"] <- 
        test_data_subset[((test_data_subset$cust_no == 
              test_cust_summary$cust_ids[i]) & 
             (test_data_subset$order_date == 
                test_cust_summary$cust_end[i])), ][1, "fam_series"]
    }
#
# create dummy variables in test data for last series ordered
# reuse last_series_factors to ensure same variables as train data
#
    dummies_temp3 <- 
      as.data.frame(matrix(0, ncol = ncol(series_dummies),
                           nrow = nrow(test_cust_summary)))
    dummies_temp3_names <- colnames(series_dummies)
    colnames(dummies_temp3) <- dummies_temp3_names
    for (i in 1:nrow(test_cust_summary)) {
      if (test_cust_summary$last_series[i] %in% dummies_temp3_names) {
        dummies_temp3[i, test_cust_summary$last_series[i]] <- 1
      }
    }
    test_cust_summary$last_series <- NULL
    test_cust_summary <- 
      cbind(test_cust_summary, dummies_temp3)
#
# create dummy variables from item numbers
# resuse item list from training data to ensure
# same varibles in test data
#
    for (i in 1:nrow(test_cust_summary)) {
      test_cust_summary[i, "last_item"] <- 
        test_data_subset[
          ((test_data_subset$cust_no == 
              test_cust_summary$cust_ids[i]) & 
             (test_data_subset$order_date == 
                test_cust_summary$cust_end[i])), ][1, "Item.Number"]
      if (!(test_cust_summary[i, ]$last_item %in% 
            item_list$Item_Number)) {
        test_cust_summary[i, ]$last_item <- "other"
      }
    }
    dummies_temp5 <- 
      as.data.frame(matrix(0, ncol = ncol(items_dummies),
                           nrow = nrow(test_cust_summary)))
    dummies_temp5_names <- colnames(items_dummies)
    colnames(dummies_temp5) <- dummies_temp5_names
    for (i in 1:nrow(test_cust_summary)) {
      if (test_cust_summary$last_item[i] %in% dummies_temp5_names) {
        dummies_temp5[i, test_cust_summary$last_item[i]] <- 1
      }
    }
    test_cust_summary$last_item <- NULL
    test_cust_summary <- 
      cbind(test_cust_summary, dummies_temp5)   
#
# save a copy of the data before scaling
#
    test_cust_summary_raw <- test_cust_summary
#
    if (plots_verbose == TRUE) {
      par(mfrow = c(3, 2))
    }
    if (plots_verbose == TRUE) {
      longevity_hist_cutoff <- 2100
      longevity_hist_bin_size <- 30
      hist(test_cust_summary$cust_longevity[
        test_cust_summary$cust_longevity <= longevity_hist_cutoff &
          test_cust_summary$cust_longevity > 0],
        breaks = seq(0, 
                     longevity_hist_cutoff +
                       longevity_hist_bin_size -
                       longevity_hist_cutoff %% longevity_hist_bin_size,
                     longevity_hist_bin_size),
        col = "lightblue",
        main = paste0("Test Data\n",
                      "Longevity\n",
                      "(earliest order, days before ", 
                      as.Date(max_date, origin = "1970-01-01"), ")"),
        cex.main = 0.75,
        ylab = "count",
        xlab = "days")
    }
#
    if (plots_verbose == TRUE) {
      recency_hist_cutoff <- 730
      recency_hist_bins <- 35
      hist(test_cust_summary$recency[
        test_cust_summary$recency <= 
          recency_hist_cutoff],
        breaks = seq(0, 
                     recency_hist_cutoff + 
                       ceiling(recency_hist_cutoff / 
                                 recency_hist_bins),
                     ceiling(recency_hist_cutoff / 
                               recency_hist_bins)),
        col = "lightblue",
        main = paste0("Test Data\n",
                      "Recency\n",
                      "(last order, days before ", 
                      as.Date(test_max_date, orgin = "1970-01-01"), ")"),
        cex.main = 0.75,
        ylab = "count",
        xlab = "days")
    }
#
    reorder_hist_cutoff <- 250
    reorder_hist_bins <- 35
    hist(test_cust_summary$ave_reorder[
      test_cust_summary$ave_reorder <= 
        reorder_hist_cutoff],
      breaks = seq(0, 
                   reorder_hist_cutoff + 
                     ceiling(reorder_hist_cutoff / 
                               reorder_hist_bins),
                   ceiling(reorder_hist_cutoff / 
                             reorder_hist_bins)),
      col = "lightblue",
      main = paste0("Test Data\n",
                    "Reorder\n",
                    "(days between orders)"),
      cex.main = 0.75,
      ylab = "count",
      xlab = "days")
#
    if (plots_verbose == TRUE) {
      freq_hist_cutoff <- 20
      freq_hist_bins <- 35
      hist(test_cust_summary$frequencies[
        test_cust_summary$frequencies <= 
          freq_hist_cutoff],
        breaks = seq(0, 
                     freq_hist_cutoff + 
                       ceiling(freq_hist_cutoff / 
                                 freq_hist_bins),
                     ceiling(freq_hist_cutoff / 
                               freq_hist_bins)),
        col = "lightblue",
        main = paste0("Test Data\n",
                      "Frequency of Orders\n",
                      "(# of orders since ",
                      min_date, " < ", 
                      freq_hist_cutoff, ")"),
        cex.main = 0.75,
        ylab = "count",
        xlab = "# of orders")
    }
#    
    value_hist_cutoff <- 50000
    value_hist_bins <- 35
    hist(test_cust_summary$value_per_order[
      test_cust_summary$value_per_order <= 
        value_hist_cutoff],
         breaks = seq(0, 
                      value_hist_cutoff + 
                        ceiling(value_hist_cutoff / 
                                  value_hist_bins),
                      ceiling(value_hist_cutoff / 
                                value_hist_bins)),
         col = "lightblue",
         main = paste0("Test Data\n",
                       "Avg. Value of Orders\n",
                       "(Avg. cust. order < $",
                       value_hist_cutoff, ")"),
         cex.main = 0.75,
         ylab = "count",
         xlab = "Avg. order value ($)") 
    par(mfrow = c(1, 1))
#
    rownames(test_cust_summary) <- 
      test_cust_summary$cust_ids
    test_cust_summary$cust_ids <- 
      as.numeric(test_cust_summary$cust_ids)
#
        test_cust_summary$cust_start <- 
      as.numeric(test_cust_summary$cust_start)
    test_cust_summary$cust_end <-
      as.numeric(test_cust_summary$cust_end)
#    
# scale the data using the factors from the training data    
#
    test_cust_summary$cust_ids <-
      (test_cust_summary$cust_ids - ids_center) /
      ids_scale
#  
    test_cust_summary$cust_start <-
      (test_cust_summary$cust_start - start_center) /
      start_scale
#
    test_cust_summary$cust_end <-
      (test_cust_summary$cust_end - end_center) / 
      end_scale
#
    test_cust_summary$frequencies <- 
      (test_cust_summary$frequencies - freq_center) /
      freq_scale
#
    test_cust_summary$average_late <-
      (test_cust_summary$average_late - lateness_center) /
      lateness_scale
#  
    test_cust_summary$value_per_order <- 
      (test_cust_summary$value_per_order - value_center) /
      value_scale
#  
    test_cust_summary$value_sds <- 
      (test_cust_summary$value_sds - value_sds_center) /
      value_sds_scale    
#
    test_cust_summary$recency <- 
      (test_cust_summary$recency - recency_center) /
      recency_scale
#
    test_cust_summary$ave_reorder <- 
      (test_cust_summary$ave_reorder - reorder_center) /
      reorder_scale
#
    test_cust_summary$sd_reorder <- 
      (test_cust_summary$sd_reorder - sd_reorder_center) /
      sd_reorder_scale
#
# define forecast period that will be used to predict
# revenue using the cluster distributions and create
# a data.frame to store the results so they can be written out later
#    
    forecast_period <- 
      min(90, max(raw_data$order_date) - max_act_test_date)
    test_forecast <- as.data.frame(matrix(data = numeric(),
                                          nrow = forecast_period,
                                          ncol = 
                                            6 * 
                                            length(all_permutations) + 1))
    forecast_colnames <- "time"
    for (i in 1:length(all_permutations)) {
      forecast_colnames <- 
        c(forecast_colnames, 
          paste0("pred_val_", all_permutations[i]),
          paste0("cum_pred_", all_permutations[i]),
          paste0("act_val_", all_permutations[i]),
          paste0("cum_act_", all_permutations[i]),
          paste0("accuracy_", all_permutations[i]),
          paste0("cum_acc_", all_permutations[i]))
    }
    colnames(test_forecast) <- forecast_colnames
    test_forecast[, ]$time <- seq(1, forecast_period, 1)
#
# transform validation data for analyses
#
    melted_val_data <- 
      melt(val_data_subset, id = "cust_no",
           measure.vars = "order_date")
    cust_start <- as.Date(acast(melted_val_data, 
                                cust_no ~ variable,
                                min),
                          origin = "1970-01-01")
    cust_ids <- rownames(cust_start)
    frequency <- melted_val_data
    frequency$value <- 1
    frequencies <- as.numeric(acast(frequency,
                                    cust_no ~ variable,
                                    sum))
    cust_end <- as.Date(acast(melted_val_data,
                              cust_no ~ variable,
                              max),
                        origin = "1970-01-01")
    cust_first <- 
      cust_firsts[cust_firsts$cust_no %in% 
                    val_data_subset$cust_no, 2]
    val_max_date <- max(val_data_subset$order_date)
    cust_longevity <- 
      as.numeric(as.Date(val_max_date, 
                         origin = "1970-01-01") - cust_first)
#
# adjust longevity for the validation data since it is all
# from later times than the training / test data
#
    cust_longevity <- cust_longevity - as.numeric(val_max_date - max_date)
#
# calcualte average time between orders in the data set (using dplyr)
# if no prior order is found, set the time to the overall average
#
    ave_reorder <- melted_val_data %>% 
      group_by(cust_no) %>% 
      arrange(value) %>% 
      summarize(ave = as.numeric(mean(diff(value))))
    ave_reorder$ave[is.na(ave_reorder$ave)] <- 
      mean(ave_reorder$ave, na.rm = TRUE)
    colnames(ave_reorder) <- c("cust_no", "ave_reorder")
    ave_reorder <- ave_reorder[, 2]
#
# calcualte standard deviation of time between orders in the data set
# (using dplyr)if no prior order is found, set the time to the 
# overall average
#
    sd_reorder <- melted_val_data %>% 
      group_by(cust_no) %>% 
      arrange(value) %>% 
      summarize(ave = as.numeric(sd(diff(value))))
    sd_reorder$ave[is.na(sd_reorder$ave)] <- 
      mean(sd_reorder$ave, na.rm = TRUE)
    colnames(sd_reorder) <- c("cust_no", "sd_reorder")
    sd_reorder <- sd_reorder[, 2]    
#  
    val_max_date <- as.Date(max(val_data_subset$order_date),
                        origin = "1970-01-01")
    melted_val_data <- 
      melt(val_data_subset, id = "cust_no",
           measure.vars = "days_late")
    average_late <- 
      (as.integer(acast(melted_val_data,
                        cust_no ~ variable,
                        mean)))
    melted_val_data <- 
      melt(val_data_subset, id = "cust_no",
           measure.vars = "value")
    value_per_order <- 
      (as.numeric(acast(melted_val_data,
                        cust_no ~ variable,
                        mean)))
    value_sds <- as.numeric(acast(melted_val_data,
                                  cust_no ~ variable,
                                  sd))
    value_sds[is.na(value_sds)] <- 0
    val_cust_summary <- data.frame(cust_ids,
                                   cust_start,
                                   cust_end,
                                   frequencies,
                                   value_per_order,
                                   value_sds,
                                   average_late,
                                   recency = 0,
                                   ave_reorder,
                                   sd_reorder,
                                   next_order = next_orders_val,
                                   cust_longevity,
                                   last_series = "",
                                   last_item = "",
                                   stringsAsFactors = FALSE)
    val_cust_summary$recency <- 
      as.integer(val_max_date - cust_end)
    if(nrow(val_cust_summary[
      val_cust_summary$value_per_order < 0, ]) > 0) {
      val_cust_summary[
        val_cust_summary$value_per_order < 0, ]$value_per_order <- 0
    }
    for (i in 1:nrow(val_cust_summary)) {
      val_cust_summary[i, "last_series"] <- 
        val_data_subset[
          ((val_data_subset$cust_no == 
              val_cust_summary$cust_ids[i]) & 
             (val_data_subset$order_date == 
                val_cust_summary$cust_end[i])), ][1, "fam_series"]
    }
#
# create dummy variables in test data for series
# reuse last_series_factors to ensure same variables as train data
#
    dummies_temp3 <- 
      as.data.frame(matrix(0, ncol = ncol(series_dummies),
                           nrow = nrow(val_cust_summary)))
    dummies_temp3_names <- colnames(series_dummies)
    colnames(dummies_temp3) <- dummies_temp3_names
    for (i in 1:nrow(val_cust_summary)) {
      if (val_cust_summary$last_series[i] %in% dummies_temp3_names) {
        dummies_temp3[i, val_cust_summary$last_series[i]] <- 1
      }
    }
    val_cust_summary$last_series <- NULL
    val_cust_summary <- 
      cbind(val_cust_summary, dummies_temp3)
#
# create dummy variables from item numbers
# resuse item list from training data to ensure
# same varibles in test data
#
    for (i in 1:nrow(val_cust_summary)) {
      val_cust_summary[i, "last_item"] <- 
        val_data_subset[
          ((val_data_subset$cust_no == 
              val_cust_summary$cust_ids[i]) & 
             (val_data_subset$order_date == 
                val_cust_summary$cust_end[i])), ][1, "Item.Number"]
      if (!(val_cust_summary[i, ]$last_item %in% 
            item_list$Item_Number)) {
        val_cust_summary[i, ]$last_item <- "other"
      }
    }
    dummies_temp5 <- 
      as.data.frame(matrix(0, ncol = ncol(items_dummies),
                           nrow = nrow(val_cust_summary)))
    dummies_temp5_names <- colnames(items_dummies)
    colnames(dummies_temp5) <- dummies_temp5_names
    for (i in 1:nrow(val_cust_summary)) {
      if (val_cust_summary$last_item[i] %in% dummies_temp5_names) {
        dummies_temp5[i, val_cust_summary$last_item[i]] <- 1
      }
    }
    val_cust_summary$last_item <- NULL
    val_cust_summary <- 
      cbind(val_cust_summary, dummies_temp5)    
#
# save a copy of the data    
#
    val_cust_summary_raw <- val_cust_summary
#    
    if (plots_verbose == TRUE) {
      par(mfrow = c(3, 2))
    }
    if (plots_verbose == TRUE) {
      longevity_hist_cutoff <- 2100
      longevity_hist_bin_size <- 30
      hist(val_cust_summary_raw$cust_longevity[
        val_cust_summary_raw$cust_longevity <= longevity_hist_cutoff &
          val_cust_summary_raw$cust_longevity > 0],
        breaks = seq(0, 
                     longevity_hist_cutoff +
                       longevity_hist_bin_size -
                       longevity_hist_cutoff %% longevity_hist_bin_size,
                     longevity_hist_bin_size),
        col = "lightblue",
        main = paste0("Validation Data\n",
                      "Longevity\n",
                      "(earliest order, days before ", 
                      as.Date(val_max_date, origin = "1970-01-01"), ")"),
        cex.main = 0.75,
        ylab = "count",
        xlab = "days")
    }
#    
    if (plots_verbose == TRUE) {
      recency_hist_cutoff <- 182
      recency_hist_bins <- 35
      hist(val_cust_summary$recency[
        val_cust_summary$recency <= 
          recency_hist_cutoff],
        breaks = seq(0, 
                     recency_hist_cutoff + 
                       ceiling(recency_hist_cutoff / 
                                 recency_hist_bins),
                     ceiling(recency_hist_cutoff / 
                               recency_hist_bins)),
        col = "lightblue",
        main = paste0("Validation Data\n",
                      "Recency\n",
                      "(last order, days before ", 
                      as.Date(val_max_date, origin = "1970-01-01"), ")"),
        cex.main = 0.75,
        ylab = "count",
        xlab = "days")
    }
#
    reorder_hist_cutoff <- 250
    reorder_hist_bins <- 35
    hist(val_cust_summary$ave_reorder[
      val_cust_summary$ave_reorder <= 
        reorder_hist_cutoff],
      breaks = seq(0, 
                   reorder_hist_cutoff + 
                     ceiling(reorder_hist_cutoff / 
                               reorder_hist_bins),
                   ceiling(reorder_hist_cutoff / 
                             reorder_hist_bins)),
      col = "lightblue",
      main = paste0("Validation Data\n",
                    "Reorder\n",
                    "(days between orders)"),
      cex.main = 0.75,
      ylab = "count",
      xlab = "days")
#
    if (plots_verbose == TRUE) {
      freq_hist_cutoff <- 20
      freq_hist_bins <- 35
      hist(val_cust_summary$frequencies[
        val_cust_summary$frequencies <= 
          freq_hist_cutoff],
        breaks = seq(0, 
                     freq_hist_cutoff + 
                       ceiling(freq_hist_cutoff / 
                                 freq_hist_bins),
                     ceiling(freq_hist_cutoff / 
                               freq_hist_bins)),
        col = "lightblue",
        main = paste0("Validation Data\n",
                      "Frequency of Orders\n",
                      "(# of orders since ",
                      min_date, " < ", 
                      freq_hist_cutoff, ")"),
        cex.main = 0.75,
        ylab = "count",
        xlab = "# of orders")
    }
#
    value_hist_cutoff <- 50000
    value_hist_bins <- 35
    hist(val_cust_summary$value_per_order[
      val_cust_summary$value_per_order <= 
        value_hist_cutoff],
      breaks = seq(0, 
                   value_hist_cutoff + 
                     ceiling(value_hist_cutoff / 
                               value_hist_bins),
                   ceiling(value_hist_cutoff / 
                             value_hist_bins)),
      col = "lightblue",
      main = paste0("Validation Data\n",
                    "Avg. Value of Orders\n",
                    "(Avg. cust. order < $",
                    value_hist_cutoff, ")"),
      cex.main = 0.75,
      ylab = "count",
      xlab = "Avg. order value ($)") 
    par(mfrow = c(1, 1))
#
    rownames(val_cust_summary) <- 
      val_cust_summary$cust_ids
    val_cust_summary$cust_ids <- 
      as.numeric(val_cust_summary$cust_ids)
#
    val_cust_summary$cust_start <- 
      as.numeric(val_cust_summary$cust_start)
    val_cust_summary$cust_end <-
      as.numeric(val_cust_summary$cust_end)
#
# scale the data using the factors from the training data
#    
    val_cust_summary$cust_ids <-
      (val_cust_summary$cust_ids - ids_center) /
      ids_scale
#  
    val_cust_summary$cust_start <-
      (val_cust_summary$cust_start - start_center) /
      start_scale
#
    val_cust_summary$cust_end <-
      (val_cust_summary$cust_end - end_center) / 
      end_scale
#
    val_cust_summary$frequencies <- 
      (val_cust_summary$frequencies - freq_center) /
      freq_scale
#
    val_cust_summary$average_late <-
      (val_cust_summary$average_late - lateness_center) /
      lateness_scale
#  
    val_cust_summary$value_per_order <- 
      (val_cust_summary$value_per_order - value_center) /
      value_scale
#  
    val_cust_summary$value_sds <- 
      (val_cust_summary$value_sds - value_sds_center) /
      value_sds_scale
#
    val_cust_summary$recency <- 
      (val_cust_summary$recency - recency_center) /
      recency_scale   
#
    val_cust_summary$ave_reorder <-
      (val_cust_summary$ave_reorder - reorder_center) /
      reorder_scale
#
    val_cust_summary$sd_reorder <-
      (val_cust_summary$sd_reorder - sd_reorder_center) /
      sd_reorder_scale
#
    val_cust_summary$sd_reorder <-
      (val_cust_summary$sd_reorder - sd_reorder_center) /
      sd_reorder_scale
#
# define forecast period that will be used to predict
# revenue using the cluster distributions and create
# a data.frame to store the results so they can be written out later
#    
    forecast_period <- 
      min(90, max(raw_data$order_date) - max_act_test_date)
    val_forecast <- as.data.frame(matrix(data = numeric(),
                                          nrow = forecast_period,
                                          ncol = 
                                            6 * 
                                            length(all_permutations) + 1))
    forecast_colnames <- "time"
    for (i in 1:length(all_permutations)) {
      forecast_colnames <- 
        c(forecast_colnames, 
          paste0("pred_val_", all_permutations[i]),
          paste0("cum_pred_", all_permutations[i]),
          paste0("act_val_", all_permutations[i]),
          paste0("cum_act_", all_permutations[i]),
          paste0("accuracy_", all_permutations[i]),
          paste0("cum_acc_", all_permutations[i]))
    }
    colnames(val_forecast) <- forecast_colnames
    val_forecast[, ]$time <- seq(1, forecast_period, 1)
#    
    for (cluster_method in 1:length(all_methods)) {
      for (clusters_used in clusters) {
#
        use_kmeans <- FALSE
        use_hclust <- FALSE
        if (all_methods[cluster_method] %in%
            kmeans_algorithms) {
          use_kmeans <- TRUE
        } else {
          use_hclust <- TRUE
        }
#
        if (use_kmeans == TRUE) {
          
          train_cust_summary_temp <- train_cust_summary
          test_cust_summary_temp <- test_cust_summary
          val_cust_summary_temp <- val_cust_summary
#
# remove customer id as it is mutually exclusie across data sets
# also remove next order to prevent leakage of information
# that is being predicted into the available data
#
          train_cust_summary_temp$cust_ids <- 0
          train_cust_summary_temp$next_order <- 0
          test_cust_summary_temp$cust_ids <- 0
          test_cust_summary_temp$next_order <- 0
          val_cust_summary_temp$cust_ids <- 0
          val_cust_summary_temp$next_order <- 0
#
# build cluster model using kmeans
#
          set.seed(2018)
          cluster_model <- 
            kmeans(train_cust_summary_temp,
                   centers = clusters_used,
                   algorithm = all_methods[cluster_method],
                   iter.max = 500,
                   nstart = 15,
                   trace = FALSE)
#
# for clusters with only 1 member, the sd_reorder is undefined
# set sd_reorder to 0 for use in later calculations
#
          cluster_model[["centers"]][is.na("sd_reorder"), 
                                     "sd_reorder"] <- 0
#
# and in some cases, rounding error can produce very small but negative
# sd values so we set those to 0 as well
#          
          cluster_model[["centers"]]["sd_reorder" < 0, 
                                     "sd_reorder"] <- 0
#
# summarize clusters in a bar plot
#
          barplot(cluster_model$size,
                  names.arg = c(1:clusters_used),
                  las = 1, 
                  cex.axis = 0.8, 
                  cex.names = 0.75,
                  main = paste0("kmeans cluster model using method ",
                                all_methods[cluster_method]),
                  cex.main = 0.75,
                  xlab = "cluster number",
                  ylab = "count in cluster",
                  col = "lightblue")
#
          train_cust_summary_temp <- 
            cbind(train_cust_summary_temp, 
                  cluster = cluster_model$cluster)
#
          recency_map <- 
            as.data.frame(matrix(rep(0, clusters_used * 2), 
                                 nrow = clusters_used, 
                                 ncol = 2))
          colnames(recency_map) = c("average recency", "count")
          reorder_map <- 
            as.data.frame(matrix(rep(0, clusters_used * 2), 
                                 nrow = clusters_used, 
                                 ncol = 2))
          colnames(reorder_map) = c("average reorder", "count")
          for (i in 1:clusters_used) {
#
# deal with any clusters with 0 members by setting to 
# a value such that no members will be matched in test or val
#
            if (cluster_model$size[i] == 0) {
              cluster_model$centers[i, ] <- 1e15
            }
#            
            recency_map[i, 1] <- 
              mean(train_cust_summary_temp[
                train_cust_summary_temp$cluster == i, ]$recency)
            recency_map[i, 1] <- 
              (recency_map[i, 1] * recency_scale) + recency_center
            recency_map[i, 2] <- 
              nrow(train_cust_summary_temp[
                train_cust_summary_temp$cluster == i, ])
            reorder_map[i, 1] <- 
              mean(train_cust_summary_temp[
                train_cust_summary_temp$cluster == i, ]$ave_reorder)
            reorder_map[i, 1] <- 
              (reorder_map[i, 1] * reorder_scale) + reorder_center
            reorder_map[i, 2] <- 
              nrow(train_cust_summary_temp[
                train_cust_summary_temp$cluster == i, ])
          }
          recency_map <- 
            recency_map[order(recency_map$`average recency`), ]
          reorder_map <- 
            reorder_map[order(reorder_map$`average reorder`), ]    
          if (plots_verbose == TRUE) {
            barplot(recency_map$count,
                    names.arg = 
                      round(recency_map$`average recency`, 0),
                    las = 3, 
                    cex.axis = 0.8, 
                    cex.names = 0.75,
                    main = paste0("kmeans cluster model using method ",
                                  all_methods[cluster_method]),
                    cex.main = 0.75,
                    xlab = paste0("Clusters by Average Recency\n(days)"),
                    ylab = "count",
                    col = "lightblue")
          }
#
          if(plots_verbose == TRUE) {
            barplot(reorder_map$count,
                    names.arg = 
                      round(reorder_map$`average reorder`, 0),
                    las = 3, 
                    cex.axis = 0.8, 
                    cex.names = 0.75,
                    main = paste0("kmeans cluster model using method ",
                                  all_methods[cluster_method]),
                    cex.main = 0.75,
                    xlab = paste0("Clusters by Average Reorder\n(days)"),
                    ylab = "count",
                    col = "lightblue")
          }
#
          cluster_reorder_map <- 
            matrix(0,
                   nrow = nrow(cluster_model$centers),
                   ncol = 2)
          cluster_model_reorder_col <- 
            which(colnames(cluster_model$centers) == "ave_reorder")
          for (i in 1:nrow(cluster_reorder_map)) {
            cluster_reorder_map[i, 1] <- i
            for (j in 1:length(will_reorder_centers)) {
              if ((cluster_model$centers[i, cluster_model_reorder_col] <= 
                   ((will_reorder_by_cutoffs[j + 1] - reorder_center) /
                    reorder_scale)) &
                  (cluster_model$centers[i, cluster_model_reorder_col] >=
                   ((will_reorder_by_cutoffs[j] - reorder_center) /
                    reorder_scale))) {
                cluster_reorder_map[i, 2] <- will_reorder_centers[j]
                break
              }
            }
            if (cluster_reorder_map[i, 2] == 0) { 
#
# if no mapping was found, then the cluster center is outside the range
# of values we are considering at present, so set the vale to beyond
# the max cutoff date to exclude from analysis of performance              
#
              cluster_reorder_map[i, 2] <- 
                2 * max(will_reorder_by_cutoffs)
            }
          }
#
# now build revenue forecast model using the trining data clusters
#
# calculate the overall distribution of reordering for each cluster
#
          train_cluster_membership <- 
            unique(train_cust_summary_temp$cluster)
          train_cluster_membership <-
            train_cluster_membership[order(train_cluster_membership)]
          samples_cluster <- 
            as.data.frame(matrix(data = numeric(),
                                 nrow = 1000,
                                 ncol = clusters_used))
          train_cluster_distribution <- 
            as.data.frame(matrix(data = numeric(),
                                 nrow = clusters_used,
                                 ncol = 7))
          colnames(train_cluster_distribution) <- 
            c("cluster", "reorder_mean", "reorder_sd", 
              "value_mean", "value_sd", "frequency", "freq_sd")
          probs <- numeric()
          for (cluster in 1:clusters_used) {
            train_cluster_distribution[cluster, ]$cluster <- cluster
            if (!(cluster %in% train_cluster_membership)) {
              train_cluster_distribution[cluster, ]$reorder_mean <- 0
              train_cluster_distribution[cluster, ]$reorder_sd <- 0
              train_cluster_distribution[cluster, ]$value_mean <- 0              
              train_cluster_distribution[cluster, ]$value_sd <- 0
              train_cluster_distribution[cluster, ]$frequency <- 0
              train_cluster_distribution[cluster, ]$freq_sd <- 0
            } else {
              temp_samples <- 
                which(train_cust_summary_temp$cluster == cluster)
              train_cluster_distribution[cluster, ]$frequency <-
                mean(train_cust_summary_raw[temp_samples, ]$frequencies)
              train_cluster_distribution[cluster, ]$freq_sd <-
                sd(train_cust_summary_raw[temp_samples, ]$frequencies)
#
# we weight the sampling by the frequency of orders for each member in the cluster
#
              probs <- 
                train_cust_summary_raw[temp_samples, ]$frequencies
#
# in some cases rounding error can result in a very small neagative
# probability which breaks later calcuations, so clean that up
# also for those members which had 1 or less orders, the sd of the reorder
# time is undefined so set that to 0 as well
#                          
              probs[probs < 0] <- 0
              probs[train_cust_summary_raw[temp_samples, ]$sd_reorder == 0] <- 0
#
# deal with two special cases:
# 1) there is only 1 member of the cluster, in which case sampling doens't work
# 2) the sum of the probabiiities is 0 due to frequency being 0
#              
              if (length(probs) > 1 & sum(probs) > 0) {
#
# normalize
#              
                probs <- probs / sum(probs)
#
# sample
#                
                samples_cluster[, cluster] <- sample(temp_samples, 
                                                     prob = probs,
                                                     size = 1000,
                                                     replace = TRUE)
#
# model reorder time using a gamma distribution
#
                temp_distribution <-
                  rgamma(n = 1000,
                         shape = 
                           ((train_cust_summary_raw$value_per_order[
                             samples_cluster[, cluster]]) /
                              train_cust_summary_raw$sd_reorder[
                                samples_cluster[, cluster]])^2,
                         scale = 
                           (train_cust_summary_raw$sd_reorder[
                             samples_cluster[, cluster]])^2 /
                           (train_cust_summary_raw$value_per_order[
                             samples_cluster[, cluster]]))
                train_cluster_distribution[cluster, ]$value_mean <-
                  mean(temp_distribution)
                train_cluster_distribution[cluster, ]$value_sd <-
                  sd(temp_distribution)
                temp_distribution <-
                  rgamma(n = 1000, 
                         shape = 
                           ((train_cust_summary_raw$ave_reorder[
                             samples_cluster[, cluster]]) /
                              train_cust_summary_raw$sd_reorder[
                                samples_cluster[, cluster]])^2,
                         scale = 
                           (train_cust_summary_raw$sd_reorder[
                             samples_cluster[, cluster]])^2 /
                           (train_cust_summary_raw$ave_reorder[
                             samples_cluster[, cluster]]))
                train_cluster_distribution[cluster, ]$reorder_mean <- 
                  mean(temp_distribution)
                train_cluster_distribution[cluster, ]$reorder_sd <-
                  sd(temp_distribution)
              } else {
                if (sum(probs) > 0) {
                  train_cluster_distribution[cluster, ]$reorder_mean <-
                    mean(train_cust_summary_temp[train_cust_summary_temp$cluster == 
                                                   cluster, ]$value_per_order)
                  train_cluster_distribution[cluster, ]$reorder_sd <- 0
                } else {
                  train_cluster_distribution[cluster, ]$reorder_mean <- 0
                  train_cluster_distribution[cluster, ]$reorder_sd <- 0
                }
              }
            }
          } # end of loop through clusters calculating reorder time distribution
          train_cluster_distribution[is.na(train_cluster_distribution)] <- 0
          metrics <- c("reorder_mean", "value_mean")
          metrics_cols <- 
            which(colnames(train_cluster_distribution) %in% metrics)
          metrics_bins <- 35
          metrics_sds <- c("reorder_sd", "value_sd")
          metrics_sds_cols <- 
            which(colnames(train_cluster_distribution) %in% metrics_sds)
#
# compute order value distribution to compare to raw data
#          
          metric <- 2
          if (plots_verbose == TRUE) {
            plot_hist <- TRUE
            par(mfrow = c(3, 3))
          } else {
            plot_hist <- FALSE
          }
          train_metric_sample <-
            matrix(nrow = 1000, 
                   ncol = nrow(train_cluster_distribution))
          train_metric_samples <- 
            matrix(nrow = metrics_bins,
                   ncol = 2 * nrow(train_cluster_distribution))
          for (i in seq(1, 2 * nrow(train_cluster_distribution) - 1, 2)) {
            if (train_cluster_distribution[(i + 1) / 2, 
                                           metrics_cols[metric]] > 0 &
                train_cluster_distribution[(i + 1) / 2, 
                                           metrics_sds_cols[metric]] > 0) {
              train_metric_sample[, (i + 1) / 2] <-
                rgamma(n = 1000, 
                       shape = 
                         (train_cluster_distribution[(i + 1) / 2, 
                                                     metrics_cols[metric]] /
                            train_cluster_distribution[(i + 1) / 2, 
                                                       metrics_sds_cols[metric]])^2,
                       scale = 
                         ((train_cluster_distribution[(i + 1) / 2, 
                                                      metrics_sds_cols[metric]])^2 /
                            train_cluster_distribution[(i + 1) / 2, 
                                                       metrics_cols[metric]]))
              bin_size <- max(
                floor((max(train_metric_sample[, (i + 1) / 2]) -
                         min(train_metric_sample[, (i + 1) / 2]))/ metrics_bins),
                1)
              if (plots_verbose == TRUE) {
                plot_hist <- TRUE
              } else {
                plot_hist <- FALSE
              }
              if (!is.na(max(train_metric_sample[, (i + 1) / 2]))) {
                temp_hist <- 
                  hist(train_metric_sample[, (i + 1) / 2],
                       breaks = 
                         seq(floor(min(train_metric_sample[, (i + 1) / 2])),
                             ceiling(max(train_metric_sample[, (i + 1) / 2]) +
                                       2 * bin_size - 
                                       (max(train_metric_sample[, (i + 1) / 2]) %% bin_size)),
                             bin_size),
                       plot = FALSE,
                       col = "lightblue",
                       xlab = "Order value ($)",
                       main = paste0("Order value distribution\n",
                                     "cluster ", (i + 1) / 2),
                       cex.main = 0.75)
                train_metric_samples[, i] <- 
                  temp_hist[["breaks"]][1:metrics_bins]
                train_metric_samples[, i + 1] <- 
                  temp_hist[["counts"]][1:metrics_bins]
              } else {
                train_metric_samples[, i] <- 0
                train_metric_samples[, i + 1] <- 0
              }
            } else {
              train_metric_samples[, i] <- 0
              train_metric_samples[, i + 1] <- 0
            }
          } # end of loop through clusters to calculate order value distribution
          par(mfrow = c(1, 1))
#
# for the order value data, we want to concatenate all the 
# values from all the histograms for each cluster to get
# the overall distribution
#            
          temp_hist_cutoff <- 50000
          temp_hist_bins <- 35
          bin_size <- temp_hist_cutoff / temp_hist_bins
          temp_data <- numeric(length = temp_hist_bins)
          for (i in 1:nrow(train_cluster_distribution)) {
            temp_temp_data <- numeric()
            temp_hist <- numeric(length = temp_hist_bins)
            for (j in 1:length(train_metric_samples[, (2 * i)])) {
              if (!is.na(train_metric_samples[j, (2 * i)])) {
#
# replicate the order value by the number of counts in the hist data
#                  
                temp_temp_data <- c(temp_temp_data, 
                                    rep(train_metric_samples[j, (2 * i) - 1],
                                        train_metric_samples[j, (2 * i)]))
              }
#
# use hist() to bin the data in order to keep the concatenation small
#                
              temp_hist <- 
                temp_hist +
                hist(temp_temp_data[temp_temp_data < temp_hist_cutoff],
                     breaks = seq(0, temp_hist_cutoff, bin_size),
                     plot = FALSE)[["counts"]]
            }
            temp_data <- temp_data + temp_hist
          }
          if (plots_verbose == TRUE) {
            barplot(temp_data,
                    names.arg = ceiling(seq(bin_size / 2, temp_hist_cutoff, bin_size)),
                    las = 2,
                    col = "lightblue",
                    main = paste0("Training Data\n",
                                  "Avg. Value of Orders estimated from cluster model\n",
                                  "(Avg. cust. order < $",
                                  temp_hist_cutoff, ")\n",
                                  "**reference only**"),
                    cex.main = 0.75,
                    ylab = "count",
                    xlab = "Avg. order value ($)")
          }
#
# now comapre the statistically modeled distribution to the actual data
#
# modeled distribution
#            
          plot(cbind(ceiling(seq(bin_size / 2, 
                                 temp_hist_cutoff, bin_size)), 
                     temp_data / max(temp_data)),
               type = "l",
               lty = 1,
               col = "lightblue",
               lwd = 2,
               xlab = "Average Order Value ($)",
               ylab = "Relative probability",
               main = paste0("Comparison of estimated order value distribution\n",
                             "to actual training data"),
               cex.main = 0.75)
#
# actual training data saved from earlier histogram
#
          lines(train_value_hist_act, 
                lty = 2,
                col = "darkgreen",
                lwd = 1)
#
# compute reorder time distribution to compare to raw data
#          
          metrics_bins <- max(will_reorder_by_cutoffs)
          metric <- 1
          if (plots_verbose == TRUE) {
            plot_hist <- TRUE
            par(mfrow = c(3, 3))
          } else {
            plot_hist <- FALSE
          }
          train_metric_sample <-
            matrix(nrow = 1000, 
                   ncol = nrow(train_cluster_distribution))
          train_metric_samples <- 
            matrix(nrow = metrics_bins,
                   ncol = 2 * nrow(train_cluster_distribution))
          for (i in seq(1, 2 * nrow(train_cluster_distribution) - 1, 2)) {
            if (train_cluster_distribution[(i + 1) / 2, 
                                           metrics_cols[metric]] > 0 &
                train_cluster_distribution[(i + 1) / 2, 
                                           metrics_sds_cols[metric]] > 0) {
              train_metric_sample[, (i + 1) / 2] <-
                rgamma(n = 1000, 
                       shape = 
                         (train_cluster_distribution[(i + 1) / 2, 
                                                     metrics_cols[metric]] /
                            train_cluster_distribution[(i + 1) / 2, 
                                                       metrics_sds_cols[metric]])^2,
                       scale = 
                         ((train_cluster_distribution[(i + 1) / 2, 
                                                      metrics_sds_cols[metric]])^2 /
                            train_cluster_distribution[(i + 1) / 2, 
                                                       metrics_cols[metric]]))
              bin_size <- 1
              if (!is.na(max(train_metric_sample[, (i + 1) / 2]))) {
                temp_hist <- 
                  hist(train_metric_sample[, (i + 1) / 2],
                       breaks = 
                         seq(floor(min(train_metric_sample[, (i + 1) / 2])),
                             ceiling(max(train_metric_sample[, (i + 1) / 2]) +
                                       2 * bin_size - 
                                       (max(train_metric_sample[, (i + 1) / 2]) %% bin_size)),
                             bin_size),
                       plot = plot_hist,
                       col = "lightblue",
                       xlab = "days to next order",
                       main = paste0("Time to reorder\n",
                                     "cluster ", (i + 1) / 2),
                       cex.main = 0.75)
                train_metric_samples[, i] <- 
                  temp_hist[["breaks"]][1:metrics_bins]
                train_metric_samples[, i + 1] <- 
                  temp_hist[["counts"]][1:metrics_bins]
              } else {
                train_metric_samples[, i] <- 0
                train_metric_samples[, i + 1] <- 0
              }
            } else {
              train_metric_samples[, i] <- 0
              train_metric_samples[, i + 1] <- 0
            }
          } # end of loop over clusters to calcuale reorder time distribution
          par(mfrow = c(1, 1))
#
# for the reorder time distribution, the data are in 
# one-day bins for each cluster, so we want to sum up the
# counts for each day across all clusters
#            
          temp_data <- 
            rowSums(train_metric_samples[, 
                                         seq(2, 
                                             ncol(train_metric_samples), 2)],
                    na.rm = TRUE)
          temp_hist_cutoff <- max(will_reorder_by_cutoffs)
          if (plots_verbose == TRUE) {
            barplot(temp_data,
                    names.arg = seq(1, temp_hist_cutoff, 1),
                    las = 2,
                    col = "lightblue",
                    main = paste0("Training Data\n",
                                  "Time to reorder estimated from cluster model\n",
                                  "**reference only**"),
                    cex.main = 0.75,
                    cex.axis = 0.75,
                    ylab = "count",
                    xlab = "Time to reorder (days)")
          }
#
# now comapre the statistically modeled distribution to the actual data
#
# modeled distribution
#            
          plot(cbind(ceiling(seq(bin_size / 2, 
                                 temp_hist_cutoff, bin_size)), 
                     temp_data / max(temp_data)),
               type = "l",
               lty = 1,
               col = "lightblue",
               lwd = 2,
               xlab = "days",
               ylab = "Relative probability",
               main = paste0("Comparison of time to reorder distribution\n",
                             "to actual training data"),
               cex.main = 0.75)
#
# actual training data saved from earlier histogram
#
          lines(train_reorder_hist_act, 
                lty = 2,
                col = "darkgreen",
                lwd = 1)
#
# compute distances to clusters for test data
#
          test_dist <- matrix(0,
                              nrow = nrow(test_cust_summary_temp),
                              ncol = clusters_used)
#  
          for (i in 1:nrow(test_cust_summary_temp)) {
            test_dist[i, ] <- 
              euc_dist_v(as.matrix(test_cust_summary_temp[i, ]),
                         as.matrix(cluster_model$centers))
          }
# 
          test_clusters <- numeric(nrow(test_dist))
          for (i in 1:nrow(test_dist)) {
            test_clusters[i] <- which(test_dist[i, ] == 
                                        min(test_dist[i, ]))
          }
          test_cust_summary_temp <- cbind(test_cust_summary_temp,
                                          cluster = test_clusters)
#
# predict the future revenue for the test data using the 
# train_cluster_distribution model with the probabilities
# by cluster based on the test data population, then compare
# to the actual future revenue for the test instances            
#
          test_cluster_membership <- 
            unique(test_cust_summary_temp$cluster)
          test_cluster_membership <- 
            test_cluster_membership[order(test_cluster_membership)]
#
# for a given cluster, the number of orders predicted is
# # in cluster * frequency * 
#          (duration of prediction window) / (date range of test data)         
# where frequency is the count of orders in the test data
# the process to predict revenue for the cluster is then
# for each cluster member
#   sample frequency
#     up to (frequecy) times, while T < forecast period
#       sample reorder time; T = T + reorder time
#       sample value; V = V + order value
# 
          temp_forecast <- data.frame(time = numeric(), value = numeric())
          sample_size <- 1000
          sample_count <- 0
          for (cluster in test_cluster_membership) {
            skip <- FALSE
            if (train_cluster_distribution$frequency[cluster] == 0) {
              skip <- TRUE
            } else if (train_cluster_distribution$freq_sd == 0) {
              temp_freq_dist <- train_cluster_distribution$frequency[cluster]
            } else {
              temp_freq_dist <- 
                rgamma(n = sample_size, 
                       shape = (train_cluster_distribution$frequency[cluster] /
                                  train_cluster_distribution$freq_sd[cluster])^2,
                       scale = train_cluster_distribution$freq_sd[cluster]^2 /
                         train_cluster_distribution$frequency[cluster])
            }
            if (train_cluster_distribution$reorder_mean[cluster] == 0) {
              skip <- TRUE
            } else if (train_cluster_distribution$reorder_sd[cluster] == 0) {
              temp_reorder_dist <- train_cluster_distribution$reorder_mean[cluster]
            } else {
              temp_reorder_dist <- 
                rgamma(n = sample_size, 
                       shape = (train_cluster_distribution$reorder_mean[cluster] /
                                  train_cluster_distribution$reorder_sd[cluster])^2,
                       scale = train_cluster_distribution$reorder_sd[cluster]^2 /
                         train_cluster_distribution$reorder_mean[cluster])
            }
            if (train_cluster_distribution$value_mean[cluster] == 0) {
              skip <- TRUE
            } else if (train_cluster_distribution$value_sd[cluster] == 0) {
              temp_value_dist <- train_cluster_distribution$value_mean[cluster]
            } else {
            temp_value_dist <- 
              rgamma(n = sample_size, 
                     shape = (train_cluster_distribution$value_mean[cluster] /
                                train_cluster_distribution$value_sd[cluster])^2,
                     scale = train_cluster_distribution$value_sd[cluster]^2 /
                       train_cluster_distribution$value_mean[cluster])
            }
            if (skip == FALSE) {
              for (member in 1:length(which(test_cust_summary_temp$cluster == 
                                            test_cluster_membership[cluster]))) {
                temp_freq <- sample(x = temp_freq_dist, size = 1)
                Time <- 0
                Steps <- 0
#
# adjust frequency by the ratio of forecast time to train data time
#              
                temp_freq <- temp_freq * forecast_period / (max_date - min_date)
                while ((Time <= forecast_period) & (Steps <= temp_freq)) {
                  Time <- Time + sample(x = temp_reorder_dist, size = 1)
                  sample_count <- sample_count + 1
                  temp_forecast[sample_count, ]$time <- Time
                  temp_forecast[sample_count, ]$value <- 
                    sample(x = temp_value_dist, size = 1)
                  Steps <- Steps + 1
                }
              }
            }
          }
          current_index <- 
            which(all_permutations ==
                    paste0(all_methods[cluster_method],
                           "_",
                           clusters_used))
          for (i in 1:forecast_period) {
            test_forecast[i, (current_index - 1) * 6 + 2] <- 
              sum(temp_forecast[temp_forecast[, ]$time <= i &
                                  temp_forecast[, ]$time > (i - 1), 2])
            test_forecast[i, (current_index - 1) * 6 + 4] <-
              sum(raw_data$value[(raw_data$order_date - 
                                    max_act_test_date) <= i &
                                   (raw_data$order_date - 
                                      max_act_test_date) > (i - 1) &
                                   raw_data$cust_no %in% test_data_subset$cust_no])
            if (i > 1) {
              test_forecast[i, (current_index - 1) * 6 + 3] <- 
                test_forecast[i, (current_index - 1) * 6 + 2] + 
                test_forecast[i - 1, (current_index - 1) * 6 + 3]
              test_forecast[i, (current_index - 1) * 6 + 5] <- 
                test_forecast[i, (current_index - 1) * 6 + 4] +
                test_forecast[i - 1, (current_index - 1) * 6 + 5]
            } else {
              test_forecast[i, (current_index - 1) * 6 + 3] <- 
                test_forecast[i, (current_index - 1) * 6 + 2]
              test_forecast[i, (current_index - 1) * 6 + 5] <- 
                test_forecast[i, (current_index - 1) * 6 + 4]
            }
          }
#
# calculate errors for the forecast
#
          for (i in 1:forecast_period) {
            test_forecast[i, (current_index - 1) * 6 + 6] <-
              ((test_forecast[i, (current_index - 1) * 6 + 2] -
                  test_forecast[i, (current_index - 1) * 6 + 4]) /
                 test_forecast[i, (current_index - 1) * 6 + 4])
            test_forecast[i, (current_index - 1) * 6 + 7] <- 
              ((test_forecast[i, (current_index - 1) * 6 + 3] -
                  test_forecast[i, (current_index - 1) * 6 + 5]) /
                 test_forecast[i, (current_index - 1) * 6 + 5])
          }
#          
          smooth_window <- 7
          smooth_cols <- c(2, 4)
          top_pad <- data.frame(numeric(length = floor(smooth_window / 2)),
                                numeric(length = floor(smooth_window / 2)))
          bottom_pad <- data.frame(numeric(length = floor(smooth_window / 2)),
                                   numeric(length = floor(smooth_window / 2)))
          for (n in 1:floor(smooth_window / 2)) {
            for (m in 1:length(smooth_cols)) {
              top_pad[n, m] <- 
                mean(test_forecast[1:(n + 1), smooth_cols[m]])
              bottom_pad[n, m] <- 
                mean(test_forecast[(forecast_period - (n + 1)):
                                     forecast_period, smooth_cols[m]])
            }
          }
          plot_forecast <- test_forecast
          for (k in 1:length(smooth_cols)) {
            plot_forecast[, smooth_cols[k]] <- 
              rollmean(plot_forecast[, smooth_cols[k]], 
                                     smooth_window, 
                       fill = NA)
            plot_forecast[1:nrow(top_pad), smooth_cols[k]] <- top_pad[, k]
            plot_forecast[(forecast_period - nrow(bottom_pad) + 1):forecast_period,
                           smooth_cols[k]] <- bottom_pad[, k]
          }
          temp_y_scale <- 
            c(0, 1.2 * max(plot_forecast[, (current_index - 1) * 6 + 2],
                           plot_forecast[, (current_index - 1) * 6 + 4]))
          scale_factor <- 1e6
          plot(x = plot_forecast[, 1], 
               y = plot_forecast[, (current_index - 1) * 6 + 4] / scale_factor, 
               type = "l", col = "blue",
               xlab = "days",
               ylab = "Forecast/Actual ($M)",
               main = paste0("test data\n",
                             "kmeans cluster model using method ",
                             all_methods[cluster_method]),
               ylim = temp_y_scale / scale_factor)
          lines(x = plot_forecast[, 1], 
                y = plot_forecast[, (current_index - 1) * 6 + 2] / scale_factor,
                col = "red",
                ylim = temp_y_scale / scale_factor)
          temp_y_scale <- 
            c(0, 1.2 * max(plot_forecast[, (current_index - 1) * 6 + 3],
                           plot_forecast[, (current_index - 1) * 6 + 5]))
          plot(x = plot_forecast[, 1], 
               y = plot_forecast[, (current_index - 1) * 6 + 3] / scale_factor, 
               type = "l", col = "red",
               xlab = "days",
               ylab = "Cumulative Forecast/Actual ($M)",
               main = paste0("test data\n",
                             "kmeans cluster model using method ",
                             all_methods[cluster_method]),
               ylim = temp_y_scale / scale_factor)
          lines(x = plot_forecast[, 1], 
                y = plot_forecast[, (current_index - 1) * 6 + 5] / scale_factor,
                col = "blue",
                ylim = temp_y_scale / scale_factor)
#              
          test_pred_reorder <- numeric(nrow(test_cust_summary_temp))
          for (i in 1:length(test_pred_reorder)) {
            test_pred_reorder[i] <-
              cluster_reorder_map[
                cluster_reorder_map[, 1] == 
                  test_cust_summary_temp$cluster[i], 2]
          }
#
          test_cust_summary_temp <- 
            cbind(test_cust_summary_temp, 
                  pred_reorder = test_pred_reorder)
#
          test_cluster_reorder <- 
            numeric(nrow(test_cust_summary_temp))
#
          for (i in 1:length(test_cluster_reorder)) {
#
# note use of the original data.frame to test reorder as
# the temp data.frame used to test cluster membership
# excludes the next order dates
#
            if (!is.na(test_cust_summary$next_order[i])) {
              for (j in 1:length(will_reorder_centers)) {
                if (test_cust_summary_raw$next_order[i] - 
                    test_cust_summary_raw$cust_end[i] <= 
                    will_reorder_by_cutoffs[j + 1] &
                    test_cust_summary_raw$next_order[i] -
                    test_cust_summary_raw$cust_end[i] >
                    will_reorder_by_cutoffs[j]) {
                  test_cluster_reorder[i] <- will_reorder_centers[j]
                  break
                }
              }
            }
            if (test_cluster_reorder[i] == 0) {
#
# if no mapping was found, then the cluster center is outside the range
# of values we are comparing; therefore set it to the mean of the largest
# test value and the actual value
#
              test_cluster_reorder[i] <- 
                mean(will_reorder_by_cutoffs[
                  length(will_reorder_by_cutoffs)],
                  test_cust_summary_temp$ave_reorder[i])
            }
          }
#
          test_cust_summary_temp <- 
            cbind(test_cust_summary_temp,
                  act_reorder = test_cluster_reorder)
#
# test if assigned cluster matches recency bin
#
          test_recency_accuracy <- 
            as.data.frame(matrix(0,
                                 nrow = nrow(test_cust_summary_temp),
                                 ncol = 5))
          colnames(test_recency_accuracy) <- 
            c("act_reorder", "pred_reorder", 
              "match", "pred > act", "act > pred")
          for (i in 1:nrow(test_cust_summary_temp)) {
            test_recency_accuracy[i, 1] <- 
              test_cust_summary_temp$act_reorder[i]
            test_recency_accuracy[i, 2] <- 
              test_cust_summary_temp$pred_reorder[i]
            if (test_cust_summary_temp$pred_reorder[i] == 
                test_cust_summary_temp$act_reorder[i]) {
              test_recency_accuracy[i, 3] <- 1
            } else if (test_cust_summary_temp$pred_reorder[i] > 
                       test_cust_summary_temp$act_reorder[i]) {
              test_recency_accuracy[i, 4] <- 1
            } else {
              test_recency_accuracy[i, 5] <- 1
            }
          }
#
          test_recency_cumulative <- 
            as.data.frame(matrix(0,
                                 nrow = 
                                   nrow(test_cust_summary_temp),
                                 ncol = 
                                   2 * length(will_reorder_centers)))
          cum_recency_colnames <- character()
          index <- 0
          for (i in seq(1, ncol(test_recency_cumulative), 2)) {
            index <- index + 1
            cum_recency_colnames[i] <-
              paste0("pred_reorder_by", will_reorder_by_cutoffs[index])
            cum_recency_colnames[i + 1] <-
              paste0("act_reorder_by", will_reorder_by_cutoffs[index])
          }
#
          colnames(test_recency_cumulative) <- 
            cum_recency_colnames
#
          for (i in 1:nrow(test_cust_summary_temp)) {
            for (j in 1:length(will_reorder_centers)) {
              if (test_cust_summary_temp$pred_reorder[i] <= 
                  will_reorder_by_cutoffs[j + 1]) {
                test_recency_cumulative[i, (j * 2 - 1)] <- 1
              }
            }
            for (j in 1:length(will_reorder_centers)) {
              if (test_cust_summary_temp$act_reorder[i] <= 
                  will_reorder_by_cutoffs[j + 1]) {
                test_recency_cumulative[i, (j * 2)] <- 1
              }
            }
          }
#      
          test_cumulative_accuracy <- 
            as.data.frame(matrix(0,
                                 nrow = 
                                   length(will_reorder_centers),
                                 ncol = 10))
          colnames(test_cumulative_accuracy) <- 
            c("data", "Algorithm", "ID", "split", 
              "clusters", "date", "Precision", 
              "Recall", "Accuracy", "F1")
#
          for (j in seq(1, length(will_reorder_centers), 1)) {
            true_pos <- 
              nrow(test_recency_cumulative[
                (test_recency_cumulative[, (j * 2 - 1)] ==
                   test_recency_cumulative[, (j * 2)]) &
                  (test_recency_cumulative[, (j * 2 - 1)] == 
                     1), ])
            false_pos <- 
              nrow(test_recency_cumulative[
                (test_recency_cumulative[, (j * 2 - 1)] !=
                   test_recency_cumulative[, (j * 2)]) &
                  (test_recency_cumulative[, (j * 2 - 1)] == 
                     1), ])
            true_neg <-
              nrow(test_recency_cumulative[
                (test_recency_cumulative[, (j * 2 - 1)] ==
                   test_recency_cumulative[, (j * 2)]) &
                  (test_recency_cumulative[, (j * 2 - 1)] == 
                     0), ])
            false_neg <-
              nrow(test_recency_cumulative[
                (test_recency_cumulative[, (j * 2 - 1)] !=
                   test_recency_cumulative[, (j * 2)]) &
                  (test_recency_cumulative[, (j * 2 - 1)] == 
                     0), ])
            if (((true_pos + false_pos) > 0) & !is.na(true_pos)) {
              Precision <- 
                true_pos / (true_pos + false_pos)
            } else {
              Precision <- 0
            }
            if (((true_pos + false_neg) > 0) & !is.na(true_pos)) {
              Recall <- 
                true_pos / (true_pos + false_neg)
            } else {
              Recall <- 0
            }
            if ((true_pos + false_pos + true_neg + false_neg) > 0) {
              Accuracy <- 
                (true_pos + true_neg) / 
                (true_pos + false_pos + true_neg + false_neg)
            } else {
              Accuracy <- 0
            }
            if ((Precision + Recall) > 0) {
              F1 <- (2 * Precision * Recall) / 
                (Precision + Recall)
            } else {
              F1 <- 0
            } 
#
            test_cumulative_accuracy[j, 1] <- "test"
            test_cumulative_accuracy[j, 2] <- 
              all_permutations[current_index]
            test_cumulative_accuracy[j, 3] <- 
              current_index
            test_cumulative_accuracy[j, 4] <- splits[split_values, 2]
            test_cumulative_accuracy[j, 5] <- clusters_used
            test_cumulative_accuracy[j, 6] <- will_reorder_centers[j]
            test_cumulative_accuracy[j, 7] <- Precision
            test_cumulative_accuracy[j, 8] <- Recall
            test_cumulative_accuracy[j, 9] <- Accuracy
            test_cumulative_accuracy[j, 10] <- F1
          } # end loop through date list to calculate perforamnce metrics
#
          cat("Summary for Test Data\n")
          print(test_cumulative_accuracy)
          test_cumulative_table <- rbind(test_cumulative_table,
                                         test_cumulative_accuracy)
#
# compute distances to train data clusters for validation data
#
          val_dist <- matrix(0,
                             nrow = nrow(val_cust_summary_temp),
                             ncol = clusters_used)
#  
          for (i in 1:nrow(val_cust_summary_temp)) {
            val_dist[i, ] <- 
              euc_dist_v(as.matrix(val_cust_summary_temp[i, ]),
                         as.matrix(cluster_model$centers))
            
            
          }
# 
          val_clusters <- numeric(nrow(val_dist))
          for (i in 1:nrow(val_dist)) {
            val_clusters[i] <- which(val_dist[i, ] == 
                                       min(val_dist[i, ]))
          }
          val_cust_summary_temp <- cbind(val_cust_summary_temp,
                                         cluster = val_clusters)
#
# predict the future revenue for the val data using the 
# train_cluster_distribution model with the probabilities
# by cluster based on the val data population, then compare
# to the actual future revenue for the val instances            
#
          val_cluster_membership <- 
            unique(val_cust_summary_temp$cluster)
          val_cluster_membership <- 
            val_cluster_membership[order(val_cluster_membership)]
#
# for a given cluster, the number of orders predicted is
# # in cluster * frequency * 
#          (duration of prediction window) / (date range of val data)         
# where frequency is the count of orders in the val data
# the process to predict revenue for the cluster is then
# for each cluster member
#   sample frequency
#     up to (frequecy) times, while T < forecast period
#       sample reorder time; T = T + reorder time
#       sample value; V = V + order value
# 
          temp_forecast <- data.frame(time = numeric(), value = numeric())
          sample_size <- 1000
          sample_count <- 0
          for (cluster in val_cluster_membership) {
            skip <- FALSE
            if (train_cluster_distribution$frequency[cluster] == 0) {
              skip <- TRUE
            } else if (train_cluster_distribution$freq_sd == 0) {
              temp_freq_dist <- train_cluster_distribution$frequency[cluster]
            } else {
              temp_freq_dist <- 
                rgamma(n = sample_size, 
                       shape = (train_cluster_distribution$frequency[cluster] /
                                  train_cluster_distribution$freq_sd[cluster])^2,
                       scale = train_cluster_distribution$freq_sd[cluster]^2 /
                         train_cluster_distribution$frequency[cluster])
            }
            if (train_cluster_distribution$reorder_mean[cluster] == 0) {
              skip <- TRUE
            } else if (train_cluster_distribution$reorder_sd[cluster] == 0) {
              temp_reorder_dist <- train_cluster_distribution$reorder_mean[cluster]
            } else {
              temp_reorder_dist <- 
                rgamma(n = sample_size, 
                       shape = (train_cluster_distribution$reorder_mean[cluster] /
                                  train_cluster_distribution$reorder_sd[cluster])^2,
                       scale = train_cluster_distribution$reorder_sd[cluster]^2 /
                         train_cluster_distribution$reorder_mean[cluster])
            }
            if (train_cluster_distribution$value_mean[cluster] == 0) {
              skip <- TRUE
            } else if (train_cluster_distribution$value_sd[cluster] == 0) {
              temp_value_dist <- train_cluster_distribution$value_mean[cluster]
            } else {
              temp_value_dist <- 
                rgamma(n = sample_size, 
                       shape = (train_cluster_distribution$value_mean[cluster] /
                                  train_cluster_distribution$value_sd[cluster])^2,
                       scale = train_cluster_distribution$value_sd[cluster]^2 /
                         train_cluster_distribution$value_mean[cluster])
            }
            if (skip == FALSE) {
              for (member in 1:length(which(val_cust_summary_temp$cluster == 
                                            val_cluster_membership[cluster]))) {
                temp_freq <- sample(x = temp_freq_dist, size = 1)
                Time <- 0
                Steps <- 0
#
# adjust frequency by the ratio of forecast time to train data time
#              
                temp_freq <- temp_freq * forecast_period / (max_date - min_date)
                while ((Time <= forecast_period) & (Steps <= temp_freq)) {
                  Time <- Time + sample(x = temp_reorder_dist, size = 1)
                  sample_count <- sample_count + 1
                  temp_forecast[sample_count, ]$time <- Time
                  temp_forecast[sample_count, ]$value <- 
                    sample(x = temp_value_dist, size = 1)
                  Steps <- Steps + 1
                }
              }
            }
          }
          current_index <- 
            which(all_permutations ==
                    paste0(all_methods[cluster_method],
                           "_",
                           clusters_used))
          for (i in 1:forecast_period) {
            val_forecast[i, (current_index - 1) * 6 + 2] <- 
              sum(temp_forecast[temp_forecast[, ]$time <= i &
                                  temp_forecast[, ]$time > (i - 1), 2])
            val_forecast[i, (current_index - 1) * 6 + 4] <-
              sum(raw_data$value[(raw_data$order_date - 
                                    max_act_val_date) <= i &
                                   (raw_data$order_date - 
                                      max_act_val_date) > (i - 1) &
                                   raw_data$cust_no %in% val_data_subset$cust_no])
            if (i > 1) {
              val_forecast[i, (current_index - 1) * 6 + 3] <- 
                val_forecast[i, (current_index - 1) * 6 + 2] + 
                val_forecast[i - 1, (current_index - 1) * 6 + 3]
              val_forecast[i, (current_index - 1) * 6 + 5] <- 
                val_forecast[i, (current_index - 1) * 6 + 4] +
                val_forecast[i - 1, (current_index - 1) * 6 + 5]
            } else {
              val_forecast[i, (current_index - 1) * 6 + 3] <- 
                val_forecast[i, (current_index - 1) * 6 + 2]
              val_forecast[i, (current_index - 1) * 6 + 5] <- 
                val_forecast[i, (current_index - 1) * 6 + 4]
            }
          }
#
# calculate errors for the forecast
#
          for (i in 1:forecast_period) {
            val_forecast[i, (current_index - 1) * 6 + 6] <-
              ((val_forecast[i, (current_index - 1) * 6 + 2] -
                  val_forecast[i, (current_index - 1) * 6 + 4]) /
                 val_forecast[i, (current_index - 1) * 6 + 4])
            val_forecast[i, (current_index - 1) * 6 + 7] <- 
              ((val_forecast[i, (current_index - 1) * 6 + 3] -
                  val_forecast[i, (current_index - 1) * 6 + 5]) /
                 val_forecast[i, (current_index - 1) * 6 + 5])
          }
#          
          smooth_window <- 7
          smooth_cols <- c(2, 4)
          top_pad <- data.frame(numeric(length = floor(smooth_window / 2)),
                                numeric(length = floor(smooth_window / 2)))
          bottom_pad <- data.frame(numeric(length = floor(smooth_window / 2)),
                                   numeric(length = floor(smooth_window / 2)))
          for (n in 1:floor(smooth_window / 2)) {
            for (m in 1:length(smooth_cols)) {
              top_pad[n, m] <- 
                mean(val_forecast[1:(n + 1), smooth_cols[m]])
              bottom_pad[n, m] <- 
                mean(val_forecast[(forecast_period - (n + 1)):
                                    forecast_period, smooth_cols[m]])
            }
          }
          plot_forecast <- val_forecast
          for (k in 1:length(smooth_cols)) {
            plot_forecast[, smooth_cols[k]] <- 
              rollmean(plot_forecast[, smooth_cols[k]], 
                       smooth_window, 
                       fill = NA)
            plot_forecast[1:nrow(top_pad), smooth_cols[k]] <- top_pad[, k]
            plot_forecast[(forecast_period - nrow(bottom_pad) + 1):forecast_period,
                          smooth_cols[k]] <- bottom_pad[, k]
          }
          temp_y_scale <- 
            c(0, 1.2 * max(plot_forecast[, (current_index - 1) * 6 + 2],
                           plot_forecast[, (current_index - 1) * 6 + 4]))
          scale_factor <- 1e6
          plot(x = plot_forecast[, 1], 
               y = plot_forecast[, (current_index - 1) * 6 + 4] / scale_factor, 
               type = "l", col = "blue",
               xlab = "days",
               ylab = "Forecast/Actual ($M)",
               main = paste0("validation data\n",
                             "kmeans cluster model using method ",
                             all_methods[cluster_method]),
               ylim = temp_y_scale / scale_factor)
          lines(x = plot_forecast[, 1], 
                y = plot_forecast[, (current_index - 1) * 6 + 2] / scale_factor,
                col = "red",
                ylim = temp_y_scale / scale_factor)
          temp_y_scale <- 
            c(0, 1.2 * max(plot_forecast[, (current_index - 1) * 6 + 3],
                           plot_forecast[, (current_index - 1) * 6 + 5]))
          plot(x = plot_forecast[, 1], 
               y = plot_forecast[, (current_index - 1) * 6 + 3] / scale_factor, 
               type = "l", col = "red",
               xlab = "days",
               ylab = "Cumulative Forecast/Actual ($M)",
               main = paste0("validation data\n", 
                             "kmeans cluster model using method ",
                             all_methods[cluster_method]),
               ylim = temp_y_scale / scale_factor)
          lines(x = plot_forecast[, 1], 
                y = plot_forecast[, (current_index - 1) * 6 + 5] / scale_factor,
                col = "blue",
                ylim = temp_y_scale / scale_factor)
#          
          val_pred_reorder <- numeric(nrow(val_cust_summary_temp))
          for (i in 1:length(val_pred_reorder)) {
            val_pred_reorder[i] <-
              cluster_reorder_map[
                cluster_reorder_map[, 1] == 
                  val_cust_summary_temp$cluster[i], 2]
          }
#
          val_cust_summary_temp <- 
            cbind(val_cust_summary_temp, 
                  pred_reorder = val_pred_reorder)
#
          val_cluster_reorder <- 
            numeric(nrow(val_cust_summary_temp))
#
          for (i in 1:length(val_cluster_reorder)) {
            for (j in 1:length(will_reorder_centers)) {
#
# note use of the original data.frame to test reorder as
# the temp data.frame used to test cluster membership
# excludes the next order dates
#
              if (!is.na(val_cust_summary$next_order[i])) {
                if (val_cust_summary_raw$next_order[i] - 
                    val_cust_summary_raw$cust_end[i] <= 
                    will_reorder_by_cutoffs[j + 1] &
                    val_cust_summary_raw$next_order[i] -
                    val_cust_summary_raw$cust_end[i] >
                    will_reorder_by_cutoffs[j]) {
                  val_cluster_reorder[i] <- will_reorder_centers[j]
                  break
                }
              }
            }
            if (val_cluster_reorder[i] == 0) {
#
# if no mapping was found, then the cluster center is outside the range
# of values we are comparing; therefore set it to the mean of the largest
# test value and the actual value
#
              val_cluster_reorder[i] <- 
                mean(will_reorder_by_cutoffs[
                  length(will_reorder_by_cutoffs)],
                  val_cust_summary_temp$ave_reorder[i])
            }
          }
#
          val_cust_summary_temp <- 
            cbind(val_cust_summary_temp,
                  act_reorder = val_cluster_reorder)
#
# test if assigned cluster matches recency bin
#
          val_recency_accuracy <- 
            as.data.frame(matrix(0,
                                 nrow = nrow(val_cust_summary_temp),
                                 ncol = 5))
          colnames(val_recency_accuracy) <- 
            c("act_reorder", "pred_reorder", 
              "match", "pred > act", "act > pred")
          for (i in 1:nrow(val_cust_summary_temp)) {
            val_recency_accuracy[i, 1] <- 
              val_cust_summary_temp$act_reorder[i]
            val_recency_accuracy[i, 2] <- 
              val_cust_summary_temp$pred_reorder[i]
            if (val_cust_summary_temp$pred_reorder[i] == 
                val_cust_summary_temp$act_reorder[i]) {
              val_recency_accuracy[i, 3] <- 1
            } else if (val_cust_summary_temp$pred_reorder[i] > 
                       val_cust_summary_temp$act_reorder[i]) {
              val_recency_accuracy[i, 4] <- 1
            } else {
              val_recency_accuracy[i, 5] <- 1
            }
          }
#
          val_recency_cumulative <- 
            as.data.frame(matrix(0,
                                 nrow = 
                                   nrow(val_cust_summary_temp),
                                 ncol = 
                                   2 * length(will_reorder_centers)))
          cum_recency_colnames <- character()
          index <- 0
          for (i in seq(1, ncol(val_recency_cumulative), 2)) {
            index <- index + 1
            cum_recency_colnames[i] <-
              paste0("pred_reorder_by", will_reorder_by_cutoffs[index])
            cum_recency_colnames[i + 1] <-
              paste0("act_reorder_by", will_reorder_by_cutoffs[index])
          }
#
          colnames(val_recency_cumulative) <- cum_recency_colnames
#
          for (i in 1:nrow(val_cust_summary_temp)) {
            for (j in 1:length(will_reorder_centers)) {
              if (val_cust_summary_temp$pred_reorder[i] <= 
                  will_reorder_by_cutoffs[j + 1]) {
                val_recency_cumulative[i, (j * 2 - 1)] <- 1
              }
            }
            for (j in 1:length(will_reorder_centers)) {
              if (val_cust_summary_temp$act_reorder[i] <= 
                  will_reorder_by_cutoffs[j + 1]) {
                val_recency_cumulative[i, (j * 2)] <- 1
              }
            }
          }
#      
          val_cumulative_accuracy <- 
            as.data.frame(matrix(0,
                                 nrow = 
                                   length(will_reorder_centers),
                                 ncol = 10))
          colnames(val_cumulative_accuracy) <- 
            c("data", "Algorithm", "ID", "split", 
              "clusters", "date", "Precision", 
              "Recall", "Accuracy", "F1")
#
          for (j in seq(1, length(will_reorder_centers), 1)) {
            true_pos <- 
              nrow(val_recency_cumulative[
                (val_recency_cumulative[, (j * 2 - 1)] ==
                   val_recency_cumulative[, (j * 2)]) &
                  (val_recency_cumulative[, (j * 2 - 1)] == 
                     1), ])
            false_pos <- 
              nrow(val_recency_cumulative[
                (val_recency_cumulative[, (j * 2 - 1)] !=
                   val_recency_cumulative[, (j * 2)]) &
                  (val_recency_cumulative[, (j * 2 - 1)] == 
                     1), ])
            true_neg <-
              nrow(val_recency_cumulative[
                (val_recency_cumulative[, (j * 2 - 1)] ==
                   val_recency_cumulative[, (j * 2)]) &
                  (val_recency_cumulative[, (j * 2 - 1)] == 
                     0), ])
            false_neg <-
              nrow(val_recency_cumulative[
                (val_recency_cumulative[, (j * 2 - 1)] !=
                   val_recency_cumulative[, (j * 2)]) &
                  (val_recency_cumulative[, (j * 2 - 1)] == 
                     0), ])
            if (((true_pos + false_pos) > 0) & !is.na(true_pos)) {
              Precision <- 
                true_pos / (true_pos + false_pos)
            } else {
              Precision <- 0
            }
            if (((true_pos + false_neg) > 0) & !is.na(true_pos)) {
              Recall <- 
                true_pos / (true_pos + false_neg)
            } else {
              Recall <- 0
            }
            if ((true_pos + false_pos + true_neg + false_neg) > 0) {
              Accuracy <- 
                (true_pos + true_neg) / 
                (true_pos + false_pos + true_neg + false_neg)
            } else {
              Accuracy <- 0
            }
            if ((Precision + Recall) > 0) {
              F1 <- (2 * Precision * Recall) / 
                (Precision + Recall)
            } else {
              F1 <- 0
            } 
#
            val_cumulative_accuracy[j, 1] <- "validation"
            val_cumulative_accuracy[j, 2] <- 
              all_permutations[current_index]
            val_cumulative_accuracy[j, 3] <- 
              current_index
            val_cumulative_accuracy[j, 4] <- splits[split_values, 3]
            val_cumulative_accuracy[j, 5] <- clusters_used
            val_cumulative_accuracy[j, 6] <- will_reorder_centers[j]
            val_cumulative_accuracy[j, 7] <- Precision
            val_cumulative_accuracy[j, 8] <- Recall
            val_cumulative_accuracy[j, 9] <- Accuracy
            val_cumulative_accuracy[j, 10] <- F1
          } # end loop through date list to calculate perforamnce metrics
#
          cat("Summary for Validation Data\n")
          print(val_cumulative_accuracy)
          val_cumulative_table <- rbind(val_cumulative_table,
                                        val_cumulative_accuracy)             
#           
            
        } # end if use kmeans block
#
        if (use_hclust == TRUE) {
          for (dist_method in 1:length(dist_methods)) {
            
            train_cust_summary_temp <- train_cust_summary
            test_cust_summary_temp <- test_cust_summary
            val_cust_summary_temp <- val_cust_summary
#
# remove customer id as it is mutually exclusie across data sets
# also remove next order to prevent leakage of information
# that is being predicted into the available data
#
            train_cust_summary_temp$cust_ids <- 0
            train_cust_summary_temp$next_order <- 0
            test_cust_summary_temp$cust_ids <- 0
            test_cust_summary_temp$next_order <- 0
            val_cust_summary_temp$cust_ids <- 0
            val_cust_summary_temp$next_order <- 0
#
# build cluster model using hclust
#
            if (dist_methods[dist_method] == "euclidean^2") {
              use_exponent <- 2
              method <- "euclidean"
            } else {
              use_exponent <- 1
              method <- dist_methods[dist_method]
            }
            hcluster_model <- 
              cutree(hclust(dist(train_cust_summary_temp,
                                 method = 
                                   method)^use_exponent,
                            method = all_methods[cluster_method]), 
                     k = clusters_used)
#
            hcluster_bars <- integer()
            for (bars in 1:clusters_used){
              hcluster_bars[bars] <- 
                length(hcluster_model[hcluster_model == bars])
            }
            main_label <- 
              paste0("hclust model ",
                     all_methods[cluster_method],
                     " using distance method ",
                     dist_methods[dist_method])
#
# summarize clusters in a bar plot
#
            barplot(hcluster_bars,
                    names.arg = c(1:clusters_used),
                    las = 1, 
                    cex.main = 0.8,
                    main = main_label,
                    cex.axis = 0.8, 
                    cex.names = 0.75,
                    xlab = "cluster number",
                    ylab = "count in cluster",
                    col = "lightblue")
#
            train_cust_summary_temp$cluster <- 
              hcluster_model
#
            recency_map <- 
              as.data.frame(matrix(rep(0, clusters_used * 2), 
                                   nrow = clusters_used, 
                                   ncol = 2))
            colnames(recency_map) = c("average recency", "count")
            reorder_map <- 
              as.data.frame(matrix(rep(0, clusters_used * 2), 
                                   nrow = clusters_used, 
                                   ncol = 2))
            colnames(reorder_map) = c("average reorder", "count")
            for (i in 1:clusters_used) {
              recency_map[i, 1] <- 
                mean(train_cust_summary_temp[
                  train_cust_summary_temp$cluster == i, ]$recency)
              recency_map[i, 1] <- 
                (recency_map[i, 1] * recency_scale) + recency_center
              recency_map[i, 2] <- 
                nrow(train_cust_summary_temp[
                  train_cust_summary_temp$cluster == i, ])
              reorder_map[i, 1] <- 
                mean(train_cust_summary_temp[
                  train_cust_summary_temp$cluster == i, ]$ave_reorder)
              reorder_map[i, 1] <- 
                (reorder_map[i, 1] * reorder_scale) + reorder_center
              reorder_map[i, 2] <- 
                nrow(train_cust_summary_temp[
                  train_cust_summary_temp$cluster == i, ])
            }
            recency_map <- 
              recency_map[order(recency_map$`average recency`), ]
            reorder_map <- 
              reorder_map[order(reorder_map$`average reorder`), ]   
            if (plots_verbose == TRUE) {
              barplot(recency_map$count,
                      names.arg = 
                        round(recency_map$`average recency`, 0),
                      las = 3, 
                      cex.axis = 0.8, 
                      cex.names = 0.75,
                      main = main_label,
                      cex.main = 0.75,
                      xlab = paste0("Clusters by Average Recency\n(days)"),
                      ylab = "count",
                      col = "lightblue")
            }
#
            if (plots_verbose == TRUE) {
              barplot(reorder_map$count,
                      names.arg = 
                        round(reorder_map$`average reorder`, 0),
                      las = 3, 
                      cex.axis = 0.8, 
                      cex.names = 0.75,
                      main = main_label,
                      cex.main = 0.75,
                      xlab = paste0("Clusters by Average Reorder\n(days)"),
                      ylab = "count",
                    col = "lightblue")
            }
#            
            hcluster_model_centers <- 
              t(sapply(unique(hcluster_model), 
                       clust.centroid, 
                       train_cust_summary_temp, 
                       hcluster_model))
            cluster_reorder_map <- 
              matrix(0,
                     nrow = nrow(hcluster_model_centers),
                     ncol = 2)
            hcluster_model_reorder_col <- 
              which(colnames(hcluster_model_centers) == "ave_reorder")
            for (i in 1:nrow(cluster_reorder_map)) {
              cluster_reorder_map[i, 1] <- i
              for (j in 1:length(will_reorder_centers)) {
                if ((hcluster_model_centers[i, hcluster_model_reorder_col] <= 
                     ((will_reorder_by_cutoffs[j + 1] - reorder_center) /
                      reorder_scale)) &
                    (hcluster_model_centers[i, hcluster_model_reorder_col] >=
                     ((will_reorder_by_cutoffs[j] - reorder_center) /
                      reorder_scale))) {
                  cluster_reorder_map[i, 2] <- will_reorder_centers[j]
                  break
                }
              }
              if (cluster_reorder_map[i, 2] == 0) { 
#
# if no mapping was found, then the cluster center is outside the range
# of values we are considering at present, so set the vale to beyond
# the max cutoff date to exclude from analysis of performance              
#
                cluster_reorder_map[i, 2] <- 
                  2 * max(will_reorder_by_cutoffs)
              }
            }
#
# now build revenue forecast model using the trining data clusters
#
# calculate the overall distribution of reordering for each cluster
#
            train_cluster_membership <- 
              unique(train_cust_summary_temp$cluster)
            train_cluster_membership <-
              train_cluster_membership[order(train_cluster_membership)]
            samples_cluster <- 
              as.data.frame(matrix(data = numeric(),
                                   nrow = 1000,
                                   ncol = clusters_used))
            train_cluster_distribution <- 
              as.data.frame(matrix(data = numeric(),
                                   nrow = clusters_used,
                                   ncol = 7))
            colnames(train_cluster_distribution) <- 
              c("cluster", "reorder_mean", "reorder_sd", 
                "value_mean", "value_sd", "frequency", "freq_sd")
            probs <- numeric()
            for (cluster in 1:clusters_used) {
              train_cluster_distribution[cluster, ]$cluster <- cluster
              if (!(cluster %in% train_cluster_membership)) {
                train_cluster_distribution[cluster, ]$reorder_mean <- 0
                train_cluster_distribution[cluster, ]$reorder_sd <- 0
                train_cluster_distribution[cluster, ]$value_mean <- 0              
                train_cluster_distribution[cluster, ]$value_sd <- 0
                train_cluster_distribution[cluster, ]$frequency <- 0
                train_cluster_distribution[cluster, ]$freq_sd <- 0
              } else {
                temp_samples <- 
                  which(train_cust_summary_temp$cluster == cluster)
                train_cluster_distribution[cluster, ]$frequency <-
                  mean(train_cust_summary_raw[temp_samples, ]$frequencies)
                train_cluster_distribution[cluster, ]$freq_sd <-
                  sd(train_cust_summary_raw[temp_samples, ]$frequencies)
#
# we weight the sampling by the frequency of orders for each member in the cluster
#
                probs <- 
                  train_cust_summary_raw[temp_samples, ]$frequencies
#
# in some cases rounding error can result in a very small neagative
# probability which breaks later calcuations, so clean that up
# also for those members which had 1 or less orders, the sd of the reorder
# time is undefined so set that to 0 as well
#                          
                probs[probs < 0] <- 0
                probs[train_cust_summary_raw[temp_samples, ]$sd_reorder == 0] <- 0
#
# deal with two special cases:
# 1) there is only 1 member of the cluster, in which case sampling doens't work
# 2) the sum of the probabiiities is 0 due to frequency being 0
#              
                if (length(probs) > 1 & sum(probs) > 0) {
#
# normalize
#              
                  probs <- probs / sum(probs)
#
# sample
#                
                  samples_cluster[, cluster] <- sample(temp_samples, 
                                                       prob = probs,
                                                       size = 1000,
                                                       replace = TRUE)
#
# model reorder time using a gamma distribution
#
                  temp_distribution <-
                    rgamma(n = 1000,
                           shape = 
                             ((train_cust_summary_raw$value_per_order[
                               samples_cluster[, cluster]]) /
                                train_cust_summary_raw$sd_reorder[
                                  samples_cluster[, cluster]])^2,
                           scale = 
                             (train_cust_summary_raw$sd_reorder[
                               samples_cluster[, cluster]])^2 /
                             (train_cust_summary_raw$value_per_order[
                               samples_cluster[, cluster]]))
                  train_cluster_distribution[cluster, ]$value_mean <-
                    mean(temp_distribution)
                  train_cluster_distribution[cluster, ]$value_sd <-
                    sd(temp_distribution)
                  temp_distribution <-
                    rgamma(n = 1000, 
                           shape = 
                             ((train_cust_summary_raw$ave_reorder[
                               samples_cluster[, cluster]]) /
                                train_cust_summary_raw$sd_reorder[
                                  samples_cluster[, cluster]])^2,
                           scale = 
                             (train_cust_summary_raw$sd_reorder[
                               samples_cluster[, cluster]])^2 /
                             (train_cust_summary_raw$ave_reorder[
                               samples_cluster[, cluster]]))
                  train_cluster_distribution[cluster, ]$reorder_mean <- 
                    mean(temp_distribution)
                  train_cluster_distribution[cluster, ]$reorder_sd <-
                    sd(temp_distribution)
                } else {
                  if (sum(probs) > 0) {
                    train_cluster_distribution[cluster, ]$reorder_mean <-
                      mean(train_cust_summary_temp[train_cust_summary_temp$cluster == 
                                                     cluster, ]$value_per_order)
                    train_cluster_distribution[cluster, ]$reorder_sd <- 0
                  } else {
                    train_cluster_distribution[cluster, ]$reorder_mean <- 0
                    train_cluster_distribution[cluster, ]$reorder_sd <- 0
                  }
                }
              }
            } # end of loop through clusters calculating reorder time distribution
            train_cluster_distribution[is.na(train_cluster_distribution)] <- 0
            metrics <- c("reorder_mean", "value_mean")
            metrics_cols <- 
              which(colnames(train_cluster_distribution) %in% metrics)
            metrics_bins <- 35
            metrics_sds <- c("reorder_sd", "value_sd")
            metrics_sds_cols <- 
              which(colnames(train_cluster_distribution) %in% metrics_sds)
#
# compute order value distribution to compare to raw data
#          
            metric <- 2
            if (plots_verbose == TRUE) {
              plot_hist <- TRUE
              par(mfrow = c(3, 3))
            } else {
              plot_hist <- FALSE
            }
            train_metric_sample <-
              matrix(nrow = 1000, 
                     ncol = nrow(train_cluster_distribution))
            train_metric_samples <- 
              matrix(nrow = metrics_bins,
                     ncol = 2 * nrow(train_cluster_distribution))
            for (i in seq(1, 2 * nrow(train_cluster_distribution) - 1, 2)) {
              if (train_cluster_distribution[(i + 1) / 2, 
                                             metrics_cols[metric]] > 0 &
                  train_cluster_distribution[(i + 1) / 2, 
                                             metrics_sds_cols[metric]] > 0) {
                train_metric_sample[, (i + 1) / 2] <-
                  rgamma(n = 1000, 
                         shape = 
                           (train_cluster_distribution[(i + 1) / 2, 
                                                       metrics_cols[metric]] /
                              train_cluster_distribution[(i + 1) / 2, 
                                                         metrics_sds_cols[metric]])^2,
                         scale = 
                           ((train_cluster_distribution[(i + 1) / 2, 
                                                        metrics_sds_cols[metric]])^2 /
                              train_cluster_distribution[(i + 1) / 2, 
                                                         metrics_cols[metric]]))
                bin_size <- max(
                  floor((max(train_metric_sample[, (i + 1) / 2]) -
                           min(train_metric_sample[, (i + 1) / 2]))/ metrics_bins),
                  1)
                if (!is.na(max(train_metric_sample[, (i + 1) / 2]))) {
                  temp_hist <- 
                    hist(train_metric_sample[, (i + 1) / 2],
                         breaks = 
                           seq(floor(min(train_metric_sample[, (i + 1) / 2])),
                               ceiling(max(train_metric_sample[, (i + 1) / 2]) +
                                         2 * bin_size - 
                                         (max(train_metric_sample[, (i + 1) / 2]) %% bin_size)),
                               bin_size),
                         plot = plot_hist,
                         col = "lightblue",
                         xlab = "Order value ($)",
                         main = paste0("Order value distribution\n",
                                       "cluster ", (i + 1) / 2),
                         cex.main = 0.75)
                  train_metric_samples[, i] <- 
                    temp_hist[["breaks"]][1:metrics_bins]
                  train_metric_samples[, i + 1] <- 
                    temp_hist[["counts"]][1:metrics_bins]
                } else {
                  train_metric_samples[, i] <- 0
                  train_metric_samples[, i + 1] <- 0
                }
              } else {
                train_metric_samples[, i] <- 0
                train_metric_samples[, i + 1] <- 0
              }
            } # end of loop through clusters to calculate order value distribution
            par(mfrow = c(1, 1))
#
# for the order value data, we want to concatenate all the 
# values from all the histograms for each cluster to get
# the overall distribution
#            
            temp_hist_cutoff <- 50000
            temp_hist_bins <- 35
            bin_size <- temp_hist_cutoff / temp_hist_bins
            temp_data <- numeric(length = temp_hist_bins)
            for (i in 1:nrow(train_cluster_distribution)) {
              temp_temp_data <- numeric()
              temp_hist <- numeric(length = temp_hist_bins)
              for (j in 1:length(train_metric_samples[, (2 * i)])) {
                if (!is.na(train_metric_samples[j, (2 * i)])) {
#
# replicate the order value by the number of counts in the hist data
#                  
                  temp_temp_data <- c(temp_temp_data, 
                                      rep(train_metric_samples[j, (2 * i) - 1],
                                          train_metric_samples[j, (2 * i)]))
                }
#
# use hist() to bin the data in order to keep the concatenation small
#                
                temp_hist <- 
                  temp_hist +
                  hist(temp_temp_data[temp_temp_data < temp_hist_cutoff],
                       breaks = seq(0, temp_hist_cutoff, bin_size),
                       plot = FALSE)[["counts"]]
              }
              temp_data <- temp_data + temp_hist
            }
            if (plots_verbose == TRUE) {
              barplot(temp_data,
                      names.arg = ceiling(seq(bin_size / 2, temp_hist_cutoff, bin_size)),
                      las = 2,
                      col = "lightblue",
                      main = paste0("Training Data\n",
                                    "Avg. Value of Orders estimated from cluster model\n",
                                    "(Avg. cust. order < $",
                                    temp_hist_cutoff, ")\n",
                                    "**reference only**"),
                      cex.main = 0.75,
                      ylab = "count",
                      xlab = "Avg. order value ($)")
            }
#
# now comapre the statistically modeled distribution to the actual data
#
# modeled distribution
#            
            plot(cbind(ceiling(seq(bin_size / 2, 
                                   temp_hist_cutoff, bin_size)), 
                       temp_data / max(temp_data)),
                 type = "l",
                 lty = 1,
                 col = "lightblue",
                 lwd = 2,
                 xlab = "Average Order Value ($)",
                 ylab = "Relative probability",
                 main = paste0("Comparison of estimated order value distribution\n",
                               "to actual training data"),
                 cex.main = 0.75)
#
# actual training data saved from earlier histogram
#
            lines(train_value_hist_act, 
                  lty = 2,
                  col = "darkgreen",
                  lwd = 1)
#
# compute reorder time distribution to compare to raw data
#          
            metrics_bins <- max(will_reorder_by_cutoffs)
            metric <- 1
            if (plots_verbose == TRUE) {
              plot_hist <- TRUE
              par(mfrow = c(3, 3))
            } else {
              plot_hist <- FALSE
            }
            train_metric_sample <-
              matrix(nrow = 1000, 
                     ncol = nrow(train_cluster_distribution))
            train_metric_samples <- 
              matrix(nrow = metrics_bins,
                     ncol = 2 * nrow(train_cluster_distribution))
            for (i in seq(1, 2 * nrow(train_cluster_distribution) - 1, 2)) {
              if (train_cluster_distribution[(i + 1) / 2, 
                                             metrics_cols[metric]] > 0 &
                  train_cluster_distribution[(i + 1) / 2, 
                                             metrics_sds_cols[metric]] > 0) {
                train_metric_sample[, (i + 1) / 2] <-
                  rgamma(n = 1000, 
                         shape = 
                           (train_cluster_distribution[(i + 1) / 2, 
                                                       metrics_cols[metric]] /
                              train_cluster_distribution[(i + 1) / 2, 
                                                         metrics_sds_cols[metric]])^2,
                         scale = 
                           ((train_cluster_distribution[(i + 1) / 2, 
                                                        metrics_sds_cols[metric]])^2 /
                              train_cluster_distribution[(i + 1) / 2, 
                                                         metrics_cols[metric]]))
                bin_size <- 1
                if (!is.na(max(train_metric_sample[, (i + 1) / 2]))) {
                  temp_hist <- 
                    hist(train_metric_sample[, (i + 1) / 2],
                         breaks = 
                           seq(floor(min(train_metric_sample[, (i + 1) / 2])),
                               ceiling(max(train_metric_sample[, (i + 1) / 2]) +
                                         2 * bin_size - 
                                         (max(train_metric_sample[, (i + 1) / 2]) %% bin_size)),
                               bin_size),
                         plot = plot_hist,
                         col = "lightblue",
                         xlab = "days to next order",
                         main = paste0("Time to reorder\n",
                                       "cluster ", (i + 1) / 2),
                         cex.main = 0.75)
                  train_metric_samples[, i] <- 
                    temp_hist[["breaks"]][1:metrics_bins]
                  train_metric_samples[, i + 1] <- 
                    temp_hist[["counts"]][1:metrics_bins]
                } else {
                  train_metric_samples[, i] <- 0
                  train_metric_samples[, i + 1] <- 0
                }
              } else {
                train_metric_samples[, i] <- 0
                train_metric_samples[, i + 1] <- 0
              }
            } # end of loop over clusters to calcuale reorder time distribution
            par(mfrow = c(1, 1))
#
# for the reorder time distribution, the data are in 
# one-day bins for each cluster, so we want to sum up the
# counts for each day across all clusters
#            
            temp_data <- 
              rowSums(train_metric_samples[, 
                                           seq(2, 
                                               ncol(train_metric_samples), 2)],
                      na.rm = TRUE)
            temp_hist_cutoff <- max(will_reorder_by_cutoffs)
            if (plots_verbose == TRUE) {
              barplot(temp_data,
                      names.arg = seq(1, temp_hist_cutoff, 1),
                      las = 2,
                      col = "lightblue",
                      main = paste0("Training Data\n",
                                    "Time to reorder estimated from cluster model\n",
                                    "**reference only**"),
                      cex.main = 0.75,
                      cex.axis = 0.75,
                      ylab = "count",
                      xlab = "Time to reorder (days)")
            }
#
# now comapre the statistically modeled distribution to the actual data
#
# modeled distribution
#            
            plot(cbind(ceiling(seq(bin_size / 2, 
                                   temp_hist_cutoff, bin_size)), 
                       temp_data / max(temp_data)),
                 type = "l",
                 lty = 1,
                 col = "lightblue",
                 lwd = 2,
                 xlab = "days",
                 ylab = "Relative probability",
                 main = paste0("Comparison of time to reorder distribution\n",
                               "to actual training data"),
                 cex.main = 0.75)
#
# actual training data saved from earlier histogram
#
            lines(train_reorder_hist_act, 
                  lty = 2,
                  col = "darkgreen",
                  lwd = 1)
#
# compute distances to clusters for test data
#
# transform the model data and remove the clusters (last) column
#
            hcluster_model_centers <- 
              as.data.frame(hcluster_model_centers[, 1:(ncol(hcluster_model_centers) - 1)])
#
            test_dist <- matrix(0,
                                nrow = nrow(test_cust_summary_temp),
                                ncol = clusters_used)
#  
            for (i in 1:nrow(test_cust_summary_temp)) {
              test_dist[i, ] <- 
                dist(rbind(test_cust_summary_temp[i, ],
                           hcluster_model_centers),
                     method =
                       method)[1:nrow(hcluster_model_centers)]
            }
# 
            test_clusters <- numeric(nrow(test_dist))
            for (i in 1:nrow(test_dist)) {
              test_clusters[i] <- which(test_dist[i, ] == 
                                          min(test_dist[i, ]))
            }
            test_cust_summary_temp <- cbind(test_cust_summary_temp,
                                            cluster = test_clusters)
#
# predict the future revenue for the test data using the 
# train_cluster_distribution model with the probabilities
# by cluster based on the test data population, then compare
# to the actual future revenue for the test instances            
#
            test_cluster_membership <- 
              unique(test_cust_summary_temp$cluster)
            test_cluster_membership <- 
              test_cluster_membership[order(test_cluster_membership)]
#
# for a given cluster, the number of orders predicted is
# # in cluster * frequency * 
#          (duration of prediction window) / (date range of test data)         
# where frequency is the count of orders in the test data
# the process to predict revenue for the cluster is then
# for each cluster member
#   sample frequency
#     up to (frequecy) times, while T < forecast period
#       sample reorder time; T = T + reorder time
#       sample value; V = V + order value
# 
            temp_forecast <- data.frame(time = numeric(), value = numeric())
            sample_size <- 1000
            sample_count <- 0
            for (cluster in test_cluster_membership) {
              skip <- FALSE
              if (train_cluster_distribution$frequency[cluster] == 0) {
                skip <- TRUE
              } else if (train_cluster_distribution$freq_sd == 0) {
                temp_freq_dist <- train_cluster_distribution$frequency[cluster]
              } else {
                temp_freq_dist <- 
                  rgamma(n = sample_size, 
                         shape = (train_cluster_distribution$frequency[cluster] /
                                    train_cluster_distribution$freq_sd[cluster])^2,
                         scale = train_cluster_distribution$freq_sd[cluster]^2 /
                           train_cluster_distribution$frequency[cluster])
              }
              if (train_cluster_distribution$reorder_mean[cluster] == 0) {
                skip <- TRUE
              } else if (train_cluster_distribution$reorder_sd[cluster] == 0) {
                temp_reorder_dist <- train_cluster_distribution$reorder_mean[cluster]
              } else {
                temp_reorder_dist <- 
                  rgamma(n = sample_size, 
                         shape = (train_cluster_distribution$reorder_mean[cluster] /
                                    train_cluster_distribution$reorder_sd[cluster])^2,
                         scale = train_cluster_distribution$reorder_sd[cluster]^2 /
                           train_cluster_distribution$reorder_mean[cluster])
              }
              if (train_cluster_distribution$value_mean[cluster] == 0) {
                skip <- TRUE
              } else if (train_cluster_distribution$value_sd[cluster] == 0) {
                temp_value_dist <- train_cluster_distribution$value_mean[cluster]
              } else {
                temp_value_dist <- 
                  rgamma(n = sample_size, 
                         shape = (train_cluster_distribution$value_mean[cluster] /
                                    train_cluster_distribution$value_sd[cluster])^2,
                         scale = train_cluster_distribution$value_sd[cluster]^2 /
                           train_cluster_distribution$value_mean[cluster])
              }
              if (skip == FALSE) {
                for (member in 1:length(which(test_cust_summary_temp$cluster == 
                                              test_cluster_membership[cluster]))) {
                  temp_freq <- sample(x = temp_freq_dist, size = 1)
                  Time <- 0
                  Steps <- 0
#
# adjust frequency by the ratio of forecast time to train data time
#              
                  temp_freq <- temp_freq * forecast_period / (max_date - min_date)
                  while ((Time <= forecast_period) & (Steps <= temp_freq)) {
                    Time <- Time + sample(x = temp_reorder_dist, size = 1)
                    sample_count <- sample_count + 1
                    temp_forecast[sample_count, ]$time <- Time
                    temp_forecast[sample_count, ]$value <- 
                      sample(x = temp_value_dist, size = 1)
                    Steps <- Steps + 1
                  }
                }
              }
            }
            current_index <- 
              which(all_permutations ==
                      paste0(all_methods[cluster_method],
                             "_",
                             dist_methods[dist_method],
                             "_",
                             clusters_used))
            for (i in 1:forecast_period) {
              test_forecast[i, (current_index - 1) * 6 + 2] <- 
                sum(temp_forecast[temp_forecast[, ]$time <= i &
                                    temp_forecast[, ]$time > (i - 1), 2])
              test_forecast[i, (current_index - 1) * 6 + 4] <-
                sum(raw_data$value[(raw_data$order_date - 
                                      max_act_test_date) <= i &
                                     (raw_data$order_date - 
                                        max_act_test_date) > (i - 1) &
                                     raw_data$cust_no %in% test_data_subset$cust_no])
              if (i > 1) {
                test_forecast[i, (current_index - 1) * 6 + 3] <- 
                  test_forecast[i, (current_index - 1) * 6 + 2] + 
                  test_forecast[i - 1, (current_index - 1) * 6 + 3]
                test_forecast[i, (current_index - 1) * 6 + 5] <- 
                  test_forecast[i, (current_index - 1) * 6 + 4] +
                  test_forecast[i - 1, (current_index - 1) * 6 + 5]
              } else {
                test_forecast[i, (current_index - 1) * 6 + 3] <- 
                  test_forecast[i, (current_index - 1) * 6 + 2]
                test_forecast[i, (current_index - 1) * 6 + 5] <- 
                  test_forecast[i, (current_index - 1) * 6 + 4]
              }
            }
#
# calculate errors for the forecast
#
            for (i in 1:forecast_period) {
              test_forecast[i, (current_index - 1) * 6 + 6] <-
                ((test_forecast[i, (current_index - 1) * 6 + 2] -
                    test_forecast[i, (current_index - 1) * 6 + 4]) /
                   test_forecast[i, (current_index - 1) * 6 + 4])
              test_forecast[i, (current_index - 1) * 6 + 7] <- 
                ((test_forecast[i, (current_index - 1) * 6 + 3] -
                    test_forecast[i, (current_index - 1) * 6 + 5]) /
                   test_forecast[i, (current_index - 1) * 6 + 5])
            }
            plot_forecast <- test_forecast
            for (k in 1:length(smooth_cols)) {
              plot_forecast[, smooth_cols[k]] <- 
                rollmean(plot_forecast[, smooth_cols[k]], 
                         smooth_window, 
                         fill = NA)
              plot_forecast[1:nrow(top_pad), smooth_cols[k]] <- top_pad[, k]
              plot_forecast[(forecast_period - nrow(bottom_pad) + 1):forecast_period,
                            smooth_cols[k]] <- bottom_pad[, k]
            }
            temp_y_scale <- 
              c(0, 1.2 * max(plot_forecast[, (current_index - 1) * 6 + 2],
                             plot_forecast[, (current_index - 1) * 6 + 4]))
            scale_factor <- 1e6
            plot(x = plot_forecast[, 1], 
                 y = plot_forecast[, (current_index - 1) * 6 + 4] / scale_factor, 
                 type = "l", col = "blue",
                 xlab = "days",
                 ylab = "Forecast/Actual ($M)",
                 main = paste0("test data\n",
                               "hclust model using method ",
                               all_methods[cluster_method]),
                 ylim = temp_y_scale / scale_factor)
            lines(x = plot_forecast[, 1], 
                  y = plot_forecast[, (current_index - 1) * 6 + 2] / scale_factor,
                  col = "red",
                  ylim = temp_y_scale / scale_factor)
            temp_y_scale <- 
              c(0, 1.2 * max(plot_forecast[, (current_index - 1) * 6 + 3],
                             plot_forecast[, (current_index - 1) * 6 + 5]))
            plot(x = plot_forecast[, 1], 
                 y = plot_forecast[, (current_index - 1) * 6 + 3] / scale_factor, 
                 type = "l", col = "red",
                 xlab = "days",
                 ylab = "Cumulative Forecast/Actual ($M)",
                 main = paste0("test data\n",
                               "hclust model using method ",
                               all_methods[cluster_method]),
                 ylim = temp_y_scale / scale_factor)
            lines(x = plot_forecast[, 1], 
                  y = plot_forecast[, (current_index - 1) * 6 + 5] / scale_factor,
                  col = "blue",
                  ylim = temp_y_scale / scale_factor)
#            
            test_pred_reorder <- numeric(nrow(test_cust_summary_temp))
            for (i in 1:length(test_pred_reorder)) {
              test_pred_reorder[i] <-
                cluster_reorder_map[
                  cluster_reorder_map[, 1] == 
                    test_cust_summary_temp$cluster[i], 2]
            }
#
            test_cust_summary_temp <- 
              cbind(test_cust_summary_temp, 
                    pred_reorder = test_pred_reorder)
#
            test_cluster_reorder <- 
              numeric(nrow(test_cust_summary_temp))
#
            for (i in 1:length(test_cluster_reorder)) {
#
# note use of the original data.frame to test reorder as
# the temp data.frame used to test cluster membership
# excludes the next order dates
#
              if (!is.na(test_cust_summary$next_order[i])) {
                for (j in 1:length(will_reorder_centers)) {
                  if (test_cust_summary_raw$next_order[i] - 
                       test_cust_summary_raw$cust_end[i] <= 
                       will_reorder_by_cutoffs[j + 1] &
                      test_cust_summary_raw$next_order[i] -
                      test_cust_summary_raw$cust_end[i] >
                      will_reorder_by_cutoffs[j]) {
                    test_cluster_reorder[i] <- will_reorder_centers[j]
                    break
                  }
                }
              }
              if (test_cluster_reorder[i] == 0) {
#
# if no mapping was found, then the cluster center is outside the range
# of values we are comparing; therefore set it to the mean of the largest
# test value and the actual value
#
                test_cluster_reorder[i] <- 
                  mean(will_reorder_by_cutoffs[
                    length(will_reorder_by_cutoffs)],
                    test_cust_summary_temp$ave_reorder[i])
              }
            }
#
            test_cust_summary_temp <- 
              cbind(test_cust_summary_temp,
                    act_reorder = test_cluster_reorder)
#
# re-evaluate test data using the hclust model
#
# test if assigned cluster matches recency bin
#
            test_recency_accuracy <- 
              as.data.frame(matrix(0,
                                   nrow = nrow(test_cust_summary_temp),
                                   ncol = 5))
            colnames(test_recency_accuracy) <- 
              c("act_reorder", "pred_reorder", 
                "match", "pred > act", "act > pred")
            for (i in 1:nrow(test_cust_summary_temp)) {
              test_recency_accuracy[i, 1] <- 
                test_cust_summary_temp$act_reorder[i]
              test_recency_accuracy[i, 2] <- 
                test_cust_summary_temp$pred_reorder[i]
              if (test_cust_summary_temp$pred_reorder[i] == 
                  test_cust_summary_temp$act_reorder[i]) {
                test_recency_accuracy[i, 3] <- 1
              } else if (test_cust_summary_temp$pred_reorder[i] > 
                         test_cust_summary_temp$act_reorder[i]) {
                test_recency_accuracy[i, 4] <- 1
              } else {
                test_recency_accuracy[i, 5] <- 1
              }
            }
#
            test_recency_cumulative <- 
              as.data.frame(matrix(0,
                                   nrow = 
                                     nrow(test_cust_summary_temp),
                                   ncol = 
                                     2 * length(will_reorder_centers)))
            cum_recency_colnames <- character()
            index <- 0
            for (i in seq(1, ncol(test_recency_cumulative), 2)) {
              index <- index + 1
              cum_recency_colnames[i] <-
                paste0("pred_reorder_by", will_reorder_by_cutoffs[index])
              cum_recency_colnames[i + 1] <-
                paste0("act_reorder_by", will_reorder_by_cutoffs[index])
            }
#
            colnames(test_recency_cumulative) <- 
              cum_recency_colnames
#
            for (i in 1:nrow(test_cust_summary_temp)) {
              for (j in 1:length(will_reorder_centers)) {
                if (test_cust_summary_temp$pred_reorder[i] <= 
                     will_reorder_by_cutoffs[j + 1]) {
                  test_recency_cumulative[i, (j * 2 - 1)] <- 1
                }
              }
              for (j in 1:length(will_reorder_centers)) {
                if (test_cust_summary_temp$act_reorder[i] <= 
                     will_reorder_by_cutoffs[j + 1]) {
                  test_recency_cumulative[i, (j * 2)] <- 1
                }
              }
            }
#      
            test_cumulative_accuracy <- 
              as.data.frame(matrix(0,
                                   nrow = 
                                     length(will_reorder_centers),
                                   ncol = 10))
            colnames(test_cumulative_accuracy) <- 
              c("data", "Algorithm", "ID", "split", 
                "clusters", "date", "Precision", 
                "Recall", "Accuracy", "F1")
#
            for (j in seq(1, length(will_reorder_centers), 1)) {
              true_pos <- 
                nrow(test_recency_cumulative[
                  (test_recency_cumulative[, (j * 2 - 1)] ==
                     test_recency_cumulative[, (j * 2)]) &
                    (test_recency_cumulative[, (j * 2 - 1)] == 
                       1), ])
              false_pos <- 
                nrow(test_recency_cumulative[
                  (test_recency_cumulative[, (j * 2 - 1)] !=
                     test_recency_cumulative[, (j * 2)]) &
                    (test_recency_cumulative[, (j * 2 - 1)] == 
                       1), ])
              true_neg <-
                nrow(test_recency_cumulative[
                  (test_recency_cumulative[, (j * 2 - 1)] ==
                     test_recency_cumulative[, (j * 2)]) &
                    (test_recency_cumulative[, (j * 2 - 1)] == 
                       0), ])
              false_neg <-
                nrow(test_recency_cumulative[
                  (test_recency_cumulative[, (j * 2 - 1)] !=
                     test_recency_cumulative[, (j * 2)]) &
                    (test_recency_cumulative[, (j * 2 - 1)] == 
                       0), ])
              if (((true_pos + false_pos) > 0) & !is.na(true_pos)) {
                Precision <- 
                  true_pos / (true_pos + false_pos)
              } else {
                Precision <- 0
              }
              if (((true_pos + false_neg) > 0) & !is.na(true_pos)) {
                Recall <- 
                  true_pos / (true_pos + false_neg)
              } else {
                Recall <- 0
              }
              if ((true_pos + false_pos + true_neg + false_neg) > 0) {
                Accuracy <- 
                  (true_pos + true_neg) / 
                  (true_pos + false_pos + true_neg + false_neg)
              } else {
                Accuracy <- 0
              }
              if ((Precision + Recall) > 0) {
                F1 <- (2 * Precision * Recall) / 
                  (Precision + Recall)
              } else {
                F1 <- 0
              }
#
              test_cumulative_accuracy[j, 1] <- "test"
              test_cumulative_accuracy[j, 2] <- 
                all_permutations[current_index]
              test_cumulative_accuracy[j, 3] <- 
                current_index
              test_cumulative_accuracy[j, 4] <- splits[split_values, 2]
              test_cumulative_accuracy[j, 5] <- clusters_used
              test_cumulative_accuracy[j, 6] <- will_reorder_centers[j]
              test_cumulative_accuracy[j, 7] <- Precision
              test_cumulative_accuracy[j, 8] <- Recall
              test_cumulative_accuracy[j, 9] <- Accuracy
              test_cumulative_accuracy[j, 10] <- F1
            } # end loop through date list to calculate perforamnce metrics
#
            cat("Summary for Test Data\n")
            print(test_cumulative_accuracy)
            test_cumulative_table <- rbind(test_cumulative_table,
                                           test_cumulative_accuracy)
#
# compute distances to train data clusters for validation data
#
            val_dist <- matrix(0,
                               nrow = nrow(val_cust_summary_temp),
                               ncol = clusters_used)
#  
            for (i in 1:nrow(val_cust_summary_temp)) {
              val_dist[i, ] <- 
                dist(rbind(val_cust_summary_temp[i, ],
                           hcluster_model_centers),
                     method =
                     method)[1:nrow(hcluster_model_centers)]
            }
# 
            val_clusters <- numeric(nrow(val_dist))
            for (i in 1:nrow(val_dist)) {
              val_clusters[i] <- which(val_dist[i, ] == 
                                         min(val_dist[i, ]))
            }
            val_cust_summary_temp <- cbind(val_cust_summary_temp,
                                           cluster = val_clusters)
#
# predict the future revenue for the val data using the 
# train_cluster_distribution model with the probabilities
# by cluster based on the val data population, then compare
# to the actual future revenue for the val instances            
#
            val_cluster_membership <- 
              unique(val_cust_summary_temp$cluster)
            val_cluster_membership <- 
              val_cluster_membership[order(val_cluster_membership)]
#
# for a given cluster, the number of orders predicted is
# # in cluster * frequency * 
#          (duration of prediction window) / (date range of val data)         
# where frequency is the count of orders in the val data
# the process to predict revenue for the cluster is then
# for each cluster member
#   sample frequency
#     up to (frequecy) times, while T < forecast period
#       sample reorder time; T = T + reorder time
#       sample value; V = V + order value
# 
            temp_forecast <- data.frame(time = numeric(), value = numeric())
            sample_size <- 1000
            sample_count <- 0
            for (cluster in val_cluster_membership) {
              skip <- FALSE
              if (train_cluster_distribution$frequency[cluster] == 0) {
                skip <- TRUE
              } else if (train_cluster_distribution$freq_sd == 0) {
                temp_freq_dist <- train_cluster_distribution$frequency[cluster]
              } else {
                temp_freq_dist <- 
                  rgamma(n = sample_size, 
                         shape = (train_cluster_distribution$frequency[cluster] /
                                    train_cluster_distribution$freq_sd[cluster])^2,
                         scale = train_cluster_distribution$freq_sd[cluster]^2 /
                           train_cluster_distribution$frequency[cluster])
              }
              if (train_cluster_distribution$reorder_mean[cluster] == 0) {
                skip <- TRUE
              } else if (train_cluster_distribution$reorder_sd[cluster] == 0) {
                temp_reorder_dist <- train_cluster_distribution$reorder_mean[cluster]
              } else {
                temp_reorder_dist <- 
                  rgamma(n = sample_size, 
                         shape = (train_cluster_distribution$reorder_mean[cluster] /
                                    train_cluster_distribution$reorder_sd[cluster])^2,
                         scale = train_cluster_distribution$reorder_sd[cluster]^2 /
                           train_cluster_distribution$reorder_mean[cluster])
              }
              if (train_cluster_distribution$value_mean[cluster] == 0) {
                skip <- TRUE
              } else if (train_cluster_distribution$value_sd[cluster] == 0) {
                temp_value_dist <- train_cluster_distribution$value_mean[cluster]
              } else {
                temp_value_dist <- 
                  rgamma(n = sample_size, 
                         shape = (train_cluster_distribution$value_mean[cluster] /
                                    train_cluster_distribution$value_sd[cluster])^2,
                         scale = train_cluster_distribution$value_sd[cluster]^2 /
                           train_cluster_distribution$value_mean[cluster])
              }
              if (skip == FALSE) {
                for (member in 1:length(which(val_cust_summary_temp$cluster == 
                                              val_cluster_membership[cluster]))) {
                  temp_freq <- sample(x = temp_freq_dist, size = 1)
                  Time <- 0
                  Steps <- 0
#
# adjust frequency by the ratio of forecast time to train data time
#              
                  temp_freq <- temp_freq * forecast_period / (max_date - min_date)
                  while ((Time <= forecast_period) & (Steps <= temp_freq)) {
                    Time <- Time + sample(x = temp_reorder_dist, size = 1)
                    sample_count <- sample_count + 1
                    temp_forecast[sample_count, ]$time <- Time
                    temp_forecast[sample_count, ]$value <- 
                      sample(x = temp_value_dist, size = 1)
                    Steps <- Steps + 1
                  }
                }
              }
            }
            current_index <- 
              which(all_permutations ==
                      paste0(all_methods[cluster_method],
                             "_",
                             dist_methods[dist_method],
                             "_",
                             clusters_used))
            for (i in 1:forecast_period) {
              val_forecast[i, (current_index - 1) * 6 + 2] <- 
                sum(temp_forecast[temp_forecast[, ]$time <= i &
                                    temp_forecast[, ]$time > (i - 1), 2])
              val_forecast[i, (current_index - 1) * 6 + 4] <-
                sum(raw_data$value[(raw_data$order_date - 
                                      max_act_val_date) <= i &
                                     (raw_data$order_date - 
                                        max_act_val_date) > (i - 1) &
                                     raw_data$cust_no %in% val_data_subset$cust_no])
              if (i > 1) {
                val_forecast[i, (current_index - 1) * 6 + 3] <- 
                  val_forecast[i, (current_index - 1) * 6 + 2] + 
                  val_forecast[i - 1, (current_index - 1) * 6 + 3]
                val_forecast[i, (current_index - 1) * 6 + 5] <- 
                  val_forecast[i, (current_index - 1) * 6 + 4] +
                  val_forecast[i - 1, (current_index - 1) * 6 + 5]
              } else {
                val_forecast[i, (current_index - 1) * 6 + 3] <- 
                  val_forecast[i, (current_index - 1) * 6 + 2]
                val_forecast[i, (current_index - 1) * 6 + 5] <- 
                  val_forecast[i, (current_index - 1) * 6 + 4]
              }
            }
#
# calculate errors for the forecast
#
            for (i in 1:forecast_period) {
              val_forecast[i, (current_index - 1) * 6 + 6] <-
                ((val_forecast[i, (current_index - 1) * 6 + 2] -
                    val_forecast[i, (current_index - 1) * 6 + 4]) /
                   val_forecast[i, (current_index - 1) * 6 + 4])
              val_forecast[i, (current_index - 1) * 6 + 7] <- 
                ((val_forecast[i, (current_index - 1) * 6 + 3] -
                    val_forecast[i, (current_index - 1) * 6 + 5]) /
                   val_forecast[i, (current_index - 1) * 6 + 5])
            }
            plot_forecast <- val_forecast
            for (k in 1:length(smooth_cols)) {
              plot_forecast[, smooth_cols[k]] <- 
                rollmean(plot_forecast[, smooth_cols[k]], 
                         smooth_window, 
                         fill = NA)
              plot_forecast[1:nrow(top_pad), smooth_cols[k]] <- top_pad[, k]
              plot_forecast[(forecast_period - nrow(bottom_pad) + 1):forecast_period,
                            smooth_cols[k]] <- bottom_pad[, k]
            }
            temp_y_scale <- 
              c(0, 1.2 * max(plot_forecast[, (current_index - 1) * 6 + 2],
                             plot_forecast[, (current_index - 1) * 6 + 4]))
            scale_factor <- 1e6
            plot(x = plot_forecast[, 1], 
                 y = plot_forecast[, (current_index - 1) * 6 + 4] / scale_factor, 
                 type = "l", col = "blue",
                 xlab = "days",
                 ylab = "Forecast/Actual ($M)",
                 main = paste0("validation data\n",
                               "hclust model using method ",
                               all_methods[cluster_method]),
                 ylim = temp_y_scale / scale_factor)
            lines(x = plot_forecast[, 1], 
                  y = plot_forecast[, (current_index - 1) * 6 + 2] / scale_factor,
                  col = "red",
                  ylim = temp_y_scale / scale_factor)
            temp_y_scale <- 
              c(0, 1.2 * max(plot_forecast[, (current_index - 1) * 6 + 3],
                             plot_forecast[, (current_index - 1) * 6 + 5]))
            plot(x = plot_forecast[, 1], 
                 y = plot_forecast[, (current_index - 1) * 6 + 3] / scale_factor, 
                 type = "l", col = "red",
                 xlab = "days",
                 ylab = "Cumulative Forecast/Actual ($M)",
                 main = paste0("validation data\n",
                               "hclust model using method ",
                               all_methods[cluster_method]),
                 ylim = temp_y_scale / scale_factor)
            lines(x = plot_forecast[, 1], 
                  y = plot_forecast[, (current_index - 1) * 6 + 5] / scale_factor,
                  col = "blue",
                  ylim = temp_y_scale / scale_factor)
#            
            val_pred_reorder <- numeric(nrow(val_cust_summary_temp))
            for (i in 1:length(val_pred_reorder)) {
              val_pred_reorder[i] <-
                cluster_reorder_map[
                  cluster_reorder_map[, 1] == 
                    val_cust_summary_temp$cluster[i], 2]
            }
#
            val_cust_summary_temp <- 
              cbind(val_cust_summary_temp, 
                    pred_reorder = val_pred_reorder)
#
            val_cluster_reorder <- 
              numeric(nrow(val_cust_summary_temp))
#
            for (i in 1:length(val_cluster_reorder)) {
              for (j in 1:length(will_reorder_centers)) {
#
# note use of the original data.frame to test reorder as
# the temp data.frame used to test cluster membership
# excludes the next order dates
#
                if (!is.na(val_cust_summary$next_order[i])) {
                  if (val_cust_summary_raw$next_order[i] - 
                       val_cust_summary_raw$cust_end[i] <= 
                       will_reorder_by_cutoffs[j + 1] &
                      val_cust_summary_raw$next_order[i] -
                      val_cust_summary_raw$cust_end[i] >
                      will_reorder_by_cutoffs[j]) {
                    val_cluster_reorder[i] <- will_reorder_centers[j]
                    break
                  }
                }
              }
              if (val_cluster_reorder[i] == 0) {
#
# if no mapping was found, then the cluster center is outside the range
# of values we are comparing; therefore set it to the mean of the largest
# test value and the actual value
#
                val_cluster_reorder[i] <- 
                  mean(will_reorder_by_cutoffs[
                    length(will_reorder_by_cutoffs)],
                    val_cust_summary_temp$ave_reorder[i])
              }
            }
#
            val_cust_summary_temp <- 
              cbind(val_cust_summary_temp,
                    act_reorder = val_cluster_reorder)
#
# re-evaluate validation data using the hclust model
#
# test if assigned cluster matches recency bin
#
            val_recency_accuracy <- 
              as.data.frame(matrix(0,
                                   nrow = nrow(val_cust_summary_temp),
                                   ncol = 5))
            colnames(val_recency_accuracy) <- 
              c("act_reorder", "pred_reorder",
                "match", "pred > act", "act > pred")
            for (i in 1:nrow(val_cust_summary_temp)) {
              val_recency_accuracy[i, 1] <- 
                val_cust_summary_temp$act_reorder[i]
              val_recency_accuracy[i, 2] <- 
                val_cust_summary_temp$pred_reorder[i]
              if (val_cust_summary_temp$pred_reorder[i] == 
                  val_cust_summary_temp$act_reorder[i]) {
                val_recency_accuracy[i, 3] <- 1
              } else if (val_cust_summary_temp$pred_reorder[i] > 
                         val_cust_summary_temp$act_reorder[i]) {
                val_recency_accuracy[i, 4] <- 1
              } else {
                val_recency_accuracy[i, 5] <- 1
              }
            }
#
            val_recency_cumulative <- 
              as.data.frame(matrix(0,
                                   nrow = 
                                     nrow(val_cust_summary_temp),
                                   ncol = 
                                     2 * length(will_reorder_centers)))
            cum_recency_colnames <- character()
            index <- 0
            for (i in seq(1, ncol(val_recency_cumulative), 2)) {
              index <- index + 1
              cum_recency_colnames[i] <-
                paste0("pred_reorder_by", will_reorder_by_cutoffs[index])
              cum_recency_colnames[i + 1] <-
                paste0("act_reorder_by", will_reorder_by_cutoffs[index])
            }
#
            colnames(val_recency_cumulative) <- cum_recency_colnames
#
            for (i in 1:nrow(val_cust_summary_temp)) {
              for (j in 1:length(will_reorder_centers)) {
                if (val_cust_summary_temp$pred_reorder[i] <= 
                     will_reorder_by_cutoffs[j + 1]) {
                  val_recency_cumulative[i, (j * 2 - 1)] <- 1
                }
              }
              for (j in 1:length(will_reorder_centers)) {
                if (val_cust_summary_temp$act_reorder[i] <= 
                     will_reorder_by_cutoffs[j + 1]) {
                  val_recency_cumulative[i, (j * 2)] <- 1
                }
              }
            }
#      
            val_cumulative_accuracy <- 
              as.data.frame(matrix(0,
                                   nrow = 
                                     length(will_reorder_centers),
                                   ncol = 10))
            colnames(val_cumulative_accuracy) <- 
              c("data", "Algorithm", "ID", "split", 
                "clusters", "date", "Precision", 
                "Recall", "Accuracy", "F1")
#
            for (j in seq(1, length(will_reorder_centers), 1)) {
              true_pos <- 
                nrow(val_recency_cumulative[
                  (val_recency_cumulative[, (j * 2 - 1)] ==
                     val_recency_cumulative[, (j * 2)]) &
                    (val_recency_cumulative[, (j * 2 - 1)] == 
                       1), ])
              false_pos <- 
                nrow(val_recency_cumulative[
                  (val_recency_cumulative[, (j * 2 - 1)] !=
                     val_recency_cumulative[, (j * 2)]) &
                    (val_recency_cumulative[, (j * 2 - 1)] == 
                       1), ])
              true_neg <-
                nrow(val_recency_cumulative[
                  (val_recency_cumulative[, (j * 2 - 1)] ==
                     val_recency_cumulative[, (j * 2)]) &
                    (val_recency_cumulative[, (j * 2 - 1)] == 
                       0), ])
              false_neg <-
                nrow(val_recency_cumulative[
                  (val_recency_cumulative[, (j * 2 - 1)] !=
                     val_recency_cumulative[, (j * 2)]) &
                    (val_recency_cumulative[, (j * 2 - 1)] == 
                       0), ])
              if (((true_pos + false_pos) > 0) & !is.na(true_pos)) {
                Precision <- 
                  true_pos / (true_pos + false_pos)
              } else {
                Precision <- 0
              }
              if (((true_pos + false_neg) > 0) & !is.na(true_pos)) {
                Recall <- 
                  true_pos / (true_pos + false_neg)
              } else {
                Recall <- 0
              }
              if ((true_pos + false_pos + true_neg + false_neg) > 0) {
                Accuracy <- 
                  (true_pos + true_neg) / 
                  (true_pos + false_pos + true_neg + false_neg)
              } else {
                Accuracy <- 0
              }
              if ((Precision + Recall) > 0) {
                F1 <- (2 * Precision * Recall) / 
                  (Precision + Recall)
              } else {
                F1 <- 0
              }
#
              val_cumulative_accuracy[j, 1] <- "validation"
              val_cumulative_accuracy[j, 2] <- 
                all_permutations[current_index]
              val_cumulative_accuracy[j, 3] <- 
                current_index
              val_cumulative_accuracy[j, 4] <- splits[split_values, 3]
              val_cumulative_accuracy[j, 5] <- clusters_used
              val_cumulative_accuracy[j, 6] <- will_reorder_centers[j]
              val_cumulative_accuracy[j, 7] <- Precision
              val_cumulative_accuracy[j, 8] <- Recall
              val_cumulative_accuracy[j, 9] <- Accuracy
              val_cumulative_accuracy[j, 10] <- F1
            } # end loop through date list to calculate perforamnce metrics
#
            cat("Summary for Validation Data\n")
            print(val_cumulative_accuracy)
            val_cumulative_table <- rbind(val_cumulative_table,
                                            val_cumulative_accuracy)     
#              
          } # end of loop through distance methods
        } # end of is use hclust block 
      } # end of number of clusters loop
    } # end of cluster method loop
  } # end of split value loop
#
  dev.off()
  write.csv(test_forecast, 
            paste0(time_stamp,
                   "test_forecast.csv"))
  write.csv(val_forecast, 
            paste0(time_stamp,
                   "val_forecast.csv"))
  write.csv(test_cumulative_table, 
              paste0(time_stamp,
                     "test_cumulative_table.csv"))
  write.table(paste("start date", as.Date(min_date, origin = "1970-01-01"),
                    sep = " "),
              paste0(time_stamp,
                     "test_cumulative_table.csv"),
              append = TRUE)
  write.table(paste("end date", 
                    as.Date(max_date, origin = "1970-01-01"),
                    sep = " "),
              paste0(time_stamp,
                     "test_cumulative_table.csv"),
              append = TRUE)
#
  write.csv(val_cumulative_table, 
            paste0(time_stamp,
                   "val_cumulative_table.csv"))
  write.table(paste("start date", as.Date(min_date, origin = "1970-01-01"),
                    sep = " "),
              paste0(time_stamp,
                     "val_cumulative_table.csv"),
              append = TRUE)
  write.table(paste("end date", 
                    as.Date(max_date, origin = "1970-01-01"),
                    sep = " "),
              paste0(time_stamp,
                     "val_cumulative_table.csv"),
              append = TRUE)
#
  