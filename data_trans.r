setwd('/Users/RafikaMomin/Desktop') 
library(readxl)
library(lubridate)

economic <- data.frame(t(read_excel("Intermodal Database May 2020.xlsx", sheet = 2, na = c("n/a"))), stringsAsFactors = F)

##Create Date Column
economic$date <- ymd(paste(economic$X1, economic$X2, "1"))

##Merge headers with their sub-variables
holder <- NA
for (i in 3:ncol(economic)){
  if (!is.na(economic[3, i])){
    economic[3, i] <- paste(holder, "-", economic[3, i])
  }
  
  else if (!is.na(economic[2, i])){
    holder <- economic[2, i]
    economic[3, i] <- economic[2, i]
  }
}

##Change Headers
names(economic) <- economic[3, ]
names(economic)[ncol(economic)] <- "Date"


##Remove top empty rows
economic <- economic[!is.na(economic[, 2]), ]

##Remove empty columns
economic <- economic[, !is.na(economic[nrow(economic), ])]

##Select our desired columns
reformatted_econ <- cbind(economic$Date, economic[, 3:(ncol(economic)-1)])
names(reformatted_econ)[1] <- "Date"

##Change character type columns to numeric type
reformatted_econ <- cbind(reformatted_econ[1]
                          ,sapply(reformatted_econ[2:ncol(reformatted_econ)], function(y) as.numeric(y)))

##Reset Indexing
rownames(reformatted_econ) <- 1:nrow(reformatted_econ)





###################################### Reformatting Sales By Department##################################
sales <- data.frame(read_excel("Labelmaster Sales By Dept.xlsx", sheet = 2, na = c("n/a")), stringsAsFactors = F)

##Selecting the right table in the worksheet
sales <- sales[, (which(is.na(sales[1,]))+1):ncol(sales)]
names(sales) <- sales[1, ]
sales <- sales[-1, ]
sales <- sales[!is.na(sales$TheYear), ]

##Create Data Column
sales$date <- ymd(paste(sales$TheYear, sales$TheMonth, "1"))

##Changing table format from long to wide
library(tidyr)
sales <- sales[, 3:ncol(sales)]
sales <- spread(sales, DeptDesc, DeptSales)

##Change character type columns to numeric type
sales <- cbind(sales[1], sapply(sales[2:ncol(sales)], function(y) as.numeric(y)))




####################Reformatting Truck and Trailer#####################################################
truck <- data.frame(t(read_excel("Truck & Trailer Database June 2020.xlsx", sheet = 3, na = c(""))), stringsAsFactors = F)

##Create Date Column
truck$date <- ymd(paste(truck$X1, truck$X2, "1"))

##Merge headers with their sub-variables
holder <- NA
for (i in 3:ncol(truck)){
  if (!is.na(truck[3, i])){
    truck[3, i] <- paste(holder, "-", truck[3, i])
  }
  
  else if (!is.na(truck[2, i])){
    holder <- truck[2, i]
    truck[3, i] <- truck[2, i]
  }
}

##Change Headers
names(truck) <- truck[3, ]
names(truck)[ncol(truck)] <- "Date"


##Remove top empty rows
truck <- truck[!is.na(truck[, 2]), ]

##Remove empty columns
truck <- truck[, !is.na(truck[nrow(truck), ])]

##Select our desired columns
reformatted_truck <- cbind(truck$Date, truck[, 3:(ncol(truck)-1)])
names(reformatted_truck)[1] <- "Date"

##Change character type columns to numeric type
reformatted_truck <- cbind(reformatted_truck[1]
                          ,sapply(reformatted_truck[2:ncol(reformatted_truck)], function(y) as.numeric(y)))

##Reset Indexing
rownames(reformatted_truck) <- 1:nrow(reformatted_truck)


##Merge Economic and Truck Datasets
exogenous <- merge(reformatted_econ, reformatted_truck, by = "Date")


#########################Cross Correlation Between Exogenous Variables and Sales By Department##########

##Find where the NAs stop for each column in exogenous and sales
nonNA_exo_start <- lapply(exogenous[2:ncol(exogenous)], function(y) min(which(!is.na(y))))
nonNA_exo_end <- lapply(exogenous[2:ncol(exogenous)], function(y) max(which(!is.na(y))))
nonNA_sales_start <- lapply(sales[2:ncol(sales)], function(y) min(which(!is.na(y))))
nonNA_sales_end <- lapply(sales[2:ncol(sales)], function(y) max(which(!is.na(y))))

##Checking for stationarity using Dickey-Fuller Test
library(tseries)

##Transform exogenous into stationary data if needed
stat_exo <- exogenous
for (i in 2:ncol(exogenous)) {
  j = i - 1
  if (adf.test(exogenous[nonNA_exo_start[[j]]:nonNA_exo_end[[j]], i])$p.value > 0.05){
    stat_exo[nonNA_exo_start[[j]], i] <- NA
    stat_exo[(nonNA_exo_start[[j]] + 1): nonNA_exo_end[[j]], i] <- 
      diff(exogenous[nonNA_exo_start[[j]]:nonNA_exo_end[[j]], i])
  }
}

##Transform sales into stationary data if needed
stat_sales <- sales
for (i in 2:ncol(sales)) {
  j = i - 1
  if (adf.test(sales[nonNA_sales_start[[j]]:nonNA_sales_end[[j]], i])$p.value > 0.05){
    stat_sales[nonNA_sales_start[[j]], i] <- NA
    stat_sales[(nonNA_sales_start[[j]] + 1): nonNA_sales_end[[j]], i] <- 
      diff(sales[nonNA_sales_start[[j]]:nonNA_sales_end[[j]], i])
  }
}

##Calculate new start and end times for sales and exogenous
exo_start <- lapply(stat_exo[2:ncol(stat_exo)], function(y) min(which(!is.na(y))))
exo_end <- lapply(stat_exo[2:ncol(stat_exo)], function(y) max(which(!is.na(y))))
sales_start <- lapply(stat_sales[2:ncol(stat_sales)], function(y) min(which(!is.na(y))))
sales_end <- lapply(stat_sales[2:ncol(stat_sales)], function(y) max(which(!is.na(y))))

##Run cross correlation between exogenous and sales
cross <- NA
cross_vals <- NA
for (i in 2:ncol(stat_sales)){
  j = i - 1
  start_date <- stat_sales$date[sales_start[[j]]]
  end_date <- stat_sales$date[sales_end[[j]]]
  for (k in 2:ncol(stat_exo)){
    l = k - 1
    if(start_date < stat_exo$Date[exo_start[[l]]]){
      start_date <- stat_exo$Date[exo_start[[l]]]
    }
    if(end_date > stat_exo$Date[exo_end[[l]]]){
      end_date <- stat_exo$Date[exo_end[[l]]]
    }
    temp <- ccf(stat_exo[which(stat_exo$Date == start_date):which(stat_exo$Date == end_date), k],
        stat_sales[which(stat_sales$date == start_date):which(stat_sales$date == end_date), i], 12)
    plot(temp, main = paste("Lagged", names(stat_exo)[k], "And", names(stat_sales)[i]))
    cross <- cbind(cross, temp)
    sig_value <- which(abs(temp$acf) ==
                         max(temp$acf[which(abs(temp$acf[1:ceiling(length(temp$acf)/2)]) >= 0.4)]))
    if (length(sig_value) > 0){
      cross_vals <- rbind(cross_vals,c(names(stat_sales)[i], names(stat_exo)[k],
                            temp$acf[sig_value], temp$lag[sig_value]))
    }
  }
}
cross_vals
cross <- cross[, -1]
names(cross) <- c("X-Variable", "Dept", "Correlation", "Lag")
