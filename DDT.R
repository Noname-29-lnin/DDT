# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("lubridate")
# install.packages("geosphere")
# install.packages("readr")
# install.packages("corrplot")
# install.packages("faraway")
# install.packages("car")
# install.packages("scatterplot3d")
# install.packages("rgl")
# install.packages("read")
# library(readr)
library(rgl)
library(scatterplot3d)
library(car) 
library(faraway)
library(dplyr)
library(ggplot2)
library(geosphere)
library(readr)
library(lubridate)
library(corrplot)
# install.packages("naniar")
library(naniar)
dirty_data_path <- "E:/projectmain/rstudio/warehoue_DDT/archive/dirty_data.csv"
missing_data_path <- "E:/projectmain/rstudio/warehoue_DDT/archive/missing_data.csv"
warehouses_path <- "E:/projectmain/rstudio/warehoue_DDT/archive/warehouses.csv"

dirty_data <- read.csv(dirty_data_path)
missing_data <- read.csv(missing_data_path)
warehouse_data <- read.csv(warehouses_path)

new_dirty_data <- rbind(dirty_data, missing_data)

mean_customer_lat <- mean(new_dirty_data$customer_lat, na.rm = TRUE)

mean_customer_long <- mean(new_dirty_data$customer_long, na.rm = TRUE)

new_dirty_data$is_happy_customer <- as.logical(new_dirty_data$is_happy_customer)
median_is_happy_customer <- median(new_dirty_data$is_happy_customer, na.rm = TRUE)

rounded_mean_customer_lat <- round(mean_customer_lat, digits = 4)
rounded_mean_customer_long <- round(mean_customer_long, digits = 4)

new_dirty_data$customer_lat[is.na(new_dirty_data$customer_lat)] <- rounded_mean_customer_lat
new_dirty_data$customer_long[is.na(new_dirty_data$customer_long)] <- rounded_mean_customer_long
new_dirty_data$is_happy_customer[is.na(new_dirty_data$is_happy_customer)] <- median_is_happy_customer
new_dirty_data$coupon_discount = as.numeric(new_dirty_data$coupon_discount)
new_dirty_data <- new_dirty_data %>%
  mutate(order_total = ifelse(is.na(order_total), order_price * (100 - coupon_discount) / 100 + delivery_charges, order_total),
         order_price = ifelse(is.na(order_price), (order_total - delivery_charges) * 100 / (100 - coupon_discount), order_price),
         delivery_charges = ifelse(is.na(delivery_charges), order_total - order_price * (100 - coupon_discount) / 100, delivery_charges),
         coupon_discount = ifelse(is.na(coupon_discount), 100 - (order_total - delivery_charges) * 100 / order_price, coupon_discount)
  )
new_dirty_data$is_expedited_delivery <- as.logical(new_dirty_data$is_expedited_delivery)
new_dirty_data$is_happy_customer <- as.logical(new_dirty_data$is_happy_customer)
new_dirty_data$date <- parse_date_time(new_dirty_data$date, orders = c("ymd", "mdy", "dmy"))
new_dirty_data$date <- as.Date(new_dirty_data$date)

distances_to_nearest_warehouse <- numeric(length = nrow(new_dirty_data))

for (i in 1:nrow(new_dirty_data)) {
  customer_location <- c(new_dirty_data$customer_long[i], new_dirty_data$customer_lat[i])
  min_distance <- Inf
  
  for (j in 1:nrow(warehouse_data)) {
    warehouse_location <- c(warehouse_data$lon[j], warehouse_data$lat[j])
    distance <- distVincentySphere(warehouse_location, customer_location)
    
    if (distance < min_distance) {
      min_distance <- distance
    }
  }
  
  distances_to_nearest_warehouse[i] <- min_distance
}

new_dirty_data$distance_to_nearest_warehouse <- round(distances_to_nearest_warehouse/1000, digits = 4)

new_dirty_data$nearest_warehouse <- tolower(new_dirty_data$nearest_warehouse)
new_dirty_data$nearest_warehouse <- tools::toTitleCase(new_dirty_data$nearest_warehouse)
new_dirty_data$season <- tolower(new_dirty_data$season)
new_dirty_data$season <- tools::toTitleCase(new_dirty_data$season)
new_dirty_data <- new_dirty_data %>%
  mutate(season = case_when(
    month(date) %in% c(3, 4, 5) ~ "Spring",
    month(date) %in% c(6, 7, 8) ~ "Summer",
    month(date) %in% c(9, 10, 11) ~ "Autumn",
    TRUE ~ "Winter"
  ))
nearest_warehouses <- character(length = nrow(new_dirty_data))

for (i in 1:nrow(new_dirty_data)) {
  customer_location <- c(new_dirty_data$customer_long[i], new_dirty_data$customer_lat[i])
  min_distance <- Inf
  nearest_warehouse <- ""
  for (j in 1:nrow(warehouse_data)) {
    warehouse_location <- c(warehouse_data$lon[j], warehouse_data$lat[j])
    distance <- distVincentySphere(warehouse_location, customer_location)
    
    if (distance < min_distance) {
      min_distance <- distance
      nearest_warehouse <- warehouse_data$names[j]
    }
  }
  
  nearest_warehouses[i]<- nearest_warehouse
}

new_dirty_data$nearest_warehouse <- nearest_warehouses

new_data <- new_dirty_data

d1=quantile(new_data$order_price) 
d2=quantile(new_data$delivery_charges) 
d3=quantile(new_data$coupon_discount) 
d4=quantile(new_data$order_total) 
d5=quantile(new_data$distance_to_nearest_warehouse) 
# IQR value 
IQR_1=d1[c(4)]-d1[c(2)] 
IQR_2=d2[c(4)]-d2[c(2)] 
IQR_3=d3[c(4)]-d3[c(2)] 
IQR_4=d4[c(4)]-d4[c(2)] 
IQR_5=d5[c(4)]-d5[c(2)]
lower_quantile <- function(Q1, IQR) {
  return(Q1 - 1.5 * IQR)
}

upper_quantile <- function(Q3, IQR) {
  return(Q3 + 1.5 * IQR)
}

# Calculating lower and upper
lower_order_price <- lower_quantile(d1[2], IQR_1)
upper_order_price <- upper_quantile(d1[4], IQR_1)

lower_delivery_charges <- lower_quantile(d2[2], IQR_2)
upper_delivery_charges <- upper_quantile(d2[4], IQR_2)

lower_coupon_discount <- lower_quantile(d3[2], IQR_3)
upper_coupon_discount <- upper_quantile(d3[4], IQR_3)

lower_order_total <- lower_quantile(d4[2], IQR_4)
upper_order_total <- upper_quantile(d4[4], IQR_4)

lower_distance_to_nearest_warehouse <- lower_quantile(d5[2], IQR_5)
upper_distance_to_nearest_warehouse <- upper_quantile(d5[4], IQR_5)

# order_price
for (i in 1:length(new_data$order_price)) {
  if (new_data$order_price[i] > upper_order_price) {
    new_data$order_price[i] = upper_order_price
  } else if (new_data$order_price[i] < lower_order_price) {
    new_data$order_price[i] = lower_order_price
  }
}
# delivery_charges
for (i in 1:length(new_data$delivery_charges)) {
  if (new_data$delivery_charges[i] > upper_delivery_charges) {
    new_data$delivery_charges[i] = upper_delivery_charges
  } else if (new_data$delivery_charges[i] < lower_delivery_charges) {
    new_data$delivery_charges[i] = lower_delivery_charges
  }
}
# coupon_discount
for (i in 1:length(new_data$coupon_discount)) {
  if (new_data$coupon_discount[i] > upper_coupon_discount) {
    new_data$coupon_discount[i] = upper_coupon_discount
  } else if (new_data$coupon_discount[i] < lower_coupon_discount) {
    new_data$coupon_discount[i] = lower_coupon_discount
  }
}
# order_total
for (i in 1:length(new_data$order_total)) {
  if (new_data$order_total[i] > upper_order_total) {
    new_data$order_total[i] = upper_order_total
  } else if (new_data$order_total[i] < lower_order_total) {
    new_data$order_total[i] = lower_order_total
  }
}
# distance_to_nearest_warehouse
for (i in 1:length(new_data$distance_to_nearest_warehouse)) {
  if (new_data$distance_to_nearest_warehouse[i] > upper_distance_to_nearest_warehouse) {
    new_data$distance_to_nearest_warehouse[i] = upper_distance_to_nearest_warehouse
  } else if (new_data$distance_to_nearest_warehouse[i] < lower_distance_to_nearest_warehouse) {
    new_data$distance_to_nearest_warehouse[i] = lower_distance_to_nearest_warehouse
  }
}

# numeric_columns <- new_data[c("order_price", "delivery_charges", "customer_long", "customer_lat", "coupon_discount", "order_total", "is_expedited_delivery", "is_happy_customer")]
# correlation_matrix <- cor(numeric_columns)
# corrplot(
#   correlation_matrix,
#   method = "color",   # Phương pháp vẽ màu sắc
#   type = "full",      # Vẽ cả hai tam giác
#   order = "hclust",   # Sắp xếp theo cụm
#   tl.col = "black",   # Màu chữ
#   tl.srt = 45,        # Góc quay chữ
#   addCoef.col = "black",  # Màu chữ số tương quan
#   number.cex = 0.7,   # Kích thước chữ số
#   tl.cex = 0.8,       # Kích thước chữ
#   diag = TRUE        # Không hiển thị các hình vuông trên đường chéo chính
# )


# Phân tích anova
# kiểm tra phân phối chuẩn
anova_data <- new_data
Thompson_data <- subset(anova_data, nearest_warehouse == "Thompson")
Nickolson_data <- subset(anova_data, nearest_warehouse == "Nickolson")
Bakers_data <- subset(anova_data, nearest_warehouse == "Bakers")
qqnorm(Thompson_data$order_total)
qqline(Thompson_data$order_total)
shapiro.test(Thompson_data$order_total)
# ANOVA một yếu tố
# ANOVA một mẫu
anova1 <- aov(data = anova_data, order_total ~ nearest_warehouse)
summary(anova1)
# phân tích sâu anova 1 yếu tố
tukeyresult <- TukeyHSD(anova1)
par(cex.axis = 0.45)
plot(tukeyresult, las = 1, col = "blue")

# ANOVA 2 mẫu
# Summer_data <- subset(anova_data, season == "Summer")
anova12 <- aov(order_total ~ nearest_warehouse, data = rbind(Thompson_data, Nickolson_data))

# ANOVA 2 yếu tố
anova2 <- aov(data = anova_data, order_total ~ season * nearest_warehouse)

# Hồi quy tuyến tính
# Hồi quy tuyến tính đơn
lm_data <- new_data
lm1 <- lm(data = lm_data, order_total ~ order_price)
summary(lm1)
# Giả sử new_data là dữ liệu mới có các giá trị order_price bạn muốn dự đoán
# new_data1 <- data.frame(order_price = c(15, 20, 25, 30))
# 
# # Dự đoán giá trị phản ứng
# predicted_values <- predict(lm1, newdata = new_data1)
# 
# # Hiển thị các giá trị dự đoán
# print(predicted_values)

# Hồi quy tuyến tính bội
# lm2 <- lm(data = lm_data, order_total ~ order_price + delivery_charges + coupon_discount + is_happy_customer + is_expedited_delivery)
# step_model <- step(lm2)
# summary(step_model)
# # dự đoán
# new_data2 <- data.frame(order_price = c(15000, 20000), coupon_discount = c(10, 15))
# predicted_values2 <- predict(step_model, newdata = new_data2)
# print(predicted_values2)
# Hồi quy logitics
glm1 <- glm(data = lm_data, is_happy_customer ~ order_price + delivery_charges + coupon_discount + order_total + is_expedited_delivery, family = binomial)
step_glm <- step(glm1)
print(step_glm)
summary(step_glm)

data_glm <- data.frame(delivery_charges = c(50), coupon_discount = c(10), is_expedited_delivery = c(TRUE))
predict_glm <- predict(step_glm, newdata = data_glm, type = "response")
print(predict_glm)
2