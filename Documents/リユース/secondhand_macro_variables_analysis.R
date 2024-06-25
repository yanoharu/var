library(here)
library(readxl)
library(dplyr)
library(zoo)
current_path <- getwd()
new_path <- here(current_path,"リユース")
setwd(new_path)
current_path <- getwd()
files_with_paths <- list.files(path = current_path, full.names = TRUE)
print(files_with_paths)

#########################################
rate_raw_data <- read_excel("exchange_rate.xlsx" )
rate_raw_data["年"] <- format(cpi_raw_data$`MMM YYYY`, "%Y")
tidy_rate_data <- rate_raw_data %>%
  dplyr::group_by(`年`) %>%
  summarise(ex_rate = mean(`JPY/USD`))%>%
  dplyr::mutate(年 = as.character.Date(`年`))
tidy_rate_data

########################################
sales_raw_data <- read_excel("tidy_sales.xlsx" )
new_car_data <- sales_raw_data %>%
  dplyr::filter(分類=="自動車（新車）小売業") %>%
  dplyr::mutate(年間商品販売額 = as.numeric(年間商品販売額)) %>%
  dplyr::filter(!is.na(年間商品販売額)) %>%
  dplyr::group_by(`年`) %>%
  summarise(年間商品販売額 = mean(`年間商品販売額`))

used_car_data <- sales_raw_data %>%
  dplyr::filter(分類=="中古自動車小売業") %>%
  dplyr::mutate(年間商品販売額 = as.numeric(年間商品販売額)) %>%
  dplyr::filter(!is.na(年間商品販売額)) %>%
  dplyr::group_by(`年`) %>%
  summarise(年間商品販売額 = mean(`年間商品販売額`))

car_data <- merge(used_car_data,new_car_data, by="年",
                  all=TRUE, suffixes = c("（中古）", "（新車）")) 
car_data <-  filter(car_data,!is.na(car_data$`年間商品販売額（中古）`))
car_data <-  filter(car_data,!is.na(car_data$`年間商品販売額（新車）`))
car_data_new <- data.frame(年 = seq(min(car_data$年), max(car_data$年), by = 1))
car_data_new <- merge(car_data_new,car_data, by="年",all=TRUE)
car_data_new$年[is.na(car_data_new["年間商品販売額（中古）"])]
tidy_car_data <- na.approx(car_data_new) %>% as.data.frame() %>%
  dplyr::mutate(年 = as.character.Date(年))
plot(tidy_car_data$`年間商品販売額（新車）`)
plot(tidy_car_data$`年間商品販売額（中古）`)

####################################################
sales_raw_data <- read_excel("tidy_sales.xlsx" )
new_appliance_data <- sales_raw_data %>%
  dplyr::filter(分類=="電気機械器具小売業") %>%
  dplyr::mutate(年間商品販売額 = as.numeric(年間商品販売額)) %>%
  dplyr::filter(!is.na(年間商品販売額)) %>%
  dplyr::group_by(`年`) %>%
  summarise(年間商品販売額 = mean(`年間商品販売額`))

used_appliance_data <- sales_raw_data %>%
  dplyr::filter(分類=="5933_中古電気製品小売業")
used_goods_data <- sales_raw_data %>%
  dplyr::filter(分類=="中古品小売業（骨とう品を除く）") %>%
  dplyr::mutate(年間商品販売額 = as.numeric(年間商品販売額)) %>%
  dplyr::mutate(年間商品販売額 = 年間商品販売額 + as.numeric(used_appliance_data$年間商品販売額))%>%
  dplyr::filter(!is.na(年間商品販売額)) %>%
  dplyr::group_by(`年`) %>%
  summarise(年間商品販売額 = mean(`年間商品販売額`))
plot(used_goods_data)
appliance_data <- merge(used_goods_data,new_appliance_data, by="年",
                  all=TRUE, suffixes = c("（中古）", "（新品）")) 
appliance_data <-  filter(appliance_data,!is.na(appliance_data$`年間商品販売額（中古）`))
appliance_data <-  filter(appliance_data,!is.na(appliance_data$`年間商品販売額（新品）`))
appliance_data_new <- data.frame(年 = seq(min(appliance_data$年), max(appliance_data$年), by = 1))
appliance_data_new <- merge(appliance_data_new,appliance_data, by="年",all=TRUE)
appliance_data_new$年[is.na(appliance_data_new["年間商品販売額（中古）"])]
tidy_appliance_data <- na.approx(appliance_data_new) %>% as.data.frame()%>%
  dplyr::mutate(年 = as.character.Date(年))
plot(tidy_appliance_data$`年間商品販売額（新品）`)
plot(tidy_appliance_data$`年間商品販売額（中古）`)

##########################################

cpi_data <- read_excel("tidy_cpi.xlsx" )
tidy_cpi_data <- cpi_data %>%
  dplyr::mutate(年 = as.character.Date(年))

######################################

ppi_raw_data <- read_csv("TimeSeriesResult_20240625221050909.csv") 

tidy_ppi_data <- ppi_raw_data %>%
  dplyr::filter(!is.na(`国内企業物価指数（総平均）2020年基準`)) %>%
  dplyr::select(時点, `国内企業物価指数（総平均）2020年基準`) %>%
  mutate(時点 = as.numeric(sub("年度", "", 時点))) %>%
  rename(
    年 = 時点,
    企業物価指数 = `国内企業物価指数（総平均）2020年基準`
  ) %>%
  dplyr::mutate(年 = as.character.Date(年))

##############car########################

master_car_data <- tidy_car_data %>%
  dplyr::left_join(tidy_cpi_data,by = "年") %>%
  dplyr::left_join(tidy_ppi_data,by = "年") %>%
  dplyr::left_join(tidy_rate_data, by = "年") %>%
  dplyr::mutate(trend = as.numeric(tidy_car_data$年) 
                - as.numeric(min(tidy_car_data$年)) + 1)

###########appliance####################

master_appliance_data <- tidy_appliance_data %>%
  dplyr::left_join(tidy_cpi_data,by = "年") %>%
  dplyr::left_join(tidy_ppi_data,by = "年") %>%
  dplyr::left_join(tidy_rate_data, by = "年")%>%
dplyr::left_join(tidy_rate_data, by = "年") %>%
  dplyr::mutate(trend = as.numeric(tidy_appliance_data$年) 
                - as.numeric(min(tidy_appliance_data$年)) + 1)
##########################################

library(vars)
master_appliance_data[c(2,4,6,7)]
info.var <- vars::VARselect(master_appliance_data[c(2,4,6,7)],
                            lag.max = 5,type="const")
info.var$selection
trend <- c(master_appliance_data$trend)
var.est1 <- VAR(master_appliance_data[c(2,4,6,7)], p = 2, type = "const",
                season = NULL,exog = master_appliance_data$trend)
summary(var.est1)
a.mat <- diag(4)
diag(a.mat) <- NA
a.mat[2, 1] <- NA
a.mat[3, 1] <- NA
a.mat[3, 2] <- NA
a.mat[4, 1] <- NA
a.mat[4, 2] <- NA
a.mat[4, 3] <- NA
print(a.mat)
b.mat <- diag(4)
diag(b.mat) <- NA
print(b.mat)
svar.one <- SVAR(var.est1, Amat = a.mat, Bmat = b.mat, max.iter = 10000, 
                 hessian = TRUE)
svar.one
one.gdp <- irf(svar.one, response = "家庭耐久財", impulse = "ex_rate", 
               n.ahead = 40, ortho = TRUE, boot = TRUE)
one.gdp <- irf(svar.one, response = "家庭耐久財", impulse = "ex_rate.x", 
               n.ahead = 40, ortho = TRUE, boot = TRUE)
one.gdp <- irf(svar.one, response = "家庭用耐久財", impulse = "ex_rate.x", 
               n.ahead = 40, ortho = TRUE, boot = TRUE)

par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(one.gdp)
