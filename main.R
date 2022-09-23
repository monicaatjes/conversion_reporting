#### main ###

### open libaries ###
library(haven)
library(tidyr)
library(tidyverse)
library(magrittr)
library(dplyr)
library(ggplot2)
library(psych)
library(GPArotation)
library(lm.beta)
library(mctest)
library(plotly)
library(dplyr)
library(stringr)
library(rmarkdown)
#ibrary(ingmarkdowntemplates)
library(ggplot2)
library(ggflags)
library(lubridate)
library(png)
library(ggimage)
library(readxl)
library(tidyxl)
library(data.table)
library(zoo)
library(shiny)
library(corrr)
library(tidyverse)
library(caret)
library(car)
library(gt)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
#library(shiny)
#library(shinydashboard)
library(xfun)
library(bslib)
#library(stdlib.h)


source("functions/funnel_plot_format_function.R")
source("functions/time_plot_format_function.R")
source("functions/post_stamp_function.R")
source("functions/post_stamp_function_option2.R")
source("functions/tab_function.R")

### Render HTML ###

rmarkdown::render("/Users/xo21bm/Documents/conversion_reporting/draft_Q22_time.Rmd", output_file = "draft_Q2_time.html")
rmarkdown::render("/Users/xo21bm/Documents/conversion_reporting/draft_Q2_Italy.Rmd", output_file = "draft_Q1_IT.html")

rmarkdown::render("/Users/xo21bm/Documents/conversion_reporting/inst/rmarkdown/templates/QPC_template/draft_Q2_time.Rmd", output_file = "draft_Q2_time.html")

### Open data ###

IT <- read_excel("~/Documents/conversion_reporting/data/CRO_data_request_Q22022.xlsx", sheet="As_raw_as_possible")

DE_1 <- read_excel("~/Documents/conversion_reporting/data/CONV_INT_2021_Q2_Q3.xlsx", sheet="CONV_INT")
DE_2 <- read_excel("~/Documents/conversion_reporting/data/CONV_INT_2021_Q4.xlsx", sheet="CONV_INT_Q4")
DE_3 <- read_excel("~/Documents/conversion_reporting/data/Conv_int_2022_Q1.xlsx", sheet="CONV_INT_AGG")
DE_4 <- read_excel("~/Documents/conversion_reporting/data/Conv_int_2022_Q2.xlsx", sheet="CONV_INT_AGG")
DE <- dplyr::full_join(DE_1, DE_2)

# File with start form + goal comepletion from Google analytics
# this shows just the open environment, people can go as far as submitting a form
#RO <- read_excel("~/Documents/reporting_conversion/data/Final QPC_data Q3 2021_Q1 2022 RO with device split.xlsx", sheet= "FINAL Q3 21-Q1 22 Mobile Split")
#RO_1 <-read_excel("~/Documents/reporting_conversion/data/Test Data variant 1 Q2-Q3.xlsx", sheet ="Sheet1")
#RO3_1 <- read_excel("~/Documents/reporting_conversion/data/Conversion leads-sales Q3-4.xlsx", sheet ="Sales_w_Leads_Flag")

# file with leads: including n2b & customer, from GA
#RO3_2 <- read_excel("~/Documents/reporting_conversion/data/Conversion leads-sales Restatement Q2 - Q4 2021.xlsx", sheet ="Sales_w_Leads_Flag")
#RO3_3 <- read_excel("~/Documents/reporting_conversion/data/Conversion leads-sales Q1 2022.xlsx", sheet ="Sales_w_Leads_Flag")

#RO3 <- dplyr::full_join(RO3_3, RO3_2)

# file with category page, no split n2b & customer. Home bank, the secured environment. 
RO_4 <- read_excel("~/Documents/conversion_reporting/data/Adobe Analytics data - HomeBank_07152022.xlsx", sheet ="Products and Services")
RO_5 <- read_excel("~/Documents/conversion_reporting/data/Adobe Analytics data - HomeBank_07152022.xlsx", sheet ="Savings account")
RO_6 <- read_excel("~/Documents/conversion_reporting/data/Adobe Analytics data - HomeBank_07152022.xlsx", sheet ="Current account")

#Q3
#PL1 <- read_excel("~/Documents/reporting_conversion/data/Kopia Draft_CRO_data_request_feedback_included (002).xlsx", sheet ="As_raw_as_possible")
PL_IP3 <- read_excel("~/Documents/conversion_reporting/data/cnv_visits_sum_inv_conv20213xlsx.xlsx", sheet ="visits")

#PL_try <- read_excel("~/Documents/reporting_conversion/data/cnv_visits_sum_inv_conv20214.xlsx", sheet ="visits")

#PL <- bind_rows(PL1, PL_IP)

#Q4
PL <- read_excel("~/Documents/conversion_reporting/data/Kopia Draft_CRO_data_request_feedback_included (002).xlsx", sheet ="As_raw_as_possible")
PL_IP <- read_excel("~/Documents/conversion_reporting/data/cnv_visits_sum_inv_conv20214.xlsx", sheet ="visits")
PL_UL <- read_excel("~/Documents/conversion_reporting/data/cnv_visits_sum_unscrd_lending20214.xlsx", sheet ="visits")
PL_SA <- read_excel("~/Documents/conversion_reporting/data/cnv_visits_sum_sav_conv20214.xlsx", sheet ="visits")
PL_CA <- read_excel("~/Documents/conversion_reporting/data/cnv_visits_sum_ca_conv20214.xlsx", sheet ="visits")
PL_IN <- read_excel("~/Documents/conversion_reporting/data/cnv_visits_sum_ins_conv20214.xlsx", sheet ="visits")

#Q1
PL_IP_1 <- read_excel("~/Documents/conversion_reporting/data/cnv_visits_sum_inv_conv20221.xlsx", sheet ="visits")
PL_UL_1 <- read_excel("~/Documents/conversion_reporting/data/cnv_visits_sum_unscrd_lending20221.xlsx", sheet ="visits")
PL_SA_1 <- read_excel("~/Documents/conversion_reporting/data/cnv_visits_sum_sav_conv20221.xlsx", sheet ="visits")
PL_CA_1 <- read_excel("~/Documents/conversion_reporting/data/cnv_visits_sum_ca_conv20221.xlsx", sheet ="visits")
PL_IN_1 <- read_excel("~/Documents/conversion_reporting/data/cnv_visits_sum_ins_conv20221.xlsx", sheet ="visits")

#Q2
PL_IP_2 <- read_excel("~/Documents/conversion_reporting/data/cnv_visits_sum_inv_conv20222.xlsx", sheet ="visits")
PL_UL_2 <- read_excel("~/Documents/conversion_reporting/data/cnv_visits_sum_unscrd_lending20222.xlsx", sheet ="visits")
PL_SA_2 <- read_excel("~/Documents/conversion_reporting/data/cnv_visits_sum_sav_conv20222.xlsx", sheet ="visits")
PL_CA_2 <- read_excel("~/Documents/conversion_reporting/data/cnv_visits_sum_ca_conv20222.xlsx", sheet ="visits")
PL_IN_2 <- read_excel("~/Documents/conversion_reporting/data/cnv_visits_sum_ins_conv20222.xlsx", sheet ="visits")

PL_IP <- dplyr::full_join(PL_IP, PL_IP_1)
PL_UL <- dplyr::full_join(PL_UL, PL_UL_1)
PL_SA <- dplyr::full_join(PL_SA, PL_SA_1)
PL_IN <- dplyr::full_join(PL_IN, PL_IN_1)
PL_CA <- dplyr::full_join(PL_CA, PL_CA_1)

PL_IP <- dplyr::full_join(PL_IP, PL_IP_2)
PL_UL <- dplyr::full_join(PL_UL, PL_UL_2)
PL_SA <- dplyr::full_join(PL_SA, PL_SA_2)
PL_IN <- dplyr::full_join(PL_IN, PL_IN_2)
PL_CA <- dplyr::full_join(PL_CA, PL_CA_2)

PL <- dplyr::full_join(PL_IP, PL_UL)
PL <- dplyr::full_join(PL, PL_SA)
PL <- dplyr::full_join(PL, PL_CA)
PL <- dplyr::full_join(PL, PL_IN)
PL <- dplyr::full_join(PL, PL_IP3)

SP <- read_excel("~/Documents/conversion_reporting/data/Draft_CRO_data_request_feedback_included_VF.xlsx", sheet ="As_raw_as_possible")
BE_1 <- read_excel("~/Documents/conversion_reporting/data/CRO BE data.xlsx", sheet ="BE CRO data")
BE_1$`Month of Year` <- BE_1$`Month Year`
BE_1$`Month Year` <- NULL
BE_1$`Device Type` <- BE_1$`Device type`
BE_1$`Device type` <- NULL

BE_2 <- read_excel("~/Documents/conversion_reporting/data/CRO data BE Q4 21.xlsx")
# plus data Q1
BE_3 <- read_excel("~/Documents/conversion_reporting/data/CRO Data BE Q1 2022.xlsx")

BE_4 <- read_excel("~/Documents/conversion_reporting/data/CRO Data BE Q2 22.xlsx")

BE <- dplyr::full_join(BE_1, BE_2)
BE <- dplyr::full_join(BE, BE_3)
BE <- dplyr::full_join(BE, BE_4)

### NL file

NL <- read_excel("~/Documents/conversion_reporting/data/NL_test.xlsx")


###
AUS <- read_excel("~/Documents/conversion_reporting/data/Australia Draft Funnel Reporting - V1 04.11.21.xlsx", sheet ="Page Reference Data")

AUS1 <- read_excel("~/Documents/conversion_reporting/data/Australia Draft Funnel Reporting - V1 04.11.21.xlsx", sheet ="Online - Desktop")
AUS2 <- read_excel("~/Documents/conversion_reporting/data/Australia Draft Funnel Reporting - V1 04.11.21.xlsx", sheet ="Online - All Devices")
AUS3 <- read_excel("~/Documents/conversion_reporting/data/Australia Draft Funnel Reporting - V1 04.11.21.xlsx", sheet ="Mobile App")

AUS4 <- read_csv("~/Documents/conversion_reporting/data/Aus_Report_desktop_Q4.csv")
AUS5 <- read_csv("~/Documents/conversion_reporting/data/Aus_Report_app_Q4.csv")
AUS6 <- read_csv("~/Documents/conversion_reporting/data/Aus_Report_mobile_Q4.csv")

AUS7 <- read_excel("~/Documents/conversion_reporting/data/Report_desktop.xlsx", sheet ="Report_desktop")
AUS7$Quarter <- "1rst Quarter 2022"
AUS8 <- read_excel("~/Documents/conversion_reporting/data/Report_mobileapp.xlsx", sheet ="Report_mobileapp")
AUS8$Quarter <- "1rst Quarter 2022"
AUS9 <- read_excel("~/Documents/conversion_reporting/data/Report_mobileweb.xlsx", sheet ="Report_mobileweb")
AUS9$Quarter <- "1rst Quarter 2022"

AUS10 <- read_excel("~/Documents/conversion_reporting/data/aus_desktop.xlsx")
AUS10$Quarter <- "2nd Quarter 2022"
AUS11 <- read_excel("~/Documents/conversion_reporting/data/aus_mobile_app.xlsx")
AUS11$Quarter <- "2nd Quarter 2022"
AUS12 <- read_excel("~/Documents/conversion_reporting/data/aus_mobile_web.xlsx")
AUS12$Quarter <- "2nd Quarter 2022"

# first step is connect data per device with each other
# Desktop
AUS1 <- dplyr::full_join(AUS1, AUS4)
AUS1 <- dplyr::full_join(AUS1, AUS7)
AUS1 <- dplyr::full_join(AUS1, AUS10)
# Mobile web
AUS2 <- dplyr::full_join(AUS2, AUS6)
AUS2 <- dplyr::full_join(AUS2, AUS9)
AUS2 <- dplyr::full_join(AUS2, AUS12)
# Mobile app
AUS3 <- dplyr::full_join(AUS3, AUS5)
AUS3 <- dplyr::full_join(AUS3, AUS8)
AUS3 <- dplyr::full_join(AUS3, AUS11)

# AUS4 is reference table!
AUS4 <- AUS %>%
  dplyr::filter(!is.na(Channel)) %>%
  dplyr::rename(
  "Pages" = `Page Name`
  )

# quarters are already combined per device. Next step is to add the 3 devices together
AUS1 <- AUS1
AUS1$device <- "Desktop"
AUS2$device <- "Mobile web"
AUS3$device <- "Mobile app"

AUS_try <- dplyr::full_join(AUS1, AUS2, by=c("Quarter", "Pages", "Visits", "device"))
AUS_try <- dplyr::full_join(AUS_try, AUS3, by=c("Quarter", "Pages", "Visits", "device"))

# as soon as devices are bundled in one file, add the referencesheet
Aussie <- dplyr::left_join(AUS4,AUS_try, by=c("Pages"))

Aussie1 <- Aussie %>%
  dplyr::select(-Channel) %>%
  dplyr::mutate(
    year =substr(Quarter,(nchar(Quarter)+1)-4,nchar(Quarter)),
    qnr = substr(Quarter, 1, 1)
  ) %>%
  dplyr::mutate(
    quarter = paste(year, qnr, sep= " Q")) %>%
  dplyr::mutate(
    quarter = as.yearqtr(quarter)
  ) %>%
  dplyr::select(-Quarter, -year, -qnr) %>%
  dplyr::mutate(
    Category = case_when(
      str_detect(`Product Name`, "Current|Orange Everyday|Account") ~ "Current accounts",
      str_detect(`Product Name`, "Invest|IP|Mutual|Pension|Focus|Fund|Self|Superannuation") ~ "Investment products",
      str_detect(`Product Name`, "Savings|Save|Savings Maximiser|Deposits") ~ "Savings",
      str_detect(`Product Name`, "Home Loans") ~ "Mortgages",
      str_detect(`Product Name`, "Credit Card|Personal Loan") ~ "Unsecured lending",
      str_detect(`Product Name`, "insurance|Insurance|Home and Contents|Car") ~ "Insurances",
      str_detect(`Product Name`, "other") ~ "Other"
    )
  ) %>%
  dplyr::mutate(Page = case_when(
    str_detect(`Funnel Step`, "Category") ~ "Category page",
    str_detect(`Funnel Step`, "Product") ~ "Product page",
    str_detect(`Funnel Step`, "Opening|Start") ~ "Start application",
    str_detect(`Funnel Step`, "Complete") ~ "Finish application"
  )) %>%
  dplyr::select(-`Funnel Step`, -`Product Category`) %>%
  dplyr::rename(
    "NewvsCurrent" = `New to Bank or Existing`,
    "Product" = `Product Name`
  ) %>%
  dplyr::mutate(
    NewvsCurrent = case_when(
      NewvsCurrent =="EXT" ~ "Customer",
      NewvsCurrent =="N2B" ~ "N2B",
      TRUE ~ "N2B"
    )
  ) %>%
  dplyr::group_by(quarter, Category, Product) %>%
  dplyr::summarise(
    total_traffic= sum(Visits, na.rm = T)
  ) %>%
  dplyr::mutate(
    rank_order = rank(desc(total_traffic), ties.method = "first")
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(quarter))

Aussie2 <- Aussie %>%
  dplyr::select(-Channel) %>%
  dplyr::mutate(
    year =substr(Quarter,(nchar(Quarter)+1)-4,nchar(Quarter)),
    qnr = substr(Quarter, 1, 1)
  ) %>%
  dplyr::mutate(
    quarter = paste(year, qnr, sep= " Q")) %>%
  dplyr::mutate(
    quarter = as.yearqtr(quarter)
  ) %>%
  dplyr::select(-Quarter, -year, -qnr) %>%
  dplyr::mutate(
    Category = case_when(
      str_detect(`Product Name`, "Current|Orange Everyday|Account") ~ "Current accounts",
      str_detect(`Product Name`, "Invest|IP|Mutual|Pension|Focus|Fund|Self|Superannuation") ~ "Investment products",
      str_detect(`Product Name`, "Savings|Save|Savings Maximiser|Deposits") ~ "Savings",
      str_detect(`Product Name`, "Home Loans") ~ "Mortgages",
      str_detect(`Product Name`, "Credit Card|Personal Loan") ~ "Unsecured lending",
      str_detect(`Product Name`, "insurance|Insurance|Home and Contents|Car") ~ "Insurances",
      str_detect(`Product Name`, "other") ~ "Other"
    )
  ) %>%
  dplyr::mutate(Page = case_when(
    str_detect(`Funnel Step`, "Category") ~ "Category page",
    str_detect(`Funnel Step`, "Product") ~ "Product page",
    str_detect(`Funnel Step`, "Opening|Start") ~ "Start application",
    str_detect(`Funnel Step`, "Complete") ~ "Finish application"
  )) %>%
  dplyr::select(-`Funnel Step`, -`Product Category`) %>%
  dplyr::rename(
    "NewvsCurrent" = `New to Bank or Existing`,
    "Product" = `Product Name`,
    "Device" = `device`
  ) %>%
  dplyr::mutate(
    NewvsCurrent = case_when(
      NewvsCurrent =="EXT" ~ "Customer",
      NewvsCurrent =="N2B" ~ "N2B",
      TRUE ~ "N2B"
    )
  ) %>%
  dplyr::mutate(
    Device = case_when(
      Device =="Mobile app" ~ "Mobile",
      Device =="Desktop" ~ "Desktop",
      TRUE ~ "Mobile"
    )
  ) %>%
  dplyr::filter(!is.na(quarter)) %>%
  dplyr::group_by(quarter, Category, Product, Page, Device, NewvsCurrent) %>%
  dplyr::summarise(
    Visits = sum(Visits)
  ) %>% 
  dplyr::ungroup() 
  
AUS2 <- dplyr::left_join(Aussie1, Aussie2, by=c("quarter", "Category", "Product")) %>%
  tidyr::spread(Page, Visits) %>%
  dplyr::mutate(
    con_1 = round((`Start application` /`Product page`), digits=2),
    con_2 = round((`Finish application` / `Start application`), digits=2),
    country = "Australia"
  ) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0))  

BE_try <- BE %>%
  dplyr::mutate(
    `Month of Year` = case_when(
      `Month of Year`=="October" ~ "October 2021",
      `Month of Year`=="November" ~ "November 2021",
      `Month of Year`=="December" ~ "December 2021",
      `Month of Year`=="January" ~ "January 2022",
      `Month of Year`=="February" ~ "February 2022",
      `Month of Year`=="March" ~ "March 2022",
      `Month of Year`=="April" ~ "April 2022",
      `Month of Year`=="May" ~ "May 2022",
      `Month of Year`=="June" ~ "June 2022",
      TRUE ~ `Month of Year`
      )
  ) %>%
  dplyr::mutate(quarter = as.yearqtr(`Month of Year`, format="%B %Y")) %>%
  dplyr::select(-`Month of Year`, -`Lion Assistance - Lead Radius (H)* (Deprecated)`, -`Lion Assistance - Target Radius (H)* (Deprecated)`) 

transBE <- as.data.frame(t(BE_try))


#transBE <- cbind(rownames(transBE), data.frame(transBE, row.names=NULL))
colnames(transBE) <- NULL


transBE1 <- transBE[-50,] 
transBE2 <- transBE[-49,] 

colnames(transBE1) <- as.character(unlist(transBE[49,]))
colnames(transBE2) <- as.character(unlist(transBE[50,]))

# Device part 
transBE1 <- transBE1[-49,] 
transBE1 <- transBE1 %>%
  rownames_to_column(var="product_level") %>%
  dplyr::mutate(
   Product = sub("\\-.*", "", product_level),
   Level = sub(".*- ", "", product_level)
  ) %>%
  dplyr::select(-product_level) %>%
  tidyr::gather(device, visits, Desktop:Tablet.11, factor_key=T) %>%
  dplyr::mutate(
    Device = case_when(
      str_detect(device, "Desktop|desktop") ~ "Desktop",
      str_detect(device, "Mobile|mobile|app|App|Mobile and Desktop") ~ "Mobile",
      str_detect(device, "Tablet") ~ "Mobile",
      TRUE ~ "Mobile"
    )
  ) %>%   
  dplyr::select(-device) 

colnames(transBE) <- as.character(unlist(transBE[50,]))

# Quarter part
transBE2 <- transBE2[-49,] 
transBE2 <- transBE2 %>%
  rownames_to_column(var="product_level") %>%
  dplyr::mutate(
    Product = sub("\\-.*", "", product_level),
    Level = sub(".*- ", "", product_level)
  ) %>%
  dplyr::select(-product_level) %>%
  # ADD THE ADDITIONAL COLUMNS HERE!!!
  tidyr::gather(quarter, visits, `2021 Q3`:`2022 Q2.8`, factor_key=T) 

transBE <- dplyr::left_join(transBE1, transBE2)

transBE <- transBE %>%
  dplyr::mutate(
    quarter = substr(quarter, 1, 7) 
  ) %>%
  dplyr::mutate(
    quarter = as.yearqtr(quarter)
    ) %>%
  dplyr::mutate(
    visits = as.numeric(as.character(visits))
  ) %>%
  dplyr::filter(visits >0)

transBE <- transBE %>%
  dplyr::mutate(Page = case_when(
    str_detect(Level, "Interest") ~ "Product page",
    str_detect(Level, "Opening|Salesflow|Lead") ~ "Start application",
    str_detect(Level, "Target") ~ "Finish application"
  )) %>%
  dplyr::select(-Level) %>%
  dplyr::mutate(
    Category = case_when(
      str_detect(Product, "Current|Payment|Pay|Go To 18|Account|Green Account") ~ "Current accounts",
      str_detect(Product, "Invest|IP|Mutual|Pension|Focus|Fund|Self") ~ "Investment products",
      str_detect(Product, "Savings|Save") ~ "Savings",
      str_detect(Product, "Mortgage") ~ "Mortgages",
      str_detect(Product, "Lending|lending|Loan|loan|credit") ~ "Unsecured lending",
      str_detect(Product, "insurance|Insurance|Assistance") ~ "Insurances",
      str_detect(Product, "other") ~ "Other"
    )
  )  %>%
  dplyr::mutate(
    Product = case_when(
      str_detect(Product, "Car insurance|Car Insurance") ~ "Car insurance",
      str_detect(Product, "Lion Account") ~ "Lion Account",
      str_detect(Product, "Green Account") ~ "Green Account",
      str_detect(Product, "Go To 18") ~ "Go To 18",
      str_detect(Product, "Home Insurance") ~ "Home Insurance",
      str_detect(Product, "Star") ~ "Star Fund",
      str_detect(Product, "Car Loan") ~ "	Car Loan",
      str_detect(Product, "Home Insurance") ~ "Home Insurance",
      str_detect(Product, "Star") ~ "Star Fund",
      str_detect(Product, "Lion Assistance") ~ "Lion Assistance",
      str_detect(Product, "Self Invest") ~ "Self Invest",
      str_detect(Product, "Focus") ~ "Focus plan",
      str_detect(Product, "credit card") ~ "credit card",
      str_detect(Product, "Mortgage|mortgage") ~ "Mortgage",
      str_detect(Product, "Convenience loan |Convenience Loan ") ~ "Convenience loan",
      str_detect(Product, "Green Savings Minors") ~ "Green Savings Minors",
      str_detect(Product, "Green Savings") ~ "Green Savings",
      str_detect(Product, "Self Invest") ~ "Self Invest",
      str_detect(Product, "Renovation loan |Renovation Loan|Immo / Renovation loan") ~ "Renovation Loan ",
      str_detect(Product, "other") ~ "Other")
    ) %>%
  dplyr::filter(!is.na(quarter)) %>%
  dplyr::group_by(Category, quarter, Product, Page, Device) %>%
  dplyr::summarise(
     Visits = sum(visits)
  ) %>% 
  dplyr::ungroup() %>%
  group_by(Category, quarter, Product, Device, Page) %>% 
  spread(Page, Visits) 

transBE2 <- transBE %>%
  dplyr::group_by(quarter, Category, Product) %>%
  dplyr::summarise(
    total_traffic= sum(`Product page`)
  ) %>%
  dplyr::mutate(
    rank_order = rank(desc(total_traffic), ties.method = "first")
  ) %>%
  dplyr::ungroup()

BE2 <- dplyr::left_join(transBE, transBE2, by=c("quarter", "Category", "Product")) %>%
  dplyr::mutate(
    con_1 = round((`Start application` /`Product page`), digits=2),
    con_2 = round((`Finish application` / `Start application`), digits=2),
    country = "Belgium"
  ) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0))

   
    
SP1 <- SP %>%
  dplyr::mutate(
    quarter =as.yearqtr(Date, format ="%Y-%m-%d")
  ) %>%
  dplyr::mutate(
    Category = case_when(
      str_detect(Category, "Current|Payment|Pay") ~ "Current accounts",
      str_detect(Category, "Invest|IP|Mutual|Pension") ~ "Investment products",
      str_detect(Category, "Savings|Save") ~ "Savings",
      str_detect(Category, "Mortgage") ~ "Mortgages",
      str_detect(Category, "Lending|lending|Loan") ~ "Unsecured lending",
      str_detect(Category, "insurance|Insurance") ~ "Insurances",
      str_detect(Category, "other") ~ "Other"
    )
  ) %>%
  dplyr::group_by(quarter, Category, Product) %>%
  dplyr::summarise(
    total_traffic= sum(Visits, na.rm = T)
  ) %>%
  dplyr::mutate(
    rank_order = rank(desc(total_traffic), ties.method = "first")
  ) %>%
  dplyr::ungroup()

SP3 <- SP %>%
  dplyr::mutate(
    quarter =as.yearqtr(Date, format ="%Y-%m-%d")
  ) %>%
  dplyr::mutate(
    Category = case_when(
      str_detect(Category, "Current|Payment|Pay") ~ "Current accounts",
      str_detect(Category, "Invest|IP|Mutual|Pension") ~ "Investment products",
      str_detect(Category, "Savings|Save") ~ "Savings",
      str_detect(Category, "Mortgage") ~ "Mortgages",
      str_detect(Category, "Lending|lending|Loan") ~ "Unsecured lending",
      str_detect(Category, "insurance|Insurance") ~ "Insurances",
      str_detect(Category, "other") ~ "Other"
    )
  ) %>%
  dplyr::mutate(
    NewvsCurrent = case_when(
      str_detect(`N2B/ Customer`, "customer|Customer|Existing") ~ "Customer",
      str_detect(`N2B/ Customer`, "N2B|n2b|new|Common") ~ "N2B",
      TRUE ~ "N2B"
    )
  ) %>% 
  dplyr::mutate(
    Device = case_when(
      str_detect(Device, "Desktop|desktop") ~ "Desktop",
      str_detect(Device, "Mobile|mobile|app|App|Mobile and Desktop") ~ "Mobile",
      TRUE ~ "Mobile"
    )
  ) %>%   
  dplyr::mutate(
    Page = case_when(
      str_detect(Page, "start|Start|started") ~ "Start application",
      str_detect(Page, "category|Category|Cartegory") ~ "Category page",
      str_detect(Page, "finish|Finish|completed") ~ "Finish application",
      str_detect(Page, "product|Product|landing|Landing") ~ "Product page",
      str_detect(Page, "approved") ~ "Account opening",
      TRUE ~ "Category page"
    )
  ) %>%  
  dplyr::group_by(quarter, Category, Product, Page, Device, NewvsCurrent) %>%
  dplyr::summarise(
    Visits = sum(Visits)
  ) %>% 
  dplyr::ungroup() 

SP2 <- dplyr::left_join(SP3, SP1, by=c("quarter", "Category", "Product")) %>%
  tidyr::spread(Page, Visits) %>%
  dplyr::mutate(
    con_1 = round((`Start application` /`Product page`), digits=2),
    con_2 = round((`Finish application` / `Start application`), digits=2),
    country = "Spain"
  ) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0))

### Poland
PL1 <- PL %>%
  dplyr::mutate(
    year =substr(date, 1, 4),
    quar = substr(date, 5, 6)
  ) %>%
  dplyr::mutate(
    Quarter = paste(year, quar, sep=" Q")
  ) %>%
  dplyr::mutate(
    quarter = as.yearqtr(Quarter)
  ) %>%
  dplyr::rename(
    Product = "product"
  ) %>%
  dplyr::mutate(
    Category = case_when(
      str_detect(category, "Current|Payment|Pay|Accounts") ~ "Current accounts",
      str_detect(category, "Invest|IP|Mutual") ~ "Investment products",
      str_detect(category, "Savings|Save") ~ "Savings",
      str_detect(category, "Mortgage") ~ "Mortgages",
      str_detect(category, "Lending|lending|Loan") ~ "Unsecured lending",
      str_detect(category, "insurance|Insurance|Insurances") ~ "Insurances",
      str_detect(category, "other") ~ "Other"
    )
  ) %>%
  dplyr::filter(!is.na(Product)) %>%
  dplyr::group_by(quarter, Category, Product) %>%
  dplyr::filter(page_type == "Product Page") %>%
  dplyr::mutate(
    NewvsCurrent = case_when(
      str_detect(client_segment, "customer|Customer|Existing") ~ "Customer",
      str_detect(client_segment, "N2B|n2b|new|Common") ~ "N2B",
      TRUE ~ "N2B"
    )
  ) %>% 
  ### First sum the mobile visits sources, all of a sudden a third category added
  dplyr::mutate(
    Mobile = sum(mobile_browser, mobile_app, mobile, na.rm=T)
  ) %>%
  dplyr::select(quarter, Category, NewvsCurrent, pagename, funnel_step, page_type, Product, Mobile, desktop) %>%
  tidyr::gather("Device", visits, Mobile:desktop) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    Device = case_when(
      str_detect(Device, "Desktop|desktop") ~ "Desktop",
      str_detect(Device, "Mobile|mobile|app|App|Mobile and Desktop") ~ "Mobile",
      TRUE ~ "Mobile"
    )
  ) %>%
  dplyr::group_by(quarter, Category, Product) %>%
  dplyr::summarise(
    total_traffic= 
      sum(as.numeric(as.character(visits)), na.rm=T)
  ) %>%
  dplyr::mutate(
    rank_order = rank(desc(total_traffic), ties.method = "first")
  ) %>%
  dplyr::ungroup() 
  

PL2 <- PL %>%
  dplyr::mutate(
    year =substr(date, 1, 4),
    quar = substr(date, 5, 6)
  ) %>%
  dplyr::mutate(
    Quarter = paste(year, quar, sep=" Q")
  ) %>%
  dplyr::mutate(
    quarter = as.yearqtr(Quarter)
  ) %>%
  dplyr::mutate(
    Category = case_when(
      str_detect(category, "Current|Payment|Pay|Accounts") ~ "Current accounts",
      str_detect(category, "Invest|IP|Mutual") ~ "Investment products",
      str_detect(category, "Savings|Save") ~ "Savings",
      str_detect(category, "Mortgage") ~ "Mortgages",
      str_detect(category, "Lending|lending|Loan") ~ "Unsecured lending",
      str_detect(category, "insurance|Insurance|Insurances") ~ "Insurances",
      str_detect(category, "other") ~ "Other"
    )
  ) %>%
  dplyr::mutate(
    NewvsCurrent = case_when(
      str_detect(client_segment, "customer|Customer|Existing") ~ "Customer",
      str_detect(client_segment, "N2B|n2b|new|Common") ~ "N2B",
      TRUE ~ "N2B"
    )
  ) %>% 
  dplyr::rename(
    Product = "product"
  ) %>%
  ### First sum the mobile visits sources, all of a sudden a third category added
  dplyr::group_by(quarter, Category, NewvsCurrent, Product, page_type) %>%
  dplyr::mutate(
    Mobile = sum(mobile_browser, mobile_app, mobile, na.rm=T)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(quarter, Category, NewvsCurrent, pagename, page_type, Product, Mobile, desktop) %>%
  tidyr::gather("Device", visits, Mobile:desktop) %>%
  dplyr::mutate(
    Device = case_when(
      str_detect(Device, "Desktop|desktop") ~ "Desktop",
      str_detect(Device, "Mobile|mobile|app|App|Mobile and Desktop") ~ "Mobile",
      TRUE ~ "Mobile"
    )
  ) %>%  
  dplyr::mutate(
    page_type = case_when(
      page_type =="Category" ~ "Category page",
      TRUE ~ page_type
    )
  ) %>%
  dplyr::group_by(quarter, Category, Product, page_type, Device, NewvsCurrent) %>%
  dplyr::summarise(
    Visits = sum(visits)
  ) %>% 
  dplyr::ungroup() 

PO2 <- dplyr::full_join(PL1, PL2, by=c("quarter", "Category", "Product"))

PO2 <- PO2 %>%
  dplyr::select(-total_traffic) %>%
  dplyr::mutate(
    Page = case_when(
      str_detect(page_type, "start|Start") ~ "Start application",
      str_detect(page_type, "category|Category|Cartegory|Category") ~ "Category page",
      str_detect(page_type, "finish|Finish") ~ "Finish application",
      str_detect(page_type, "product|Product|landing|Landing") ~ "Product page",
      TRUE ~ "Category page"
    )
  ) %>%  
  dplyr::select(-page_type) %>%
  tidyr::spread(Page, Visits) %>%
  dplyr::mutate(
    con_1 = round((`Start application` /`Product page`), digits=2),
    con_2 = round((`Finish application` / `Start application`), digits=2)
  ) %>% 
  dplyr::group_by(quarter, Category, Device, NewvsCurrent) %>%
  fill(`Category page`, .direction= "downup") %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(Product)) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0))
PO2$country <- "Poland"

### Romania
### this is to have visits on home bank (closed domain)
RO5 <- RO_4 %>%
  dplyr::mutate(
    quarter = 
      # let op deze case_when moet volgende x worden opgelost in de bron data 
    case_when(
        Quarter =="Q1" ~ paste("2022", Quarter, sep= " "),
        TRUE ~ paste("2021", Quarter, sep= " ")
      ) 
  ) %>%
  dplyr::mutate(
    quarter = as.yearqtr(quarter)
  ) %>%
  dplyr::mutate(
    date = case_when(
      !is.na(Date) ~ as.yearqtr(Date, format = "%Y-%m-%d"))
  ) %>%
  dplyr::mutate(
    quarter = case_when(
      is.na(Date) ~ quarter,
      TRUE ~ date
    )
  ) %>%
  dplyr::mutate(
    Category = case_when(
      str_detect(Category, "Current|Payment|Pay") ~ "Current accounts",
      str_detect(Category, "Invest|IP|Mutual") ~ "Investment products",
      str_detect(Category, "Savings|Save") ~ "Savings",
      str_detect(Category, "Mortgage") ~ "Mortgages",
      str_detect(Category, "Lending|lending|Loan") ~ "Unsecured lending",
      str_detect(Category, "insurance|Insurance") ~ "Insurances",
      str_detect(Category, "other") ~ "Other"
    )
  ) %>%
  dplyr::mutate(Category = case_when(
    Product =="Mutual Funds" ~ "Investment products",
    Product =="ING Credit Ipotecar" ~ "Mortgages",
    Product =="Mortgage Loan" ~ "Mortgages",
    Product =="Card Complet" ~ "Current accounts",
    Product =="ING Gold Card" ~ "Current accounts",
    TRUE ~ Category
  )
  ) %>%
  dplyr::mutate(
    Product = case_when(
      str_detect(Product, "Card Complet") ~ "Current accounts",
      str_detect(Product, "ING Gold Card") ~ "Current accounts",
      str_detect(Product, "ING Credit Card") ~ "Credit Card",
      str_detect(Product, "ING Credit de investitii|ING Credit Ipotecar|ING Prima Casa") ~ "Mortgage Loan",
      str_detect(Product, "ING Savings") ~ "Savings",
      str_detect(Product, "ING Deposit") ~ "Deposits",
      str_detect(Product, "Personal Loan") ~ "ING Personal",
      str_detect(Product, "Mutual") ~ "Mutual Funds",
      TRUE ~ Product
    )
  ) %>%
  dplyr::mutate(
    Device = case_when(
      str_detect(`Device category`, "Desktop|desktop|Web") ~ "Desktop",
      str_detect(`Device category`, "Mobile|mobile|app|App||tablet") ~ "Mobile",
      TRUE ~ "Mobile"
    )
  ) %>% 
  dplyr::group_by(quarter, Category, Product) %>%
  dplyr::summarise(
    total_traffic= sum(`Sessions (Adobe Analytics)`, na.rm = T)
  ) %>%
  dplyr::mutate(
    rank_order = rank(desc(total_traffic), ties.method = "first")
  ) %>%
  dplyr::ungroup()

RO6 <- RO_4 %>%
  dplyr::mutate(
    quarter = 
      # let op deze case_when moet volgende x worden opgelost in de bron data 
      case_when(
        Quarter =="Q1" ~ paste("2022", Quarter, sep= " "),
        TRUE ~ paste("2021", Quarter, sep= " ")
      ) 
  ) %>%
  dplyr::mutate(
    quarter = as.yearqtr(quarter)
  ) %>%
  dplyr::mutate(
    date = case_when(
      !is.na(Date) ~ as.yearqtr(Date, format = "%Y-%m-%d"))
  ) %>%
  dplyr::mutate(
    quarter = case_when(
      is.na(Date) ~ quarter,
      TRUE ~ date
    )
  ) %>%
  dplyr::mutate(
    Category = case_when(
      str_detect(Category, "Current|Payment|Pay") ~ "Current accounts",
      str_detect(Category, "Invest|IP|Mutual") ~ "Investment products",
      str_detect(Category, "Savings|Save") ~ "Savings",
      str_detect(Category, "Mortgage") ~ "Mortgages",
      str_detect(Category, "Lending|lending|Loan") ~ "Unsecured lending",
      str_detect(Category, "insurance|Insurance") ~ "Insurances",
      str_detect(Category, "other") ~ "Other"
    )
  ) %>%
  dplyr::mutate(Category = case_when(
    Product =="Mutual Funds" ~ "Investment products",
    Product =="ING Credit Ipotecar" ~ "Mortgages",
    Product =="Mortgage Loan" ~ "Mortgages",
    Product =="Card Complet" ~ "Current accounts",
    Product =="ING Gold Card" ~ "Current accounts",
    TRUE ~ Category
  )
  ) %>%
  dplyr::mutate(
    Product = case_when(
      str_detect(Product, "Card Complet") ~ "Current accounts",
      str_detect(Product, "ING Gold Card") ~ "Current accounts",
      str_detect(Product, "ING Credit Card") ~ "Credit Card",
      str_detect(Product, "ING Credit de investitii|ING Credit Ipotecar|ING Prima Casa") ~ "Mortgage Loan",
      str_detect(Product, "ING Savings") ~ "Savings",
      str_detect(Product, "Personal Loan") ~ "ING Personal",
      str_detect(Product, "ING Deposit") ~ "Deposits",
      str_detect(Product, "Mutual") ~ "Mutual Funds",
      TRUE ~ Product
    )
  ) %>%
  dplyr::mutate(
    Device = case_when(
      str_detect(`Device category`, "Desktop|desktop|Web") ~ "Desktop",
      str_detect(`Device category`, "Mobile|mobile|app|App||tablet") ~ "Mobile",
      TRUE ~ "Mobile"
    )
  ) %>% 
  dplyr::group_by(Category, Product, Device, quarter) %>%
  dplyr::summarise(
    `Product page` = sum(`Sessions (Adobe Analytics)`)
  ) %>% 
  dplyr::ungroup()


## this is to report visits home bank (so not other than product page, so no CA)
RO7 <- dplyr::left_join(RO6, RO5, by=c("quarter", "Category", "Product"))



## Adobe, homebank, savings with flow steps
RO_51 <- RO_5 %>%
  dplyr::mutate(
  quarterz = 
  # let op deze case_when moet volgende x worden opgelost in de bron data 
  case_when(
    is.na(...8) & Quarter =="Q1"~ paste("2022", Quarter, sep= " "),
    TRUE ~ paste("2021", Quarter, sep= " ")
  )) %>%
  dplyr::mutate(
    quarter = as.yearqtr(quarterz)
  ) %>%
  dplyr::mutate(
    Quarter= as.yearqtr(...8, format = "%Y-%m-%d")
  ) %>%
  dplyr::mutate(
    quarter = 
      case_when(
        is.na(as.character(Quarter)) ~ quarterz,
        TRUE ~ as.character(Quarter)
      ) 
  ) %>%
  dplyr::mutate(
    quarter = as.yearqtr(quarter)
  ) %>%
  dplyr::mutate(
    Category = case_when(
      str_detect(Category, "Current|Payment|Pay") ~ "Current accounts",
      str_detect(Category, "Invest|IP|Mutual") ~ "Investment products",
      str_detect(Category, "Savings|Save") ~ "Savings",
      str_detect(Category, "Mortgage") ~ "Mortgages",
      str_detect(Category, "Lending|lending|Loan") ~ "Unsecured lending",
      str_detect(Category, "insurance|Insurance") ~ "Insurances",
      str_detect(Category, "other") ~ "Other"
    )
  ) %>%
  dplyr::mutate(Category = case_when(
    Product =="Mutual Funds" ~ "Investment products",
    Product =="ING Credit Ipotecar" ~ "Mortgages",
    Product =="Mortgage Loan" ~ "Mortgages",
    Product =="Card Complet" ~ "Current accounts",
    Product =="ING Gold Card" ~ "Current accounts",
    TRUE ~ Category
  )
  ) %>%
  dplyr::mutate(
    Product = case_when(
      str_detect(Product, "Card Complet") ~ "Current accounts",
      str_detect(Product, "ING Gold Card") ~ "Current accounts",
      str_detect(Product, "ING Credit Card") ~ "Credit Card",
      str_detect(Product, "ING Credit de investitii|ING Credit Ipotecar|ING Prima Casa") ~ "Mortgage Loan",
      str_detect(Product, "ING Savings") ~ "Savings",
      str_detect(Product, "Personal Loan") ~ "ING Personal",
      str_detect(Product, "Mutual") ~ "Mutual Funds",
      TRUE ~ Product
    )
  ) %>%
  dplyr::mutate(
    Device = case_when(
      str_detect(`Device category`, "Desktop|desktop|Web") ~ "Desktop",
      str_detect(`Device category`, "Mobile|mobile|app|App||tablet") ~ "Mobile",
      TRUE ~ "Mobile"
    )
  ) %>% 
  dplyr::select(-Quarter) %>%
  dplyr::group_by(Category, quarter, Device, Product, `Page path`) %>%
  dplyr::summarise(
    visits = sum(`Sessions (Adobe Analytics)`)
  ) %>%
  tidyr::spread(`Page path`, visits) %>%
  dplyr::rename(
    "Start application" = `HB/ING Savings (account opening flow start)`,
    "Finish application" = `HB/ING Savings (conversion page)`
  ) 

## Adobe, current accounts with flow steps
RO_61 <- RO_6 %>%
  dplyr::select( -...9, -...10, -...11, -...12) %>%
  dplyr::mutate(
    quarterz = 
      # let op deze case_when moet volgende x worden opgelost in de bron data 
      case_when(
        is.na(...8) & Quarter =="Q1"~ paste("2022", Quarter, sep= " "),
        TRUE ~ paste("2021", Quarter, sep= " ")
      )) %>%
  dplyr::mutate(
    quarter = as.yearqtr(quarterz)
  ) %>%
  dplyr::mutate(
    Quarter= as.yearqtr(...8, format = "%Y-%m-%d")
  ) %>%
  dplyr::mutate(
    quarter = 
      case_when(
        is.na(as.character(Quarter)) ~ quarterz,
        TRUE ~ as.character(Quarter)
      ) 
  ) %>%
  dplyr::mutate(
    quarter = as.yearqtr(quarter)
  ) %>%
  dplyr::mutate(
    Category = case_when(
      str_detect(Category, "Current|Payment|Pay|Accounts") ~ "Current accounts",
      str_detect(Category, "Invest|IP|Mutual") ~ "Investment products",
      str_detect(Category, "Savings|Save") ~ "Savings",
      str_detect(Category, "Mortgage") ~ "Mortgages",
      str_detect(Category, "Lending|lending|Loan") ~ "Unsecured lending",
      str_detect(Category, "insurance|Insurance") ~ "Insurances",
      str_detect(Category, "other") ~ "Other"
    )
  ) %>%
  dplyr::mutate(Category = case_when(
    Product =="Mutual Funds" ~ "Investment products",
    Product =="ING Credit Ipotecar" ~ "Mortgages",
    Product =="Mortgage Loan" ~ "Mortgages",
    Product =="Card Complet" ~ "Current accounts",
    Product =="ING Gold Card" ~ "Current accounts",
    TRUE ~ Category
  )
  ) %>%
  dplyr::mutate(
    Product = case_when(
      str_detect(Product, "Card Complet") ~ "Current accounts",
      str_detect(Product, "ING Gold Card") ~ "Current accounts",
      str_detect(Product, "Current account") ~ "Current accounts",
      str_detect(Product, "ING Credit Card") ~ "Credit Card",
      str_detect(Product, "ING Credit de investitii|ING Credit Ipotecar|ING Prima Casa") ~ "Mortgage Loan",
      str_detect(Product, "ING Savings") ~ "Savings",
      str_detect(Product, "Personal Loan") ~ "ING Personal",
      str_detect(Product, "Mutual") ~ "Mutual Funds",
      TRUE ~ Product
    )
  ) %>%
  dplyr::mutate(
    Device = case_when(
      str_detect(`Device category`, "Desktop|desktop|Web") ~ "Desktop",
      str_detect(`Device category`, "Mobile|mobile|app|App||tablet") ~ "Mobile",
      TRUE ~ "Mobile"
    )
  ) %>% 
  dplyr::select(-Quarter) %>%
  dplyr::group_by(Category, quarter, Device, Product, `Page path`) %>%
  dplyr::summarise(
    visits = sum(`Sessions (Adobe Analytics)`)
  ) %>%
  tidyr::spread(`Page path`, visits) %>%
  dplyr::rename(
    "Start application" = `HB/Account opening (flow start)`,
    "Finish application" = `HB/Account opening conversion type 1`
  ) %>%
  dplyr::select(-`HB/Account opening conversion type 2`)

# Combine both files: start and finish for savings and current accounts

RO_sav_complete1 <- dplyr::full_join(RO_61, RO_51) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0))


# make sure that the start and finish from home bank en GA for specifically savings

# combine funnel flow steps with traffic Adobe file 
RO_sav_complete1 <- dplyr::full_join(RO_sav_complete1, RO7) %>%
 mutate_if(is.numeric, ~replace(., is.na(.), 0))

RO2 <- RO_sav_complete1 %>%
  dplyr::group_by(quarter, Category, Product, Device) %>%
  dplyr::summarise(
    `Start application` = sum(`Start application`),
    `Finish application` = sum(`Finish application`),
    `Product page` = sum(`Product page`)
  )

# and add the # visits product page in total traffic (GA)
#RO2 <- dplyr::full_join(RO_sav_complete, RO_12, by=c("quarter", "Category", "Product")) 

RO2$country <- "Romania"

RO2 <- RO2%>%
  dplyr::mutate(
    con_1 = round((`Start application` /`Product page`), digits=2),
    con_2 = round((`Finish application` / `Start application`), digits=2)
    ) %>%
    mutate_if(is.numeric, ~replace(., is.na(.), 0))


### Germany
DE1 <- DE_4 %>%
  dplyr::mutate(
    year =substr(QUARTER, 1, 4),
    quar =str_sub(QUARTER, 5)
  ) %>%
  dplyr::mutate(
    Quarter = paste(year, quar, sep=" Q")
  ) %>%
  dplyr::mutate(
    quarter = as.yearqtr(Quarter)
  ) %>%
  dplyr::mutate(
    Category = case_when(
      str_detect(product, "Current|Payment") ~ "Current accounts",
      str_detect(product, "Savings") ~ "Savings",
      str_detect(product, "Mortgage") ~ "Mortgages",
      str_detect(product, "Lending|lending|Loans") ~ "Unsecured lending",
      str_detect(product, "Invest|IP") ~ "Investment products",
      str_detect(product, "insurance|Insurance") ~ "Insurances",
      str_detect(product, "other") ~ "Other"
    )
  ) %>% 
  dplyr::mutate(product_detail = case_when(
    PRODUCT_DETAIL == "IP" ~ "IP Standard",
    TRUE ~ PRODUCT_DETAIL
  )) %>%
  dplyr::group_by(quarter, Category, PRODUCT_DETAIL) %>%
  dplyr::summarise(
    total_traffic= sum(product_page, na.rm = T)
  ) %>%
  dplyr::mutate(
    rank_order = rank(desc(total_traffic), ties.method = "first")
  ) %>%
  dplyr::ungroup()

DE <- DE_4 %>%
  dplyr::mutate(
    year =substr(QUARTER, 1, 4),
    quar =str_sub(QUARTER, 5)
  ) %>%
  dplyr::mutate(
    Quarter = paste(year, quar, sep=" Q")
  ) %>%
  dplyr::mutate(
    quarter = as.yearqtr(Quarter)
  ) %>%
  dplyr::mutate(
    Category = case_when(
      str_detect(product, "Current|Payment") ~ "Current accounts",
      str_detect(product, "Savings") ~ "Savings",
      str_detect(product, "Mortgage") ~ "Mortgages",
      str_detect(product, "Lending|lending|Loans") ~ "Unsecured lending",
      str_detect(product, "Invest|IP") ~ "Investment products",
      str_detect(product, "insurance|Insurance") ~ "Insurances",
      str_detect(product, "other") ~ "Other"
    )
  ) %>%
  dplyr::mutate(DEVICE = case_when(
    DEVICE=="NativeApp" ~ "Mobile",
    TRUE ~ DEVICE
  )) %>%
  dplyr::mutate(product_detail = case_when(
    PRODUCT_DETAIL == "IP" ~ "IP Standard",
    TRUE ~ PRODUCT_DETAIL
  )) %>%
  dplyr::group_by(quarter, PRODUCT_DETAIL, DEVICE, NEW_TO_BANK) %>%
  dplyr::summarise(
    CATEGORY = sum(category),
    PRODUCT_PAGE = sum(product_page),
    BASKET = sum(basket),
    WEB_COMPLETE = sum(web_complete),
    ACCOUNT = sum(account)
  ) %>%
  dplyr::ungroup()
 
DE2 <- dplyr::left_join(DE, DE1, by=c("quarter", "PRODUCT_DETAIL"))

DE2 <- DE2 %>%
  dplyr::select(quarter, CATEGORY, rank_order, DEVICE, Category,
                NEW_TO_BANK, PRODUCT_PAGE, PRODUCT_DETAIL, BASKET, WEB_COMPLETE, ACCOUNT) %>%
  dplyr::group_by(quarter, Category, PRODUCT_DETAIL, rank_order, DEVICE, NEW_TO_BANK) %>%
  dplyr::summarise(
    CATEGORY = sum(CATEGORY),
    PRODUCT_PAGE = sum(PRODUCT_PAGE),
    BASKET = sum(BASKET),
    WEB_COMPLETE = sum(WEB_COMPLETE),
    ACCOUNT = sum(ACCOUNT)
  ) %>%
  dplyr::mutate(
    con_1 = round((BASKET / PRODUCT_PAGE),digits=2),
    con_2 = round((WEB_COMPLETE/ BASKET), digits=2) 
  ) %>%
  rename("Category page" =CATEGORY,
         "Product page" =PRODUCT_PAGE,
         "Start application" =BASKET,
         "Finish application" =WEB_COMPLETE,
         "Account opening" = ACCOUNT,
         "NewvsCurrent" =NEW_TO_BANK,
         "Device" =DEVICE,
         "Product" = PRODUCT_DETAIL) %>%
  dplyr::mutate(NewvsCurrent = case_when(
    NewvsCurrent ==0 ~ "Customer",
    NewvsCurrent ==1 ~ "N2B"
  )) %>%
  dplyr::group_by(Device, NewvsCurrent, quarter, Category) %>%
  tidyr::fill(`Category page`, .direction = "downup") %>%
  dplyr::ungroup() %>%
  dplyr::group_by(Device, NewvsCurrent, quarter, Category) %>%
  tidyr::fill(`Product page`, .direction = "updown") %>%
  dplyr::mutate(
    con_1 = round((`Start application`/ `Product page`),digits=2),
    con_2 = round((`Finish application`/ `Start application`), digits=2) 
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(rank_order< 4) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  dplyr::filter(con_1>0)
DE2$country <- "Germany"



# Add product specifically

IT1 <- IT %>%
  dplyr::mutate(
    quarter = as.yearqtr(Date, format = "%Y-%m-%d")
  ) %>%
  dplyr::select(-Date) %>%
  dplyr::mutate(
    Product = case_when(
      Category == "Instant" ~ "Instant lending",
      Category == "Not instant" ~ "Lending",
      TRUE ~ Product
    )
  ) %>%
  dplyr::select(-Category) %>%
  dplyr::mutate(
    Category = case_when(
      str_detect(Product, "Current") ~ "Current accounts",
      str_detect(Product, "Savings") ~ "Savings",
      str_detect(Product, "Mortgage|Mortage") ~ "Mortgages",
      str_detect(Product, "Lending|lending") ~ "Unsecured lending",
      str_detect(Product, "Invest|My|Trading|self|Investment") ~ "Investment products",
      str_detect(Product, "insurance|Insurance") ~ "Insurances"
      )
    ) %>%
  dplyr::mutate(
    Page = case_when(
      str_detect(Page, "start|Start|started") ~ "Start application",
      str_detect(Page, "category|Category|Cartegory") ~ "Category page",
      str_detect(Page, "finish|Finish|completed") ~ "Finish application",
      str_detect(Page, "product|Product|landing|Landing") ~ "Product page",
      str_detect(Page, "account|approved") ~ "Account opening",
      TRUE ~ "Category page"
    )
  ) %>%   
  dplyr::mutate(
    NewvsCurrent = case_when(
      str_detect(`N2B/ Customer`, "customer|Customer") ~ "Customer",
      str_detect(`N2B/ Customer`, "N2B|n2b|new|Common") ~ "N2B",
      Product =="MyMoneyCoach" ~ "Customer",
      Product =="Investment in self" ~ "Customer",
      TRUE ~ "N2B"
    )
  ) %>%   
  dplyr::select(-`N2B/ Customer`) %>%
  dplyr::mutate(
    Device = case_when(
      str_detect(Device, "Desktop|desktop") ~ "Desktop",
      str_detect(Device, "Mobile|mobile|app|App") ~ "Mobile",
      TRUE ~ "Desktop"
    )
  ) %>%    
  dplyr::group_by(quarter, Category, Product) %>%
  dplyr::summarise(
      total_traffic= sum(Visits, na.rm = T)
  ) %>%
  dplyr::mutate(
    rank_order = rank(desc(total_traffic), ties.method = "first")
    ) %>%
    dplyr::ungroup() 

IT3 <- IT %>%
 # dplyr::select(-...8) %>%
  dplyr::mutate(
    quarter = as.yearqtr(Date, format = "%Y-%m-%d")
  ) %>%
  dplyr::select(-Date) %>%
  dplyr::mutate(
    Product = case_when(
      Category == "Instant" ~ "Instant lending",
      Category == "Not instant" ~ "Lending",
      TRUE ~ Product
    )
  ) %>%
  dplyr::select(-Category) %>%
  dplyr::mutate(
    Category = case_when(
      str_detect(Product, "Current") ~ "Current accounts",
      str_detect(Product, "Savings") ~ "Savings",
      str_detect(Product, "Mortgage|Mortage") ~ "Mortgages",
      str_detect(Product, "Lending|lending") ~ "Unsecured lending",
      str_detect(Product, "Invest|My|Trading|self|Investment") ~ "Investment products",
      str_detect(Product, "insurance|Insurance") ~ "Insurances"
    )
  ) %>%
  dplyr::mutate(
    Page = case_when(
      str_detect(Page, "start|Start|started") ~ "Start application",
      str_detect(Page, "category|Category|Cartegory") ~ "Category page",
      str_detect(Page, "finish|Finish|completed") ~ "Finish application",
      str_detect(Page, "product|Product|landing|Landing") ~ "Product page",
      str_detect(Page, "account|approved") ~ "Account opening",
      TRUE ~ "Category page"
    )
  ) %>%   
  dplyr::mutate(
    NewvsCurrent = case_when(
      str_detect(`N2B/ Customer`, "customer|Customer") ~ "Customer",
      str_detect(`N2B/ Customer`, "N2B|n2b|new|Common") ~ "N2B",
      Product =="MyMoneyCoach" ~ "Customer",
      Product =="Investment in self" ~ "Customer",
      TRUE ~ "N2B"
    )
  ) %>%   
  dplyr::select(-`N2B/ Customer`) %>%
  dplyr::mutate(
    Device = case_when(
      str_detect(Device, "Desktop|desktop") ~ "Desktop",
      str_detect(Device, "Mobile|mobile|app|App") ~ "Mobile",
      TRUE ~ "Desktop"
    )
  ) %>%
  dplyr::group_by(quarter, Category, Product, Page, Device, NewvsCurrent) %>%
  dplyr::summarise(
    Visits =sum(Visits)
  ) %>%
  dplyr::ungroup()
  
IT2 <- dplyr::left_join(IT1, IT3, by=c("quarter", "Category", "Product"))

IT2 <- IT2 %>%
  tidyr::spread(Page, Visits) %>%
  dplyr::group_by(quarter, Device, NewvsCurrent, Category) %>%
  tidyr::fill(`Product page`, .direction = "downup") %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    con_1 = round((`Start application` /`Product page`), digits=2),
    con_2 = round((`Finish application` / `Start application`), digits=2)
  ) %>% 
  dplyr::group_by(quarter, Device, NewvsCurrent, Category) %>%
  tidyr::fill(`Product page`, .direction = "downup") %>%
  dplyr::ungroup() %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0))
IT2$country <- "Italy" 

### NL snippet for mortgages



########### Combine and cross analysis ################

# Countries: IT, PL, DE, SP, RO

data_tot <- dplyr::full_join(IT2, PO2)
data_tot <- dplyr::full_join(data_tot, DE2)
data_tot <- dplyr::full_join(data_tot, SP2)
data_tot <- dplyr::full_join(data_tot, RO2)
data_tot <- dplyr::full_join(data_tot, NL2)
data_tot <- dplyr::full_join(data_tot, BE2)
data_tot <- dplyr::full_join(data_tot, AUS2)

# Add conversion tot

data_tot <- data_tot %>%
  dplyr::mutate(
    conv_tot = round(`Finish application` / `Product page`, digits=2)*100,
    con_1 = con_1 * 100,
    con_2 = con_2 * 100
  )  %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0))

# Create dataset for overviews

data_overviews <- data_tot %>%
  dplyr::arrange(quarter) %>%
  
  dplyr::mutate(country = case_when(
    country=="The Netherlands" ~ "NL",
    TRUE ~ country
  )) %>%
  #dplyr::filter(rank_order==1 | rank_order==2 | rank_order ==3) %>%
  dplyr::filter(Device =="Mobile" |Device =="Desktop") %>%
  dplyr::group_by(quarter, country, Category, Device, Product, rank_order, NewvsCurrent) %>%
  dplyr::summarise(
    `Category page` = sum(`Category page`, na.rm=T),
    `Product page` = sum(`Product page`, na.rm=T),
    `Start application` = sum(`Start application`, na.rm=T),
    `Finish application` = sum(`Finish application`, na.rm=T),
    `Account opening` = sum(`Account opening`, na.rm=T)
  ) %>%
  dplyr::ungroup () %>%
  dplyr::mutate(
    con_1 = round((`Start application` /`Product page`), digits=2),
    con_2 = round((`Finish application` / `Start application`), digits=2),
    conv_tot = round(`Finish application` / `Product page`, digits=2),
  ) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  dplyr::arrange(quarter) %>%
  dplyr::group_by(country, Category, Device, Product, rank_order, NewvsCurrent) %>%
  dplyr::mutate(
    QoQ_con_1 = con_1 - lag(con_1),
    QoQ_con_2 = con_2 - lag(con_2),
    QoQ_con_tot = conv_tot - lag(conv_tot)
  ) %>%
  dplyr::ungroup()
  


## Create little QPC slice of data overviews 

# Run this and copy to excel for customers 
data_overviews_test_customer <- data_overviews %>%
  dplyr::mutate(
    Category= case_when(
      Category =="Current accounts" ~"CA",
      Category =="Savings" ~"SV",
      Category =="Insurances" ~"IN",
      Category =="Investment products" ~"IP",
      Category =="Investing" ~"IP",
      Category =="Mortgages" ~"MG",
      Category =="Unsecured lending" ~"UL"
    )
  ) %>%
  dplyr::mutate(comb_name = paste(Category, Product, sep="_", collapse=NULL)) %>%
  dplyr::select(quarter, country, Category, comb_name, Device, 
                NewvsCurrent, rank_order, `Product page`, `Start application`, `Finish application`,
                con_1, con_2, conv_tot, QoQ_con_tot, QoQ_con_1, QoQ_con_2
                ) %>%
  gather(variable, value, -(quarter:rank_order)) %>%
  unite(temp, Device, variable) %>%
  spread(temp, value) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  dplyr::arrange(quarter) %>%
  dplyr::filter(quarter == max(quarter) & quarter !="2021 Q4") %>%
  dplyr::select(quarter, country, comb_name, NewvsCurrent, rank_order, starts_with("Mobile")) 

# Run this and copy to excel for N2B 
data_overviews_test_n2b <- data_overviews %>%
  dplyr::mutate(
    Category= case_when(
      Category =="Current accounts" ~"CA",
      Category =="Savings" ~"SV",
      Category =="Insurances" ~"IN",
      Category =="Investment products" ~"IP",
      Category =="Mortgages" ~"MG",
      Category =="Unsecured lending" ~"UL"
    )
  ) %>%
  dplyr::mutate(comb_name = paste(Category, Product, sep="_", collapse=NULL)) %>%
  dplyr::select(quarter, country, Category, comb_name, Device, 
                NewvsCurrent, rank_order, `Product page`, `Start application`, `Finish application`,
                con_1, con_2, conv_tot, QoQ_con_tot, QoQ_con_1, QoQ_con_2
  ) %>%
  gather(variable, value, -(quarter:rank_order)) %>%
  unite(temp, Device, variable) %>%
  spread(temp, value) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  dplyr::arrange(quarter) %>%
  dplyr::filter(quarter == max(quarter) & quarter !="2021 Q4") %>%
  dplyr::filter(NewvsCurrent=="N2B") 
  
x <- data_overviews %>%
  dplyr::mutate(
    Category= case_when(
      Category =="Current accounts" ~"CA",
      Category =="Savings" ~"SV",
      Category =="Insurances" ~"IN",
      Category =="Investment products" ~"IP",
      Category =="Mortgages" ~"MG",
      Category =="Unsecured lending" ~"UL"
    )
  ) %>%
  dplyr::mutate(comb_name = paste(Category, Product, sep="_", collapse=NULL)) %>%
  dplyr::select(quarter, country, Category, comb_name, Device, 
                NewvsCurrent, rank_order, `Product page`, `Start application`, `Finish application`,
                con_1, con_2, conv_tot, QoQ_con_tot, QoQ_con_1, QoQ_con_2
  ) %>%
  gather(variable, value, -(quarter:rank_order)) %>%
  unite(temp, Device, variable) %>%
  spread(temp, value) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  dplyr::arrange(quarter) %>%
  dplyr::filter(quarter == max(quarter) & quarter !="2021 Q4") %>%
  dplyr::filter(NewvsCurrent=="Customer" & rank_order==1) %>%
  dplyr::filter(country=="Australia")


### Clean up memory
rm(RO_51, RO_sav_complete1, RO2, RO5, RO6, RO7, SP, SP1, SP2, SP3, transBE, transBE1, transBE2)
rm(IT1, PL_SA_1, PL_SA_2, PL_UL, PL_UL_1, PL_UL_2, PL1, PL2, PO2, RO_4, RO_5, RO_6, RO_61)
rm(AUS, IT2, PL_IN_2, PL_IP, PL_IP_1, PL_IP_2, PL_IP3, PL_SA)
rm(DE1, DE2, IT, IT2, IT3, NL_adobe_FLOW, NL_adobe_FLOW1, NL_adobe_PP, NL_adobe_PP_DT_app)
rm(NL_adobe_PP_DT_mob, NL_adobe_PP_ranking, NL2, PL, PL_CA, PL_CA_1, PL_CA_2, PL_IN, PL_IN_1)
rm(AUS, IT2, PL_IN_2, PL_IP, PL_IP_1, PL_IP_2, PL_IP3, PL_SA, PL_SA)
rm(AUS11, AUS12, Aussie1, Aussie2, BE, BE_1, BE_2, BE_3, BE_4, BE_try, BE2, DE, DE_1, DE_2, DE_3, DE_4)
rm(AU2, AUS_try, AUS1, AUS10, AUS2, AUS3, AUS4, AUS5, AUS6, AUS7, AUS8, AUS9, Aussie )
rm(AU11, AUS12, Aussie1, Aussie2, BE, BE_1, BE_2, BE_3, BE_4, BE_try, BE2, DE, DE_1, DE_2)
rm(AU2, AUS_try, AUS1, AUS10, AUS2, AUS3, AUS4, AUS5, AUS5, AUS6, AUS7, AUS8, AUS9, Aussie)
rm(x)
