### NL
## data transformation Adobe NL

NL_adobe_PP_DT_mob <- read_excel("~/Documents/reporting_conversion/data/obeya_Moniek - ING_NL_PRD - Jul 13, 2022.xlsx", range = "A13:C64")
NL_adobe_PP_DT_app <- read_excel("~/Documents/reporting_conversion/data/obeya_Moniek - ING_NL_PRD - Jul 13, 2022.xlsx", range = "A78:B129")
NL_adobe_FLOW <- read_excel("~/Documents/reporting_conversion/data/obeya_Moniek - ING_NL_PRD - Jul 13, 2022.xlsx", range = "A143:E149")

# add quarter
NL_adobe_PP_DT_mob$quarter <- as.yearqtr("2022 Q2")
NL_adobe_PP_DT_app$quarter <- as.yearqtr("2022 Q2")
NL_adobe_FLOW$quarter <- as.yearqtr("2022 Q2")

# Transform product pages website
NL_adobe_PP_DT_mob <- NL_adobe_PP_DT_mob %>%
  dplyr::mutate(
    page_type = case_when(
      str_detect(...1 , "ProductPage") ~ "Product page"
    )
  ) %>%
  dplyr::mutate(
    NewvsCurrent = case_when(
      str_detect(...1 , "closed") ~ "Customer"
    )
  ) %>%
  dplyr::mutate(
    product2 = 
      sub(".*-","",  ...1)
  ) %>%
  dplyr::mutate(
    Category = case_when(
      str_detect(product2, "Betaalrekening|MobielBetalen|12jaar|18jaar|GroterRekening|Jointcurrentaccount|
                 Jongerenrekening|Kinder|Zakelijkerekening|creditcard|Creditcard|Paywithyourphone|
                 Extracurrentaccount|18years|OmzettennaarStudentenrekening|Jongerenrekening|Voorstudenten|Extracurrentaccount|
                 BetaalpasZakelijk|INGBusinesscard|BetaalpasZakelijk|
                 ExtraZakelijkeRekening|SwitchtoStudentaccount") ~ "Current accounts",
      str_detect(product2, "verzekering|Homeinsurance|Mobielverzekering|Rechtsbijstandverzekering|
                 Woonverzekering|Zorgverzekering") ~ "Insurances",
      str_detect(product2, "Vermogensbeheer|ZelfopdeBeurs|EenvoudigBeleggen|beleggen|investing") ~ "Investment products",
      str_detect(product2, "lening|Autolening|Financieringaanvragen|Leningverhogen|PersoonlijkeLening|Personalloan|
                 Personalloan|Verbouwingfinancieren|Rood|INGRoodStaan") ~ "Unsecured lending",
      str_detect(product2, "hypotheek|Hypotheekverhogenvoorjeverbouwing|AfspraakmeteenINGadviseur|
                 HypotheekoversluitennaarING|HypotheekoversluitennaarING") ~ "Mortgages",
      str_detect(product2, "Sparen|OranjeSpaarrekening|Doelsparen|Savingsaccount") ~ "Savings",
      str_detect(...1, "ProductPage763|ProductPage750|ProductPage754|ProductPage764|smartinvestment|beleggen") ~ "Investment products"
    )
  ) %>%
  dplyr::mutate(
    Product = case_when(
      str_detect(product2, "12jaar|18jaar|18years|GroterRekening|Kinder|GroeiGroterRekening") ~ "Kinderrekening",
      str_detect(product2, "EnofBetaalrekening|Jointcurrentaccount") ~ "EnofBetaalrekening",
      str_detect(product2, "Studentenrekening|OmzettennaarStudentenrekening|SwitchtoStudentaccount|studenten") ~ "Studentenrekening",
      str_detect(product2, "INGRoodStaan") ~ "RoodStaan",
      str_detect(product2, "Creditcard|creditcard|credit") ~ "Creditcard",
      str_detect(product2, "Zakelijkerekening|BetaalpasZakelijk|INGBusinesscard|ExtraZakelijkeRekening") ~ "Zakelijke rekening",
      str_detect(product2, "SoepelMobielBetalen|Paywithyourphone|Betaalrekening|betaalrekening
                 |Extracurrentaccount") ~ "ING rekening",
      str_detect(product2, "EenvoudigBeleggen") ~ "Eenvoudig beleggen",
      str_detect(...1, "products:763|763") ~ "Eenvoudig beleggen",
      str_detect(...1, "ProductPage754|754") ~ "Vermogensbeheer",
      str_detect(product2, "Vermogensbeheer") ~ "Vermogensbeheer",
      str_detect(product2, "ZelfopdeBeurs") ~ "ZelfopdeBeurs",
      str_detect(...1, "products:750|750") ~ "ZelfopdeBeurs",
      str_detect(product2, "hypotheek|Hypotheekverhogenvoorjeverbouwing|AfspraakmeteenINGadviseur|
                 HypotheekoversluitennaarING|HypotheekoversluitennaarING") ~ "Hypotheken",
      str_detect(product2, "Sparen|OranjeSpaarrekening|Savingsaccount|ZakelijkeOranjeSpaarrekening") ~ "Oranje Spaarrekening",
      str_detect(product2, "Doelsparen") ~ "Doelsparen",
      str_detect(product2, "Homeinsurance|Woonverzekering") ~ "Woonverzekering",
      str_detect(product2, "Autoverzekering") ~ "Autoverzekering",
      str_detect(product2, "Mobielverzekering") ~ "Mobielverzekering",
      str_detect(product2, "Aansprakelijkheidsverzekering") ~ "Aansprakelijkheidsverzekering",
      str_detect(product2, "Rechtsbijstandverzekering") ~ "Rechtsbijstandverzekering",
      str_detect(product2, "Zorgverzekering") ~ "Zorgverzekering",
      str_detect(product2, "Financieringaanvragen|Leningverhogen|Personalloan|PersoonlijkeLening|
                 Verbouwingfinancieren|Autolening|lening") ~ "Persoonlijke lening",
      str_detect(product2, "INGRoodStaan") ~ "RoodStaan"
    )
  ) %>%
  dplyr::select(quarter, page_type, NewvsCurrent,
                Product, Category, `Visits from Mobile Devices`, `Visits from Non-Mobile Devices`) %>%
  dplyr::filter(!is.na(page_type)) %>%
  tidyr::gather(device, Visits, `Visits from Mobile Devices`:`Visits from Non-Mobile Devices`, factor_key=TRUE) %>%
  dplyr::mutate(
    Device =case_when(
      str_detect(device, "Non-Mobile|Visits from Non-Mobile Devices") ~ "Desktop",
      str_detect(device, "Mobile Devices") ~ "Mobile"
    )
  ) %>% 
  dplyr::select(quarter, page_type, NewvsCurrent, Visits, Device, Product, Category) %>%
  dplyr::group_by(quarter, page_type, NewvsCurrent, Device, Product, Category) %>%
  dplyr::summarise(
    Visits =sum(Visits)
  ) %>% 
  dplyr::ungroup()

# transform productshop app
NL_adobe_PP_DT_app <- NL_adobe_PP_DT_app %>%
  dplyr::mutate(
    page_type = case_when(
      str_detect(...1 , "ProductPage") ~ "Product page"
    )
  ) %>%
  dplyr::mutate(
    NewvsCurrent = case_when(
      str_detect(...1 , "moba") ~ "Customer"
    )
  ) %>%
  dplyr::mutate(
    Device = case_when(
      str_detect(...1 , "moba") ~ "Mobile"
    )
  ) %>%
  dplyr::mutate(
    product2 = 
      sub(".*-","",  ...1)
  ) %>%
  dplyr::mutate(
    Category = case_when(
      str_detect(product2, "Betaalrekening|MobielBetalen|12jaar|18jaar|GroterRekening|Jointcurrentaccount|
                 Jongerenrekening|Kinder|Zakelijkerekening|creditcard|Creditcard|Paywithyourphone|
                 Extracurrentaccount|18years|OmzettennaarStudentenrekening|Jongerenrekening|Voorstudenten|Extracurrentaccount|
                 BetaalpasZakelijk|INGBusinesscard|BetaalpasZakelijk|Joint|
                 ExtraZakelijkeRekening|SwitchtoStudentaccount|Creditcard|betalen|Betalen|Pay|
                 creditcard|Credit card|Extra credit card|Extra current account|
                 Joint current account|Business account|Betaalpas Zakelijk|Voor studenten") ~ "Current accounts",
      str_detect(product2, "verzekering|Homeinsurance|Mobielverzekering|Rechtsbijstandverzekering|
                 Woonverzekering|Zorgverzekering|Annual travel insurance|Car insurance|Liability insurance|
                 Liability insurance|Annualtravelinsurance|Legal expenses insurance") ~ "Insurances",
      str_detect(product2, "Vermogensbeheer|ZelfopdeBeurs|EenvoudigBeleggen|beleggen|investing") ~ "Investment products",
      str_detect(product2, "lening|Autolening|Financieringaanvragen|Leningverhogen|PersoonlijkeLening|Personalloan
                  |Personal loan|Verbouwingfinancieren|Rood|INGRoodStaan|Carloan|loan|Krediet|Car loan|Renovation loan
                  |DoorlopendKrediet") ~ "Unsecured lending",
      str_detect(product2, "hypotheek|Hypotheekverhogenvoorjeverbouwing|AfspraakmeteenINGadviseur|
                 HypotheekoversluitennaarING|HypotheekoversluitennaarING|huis|
                 Energiezuinighuiskopen?|Je eerste huis kopen|Jeeerstehuiskopen") ~ "Mortgages",
      str_detect(product2, "Sparen|OranjeSpaarrekening|Doelsparen|Savingsaccount") ~ "Savings",
      str_detect(...1, "ProductPage763|ProductPage750|ProductPage754|ProductPage764|smartinvestment|beleggen") ~ "Investment products"
    )
  ) %>%
  dplyr::mutate(
    Product = case_when(
      str_detect(product2, "12jaar|18jaar|18years|GroterRekening|Kinder|GroeiGroterRekening") ~ "Kinderrekening",
      str_detect(product2, "EnofBetaalrekening|Jointcurrentaccount|Joint") ~ "EnofBetaalrekening",
      str_detect(product2, "Studentenrekening|OmzettennaarStudentenrekening|SwitchtoStudentaccount|studenten") ~ "Studentenrekening",
      str_detect(product2, "INGRoodStaan") ~ "RoodStaan",
      str_detect(product2, "Creditcard|creditcard|credit|Credit card|Student Credit card") ~ "Creditcard",
      str_detect(product2, "Zakelijkerekening|BetaalpasZakelijk|INGBusinesscard|ExtraZakelijkeRekening|Business|Betaalpas Zakelijk") ~ "Zakelijke rekening",
      str_detect(product2, "SoepelMobielBetalen|Paywithyourphone|Betaalrekening|betaalrekening
                 |Extracurrentaccount|Soepel|Pay|Extra current") ~ "ING rekening",
      str_detect(product2, "EenvoudigBeleggen") ~ "Eenvoudig beleggen",
      str_detect(...1, "products:763|763") ~ "Eenvoudig beleggen",
      str_detect(...1, "ProductPage754|754") ~ "Vermogensbeheer",
      str_detect(product2, "Vermogensbeheer") ~ "Vermogensbeheer",
      str_detect(product2, "ZelfopdeBeurs") ~ "ZelfopdeBeurs",
      str_detect(...1, "products:750|750") ~ "ZelfopdeBeurs",
      str_detect(product2, "hypotheek|Hypotheekverhogenvoorjeverbouwing|AfspraakmeteenINGadviseur|
                 HypotheekoversluitennaarING|HypotheekoversluitennaarING|Energiezuinighuiskopen?|
                 Energie zuinig huiskopen?|Je eerste huis kopen|Jeeerstehuiskopen") ~ "Hypotheken",
      str_detect(product2, "Sparen|OranjeSpaarrekening|Savingsaccount|ZakelijkeOranjeSpaarrekening") ~ "Oranje Spaarrekening",
      str_detect(product2, "Doelsparen") ~ "Doelsparen",
      str_detect(product2, "Homeinsurance|Woonverzekering") ~ "Woonverzekering",
      str_detect(product2, "Autoverzekering|Car insurance") ~ "Autoverzekering",
      str_detect(product2, "Mobielverzekering") ~ "Mobielverzekering",
      str_detect(product2, "Aansprakelijkheidsverzekering|Liability insurance") ~ "Aansprakelijkheidsverzekering",
      str_detect(product2, "Rechtsbijstandverzekering|Legal expenses insurance") ~ "Rechtsbijstandverzekering",
      str_detect(product2, "Zorgverzekering") ~ "Zorgverzekering",
      str_detect(product2, "Annual travel insurance|Annualtravelinsurance") ~ "Reisverzekering",
      str_detect(product2, "Financieringaanvragen|Leningverhogen|Personalloan|PersoonlijkeLening|
                 Verbouwingfinancieren|Autolening|lening|Personal loan|Doorlopend Krediet|Renovation loan|
                 DoorlopendKrediet|Carloan|Top up your loan") ~ "Persoonlijke lening",
      str_detect(product2, "INGRoodStaan") ~ "RoodStaan"
    )
  ) %>% dplyr::filter(!is.na(page_type)) %>%
  dplyr::select(quarter, page_type, NewvsCurrent, Visits, Device, Product, Category) %>%
  dplyr::group_by(quarter, page_type, NewvsCurrent, Device, Product, Category) %>%
  dplyr::summarise(
    Visits =sum(Visits)
  ) %>% 
  dplyr::ungroup()

NL_adobe_PP <- dplyr::full_join(NL_adobe_PP_DT_mob, NL_adobe_PP_DT_app) %>%
  dplyr::group_by(quarter, Category, Product, Device, NewvsCurrent) %>%
  dplyr::summarise(
    Visits = sum(Visits)
  ) %>%
  dplyr::rename(
    "Product page"= Visits
  ) %>% 
  dplyr::ungroup()

NL_adobe_PP_ranking <- NL_adobe_PP %>%
  dplyr::group_by(quarter, Category, Product) %>%
  dplyr::summarise(
    total_traffic= sum(`Product page`, na.rm = T)
  ) %>%
  dplyr::mutate(
    rank_order = rank(desc(total_traffic), ties.method = "first")
  ) %>%
  dplyr::ungroup()


# tranform form
NL_adobe_FLOW <- NL_adobe_FLOW %>%
  dplyr::mutate(Category = case_when(
    str_detect(...1, "invest|beleggen") ~ "Investment products"
  )
  ) %>%
  dplyr::mutate(
    Product = case_when(
      str_detect(...1, "EenvoudigBeleggen|eenvoudig") ~ "Eenvoudig beleggen",
      #str_detect(...1, "Vermogensbeheer") ~ "Vermogensbeheer",
      str_detect(...1, "ZelfopdeBeurs|Zelf op de Beurs|self-invest") ~ "ZelfopdeBeurs",
    )
  ) %>%
  dplyr::rename(
    "Start application_mobile" = `form start >> form view...2`,
    "Start application_desktop" = `form start >> form view...3`,
    "Finish application_mobile" = `form complete >> form confirmation...4`,
    "Finish application_desktop" = `form complete >> form confirmation...5`
  ) 

NL_adobe_FLOW <-NL_adobe_FLOW[-c(1:2),]

NL_adobe_FLOW1 <- NL_adobe_FLOW %>%
  tidyr::gather(device, Visits,`Start application_mobile`:`Finish application_desktop`, factor_key=TRUE) %>%
  dplyr::mutate(
    page_type = case_when(
      str_detect(device, "Start") ~ "Start application",
      str_detect(device, "Finish") ~ "Finish application"
    )
  ) %>%
  dplyr::mutate(
    Device = case_when(
      str_detect(device, "mobile") ~ "Mobile",
      str_detect(device, "desktop") ~ "Desktop"
    )
  ) %>%
  dplyr::group_by(quarter, Category, Product, Device, page_type) %>%
  dplyr::summarise(
    Visits = sum(as.numeric(as.character(Visits)))
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    NewvsCurrent = "Customer"
  ) %>%
  tidyr::spread(page_type, Visits)



### Combine
NL2 <- dplyr::left_join(NL_adobe_PP, NL_adobe_PP_ranking)


NL2 <- dplyr::left_join(NL2, NL_adobe_FLOW1, by=c("quarter", "Category", "Product", "Device",
                                                  "NewvsCurrent"))

NL2 <- NL2 %>%
  dplyr::mutate(
    con_1 = round((`Start application` /`Product page`), digits=2),
    con_2 = round((`Finish application` / `Start application`), digits=2),
    country = "The Netherlands"
  ) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0))  
