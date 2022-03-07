#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(pxweb)
library(ggplot2)
library(tidyverse)
library(plotly)
library(ggthemes)
library(thematic)
library(scales)
library(shinydashboard)
library(geofi)
library(rjstat)
library(shinycssloaders)
library(stringr)
library(readr)


# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%;background-color: #FAEBD7;}"),
  tags$style(type = "text/css", ".info-box-text, .info-box-content p {font-family:Arial Narrow;font-size:5;}"),
  tags$style(type = "text/css", ".sourceCite {font-family:Arial Narrow;font-size:5;text-align:right;font-style:italic;color:grey;}"),
  tags$style(type = "text/css", ".kpiHeader {font-family:Arial Narrow;font-size:14;text-align:left;font-style:regular; }"),
  tags$style(type = "text/css", ".pxwebcite {font-family:Arial Narrow;font-size:14;text-align:right;font-style:regular; }"),
  theme = bslib::bs_theme(
    base_font = bslib::font_google("Archivo Black")
  ),
  fluidRow(
    column(2,
           plotOutput("finmapOutput", height = "150px")
    ),
    column(8,
           HTML("<h1 style='text-align:left;font-style:italic;'>Finland's Economy in Figures</h1>"),
           HTML("<p style='text-align:left;font-style:italic;'>Dashboard in the spirit of Teollisuuden työnantajat TT's print publication from 1995</p>")
    ),
    column(2)
  ),
  fluidRow(
    column(2,
           HTML("<h6 style='text-align:center;'><i class='fas fa-industry'></i> GDP</h6>"),
           hr(color="black"),
           fluidRow(
             infoBoxOutput ("gdpKPI", width = 6),
             infoBoxOutput("gdpKPI2", width = 6)
           ),
           span(div(textOutput("current_gdp_YearOutput"), class="kpiHeader"),
                div(textOutput("cite_B1GMHOutput"), class = "sourceCite"))
    ),
    column(2,
           HTML("<h6 style='text-align:center;'><i class='fas fa-wallet'></i> INFLATION</h6>"),
           hr(color="black"),
           fluidRow(
             infoBoxOutput("inflationKPI2", width = 4),
             infoBoxOutput ("inflationKPI", width = 4),
             infoBoxOutput("inflationKPI3", width = 4)
           ),
           div(textOutput("current_infl_MonthOutput"), class="kpiHeader"),
           div( textOutput("cite_unemployOutput"), class = "sourceCite")
    ),
    column(3,
           HTML("<h6 style='text-align:center;'><i class='fas fa-globe'></i> EXPORTS AND IMPORTS</h6>"),
           hr(color="black"),
           fluidRow(
             infoBoxOutput ("exportKPI", width = 4),
             infoBoxOutput("importKPI", width = 4),
             infoBoxOutput("netexportKPI", width = 4)
           ),
           div(textOutput("current_bop_MonthOutput"), class="kpiHeader"),
           div( textOutput("cite_bopOutput"), class = "sourceCite")
    ),
    column(3,
           HTML("<h6 style='text-align:center;'><i class='fas fa-user'></i> LABOUR</h6>"),
           hr(color="black"),
           fluidRow(
             infoBoxOutput("employmentKPI"),
             infoBoxOutput("unemploymentKPI"),
             infoBoxOutput("activityKPI")
           ),
           div(textOutput("current_labour_MonthOutput"), class="kpiHeader"),
           div( textOutput("cite_labourOutput"), class = "sourceCite")
           
    ),
    column(2,
           HTML("<h6 style='text-align:center;'><i class='fas fa-thin fa-virus'></i> COVID-19</h6>"),
           hr(color="black"),
           fluidRow(
             infoBoxOutput("covidcaseKPI", width = 6),
             infoBoxOutput("covidtestKPI", width = 6)
           ),
           div(textOutput("current_covid_MonthOutput"), class="kpiHeader"),
           div( textOutput("cite_covidOutput"), class = "sourceCite")
    )
  ),
  fluidRow(
    #column(12, hr(color="black"))
  ),
  fluidRow(
    column(6,
           fluidRow(
             column(6,
                    HTML("<h6 style='text-align:center;'>GDP TREND</h6>"),
                    hr(color="black"),
                    plotOutput("gdpPlot", height = "300px") %>% withSpinner(color="black"),
                    div( textOutput("cite_B1GMHOutput2"), class = "sourceCite")
             ),
             column(6,
                    HTML("<h6 style='text-align:center;'>GOVERNMENT DEBT & DEFICIT</h6>"),
                    hr(color="black"),
                    plotOutput("debtPlot", height = "300px") %>% withSpinner(color="black"),
                    div( textOutput("cite_debtOutput"), class = "sourceCite")
             )
           ),
           fluidRow(
             column(4,
                    HTML("<h6 style='text-align:center;'>UNEMPLOYMENT RATE</h6>"),
                    hr(color="black"),
                    plotOutput("unemploymentPlot", height = "300px") %>% withSpinner(color="black"),
                    div( textOutput("cite_unemployOutput2"), class = "sourceCite")
             ),
             column(4,
                    HTML("<h6 style='text-align:center;'>EXPORTS BY COUNTRY</h6>"),
                    hr(color="black"),
                    plotOutput("exportsByCountryPlot", height = "300px") %>% withSpinner(color="black"),
                    div( textOutput("cite_bopOutput2"), class = "sourceCite")
             ),
             column(4,
                    HTML("<h6 style='text-align:center;'>EXPORTS BY PRODUCT</h6>"),
                    hr(color="black"),
                    plotOutput("exportsByIndustryPlot", height = "300px") %>% withSpinner(color="black"),
                    div( textOutput("cite_exportsByIndustry"), class = "sourceCite")
             )
           )
    ),
    column(6,
           HTML("<h6 style='text-align:center;'>INFLATION TREND BY PRODUCT</h6>"),
           hr(color="black"),
           plotOutput("hyodykePlot", height = "750px") %>% withSpinner(color="black"),
           div( textOutput("cite_current_inflMonthOutput"), class = "sourceCite")
    )
  ),
  
  fluidRow(
    column(12,
           HTML("<div class='pxwebcite'>Sources:</div>"),
           HTML("<div class='pxwebcite'>https://sampo.thl.fi/pivot/prod/fi/epirapo/covid19case/fact_epirapo_covid19case</div>"),
           HTML("<div class='pxwebcite'>https://pxnet2.stat.fi/PXWeb/pxweb/fi/</div>"),
           HTML("<div class='pxwebcite'>Mans Magnusson, Markus Kainu, Janne Huovari, and Leo Lahti (rOpenGov).  pxweb: R tools for PXWEB API.  URL: http://github.com/ropengov/pxweb</div>"),
           HTML("<div class='pxwebcite'>Data is licensed under: Attribution 4.0 International (CC BY 4.0)</div>")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  thematic_shiny(font = "auto")
  
  # Helper to check if new data present ----------------
  fShouldUpdate <- function(pxdataset) {
    object = deparse(substitute(pxdataset))
    Updated <- pxdataset[["metadata"]][[1]][["updated"]]
    df.test <- as.data.frame(Updated)
    df.old.value <- read_csv2(paste0(object, ".csv"))
    if (df.test$Updated > df.old.value$Updated) {
      write_csv2(df.test, paste0(object, ".csv"))
      return(TRUE)
    }
    else
      return(FALSE)
  }
  
  ####################
  ### Unemployment ###
  ####################
  pxweb_query_list.unemploy.test <- list("unit"=c("PC_ACT"), "geo"=c("FI"), "time"=c("2020"))
  px_data.unemploy.test <- pxweb_get(url = "https://pxnet2.stat.fi/PXWeb/api/v1/en/Eurostat/tym/tps00203.px", query = pxweb_query_list.unemploy.test)
  test.unemploy <- fShouldUpdate(px_data.unemploy.test)
  
  if (test.unemploy == TRUE) {
    pxweb_query_list.unemploy <- list("unit"=c("PC_ACT"), "geo"=c("EA19","EU27_2020","FI"), "time"=c("*"))
    px_data.unemploy <- pxweb_get(url = "https://pxnet2.stat.fi/PXWeb/api/v1/en/Eurostat/tym/tps00203.px", query = pxweb_query_list.unemploy)
    df.unemploy <- as.data.frame(px_data.unemploy, column.name.type = "text", variable.value.type = "text")
    df.unemploy <- mutate(
      df.unemploy,
      Date = as.Date.character(paste(time, "-01", "-01", sep=""))
    )
    write_csv2(df.unemploy, "df.unemploy.csv")
  }
  else {
    df.unemploy <- read_csv2("df.unemploy.csv")
  }
  
  cite.unemploy <- px_data.unemploy.test$metadata[[1]]$source
  
  ###########################
  ### Balance of Payments ###
  ###########################
  pxweb_query_list.bop.test <- list("Kuukausi"=c("2021M12"), "Maksutase-er\U00E4"=c("CA"), "Tiedot"=c("C","D"))
  px_data.bop.test <- pxweb_get(url = "https://pxnet2.stat.fi/PXWeb/api/v1/en/StatFin/kan/mata/kk/statfin_mata_pxt_12gf.px", query = pxweb_query_list.bop.test)
  test.bop <- fShouldUpdate(px_data.bop.test)
  
  if (test.bop == TRUE) {
    pxweb_query_list.bop <- list("Kuukausi"=c("*"), "Maksutase-er\U00E4"=c("CA","GS","G","S","IN1","D1","D4P","D4O","IN2","KA"), "Tiedot"=c("C","D"))
    px_data.bop <- pxweb_get(url = "https://pxnet2.stat.fi/PXWeb/api/v1/en/StatFin/kan/mata/kk/statfin_mata_pxt_12gf.px", query = pxweb_query_list.bop)
    df.bop <- as.data.frame(px_data.bop, column.name.type = "text", variable.value.type = "text")
    df.bop <- df.bop %>%
      filter(
        `BOP item` == "CA Current account"
      ) %>%
      mutate(
        Income = `Income, millions of euro`,
        Expenditure = `Expenditure, millions of euro`,
        Net =  `Income, millions of euro` - `Expenditure, millions of euro`,
        Date = as.Date.character(paste0(sub("M", "-", Month), "-01"))
      )
    write_csv2(df.bop, "df.bop.csv")
  }
  else {
    df.bop <- read_csv2("df.bop.csv")
  }
  
  euro <- dollar_format(
    prefix = "",
    suffix = "\u20ac",
    big.mark = ".",
    decimal.mark = ","
  )
  
  current_bop <- df.bop %>%
    filter(
      Date == max(Date)
    )
  cite.bop <- px_data.bop.test$metadata[[1]]$source
  
  ###########################
  ### Exports by country  ###
  ###########################
  pxweb_query_list.exp_country.test <- list("Maksutase-er\U00E4"=c("GS"), "Maa"=c("EUR"), "Vuosinelj\U00E4nnes"=c("2021Q3"), "Tiedot"=c("C"))
  px_data.exp_country.test <- pxweb_get(url = "https://pxnet2.stat.fi/PXWeb/api/v1/en/StatFin/kan/mata/nj/statfin_mata_pxt_12gg.px", query = pxweb_query_list.exp_country.test)
  test.exp_country <- fShouldUpdate(px_data.exp_country.test)
  
  if(test.exp_country == TRUE) {
    pxweb_query_list.exp_country <- list("Maksutase-er\U00E4"=c("GS"), "Maa"=c("*"), "Vuosinelj\U00E4nnes"=c("*"), "Tiedot"=c("C"))
    px_data.exp_country <- pxweb_get(url = "https://pxnet2.stat.fi/PXWeb/api/v1/en/StatFin/kan/mata/nj/statfin_mata_pxt_12gg.px", query = pxweb_query_list.exp_country)
    df.exp_country <- as.data.frame(px_data.exp_country, column.name.type = "text", variable.value.type = "code")
    df.exp_country <- df.exp_country %>%
      mutate(
        Year = substr(Quarter, 1, 4),
        IsNumeric = as.integer(Country)
      ) %>%
      filter(
        Quarter == max(Quarter)
      )
    df.exp_country <- df.exp_country %>%
      filter(
        df.exp_country$IsNumeric != "NA"
      )
    df.exp_country <- top_n(df.exp_country, n = 10, wt = df.exp_country$`Income, millions of euro`)
    df.exp_country <- df.exp_country %>%
      inner_join(as.data.frame(px_data.exp_country$pxweb_metadata[[2]][[2]]), by = c("Country" = "values"))
    write_csv2(df.exp_country, "df.exp_country.csv")
  }
  else {
    df.exp_country <- read_csv2("df.exp_country.csv")
  }
  
  cite.exp_country <- px_data.exp_country.test$metadata[[1]]$source
  
  ###################################
  ### Exports: Goods vs. Services ###
  ###################################
  pxweb_query_list.exportBranch.test <- list("Palveluer\U00E4"=c("G"), "Alue"=c("ULK"), "Vuosinelj\U00E4nnes"=c("2021Q3"), "Tiedot"=c("C"))
  px_data.exportBranch.test <- pxweb_get(url = "https://pxnet2.stat.fi/PXWeb/api/v1/en/StatFin/kan/tpulk/statfin_tpulk_pxt_12gq.px", query = pxweb_query_list.exportBranch.test)
  test.exportBranch <- fShouldUpdate(px_data.exportBranch.test)
  
  if(test.exportBranch == TRUE) {
    pxweb_query_list.exportBranch <- list("Palveluer\U00E4"=c("*"), "Alue"=c("ULK"), "Vuosinelj\U00E4nnes"=c("*"), "Tiedot"=c("C","D"))
    px_data.exportBranch <- pxweb_get(url = "https://pxnet2.stat.fi/PXWeb/api/v1/en/StatFin/kan/tpulk/statfin_tpulk_pxt_12gq.px", query = pxweb_query_list.exportBranch)
    df.exportBranch <- as.data.frame(px_data.exportBranch, column.name.type = "text", variable.value.type = "text") %>%
      mutate(
        code = trimws(substr(`Service item`, 1, str_locate(`Service item`, " "))),
        item = trimws(substr(`Service item`, str_locate(`Service item`, " "), length(`Service item`))),
        Year = substr(Quarter, 1, 4),
      ) %>%
      filter(
        code %in% c("G", "S")
      )
    write_csv2(df.exportBranch, "df.exportBranch.csv")
  }
  else {
    df.exportBranch <- read_csv2("df.exportBranch.csv")
  }
  cite.exportBranch <- px_data.exportBranch.test$metadata[[1]]$source
  
  ################
  ### S13 Debt ###
  ################
  pxweb_query_list.debt.test <- list("Vuosi"=c("2018"), "Sektori"=c("S13"), "Tiedot"=c("*"))
  px_data.debt.test <- pxweb_get(url = "https://pxnet2.stat.fi/PXWeb/api/v1/en/StatFin/jul/jali/statfin_jali_pxt_122g.px", query = pxweb_query_list.debt.test)
  test.px_data.debt <- fShouldUpdate(px_data.debt.test)
  
  if (test.px_data.debt == TRUE) {
    pxweb_query_list.debt <- list("Vuosi"=c("*"), "Sektori"=c("*"), "Tiedot"=c("*"))
    px_data.debt <- pxweb_get(url = "https://pxnet2.stat.fi/PXWeb/api/v1/en/StatFin/jul/jali/statfin_jali_pxt_122g.px", query = pxweb_query_list.debt)
    df.debt <- as.data.frame(px_data.debt, column.name.type = "text", variable.value.type = "text") %>%
      filter(
        Sector == "S13 General government"
      ) %>%
      mutate(
        Date = as.Date.character(paste0( substr(Year, 1, 4), "-01", "-01"))
      )
    write_csv2(df.debt, "df.debt.csv")
  }
  else {
    df.debt <- read_csv2("df.debt.csv")
  }
  cite.debt = px_data.debt.test$metadata[[1]]$source
  
  #################
  ### GDP B1GMH ###
  #################
  pxweb_query_list.B1GMH2.test <- list("Taloustoimi"=c("B1GMH"), "Vuosi"=c("2019"), "Tiedot"=c("cp"))
  px_data.B1GMH2.test <- pxweb_get(url = "https://pxnet2.stat.fi/PXWeb/api/v1/en/StatFin/kan/vtp/statfin_vtp_pxt_11sf.px", query = pxweb_query_list.B1GMH2.test)
  test.px_data.B1GMH <- fShouldUpdate(px_data.B1GMH2.test)
  
  if (test.px_data.B1GMH == TRUE) {
    pxweb_query_list.B1GMH2 <- list("Taloustoimi"=c("B1GMH"), "Vuosi"=c("*"), "Tiedot"=c("*"))
    px_data.B1GMH2 <- pxweb_get(url = "https://pxnet2.stat.fi/PXWeb/api/v1/en/StatFin/kan/vtp/statfin_vtp_pxt_11sf.px", query = pxweb_query_list.B1GMH2)
    df.B1GMH2 <- as.data.frame(px_data.B1GMH2, column.name.type = "text", variable.value.type = "text")
    df.B1GMH2 <- df.B1GMH2 %>%
      mutate(
        Date = as.Date.character(paste0( substr(Year, 1, 4), "-01", "-01"))
      )
    write_csv2(df.B1GMH2, "df.B1GMH2.csv")
  }
  else {
    df.B1GMH2 <- read_csv2("df.B1GMH2.csv")
  }
  current_gdp <- df.B1GMH2 %>%
    filter(
      Date == max(Date)
    )
  cite.B1GMH2 <- px_data.B1GMH2.test$metadata[[1]]$source
  
  #################
  ### Inflation ###
  #################
  pxweb_query_list.test <- list("Kuukausi"=c("*"), "Tiedot"=c("*"))
  Hyodyke.test <- paste0("Hy", "\U00F6", "dyke")
  pxweb_query_list.test[[Hyodyke.test]] <- c("0")
  px_data.inflation.test <- pxweb_get(url = "https://pxnet2.stat.fi/PXWeb/api/v1/en/StatFin/hin/khi/kk/statfin_khi_pxt_11xd.px", query = pxweb_query_list.test)
  test.inflation <- fShouldUpdate(px_data.inflation.test)
  
  if (test.inflation == TRUE) {
    pxweb_query_list <- list("Kuukausi"=c("*"), "Tiedot"=c("*"))
    # UTF-8 conversion for scandics
    Hyodyke <- paste("Hy", "\U00F6", "dyke", sep = "")
    pxweb_query_list[[Hyodyke]] <- c("*")
    px_data <- pxweb_get(url = "https://pxnet2.stat.fi/PXWeb/api/v1/en/StatFin/hin/khi/kk/statfin_khi_pxt_11xd.px", query = pxweb_query_list)
    px.df <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
    px.df <- rename(px.df, Hyodyke_new = Commodity)
    px.df <- px.df %>%
      mutate(
        Date = as.Date.character(paste(sub("M", "-", Month), "-01", sep = ""))
      ) %>%
      filter(
        !grepl("\\.", Hyodyke_new)
      )
    write_csv2(px.df, "df.inflation.csv")
  }
  else {
    px.df <- read_csv2("df.inflation.csv")
  }
  
  cite <- paste("Source:", px_data.inflation.test$metadata[[1]]$source)
  current_infl <- px.df %>%
    filter(
      Month == max(Month) & Hyodyke_new == "0 CONSUMER PRICE INDEX"
    )
  px.df <- px.df %>%
    filter(
      Hyodyke_new != "0 CONSUMER PRICE INDEX" & !is.na(`Annual change (%)`)
    )
  
  prods <- as.factor(px.df$Hyodyke_new)
  title <- paste("Inflation by product", sep = "")
  ytitle <- paste("Hy", "\U00F6", "dykkeet", sep = "")
  cite.infl <- px_data.inflation.test$metadata[[1]]$source
  
  ############
  ## LABOUR ##
  ############    
  pxweb_query_list.labour.test <- list("Vuosi"=c("2021"), "Sukupuoli"=c("SSS"), "Suuralue 2012"=c("SSS"), "Tiedot"=c("Vaesto","Tyovoima","Tyolliset","Tyottomat","Tyov_kuulumattomat","tyovoimaosuus","Tyollisyysaste_15_64","Tyottomyysaste"))
  px_data.labour.test <- pxweb_get(url = "https://pxnet2.stat.fi/PXWeb/api/v1/en/StatFin/tym/tyti/vv/statfin_tyti_pxt_13ak.px", query = pxweb_query_list.labour.test)
  test.labour <- fShouldUpdate(px_data.labour.test)
  
  if(test.labour) {
    pxweb_query_list.labour <- list("Vuosi"=c("*"), "Sukupuoli"=c("SSS"), "Suuralue 2012"=c("SSS"), "Tiedot"=c("Vaesto","Tyovoima","Tyolliset","Tyottomat","Tyov_kuulumattomat","tyovoimaosuus","Tyollisyysaste_15_64","Tyottomyysaste"))
    px_data.labour <- pxweb_get(url = "https://pxnet2.stat.fi/PXWeb/api/v1/en/StatFin/tym/tyti/vv/statfin_tyti_pxt_13ak.px", query = pxweb_query_list.labour)
    df.labour <- as.data.frame(px_data.labour, column.name.type = "text", variable.value.type = "text")
    df.labour <- df.labour %>%
      filter(
        df.labour$Year == max(Year)
      )
    write_csv2(df.labour, "df.labour.csv")
  }
  else {
    df.labour <- read_csv2("df.labour.csv")
  }
  
  cite.labour <- px_data.labour.test$metadata[[1]]$source
  
  ############
  ## COVID  ##
  ############
  url_base <- "https://sampo.thl.fi/pivot/prod/fi/epirapo/covid19case/fact_epirapo_covid19case.json"
  #request <- "?row=dateweek20200101-509030&row=hcdmunicipality2020-445222&column=measure-444833.445356.492118.445344.&fo=1&row=445268L"
  request <- "?row=dateweek20200101-509030&column=measure-444833.445356.492118.445344.&fo=1"
  url <- paste0(url_base, request)
  cube <- fromJSONstat(url, naming = "label", use_factors = F, silent = T)
  res <- cube[[1]]
  df.covid <- res %>%
    filter(
      res$dateweek20200101 == max(res$dateweek20200101)
      #res$dateweek20200101 == max(res$dateweek20200101) & trimws(res$hcdmunicipality2020) == "Kaikki Alueet" & trimws(res$hcdmunicipality20201) == "Yhteens\U00E4"
    ) %>%
    group_by(
      dateweek20200101,
      measure
    )
  cases <- paste0("Tapausten lukum", "\U00E4\U00E4r\U00E4")
  tests <- paste0("Testausm", "\U00E4\U00E4r\U00E4")
  df.covid.cases <- df.covid %>%
    filter(
      trimws(measure) == cases
    )
  df.covid.tests <- df.covid %>%
    filter(
      measure == tests
    )
  
  #########
  ## MAP ##
  #########
  
  finmap <- get_municipalities(year = 2020)
  finmap <- finmap %>%
    transmute(
      geom = finmap$geom,
      vuosi = finmap$vuosi
    )
  
  
  ###################################################################################################
  ###########                           OUTPUTS                                           ###########
  ###################################################################################################
  
  
  output$hyodykePlot <- renderPlot({
    ggplot(px.df, aes(Date, px.df$`Annual change (%)`)) +
      geom_area(alpha = 0.5) +
      geom_point(size=1, colour="black", alpha=0.5) + 
      geom_smooth(na.rm = TRUE, color="black", span=1.0) +
      facet_wrap(~Hyodyke_new, ncol = 3) +
      theme_void() +
      theme(
        legend.position = "none",
        panel.border = element_rect(colour = "grey70", fill = NA),
        panel.grid.major = element_line(colour = "grey70"),
        strip.text = element_text(size = 12),
        title = element_text(face = "italic"),
        axis.text.x = element_text(
          size = 12,
          hjust = 1
        ),
        axis.text.y = element_text(
          vjust = 0.5,
          size = 12
        )
      ) +
      scale_y_continuous(expand = c(0,0), labels = scales::number_format(accuracy = 1, suffix = " %")) +
      scale_x_date(expand = c(0,0)) +
      ylab(ytitle)
  })
  
  output$gdpPlot <- renderPlot({
    ggplot(df.B1GMH2, aes(Date, `Volume series, reference yea 2020`)) +
      geom_area(alpha = 0.5) +
      geom_point(size=2, colour="black", alpha=0.5) + 
      geom_text(
        aes(y = df.B1GMH2$`Volume series, reference yea 2020`),
        vjust = -1,
        size = 3,
        check_overlap = T,
        label = round(df.B1GMH2$`Volume series, reference yea 2020` / 1000, 0)
      ) +
      theme_void() +
      ggtitle("GDP volume, reference year 2020") +
      theme(
        plot.title = element_text(
          vjust = 0.7,
          hjust = 0.1
        ),
        panel.grid.major = element_line(colour = "grey70"),
        strip.text = element_text(size = 12),
        axis.text.x = element_text(
          angle = 45,
          size = 12,
          hjust = 1
        ),
        axis.text.y = element_text(
          vjust = 1,
          size = 12
        )
      ) +
      coord_cartesian(clip = "on") +
      scale_y_continuous(expand = c(0.1,0.1), limits = c(0, 300000), labels = scales::number_format(big.mark = " ", accuracy = 1, suffix = " M\U20ac")) +
      scale_x_date(expand = c(0,0))
  })
  
  output$unemploymentPlot <- renderPlot({
    ggplot(df.unemploy, aes(df.unemploy$Date, df.unemploy$`Total unemployment rate`, shape=factor(df.unemploy$geo) )) +
      geom_line() +
      geom_point(color="black", fill="grey70", size=4) +
      theme_void() +
      theme(
        legend.position = c(0.025, 0.4),
        legend.justification = c(0, 1),
        legend.direction = "vertical",
        legend.title = element_blank(),
        panel.grid.major = element_line(colour = "grey70"),
        strip.text = element_text(size = 12),
        title = element_text(face = "italic"),
        axis.text.x = element_text(
          size = 12,
          angle = 45,
          hjust = 1
        ),
        axis.text.y = element_text(
          vjust = 1,
          size = 12
        )
      ) +
      scale_color_manual(values = c("black", "grey20", "grey80")) +
      scale_fill_grey() +
      scale_y_continuous(expand = c(0.1,0.1), limits = c(0, 15), labels = scales::number_format(accuracy = 1, suffix = " %")) +
      scale_x_date(expand = c(0,0))
  })
  
  output$exportsByCountryPlot <- renderPlot({
    ggplot(df.exp_country, aes(x = reorder(df.exp_country$valueTexts, df.exp_country$`Income, millions of euro` ), y = df.exp_country$`Income, millions of euro`)) +
      geom_col() +
      geom_text(
        label = df.exp_country$`Income, millions of euro`,
        size = 3,
        check_overlap = TRUE,
        position = position_stack(vjust = 0.5),
        color = "white"
      ) +
      theme_void() +
      coord_flip() +
      ggtitle(df.exp_country$Quarter) +
      theme(
        plot.title = element_text(
          vjust = 0.7,
          hjust = 0.1
        ),
        #legend.title = element_blank(),
        panel.grid.major.x = element_line(colour = "grey70"),
        strip.text = element_text(size = 12),
        #title = element_text(face = "italic"),
        axis.text.x = element_text(
          vjust = 0.5,
          angle = 45,
          size = 12
        ),
        axis.text.y = element_text(
          vjust = 0.5,
          size = 12
        )
      ) +
      scale_y_continuous(expand = c(0,0), labels = scales::number_format(big.mark = " ", accuracy = 1, suffix = " M\U20ac"))
  })
  
  output$exportsByIndustryPlot <- renderPlot({
    ggplot(df.exportBranch, aes(x = Year, y=df.exportBranch$`Export, millions of euro`, fill = df.exportBranch$item, color = df.exportBranch$item)) +
      geom_bar(position="dodge", stat="identity") +
      theme_void() +
      scale_color_grey() +
      theme(
        legend.position = c(0.025, 0.975),
        legend.justification = c(0, 1),
        legend.direction = "horizontal",
        legend.title = element_blank(),
        panel.grid.major = element_line(colour = "grey70"),
        strip.text = element_text(size = 12),
        title = element_text(face = "italic"),
        axis.text.x = element_text(
          vjust = 0.5,
          angle = 45,
          size = 12
        ),
        axis.text.y = element_text(
          vjust = 1,
          size = 12
        )
      ) +
      scale_y_continuous(expand = c(0,0), labels = scales::number_format(big.mark = " ", accuracy = 1, suffix = " M\U20ac")) +
      scale_fill_grey()
  })
  
  output$debtPlot <- renderPlot({
    ggplot(df.debt, aes(x=Date)) +
      geom_bar(aes(y = df.debt$`EDP deficit (-) / EDP surplus (+), millions of euro`), stat="identity", fill="grey70", color="black", alpha=0.5) +
      geom_line(aes(y = df.debt$`EDP debt, ratio to GDP, %`*100, group = 1)) +
      geom_point(aes(y = df.debt$`EDP debt, ratio to GDP, %`*100)) +
      scale_y_continuous(
        expand = c(0,0),
        labels = scales::number_format(big.mark = " ", accuracy = 1, suffix = " M\U20ac"),
        name = "Deficit",
        sec.axis = sec_axis(~./100, name = "Debt % of GDP", labels = scales::number_format(big.mark = " ", accuracy = 1, suffix = " %"))
      ) +
      geom_text(
        aes(y = df.debt$`EDP debt, ratio to GDP, %`*100),
        vjust = -1,
        size = 3,
        label = df.debt$`EDP debt, ratio to GDP, %`,
        check_overlap = T,
      ) +
      theme_void() +
      theme(
        legend.position = c(0.025, 0.975),
        legend.justification = c(0, 1),
        legend.direction = "horizontal",
        #legend.title = element_blank(),
        panel.grid.major = element_line(colour = "grey70"),
        strip.text = element_text(size = 12),
        title = element_text(face = "italic"),
        axis.text.x = element_text(
          vjust = 0.5,
          angle = 45,
          size = 12
        ),
        axis.text.y = element_text(
          size = 12
        ),
        axis.title.y = element_text(
          size = 12,
          angle = 90,
          hjust = 1
        )
      )
  })
  
  output$finmapOutput <- renderPlot({
    plot(finmap, border = NA, col="black", main = NULL)
  })
  
  
  output$gdpKPI <- renderInfoBox({
    infoBox("", paste(format(round(current_gdp$`Current prices, millions of euro`, 0), big.mark = " "), " M\U20ac"), subtitle = "Current prices")
  })
  
  output$gdpKPI2 <- renderInfoBox({
    infoBox("", paste(current_gdp$`Changes in volume, %`, "%"), subtitle = "Annual change in volume")
  })
  
  output$inflationKPI <- renderInfoBox({
    infoBox("", current_infl$`Point figure`, subtitle = "Index")
  })
  
  output$inflationKPI2 <- renderInfoBox({
    infoBox("", paste(current_infl$`Annual change (%)`, "%"), subtitle = "Annual change")
  })
  
  output$inflationKPI3 <- renderInfoBox({
    infoBox("", paste(current_infl$`Monthly change (%)`, "%"), subtitle = "Monthly change")
  })
  
  output$exportKPI <- renderInfoBox({
    infoBox("", paste(format(current_bop$`Income, millions of euro`, big.mark = " "), "M\U20ac"), subtitle = "Exports")
  })
  
  output$importKPI <- renderInfoBox({
    infoBox("", paste(format(current_bop$`Expenditure, millions of euro`, big.mark = " "), "M\U20ac"), subtitle = "Imports")
  })
  
  output$netexportKPI <- renderInfoBox({
    infoBox("", paste(format(current_bop$Net, big.mark = " "), "M\U20ac"), subtitle = "Net exports")
  })
  
  output$covidcaseKPI <- renderInfoBox({
    infoBox("", format( as.integer(df.covid.cases$value), big.mark = " "), subtitle = "Cases")
  })
  
  output$covidtestKPI <- renderInfoBox({
    infoBox("", format(as.integer(df.covid.tests$value), 0, big.mark = " "), subtitle = "Tests")
  })
  
  output$employmentKPI <- renderInfoBox({
    infoBox("", paste(df.labour$`Employment rate, persons aged 15-64, %`, "%"), subtitle = "Employment rate")
  })
  
  output$unemploymentKPI <- renderInfoBox({
    infoBox("", paste(df.labour$`Unemployment rate, %`, "%"), subtitle = "Unemployment rate")
  })
  
  output$activityKPI <- renderInfoBox({
    infoBox("", paste(df.labour$`Activity rate, %`, "%"), subtitle = "Activity rate")
  })
  
  output$current_gdp_YearOutput <- renderText({current_gdp$Year})
  
  output$cite_B1GMHOutput <- renderText({cite.B1GMH2})
  
  output$cite_B1GMHOutput2 <- renderText({cite.B1GMH2})
  
  output$current_infl_MonthOutput <- renderText({current_infl$Month})
  
  output$cite_current_inflMonthOutput <- renderText({cite.infl})
  
  output$cite_unemployOutput <- renderText({cite.unemploy})
  
  output$cite_unemployOutput2 <- renderText({cite.unemploy})
  
  output$current_covid_MonthOutput <- renderText({df.covid.cases$dateweek20200101})
  
  output$cite_covidOutput <- renderText({ "Finnish National Infectious Diseases Register" })
  
  output$current_bop_MonthOutput <- renderText({ current_bop$Month })
  
  output$cite_bopOutput <- renderText({cite.bop})
  
  output$cite_bopOutput2 <- renderText({cite.bop})
  
  output$cite_exportsByIndustry <- renderText({cite.exportBranch})
  
  output$cite_debtOutput <- renderText({cite.debt})
  
  output$current_labour_MonthOutput <- renderText({ df.labour$Year })
  
  output$cite_labourOutput <- renderText({cite.labour})
  
}

# Run the application 
shinyApp(ui = ui, server = server)
