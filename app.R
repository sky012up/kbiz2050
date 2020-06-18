# Load packages
#install.packages(c("shiny","httpuv","shinythemes","dplyr","readxl","rvest","stringr"))
library(shiny)
library(shinythemes)
library(keras)
library(recommenderlab)
library(DT)

# Define UI
ui = shinyUI(fluidPage(

  titlePanel("인공신경망기반 이종 DB 연계를 통한 성장단계별 맞춤형지원정책추천서비스_V1 by_JSLEE"),

  sidebarLayout(

    sidebarPanel(

      fileInput("file1", "Import CSV File",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
        # Horizontal line ----
        tags$hr(),

        # Input: Checkbox if file has header ----
        p(strong("Input: Checkbox if file has header")),
        checkboxInput("header", "Header", TRUE),

        p(strong("Session info:")),
        p("R version 3.5.1 (2020-06-18)"),
        p("Platform: x86_64-w64-mingw32/x64 (64-bit)"),
        p("Running under: Windows 10 x64 (build 17134)"),
        p("Matrix products: default"),

        tags$br(),

        p(strong("locale:")),
        p("LC_COLLATE=Korean_Korea.949  LC_CTYPE=Korean_Korea.949    LC_MONETARY=Korean_Korea.949 LC_NUMERIC=C                
            LC_TIME = Korean_Korea.949 "),

        tags$br(),

        p(strong("attached base packages:")),
        p("stats     graphics  grDevices utils     datasets  methods   base     "),

        tags$br(),

        p(strong("other attached packages:")),
        p("DT_0.5  recommenderlab_0.2-3  registry_0.5  proxy_0.4-23  arules_1.6-2  Matrix_1.2-15  keras_2.2.4.1  shinythemes_1.1.2  shiny_1.4.0"),

        tags$br(),

        p(strong("loaded via a namespace (and not attached):")),
        p("Rcpp_0.12.18      whisker_0.3-2     magrittr_1.5      rtvs_1.0.0.0      xtable_1.8-4      lattice_0.20-38   R6_2.4.0          rlang_0.4.1       fastmap_1.0.1     tools_3.5.3       grid_3.5.3        irlba_2.3.2       crosstalk_1.0.0   htmltools_0.4.0   tfruns_1.4        yaml_2.2.0        digest_0.6.15     crayon_1.3.4      tensorflow_1.13.1    later_1.0.0       htmlwidgets_1.3   base64enc_0.1-3   promises_1.1.0    zeallot_0.1.0     mime_0.5          compiler_3.5.3    generics_0.0.2   reticulate_1.12   jsonlite_1.6      httpuv_1.5.2"),

        p(strong("JAVA_version : 8.xx")),

        p(strong("Made by : jslee"), a(strong("  (Mail)"), href = "mailto:jslee@kisti.re.kr")),

        tags$br()

      ),

      mainPanel(
                tabsetPanel(
                            tabPanel(title = "도입기", 
                                     h4("Table (0점 = 활용가능성 없음, 1점 = 활용가능성 낮음, 2점 = 활용가능성 높음)"),
                                     dataTableOutput("textDisplay1")
                                     ),

                            tabPanel(title = "성장기", 
                                     h4("Table (0점 = 활용가능성 없음, 1점 = 활용가능성 낮음, 2점 = 활용가능성 높음)"),
                                     dataTableOutput("textDisplay2")
                                     ),

                            tabPanel(title = "성숙기", 
                                     h4("Table (0점 = 활용가능성 없음, 1점 = 활용가능성 낮음, 2점 = 활용가능성 높음)"),
                                     dataTableOutput("textDisplay3")
                                     ),

                            tabPanel(title = "쇠퇴기", 
                                     h4("Table (0점 = 활용가능성 없음, 1점 = 활용가능성 낮음, 2점 = 활용가능성 높음)"),
                                     dataTableOutput("textDisplay4")
                                     )
                            )
                 )
                


          )
        )
      )

# Define server function
server = function(input, output) {

    mydata = reactive({
                    req(input$file1)
                    if (is.null(input))
                        return("Please import the data")

                    query = read.csv(input$file1$datapath, header = input$header)
                    
                    #01_산업분류
                    ind.kbiz = as.numeric(substr(query$KSIC, 1, 2))

                    kbizInd = which(ind.kbiz == 10 | ind.kbiz == 11 | ind.kbiz == 13 | ind.kbiz == 14 | ind.kbiz == 15 | ind.kbiz == 16 |
                                    ind.kbiz == 17 | ind.kbiz == 18 | ind.kbiz == 19 | ind.kbiz == 20 | ind.kbiz == 21 | ind.kbiz == 22 |
                                    ind.kbiz == 23 | ind.kbiz == 24 | ind.kbiz == 25 | ind.kbiz == 26 | ind.kbiz == 27 | ind.kbiz == 28 |
                                    ind.kbiz == 29 | ind.kbiz == 30 | ind.kbiz == 31 | ind.kbiz == 32 | ind.kbiz == 33)
                    query = query[kbizInd,]

                    #03_설립연도
                    year.kbiz = query$YEAR

                    #04_기업유형.벤처
                    type.venture.kbiz = as.numeric(query$SEP2)
                    type.venture.kbiz[which(is.na(type.venture.kbiz) == T)] = 0

                    #05_기업유형.기술혁신형
                    type.inno.kbiz = as.numeric(query$SEP1)
                    type.inno.kbiz[which(is.na(type.inno.kbiz) == T)] = 0

                    #06_기업유형.경영혁신형
                    type.main.kbiz = as.numeric(query$SEP3)
                    type.main.kbiz[which(is.na(type.main.kbiz) == T)] = 0

                    #07_주력판매처.민간 (60 이상)
                    query$market.private.kbiz = 0
                    query$market.private.kbiz[which((query$SEP6 + query$SEP7) >= 60)] = 1
                    market.private.kbiz = query$market.private.kbiz

                    #08_주력판매처.공공 (60 이상)
                    query$market.public.kbiz = 0
                    query$market.public.kbiz[which(query$SEP9 >= 60)] = 1       
                    market.public.kbiz = query$market.public.kbiz

                    #09_주력판매처.일반소비자
                    query$market.customer.kbiz = 0
                    query$market.customer.kbiz[which(query$SEP8 >= 60)] = 1
                    market.customer.kbiz = query$market.customer.kbiz

                    #10_주력판매처.해외
                    query$market.global.kbiz = 0
                    query$market.global.kbiz[which(query$SEP10 >= 60)] = 1
                    market.global.kbiz = query$market.global.kbiz

                    #11_정보원천.내부
                    query$inform.inner.kbiz = 0
                    query$inform.inner.kbiz[which(query$A4S1 == 3 | query$A4S2 == 3 | query$A4S3 == 3)] = 1
                    infrom.inner.kbiz = query$inform.inner.kbiz

                    #12_정보원천.컨퍼런스
                    query$inform.conference.kbiz = 0
                    query$inform.conference.kbiz[which(query$A4S1 == 1 | query$A4S2 == 1 | query$A4S3 == 1)] = 1
                    infrom.conference.kbiz = query$inform.conference.kbiz

                    #13_정보원천.전문저널
                    query$inform.journal.kbiz = 0
                    query$inform.journal.kbiz[which(query$A4S1 == 2 | query$A4S2 == 2 | query$A4S3 == 2)] = 1
                    infrom.journal.kbiz = query$inform.journal.kbiz

                    #14_정보원천.경쟁자
                    query$inform.competer.kbiz = 0
                    query$inform.competer.kbiz[which(query$A4S1 == 4 | query$A4S2 == 4 | query$A4S3 == 4)] = 1
                    infrom.competer.kbiz = query$inform.competer.kbiz

                    #15_정보원천.민간서비스업체
                    query$inform.private.kbiz = 0
                    query$inform.private.kbiz[which(query$A4S1 == 6 | query$A4S2 == 6 | query$A4S3 == 6)] = 1
                    infrom.private.kbiz = query$inform.private.kbiz

                    #16_정보원천.공급업체
                    query$inform.supply.kbiz = 0
                    query$inform.supply.kbiz[which(query$A4S1 == 5 | query$A4S2 == 5 | query$A4S3 == 5)] = 1
                    infrom.supply.kbiz = query$inform.supply.kbiz

                    #17_정보원천.고객
                    query$inform.customer.kbiz = 0
                    query$inform.customer.kbiz[which(query$A4S1 == 7 | query$A4S2 == 7 | query$A4S3 == 7)] = 1
                    infrom.customer.kbiz = query$inform.customer.kbiz

                    #18_정보원천.출연연
                    query$inform.gri.kbiz = 0
                    query$inform.gri.kbiz[which(query$A4S1 == 9 | query$A4S2 == 9 | query$A4S3 == 9)] = 1
                    infrom.gri.kbiz = query$inform.gri.kbiz

                    #19_기술협력파트너.출연연 #NOTE:kbiz17는 매우만족이 1 매우 불만족이 5
                    query$partner.gri.kbiz = 0
                    query$partner.gri.kbiz[which(query$A3Q2 == 1 | query$A3Q2 == 2 | query$A3Q2 == 3)] = 1
                    partner.gri.kbiz = query$partner.gri.kbiz

                    #20_기술협력파트너.대학
                    query$partner.univ.kbiz = 0
                    query$partner.univ.kbiz[which(query$A3Q1 == 1 | query$A3Q1 == 2 | query$A3Q1 == 3)] = 1
                    partner.univ.kbiz = query$partner.univ.kbiz

                    #21_기술협력파트너.민간연구소
                    query$partner.pri.kbiz = 0
                    query$partner.pri.kbiz[which(query$A3Q3 == 1 | query$A3Q3 == 2 | query$A3Q3 == 3)] = 1
                    partner.pri.kbiz = query$partner.pri.kbiz

                    #22_연구개발비(연속형)
                    rnd.total.kbiz = query$C1S1

                    #23_내부R&D비중(연속형)
                    rnd.innerPortion.kbiz = round((query$C1S2 / (query$C1S2 + query$C1S7)) * 100, 0)
                    rnd.innerPortion.kbiz[which(is.nan(rnd.innerPortion.kbiz) == T)] = 0

                    #24_자금조달.자체조달(주로)
                    self = query$C1S7
                    gov = query$C1S8 + query$C1S9
                    pri = query$C1S10 + query$C1S11 + query$C1S12


                    funding = list()
                        for (i in 1:nrow(query)) {
                        funding[[i]] = which.max(cbind(self, gov, pri)[i,])
                        }
                        funding.result = do.call(rbind, funding)

                    query$funding.self.kbiz = 0
                    query$funding.self.kbiz[which(funding.result == 1)] = 1
                    funding.self.kbiz = query$funding.self.kbiz

                    #25_자금조달.정부재원(주로)
                    query$funding.gov.kbiz = 0
                    query$funding.gov.kbiz[which(funding.result == 2)] = 1
                    funding.gov.kbiz = query$funding.gov.kbiz

                    #26_자금조달.민간재원(주로)
                    query$funding.private.kbiz = 0
                    query$funding.private.kbiz[which(funding.result == 3)] = 1
                    funding.private.kbiz = query$funding.private.kbiz

                    #27 종업원(연속형)
                    emp.kbiz = query$PEOTOT
                    emp.kbiz[is.na(emp.kbiz)] = 0

                    #28 매출액(연속형)
                    sales.kbiz = query$F2S1
                    sales.kbiz[is.na(sales.kbiz)] = 0

                    #29 수출액(연속형)
                    export.kbiz = query$F2S4
                    export.kbiz[is.na(export.kbiz)] = 0

                    zeroOneNorm = function(X_train_num) {
                        X_max = max(X_train_num)
                        X_min = min(X_train_num)
                        X_train_num_scaled = scale(X_train_num, center = X_min, scale = (X_max - X_min))
                        print(round(X_train_num_scaled, 5))
                    }

                    year.kbiz = cbind(
                                y1 = ifelse(year.kbiz < 2000, 1, 0),
                                y2 = ifelse(year.kbiz >= 2000 & year.kbiz < 2010, 1, 0),
                                y3 = ifelse(year.kbiz >= 2010, 1, 0))
                    #year.kbiz = year.kbiz[, - c(1)]

                    kbiz = cbind(keras::to_categorical(ind.kbiz[kbizInd]), year.kbiz, type.venture.kbiz, type.inno.kbiz, query[, as.numeric(ncol(query) - 17):ncol(query)], zeroOneNorm(emp.kbiz), zeroOneNorm(sales.kbiz), zeroOneNorm(export.kbiz))
                    kbiz = kbiz[, - which(colnames(kbiz) == "funding.self.kbiz")]
                    kbiz = kbiz[, - which(colnames(kbiz) == "partner.pri.kbiz")]

                    colnames(kbiz) = c("ksic1", "ksic2", "ksic3", "ksic4", "ksic5", "ksic6", "ksic7", "ksic8", "ksic9", "ksic10", "ksic11", "ksic12", "ksic13", "ksic14", "ksic15", "ksic16", "ksic17", "ksic18", "ksic19", "ksic20", "ksic21", "ksic22", "ksic23", "ksic24", "ksic25", "ksic26", "ksic27", "ksic28", "ksic29", "ksic30", "ksic31", "ksic32", "ksic33", "ksic34",
                    "y1", "y2", "y3", "type.venture", "type.inno", "market.private", "market.public", "market.customer", "market.global",
                    "inform.inner", "inform.conference", "inform.journal", "inform.competer", "inform.private", "inform.supply", "inform.customer", "inform.gri",
                    "partner.gri", "partner.univ", "funding.gov", "funding.private", "emp", "sales", "export")

                    kbizDf = kbiz

                    load("kisDf_prcomp.RData")

                    kbizDf_pc1 = zeroOneNorm(predict(kisDf_prcomp, newdata = kbizDf)[, 1])
                    kbizDf_pc2 = zeroOneNorm(predict(kisDf_prcomp, newdata = kbizDf)[, 2])
                    kbizDf_pc3 = zeroOneNorm(predict(kisDf_prcomp, newdata = kbizDf)[, 3])
                    kbizDf_pc4 = zeroOneNorm(predict(kisDf_prcomp, newdata = kbizDf)[, 4])
                    kbizDf_pc5 = zeroOneNorm(predict(kisDf_prcomp, newdata = kbizDf)[, 5])
                    kbizDf_pc6 = zeroOneNorm(predict(kisDf_prcomp, newdata = kbizDf)[, 6])
                    kbizDf_pc7 = zeroOneNorm(predict(kisDf_prcomp, newdata = kbizDf)[, 7])
                    kbizDf_pc8 = zeroOneNorm(predict(kisDf_prcomp, newdata = kbizDf)[, 8])
                    kbizDf_pcmat = as.matrix(cbind(kbizDf_pc1, kbizDf_pc2, kbizDf_pc3, kbizDf_pc4, kbizDf_pc5, kbizDf_pc6, kbizDf_pc7, kbizDf_pc8))
                    kbizDf_pcmat_raw = kbizDf_pcmat
                    rownames(kbizDf_pcmat_raw) = NULL
                    head(kbizDf_pcmat_raw)

                   #연계시스템
                    maxn = function(n) function(x) order(x, decreasing = TRUE)[n]

                    pred = list()
                    for (i in 1:7) {
                    cat("iteration: ", i, "/ 7 \n")
                    model = load_model_hdf5(paste0("Q31_", i, "(0409).h5"))
                    prediction = model %>% predict(kbizDf_pcmat_raw)
                    pred[[i]] = apply(prediction, 1, maxn(1)) - 1
                    }
                    pred.result = do.call(cbind, pred)

                    #성장단계 라벨링
                    grow.class = query$SEP5
                    mydata = cbind(grow.class, pred.result)
                    mydata

    })

  
    output$textDisplay1 = DT::renderDataTable({

        gclass01 = mydata()
        gclass01 = gclass01[which(gclass01[, 1] == 1), 2:ncol(gclass01)] #gclass[,1]==?
        model01 = readRDS("0421 rec01.rds") #rec0?.rds

        reScore = function(temp) {
            temp[which(temp == 2)] = NA
            temp[which(temp == 0)] = 2
            return(temp)
        }

        inputData01 = reScore(gclass01)
        r01 = as(inputData01, "realRatingMatrix")
        recom01 = predict(model01, r01, verbose = TRUE)
        colnames(recom01) = c("조세지원", "자금지원", "금융지원", "인력지원", "기술지원", "인증지원", "구매지원")
        RMSE = recommenderlab::RMSE(as(r01, "matrix"), as.matrix(recom01))
        result = cbind(round(RMSE, 2), round(recom01, 2))
        result
    })


    output$textDisplay2 = DT::renderDataTable({

        gclass02 = mydata()
        gclass02 = gclass02[which(gclass02[, 1] == 2), 2:ncol(gclass02)] #gclass[,1]==?
        model02 = readRDS("0421 rec02.rds") #rec0?.rds

        reScore = function(temp) {
            temp[which(temp == 2)] = NA
            temp[which(temp == 0)] = 2
            return(temp)
        }

        inputData02 = reScore(gclass02)
        r02 = as(inputData02, "realRatingMatrix")
        recom02 = predict(model02, r02, verbose = TRUE)
        colnames(recom02) = c("조세지원", "자금지원", "금융지원", "인력지원", "기술지원", "인증지원", "구매지원")
        RMSE = recommenderlab::RMSE(as(r02, "matrix"), as.matrix(recom02))
        result = cbind(round(RMSE, 2), round(recom02, 2))
        result
    })


    output$textDisplay3 = DT::renderDataTable({

        gclass03 = mydata()
        gclass03 = gclass03[which(gclass03[, 1] == 3), 2:ncol(gclass03)] #gclass[,1]==?
        model03 = readRDS("0421 rec03.rds") #rec0?.rds

        reScore = function(temp) {
            temp[which(temp == 2)] = NA
            temp[which(temp == 0)] = 2
            return(temp)
        }

        inputData03 = reScore(gclass03)
        r03 = as(inputData03, "realRatingMatrix")
        recom03 = predict(model03, r03, verbose = TRUE)
        colnames(recom03) = c("조세지원", "자금지원", "금융지원", "인력지원", "기술지원", "인증지원", "구매지원")
        RMSE = recommenderlab::RMSE(as(r03, "matrix"), as.matrix(recom03))
        result = cbind(round(RMSE, 2), round(recom03, 2))
        result
    })


    output$textDisplay4 = DT::renderDataTable({

        gclass04 = mydata()
        gclass04 = gclass04[which(gclass04[, 1] == 4), 2:ncol(gclass04)] #gclass[,1]==?
        model04 = readRDS("0421 rec04.rds") #rec0?.rds

        reScore = function(temp) {
            temp[which(temp == 2)] = NA
            temp[which(temp == 0)] = 2
            return(temp)
        }

        inputData04 = reScore(gclass04)
        r04 = as(inputData04, "realRatingMatrix")
        recom04 = predict(model04, r04, verbose = TRUE)
        colnames(recom04) = c("조세지원", "자금지원", "금융지원", "인력지원", "기술지원", "인증지원", "구매지원")
        RMSE = recommenderlab::RMSE(as(r04, "matrix"), as.matrix(recom04))
        result = cbind(round(RMSE, 2), round(recom04, 2))
        result
    })
}

# Create Shiny object
shinyApp(ui = ui, server = server)