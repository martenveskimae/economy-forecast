library(shiny)
library(ggplot2)
library(ggthemes)
library(plotly)
library(dplyr)
library(tibble)
library(reshape2)
library(tidyr)
library(lubridate)
library(lettercase)
library(readxl)
library(XML)
library(glmnet)
library(eurostat)
library(randomForest)
library(tseries)
library(forecast)

Sys.setlocale("LC_ALL", "UTF-8")

download.external.variables = function(){
  ########################################
  # 1. Konjunktuuriinstituudi lehelt Exceli failid, NB! Exceli ridade nimed erinevad eri failides
  ########################################

  # Leht kus failidele lingid
  url = "http://www.ki.ee/baromeetrid/baromeetrid.htm"
  doc = htmlParse(url)
  # Otsime lehelt kõik lingid
  links = xpathSApply(doc, "//a/@href")
  # Otsime linkidest kõik .xls lõpuga failid
  excel.files = links[grepl("*.xls", links)]
  
  # Jätame alles ainult need, mis meile huvi pakuvad
  wanted = c("industry","ehitus","kaubandus","teenind", "majandus")
  excel.files = excel.files[grepl(paste(wanted, collapse = "|"), excel.files)]

  # Vaatame, mis xls failid juba kaustas olemas on. Neid, mis olemas, enam uuesti ei tõmba
  files.in.folder = list.files(pattern = "xls")
  files.to.download = excel.files[!(excel.files %in% files.in.folder)]
  
  # Laeme alla need failid, mida veel ei ole
  if (length(files.to.download) > 0) {
    url = paste("http://www.ki.ee/baromeetrid/", files.to.download, sep = "")
    # Laeme alla
    lapply(seq_along(url),
           function(x) download.file(url[x], files.to.download[x], mode = "wb"))
  }

  # Loeme sisse soovitud Exceli failid
  names = data.frame(str_split_fixed(excel.files, "_", n = 2),
                     as.Date(paste0(readr::parse_number(excel.files),"01"), format = "%y%m%d"),
                     stringsAsFactors = F)
  colnames(names) = c("sector", "file", "end.date")

  for(i in 1:length(excel.files)) {
    file.name = excel.files[i]
    name = names$sector[i]
    if (!(name %in% wanted)) stop("Error when loading data")
    tmp.df = read_excel(file.name)
    assign(name, tmp.df)
  }

  # Ükshaaval failide puhastamine: jäta alles vajalikud read ja veerud alates 2003. aastast
  # NB! seda peab käsitsi kontrollima aeg-ajalt, et Konjunktuuriinstituut ei oleks muutnud veerge ega ridu

  ehitus =  ehitus[c(2,4:15), c(1,44:ncol(ehitus))]
  industry = industry[c(2:8,10:24), c(1,56:ncol(industry))]
  kaubandus = kaubandus[c(2:8), c(1,44:ncol(kaubandus))]
  teenind = teenind[c(2:7, 9:15), c(1,11:ncol(teenind))]
  majandus = majandus[c(2:7), c(1,48:ncol(majandus))]

  # Tsükliga teisendame
  # Veergudele õigete kuude nimede panemine
  start.date = as.Date("2003-01-01")

  for(i in 1:nrow(names)) {
    data = get(names$sector[i])
    end = names$end.date[i]
    
    # Paneme kuud veerunimedeks
    colnames(data) = c("indicator", as.character(seq.Date(start.date, end,"month")[1:(ncol(data) - 1)]))
    # Keerame teistpidi
    data = melt(data, id.vars = c("indicator"))
    
    # Teeme kuupäeva; sektori nimed hilisemaks ühendamiseks; nimede puhastamine;
    data = data %>%
      mutate(date = as.Date(variable),
             value = as.numeric(value),
             variable = NULL,
             sector = names$sector[i],
             indicator = gsub("[\n|*]","", indicator))
    
    data$indicator[is_lowercase(substr(data$indicator, 1,1))] = paste0("Piirab praegu: ", data$indicator[is_lowercase(substr(data$indicator, 1,1))])
    
    # Salvestame sama nime alla, mis on Konjunktuuriinstituudi failid
    assign(names$sector[i],data)
  }
  
  ki = plyr::ldply(names$sector, function(x){
    data = get(x)
    n = "Ehitus"
    if(x == "teenind") n = "Teeninuds"
    else if(x == "kaubandus") n = "Kaubandus"
    else if(x == "industry") n = "Tööstus"
    else if(x == "majandus") n = "Kogu majandus"
    
    data = data %>% mutate(indicator = paste(n,"-",indicator)) %>%
      select(indicator, value, date)
    
    return(data)
  })
  
  save(ki, file = "data/ki.Rda")

  ########################################################
  # 2. Eurostatist Eesti ja EU majanduse usaldusindikaatorite toomine;
  # Praegu olemas EU28 ja EA19
  ########################################################

  euconfind = get_eurostat("ei_bssi_m_r2", filters = list(geo = c("EE", "EU28", "SE", "FI", "LT", "LV")), stringsAsFactors = FALSE) %>%
    filter(!is.na(values), year(time)>=2003) %>%
    mutate(date = time,
           time = NULL,
           indic = factor(indic, labels=c("Construction","Consumer","Economy","Industry","Retail","Service")))
  
  save(euconfind, file = "data/euconfind.Rda")
 
  ########################################################
  # 3. Eurostatist Eesti ja EU harmoniseeritud hinnaindeks
  # Indeces for Estonia and EU28, 2005=100, COICOP=CP00 or all-items HICP
  ########################################################
  
  euprices = get_eurostat("prc_hicp_midx", filters = list(geo = c("EE", "EU28", "SE", "FI", "LT", "LV"), coicop=c("CP00"), unit=c("I05")), stringsAsFactors = FALSE) %>%
    filter(!is.na(values), year(time) >= 2003) %>%
    mutate(date = time,
           hicp = values) %>%
    select(geo, hicp, date) %>%
    group_by(geo) %>%
    mutate(d1hicp =(hicp/lag(hicp,1)-1)*100,
           d12hicp = (hicp/lag(hicp,12)-1)*100) %>%
    ungroup() %>%
    select(geo, date, d1hicp, d12hicp)
  
  save(euprices, file = "data/euprices.Rda")
  
  #################################
  # Facebook data
  #
  #################################
  library(Rfacebook)
  oauth = fbOAuth(app_id = 341802779572316, app_secret = "d5de35d0f7f60e4ec98cda6af83cef5d")
  me = getUsers("me", token=oauth, private_info = T)

  fbPageDf = getPage(405970410082, token = oauth, n=10000)

  fb.df = fbPageDf %>%
    mutate(date = as.Date(format(as.POSIXlt(created_time), "%Y-%m-%d")),
           yearMonth = format(date,"%Y-%m")) %>%
    group_by(date = as.Date(paste0(yearMonth,"-01"))) %>%
    summarise(posts = n(),
              likes = sum(likes_count),
              shares = sum(shares_count),
              comments = sum(comments_count))

  save(fb.df, file = "data/facebook.Rda")

}

needed = c("KMD.Rda", "euconfind.Rda", "euprices.Rda", "facebook.Rda", "ki.Rda")
present = list.files(path="data/", pattern = "Rda")
missing = needed[!(needed %in% present)]
# if(length(missing)!=0) download.external.variables()

load("data/ki.Rda")
load("data/euconfind.Rda")
load("data/euprices.Rda")
load("data/facebook.Rda")
load("data/KMD.Rda")

KMD = KMD %>% mutate(date = as.Date(paste0(aasta, "-", kuu, "-01")))
numeric.cols = sapply(KMD, is.numeric)

library(shinyjs)

jsCode = "shinyjs.moveBG = function(d){
            $('.shiny-notification-content').css('margin-left', d + '%');
          }"

ui = fluidPage(
  useShinyjs(),
  extendShinyjs(text = jsCode, functions = c("moveBG")),
  includeCSS("css/custom.css"),
  # HTML("<div style='width: 80%; margin: 0 auto;'><h2 style='letter-spacing: 15px;margin-left: 28px; margin-bottom:0px;'>ALMA</h2>
  #      <p style='font-size:10px; white-space: nowrap'>aegridade liigendmodelleerimise algoritm</p></div>"),
  h2("Testversioon"),
  sidebarLayout(
    sidebarPanel(
      h3("Prognoositav tunnus"),
      selectInput("sektor", label="Vali sektor", unique(KMD$tekst), selected = "Hoonete ehitus", multiple = F),
      selectInput("target", label="Vali prognoositav tunnus", colnames(KMD[,numeric.cols]), selected = "Maksustatavkaive", multiple = F),  
      sliderInput("h_int","Prognoosi pikkus kuudes", min = 2, max = 12, value = 6, step = 1),
      h4("Mudeli treeningperioodi valikud"),
      radioButtons("cvornotcv", "Meetod:",
                   choices = list("Oskan ise" = "noCV", "Ristvalideerimine" = "CV"), 
                   selected = "CV"),
      uiOutput("sliders"),
      actionButton("update", "Prognoosi", width="100%", icon("gears"), 
                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      HTML("<br><br>"),
      
      h3("Selgitavad tunnused"),
      h4("Eesti ettevõtjate detailsed hinnangud"),
      selectInput("kifail", label="Vali indikaator", unique(ki$indicator), selected = unique(ki$indicator)[1], multiple = T),
      
      h4("Teiste riikide kindlustunde indikaatorid"),
      checkboxGroupInput("riigid", "Vali riigid", 
                         choices = unique(euconfind$geo),
                         selected = c("EE", "EU28")),
      
      checkboxGroupInput("sector", "Vali sektorid", 
                         choices = unique(euconfind$indic),
                         selected = "Construction"),
      
      radioButtons("sa", "Vali sesoonsus",
                   choices = list("Sesoonselt kohandatud" = "SA", "Sesoonselt kohandamata" = "NSA"), 
                   selected = "SA"),
      
      h4("Tarbijahinnaindeks"),
      checkboxGroupInput("riigidhcpi", "Vali riigid", 
                         choices = unique(euprices$geo),
                         selected = c("EE", "EU28")),
      
      h4("Sotsiaalmeedia"),
      radioButtons("sm", "Facebooki tunnused",
                   choices = list("Jah" = "Jah", "Ei" = "Ei"), 
                   selected = "Ei")
  
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Prognoos",
                 plotOutput("joonislasso"),
                 uiOutput("models"),
                 plotOutput("best.length")
        ),
        tabPanel("Selgitavad tunnused",
                 h4("Eesti ettevõtjate detailsed hinnangud"),
                 plotOutput("joonis1"),
                 hr(),
                 h4("Teiste riikide kindlustunde indikaatorid"),
                 plotOutput("joonis2"),
                 hr(),
                 h4("Tarbijahinnaindeks"),
                 plotOutput("joonis3"),
                 plotOutput("joonis4")
        )
      )
    )
  )
)

server = function(input, output, session) {
  
  output$sliders = renderUI({
    if(input$cvornotcv == "noCV"){
      sliderInput("noCVSlider","Treeningperioodi algus", min = 1, max = 100, value = 1, step = 1)
    } else if(input$cvornotcv == "CV"){
      sliderInput("validate","Ristvalideerimise maksimaalne periood", min = 1, max = 10, value = 5, step = 1)
    }
  })
  
  kidf = reactive({
    df = ki %>%
      filter(indicator %in% input$kifail) %>%
      select(date, indicator, value)
    return(df)
  })
  
  output$joonis1 = renderPlot({
    kidf() %>%
      filter(!is.na(value)) %>%
      ggplot() +
      geom_line(aes(date, value, color=indicator)) +
      scale_x_date(date_breaks = "1 year" ,date_labels = "%Y") +
      labs(y="Indikaatori väärtused", x="", color="Indikaator",
           title="Eesti ettevõtete hinnangud",
           subtitle="Kojunktuuriinstituudi andmed", caption=paste0("http://www.ki.ee/baromeetrid/baromeetrid.htm, ",Sys.Date())) +
      theme_bw()
  })

  # Riikide võrdlev joonis
  euconf = reactive({
    dfeu = euconfind  %>% filter((geo %in% input$riigid) & (s_adj %in% input$sa) & (indic %in% input$sector)) %>% 
      select(date, indic, values, geo, s_adj)
    return(dfeu)
  })

  output$joonis2 = renderPlot({
    euconf() %>%
      ggplot() +
      geom_line(aes(date, values, color=geo)) +
      scale_x_date(date_breaks = "1 year" ,date_labels = "%Y") +
      facet_wrap(~indic, scales = "free")+
      labs(y="Indikaatori väärtused", x="", color="Riik",
           title="Ettevõtete kindlustunde indikaatorid sektorite ja majanduste lõikes",
           subtitle="", caption=paste0("Allikas: Eurostat, ", Sys.Date())) +
      theme_bw()
  })
  
  euprice = reactive({
    dfeu2 = euprices %>%
      filter(geo %in% input$riigidhcpi) %>% 
      select(geo, date, d1hicp, d12hicp)
      return(dfeu2)
  })
  
  output$joonis3 = renderPlot({
    euprice() %>%
      ggplot() +
      geom_line(aes(date, d1hicp, color=geo)) +
      scale_x_date(date_breaks = "1 year" ,date_labels = "%Y") +
      labs(y="Harmoniseeritud hinnaindeks", x="", color="Riik",
           title="Hindade muutus võrreldes eelmise kuuga",
           subtitle="", caption=paste0("Allikas: Eurostat, ", Sys.Date())) +
      theme_bw()
  })
  
  output$joonis4 = renderPlot({
    euprice() %>%
      ggplot() +
      geom_line(aes(date, d12hicp, color=geo)) +
      scale_x_date(date_breaks = "1 year" ,date_labels = "%Y") +
      labs(y="Harmoniseeritud hinnaindeks", x="", color="Riik",
           title="Hindade muutus võrreldes eelmise aasta sama kuuga",
           subtitle="", caption=paste0("Allikas: Eurostat, ", Sys.Date())) +
      theme_bw()
  })
   
  targetandmed = reactive({
    dftarget = KMD %>%
      filter(tekst %in% input$sektor) %>% 
      select(date, y = get(input$target))
    return(dftarget)
  })
    
  output$joonis5 = renderPlot({
    targetandmed() %>%
      ggplot() +
      geom_line(aes(date, y)) +
      scale_x_date(date_breaks = "1 year" ,date_labels = "%Y") +
      labs(y="Eur", x="", 
           title=input$sektor,
           subtitle=input$target, caption=paste0("Allikas: EMTA agregeeritud andmed")) +
      theme_bw()
    
  })
  
  facebook = reactive({
    fb = fb.df %>% select(date)
    if(input$sm=="Jah") fb = fb.df
    return(fb)
  })
  
  model.data = eventReactive(input$update,{
    # Data prep
    pr.period = input$h_int
    
    kidf = kidf() %>% dcast(date~indicator, value.var="value")
    colnames(kidf)=c("date", LETTERS[1:(ncol(kidf)-1)])
    
    euconf = euconf() %>% dcast(date~geo+indic, value.var="values")
    euprice = euprice() %>%
      melt(id.vars=c("geo", "date")) %>%
      dcast(date~geo+variable, value.var="value")
    
    fb = facebook()

    dfmodel = targetandmed() %>%
      merge(kidf, by="date", all=T) %>%
      merge(euconf, by="date", all=T) %>%
      merge(euprice, by="date", all=T) %>%
      merge(fb, by="date", all=T) %>%
      filter(year(date) > 2005) %>%
      mutate(y.lag6 = lag(y,6),
             y.lag9 = lag(y,9),
             y.lag12 = lag(y,12))
    
    start = as.Date(max(dfmodel$date+months(1)))
    end = as.Date(max(dfmodel$date+months(pr.period)))

    dfmodel = dfmodel %>%
      as_tibble() %>%
      add_row(date=seq.Date(start,end,"month")[1:pr.period])
    
    dfmodel$y.hint = lead(dfmodel$y, pr.period)
    dfmodel$date.hint = seq.Date(dfmodel$date[pr.period], by="month", length.out=nrow(dfmodel))

    #####
    # save(dfmodel, file = "dfmodel.Rda")
    dfmodel = dfmodel %>% select(-y, -date) %>% as.data.frame()
    
    comp.cases = which(complete.cases(dfmodel))
    
    # X and Y for the model
    x = model.matrix(y.hint~., data=dfmodel)[,-1]
    y = dfmodel$y.hint[comp.cases]
    
    length.test.period = pr.period
    
    # Model functions
    best.t.length = function(max.length = nrow(x)){
      if(max.length<(length.test.period*2)+pr.period) max.length=(length.test.period*2)+pr.period
      substract = (nrow(x)+1-max.length):(nrow(x)-(length.test.period*2)-pr.period)
      
      index = lapply(substract, function(d){
        l = d:(nrow(x)-pr.period)
        return(l)
      })
      
      best.length = plyr::ldply(1:length(index), function(d){
        tmp.x = x[index[[d]],]
        tmp.y = y[index[[d]]]
        
        if (is.function(updateProgress)) {
          text = paste("Arvutab treeningperioodi -", d, "/", length(substract))
          updateProgress(detail = text)
          if(length(substract)-d<=10) system(paste("say", length(substract)-d))
          # system(paste("say", d))
        }
        
        out = models(tmp.x, tmp.y, d, "training", updateProgress)
        
        return(out)
      })

      return(best.length)
    }
    
    models = function(x.model=x, y.model=y, id=1, method="best", best.value=nrow(x.model), updateProgress = NULL){
      if(!(method%in%c("training","best"))) return("Error in method.")
      
      m = function(m.train, m.test, period=1){
        # df = list(x.model,
        #           y.model,
        #           m.train,
        #           m.test,
        #           id,
        #           period)
        # save(df, file="df.Rda")
        # load("dfmodel.Rda")
        # load("df.Rda")
        # pr.period=6
        # x.model=df[[1]]
        # y.model=df[[2]]
        # m.train=df[[3]]
        # m.test=df[[4]]
        # id=df[[5]]
        # period=df[[6]]
        
        y.test = y.model[m.test]
        
        # LASSO
        grid = 10^seq(10,-2,length=100)
        lasso.mod = glmnet(x.model[m.train,], y.model[m.train], alpha=1, lambda=grid, standardize=T)
        cv.out = cv.glmnet(x.model[m.train,], y.model[m.train], alpha=1, lambda=grid, standardize=T, grouped=F)
        bestlam = cv.out$lambda.min
        
        # Random forest
        rf.mod = randomForest(y.model~., data=cbind(y.model,x.model), subset=m.train, importance=T)
        
        # Stacking (rf)
        combined = data.frame(LASSO = as.numeric(predict(lasso.mod, s=bestlam, newx=x.model)),
                              randomForest = predict(rf.mod, newdata=x.model),
                              Target = y.model)
        
        combined.rf.mod = randomForest(Target~., data=combined, subset=m.train, importance=T)

        # ARIMA
        combined.rf.resid = y.model[m.train] - predict(combined.rf.mod)
        
        resid.ts = ts(combined.rf.resid,
                      start = c(year(as.Date(x.model[1,"date.hint"],"1970-01-01")),
                                month(as.Date(x.model[1,"date.hint"],"1970-01-01"))),
                      freq = 12)
        
        set.seed(100)
        # arima.mod = arima(resid.ts, order = c(0,2,1), seasonal = list(order = c(0,1,1)))
        arima.mod = auto.arima(resid.ts)
        arima.pred = forecast(arima.mod, h = pr.period)$mean
        # ts.plot(resid.ts,arima.pred, lty = c(1,3))
        
        combined.rf.pred = predict(combined.rf.mod, newdata=combined[m.test,]) + arima.pred

        # MSE
        combined.rf.mse = mean((combined.rf.pred-y.test)^2)
        
        if(method=="training"){
          output.df = data.frame(mse = combined.rf.mse,
                                 index = id,
                                 Periood = period)
          return(output.df)
        } else if(method=="best"){
          
          forecast = predict(combined.rf.mod, newdata=combined)
          forecast[m.test] = forecast[m.test] - as.numeric(predict(arima.mod, n.ahead = nrow(combined[m.test,]))$pred)

          output.df = dfmodel %>%
            filter(complete.cases(.)) %>%
            mutate(Stacked = c(rep(NA,best.value-1), as.numeric(forecast)),
                   Target = y.hint,
                   y.hint=NULL) %>%
            melt(id.vars=c("date.hint"),
                 measure.vars=c("Stacked", "Target"))
          
          output = list(df = output.df,
                        models.list = list(lasso.mod, rf.mod, combined.rf.mod, arima.mod),
                        variables = paste("y ~", paste(colnames(dfmodel[,colnames(dfmodel)!="y.hint"]) ,collapse=" + ")),
                        mse = combined.rf.mse,
                        t.length = paste0(best.value,":",nrow(x)-pr.period),
                        min=best.value,
                        max=nrow(x)-pr.period)
          
          return(output)
        }
      }
      
      if(method=="training"){
        max.test.periods = round(((nrow(x.model)-length.test.period))/length.test.period/1.5,0)
        if(max.test.periods > input$validate) max.test.periods = input$validate
        
        train = lapply(1:max.test.periods, function(d){
          max.index = rev(nrow(x.model)-(d*length.test.period))
          l = 1:max.index
          return(l)
        })
        
        test = lapply(1:length(train), function(d){
          l = (max(train[[d]])+1):(max(train[[d]])+length.test.period)
          return(l)
        })
        
        test.period.mse = plyr::ldply(1:length(train), function(d){
          tr = train[[d]]
          te = test[[d]]
          
          out = m(tr, te, as.character(d))
          return(out)
        })
        
      } else if(method=="best"){
        tr = 1:((nrow(x.model)-pr.period))
        te = (-tr)
        
        out = m(tr, te)
        return(out)
      }
    }
    
    # Traning limit
    max.length = nrow(x)
    max.progress = length((nrow(x)+1-max.length):(nrow(x)-(length.test.period*2)-pr.period)) + 1
    
    # Progress
    progress = shiny::Progress$new()
    progress$set(message = "Treenib.", value = 1/max.progress)
    on.exit(progress$close())
    
    updateProgress = function(value = NULL, detail = NULL) {
      if (is.null(value)) {
        value = progress$getValue()
        value = value + (progress$getMax() - value) / max.progress
      }
      
      js$moveBG(value*100)
      progress$set(value = value, detail = detail)
    }
    
    if(input$cvornotcv=="CV"){
      best = best.t.length(max.length)
      
      best2 = best %>%
        group_by(index) %>%
        summarize(mse = mean(mse))
      
      bv = best2$index[best2$mse==min(best2$mse)]
    } else if(input$cvornotcv=="noCV"){
      bv = input$noCVSlider
      best = c()
    }

    output = models(x[bv:nrow(x),], y[bv:length(y)], best.value = bv, updateProgress)
    output[length(output)+1] = list(best)
    
    return(output)
  })
  
  output$joonislasso = renderPlot({
    
    tmp = model.data()[[1]]
    tmp2 = tmp %>% filter(variable=="Target")
    tmp2 = tmp2[model.data()[[6]]:model.data()[[7]],]
    
    tmp %>%
      ggplot() +
      geom_rect(data=tmp2,
                aes(xmin=min(date.hint),xmax=max(date.hint),ymin=-Inf,ymax=Inf),
                fill="lightgrey",alpha=.5,show.legend = FALSE) +
      geom_line(aes(date.hint, value, colour=variable)) +
      scale_x_date(date_breaks = "1 year" ,date_labels = "%Y") +
      labs(colour="Näitaja", x="", y="",
           title=paste0("Sektor: ", input$sektor, ", Näitaja: ", input$target),
           subtitle="Prognoos LASSO, ARIMA ja random forestiga (koondatud random forestiga).") +
      theme_bw()
  })
  
  output$models = renderUI({
    HTML("Mudeli spetsifikatsioon: <br>",
         model.data()[[3]], "<br><br>",
         "Treeningperiood: ", model.data()[[5]], "<br><br>",
         "MSE prognoositaval perioodil (log10): <br>", round(log10(model.data()[[4]]),3), "<br><br>")
  })
  
  output$best.length = renderPlot({
    if(input$cvornotcv=="CV"){
      tmp = model.data()[[8]]
      
      tmp2 = tmp %>%
        group_by(index) %>%
        summarize(mse = mean(mse))
      
      tmp %>%
        ggplot() +
        geom_point(aes(index, log10(mse), color=Periood)) +
        geom_line(aes(index, log10(mse), color=Periood, group=Periood), size=.8) +
        geom_line(data=tmp2, aes(index, log10(mse)), size=1, linetype="dashed") +
        labs(title="Koondprognoosi MSE",
             x="Treeningperioodi algus",
             caption="Katkendlik joon tähistab valideerimisperioodide keskmist") +
        theme_bw()
    } else if(input$cvornotcv=="noCV"){
      
    }
  })

}

shinyApp(ui = ui, server = server)
