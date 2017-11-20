library(shiny)
library(shinydashboard)
library(plotly)
library(tidyverse)
library(lubridate)
library(rhandsontable)
library(readxl)
library(shinyBS)
library(openxlsx)
#source("batchforecast.R")

NewBPlan <- data.frame()

##load functions
batch_Forecast <- function(inpdata){
        
        library(lubridate)
        CurveID <- RCID$CurveID[RCID$Func==inpdata$Func && RCID$SubFunc==inpdata$SubFunc]
        Curve <- RCurves[,CurveID+1]        
        
        B.Forecast.Voice.Calls <- inpdata$`Batch Vol`* inpdata$VoiceRR *Curve
        B.Forecast.Voice.BB <- B.Forecast.Voice.Calls * inpdata$VConvBB
        B.Forecast.Voice.TV <- B.Forecast.Voice.Calls* inpdata$VConvTV
        B.Forecast.Voice.Mob <- B.Forecast.Voice.Calls* inpdata$VConvMob
        
        B.Forecast.Web.Calls <- inpdata$`Batch Vol`* inpdata$WebRR *Curve
        B.Forecast.Web.BB <- B.Forecast.Web.Calls *inpdata$ WConvBB
        B.Forecast.Web.TV <- B.Forecast.Web.Calls* inpdata$WConvTV
        B.Forecast.Web.Mob <- B.Forecast.Web.Calls* inpdata$WConvMob
        
        
        F.Days=nrow(B.Forecast.Voice.Calls)
        
        datelist <- seq.Date(from=as_date(inpdata$`Batch Send Date`),length.out = F.Days, by= "day")
        BF <- cbind(datelist,B.Forecast.Voice.Calls,B.Forecast.Voice.BB,B.Forecast.Voice.TV,B.Forecast.Voice.Mob,
                    B.Forecast.Web.Calls,B.Forecast.Web.BB,B.Forecast.Web.TV,B.Forecast.Web.Mob)
        colnames(BF) <- c("datelist","Voice Calls","Voice BB Sales",
                          "Voice TV Sales","Voice Mob Sales",
                          "Web Visits","Web BB Sales","Web TV Sales",
                          "Web Mob Sales")
        BF
}



### Forecast

run_Forecast <- function(inputdata) {
        
        FullData <- inputdata
        
        FD <- FullData %>%
                mutate(BRespV=`Batch Vol`*VoiceRR,
                       BRespW=`Batch Vol`*WebRR,
                       BBVoice=BRespV*VConvBB,
                       BBWeb=BRespW*WConvBB,
                       TotBB=BBVoice + BBWeb
                       
                ) %>%
                #select(CampaignID,`Campaign Name`,Format,BRespV,BRespW,
                #      BBVoice,BBWeb,TotBB,`Batch Send Date`) %>%
                group_by(CampaignID, Batch)
        
        FD_nest <- FD %>%
                nest()
        #FD_nest
        
        FD_nest <- FD_nest %>%
                mutate(forecast_Resp = map(data,batch_Forecast))
        FD_nest
        #FD_nest$forecast_voiceCalls[[1]]
        
        library(broom)
        FD_tidy <- FD_nest %>%
                mutate(td=map(forecast_Resp,bind_rows)) %>%
                select(CampaignID,Batch,td) %>%
                unnest()
        FD_tidy
}

##load AdminData


DataSource <- "Excel"

if (DataSource=="Excel"){
        
        DatePeriods <- read_excel("./data/AdminData.xlsx",sheet=1)
        #DatePeriods$Start <- as_date(DatePeriods$Start)
        #DatePeriods$End <- as_date(DatePeriods$End)
        OrgSetUp<- read_excel("./data/AdminData.xlsx",sheet=2)
        RCID <- read_excel("./data/AdminData.xlsx",sheet=4)
        RCurves <- read_excel("./data/AdminData.xlsx",sheet=3)
        
        
}
        


header <- dashboardHeader(title = "Campaign Manager")

sidebar <- dashboardSidebar(
        sidebarMenu(
                menuItem("Plan Summary", tabName = "PlanSum", icon = icon("dashboard")),
                menuItem("Scenario Planning", icon = icon("th"), tabName = "Scen")
        )
        
)

body <- dashboardBody(
        # tags$head(
        #         tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        # ),
        tabItems(
                tabItem(tabName = "PlanSum",
                        h4("Campaign Plan Overview"),
                        box(title="Select which plan to Review", width=12,solidHeader = TRUE,
                            status = "primary",
                            column(width = 3,
                                selectInput("Func","Area",choices=as.list(unique(OrgSetUp$Func)),
                                                                          selected=1)
                                    
                                
                                ),
                                column(width=3,
                                       uiOutput("ListInp1")
                                       # selectInput("SubFunc","Area",choices=c("Acq",
                                       #                                     "Mob",
                                       #                                     "Keep",
                                       #                                     "Grow"))
                                       ),
                                column(width=3,
                                       selectInput("Per","Period",choices=as.list(
                                               DatePeriods$`Reporting Period`),
                                               selected = "2017 Q3"
                                       )
                                ),
                            column(width=3,
                                   h1(""),
                                   actionButton("Exec","Execute")
                                   )
                            
                                
                        ),
                        
                                h4("Plan Overview"),
                        #tableOutput("test"),
                        fluidRow(
                                tabBox(title="Forecast Output", id="tabset1", height='500px', width = 12,
                                       tabPanel("Global",
                                column(width=8,
                                       plotlyOutput("PlanCurr")),
                                box(width=4,
                                       title="ViewOutputs", solidHeader = TRUE,
                                           status = "primary",
                                           checkboxGroupInput("ViewCriteria", "View", choices=c(
                                                   "Voice Calls",
                                                   "Voice BB Sales",
                                                   "Voice TV Sales",
                                                   "Voice Mob Sales",
                                                   "Web Visits",
                                                   "Web BB Sales",
                                                   "Web TV Sales",
                                                   "Web Mob Sales"),
                                                   selected="Voice Calls"
                                                   
                                           )
                                               
                                       )
                                
                        
                                ),
                                tabPanel("Campaign Contribution",
                                         column(width=8,
                                                plotlyOutput("PlanCurr2")),
                                         box(width=4,
                                             title="ViewOutputs", solidHeader = TRUE,
                                             status = "primary",
                                             selectInput("ViewCriteriaList", "View", choices=c(
                                                     "Voice Calls",
                                                     "Voice BB Sales",
                                                     "Voice TV Sales",
                                                     "Voice Mob Sales",
                                                     "Web Visits",
                                                     "Web BB Sales",
                                                     "Web TV Sales",
                                                     "Web Mob Sales"),
                                                     selected="Voice Calls"
                                                     
                                             )
                                             
                                         )),
                                tabPanel("Campaign Summary",
                                         column(width=8,
                                                tableOutput("sumtable"))#plotlyOutput("PlanCurr2")),
                                         
                                             
                                         )
                                )
                        ),
                        h4("Plan Detail"),
                        box(width=12,
                            title="Actions", solidHeader = TRUE,
                            status = "primary",
                            column(width=12,
                                actionButton("EditP","Edit Plan",icon=icon("cogs")),
                                
                                actionButton("SubmitChanges","Update Plot", icon=icon("refresh")),
                                actionButton("SaveScen", "Save as Scenario", icon=icon("area-chart")),
                                bsModal("ScenSv", "Save Scenario", "SaveScen", size="small",
                                        wellPanel(
                                                h4("Scenario Name"),
                                                textInput("TBScen","Enter Scenario Name"),
                                                actionButton("confirmsave", "Save Scenario"),
                                                actionButton("cancel", "Cancel")
                                        )),
                                actionButton("SubChgs", "Submit Changes", icon=icon("handshake-o"))
                                ),
                        fluidRow(
                        tabBox(title="Plan", id="tabset2", height='500px', width = 12,
                               tabPanel("Plan Summary",
                        column(width=12,
                                tableOutput("CampTable")
                        )
                               ),
                        tabPanel("Edit Plan",
                                box(width=7,title="Campaign Plan", solidHeader = TRUE,
                                    status = "primary",
                                    actionButton("NewCamp","Add Campaign",icon=icon("plus-square")),
                                    actionButton("DeleteCamp","Delete Campaign", icon=icon("close")),
                                rHandsontableOutput("hot_t1")),
                                box(width=5,
                                    title="Send Plan", solidHeader = TRUE,
                                    status = "primary",
                                    actionButton("NewBatch","Add Batch",icon=icon("plus-square")),
                                    actionButton("DeleteBatch","Delete Batch", icon=icon("close")),
                                    rHandsontableOutput("hot_SendPlan"))
                        )
                               )
                        )
                        )
                ),
                
                tabItem(tabName = "Scen",
                        h4("Campaign Plan Overview"),
                        box(title="Select which plan to Review", width=12,solidHeader = TRUE,
                            status = "primary",
                            column(width = 3,
                                   selectInput("Func","Area",choices=as.list(unique(OrgSetUp$Func)),
                                               selected=1)
                                   
                                   
                            ),
                            column(width=3,
                                   uiOutput("ListInp2")
                                   # selectInput("SubFunc","Area",choices=c("Acq",
                                   #                                     "Mob",
                                   #                                     "Keep",
                                   #                                     "Grow"))
                            ),
                            column(width=3,
                                   selectInput("Per","Period",choices=as.list(
                                           DatePeriods$`Reporting Period`),
                                           selected = "2017 Q3"
                                   )
                            ),
                            column(width=3,
                                   h1(""),
                                   actionButton("ExecScen","Execute")
                            )
                            
                            
                        ),
                        box(title="Compare Scenarios", width=12,solidHeader = TRUE,
                            status = "primary",
                            column(width=2,
                                   uiOutput("Sc")
                                   ),
                            column(width=8,
                                   tabBox(title="Compare Scenarios", id="tabset3", height='500px', width = 12,
                                          tabPanel("Graphical", 
                                                   radioButtons("GType","Plot Type", 
                                                                choices = c("Profile","Outcome"),
                                                                inline=TRUE),
                                                   plotlyOutput("ScGraph")),
                                          tabPanel("Tabular"))
                             ),
                            column(width=2,
                                   uiOutput("Check")
                                   )
                )
                )
        )
        
)












ui <- dashboardPage(
        header,
        sidebar,
        body
        
)

server <- function(input, output, session) {
        
        QStart <- reactive({
                input$Exec
                
                isolate(as_date(DatePeriods$Start[DatePeriods$`Reporting Period`==input$Per]))
        })
        QEnd3 <- reactive({
                input$Exec
                isolate(as_date(DatePeriods$End[DatePeriods$`Reporting Period`==input$Per]) %m+% months(3))
        })
        
        QEnd <- reactive({
                input$Exec
                isolate(as_date(DatePeriods$End[DatePeriods$`Reporting Period`==input$Per]))
        })
        
        
        values <- reactiveValues()
        
        
        observeEvent(input$Exec,{
                
                values$FID <- read_excel("./data/InitCampDB.xlsx",sheet=3)
                
                values$Camp1 <- read_excel("./data/InitCampDB.xlsx",sheet=1)
                values$Camp1 <- values$Camp1 %>%
                        left_join(values$FID) %>%
                        filter(Func==input$Func, SubFunc==input$SubFunc,
                               Period==input$Per, CurrentPlan=="Current") %>%
                        select(-ScenarioName,-CurrentPlan)
                
                values$Batch1 <- read_excel("./data/InitCampDB.xlsx",sheet=2)
                values$Batch1 <- values$Batch1 %>%
                        left_join(values$FID)%>%
                        filter(CurrentPlan=="Current") %>%
                        select(-ScenarioName,-CurrentPlan,-Func,-SubFunc,-Period)
                
                values$Camp2 <- values$Camp1
                values$Camp2$ForecastID <- 0
                values$Batch2 <- values$Batch1
                values$Batch2$ForecastID <- 0
                
                
        })
        
        
        Baseline <- reactive({
                Camps%>%
                 left_join(CampBatch) %>%
                 mutate(Vers="Current")
        })
        
        BaseForecast <- reactive({
                input$Exec
                isolate({run_Forecast(CampData())})
        })
        
        CampData <- reactive({
                input$Exec
                isolate({
                 FullData <- values$Camp1%>%
                         left_join(values$Batch1) %>%
                         left_join(values$FID) %>%
                         filter(CurrentPlan=="Current")
                         #mutate(Vers="Current")
                })
         })
        
        NewForecast<- reactive({
                input$SubmitChanges
                isolate({
                        NewD <- values$Camp2%>%
                                left_join(values$Batch2)

                        run_Forecast(NewD)})
        })
        
        output$CampTable <- renderTable({
                if(input$Exec==0)
                        return()
                TableView <- CampData() %>%
                        mutate(BRespV=`Batch Vol`*VoiceRR,
                               BRespW=`Batch Vol`*WebRR,
                               BBVoice=BRespV*VConvBB,
                               BBWeb=BRespW*WConvBB,
                               TotBB=BBVoice + BBWeb
                                
                        ) %>%
                        #filter(Func==input$Func, SubFunc==input$SubFunc) %>%
                        select(CampaignID,`Campaign Name`,Format,BRespV,BRespW,
                               BBVoice,BBWeb,TotBB,`Batch Send Date`) %>%
                        
                        group_by(CampaignID) %>%
                        summarise(VoiceCalls=sprintf("%7.0f",sum(BRespV)), 
                                  WebVisits=sprintf("%7.0f",sum(BRespW)),
                                  VoiceSalesBB=sprintf("%7.0f",sum(BBVoice)),
                                  WebSalesBB=sprintf("%7.0f",sum(BBWeb)),
                                  TotalBBSales=sprintf("%7.0f",sum(TotBB)),
                                  FirstMailDate=as.character(as_date(min(`Batch Send Date`))),                   
                                  LastMailDate=as.character(as_date(max(`Batch Send Date`)))) %>%
                        as_data_frame()
                TableView$CampaignID <- sprintf("%7.0f",TableView$CampaignID)
                TableView
        })
        
        
        output$PlanCurr <- renderPlotly({
                if(input$Exec==0)
                        return()
                
                
                InpFrame <- left_join(BaseForecast(),values$Camp1)
                
                if(input$SubmitChanges==0){
                #QStart <- DatePeriods$Start[DatePeriods$`Reporting Period`==input$Per,]
                InpFrame <- InpFrame %>%
                        filter(datelist>=QStart(),datelist<=QEnd3()) %>%
                        filter(Func==input$Func, SubFunc==input$SubFunc) %>%
                        gather(Type,Vol, `Voice Calls`:`Web Mob Sales`)%>%
                        filter(Type %in% input$ViewCriteria)
                
                p <- ggplot(InpFrame) +
                        stat_summary(aes(datelist,Vol,group=Type, colour= Type),geom='line',
                                     fun.y='sum')
                }
                if(input$SubmitChanges!=0){
                InpFrame2 <- left_join(NewForecast(),values$Camp2)
                InpFrame2 <- mutate(InpFrame2, CurrentPlan="New")
                
                InpFrame <- mutate(InpFrame, CurrentPlan="Current")
                
                InpFrame <- rbind(InpFrame,InpFrame2)
                InpFrame <- InpFrame %>%
                        filter(datelist>=QStart(),datelist<=QEnd3()) %>%
                        filter(Func==input$Func, SubFunc==input$SubFunc) %>%
                        gather(Type,Vol, `Voice Calls`:`Web Mob Sales`)%>%
                        filter(Type %in% input$ViewCriteria)
                
                p <- ggplot(InpFrame) +
                        stat_summary(aes(datelist,Vol,group=interaction(CurrentPlan,Type), colour= interaction(CurrentPlan,Type)),geom='line',
                                     fun.y='sum')
                }

                ggplotly(p)

        })
        output$PlanCurr2 <- renderPlotly({
                if(input$Exec==0)
                        return()
                InpFrame <- left_join(BaseForecast(),values$Camp1)
                InpFrame <- InpFrame %>%
                        filter(datelist>=QStart(),datelist<=QEnd3()) %>%
                        filter(Func==input$Func, SubFunc==input$SubFunc) %>%
                        gather(Type,Vol, `Voice Calls`:`Web Mob Sales`)%>%
                        filter(Type %in% input$ViewCriteriaList)
                
                p <- ggplot(InpFrame) +
                        stat_summary(aes(datelist,Vol,group=CampaignID, fill= factor(CampaignID),
                                         colour = factor(CampaignID)),
                                     geom='density', position='stack',
                                     fun.y='sum')
                
                ggplotly(p)
                
        })
        # output$test <- renderTable({
        #         DT <- data.frame(input$hot_SendPlan$data, stringsAsFactors = FALSE)
        #         nc <- ncol(DT)
        #         nr <- nc/5
        #         DT_m <- data.frame()
        #         i <- 1
        #         for (j in 1:nr){
        #                 for (k in 1:5){
        #                       DT_m[j,k] <- DT[i]
        #                       i=i+1
        #                 }
        #         }
        #         DT_m
        #         #str(DT)
        # })
        
        FuncData <- reactive({
                filter(OrgSetUp,Func %in% input$Func)
        })
        
        
        output$ListInp1 <- renderUI({
                LD2 <- FuncData()
                selectInput("SubFunc","Area",
                            choices = as.list(unique(LD2$SubFunc)),
                            selected=1)
        })
        
        output$ListInp2 <- renderUI({
                LD2 <- FuncData()
                selectInput("SubFunc","Area",
                            choices = as.list(unique(LD2$SubFunc)),
                            selected=1)
        })
        
        output$sumtable <- renderTable({
                if(input$Exec==0)
                        return()
                T1Full <- CampData() %>%
                        mutate(BRespV=`Batch Vol`*VoiceRR,
                               BRespW=`Batch Vol`*WebRR,
                               BBVoice=BRespV*VConvBB,
                               BBWeb=BRespW*WConvBB,
                               TotBB=BBVoice + BBWeb
                               
                        ) %>%
                        filter(Func==input$Func, SubFunc==input$SubFunc) %>%
                        select(SubFunc,BRespV,BRespW,
                               BBVoice,BBWeb,TotBB,`Batch Send Date`) %>%
                        
                        group_by(SubFunc) %>%
                        summarise(VoiceCalls=sum(BRespV), 
                                  WebVisits=sum(BRespW),
                                  VoiceSalesBB=sum(BBVoice),
                                  WebSalesBB=sum(BBWeb),
                                  TotalBBSales=sum(TotBB)) %>%
                        as_data_frame() %>%
                        mutate(View="Total") %>%
                        select(View,
                               VoiceCalls, 
                               WebVisits,
                               VoiceSalesBB,
                               WebSalesBB,
                               TotalBBSales)
                
                T1IQ <- left_join(BaseForecast(),values$Camp1)%>%
                        filter(Func==input$Func, SubFunc==input$SubFunc, datelist<QEnd()) %>%
                        select(SubFunc,`Voice Calls`,`Web Visits`,`Voice BB Sales`,
                               `Web BB Sales`) %>%
                        
                        group_by(SubFunc) %>%
                        summarise(VoiceCalls=sum(`Voice Calls`), 
                                  WebVisits=sum(`Web Visits`),
                                  VoiceSalesBB=sum(`Voice BB Sales`),
                                  WebSalesBB=sum(`Web BB Sales`)) %>%
                        as_data_frame() %>%
                        mutate(View="InQ", TotalBBSales = VoiceSalesBB+WebSalesBB) %>%
                        select(View,
                               VoiceCalls, 
                               WebVisits,
                               VoiceSalesBB,
                               WebSalesBB,
                               TotalBBSales)
                
                TB <- bind_rows(T1IQ,T1Full)
                
        })
        
        output$hot_t1 <- renderRHandsontable({
                
                CampLoad <- values$Camp2 %>%
                        select(-Func,-SubFunc, -Period, -ForecastID)
                
                rhandsontable(CampLoad, readOnly=FALSE, width=700, 
                              selectCallback = TRUE) %>%
                        hot_table(highlightRow = TRUE)
        })
        
        observeEvent(input$hot_t1,{
                OutObj <- hot_to_r(input$hot_t1)
                Outcols <- colnames(OutObj)
                OutObj <- OutObj %>%
                        mutate(Func=input$Func,
                               SubFunc=input$SubFunc,
                               Period=input$Per,
                               ForecastID=0) %>%
                        select(Func,SubFunc,Period,ForecastID,one_of(Outcols))
                
                
                values$Camp2 <- OutObj
        })
        
        UPDCamp <- reactive({
                input$SubmitChanges
                isolate({
                NewData <- hot_to_r(input$hot)
                NewData})
        })
        
        output$upd <- renderTable({
                UPDCamp()
        })
        
      observeEvent(input$hot_SendPlan,{
              if (!is.null(input$hot_SendPlan)){
                MasterData <- hot_to_r(input$hot_t1)
                CampID <- MasterData[input$hot_t1_select$select$r,1]
                
                

                #BP <- data.frame(input$hot_SendPlan$data)
 
                
                DT <- data.frame(input$hot_SendPlan$data, stringsAsFactors = FALSE)
                print(nrow(DT))
                print(CampID)
                print(DT)
                if(nrow(DT)!=0){
                nc <- ncol(DT)
                nr <- nc/5
                DT_m <- data.frame()
                i <- 1
                for (j in 1:nr){
                        for (k in 1:5){
                                DT_m[j,k] <- DT[i]
                                i=i+1
                        }
                }
                
                #DT_m[,5] <- as.POSIXct(DT_m[,5])
                
                colnames(DT_m) <- c("ForecastID","CampaignID","Batch","Batch Vol","Batch Send Date")
                DT_m$`Batch Send Date` <- as.POSIXct(as_date(DT_m$`Batch Send Date`))
                #print(DT_m)
                #BP <- as.data.frame(BP)
                BatchPlanRem<- values$Batch2 %>%
                        filter(CampaignID!=CampID)
                
                

                values$Batch2 <- bind_rows(BatchPlanRem,DT_m)
                
                #print(values$Batch2)
                }
              }
        })
        
        output$hot_SendPlan <- renderRHandsontable({
                
                if(input$Exec==0)
                        return()
                
                if(is.null(input$hot_t1_select$select$r))
                      return()
                
                MasterData <- hot_to_r(input$hot_t1)
                CampID <- MasterData[input$hot_t1_select$select$r,1]
                
                vals <- values$Batch2 %>%
                        filter(CampaignID==CampID)
                rhandsontable(vals, readonly=FALSE, width=500,
                              selectCallback = TRUE)

               
        })
        # observeEvent(input$SaveScen,{
        #         toggleModal(session,"ScenSv","open")
        # })
        
        observeEvent(input$cancel,{
                toggleModal(session,"ScenSv","close")
        })
        observeEvent(input$confirmsave,{
                
                #load ForecastDB
                FullDB <- reactiveValues()
                
                FullDB$Camp <- read_excel("./data/InitCampDB.xlsx",sheet=1)
                FullDB$Batch <- read_excel("./data/InitCampDB.xlsx",sheet=2)
                FullDB$FID <- read_excel("./data/InitCampDB.xlsx",sheet=3)
                
                #get max number, forecastIDis +1
                m.FID <- max(FullDB$FID$ForecastID)
                new.ID <- m.FID +1
                
                #get scnarioname
                ScenName <- input$TBScen
                
                #saveinputs to DBFIle
                NewFCDets <- data.frame(ForecastID=new.ID,
                                        Func=input$Func,
                                        SubFunc=input$SubFunc,
                                        Period=input$Per,
                                        ScenarioName=ScenName,
                                        CurrentPlan="")
                
                
                FullDB$FID <- bind_rows(FullDB$FID,NewFCDets)
                
                excols <- colnames(values$Camp2)
                NewCamp <- values$Camp2 
                NewCamp <- NewCamp %>%
                        mutate(Func=as.character(input$Func),
                               SubFunc=as.character(input$SubFunc),
                               Period=as.character(input$Per)) %>%
                        select(Func,SubFunc,Period,one_of(excols))
                NewCamp$ForecastID <- new.ID
                

                
                FullDB$Camp <- bind_rows(FullDB$Camp,NewCamp)
                
                NewBatch <- values$Batch2
                NewBatch$ForecastID <- new.ID
                
                FullDB$Batch <- bind_rows(FullDB$Batch, NewBatch)
                
                
                l <- list("Campaigns"=FullDB$Camp, "CampaignDetails"=FullDB$Batch, "ForecastID"=FullDB$FID) 
                write.xlsx(l,"./data/InitCampDB.xlsx", colNames=TRUE)
                
                toggleModal(session,"ScenSv","close")
        })
        
        
        observeEvent(input$NewCamp,{
                max.camp <- max(values$Camp2$CampaignID)
                new.camp <- max.camp+1
                
                
                values$Camp2 <- values$Camp2 %>%
                        add_row(CampaignID=new.camp,
                                `Campaign Name`="NewCampaign",
                                Format="DM",
                                VoiceRR=0,
                                WebRR=0,
                                VConvBB=0,
                                VConvTV=0,
                                VConvMob=0,
                                WConvBB=0,
                                WConvTV=0,
                                WConvMob=0)
                
                
               values$Batch2 <- values$Batch2 %>%
                       add_row(ForecastID=0,
                               CampaignID=new.camp,
                               Batch=1,
                               `Batch Vol`=0,
                               `Batch Send Date`='2017-01-01')
                
                
        })
        
        observeEvent(input$NewBatch,{
                MasterData <- hot_to_r(input$hot_t1)
                CampID <- MasterData[input$hot_t1_select$select$r,1]
                #CampID
                
                max.batch <- values$Batch2 %>%
                        filter(CampaignID==CampID)
                
                max.batch <- max(max.batch$Batch)
                new.batch <- max.batch+1
                
                values$Batch2 <- values$Batch2 %>%
                        add_row(ForecastID=0,
                                CampaignID=CampID,
                                Batch=new.batch,
                                `Batch Vol`=0,
                                `Batch Send Date`='2017-01-01')
                
        })
        
        observeEvent(input$DeleteCamp,{
                MasterData <- hot_to_r(input$hot_t1)
                CampID <- MasterData[input$hot_t1_select$select$r,1]
                

                
                values$Batch2 <- values$Batch2 %>%
                        filter(CampaignID!=CampID)
                
                values$Camp2 <- values$Camp2 %>%
                        filter(CampaignID!=CampID)
                
        })
        
        observeEvent(input$DeleteBatch,{
                selected.row <- input$hot_SendPlan_select$select$r
                
                MasterData <- hot_to_r(input$hot_t1)
                CampID <- MasterData[input$hot_t1_select$select$r,1]
                
                UNaffected <- values$Batch2 %>%
                        filter(CampaignID!=CampID)
                NC <- values$Batch2 %>%
                        filter(CampaignID==CampID)
                
                if(nrow(NC)>1){
                
                NC <- NC[-selected.row,]
                NC <- bind_rows(UNaffected,NC)
                values$Batch2 <- NC
                }
                #print(NC)
                #print(values$Batch2)
        })
        
        
        ###ScenarioPlanning
        
        valuesscenario <- reactiveValues()
        
        #ScenForecasts <- reactiveValues()
        
        
        observeEvent(input$ExecScen,{
                
                valuesscenario$FID <- read_excel("./data/InitCampDB.xlsx",sheet=3)
                
                valuesscenario$Camp1 <- read_excel("./data/InitCampDB.xlsx",sheet=1)
                valuesscenario$Camp1 <- valuesscenario$Camp1 %>%
                        left_join(valuesscenario$FID) %>%
                        filter(Func==input$Func, SubFunc==input$SubFunc,
                               Period==input$Per) %>%
                        select(-ScenarioName,-CurrentPlan)
                
                valuesscenario$Batch1 <- read_excel("./data/InitCampDB.xlsx",sheet=2)
                valuesscenario$Batch1 <- valuesscenario$Batch1 %>%
                        left_join(valuesscenario$FID)%>%
                        select(-ScenarioName,-CurrentPlan,-Func,-SubFunc,-Period)
                
                
                valuesscenario$FID <- valuesscenario$FID %>%
                        filter(Func==input$Func, SubFunc==input$SubFunc,
                               Period==input$Per)
                
                CampDataScen <- valuesscenario$Camp1 %>%
                        left_join(valuesscenario$Batch1) %>%
                        left_join(valuesscenario$FID) %>%
                        group_by(ScenarioName)
                
                CampDataScen
                
                Camp_nest <- CampDataScen %>%
                        nest()
                Camp_nest
                
                Camp_nest <- Camp_nest %>%
                        mutate(FC = map(data,run_Forecast))
                Camp_nest
                
                Camp_tidy <- Camp_nest %>%
                        mutate(td=map(FC,bind_rows)) %>%
                        select(ScenarioName,td) %>%
                        unnest()
                Camp_tidy
                
                valuesscenario$Forecasts1 <- Camp_tidy
                
                #print(valuesscenario$Forecasts1)
                
                CN <- Camp_tidy[,-c(1:4)]
                
                print(CN)
                CN2 <- colnames(CN)
                print(CN2)
                
                valuesscenario$Metrics <- CN2
                print(valuesscenario$Metrics)
                
        })
        output$Sc <- renderUI({
                #if(input$Exec2==0)
                #   return()
                
                selectInput("Scenario","Select Scenario",
                            choices=as.list(unique(valuesscenario$FID$ScenarioName)),
                            multiple = TRUE,
                            selected=1)
        })
        
        
        output$ScGraph <- renderPlotly({
                if(input$ExecScen==0)
                        return()
                
                if(input$GType=="Profile"){
                
                #list scenarios
                PF <- valuesscenario$Forecasts1
                
                PF <- PF %>%
                        filter(datelist>=QStart(),datelist<=QEnd3()) %>%
                        #filter(Func==input$Func, SubFunc==input$SubFunc) %>%
                        gather(Type,Vol, `Voice Calls`:`Web Mob Sales`)%>%
                        filter(ScenarioName %in% input$Scenario)%>%
                        filter(Type %in% input$Metric)
                
                p <- ggplot(PF)+
                        stat_summary(aes(datelist,Vol,group=ScenarioName, colour= ScenarioName),geom='line',
                                     fun.y='sum')
                
                } else if(input$GType=="Outcome"){
                        PF <- valuesscenario$Forecasts1
                        
                        PF <- PF %>%
                                filter(datelist>=QStart(),datelist<=QEnd3()) %>%
                                #filter(Func==input$Func, SubFunc==input$SubFunc) %>%
                                gather(Type,Vol, `Voice Calls`:`Web Mob Sales`)%>%
                                filter(ScenarioName %in% input$Scenario) %>%
                                filter(Type %in% input$Metric)
                        
                        p <- ggplot(PF)+
                                stat_summary(aes(Type,Vol,group=ScenarioName, fill= ScenarioName),geom='col',
                                             fun.y='sum', position='dodge')#+
                                #stat_summary(aes(Type,Vol, label=Vol), vjust=-0.3, size=3.5,geom='text',
                                             #fun.y='sum', position='dodge')
                        
                }
                
                ggplotly(p)
                
        })
        
        output$Check <- renderUI({
                if(input$ExecScen==0)
                        return()
                
                checkboxGroupInput("Metric","Metrics", 
                                   choices=as.list(unique(valuesscenario$Metrics)),
                                   selected = 1)
                
        })
        
        
}

shinyApp(ui, server)