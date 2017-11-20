library(tidyverse)
library(readxl)

Camps <- read_excel("./data/InitCampDB.xlsx",sheet=1)
CampBatch <- read_excel("./data/InitCampDB.xlsx",sheet=2)
RCID <- read_excel("./data/InitCampDB.xlsx",sheet=4)
RCurves <- read_excel("./data/InitCampDB.xlsx",sheet=3)

DatePeriods <- read_excel("./data/AdminData.xlsx",sheet=1)
OrgSetUp<- read_excel("./data/AdminData.xlsx",sheet=2)

#### testing

##SUmmary table
FullData <- Camps%>%
        left_join(CampBatch)

FullData %>%
mutate(BRespV=`Batch Vol`*VoiceRR,
       BRespW=`Batch Vol`*WebRR,
       BBVoice=BRespV*VConvBB,
       BBWeb=BRespW*WConvBB,
       TotBB=BBVoice + BBWeb
       
) %>%
        select(CampaignID,`Campaign Name`,Format,BRespV,BRespW,
               BBVoice,BBWeb,TotBB,`Batch Send Date`) %>%
        group_by(CampaignID) %>%
        summarise(VoiceCalls=sum(BRespV),
                  WebVisits=sum(BRespW),
                  VoiceSalesBB=sum(BBVoice),
                  WebSalesBB=sum(BBWeb),
                  TotalBBSales=sum(TotBB),
                  FirstMailDate=min(`Batch Send Date`),
                  LastMailDate=min(`Batch Send Date`))


### Forecast

FullData <- Camps%>%
        left_join(CampBatch)

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
FD_nest


batch_Forecast <- function(inpdata){
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
        library(lubridate)
        datelist <- seq.Date(from=as_date(inpdata$`Batch Send Date`),length.out = F.Days, by= "day")
        BF <- cbind(datelist,B.Forecast.Voice.Calls,B.Forecast.Voice.BB,B.Forecast.Voice.TV,B.Forecast.Voice.Mob,
              B.Forecast.Web.Calls,B.Forecast.Web.BB,B.Forecast.Web.TV,B.Forecast.Web.Mob)
        colnames(BF) <- c("datelist","Voice Calls","Voice BB Sales",
                         "Voice TV Sales","Voice Mob Sales",
                         "Web Visits","Web BB Sales","Web TV Sales",
                         "Web Mob Sales")
        BF
}


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

#idy(FD_nest)

TD <- bind_rows(FD_nest$forecast_voiceCalls)

FD_nest$forecast_Resp[[1]]

ggplot(FD_tidy)+stat_summary(aes(datelist,B.Forecast.Voice.Calls), geom = 'line', fun.y = 'sum')


FD_tidy %>% filter(datelist>='2017-10-01',datelist<='2018-03-31') %>%
        gather(Type,Vol, B.Forecast.Voice.Calls:B.Forecast.Web.Mob)
