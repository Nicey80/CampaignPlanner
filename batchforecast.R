

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
        mutate(forecast_Resp = map(data,batch_Forecast,Curves,CurveID))
FD_nest
#FD_nest$forecast_voiceCalls[[1]]

library(broom)
FD_tidy <- FD_nest %>%
        mutate(td=map(forecast_Resp,bind_rows)) %>%
        select(CampaignID,Batch,td) %>%
        unnest()
FD_tidy
}