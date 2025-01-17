
#setup 
rm(list=ls())

library("readstata13")
library(dplyr)
library(stats)
#your work dir (=Anna's main)
ruta <- "//me-filer1/home$/au232/My Documents/1.CEDAR/3_Studies !!/28-DfT2.0/4-Manchester/1-Model OD data DFT2.0/modelODdata~/6-Anna PCT code/Scenarios_R_code/"
setwd(ruta)

#read flow file
l <- readRDS('./1a_dataoriginal/Census2011/Flow_Level/MSOA_T2W/wu03bew_msoa.Rds')

#RENAMES v1..v14 to FINAL NAMES
cols <-c('home_msoa','work_msoa','all','from_home','light_rail','train','bus','taxi','motorbike','car_driver',
'car_passenger','bicycle','foot','other')  

colnames(l) <- cols

#read mortality + add 3 cols.
mortality <- readRDS('./1b_datacreated/0temp/mortrate_msoa.Rds')
colnames(mortality)[1] <- 'home_msoa'
l <-inner_join(l,mortality, by='home_msoa')
rm(mortality)

#add cyclestreet variables: dist, dist_fast, av_slope
cyclestreet <- readRDS('./1b_datacreated/0temp/cyclestreets_speedhilliness.Rds')
l <- left_join(l, cyclestreet[, -c(3, 4)], by=c('home_msoa', 'work_msoa'))       
rm(cyclestreet)

#drop _merge==2 (elements only in right set -> none in R)

# GROUP OVERSEAS WORKPLACES TO 'OTHER', GENERATE WORKPLACE TYPE 
l$hometemp <- substr(l$home_msoa,start = 1,stop = 1)     #picks first letter
l$worktemp <- substr(l$work_msoa,start = 1,stop = 1)  

##### FIRST 2 LINES CAUSE TROUBLES
#l$work_msoa[l$work_msoa=="OD0000002" | l$worktemp %in% c("W","S",'0','1','2','3','4','5','9') ] <- 'other'
#l$work_msoa[l$work_msoa=="OD0000002" | (l$worktemp %in% c("W","S",'0','1','2','3','4','5','9')) ] <- 888
l$work_msoa[l$work_msoa=="OD0000002" ] <- 'other'
l$work_msoa[l$work_msoa=="W" ] <- 'other'
l$work_msoa[l$work_msoa=="S" ] <- 'other'
l$work_msoa[l$work_msoa=="0" ] <- 'other'
l$work_msoa[l$work_msoa=="1" ] <- 'other'
l$work_msoa[l$work_msoa=="2" ] <- 'other'
l$work_msoa[l$work_msoa=="3" ] <- 'other'
l$work_msoa[l$work_msoa=="4" ] <- 'other'
l$work_msoa[l$work_msoa=="5" ] <- 'other'
l$work_msoa[l$work_msoa=="9" ] <- 'other'


#generate & assign flowtypes
l$flowtype <- NA

l$flowtype[l$worktemp=="E" & l$dist_fast<30 & l$home_msoa!=l$work_msoa] <- 1  # the 'standard' commuter
l$flowtype[l$home_msoa==l$work_msoa]      <- 2     #work from home
l$flowtype[l$work_msoa=="OD0000003" ]     <- 3     #no fixed place
l$flowtype[l$work_msoa=="other"]         <- 4      # other
l$flowtype[ l$worktemp=="E" & l$dist_fast>=30 ] <- 4    #long distance commuters

table(l$flowtype[l$hometemp=="E" & l$work_msoa!="OD0000001"] )    #to compare vs. Stata


#REPLACE 'all' & 'other' by their sum by OD
l$all <- aggregate(l$all, by=list(l$home_msoa, l$work_msoa), FUN=sum, na.rm=T)
l$other <- aggregate(l$other, by=list(l$home_msoa, l$work_msoa), FUN=sum, na.rm=T)


#del temp cols.
drops <- c('hometemp','worktemp')
l <- l[, !names(l) %in% drops]

#duplicates drop : all entries with same cols.
l <- l[!dplyr::duplicated(l), ]
#l <- l[!(base::duplicated(l)),]
#l <- l %>% distinct()


#read sex ratio file per flow
msoa_sex <- readRDS("./1b_datacreated/0temp/msoa_t2w_sex.Rds")
l <- inner_join(l,msoa_sex,by=c('home_msoa'='home_msoa', 'work_msoa'='work_msoa'))


sum((l$allcom_male+  l$allcom_female)!= l$all)  #check it *is* zero

#order by home_msoa_names & work_msoa_name
l <- sort(l,by=~ home_msoa_name + home_la_name)
# l <- l[with(l,order=c(home_msoa_name,home_la_name)), ]  #A>G. uses also work_msoa

l <-l[ l$work_msoa!="OD0000001",]
l$hometemp <- substr(l$home_msoa,start = 1, stop = 1)     #picks first home_msoa letter
l <- l[l$hometemp=='E',]   #subset only to people in England

saveRDS(l,file.choose())   #saving temp files for mid-process tracking (wu03bew_preproc.Rds)


#################
## STEP 1: COLLAPSE TYPE 4 FLOWS TO 'OTHER'
#################	
# COLLAPSE ALL OUT-OF-SCOPE FLOWS TO 'OTHER' (2 of 2)

l$work_msoa[l$flowtype==4] <-    'other'

l$all <- aggregate(l$all, by=list(l$home_msoa, l$work_msoa), FUN=sum, na.rm=T)
l$other <- aggregate(l$other, by=list(l$home_msoa, l$work_msoa), FUN=sum, na.rm=T)
l$allcom_male  <-  aggregate(l$allcom_male, by=list(l$home_msoa, l$work_msoa), FUN=sum, na.rm=T)
l$bicycle_female <- aggregate(l$bicycle_female, by=list(l$home_msoa, l$work_msoa), FUN=sum, na.rm=T)

l$dist[l$work_msoa=='other'] <- 
    l$dist_fast[l$work_msoa=='other'] <-
    l$avslope_perc[l$work_msoa=='other']    <-   NA

l <- l[!duplicated(l),] 


#################
## STEP 2: ASSIGN VALUES IF DISTANCE/HILLINESS UNKNOWN
#################
## ASSIGN HILLINESS + DISTANCE VALUES IF START AND END IN SAME PLACE
# ??????????????????
#by home_msoa (dist_fast), sort: gen littlen=_n

# l <- l[with(l, order(home_msoa, dist_fast)),  ]
#l <- sort(l, by=~home_msoa+dist_fast )
#l <- transform(l, Sequence=ave(seq_along(littlen), home_msoa,dist_fast, FUN=seq_along))

l <- arrange(l, home_msoa, dist_fast)
l$littlen <- with(l, ave(rep(1, nrow(l)), l$home_msoa, l$dist_fast,  FUN = seq_along))


l$dist_fasttemp[l$littlegen > 3] <- NA
l$avslope_percenttemp [l$littlegen > 3] <- NA

l$dist_fasttemp2 <- aggregate(l$dist_fasttemp, by=l$home_msoa,FUN=mean(),na.rm=T)
l$avslope_percenttemp2 <- aggregate(l$avslope_percenttemp, by=l$home_msoa,FUN=mean(),na.rm=T)

l$dist_fasttemp[l$flowtype==2 & is.na(l$dist_fast)]        <-  l$dist_fasttemp2
l$avslope_percenttemp [ l$flowtype==2 & is.na(l$avslope_percent) ]   <- l$avslope_percenttemp2

# DISTANCE = A THIRD OF THE MEAN DISTANCE OF SHORTEST 3 FLOWS
# HILLINESS = MEAN HILLINESS OF SHORTEST 3 FLOWS
l$dist_fast [l$flowtype==2 ] <- l$dist_fast /3

#special case:  isles of Scilly
l$dist_fast[l$home_msoa=='E02006781' &l$work_msoa=='E02006781'] = 0.79    
l$avslope_perc[l$home_msoa=='E02006781' &l$work_msoa=='E02006781'] = 0.2

###get rid of temp columns
#use regex
l <- l[,-c('littlen', 'dist_fasttemp', 'dist_fasttemp2', 'avslope_perctemp', 'avslope_perctemp2')]

       
## ASSIGN DISTANCE AMONG CYCLISTS VALUES IF NO FIXED PLACE : MEAN DIST AMONG CYCLISTS TRAVELLING <15KM
l$cycdist_fast <- l$dist_fast

l$dist_fast15  <- l$dist_fast30 <- l$dist_fast
l$bicycle15 <- l$bicycle30 <- l$bicycle

if (l$dist_fast>15 | flowtype>2) {l$dist_fast15 <- NA}
if (l$dist_fast>30 | flowtype>2) {l$dist_fast30 <- NA}

if (is.na(l$dist_fast15))   { l$bicycle15 <- NA}
if (is.na(l$dist_fast30))   { l$bicycle30 <- NA}


l$numndist_fast15   <- aggregate(x = (l$dist_fast15 * l$bicycle15), by=l$home_msoa,FUN=sum(),na.rm=T)
l$dendist_fast15   <- aggregate(x = l$bicycle15, by=l$home_msoa,FUN=sum(),na.rm=T)

l$meandist_fast15 <-       l$numndist_fast15  /   l$dendist_fast15
l$cycdist_fast[l$flowtype==3 ]    <-  l$meandist_fast15 
#OK

## ASSIGN DISTANCE VALUES IF OVERSEAS OR >30KM: MEAN DIST AMONG CYCLISTS TRAVELLING <30KM

l$numndist_fast30   <-  sum(l$dist_fast30 * l$bicycle30)
l$dendist_fast30    <-  sum(l$bicycle30)
l$meandist_fast30   <-   l$numndist_fast30/ l$dendist_fast30

l$cycdist_fast[l$flowtype==4]    <-  l$meandist_fast30
#OK

#add new columns
l <-cbind.data.frame(l, dist_fastmissing=NA, cycdist_fastmissing= NA)

l$dist_fastmissing[is.na(l$dist_fast)] <- NA
l$cycdist_fastmissing [is.na(cycdist_fast) ] <- NA

table(l$dist_fastmissing)
table(l$cycdist_fastmissing)

#SAVE RELEVANT VARIABLES (TO FIT INDIVIDUAL MODEL)
l <-l[,c('home_msoa', 'work_msoa', 'home_msoa_name', 'all-other', 'flowtype',
         'allcom_male', 'bicycle_female', 'mortrate_govtarget', 'mortrate_gendereq',
         'mortrate_dutch', 'dist', 'dist_fast', 'avslope_perc', 'cycdist_fast')]

saveRDS(object =l[, c('home_msoa','cycdist_fastmissing')],file="./1b_datacreated/0temp/MSOA_ODpairs_process2.1.Rds")

#OK

####### STEP 3A:    CALCULATE PROPENSITY to CYCLE

##MODEL FITTING FOR TRIPS <30KM

l$dist_fastsq       <-  l$dist_fast^2
l$dist_fastsqrt     <- sqrt(l$dist_fast)
l$ned_avslope_perc  <- l$avslope_perc-0.57
l$interact          <-   l$dist_fast * l$ned_avslope_perc
l$interactsqrt      <-  l$dist_fastsqrt * l$ned_avslope_perc

# FIT REGRESSION EQUATION

l$pred_base <- -3.894 + (-0.5872 * l$dist_fast) + (1.832 * l$dist_fastsqrt) + (0.007956 * l$dist_fastsq)
+ (-0.2872 * l$ned_avslope_perc) + (0.01784 * l$dist_fast* l$ned_avslope_perc)
+ (-0.09770 * l$dist_fastsqrt * l$ned_avslope_perc)

l$bdutch <-  2.499+(-0.07384 * l$dist_fast)                 #Dutch travel survey

l$bdutch[l$flowtype ==3] <- NA
l$bebike <- (0.05710 * l$dist_fast) + (-0.0001087 * l$dist_fastsq)
l$bebike <- l$bebike + (-0.67 * -0.2872 * l$ned_avslope_perc)  #Swiss travel survey

l$pred_dutch <- l$pred_base + l$bdutch
l$pred_ebike <- l$pred_dutch + l$bebike

for (x in c('base','dutch','ebike')) {
    l[[paste0('pred_',x)]] <- exp( l[[paste0('pred_',x)]] ) / (1 +  exp( l[[paste0('pred_',x)]] ) )
    
        }


## MODEL FITTING FOR TRIPS WITH NO FIXED PLACE
# INPUT PARAMETERS
l$pred_base[l$flowtype!=1 & l$flowtype!=2] <- NA  
l$bdutch[l$flowtype!=1 & l$flowtype!=2] <- NA
l$bebike[l$flowtype!=1 & l$flowtype!=2] <- NA

l$all[(is.na(l$pred_base))] <- NA
l$all[is.na(l$bdutch) ] <- NA
l$all[is.na(l$bebike) ] <- NA

############
# all temp thing - -CHECK!

############

l$nummeanpred_base  <- aggregate(x = (l$pred_base*l$all), by=list(l$home_msoa), FUN=sum, na.rm=T)
l$nummeanbdutch     <- aggregate(x=(l$bdutch * l$all), by=list(l$home_msoa), FUN=sum, na.rm=T)
l$nummeanbebike     <- aggregate(x= (l$bebike * l$all), by=list(l$home_msoa), FUN=sum, na.rm=T)


l$denmeanpred_base  <- aggregate (x=l$all, by=list(l$home_msoa), FUN=sum, na.rm=T)
l$denmeanbdutch     <- aggregate (x=l$all, by=list(l$home_msoa), FUN=sum, na.rm=T)
l$denmeanbebike     <- aggregate (x=l$all, by=list(l$home_msoa), FUN=sum, na.rm=T)

l$meanpred_base  <- l$nummeanpred_base  / l$denmeanpred_base
l$meanbdutch     <- l$nummeanbdutch  / l$denmeanbdutch
l$meanbebike     <- l$nummeanbebike  / l$denmeanbebike

l <- l[, -c(nummeanpred_base,nummeanbdutch,nummeanbebike,denmeanpred_base, denmeanbdutch,denmeanbebike)]
l <-l[, -grep(pattern =(nummean|denmean),x =names(l) )]   #alternative


l$meanpred_basesq   <-     l$meanpred_base^2
l$meanpred_basesqrt <-     l$meanpred_base^0.5


##FIT REGRESSION EQUATION
l$pred2_base<-  -6.218 + (189.9 * l$meanpred_basesq) + (9.275 * l$meanpred_basesqrt) 
l$pred2_dutch<-  l$pred2_base + l$meanbdutch
l$pred2_ebike<-  l$pred2_dutch + l$meanbebike

l$pred2_base  <-   exp(l$pred2_base) / (1 +exp(l$pred2_base) )
l$pred2_dutch <-    exp(l$pred2_dutch) / (1 +exp(l$pred2_dutch) )
l$pred2_ebike  <-  exp(l$pred2_ebike) / (1 +exp(l$pred2_ebike) )


l$pred_base [l$flowtype==3] <-  l$pred2_base[which(l$flowtype==3)] 
l$pred_dutch [l$flowtype==3] <- l$pred2_dutch[which(l$flowtype==3)]
l$pred_ebike [l$flowtype==3] <- l$pred2_ebike[which(l$flowtype==3)] 

#delete pred2 cols.
l <- l[,-grep(pattern='pred2',x = names(l))]   #check
l <- l[,-c('dist_fastsq', 'dist_fastsqrt', 'ned_avslope_perc', 'interact',
 'interactsqrt', 'meanpred_base', 'bdutch', 'bebike', 'meanbdutch', 'meanbebike', 'meanpred_basesq', 'meanpred_basesqrt')]


########################################
## PART 3B: APPLY SCENARIOS TO MSOA DATA
#################
## CALCULATE NO. CYCLISTS IN EACH SCENARIO

l$nocyclists_slc<- 0
l$nocyclists_sic <-    l$nocyclists_slc - l$bicycle

l$govtarget_slc <-  l$bicycle+ (l$pred_base * l$all)
l$govtarget_slc [l$govtarget_slc > l$all & !is.na(l$govtarget_slc) ]  <- l$all
l$govtarget_sic  <-  l$govtarget_slc   -  l$bicycle

#??????? order govtarget_slc, before(govtarget_sic)

l$gendereq_slc <- (l$bicycle_male  * (1 + (l$allcom_female/l$allcom_male)))

l$gendereq_slc[l$gendereq_slc > l$all & !is.na(l$gendereq_slc) ] <-  l$all 

#tab all if female==0 | male==0		

l$gendereq_slc[l$allcom_female==0 | l$allcom_male==0 | (l$gendereq_slc< l$bicycle)] <-  l$bicycle
# [not needed] NO CHANGE IF NO FEMALES IN FLOW-NO MALES IN FLOW- SLC < BASELINE

l$gendereq_sic      <-     l$gendereq_slc - l$bicycle     #ajuste final

l$dutch_slc  <- l$pred_dutch * l$all
l$ebike_slc  <- l$pred_ebike * l$all   

l$dutch_slc[l$dutch_slc >  l$all  & !is.na(l$dutch_slc) ]  <-    l$all  #max. is 100%
l$ebike_slc[l$ebike_slc >  l$all  & !is.na(l$ebike_slc) ]    <-  l$all 

l$dutch_slc[l$dutch_slc <  l$bicycle ]  <-    l$bicycle  #we set min  = BASELINE
l$ebike_slc[l$ebike_slc <  l$bicycle ]    <-  l$bicycle

#increase in no. cyclists
l$dutch_sic  <- l$dutch_slc  - l$bicycle
l$ebike_sic  <- l$ebike_slc - l$bicycle


l$govtarget_slc[l$work_msoa=="other"]  <-    l$bicycle
l$gendereq_slc [l$work_msoa=="other"]  <-    l$bicycle
l$dutch_slc [l$work_msoa=="other"]  <-    l$bicycle
l$ebike_slc [l$work_msoa=="other"]  <-    l$bicycle

l$govtarget_sic[l$work_msoa=="other"]  <-    0
l$gendereq_sic [l$work_msoa=="other"]  <-    0
l$dutch_sic [l$work_msoa=="other"]  <-    0
l$ebike_sic [l$work_msoa=="other"]  <-    0


## CALCULATE % NON-CYCLISTS MADE CYCLISTS IN EACH SCENARIO: TURN THAT % AWAY FROM WALKING

# foreach x in nocyclists {
l$pchange_nocyclists <- (l$all- l$nocyclists_slc)/(l$all - l$bicycle) 
l$nocyclists_slw  <-  l$foot * l$pchange_nocyclists					# most flows - scale walking according to %change

l$nocyclists_slw[l$bicycle==l$all]  <- ((l$all-l$nocyclists_slc) * 0.31) 	# Flows with pure bicycles at baseline - make walking 31% of new flows
l$nocyclists_siw <- l$nocyclists_slw - l$foot
l$nocyclists_sld <-  l$car_driver * l$pchange_nocyclists

l$nocyclists_sld[ l$bicycle==l$all] <-  (l$all- l$nocyclists_slc) * 0.35
# Flows with pure bicycles at baseline - make driving 35% of new flows

l$nocyclists_sid <- l$nocyclists_sld -  l$car_driver	
#order `x'_slw `x'_siw `x'_sld `x'_sid, after(`x'_sic)


for (x in c('govtarget','gendereq', 'dutch','ebike') ) {
    
    l[[paste0('pchange_',x)]] = l$all - l[[paste0(x,'_slc')]] / (l$all - l$bicycle)
    
    if (l$all==l$bicycle)  { l[[paste0('pchange_',x)]] = 1 }
    
    l[[paste0(x,'_slw')]] = l$foot * l[[paste0('pchange_',x)]]
    l[[paste0(x,'_siw')]] = l[[paste0(x,'_slw')]] - l$foot
    l[[paste0(x,'_sld')]] = l$car_driver  * l[[paste0('pchange_',x)]]
    l[[paste0(x,'_sid')]] = l[[paste0(x,'_sld')]] - l$car_driver

                                    }

# order `x'_slw `x'_siw `x'_sld `x'_sid, after(`x'_sic)

## DROP INTERMEDIARY VARIABLES
# drop pred_base pred_dutch pred_ebike 
# drop pchange_nocyclists pchange_govtarget pchange_gendereq pchange_dutch pchange_ebike

l <- l[, -c('pred_base', 'pred_dutch', 'pred_ebike', 'pchange_nocyclists', 'pchange_govtarget', 'pchange_gendereq', 'pchange_dutch', 'pchange_ebike')]

#################
## STEP 4: DO HEAT
#################
# INPUT PARAMETERS

cyclecommute_tripspertypicalweek  <-  7.16 	
cspeed  <-  14	
wspeed  <-  4.8
ebikespeed  <-  15.8
ebikemetreduction  <-  0.648

percentebike_dutch <- switch(min(l$cycdist_fast)<=l$cycdist_fast<=4.9999999,0.06, 5<=l$cycdist_fast<=9.9999999,0.11,
                               10<=l$cycdist_fast <=19.999999, 0.17,20<= l$cycdist_fast< max(l$cycdist_fast),0.23)

percentebike_ebike <- switch(min(l$cycdist_fast)<=l$cycdist_fast<=4.9999999, 0.71, 5<=l$cycdist_fast<= 19.9999999,0.92,
                               20<= l$cycdist_fast<=max(l$cycdist_fast), 1)

#check which are constants
crr_webtag <- 0.72
crr_heat <- 0.9 
cdur_ref_webtag <- 180
cdur_ref_heat <- 100	
wrr_webtag <- 0.78
wrr_heat <- 0.89 
wdur_ref_webtag <- 203
wdur_ref_heat <- 168
l$mortrate_nocyclists <- l$mortrate_govtarget
l$mortrate_ebike <- l$mortrate_dutch
vsl <- 1855315		# VALUE IN POUNDS

# DURATION OF CYCLING/WALKING

# TIME CYCLING PER DAY min. AMONG NEW CYCLISTS
l$cdur_obs  <-  60 * ((l$cycdist_fast * cyclecommute_tripspertypicalweek) / l$cspeed)
l$cdur_obs_dutch <- ((1-l$percentebike_dutch) * l$cdur_obs)+(l$percentebike_dutch * l$cdur_obs * ebikemetreduction * (cspeed/ ebikespeed))
l$cdur_obs_ebike <- ((1-l$percentebike_ebike) * l$cdur_obs)+(l$percentebike_ebike * l$cdur_obs * ebikemetreduction * (cspeed / ebikespeed))

# TIME WALKING PER DAY IN MINUTES AMONG THOSE NOW SWITCHING TO CYCLING
l$wdur_obs  <-  60 * ((l$cycdist_fast  * cyclecommute_tripspertypicalweek) / wspeed)
l<-l[,-c('cyclecommute_tripspertypicalweek', 'cspeed', 'wspeed', 'ebiketimereduction', 'ebikemetreduction', 'percentebike_dutch', 'percentebike_ebike')]

#	drop cyclecommute_tripspertypicalweek cspeed wspeed ebiketimereduction ebikemetreduction percentebike_dutch percentebike_ebike


# MORTALITY PROTECTION
## this needs a loop and df[[paste0(,)]]

#var <-paste0(cprotection_govtarget,z)    # SCALE RR DEPENDING ON HOW DURATION IN THIS POP COMPARES TO REF
cprotection_govtarget_webtag  <- (1-crr_webtag) * (cdur_obs/cdur_ref_webtag)	
cprotection_nocyclists_webtag    <- cprotection_govtarget_webtag
cprotection_gendereq_webtag  <-cprotection_govtarget_webtag
cprotection_dutch_webtag  <- (1-crr_webtag) * (cdur_obs_dutch/cdur_ref_webtag)
cprotection_ebike_webtag  <- (1-crr_webtag) * (cdur_obs_ebike/cdur_ref_webtag)

cprotection_govtarget_heat  <- (1-crr_heat) * (cdur_obs/cdur_ref_heat)	
cprotection_nocyclists_heat  <-cprotection_govtarget_heat
cprotection_gendereq_heat  <-cprotection_govtarget_heat
cprotection_dutch_heat  <- (1-crr_heat) * (cdur_obs_dutch/cdur_ref_heat)
cprotection_ebike_heat  <- (1-crr_heat) * (cdur_obs_ebike/cdur_ref_heat)



cols <- grep(pattern ='nocyclists|govtarget |gendereq |dutch| ebike',x = names(l))
if (l[,cols] >0.45)  {l[, cols] <- 0.45}

l$wprotection_webtag <-  (1- wrr_webtag) * (l$wdur_obs/  wdur_ref_webtag)
l$wprotection_heat <-  (1- wrr_heat) * (l$wdur_obs/ wdur_ref_heat) 

#recode wprotection_webtag 0.50/max=0.5
l$wprotection_heat[l$wprotection_heat> 0.30] = 0.30


# DEATHS AND VALUES
target1 <- c('webtag', 'heat')
target2 <- c('nocyclists', 'govtarget','gendereq','dutch','ebike')
target3 <- c('govtarget','gendereq','dutch','ebike')


for (z in target1) {

    for (x in target2) {
    
    l[[paste0(x,'_sic_death_',z)]]  = -1 * l[[paste0(x,'_sic')]]  * l[[paste0('mortrate_',x) ]] 
        * l[[paste0('cprotection_',x,'_',z)]] 
    
    l[[paste0(x,'_siw_death_',z)]]  = -1 * l[[paste0(x,'_siw')]] * l[[paste0('mortrate_',x) ]] 
    * l[[paste0('cprotection_', x,'_',z)]] 
    
    l[[paste0(x,'_sideath_',z)]]  =  l[[paste0(x,'_sic_death_')]] + l[[paste0(x,'_siw_death_',z) ]] 
 
    l[[paste0(x,'_sivalue_',z)]]  = -1 * l[[paste0(x,'_sideath_',z)]] * vsl   #long ommited!
    
    
 #drop `x'_sic_death_`z' `x'_siw_death_`z'
    }
    
l[[paste0('base_sldeath_',z)]]  <- -1 * l[[paste0(nocyclists,'_sideath_',z )]]	
# BASELINE LEVEL IS INVERSE OF 'NO CYCLISTS' SCENARIO

l[[paste0(base,'_slvalue_', z) ]] <- -1 * l[[paste0(nocyclists,'_sivalue_l', z)  ]]			


for (x in target3)  {
    l[[x,'_sldeath_' , z]]  = l[[x,'_sideath_' , z]] + l[['base_sldeath_' , z]]  
    l[[x,'_slvalue_' , z]] = l[[x,'_sivalue_' , z]] + l[['base_slvalue_', z]]
 
#order `x'_sideath_`z' `x'_sivalue_`z', after(`x'_slvalue_`z')
                    }  #for x

        }  #for z

# DROP INTERMEDIARY VARIABLES

l <- l[ , -c('mortrate_govtarget', 'mortrate_gendereq', 'mortrate_dutch', 'cyclecommute_tripspertypicalweek', 'wprotection_heat',
'nocyclists_sideath_webtag', 'nocyclists_sivalue_webtag', 'nocyclists_sideath_heat', 'nocyclists_sivalue_heat')]


#################
##  STEP 5: DO CO2 EMISSIONS CALCS 
#################

cyclecommute_tripsperweek  <- 5.24
co2kg_km  <- 0.186

target <- c('nocyclists','govtarget', 'gendereq', 'dutch', 'ebike')

for (x in target) { 
l[[paste0(x,'_sico2')]] <- l[[x,'_sid']] * l$cycdist_fast * 
cyclecommute_tripsperweek * 52.2 * co2kg_km 	# NO CYCLISTS *DIST * COMMUTE PER DAY * CO2 EMISSIONS FACToR
                    }

l$base_slco2 <- -1 * l$nocyclists_sico2	## BASELINE LEVEL IS INVERSE OF 'NO CYCLISTS' SCENARIO INCREASE


target <-target[-1]  #amend list

for (x in target) {
    l[[x,'_slco2']]<- l[[x, '_sico2']] + l$base_slco2

#order `x'_sico2 , after(`x'_slco2)
}

#drop columns
l <- l[,  -c('nocyclists', 'cyclecommute_tripsperweek', 'co2kg_km', 'cycdist_fast')]



#################
## FINISH: SAVE TEMPORARY DATASET, PRE-AGGREGATION
#################

saveRDS(l,file='./1b_datacreated/0temp/MSOA_ODpairs_process2.5.Rds')



#################
## PART 3A: AGGREGATE TO AREA LEVEL
#################


# AGGREGATE MSOA FIGURES

target <-c('all', ' other', ' govtarget_slc', 'ebike_sico2')

for (x in target)  {
                l[[paste0('a_', x)]] <- aggregate(l[[x]], by=list(l$home_msoa),FUN=sum, na.rm=T) 

                }
# AREA FILE KEEP+RENAME+ORDER
tcols <- grep(pattern ='home_',names(l) )
l  <- l[, c(tcols)]
#   rename a_* *
tcols1 <- grep(pattern = 'a_', names(l) )
### rename HERE

l <- l[,-c('from_home')]

#order home_msoa home_msoa_name all light_rail- other 
l <- l[!duplicated(l),]

# ROUND to 3, 5, 1 decimals
# for (x in  c('govtarget_slc','ebike_sid')) {
#     l[[x]]  = round(l[[x]], 3)  }

targetx <-  c('govtarget_slc','ebike_sid')
l[, target] <- sapply(target, function(x) {round(l[,targetx],3)} )


for (x in c('base_sl', 'govtarget_sl', 'govtarget_si', 'gendereq_sl', 'gendereq_si', 'dutch_sl', 'dutch_si', 'ebike_sl', 'ebike_si')) {
for (y in c('death_webtag', 'death_heat') ) {
    l[[paste0(x, y)]] =round(l[[paste0(x, y)]], 5)
                                    }

for (y in c('value_webtag', 'value_heat') ){
    l[[paste0(x, y)]]= round(l[[paste0(x, y)]],1)
                                        }

    l[[paste0(x,'co2')]] =  round(l[[paste0(x,'co2')]], 4)
}

saveRDS(l,file='./1b_datacreated/pct_area.Rds')

    
#################
## PART 3B: AGGREGATE TO FLOW LEVEL
#################

# MAKE BIDIRECTIONAL MSOAS
l$homesub=substr(l$home_msoa,2, 10 )
l$worksub=substr(l$work_msoa,2, 10)    #or replace 10 by real length

l$homesub <- as.numeric(l$homesub)
l$worksub <- as.numeric(l$worksub)

l$msoa1= as.character (min(l$homesub, l$worksub))
l$msoa2= as.character (max(l$homesub, l$worksub))    #check this


l$msoa1="E0"+msoa1
l$msoa2="E0"+msoa2

if (l$work_msoa=="OD0000003" | l$work_msoa=="other")  {
    l$msoa1=l$home_msoa
    l$msoa2=l$work_msoa             } 
    

# AGGREGATE UP FLOW FIGURES                       =======>  LOOOOOOONGEST OF ALL !!!
for  (x in  c(all, other, govtarget_slc, ebike_sico2)  )  {

    l[[ paste0('f_',x)]]= aggregate(l[[x]], by=c('msoa1','msoa2'), na.rm=T)
                            
                                                        }
# FLOW FILE KEEP + RENAME + ORDER + no duplicates
colstokeep <- grep(pattern=('msoa1'|'msoa2'|'f_') ,x=names(l))

#keep msoa1 msoa2 f_*

#rename f_* *
#drop from_home
#order msoa1 msoa2 all light_rail- other 
l <- l[!duplicated(l),] 

#vars to round to 2D
target<- c('govtarget_slc', 'ebike_sid')
l[target] <- round(l[target],digits =2)

#vars to round to 5D
x<- c('base_sl', 'govtarget_sl', 'govtarget_si', 'gendereq_sl', 'gendereq_si', 'dutch_sl', 'dutch_si', 'ebike_sl', 'ebike_si')
y = c('death_webtag', 'death_heat')
z = c('value_webtag', 'value_heat')

target1 <- expand.grid(x,y)
target2 <- expand.grid(x,z)
target3 <- expand.grid(x,'co2')

#set appropriate rounding per var
l[[target1]] <- round(l[target1],digits =5)
l[[target2]] <- round(l[target1],digits =1)
l[[target3]] <- round(l[target1],digits = 2)

# for (x in base_sl govtarget_sl govtarget_si gendereq_sl gendereq_si dutch_sl dutch_si ebike_sl ebike_si {
# for (y in death_webtag death_heat{
# replace `x'`y'=round(`x'`y',0.00001)
# }
# for (y in value_webtag value_heat {
# replace `x'`y'=round(`x'`y',1)
# }
# replace `x'co2=round(`x'co2,0.01)
# }


saveRDS(l,"./1b_datacreated/pct_lines_v1.Rds")

#delete intermediate files


