clear
clear matrix

cd "\\me-filer1\home$\au232\My Documents\1.CEDAR\3_Studies !!\28-DfT2.0\4-Manchester\1-Model OD data DFT2.0\modelODdata~\6-Anna PCT code\160409_PCTforReplication\"		

	use "1a_dataoriginal\Census2011\Flow_Level\MSOA_T2W\wu03bew_msoa.dta", clear
			rename v1 home_msoa
			rename v2 work_msoa
			rename v3 all
			rename v4 from_home
			rename v5 light_rail
			rename v6 train
			rename v7 bus
			rename v8 taxi
			rename v9 motorbike
			rename v10 car_driver
			rename v11 car_passenger
			rename v12 bicycle
			rename v13 foot
			rename v14 other

		* MERGE IN MORT RATES (DATASETS D2)
			rename home_msoa msoa11cd
			merge m:1 msoa11cd using "1b_datacreated\0temp\mortrate_msoa.dta", nogen 
			rename msoa11cd home_msoa
		
		* MERGE IN CYCLE STREETS VARIABLES (DATASET D3) 
			merge 1:1 home_msoa work_msoa using "1b_datacreated\0temp\cyclestreets_speedhilliness.dta", keepus(dist dist_fast avslope_perc)
			drop if _merge==2
			drop _merge  /*delete the ones only on using */
			
		* GROUP OVERSEAS WORKPLACES TO 'OTHER', GENERATE WORKPLACE TYPE 
			gen hometemp=substr(home_msoa,1,1)
			gen worktemp=substr(work_msoa,1,1)
				* NB Sxx = scotland, Wxx = wales, 9xx = Northern Ireland, O = offshore installation or no fixed place, 01-59 = foreign countries
			replace work_msoa="other" if work_msoa=="OD0000002" | worktemp=="W" | worktemp=="S" | worktemp=="0" | worktemp=="1" | worktemp=="2" | worktemp=="3" | worktemp=="4" | worktemp=="5" | worktemp=="9"

			gen flowtype=.
			recode flowtype .=1 if worktemp=="E" & dist_fast<30 & home_msoa!=work_msoa
			recode flowtype .=2 if home_msoa==work_msoa
			recode flowtype .=3 if work_msoa=="OD0000003"
			recode flowtype .=4 if worktemp=="E" & dist_fast>=30 
			recode flowtype .=4 if work_msoa=="other"
			label def flowtypelab 1 "England, <30km fastest" 2 "Within zone" 3 "No fixed place" 4 "Outside England/offshore installation", modify
			label val flowtype flowtypelab

			tab flowtype if hometemp=="E" & work_msoa!="OD0000001", mi		// ALL COMUTERS NOT WORKING FROM HOME
			tab flowtype if hometemp=="E" & work_msoa!="OD0000001" [fw=all], mi		
			tab flowtype if hometemp=="E" & work_msoa!="OD0000001" [fw=bicycle], mi	

		* COLLAPSE 'OTHER' FLOW TYPES (1 of 2), AND MERGE IN LSOA-CREATED NO MALES/FEMALES (D3)
			foreach var of varlist all- other {
			bysort home_msoa work_msoa: egen `var'temp=sum(`var')
			replace `var'=`var'temp
			drop `var'temp
			}
			drop hometemp worktemp
			duplicates drop

			merge 1:1 home_msoa work_msoa using "1b_datacreated\0temp\msoa_t2w_sex.dta", nogen
							
			count if ( allcom_male+ allcom_female)!=all  // CHECK ZERO
				* NB IN THE 'OTHER' ONES THE NO. MALES/FEMALES DIFFERED SOMETIMES FROM THE MSOA DATASET - PERHAPS THE 'OVERSEAS' ARE NOT INCLUDED IN THE SAME WAY - BUT THAT DOES NOT MATTER TO OUR ESTIMATES
			order home_msoa_name home_la_name, after(work_msoa)

		* LIMIT TO COMMUTERS LIVING IN ENGLAND
			drop if work_msoa=="OD0000001"
				* drop if work mainly at home
			gen hometemp=substr(home_msoa,1,1)
			keep if hometemp=="E"
				* drop if don't live in England

			**************** save "1a_dataoriginal\Census2011\Flow_Level\MSOA_T2W\example.dta", replace
		    save "1a_dataoriginal\Census2011\Flow_Level\MSOA_T2W\wu03bew_preproc.dta", replace
				
	*****************
	** STEP 1: COLLAPSE TYPE 4 FLOWS TO 'OTHER'
	*****************	
		* COLLAPSE ALL OUT-OF-SCOPE FLOWS TO 'OTHER' (2 of 2)
			replace work_msoa="other" if flowtype==4
			foreach var of varlist all-other allcom_male-bicycle_female {         
			bysort home_msoa work_msoa: egen `var'temp=sum(`var')					
			replace `var'=`var'temp													
			drop `var'temp															
			}
			foreach var of varlist dist dist_fast avslope_perc {						
			replace `var'=. if work_msoa=="other"	// MAKE DISTANCE VARS MISSING IF 'OTHER'  
			}
			duplicates drop
			
	*****************
	** STEP 2: ASSIGN VALUES IF DISTANCE/HILLINESS UNKNOWN
	*****************				
		** ASSIGN HILLINESS + DISTANCE VALUES IF START AND END IN SAME PLACE
			by home_msoa (dist_fast), sort: gen littlen=_n
			foreach x in dist_fast avslope_perc {
			gen `x'temp=`x'
			replace `x'temp=. if littlen>3
			bysort home_msoa: egen `x'temp2=mean(`x'temp)
			replace `x'=`x'temp2 if flowtype==2 & `x'==.
			}
			replace dist_fast=dist_fast/3 if flowtype==2 
				* DISTANCE = A THIRD OF THE MEAN DISTANCE OF SHORTEST 3 FLOWS
				* HILLINESS = MEAN HILLINESS OF SHORTEST 3 FLOWS
			recode dist_fast .=0.79 if home_msoa=="E02006781" & work_msoa=="E02006781"  //ISLES OF SCILLY - ESTIMATED FROM DISTANCE DISTRIBUTION
			recode avslope_perc .=0.2 if home_msoa=="E02006781" & work_msoa=="E02006781"  //ISLES OF SCILLY - ESTIMATED FROM CYCLE STREET 
			drop littlen dist_fasttemp dist_fasttemp2 avslope_perctemp avslope_perctemp2 
			
			
		** ASSIGN DISTANCE *AMONG CYCLISTS* VALUES IF NO FIXED PLACE : MEAN DIST AMONG CYCLISTS TRAVELLING <15KM
			gen cycdist_fast=dist_fast
			foreach x in 15 30 {
			gen dist_fast`x'=dist_fast
			replace dist_fast`x'=. if dist_fast>`x' | flowtype>2
			gen bicycle`x'=bicycle
			replace bicycle`x'=. if dist_fast`x'==.
			}
			bysort home_msoa: egen numndist_fast15=sum(dist_fast15*bicycle15)
			bysort home_msoa: egen dendist_fast15=sum(bicycle15)
			gen meandist_fast15=numndist_fast15/dendist_fast15
			replace cycdist_fast=meandist_fast15 if flowtype==3 

		** ASSIGN DISTANCE VALUES IF OVERSEAS OR >30KM: MEAN DIST AMONG CYCLISTS TRAVELLING <30KM
			egen numndist_fast30=sum(dist_fast30*bicycle30)
			egen dendist_fast30=sum(bicycle30)
			gen meandist_fast30=numndist_fast30/dendist_fast30
			replace cycdist_fast=meandist_fast30 if flowtype==4 
			
			gen dist_fastmissing=(dist_fast==.)
			gen cycdist_fastmissing=(cycdist_fast==.)
			tab flowtype dist_fastmissing, mi	//SHOULD BE ZERO IN TYPES 1 AND 2
			tab flowtype cycdist_fastmissing, mi	//SHOULD BE ZERO IN ALL CATEGORIES
	
	
	
	** SAVE RELEVANT VARIABLES (USE THIS TO FIT INDIVIDUAL MODEL)
		order home_msoa work_msoa home_msoa_name all-other flowtype allcom_male- bicycle_female mortrate_govtarget mortrate_gendereq mortrate_dutch /*
			*/ dist dist_fast avslope_perc cycdist_fast
		keep home_msoa-cycdist_fast
		compress

		saveold "1b_datacreated\0temp\MSOA_ODpairs_process2.1.dta", replace
		use "1b_datacreated\0temp\MSOA_ODpairs_process2.1.dta"

	*****************
	** STEP 3A: CALCULATE PROPENSITY TO CYCLE
	*****************
		** MODEL FITTING FOR TRIPS <30KM
			* INPUT PARAMETERS
				gen dist_fastsq=dist_fast^2
				gen dist_fastsqrt=sqrt(dist_fast)
				gen ned_avslope_perc=avslope_perc-0.57 
				gen interact=dist_fast*ned_avslope_perc
				gen interactsqrt=dist_fastsqrt*ned_avslope_perc
				
			* FIT REGRESSION EQUATION
				gen pred_base= /*
					*/ -3.894 + (-0.5872 * dist_fast) + (1.832 * dist_fastsqrt) + (0.007956 * dist_fastsq) + (-0.2872 * ned_avslope_perc) + (0.01784 * dist_fast*ned_avslope_perc) + (-0.09770 * dist_fastsqrt*ned_avslope_perc) 
				gen bdutch = 2.499+(-0.07384*dist_fast)					// FROM DUTCH NTS
				
				
				replace bdutch=. if flowtype==3
				gen bebike= (0.05710*dist_fast)+(-0.0001087*dist_fastsq)	// PARAMETERISED FROM DUTCH TRAVEL SURVEY, ASSUMING NO DIFFERENCE AT 0 DISTANCE
				replace bebike=bebike+(-0.67 * -0.2872 *ned_avslope_perc)	// SWISS TRAVEL SURVEY
				gen pred_dutch= pred_base + bdutch
				gen pred_ebike= pred_dutch + bebike
				foreach x in base dutch ebike {
				replace pred_`x'=exp(pred_`x')/(1+exp(pred_`x'))
				}

			** MODEL FITTING FOR TRIPS WITH NO FIXED PLACE
			* INPUT PARAMETERS
				foreach x in pred_base bdutch bebike { // WEIGHTED AVERAGE OF COMMUTERS IN FLOWTYPE 1 & "
				gen `x'temp=`x'
				replace `x'temp=. if flowtype!=1 & flowtype!=2	// WEIGHTED AVERAGE OF COMMUTERS IN FLOWTYPE 1 & "
				gen alltemp=all
				replace alltemp=. if `x'temp==.
				bsort home_msoa: egen nummean`x'=sum(`x'temp*alltemp)      
				bysort home_msoa: egen denmean`x'=sum(alltemp)
				gen mean`x'=nummean`x'/denmean`x'
				drop `x'temp alltemp nummean`x' denmean`x'
				}
				gen meanpred_basesq=meanpred_base^2
				gen meanpred_basesqrt=meanpred_base^0.5

			* FIT REGRESSION EQUATION
				gen pred2_base= -6.218 + (189.9 * meanpred_basesq) + (9.275 * meanpred_basesqrt) 
				gen pred2_dutch= pred2_base + meanbdutch
				gen pred2_ebike= pred2_dutch + meanbebike
				foreach x in base dutch ebike {
				replace pred2_`x'=exp(pred2_`x')/(1+exp(pred2_`x'))
				replace pred_`x'=pred2_`x' if flowtype==3
				drop pred2_`x'
				}

				** DROP INTERMEDIARY VARIABLES
			drop dist_fastsq dist_fastsqrt ned_avslope_perc interact interactsqrt meanpred_base bdutch bebike meanbdutch meanbebike meanpred_basesq meanpred_basesqrt

	*****************
	** PART 3B: APPLY SCENARIOS TO MSOA DATA
	*****************
		** CALCULATE NO. CYCLISTS IN EACH SCENARIO
			gen nocyclists_slc=0
			gen nocyclists_sic=nocyclists_slc-bicycle
		
			gen govtarget_slc=bicycle+(pred_base*all)
			replace govtarget_slc=all if govtarget_slc>all & govtarget_slc!=. // MAXIMUM PERCENT CYCLISTS IS 100%
			gen govtarget_sic=govtarget_slc-bicycle
			order govtarget_slc, before(govtarget_sic)
	
			gen gendereq_slc=(bicycle_male*(1 + (allcom_female/allcom_male)))
			replace gendereq_slc=all if gendereq_slc>all & gendereq_slc!=. // [not needed] MAXIMUM PERCENT CYCLISTS IS 100%
			*tab all if female==0 | male==0		
			replace gendereq_slc=bicycle if allcom_female==0 	// [not needed] NO CHANGE IF NO FEMALES IN FLOW
			replace gendereq_slc=bicycle if allcom_male==0 	// NO CHANGE IF NO MALES IN FLOW
			replace gendereq_slc=bicycle if gendereq_slc<bicycle 	// NO CHANGE IF SLC < BASELINE
			gen gendereq_sic=gendereq_slc-bicycle
	
			foreach x in dutch ebike {
			gen `x'_slc=pred_`x'*all
			replace `x'_slc=all if `x'_slc>all & `x'_slc!=. // MAXIMUM PERCENT CYCLISTS IS 100%
			*tab all if `x'_slc<bicycle
			replace `x'_slc=bicycle if `x'_slc<bicycle 		 // MINIMUM NO. CYCLISTS IS BASELINE
			gen `x'_sic=`x'_slc-bicycle
			}
			foreach x in govtarget gendereq dutch ebike {
			replace `x'_slc=bicycle if work_msoa=="other"	// NO INCREASE AMONG FLOWS OUT OF SCOPE AS TOO LONG/OVERSEAS ETC
			replace `x'_sic=0 if work_msoa=="other"
			}
	
		** CALCULATE % NON-CYCLISTS MADE CYCLISTS IN EACH SCENARIO: TURN THAT % AWAY FROM WALKING
			foreach x in nocyclists {
			gen pchange_`x'=(all-`x'_slc)/(all-bicycle) 
	
		
	
	
			gen `x'_slw=foot*pchange_`x'					// most flows - scale walking according to %change
			replace `x'_slw=((all-`x'_slc)*0.31) if bicycle==all	// Flows with pure bicycles at baseline - make walking 13% of new flows
			gen `x'_siw=`x'_slw-foot
			gen `x'_sld=car_driver*pchange_`x'			
			replace `x'_sld=((all-`x'_slc)*0.35) if bicycle==all	// Flows with pure bicycles at baseline - make driving 44% of new flows
			gen `x'_sid=`x'_sld-car_driver	
			order `x'_slw `x'_siw `x'_sld `x'_sid, after(`x'_sic)
			}
			
			foreach x in govtarget gendereq dutch ebike {
			gen pchange_`x'=(all-`x'_slc)/(all-bicycle) 	// % change in non-cycle modes
			recode pchange_`x' .=1 if all==bicycle 			// make 1 (i.e. no change) if everyone in the flow cycles
			gen `x'_slw=foot*pchange_`x'
			gen `x'_siw=`x'_slw-foot
			gen `x'_sld=car_driver*pchange_`x'
			gen `x'_sid=`x'_sld-car_driver
			order `x'_slw `x'_siw `x'_sld `x'_sid, after(`x'_sic)
			}
		
		** DROP INTERMEDIARY VARIABLES
			compress
			drop pred_base pred_dutch pred_ebike 
			drop pchange_nocyclists pchange_govtarget pchange_gendereq pchange_dutch pchange_ebike

	*****************
	** STEP 4: DO HEAT
	*****************
		* INPUT PARAMETERS
			gen cyclecommute_tripspertypicalweek = 7.16 	
			gen cspeed = 14	
			gen wspeed = 4.8
			gen ebikespeed = 15.8
			gen ebikemetreduction = 0.648
			recode cycdist_fast min/4.9999999=.06 5/9.9999999=.11 10/19.999999=.17 20/max=.23, gen(percentebike_dutch)
			recode cycdist_fast min/4.9999999=.71 5/19.9999999=.92 20/max=1, gen(percentebike_ebike)	
			gen crr_webtag=0.72
			gen crr_heat=0.9 
			gen cdur_ref_webtag=180
			gen cdur_ref_heat=100	
			gen wrr_webtag=0.78
			gen wrr_heat=0.89 
			gen wdur_ref_webtag=203
			gen wdur_ref_heat=168
			gen mortrate_nocyclists=mortrate_govtarget
			gen mortrate_ebike=mortrate_dutch
			gen vsl=1855315		// VALUE IN POUNDS
			
		* DURATION OF CYCLING/WALKING
			gen cdur_obs = 60*((cycdist_fast*cyclecommute_tripspertypicalweek)/cspeed) // TIME CYCLING PER DAY IN MINUTES AMONG NEW CYCLISTS
			gen cdur_obs_dutch=((1-percentebike_dutch)*cdur_obs)+(percentebike_dutch*cdur_obs*ebikemetreduction*(cspeed/ebikespeed))
			gen cdur_obs_ebike=((1-percentebike_ebike)*cdur_obs)+(percentebike_ebike*cdur_obs*ebikemetreduction*(cspeed/ebikespeed))
			
			gen wdur_obs = 60*((cycdist_fast*cyclecommute_tripspertypicalweek)/wspeed) // TIME WALKING PER DAY IN MINUTES AMONG THOSE NOW SWITCHING TO CYCLING
		*	drop cyclecommute_tripspertypicalweek cspeed wspeed ebiketimereduction ebikemetreduction percentebike_dutch percentebike_ebike
		*	compress 

		* MORTALITY PROTECTION
			foreach z in webtag heat {
			gen cprotection_govtarget_`z'= (1-crr_`z')*(cdur_obs/cdur_ref_`z')	// SCALE RR DEPENDING ON HOW DURATION IN THIS POP COMPARES TO REF
			gen cprotection_nocyclists_`z'=cprotection_govtarget_`z'
			gen cprotection_gendereq_`z'=cprotection_govtarget_`z'
			gen cprotection_dutch_`z'= (1-crr_`z')*(cdur_obs_dutch/cdur_ref_`z')
			gen cprotection_ebike_`z'= (1-crr_`z')*(cdur_obs_ebike/cdur_ref_`z')
			}
			foreach x in nocyclists govtarget gendereq dutch ebike {			
		*	recode cprotection_`x'_webtag 0.5/max=0.5	
			recode cprotection_`x'_heat 0.45/max=0.45			
			}
			gen wprotection_webtag= (1-wrr_webtag)*(wdur_obs/wdur_ref_webtag)
			gen wprotection_heat= (1-wrr_heat)*(wdur_obs/wdur_ref_heat)
		*	recode wprotection_webtag 0.50/max=0.5
			recode wprotection_heat 0.30/max=0.30		
			
		* DEATHS AND VALUES
			foreach z in webtag heat {
			foreach x in nocyclists govtarget gendereq dutch ebike {
			gen `x'_sic_death_`z'=`x'_sic*mortrate_`x'*cprotection_`x'_`z'*-1
			gen `x'_siw_death_`z'=`x'_siw*mortrate_`x'*wprotection_`z'*-1
			gen `x'_sideath_`z'=`x'_sic_death_`z'+`x'_siw_death_`z'
			gen long `x'_sivalue_`z'=`x'_sideath_`z'*vsl*-1
			drop `x'_sic_death_`z' `x'_siw_death_`z'
			}
			gen base_sldeath_`z'=-1*nocyclists_sideath_`z'	// BASELINE LEVEL IS INVERSE OF 'NO CYCLISTS' SCENARIO INCREASE
			gen base_slvalue_`z'=-1*nocyclists_sivalue_`z'			
			foreach x in govtarget gendereq dutch ebike {
			gen `x'_sldeath_`z'=`x'_sideath_`z'+base_sldeath_`z'
			gen long `x'_slvalue_`z'=`x'_sivalue_`z'+base_slvalue_`z'
			order `x'_sideath_`z' `x'_sivalue_`z', after(`x'_slvalue_`z')
			}
			}
		
		* DROP INTERMEDIARY VARIABLES
			drop mortrate_govtarget mortrate_gendereq mortrate_dutch cyclecommute_tripspertypicalweek - wprotection_heat
			drop nocyclists_sideath_webtag nocyclists_sivalue_webtag nocyclists_sideath_heat nocyclists_sivalue_heat

	*****************
	**  STEP 5: DO CO2 EMISSIONS CALCS 
	*****************
		gen cyclecommute_tripsperweek=5.24
		gen co2kg_km=0.186
		foreach x in nocyclists govtarget gendereq dutch ebike {
		gen long `x'_sico2=`x'_sid * cycdist_fast * cyclecommute_tripsperweek * 52.2 * co2kg_km 	// NO CYCLISTS * DIST * COMMUTE PER DAY * CO2 EMISSIONS FACOTR
		}
		gen base_slco2=-1*nocyclists_sico2	// BASELINE LEVEL IS INVERSE OF 'NO CYCLISTS' SCENARIO INCREASE
		foreach x in govtarget gendereq dutch ebike {
		gen long `x'_slco2=`x'_sico2+base_slco2
		order `x'_sico2 , after(`x'_slco2)
		}
		drop nocyclists* cyclecommute_tripsperweek co2kg_km cycdist_fast

	*****************
	** FINISH: SAVE TEMPORARY DATASET, PRE-AGGREGATION
	*****************
		compress 
	    saveold "1b_datacreated\0temp\MSOA_ODpairs_process2.5.dta", replace
x
	*****************
	** PART 3A: AGGREGATE TO AREA LEVEL
	*****************
	
		use "1b_datacreated\0temp\MSOA_ODpairs_process2.5.dta
		
		* AGGREGATE UP AREA FIGURES -- this takes LOOOOOOOOOONG
			foreach var of varlist all- other govtarget_slc- ebike_sico2 {
			bysort home_msoa: egen a_`var'=sum(`var')
			}
		* AREA FILE KEEP+RENAME+ORDER
			keep home_* a_*
			rename a_* *
			drop from_home
			order home_msoa home_msoa_name all light_rail- other 
			duplicates drop
		* ROUND
			foreach var of varlist govtarget_slc- ebike_sid {
			replace `var'=round(`var',0.001)
			}
			foreach x in base_sl govtarget_sl govtarget_si gendereq_sl gendereq_si dutch_sl dutch_si ebike_sl ebike_si {
			foreach y in death_webtag death_heat{
			replace `x'`y'=round(`x'`y',0.00001)
			}
			foreach y in value_webtag value_heat {
			replace `x'`y'=round(`x'`y',1)
			}
			replace `x'co2=round(`x'co2,0.0001)
			}
			* THIS MOST LIKELY IS NOT NEEDED IN STATA
		*LABEL, SAVE
			label var home_msoa "home MSOA, code"
			label var home_msoa_name "home MSOA, name"
			label var all "total no. commuters"
			label var light_rail "baseline no. light rail"
			label var train "baseline no. train"
			label var bus "baseline no. bus"
			label var taxi "baseline no. taxi"
			label var motorbike "baseline no. motorbike"
			label var car_driver "baseline no. car drivers"
			label var car_passenger "baseline no. car passengers"
			label var bicycle "baseline no. cyclists"
			label var foot "baseline no. pedestrians"
			label var other "baseline no. other-mode commuters"			
			foreach x in base {
				foreach y in webtag heat {
				label var `x'_sldeath_`y' "change in deaths/year in `x' versus 'no cycling', using `y'"
				label var `x'_slvalue_`y' "value from change in deaths in `x' versus 'no cycling', using `y'"
				}
				label var `x'_slco2 "change in CO2 (kg) in `x' versus 'no cycling'"
			}			
			foreach x in govtarget gendereq dutch ebike {
				label var `x'_slc "`x' no. cyclists"
				label var `x'_sic "`x' increase cyclists relative to baseline"
				label var `x'_slw "`x' no. pedestrians"
				label var `x'_siw "`x' increase pedestrians relative to baseline"
				label var `x'_sld "`x' no. car drivers"
				label var `x'_sid "`x' increase car drivers relative to baseline"
				foreach y in webtag heat {
				label var `x'_sldeath_`y' "change in deaths/year in `x' versus 'no cycling', using `y'"
				label var `x'_slvalue_`y' "value from change in deaths in `x' versus 'no cycling', using `y'"
				label var `x'_sideath_`y' "change in deaths/year in `x' versus 'baseline', using `y'"
				label var `x'_sivalue_`y' "value from change in deaths in `x' versus 'baseline', using `y'"
				}
				label var `x'_slco2 "change in CO2/year (kg) in `x' versus 'no cycling'"
				label var `x'_sico2 "change in CO2/year (kg) in `x' versus 'baseline'"
			}
			
			saveold "1b_datacreated\pct_area.dta", replace 
			
			
			*export delimited using "1b_datacreated\pct_area.csv", replace
			*desc
x			
	*****************
	** PART 3B: AGGREGATE TO FLOW LEVEL
	*****************
		
		use "1b_datacreated\0temp\MSOA_ODpairs_process2.5.dta
		
		* MAKE BIDIRECTIONAL MSOAS
			gen homesub=substr(home_msoa,2,.)
			gen worksub=substr(work_msoa,2,.)
			destring homesub worksub, replace force
			egen msoa1=rowmin(homesub worksub)
			egen msoa2=rowmax(homesub worksub)
			tostring msoa1 msoa2, replace
			replace msoa1="E0"+msoa1
			replace msoa2="E0"+msoa2
			replace msoa1=home_msoa if work_msoa=="OD0000003" | work_msoa=="other"
			replace msoa2=work_msoa if work_msoa=="OD0000003" | work_msoa=="other"
			*gen test=(msoa1==home_msoa & msoa2==work_msoa) | (msoa2==home_msoa & msoa1==work_msoa)
			*tab test		// SHOULD ALL BE 1
		* AGGREGATE UP FLOW FIGURES                       =======>  LOOOOOOONGEST OF ALL !!!
			foreach var of varlist all- other govtarget_slc- ebike_sico2 {
			bysort msoa1 msoa2: egen f_`var'=sum(`var')
			}
		
		
		* FLOW FILE KEEP + RENAME + ORDER 
			keep msoa1 msoa2 f_*
			rename f_* *
			drop from_home
			order msoa1 msoa2 all light_rail- other 
			duplicates drop		
	
		*	foreach var of varlist avslope_perc- distq_f {
		*	replace `var'=round(`var',0.01)
		*	}
			foreach var of varlist govtarget_slc- ebike_sid {
			replace `var'=round(`var',0.01)
			}
			foreach x in base_sl govtarget_sl govtarget_si gendereq_sl gendereq_si dutch_sl dutch_si ebike_sl ebike_si {
			foreach y in death_webtag death_heat{
			replace `x'`y'=round(`x'`y',0.00001)
			}
			foreach y in value_webtag value_heat {
			replace `x'`y'=round(`x'`y',1)
			}
			replace `x'co2=round(`x'co2,0.01)
			}
		*LABEL, SAVE
			label var msoa1 "MSOA 1"
			label var msoa2 "MSOA 2"
			label var all "total no. commuters"
			label var light_rail "baseline no. light rail"
			label var train "baseline no. train"
			label var bus "baseline no. bus"
			label var taxi "baseline no. taxi"
			label var motorbike "baseline no. motorbike"
			label var car_driver "baseline no. car drivers"
			label var car_passenger "baseline no. car passengers"
			label var bicycle "baseline no. cyclists"
			label var foot "baseline no. pedestrians"
			label var other "baseline no. other-mode commuters"
			foreach x in base {
				foreach y in webtag heat {
				label var `x'_sldeath_`y' "change in deaths/year in `x' versus 'no cycling', using `y'"
				label var `x'_slvalue_`y' "value from change in deaths in `x' versus 'no cycling', using `y'"
				}
				label var `x'_slco2 "change in CO2 (kg) in `x' versus 'no cycling'"
			}			
			foreach x in govtarget gendereq dutch ebike {
				label var `x'_slc "`x' no. cyclists"
				label var `x'_sic "`x' increase cyclists relative to baseline"
				label var `x'_slw "`x' no. pedestrians"
				label var `x'_siw "`x' increase pedestrians relative to baseline"
				label var `x'_sld "`x' no. car drivers"
				label var `x'_sid "`x' increase car drivers relative to baseline"
				foreach y in webtag heat {
				label var `x'_sldeath_`y' "change in deaths/year in `x' versus 'no cycling', using `y'"
				label var `x'_slvalue_`y' "value from change in deaths in `x' versus 'no cycling', using `y'"
				label var `x'_sideath_`y' "change in deaths/year in `x' versus 'baseline', using `y'"
				label var `x'_sivalue_`y' "value from change in deaths in `x' versus 'baseline', using `y'"
				}
				label var `x'_slco2 "change in CO2/year (kg) in `x' versus 'no cycling'"
				label var `x'_sico2 "change in CO2/year (kg) in `x' versus 'baseline'"
			}
		
		
			saveold "1b_datacreated\pct_lines.dta", replace 
			
				
x			
	******************
	* ERASE LARGE FILES
	******************
		erase "1b_datacreated\0temp\GM_MSOA_ODpairs_process2.5.dta"
		erase "1b_datacreated\0temp\GM_MSOA_ODpairs_process2.1.dta"       
		


