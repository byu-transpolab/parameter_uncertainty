;;<<Default Template>><<NETWORK>><<Default>>;;
; Do not change filenames or add or remove FILEI/FILEO statements using an editor. Use Cube/Application Manager.
RUN PGM=NETWORK PRNFILE="{SCENARIO_DIR}\Output\Merge_Hwy_Assignments.PRN"
FILEO NETO = "{SCENARIO_DIR}\Output\LOADED_{Year}{Alternative}.NET",
 EXCLUDE = V_1, VC_1, V1_1, V2_1, V3_1, V4_1, VT_1, V1T_1, V2T_1, V3T_1, V4T_1, TIME_1 ,CSPD_1 ,VDT_1, VDT_2 ,VHT_1 ,TIME_2 ,VC_2 ,CSPD_2 ,CDT_2 ,VHT_2 ,V_2 ,V1_2 ,V2_2 ,V3_2 ,V4_2 ,V5_2 ,V6_2 ,V7_2 ,V8_2 ,VT_2 ,V1T_2 ,V2T_2 ,V3T_2 ,V4T_2 ,V5T_2 ,V6T_2 ,V7T_2 ,V8T_2

FILEI LINKI[4] = "{SCENARIO_DIR}\Output\LOADED_NT.NET"
FILEI LINKI[3] = "{SCENARIO_DIR}\Output\LOADED_PM.NET"
FILEI LINKI[2] = "{SCENARIO_DIR}\Output\LOADED_MD.NET"
FILEI LINKI[1] = "{SCENARIO_DIR}\Output\LOADED_AM.NET"
FILEO PRINTO[1] = "{SCENARIO_DIR}\Output\Hwy_eval_period.csv"
 
; =========================================================
; LINKMERGE PHASE
; =========================================================
PHASE=LINKMERGE  
; Total Volume
AM_Vol=li.1.V_2 
MD_Vol=li.2.V_2
PM_Vol=li.3.V_2 
NT_Vol=li.4.V_2 
Total_Vol = AM_Vol + MD_Vol + PM_Vol + NT_Vol

; Drive Alone
AM_DA=li.1.V1_2 
MD_DA=li.2.V1_2 
PM_DA=li.3.V1_2 
NT_DA=li.4.V1_2 
TOTAL_DA = AM_DA + MD_DA + PM_DA + NT_DA

; ShareRide 2+
AM_SR=li.1.V2_2 
MD_SR=li.2.V2_2 
PM_SR=li.3.V2_2 
NT_SR=li.4.V2_2 
TOTAL_SR = AM_SR + MD_SR + PM_SR + NT_SR

; IEEI
AM_IEEI=li.1.V3_2 
MD_IEEI=li.2.V3_2 
PM_IEEI=li.3.V3_2 
NT_IEEI=li.4.V3_2 
TOTAL_IEEI = AM_IEEI + MD_IEEI + PM_IEEI + NT_IEEI

; Commercial Vehicles
AM_CV=li.1.V4_2 
MD_CV=li.2.V4_2 
PM_CV=li.3.V4_2 
NT_CV=li.4.V4_2 
TOTAL_CV = AM_CV + MD_CV + PM_CV + NT_CV

; EE
AM_EX=li.1.V1_1 
MD_EX=li.2.V1_1 
PM_EX=li.3.V1_1 
NT_EX=li.4.V1_1 
TOTAL_EX = AM_EX + MD_EX + PM_EX + NT_EX

;Sum period specific loaded attributes to all period (24 hour)...
; Congested Speed by Time Period
AM_CongSpeed=li.1.CSPD_2 
MD_CongSpeed=li.2.CSPD_2 
PM_CongSpeed=li.3.CSPD_2 
NT_CongSpeed=li.4.CSPD_2 

; Congested Time by Time Period
AM_Time=li.1.TIME_2 
MD_Time=li.2.TIME_2 
PM_Time=li.3.TIME_2 
NT_Time=li.4.TIME_2 

; Vehicle Hours of Delay (Minutes)
AM_VDT=li.1.VDT_2 
MD_VDT=li.2.VDT_2 
PM_VDT=li.3.VDT_2 
NT_VDT=li.4.VDT_2 
TOTAL_VDT = AM_VDT + MD_VDT + PM_VDT + NT_VDT

; Vehcile Hours of Travel (Miles)
AM_VHT=li.1.VHT_2                               
MD_VHT=li.2.VHT_2                               
PM_VHT=li.3.VHT_2                               
NT_VHT=li.4.VHT_2                               
TOTAL_VHT = AM_VHT + MD_VHT + PM_VHT + NT_VHT   

; Volume to Capacity Ratio
AM_VC=li.1.VC_2 
MD_VC=li.2.VC_2 
PM_VC=li.3.VC_2 
NT_VC=li.4.VC_2

IF (Total_Vol > 0)
  TOTAL_VC = AM_VC * (AM_Vol/Total_Vol) + MD_VC * (MD_Vol/Total_Vol)+ PM_VC * (PM_Vol/Total_Vol) + NT_VC * (NT_Vol/Total_Vol) 
ELSE
  TOTAL_VC = 0
ENDIF

; Select Link Volumes
sl_DA=li.1.V5_2 +li.2.V5_2 +li.3.V5_2 +li.4.V5_2  
sl_SR=li.1.V6_2 +li.2.V6_2 +li.3.V6_2 +li.4.V6_2  
sl_IEEI=li.1.V7_2 +li.2.V7_2 +li.3.V7_2 +li.4.V7_2  
sl_CV=li.1.V8_2 +li.2.V8_2 +li.3.V8_2 +li.4.V8_2  
sl_EE=li.1.V2_1 +li.2.V2_1 +li.3.V2_1 +li.4.V2_1  
sl_tot=sl_DA+ sl_SR +  sl_IEEI + sl_CV + sl_EE

; write out peak and off-peak congested time to use in feedback
SPEED_PK_CNG =  li.1.CSPD_2  ; AM Congested Speed
TIME_PK_CNG = li.1.TIME_2    ; AM Congested Time
SPEED_OP_CNG =  li.2.CSPD_2  ; MD Congested Speed
TIME_OP_CNG = li.2.TIME_2    ; MD Congested Time

; Group facility types
oft=LI.1.FACTYPE                                 ; Facility Type
if (oft = 1 | oft =2 | oft =9 | oft = 10) ft = 1 ; Freeway       
if (oft = 3 | oft =4)                     ft = 2 ; Major Arterial
if (oft = 5)                              ft = 3 ; Minor Arterial
if (oft = 6 | oft = 7 | oft = 8)          ft = 4 ; Collector + Local         
if (oft = 10)                             ft = 5 ; Connectors   

; Write a header file for links
 if (A=1) print CSV=T,list='ScreenLine', 'A','B','Area Type','Fac Type','Fac Type Group', 'FFSPEED', 'Count' ,'Volume','Distance', 'AM_Vol', 'MD_Vol',  'PM_Vol', 'NT_Vol',  'AM_VMT', 'MD_VMT' , 'PM_VMT' , 'NT_VMT',   
 'AM_VHT', 'MD_VHT' , 'PM_VHT' , 'NT_VHT', 'AM_Speed', 'MD_Speed' , 'PM_Speed' , 'NT_Speed' printo=1 
 
IF (Li.1.AAWDT>0)
 ; Write out links with counts
 print CSV=T list = LI.1.SCREENLN , LI.1.A, LI.1.B, LI.1.ATYPE, LI.1.FACTYPE, FT,  FFSPEED, LI.1.AAWDT, TOTAL_VOL, LI.1.DISTANCE,  AM_Vol, MD_Vol,  PM_Vol, NT_Vol, li.1.VDT_2, li.2.VDT_2, li.3.VDT_2 , li.4.VDT_2, li.1.VHT_2,  li.2.VHT_2,  li.3.VHT_2,  li.4.VHT_2, li.1.CSPD_2, li.2.CSPD_2, li.3.CSPD_2, li.4.CSPD_2 printo=1

 ENDIF
 
 
ENDPROCESS
ENDRUN
