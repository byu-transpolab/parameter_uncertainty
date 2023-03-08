; Do not change filenames or add or remove FILEI/FILEO statements using an editor. Use Cube/Application Manager.
RUN PGM=HIGHWAY PRNFILE="{SCENARIO_DIR}\Output\Logs\@PERIOD_NAME@.PRN" MSG='Load Final Trips onto Network'
FILEI TURNPENI = "{SCENARIO_DIR}\Input\TurnPenalties.pen"
FILEO MATO[1] = "{SCENARIO_DIR}\Output\SL_Loaded_@PERIOD_NAME@.MAT",
MO=5-8, NAME=SL_DA,SL_SR,SL_IEEI, SL_CV
FILEI NETI = "{SCENARIO_DIR}\Output\PRELOADED_@PERIOD_NAME@.NET"
FILEI MATI[1] = "{SCENARIO_DIR}\Output\ODAUTO_@PERIOD_NAME@.MAT"
FILEO PATHO[1] = "{SCENARIO_DIR}\Output\LOADED_@PERIOD_NAME@.PTH"
FILEO NETO = "{SCENARIO_DIR}\Output\LOADED_@PERIOD_NAME@.NET",
      DEC = 0

;Set run PARAMETERS and Controls
PARAMETERS ZONES={Total ZONES}, MAXITERS=500, COMBINE=EQUI, GAP= 0.0, RELATIVEGAP = 0.00001

PHASE=LINKREAD
    LW.EEVOL = LI.V_1 
    
   T0 = 60* (LI.DISTANCE/LI.FFSPEED)
  ; T0 = 60* (LI.DISTANCE/LI.POST_SPEED)
  C  = LI.CAPE_@PERIOD_NAME@ 

  LW.COSTa = T0 + 0.25*LI.DISTANCE
  LW.COSTb = T0 + 0.25*LI.DISTANCE
  LW.COSTc = T0 + 0.25*LI.DISTANCE
  LW.COSTd = T0 + 0.25*LI.DISTANCE
  
/*
1	Interstate/Principal Freeway
2	Minor Freeway
3	Principal Arterial
4	Major Arterial
5	Minor Arterial
6	Major Collector
7	Minor Collector
8	Local
9	High-speed Ramp
10	Low-speed Ramp
11	Centroid Connector
12	External Station Connector
*/

; Group facility types
  IF(LI.FACTYPE=1,2,9,10)    LINKCLASS=1 ; Freeway
  IF(LI.FACTYPE=3,4)    LINKCLASS=2 ; Major Arterial
  IF(LI.FACTYPE=5)      LINKCLASS=3 ; Minor Arterial
  IF(LI.FACTYPE=6,7)    LINKCLASS=4 ; Collector
  IF(LI.FACTYPE=8)      LINKCLASS=5 ; Local
  IF(LI.FACTYPE>10)     LINKCLASS=6 ; Connectors

ENDPHASE

PHASE=ILOOP
/*
  PATHLOAD VOL[1] = MI.1.1,  PATH=LW.COSTa  
  PATHLOAD VOL[2] = MI.1.2,  PATH=LW.COSTb 
  PATHLOAD VOL[3] = MI.1.3,  PATH=LW.COSTc  
  PATHLOAD VOL[4] = MI.1.4,  PATH=LW.COSTd 
  */
  PATHLOAD PATH=LW.COSTa , MW[1] = MI.1.1, VOL[1] = MW[1],   
      MW[5] = MI.1.1, SELECTLINK=({SelectLink}), VOL[5]=MW[5] 
  PATHLOAD PATH=LW.COSTb , MW[2] = MI.1.2, VOL[2] = MW[2],
     ; MW[6] = MI.1.2, SELECTLINK=(L= 4451-4424), VOL[6]=MW[6]      
  MW[6] = MI.1.2, SELECTLINK=({SelectLink}), VOL[6]=MW[6]    
  PATHLOAD PATH=LW.COSTc , MW[3] = MI.1.3, VOL[3] = MW[3] ,
      MW[7] = MI.1.3, SELECTLINK=({SelectLink}), VOL[7]=MW[7]  
  PATHLOAD PATH=LW.COSTd , MW[4] = MI.1.4, VOL[4] = MW[4], 
      MW[8] = MI.1.4, SELECTLINK=({SelectLink}), VOL[8]=MW[8]    
  
ENDPHASE

PHASE=ADJUST

function {
    V=VOL[1]+VOL[2]+VOL[3]+VOL[4]+  LW.EEVOL ; Add preloaded EE Volumes here
   ; V=VOL[1]+VOL[2]+VOL[4]
     ; TC[1] = Min(T0 * (1 + 0.83*(V/C)^5.5), T0*100)    ; Freeway     
     ; TC[2] = Min(T0 * (1 + 0.83*(V/C)^3.7), T0*100)    ; Major Arterial  
    TC[1] = Min(T0 * (1 + 0.65*(V/C)^4.5), T0*100)    ; Freeway        
    TC[2] = Min(T0 * (1 + 0.65*(V/C)^4.0), T0*100)    ; Major Arterial         
    TC[3] = Min(T0 * (1 + 0.83*(V/C)^2.8), T0*100)    ; Minor Arterial    
    TC[4] = Min(T0 * (1 + 0.2*(V/C)^4), T0*100)       ; Collector
    TC[5] = Min(T0 * (1 + 0.6*(V/C)^5.5), T0*100)     ; Local
    TC[6] = T0                                        ; Connectors                       
    }

  LW.COSTa=TIME + 0.25*LI.DISTANCE
  LW.COSTb=TIME + 0.25*LI.DISTANCE
  LW.COSTc=TIME + 0.25*LI.DISTANCE
  LW.COSTd=TIME + 0.25*LI.DISTANCE
  
ENDPHASE
ENDRUN
