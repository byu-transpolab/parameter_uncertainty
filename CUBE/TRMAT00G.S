; Do not change filenames or add or remove FILEI/FILEO statements using an editor. Use Cube/Application Manager.
RUN PGM=MATRIX PRNFILE="{SCENARIO_DIR}\OUTPUT\LOGS\MC_Transit_Trips.PRN" MSG='Transit Trips'
FILEO MATO[2] = "{SCENARIO_DIR}\Output\OP_Transit.MAT",
MO = 3-4, NAME = WBUS, WPREM

FILEI MATI[1] = "{SCENARIO_DIR}\OUTPUT\NHB_MCTRIPS.MAT"
FILEI MATI[4] = "{SCENARIO_DIR}\OUTPUT\HBSC_MCTRIPS.MAT"
FILEI MATI[3] = "{SCENARIO_DIR}\OUTPUT\HBO_MCTRIPS.MAT"
FILEI MATI[2] = "{SCENARIO_DIR}\OUTPUT\HBW_MCTRIPS.MAT"
FILEO MATO[1] = "{SCENARIO_DIR}\Output\PK_Transit.MAT",
MO = 1-2, NAME = WBUS, WPREM

; Write out HBW transit trips as Peak Trn trips
MW[1] =  MI.2.5 
MW[2] =  MI.2.3 

; Write out Off-Peak Trn trips
MW[3] = MI.1.5 + MI.3.5 + MI.4.3
MW[4] = MI.1.3 + MI.3.3 + MI.4.4

ENDRUN
