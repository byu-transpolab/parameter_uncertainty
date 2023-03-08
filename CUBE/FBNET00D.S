; Do not change filenames or add or remove FILEI/FILEO statements using an editor. Use Cube/Application Manager.
RUN PGM=NETWORK
FILEO LINKO = "{SCENARIO_DIR}\Output\LHS_LOADED.DBF"
FILEI LINKI[1] = "{SCENARIO_DIR}\Output\LOADED_{Year}{Alternative}.NET"


ENDRUN
