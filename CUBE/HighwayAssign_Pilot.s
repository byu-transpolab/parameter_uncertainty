; Do not change filenames or add or remove FILEI/FILEO statements using an editor. Use Cube/Application Manager.

IF (PER=1) ; AM
PERIOD_COL=3
PERIOD_NAME = 'AM'

ELSEIF (PER=2) ; MD
PERIOD_COL=4
PERIOD_NAME = 'MD'

ELSEIF (PER=3) ; PM
PERIOD_COL=5
PERIOD_NAME = 'PM'

ELSEIF (PER=4) ; NT 
PERIOD_COL=6
PERIOD_NAME = 'NT'

ENDIF


