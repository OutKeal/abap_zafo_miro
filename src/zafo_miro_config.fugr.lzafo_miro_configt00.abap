*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZAFO_MIRO_COST_T................................*
DATA:  BEGIN OF STATUS_ZAFO_MIRO_COST_T              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZAFO_MIRO_COST_T              .
CONTROLS: TCTRL_ZAFO_MIRO_COST_T
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZAFO_MIRO_COST_T              .
TABLES: ZAFO_MIRO_COST_T               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
