*&---------------------------------------------------------------------*
*& 包含               ZAFO_MIRO_TAG
*&---------------------------------------------------------------------*

*&SPWIZARD: FUNCTION CODES FOR TABSTRIP 'TAG0200'
CONSTANTS: BEGIN OF c_tag0200,
             tab1 LIKE sy-ucomm VALUE 'TAG0200_FC1',
             tab2 LIKE sy-ucomm VALUE 'TAG0200_FC2',
             tab3 LIKE sy-ucomm VALUE 'TAG0200_FC3',
           END OF c_tag0200.
*&SPWIZARD: DATA FOR TABSTRIP 'TAG0200'
CONTROLS:  tag0200 TYPE TABSTRIP.
DATA: BEGIN OF g_tag0200,
        subscreen   LIKE sy-dynnr,
        prog        LIKE sy-repid VALUE 'ZAFO_MIRO',
        pressed_tab LIKE sy-ucomm VALUE c_tag0200-tab1,
      END OF g_tag0200.
DATA:      ok_code LIKE sy-ucomm.

*&SPWIZARD: OUTPUT MODULE FOR TS 'TAG0200'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: SETS ACTIVE TAB
MODULE tag0200_active_tab_set OUTPUT.
  tag0200-activetab = g_tag0200-pressed_tab.
  CASE g_tag0200-pressed_tab.
    WHEN c_tag0200-tab1.
      g_tag0200-subscreen = '0201'.
    WHEN c_tag0200-tab2.
      g_tag0200-subscreen = '0202'.
    WHEN c_tag0200-tab3.
      g_tag0200-subscreen = '0203'.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.

*&SPWIZARD: INPUT MODULE FOR TS 'TAG0200'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GETS ACTIVE TAB
MODULE tag0200_active_tab_get INPUT.
  ok_code = sy-ucomm.
  CASE ok_code.
    WHEN c_tag0200-tab1.
      g_tag0200-pressed_tab = c_tag0200-tab1.
    WHEN c_tag0200-tab2.
      g_tag0200-pressed_tab = c_tag0200-tab2.
    WHEN c_tag0200-tab3.
      g_tag0200-pressed_tab = c_tag0200-tab3.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.
