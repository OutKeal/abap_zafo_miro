*&---------------------------------------------------------------------*
*& 包含               ZAFO_MIRO_SEL
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN: COMMENT 1(31) FOR FIELD p_werks MODIF ID cre.
    PARAMETERS: p_werks TYPE ekpo-werks  MEMORY ID wrk MODIF ID cre.
    SELECTION-SCREEN: COMMENT 45(40) wr_name MODIF ID cre.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN: COMMENT 1(31) FOR FIELD p_lifnr MODIF ID cre.
    PARAMETERS:   p_lifnr LIKE ekko-lifnr  MODIF ID cre.
    SELECTION-SCREEN: COMMENT 45(40) li_name MODIF ID cre.
  SELECTION-SCREEN END OF LINE.


  SELECT-OPTIONS:
  s_budat FOR zafo_miro_head-budat MODIF ID cre,
  s_ebeln FOR ekko-ebeln MODIF ID cre,
  s_mblnr FOR mseg-mblnr MODIF ID cre,
  s_ernam FOR ekko-ernam MODIF ID cre,
  s_ekgrp FOR ekko-ekgrp MODIF ID cre,
  s_satnr FOR ekpo-satnr MODIF ID cre,

  s_bukrs FOR zafo_miro_head-bukrs MODIF ID mod,
  s_lifnr FOR zafo_miro_head-lifnr MODIF ID mod,
  s_mirono FOR zafo_miro_head-miro_no MODIF ID mod,
  s_status FOR zafo_miro_head-status MODIF ID mod,
  s_crnam FOR zafo_miro_head-ernam MODIF ID mod.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_cre RADIOBUTTON GROUP g01 USER-COMMAND cremod DEFAULT 'X',
              p_mod RADIOBUTTON GROUP g01.
SELECTION-SCREEN END OF BLOCK b2.

AT SELECTION-SCREEN ON  p_lifnr.
  CHECK p_lifnr IS NOT INITIAL.
  CHECK p_cre EQ abap_true.
  PERFORM frm_vendor_search .

AT SELECTION-SCREEN ON  p_werks.
  PERFORM frm_set_name.


AT SELECTION-SCREEN ON  VALUE-REQUEST FOR p_lifnr.
  CHECK p_cre EQ abap_true.
  PERFORM frm_vendor_search .

AT SELECTION-SCREEN ON  s_lifnr.
  CHECK p_mod EQ abap_true.
  PERFORM frm_vendor_s_search USING ''.

AT SELECTION-SCREEN ON  VALUE-REQUEST FOR s_lifnr-low.
  CHECK p_mod EQ abap_true.
  PERFORM frm_vendor_s_search USING 'F4'.

FORM frm_vendor_s_search  USING    act.
  DATA l_werks TYPE werks_d.
  IF act = 'F4'.
    CLEAR s_lifnr[].
    INSERT INITIAL LINE INTO s_lifnr INDEX 1 ASSIGNING FIELD-SYMBOL(<line>).
    <line>-sign = 'I'.
    <line>-option = 'EQ'.
    <line>-low = '%'.
  ELSE.
    READ TABLE s_lifnr ASSIGNING <line> INDEX 1.
    CHECK sy-subrc EQ 0.
    FIND '*' IN <line>-low.
    CHECK sy-subrc NE 0.
  ENDIF.
  READ TABLE s_bukrs INDEX 1.
  IF sy-subrc EQ 0 AND s_bukrs-low IS NOT INITIAL.
    l_werks = 'W' && s_bukrs-low+0(3).
  ELSE.
    GET PARAMETER ID 'WRK' FIELD l_werks.
  ENDIF.
  CHECK l_werks IS NOT INITIAL.
  zwft_common=>search_vendor( EXPORTING werks = l_werks CHANGING lifnr = <line>-low ).
  PERFORM frm_set_dynp_value USING 'S_LIFNR-LOW' <line>-low.
  DELETE s_lifnr WHERE low = '' AND high = ''.
ENDFORM.

FORM frm_set_dynp_value USING i_field
      i_value .
  DATA field TYPE dynpread-fieldname.
  DATA value TYPE dynpread-fieldvalue.
  field = i_field.
  value = |{ i_value ALPHA = OUT }|.
  CALL FUNCTION 'SET_DYNP_VALUE'
    EXPORTING
      i_field = field
      i_repid = sy-repid
      i_dynnr = sy-dynnr
      i_value = value.
ENDFORM.

FORM frm_vendor_search.
  CHECK sy-ucomm EQ ''.
  CALL FUNCTION 'GET_DYNP_VALUE'
    EXPORTING
      i_field = 'P_WERKS'
      i_repid = sy-repid
      i_dynnr = sy-dynnr
    CHANGING
      o_value = p_werks.
  p_lifnr = COND #( WHEN p_lifnr IS INITIAL THEN '%' ELSE p_lifnr ).
  zwft_common=>search_vendor( EXPORTING werks = p_werks CHANGING lifnr = p_lifnr name = li_name ).
  PERFORM frm_set_dynp_value USING 'LI_NAME' li_name.

ENDFORM.

FORM frm_set_name.
  IF p_werks IS NOT INITIAL.
    wr_name = zwft_single_read=>t001w( p_werks )-name1.
  ENDIF.

ENDFORM.
