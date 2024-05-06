*&---------------------------------------------------------------------*
*& Report ZAFO_MIRO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zafo_miro MESSAGE-ID zafo_miro.

INCLUDE zafo_miro_top.

INCLUDE zafo_miro_sel.

INCLUDE zafo_miro_tag.

INCLUDE zafo_miro_f01.

INCLUDE zafo_miro_pbo.

INCLUDE zafo_miro_pai.

INCLUDE zafo_miro_alv.

INCLUDE zafo_miro_callback.

INITIALIZATION.

  IF sy-tcode = 'ZMM540B'.
    p_cre = ''.
    p_mod = 'X'.
  ENDIF.
  GET PARAMETER ID 'WRK' FIELD p_werks.
  PERFORM frm_set_name.


AT SELECTION-SCREEN OUTPUT .

  PERFORM frm_set_screen.

START-OF-SELECTION.

  CASE abap_true.
    WHEN p_cre.
      DATA(i_bukrs) = zwft_single_read=>t001k( p_werks )-bukrs.
      CHECK zwft_auth_check=>bukrs( i_object = 'ZAFO_MIRO' i_actvt = '01' i_bukrs = i_bukrs ) = abap_true.
      IF p_lifnr IS INITIAL OR p_werks IS INITIAL.
        MESSAGE s021 DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.
      PERFORM frm_get_miro_todo.
      IF gt_todo IS  NOT INITIAL.
        CALL SCREEN 100.
      ENDIF.
    WHEN p_mod.
      IF sy-calld = 'X' AND lines( s_mirono ) = 1.
        class = zafo_miro_class=>maintain( s_mirono[ 1 ]-low ).
        CHECK zwft_auth_check=>bukrs( EXPORTING i_object = 'ZAFO_MIRO' i_actvt = '03' i_bukrs = class->head-bukrs
          ) = abap_true.
        ASSIGN class TO <class>.
        ASSIGN <class>->head TO <head>.
        CALL SCREEN 200.
        RETURN.
      ENDIF.
      CHECK zwft_auth_check=>bukrs_range( EXPORTING i_object = 'ZAFO_MIRO' i_actvt = '03' i_ranges_bukrs = s_bukrs[]
      IMPORTING e_ranges_bukrs = auth_burks[] ) = abap_true.
      PERFORM frm_get_head.
      CHECK gt_head IS NOT INITIAL.
      CALL SCREEN 110.
  ENDCASE.
