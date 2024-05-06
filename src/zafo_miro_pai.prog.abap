*&---------------------------------------------------------------------*
*& 包含               ZAFO_MIRO_PAI
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_command INPUT.
  IF container201 IS BOUND.
    container201->free( ).
    FREE container201.
  ENDIF.
  IF container202 IS BOUND.
    container202->free( ).
    FREE container202.
  ENDIF.
  IF container203 IS BOUND.
    container203->free( ).
    FREE container203.
  ENDIF.
  CASE sy-ucomm.
    WHEN 'BACK'.
      CLEAR sy-ucomm.
      LEAVE TO CURRENT TRANSACTION.

    WHEN 'CANCEL'.
      CLEAR sy-ucomm.
      LEAVE TO CURRENT TRANSACTION.

    WHEN 'EXIT' OR 'QUIT'.
      CLEAR sy-ucomm.
      LEAVE PROGRAM.

    WHEN OTHERS .
      CLEAR sy-ucomm.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  PERFORM frm_user_command_todo USING falv_todo sy-ucomm.
ENDMODULE.

MODULE user_command_0200 INPUT.
  <class>->user_command_main( sy-ucomm ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  MWSKZ_LIST  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

MODULE get_focus INPUT.
  GET CURSOR FIELD <class>->focus.
ENDMODULE.

MODULE mwskz_list INPUT.

  SELECT mwskz, text1 AS name
    FROM t007s
  WHERE spras = @sy-langu
    AND kalsm = 'TAXCN'
    AND MWSKZ LIKE 'J%'
  INTO TABLE @DATA(lt_t007s).

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      value_org   = 'S'
      dynprofield = '<HEAD>-MWSKZ'
      retfield    = 'MWSKZ'
    TABLES
      value_tab   = lt_t007s.
ENDMODULE.
