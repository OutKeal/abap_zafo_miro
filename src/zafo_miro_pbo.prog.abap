*&---------------------------------------------------------------------*
*& 包含               ZAFO_MIRO_PBO
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS '100' .
  DATA(name1) = zwft_single_read=>lfa1( p_lifnr )-name1.
  SET TITLEBAR '100' WITH name1.
ENDMODULE.

MODULE status_0110 OUTPUT.
  SET PF-STATUS '110' .
  SET TITLEBAR '110'.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module CREATE_OBJECT_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE create_object_0100 OUTPUT.
  PERFORM frm_falv_display_0100.
ENDMODULE.

MODULE create_object_0110 OUTPUT.
  PERFORM frm_falv_display_0110.
ENDMODULE.


*&---------------------------------------------------------------------*
*& Module STATUS_0200 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  DATA(fcode) = <class>->set_gui_status_exclude( ).
  SET PF-STATUS '200' EXCLUDING fcode.
  SET TITLEBAR '200' WITH <head>-bukrs_name.
ENDMODULE.

MODULE create_gos_service OUTPUT.
  DATA:obj TYPE borident.
  CHECK <class>->gos_manager IS INITIAL.
  CHECK <class>->head-miro_no IS NOT INITIAL .
  obj-objtype = 'ZAFO_MIRO'.
  obj-objkey = <class>->head-miro_no .
  CREATE OBJECT <class>->gos_manager
    EXPORTING
      is_object = obj
    EXCEPTIONS
      OTHERS    = 1.
  cl_gui_cfw=>flush( ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Module CREATE_OBJECT_0201 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE create_object_0201 OUTPUT.
  PERFORM frm_falv_display_0201.
ENDMODULE.

MODULE create_object_0202 OUTPUT.
  PERFORM frm_falv_display_0202.
ENDMODULE.

MODULE create_object_0203 OUTPUT.
  PERFORM frm_falv_display_0203.
ENDMODULE.

MODULE set_screen_mod_300 OUTPUT.
  CHECK <class>->readonly = 'X'.
  LOOP AT SCREEN.
    screen-input = 0.
    MODIFY SCREEN.
  ENDLOOP.
ENDMODULE.
