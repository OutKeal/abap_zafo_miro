*&---------------------------------------------------------------------*
*& 包含               ZAFO_MIRO_ALV
*&---------------------------------------------------------------------*


FORM frm_todo_fcat .
  zwft_common=>fcat_from_config( EXPORTING i_repid = sy-repid i_name = 'TODO'
CHANGING ct_fcat = falv_todo->fcat ).
ENDFORM.

FORM frm_todo_layout.
  falv_todo->layout->set_no_toolbar( abap_false ).
  falv_todo->layout->set_cwidth_opt( abap_true ).
  falv_todo->layout->set_zebra( abap_true ).
  falv_todo->layout->set_info_fname( 'ROW_COLOR' ).
  falv_todo->layout->set_sel_mode( 'A' ).
ENDFORM.

FORM frm_dis_fcat .
  zwft_common=>fcat_from_config( EXPORTING i_repid = sy-repid i_name = 'DIS'
  CHANGING ct_fcat = falv_dis->fcat ).
ENDFORM.

FORM frm_dis_layout.
  falv_dis->layout->set_no_toolbar( abap_false ).
  falv_dis->layout->set_cwidth_opt( abap_true ).
  falv_dis->layout->set_zebra( abap_true ).
  falv_dis->layout->set_info_fname( 'ROW_COLOR' ).
  falv_dis->layout->set_sel_mode( 'A' ).
ENDFORM.

FORM frm_user_command_todo USING c_falv TYPE REF TO zwft_falv
      e_ucomm TYPE sy-ucomm.
  CASE e_ucomm.
    WHEN '&SELECTALL'."全选
      PERFORM frm_select_all .
    WHEN '&DESELECT'."取消全选
      PERFORM frm_unselect_all.
    WHEN '&SURE'."确认选择
      PERFORM frm_todo_sure.
  ENDCASE.
  c_falv->soft_refresh( ).
*  msg->pop_all_msg( abap_true ).
ENDFORM.

FORM frm_data_changed_finished_todo USING c_falv TYPE REF TO zwft_falv
      e_modified TYPE char01
      et_good_cells TYPE lvc_t_modi.

  LOOP AT et_good_cells INTO DATA(ls_cell).
    READ TABLE gt_todo ASSIGNING FIELD-SYMBOL(<line>) INDEX ls_cell-row_id.
    CHECK sy-subrc EQ 0.
    CASE ls_cell-fieldname.
      WHEN 'SELECTED'.
        <line>-row_color = COND #( WHEN <line>-selected = 'X' THEN 'C500' ELSE '' ).
    ENDCASE.
  ENDLOOP.
  IF sy-subrc EQ 0.
    c_falv->soft_refresh( ).
  ENDIF.
ENDFORM.

FORM frm_double_click_dis  USING c_falv TYPE REF TO zwft_falv
      e_row  TYPE lvc_s_row
      e_column TYPE lvc_s_col
      es_row_no TYPE lvc_s_roid.
  READ TABLE gt_head INDEX e_row-index ASSIGNING FIELD-SYMBOL(<f_head>).
  CHECK sy-subrc EQ 0.

  class = zafo_miro_class=>maintain( <f_head>-miro_no ).
  ASSIGN class TO <class>.
  ASSIGN <class>->head TO <head>.
  CALL SCREEN 200.
  MOVE-CORRESPONDING <head> TO <f_head>.
  zafo_miro_class=>set_icon( EXPORTING status = <f_head>-status
                                                     IMPORTING icon = <f_head>-icon
                                                                          text = <f_head>-text ).
  c_falv->soft_refresh( ).
ENDFORM.

FORM frm_hotspot_click_dis USING c_falv TYPE REF TO zwft_falv
      e_row TYPE lvc_s_row
      e_column TYPE lvc_s_col
      es_row_no TYPE lvc_s_roid .
  READ TABLE gt_head INDEX e_row-index ASSIGNING FIELD-SYMBOL(<f_head>).
  CHECK sy-subrc EQ 0.
  class = zafo_miro_class=>maintain( <f_head>-miro_no ).
  ASSIGN class TO <class>.
  ASSIGN <class>->head TO <head>.
  CALL SCREEN 200.
  MOVE-CORRESPONDING <head> TO <f_head>.
  zafo_miro_class=>set_icon( EXPORTING status = <f_head>-status
  IMPORTING icon = <f_head>-icon
    text = <f_head>-text ).
  c_falv->soft_refresh( ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_todo_display
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_falv_display_0100 .
  IF splitter_100 IS  INITIAL.
    splitter_100 = NEW cl_gui_splitter_container(
    parent = NEW cl_gui_docking_container( extension = '3000' )
    rows = 1 columns = 1 ).
    falv_todo = zwft_falv=>create( EXPORTING
      i_popup = ''
      i_repid = sy-repid
      i_handle = '1'
      i_suffix = '_TODO'
      i_parent = splitter_100->get_container( row = 1 column = 1 )
    CHANGING ct_table = gt_todo ) .
    PERFORM frm_todo_fcat.
    PERFORM frm_todo_layout.
    falv_todo->exclude_edit_function( ).
    falv_todo->register_edit_event( falv_todo->mc_evt_modified ).
    falv_todo->display( ).
  ELSE.
    falv_todo->soft_refresh( ).
  ENDIF.
ENDFORM.

FORM frm_falv_display_0110 .
  IF splitter_110 IS  INITIAL.
    splitter_110 = NEW cl_gui_splitter_container(
    parent = NEW cl_gui_docking_container( extension = '3000' )
    rows = 1 columns = 1 ).
    falv_dis = zwft_falv=>create( EXPORTING
      i_popup = ''
      i_repid = sy-repid
      i_handle = '5'
      i_suffix = '_DIS'
      i_parent = splitter_110->get_container( row = 1 column = 1 )
    CHANGING ct_table = gt_head ) .
    PERFORM frm_dis_fcat.
    PERFORM frm_dis_layout.
    falv_dis->exclude_edit_function( ).
    falv_dis->register_edit_event( falv_dis->mc_evt_modified ).
    falv_dis->display( ).
  ELSE.
    falv_dis->soft_refresh( ).
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_falv_display_0201
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_falv_display_0201 .
  IF container201 IS INITIAL.
    container201 = NEW cl_gui_custom_container( container_name = 'ITEM' ).
  ENDIF.
  IF <class>->falv_item IS  INITIAL.
    <class>->init_falv( i_name = 'ITEM' i_parent = container201 ).
    <class>->falv_item->display( ).
  ELSE.
    <class>->set_falv_readonly( <class>->falv_item ).
    <class>->falv_item->soft_refresh( ).
  ENDIF.
ENDFORM.

FORM frm_falv_display_0202 .
  IF container202 IS INITIAL.
    container202 = NEW cl_gui_custom_container( container_name = 'INV' ).
  ENDIF.
  IF <class>->falv_inv IS  INITIAL.
    <class>->init_falv( i_name = 'INV' i_parent = container202 ).
    <class>->falv_inv->display( ).
  ELSE.
    <class>->set_falv_readonly( <class>->falv_inv ).
    <class>->falv_inv->soft_refresh( ).
  ENDIF.
ENDFORM.

FORM frm_falv_display_0203 .
  IF container203 IS INITIAL.
    container203 = NEW cl_gui_custom_container( container_name = 'COST' ).
  ENDIF.
  IF <class>->falv_cost IS  INITIAL.
    <class>->init_falv( i_name = 'COST' i_parent = container203 ).
    <class>->falv_cost->display( ).
  ELSE.
    <class>->set_falv_readonly( <class>->falv_cost ).
    <class>->falv_cost->soft_refresh( ).
  ENDIF.
ENDFORM.
