*&---------------------------------------------------------------------*
*& 包含               ZAFO_MIRO_F01
*&---------------------------------------------------------------------*

FORM frm_set_screen.
  LOOP AT SCREEN.
    IF p_cre = 'X'.
      IF screen-group1 = 'CRE'.
        screen-active = 1.
      ELSEIF screen-group1 = 'MOD'.
        screen-active = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

    IF p_mod = 'X'.
      IF screen-group1 = 'MOD'.
        screen-active = 1.
      ELSEIF screen-group1 = 'CRE'.
        screen-active = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

  ENDLOOP.


ENDFORM.

FORM frm_get_miro_todo.



  SELECT * FROM zafo_miro_todo_t
    INTO CORRESPONDING FIELDS OF TABLE @gt_todo
    WHERE werks = @p_werks
    AND lifnr = @p_lifnr
    AND lfbnr IN @s_mblnr
    AND ebeln IN @s_ebeln
    AND ernam IN @s_ernam
    AND ekgrp IN @s_ekgrp
    AND bednr IN @s_satnr
    AND quantity <> 0.

  IF sy-subrc NE 0.
    MESSAGE s001 DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  SELECT lfgja,lfbnr,lfpos
    INTO TABLE @DATA(gt_miro)
    FROM
    zafo_miro_head AS h
    INNER JOIN zafo_miro_item AS i ON h~miro_no = i~miro_no
    FOR ALL ENTRIES IN @gt_todo
    WHERE lfgja = @gt_todo-lfgja
    AND lfbnr = @gt_todo-lfbnr
    AND lfpos = @gt_todo-lfpos
    AND status IN ( 'A' , 'B' ).
  SORT gt_miro BY lfgja lfbnr lfpos.
  LOOP AT gt_todo ASSIGNING FIELD-SYMBOL(<todo>).
    READ TABLE gt_miro TRANSPORTING NO FIELDS
                                        WITH KEY lfgja = <todo>-lfgja
                                                         lfbnr = <todo>-lfbnr
                                                         lfpos = <todo>-lfpos
                                                         BINARY SEARCH.
    IF <todo>-bsart = 'ZC04'.
      CLEAR <todo>-aufnr.
    ENDIF.
    IF sy-subrc EQ 0.
      <todo>-frgrl = 'L'.
    ENDIF.
    IF <todo>-frgrl = 'X'.
      <todo>-icon = icon_led_yellow.
      <todo>-text = '待审批'.
    ELSEIF <todo>-frgrl = 'L'.
      <todo>-icon = icon_locked.
      <todo>-text = '对账中'.
    ELSE.
      <todo>-icon = icon_led_green.
      <todo>-text = '已审批'.
    ENDIF.
  ENDLOOP.

ENDFORM.

FORM frm_open_miro_single.


ENDFORM.

FORM frm_get_head.
  SELECT * FROM zafo_miro_head
  INTO CORRESPONDING FIELDS OF TABLE @gt_head
  WHERE bukrs IN @auth_burks
  AND lifnr IN @s_lifnr
  AND miro_no IN @s_mirono
  AND status IN @s_status
  AND ernam IN @s_crnam
  ORDER BY miro_no DESCENDING.
  IF sy-subrc NE 0.
    MESSAGE s006 DISPLAY LIKE 'E'.
  ENDIF.

  LOOP AT gt_head ASSIGNING FIELD-SYMBOL(<f_head>).
    zafo_miro_class=>set_icon( EXPORTING status = <f_head>-status
                                                IMPORTING icon = <f_head>-icon
                                                                     text = <f_head>-text ).

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_select_all
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_select_all .
  falv_todo->get_selected_rows( IMPORTING et_index_rows = DATA(lt_rows) ).
  IF lt_rows IS INITIAL.
    LOOP AT gt_todo ASSIGNING FIELD-SYMBOL(<line>).
      <line>-selected = 'X'.
      <line>-row_color = 'C500'.
    ENDLOOP.
  ELSE.
    LOOP AT lt_rows INTO DATA(ls_rows).
      READ TABLE gt_todo INDEX ls_rows-index ASSIGNING <line>.
      IF sy-subrc EQ 0.
        <line>-selected = 'X'.
        <line>-row_color = 'C500'.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_unselect_all
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_unselect_all .
  LOOP AT gt_todo ASSIGNING FIELD-SYMBOL(<line>) WHERE selected = 'X'.
    <line>-selected = ''.
    <line>-row_color = ''.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_todo_sure
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_todo_sure .
  DATA lt_todo TYPE TABLE OF zafo_miro_po_stodo.
  CLEAR gt_head.
  DATA head TYPE zafo_miro_head.
  DATA item TYPE zafo_miro_class=>tt_item.
  LOOP AT gt_todo INTO DATA(ls_todo) WHERE selected = 'X'.
    IF ls_todo-icon <> icon_led_green.
      MESSAGE s002 DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
    APPEND VALUE #( bukrs = ls_todo-bukrs
                                    bukrs_name = ls_todo-bukrs_name
                                    lifnr = ls_todo-lifnr
                                    lifnr_name = ls_todo-lifnr_name
                                    waers = ls_todo-waers
                                    mwskz = ls_todo-mwskz
                                   ) TO gt_head.
    APPEND ls_todo TO lt_todo.
  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM gt_head .

  IF lines( gt_head ) <> 1.
    MESSAGE e003.
  ENDIF.

  head = CORRESPONDING #( gt_head[ 1 ] ).
  item = CORRESPONDING #( lt_todo
                                                MAPPING "menge = quantity_gr
                                                                  netwr = net
                                                  ).

  class = zafo_miro_class=>create( EXPORTING i_head = head  it_item = item ) .
  ASSIGN class TO <class>.
  ASSIGN <class>->head TO <head>.

  CALL SCREEN 200.
ENDFORM.
