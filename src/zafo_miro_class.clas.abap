class ZAFO_MIRO_CLASS definition
  public
  final
  create public .

public section.

  types:
    tt_screen TYPE TABLE OF zwft_screen .
  types:
    BEGIN OF ty_split_cost,
        cost_key_type TYPE zafo_miro_cost_key_type,
        cost_key      TYPE zafo_miro_cost_key,
*        cost_base     TYPE zafo_miro_cost,
        cost_sum      TYPE zafo_miro_ecost,
*        cost_leave    TYPE zafo_miro_ecost,
      END OF ty_split_cost .
  types:
    tt_split_cost TYPE TABLE OF ty_split_cost .
  types:
    BEGIN OF ty_cost_key,
        key   TYPE char10,
        value TYPE char20,
        name  TYPE char40,
      END OF ty_cost_key .
  types:
    tt_cost_key TYPE TABLE OF ty_cost_key .
  types:
    tt_item TYPE TABLE OF zafo_miro_item .
  types:
    tt_cost TYPE TABLE OF zafo_miro_cost .
  types:
    tt_inv  TYPE TABLE OF zafo_miro_inv .

  data HEAD type ZAFO_MIRO_HEAD .
  data ITEM type TT_ITEM .
  data COST type TT_COST .
  data INV type TT_INV .
  data READONLY type ABAP_BOOL .
  data FALV_ITEM type ref to ZWFT_FALV .
  data FALV_COST type ref to ZWFT_FALV .
  data FALV_INV type ref to ZWFT_FALV .
  data COST_KEY type TT_COST_KEY .
  data MSG type ref to ZWFT_MESSAGE .
  data SCOST type TT_SPLIT_COST .
  data FALV_SCREEN type TT_SCREEN .
  constants TAX_DIFF type ZAFO_MIRO_TAX value '0.1' ##NO_TEXT.
  data FOCUS type FIELDNAME .
  data GOS_MANAGER type ref to CL_GOS_MANAGER .
  data:
    tt_cost_type TYPE TABLE OF zafo_miro_cost_t .
  data SAKNR type SAKNR value '1405009900' ##NO_TEXT.

  methods CONSTRUCTOR .
  class-methods CREATE
    importing
      !I_HEAD type ZAFO_MIRO_HEAD
      !IT_ITEM type TT_ITEM
    returning
      value(R_CLASS) type ref to ZAFO_MIRO_CLASS .
  class-methods MAINTAIN
    importing
      !MIRO_NO type ZAFO_MIRO_NO
    returning
      value(R_CLASS) type ref to ZAFO_MIRO_CLASS .
  class-methods SET_ICON
    importing
      !STATUS type ZAFO_MIRO_STATUS
    exporting
      !ICON type ICON_D
      !TEXT type TEXT .
  methods LOCK .
  methods GET_TAX .
  methods INIT_HEAD .
  methods CALCULATE_ITEMS .
  methods CALCULATE_HEAD .
  methods CALCULATE_ITEM
    changing
      !ITEM type ZAFO_MIRO_ITEM .
  methods INIT_FALV
    importing
      !I_NAME type CLIKE
      !I_PARENT type ref to CL_GUI_CONTAINER .
  methods SET_FALV_READONLY
    importing
      !I_FALV type ref to ZWFT_FALV .
  methods TOOLBAR
    importing
      !I_OBJECT type ref to CL_ALV_EVENT_TOOLBAR_SET
      !C_FALV type ref to ZWFT_FALV .
  methods USER_COMMAND
    importing
      !C_FALV type ref to ZWFT_FALV
      !I_UCOMM type SY-UCOMM .
  methods USER_COMMAND_MAIN
    importing
      !I_UCOMM type SY-UCOMM .
  methods USER_COMMAND_ITEM
    importing
      !C_FALV type ref to ZWFT_FALV
      !I_UCOMM type SY-UCOMM .
  methods USER_COMMAND_COST
    importing
      !C_FALV type ref to ZWFT_FALV
      !I_UCOMM type SY-UCOMM .
  methods USER_COMMAND_INV
    importing
      !C_FALV type ref to ZWFT_FALV
      !I_UCOMM type SY-UCOMM .
  methods DATA_CHANGED
    importing
      !C_FALV type ref to ZWFT_FALV
      !CL_DATA_CHANGED type ref to CL_ALV_CHANGED_DATA_PROTOCOL .
  methods DATA_CHANGED_FINISHED
    importing
      !C_FALV type ref to ZWFT_FALV
      !IT_GOOD_CELLS type LVC_T_MODI .
  methods HOTSPOT_CLICK
    importing
      !C_FALV type ref to ZWFT_FALV
      !I_ROW type LVC_S_ROW
      !I_COLUMN type LVC_S_COL .
  methods DOUBLE_CLICK
    importing
      !C_FALV type ref to ZWFT_FALV
      !I_ROW type LVC_S_ROW
      !I_COLUMN type LVC_S_COL .
  methods REGISTER_F4
    importing
      !C_FALV type ref to ZWFT_FALV .
  methods ONF4
    importing
      !C_FALV type ref to ZWFT_FALV
      !C_EVENT_DATA type ref to CL_ALV_EVENT_DATA
      !I_FIELDNAME type FIELDNAME
      !I_INDEX type INDEX
      !I_DISPLAY type ABAP_BOOL optional .
  methods SET_COST_DROPDOWN .
  methods COST_KEY_F4
    changing
      !C_COST type ZAFO_MIRO_COST .
  methods SET_GUI_STATUS_EXCLUDE
    returning
      value(FCODES) type TT_FCODES .
  methods SAVE .
  methods CHECK
    importing
      !MSGTY type MSGTY default 'E' .
  methods SET_STATUS
    importing
      !STATUS type ZAFO_MIRO_STATUS .
  methods SET_READONLY .
  methods COMMIT .
  methods UNCOMMIT .
  methods POST .
  methods CANCEL .
  methods CALCULATE .
  methods SPLIT_COST .
  methods CLICK_FOCUS .
  methods ADD_GOS_RELATIONSHIP
    importing
      !OBJKEY type SWO_TYPEID
      !OBJTYPE type SWO_OBJTYP default 'BUS2081' .
  methods GOS_CALL .
  methods PRINT_HTML .
  class-methods AUTH_CHECK
    importing
      !ACTVT type ACTIV_AUTH
      !BUKRS type BUKRS
    returning
      value(BOOL) type ABAP_BOOL .
protected section.
PRIVATE SECTION.

  TYPES:
    BEGIN OF ty_split_aufnr ,
      aufnr TYPE aufnr,
      werks TYPE werks_d,
      cost  TYPE zafo_cost_amount,
    END OF ty_split_aufnr .
  TYPES:
    BEGIN OF ty_split_mat ,
      matnr TYPE matnr,
      werks TYPE werks_d,
      meins TYPE meins,
      menge TYPE menge_d,
      cost  TYPE zafo_cost_amount,
    END OF ty_split_mat .

  METHODS check_falv_changed_data .
  METHODS save_data .
  METHODS db_save .
  METHODS split_cost_to_mat
    CHANGING
      !mat TYPE bapi_incinv_create_material_t .
ENDCLASS.



CLASS ZAFO_MIRO_CLASS IMPLEMENTATION.


  METHOD add_gos_relationship.
    DATA: borident1 TYPE borident.
    DATA: borident2 TYPE borident.
    CHECK head-miro_no IS NOT INITIAL.
    CHECK objkey IS NOT INITIAL.
    CHECK objtype IS NOT INITIAL.

    IF gos_manager IS BOUND.
      FREE gos_manager.
    ENDIF.

    IF head-miro_no IS NOT INITIAL.

      borident1-objkey = objkey.
      borident1-objtype = objtype.
      borident2-objkey = head-miro_no.
      borident2-objtype = 'ZAFO_MIRO'.

      CALL FUNCTION 'BINARY_RELATION_CREATE'
        EXPORTING
          obj_rolea      = borident1
          obj_roleb      = borident2
          relationtype   = 'VORL'
        EXCEPTIONS
          no_model       = 1
          internal_error = 2
          unknown        = 3
          OTHERS         = 4.
      IF sy-subrc <> 0.
      ENDIF.
    ENDIF.



  ENDMETHOD.


  METHOD auth_check.
    bool = abap_false.
    AUTHORITY-CHECK OBJECT 'ZAFO_MIRO'
    ID 'BUKRS' FIELD bukrs
    ID 'ACTVT' FIELD actvt.
    IF sy-subrc NE 0 .
      MESSAGE s020 DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
    bool = abap_true.
  ENDMETHOD.


  METHOD calculate.
    get_tax( ).

    split_cost( ).

    calculate_items( ).

    calculate_head( ).
  ENDMETHOD.


  METHOD calculate_head.
    CLEAR:head-menge,
                head-amount,
                head-cost,
                head-reamount,
                head-net,
                head-tax,
                head-amount_in,
                head-tax_in.
    LOOP AT item INTO DATA(l_item).
      head-menge += l_item-quantity.
      head-amount += l_item-amount.
      head-cost += l_item-cost.
      head-reamount += l_item-reamount.
      head-net += l_item-netwr.
      head-tax += l_item-tax.
    ENDLOOP.

    LOOP AT inv INTO DATA(l_inv).
      head-amount_in += l_inv-amount_in.
      head-tax_in += l_inv-tax_in.
    ENDLOOP.


*    LOOP AT cost INTO DATA(l_cost).
*      head-cost += l_cost-cost.
*    ENDLOOP.

  ENDMETHOD.


  METHOD calculate_item.

    IF item-quantity = item-quantity_gr - item-quantity_ir.
      item-amount = item-amount_gr - item-amount_ir.
      item-net = item-net_gr - item-net_ir.
    ELSE.
      item-amount = item-quantity * item-brtwr / item-menge.
      item-net = item-quantity * item-net_gr / item-menge.
    ENDIF.

    item-reamount = item-amount + item-cost.
    item-tax = item-reamount * head-tax_rate / ( 100 + head-tax_rate ).
    item-netwr = item-reamount  - item-tax.
  ENDMETHOD.


  METHOD CALCULATE_ITEMS.
    LOOP AT item ASSIGNING FIELD-SYMBOL(<item>).
      <item>-miro_nr = sy-tabix.
      calculate_item( CHANGING item = <item> ).
    ENDLOOP.

  ENDMETHOD.


  METHOD cancel.
    DATA iv_reason_rev TYPE bapi_incinv_fld-reason_rev          .
    DATA iv_pstng_date TYPE bapi_incinv_fld-pstng_date          .
    DATA ev_inv_doc_no TYPE bapi_incinv_fld-inv_doc_no          .
    DATA ev_fisc_year TYPE bapi_incinv_fld-fisc_year          .
    DATA lt_return TYPE TABLE OF bapiret2.

    CHECK zwft_common=>confirm_date( EXPORTING iv_datetext = TEXT-008
                                                                                    iv_text = TEXT-007
                                                                                    iv_default = head-budat
                                                                                  IMPORTING
                                                                                    ev_date = iv_pstng_date
                                                                                    ) = abap_true.
    IF iv_pstng_date < head-budat.
      MESSAGE s016 DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    iv_reason_rev = COND #( WHEN iv_pstng_date = head-budat THEN '03' ELSE '04' ).

    CALL FUNCTION 'BAPI_INCOMINGINVOICE_CANCEL'
      EXPORTING
        invoicedocnumber          = head-belnr
        fiscalyear                = head-gjahr
        reasonreversal            = iv_reason_rev
        postingdate               = iv_pstng_date
      IMPORTING
        invoicedocnumber_reversal = ev_inv_doc_no
        fiscalyear_reversal       = ev_fisc_year
      TABLES
        return                    = lt_return.

    LOOP AT lt_return INTO DATA(ls_return) WHERE type CA 'EAX'.
      msg->add_line( ls_return ).
    ENDLOOP.
    IF sy-subrc EQ 0.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ELSE.
      msg->add_single( msgty = 'S' msgid = 'ZAFO_MIRO' msgno = '017' msgv1 = head-belnr ).
      head-gjahr = ''.
      head-belnr = ''.
      set_status( 'B' ).
      add_gos_relationship( objkey = ev_inv_doc_no && ev_fisc_year ).
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
    ENDIF.
    msg->pop_msg( 'X' ).
  ENDMETHOD.


  METHOD check.
    LOOP AT item INTO DATA(l_item).
      IF ( l_item-reamount > 0 AND l_item-amount < 0 )
        OR ( l_item-reamount < 0 AND l_item-amount > 0 ).
        msg->add_single( msgty = msgty msgid = 'ZAFO_MIRO' msgno = '007' msgv1 = l_item-miro_nr  ).
      ENDIF.
    ENDLOOP.

    DATA(cost_screen) = falv_screen.
    DELETE cost_screen WHERE name <> 'COST'.

    LOOP AT cost INTO DATA(l_cost).
      LOOP AT cost_screen INTO DATA(l_screen) WHERE requi IS NOT INITIAL.
        ASSIGN COMPONENT l_screen-fieldname OF STRUCTURE l_cost TO FIELD-SYMBOL(<cost_value>).
        CHECK sy-subrc EQ 0.
        IF <cost_value> IS INITIAL.
          msg->add_single( msgty = msgty
                              msgid = 'ZAFO_MIRO'
                              msgno = '008'
                              msgv1 = TEXT-004 "费用清单
                              msgv2 = l_cost-cost_nr "费用类型
                              msgv3 = l_screen-coltext "
                              ).
        ENDIF.
      ENDLOOP.

      IF l_cost-cost_key_type <> '对账单' AND l_cost-cost_key IS INITIAL.
        msg->add_single( msgty = msgty
        msgid = 'ZAFO_MIRO'
        msgno = '008'
        msgv1 = TEXT-004 "费用清单
        msgv2 = l_cost-cost_nr "费用类型
        msgv3 = '分摊关键字' "
        ).
      ENDIF.

    ENDLOOP.


    DATA(inv_screen) = falv_screen.
    DELETE inv_screen WHERE name <> 'INV'.

    LOOP AT inv INTO DATA(l_inv).
      LOOP AT inv_screen INTO l_screen WHERE requi IS NOT INITIAL.
        ASSIGN COMPONENT l_screen-fieldname OF STRUCTURE l_inv TO FIELD-SYMBOL(<inv_value>).
        CHECK sy-subrc EQ 0.
        IF <inv_value> IS INITIAL.
          msg->add_single( msgty = msgty
          msgid = 'ZAFO_MIRO'
          msgno = '008'
          msgv1 = TEXT-005 "费用清单
          msgv2 = l_inv-inv_nr "费用类型
          msgv3 = l_screen-coltext
          ).
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    IF head-reamount <> head-amount_in.
      msg->add_single( msgty = msgty
      msgid = 'ZAFO_MIRO'
      msgno = '009'
      ).
    ENDIF.

    IF head-tax - head-tax_in > tax_diff
      OR head-tax_in - head-tax > tax_diff.
      msg->add_single( msgty = msgty
      msgid = 'ZAFO_MIRO'
      msgno = '009'
      msgv1 = |{ tax_diff }|
      ).
    ENDIF.
  ENDMETHOD.


  METHOD check_falv_changed_data.
    IF falv_item IS BOUND.
      falv_item->check_changed_data( ).
    ENDIF.

    IF falv_inv IS BOUND.
      falv_inv->check_changed_data( ).
    ENDIF.

    IF falv_cost IS BOUND.
      falv_cost->check_changed_data( ).
    ENDIF.
  ENDMETHOD.


  METHOD click_focus.
    CASE focus.
      WHEN '<HEAD>-BELNR'.
        CHECK head-belnr IS NOT INITIAL.
        SET PARAMETER ID 'RBN' FIELD head-belnr.
        SET PARAMETER ID 'GJR' FIELD head-gjahr.
        CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.
      WHEN '<HEAD>-MIRO_NO'.
        gos_call( ).
    ENDCASE.
  ENDMETHOD.


  METHOD commit.
    check( 'E' ).
    IF msg->get_error( ) = abap_true.
      msg->pop_msg( 'X' ).
      RETURN.
    ENDIF.
    set_status( 'B' ).
    msg->add_single( msgty = 'S' msgid = 'ZAFO_MIRO' msgno = '012' )."保存成功
    msg->pop_msg( 'X' ).
  ENDMETHOD.


  METHOD constructor.
    msg = NEW zwft_message( ).

    SELECT * FROM zwft_screen
      WHERE repid = 'ZAFO_MIRO'
      INTO TABLE @falv_screen.

    SELECT * FROM zafo_miro_cost_t
      INTO TABLE tt_cost_type.
  ENDMETHOD.


  METHOD cost_key_f4.
    DATA return_tab TYPE TABLE OF ddshretval.
    DATA lt_cost_key TYPE tt_cost_key.
    DATA lt_f4 TYPE TABLE OF value.
    DATA field_tab TYPE TABLE OF dfies.
    DATA l_reset TYPE char1.
    IF cost_key IS INITIAL.
      LOOP AT item INTO DATA(l_item).
        APPEND VALUE #( key = '物料编号'
                                        value = l_item-matnr
                                        name = l_item-txz01 ) TO cost_key.

        APPEND VALUE #( key = '采购订单'
                                        value = l_item-ebeln ) TO cost_key.

        IF l_item-bednr IS NOT INITIAL.
          APPEND VALUE #( key = '款号'
                                  value = l_item-bednr ) TO cost_key.
        ENDIF.
      ENDLOOP.
      SORT cost_key.
      DELETE ADJACENT DUPLICATES FROM cost_key.
    ENDIF.

    field_tab = zwft_common=>get_fields_dfies_by_data( cost_key ).
    LOOP AT field_tab ASSIGNING FIELD-SYMBOL(<field_tab>).
      CASE <field_tab>-fieldname.
        WHEN 'KEY'.
          <field_tab>-reptext = '类型'.
          <field_tab>-offset = 0.
        WHEN 'VALUE'.
          <field_tab>-reptext =  c_cost-cost_key_type.
          <field_tab>-offset = 10.
        WHEN 'NAME'.
          <field_tab>-reptext = '名称'.
          <field_tab>-offset = 40.
      ENDCASE.
      <field_tab>-scrtext_s = <field_tab>-reptext .
      <field_tab>-scrtext_m = <field_tab>-reptext .
      <field_tab>-scrtext_l = <field_tab>-reptext .
      <field_tab>-fieldtext = <field_tab>-reptext .
    ENDLOOP.

    lt_cost_key = VALUE #( FOR wa IN cost_key WHERE ( key = c_cost-cost_key_type ) ( wa ) ).
    CLEAR c_cost-cost_key.
    CHECK lt_cost_key IS NOT INITIAL.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'VALUE'
        value_org       = 'S'
      IMPORTING
        user_reset      = l_reset
      TABLES
        field_tab       = field_tab
        value_tab       = lt_cost_key
        return_tab      = return_tab
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    CHECK l_reset IS INITIAL.
    READ TABLE return_tab INTO DATA(l_return) INDEX 1.
    IF sy-subrc EQ 0.
      c_cost-cost_key = l_return-fieldval.
      CONDENSE c_cost-cost_key NO-GAPS.
    ENDIF.
  ENDMETHOD.


  METHOD create.
    r_class = NEW zafo_miro_class( ).
    r_class->head = i_head.
    r_class->item = it_item.
    r_class->init_head( ).
    r_class->calculate_items( ).
    r_class->calculate_head( ).
  ENDMETHOD.


  METHOD data_changed.

    IF c_falv->title_v5 = 'ITEM'.
      LOOP AT cl_data_changed->mt_good_cells INTO DATA(ls_modi).
        READ TABLE item ASSIGNING FIELD-SYMBOL(<item>) INDEX ls_modi-row_id.
        IF sy-subrc EQ 0.
          CASE ls_modi-fieldname.
            WHEN 'QUANTITY'.
              DATA(l_quantity) = <item>-quantity.
              l_quantity = ls_modi-value.
              IF ( l_quantity > 0 AND <item>-quantity_gr < 0 )
                OR ( l_quantity < 0 AND <item>-quantity_gr > 0 ).
                cl_data_changed->add_protocol_entry( i_msgid     = 'ZAFO_MIRO' i_msgty     = 'E'  i_msgno     = '018'
                                                                               i_fieldname = ls_modi-fieldname  i_row_id    = ls_modi-row_id ).
              ELSEIF abs( l_quantity ) > abs( <item>-quantity_gr ).
                cl_data_changed->add_protocol_entry( i_msgid     = 'ZAFO_MIRO' i_msgty     = 'E'  i_msgno     = '019'
                                                                               i_fieldname = ls_modi-fieldname  i_row_id    = ls_modi-row_id ).
              ENDIF.
          ENDCASE.
        ENDIF.
      ENDLOOP.

      IF cl_data_changed->mt_protocol IS NOT INITIAL.
        cl_data_changed->display_protocol( ).
      ENDIF.

    ENDIF.
  ENDMETHOD.


  METHOD data_changed_finished.
    CASE c_falv->title_v5.
      WHEN 'ITEM'.

        LOOP AT it_good_cells INTO DATA(is_cell).
          READ TABLE item INDEX is_cell-row_id ASSIGNING FIELD-SYMBOL(<item>).
          CHECK sy-subrc EQ 0.
          calculate_item( CHANGING item = <item> ).
        ENDLOOP.


      WHEN 'COST'.
        LOOP AT it_good_cells INTO is_cell.
          READ TABLE cost INDEX is_cell-row_id ASSIGNING FIELD-SYMBOL(<cost>).
          CHECK sy-subrc EQ 0.
          <cost>-icon = icon_led_yellow.
          CASE is_cell-fieldname.
            WHEN 'COST_KEY_TYPE'.
              CLEAR <cost>-cost_key.
            WHEN 'REMARK'.
              <cost>-remark_string = <cost>-remark.
          ENDCASE.
        ENDLOOP.
      WHEN 'INV'.
        LOOP AT it_good_cells INTO is_cell.
          READ TABLE inv INDEX is_cell-row_id ASSIGNING FIELD-SYMBOL(<inv>).
          CHECK sy-subrc EQ 0.
          <inv>-icon = icon_led_yellow.
          CASE is_cell-fieldname.
            WHEN 'AMOUNT_IN' .
              IF <inv>-tax_in IS INITIAL.
                <inv>-tax_in = <inv>-amount_in / ( 100 + head-tax_rate ) * head-tax_rate.
              ENDIF.
              <inv>-net_in = <inv>-amount_in - <inv>-tax_in.
            WHEN 'TAX_IN'.
              <inv>-net_in = <inv>-amount_in - <inv>-tax_in.
          ENDCASE.
        ENDLOOP.
    ENDCASE.
    IF sy-subrc EQ 0.
      c_falv->soft_refresh( ).
      calculate( ).
      cl_gui_cfw=>set_new_ok_code( '&ENTR' ).
    ENDIF.
  ENDMETHOD.


  METHOD db_save.
    DELETE FROM zafo_miro_item WHERE miro_no = head-miro_no.
    DELETE FROM zafo_miro_cost WHERE miro_no = head-miro_no.
    DELETE FROM zafo_miro_inv WHERE miro_no = head-miro_no.

    MODIFY zafo_miro_head FROM head.
    MODIFY zafo_miro_item FROM TABLE item.
    MODIFY zafo_miro_cost FROM TABLE cost.
    MODIFY zafo_miro_inv FROM TABLE inv.
    IF sy-subrc EQ 0.
      msg->add_single( msgty = 'S' msgid = 'ZAFO_MIRO' msgno = '011' )."保存成功
    ENDIF.
    COMMIT WORK AND WAIT.
  ENDMETHOD.


  METHOD double_click.
    CASE c_falv->title_v5.
      WHEN 'INV'.
        READ TABLE inv ASSIGNING FIELD-SYMBOL(<inv>) INDEX i_row-index.
        CHECK sy-subrc EQ 0.
        CHECK <inv>-image_xstring IS NOT INITIAL.
        IF <inv>-image_icon = icon_pdf.
          zwft_html=>show_pdf( <inv>-image_xstring ).
        ELSE.
          zwft_html=>show_picture( <inv>-image_xstring ).
        ENDIF.
      WHEN 'COST'.
        CASE i_column-fieldname.
          WHEN 'REMARK'.
            READ TABLE cost ASSIGNING FIELD-SYMBOL(<cost>) INDEX i_row-index.
            CHECK sy-subrc EQ 0.
            IF <cost>-remark_string IS INITIAL.
              <cost>-remark_string = <cost>-remark.
            ENDIF.
            DATA t_text TYPE  hrpad_t_texteditor .
            SPLIT <cost>-remark_string AT cl_abap_char_utilities=>cr_lf INTO TABLE t_text.
            DATA(change_made) = zwft_common=>text_display( CHANGING text = t_text ).
            CHECK change_made IS NOT INITIAL.
            IF readonly = abap_true.
              MESSAGE TEXT-010 TYPE 'I'.
              RETURN.
            ENDIF.
            CONCATENATE LINES OF t_text INTO <cost>-remark_string SEPARATED BY cl_abap_char_utilities=>cr_lf.
            <cost>-remark = <cost>-remark_string.
            c_falv->soft_refresh( ).

        ENDCASE.
    ENDCASE.

  ENDMETHOD.


  METHOD get_tax.
    DATA:lt_ftaxp TYPE  TABLE OF ftaxp .
    CALL FUNCTION 'GET_TAX_PERCENTAGE'
      EXPORTING
        aland   = 'CN'
        datab   = sy-datum
        mwskz   = head-mwskz
        txjcd   = ''
*       EXPORT  = ' '
      TABLES
        t_ftaxp = lt_ftaxp.
    READ TABLE lt_ftaxp INTO DATA(ls_ftaxp) WITH KEY kschl = 'MWVS' .
    IF sy-subrc EQ 0.
      head-tax_rate = ls_ftaxp-kbetr / 10.
    ELSE.
      head-tax_rate = 0.
    ENDIF.
  ENDMETHOD.


  METHOD gos_call.
    CHECK head-miro_no IS NOT INITIAL.
    CHECK gos_manager IS NOT INITIAL.
    DATA:l_sgs_srvnam TYPE sgs_srvnam.
    DATA:l_object TYPE borident.
    l_sgs_srvnam = 'SRELATIONS'.
    l_object-objkey = head-miro_no.
    l_object-objtype = 'ZAFO_MIRO'.
    CALL METHOD gos_manager->start_service_direct
      EXPORTING
        ip_service       = l_sgs_srvnam
        is_object        = l_object
      EXCEPTIONS
        no_object        = 1
        object_invalid   = 2
        execution_failed = 3
        OTHERS           = 4.
  ENDMETHOD.


  method HOTSPOT_CLICK.
  endmethod.


  METHOD init_falv.
    FIELD-SYMBOLS <falv> TYPE REF TO zwft_falv.
    CASE i_name.
      WHEN 'ITEM'.
        falv_item = zwft_falv=>create( EXPORTING
          i_popup = ''
          i_repid = sy-cprog
          i_handle = '2'
          i_parent = i_parent
        CHANGING ct_table = item ) .
        ASSIGN falv_item TO <falv>.
      WHEN 'INV'.
        falv_inv = zwft_falv=>create( EXPORTING
          i_popup = ''
          i_repid = sy-cprog
          i_handle = '3'
          i_parent = i_parent
        CHANGING ct_table = inv ) .
        ASSIGN falv_inv TO <falv>.
      WHEN 'COST'.
        falv_cost = zwft_falv=>create( EXPORTING
          i_popup = ''
          i_repid = sy-cprog
          i_handle = '4'
          i_parent = i_parent
        CHANGING ct_table = cost ) .
        ASSIGN falv_cost TO <falv>.
    ENDCASE.
    <falv>->title_v5 = i_name.
    zwft_common=>fcat_from_config( EXPORTING i_repid = sy-cprog i_name = |{ <falv>->title_v5 }|
                                                           CHANGING ct_fcat = <falv>->fcat ).
    <falv>->exclude_edit_function( ).
    <falv>->register_edit_event( <falv>->mc_evt_modified ).
    register_f4( <falv> ).
    set_falv_readonly( <falv> ).
    IF <falv>->title_v5 = 'COST'.
      set_cost_dropdown( ).
    ENDIF.
  ENDMETHOD.


  METHOD init_head.
    head-status = 'A'.
    head-bldat = sy-datum.
    head-budat = sy-datum.
    get_tax( ).
  ENDMETHOD.


  METHOD lock.
    CHECK head-miro_no IS NOT INITIAL.
    CALL FUNCTION 'ENQUEUE_EZAFO_HEAD'
      EXPORTING
        mode_zafo_head = 'E'
        mandt          = sy-mandt
        afono          = me->head-miro_no
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid
      TYPE 'I'
      NUMBER sy-msgno
      WITH sy-msgv1
      sy-msgv2
      sy-msgv3
      sy-msgv4 .
      head-status = 'L'.
    ENDIF.
  ENDMETHOD.


  METHOD maintain.
    r_class = NEW zafo_miro_class(  ).

    SELECT SINGLE * FROM zafo_miro_head
        INTO r_class->head
        WHERE miro_no = miro_no.

    SELECT * FROM zafo_miro_item
      INTO CORRESPONDING FIELDS OF TABLE r_class->item
      WHERE miro_no = miro_no
      ORDER BY miro_nr.

    SELECT * FROM zafo_miro_cost
    INTO CORRESPONDING FIELDS OF TABLE r_class->cost
    WHERE miro_no = miro_no
    ORDER BY cost_nr.

    SELECT * FROM zafo_miro_inv
    INTO CORRESPONDING FIELDS OF TABLE r_class->inv
    WHERE miro_no = miro_no
    ORDER BY inv_nr.

    r_class->set_readonly( ).
    r_class->lock( ).

  ENDMETHOD.


  METHOD onf4.
    CASE c_falv->title_v5.
      WHEN 'COST'.
        READ TABLE cost INDEX i_index ASSIGNING FIELD-SYMBOL(<line>).
        CASE i_fieldname.
          WHEN 'COST_KEY' .
            cost_key_f4( CHANGING c_cost = <line> ).
            IF <line>-cost_key IS NOT INITIAL.
              c_event_data->m_event_handled = abap_true.
              calculate( ).
            ENDIF.
        ENDCASE.
    ENDCASE.

    c_falv->soft_refresh( ).
  ENDMETHOD.


  METHOD post.
    DATA: ls_head     TYPE bapi_incinv_create_header,
          lt_itemdata TYPE TABLE OF bapi_incinv_create_item,
          lt_mat      TYPE TABLE OF bapi_incinv_create_material,
          lt_tax      TYPE TABLE OF bapi_incinv_create_tax,
          lv_invoice  TYPE          bapi_incinv_fld-inv_doc_no,
          lt_accdata  TYPE TABLE OF bapi_incinv_create_account,
          lv_year     TYPE          bapi_incinv_fld-fisc_year.
    DATA l_rblgp TYPE rblgp.

    DATA l_cost TYPE zafo_miro_amount.
    DATA l_menge TYPE menge_d.
    DATA lt_return TYPE bapiret2_t.

    head-vorgang = COND #( WHEN head-reamount < 0 THEN 2 ELSE 1 ).

    CHECK zwft_common=>confirm_date( EXPORTING iv_datetext = TEXT-006
                                                                                       iv_text = TEXT-007
                                                                                       iv_default = sy-datum
                                                                  IMPORTING
                                                                                       ev_date = head-budat
                                                                                  ) = abap_true.

    IF head-vorgang EQ '1'.
      ls_head-invoice_ind = 'X'.
    ELSE.
      ls_head-invoice_ind = ''.
    ENDIF.

    ls_head-doc_type   = 'RE'.
    ls_head-doc_date   = head-bldat.
    ls_head-pstng_date = head-budat.
    ls_head-bline_date = head-budat.
    ls_head-ref_doc_no = head-miro_no.
    ls_head-comp_code  = head-bukrs.
    ls_head-header_txt = head-remark.
    ls_head-item_text = head-remark.
    ls_head-alloc_nmbr = head-miro_no.
    ls_head-currency       = head-waers.
    ls_head-gross_amount   = abs( head-reamount ).
    ls_head-del_costs_taxc = head-mwskz.
    ls_head-diff_inv       = head-lifnr. "指定供应商代码，发票方
    ls_head-deliv_posting  = 'S'.
    ls_head-return_posting = 'H'.

    APPEND VALUE #( tax_amount = abs( head-tax_in )
                                    tax_code = head-mwskz
                                  ) TO lt_tax.


    LOOP AT item INTO DATA(l_item).
      APPEND VALUE #( invoice_doc_item = l_item-miro_nr
                                   po_number = l_item-ebeln
                                   po_item = l_item-ebelp
                                   ref_doc = l_item-lfbnr
                                   ref_doc_year = l_item-lfgja
                                   ref_doc_it = l_item-lfpos
*                                   item_amount = abs( l_item-net )
                                   item_amount = COND #( WHEN l_item-matnr IS  INITIAL AND l_item-aufnr IS  INITIAL THEN abs( l_item-netwr )
                                                                            ELSE abs( l_item-net ) )
                                   quantity = abs( l_item-quantity )
                                   po_unit = l_item-meins
                                   tax_code = head-mwskz ) TO lt_itemdata.
    ENDLOOP.

    split_cost_to_mat( CHANGING mat = lt_mat ).


    CALL FUNCTION 'BAPI_INCOMINGINVOICE_CREATE'
      EXPORTING
        headerdata       = ls_head
      IMPORTING
        invoicedocnumber = lv_invoice
        fiscalyear       = lv_year
      TABLES
        accountingdata   = lt_accdata
        itemdata         = lt_itemdata
        taxdata          = lt_tax
        materialdata     = lt_mat
        return           = lt_return.

    LOOP AT lt_return INTO DATA(ls_return) WHERE type CA 'EAX'.
      msg->add_line( ls_return ).
    ENDLOOP.
    IF sy-subrc EQ 0.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ELSE.
      head-gjahr = lv_year.
      head-belnr = lv_invoice.
      set_status( 'C' ).
      add_gos_relationship( objkey = lv_invoice && lv_year ).
      msg->add_single( msgty = 'S' msgid = 'ZAFO_MIRO' msgno = '015' msgv1 = lv_invoice ).

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
    ENDIF.
    msg->pop_msg( 'X' ).
  ENDMETHOD.


  METHOD print_html.
    DATA(html) = NEW zwft_print_html( ).
    html->add_css( 'ZWFT_PRINT_HTML.CSS' ).
    html->add_title( LOGO_SRC = 'ZWFT_HTML_LOGO.PNG' title =  CONV #( head-bukrs_name ) ).
    DATA(header) = html->add_header( '抬头信息' ).
    html->add_header_item( parent = header name = '供应商' value = head-lifnr_name ).
    html->add_header_item( parent = header name = '对账单'  value = head-miro_no ).
    html->add_header_item( parent = header name = '币种' value =  head-waers ).
    html->add_header_item( parent = header name = '税码' value = head-mwskz ).
    html->add_header_item( parent = header name = '对账日期' value = head-bldat ).
    html->add_header_item( parent = header name = '总数量' value = head-menge ).
    html->add_header_item( parent = header name = '总金额' value = head-amount ).
    html->add_header_item( parent = header name = '费用金额' value = head-cost ).
    html->add_header_item( parent = header name = '对账金额' value = head-reamount ).
    html->add_header_item( parent = header name = '对账税额' value = head-tax ).
    html->add_header_item( parent = header name = '发票金额' value = head-amount_in ).
    html->add_header_item( parent = header name = '发票税额' value = head-tax_in ).
    html->add_header_item( parent = header name = '备注' value = head-remark ).
*    html->close_grid( ).
    IF item IS NOT INITIAL.
      DATA(fcat_item) = zwft_falv=>lvc_fcat_from_itab( it_table = item ).

      zwft_common=>fcat_from_config( EXPORTING i_repid = sy-cprog i_name = 'ITEM'
                                                             CHANGING ct_fcat = fcat_item ).
      DELETE fcat_item WHERE fieldname = 'EBELN'
                                 OR fieldname = 'LFBNR'
                                 OR fieldname = 'QUANTITY_GR'
                                 OR fieldname = 'QUANTITY_IR'
                                 OR fieldname = 'NETWR'
                                 OR fieldname = 'NET_GR'.
      html->add_table( title = '到货明细' fcat = fcat_item tab = item  ).
    ENDIF.

    IF cost IS NOT INITIAL.
      DATA(fcat_cost) = zwft_falv=>lvc_fcat_from_itab( it_table = cost  ).
      zwft_common=>fcat_from_config( EXPORTING i_repid = sy-cprog i_name = 'COST'
      CHANGING ct_fcat = fcat_cost ).
      html->add_table( title = '费用明细' fcat = fcat_cost tab = cost  ).
    ENDIF.

    IF inv IS NOT INITIAL.
      DATA(fcat_inv) = zwft_falv=>lvc_fcat_from_itab( it_table = inv ).
      zwft_common=>fcat_from_config( EXPORTING i_repid = sy-cprog i_name = 'INV'
      CHANGING ct_fcat = fcat_inv ).
      html->add_table( title = '发票明细' fcat = fcat_inv tab = inv  ).
    ENDIF.

    html->display( ).

  ENDMETHOD.


  METHOD register_f4.
    DATA f4 TYPE lvc_t_f4.
    CASE c_falv->title_v5.
      WHEN 'COST'.
        APPEND VALUE #( fieldname = 'COST_KEY'
                                      register  = abap_true
                                      getbefore  = abap_true
                                      chngeafter = abap_true
                                      internal   = space
                                      ) TO f4.
    ENDCASE.
    c_falv->register_f4_for_fields( f4 ).
  ENDMETHOD.


  METHOD save.

    calculate( ).

    check( 'W' ).

    save_data( ).

    msg->pop_msg( 'X' ).

  ENDMETHOD.


  METHOD save_data.

    IF head-miro_no IS INITIAL.
      DATA(num) = zwft_common=>get_nr_number( iv_nr = '01'
                                                                iv_object = 'ZAFO_MIRO' ).
      ASSIGN num->* TO FIELD-SYMBOL(<NUM>).
      head-miro_no = <NUM>.
      head-erdat = COND #( WHEN head-erdat IS INITIAL THEN sy-datum ELSE head-erdat ).
      head-erzet = COND #( WHEN head-erzet IS INITIAL THEN sy-uzeit ELSE head-erzet ).
      head-ernam = COND #( WHEN head-ernam IS INITIAL THEN sy-uname ELSE head-ernam ).
    ENDIF.
    head-aenam = sy-uname.
    head-aedat = sy-datum.
    head-aetim = sy-uzeit.

    LOOP AT item ASSIGNING FIELD-SYMBOL(<item>).
      <item>-miro_no = head-miro_no.
    ENDLOOP.

    LOOP AT cost ASSIGNING FIELD-SYMBOL(<cost>).
      <cost>-miro_no = head-miro_no.
      <cost>-cost_nr = sy-tabix.
    ENDLOOP.

    LOOP AT inv ASSIGNING FIELD-SYMBOL(<inv>).
      <inv>-miro_no = head-miro_no.
    ENDLOOP.

    db_save( ).
    lock( ).
    set_readonly( ).

  ENDMETHOD.


  METHOD set_cost_dropdown.
    DATA:lt_dropdown TYPE lvc_t_drop.
*    DATA(lt_cost_type) = zwft_common=>doma_value_get_single( EXPORTING rollname = 'ZAFO_MIRO_COST_TYPE' ).
    DATA(lt_cost_key_type) = zwft_common=>doma_value_get_single( EXPORTING rollname = 'ZAFO_MIRO_COST_KEY_TYPE' ).
    TRY.
        LOOP AT tt_cost_type INTO DATA(l_cost_type) .
          APPEND VALUE #( handle = falv_cost->fcat[ fieldname = 'COST_TYPE' ]-drdn_hndl
                                          value  = l_cost_type-cost_type ) TO lt_dropdown.
        ENDLOOP.

        LOOP AT lt_cost_key_type INTO DATA(line) WHERE domval IS NOT INITIAL.
          APPEND VALUE #( handle = falv_cost->fcat[ fieldname = 'COST_KEY_TYPE' ]-drdn_hndl
                                          value  = line-domval ) TO lt_dropdown.
        ENDLOOP.
        CHECK lt_dropdown IS NOT INITIAL.
        falv_cost->set_drop_down_table( it_drop_down =  lt_dropdown ).
      CATCH cx_root.
    ENDTRY.
  ENDMETHOD.


  METHOD SET_FALV_READONLY.
    CASE readonly.
      WHEN abap_true.
        i_falv->set_ready_for_input( 0 ).
      WHEN abap_false.
        i_falv->set_ready_for_input( 1 ).
    ENDCASE.
  ENDMETHOD.


  METHOD set_gui_status_exclude.
    zwft_common=>get_status_functions( EXPORTING program = sy-cprog status = '200'
                                                                 IMPORTING  functions = DATA(functions) ).
    DELETE functions WHERE fcode+0(1) <> '&'.
    LOOP AT functions INTO DATA(fun).
      APPEND fun-fcode TO fcodes.
    ENDLOOP.

    CASE head-status.
      WHEN 'A'.
        IF readonly = abap_true.
          DELETE fcodes WHERE table_line = '&EDIT' OR table_line = '&COMMIT' OR table_line = '&DELETE'.
        ELSE.
          DELETE fcodes WHERE table_line = '&SAVE' .
        ENDIF.
      WHEN 'B'.
        DELETE fcodes WHERE table_line = '&UNCOMMIT' OR table_line = '&POST' .
      WHEN 'C'.
        DELETE fcodes WHERE table_line = '&CANCEL'  .
      WHEN 'D'.
    ENDCASE.
    DELETE fcodes WHERE table_line = '&PRINT'  .

  ENDMETHOD.


  METHOD set_icon.
    CASE status.
      WHEN 'A'.
        icon = icon_led_inactive.
        text = '已保存'.
      WHEN 'B'.
        icon = icon_led_green.
        text = '已提交'.
      WHEN 'C'.
        icon = icon_complete.
        text = '已过账'.
      WHEN 'D'.
        icon = icon_delete.
        text = '已作废'.
      WHEN 'E'.
        icon = icon_message_error.
        text = '已驳回'.
    ENDCASE.
  ENDMETHOD.


  METHOD set_readonly.
    CASE head-status.
      WHEN 'A'.
        readonly = COND #( WHEN readonly = abap_true THEN abap_false ELSE abap_true ).
      WHEN 'B' OR 'C' OR 'D' OR 'E' .
        readonly = abap_true.
    ENDCASE.


  ENDMETHOD.


  METHOD set_status.
    head-status = status.
    LOOP AT cost ASSIGNING FIELD-SYMBOL(<cost>).
      set_icon( EXPORTING status = status IMPORTING icon = <cost>-icon  ).
    ENDLOOP.
    LOOP AT inv ASSIGNING FIELD-SYMBOL(<inv>).
      set_icon( EXPORTING status = status IMPORTING icon = <inv>-icon  ).
    ENDLOOP.
    set_readonly( ).
    head-aenam = sy-uname.
    head-aedat = sy-datum.
    head-aetim = sy-uzeit.
    MODIFY zafo_miro_head
     FROM head .
    COMMIT WORK AND WAIT.
  ENDMETHOD.


  METHOD split_cost.
    CLEAR scost.
    DATA l_scost TYPE ty_split_cost.
    DATA fname TYPE fieldname.
    DATA cost_key TYPE zafo_miro_cost_key.

    DATA cost_base TYPE zafo_miro_ecost.
    DATA cost_sum TYPE zafo_miro_ecost.
    DATA cost_leave TYPE zafo_miro_ecost.
    DATA cost_split TYPE zafo_miro_ecost.
    LOOP AT item ASSIGNING FIELD-SYMBOL(<item>).
      CLEAR <item>-cost.

      l_scost-cost_key_type = '物料编号'.
      l_scost-cost_key = <item>-matnr.
      l_scost-cost_sum = <item>-amount.
      COLLECT l_scost INTO scost .
      CLEAR l_scost.

      l_scost-cost_key_type = '采购订单'.
      l_scost-cost_key = <item>-ebeln.
      l_scost-cost_sum = <item>-amount.
      COLLECT l_scost INTO scost .
      CLEAR l_scost.

      l_scost-cost_key_type = '款号'.
      l_scost-cost_key = <item>-bednr.
      l_scost-cost_sum = <item>-amount.
      COLLECT l_scost INTO scost .
      CLEAR l_scost.

      l_scost-cost_key_type = '对账单'.
      l_scost-cost_key = <item>-miro_no.
      l_scost-cost_sum = <item>-amount.
      COLLECT l_scost INTO scost .
      CLEAR l_scost.
    ENDLOOP.

    LOOP AT cost INTO DATA(l_cost)
      GROUP BY ( cost_key_type = l_cost-cost_key_type
                           cost_key = l_cost-cost_key )
       INTO DATA(cost_group).
*      APPEND INITIAL LINE TO scost ASSIGNING FIELD-SYMBOL(<scost>).
      CLEAR: cost_sum,cost_leave,cost_base,cost_key.

      cost_key = COND #( WHEN cost_group-cost_key_type = '对账单' THEN head-miro_no
                                             ELSE cost_group-cost_key ).

      LOOP AT GROUP cost_group ASSIGNING FIELD-SYMBOL(<cost_group>).
        cost_sum += <cost_group>-cost .
      ENDLOOP.
      CHECK cost_sum IS NOT INITIAL.
      cost_leave = cost_sum.

      READ TABLE scost WITH KEY cost_key_type = cost_group-cost_key_type
                                                          cost_key = cost_key
                                                          INTO l_scost.
      IF sy-subrc NE 0.
*        msg->add_single( msgty = 'E'
*                                          msgid = 'ZAFO_MIRO'
*                                          msgno = '005'
*                                          msgv1 = cost_group-cost_key_type
*                                          msgv2 = cost_group-cost_key ).
        RETURN.
      ENDIF.
      cost_base = l_scost-cost_sum.

      fname = SWITCH #( cost_group-cost_key_type
                                         WHEN '物料编号' THEN  'MATNR'
                                         WHEN '款号' THEN  'BEDNR'
                                         WHEN '采购订单' THEN  'EBELN'
                                         WHEN '对账单' THEN  'MIRO_NO' ).

      LOOP AT item ASSIGNING <item>.
        ASSIGN COMPONENT fname OF STRUCTURE <item> TO FIELD-SYMBOL(<value>).
        CHECK sy-subrc EQ 0.
        CHECK <value> = cost_key.
        IF cost_base NE 0.
          cost_split = cost_sum / cost_base * <item>-amount.
        ELSE.
          cost_split = 0.
        ENDIF.
        <item>-cost += cost_split.
        cost_leave -= cost_split.
      ENDLOOP.

      IF cost_leave IS NOT INITIAL.
        LOOP AT item ASSIGNING <item>.
          ASSIGN COMPONENT fname OF STRUCTURE <item> TO <value>.
          CHECK sy-subrc EQ 0.
          CHECK <value> = cost_key.
          <item>-cost += cost_leave.
          cost_leave -= cost_leave.
        ENDLOOP.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD split_cost_to_mat.
    DATA ls_split_mat TYPE  ty_split_mat.
    DATA lt_split_mat TYPE TABLE OF ty_split_mat.
    DATA ls_split_aufnr TYPE  ty_split_aufnr.
    DATA lt_split_aufnr TYPE TABLE OF ty_split_aufnr.
    DATA l_rblgp TYPE rblgp.
    DATA sum_afpo_wemng TYPE wemng.
    DATA l_remain_cost TYPE zafo_miro_amount.

    LOOP AT item INTO DATA(l_item).
      CHECK l_item-netwr <> l_item-net.
      IF l_item-matnr IS NOT INITIAL.
        ls_split_mat = VALUE #( matnr = l_item-matnr
                                               werks = l_item-werks
                                               meins = l_item-meins
                                               menge = l_item-quantity
                                               cost = l_item-netwr - l_item-net
                                            ).
        COLLECT ls_split_mat INTO lt_split_mat.
        CONTINUE.
      ELSEIF l_item-aufnr IS NOT INITIAL.
        ls_split_aufnr = VALUE #( aufnr = l_item-aufnr
                                                 werks = l_item-werks
                                                 cost = l_item-netwr - l_item-net ).
        COLLECT ls_split_aufnr INTO lt_split_aufnr.
      ENDIF.
    ENDLOOP.

    IF lt_split_aufnr IS NOT INITIAL.
      LOOP AT lt_split_aufnr INTO ls_split_aufnr.
        CHECK ls_split_aufnr-cost IS NOT INITIAL.
        SELECT matnr,wemng,meins FROM afpo
          WHERE aufnr = @ls_split_aufnr-aufnr
          AND wemng <> 0
          INTO TABLE @DATA(lt_afpo).
        IF sy-subrc NE 0.
          SELECT matnr,psmng AS wemng ,meins FROM afpo
          WHERE aufnr = @ls_split_aufnr-aufnr
          AND psmng <> 0 AND posnr > '0001'
          INTO TABLE @lt_afpo.
        ENDIF.
        CLEAR sum_afpo_wemng.
        LOOP AT lt_afpo INTO DATA(l_afpo).
          sum_afpo_wemng += l_afpo-wemng.
        ENDLOOP.
        CHECK sum_afpo_wemng IS NOT INITIAL.
        l_remain_cost = ls_split_aufnr-cost.
        LOOP AT lt_afpo INTO l_afpo.
          ls_split_mat = VALUE #( matnr = l_afpo-matnr
                                                werks = ls_split_aufnr-werks
                                                meins = l_afpo-meins
                                                menge = l_afpo-wemng
                                                cost = ls_split_aufnr-cost * l_afpo-wemng / sum_afpo_wemng
                                                  ).
          l_remain_cost -= ls_split_mat-cost.
          COLLECT ls_split_mat INTO lt_split_mat.
        ENDLOOP.
        IF sy-subrc EQ 0 AND l_remain_cost <> 0.
          ls_split_mat-cost = l_remain_cost.
          COLLECT ls_split_mat INTO lt_split_mat.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF lt_split_mat IS NOT INITIAL.
      LOOP AT lt_split_mat INTO ls_split_mat.
        CHECK ls_split_mat-cost IS NOT INITIAL.
        ADD 1 TO l_rblgp.
        APPEND VALUE #( invoice_doc_item = l_rblgp
                                        material = ls_split_mat-matnr
                                        val_area = ls_split_mat-werks
                                        db_cr_ind = COND #( WHEN ls_split_mat-cost < 0 THEN 'H'
                                                                              ELSE 'S' )
                                        item_amount = abs( ls_split_mat-cost )
                                        quantity = abs( ls_split_mat-menge )
                                        base_uom = ls_split_mat-meins
                                        tax_code = head-mwskz
                                      ) TO mat.

      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD toolbar.
    CHECK readonly NE abap_true.
    CASE c_falv->title_v5.
      WHEN 'ITEM'.
        APPEND VALUE #( function = '&DEL'
                                        icon = icon_delete
                                        quickinfo = TEXT-002
                                        disabled = ''
                                        text = TEXT-002
                                        ) TO i_object->mt_toolbar.
      WHEN 'COST'.
        APPEND VALUE #( function = '&ADD'
                                        icon = icon_insert_row
                                        quickinfo = TEXT-001
                                        disabled = ''
                                        text = TEXT-001
                                        ) TO i_object->mt_toolbar.
        APPEND VALUE #( function = '&DEL'
                                        icon = icon_delete
                                        quickinfo = TEXT-002
                                        disabled = ''
                                        text = TEXT-002
                                        ) TO i_object->mt_toolbar.
      WHEN 'INV'.
        APPEND VALUE #( function = '&ADD'
                                      icon = icon_insert_row
                                      quickinfo = TEXT-001
                                      disabled = ''
                                      text = TEXT-001
                                      ) TO i_object->mt_toolbar.
        APPEND VALUE #( function = '&DEL'
                                        icon = icon_delete
                                        quickinfo = TEXT-002
                                        disabled = ''
                                        text = TEXT-002
                                        ) TO i_object->mt_toolbar.
        APPEND VALUE #( function = '&ORC'
                                      icon = icon_helpassistent_on
                                      quickinfo = TEXT-009
                                      disabled = ''
                                      text = TEXT-009
                                      ) TO i_object->mt_toolbar.
    ENDCASE.
  ENDMETHOD.


  METHOD uncommit.
    set_status( 'A' ).
    msg->add_single( msgty = 'S' msgid = 'ZAFO_MIRO' msgno = '013' )."保存成功
    msg->pop_msg( 'X' ).
  ENDMETHOD.


  METHOD user_command.
    CHECK readonly IS INITIAL.
    CASE c_falv->title_v5.
      WHEN 'ITEM'.
        user_command_item( EXPORTING c_falv = c_falv i_ucomm = i_ucomm ).
      WHEN 'COST'.
        user_command_cost( EXPORTING c_falv = c_falv i_ucomm = i_ucomm ).
      WHEN 'INV'.
        user_command_inv( EXPORTING c_falv = c_falv i_ucomm = i_ucomm ).
    ENDCASE.


  ENDMETHOD.


  METHOD user_command_cost.
    CASE i_ucomm.
      WHEN '&ADD'.
        SORT cost BY cost_nr.
        READ TABLE cost INTO DATA(l_cost) INDEX lines( cost ).
        IF sy-subrc EQ 0.
          DATA(new_nr) = l_cost-cost_nr + 1.
        ELSE.
          new_nr = 1.
        ENDIF.
        APPEND INITIAL LINE TO cost ASSIGNING FIELD-SYMBOL(<cost>).
        <cost>-cost_nr = new_nr.
        <cost>-icon = icon_led_inactive.

      WHEN '&DEL'.
        c_falv->get_selected_rows( IMPORTING et_row_no = DATA(lt_row) ).
        IF lt_row IS INITIAL.
          MESSAGE s004 DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.
        CHECK zwft_common=>confirm( TEXT-003 ) .
        LOOP AT lt_row INTO DATA(ls_row).
          READ TABLE cost INDEX ls_row-row_id ASSIGNING <cost>.
          IF sy-subrc EQ 0.
            <cost>-icon = icon_delete.
          ENDIF.
        ENDLOOP.
        DELETE cost WHERE icon = icon_delete.
        calculate_head( ).
    ENDCASE.

    c_falv->soft_refresh( ).

  ENDMETHOD.


  METHOD user_command_inv.
    CASE i_ucomm.
      WHEN '&ADD'.
        SORT inv BY inv_nr.
        READ TABLE inv INTO DATA(l_inv) INDEX lines( inv ).
        IF sy-subrc EQ 0.
          DATA(new_nr) = l_inv-inv_nr + 1.
        ELSE.
          new_nr = 1.
        ENDIF.
        APPEND INITIAL LINE TO inv ASSIGNING FIELD-SYMBOL(<inv>).
        <inv>-inv_nr = new_nr.
        <inv>-icon = icon_led_inactive.
      WHEN '&DEL'.
        c_falv->get_selected_rows( IMPORTING et_row_no = DATA(lt_row) ).
        IF lt_row IS INITIAL.
          MESSAGE s004 DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.
        CHECK zwft_common=>confirm( TEXT-003 ).
        LOOP AT lt_row INTO DATA(ls_row).
          READ TABLE inv INDEX ls_row-row_id ASSIGNING <inv>.
          IF sy-subrc EQ 0.
            <inv>-icon = icon_delete.
          ENDIF.
        ENDLOOP.
        DELETE inv WHERE icon = icon_delete.
        calculate_head( ).
      WHEN '&ORC'.
        DATA lo_orc TYPE REF TO zwft_invoice_orc.
        lo_orc = NEW zwft_invoice_orc( l_api_key = '1Bf4y1kOpejGp4u5fbtCYYwB'
                                                   l_secret_key = 'XW8x6VdBHwSZ2b1ikRca4SVRQamP5M3c' ).
        lo_orc->get_invoice( IMPORTING invoice = DATA(ls_invoice) ).
        CHECK ls_invoice-words_result_num IS NOT INITIAL.

        SORT inv BY inv_nr.
        READ TABLE inv INTO l_inv INDEX lines( inv ).
        IF sy-subrc EQ 0.
          new_nr = l_inv-inv_nr + 1.
        ELSE.
          new_nr = 1.
        ENDIF.
        APPEND INITIAL LINE TO inv ASSIGNING <inv>.
        <inv>-inv_nr = new_nr.
        <inv>-inv_number = ls_invoice-words_result-invoicenum.
        <inv>-net_in = ls_invoice-words_result-totalamount.
        <inv>-tax_in = ls_invoice-words_result-totaltax.
        <inv>-amount_in = <inv>-net_in + <inv>-tax_in.
        DATA(date) = ls_invoice-words_result-invoicedate.
        REPLACE '年' IN date WITH ''.
        REPLACE '月' IN date WITH ''.
        REPLACE '日' IN date WITH ''.
        <inv>-redat = date.
        <inv>-remark = ls_invoice-words_result-remarks.
        <inv>-image_xstring = lo_orc->image_xstring.
        IF lo_orc->extfile = 'PDF'.
          <inv>-image_icon = icon_pdf.
        ELSE.
          <inv>-image_icon = icon_wd_image.
        ENDIF.
        <inv>-icon = icon_led_yellow.
        FREE lo_orc.
        calculate( ).
    ENDCASE.

    c_falv->soft_refresh( ).
  ENDMETHOD.


  METHOD user_command_item.
    CASE i_ucomm.
      WHEN '&DEL'.
        c_falv->get_selected_rows( IMPORTING et_row_no = DATA(lt_row) ).
        IF lt_row IS INITIAL.
          MESSAGE s004 DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.
        CHECK zwft_common=>confirm( TEXT-003 ).
        LOOP AT lt_row INTO DATA(ls_row).
          READ TABLE item INDEX ls_row-row_id ASSIGNING FIELD-SYMBOL(<item>).
          IF sy-subrc EQ 0.
            <item>-ebeln = ''.
          ENDIF.
        ENDLOOP.
        DELETE item WHERE ebeln = ''.
        calculate_head( ).
    ENDCASE.

    c_falv->soft_refresh( ).

  ENDMETHOD.


  METHOD user_command_main.
    check_falv_changed_data( ).

    CASE i_ucomm .
      WHEN 'ENTR'.
        click_focus( ).
      WHEN '&TAX'.
        calculate( ).
      WHEN '&EDIT'.
        CHECK auth_check( actvt = '02' bukrs = head-bukrs ) EQ abap_true.
        set_readonly( ).
      WHEN '&SAVE'.
        CHECK auth_check( actvt = '01' bukrs = head-bukrs ) EQ abap_true.
        save( ).
      WHEN '&DELETE'.
        CHECK auth_check( actvt = '06' bukrs = head-bukrs ) EQ abap_true.
        set_status( 'D' ).
      WHEN '&COMMIT'.
        CHECK auth_check( actvt = '02' bukrs = head-bukrs ) EQ abap_true.
        commit( ).
      WHEN '&UNCOMMIT'.
        CHECK auth_check( actvt = '02' bukrs = head-bukrs ) EQ abap_true.
        uncommit( ).
      WHEN '&POST'.
        CHECK auth_check( actvt = '10' bukrs = head-bukrs ) EQ abap_true.
        post( ).
      WHEN '&CANCEL'.
        CHECK auth_check( actvt = '10' bukrs = head-bukrs ) EQ abap_true.
        cancel( ).
      WHEN '&REFUSE'.
        CHECK auth_check( actvt = '10' bukrs = head-bukrs ) EQ abap_true.
        set_status( 'E' ).
      WHEN '&PRINT'.
        CHECK auth_check( actvt = '04' bukrs = head-bukrs ) EQ abap_true.
        print_html( ).
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
