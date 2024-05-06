*&---------------------------------------------------------------------*
*& 包含               ZAFO_MIRO_TOP
*&---------------------------------------------------------------------*

TABLES: zafo_miro_head,ekko,mseg,ekpo.

DATA falv_todo TYPE REF TO zwft_falv.
DATA falv_dis TYPE REF TO zwft_falv.
DATA: gt_head TYPE TABLE OF zafo_miro_shead,
      gt_todo TYPE TABLE OF zafo_miro_po_stodo.
*      gt_cost    TYPE TABLE OF zafo_miro_cost,
*      gt_invoice TYPE TABLE OF zafo_miro_invoice.
DATA:
  splitter_100 TYPE REF TO   cl_gui_splitter_container,
  splitter_110 TYPE REF TO   cl_gui_splitter_container,
  container201 TYPE REF TO   cl_gui_custom_container,
  container202 TYPE REF TO   cl_gui_custom_container,
  container203 TYPE REF TO   cl_gui_custom_container.

DATA: class TYPE REF TO zafo_miro_class.
FIELD-SYMBOLS: <class> TYPE REF TO zafo_miro_class.
FIELD-SYMBOLS <head> TYPE  zafo_miro_head.

ranges auth_burks for t001-bukrs.
