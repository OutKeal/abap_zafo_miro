*****           Implementation of object type ZAFO_MIRO            *****
INCLUDE <object>.
begin_data object. " Do not change.. DATA is generated
* only private members may be inserted into structure private
DATA:
  " begin of private,
  "   to declare private attributes remove comments and
  "   insert private attributes here ...
  " end of private,
  BEGIN OF key,
    miro_no LIKE zafo_miro_head-miro_no,
  END OF key.
end_data object. " Do not change.. DATA is generated

begin_method display changing container.

end_method.

begin_method gosaddobjects changing container.
DATA:
      service(255),
      busidentifs LIKE borident OCCURS 0.
DATA: ls_borident TYPE borident.

CLEAR ls_borident.

ls_borident-logsys = space.
ls_borident-objtype = 'ZAFO_MIRO'.
ls_borident-objkey = object-KEY.
APPEND ls_borident TO busidentifs.

swc_get_element container 'Service' service.
swc_set_table container 'BusIdentifs' busidentifs.
end_method.

begin_method existencecheck changing container.
DATA:ls_miro_no TYPE zafo_miro_no.
SELECT SINGLE miro_no  INTO ls_miro_no FROM zafo_miro_head WHERE miro_no = object-key-miro_no.
IF sy-subrc NE 0.
  exit_object_not_found.
ENDIF.
end_method.
