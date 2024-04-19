FUNCTION ZGET_MATDETAILS.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IMATNR) TYPE  MATNR
*"  EXPORTING
*"     VALUE(IMSTAE) TYPE  MSTAE
*"----------------------------------------------------------------------

data: imara_tab type standard table of mara with header line.
data: wmara type mara.

select single * from mara into wmara where matnr = imatnr.
  append wmara to imara_tab.

  imstae = wmara-mstae.


ENDFUNCTION.
