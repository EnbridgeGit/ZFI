FUNCTION ZGET_CONTRACTDTE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(ICONTRACT) TYPE  EBELN
*"  EXPORTING
*"     VALUE(IPLIFZ) TYPE  PLIFZ
*"----------------------------------------------------------------------

data: iekpo_tab type standard table of ekpo with header line.
data: wekpo type ekpo.

select single * from ekpo into wekpo where ebeln = icontract.
  append wekpo to iekpo_tab.

  iplifz = wekpo-plifz.




ENDFUNCTION.
