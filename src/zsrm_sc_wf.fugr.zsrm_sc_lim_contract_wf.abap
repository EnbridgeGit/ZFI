FUNCTION ZSRM_SC_LIM_CONTRACT_WF.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(OBJTYPE) LIKE  SWETYPECOU-OBJTYPE
*"     REFERENCE(OBJKEY) LIKE  SWEINSTCOU-OBJKEY
*"     REFERENCE(EVENT) LIKE  SWEINSTCOU-EVENT
*"     REFERENCE(RECTYPE) LIKE  SWETYPECOU-RECTYPE
*"  TABLES
*"      EVENT_CONTAINER STRUCTURE  SWCONT
*"  EXCEPTIONS
*"      WORKFLOW_NOT_REQUIRED
*"----------------------------------------------------------------------

  DATA: lv_ebeln type ebeln,
        lv_ind type c,
        lv_bsart like ekko-bsart.

  DATA: it_ekpo type table of ekpo with header line.

  lv_ebeln = objkey(10).

  clear lv_ind.

  select single bsart from ekko into lv_bsart where
  ebeln = lv_ebeln.

  IF lv_bsart NE 'ZF'.
    RAISE workflow_not_required.
  ENDIF.

  select * from ekpo into table it_ekpo
  where ebeln = lv_ebeln.

  loop at it_ekpo where not konnr is initial or not banfn is initial.
    lv_ind = 'X'.
    exit.
  endloop.

  IF lv_ind = 'X'.
    RAISE workflow_not_required.
  ENDIF.


ENDFUNCTION.
