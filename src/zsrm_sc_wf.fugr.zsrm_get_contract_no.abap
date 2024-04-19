FUNCTION ZSRM_GET_CONTRACT_NO.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(EBELN) TYPE  EBELN
*"     VALUE(EBELP) TYPE  EBELP OPTIONAL
*"  TABLES
*"      DETAILS STRUCTURE  ZPOCONTRACT
*"----------------------------------------------------------------------

  Data: Begin of t_details occurs 0,
        ponum type char20,
        item type char10,
        contract type char40,
        citem type char40,
        end of t_details.


  Data: lv_sc type char10,
        lv_po type char20,
        lv_item type char10.

  DATA: ltp_log_dest TYPE tb_rfcdest,
        lv_val type Z_VARVALUEL.


  select single VALUE1 from ZVARSYS into lv_val where
  PROGRAMM = 'LOGSYS' AND
  VARNAME = 'SRM_RFC'.

  ltp_log_dest = lv_val.
  lv_po = EBELN.

  IF NOT EBELP IS INITIAL.
    lv_item = EBELP.
  ENDIF.

  CALL FUNCTION 'ZSRM_SC_DETAILS_FROM_PO' DESTINATION ltp_log_dest
    EXPORTING
      PONUM   = lv_po
      ITEM    = lv_item
    TABLES
      DETAILS = t_details.

  loop at t_details.
    clear details.
    details-ebeln = t_details-ponum.
    details-ebelp = t_details-item.
    details-KONNR = t_details-contract.
    if not t_details-citem is initial.
      details-KTPNR = t_details-citem.
    else.
      select single ebelp from EKPO into details-KTPNR
      where ebeln = t_details-contract AND
            meins = 'SRV'.
    endif.
    append details.
  endloop.

ENDFUNCTION.
