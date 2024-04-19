FUNCTION zfi_getapprovers.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IMP_USERNAME) TYPE  WFSYST-INITIATOR
*"  EXPORTING
*"     VALUE(EXP_MANAGER) TYPE  WFSYST-INITIATOR
*"----------------------------------------------------------------------

*&---------------------------------------------------------------------*
*& Function Module    :  ZFI_GETAPPROVERS                              *
*& Author             :  Shankar Balasubramaniam                       *
*& Creation Date      :  13-Sep-2011                                   *
*& Object ID          :  NA                                            *
*& Application Area   :  NA                                            *
*& Description        :  Get manager                                   *
*&---------------------------------------------------------------------*
  DATA:  ltp_log_dest TYPE tb_rfcdest.

  CONSTANTS : c_tcode TYPE tstc-tcode VALUE 'FV60'.

* Begin of Changes <Chaya>
* DOA Changes from US to UG
*  "Get Logical Destination Name
*  CALL FUNCTION 'ZFI_GET_LOGICAL_DEST'
*    EXPORTING
*      imp_paramtype = 'ECCUS'
*    IMPORTING
*      exp_rfcdest   = ltp_log_dest.
* End of Changes <Chaya>

  "Get the manager of the user
* Begin of Changes <Chaya>
* DOA Changes from US to UG
*  CALL FUNCTION 'ZFI_GET_MANAGER' DESTINATION ltp_log_dest
CALL FUNCTION 'ZFI_GET_MANAGER'
* End of Changes <Chaya>
    EXPORTING
      imp_username = imp_username
    IMPORTING
      exp_manager  = exp_manager
    EXCEPTIONS
      nobody_found = 1
      OTHERS       = 2.

  IF sy-subrc <> 0.
    MESSAGE e013(zfi_workflow) WITH imp_username DISPLAY LIKE 'E'.
  ENDIF.
ENDFUNCTION.
