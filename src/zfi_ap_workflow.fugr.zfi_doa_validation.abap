FUNCTION zfi_doa_validation.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IMP_SYSTEM) TYPE  CHAR02 DEFAULT 'UG'
*"     VALUE(IMP_LOOKUP_KEY) TYPE  ZFI_LOOKUP_KEY
*"     VALUE(IMP_TCODE) TYPE  SYTCODE
*"     VALUE(IMP_DOC_TYPE) TYPE  ZFI_DOC_TYPE OPTIONAL
*"     VALUE(IMP_AMOUNT) TYPE  ZFI_DOA_AMOUNT
*"     VALUE(IMP_WAERS) TYPE  WAERS OPTIONAL
*"     VALUE(IMP_RFCDEST) TYPE  TB_RFCDEST OPTIONAL
*"  EXPORTING
*"     VALUE(EXP_DOA) TYPE  ZFI_DOA_VALIDITY
*"     VALUE(EXP_AMOUNT) TYPE  ZFI_DOA_AMOUNT
*"  EXCEPTIONS
*"      BAD_LOOKUP_KEY
*"      NO_DOA_FOUND
*"      NO_GRADE
*"----------------------------------------------------------------------
*----------------------------------------------------------------------*
* Version No    : MZH01                                                *
* Date          : Mar 09, 2023                                         *
* Modified By   : Mohammed Hossain                                     *
* Correction No :                                                      *
* Description   : DOA migration changes                                *
*----------------------------------------------------------------------*
  DATA: lv_rfcdest LIKE imp_rfcdest.

  IF imp_rfcdest IS INITIAL.
* Begin of Changes <Chaya>
* DOA Changes
*    CALL FUNCTION 'ZFI_GET_RFC_DEST'
*      EXPORTING
*        imp_paramtype = 'ECCUS'
*      IMPORTING
*        exp_rfcdest   = lv_rfcdest.

* IF the same system calls mark it as None
    lv_rfcdest = 'NONE'.
* End of Changes <Chaya>
  ELSE.
    lv_rfcdest = imp_rfcdest.
  ENDIF.

* Begin of Changes <Chaya>
*DOA Changes
*  CALL FUNCTION 'ZFI_DOA_VALIDATION' DESTINATION lv_rfcdest
*    EXPORTING
*      imp_system     = imp_system
*      imp_lookup_key = imp_lookup_key
*      imp_tcode      = imp_tcode
*      imp_doc_type   = imp_doc_type
*      imp_amount     = imp_amount
*      imp_waers      = imp_waers
*    IMPORTING
*      exp_doa        = exp_doa
*    EXCEPTIONS
*      BAD_LOOKUP_KEY = 1
*      NO_DOA_FOUND   = 2
*      NO_GRADE       = 3
*      OTHERS         = 4.

  DATA: lv_lookup_key TYPE zfi_lookup_key,
         lv_userid     TYPE username,
         lv_pernr      TYPE pernr_d,
         lv_grade      TYPE zfi_lookup_key,
         lv_doc_type   TYPE zfi_doc_type,
         ls_doa        TYPE zfit_doa_new.


  IF imp_lookup_key CO '0123456789 '.
    CLEAR lv_userid.
    lv_pernr = imp_lookup_key.
  ELSE.
    lv_userid = imp_lookup_key.
*    Begin of comment MZH01
*    CALL FUNCTION 'ZFI_CONVERT_USER_TO_PERNR'
*      EXPORTING
*        imp_username = lv_userid
*      IMPORTING
*        exp_pernr    = lv_pernr.
*End of comment MZH01

*Begin of MZH01
    DATA: ltp_log_dest TYPE tb_rfcdest.
    CALL FUNCTION 'ZFI_GET_RFC_DEST'
      EXPORTING
        imp_paramtype = 'HR'
      IMPORTING
        exp_rfcdest   = ltp_log_dest.

    CALL FUNCTION 'ZFI_CONVERT_USER_TO_PERNR' DESTINATION ltp_log_dest
      EXPORTING
        imp_username = lv_userid
      IMPORTING
        exp_pernr    = lv_pernr.
  ENDIF.
*End of MZH01

  IF lv_pernr IS INITIAL.
    MESSAGE e005(zfi_workflow) WITH imp_lookup_key RAISING bad_lookup_key.
  ENDIF.


  IF imp_doc_type IS NOT INITIAL.
    CONCATENATE imp_doc_type '_' imp_system INTO lv_doc_type.
  ENDIF.

  PERFORM get_table_entries USING lv_pernr
                                  lv_doc_type
                                  imp_tcode
                                  imp_system
                         CHANGING ls_doa.

  "Still nothing found.
  IF ls_doa IS INITIAL.
    MESSAGE e005(zfi_workflow) WITH lv_pernr RAISING no_doa_found.
  ENDIF.

  "If we get here, the person has doa in LST_DOA, check for amounts.

  exp_amount = ls_doa-amount.
  IF ls_doa-amount GE imp_amount.
    exp_doa = 'Y'.
  ELSE.
    exp_doa = 'N'.
  ENDIF.

* End of Changes

ENDFUNCTION.
