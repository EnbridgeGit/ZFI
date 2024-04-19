FUNCTION zsample_process_00001830.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_BUDAT) LIKE  F110C-BUDAT OPTIONAL
*"     REFERENCE(I_NEDAT) LIKE  F110V-NEDAT OPTIONAL
*"     REFERENCE(I_FDEBI) LIKE  F110V-FDEBI OPTIONAL
*"     REFERENCE(I_TRACE) LIKE  TRCOPT STRUCTURE  TRCOPT OPTIONAL
*"  TABLES
*"      T_REGUP STRUCTURE  REGUP_1830
*"  CHANGING
*"     REFERENCE(C_REGUH) TYPE  REGUH_1830
*"----------------------------------------------------------------------
*force the posting date of documents for check payments with payment
*method P to be the same date as the check issue date
*Save date in Global memory and import at BTE 1120.
************************************************************************

  DATA: lt_regup TYPE TABLE OF regup_1830,
        ls_regup TYPE regup_1830,
        lv_check_date TYPE sy-datum.

  IF c_reguh-rzawe = 'P'.
    IF t_regup[] IS NOT INITIAL.
      lt_regup[] = t_regup[].
      SORT lt_regup BY zfbdt.
      READ TABLE lt_regup INTO ls_regup INDEX 1.
      IF ls_regup-zfbdt >= sy-datum.
        lv_check_date = ls_regup-zfbdt.
      ELSE.
        lv_check_date = sy-datum.
      ENDIF.
      EXPORT lv_check_date TO MEMORY ID 'ZBTE1830'.
    ENDIF.
  ENDIF.

ENDFUNCTION.
