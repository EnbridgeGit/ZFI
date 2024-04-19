*---------------------------------------------------------------------*
*       REPORT RKROS000                                               *
*---------------------------------------------------------------------*
*       This report contains the Standard-Exits Sxx for summarization *
*       that can be used in every client                              *
*---------------------------------------------------------------------*
* CHANGES:                                                            *
* 2016/04/07 ACR-881 G.Ymana                                          *
*            Add New ZCO_BWEXCEPTIONS mapping table to determine      *
*            Cost Code based on Cost Center/Cost Element              *
*---------------------------------------------------------------------*
REPORT zbwco050 MESSAGE-ID gl.
TABLES: ccss.
*---------------------------------------------------------------------*
*       FORM C01_USER_EXIT                                            *
*---------------------------------------------------------------------*
*       Summarization to cost element groups                          *
*---------------------------------------------------------------------*
FORM c01_user_exit.
  IF     ccss-kstar BETWEEN  '0000100000'
                    AND      '0000199999'.
    ccss-kstar = '0000100000'.
  ELSEIF ccss-kstar BETWEEN  '0000200000'
                    AND      '0000299999'.
    ccss-kstar = '0000200000'.
  ENDIF.
*Clear return-code, so that the exit-routine is recognized (important)
  CLEAR sy-subrc.
ENDFORM.                    "C01_USER_EXIT
*---------------------------------------------------------------------*
*       FORM C01_USER_EXIT                                            *
*---------------------------------------------------------------------*
*       Summarization to cost element groups                          *
*---------------------------------------------------------------------*
FORM c02_user_exit.

  TABLES: ZCO_BWEXCEPTIONS.                                      "ACR881

  CONSTANTS: lc_0102 TYPE setnode-setclass VALUE '0102',
             lc_exec_roll TYPE setnode-setname VALUE 'EXEC_ROLL',
             lc_datbi TYPE csks-datbi VALUE '99991231'.
  DATA: lv_bukrs   TYPE ccss-bukrs,
        lv_cgroup  TYPE zco_bwexceptions-cgroup,                 "ACR881
        ls_csks    TYPE csks,
        lt_setnode TYPE TABLE OF setnode,
        ls_setnode TYPE setnode,
        lt_setleaf TYPE TABLE OF setleaf,
        ls_setleaf TYPE setleaf.

  IF ccss-kokrs IS NOT INITIAL AND
     ccss-lednr = '00'.
    IF ccss-bukrs IS INITIAL.
      SELECT SINGLE * FROM csks INTO ls_csks
        WHERE kokrs = ccss-kokrs
          AND kostl = ccss-kostl
          AND datbi = lc_datbi.
      lv_bukrs = ls_csks-bukrs.
    ELSE.
      lv_bukrs = ccss-bukrs.
    ENDIF.

    SELECT cgroup from zco_bwexceptions into lv_cgroup           "ACR881
     WHERE kostl = ccss-kostl                                    "ACR881
       AND ( kstar = ccss-kstar OR                               "ACR881
           kstar = '*' ).                                        "ACR881
    ENDSELECT.                                                   "ACR881
                                                                 "ACR881
    IF sy-subrc = 0.                                             "ACR881
      CONCATENATE lv_cgroup '_' lv_bukrs INTO ccss-kstar.        "ACR881
    ELSE.                                                        "ACR881
      SELECT * FROM setnode INTO TABLE lt_setnode
        WHERE setclass = lc_0102
          AND setname = lc_exec_roll.
      IF lt_setnode[] IS NOT INITIAL.
        SELECT * FROM setleaf INTO TABLE lt_setleaf
          FOR ALL ENTRIES IN lt_setnode
          WHERE setclass = lt_setnode-setclass
            AND subclass = lt_setnode-subclass
            AND setname  = lt_setnode-subsetname.
      ENDIF.
      READ TABLE lt_setleaf INTO ls_setleaf
                            WITH KEY valfrom = ccss-kstar.
      IF sy-subrc = 0.
        CONCATENATE ls_setleaf-setname+3(4) '_' lv_bukrs
               INTO ccss-kstar.
*      ELSE.
*        CONCATENATE 'NOVAL_' ccss-bukrs INTO ccss-kstar.
      ENDIF.
    ENDIF.                                                       "ACR881
  ENDIF.
*Clear return-code, so that the exit-routine is recognized (important)
  CLEAR sy-subrc.
ENDFORM.                    "C02_USER_EXIT
