class ZCL_IM_TRIP_POST_FI definition
  public
  final
  create public .

public section.
*"* public components of class ZCL_IM_TRIP_POST_FI
*"* do not include other source files here!!!

  interfaces IF_EX_TRIP_POST_FI .
protected section.
*"* protected components of class ZCL_IM_TRIP_POST_FI
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_IM_TRIP_POST_FI
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_IM_TRIP_POST_FI IMPLEMENTATION.


method IF_EX_TRIP_POST_FI~DETERMINE_SRFC.
endmethod.


method IF_EX_TRIP_POST_FI~EXA706K.
endmethod.


method IF_EX_TRIP_POST_FI~EXB706K.
endmethod.


method IF_EX_TRIP_POST_FI~EX_CCARD.

endmethod.


METHOD if_ex_trip_post_fi~ex_sgtxt.

  DATA: lv_sptxt     TYPE t706b5-sptxt,
        lv_receiptno TYPE ptrv_srec-receiptno,
        ls_ptrv_srec TYPE ptrv_srec,
        ls_ptrv_sadd TYPE ptrv_sadd.

  IF sgtxt(5) = '*Trip'.
    "This is mileage entry
    sgtxt = 'Mileage'.
  ELSE.
    "Get line item detail.
    lv_receiptno = sgtxt+13(3).

    SELECT SINGLE *
       FROM ptrv_sadd
       INTO ls_ptrv_sadd
       WHERE reinr = t_head-reinr
         AND receiptno = lv_receiptno.

    IF sy-subrc = 0 AND ls_ptrv_sadd-c_txt IS NOT INITIAL.
      sgtxt = ls_ptrv_sadd-c_txt. "CC Description
    ELSE.
      "OOP
      SELECT SINGLE *
        FROM ptrv_srec
        INTO ls_ptrv_srec
        WHERE reinr = t_head-reinr
          AND receiptno = lv_receiptno.

      IF sy-subrc = 0.
        SELECT SINGLE sptxt
          FROM t706b5
          INTO lv_sptxt
          WHERE spras = 'EN'
            AND morei = 'UG'
            AND spkzl = ls_ptrv_srec-exp_type.

        IF sy-subrc IS INITIAL.
          sgtxt = lv_sptxt.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDMETHOD.


method IF_EX_TRIP_POST_FI~EX_ZWEP.

  DATA: ls_pa0001 TYPE pa0001,
        ls_t500p TYPE t500p,
        ls_tax_codes TYPE zftv_ca_tax_xref,
        lv_spkzl TYPE zftv_ca_tax_xref-spkzl VALUE 'FAK',
        lv_rgion TYPE zftv_ca_tax_xref-rgion.

  break sahmad.
*  IF zwep-mwskz = 'ZM'.   "Tax code specific to Mileage
  IF zwep-LGART = '7103'.
* tax calculation for US destination to use home province tax code
*    IF zwep-zland = 'CA'.
      SELECT SINGLE * FROM pa0001 INTO ls_pa0001 WHERE pernr = zwep-pernr.
      SELECT SINGLE * FROM t500p INTO ls_t500p WHERE persa = ls_pa0001-werks.
      IF ls_t500p-regio IS NOT INITIAL.
        lv_rgion = ls_t500p-regio.
        SELECT SINGLE * FROM zftv_ca_tax_xref  INTO ls_tax_codes
          WHERE spkzl = lv_spkzl
            AND rgion = lv_rgion.  "ls_t500p-regio.
        IF sy-subrc = 0.
          zwep-mwskz = ls_tax_codes-mwskz.
        ELSE.
          zwep-mwskz = 'I0'. "'T2'.
        ENDIF.
      ELSE.
        zwep-mwskz = 'I0'. "Region is blank
      ENDIF.
*    ELSE.
*      zwep-mwskz = 'I0'.  "Country <> CA
*    ENDIF.
  ENDIF.

endmethod.


method IF_EX_TRIP_POST_FI~EX_ZWEP_ACCOUNT1.
endmethod.


method IF_EX_TRIP_POST_FI~EX_ZWEP_ACCOUNT2.
endmethod.


method IF_EX_TRIP_POST_FI~EX_ZWEP_COMPLETE.
endmethod.


method IF_EX_TRIP_POST_FI~FILL_EXT_RAWDATA.
endmethod.


method IF_EX_TRIP_POST_FI~MODIFY_PTRV_DOC_HD.
endmethod.


method IF_EX_TRIP_POST_FI~MODIFY_PTRV_DOC_IT.
endmethod.


method IF_EX_TRIP_POST_FI~MODIFY_PTRV_DOC_TAX.
endmethod.
ENDCLASS.
