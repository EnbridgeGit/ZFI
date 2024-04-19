class ZCL_IM_INVOICE_UPDATE definition
  public
  final
  create public .

public section.
*"* public components of class ZCL_IM_INVOICE_UPDATE
*"* do not include other source files here!!!

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_INVOICE_UPDATE .
protected section.
*"* protected components of class ZCL_IM_INVOICE_UPDATE
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_IM_INVOICE_UPDATE
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_IM_INVOICE_UPDATE IMPLEMENTATION.


method IF_EX_INVOICE_UPDATE~CHANGE_AT_SAVE.
*  break sahmad.
*  MESSAGE e023(zmm_message) WITH '&' '&' '&' '&'.
*   & & & &
TYPES : BEGIN OF ty_ebeln,
           ebeln TYPE nast-objky,
        END OF ty_ebeln.
DATA : lt_drseg TYPE mrm_tab_mrmrseg,
       ls_rseg_new TYPE LINE OF mrm_tab_mrmrseg,
       lt_ebeln   TYPE STANDARD TABLE OF ty_ebeln,
       ls_ebeln  TYPE ty_ebeln,
       lt_nast TYPE STANDARD TABLE OF nast,
       ls_nast TYPE nast,
       lt_lfa1 TYPE STANDARD TABLE OF lfa1,
       ls_lfa1 TYPE lfa1,
       ls_ekko TYPE ekko,
       lv_ariba TYPE c,
       lv_memid TYPE c LENGTH 17,
       lv_ariba_po TYPE  c LENGTH 10.
.
*       lv_test type char35,
*       lv_ucomm type sy-ucomm.

  CONSTANTS:  c_zari(4)   TYPE c VALUE 'ZARI'.

  break sahmad.

  CONCATENATE sy-uname 'ARIBA' INTO lv_memid.

  IF sy-batch IS INITIAL AND
     ( s_rbkp_new-blart <> 'ZR'  AND
       s_rbkp_new-blart <> 'ZN' ).
    IF sy-tcode <> 'MIR4' AND
       sy-tcode <> 'MRRL' AND
       sy-tcode <> 'MR8M'.
     IF ti_rseg_new[] IS NOT INITIAL.
        LOOP AT ti_rseg_new INTO ls_rseg_new.
          CLEAR: ls_ebeln.
          ls_ebeln-ebeln = ls_rseg_new-ebeln.
          APPEND ls_ebeln TO lt_ebeln.
        ENDLOOP.
        SORT lt_ebeln BY ebeln.
        DELETE ADJACENT DUPLICATES FROM lt_ebeln COMPARING ebeln.
        DELETE lt_ebeln WHERE ebeln EQ space.
*   Get output type and vendor from NAST table
        IF lt_ebeln IS NOT INITIAL.
          SELECT * FROM nast INTO TABLE lt_nast
                   FOR ALL ENTRIES IN lt_ebeln
                   WHERE objky = lt_ebeln-ebeln
                     AND kschl = c_zari
                     AND spras = sy-langu.
          IF sy-subrc = 0.
            SORT lt_nast BY objky.
            DELETE ADJACENT DUPLICATES FROM lt_nast COMPARING objky.
          ENDIF.
*     Get Ariba number from LFA1 table
          SELECT * FROM lfa1 INTO TABLE lt_lfa1
                   FOR ALL ENTRIES IN lt_nast
                   WHERE lifnr = lt_nast-parnr.
          LOOP AT lt_ebeln INTO ls_ebeln.
            READ TABLE lt_nast INTO ls_nast WITH KEY objky = ls_ebeln-ebeln.
            check sy-subrc = 0.
            READ TABLE lt_lfa1 INTO ls_lfa1 WITH KEY lifnr = ls_nast-parnr.
            IF sy-subrc = 0 AND ls_lfa1-emnfr IS NOT INITIAL.
*               CLEAR: ls_ekko.
*               SELECT SINGLE * FROM ekko INTO ls_ekko
*                         WHERE ebeln = ls_ebeln-ebeln.
*               IF ls_ekko-bsart <> 'ZF'.
*                  lv_ariba = 'A'.
*               ENDIF.
              lv_ariba = 'A'.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.
     IF s_rbkp_new-blart <> 'RF' AND
        lv_ariba = 'A'.
        MESSAGE e023(zmm_message) WITH 'Document Type RF is allowed for Ariba PO,' 'Change from Details Tab' '' ''.
     ENDIF.
     IF s_rbkp_new-blart = 'RF' AND
        lv_ariba <> 'A'.
        MESSAGE e023(zmm_message) WITH 'Document Type RF is not allowed for non-Ariba PO,' 'Change from Details Tab' '' ''.
     ENDIF.
    ENDIF. "MIR4
  ENDIF.  "background
*Initialize ABAP memory relevant to Ariba PO
*lv_Ariba_po is blank
*      EXPORT lv_ariba_po FROM lv_ariba_po TO MEMORY ID lv_memid.
  FREE MEMORY ID lv_memid.

endmethod.


method IF_EX_INVOICE_UPDATE~CHANGE_BEFORE_UPDATE.

endmethod.


method IF_EX_INVOICE_UPDATE~CHANGE_IN_UPDATE.
endmethod.
ENDCLASS.
