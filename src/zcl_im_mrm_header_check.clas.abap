class ZCL_IM_MRM_HEADER_CHECK definition
  public
  final
  create public .

public section.
*"* public components of class ZCL_IM_MRM_HEADER_CHECK
*"* do not include other source files here!!!

  interfaces IF_EX_MRM_HEADER_CHECK .
protected section.
*"* protected components of class ZCL_IM_MRM_HEADER_CHECK
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_IM_MRM_HEADER_CHECK
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_IM_MRM_HEADER_CHECK IMPLEMENTATION.


METHOD if_ex_mrm_header_check~headerdata_check.

  DATA: gt_errtab    TYPE TABLE OF mrm_errprot,
          gs_errtab    TYPE mrm_errprot.
  CONSTANTS:     c_errprot(23)   TYPE c VALUE '(SAPLMRMF)TAB_ERRPROT[]'.

  FIELD-SYMBOLS: <fs_errprot> TYPE table.
  ASSIGN (c_errprot) TO <fs_errprot>.

  REFRESH gt_errtab[].
  gt_errtab[] = <fs_errprot>[].


  DATA: git_bkpf      TYPE STANDARD TABLE OF bkpf,
        git_bseg      TYPE STANDARD TABLE OF bseg,
        git_vbsegk    TYPE STANDARD TABLE OF vbsegk.

  DATA: lv_belnr      TYPE belnr_d,
        lv_awkey      TYPE awkey.

  CONSTANTS:  lc_awtyp_ir TYPE awtyp VALUE 'RMRP'. "Inv Receipt/MIRO"

  CONSTANTS:  co_h(1)   TYPE c VALUE 'H'.

*BOC by PANUSURI Ticket 56433
**BOI by PANUSURI Ticket 56433
  TYPES : BEGIN OF ty_ebeln,
           ebeln TYPE nast-objky,
          END OF ty_ebeln,
          BEGIN OF ty_nast,
           objky TYPE nast-objky,
           kschl TYPE sna_kschl,
           parnr TYPE na_parnr,
          END OF ty_nast,
          BEGIN OF ty_lfa1,
           lifnr TYPE lifnr,
           emnfr TYPE emnfr,
          END OF ty_lfa1.

  DATA : lt_errtab TYPE TABLE OF mrm_errprot,
         lt_drseg TYPE mmcr_tdrseg,
         lwa_drseg TYPE mmcr_drseg,
         lt_ebeln   TYPE STANDARD TABLE OF ty_ebeln,
         lwa_ebeln  TYPE ty_ebeln,
         lt_nast TYPE STANDARD TABLE OF ty_nast,
         lwa_nast TYPE ty_nast,
         lt_lfa1 TYPE STANDARD TABLE OF ty_lfa1,
         lwa_lfa1 TYPE ty_lfa1,
         ls_ekko TYPE ekko,
         lv_flag TYPE c,
         lv_ariba TYPE c,
         lv_memid TYPE c LENGTH 17,
         lv_space TYPE c LENGTH 17,
         lv_ariba_po TYPE  c LENGTH 10. " VALUE 'ARIBA_PO'.

  CONSTANTS:  c_zari(4)   TYPE c VALUE 'ZARI'.

  CONCATENATE sy-uname 'ARIBA' INTO lv_memid.

*  EXPORT lv_space FROM lv_space TO MEMORY ID lv_memid.
  EXPORT lv_ariba_po FROM lv_ariba_po TO MEMORY ID lv_memid.

  IF sy-batch IS INITIAL.
    break sahmad.

    IF sy-tcode <> 'MIR4'.

* First clear the message because the user might have corrected
* the situation that causes the error
      LOOP AT gt_errtab INTO gs_errtab
      WHERE msgty = 'W'
      AND msgid = 'ZFI01'
      AND msgno = '059'.
        DELETE gt_errtab INDEX sy-tabix.
      ENDLOOP.

      <fs_errprot>[] = gt_errtab[].

* Begin changes - insert code   JRHARTUNG  09/30/13  D30K922480

      CLEAR                                lv_awkey.
      MOVE     i_rbkpv-belnr+00(10)     TO lv_awkey+00(10).
      MOVE     i_rbkpv-gjahr+00(04)     TO lv_awkey+10(04).

      CLEAR    lv_belnr.
      SELECT   SINGLE belnr
        INTO   lv_belnr
        FROM   bkpf
       WHERE   awtyp = lc_awtyp_ir
         AND   awkey = lv_awkey.

* End changes   - insert code   JRHARTUNG  09/30/13  D30K922480

      SELECT * FROM bkpf
        INTO TABLE git_bkpf
        WHERE xblnr = i_rbkpv-xblnr
          AND belnr <> lv_belnr.                            "D30K922480
*     AND belnr <> i_rbkpv-belnr.                   "D30K922480

      IF git_bkpf[] IS NOT INITIAL.
        SELECT * FROM bseg
          INTO TABLE git_bseg
          FOR ALL ENTRIES IN git_bkpf
          WHERE bukrs = git_bkpf-bukrs
            AND belnr = git_bkpf-belnr
            AND gjahr = git_bkpf-gjahr
            AND wrbtr = i_rbkpv-rmwwr.
* if the condition is not met, trigger the message
        IF git_bseg[] IS NOT INITIAL.

          gs_errtab-msgty = 'W'.
          gs_errtab-msgid = 'ZFI01'.
          gs_errtab-msgno = '059'.
          gs_errtab-msgv1 = i_rbkpv-xblnr.
          gs_errtab-msgv2 = i_rbkpv-rmwwr.
          APPEND gs_errtab TO gt_errtab.

          CALL FUNCTION 'MRM_PROT_FILL'
            TABLES
              t_errprot = gt_errtab.
        ELSE.
          "Check vbsegk.
          SELECT * FROM vbsegk
            INTO TABLE git_vbsegk
            FOR ALL ENTRIES IN git_bkpf
            WHERE bukrs = git_bkpf-bukrs
              AND belnr = git_bkpf-belnr
              AND gjahr = git_bkpf-gjahr
              AND wrbtr = i_rbkpv-rmwwr.

          IF git_vbsegk[] IS NOT INITIAL.
            gs_errtab-msgty = 'W'.
            gs_errtab-msgid = 'ZFI01'.
            gs_errtab-msgno = '059'.
            gs_errtab-msgv1 = i_rbkpv-xblnr.
            gs_errtab-msgv2 = i_rbkpv-rmwwr.
            APPEND gs_errtab TO gt_errtab.

            CALL FUNCTION 'MRM_PROT_FILL'
              TABLES
                t_errprot = gt_errtab.
          ENDIF.
        ENDIF.
      ENDIF.

      gt_errtab[] = <fs_errprot>[].
      IF ti_drseg[] IS NOT INITIAL.
        LOOP AT ti_drseg INTO lwa_drseg.
          CLEAR: lwa_ebeln.
          lwa_ebeln-ebeln = lwa_drseg-ebeln.
          APPEND lwa_ebeln TO lt_ebeln.
        ENDLOOP.
        SORT lt_ebeln BY ebeln.
        DELETE ADJACENT DUPLICATES FROM lt_ebeln COMPARING ebeln.
        DELETE lt_ebeln WHERE ebeln EQ space.
*   Get output type and vendor from NAST table
        IF lt_ebeln IS NOT INITIAL.
          SELECT objky
                 kschl
                 parnr
                 FROM nast
                 INTO TABLE lt_nast
                 FOR ALL ENTRIES IN lt_ebeln
                 WHERE objky = lt_ebeln-ebeln
                 AND kschl = c_zari
                 AND spras = sy-langu.
          IF sy-subrc = 0.
            SORT lt_nast BY objky.
            DELETE ADJACENT DUPLICATES FROM lt_nast COMPARING objky.
          ENDIF.
*     Get Ariba number from LFA1 table
          SELECT lifnr
                 emnfr
                 FROM lfa1
                 INTO TABLE lt_lfa1
                 FOR ALL ENTRIES IN lt_nast
                 WHERE lifnr = lt_nast-parnr.

          LOOP AT lt_ebeln INTO lwa_ebeln.
            READ TABLE lt_nast INTO lwa_nast WITH KEY objky = lwa_ebeln-ebeln.
            check sy-subrc = 0.
              READ TABLE lt_lfa1 INTO lwa_lfa1 WITH KEY lifnr = lwa_nast-parnr.
              IF sy-subrc = 0 AND lwa_lfa1-emnfr IS NOT INITIAL.
                CLEAR: gs_errtab,
                       ls_ekko.
                gs_errtab-msgty = 'W'.
                gs_errtab-msgid = 'ZMM_MESSAGE'.
                gs_errtab-msgno =  '021'.
                gs_errtab-msgv1 = lwa_ebeln-ebeln.
                gs_errtab-msgv2 = 'ARIBA MESSAGE'.
                gs_errtab-source = abap_true.
*Save in memory id.  It is checked in program LFDCBFC0 's implicit enhancement
*for determination of Document Type
*                SELECT SINGLE * FROM ekko INTO ls_ekko
*                  WHERE ebeln = lwa_ebeln-ebeln.
*                IF ls_ekko-bsart = 'ZF'.
*                  gs_errtab-msgv3 = 'SERVICE_PO'.
*                  clear lv_ariba.
*                else.
*                  EXPORT lv_ariba_po FROM lv_ariba_po TO MEMORY ID lv_memid.
*                  lv_ariba = 'A'.
*                ENDIF.
                lv_ariba_po = 'ARIBA_PO'.
                EXPORT lv_ariba_po FROM lv_ariba_po TO MEMORY ID lv_memid.
                lv_ariba = 'A'.
                APPEND gs_errtab TO lt_errtab.
*                CLEAR lv_ariba_po.
                READ TABLE gt_errtab TRANSPORTING NO FIELDS WITH KEY msgty = 'W'
                                                                      msgid = 'ZMM_MESSAGE'
                                                                      msgno =  '021'
                                                                      msgv1 = lwa_ebeln-ebeln.
                IF sy-subrc NE 0.
*             For displaying popup message only once
                  lv_flag = 'X'.
                ENDIF.
              ENDIF.
          ENDLOOP.

          DELETE gt_errtab[] WHERE msgty = 'W'
                               AND msgid = 'ZMM_MESSAGE'
                               AND msgno = '021'.
          <fs_errprot>[] = gt_errtab[].

          IF lt_errtab[] IS NOT INITIAL.
            APPEND LINES OF lt_errtab TO gt_errtab.
          ENDIF.

          CALL FUNCTION 'MRM_PROT_FILL'
            TABLES
              t_errprot = gt_errtab.

*     Display popup message
          IF lv_flag = 'X'.

            CALL FUNCTION 'POPUP_TO_INFORM'
              EXPORTING
                titel = 'Warning'(001)
                txt1  = '@1A@'(003)
                txt2  = 'This is an ARIBA Purchase Order'(002)
*               TXT3  = ' '
*               TXT4  = ' '
              .
          ENDIF.

          CLEAR : lwa_ebeln,
                  lwa_nast,
                  lwa_lfa1,
                  lv_flag.
          REFRESH : lt_ebeln,
                    lt_nast,
                    lt_lfa1,
                    lt_errtab.
        ENDIF.
      ENDIF.

    ENDIF.
  ENDIF.  "background

ENDMETHOD.
ENDCLASS.
