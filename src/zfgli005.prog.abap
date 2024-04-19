REPORT  zfgli005 MESSAGE-ID zs.

*----------------------------------------------------------------------*
*  Author    : Glenn Ymana                          SAP : East
*  Date      : March, 2007                 Program Type : Interface
*  Issue Log : TR153
*----------------------------------------------------------------------*
*  Title : Mercator Replacement - IFFI051 Banner G/L Interface
*----------------------------------------------------------------------*
*  Part 2 for XLATBANN processes:
*            : this is the main program which will take the input
*            : records coming from ZFGLI004
*----------------------------------------------------------------------*
*  Description:
*     - ABAP replacement for Mercator maps that take an input file
*       from ZFGLI004 and creates multiple ZBSEG-format BDC
*       sessions: (Gas, Rental, Other, Error).
*----------------------------------------------------------------------
* Changes:
* 2011/03/29 mkhan    - TR804 - Program changes for COG Project
* 2010/10/21 btboundy - TR841 - Added new WBS Code for Customer Signal.
* 2007/08/22 gymana - TR392 - Added new Rate Class field in input file
*                     and removed all ZFB02 rate class references
* 2008/04/17 gymana - TR351 - Production fix. Removed line that was
*                     checking for a numeric service class code. Codes
*                     are now alphanumeric.
* 2008/08/21 gymana - TR611 - Production fix. Posting Key in offset
*                     error line item was not being set correctly.
*                     The offset key check was different from the
*                     original check.
* 2019/08/05 ahmadt - D30K930081 - CHG0155278
*                     Form Initialize Data was modified such that the
*                     program now reads General Ledger Classification
*                     codes of CLC* and CLF*
*----------------------------------------------------------------------*
TABLES:  dd03l,
*        zfb01,                                     "tr804
         zbacct,                                    "tr804
         zfb02,
         zfb03,
         zfb04,
         t247.

DATA: BEGIN OF zbseg1,
        stype     LIKE zbseg-stype,    tbnam     LIKE zbseg-tbnam,
        newbs     LIKE zbseg-newbs,    newum     LIKE zbseg-newum,
        newbk     LIKE zbseg-newbk,    wrbtr     LIKE zbseg-wrbtr,
        wmwst     LIKE zbseg-wmwst,    mwskz     LIKE zbseg-mwskz,
        xskrl     LIKE zbseg-xskrl,    kostl     LIKE zbseg-kostl,
        aufnr     LIKE zbseg-aufnr,    matnr     LIKE zbseg-matnr,
        werks(04) TYPE c          ,    menge     LIKE zbseg-menge,
        meins     LIKE zbseg-meins,    valut     LIKE zbseg-valut,
        zfbdt     LIKE zbseg-zfbdt,    zuonr     LIKE zbseg-zuonr,
        sgtxt     LIKE zbseg-sgtxt,    skfbt     LIKE zbseg-skfbt,
        wskto     LIKE zbseg-wskto,    zterm     LIKE zbseg-zterm,
        zbd1t     LIKE zbseg-zbd1t,    zbd1p     LIKE zbseg-zbd1p,
        zbd2t     LIKE zbseg-zbd2t,    zbd2p     LIKE zbseg-zbd2p,
        zbd3t     LIKE zbseg-zbd3t,    zlspr     LIKE zbseg-zlspr,
        zlsch     LIKE zbseg-zlsch,    zbfix     LIKE zbseg-zbfix,
        qsskz     LIKE zbseg-qsskz,    qsshb     LIKE zbseg-qsshb,
        qsfbt     LIKE zbseg-qsfbt,    regul     LIKE zbseg-regul,
        name1     LIKE zbseg-name1,    name2     LIKE zbseg-name2,
        name3     LIKE zbseg-name3,    name4     LIKE zbseg-name4,
        stras     LIKE zbseg-stras,    ort01     LIKE zbseg-ort01,
        pstlz     LIKE zbseg-pstlz,    land1     LIKE zbseg-land1,
        regio     LIKE zbseg-regio,    stcd1     LIKE zbseg-stcd1,
        stcd2     LIKE zbseg-stcd2,    pfach     LIKE zbseg-pfach,
        pstl2     LIKE zbseg-pstl2,    spras     LIKE zbseg-spras,
        hkont     LIKE zbseg-hkont,    fwbas     LIKE zbseg-fwbas,
        projk     LIKE zbseg-projk,    uzawe     LIKE zbseg-uzawe,
        sende     LIKE zbseg-sende,    kndnr(10) TYPE c          ,
        vkorg(04) TYPE c          ,    wwbrn(04) TYPE c          ,
        wwsct(01) TYPE c          ,    wwseg(02) TYPE c          ,
        wwrat(03) TYPE c          ,    wwser(02) TYPE c          ,
        artnr(18) TYPE c          ,    prdha(18) TYPE c          ,
        wwsub(02) TYPE c          ,    wwprg(02) TYPE c          ,
        wwlob(04) TYPE c          ,    wwreg(02) TYPE c          ,
        wwdvs(02) TYPE c          ,    wwrgn(04) TYPE c.
DATA: END OF zbseg1.

DATA: BEGIN OF wa OCCURS 0,
        process_date        TYPE d,
        serv_type(4)        TYPE c,
        serv_cat(4)         TYPE c,
        gl_classcd(4)       TYPE c,
        serv_class(2)       TYPE c,
        town_cd(2)          TYPE c,
        munic_cd(4)         TYPE c,
        budget_ind(1)       TYPE c,
        cust_num(16)        TYPE n,
        trans_amt(14)       TYPE c,
        budget_amt(12)      TYPE c,
        cons(14)            TYPE c,
        no_of_custs(6)      TYPE n,
        eff_date            TYPE d,
        rate_class(4)       TYPE c.
DATA: END OF wa.

DATA: BEGIN OF wd OCCURS 0,
        seqno    LIKE zbseg-zterm,
        pdate(8) TYPE c.
DATA: END OF wd.

DATA: BEGIN OF isort OCCURS 0,
*-      sortkey group
        izterm     LIKE zbseg-zterm,
        iservtype(04) TYPE c,
        ikostl     LIKE zbseg-kostl,
        ihkont     LIKE zbseg-hkont,
        inewbs     LIKE zbseg-newbs,
        imatnr     LIKE zbseg-matnr,
        iaufnr     LIKE zbseg-aufnr,
        isgtxt     LIKE zbseg-sgtxt,
        iwerks(04) TYPE c,
        iprojk     LIKE zbseg-projk,
        ikndnr(10) TYPE c,
        ivkorg(04) TYPE c,
        iwwbrn(04) TYPE c,
        iwwrat(03) TYPE c,
        iwwser(02) TYPE c,
        iwwsct(01) TYPE c,
        iwwseg(02) TYPE c.
*-      sortkey group end here
        INCLUDE STRUCTURE zbseg1.
DATA: END OF isort.
DATA: isum LIKE isort OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF z_bgr00.
        INCLUDE STRUCTURE bgr00.
DATA: END OF z_bgr00.

DATA: BEGIN OF z_bbkpf.
        INCLUDE STRUCTURE bbkpf.
DATA: END OF z_bbkpf.

DATA: wa_seqno       TYPE i,
      wa_zbd1p       TYPE i,
      wa_zbd1p_num   TYPE p DECIMALS 5 VALUE 0,
      wa_docsize     TYPE i,
      wa_zbacct_found TYPE c VALUE 'N',
      wa_zfb02_found TYPE c VALUE 'N',
      wa_zfb03_found TYPE c VALUE 'N',
      wa_zfb04_found TYPE c VALUE 'N',
      wa_wrbtr       TYPE p DECIMALS 2,
      wa_amount      TYPE p DECIMALS 2,
      wa_quantity    TYPE p DECIMALS 3,
      char(21)       TYPE c,
      nodata(1)      VALUE '/',
      wa_err_hdr_sw  TYPE c VALUE 'Y',
      switch         TYPE i VALUE 0.

DATA: va_gstpct   TYPE p DECIMALS 2.
DATA: va_amt      TYPE p DECIMALS 2.
DATA: va_gst      TYPE p DECIMALS 2.

RANGES: ra_glclass FOR wa-gl_classcd,
        ra_compuse FOR wa-gl_classcd.
DATA: delimeter VALUE '09' TYPE x.
DATA: inrec(400), outrec(2000), infile(70), outfile(70).
DATA: wrk_symbolic(4) TYPE c VALUE '$sys'.

FIELD-SYMBOLS: <f1>.

*---------------------------------------------------------------------*
* selection screen
*---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK box0 WITH FRAME.
SELECTION-SCREEN BEGIN OF BLOCK box1 WITH FRAME TITLE text-100.

PARAMETERS: p_file1 LIKE filename-fileextern OBLIGATORY DEFAULT
              '/usr/sap/interfaces/$sys/BANNER/zbis100.tmp'.

SELECTION-SCREEN SKIP.
PARAMETERS: p_gasout LIKE filename-fileextern OBLIGATORY DEFAULT
              '/usr/sap/interfaces/$sys/BANNER/zbis100_gas.sap',

            p_othout LIKE filename-fileextern OBLIGATORY DEFAULT
              '/usr/sap/interfaces/$sys/BANNER/zbis100_oth.sap',

            p_rntout LIKE filename-fileextern OBLIGATORY DEFAULT
              '/usr/sap/interfaces/$sys/BANNER/zbis100_rnt.sap',

            p_errout LIKE filename-fileextern OBLIGATORY DEFAULT
              '/usr/sap/interfaces/$sys/BANNER/zbis100_err.sap'.

SELECTION-SCREEN SKIP.
PARAMETERS: p_tcode  LIKE tstc-tcode  OBLIGATORY DEFAULT 'FB01',
            p_bukrs  LIKE bbkpf-bukrs OBLIGATORY DEFAULT 'UGL',
            p_waers  LIKE bbkpf-waers OBLIGATORY DEFAULT 'CAD',
            p_xblnr  LIKE bbkpf-xblnr OBLIGATORY DEFAULT 'BANNER',
            p_gspct(1) TYPE c OBLIGATORY DEFAULT '6',
            p_dsize(4) TYPE c OBLIGATORY DEFAULT '5000'.

SELECTION-SCREEN END OF BLOCK box1.
SELECTION-SCREEN END OF BLOCK box0.

*----------------------------------------------------------------------
* AT SELECTION-SCREEN
*----------------------------------------------------------------------
AT SELECTION-SCREEN OUTPUT.
  REPLACE wrk_symbolic WITH sy-sysid INTO:
     p_file1, p_gasout, p_othout, p_rntout, p_errout.
  CONDENSE: p_file1 NO-GAPS,  p_gasout NO-GAPS, p_othout NO-GAPS,
            p_rntout NO-GAPS, p_errout NO-GAPS.

*---------------------------------------------------------------------*
* start-of-selection
*---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM initialize_data.
  PERFORM open_files.
  PERFORM f_transfer_records.
  PERFORM f_xlatbann.
  PERFORM f_summarize_data.

* Split line items into Gas, Rentals, Others and create a separate
* session for each.

  PERFORM f_create_output_files.

  CLOSE DATASET: p_file1, p_gasout, p_othout, p_rntout, p_errout.

*-----------------------------------------------------------------------
*  Routine to open the physical file to determine if there are any
*  errors reading it.
*-----------------------------------------------------------------------
FORM open_files.
  DATA: msg(100).
*-----------------------------------------------------------------------
  OPEN DATASET p_file1 FOR INPUT IN TEXT MODE MESSAGE msg.
  IF ( sy-subrc <> 0 ).
    MESSAGE e002 WITH infile msg.
  ENDIF.
*-----------------------------------------------------------------------
  OPEN DATASET p_gasout FOR OUTPUT IN TEXT MODE MESSAGE msg.
  IF ( sy-subrc <> 0 ).
    MESSAGE e002 WITH outfile msg.
  ENDIF.
*-----------------------------------------------------------------------
  OPEN DATASET p_othout FOR OUTPUT IN TEXT MODE MESSAGE msg.
  IF ( sy-subrc <> 0 ).
    MESSAGE e002 WITH outfile msg.
  ENDIF.
*-----------------------------------------------------------------------
  OPEN DATASET p_rntout FOR OUTPUT IN TEXT MODE MESSAGE msg.
  IF ( sy-subrc <> 0 ).
    MESSAGE e002 WITH outfile msg.
  ENDIF.
*-----------------------------------------------------------------------
  OPEN DATASET p_errout FOR OUTPUT IN TEXT MODE MESSAGE msg.
  IF ( sy-subrc <> 0 ).
    MESSAGE e002 WITH outfile msg.
  ENDIF.
ENDFORM.                    "OPEN_FILES

*-----------------------------------------------------------------------
*  This routine reads all the records from the input area, and adds
*  them, one-by-one, to the internal work table (wa), separating
*  the record into its various fields and save to internal table itab.
*-----------------------------------------------------------------------
FORM f_transfer_records.
  REFRESH wa.
  DO.
    CLEAR: wa, inrec.
    READ DATASET p_file1 INTO inrec.
    IF ( inrec IS INITIAL OR sy-subrc <> 0 ).
      EXIT.
    ELSE.
      MOVE inrec TO wa. APPEND wa.
    ENDIF.
  ENDDO.
ENDFORM.                    "F_TRANSFER_RECORDS

*---------------------------------------------------------------------*
* This the main translation program which will take the input records
* and format them into a ZBSEG format. It is divided into two maps:
* F_CHECK_SPECIAL_PROC & F_ERROR_PROC.
*---------------------------------------------------------------------*
FORM f_xlatbann.



  LOOP AT wa.
    SELECT SINGLE * FROM zbacct WHERE glcode  = wa-gl_classcd AND
                                     ( b_srvcl = wa-serv_class OR
                                       b_srvcl = '**' ).
    IF sy-subrc = 0.
      PERFORM f_check_special_proc.
    ELSE.
      MOVE wa-trans_amt TO wa_amount.
      MOVE wa-cons      TO wa_quantity.
      IF NOT wa_amount IS INITIAL OR
         NOT wa_quantity IS INITIAL.
        PERFORM f_error_proc.
      ENDIF.
    ENDIF.
    MOVE '0.00' TO wa_amount.
    MOVE '0.000' TO wa_quantity.
  ENDLOOP.

ENDFORM.                    "F_XLATBANN

*---------------------------------------------------------------------*
* This routine will look at the records with valid GL class codes and
* Service class codes and see if a special process indicator has been
* set which will determine if there is any special mapping to be done.
*---------------------------------------------------------------------*
FORM f_check_special_proc.
  PERFORM init_isort_record.

  IF zbacct-special IS INITIAL.
    PERFORM f_zbseg.
  ELSE.
    IF zbacct-special = 'GST'.
      va_gstpct = ( p_gspct / 100 ).    "GST %
      va_gst = 1 + va_gstpct.                               "1 + GST
      PERFORM f_zbseg_gst1.
      PERFORM f_zbseg_gst2.
    ELSE.
      PERFORM f_zbseg.
    ENDIF.
  ENDIF.

ENDFORM.                    "F_CHECK_SPECIAL_PROC
*----------------------------------------------------------------------*
* Build ZBSEG line item record & Offset line item record
*----------------------------------------------------------------------*
FORM f_zbseg.

*build sortkey group
  PERFORM build_sortkey_group.

*build zbseg
  MOVE '2' TO isort-stype.
  MOVE 'ZBSEG' TO isort-tbnam.
  MOVE isort-inewbs TO isort-newbs.
  wa_wrbtr = ABS( wa-trans_amt ).
  MOVE wa_wrbtr TO isort-wrbtr.
  SHIFT isort-wrbtr RIGHT DELETING TRAILING space.

  IF NOT zbacct-mwskz IS INITIAL.
    MOVE '0.00' TO isort-wmwst.
    SHIFT isort-wmwst RIGHT DELETING TRAILING space.
    MOVE zbacct-mwskz TO isort-mwskz.
  ENDIF.

  MOVE isort-ikostl TO isort-kostl.
  MOVE isort-iaufnr TO isort-aufnr.
  MOVE isort-imatnr TO isort-matnr.
  MOVE isort-iwerks TO isort-werks.

*get MENGE, MEINS
  MOVE wa-cons TO wa_quantity.
  IF wa-serv_cat+0(3) = 'GAS' AND
     ( isort-ihkont+4(1) = '3' OR wa-gl_classcd IN ra_compuse ).
    IF NOT wa_quantity = '0.000'.
      isort-menge = wa-cons.
      TRANSLATE isort-menge USING '- '.
      SHIFT isort-menge RIGHT DELETING TRAILING space.
      isort-meins = 'M3'.
    ENDIF.
  ENDIF.

  MOVE isort-isgtxt TO isort-sgtxt.
  MOVE isort-izterm TO isort-zterm.
  MOVE isort-ihkont TO isort-hkont.
  MOVE isort-iprojk TO isort-projk.
  MOVE isort-ikndnr TO isort-kndnr.
  MOVE isort-ivkorg TO isort-vkorg.
  MOVE isort-iwwbrn TO isort-wwbrn.
  MOVE isort-iwwsct TO isort-wwsct.
  MOVE isort-iwwseg TO isort-wwseg.
  MOVE isort-iwwrat TO isort-wwrat.
  MOVE isort-iwwser TO isort-wwser.
  MOVE isort-imatnr TO isort-artnr.

  APPEND isort.

*build offset sortkey group
  PERFORM build_offset_sortkey_group.

*build offset zbseg

  MOVE '2'          TO isort-stype.
  MOVE 'ZBSEG'      TO isort-tbnam.
  MOVE isort-inewbs TO isort-newbs.
  wa_wrbtr = ABS( wa-trans_amt ).
  MOVE wa_wrbtr TO isort-wrbtr.
  SHIFT isort-wrbtr RIGHT DELETING TRAILING space.
  MOVE '/'          TO isort-wmwst.

  IF NOT zbacct-omwskz IS INITIAL.
    MOVE zbacct-omwskz TO isort-mwskz.
  ELSE.
    MOVE '/' TO isort-mwskz.
  ENDIF.

  MOVE isort-ikostl TO isort-kostl.
  MOVE isort-iaufnr TO isort-aufnr.
  MOVE isort-imatnr TO isort-matnr.
  MOVE isort-iwerks TO isort-werks.

*get offset MENGE, MEINS
  isort-menge = '/'.
  isort-meins = '/'.
  MOVE wa-cons TO wa_quantity.
  IF wa-gl_classcd IN ra_compuse.
    IF NOT wa_quantity = '0.000'.
      isort-menge = wa-cons.
      TRANSLATE isort-menge USING '- '.
      SHIFT isort-menge RIGHT DELETING TRAILING space.
      isort-meins = 'M3'.
    ENDIF.
  ENDIF.

*get offset SGTXT
  IF wa-gl_classcd IN ra_compuse.
    CONCATENATE 'Banner Interface - Gas' wa-gl_classcd
                wa-cust_num+0(9) '-' wa-cust_num+9(7)
                INTO isort-sgtxt SEPARATED BY space.
  ELSE.
    IF wa-gl_classcd = 'REMR'.
      CONCATENATE 'Banner Interface - ' wa-gl_classcd
                  INTO isort-sgtxt.
    ELSE.
      MOVE '/' TO isort-sgtxt.
    ENDIF.
  ENDIF.

*get the rest of the fields from the offset sortkey group
  MOVE isort-izterm TO isort-zterm.
  MOVE isort-ihkont TO isort-hkont.
  MOVE isort-iprojk TO isort-projk.
  MOVE isort-ikndnr TO isort-kndnr.
  MOVE isort-ivkorg TO isort-vkorg.
  MOVE isort-iwwsct TO isort-wwsct.
  MOVE isort-iwwseg TO isort-wwseg.
  MOVE isort-iwwrat TO isort-wwrat.
  MOVE isort-iwwser TO isort-wwser.
  MOVE isort-imatnr TO isort-artnr.

  APPEND isort.
  CLEAR isort.

ENDFORM.                    "F_ZBSEG
*----------------------------------------------------------------------*
* Build GST1 ZBSEG line item record & Offset line item record
*----------------------------------------------------------------------*
FORM f_zbseg_gst1.

*build generic sortkey group
  PERFORM build_sortkey_group.

* Replace the generic code from the sortkey routine
* with the code below

* get SGTXT
  MOVE '/' TO isort-isgtxt.

* get WWRAT
  IF zbacct-matnr IS INITIAL.
    IF wa-serv_type+0(3) = 'GAS' OR wa-serv_type+0(3) = 'CHR' OR
       wa-serv_type+0(3) = 'GLI'.
      MOVE wa-rate_class TO isort-iwwrat.
*      IF WA_ZFB02_FOUND = 'Y'.
*        MOVE ZFB02-RATECL TO ISORT-IWWRAT.
*      ELSE.
*        MOVE WA-GL_CLASSCD+1(3) TO ISORT-IWWRAT.
*      ENDIF.
    ENDIF.
  ENDIF.

* get WWSER
  IF zbacct-matnr IS INITIAL.
    IF wa-serv_type+0(3) = 'GAS' OR wa-serv_type+0(3) = 'CHR' OR
       wa-serv_type+0(3) = 'GLI'.
      IF wa_zfb02_found = 'Y'.
        MOVE zfb02-servcl TO isort-iwwser.
      ELSE.
        MOVE wa-serv_class TO isort-iwwser.
      ENDIF.
    ENDIF.
  ENDIF.

*----------------------------------------------------------------------*
*build GST1 zbseg
*----------------------------------------------------------------------*
  MOVE '2' TO isort-stype.
  MOVE 'ZBSEG' TO isort-tbnam.
  MOVE isort-inewbs TO isort-newbs.

* get WRBTR (Transaction Amt + GST)
  wa_wrbtr = ABS( wa-trans_amt * va_gst ).
  MOVE wa_wrbtr TO isort-wrbtr.
  SHIFT isort-wrbtr RIGHT DELETING TRAILING space.

  IF NOT zbacct-mwskz IS INITIAL.
    MOVE '0.00' TO isort-wmwst.
    SHIFT isort-wmwst RIGHT DELETING TRAILING space.
    MOVE zbacct-mwskz TO isort-mwskz.
  ENDIF.

  MOVE isort-ikostl TO isort-kostl.
  MOVE isort-iaufnr TO isort-aufnr.
  MOVE isort-imatnr TO isort-matnr.
  MOVE isort-iwerks TO isort-werks.

*get MENGE, MEINS
  MOVE wa-cons TO wa_quantity.
  IF wa-serv_cat+0(3) = 'GAS' AND
     ( isort-ihkont+4(1) = '3' OR wa-gl_classcd IN ra_compuse ).
    IF NOT wa_quantity = '0.000'.
      isort-menge = wa-cons.
      TRANSLATE isort-menge USING '- '.
      SHIFT isort-menge RIGHT DELETING TRAILING space.
      isort-meins = 'M3'.
    ENDIF.
  ENDIF.

  MOVE isort-isgtxt TO isort-sgtxt.
  MOVE isort-izterm TO isort-zterm.
  MOVE isort-ihkont TO isort-hkont.
  MOVE isort-iprojk TO isort-projk.
  MOVE isort-ikndnr TO isort-kndnr.
  MOVE isort-ivkorg TO isort-vkorg.
  MOVE isort-iwwbrn TO isort-wwbrn.
  MOVE isort-iwwsct TO isort-wwsct.
  MOVE isort-iwwseg TO isort-wwseg.
  MOVE isort-iwwrat TO isort-wwrat.
  MOVE isort-iwwser TO isort-wwser.
  MOVE isort-imatnr TO isort-artnr.

  APPEND isort.

*build offset sortkey group
  PERFORM build_offset_sortkey_group.

* Replace the generic code from the offset sortkey routine
* with the code below

* get SGTXT
  MOVE '/' TO isort-isgtxt.

*----------------------------------------------------------------------*
*Build Offset GST1 ZBSEG
*----------------------------------------------------------------------*

  MOVE '2'          TO isort-stype.
  MOVE 'ZBSEG'      TO isort-tbnam.
  MOVE isort-inewbs TO isort-newbs.

* get WRBTR (Transaction Amt + GST)
  wa_wrbtr = ABS( wa-trans_amt * va_gst ).
  MOVE wa_wrbtr TO isort-wrbtr.
  SHIFT isort-wrbtr RIGHT DELETING TRAILING space.

  MOVE '/'          TO isort-wmwst.

  IF NOT zbacct-omwskz IS INITIAL.
    MOVE zbacct-omwskz TO isort-mwskz.
  ELSE.
    MOVE '/' TO isort-mwskz.
  ENDIF.

  MOVE isort-ikostl TO isort-kostl.
  MOVE isort-iaufnr TO isort-aufnr.
  MOVE isort-imatnr TO isort-matnr.
  MOVE isort-iwerks TO isort-werks.

*get offset MENGE, MEINS
  MOVE '/' TO isort-menge.
  MOVE '/' TO isort-meins.

*get offset SGTXT
  MOVE '/' TO isort-sgtxt.

*get the rest of the fields from the offset sortkey group
  MOVE isort-izterm TO isort-zterm.
  MOVE isort-ihkont TO isort-hkont.
  MOVE isort-iprojk TO isort-projk.
  MOVE isort-ikndnr TO isort-kndnr.
  MOVE isort-ivkorg TO isort-vkorg.
  MOVE isort-iwwbrn TO isort-wwbrn.
  MOVE isort-iwwsct TO isort-wwsct.
  MOVE isort-iwwseg TO isort-wwseg.
  MOVE isort-iwwrat TO isort-wwrat.
  MOVE isort-iwwser TO isort-wwser.
  MOVE isort-imatnr TO isort-artnr.

ENDFORM.                    "F_ZBSEG_GST1

*----------------------------------------------------------------------*
* Build GST2 ZBSEG line item record & Offset line item record
*----------------------------------------------------------------------*
FORM f_zbseg_gst2.

*build generic sortkey group
  PERFORM build_sortkey_group.

* Replace the generic code from the sortkey routine
* with the code below

  MOVE '256915' TO isort-ihkont.
  MOVE: '/' TO isort-imatnr, '/' TO isort-iaufnr,
        '/' TO isort-isgtxt, '/' TO isort-iwerks,
        '/' TO isort-iprojk, '/' TO isort-ikndnr,
        '/' TO isort-ivkorg, '/' TO isort-iwwbrn,
        '/' TO isort-iwwrat, '/' TO isort-iwwser,
        '/' TO isort-iwwsct, '/' TO isort-iwwseg.

*----------------------------------------------------------------------*
*build GST2 zbseg
*----------------------------------------------------------------------*
  MOVE '2' TO isort-stype.
  MOVE 'ZBSEG' TO isort-tbnam.
  MOVE isort-inewbs TO isort-newbs.

* get WRBTR (GST amt only)
  wa_wrbtr = ABS( wa-trans_amt * va_gstpct ).
  MOVE wa_wrbtr TO isort-wrbtr.
  SHIFT isort-wrbtr RIGHT DELETING TRAILING space.

  MOVE '/' TO isort-wmwst.
  MOVE '/' TO isort-mwskz.

  MOVE isort-ikostl TO isort-kostl.
  MOVE isort-iaufnr TO isort-aufnr.
  MOVE isort-imatnr TO isort-matnr.
  MOVE isort-iwerks TO isort-werks.

*get offset MENGE, MEINS
  MOVE '/' TO isort-menge.
  MOVE '/' TO isort-meins.

  MOVE isort-isgtxt TO isort-sgtxt.
  MOVE isort-izterm TO isort-zterm.
  MOVE isort-ihkont TO isort-hkont.
  MOVE isort-iprojk TO isort-projk.
  MOVE isort-ikndnr TO isort-kndnr.
  MOVE isort-ivkorg TO isort-vkorg.
  MOVE isort-iwwbrn TO isort-wwbrn.
  MOVE isort-iwwsct TO isort-wwsct.
  MOVE isort-iwwseg TO isort-wwseg.
  MOVE isort-iwwrat TO isort-wwrat.
  MOVE isort-iwwser TO isort-wwser.
  MOVE isort-imatnr TO isort-artnr.

  APPEND isort.

*build offset sortkey group
  PERFORM build_offset_sortkey_group.

* Replace the generic code from the offset sortkey routine
* with the code below

* get SGTXT
  MOVE '/' TO isort-isgtxt.

*----------------------------------------------------------------------*
*Build Offset GST2 ZBSEG
*----------------------------------------------------------------------*

  MOVE '2'          TO isort-stype.
  MOVE 'ZBSEG'      TO isort-tbnam.
  MOVE isort-inewbs TO isort-newbs.

* get WRBTR (GST Amt Only)
  wa_wrbtr = ABS( wa-trans_amt * va_gstpct ).
  MOVE wa_wrbtr TO isort-wrbtr.
  SHIFT isort-wrbtr RIGHT DELETING TRAILING space.

  MOVE '/'          TO isort-wmwst.

  IF NOT zbacct-omwskz IS INITIAL.
    MOVE zbacct-omwskz TO isort-mwskz.
  ELSE.
    MOVE '/' TO isort-mwskz.
  ENDIF.

  MOVE isort-ikostl TO isort-kostl.
  MOVE isort-iaufnr TO isort-aufnr.
  MOVE isort-imatnr TO isort-matnr.
  MOVE isort-iwerks TO isort-werks.

*get offset MENGE, MEINS
  MOVE '/' TO isort-menge.
  MOVE '/' TO isort-meins.

*get offset SGTXT
  MOVE '/' TO isort-sgtxt.

*get the rest of the fields from the offset sortkey group
  MOVE isort-izterm TO isort-zterm.
  MOVE isort-ihkont TO isort-hkont.
  MOVE isort-iprojk TO isort-projk.
  MOVE isort-ikndnr TO isort-kndnr.
  MOVE isort-ivkorg TO isort-vkorg.
  MOVE isort-iwwbrn TO isort-wwbrn.
  MOVE isort-iwwsct TO isort-wwsct.
  MOVE isort-iwwseg TO isort-wwseg.
  MOVE isort-iwwrat TO isort-wwrat.
  MOVE isort-iwwser TO isort-wwser.
  MOVE isort-imatnr TO isort-artnr.

ENDFORM.                    "F_ZBSEG_GST2

*----------------------------------------------------------------------*
* Build main sortkey record                                            *
*----------------------------------------------------------------------*
FORM build_sortkey_group.

  MOVE p_dsize TO wa_docsize.
  wa_zbd1p_num = ABS( sy-tabix / wa_docsize ).
  wa_zbd1p = TRUNC( wa_zbd1p_num ).

  MOVE wa_zbd1p    TO isort-izterm.

* set up Banner Service Type
  IF wa-serv_type+0(3) = 'GAS' OR
     wa-serv_type+0(3) = 'CHR' OR
     wa-serv_type+0(3) = 'GLI'.
    MOVE 'GAS' TO isort-iservtype.
  ELSE.
    IF wa-serv_type IS INITIAL.
      MOVE '/' TO isort-iservtype.
    ELSE.
      MOVE wa-serv_type TO isort-iservtype.
    ENDIF.
  ENDIF.

* get corresponding ZFB02, ZFB02, & ZFB04 table records
  PERFORM read_lookup_tables.

* get KOSTL

  IF NOT ( zbacct-cu_ind IS INITIAL AND zbacct-ca_ind IS INITIAL AND
           zbacct-cs_ind IS INITIAL AND zbacct-sa_ind IS INITIAL ).
    PERFORM lookup_cost_centre USING wa-town_cd wa-munic_cd
                                     wa-cust_num
                               CHANGING isort-ikostl.
  ENDIF.

* get HKONT
  IF NOT zbacct-saknr IS INITIAL.
    MOVE zbacct-saknr TO isort-ihkont.
  ELSE.
    CONCATENATE '*' wa-gl_classcd '*' wa-serv_class INTO isort-ihkont.
  ENDIF.

* get NEWBS
* Start of TR804 changes
*  MOVE wa-trans_amt TO wa_amount.
*  IF wa_amount < 0.
*    isort-inewbs = '40'.
*  ELSE.
*    isort-inewbs = '50'.
*  ENDIF.
  MOVE wa-trans_amt TO wa_amount.
  IF wa_amount < 0.
     IF ZBACCT-SAKNR = 'BANNER'.
        isort-inewbs = '02'.
     ELSE.
        isort-inewbs = '40'.
     ENDIF.
  ELSE.
     IF ZBACCT-SAKNR = 'BANNER'.
        isort-inewbs = '11'.
     ELSE.
        isort-inewbs = '50'.
     ENDIF.
  ENDIF.

* End of TR804 changes

* get MATNR
  IF NOT zbacct-matnr IS INITIAL.
    MOVE zbacct-matnr TO isort-imatnr.
  ENDIF.

* get AUFNR
  IF NOT zbacct-gl_io IS INITIAL.
    MOVE zbacct-gl_io TO isort-iaufnr.
  ELSE.
    PERFORM f_lookup_internal_order USING wa-town_cd wa-munic_cd
                                    CHANGING isort-iaufnr.
  ENDIF.

* get SGTXT
  IF wa-gl_classcd = 'REMR'.
    CONCATENATE 'Banner Interface - ' wa-gl_classcd
                INTO isort-isgtxt.
  ENDIF.

* get WERKS
  IF NOT zbacct-matnr IS INITIAL.
    IF wa-town_cd = '00' OR wa-town_cd = '99'.
      MOVE 'S100' TO isort-iwerks.
    ELSE.
      IF wa_zfb03_found = 'Y'.
        MOVE zfb03-werks TO isort-iwerks.
      ELSE.
        CONCATENATE '*' wa-town_cd '*' INTO isort-iwerks.
      ENDIF.
    ENDIF.
  ENDIF.

* get PROJK
  IF NOT ( zbacct-wbs_ef_ind IS INITIAL
           AND zbacct-wbs_me_ind IS INITIAL
           AND zbacct-wbs_pr_ind IS INITIAL
           AND zbacct-wbs_cs_ind IS INITIAL                  "TR841
          ).
    PERFORM f_lookup_projk USING wa-gl_classcd
                                 wa-serv_class
                                 wa-town_cd
                                 wa-munic_cd
                        CHANGING isort-iprojk.
  ENDIF.

* get KNDNR
  IF NOT zbacct-matnr IS INITIAL.
    MOVE '1' TO isort-ikndnr.
  ELSE.
    MOVE '/' TO isort-ikndnr.
  ENDIF.

* get CE11100_VKORG
  PERFORM get_vkorg USING isort-iwerks wa-serv_class
                    CHANGING isort-ivkorg.

* get CE11100_WWBRN
  PERFORM get_wwbrn USING wa-town_cd CHANGING isort-iwwbrn.

* get WWRAT
  IF NOT zbacct-matnr IS INITIAL.
    IF ( ( wa-serv_type+0(3) = 'GAS' OR wa-serv_type+0(3) = 'CHR' OR
         wa-serv_type+0(3) = 'GLI' ) OR
       ( wa-gl_classcd = 'RTOT' OR wa-gl_classcd = 'RTRT' ) ) AND
         wa-gl_classcd NOT IN ra_glclass.
      MOVE wa-rate_class TO isort-iwwrat.
*      IF WA_ZFB02_FOUND = 'Y'.
*        MOVE ZFB02-RATECL TO ISORT-IWWRAT.
*      ELSE.
*        MOVE WA-GL_CLASSCD+1(3) TO ISORT-IWWRAT.
*      ENDIF.
    ENDIF.
  ENDIF.

* get WWSER
  IF NOT zbacct-matnr IS INITIAL.
    IF ( ( wa-serv_type+0(3) = 'GAS' OR wa-serv_type+0(3) = 'CHR' OR
         wa-serv_type+0(3) = 'GLI' ) OR
       ( wa-gl_classcd = 'RTOT' OR wa-gl_classcd = 'RTRT' ) ) AND
         wa-gl_classcd NOT IN ra_glclass.
      MOVE wa-serv_class TO isort-iwwser.
      IF wa_zfb02_found = 'Y'.
        MOVE zfb02-servcl TO isort-iwwser.
      ELSE.
        MOVE wa-serv_class TO isort-iwwser.
      ENDIF.
    ENDIF.
  ENDIF.

* get WWSCT, WWSEG
  IF NOT zbacct-matnr IS INITIAL.
    MOVE 'Z' TO isort-iwwsct.
    MOVE 'ZZ' TO isort-iwwseg.
  ELSE.
    MOVE '/' TO isort-iwwsct.
    MOVE '/' TO isort-iwwseg.
  ENDIF.

ENDFORM.                    "BUILD_SORTKEY_GROUP

*----------------------------------------------------------------------*
* Build Offset Sortkey Group
*----------------------------------------------------------------------*
FORM build_offset_sortkey_group.

* set up Banner Service Type
  IF wa-serv_type+0(3) = 'GAS' OR
     wa-serv_type+0(3) = 'CHR' OR
     wa-serv_type+0(3) = 'GLI'.
    MOVE 'GAS' TO isort-iservtype.
  ELSE.
    IF wa-serv_type IS INITIAL.
      MOVE '/' TO isort-iservtype.
    ELSE.
      MOVE wa-serv_type TO isort-iservtype.
    ENDIF.
  ENDIF.

* get offset IKOSTL
  IF NOT zbacct-occ IS INITIAL.
    MOVE zbacct-occ TO isort-ikostl.
  ELSE.
    MOVE '/' TO isort-ikostl.
  ENDIF.

* get offset IHKONT
  IF NOT zbacct-oacct IS INITIAL.
    MOVE zbacct-oacct TO isort-ihkont.
  ELSE.
    CONCATENATE '*' wa-gl_classcd '*' wa-serv_class
                INTO isort-ihkont.
  ENDIF.

* get offset INEWBS
*Start of TR804 Changes
*  MOVE wa-trans_amt TO wa_amount.
*  IF wa_amount < 0.
*    isort-inewbs = '50'.
*  ELSE.
*    isort-inewbs = '40'.
*  ENDIF.

  MOVE wa-trans_amt TO wa_amount.
  IF wa_amount < 0.
     IF ZBACCT-OACCT = 'BANNER'.
        isort-inewbs = '11'.
     ELSE.
        isort-inewbs = '50'.
     ENDIF.
  ELSE.
     IF ZBACCT-OACCT = 'BANNER'.
        isort-inewbs = '02'.
     ELSE.
        isort-inewbs = '40'.
     ENDIF.
  ENDIF.
*End of TR804 Changes

*get offset IMATNR
  MOVE '/' TO isort-imatnr.

*get offset IAUFNR
  IF NOT zbacct-oio_ind IS INITIAL.
    PERFORM get_offset_io_number CHANGING isort-iaufnr.
  ELSE.
    MOVE '/' TO isort-iaufnr.
  ENDIF.

*get offset ISGTXT
  IF wa-gl_classcd IN ra_compuse.
    CONCATENATE 'Banner Interface - Gas ' wa-gl_classcd
                wa-cust_num+0(9) '-' wa-cust_num+9(7)
                INTO isort-isgtxt.
  ELSE.
    MOVE '/' TO isort-isgtxt.
  ENDIF.

* get CE11100 fields
  MOVE: '/' TO isort-iwerks, '/' TO isort-iprojk,
        '/' TO isort-ikndnr, '/' TO isort-ivkorg,
        '/' TO isort-iwwbrn, '/' TO isort-iwwrat,
        '/' TO isort-iwwser, '/' TO isort-iwwsct,
        '/' TO isort-iwwseg.

ENDFORM.                    "build_offset_sortkey_group

*---------------------------------------------------------------------*
* This routine will get the correct cost centre based on the zbacct
* cost centre indicator fields.
*---------------------------------------------------------------------*
FORM lookup_cost_centre USING itown imunic icust CHANGING okostl.
  IF NOT zbacct-cu_ind IS INITIAL.
    PERFORM lookup_cu_cc USING itown imunic icust
                         CHANGING okostl.
  ELSEIF NOT zbacct-ca_ind IS INITIAL.
    PERFORM lookup_ca_cc USING itown imunic
                         CHANGING okostl.
  ELSEIF NOT zbacct-cs_ind IS INITIAL.
    PERFORM lookup_cs_cc USING itown imunic
                         CHANGING okostl.
  ELSE.
    PERFORM lookup_sa_cc USING itown imunic
                         CHANGING okostl.
  ENDIF.
ENDFORM.                    "lookup_cost_centre

*---------------------------------------------------------------------*
* This routine will get the CU cost centre.
*---------------------------------------------------------------------*
FORM lookup_cu_cc USING itown imunic icust CHANGING okostl.

  IF wa_zfb04_found = 'Y'.
    MOVE zfb04-kostl TO okostl.
  ELSE.
    CONCATENATE '*' icust+7(9) INTO okostl.
  ENDIF.

ENDFORM.                    "lookup_cu_cc
*---------------------------------------------------------------------*
* This routine will get the C/A cost centre.
*---------------------------------------------------------------------*
FORM lookup_ca_cc USING itown imunic CHANGING okostl.

  IF wa_zfb03_found = 'Y'.
    MOVE zfb03-cakostl TO okostl.
  ELSE.
    CONCATENATE 'TC' itown 'MC' imunic INTO okostl.
  ENDIF.

ENDFORM.                    "lookup_ca_cc
*---------------------------------------------------------------------*
* This routine will get the C/S cost centre.
*---------------------------------------------------------------------*
FORM lookup_cs_cc USING itown imunic CHANGING okostl.

  IF wa_zfb03_found = 'Y'.
    MOVE zfb03-cskostl TO okostl.
  ELSE.
    CONCATENATE 'TC' itown 'MC' imunic INTO okostl.
  ENDIF.

ENDFORM.                    "lookup_cs_cc
*---------------------------------------------------------------------*
* This routine will get the sales cost centre.
*---------------------------------------------------------------------*
FORM lookup_sa_cc USING itown imunic CHANGING okostl.

  IF wa_zfb03_found = 'Y'.
    MOVE zfb03-sakostl TO okostl.
  ELSE.
    CONCATENATE 'TC' itown 'MC' imunic INTO okostl.
  ENDIF.

ENDFORM.                    "lookup_sa_cc

*----------------------------------------------------------------------*
* This routine will get the internal order number based on the zbacct
* IO indicator
*----------------------------------------------------------------------*
FORM f_lookup_internal_order USING itown imunic CHANGING oaufnr.
  IF NOT zbacct-dmio_ind IS INITIAL.
    PERFORM lookup_io_dm USING itown imunic
                         CHANGING oaufnr.
  ELSEIF NOT zbacct-bd_g_ind IS INITIAL.
    PERFORM lookup_io_bdg USING itown imunic
                          CHANGING oaufnr.
  ELSEIF NOT zbacct-bd_m_ind IS INITIAL.
    PERFORM lookup_io_bdm USING itown imunic
                          CHANGING oaufnr.
  ELSEIF NOT zbacct-col_io_ind IS INITIAL.
    PERFORM lookup_io_coll USING itown imunic
                           CHANGING oaufnr.
  ELSE.
    MOVE '/' TO oaufnr.
  ENDIF.
ENDFORM.                    "F_lookup_internal_order

*---------------------------------------------------------------------*
* This routine will get the DM Internal Order number.
*---------------------------------------------------------------------*
FORM lookup_io_dm USING itown imunic CHANGING oaufnr.

  IF wa_zfb03_found = 'Y'.
    MOVE zfb03-dm_io TO oaufnr.
  ELSE.
    CONCATENATE '*TC' itown '*MC' imunic INTO oaufnr.
  ENDIF.

ENDFORM.                    "lookup_IO_DM
*---------------------------------------------------------------------*
* This routine will get the Bad Debt Gas Internal Order number.
*---------------------------------------------------------------------*
FORM lookup_io_bdg USING itown imunic CHANGING oaufnr.

  IF wa_zfb03_found = 'Y'.
    MOVE zfb03-bd_io_gas TO oaufnr.
  ELSE.
    CONCATENATE '*TC' itown '*MC' imunic INTO oaufnr.
  ENDIF.

ENDFORM.                    "lookup_IO_BDG
*---------------------------------------------------------------------*
* This routine will get the Bad Debt Merch Internal Order number.
*---------------------------------------------------------------------*
FORM lookup_io_bdm USING itown imunic CHANGING oaufnr.

  IF wa_zfb03_found = 'Y'.
    MOVE zfb03-bd_io_mdse TO oaufnr.
  ELSE.
    CONCATENATE '*TC' itown '*MC' imunic INTO oaufnr.
  ENDIF.

ENDFORM.                    "lookup_IO_BDM
*---------------------------------------------------------------------*
* This routine will get the 3rd Party Internal Order number.
*---------------------------------------------------------------------*
FORM lookup_io_coll USING itown imunic CHANGING oaufnr.

  IF wa_zfb03_found = 'Y'.
    MOVE zfb03-collect_io TO oaufnr.
  ELSE.
    CONCATENATE '*TC' itown '*MC' imunic INTO oaufnr.
  ENDIF.

ENDFORM.                    "lookup_IO_COLL
*---------------------------------------------------------------------*
* This routine will get the correct WBS based on the zbacct
* WBS indicator fields.
*---------------------------------------------------------------------*
FORM f_lookup_projk USING iglclass iservclass itown imuni
                    CHANGING oprojk.
  CLEAR oprojk.
  CASE iglclass.
    WHEN 'RBUY'.  MOVE '29-00-591-9760' TO oprojk.
    WHEN 'MSTB'.  MOVE '06-02-308-5851' TO oprojk.
    WHEN 'MSTL'.  MOVE '04-02-311-5851' TO oprojk.
    WHEN 'MSTW'.  MOVE '02-02-368-5851' TO oprojk.
    WHEN OTHERS.
      PERFORM lookup_wbs CHANGING oprojk.                   "TR841
  ENDCASE.
ENDFORM.                    "F_LOOKUP_PROJK

*----------------------------------------------------------------------*
* Look up WBS number based on zbacct WBS Indicator
*
*----------------------------------------------------------------------*

FORM lookup_wbs CHANGING oprojk.
  IF NOT zbacct-wbs_ef_ind IS INITIAL.
    PERFORM lookup_wbs_ef USING wa-town_cd wa-munic_cd
                          CHANGING oprojk.
  ELSEIF NOT zbacct-wbs_me_ind IS INITIAL.
    PERFORM lookup_wbs_mext USING wa-town_cd wa-munic_cd
                            CHANGING oprojk.
  ELSEIF NOT zbacct-wbs_pr_ind IS INITIAL.
    PERFORM lookup_wbs_propane USING wa-town_cd wa-munic_cd
                               CHANGING oprojk.
  ELSEIF NOT zbacct-wbs_cs_ind IS INITIAL.                   "TR841
    PERFORM lookup_wbs_customer USING wa-town_cd wa-munic_cd "TR841
                               CHANGING oprojk.             "TR841
  ENDIF.

ENDFORM.                    "lookup_WBS

*----------------------------------------------------------------------*
* Get WBS Excess Footage                                               *
*----------------------------------------------------------------------*

FORM lookup_wbs_ef USING itown imuni CHANGING oprojk.
  IF wa_zfb03_found = 'Y'.
    MOVE zfb03-wbs_ef TO oprojk.
  ELSE.
    CONCATENATE 'TC' itown 'MC' imuni INTO oprojk.
  ENDIF.
ENDFORM.                    "lookup_wbs_ef

*----------------------------------------------------------------------*
* Get WBS MEXT                                                         *
*----------------------------------------------------------------------*

FORM lookup_wbs_mext USING itown imuni CHANGING oprojk.
  IF wa_zfb03_found = 'Y'.
    MOVE zfb03-wbs_mext TO oprojk.
  ELSE.
    CONCATENATE 'TC' itown 'MC' imuni INTO oprojk.
  ENDIF.
ENDFORM.                    "lookup_wbs_mext

*----------------------------------------------------------------------*
* Get WBS Proprane                                                     *
*----------------------------------------------------------------------*

FORM lookup_wbs_propane USING itown imuni CHANGING oprojk.
  IF wa_zfb03_found = 'Y'.
    MOVE zfb03-wbs_prop TO oprojk.
  ELSE.
    CONCATENATE 'TC' itown 'MC' imuni INTO oprojk.
  ENDIF.
ENDFORM.                    "lookup_wbs_propane

*----------------------------------------------------------------------*
* Get WBS CS   "TR841                                                  *
*----------------------------------------------------------------------*

FORM lookup_wbs_customer USING itown imuni CHANGING oprojk.
  IF wa_zfb03_found = 'Y'.
    MOVE zfb03-wbs_cs TO oprojk.
  ELSE.
    CONCATENATE 'TC' itown 'MC' imuni INTO oprojk.
  ENDIF.
ENDFORM.                    "lookup_wbs_customer

*----------------------------------------------------------------------*
* Get VKORG                                                            *
*----------------------------------------------------------------------*
FORM get_vkorg USING iwerks iservcl CHANGING ovkorg.

*Start of TR804 changes

  MOVE '/' TO ovkorg.
  IF NOT zbacct-matnr IS INITIAL.
      ovkorg = 'Z001'.                 "new
*    IF iwerks = 'S100'.
*      IF iservcl = '21' OR iservcl = '31'.
*        MOVE 'U015' TO ovkorg.
*      ELSE.
*        MOVE 'U010' TO ovkorg.
*      ENDIF.
*    ELSE.
*      IF iservcl = 'GJ' OR iservcl = 'GS'.
*        MOVE 'C015' TO ovkorg.
*      ELSE.
*        MOVE 'C010' TO ovkorg.
*      ENDIF.
*    ENDIF.
  ENDIF.
*end ofTR804 changes
ENDFORM.                    "get_vkorg

*----------------------------------------------------------------------*
* Get WWBRN                                                            *
*----------------------------------------------------------------------*
FORM get_wwbrn USING itown CHANGING owwbrn.
  MOVE '/' TO owwbrn.
  IF NOT zbacct-matnr IS INITIAL.
    IF wa_zfb03_found = 'Y'.
      MOVE zfb03-wwbrn TO owwbrn.
    ELSE.
      CONCATENATE '*' itown '*' INTO owwbrn.
    ENDIF.
  ENDIF.
ENDFORM.                    "get_wwbrn

*----------------------------------------------------------------------*
* Get offset I/O number                                                *
*----------------------------------------------------------------------*
FORM get_offset_io_number CHANGING oaufnr.
  IF wa_zfb04_found = 'Y'.
    IF NOT zfb04-oaufnr IS INITIAL.
      MOVE zfb04-oaufnr TO oaufnr.
    ELSE.
      MOVE '/' TO oaufnr.
    ENDIF.
  ENDIF.
ENDFORM.                    "get_offset_IO_number

*----------------------------------------------------------------------*
* Build File and Document Headers for Error Session
*----------------------------------------------------------------------*
FORM build_error_header.

  PERFORM init_structures USING 'BGR00'.
  MOVE '0'                     TO z_bgr00-stype.
  MOVE 'ZFI-BANN-ERR'          TO z_bgr00-group.
  MOVE sy-mandt                TO z_bgr00-mandt.
  MOVE 'BATCH'                 TO z_bgr00-usnam.
  TRANSFER z_bgr00 TO p_errout.

  PERFORM init_structures USING 'BBKPF'.
  MOVE '1'                     TO z_bbkpf-stype.
  MOVE p_tcode                 TO z_bbkpf-tcode.
  MOVE sy-datum                TO z_bbkpf-bldat.
  MOVE 'S9'                    TO z_bbkpf-blart.
  MOVE p_bukrs                 TO z_bbkpf-bukrs.
  MOVE wa-process_date         TO z_bbkpf-budat.
  MOVE p_waers                 TO z_bbkpf-waers.
  MOVE p_xblnr                 TO z_bbkpf-xblnr.
  MOVE 'Banner Error Records'  TO z_bbkpf-bktxt.

  CLEAR: z_bbkpf-numpg, z_bbkpf-stgrd, z_bbkpf-kursf_m,
         z_bbkpf-augtx, z_bbkpf-xprfg, z_bbkpf-sende  ,
         outrec.

  MOVE z_bbkpf(216) TO outrec.
*  concatenate outrec(5) outrec+21 into outrec.
  TRANSFER outrec TO p_errout.

ENDFORM.                    "build_error_header

*---------------------------------------------------------------------*
* This routine will generate an error sessions with records that have
* GL class codes and Service class codes that do not match the zbacct
* table.
*---------------------------------------------------------------------*
FORM f_error_proc.

  IF wa_err_hdr_sw = 'Y'.
    PERFORM build_error_header.
    MOVE 'N' TO wa_err_hdr_sw.
  ENDIF.

  PERFORM init_zbseg1_record.

* Get default 'ERROR' zbacct Record
  MOVE 'N' TO wa_zbacct_found.
  SELECT SINGLE * FROM zbacct WHERE glcode = 'ERR'.

  IF sy-subrc = 0.
    MOVE 'Y' TO wa_zbacct_found.
  ENDIF.

* Get corresponding ZFB02, ZFB03, & ZFB04 records
  PERFORM read_lookup_tables.

  IF wa-budget_amt = 0.
    PERFORM generate_zbseg_err.
  ELSE.
    PERFORM generate_zbseg_errb1.
    PERFORM generate_zbseg_errb2.
  ENDIF.

ENDFORM.                    "F_ERROR_PROC

*---------------------------------------------------------------------*
* This routine will generate zbseg formatted error records for
* posting.
*---------------------------------------------------------------------*
FORM generate_zbseg_err.

* Build ZBSEG record

*Start of TR804 Changes**
*  MOVE wa-trans_amt TO wa_amount.
*  IF wa_amount < 0.
*    zbseg1-newbs = '40'.
*  ELSE.
*    zbseg1-newbs = '50'.
*  ENDIF.

  MOVE wa-trans_amt TO wa_amount.
  IF wa_amount < 0.
     IF ZBACCT-SAKNR = 'BANNER'.
        isort-inewbs = '11'.
     ELSE.
        isort-inewbs = '50'.
     ENDIF.
  ELSE.
     IF ZBACCT-SAKNR = 'BANNER'.
        isort-inewbs = '02'.
     ELSE.
        isort-inewbs = '40'.
     ENDIF.
  ENDIF.

*End of TR804 Changes

  wa_wrbtr = ABS( wa-trans_amt ).
  MOVE wa_wrbtr TO zbseg1-wrbtr.
  SHIFT zbseg1-wrbtr RIGHT DELETING TRAILING space.

* get KOSTL

  IF NOT ( zbacct-cu_ind IS INITIAL AND zbacct-ca_ind IS INITIAL AND
           zbacct-cs_ind IS INITIAL AND zbacct-sa_ind IS INITIAL ).
    PERFORM lookup_cost_centre USING 'ERR ' ' '
                                     wa-cust_num
                               CHANGING zbseg1-kostl.
  ENDIF.

* get SGTXT.
  CONDENSE wa-cons.
  CONCATENATE 'Banner Err: ' wa-serv_type '/' wa-serv_cat '/'
              wa-gl_classcd '/' wa-serv_class '/' wa-town_cd '/'
              wa-munic_cd '/' wa-cons 'M3'
              INTO zbseg1-sgtxt.


* get PROJK
  IF NOT ( zbacct-wbs_ef_ind IS INITIAL
            AND zbacct-wbs_me_ind IS INITIAL
            AND zbacct-wbs_pr_ind IS INITIAL
            AND zbacct-wbs_cs_ind IS INITIAL                 "TR841
         ).
    PERFORM f_lookup_projk USING 'ERR'
                                 ' '
                                 wa-town_cd
                                 wa-munic_cd
                        CHANGING zbseg1-projk.
  ENDIF.

  PERFORM move_common_err_zbseg_fields.

  TRANSFER zbseg1 TO p_errout.

*----------------------------------------------------------------------*
* Build ZBSEG offset record (ERR)                                      *
*----------------------------------------------------------------------*

*Start of TR804 changes
*  MOVE wa-trans_amt TO wa_amount.
*  IF wa_amount < 0.
*    zbseg1-newbs = '50'.
*  ELSE.
*    zbseg1-newbs = '40'.
*  ENDIF.
  MOVE wa-trans_amt TO wa_amount.
  IF wa_amount < 0.
        zbseg1-newbs = '11'.
  ELSE.
        zbseg1-newbs = '02'.
  ENDIF.
*End of TR804 changes

  wa_wrbtr = ABS( wa-trans_amt ).
  MOVE wa_wrbtr TO zbseg1-wrbtr.
  SHIFT zbseg1-wrbtr RIGHT DELETING TRAILING space.

  MOVE '/' TO zbseg1-wmwst.

* Get offset tax code

  IF NOT zbacct-omwskz IS INITIAL.
    MOVE zbacct-omwskz TO zbseg1-mwskz.
  ENDIF.

* Get offset cost centre.

  IF NOT zbacct-occ IS INITIAL.
    MOVE zbacct-occ TO zbseg1-kostl.
  ENDIF.

* Get offset internal order number.

  IF NOT zbacct-oio_ind IS INITIAL.
    PERFORM get_offset_io_number CHANGING zbseg1-aufnr.
  ELSE.
    MOVE '/' TO zbseg1-aufnr.
  ENDIF.

  MOVE '/' TO zbseg1-matnr.
  MOVE '/' TO zbseg1-werks.

* get SGTXT.

  CONCATENATE 'Banner Err: ' wa-serv_type '/' wa-serv_cat '/'
              wa-gl_classcd '/' wa-serv_class '/' wa-town_cd '/'
              wa-munic_cd INTO zbseg1-sgtxt.

* Get offset account number.

  IF NOT zbacct-oacct IS INITIAL.
    MOVE zbacct-oacct TO zbseg1-hkont.
  ELSE.
    CONCATENATE '*' wa-gl_classcd '*' wa-serv_class INTO zbseg1-hkont.
  ENDIF.

  MOVE: '/' TO zbseg1-projk, '/' TO zbseg1-kndnr,
        '/' TO zbseg1-vkorg, '/' TO zbseg1-wwbrn,
        '/' TO zbseg1-wwsct, '/' TO zbseg1-wwseg,
        '/' TO zbseg1-wwrat, '/' TO zbseg1-wwser,
        '/' TO zbseg1-artnr.

  TRANSFER zbseg1 TO p_errout.

ENDFORM.                    "GENERATE_ZBSEG_ERR

*---------------------------------------------------------------------*
* This routine will generate zbseg formatted error records with
* a Budget amount for posting.
*---------------------------------------------------------------------*
FORM generate_zbseg_errb1.

* Build ZBSEG record
*Start of TR804 changes
*  IF wa-budget_amt < 0.
*    zbseg1-newbs = '40'.
*  ELSE.
*    zbseg1-newbs = '50'.
*  ENDIF.
  IF wa-budget_amt < 0.
     IF ZBACCT-SAKNR = 'BANNER'.
        zbseg1-newbs = '02'.
     ELSE.
        zbseg1-newbs = '40'.
     ENDIF.
  ELSE.
     IF ZBACCT-SAKNR = 'BANNER'.
        zbseg1-newbs = '11'.
     ELSE.
        zbseg1-newbs = '50'.
     ENDIF.
  ENDIF.
*End of TR804 changes

  wa_wrbtr = ABS( wa-budget_amt ).
  MOVE wa_wrbtr TO zbseg1-wrbtr.
  SHIFT zbseg1-wrbtr RIGHT DELETING TRAILING space.

* get KOSTL

  IF NOT ( zbacct-cu_ind IS INITIAL AND zbacct-ca_ind IS INITIAL AND
           zbacct-cs_ind IS INITIAL AND zbacct-sa_ind IS INITIAL ).
    PERFORM lookup_cost_centre USING wa-town_cd wa-munic_cd
                                     wa-cust_num
                               CHANGING zbseg1-kostl.
  ENDIF.

* get SGTXT.

  CONCATENATE 'Banner Err: ' wa-serv_type '/' wa-serv_cat '/'
              wa-gl_classcd '/' wa-serv_class '/' wa-town_cd '/'
              wa-munic_cd '/' wa-cons 'M3'
              INTO zbseg1-sgtxt.

* get PROJK
  IF NOT ( zbacct-wbs_ef_ind IS INITIAL
           AND zbacct-wbs_me_ind IS INITIAL
           AND zbacct-wbs_pr_ind IS INITIAL
           AND zbacct-wbs_cs_ind IS INITIAL                  "TR841
            ).
    PERFORM f_lookup_projk USING wa-gl_classcd
                                 wa-serv_class
                                 wa-town_cd
                                 wa-munic_cd
                        CHANGING zbseg1-projk.
  ENDIF.

  PERFORM move_common_err_zbseg_fields.

  TRANSFER zbseg1 TO p_errout.

*----------------------------------------------------------------------*
* Build offset ZBSEG error record (ERRB1)                              *
*----------------------------------------------------------------------*

*Start of TR804 Changes
*  IF wa-budget_amt < 0.
*    zbseg1-newbs = '50'.
*  ELSE.
*    zbseg1-newbs = '40'.
*  ENDIF.
  IF wa-budget_amt < 0.
     IF ZBACCT-SAKNR = 'BANNER'.
        zbseg1-newbs = '11'.
     ELSE.
        zbseg1-newbs = '50'.
     ENDIF.
  ELSE.
     IF ZBACCT-SAKNR = 'BANNER'.
        zbseg1-newbs = '02'.
     ELSE.
        zbseg1-newbs = '40'.
     ENDIF.
  ENDIF.
*End of TR804 Changes

  wa_wrbtr = ABS( wa-budget_amt ).
  MOVE wa_wrbtr TO zbseg1-wrbtr.
  SHIFT zbseg1-wrbtr RIGHT DELETING TRAILING space.

  MOVE '/' TO zbseg1-wmwst.

* Get offset tax code

  IF NOT zbacct-omwskz IS INITIAL.
    MOVE zbacct-omwskz TO zbseg1-mwskz.
  ENDIF.

* Get offset cost centre.

  IF NOT zbacct-occ IS INITIAL.
    MOVE zbacct-occ TO zbseg1-kostl.
  ENDIF.

* Get offset internal order number.

  IF NOT zbacct-oio_ind IS INITIAL.
    PERFORM get_offset_io_number CHANGING zbseg1-aufnr.
  ELSE.
    MOVE '/' TO zbseg1-aufnr.
  ENDIF.

  MOVE '/' TO zbseg1-matnr.
  MOVE '/' TO zbseg1-werks.

* get SGTXT.

  CONCATENATE 'Banner Error: ' wa-serv_type '/' wa-serv_cat '/'
              wa-gl_classcd '/' wa-serv_class '/' wa-town_cd '/'
              wa-munic_cd INTO zbseg1-sgtxt.

* Get offset account number.

  IF NOT zbacct-oacct IS INITIAL.
    MOVE zbacct-oacct TO zbseg1-aufnr.
  ELSE.
    MOVE '/' TO zbseg1-aufnr.
  ENDIF.

  MOVE: '/' TO zbseg1-projk, '/' TO zbseg1-kndnr,
        '/' TO zbseg1-vkorg, '/' TO zbseg1-wwbrn,
        '/' TO zbseg1-wwsct, '/' TO zbseg1-wwseg,
        '/' TO zbseg1-wwrat, '/' TO zbseg1-wwser,
        '/' TO zbseg1-artnr.

  TRANSFER zbseg1 TO p_errout.

ENDFORM.                    "GENERATE_ZBSEG_ERRB1

*---------------------------------------------------------------------*
* This routine will generate the 2nd set of zbseg formatted error
* records with a Budget amount for posting.
*---------------------------------------------------------------------*
FORM generate_zbseg_errb2.

  DATA  va_diff LIKE wa-trans_amt.

* Build ZBSEG record
*Start of TR804 changes
*  va_diff = wa-trans_amt - wa-budget_amt.
*
*  IF va_diff < 0.
*    zbseg1-newbs = '40'.
*  ELSE.
*    zbseg1-newbs = '50'.
*  ENDIF.

  va_diff = wa-trans_amt - wa-budget_amt.
  IF va_diff < 0.
     IF ZBACCT-OACCT = 'BANNER'.
        zbseg1-newbs = '02'.
     ELSE.
        zbseg1-newbs = '40'.
     ENDIF.
  ELSE.
     IF ZBACCT-OACCT = 'BANNER'.
        zbseg1-newbs = '11'.
     ELSE.
        zbseg1-newbs = '50'.
     ENDIF.
  ENDIF.
*End of TR804 changes

  wa_wrbtr = ABS( va_diff ).
  MOVE wa_wrbtr TO zbseg1-wrbtr.
  SHIFT zbseg1-wrbtr RIGHT DELETING TRAILING space.

* get KOSTL

  IF NOT ( zbacct-cu_ind IS INITIAL AND zbacct-ca_ind IS INITIAL AND
           zbacct-cs_ind IS INITIAL AND zbacct-sa_ind IS INITIAL ).
    PERFORM lookup_cost_centre USING wa-town_cd wa-munic_cd
                                     wa-cust_num
                               CHANGING zbseg1-kostl.
  ENDIF.

* get SGTXT.

  CONCATENATE 'Banner Err: ' wa-serv_type '/' wa-serv_cat '/'
              wa-gl_classcd '/' wa-serv_class '/' wa-town_cd '/'
              wa-munic_cd '/' '0.00' 'M3'
              INTO zbseg1-sgtxt.

* get PROJK
  IF NOT ( zbacct-wbs_ef_ind IS INITIAL
            AND zbacct-wbs_me_ind IS INITIAL
            AND zbacct-wbs_pr_ind IS INITIAL
            AND zbacct-wbs_cs_ind IS INITIAL                 "TR841
         ).
    PERFORM f_lookup_projk USING wa-gl_classcd
                                 wa-serv_class
                                 wa-town_cd
                                 wa-munic_cd
                        CHANGING zbseg1-projk.
  ENDIF.

  PERFORM move_common_err_zbseg_fields.

  TRANSFER zbseg1 TO p_errout.

*----------------------------------------------------------------------*
* Build offset ZBSEG error record (ERRB2)
*----------------------------------------------------------------------*

*Start of TR804 changes
*  IF va_diff < 0.
*    zbseg1-newbs = '50'.
*  ELSE.
*    zbseg1-newbs = '40'.
*  ENDIF.
  IF va_diff < 0.
     IF ZBACCT-SAKNR = 'BANNER'.
        zbseg1-newbs = '11'.
     ELSE.
        zbseg1-newbs = '50'.
     ENDIF.
  ELSE.
     IF ZBACCT-SAKNR = 'BANNER'.
        zbseg1-newbs = '02'.
     ELSE.
        zbseg1-newbs = '40'.
     ENDIF.
  ENDIF.
*End of TR804 changes
  wa_wrbtr = ABS( va_diff ).
  MOVE wa_wrbtr TO zbseg1-wrbtr.
  SHIFT zbseg1-wrbtr RIGHT DELETING TRAILING space.

  MOVE '/' TO zbseg1-wmwst.

* Get offset tax code

  IF NOT zbacct-omwskz IS INITIAL.
    MOVE zbacct-omwskz TO zbseg1-mwskz.
  ENDIF.

* Get offset cost centre.

  IF NOT zbacct-occ IS INITIAL.
    MOVE zbacct-occ TO zbseg1-kostl.
  ENDIF.

* Get offset internal order number.

  IF NOT zbacct-oio_ind IS INITIAL.
    PERFORM get_offset_io_number CHANGING zbseg1-aufnr.
  ELSE.
    MOVE '/' TO zbseg1-aufnr.
  ENDIF.

  MOVE '/' TO zbseg1-matnr.
  MOVE '/' TO zbseg1-werks.

* get SGTXT.

  CONCATENATE 'Banner Error: ' wa-serv_type '/' wa-serv_cat '/'
              wa-gl_classcd '/' wa-serv_class '/' wa-town_cd '/'
              wa-munic_cd INTO zbseg1-sgtxt.

* Get offset account number.

  MOVE '140110' TO zbseg1-aufnr.


  MOVE: '/' TO zbseg1-projk, '/' TO zbseg1-kndnr,
        '/' TO zbseg1-vkorg, '/' TO zbseg1-wwbrn,
        '/' TO zbseg1-wwsct, '/' TO zbseg1-wwseg,
        '/' TO zbseg1-wwrat, '/' TO zbseg1-wwser,
        '/' TO zbseg1-artnr.

  TRANSFER zbseg1 TO p_errout.

ENDFORM.                    "GENERATE_ZBSEG_ERRB2

*---------------------------------------------------------------------*
* This routine will set up all common ZBSEG error fields
*---------------------------------------------------------------------*
FORM move_common_err_zbseg_fields.

  MOVE '2'           TO zbseg1-stype.
  MOVE 'ZBSEG'       TO zbseg1-tbnam.

  IF NOT zbacct-mwskz IS INITIAL.
    MOVE '0.00'      TO zbseg1-wmwst.
    SHIFT zbseg1-wmwst RIGHT DELETING TRAILING space.
    MOVE zbacct-mwskz TO zbseg1-mwskz.
  ENDIF.

* get MATNR

  MOVE zbacct-matnr TO zbseg1-matnr.

* get WERKS

  IF zbacct-matnr IS INITIAL.
    MOVE '/' TO zbseg1-werks.
  ELSEIF wa_zfb03_found = 'Y'.
    MOVE zfb03-werks TO zbseg1-werks.
  ELSE.
    CONCATENATE '*' wa-town_cd '*' wa-munic_cd INTO zbseg1-werks.
  ENDIF.

* get HKONT
  IF NOT zbacct-saknr IS INITIAL.
    MOVE zbacct-saknr TO zbseg1-hkont.
  ELSE.
    CONCATENATE '*' wa-gl_classcd '*' wa-serv_class INTO zbseg1-hkont.
  ENDIF.

* get KNDNR, WWSCT, WWSEG
  IF NOT zbacct-matnr IS INITIAL.
    MOVE '1' TO zbseg1-kndnr.
    MOVE 'Z' TO zbseg1-wwsct.
    MOVE 'ZZ' TO zbseg1-wwseg.
  ELSE.
    MOVE: '/' TO zbseg1-kndnr,
          '/' TO zbseg1-wwsct,
          '/' TO zbseg1-wwseg.
  ENDIF.

* get CE11100_VKORG
  PERFORM get_vkorg USING zbseg1-werks wa-serv_class
                    CHANGING zbseg1-vkorg.

* get CE11100_WWBRN
  PERFORM get_wwbrn USING wa-town_cd CHANGING zbseg1-wwbrn.

* get WWRAT, WWSER
  MOVE wa-rate_class TO zbseg1-wwrat.
  IF wa_zfb02_found = 'Y'.
*    MOVE ZFB02-RATECL TO ZBSEG1-WWRAT.
    MOVE zfb02-servcl TO zbseg1-wwser.
  ENDIF.

* get CE11100_ARTNR
  MOVE zbseg1-matnr TO zbseg1-artnr.


ENDFORM.                    "MOVE_COMMON_ERR_ZBSEG_FIELDS

*----------------------------------------------------------------------*
* Read ZFB02, ZFB03, and ZFB04 tables                                  *
*----------------------------------------------------------------------*
FORM read_lookup_tables.
  MOVE 'N' TO wa_zfb02_found.
  MOVE 'N' TO wa_zfb03_found.
  MOVE 'N' TO wa_zfb04_found.

* Read ZFB02 Service Class table
  SELECT SINGLE * FROM zfb02 WHERE glcode = wa-gl_classcd AND
                                   b_servcl = wa-serv_class.
  IF sy-subrc = 0.
    MOVE 'Y' TO wa_zfb02_found.
  ELSE.
    MOVE 'N' TO wa_zfb02_found.
  ENDIF.

* Read ZFB03 Town Code table
  SELECT SINGLE * FROM zfb03 WHERE towncode = wa-town_cd AND
                                   municode = wa-munic_cd.
  IF sy-subrc = 0.
    MOVE 'Y' TO wa_zfb03_found.
  ELSE.
    MOVE 'N' TO wa_zfb03_found.
  ENDIF.

* Read ZFB04 Company Used Gas table
  SELECT SINGLE * FROM zfb04 WHERE z_bcust = wa-cust_num.

  IF sy-subrc = 0.
    MOVE 'Y' TO wa_zfb04_found.
  ELSE.
    MOVE 'N' TO wa_zfb04_found.
  ENDIF.

ENDFORM.                    "read_lookup_tables

*---------------------------------------------------------------------*
* sum data
*---------------------------------------------------------------------*
FORM f_summarize_data.
  DATA: va_menge TYPE p DECIMALS 3 VALUE 0,
        va_wrbtr TYPE p DECIMALS 2 VALUE 0,
        va_volume(18) TYPE c.


*- prepare table summary
  REFRESH isum.

*- sort data
  SORT isort BY izterm iservtype ikostl ihkont inewbs
                imatnr iaufnr    isgtxt iwerks iprojk ikndnr
                ivkorg iwwbrn    iwwrat iwwser iwwsct iwwseg.

*- summarize data
  LOOP AT isort.

    READ TABLE isum WITH KEY izterm = isort-izterm
                             iservtype = isort-iservtype
                             ikostl = isort-ikostl
                             ihkont = isort-ihkont
                             inewbs = isort-inewbs
                             imatnr = isort-imatnr
                             iaufnr = isort-iaufnr
                             isgtxt = isort-isgtxt
                             iwerks = isort-iwerks
                             iprojk = isort-iprojk
                             ikndnr = isort-ikndnr
                             ivkorg = isort-ivkorg
                             iwwbrn = isort-iwwbrn
                             iwwrat = isort-iwwrat
                             iwwser = isort-iwwser
                             iwwsct = isort-iwwsct
                             iwwseg = isort-iwwseg.

* If there is a match in table isum, then add isort
* amount and volume to isum record (sum up)
* If no match, then isort record is added to isum.

    IF sy-subrc = 0.
* sum up dollar amounts.
      MOVE isum-wrbtr TO va_wrbtr.
      ADD isort-wrbtr TO va_wrbtr.
      MOVE va_wrbtr TO isum-wrbtr.
      SHIFT isum-wrbtr RIGHT DELETING TRAILING space.
* sum up non-zero consumption.
      IF isort-menge <> '/'.
        IF isum-menge <> '/'.
          MOVE isum-menge TO va_menge.
        ELSE.
          MOVE 0 TO va_menge.
        ENDIF.
        ADD isort-menge TO va_menge.
        MOVE va_menge TO isum-menge.
        SHIFT isum-menge RIGHT DELETING TRAILING space.
        MOVE isort-meins TO isum-meins.
      ENDIF.
      MODIFY isum INDEX sy-tabix TRANSPORTING wrbtr menge meins.
      CLEAR isum.
    ELSE.
*  no match in isum. Add isort to isum table.
      MOVE-CORRESPONDING isort TO isum.
      APPEND isum. CLEAR isum.
    ENDIF.
  ENDLOOP.

ENDFORM.                    "f_summarize_data

*---------------------------------------------------------------------*
* Create Gas, Rental, Other BDC Session files
*---------------------------------------------------------------------*
FORM f_create_output_files.

  PERFORM build_file_headers.

  LOOP AT isum.
    AT NEW izterm.
      switch = 0.
    ENDAT.
    MOVE isum-wrbtr TO va_amt.
    IF ( isum-iservtype+0(3) = 'GAS' OR
         isum-iservtype+0(3) = 'CHR' OR
         isum-iservtype = 'PEAK' ) AND
         NOT va_amt = '0.00'.
      PERFORM create_gas_line_item.
    ELSEIF ( isum-iservtype = 'RENT' OR
             isum-iservtype = 'RVRA' OR
             isum-iservtype = 'RNGV' ) AND
           NOT va_amt = '0.00'.
      PERFORM create_rntl_line_item.
    ELSEIF NOT va_amt = '0.00'.
      PERFORM create_other_line_item.
    ENDIF.
  ENDLOOP.
ENDFORM.                    "f_create_output_files

*---------------------------------------------------------------------*
* Create Gas, Rental, Other BDC Session Headers
*---------------------------------------------------------------------*
FORM build_file_headers.
  PERFORM init_structures USING 'BGR00'.
  PERFORM init_structures USING 'BBKPF'.

* Create Gas BDC Session Header Only. Document Header
* will be created when izterm changes value.

  MOVE '0'                     TO z_bgr00-stype.
  MOVE 'ZFI-BANN-GAS'          TO z_bgr00-group.
  MOVE sy-mandt                TO z_bgr00-mandt.
  MOVE 'BATCH'                 TO z_bgr00-usnam.
  TRANSFER z_bgr00 TO p_gasout.

* Create Rental BDC Session & Document Headers.

  MOVE 'ZFI-BANN-RNT'              TO z_bgr00-group.
  TRANSFER z_bgr00 TO p_rntout.

  MOVE '1'                         TO z_bbkpf-stype.
  MOVE p_tcode                     TO z_bbkpf-tcode.
  MOVE sy-datum                    TO z_bbkpf-bldat.
  MOVE 'S9'                        TO z_bbkpf-blart.
  MOVE p_bukrs                     TO z_bbkpf-bukrs.
  MOVE wa-process_date             TO z_bbkpf-budat.
  MOVE p_waers                     TO z_bbkpf-waers.
  MOVE p_xblnr                     TO z_bbkpf-xblnr.
  MOVE 'Banner Interface - Rental' TO z_bbkpf-bktxt.

  CLEAR: z_bbkpf-numpg, z_bbkpf-stgrd, z_bbkpf-kursf_m,
         z_bbkpf-augtx, z_bbkpf-xprfg, z_bbkpf-sende  ,
         outrec.

  MOVE z_bbkpf(216) TO outrec.
*  concatenate outrec(5) outrec+21 into outrec.
  TRANSFER outrec TO p_rntout.

* Create Other BDC Session & Document Headers.

  MOVE 'ZFI-BANN-OTH'              TO z_bgr00-group.
  TRANSFER z_bgr00 TO p_othout.

  MOVE '1'                         TO z_bbkpf-stype.
  MOVE p_tcode                     TO z_bbkpf-tcode.
  MOVE sy-datum                    TO z_bbkpf-bldat.
  MOVE 'S9'                        TO z_bbkpf-blart.
  MOVE p_bukrs                     TO z_bbkpf-bukrs.
  MOVE wa-process_date             TO z_bbkpf-budat.
  MOVE p_waers                     TO z_bbkpf-waers.
  MOVE p_xblnr                     TO z_bbkpf-xblnr.
  MOVE 'Banner Interface - Other'  TO z_bbkpf-bktxt.

  CLEAR: z_bbkpf-numpg, z_bbkpf-stgrd, z_bbkpf-kursf_m,
         z_bbkpf-augtx, z_bbkpf-xprfg, z_bbkpf-sende  ,
         outrec.

  MOVE z_bbkpf(216) TO outrec.
*  concatenate outrec(5) outrec+21 into outrec.
  TRANSFER outrec TO p_othout.
ENDFORM.                    "build_file_headers

*---------------------------------------------------------------------*
* Create Gas ZBSEG records
*---------------------------------------------------------------------*
FORM create_gas_line_item.

  IF switch = 0.
    switch = 1.
    PERFORM init_structures USING 'BBKPF'.
    MOVE '1'                      TO z_bbkpf-stype.
    MOVE p_tcode                  TO z_bbkpf-tcode.
    MOVE sy-datum                 TO z_bbkpf-bldat.
    MOVE 'S8'                     TO z_bbkpf-blart.
    MOVE p_bukrs                  TO z_bbkpf-bukrs.
    MOVE wa-process_date          TO z_bbkpf-budat.
    MOVE p_waers                  TO z_bbkpf-waers.
    MOVE p_xblnr                  TO z_bbkpf-xblnr.
    MOVE 'Banner Interface - Gas' TO z_bbkpf-bktxt.

    CLEAR: z_bbkpf-numpg, z_bbkpf-stgrd, z_bbkpf-kursf_m,
           z_bbkpf-augtx, z_bbkpf-xprfg, z_bbkpf-sende  ,
           outrec.

    MOVE z_bbkpf(216) TO outrec.
*    concatenate outrec(5) outrec+21 into outrec.
    TRANSFER outrec TO p_gasout.
  ENDIF.

  IF isum-sgtxt = '/'.
    MOVE 'Banner Interface - Gas' TO isum-sgtxt.
  ENDIF.
  MOVE '/' TO isum-zterm.
  MOVE-CORRESPONDING isum TO zbseg1.
  TRANSFER zbseg1 TO p_gasout.
ENDFORM.                    "create_gas_line_item

*---------------------------------------------------------------------*
* Create Rental ZBSEG records
*---------------------------------------------------------------------*
FORM create_rntl_line_item.
  MOVE '/' TO isum-zterm.
  MOVE 'Banner Interface - Rental' TO isum-sgtxt.
  MOVE-CORRESPONDING isum TO zbseg1.
  TRANSFER zbseg1 TO p_rntout.
ENDFORM.                    "create_rntl_line_item

*---------------------------------------------------------------------*
* Create Other ZBSEG records
*---------------------------------------------------------------------*
FORM create_other_line_item.
  MOVE '/' TO isum-zterm.
  IF isum-sgtxt = '/'.
    MOVE 'Banner Interface - Other' TO isum-sgtxt.
  ENDIF.
  MOVE-CORRESPONDING isum TO zbseg1.
  TRANSFER zbseg1 TO p_othout.
ENDFORM.                    "create_other_line_item

*---------------------------------------------------------------------*
*  Used to initialize the record to '/' - for internal table ISORT
*---------------------------------------------------------------------*
FORM init_isort_record.
  DATA: vf_index TYPE i VALUE 1.
  WHILE vf_index <= 88.
    ASSIGN COMPONENT vf_index OF STRUCTURE isort TO <f1>.
    <f1> = nodata.
    ADD 1 TO vf_index.
  ENDWHILE.
ENDFORM.                    "init_table_record

*---------------------------------------------------------------------*
*  Used to initialize the record to '/' - for internal table ZBSEG1
*---------------------------------------------------------------------*
FORM init_zbseg1_record.
  DATA: vf_index TYPE i VALUE 1.
  WHILE vf_index <= 88.
    ASSIGN COMPONENT vf_index OF STRUCTURE zbseg1 TO <f1>.
    <f1> = nodata.
    ADD 1 TO vf_index.
  ENDWHILE.
ENDFORM.                    "init_zbseg1_record

*---------------------------------------------------------------------*
*  Used to initialize the record to '/' - FOR BGR00, BBKPF, BBSEG
*---------------------------------------------------------------------*
FORM init_structures USING tabname.
  SELECT * FROM dd03l WHERE tabname = tabname.
    CLEAR char.
    char(2)   = 'Z_'.
    char+2(5) = tabname.
    char+7(1) = '-'.
    char+8    = dd03l-fieldname.
    ASSIGN (char) TO <f1>.
    <f1> = nodata.
  ENDSELECT.
ENDFORM.                    "init_structures


*&---------------------------------------------------------------------*
*&      Form  INITIALIZE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM initialize_data.
  REFRESH: ra_glclass.

  ra_glclass-sign     =  'I'.
  ra_glclass-option   =  'EQ'.
  ra_glclass-low      =  'DNP '.
  APPEND ra_glclass.

  ra_glclass-sign     =  'I'.
  ra_glclass-option   =  'EQ'.
  ra_glclass-low      =  'ZAOC'.
  APPEND ra_glclass.

  ra_glclass-sign     =  'I'.
  ra_glclass-option   =  'EQ'.
  ra_glclass-low      =  'ZPRE'.
  APPEND ra_glclass.

  ra_glclass-sign     =  'I'.
  ra_glclass-option   =  'EQ'.
  ra_glclass-low      =  'ZCSC'.
  APPEND ra_glclass.

  ra_glclass-sign     =  'I'.
  ra_glclass-option   =  'EQ'.
  ra_glclass-low      =  'PREM'.
  APPEND ra_glclass.

  ra_glclass-sign     =  'I'.
  ra_glclass-option   =  'EQ'.
  ra_glclass-low      =  'LP  '.
  APPEND ra_glclass.

* Company Use range

  REFRESH: ra_compuse.

  ra_compuse-sign     =  'I'.
  ra_compuse-option   =  'EQ'.
  ra_compuse-low      =  'OWNC'.
  APPEND ra_compuse.

  ra_compuse-sign     =  'I'.
  ra_compuse-option   =  'EQ'.
  ra_compuse-low      =  'OWNS'.
  APPEND ra_compuse.

  ra_compuse-sign     =  'I'.
  ra_compuse-option   =  'EQ'.
  ra_compuse-low      =  'FRAN'.
  APPEND ra_compuse.

  ra_compuse-sign     =  'I'.
  ra_compuse-option   =  'EQ'.
  ra_compuse-low      =  'CMPN'.
  APPEND ra_compuse.

  ra_compuse-sign     =  'I'.
  ra_compuse-option   =  'EQ'.
  ra_compuse-low      =  'CMPS'.
  APPEND ra_compuse.

**  Start of changes by AHMADT FOR CHG0155278 on 08/05/2019

  ra_compuse-sign     =  'I'.
  ra_compuse-option   =  'EQ'.
  ra_compuse-low      =  'CLC1'.
  APPEND ra_compuse.

  ra_compuse-sign     =  'I'.
  ra_compuse-option   =  'EQ'.
  ra_compuse-low      =  'CLC2'.
  APPEND ra_compuse.

  ra_compuse-sign     =  'I'.
  ra_compuse-option   =  'EQ'.
  ra_compuse-low      =  'CLC3'.
  APPEND ra_compuse.

  ra_compuse-sign     =  'I'.
  ra_compuse-option   =  'EQ'.
  ra_compuse-low      =  'CLC4'.
  APPEND ra_compuse.

  ra_compuse-sign     =  'I'.
  ra_compuse-option   =  'EQ'.
  ra_compuse-low      =  'CLC9'.
  APPEND ra_compuse.

  ra_compuse-sign     =  'I'.
  ra_compuse-option   =  'EQ'.
  ra_compuse-low      =  'CLF1'.
  APPEND ra_compuse.

  ra_compuse-sign     =  'I'.
  ra_compuse-option   =  'EQ'.
  ra_compuse-low      =  'CLF2'.
  APPEND ra_compuse.

  ra_compuse-sign     =  'I'.
  ra_compuse-option   =  'EQ'.
  ra_compuse-low      =  'CLF3'.
  APPEND ra_compuse.

  ra_compuse-sign     =  'I'.
  ra_compuse-option   =  'EQ'.
  ra_compuse-low      =  'CLF4'.
  APPEND ra_compuse.

  ra_compuse-sign     =  'I'.
  ra_compuse-option   =  'EQ'.
  ra_compuse-low      =  'CLF9'.
  APPEND ra_compuse.

**  End of changes by AHMADT FOR CHG0155278 on 08/05/2019

ENDFORM.                    "INITIALIZE_DATA
