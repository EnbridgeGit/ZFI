REPORT zffii030 MESSAGE-ID zs NO STANDARD PAGE HEADING
                              LINE-SIZE 130
                              LINE-COUNT 65.

*----------------------------------------------------------------------*
*  Author    : Adel D. Mendoza                    SAP : East
*  Date      : June, 2006                Program Type : Interface
*----------------------------------------------------------------------*
*  Issue Log : TR151
*----------------------------------------------------------------------*
*  Title : Mercator Replacement - IFFI058 Contrax Estimates Interface
*        :                      - IFFI058E Liberty Estimates Interface
*----------------------------------------------------------------------*
*  Issue Log : TR149
*----------------------------------------------------------------------*
*  Title : Mercator Replacement - IFFI053 Contrax Actuals Interface
*        :                      - IFFI053D Liberty DP Billing Interface
*----------------------------------------------------------------------*
*  Description:
*     This ABAP program will replace 4 Mercator mapping
*     programs that take an input file and creates a ZBSEG format
*     BDC session.
*     The new names for the interfaces and BDC sessions are as follows:
*
*     Old Name   Desc                New Name    New BDC name
*
*     IFFI053    Contrax Actuals     IFFI061A    ZFI-CTRX-REV
*     IFFI058    Contrax Estimates   IFFI061E    ZFI-CTRX-EST
*     IFFI053D   Liberty Actuals     IFFI062A    ZFI-DP-REV
*     IFFI058E   Liberty Estimates   IFFI062E    ZFI-DP-EST
*----------------------------------------------------------------------*
*  Changes:
* 2006/11/01 - gymana - Made coding corrections as a result of unit
*                       testing
* 2009/01/16 - gymana - TR580 - 6.0 SAP Upgrade
*                       Modified code to use ZKPAA005 BDC create ABAP.
*                       ZBSEG1-TBNAM now defined using table ZBSEG.
*                       20 byte offset removed from outrec when
*                       creating BBKPF record.
* 2011/05/13 - mkhan  - TR804 - Changes to recive flat data insteat of
*                               Tab Delimited in the input file.
* 2011/09/29 - gymana - TR804 - Change Plant from S299 to GSTH
* 2013/02/13 - gymana - SDP23185 = Modify error text to identify actuals
*                       or estimates.
* 2014/09/15 - gymana - SDP74189 - Add invoice number to input file
*                       layout.
* 2016/12/06 - gymana - acr-244 - Add rebill_yr, rebill_mth, and so_id
*                       fields to wa input file layout
* 2021/02/23 BIRUDURD COG changes to pass Aggregate Customer from
*                         selection screen.
* 2022/04/25 NAGIRIR CHG0247976 Populate Party ID At Text Field for    *
*                    D30K932171 Contrax & DP Postings                  *
* 2023/07/05 APPUKUTA RLSE0013046 D30K932536                           *
*                     Populate  i) Agreement ID -> Assignment And      *
*                              ii) Customer ID -> Text                 *
* 2023/07/31 APPUKUTA D30K932544   Move Agreement to Assignment field  *
* 2023/07/31 APPUKUTA D30K932546   Move Agreement to Assignment field II
*----------------------------------------------------------------------*

TABLES:  dd03l,                      "Table fields
         zfc01,                      "Contrax G/L Account lookup
         zfc02,                      "Contrax Rate Class lookup
         zfc03,                      "Contrax Service Class lookup
         zfc04,                      "Contrax Sales Org derivation
         zfc06,                      "Contrax Sector lookup
         zfb03,                      "Banner Town Code lookup
         zwacog,                     "WACOG Table
         zstconv.                    "Volume Conversion Table

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
        app_yr(04)             TYPE c,  "year transaction applied
        app_mth(02)            TYPE c,  "month transaction applied
        cust_id(08)            TYPE c,  "customer number
        rate_cl(12)            TYPE c,  "rate class
        serv_type(06)          TYPE c,  "service type
        serv_cl(02)            TYPE c,  "service class
        seas_cl(04)            TYPE c,  "seasonal class
        rate_type(04)          TYPE c,  "rate type
        charge_type(04)        TYPE c,  "charge type
        sr_usage(04)           TYPE c,  "sr usage
        st_subtype(06)         TYPE c,  "st subtype
        non_rate_item_typ(08)  TYPE c,  "non-rate item type
        tier_step_lvl(02)      TYPE c,  "tier step level
        sector_size(01)        TYPE c,  "sector size
        sector(06)             TYPE c,  "sector
        amount(16)             TYPE c,  "transaction amount
        volume(18)             TYPE c,  "transaction volume
        cust_type(01)          TYPE c,  "customer type
        geca_code(06)          TYPE c,  "geographic area
        vol_uom(08)            TYPE c,  "volume unit of measure
        sa_num(08)             TYPE c,  "contract number
        inv_num(08)            TYPE c,  "invoice number       SDP74189
        rebill_yr(4)           TYPE c,     "REBILL YEAR         ACR244
        rebill_mth(2)          TYPE c,     "REBILL MONTH        ACR244
        so_id(4)               TYPE c,     "SERVICE OFFERING ID ACR244
        cust_id1(8)            TYPE c.  " Added for Change D30K932171
DATA: END OF wa.
DATA: wa_err  LIKE wa OCCURS 0 WITH HEADER LINE.
DATA: contrax LIKE wa OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF isort OCCURS 0,
        izbd1p(02) TYPE n,                        "seqnum/docsize
        ikostl     LIKE zbseg-kostl,              "/
        ihkont     LIKE zbseg-hkont,              "G/L account
        inewbs     LIKE zbseg-newbs,              "40 or 50
        imatnr     LIKE zbseg-matnr,              "product
        iaufnr     LIKE zbseg-aufnr,              "/
        iwerks(04) TYPE c,                        "plant
        izuonr     LIKE zbseg-zuonr,              "assignment number
        ikndnr(10) TYPE c,                        "1
        ivkorg(04) TYPE c,                        "sales org
        iwwbrn(04) TYPE c,                        "branch
        iwwrat(03) TYPE c,                        "rate class
        iwwser(02) TYPE c,                        "service class
        iwwsct(01) TYPE c,                        "Z
        iwwseg(02) TYPE c,                        "/
        ibudat(08) TYPE c,
        ivolum(15) TYPE c,
        cust_id1(8) TYPE c. " Added for ChangeD30K932171
        INCLUDE STRUCTURE zbseg1.
DATA: END OF isort.
DATA: isum LIKE isort OCCURS 0 WITH HEADER LINE.

*data: begin of ierr occurs 0.
*        include structure zbseg1.
*data: end of ierr.

DATA: BEGIN OF z_bgr00.
        INCLUDE STRUCTURE bgr00.
DATA: END OF z_bgr00.

DATA: BEGIN OF z_bbkpf.
        INCLUDE STRUCTURE bbkpf.
DATA: END OF z_bbkpf.

DATA: va_amount  TYPE p DECIMALS 2 VALUE 0,
      va_amount2 TYPE p DECIMALS 2 VALUE 0,
      va_volume  TYPE p DECIMALS 3 VALUE 0,
      va_menge   TYPE p DECIMALS 3 VALUE 0.

DATA: wa_zbd1p     TYPE i,
      wa_zbd1p_num TYPE p DECIMALS 5 VALUE 0,
      wa_docsize   TYPE i,
      char(21)     TYPE c,
      nodata(1)    VALUE '/',
      wa_wacog     LIKE zwacog-wacog,
      wa_heatv     LIKE zstconv-factor.

data: va_sa_num(8) .      " <== Insert D30K932539

DATA: delimeter VALUE '09' TYPE x.
DATA: inrec(400), outrec(2000), infile(70), outfile(70).
DATA: wrk_symbolic(4) TYPE c VALUE '$sys'.

DATA: v_liberty TYPE char1,
      w_value TYPE tvarvc-low.

CONSTANTS: co_hkont(10) VALUE '0000302469',
           co_kostl(10) VALUE '0000020310'.

RANGES: ra_nonrat FOR contrax-non_rate_item_typ.

FIELD-SYMBOLS: <f1>.

*---------------------------------------------------------------------*
* selection screen
*---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK box0 WITH FRAME.
SELECTION-SCREEN BEGIN OF BLOCK box1 WITH FRAME TITLE text-100.

PARAMETERS: p_filein LIKE filename-fileextern OBLIGATORY DEFAULT
                  '/usr/sap/interfaces/$sys/CONTRAX/zctrxactgsacc.dat',

            p_fileot LIKE filename-fileextern OBLIGATORY DEFAULT
                     '/usr/sap/interfaces/$sys/CONTRAX/ctrx_act.sap'.

SELECTION-SCREEN SKIP.
PARAMETERS: p_group  LIKE bgr00-group OBLIGATORY DEFAULT
                     'ZFI-CTRX-REV'.

SELECTION-SCREEN SKIP.
PARAMETERS: p_tcode  LIKE tstc-tcode  OBLIGATORY DEFAULT 'FB01',
            p_blart  LIKE bbkpf-blart OBLIGATORY DEFAULT 'S6',
            p_bukrs  LIKE bbkpf-bukrs OBLIGATORY DEFAULT 'UGL',
            p_waers  LIKE bbkpf-waers OBLIGATORY DEFAULT 'CAD',
            p_xblnr  LIKE bbkpf-xblnr OBLIGATORY DEFAULT
                     'CONTRAX',
            p_bktxt  LIKE bbkpf-bktxt OBLIGATORY DEFAULT
                     'Contrax Revenue: Actual',
            p_kunnr  TYPE kna1-kunnr OBLIGATORY DEFAULT ''.  " COG

SELECTION-SCREEN SKIP.
PARAMETERS: p_dsize(4) TYPE c OBLIGATORY DEFAULT '499',
            p_aeidx(8) TYPE c OBLIGATORY DEFAULT 'ACTUAL'.

SELECTION-SCREEN END OF BLOCK box1.
SELECTION-SCREEN END OF BLOCK box0.

*----------------------------------------------------------------------
* AT SELECTION-SCREEN
*----------------------------------------------------------------------
AT SELECTION-SCREEN OUTPUT.
  REPLACE wrk_symbolic WITH sy-sysid INTO: p_filein, p_fileot.
  CONDENSE: p_filein NO-GAPS, p_fileot NO-GAPS.

*---------------------------------------------------------------------*
* start-of-selection
*---------------------------------------------------------------------*
START-OF-SELECTION.
  IF p_aeidx = 'ACTUAL'.
    PERFORM initialize_data.
  ENDIF.

* Start of change      COG
  CLEAR v_liberty.
  IF p_xblnr = 'LIBERTY'.
    v_liberty = 'X'.
  ENDIF.
* End of change        COG

  PERFORM get_wacog_hv_values.
  PERFORM open_files.
  PERFORM input_file_to_wa.
  PERFORM collect_data.

*  compare to zbis254e.vol file
*  loop at contrax.
*     TRANSFER contrax to P_FILEOT.
*  endloop.

  PERFORM xlatctrxest.
* compare to zbis254e1.tmp
*  loop at isort.
*     TRANSFER isort to P_FILEOT.
*  endloop.

  PERFORM summarize_data.

* compare to zbis254e2.tmp
*  loop at isum.
*     TRANSFER isum to P_FILEOT.
*  endloop.

  PERFORM create_output_file.

  CLOSE DATASET: p_filein, p_fileot.

  IF NOT wa_err[] IS INITIAL.
    PERFORM display_error_lst.
  ENDIF.


*---------------------------------------------------------------------*
* top of page
*---------------------------------------------------------------------*
TOP-OF-PAGE.
  FORMAT INTENSIFIED OFF.
  WRITE: /001 sy-datum,
          050 p_bktxt,
          117 'Page:', sy-pagno.

  WRITE: /    sy-uzeit UNDER sy-datum,
              '  Input File Errors' UNDER p_bktxt,
              sy-repid UNDER 'Page:'.

  WRITE: / sy-sysid UNDER sy-repid.

  FORMAT INTENSIFIED ON.

  WRITE: / sy-uline.
  WRITE: /002 'Date',
          011 'Cust',
          018 'Rate',
          031 'Serv',
          038 'Serv',
          051 'Seasonal',
          060 'Rate',
          065 'Charge',
          072 'Non Rate',
          085 'Transaction',
          103 'Transaction',
          123 'Contract'.

  WRITE: /001 'Applied',
          011 'No.',
          018 'Class',
          031 'Type',
          038 'Class',
          051 'Class',
          060 'Type',
          065 'Type',
          072 'Item Type',
          085 'Amount',
          103 'Volume',
          123 'Number'.
  WRITE: / sy-uline.
  FORMAT INTENSIFIED OFF.

*---------------------------------------------------------------------*
*  translate records
*---------------------------------------------------------------------*
FORM xlatctrxest.
  REFRESH: isort, wa_err.   "ierr

  LOOP AT contrax.
    CLEAR zfc01.
    SELECT SINGLE * FROM zfc01
       WHERE c_nrttyp  = contrax-non_rate_item_typ
       AND ( c_svctyp  = contrax-serv_type   OR c_svctyp  = '******' )
       AND ( c_custtyp = contrax-cust_type   OR c_custtyp = '*' )
       AND ( c_rtetyp  = contrax-rate_type   OR c_rtetyp  = '****' )
       AND ( c_chgtyp  = contrax-charge_type OR c_chgtyp  = '****' )
       AND ( c_sruse   = contrax-sr_usage    OR c_sruse   = '****' )
       AND ( c_stsub   = contrax-st_subtype  OR c_stsub   = '******' )
       AND ( c_sccode  = contrax-serv_cl     OR c_sccode  = '**' ).
    IF sy-subrc = 0.
      IF zfc01-c_stsub = 'SPOT'.
        MOVE '0000179016' TO zfc01-saknr.
      ENDIF.
      PERFORM f_contrax_to_bseg.
    ELSE.
      PERFORM f_contrax_error.
      MOVE-CORRESPONDING contrax TO wa_err.
      APPEND wa_err. CLEAR wa_err.
    ENDIF.
  ENDLOOP.

ENDFORM.                    "XLATCTRXEST

*---------------------------------------------------------------------
* gather all records
*---------------------------------------------------------------------
FORM f_contrax_to_bseg.
  CLEAR: wa_zbd1p, wa_docsize.
  PERFORM init_table_record.

  SHIFT contrax-sr_usage          LEFT DELETING LEADING space.
  SHIFT contrax-rate_cl           LEFT DELETING LEADING space.
  SHIFT contrax-non_rate_item_typ LEFT DELETING LEADING space.
  SHIFT contrax-serv_type         LEFT DELETING LEADING space.
  SHIFT contrax-vol_uom           LEFT DELETING LEADING space.

  MOVE p_dsize TO wa_docsize.
  wa_zbd1p_num = abs( sy-tabix / wa_docsize ).
  wa_zbd1p = trunc( wa_zbd1p_num ).

  MOVE wa_zbd1p    TO isort-izbd1p.

  PERFORM f_lookup_kostl USING contrax-rate_cl
                               contrax-non_rate_item_typ
                               contrax-geca_code
                      CHANGING isort-ikostl.

  IF isort-ikostl IS INITIAL.
    MOVE '/' TO isort-ikostl.
  ENDIF.
*Start of TR804 changes
  IF NOT zfc01-saknr IS INITIAL.
    IF ( contrax-non_rate_item_typ EQ 'CHQREQ' OR
       contrax-non_rate_item_typ EQ 'TRANSF' ).
* Start of changes    COG
*    concatenate 'CX' contrax-cust_id into isort-IHKONT.
      IF v_liberty = 'X'.
        CONCATENATE 'CX' contrax-cust_id INTO isort-ihkont.
      ELSE.
        isort-ihkont = contrax-cust_id.
      ENDIF.
* End of changes    COG
      CONDENSE isort-ihkont NO-GAPS.
    ELSE.
      MOVE zfc01-saknr TO isort-ihkont.
      CONDENSE isort-ihkont NO-GAPS.
    ENDIF.
    isort-cust_id1 = contrax-cust_id1. " Added for Change D30K932171
  ENDIF.
*End of TR804 changes

  CLEAR: va_amount, va_volume.
  MOVE: contrax-amount TO va_amount, contrax-volume TO va_volume.
  IF contrax-non_rate_item_typ EQ 'CHQREQ' OR              "TR804
     contrax-non_rate_item_typ EQ 'TRANSF'.                "TR804
    IF va_amount < 0 OR ( contrax-sr_usage = 'FUEL' AND va_volume < 0 ).
      isort-inewbs = '02'.
    ELSE.
      isort-inewbs = '11'.
    ENDIF.
  ELSE.                                                     "TR804
    IF va_amount < 0.                                       "TR804
      isort-inewbs = '40'.                                 "TR804
    ELSE.                                                   "TR804
      isort-inewbs = '50'.                                 "TR804
    ENDIF.                                                  "TR804
  ENDIF.

  IF NOT zfc01-matnr IS INITIAL.
    MOVE zfc01-matnr TO isort-imatnr.
  ENDIF.

  IF NOT zfc01-bd_g_ind IS INITIAL.
    PERFORM f_lookup_io USING contrax-geca_code
                     CHANGING isort-iaufnr.
  ELSE.
    MOVE '/' TO isort-iaufnr.
  ENDIF.

  PERFORM f_lookup_werks USING isort-imatnr
                               contrax-rate_cl
                               contrax-non_rate_item_typ
                               contrax-geca_code
                      CHANGING isort-iwerks.
* Following Lines Added D30K932536
Case p_xblnr .
  when 'ZRV' or 'ZAC' or 'ZES' .
* Following Lines Added D30K932546
      clear: va_sa_num .
      va_sa_num = contrax-sa_num .
      shift va_sa_num LEFT DELETING LEADING space .
* End of New Lines  D30K932546
*   MOVE contrax-so_id TO isort-izuonr .  <== Delete D30K932546
    MOVE va_sa_num TO isort-izuonr .    " <== Insert D30K932546
  when others .
* End of New Lines  D30K932536
  MOVE contrax-sa_num TO isort-izuonr.
 endcase .           " <== Insert D30K932536
  SHIFT isort-izuonr LEFT DELETING LEADING space.

* Following Lines Added D30K932536
Case p_xblnr .
  when 'ZRV' or 'ZAC' or 'ZES' .
* Following Lines Deleted D30K932546
** Following Lines Added D30K932544
*      clear: va_sa_num .
*      va_sa_num = contrax-sa_num .
*      shift va_sa_num LEFT DELETING LEADING space .
** End of New Lines  D30K932544
* End of Deletion  D30K932546
      PERFORM f_get_kndnr USING isort-imatnr
                            va_sa_num  " <== Insert D30K932544
*                      contrax-sa_num    <== Delete D30K932544
                   CHANGING isort-ikndnr .
  when others .
* End of New Lines  D30K932536
  PERFORM f_get_kndnr USING isort-imatnr
                      contrax-cust_id
                   CHANGING isort-ikndnr.
  endcase .        " <== Insert D30K932536

  PERFORM f_lookup_vkorg USING isort-imatnr
                               contrax-sector_size
                               contrax-rate_cl
                      CHANGING isort-ivkorg.

  PERFORM f_lookup_wwbrn USING isort-imatnr
                               contrax-geca_code
                      CHANGING isort-iwwbrn.

  PERFORM f_lookup_wwrat USING isort-imatnr
                               contrax-rate_cl
                               contrax-non_rate_item_typ
                               contrax-serv_type
                               contrax-seas_cl
                      CHANGING isort-iwwrat.

  PERFORM f_lookup_wwser USING isort-imatnr
                               contrax-rate_cl
                               contrax-non_rate_item_typ
                               contrax-cust_type
                      CHANGING isort-iwwser.

  PERFORM f_lookup_wwsct USING isort-imatnr
                               contrax-sector
                      CHANGING isort-iwwsct.

  PERFORM f_lookup_wwseg USING isort-imatnr
                      CHANGING isort-iwwseg.

  IF p_aeidx = 'ESTIMATE'.
    CONCATENATE contrax-app_yr contrax-app_mth '01'
       INTO isort-ibudat.
  ELSE.
    MOVE sy-datum TO isort-ibudat.
  ENDIF.

  PERFORM common_moves_to_isort.
  APPEND isort.

* -----------------------------------------
* offset line items
* -----------------------------------------
  PERFORM f_lookup_kostl2 USING contrax-non_rate_item_typ
                                isort-ikostl
                       CHANGING isort-ikostl.

  IF isort-ikostl IS INITIAL.
    MOVE '/' TO isort-ikostl.
  ENDIF.

*  if not zfc01-OACCT is initial.                      "TR804
*    move zfc01-OACCT to isort-IHKONT.                 "TR804
*  endif.                                              "TR804
* Start of changes    COG
*concatenate 'CX' contrax-cust_id into isort-IHKONT. "TR804
  IF v_liberty = 'X'.
    CONCATENATE 'CX' contrax-cust_id INTO isort-ihkont. "TR804
  ELSE.
    isort-ihkont = contrax-cust_id.
    isort-cust_id1 = contrax-cust_id1. " Added for Change D30K932171
  ENDIF.
* End of changes    COG
  CONDENSE isort-ihkont NO-GAPS.                      "TR804

  CLEAR: va_amount, va_volume.
  MOVE: contrax-amount TO va_amount, contrax-volume TO va_volume.
  IF va_amount < 0 OR ( contrax-sr_usage = 'FUEL' AND va_volume < 0 ).
*    isort-INEWBS = '50'.                              "TR804
    isort-inewbs = '11'.                               "TR804
  ELSE.
*    isort-INEWBS = '40'.                              "TR804
    isort-inewbs = '02'.                               "TR804
  ENDIF.

  MOVE: '/' TO isort-imatnr, '/' TO isort-iaufnr,
        '/' TO isort-iwerks, '/' TO isort-ikndnr,
        '/' TO isort-ivkorg, '/' TO isort-iwwbrn,
        '/' TO isort-iwwrat, '/' TO isort-iwwser,
        '/' TO isort-iwwsct, '/' TO isort-iwwseg.

  PERFORM common_moves_to_isort.
  IF NOT zfc01-omwskz IS INITIAL.
    MOVE zfc01-omwskz TO isort-mwskz.
  ELSE.
    MOVE '/' TO isort-mwskz.
  ENDIF.
  MOVE '/' TO isort-menge.
  MOVE '/' TO isort-meins.
  APPEND isort. CLEAR isort.

ENDFORM.                    "f_CONTRAX_TO_BSEG

*---------------------------------------------------------------------
* get KOSTL - Offset account
*---------------------------------------------------------------------
FORM f_lookup_kostl2 USING inonrat ikostl CHANGING ekostl.
  CLEAR ekostl.
  IF ( inonrat = 'SSOCF' OR inonrat = 'SPODF' OR inonrat = 'SDINT' ).
    MOVE '/' TO ekostl.
  ELSE.
    MOVE ikostl TO ekostl.
  ENDIF.
ENDFORM.                    "F_LOOKUP_KOSTL2

*---------------------------------------------------------------------
* COMMON MOVES TO ISORT
*---------------------------------------------------------------------
FORM common_moves_to_isort.
  MOVE '2'           TO isort-stype.
  MOVE 'ZBSEG'       TO isort-tbnam.

  MOVE  isort-inewbs  TO  isort-newbs.

  PERFORM f_compute_wrbtr USING contrax-amount
                                contrax-volume
                                contrax-sr_usage
                       CHANGING isort-wrbtr.

  IF NOT zfc01-mwskz IS INITIAL.
    MOVE  zfc01-mwskz   TO  isort-mwskz.
  ELSE.
    MOVE '/' TO isort-mwskz.
  ENDIF.

  MOVE  isort-ikostl  TO  isort-kostl.
  MOVE  isort-iaufnr  TO  isort-aufnr.
  MOVE  isort-imatnr  TO  isort-matnr.
  MOVE  isort-iwerks  TO  isort-werks.

  PERFORM f_compute_menge USING isort-imatnr
                                contrax-serv_type
                                contrax-sr_usage
                                contrax-vol_uom
                                contrax-volume
                                contrax-non_rate_item_typ
                       CHANGING isort-menge
                                isort-meins.

  CLEAR va_menge.
  MOVE isort-menge TO va_menge.
  IF isort-menge IS INITIAL OR va_menge = 0.
    MOVE '/' TO isort-menge.
  ENDIF.
  IF isort-meins IS INITIAL.
    MOVE '/' TO isort-meins.
  ENDIF.

*  perform F_GET_MEINS using isort-IMATNR
*                            contrax-SERV_TYPE
*                            contrax-SR_USAGE
*                            contrax-VOLUME
*                            contrax-RATE_CL
*                            contrax-NON_RATE_ITEM_TYP
*                   changing isort-MEINS.

  MOVE  isort-izuonr  TO  isort-zuonr.

*{Begin of Change D30K932171
  IF v_liberty = 'X'.
    MOVE  p_bktxt       TO  isort-sgtxt.
  ELSE.
    MOVE isort-cust_id1 TO isort-sgtxt.
  ENDIF.
*End of Change D30K932171 }
*  move  isort-IZBD1P  to  isort-ZBD1P.
  MOVE  isort-ihkont  TO  isort-hkont.

  MOVE  isort-ikndnr  TO  isort-kndnr.
  MOVE  isort-ivkorg  TO  isort-vkorg.
  MOVE  isort-iwwbrn  TO  isort-wwbrn.
  MOVE  isort-iwwsct  TO  isort-wwsct.
  MOVE  isort-iwwseg  TO  isort-wwseg.
  MOVE  isort-iwwrat  TO  isort-wwrat.
  MOVE  isort-iwwser  TO  isort-wwser.
  MOVE  isort-imatnr  TO  isort-artnr.

  CLEAR: isort-wwrgn.

ENDFORM.                    "COMMON_MOVES_TO_ISORT

*---------------------------------------------------------------------
* get MEINS - Not used but saved as reference to mercator logic
*---------------------------------------------------------------------
*form F_GET_MEINS using Imatnr  Isrvtyp Isrusage
*                       Ivolume Iratecl Inonrat  changing Emeins.
*  clear Emeins.
*  if iMATNR <> '/' and ( iSRVTYP <> 'STORE' or iSRUSAGE = 'FUEL' ).
*    if iVOLUME <> 0.
*      if iRATECL is initial.
*        if iNONRAT = 'M7TOTINV'.
*          move 'M3' to Emeins.
*        endif.
*      else.
*        move 'M3' to Emeins.
*      endif.
*    endif.
*  endif.
*  if Emeins is initial.
*    move '/' to Emeins.
*  endif.
*endform.

*---------------------------------------------------------------------
* get MENGE
*---------------------------------------------------------------------
FORM f_compute_menge USING imatnr  isrvtyp isrusage
                           ivoluom ivolume inonrat
                  CHANGING emenge emeins.
  CLEAR: emenge, emeins.
  IF imatnr <> '/' AND ( isrvtyp <> 'STORE' OR isrusage = 'FUEL' ) OR
    ( inonrat IN ra_nonrat ).
    IF ivoluom = 'CUST'.
    ELSE.
      PERFORM f_convert_uom USING ivoluom ivolume inonrat
                            CHANGING emenge emeins.
    ENDIF.
  ENDIF.
ENDFORM.                    "F_COMPUTE_MENGE

*---------------------------------------------------------------------
* convert UOM - convert GJ / MJ to M3
*---------------------------------------------------------------------
FORM f_convert_uom USING ivoluom ivolume inonrat
                CHANGING emenge emeins.
  DATA: wa_menge      TYPE p DECIMALS 3.
  DATA  wa_heatv_mj   LIKE zstconv-factor.

  CLEAR: emenge, emeins.

  IF ( isort-ihkont+4(6) = '152005' OR
       isort-ihkont+4(6) = '152100' OR
       isort-ihkont+4(6) = '152500' OR
       isort-ihkont+4(6) = '152600' ).
    MOVE ivolume TO wa_menge.
    MOVE ivoluom TO emeins.
  ELSE.
    MOVE 'M3' TO emeins.
    IF ivoluom = 'GJ'.
      wa_heatv_mj = ( wa_heatv / 1000 ).
      wa_menge = ( ivolume / wa_heatv_mj ).
    ELSEIF ivoluom = 'MJ'.
      wa_menge = ( ivolume / wa_heatv ).
    ELSE.
      MOVE ivolume TO wa_menge.
    ENDIF.
  ENDIF.

  IF wa_menge = 0.
    MOVE '/' TO emeins.
  ENDIF.

  MOVE wa_menge TO emenge.
* remove the neg. sign from the volume before passing the value back.

  TRANSLATE emenge USING '- '.
  SHIFT emenge RIGHT DELETING TRAILING space.
ENDFORM.                    "F_CONVERT_UOM

*---------------------------------------------------------------------
* compute WRBTR
*---------------------------------------------------------------------
FORM f_compute_wrbtr USING iamount ivolume isrusage CHANGING ewrbtr.
  DATA: wa_wrbtr    TYPE p DECIMALS 2.
  DATA: wa_wacog_mj LIKE zwacog-wacog.
  DATA: wa_wrbtr2(16) TYPE c.

  CLEAR ewrbtr.
  IF isrusage = 'FUEL'.
    wa_wacog_mj = ( wa_wacog / 1000 ).
    wa_wrbtr = ( ivolume * wa_wacog_mj ).
  ELSE.
    MOVE iamount TO wa_wrbtr.
  ENDIF.

* remove the negative sign from the amount before passing the value
* back. +ve / -ve is identified by the 40/50 newbs posting key.

  MOVE wa_wrbtr TO ewrbtr.
  TRANSLATE ewrbtr USING '- '.
  SHIFT ewrbtr RIGHT DELETING TRAILING space.


ENDFORM.                    "F_COMPUTE_WRBTR

*---------------------------------------------------------------------
* get WWSEG
*---------------------------------------------------------------------
FORM f_lookup_wwseg USING imatnr CHANGING ewwseg.
  CLEAR ewwseg.
  IF imatnr = '/'.
    MOVE '/' TO ewwseg.
  ELSE.
    MOVE 'ZZ' TO ewwseg.
  ENDIF.
ENDFORM.                    "F_LOOKUP_WWSEG

*---------------------------------------------------------------------
* get WWSCT
*---------------------------------------------------------------------
FORM f_lookup_wwsct USING imatnr isector CHANGING ewwsct.
  CLEAR ewwsct.
  IF imatnr = '/'.
    MOVE '/' TO ewwsct.
  ELSE.
    SELECT SINGLE * FROM zfc06 WHERE c_sector = isector.
    IF sy-subrc = 0.
      MOVE zfc06-wwsct TO ewwsct.
    ELSE.
      MOVE 'Z' TO ewwsct.
    ENDIF.
  ENDIF.
ENDFORM.                    "F_LOOKUP_WWSCT

*---------------------------------------------------------------------
* get WWSER
*---------------------------------------------------------------------
FORM f_lookup_wwser USING imatnr iratecl inonrat icustyp
                 CHANGING ewwser.
  CLEAR ewwser.
  IF imatnr = '/'.
    MOVE '/' TO ewwser.
  ELSE.
    IF iratecl IS INITIAL.
      IF inonrat = 'M7TOTINV'.
        MOVE '30' TO ewwser.
      ELSE.
        MOVE '/' TO ewwser.
      ENDIF.
    ELSE.
      IF ( iratecl = 'N/A' OR iratecl = 'R1' OR iratecl = 'U2' OR
         iratecl = 'S1' ).
        MOVE '/' TO ewwser.
      ELSE.
        SELECT SINGLE * FROM zfc02 WHERE c_ratecl = iratecl.
        IF sy-subrc = 0.
          SELECT SINGLE * FROM zfc03 WHERE c_custyp = icustyp
                                       AND bukrs    = zfc02-bukrs.
          IF sy-subrc = 0.
            MOVE zfc03-wwser TO ewwser.
          ENDIF.
        ENDIF.
        IF ewwser IS INITIAL.
          CONCATENATE '*' icustyp INTO ewwser.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    "F_LOOKUP_WWSER

*---------------------------------------------------------------------
* get WWRAT
*---------------------------------------------------------------------
FORM f_lookup_wwrat USING imatnr iratecl inonrat isrvtyp iseacla
                 CHANGING ewwrat.
  CLEAR ewwrat.
  IF imatnr = '/' OR iratecl = 'N/A'.
    MOVE '/' TO ewwrat.
  ELSE.
    IF iratecl IS INITIAL.
      IF inonrat = 'M7TOTINV'.
        MOVE 'M7' TO ewwrat.
      ELSEIF inonrat = 'GS-CT'.
        MOVE 'T-4' TO ewwrat.
      ELSE.
        MOVE '/' TO ewwrat.
      ENDIF.
    ELSE.
      SELECT SINGLE * FROM zfc02
        WHERE c_ratecl = iratecl
          AND ( c_svctyp = isrvtyp OR c_svctyp = '******' )
          AND ( c_seascl = iseacla OR c_seascl = '****' ).
      IF sy-subrc = 0.
        MOVE zfc02-wwrat TO ewwrat.
      ELSE.
        MOVE iratecl(3) TO ewwrat.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    "F_LOOKUP_WWRAT

*---------------------------------------------------------------------
* get WWBRN
*---------------------------------------------------------------------
FORM f_lookup_wwbrn USING imatnr igeca CHANGING ewwbrn.
  CLEAR ewwbrn.
  IF imatnr = '/'.
    MOVE '/' TO ewwbrn.
  ELSE.
    IF NOT igeca IS INITIAL.
      SELECT SINGLE * FROM zfb03 WHERE towncode = igeca+0(2)
                                   AND municode = igeca+2(4).
      IF sy-subrc = 0.
        MOVE zfb03-wwbrn TO ewwbrn.
      ELSE.
        MOVE igeca TO ewwbrn.
      ENDIF.
    ELSE.
      MOVE 'UNKN' TO ewwbrn.
    ENDIF.
  ENDIF.
ENDFORM.                    "F_LOOKUP_WWBRN

*---------------------------------------------------------------------
* get VKORG
*---------------------------------------------------------------------
FORM f_lookup_vkorg USING imatnr isecsiz iratecl CHANGING evkorg.
  CLEAR evkorg.
*Start of TR804 changes
  IF imatnr = '/'.
    MOVE '/' TO evkorg.
  ELSE.
    MOVE 'Z002' TO evkorg.
  ENDIF.
*    select single * from ZFC02 where C_RATECL = Iratecl.
*    if sy-subrc = 0.
*      select single * from ZFC04 where C_SECSIZ = Isecsiz
*                                   and BUKRS    = zfc02-bukrs.
*      if sy-subrc = 0.
*        move ZFC04-VKORG to Evkorg.
*      endif.
*    endif.
*    if eVKORG is initial.
*      move 'U020' to eVKORG.
*    endif.
*  endif.
*End of TR804 changes
ENDFORM.                    "F_LOOKUP_VKORG

*---------------------------------------------------------------------
* get KNDNR
*---------------------------------------------------------------------
FORM f_get_kndnr USING imatnr icustid CHANGING ekndnr.
  DATA: wa_custid LIKE contrax-cust_id.
  CLEAR ekndnr.
  IF imatnr = '/'.
    MOVE '/' TO ekndnr.
  ELSE.
    MOVE icustid TO wa_custid.
    SHIFT wa_custid LEFT DELETING LEADING space.
*Start of Change COG
    IF v_liberty = 'X'.
      CONCATENATE 'CX' wa_custid INTO ekndnr.
    ELSE.
      MOVE wa_custid TO ekndnr.
    ENDIF.
*End of Change COG
    CONDENSE ekndnr NO-GAPS.
  ENDIF.
ENDFORM.                    "F_GET_KNDNR

*---------------------------------------------------------------------
* get plant
*---------------------------------------------------------------------
FORM f_lookup_werks USING imatnr iratecl inonrat igeca
                 CHANGING ewerks.
  CLEAR ewerks.
  IF imatnr = '/' OR ( iratecl IS INITIAL AND inonrat <> 'GS-CT' ).
    MOVE '/' TO ewerks.
  ELSE.
    IF NOT igeca IS INITIAL.
      SELECT SINGLE * FROM zfb03 WHERE towncode = igeca+0(2)
                                   AND municode = igeca+2(4).
      IF sy-subrc = 0.
        MOVE zfb03-werks TO ewerks.
      ENDIF.
    ELSE.
      MOVE 'GSTH' TO ewerks.
    ENDIF.
  ENDIF.
  IF ewerks IS INITIAL.
    MOVE '/' TO ewerks.
  ENDIF.
ENDFORM.                    "F_LOOKUP_WERKS

*---------------------------------------------------------------------
* get internal order
*---------------------------------------------------------------------
FORM f_lookup_io USING igeca CHANGING eaufnr.
  CLEAR eaufnr.
  SELECT SINGLE * FROM zfb03 WHERE towncode = igeca+0(2)
                               AND municode = igeca+2(4).
  IF sy-subrc = 0.
    MOVE zfb03-bd_io_gas TO eaufnr.
  ELSE.
    CONCATENATE '*' igeca '*' INTO eaufnr.
  ENDIF.
  IF eaufnr = '**'.
    MOVE '/' TO eaufnr.
  ENDIF.
ENDFORM.                    "F_LOOKUP_IO

*---------------------------------------------------------------------
* get cost center
*---------------------------------------------------------------------
FORM f_lookup_kostl USING iratecl inonrat igeca CHANGING ekostl.
  CLEAR ekostl.
  IF ( iratecl = '' AND
     ( inonrat = 'AOC' OR inonrat = 'TTF' OR inonrat = 'HSTDATA' ) ).

    PERFORM f_lookup_cc USING igeca CHANGING ekostl.

*  elseif ( iNONRAT = 'SSOCF' or iNONRAT = 'SPODF' or    "SDP54795
*           iNONRAT = 'SDINT' ).                         "SDP54795
  ELSEIF   inonrat = 'SDINT'.                               "SDP54795
    MOVE '0000020310' TO ekostl.
  ELSE.
    MOVE '/' TO ekostl.
  ENDIF.
ENDFORM.                    "F_LOOKUP_KOSTL

*---------------------------------------------------------------------
* lookup cost center
*---------------------------------------------------------------------
FORM f_lookup_cc USING igeca CHANGING ekostl.
  CLEAR ekostl.
  SELECT SINGLE * FROM zfb03 WHERE towncode = igeca+0(2)
                               AND municode = igeca+2(4).
  IF sy-subrc = 0.
    MOVE zfb03-cskostl TO ekostl.
  ELSE.
    CONCATENATE '*' igeca '*' INTO ekostl.
  ENDIF.
ENDFORM.                    "F_LOOKUP_CC

*---------------------------------------------------------------------
* gather all records with error
*---------------------------------------------------------------------
FORM f_contrax_error.
  CLEAR: wa_zbd1p, wa_docsize.
  PERFORM init_table_record.

  SHIFT contrax-sr_usage LEFT DELETING LEADING space.

  MOVE p_dsize TO wa_docsize.
  wa_zbd1p = abs( sy-tabix / wa_docsize ).
  MOVE wa_zbd1p    TO isort-izbd1p.
  MOVE '2'           TO isort-stype.
  MOVE 'ZBSEG'       TO isort-tbnam.

  IF p_aeidx = 'ESTIMATE'.
    CONCATENATE contrax-app_yr contrax-app_mth '01'
       INTO isort-ibudat.
  ELSE.
    MOVE sy-datum TO isort-ibudat.
  ENDIF.

  CLEAR: va_amount, va_volume.
  MOVE: contrax-amount TO va_amount, contrax-volume TO va_volume.
  IF va_amount < 0 OR ( contrax-sr_usage = 'FUEL' AND va_volume < 0 ).
    isort-newbs = '40'.                              "TR804
  ELSE.
    isort-newbs = '50'.                              "TR804
  ENDIF.
  MOVE isort-newbs TO isort-inewbs.

  isort-wrbtr = contrax-amount.
  TRANSLATE isort-wrbtr USING '- '.
  SHIFT isort-wrbtr RIGHT DELETING TRAILING space.

  isort-zuonr = contrax-sa_num.
  SHIFT isort-zuonr LEFT DELETING LEADING space.
  MOVE isort-zuonr TO isort-izuonr.

  CLEAR inrec.
  CONCATENATE contrax-rate_cl '/' contrax-serv_type '/'
    contrax-non_rate_item_typ INTO inrec.
  IF p_aeidx = 'ESTIMATE'.                                  "SDP23185
    CONCATENATE 'Contrax Rev Est Error:' inrec INTO inrec   "SDP23185
                 SEPARATED BY space.                        "SDP23185
  ELSE.                                                     "SDP23185
    CONCATENATE 'Contrax Rev Act Error:' inrec INTO inrec   "SDP23185
                 SEPARATED BY space.                        "SDP23185
  ENDIF.                                                    "SDP23185
*{Begin of Change D30K932171
  isort-cust_id1 = contrax-cust_id1.
  IF v_liberty = 'X'.
    isort-sgtxt = inrec.
  ELSE.

    MOVE isort-cust_id1 TO isort-sgtxt.
  ENDIF.
*End of Change D30K932171 }
  SELECT SINGLE * FROM zfc01 WHERE c_svctyp = 'ERROR'.
  isort-hkont = zfc01-saknr.                         "TR804
*  concatenate 'CX' contrax-cust_id into isort-HKONT.  "TR804
*  condense isort-HKONT.                               "TR804
  MOVE isort-hkont TO isort-ihkont.
  APPEND isort.

  IF va_amount < 0 OR ( contrax-sr_usage = 'FUEL' AND va_volume < 0 ).
    isort-newbs = '11'.
  ELSE.
    isort-newbs = '02'.
  ENDIF.
  MOVE isort-newbs TO isort-inewbs.
* Start of Changes   COG
*  concatenate 'CX' contrax-cust_id into isort-HKONT.  "TR804
  IF v_liberty = 'X'.
    CONCATENATE 'CX' contrax-cust_id INTO isort-hkont.  "TR804
  ELSE.
    isort-hkont = contrax-cust_id.
  ENDIF.
* End of Changes   COG
  CONDENSE isort-hkont NO-GAPS.
*  isort-HKONT = zfc01-OACCT.

  MOVE isort-hkont TO isort-ihkont.
  CLEAR: isort-wwrgn.
  APPEND isort. CLEAR isort.

ENDFORM.                    "f_CONTRAX_ERROR

*---------------------------------------------------------------------*
*  Used to initialize the record to '/' - for internal table ISORT
*---------------------------------------------------------------------*
FORM init_table_record.
  DATA: vf_index TYPE i VALUE 1.
  WHILE vf_index <= 88.
    ASSIGN COMPONENT vf_index OF STRUCTURE isort TO <f1>.
    <f1> = nodata.
    ADD 1 TO vf_index.
  ENDWHILE.
ENDFORM.                    "init_table_record

**---------------------------------------------------------------------*
**  Used to initialize the record to '/' - for internal table IERR
**---------------------------------------------------------------------*
*form init_table_record2.
*  data: vf_index type i value 1.
*  while vf_index <= 68.
*    ASSIGN COMPONENT vf_index OF STRUCTURE iERR TO <F1>.
*    <F1> = nodata.
*    add 1 to vf_index.
*  endwhile.
*endform.

*---------------------------------------------------------------------*
* collect data
*---------------------------------------------------------------------*
FORM collect_data.
*- prepare records table
  REFRESH: contrax.
*- collect data
  LOOP AT wa.
    SHIFT wa-amount   LEFT DELETING LEADING space.
    SHIFT wa-volume   LEFT DELETING LEADING space.
    SHIFT wa-sr_usage LEFT DELETING LEADING space.
* Start of Changes    COG
    IF v_liberty IS INITIAL.
      wa-cust_id1 = wa-cust_id. " Added for Change D30K932171
      CONDENSE wa-cust_id1.     " Added for Change D30K932171
      wa-cust_id = p_kunnr. "COG
    ENDIF.
* End of Changes      COG
    CLEAR: va_amount, va_amount2, va_volume.
    MOVE: wa-amount TO va_amount, wa-volume TO va_volume.

    IF va_amount <> 0 OR va_volume <> 0.
      IF ( ( ( va_amount > 0 AND va_volume >= 0 ) OR
            ( va_amount < 0 AND va_volume <= 0 ) ) AND
             va_amount <> 0 ) OR wa-sr_usage = 'FUEL'.
*-       single record
        MOVE-CORRESPONDING wa TO contrax.
        APPEND contrax. CLEAR contrax.
      ELSE.
*-       double record
        PERFORM double_record.
        MOVE-CORRESPONDING wa TO contrax.
        MOVE '0.00' TO contrax-volume.
        APPEND contrax. CLEAR contrax.

*-       double record 2
        PERFORM double_record2.
        MOVE-CORRESPONDING wa TO contrax.
        APPEND contrax. CLEAR contrax.
      ENDIF.
    ENDIF.
  ENDLOOP.
*   contrax1[] = contrax.
ENDFORM.                    "collect_data

*-----------------------------------------------------------------------
* Double Record
*-----------------------------------------------------------------------
FORM double_record.
  IF ( va_amount > 0 OR ( va_amount = 0 AND va_volume < 0 ) ).
    va_amount2 = va_amount + '0.01'.
  ELSE.
    va_amount2 = va_amount - '0.01'.
  ENDIF.
  MOVE va_amount2 TO wa-amount.
ENDFORM.                    "DOUBLE_RECORD

*-----------------------------------------------------------------------
* Double Record 2
*-----------------------------------------------------------------------
FORM double_record2.
  IF ( va_amount > 0 OR ( va_amount = 0 AND va_volume < 0 ) ).
    wa-amount = '-0.01'.
  ELSE.
    wa-amount = '0.01'.
  ENDIF.
ENDFORM.                    "DOUBLE_RECORD2

*---------------------------------------------------------------------*
* sum data
*---------------------------------------------------------------------*
FORM summarize_data.
  DATA: va_menge TYPE p DECIMALS 3 VALUE 0,
        va_wrbtr TYPE p DECIMALS 2 VALUE 0,
        va_volume(18) TYPE c.


*- prepare table summary
  REFRESH isum.

*- sort data
  SORT isort BY izbd1p ikostl ihkont inewbs imatnr iaufnr
                iwerks izuonr ikndnr ivkorg iwwbrn iwwrat
                iwwser iwwsct iwwseg.

* Compare to zbis254e.srt
*  loop at isort.
*     TRANSFER isort to P_FILEOT.
*  endloop.

*- summarize data
  LOOP AT isort.

    IF isort-hkont = co_hkont.
      MOVE co_kostl TO isort-kostl.
    ENDIF.

    READ TABLE isum WITH KEY izbd1p = isort-izbd1p
                             ikostl = isort-ikostl
                             ihkont = isort-ihkont
                             inewbs = isort-inewbs
                             imatnr = isort-imatnr
                             iaufnr = isort-iaufnr
                             iwerks = isort-iwerks
                             izuonr = isort-izuonr
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
*  no match in isum. Add isort to isum.
      IF isort-wrbtr = '0' AND isort-menge = '/'.
      ELSE.
        MOVE-CORRESPONDING isort TO isum.
        APPEND isum. CLEAR isum.
      ENDIF.
    ENDIF.
  ENDLOOP.

*-----------------------------------------------------------------
* Original Mercator logic - not used (TR078)
* - these accounts will now post volumes in GJ
*-----------------------------------------------------------------
*  IF P_AEIDX = 'ACTUAL'.
*    loop at isum.
*      if ( isum-HKONT+4(6) = '152005' or
*           isum-HKONT+4(6) = '152100' or
*           isum-HKONT+4(6) = '152500' or
*           isum-HKONT+4(6) = '152600' ).
*         move '/' to isum-meins.
*         move isum-MENGE to va_volume.
*         shift va_volume left deleting leading space.
*         concatenate P_BKTXT 'Vol:' va_volume 'GJ' into isum-SGTXT
*           separated by space.
*         move '/' to isum-menge.
*         modify isum index sy-tabix transporting SGTXT MENGE MEINS.
*      endif.
*
*      if ( isort-MENGE = '/' and isort-MEINS <> 'M3' )
*         move '/' to isum-meins.
*         move '/' to isum-menge.
*         modify isum index sy-tabix transporting SGTXT MENGE MEINS.
*      endif.
*
*    endloop.
*  ENDIF.

ENDFORM.                    "summarize_data

*---------------------------------------------------------------------*
* create output file
*---------------------------------------------------------------------*
FORM create_output_file.
  DATA: mmenge TYPE p DECIMALS 3.
  DATA: switch TYPE i VALUE 0.

  PERFORM init_structures USING 'BGR00'.
  MOVE '0'              TO z_bgr00-stype.
  MOVE p_group          TO z_bgr00-group.
  MOVE sy-mandt         TO z_bgr00-mandt.
  MOVE 'BATCH'          TO z_bgr00-usnam.
  TRANSFER z_bgr00 TO p_fileot LENGTH 38.

  LOOP AT isum.
    AT NEW izbd1p.
      switch = 0.
    ENDAT.
    IF switch = 0.
      switch = 1.

      PERFORM init_structures USING 'BBKPF'.
      MOVE '1'              TO z_bbkpf-stype.
      MOVE p_tcode          TO z_bbkpf-tcode.
      MOVE sy-datum         TO z_bbkpf-bldat.
      MOVE p_blart          TO z_bbkpf-blart.
      MOVE p_bukrs          TO z_bbkpf-bukrs.
      MOVE isum-ibudat      TO z_bbkpf-budat.
      MOVE p_waers          TO z_bbkpf-waers.
      MOVE p_xblnr          TO z_bbkpf-xblnr.
      MOVE p_bktxt          TO z_bbkpf-bktxt.

      CLEAR: z_bbkpf-numpg, z_bbkpf-stgrd, z_bbkpf-kursf_m,
             z_bbkpf-augtx, z_bbkpf-xprfg, z_bbkpf-sende  ,
             outrec.

      MOVE z_bbkpf(216) TO outrec.
      TRANSFER outrec TO p_fileot LENGTH 284.
    ENDIF.

    CLEAR zbseg1.
    MOVE-CORRESPONDING isum TO zbseg1.
    TRANSLATE zbseg1-wrbtr USING '- '.
    TRANSLATE zbseg1-menge USING '- '.
    SHIFT zbseg1-wrbtr RIGHT DELETING TRAILING space.
    IF zbseg1-menge CA '/'.
    ELSE.
      MOVE zbseg1-menge TO mmenge.
      CLEAR zbseg1-menge.
      MOVE mmenge TO zbseg1-menge.
      SHIFT zbseg1-menge RIGHT DELETING TRAILING space.
    ENDIF.
    TRANSFER zbseg1 TO p_fileot LENGTH 703.
  ENDLOOP.
ENDFORM.                    "create_output_file

*-----------------------------------------------------------------------
* Report: Display Error List
*-----------------------------------------------------------------------
FORM display_error_lst.
  LOOP AT wa_err.
    WRITE: /001 wa_err-app_yr,
            006 wa_err-app_mth,
            009 wa_err-cust_id,
            018 wa_err-rate_cl,
            031 wa_err-serv_type,
            038 wa_err-serv_cl,
            051 wa_err-seas_cl,
            060 wa_err-rate_type,
            065 wa_err-charge_type,
            072 wa_err-non_rate_item_typ,
            085 wa_err-amount,
            103 wa_err-volume,
            123 wa_err-sa_num,
            132 wa_err-inv_num.
  ENDLOOP.

ENDFORM.                    "DISPLAY_ERROR_LST

*-----------------------------------------------------------------------
*  This routine reads all the records from the input area, and adds
*  them, one-by-one, to the internal work table (wa), separating
*  the record into its various fields.
*-----------------------------------------------------------------------
FORM input_file_to_wa.
  DO.
    CLEAR: wa, inrec.
    READ DATASET p_filein INTO wa.               "TR804
*    read dataset p_filein into INREC.           "TR804
**    if sy-index < 2.
**    elseif ( INREC is initial or sy-subrc <> 0 ).
    IF sy-subrc <> 0.
      EXIT.
    ELSE.
*      split inrec at delimeter into                             "TR804
*        wa-APP_YR         wa-APP_MTH      wa-CUST_ID            "TR804
*        wa-RATE_CL        wa-SERV_TYPE    wa-SERV_CL            "TR804
*        wa-SEAS_CL        wa-RATE_TYPE    wa-CHARGE_TYPE        "TR804
*        wa-SR_USAGE       wa-ST_SUBTYPE   wa-NON_RATE_ITEM_TYP  "TR804
*        wa-TIER_STEP_LVL  wa-SECTOR_SIZE  wa-SECTOR             "TR804
*        wa-AMOUNT         wa-VOLUME       wa-CUST_TYPE          "TR804
*        wa-GECA_CODE      wa-VOL_UOM      wa-SA_NUM.            "TR804
      APPEND wa.
    ENDIF.
  ENDDO.
ENDFORM.                    "INPUT_FILE_TO_WA

*---------------------------------------------------------------------*
*  Used to initialize the record to '/' - FOR BGR00, BBKPF
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

*-----------------------------------------------------------------------
*  Routine to open the physical file to determine if there are any
*  errors reading it.
*-----------------------------------------------------------------------
FORM open_files.
  DATA: msg(100).
*-----------------------------------------------------------------------
  OPEN DATASET p_filein FOR INPUT IN TEXT MODE MESSAGE msg.
  IF ( sy-subrc <> 0 ).
    MESSAGE e002 WITH infile msg.
  ENDIF.
*-----------------------------------------------------------------------
  OPEN DATASET p_fileot FOR OUTPUT IN TEXT MODE MESSAGE msg.
  IF ( sy-subrc <> 0 ).
    MESSAGE e002 WITH outfile msg.
  ENDIF.
*-----------------------------------------------------------------------
*  open dataset p_fileer for output in text mode message msg.
*  if ( sy-subrc <> 0 ).
*    message E002 with outfile msg.
*  endif.
ENDFORM.                    "OPEN_FILES

*-----------------------------------------------------------------------
*  Initialize data
*-----------------------------------------------------------------------
FORM initialize_data.
  REFRESH: ra_nonrat.

  ra_nonrat-sign     =  'I'.
  ra_nonrat-option   =  'EQ'.
  ra_nonrat-low      =  'DCCREC'.
  APPEND ra_nonrat.

  ra_nonrat-sign     =  'I'.
  ra_nonrat-option   =  'EQ'.
  ra_nonrat-low      =  'DPTRANS'.
  APPEND ra_nonrat.

  ra_nonrat-sign     =  'I'.
  ra_nonrat-option   =  'EQ'.
  ra_nonrat-low      =  'TTFEE'.
  APPEND ra_nonrat.

  ra_nonrat-sign     =  'I'.
  ra_nonrat-option   =  'EQ'.
  ra_nonrat-low      =  'IMTFEE'.
  APPEND ra_nonrat.

  ra_nonrat-sign     =  'I'.
  ra_nonrat-option   =  'EQ'.
  ra_nonrat-low      =  'DCCREVAD'.
  APPEND ra_nonrat.

  ra_nonrat-sign     =  'I'.
  ra_nonrat-option   =  'EQ'.
  ra_nonrat-low      =  'SPOCNW'.
  APPEND ra_nonrat.

  ra_nonrat-sign     =  'I'.
  ra_nonrat-option   =  'EQ'.
  ra_nonrat-low      =  'SPOD'.
  APPEND ra_nonrat.

  ra_nonrat-sign     =  'I'.
  ra_nonrat-option   =  'EQ'.
  ra_nonrat-low      =  'SPODF'.
  APPEND ra_nonrat.

  ra_nonrat-sign     =  'I'.
  ra_nonrat-option   =  'EQ'.
  ra_nonrat-low      =  'SPODN'.
  APPEND ra_nonrat.

  ra_nonrat-sign     =  'I'.
  ra_nonrat-option   =  'EQ'.
  ra_nonrat-low      =  'SPODNP'.
  APPEND ra_nonrat.

  ra_nonrat-sign     =  'I'.
  ra_nonrat-option   =  'EQ'.
  ra_nonrat-low      =  'SPODNW'.
  APPEND ra_nonrat.

  ra_nonrat-sign     =  'I'.
  ra_nonrat-option   =  'EQ'.
  ra_nonrat-low      =  'SPODSP'.
  APPEND ra_nonrat.

  ra_nonrat-sign     =  'I'.
  ra_nonrat-option   =  'EQ'.
  ra_nonrat-low      =  'SPODSW'.
  APPEND ra_nonrat.

  ra_nonrat-sign     =  'I'.
  ra_nonrat-option   =  'EQ'.
  ra_nonrat-low      =  'SPODW'.
  APPEND ra_nonrat.

  ra_nonrat-sign     =  'I'.
  ra_nonrat-option   =  'EQ'.
  ra_nonrat-low      =  'SPODWW'.
  APPEND ra_nonrat.

  ra_nonrat-sign     =  'I'.
  ra_nonrat-option   =  'EQ'.
  ra_nonrat-low      =  'SSOC'.
  APPEND ra_nonrat.

  ra_nonrat-sign     =  'I'.
  ra_nonrat-option   =  'EQ'.
  ra_nonrat-low      =  'SSOCF'.
  APPEND ra_nonrat.

  ra_nonrat-sign     =  'I'.
  ra_nonrat-option   =  'EQ'.
  ra_nonrat-low      =  'SSOCNW'.
  APPEND ra_nonrat.

  ra_nonrat-sign     =  'I'.
  ra_nonrat-option   =  'EQ'.
  ra_nonrat-low      =  'SSOCN'.
  APPEND ra_nonrat.

  ra_nonrat-sign     =  'I'.
  ra_nonrat-option   =  'EQ'.
  ra_nonrat-low      =  'SSOCNP'.
  APPEND ra_nonrat.

  ra_nonrat-sign     =  'I'.
  ra_nonrat-option   =  'EQ'.
  ra_nonrat-low      =  'SSOCPW'.
  APPEND ra_nonrat.

  ra_nonrat-sign     =  'I'.
  ra_nonrat-option   =  'EQ'.
  ra_nonrat-low      =  'SSOCSP'.
  APPEND ra_nonrat.

  ra_nonrat-sign     =  'I'.
  ra_nonrat-option   =  'EQ'.
  ra_nonrat-low      =  'SSOCSW'.
  APPEND ra_nonrat.

  ra_nonrat-sign     =  'I'.
  ra_nonrat-option   =  'EQ'.
  ra_nonrat-low      =  'SSOCW'.
  APPEND ra_nonrat.

  ra_nonrat-sign     =  'I'.
  ra_nonrat-option   =  'EQ'.
  ra_nonrat-low      =  'SSOCWW'.
  APPEND ra_nonrat.

  ra_nonrat-sign     =  'I'.
  ra_nonrat-option   =  'EQ'.
  ra_nonrat-low      =  'SURPLSS'.
  APPEND ra_nonrat.

  ra_nonrat-sign     =  'I'.
  ra_nonrat-option   =  'EQ'.
  ra_nonrat-low      =  'WAIVES'.
  APPEND ra_nonrat.

  ra_nonrat-sign     =  'I'.
  ra_nonrat-option   =  'EQ'.
  ra_nonrat-low      =  'WAIVESS'.
  APPEND ra_nonrat.

ENDFORM.                    "INITIALIZE_DATA

*-----------------------------------------------------------------------
*  Get WACOG and heating value constants from tables
*-----------------------------------------------------------------------
FORM get_wacog_hv_values.

  SELECT SINGLE * FROM zwacog WHERE volume_uom = 'GJ'.
  wa_wacog = zwacog-wacog.

  SELECT SINGLE * FROM zstconv WHERE volume_uom = '103M3'.
  wa_heatv = zstconv-factor.

ENDFORM.                    "GET_WACOG_HV_VALUES
