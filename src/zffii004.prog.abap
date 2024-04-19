REPORT zffii004 MESSAGE-ID zs.
************************************************************************
* 2012/05/18 sahmad   #--  Additions for UG - CRA Auditor
* 2010/11/30 jsennett #883 Change Company Code field to a range field
*                          and add column delimitor char "|".
* 2008/08/15 mdemeest #--- Changes for Houston auditor - selection
* 2001/09/19 mdemeest #--- USNAM always required - may not always be
*                          included in written specs - speak to auditor
*                          before removing
* 1999/03/04 mdemeest #639 Extract BKPF from SAP into flat file for
*                          Revenue Canada.  Fiscal Year selection
*
************************************************************************
TABLES: bkpf, bseg.

* Global Data Definitions
DATA: recsize  type i value 250,
      recsize2 type i value 1000.

DATA: BEGIN OF wabkpf OCCURS 50000,
      mandt LIKE bkpf-mandt,
      sp1   TYPE c VALUE '~',
      bukrs LIKE bkpf-bukrs,
      sp2   TYPE c VALUE '~',
      belnr LIKE bkpf-belnr,
      sp3   TYPE c VALUE '~',
      gjahr LIKE bkpf-gjahr,
      sp4   TYPE c VALUE '~',
      blart LIKE bkpf-blart,
      sp5   TYPE c VALUE '~',
      bldat LIKE bkpf-bldat,
      sp6   TYPE c VALUE '~',
      budat LIKE bkpf-budat,
      sp7   TYPE c VALUE '~',
      monat LIKE bkpf-monat,
      sp8   TYPE c VALUE '~',
      tcode LIKE bkpf-tcode,
      sp9   TYPE c VALUE '~',
      xblnr LIKE bkpf-xblnr,
      sp10  TYPE c VALUE '~',
      dbblg LIKE bkpf-dbblg,
      sp11  TYPE c VALUE '~',
      bktxt LIKE bkpf-bktxt,
      sp12  TYPE c VALUE '~',
      waers LIKE bkpf-waers,
      sp13  TYPE c VALUE '~',
      bstat LIKE bkpf-bstat,
      sp14  TYPE c VALUE '~',
      awkey TYPE bkpf-awkey,
      sp15  TYPE c VALUE '~',
      stblg TYPE bkpf-stblg,
      sp16  TYPE c VALUE '~',
      stjah TYPE bkpf-stjah,
      sp17  TYPE c VALUE '~',
      stgrd TYPE bkpf-stgrd,
      sp18  TYPE c VALUE '~',
      kursf(12) type c, " TYPE bkpf-kursf,
      sp19  TYPE c VALUE '~',
      cpudt TYPE bkpf-cpudt,
      sp20  TYPE c VALUE '~',
      cputm TYPE bkpf-cputm,
      sp21  TYPE c VALUE '~',
      usnam TYPE bkpf-usnam,
      sp22  TYPE c VALUE '~',
      xmwst TYPE bkpf-xmwst,
      sp23  TYPE c VALUE '~',
      ppnam TYPE bkpf-ppnam,
      END OF wabkpf.

DATA: BEGIN OF wabseg OCCURS 100000,
      mandt    LIKE bseg-mandt,
      sp1      TYPE c VALUE '~',
      bukrs    LIKE bseg-bukrs,
      sp2      TYPE c VALUE '~',
      belnr    LIKE bseg-belnr,
      sp3      TYPE c VALUE '~',
      gjahr    LIKE bseg-gjahr,
      sp4      TYPE c VALUE '~',
      buzei    LIKE bseg-buzei,
      sp5      TYPE c VALUE '~',
      bschl    LIKE bseg-bschl,
      sp6      TYPE c VALUE '~',
      koart    LIKE bseg-koart,
      sp7      TYPE c VALUE '~',
      shkzg    LIKE bseg-shkzg,
      sp8      TYPE c VALUE '~',
      mwskz    LIKE bseg-mwskz,
      sp9      TYPE c VALUE '~',
      gsber    LIKE bseg-gsber,                " per TIM
      sp10     TYPE c VALUE '~',
      dmbtr(16) TYPE c,                        " like bseg-dmbtr,
      sp11     TYPE c VALUE '~',
      wrbtr(16) TYPE c,
      sp12     TYPE c VALUE '~',
      mwart    LIKE bseg-mwart,
      sp13     TYPE c VALUE '~',
      sgtxt    LIKE bseg-sgtxt,
      sp14     TYPE c VALUE '~',
      bewar    LIKE bseg-bewar,
      sp15     TYPE c VALUE '~',
      vorgn    LIKE bseg-vorgn,
      sp16     TYPE c VALUE '~',
      kostl    LIKE bseg-kostl,
*      PROJN    LIKE BSEG-PROJN,
*      AUFNR    LIKE BSEG-AUFNR,
      sp17     TYPE c VALUE '~',
      vbeln    LIKE bseg-vbeln,
      sp18     TYPE c VALUE '~',
      vbel2    LIKE bseg-vbel2,
      sp19     TYPE c VALUE '~',
      saknr    LIKE bseg-saknr,
      sp20     TYPE c VALUE '~',
      hkont    LIKE bseg-hkont,
      sp21     TYPE c VALUE '~',
      kunnr    LIKE bseg-kunnr,                " per TIM
      sp22     TYPE c VALUE '~',
      lifnr    LIKE bseg-lifnr,
      sp23     TYPE c VALUE '~',
      matnr    LIKE bseg-matnr,
      sp24     TYPE c VALUE '~',
      ebeln    LIKE bseg-ebeln,
      sp25     TYPE c VALUE '~',
      ebelp    TYPE bseg-ebelp,
      sp37     TYPE c VALUE '~',
      anln1    TYPE bseg-anln1,
      sp26     TYPE c VALUE '~',
      anln2    TYPE bseg-anln2,
      sp27     TYPE c VALUE '~',
      anbwa    TYPE bseg-anbwa,
      sp28     TYPE c VALUE '~',
      bzdat(8) type c, "    TYPE bseg-bzdat,
      sp29     TYPE c VALUE '~',
      aufnr    TYPE bseg-aufnr,  "order, unavailable
      sp30     TYPE c VALUE '~',
      projk    TYPE bseg-projk,  "ps_posid, unavailable
      sp31     TYPE c VALUE '~',
      zuonr    LIKE bseg-zuonr,
      sp32     TYPE c VALUE '~',
      werks    TYPE bseg-werks,
      sp33     TYPE c VALUE '~',
      prctr    TYPE bseg-prctr,
      sp34     TYPE c VALUE '~',
      kokrs(5) TYPE c,
      sp35     TYPE c VALUE '~',
      augdt    TYPE bseg-augdt,
      sp36     TYPE c VALUE '~',
      augbl    TYPE bseg-augbl,
      sp38     TYPE c VALUE '~',
      xref3    TYPE bseg-xref3,
      sp39     TYPE c VALUE '~',
      mwsts(16) TYPE c,
      sp40     TYPE c VALUE '~',
      wmwst(16) TYPE c,
      sp41     TYPE c VALUE '~',
      zfbdt    TYPE bseg-zfbdt,
      sp42     TYPE c VALUE '~',
      txjcd    TYPE bseg-txjcd,
      sp43     TYPE c VALUE '~',
      txdat    TYPE bseg-txdat,
      sp44     TYPE c VALUE '~',
      zterm(5) TYPE c,
      sp45     TYPE c VALUE '~',
      zbd1t(5) TYPE c,
      sp46     TYPE c VALUE '~',
      zbd2t(5) TYPE c,
      sp47     TYPE c VALUE '~',
      zbd3t(5) TYPE c,
      sp48     TYPE c VALUE '~',
      zbd1p(6) TYPE c,
      sp49     TYPE c VALUE '~',
      zbd2p(6) TYPE c,
      sp50     TYPE c VALUE '~',
      kostl2   TYPE kostl,
      sp51     TYPE c VALUE '~',
      prozs(6) TYPE c,
      sp52     TYPE c VALUE '~',
      ltext    TYPE kltxt,
      sp53     TYPE c VALUE '~',
      stras    TYPE stras_gp,
      sp54     TYPE c VALUE '~',
      ort01    TYPE ort01_gp,
      sp55     TYPE c VALUE '~',
      ort02    TYPE ort02_gp,
      sp56     TYPE c VALUE '~',
      regio(5) TYPE c,
      sp57     TYPE c VALUE '~',
      pstlz    TYPE pstlz,
      sp58     TYPE c VALUE '~',
      land1(5) TYPE c,
      END OF wabseg.

DATA: BEGIN OF wabkpf_header occurs 1,
      mandt(5) value 'MANDT',
      sp1   TYPE c VALUE '~',
      bukrs(5) value 'BUKRS',
      sp2   TYPE c VALUE '~',
      belnr(10) value 'BELNR',
      sp3   TYPE c VALUE '~',
      gjahr(5) value 'GJAHR',
      sp4   TYPE c VALUE '~',
      blart(5) value 'BLART',
      sp5   TYPE c VALUE '~',
      bldat(8) value 'BLDAT',
      sp6   TYPE c VALUE '~',
      budat(8) value 'BUDAT',
      sp7   TYPE c VALUE '~',
      monat(5) value 'MONAT',
      sp8   TYPE c VALUE '~',
      tcode(20) value 'TCODE',
      sp9   TYPE c VALUE '~',
      xblnr(16) value 'XBLNR',
      sp10  TYPE c VALUE '~',
      dbblg(10) value 'DBBLG',
      sp11  TYPE c VALUE '~',
      bktxt(25) value 'BKTXT',
      sp12  TYPE c VALUE '~',
      waers(5) value 'WAERS',
      sp13  TYPE c VALUE '~',
      bstat(5) value 'BSTAT',
      sp14  TYPE c VALUE '~',
      awkey(20) value 'AWKEY',
      sp15  TYPE c VALUE '~',
      stblg(10) value 'STBLG',
      sp16  TYPE c VALUE '~',
      stjah(5) value 'STJAH',
      sp17  TYPE c VALUE '~',
      stgrd(5) value 'STGRD',
      sp18  TYPE c VALUE '~',
      kursf(12) value 'KURSF',
      sp19  TYPE c VALUE '~',
      cpudt(8) value 'CPUDT',
      sp20  TYPE c VALUE '~',
      cputm(6) value 'CPUTM',
      sp21  TYPE c VALUE '~',
      usnam(12) value 'USNAM',
      sp22  TYPE c VALUE '~',
      xmwst(5) value 'XMWST',
      sp23  TYPE c VALUE '~',
      ppnam(12) value 'PPNAM',
      END OF wabkpf_header.

DATA: BEGIN OF wabseg_header occurs 1,
      mandt(5) value 'MANDT',
      sp1      TYPE c VALUE '~',
      bukrs(5) value 'BUKRS',
      sp2      TYPE c VALUE '~',
      belnr(10) value 'BELNR',
      sp3      TYPE c VALUE '~',
      gjahr(5) value 'GJAHR',
      sp4      TYPE c VALUE '~',
      buzei(5) value 'BUZEI',
      sp5      TYPE c VALUE '~',
      bschl(5) value 'BSCHL',
      sp6      TYPE c VALUE '~',
      koart(5) value 'KOART',
      sp7      TYPE c VALUE '~',
      shkzg(5) value 'SHKZG',
      sp8      TYPE c VALUE '~',
      mwskz(5) value 'MWSKZ',
      sp9      TYPE c VALUE '~',
      gsber(5) value 'GSBER',
      sp10     TYPE c VALUE '~',
      dmbtr(16) value 'DMBTR',
      sp11     TYPE c VALUE '~',
      wrbtr(16) value 'WRBTR',
      sp12     TYPE c VALUE '~',
      mwart(5) value 'MWART',
      sp13     TYPE c VALUE '~',
      sgtxt(50) value 'SGTXT',
      sp14     TYPE c VALUE '~',
      bewar(5) value 'BEWAR',
      sp15     TYPE c VALUE '~',
      vorgn(5) value 'VORGN',
      sp16     TYPE c VALUE '~',
      kostl(10) value 'KOSTL',
*      PROJN    LIKE BSEG-PROJN,
*      AUFNR    LIKE BSEG-AUFNR,
      sp17     TYPE c VALUE '~',
      vbeln(10) value 'VBELN',
      sp18     TYPE c VALUE '~',
      vbel2(10) value 'VBEL2',
      sp19     TYPE c VALUE '~',
      saknr(10) value 'SAKNR',
      sp20     TYPE c VALUE '~',
      hkont(10) value 'HKONT',
      sp21     TYPE c VALUE '~',
      kunnr(10) value 'KUNNR',
      sp22     TYPE c VALUE '~',
      lifnr(10) value 'LIFNR',
      sp23     TYPE c VALUE '~',
      matnr(18) value 'MATNR',
      sp24     TYPE c VALUE '~',
      ebeln(10) value 'EBELN',
      sp25     TYPE c VALUE '~',
      ebelp(5) value 'EBELP',
      sp37     TYPE c VALUE '~',
      anln1(12) value 'ANLN1',
      sp26     TYPE c VALUE '~',
      anln2(5) value 'ANLN2',
      sp27     TYPE c VALUE '~',
      anbwa(5) value 'ANBWA',
      sp28     TYPE c VALUE '~',
      bzdat(8) value 'BZDAT',
      sp29     TYPE c VALUE '~',
      aufnr(12) value 'AUFNR',
      sp30     TYPE c VALUE '~',
      projk(8) value 'PROJK',
      sp31     TYPE c VALUE '~',
      zuonr(18) value 'ZUONR',
      sp32     TYPE c VALUE '~',
      werks(5) value 'WERKS',
      sp33     TYPE c VALUE '~',
      prctr(10) value 'PRCTR',
      sp34     TYPE c VALUE '~',
      kokrs(5) value 'KOKRS',
      sp35     TYPE c VALUE '~',
      augdt(8) value 'AUGDT',
      sp36     TYPE c VALUE '~',
      augbl(10) value 'AUGBL',
      sp38     TYPE c VALUE '~',
      xref3(20) value 'XREF3',
      sp39     TYPE c VALUE '~',
      mwsts(16) value 'MWSTS',
      sp40     TYPE c VALUE '~',
      wmwst(16) value 'WMWST',
      sp41     TYPE c VALUE '~',
      zfbdt(8) value 'ZFBDT',
      sp42     TYPE c VALUE '~',
      txjcd(15) value 'TXJCD',
      sp43     TYPE c VALUE '~',
      txdat(8) value 'TXDAT',
      sp44     TYPE c VALUE '~',
      zterm(5) value 'ZTERM',
      sp45     TYPE c VALUE '~',
      zbd1t(5) value 'ZBD1T',
      sp46     TYPE c VALUE '~',
      zbd2t(5) value 'ZBD2T',
      sp47     TYPE c VALUE '~',
      zbd13(5) value 'ZBD3T',
      sp48     TYPE c VALUE '~',
      zbd1p(6) value 'ZBD1P',
      sp49     TYPE c VALUE '~',
      zbd2p(6) value 'ZBD2P',
      sp50     TYPE c VALUE '~',
      kostl2(10) value 'KOSTL2',
      sp51     TYPE c VALUE '~',
      prozs(6) value 'PROZS',
      sp52     TYPE c VALUE '~',
      ltext(40) value 'LTEXT',
      sp53     TYPE c VALUE '~',
      stras(35) value 'STRAS',
      sp54     TYPE c VALUE '~',
      ort01(35) value 'ORT01',
      sp55     TYPE c VALUE '~',
      ort02(35) value 'ORT02',
      sp56     TYPE c VALUE '~',
      regio(5) value 'REGIO',
      sp57     TYPE c VALUE '~',
      pstlz(10) value 'PSTLZ',
      sp58     TYPE c VALUE '~',
      land1(5) value 'LAND1',
      END OF wabseg_header.

TYPES:  ty_rt_avorg      TYPE RANGE OF avorg.    "Settlement Transaction

DATA:   grt_avorg        TYPE ty_rt_avorg,
        grw_avorg        LIKE LINE OF grt_avorg.

TYPES: BEGIN OF ty_srule,
        objnr_p          TYPE j_objnr,
        objnr            TYPE j_objnr,
        prozs            TYPE brgprozs,
        kokrs            TYPE kokrs,
        kostl            TYPE kostl,
        ltext            TYPE kltxt,
        stras            TYPE stras_gp,
        ort01            TYPE ort01_gp,
        ort02            TYPE ort02_gp,
        regio            TYPE regio,
        pstlz            TYPE pstlz,
        land1            TYPE land1,
       END   OF ty_srule.

DATA:   gwa_srule        TYPE ty_srule,
        git_srule        TYPE STANDARD TABLE OF ty_srule.

TYPES: BEGIN OF ty_cost_center,
        kokrs            TYPE kokrs,
        kostl            TYPE kostl,
        ltext            TYPE kltxt,
        stras            TYPE stras_gp,
        ort01            TYPE ort01_gp,
        ort02            TYPE ort02_gp,
        regio            TYPE regio,
        pstlz            TYPE pstlz,
        land1            TYPE land1,
       END   OF ty_cost_center.

DATA:   git_cost_center  TYPE STANDARD TABLE OF ty_cost_center.

* Selection Screen
SELECTION-SCREEN BEGIN OF BLOCK box WITH FRAME TITLE text-001.
PARAMETER: phyfile LIKE filenameci-fileextern
                     DEFAULT '/usr/sap/interfaces/P01/CFMM001/BKPF.SAP'.
*PARAMETER: recsize TYPE i DEFAULT 118.
SELECTION-SCREEN END OF BLOCK box.
SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK box2 WITH FRAME TITLE text-002.
PARAMETER: phyfile2 LIKE filenameci-fileextern
                     DEFAULT '/usr/sap/interfaces/P01/CFMM001/BSEG.SAP'.
*PARAMETER: recsize2 TYPE i DEFAULT 246.
SELECTION-SCREEN END OF BLOCK box2.
SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK box3 WITH FRAME TITLE text-003.
SELECT-OPTIONS: s_bukrs FOR bkpf-bukrs DEFAULT 'UGL',
                s_gjahr FOR bkpf-gjahr DEFAULT sy-datum(4).
SELECTION-SCREEN END OF BLOCK box3.

SELECTION-SCREEN BEGIN OF BLOCK box4 WITH FRAME TITLE text-004.
SELECT-OPTIONS: s_monat FOR bkpf-monat NO INTERVALS,
                s_blart FOR bkpf-blart NO INTERVALS,
                s_budat FOR bkpf-budat NO INTERVALS.
SELECTION-SCREEN SKIP 1.
PARAMETERS:     p_kokrs  TYPE kokrs            "Controlling Area"
                         MEMORY ID CAC
                         OBLIGATORY.
SELECTION-SCREEN END OF BLOCK box4.

SELECTION-SCREEN COMMENT 1(80) text-900.

************************************************************************
*                   INITIALIZATION                                     *
************************************************************************
INITIALIZATION.

  CLEAR                                grt_avorg[].
  CLEAR                                grw_avorg.
  MOVE     'I'                      TO grw_avorg-sign.
  MOVE     'EQ'                     TO grw_avorg-option.
  MOVE     SPACE                    TO grw_avorg-low.
  APPEND                               grw_avorg
                                    TO grt_avorg.
  CLEAR                                grw_avorg-low.
  MOVE     'KOAO'                   TO grw_avorg-low.
  APPEND                               grw_avorg
                                    TO grt_avorg.
  CLEAR                                grw_avorg-low.
  MOVE     'KOAW'                   TO grw_avorg-low.
  APPEND                               grw_avorg
                                    TO grt_avorg.

  CLEAR    git_srule[].

  CLEAR    git_cost_center[].

************************************************************************
*                   START-OF-SELECTION                                 *
************************************************************************
START-OF-SELECTION.

  PERFORM open_output_file.
  Perform file_header.

  SELECT * FROM bkpf                       "Selection based on fiscal year
     WHERE bukrs IN s_bukrs                 "and month
       AND gjahr IN s_gjahr
       AND monat IN s_monat
       AND blart IN s_blart
       AND budat IN s_budat.

    PERFORM move_info_bkpf.                       "Move info to work area
    TRANSFER wabkpf TO phyfile LENGTH recsize.   "Output BKPF

    SELECT * FROM bseg
      WHERE bukrs = bkpf-bukrs
        AND belnr = bkpf-belnr
        AND gjahr = bkpf-gjahr.

      PERFORM move_info_bseg.                     "Move info to work area
      TRANSFER wabseg TO phyfile2 LENGTH recsize2."Output BSEG
    ENDSELECT.                                     "END of BSEG
  ENDSELECT.                                       "END of BKPF

  CLOSE DATASET: phyfile, phyfile2.
  IF sy-subrc = 0 .
    WRITE: / 'Process has been completed.'.
  ENDIF.
************************************************************************
* Routine to convert the logical file name provided as input
* to a physical file name and then to open the physical file
* for output in text mode.
************************************************************************
FORM open_output_file.
  OPEN DATASET phyfile FOR OUTPUT IN TEXT MODE.
  IF sy-subrc <> 0.
    WRITE: / '(BKPF) Can not open output file for process, '.
    WRITE: 'Please check text file, security or path'.
  ENDIF.
  OPEN DATASET phyfile2 FOR OUTPUT IN TEXT MODE.
  IF sy-subrc <> 0.
    WRITE: / '(BSEG) Can not open output file for process, '.
    WRITE: 'Please check text file, security or path'.
  ENDIF.
ENDFORM.                    "OPEN_OUTPUT_FILE
*---------------------- MOVE_INFO_BKPF ---------------------------------
* move fields from BSEG to OUTPUT RECORD
*-----------------------------------------------------------------------
FORM move_info_bkpf.
  CLEAR wabkpf.
  wabkpf-sp1 = '~'.
  wabkpf-sp2 = '~'.
  wabkpf-sp3 = '~'.
  wabkpf-sp4 = '~'.
  wabkpf-sp5 = '~'.
  wabkpf-sp6 = '~'.
  wabkpf-sp7 = '~'.
  wabkpf-sp8 = '~'.
  wabkpf-sp9 = '~'.
  wabkpf-sp10 = '~'.
  wabkpf-sp11 = '~'.
  wabkpf-sp12 = '~'.
  wabkpf-sp13 = '~'.
  wabkpf-sp14 = '~'.
  wabkpf-sp15 = '~'.
  wabkpf-sp16 = '~'.
  wabkpf-sp17 = '~'.
  wabkpf-sp18 = '~'.
  wabkpf-sp19 = '~'.
  wabkpf-sp20 = '~'.
  wabkpf-sp21 = '~'.
  wabkpf-sp22 = '~'.
  wabkpf-sp23 = '~'.
  MOVE-CORRESPONDING bkpf TO wabkpf.
  wabkpf-kursf = bkpf-kursf.
ENDFORM.                    "MOVE_INFO_BKPF

*---------------------- MOVE_INFO_BSEG ---------------------------------
* move fields from BSEG to OUTPUT RECORD
*-----------------------------------------------------------------------
FORM move_info_bseg.

  CLEAR    gwa_srule.

  PERFORM  f_get_srule_cc CHANGING gwa_srule.

  CLEAR wabseg.
  wabseg-sp1 = '~'.
  wabseg-sp2 = '~'.
  wabseg-sp3 = '~'.
  wabseg-sp4 = '~'.
  wabseg-sp5 = '~'.
  wabseg-sp6 = '~'.
  wabseg-sp7 = '~'.
  wabseg-sp8 = '~'.
  wabseg-sp9 = '~'.
  wabseg-sp10 = '~'.
  wabseg-sp11 = '~'.
  wabseg-sp12 = '~'.
  wabseg-sp13 = '~'.
  wabseg-sp14 = '~'.
  wabseg-sp15 = '~'.
  wabseg-sp16 = '~'.
  wabseg-sp17 = '~'.
  wabseg-sp18 = '~'.
  wabseg-sp19 = '~'.
  wabseg-sp20 = '~'.
  wabseg-sp21 = '~'.
  wabseg-sp22 = '~'.
  wabseg-sp23 = '~'.
  wabseg-sp24 = '~'.
  wabseg-sp25 = '~'.
  wabseg-sp26 = '~'.
  wabseg-sp27 = '~'.
  wabseg-sp28 = '~'.
  wabseg-sp29 = '~'.
  wabseg-sp30 = '~'.
  wabseg-sp31 = '~'.
  wabseg-sp32 = '~'.
  wabseg-sp33 = '~'.
  wabseg-sp34 = '~'.
  wabseg-sp35 = '~'.
  wabseg-sp36 = '~'.
  wabseg-sp37 = '~'.
  wabseg-sp38 = '~'.
  wabseg-sp39 = '~'.
  wabseg-sp40 = '~'.
  wabseg-sp41 = '~'.
  wabseg-sp42 = '~'.
  wabseg-sp43 = '~'.
  wabseg-sp44 = '~'.
  wabseg-sp45 = '~'.
  wabseg-sp46 = '~'.
  wabseg-sp47 = '~'.
  wabseg-sp48 = '~'.
  wabseg-sp49 = '~'.
  wabseg-sp50 = '~'.
  wabseg-sp51 = '~'.
  wabseg-sp52 = '~'.
  wabseg-sp53 = '~'.
  wabseg-sp54 = '~'.
  wabseg-sp55 = '~'.
  wabseg-sp56 = '~'.
  wabseg-sp57 = '~'.
  wabseg-sp58 = '~'.
  MOVE-CORRESPONDING bseg TO wabseg.
  wabseg-dmbtr = bseg-dmbtr.
  wabseg-wrbtr = bseg-wrbtr.
  wabseg-bzdat = bseg-bzdat.
  wabseg-mwsts = bseg-mwsts.
  wabseg-wmwst = bseg-wmwst.
  wabseg-zbd1t = bseg-zbd1t.
  wabseg-zbd2t = bseg-zbd2t.
  wabseg-zbd3t = bseg-zbd3t.
  wabseg-zbd1p = bseg-zbd1p.
  wabseg-zbd2p = bseg-zbd2p.
  wabseg-kostl2 = gwa_srule-kostl.
  wabseg-prozs  = gwa_srule-prozs.
  wabseg-ltext  = gwa_srule-ltext.
  wabseg-stras  = gwa_srule-stras.
  wabseg-ort01  = gwa_srule-ort01.
  wabseg-ort02  = gwa_srule-ort02.
  wabseg-regio  = gwa_srule-regio.
  wabseg-pstlz  = gwa_srule-pstlz.
  wabseg-land1  = gwa_srule-land1.
ENDFORM.                    "MOVE_INFO_BSEG
*&---------------------------------------------------------------------*
*&      Form  FILE_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form FILE_HEADER .

  TRANSFER wabkpf_header TO phyfile length recsize.
  TRANSFER wabseg_header TO phyfile2 length recsize2.
endform.                    " FILE_HEADER
*eject
*&---------------------------------------------------------------------*
*&      Form  f_get_srule_cc
*&---------------------------------------------------------------------*
*       Get the settlement rule cost center
*----------------------------------------------------------------------*
FORM f_get_srule_cc
  CHANGING cwa_srule              TYPE ty_srule.

  TYPES: BEGIN OF ty_receivers,
           objnr                  TYPE j_objnr,
           prozs                  TYPE brgprozs,
         END   OF ty_receivers.

  TYPES: BEGIN OF ty_period,
           gjahr                  TYPE gabja,
           buper                  TYPE gabpe,
         END   OF ty_period.

  DATA:    lwa_srule              TYPE ty_srule,
           lwa_cobrb              TYPE cobrb,
           lit_cobrb              TYPE STANDARD TABLE OF cobrb,
           lwa_receivers          TYPE ty_receivers,
           lwa_receivers_p1       TYPE ty_receivers,
           lit_receivers          TYPE STANDARD TABLE OF ty_receivers,
           lit_receivers_p1       TYPE STANDARD TABLE OF ty_receivers,
           lit_receivers_p2       TYPE STANDARD TABLE OF ty_receivers.

  DATA:    lwa_requested_buper    TYPE ty_period,
           lwa_from_buper         TYPE ty_period,
           lwa_to_buper           TYPE ty_period.

  DATA:    lv_subrc               TYPE sysubrc,
           lv_tabix               TYPE sytabix,
           lv_objnr               TYPE j_objnr,
           lv_obart               TYPE j_obart,
           lv_kokrs               TYPE kokrs.

  CLEAR    cwa_srule.

  CLEAR    lit_receivers[].
  CLEAR    lit_receivers_p1[].
  CLEAR    lit_receivers_p2[].

*eject
* Set the initial object number according to the cost object
  CLEAR                                lv_objnr.

* Internal Order
  IF     ( bseg-aufnr               IS NOT INITIAL ).
    CONCATENATE                   'OR' bseg-aufnr
                                  INTO lv_objnr.
* WBS Element
  ELSEIF ( bseg-projk               IS NOT INITIAL ).
    CONCATENATE                   'PR' bseg-projk
                                  INTO lv_objnr.
* Network-Activity
  ELSEIF ( bseg-aufpl               IS NOT INITIAL ) AND
         ( bseg-aplzl               IS NOT INITIAL ).
    CONCATENATE                   'NV' bseg-aufpl
                                       bseg-aplzl
                                  INTO lv_objnr.
* Network
  ELSEIF ( bseg-nplnr               IS NOT INITIAL ).
    CONCATENATE                   'NP' bseg-nplnr
                                  INTO lv_objnr.
  ENDIF.

  IF     ( lv_objnr IS INITIAL ).
    RETURN.
  ENDIF.

  CLEAR                                lwa_srule.
  READ     TABLE git_srule        INTO lwa_srule
                              WITH KEY objnr_p = lv_objnr
                        BINARY SEARCH.
  lv_subrc = sy-subrc.
  lv_tabix = sy-tabix.

  IF     ( lv_subrc EQ 0 ).
    CLEAR                              cwa_srule.
    MOVE   lwa_srule                TO cwa_srule.
    RETURN.
  ELSE.
    CLEAR                              lwa_srule.
    MOVE   lv_objnr                 TO lwa_srule-objnr_p.
  ENDIF.

  CLEAR    lwa_requested_buper.

  IF     ( bseg-abper               IS INITIAL ).
    MOVE   bkpf-gjahr               TO lwa_requested_buper-gjahr.
    MOVE   bkpf-monat               TO lwa_requested_buper-buper.
  ELSE.
    MOVE   bseg-abper+0(4)          TO lwa_requested_buper-gjahr.
    MOVE   bseg-abper+4(2)          TO lwa_requested_buper-buper.
  ENDIF.

*eject
  CLEAR                                lit_receivers[].
  CLEAR                                lwa_receivers.
  MOVE     lv_objnr                 TO lwa_receivers-objnr.
  APPEND                               lwa_receivers
                                    TO lit_receivers.

  DO 10 TIMES.

    IF     ( lit_receivers[] IS INITIAL ).
      EXIT.
    ENDIF.

    CLEAR                              lwa_receivers.
    LOOP AT  lit_receivers        INTO lwa_receivers.

      CLEAR                            lv_objnr.
      MOVE     lwa_receivers-objnr  TO lv_objnr.

      CLEAR    lit_cobrb[].
      SELECT   *
        INTO   TABLE lit_cobrb
        FROM   cobrb
       WHERE   objnr = lv_objnr.
      IF     ( sy-subrc NE 0 ).
        CLEAR  lit_cobrb[].
      ENDIF.

      CLEAR                            lwa_cobrb.
      LOOP AT  lit_cobrb          INTO lwa_cobrb.

        IF   ( lwa_cobrb-avorg      IN grt_avorg ).
        ELSE.
          CLEAR                        lwa_cobrb.
          CONTINUE.
        ENDIF.

        IF   ( lwa_requested_buper  IS NOT INITIAL ).
          CLEAR                        lwa_from_buper.
          MOVE   lwa_cobrb-gabja    TO lwa_from_buper-gjahr.
          MOVE   lwa_cobrb-gabpe    TO lwa_from_buper-buper.
          CLEAR                        lwa_to_buper.
          MOVE   lwa_cobrb-gbisj    TO lwa_to_buper-gjahr.
          MOVE   lwa_cobrb-gbisp    TO lwa_to_buper-buper.
          IF   ( lwa_from_buper     LE lwa_requested_buper ) OR
               ( lwa_from_buper     IS INITIAL             ).
          ELSE.
            CLEAR                      lwa_cobrb.
            CONTINUE.
          ENDIF.
          IF   ( lwa_to_buper       GE lwa_requested_buper ) OR
               ( lwa_to_buper       IS INITIAL             ).
          ELSE.
            CLEAR                      lwa_cobrb.
            CONTINUE.
          ENDIF.
        ENDIF.

*eject
        CLEAR                          lv_objnr.
        MOVE   lwa_cobrb-rec_objnr1 TO lv_objnr.

        CLEAR    lv_obart.

        CALL FUNCTION 'OBJECT_NUMBER_TYPE_GET'
          EXPORTING
            OBJNR = lv_objnr
          IMPORTING
            OBART = lv_obart.

        CASE     lv_obart.
          WHEN     'KS'. "Terminate with a Cost Center
            CLEAR                      lwa_receivers_p1.
            MOVE   lv_objnr         TO lwa_receivers_p1-objnr.
            MOVE   lwa_cobrb-prozs  TO lwa_receivers_p1-prozs.
            APPEND                     lwa_receivers_p1
                                    TO lit_receivers_p2.
          WHEN     'SK'. "Terminate with a G/L Account
            CLEAR                      lwa_receivers_p1.
          WHEN     OTHERS. "
            CLEAR                      lwa_receivers_p1.
            MOVE   lv_objnr         TO lwa_receivers_p1-objnr.
            APPEND                     lwa_receivers_p1
                                    TO lit_receivers_p1.
        ENDCASE.

        CLEAR  lwa_cobrb.
      ENDLOOP.

      CLEAR  lwa_receivers.
    ENDLOOP.

    CLEAR    lit_receivers[].

    IF     ( lit_receivers_p1[] IS INITIAL ).
    ELSE.

      lit_receivers[] = lit_receivers_p1[].

    ENDIF.

    CLEAR    lit_receivers_p1[].

  ENDDO.

  IF     ( lit_receivers_p2[] IS INITIAL ).
    RETURN.
  ENDIF.

*eject
  SORT     lit_receivers_p2 DESCENDING BY prozs.

  CLEAR                                  lwa_receivers.
  READ     TABLE lit_receivers_p2   INTO lwa_receivers INDEX 1.
  IF     ( sy-subrc EQ 0 ).
    CLEAR                                lwa_srule-objnr.
    MOVE   lwa_receivers-objnr        TO lwa_srule-objnr.
    CLEAR                                lwa_srule-prozs.
    MOVE   lwa_receivers-prozs        TO lwa_srule-prozs.

    CLEAR                                lv_kokrs.
    IF     ( bseg-kokrs               IS INITIAL ).
      MOVE   p_kokrs                  TO lv_kokrs.
    ELSE.
      MOVE   bseg-kokrs               TO lv_kokrs.
    ENDIF.

    PERFORM  f_get_cost_center  USING    lv_kokrs
                                CHANGING lwa_srule.

    INSERT                               lwa_srule
                                    INTO git_srule
                                   INDEX lv_tabix.

    CLEAR                                cwa_srule.
    MOVE                                 lwa_srule
                                      TO cwa_srule.
  ENDIF.

ENDFORM.                    " f_get_srule_cc
*eject
*&---------------------------------------------------------------------*
*&      Form  f_get_cost_center
*&---------------------------------------------------------------------*
*       Get the cost center
*----------------------------------------------------------------------*
FORM f_get_cost_center
  USING    iv_kokrs               TYPE kokrs
  CHANGING cwa_srule              TYPE ty_srule.

  DATA:    lwa_csks               TYPE csks,
           lwa_cskt               TYPE cskt,
           lwa_cost_center        TYPE ty_cost_center.

  DATA:    lv_subrc               TYPE sysubrc,
           lv_tabix               TYPE sytabix,
           lv_kostl               TYPE kostl.

  IF     ( iv_kokrs                 IS INITIAL ).
    RETURN.
  ENDIF.

  IF     ( cwa_srule-objnr          IS INITIAL ).
    RETURN.
  ENDIF.

  CLEAR                                lv_kostl.
  MOVE     cwa_srule-objnr+6(10)    TO lv_kostl.

  CLEAR                                lwa_cost_center.
  READ     TABLE git_cost_center  INTO lwa_cost_center
                              WITH KEY kokrs = iv_kokrs
                                       kostl = lv_kostl
                         BINARY SEARCH.
  lv_subrc = sy-subrc.
  lv_tabix = sy-tabix.

*eject
  IF     ( lv_subrc NE 0 ).

    CLEAR    lwa_csks.
    SELECT   *
      INTO   lwa_csks
      FROM   csks UP TO 1 ROWS
     WHERE   kokrs  = iv_kokrs
       AND   kostl  = lv_kostl
       AND   datbi >= sy-datum
       AND   datab <= sy-datum.

      CLEAR    lwa_cskt.
      SELECT   *
        INTO   lwa_cskt
        FROM   cskt UP TO 1 ROWS
       WHERE   spras  = 'E'
         AND   kokrs  = iv_kokrs
         AND   kostl  = lv_kostl
         AND   datbi >= sy-datum.
      ENDSELECT.
      IF ( sy-subrc NE 0 ).
        CLEAR  lwa_cskt.
      ENDIF.

    ENDSELECT.
    IF     ( sy-subrc NE 0 ).
      CLEAR  lwa_csks.
      CLEAR  lwa_cskt.
    ENDIF.

    MOVE     iv_kokrs               TO lwa_cost_center-kokrs.
    MOVE     lv_kostl               TO lwa_cost_center-kostl.
    MOVE     lwa_cskt-ltext         TO lwa_cost_center-ltext.
    IF     ( lwa_csks-stras         IS NOT INITIAL ).
      MOVE   lwa_csks-stras         TO lwa_cost_center-stras.
      MOVE   lwa_csks-pstlz         TO lwa_cost_center-pstlz.
    ELSEIF ( lwa_csks-pfach         IS NOT INITIAL ).
      CONCATENATE                      'P.O. BOX'
                                       lwa_csks-pfach
                                  INTO lwa_cost_center-stras
                    SEPARATED BY SPACE.
      MOVE   lwa_csks-pstl2         TO lwa_cost_center-pstlz.
    ENDIF.
    MOVE     lwa_csks-ort01         TO lwa_cost_center-ort01.
    MOVE     lwa_csks-ort02         TO lwa_cost_center-ort02.
    MOVE     lwa_csks-regio         TO lwa_cost_center-regio.
    MOVE     lwa_csks-land1         TO lwa_cost_center-land1.


    INSERT                             lwa_cost_center
                                  INTO git_cost_center
                                 INDEX lv_tabix.

  ENDIF.

*eject
  IF   ( ( cwa_srule-objnr          CS lv_kostl ) AND
         ( lwa_cost_center-kokrs    EQ iv_kokrs ) AND
         ( lwa_cost_center-kostl    EQ lv_kostl )     ).
    MOVE   lwa_cost_center-kokrs    TO cwa_srule-kokrs.
    MOVE   lwa_cost_center-kostl    TO cwa_srule-kostl.
    MOVE   lwa_cost_center-ltext    TO cwa_srule-ltext.
    MOVE   lwa_cost_center-stras    TO cwa_srule-stras.
    MOVE   lwa_cost_center-ort01    TO cwa_srule-ort01.
    MOVE   lwa_cost_center-ort02    TO cwa_srule-ort02.
    MOVE   lwa_cost_center-regio    TO cwa_srule-regio.
    MOVE   lwa_cost_center-pstlz    TO cwa_srule-pstlz.
    MOVE   lwa_cost_center-land1    TO cwa_srule-land1.
  ENDIF.

ENDFORM.                    " f_get_cost_center
