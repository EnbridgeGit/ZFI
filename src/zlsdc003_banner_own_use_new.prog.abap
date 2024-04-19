*&---------------------------------------------------------------------*
*& Report  ZLSDC003_BANNER_OWN_USE_NEW
*&
*&---------------------------------------------------------------------*
*&**********************************************************************
*  Author:      Mohammad T. Khan                                       *
*  Date:        January  2011.                                         *
*  Issue Log:   TR804                                                  *
*  Description:                                                        *
*     - This program will receive the file and translate Billing Data  *
*  from the Banner billing system into the data required to fill the   *
*  Goods Issue IDOC.  Program will create flat file with the translated*
*  data.                                                               *
*&---------------------------------------------------------------------*
*&                      Modification Log                               *
*&                                                                     *
*& Changed On   Changed By    CTS        Description                   *
*& --------------------------------------------------------------------*
*& 17-Jul-2020  AHMADT        D30K930631 CHG0185744 The report has been*
*&                            copied from ZLSDC003_BANNER_OWN_USE. GL  *
*&                            accnt no field added at SS. Hard coatings*
*&                            removed for GL Account and Order Number  *
*&---------------------------------------------------------------------*

REPORT  zlsdc003_banner_own_use_new MESSAGE-ID zm.

TABLES: zfb04,    "Banner Company Used Gas Lookup
        mbew.     "Material Valuation

* Input file format
DATA:  BEGIN OF own_rec,
       process_date       TYPE d,
       serv_type(4)       TYPE c,
       serv_cat(4)        TYPE c,
       gl_classcd(4)      TYPE c,
       serv_class(2)      TYPE c,
       town_cd(2)         TYPE c,
       munic_cd(4)        TYPE c,
       budget_ind(1)      TYPE c,
       cust_num(16)       TYPE n,
       trans_amt_sign(1)  TYPE c,
       trans_amt(13)      TYPE c,
       cust_chrg_sign(1)  TYPE c,
       cust_chrg(11)      TYPE c,
       cons_sign(1)       TYPE c,
       cons(13)           TYPE c,
       no_of_custs(6)     TYPE n,
       eff_date           TYPE d,
       rate_class(4)      TYPE c.
DATA:  END OF own_rec.

* Output file format
DATA: BEGIN OF ogi_rec,
      budat	            TYPE d,     "POSTNG DATE
      bldat	            TYPE d,     "DOCUMENT DATE
      xblnr(16)           TYPE c,     "REF DOC NO
      frbnr(16)           TYPE c,	    "BILL OF LADING
      u_name(12)          TYPE c,     "PR UNAME
      bktxt(25)           TYPE c,     "HEADER TEXT
      matnr(18)           TYPE c,     "MATERIAL
      werks_d(4)          TYPE c,     "PLANT
      lgort_d(4)        TYPE c,     "STORAGE LOC
      bwart(3)            TYPE c,     "MOVEMENT TYPE
      erfmg(13)           TYPE c,     "ENTRY QNT
      erfme(3)            TYPE c,     "ENTRY UOM
      isocd_unit(3)       TYPE c,     "ENTRY UOM ISO
      sgtxt(30)           TYPE c,	    "ITEM TEXT
      aufnr(12)           TYPE c,	    "ORDER ID
      kostl(10)           TYPE c,     "Cost Center
      saknr(10)           TYPE c.     "GL ACCOUNT
DATA: END OF ogi_rec.

DATA: w_trans_amt LIKE  mbew-stprs,
      w_erfmg     LIKE  mseg-erfmg,
      w_cust_num  LIKE  own_rec-cust_num.

*------------------------  Selection Screen  ---------------------------
* Input File
SELECTION-SCREEN BEGIN OF BLOCK box WITH FRAME TITLE text-001.
PARAMETERS:
  p_xblnr  LIKE  bkpf-xblnr DEFAULT 'BANNER: OWN USE'  OBLIGATORY,
  p_frbnr  LIKE  mkpf-frbnr DEFAULT 'OWN USE'          OBLIGATORY,
  p_bktxt  LIKE  bkpf-bktxt DEFAULT 'BANNER OWN USE'   OBLIGATORY,
*  P_SGTXT  LIKE  BSEG-SGTXT DEFAULT 'BANNER INTERFACE' OBLIGATORY,
  p_matnr  LIKE  bseg-matnr DEFAULT 'NATGAS'           OBLIGATORY,
  p_lgort  LIKE  mard-lgort DEFAULT 'A001'             OBLIGATORY,
  p_erfme  LIKE  mseg-erfme DEFAULT 'GJ1'              OBLIGATORY,
  p_isocd(3) TYPE c         DEFAULT 'GJ'               OBLIGATORY,
*  Start of changes by AHMADT for CHG0185744
*  p_kostl  LIKE zfb04-kostl DEFAULT '0000020310'       OBLIGATORY,
  p_kostl  LIKE zfb04-kostl                            OBLIGATORY,
  p_saknr  LIKE ska1-saknr                             OBLIGATORY.
* End of changes by AHMADT for CHG0185744
SELECTION-SCREEN SKIP.
* Output Files
PARAMETER: infile LIKE filenameci-fileextern OBLIGATORY,
           outfile LIKE filenameci-fileextern OBLIGATORY.
SELECTION-SCREEN END OF BLOCK box.

*---------------------------------------------------------------------*
*------------------------  Initialization  ---------------------------*
*---------------------------------------------------------------------*
INITIALIZATION.
  CONCATENATE '/usr/sap/interfaces/' sy-sysid+0(3)
              '/BANNER/zbis100owngi.dat' INTO infile.
  CONCATENATE '/usr/sap/interfaces/' sy-sysid+0(3)
              '/BANNER/zbannowngi.dat' INTO outfile.

***********************************************************************
START-OF-SELECTION.
  OPEN DATASET infile FOR INPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc NE 0.
    MESSAGE e006(zm) WITH infile.
  ENDIF.

  OPEN DATASET outfile FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc NE 0.
    MESSAGE e006(zm) WITH outfile.
  ENDIF.

  DO.
    CLEAR: own_rec, ogi_rec.
    READ DATASET infile INTO own_rec.
    IF sy-subrc <> '0'.
      EXIT.
    ENDIF.

    IF own_rec-trans_amt <> '0000000000.00'.
*Move data from input file.
      MOVE: own_rec-process_date   TO  ogi_rec-budat,
            own_rec-process_date   TO  ogi_rec-bldat,
            sy-uname               TO  ogi_rec-u_name.
      IF own_rec-gl_classcd = 'OWNS' OR own_rec-gl_classcd = 'CMPS'.
        MOVE 'GSTH'       TO  ogi_rec-werks_d.
      ELSE.
        MOVE 'GNTH'       TO  ogi_rec-werks_d.
      ENDIF.
* Start of changes by AHMADT for CHG0185744
*      IF own_rec-gl_classcd = 'OWNS' OR own_rec-gl_classcd = 'OWNC'.
*        MOVE '0000452005' TO  ogi_rec-saknr.
*      ELSE.
*        MOVE '0000302900' TO  ogi_rec-saknr.
*      ENDIF.
      ogi_rec-saknr = p_saknr.

*      IF ogi_rec-saknr = '0000452005'.
        IF own_rec-cons_sign = '+'.
          MOVE '261'  TO  ogi_rec-bwart.
        ELSE.
          MOVE '262'  TO  ogi_rec-bwart.
        ENDIF.
*      ELSEIF ogi_rec-saknr = '0000302900'.
*        IF own_rec-cons_sign = '+'.
*          MOVE '201'   TO  ogi_rec-bwart.
*        ELSE.
*          MOVE '202'   TO  ogi_rec-bwart.
*        ENDIF.
*      ENDIF.
* End of changes by AHMADT for CHG0185744

*Move Variant Fields
      MOVE: p_xblnr    TO   ogi_rec-xblnr,
            p_frbnr    TO   ogi_rec-frbnr,
            p_bktxt    TO   ogi_rec-bktxt,
            p_matnr    TO   ogi_rec-matnr,
            p_lgort    TO   ogi_rec-lgort_d,
            p_erfme    TO   ogi_rec-erfme,
            p_isocd    TO   ogi_rec-isocd_unit.
      CONCATENATE own_rec-cust_num+2(7) '-' own_rec-cust_num+9(7)
                                            INTO  w_cust_num.

      CONCATENATE p_bktxt w_cust_num INTO  ogi_rec-sgtxt
                  SEPARATED BY space.

*Get data from table MBEW
      CLEAR mbew-stprs.
      SELECT SINGLE stprs INTO mbew-stprs FROM mbew
       WHERE matnr = p_matnr
         AND bwkey = ogi_rec-werks_d.
      IF sy-subrc = 0.
        MOVE own_rec-trans_amt TO  w_trans_amt.
        w_erfmg = w_trans_amt / ( mbew-stprs / 1000 ).
        MOVE w_erfmg TO ogi_rec-erfmg.
      ENDIF.
* Start of changes by AHMADT for CHG0185744
*      IF own_rec-gl_classcd = 'CMPS'  OR  own_rec-gl_classcd = 'CMPN'.
        MOVE p_kostl  TO  ogi_rec-kostl.
*      ELSE.
*        CLEAR zfb04-oaufnr.
*        SELECT SINGLE oaufnr INTO zfb04-oaufnr
*          FROM zfb04
*         WHERE z_bcust = own_rec-cust_num.
*        IF sy-subrc = 0.
*          MOVE zfb04-oaufnr TO ogi_rec-aufnr.
*        ENDIF.
*      ENDIF.
* End of changes by AHMADT for CHG0185744
*Insert Record to Output file
      TRANSFER ogi_rec TO outfile.
    ENDIF.
  ENDDO.

  MESSAGE i100(zm) WITH text-100.

END-OF-SELECTION.
