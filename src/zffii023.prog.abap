REPORT ZFFII023 MESSAGE-ID ZS.
************************************************************************
* 2009/02/19 gymana - TR580 6.0 Upgrade
*                     Modified profitability segment code to match
*                     changes in screen SAPLKEAK-0300
* 2007/10/31 mdemeest TR453 Add salesperson
* 2006/06/07 mdemeest #--- S&T Added SKB! validation for material & pa
* 2006/06/05 mdemeest #--- S&T with volumes
* 2005/12/02 mdemeest #--- Use year from file for posting date
* 2005/07/22 mdemeest #426 Fix Document Text
* 2005/04/06 mdemeest #--- S&T file - reformatted into BDC session
*                                     for G/L posting
* 2022/04/25 NAGIRIR CHG0247976 Populate Party ID At Text Field for    *
*                    D30K932171 S&T Postings                           *
************************************************************************
TABLES: ZF017,             "Material Number Derivation
        ZFSTAR,           "Offset account
        ZFSTCN,
        SKB1,             "G/L Master
        ZFC02.

*------------------------  Selection Screen  ---------------------------
* Input File
SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME.        "TITLE TEXT-001.
PARAMETER: INFILE LIKE FILENAMECI-FILEEXTERN
              DEFAULT '/usr/sap/interfaces/P01/DRCO0078/zbis231_gl.chk',
            P_GROUP  LIKE BGR00-GROUP  DEFAULT 'ZFI-GL_EST',
            P_TCODE  LIKE TSTC-TCODE DEFAULT 'FB01'.
SELECTION-SCREEN END OF BLOCK BOX.
SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME.       "TITLE TEXT-003.
SELECTION-SCREEN END OF BLOCK BOX3.
*-------------------------  End of Input Screen  -----------------------

*-----------------------------------------------------------------------
*-----------------------------------------------------------------------
DATA:  DELIMITER    TYPE X   VALUE '09'.
*       CHAR(21)     TYPE C.

DATA:  WA_INREC1(400)  TYPE C.                      "Input Record

* Used to parse file into various fields
DATA:  BEGIN OF WA        OCCURS 0,
       ITEM_TYPE(10)           TYPE C,
       APPLIED_YEAR(4)            TYPE C,
       APPLIED_MONTH(2)           TYPE C,
       PTY_ID(8)                  TYPE C,
       RATE_CLASS(12)             TYPE C,
       SR_USAGE(4)                TYPE C,
       AMOUNTZ(15) TYPE C,
       CONV_VOLZ(15) TYPE C,
       VOLUME_UOM(8)              TYPE C,
       ITEM_CLASS(8)              TYPE C,
       EXCHANGE_RATE(20)          TYPE C,
       SAKNR LIKE ZSTGL-SAKNR,
       MWSKZ LIKE ZSTGL-MWSKZ,
       MATNR LIKE ZSTGL-MATNR,
       OACCT LIKE ZSTGL-OACCT,
       OMWSKZ LIKE ZSTGL-OMWSKZ,
       WWRATE LIKE ZFC02-WWRAT,
       BUKRS LIKE ZFC02-BUKRS,
       SRUSERSUP(10)  TYPE C,
       WAERS LIKE ZSTCURR-WAERS,
       CONV_VOL TYPE P DECIMALS 5,
       AMOUNTN TYPE P DECIMALS 2,
       WWSLS(18)                  TYPE C,                   "TR453
       PTY_ID1(8)                  TYPE c." Added for D30K932171
DATA:  END OF WA.

*-----------------------------------------------------------------------
*  records for F-22 layout - Customer A/R
*-----------------------------------------------------------------------
DATA:  BEGIN OF WA1       OCCURS 0,
       SORT_KEY(28)               TYPE C,                        "ML
       PTY_ID(8)                  TYPE C,
       EXCHANGE_RATE(20)          TYPE C,

       ITEM_TYPE(10)              TYPE C,
       APPLIED_YEAR(4)            TYPE C,
       APPLIED_MONTH(2)           TYPE C,
       RATE_CLASS(12)             TYPE C,
       SR_USAGE(4)                TYPE C,
       AMOUNTZ(15) TYPE C,
       CON_VOLZ(15) TYPE C,

       VOLUME_UOM(8)              TYPE C,
       ITEM_CLASS(8)              TYPE C,
       SAKNR LIKE ZSTGL-SAKNR,
       MWSKZ LIKE ZSTGL-MWSKZ,
       MATNR LIKE ZSTGL-MATNR,
       OACCT LIKE ZSTGL-OACCT,
       OMWSKZ LIKE ZSTGL-OMWSKZ,
       WWRATE LIKE ZFC02-WWRAT,
       BUKRS LIKE ZFC02-BUKRS,
       SRUSERSUP(10)  TYPE C,
       WAERS LIKE ZSTCURR-WAERS,
       CONV_VOL TYPE P DECIMALS 5,
       WWSLS(18)                  TYPE C,                   "TR453
       PTY_ID1(8)                 TYPE C." Added for D30K932171
DATA:  END OF WA1.


*DATA:  WA_CSTCD  LIKE WA-CSTCD,         "Hold field for Customer Id
DATA:   WA_WWSCT  LIKE ZFSTCN-WWSCT,     "Hold field for Customer Sector
       WA_AMOUNT(17)  TYPE C,
       WA_VOLUME3 TYPE P DECIMALS 3,
       WA_VOLUME(17) TYPE C,
        WA_EXCHANGE_RATE TYPE P DECIMALS 5,

*
       WA_ZXBLNR(16),                   "Reference
       WA_ZBKTXT(25),                   "Document Header Text

       WA1_ZBKTXT(25),                   "Document Header Text    #426
       WA_TYPE   LIKE WA1-ITEM_TYPE,        "Temp. storage of TYPE
       WA_MATNR  LIKE ZF017-MATNR,
       FIRST_TIME(1)  TYPE C,
       SECOND_SIDE(1)  TYPE C,
       NEGATIVE_FLAG(1) TYPE C,
       WA_OACCT  LIKE ZFSTAR-OACCT,     "Offset account for FB01's
       WA_CSTCD LIKE ZFSTCN-CSTCD,
       WA_CUSTNAME LIKE BSEG-SGTXT.     "Customer Name

DATA:  TBLDAT TYPE D,
       TBUDAT TYPE D,
       ZBLDAT(10),
       ZBUDAT(10).



DATA:  BEGIN OF BDCDATA    OCCURS 500.
        INCLUDE STRUCTURE BDCDATA.
DATA:  END OF BDCDATA.
*-----------------------------------------------------------------------
PERFORM OPEN_FILES.            "Open files
PERFORM READ_INPUT_FILE.       "Read file into Work Area
CLOSE DATASET INFILE.

SORT WA1 BY SORT_KEY.          "Sort into Customer Order & Exchange Rate

PERFORM CREATE_BDC.
*----------------------------  END OF PROCESSING  ---------------------

*-----------------------------------------------------------------------
* Routine to open the physical files for input in text mode.
*------------------------  OPEN_FILES ----------------------------------
FORM OPEN_FILES.
  OPEN DATASET INFILE  FOR INPUT  IN TEXT MODE.
ENDFORM.                    "OPEN_FILES

*--------------------- READ_INPUT_FILE  --------------------------------
*  This routine reads the tab-delimited input file sent by the
*  client community and splits it into its various components.
*-----------------------------------------------------------------------
FORM  READ_INPUT_FILE.
  DO.
    CLEAR WA_INREC1.
    READ DATASET INFILE INTO WA.

    IF SY-SUBRC <> 0.           "Exit when file is completely read in
      EXIT.
    ENDIF.

    MOVE-CORRESPONDING WA TO WA1.
    CONCATENATE WA1-PTY_ID WA1-EXCHANGE_RATE INTO WA1-SORT_KEY.   "ML
    APPEND WA1.

  ENDDO.
ENDFORM.                    "READ_INPUT_FILE

*-----------------------------------------------------------------------
FORM CREATE_BDC.

  DATA:  ZERO_AMT(5) TYPE C VALUE '0.00'.
*  DELETE WA1 WHERE AMOUNTn = zero_amt.     " 0 amounts cannot be posted

* This routine creates the standard reference & doc. header based
* on the input file -
  DO 1 TIMES.
    CONCATENATE 'S&T-' WA1-ITEM_TYPE(3) '-' WA1-APPLIED_YEAR
                                          '/' WA1-APPLIED_MONTH
                                              INTO WA_ZXBLNR.
    CONCATENATE 'S&TRev-' WA1-ITEM_TYPE(3) '-'
                WA1-APPLIED_YEAR '/' WA1-APPLIED_MONTH INTO WA1_ZBKTXT.

  ENDDO.

  LOOP AT WA1.
    MOVE ' ' TO SECOND_SIDE.
*---------------------------------------------------------------------
*  if the GL number on the input file is found on the ZFSTAR file,
*  then it's a G/L Posting (40/50 or (50/40)
*----------------------------------------------------------------------
*    CLEAR WA_OACCT.
*    PERFORM OFFSET_DERIVATION.
*-------------------------------------------------------------------
*  negative amounts can not post into SAP.
*  Must change posting code & change amount to positive.
*-------------------------------------------------------------------
    MOVE 'X' TO NEGATIVE_FLAG..
    MOVE WA1-ITEM_TYPE TO WA_TYPE.
    DATA: WA_AMOUNTN TYPE P DECIMALS 2.
    DATA:  WA_AMOUNTNEG(15) TYPE C VALUE '0.00'.
    DATA:  WA_VOLNEG(15)    TYPE C VALUE '0.00000'.
    MOVE WA1-AMOUNTZ TO WA_AMOUNTN.
    IF WA_AMOUNTN < WA_AMOUNTNEG. "0.
      NEGATIVE_FLAG = ' '.
      COMPUTE WA_AMOUNTN = WA_AMOUNTN * -1.
      MOVE WA_AMOUNTN TO WA1-AMOUNTZ.

      IF WA1-CONV_VOL < WA_VOLNEG.
        COMPUTE WA1-CONV_VOL = WA1-CONV_VOL * -1.
        MOVE WA1-CONV_VOL TO WA1-CON_VOLZ.
      ENDIF.

      MODIFY WA1.
    ENDIF.
*--------------------------------------------------------------------
    AT FIRST.
      PERFORM OPEN_BDC_SESSION.

    ENDAT.
    AT NEW SORT_KEY.     "PTY_ID.
      REFRESH BDCDATA.
      FIRST_TIME = 'X'.
    ENDAT.

    IF FIRST_TIME = 'X'.
      PERFORM SCREEN_100.
      PERFORM POSTING_KEY_100.

      PERFORM SCREEN_300.
      SELECT SINGLE * FROM SKB1
         WHERE BUKRS = WA1-BUKRS
           AND SAKNR = WA1-SAKNR
           AND FSTAG IN ('PROJ','PAYR','EXMM','BSHT').
      IF SY-SUBRC = '0'.                          "NO MATERIAL or PA
      ELSE.
        PERFORM CODE_BLOCK.
        PERFORM SCREEN_PA.
      ENDIF.

      CLEAR FIRST_TIME.

      PERFORM BDC_SCREEN USING 'SAPLKACB' '0002'.
      PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/8'.

    ELSE.
      PERFORM POSTING_KEY_300_1.

      PERFORM BDC_SCREEN USING 'SAPLKACB' '0002'.
      PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/8'.
    ENDIF.

    CLEAR SECOND_SIDE.

    PERFORM POSTING_KEY_300_2.

    PERFORM BDC_SCREEN USING 'SAPLKACB' '0002'.
    PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/8'. "'ENTE'.  '/8'.

    MOVE 'X' TO SECOND_SIDE.
    PERFORM SCREEN_300.                                     "2nd Screen


    PERFORM BDC_SCREEN USING 'SAPLKACB' '0002'.
    PERFORM BDC_FIELD  USING 'BDC_OKCODE' 'ENTE'.

    AT END OF  SORT_KEY.   "PTY_ID.         "ML
      PERFORM BDC_SCREEN USING 'SAPMF05A'   '0300'.  "Saves transaction
      PERFORM BDC_FIELD  USING 'BDC_OKCODE' 'BU'.

      PERFORM BDC_SCREEN USING 'SAPLKACB' '0002'.
      PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/8'.


      PERFORM INSERT_SESSION.
    ENDAT.
    AT LAST.
      PERFORM CLOSE_BDC_SESSION.
    ENDAT.
  ENDLOOP.
ENDFORM.                    "CREATE_BDC




*------------------------  INIT_STRUCTURES  ----------------------------
*  Initializes the specified file structure with '/' so that SAP
*  understands that a '/' indicates no data is being passed.

*----------------------------  OPEN BDC_SESSION ---------------------
FORM OPEN_BDC_SESSION.
  DATA:  WA_HOLDDATE  LIKE SY-DATUM.

  COMPUTE WA_HOLDDATE = SY-DATUM - 1.

  CALL FUNCTION 'BDC_OPEN_GROUP'
    EXPORTING
      CLIENT            = SY-MANDT
      GROUP             = P_GROUP
      HOLDDATE          = WA_HOLDDATE
      KEEP              = 'X'
      USER              = SY-UNAME
    EXCEPTIONS
      GROUP_INVALID     = 1
      GROUP_IS_LOCKED   = 2
      HOLDDATE_INVALID  = 3
      INTERNAL_ERROR    = 4
      QUEUE_ERROR       = 5
      RUNNING           = 6
      SYSTEM_LOCK_ERROR = 7
      USER_INVALID      = 8.

  IF SY-SUBRC <> '0'.
    MESSAGE ID 'ZS' TYPE 'E' NUMBER '004' WITH P_GROUP.
  ENDIF.
ENDFORM.                    "OPEN_BDC_SESSION

*----------------------------  CLOSE_BDC_SESSION  ---------------------

FORM CLOSE_BDC_SESSION.
  CALL FUNCTION 'BDC_CLOSE_GROUP'
    EXCEPTIONS
      NOT_OPEN
      QUEUE_ERROR.
  IF SY-SUBRC = '0'.
    MESSAGE ID 'ZS' TYPE 'I' NUMBER '003' WITH P_GROUP.
  ENDIF.
ENDFORM.                    "CLOSE_BDC_SESSION

*-----------------------  BDC_SCREEN  ---------------------------------
*  This routine adds an entry to the table BDCDATA with screen info
*  from a particular transaction.  This is used as part of the process
* from creating data for batch input.

FORM BDC_SCREEN USING PROGRAM DYNPRO.
  CLEAR BDCDATA.
  BDCDATA-PROGRAM  = PROGRAM.
  BDCDATA-DYNPRO   = DYNPRO.
  BDCDATA-DYNBEGIN ='X'.
  APPEND BDCDATA.
ENDFORM.                    "BDC_SCREEN

*-----------------------  BDC_FIELD  ----------------------------------
*  This routine adds an entry to the table BDCDATA with field info from
*  a particular transaction.  This is used as part of the process for
*  creating data for batch input.

FORM BDC_FIELD USING FNAM FVAL.
  CHECK FVAL <> '/' AND FVAL <> SPACE.
  CLEAR BDCDATA.
  BDCDATA-FNAM = FNAM.
  BDCDATA-FVAL = FVAL.
  APPEND BDCDATA.
ENDFORM.                    "BDC_FIELD

*------------------------  INSERT_SESSION  ----------------------------
*  This routine inserts the BDC data for one transaction into the batch
*  input session
*-----------------------------------------------------------------------
FORM INSERT_SESSION.

  CALL FUNCTION 'BDC_INSERT'
    EXPORTING
      TCODE          = P_TCODE
    TABLES
      DYNPROTAB      = BDCDATA
    EXCEPTIONS
      INTERNAL_ERROR = 1
      NOT_OPEN       = 2
      QUEUE_ERROR    = 3
      TCODE_INVALID  = 4.
  IF SY-SUBRC <> '0'.
    MESSAGE ID 'ZS' TYPE 'E' NUMBER '013' WITH SY-SUBRC.
  ENDIF.
ENDFORM.                    "INSERT_SESSION



*&---------------------------------------------------------------------*
*&      Form  SCREEN_100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SCREEN_100.

  DATA:  LAST_DAY_OF_MONTH  LIKE SY-DATUM,
         WA_BUDAT           LIKE SY-DATUM.


* convert dates
  IF WA1-ITEM_TYPE = 'ACTUAL'.
    MOVE SY-DATUM                         TO TBLDAT.
    MOVE SY-DATUM                         TO TBUDAT.
  ELSE.
    MOVE SY-DATUM                     TO TBLDAT.
    CONCATENATE WA1-APPLIED_YEAR WA1-APPLIED_MONTH INTO WA_BUDAT.
    CALL FUNCTION 'LAST_DAY_OF_MONTHS'
        EXPORTING
            DAY_IN         = WA_BUDAT
        IMPORTING
            LAST_DAY_OF_MONTH = LAST_DAY_OF_MONTH
        EXCEPTIONS
            DAY_IN_NO_DATE.
    IF  SY-SUBRC <> 0.
      WRITE: /1 'date error sy-subrc = ', SY-SUBRC, TBUDAT.
    ELSE.
      MOVE LAST_DAY_OF_MONTH TO TBUDAT.
    ENDIF.
  ENDIF.

  WRITE: TBLDAT TO ZBLDAT DD/MM/YYYY..
  WRITE: TBUDAT TO ZBUDAT DD/MM/YYYY.

  PERFORM BDC_SCREEN USING 'SAPMF05A' '100'.
  PERFORM BDC_FIELD  USING 'BKPF-BLDAT' ZBLDAT.
  IF WA1-ITEM_TYPE = 'ESTIMATE'.
    PERFORM BDC_FIELD  USING 'BKPF-BLART' 'ST'.
    PERFORM BDC_FIELD  USING 'BKPF-XBLNR' WA_ZXBLNR.
  ELSE.
    PERFORM BDC_FIELD  USING 'BKPF-BLART' 'S3'.
  ENDIF.
  PERFORM BDC_FIELD  USING 'BKPF-BUKRS' WA1-BUKRS.
  PERFORM BDC_FIELD  USING 'BKPF-BUDAT' ZBUDAT.
  PERFORM BDC_FIELD  USING 'BKPF-WAERS' WA1-WAERS.
  DATA: WA_CTR TYPE I.
  IF WA1-WAERS <> 'CAD'.
    IF WA1-EXCHANGE_RATE <> ' '.
      SEARCH WA1-EXCHANGE_RATE FOR '.'.
      SHIFT WA1-EXCHANGE_RATE LEFT DELETING LEADING SPACE.
      COMPUTE WA_CTR = SY-FDPOS + 6.  "decimal + 5 positions
      PERFORM BDC_FIELD USING 'BKPF-KURSF' WA1-EXCHANGE_RATE(WA_CTR).
    ENDIF.
  ENDIF.

  MOVE WA_ZXBLNR TO WA_ZBKTXT.
  PERFORM BDC_FIELD  USING 'BKPF-BKTXT' WA_ZBKTXT.
ENDFORM.                                                    "SCREEN_100

*&---------------------------------------------------------------------*
*&      Form  POSTING_KEY_100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM POSTING_KEY_100.
  DATA:  WA_POSTING_KEY(2)  TYPE C.
  PERFORM BDC_FIELD  USING 'RF05A-NEWKO' WA1-SAKNR.         "OACCT.
  CASE NEGATIVE_FLAG.
    WHEN ' '.
      PERFORM BDC_FIELD  USING 'RF05A-NEWBS'  '40'.
      MOVE '40 -100 ' TO WA_POSTING_KEY.
    WHEN 'X'.
      PERFORM BDC_FIELD  USING 'RF05A-NEWBS'  '50'.
      MOVE '50 - 100' TO WA_POSTING_KEY.
  ENDCASE.

ENDFORM.                    "POSTING_KEY_100


*&---------------------------------------------------------------------*
*&      Form  SCREEN_300
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SCREEN_300.

  PERFORM BDC_SCREEN USING 'SAPMF05A'    '300'.
* move wa1-amountn to wa_amount.
  PERFORM BDC_FIELD  USING 'BSEG-WRBTR'  WA1-AMOUNTZ.
  CASE SECOND_SIDE.
    WHEN ' '.                                               "Side 1
      IF WA1-MWSKZ = ' '.
      ELSE.
        PERFORM BDC_FIELD  USING 'BSEG-MWSKZ'  WA1-MWSKZ.
      ENDIF.
    WHEN 'X'.                                               "side 2
      IF WA1-OMWSKZ = '  '.
      ELSE.
        PERFORM BDC_FIELD  USING 'BSEG-MWSKZ'  WA1-OMWSKZ.
      ENDIF.
  ENDCASE.
*{Begin of Change D30K932171
  CONDENSE: WA1-PTY_ID, WA1-PTY_ID1.
  IF WA1-PTY_ID = WA1-PTY_ID1.
    PERFORM BDC_FIELD  USING 'BSEG-SGTXT' WA1-PTY_ID.
  ELSE.
    PERFORM BDC_FIELD  USING 'BSEG-SGTXT' WA1-PTY_ID1.
  ENDIF.
*End of Change D30K932171 }
  IF SECOND_SIDE = ' '.
    MOVE WA1-CONV_VOL TO WA_VOLUME3.
    MOVE WA_VOLUME3 TO WA_VOLUME.
*     if wa_volume <> 0.
* Do not post a 0 volume or volume if a suspense account
    IF ( WA_VOLUME = 0 OR WA1-SAKNR = '0000160301' ).
    ELSE.
      PERFORM BDC_FIELD USING 'BSEG-MENGE'  WA_VOLUME.
      PERFORM BDC_FIELD USING 'BSEG-MEINS'  WA1-VOLUME_UOM.
    ENDIF.
    PERFORM BDC_FIELD USING 'BSEG-ZUONR'  WA1-SRUSERSUP.
  ENDIF.
ENDFORM.                                                    "SCREEN_300

*&---------------------------------------------------------------------*
*&      Form  CODE_BLOCK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM CODE_BLOCK.
  PERFORM BDC_SCREEN USING 'SAPLKACB' '0002'.
  PERFORM BDC_FIELD  USING 'DKACB-XERGO' 'X'.
  PERFORM BDC_FIELD  USING 'COBL-MATNR'  WA1-MATNR.
ENDFORM.                    "CODE_BLOCK

*&---------------------------------------------------------------------*
*&      Form  POSTING_KEY_300_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM POSTING_KEY_300_1.

  DATA:  WA_POSTING_KEY(8)  TYPE C.
  PERFORM BDC_SCREEN USING 'SAPMF05A'    '0300'.
  PERFORM BDC_FIELD  USING 'RF05A-NEWKO' WA1-SAKNR.
  CASE NEGATIVE_FLAG.
    WHEN ' '.
      PERFORM BDC_FIELD  USING 'RF05A-NEWBS'  '40'.
      MOVE '300-1-40' TO WA_POSTING_KEY.
    WHEN 'X '.
      PERFORM BDC_FIELD  USING 'RF05A-NEWBS'  '50'.
      MOVE '300-1-50' TO WA_POSTING_KEY.
  ENDCASE.
  PERFORM BDC_SCREEN USING 'SAPLKACB' '0002'.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/8'.

  PERFORM SCREEN_300.
  SELECT SINGLE * FROM SKB1
     WHERE BUKRS = WA1-BUKRS
       AND SAKNR = WA1-SAKNR
       AND FSTAG IN ('PROJ','PAYR','EXMM','BSHT' ).
  IF SY-SUBRC = '0'.                          "NO MATERIAL or PA
  ELSE.
    PERFORM CODE_BLOCK.
    PERFORM SCREEN_PA.
  ENDIF.




ENDFORM.                    "POSTING_KEY_300_1

*&---------------------------------------------------------------------*
*&      Form  POSTING_KEY_300_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM POSTING_KEY_300_2.

  PERFORM BDC_SCREEN USING 'SAPMF05A' '0300'.

  DATA:  WA_POSTING_KEY(8)  TYPE C.
  PERFORM BDC_FIELD  USING 'RF05A-NEWKO' WA1-OACCT.   "GL_ACCT.
  CASE NEGATIVE_FLAG.
    WHEN ' '.
      PERFORM BDC_FIELD  USING 'RF05A-NEWBS'  '50'.
      MOVE '300-2-50' TO WA_POSTING_KEY.
    WHEN 'X'.
      PERFORM BDC_FIELD  USING 'RF05A-NEWBS'  '40'.
      MOVE '300-2-40' TO WA_POSTING_KEY.
  ENDCASE.
ENDFORM.                    "POSTING_KEY_300_2

*&---------------------------------------------------------------------*
*&      Form  POST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM POST.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE'  '/11'.
ENDFORM.                    "POST

*&---------------------------------------------------------------------*
*&      Form  SCREEN_PA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SCREEN_PA.
  PERFORM BDC_FIELD USING 'BDC_OKCODE' 'ANRE'.
  PERFORM BDC_SCREEN USING 'SAPLKEAK' '300'.
*-----------------------------------------------------------------------
* TR580 - 6.0 Upgrade - Screen SAPLKEAK-300 has changed. Fields below
*                       have been adjusted accordingly.
*-----------------------------------------------------------------------
  SHIFT WA1-PTY_ID LEFT DELETING LEADING SPACE.
  SELECT SINGLE * FROM ZFSTCN
     WHERE CSTCD = WA1-PTY_ID.
  IF SY-SUBRC = '0'.
    PERFORM BDC_FIELD USING 'RKEAK-FIELD(1)' ZFSTCN-KUNNR. "Customer
    PERFORM BDC_FIELD  USING 'BDC_OKCODE' 'ENTE'.
    PERFORM BDC_SCREEN USING 'SAPLKEAK' '300'.
    PERFORM BDC_FIELD USING 'RKEAK-FIELD(8)' 'U020'.       "Sales Org
    PERFORM BDC_FIELD  USING 'BDC_OKCODE' 'ENTE'.
    PERFORM BDC_FIELD  USING 'BDC_OKCODE' 'P+  '.
    PERFORM BDC_SCREEN USING 'SAPLKEAK' '300'.              "Page 2
    PERFORM BDC_FIELD USING 'RKEAK-FIELD(2)' ZFSTCN-WWSCT. "Sector
    PERFORM BDC_FIELD  USING 'BDC_OKCODE' 'ENTE'.
    PERFORM BDC_SCREEN USING 'SAPLKEAK' '300'.
    PERFORM BDC_FIELD USING 'RKEAK-FIELD(12)' 'UNKN'.      "Branch
    PERFORM BDC_FIELD  USING 'BDC_OKCODE' 'ENTE'.
    PERFORM BDC_SCREEN USING 'SAPLKEAK' '300'.
  ELSE.
    PERFORM BDC_FIELD USING 'RKEAK-FIELD(1)' WA1-PTY_ID.
    PERFORM BDC_FIELD  USING 'BDC_OKCODE' 'ENTE'.
    PERFORM BDC_SCREEN USING 'SAPLKEAK' '300'.
    PERFORM BDC_FIELD USING 'RKEAK-FIELD(8)' 'U020'.       "Sales Org
    PERFORM BDC_FIELD  USING 'BDC_OKCODE' 'ENTE'.
    PERFORM BDC_FIELD  USING 'BDC_OKCODE' 'P+  '.
    PERFORM BDC_SCREEN USING 'SAPLKEAK' '300'.              "Page 2
    PERFORM BDC_FIELD USING 'RKEAK-FIELD(2)' '3'.          "Default
    PERFORM BDC_FIELD  USING 'BDC_OKCODE' 'ENTE'.
    PERFORM BDC_SCREEN USING 'SAPLKEAK' '300'.
    PERFORM BDC_FIELD USING 'RKEAK-FIELD(12)' 'UNKN'.      "Branch
    PERFORM BDC_FIELD  USING 'BDC_OKCODE' 'ENTE'.
    PERFORM BDC_SCREEN USING 'SAPLKEAK' '300'.
  ENDIF.
*-----------------------------------------------------------------------
  SELECT SINGLE * FROM ZFC02
      WHERE C_RATECL = WA1-RATE_CLASS.
  IF SY-SUBRC = '0'.
    PERFORM BDC_FIELD USING 'RKEAK-FIELD(8)' ZFC02-WWRAT.
    PERFORM BDC_FIELD  USING 'BDC_OKCODE' 'ENTE'.
  ENDIF.

  PERFORM BDC_FIELD  USING 'BDC_OKCODE' 'P+  '.
  PERFORM BDC_SCREEN USING 'SAPLKEAK' '300'.                "Page 3
  PERFORM BDC_FIELD USING 'RKEAK-FIELD(4)' WA1-WWSLS.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' 'ENTE'.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' 'WEIT'.
ENDFORM.                    "SCREEN_PA
