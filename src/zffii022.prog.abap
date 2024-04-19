REPORT ZFFII022 MESSAGE-ID ZS.
************************************************************************
* 2005/06/28 mdemeest #426 Fixed Document Header text for Intercompany
* 2005/04/06 mdemeest #426 S&T file reformatted into BDC session
*                          This creates the customer BDC session for S&T
*
************************************************************************
TABLES: ZF017,             "Material Number Derivation
        ZFSTCN,            "Customer Offset
        ZFSTAR.            "Offset account

*------------------------  Selection Screen  ---------------------------
* Input File
SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-001.
PARAMETER: INFILE LIKE FILENAMECI-FILEEXTERN
            DEFAULT '/usr/sap/interfaces/P01/DRCO0078/zbis231_cust.chk',
           P_GROUP  LIKE BGR00-GROUP  DEFAULT 'ZFI_ST*',
           P_TCODE  LIKE TSTC-TCODE DEFAULT 'F-22'.
SELECTION-SCREEN END OF BLOCK BOX.
SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME TITLE TEXT-003.
SELECTION-SCREEN END OF BLOCK BOX3.
*-------------------------  End of Input Screen  -----------------------

*-----------------------------------------------------------------------
*-----------------------------------------------------------------------
DATA:  DELIMITER    TYPE X   VALUE '09',
       CHAR(21)     TYPE C.

DATA:  WA_INREC1(400)  TYPE C.                      "Input Record

* Used to parse file into various fields
DATA:  BEGIN OF WA        OCCURS 0,
       TYPE(10)           TYPE C,
       YEAR(4)            TYPE C,
       MONTH(2)           TYPE C,
       GL_ACCT(10)        TYPE C,
       CSTCD(6)           TYPE C,              "S&T Customer Id
       ITEM_TITLE(20)     TYPE C,
       ITEM_SUB_TITLE(20) TYPE C,
       AMOUNT(15)         TYPE C,
       VOLUME(15)         TYPE C,
       TRUE_UP_AMOUNT(20) TYPE C,
       ITEM_CLASS(7)      TYPE C.
DATA:  END OF WA.

*-----------------------------------------------------------------------
*  records for F-22 layout - Customer A/R
*-----------------------------------------------------------------------
DATA:  BEGIN OF WA1       OCCURS 0,
       CSTCD(6)           TYPE C,              "S&T Customer Id
       TYPE(10)           TYPE C,
       YEAR(4)            TYPE C,
       MONTH(2)           TYPE C,
       GL_ACCT(10)        TYPE C,
       ITEM_TITLE(20)     TYPE C,
       ITEM_SUB_TITLE(20) TYPE C,
       AMOUNT(15)         TYPE C,
       VOLUME(15)         TYPE C,
       TRUE_UP_AMOUNT(20) TYPE C,
       ITEM_CLASS(7)      TYPE C.
DATA:  END OF WA1.


DATA:  WA_CSTCD  LIKE WA-CSTCD,         "Hold field for Customer Id
       WA_WWSCT  LIKE ZFSTCN-WWSCT,     "Hold field for Customer Sector
       WA_ZXBLNR(16),                   "Reference
       WA_ZBKTXT(25),                   "Document Header Text
       WA1_ZBKTXT(25),                  "Document Header Text
       WA_TYPE   LIKE WA1-TYPE,        "Temp. storage of TYPE
       WA_MATNR  LIKE ZF017-MATNR,
       FIRST_TIME(1)  TYPE C,
       FIRST_SIDE(1)  TYPE C,
       NEGATIVE_FLAG(1) TYPE C,
       WA_OACCT  LIKE ZFSTAR-OACCT,     "Offset account for FB01's
       WA_CUSTNAME    LIKE BSEG-SGTXT,     "Customer Name
       WA_CUSTNAMEGST LIKE BSEG-SGTXT.     "Customer Name


DATA:  TBLDAT TYPE D,
       TBUDAT TYPE D,
       ZBLDAT(10),
       ZBUDAT(10),
       WA_BUDAT  LIKE SY-DATUM.



DATA:  BEGIN OF BDCDATA    OCCURS 500.
           INCLUDE STRUCTURE BDCDATA.
DATA:  END OF BDCDATA.
*-----------------------------------------------------------------------
PERFORM OPEN_FILES.                      "Open files
PERFORM READ_INPUT_FILE.                 "Read file into Work Area
CLOSE DATASET INFILE.

SORT WA1 BY CSTCD.                       "Sort into Customer Order

PERFORM CREATE_BDC.
*----------------------------  END OF PROCESSING  ---------------------

*-----------------------------------------------------------------------
* Routine to open the physical files for input in text mode.
*------------------------  OPEN_FILES ----------------------------------
FORM OPEN_FILES.
     OPEN DATASET INFILE  FOR INPUT  IN TEXT MODE.
ENDFORM.

*--------------------- READ_INPUT_FILE  --------------------------------
*  This routine reads the tab-delimited input file sent by the
*  client community and splits it into its various components.
*-----------------------------------------------------------------------
FORM  READ_INPUT_FILE.
  DO.
    CLEAR WA_INREC1.
    READ DATASET INFILE INTO WA_INREC1.

    IF SY-SUBRC <> 0.           "Exit when file is completely read in
       EXIT.
    ENDIF.

    SPLIT WA_INREC1 AT DELIMITER INTO
          WA-TYPE      WA-YEAR           WA-MONTH           WA-GL_ACCT
          WA-CSTCD     WA-ITEM_TITLE     WA-ITEM_SUB_TITLE  WA-AMOUNT
          WA-VOLUME    WA-TRUE_UP_AMOUNT WA-ITEM_CLASS.
    CONCATENATE '0000' WA-GL_ACCT INTO WA-GL_ACCT.
    MOVE-CORRESPONDING WA TO WA1.
    APPEND WA1.

*--------------------------------------------------------------------
  ENDDO.
ENDFORM.

FORM CREATE_BDC.

data: zero_amt(5) type c  value '0.00'.

  DELETE WA1 WHERE AMOUNT = zero_amt.  " 0 amounts cannot be posted

* This routine creates the standard reference & doc. header based
* on the input file -
  DO 1 TIMES.
     CONCATENATE 'S&T-' WA1-TYPE(3) '-' WA1-YEAR '/' WA1-MONTH
                                               INTO WA_ZXBLNR.
     CONCATENATE 'S&TRev-' WA1-TYPE(3) '-'
                        WA1-YEAR '/' WA1-MONTH INTO WA1_ZBKTXT.

  ENDDO.

  LOOP AT WA1.

    MOVE 'X' TO FIRST_SIDE.
*---------------------------------------------------------------------
*  if the GL number on the input file is found on the ZFSTAR file,
*  then it's a G/L Posting (40/50 or (50/40); otherwise it's a Customer
*  Posting (01/50) (11/40).
*----------------------------------------------------------------------
    CLEAR WA_OACCT.
    PERFORM OFFSET_DERIVATION.
*-------------------------------------------------------------------
*  negative amounts can not post into SAP.
*  Must change posting code & change amount to positive.
*-------------------------------------------------------------------
    CLEAR: NEGATIVE_FLAG.
    MOVE WA1-TYPE TO WA_TYPE.
    IF WA1-AMOUNT < 0.
       NEGATIVE_FLAG = 'X'.
       COMPUTE WA1-AMOUNT = WA1-AMOUNT * -1.
       MODIFY WA1.
    ENDIF.
*--------------------------------------------------------------------
    AT FIRST.
       PERFORM OPEN_BDC_SESSION.

    ENDAT.
    AT NEW CSTCD.
       REFRESH BDCDATA.
       PERFORM GET_CUSTOMER_INFO.
       FIRST_TIME = 'X'.
    ENDAT.

    IF FIRST_TIME = 'X'.
      PERFORM SCREEN_100.
      PERFORM POSTING_KEY_100.
      CLEAR: FIRST_TIME, FIRST_SIDE.
    ELSE.
      PERFORM BDC_SCREEN USING 'SAPMF05A' '0300'.
      PERFORM POSTING_KEY_300_1.

      PERFORM BDC_FIELD  USING 'RF05A-NEWKO' WA_CSTCD.
      PERFORM BDC_SCREEN USING 'SAPLKACB' '0002'.
      PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/8'.
    ENDIF.

* Choose the appropriate 2nd screen based on the transaction code
   IF  WA_OACCT = SPACE.
       PERFORM SCREEN_301.
       PERFORM POSTING_KEY_301.
   ELSE.
       PERFORM SCREEN_300.
       PERFORM POSTING_KEY_300_2.
    ENDIF.
    CLEAR FIRST_SIDE.

    PERFORM SCREEN_300.

    IF WA1-GL_ACCT = '0000256925'.   " for GST only
        PERFORM BDC_SCREEN USING 'SAPLKACB' '0002'.
        PERFORM BDC_SCREEN USING 'BDC_OKCODE' '/8'.
        PERFORM BDC_SCREEN USING 'SAPLKACB' '0002'.
        PERFORM BDC_SCREEN USING 'BDC_OKCODE' '/8'.

    ELSE.
        PERFORM CODE_BLOCK.
        PERFORM SCREEN_PA.
    ENDIF.

    AT END OF  CSTCD.
       PERFORM BDC_SCREEN USING 'SAPMF05A' '0300'.
       PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/11'.
       PERFORM BDC_SCREEN USING 'SAPLKACB' '0002'.
       PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/8'.
       PERFORM INSERT_SESSION.
    ENDAT.
    AT LAST.
       PERFORM CLOSE_BDC_SESSION.
    ENDAT.
  ENDLOOP.
ENDFORM.




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
 ENDFORM.

*----------------------------  CLOSE_BDC_SESSION  ---------------------

FORM CLOSE_BDC_SESSION.
     CALL FUNCTION 'BDC_CLOSE_GROUP'
       EXCEPTIONS
          NOT_OPEN
          QUEUE_ERROR.
     IF SY-SUBRC = '0'.
        MESSAGE ID 'ZS' TYPE 'I' NUMBER '003' WITH P_GROUP.
     ENDIF.
 ENDFORM.

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
ENDFORM.

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
ENDFORM.

*------------------------  INSERT_SESSION  ----------------------------
*  This routine inserts the BDC data for one transaction into the batch
*  input session
*-----------------------------------------------------------------------
FORM INSERT_SESSION.

  CALL FUNCTION 'BDC_INSERT'
      EXPORTING
       TCODE             = P_TCODE
      TABLES
       DYNPROTAB         = BDCDATA
      EXCEPTIONS
        INTERNAL_ORDER   = 1
        NOT_OPEN         = 2
        QUEUE_ERROR      = 3
        TCODE_INVALID    = 4.
  IF SY-SUBRC <> '0'.
     MESSAGE ID 'ZS' TYPE 'E' NUMBER '013' WITH SY-SUBRC.
  ENDIF.
ENDFORM.


*-----------------------  GET_CUSTOMER_INFO  --------------------------
FORM GET_CUSTOMER_INFO.
   CLEAR:  WA_CSTCD, WA_CUSTNAME, wa_zbktxt.
   SELECT SINGLE * FROM ZFSTCN
          WHERE CSTCD = WA1-CSTCD.
   IF SY-SUBRC = '0'.
      MOVE ZFSTCN-KUNNR              TO WA_CSTCD.  "Customer
      MOVE ZFSTCN-WWSCT              TO WA_WWSCT.  "Sector
      CONCATENATE ZFSTCN-NAME1 '-' WA_TYPE(3) INTO WA_CUSTNAME.
      IF ZFSTCN-INTERCO = 'Y'.
         CONCATENATE WA1_ZBKTXT 'Interco'     INTO WA_ZBKTXT.
      ELSE.
         CONCATENATE WA1_ZBKTXT 'Non-Interco' INTO WA_ZBKTXT.
      ENDIF.
   ELSE.
      CONCATENATE 'S' WA1-CSTCD    INTO WA_CSTCD.
      MOVE '?'                       TO WA_WWSCT.
      CONCATENATE WA1_ZBKTXT '??????'            INTO WA_ZBKTXT.
   ENDIF.
*  CONCATENATE WA_ZBKTXT WA1-MONTH '/' WA1-YEAR INTO WA_ZBKTXT.

ENDFORM.

FORM SCREEN_100.

DATA:  LAST_DAY_OF_MONTH LIKE SY-DATUM.

* convert dates
  IF WA1-TYPE = 'ACTUAL'.
     MOVE SY-DATUM                         TO TBLDAT.
     MOVE SY-DATUM                         TO TBUDAT.
  ELSE.
     MOVE SY-DATUM                            TO TBLDAT.
     CONCATENATE SY-DATUM(4) WA1-MONTH  '01' INTO WA_BUDAT.
     CALL FUNCTION 'LAST_DAY_OF_MONTHS'
         EXPORTING
           DAY_IN            = WA_BUDAT
         IMPORTING
           LAST_DAY_OF_MONTH = LAST_DAY_OF_MONTH
         EXCEPTIONS
           DAY_IN_NO_DATE.
     IF SY-SUBRC <> 0.
        WRITE: /1  'date error sy-subrc=', SY-SUBRC, TBUDAT.
     ELSE.
        MOVE LAST_DAY_OF_MONTH TO TBUDAT.
     ENDIF.
  ENDIF.

  WRITE: TBLDAT TO ZBLDAT DD/MM/YYYY.
  WRITE: TBUDAT TO ZBUDAT DD/MM/YYYY.

  PERFORM BDC_SCREEN USING 'SAPMF05A' '100'.
  PERFORM BDC_FIELD  USING 'BKPF-BLDAT' ZBLDAT.
  IF WA-TYPE = 'ESTIMATE'.
     PERFORM BDC_FIELD  USING 'BKPF-BLART' 'ST'.
  ELSE.
     PERFORM BDC_FIELD  USING 'BKPF-BLART' 'S3'.
  ENDIF.
  PERFORM BDC_FIELD  USING 'BKPF-BUKRS' 'UGL'.
  PERFORM BDC_FIELD  USING 'BKPF-BUDAT' ZBUDAT.
  PERFORM BDC_FIELD  USING 'BKPF-WAERS' 'CAD'.
  PERFORM BDC_FIELD  USING 'BKPF-XBLNR' WA_ZXBLNR.
  PERFORM BDC_FIELD  USING 'BKPF-BKTXT' WA_ZBKTXT.
ENDFORM.

FORM POSTING_KEY_100.
DATA:  WA_POSTING_KEY(2)  TYPE C.
  IF  WA_OACCT = SPACE.        "  --> Customer Number.
      PERFORM BDC_FIELD  USING 'RF05A-NEWKO' WA_CSTCD.
      CASE NEGATIVE_FLAG.
        WHEN 'X'.
          PERFORM BDC_FIELD  USING 'RF05A-NEWBS'  '11'.
          MOVE '11' TO WA_POSTING_KEY.
        WHEN ' '.
          PERFORM BDC_FIELD  USING 'RF05A-NEWBS'  '01'.
          MOVE '01' TO WA_POSTING_KEY.
      ENDCASE.
*  ELSE.                       "  --> G/L Acct
*     PERFORM BDC_FIELD  USING 'RF05A-NEWKO' WA1-GL_ACCT.
*     CASE NEGATIVE_FLAG.
*        WHEN 'X'.
*          PERFORM BDC_FIELD  USING 'RF05A-NEWBS'  '50'.
*X         MOVE '50' TO WA_POSTING_KEY.
*        WHEN ' '.
*          PERFORM BDC_FIELD  USING 'RF05A-NEWBS'  '40'.
*          MOVE '40' TO WA_POSTING_KEY.
*      ENDCASE.
  ENDIF.


ENDFORM.


*----------------------  SCREEN_301  -----------------------------------
*  Screen 301 is used only for the F-22 transaction
*-----------------------------------------------------------------------
FORM SCREEN_301.
  PERFORM BDC_SCREEN USING 'SAPMF05A'  '301'.
  PERFORM BDC_FIELD  USING 'BSEG-WRBTR'  WA1-AMOUNT.
  PERFORM BDC_FIELD  USING 'BSEG-SGTXT'  WA_ZXBLNR.
  PERFORM BDC_FIELD  USING 'BSEG-ZUONR'  TBLDAT.
ENDFORM.

FORM POSTING_KEY_301.
DATA: WA_POSTING_KEY(2) TYPE C.
  CASE NEGATIVE_FLAG.
     WHEN 'X'.
       PERFORM BDC_FIELD  USING 'RF05A-NEWBS'  '40'.
     WHEN ' '.
       PERFORM BDC_FIELD  USING 'RF05A-NEWBS'  '50'.
     ENDCASE.
  PERFORM BDC_FIELD  USING 'RF05A-NEWKO' WA1-GL_ACCT.


ENDFORM.

FORM SCREEN_300.
  PERFORM MATERIAL_DERIVATION.
  PERFORM BDC_SCREEN USING 'SAPMF05A'    '300'.
  PERFORM BDC_FIELD  USING 'BSEG-WRBTR'  WA1-AMOUNT.

  IF WA1-GL_ACCT = '0000256925'.
     CONCATENATE WA_CUSTNAME '-' 'GST' INTO WA_CUSTNAMEGST.
     PERFORM BDC_FIELD  USING 'BSEG-SGTXT'  WA_CUSTNAMEGST.
  ELSE.
     PERFORM BDC_FIELD  USING 'BSEG-SGTXT'  WA_CUSTNAME.
     PERFORM BDC_FIELD  USING 'BSEG-MWSKZ'  'O0'.
  ENDIF.

ENDFORM.

FORM CODE_BLOCK.
  PERFORM BDC_SCREEN USING 'SAPLKACB' '0002'.

  PERFORM BDC_FIELD  USING 'DKACB-XERGO' 'X'.
  PERFORM BDC_FIELD  USING 'COBL-MATNR'  WA_MATNR.

ENDFORM.

FORM POSTING_KEY_300_1.

  DATA:  WA_POSTING_KEY(8)  TYPE C.
  IF  FIRST_SIDE = 'X'.
      IF WA_OACCT = SPACE.     " --> Customer Number
         PERFORM BDC_FIELD  USING 'RF05A-NEWKO' WA_CSTCD.
         CASE NEGATIVE_FLAG.
           WHEN 'X'.
            PERFORM BDC_FIELD  USING 'RF05A-NEWBS'  '11'.
            MOVE '300-1-11' TO WA_POSTING_KEY.
           WHEN ' '.
            PERFORM BDC_FIELD  USING 'RF05A-NEWBS'  '01'.
            MOVE '300-1-01' TO WA_POSTING_KEY.
          ENDCASE.
*       ELSE.
*         PERFORM BDC_FIELD  USING 'RF05A-NEWKO' WA1-GL_ACCT.
*         CASE NEGATIVE_FLAG.
*           WHEN 'X'.
*            PERFORM BDC_FIELD  USING 'RF05A-NEWBS'  '50'.
*            MOVE '300-1-50' TO WA_POSTING_KEY.
*           WHEN ' '.
*            PERFORM BDC_FIELD  USING 'RF05A-NEWBS'  '40'.
*            MOVE '300-1-40' TO WA_POSTING_KEY.
*          ENDCASE.

      ENDIF.
  ELSE.                       "  --> G/L Acct
     PERFORM BDC_FIELD  USING 'RF05A-NEWKO' WA1-GL_ACCT.
     CASE NEGATIVE_FLAG.
        WHEN 'X'.
          PERFORM BDC_FIELD  USING 'RF05A-NEWBS'  '40'.
          MOVE '300-1-40' TO WA_POSTING_KEY.
        WHEN ' '.
          PERFORM BDC_FIELD  USING 'RF05A-NEWBS'  '50'.
          MOVE '300-1-50' TO WA_POSTING_KEY.
      ENDCASE.
  ENDIF.

ENDFORM.

FORM POSTING_KEY_300_2.

  DATA:  WA_POSTING_KEY(8)  TYPE C.
  IF  FIRST_SIDE = 'X'.
      PERFORM BDC_FIELD  USING 'RF05A-NEWKO' WA_CSTCD.
      CASE NEGATIVE_FLAG.
        WHEN 'X'.
          PERFORM BDC_FIELD  USING 'RF05A-NEWBS'  '40'.
          MOVE '300-2-11' TO WA_POSTING_KEY.
        WHEN ' '.
          PERFORM BDC_FIELD  USING 'RF05A-NEWBS'  '50'.
          MOVE '300-2-50' TO WA_POSTING_KEY.
      ENDCASE.
*   ELSE.                       "  --> G/L Acct
*     PERFORM BDC_FIELD  USING 'RF05A-NEWKO' WA_OACCT.   "WA1-GL_ACCT.
*     CASE NEGATIVE_FLAG.
*        WHEN 'X'.
*          PERFORM BDC_FIELD  USING 'RF05A-NEWBS'  '40'.
*          MOVE '300-2-40' TO WA_POSTING_KEY.
*        WHEN ' '.
*          PERFORM BDC_FIELD  USING 'RF05A-NEWBS'  '50'.
*          MOVE '300-2-50' TO WA_POSTING_KEY.
*      ENDCASE.
  ENDIF.


ENDFORM.

FORM POST.
    PERFORM BDC_FIELD  USING 'BDC_OKCODE'  '/11'.
ENDFORM.

FORM SCREEN_PA.
    PERFORM BDC_FIELD USING 'BDC_OKCODE' 'ANRE'.
    PERFORM BDC_SCREEN USING 'SAPLKEAK' '300'.
    PERFORM BDC_FIELD USING 'RKEAK-FIELD(1)' WA_CSTCD.       "Customer
    PERFORM BDC_FIELD USING 'RKEAK-FIELD(2)' 'U020'.         "Sales Org
    PERFORM BDC_FIELD USING 'RKEAK-FIELD(3)' 'UNKN'.         "Branch
    PERFORM BDC_FIELD USING 'RKEAK-FIELD(4)' WA_WWSCT.       "Sector
*    perform bdc_field using 'RKEAK-FIELD(5)'.
*    perform bdc_field using 'RKEAK-FIELD(6)'.
*    perform bdc_field using 'RKEAK-FIELD(7)'.

    PERFORM BDC_SCREEN USING 'SAPLKACB' '002'.
    PERFORM BDC_FIELD  USING 'BDC_OKCODE' 'ANRE'.
    PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/8'.


ENDFORM.

*------------------------  MATERIAL_DERIVATION -------------------------
FORM MATERIAL_DERIVATION.
* Material Number Derivation from ZF017 table.
     SELECT SINGLE * FROM ZF017
         WHERE SAKNR = WA1-GL_ACCT.
     IF SY-SUBRC = 0.
         MOVE ZF017-MATNR TO WA_MATNR.
     ELSE.
         MOVE '??MATERIAL'   TO WA_MATNR.
     ENDIF.
ENDFORM.
*-----------------------------------------------------------------------

*-------------------------  OFFSET_DERIVATION  -------------------------
FORM OFFSET_DERIVATION.

     SELECT SINGLE * FROM ZFSTAR
        WHERE SAKNR = WA1-GL_ACCT
          AND ITEMCL = WA1-ITEM_CLASS.
     IF SY-SUBRC = '0'.
        MOVE ZFSTAR-OACCT TO WA_OACCT.
     ENDIF.



ENDFORM.
