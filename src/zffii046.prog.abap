REPORT ZFFII046 MESSAGE-ID ZS NO STANDARD PAGE HEADING LINE-SIZE 255.
************************************************************************
* 2012/02/20 gymana SDP41375 - Modified selection screen to add clearing
*                              date as an option.
* 2011/01/17 gymana TR894 - Renaming this program from ZFFII005 to
*                           ZFFII046 so it can be used in the EAST.
* 2004/01/14 ug6728   i1205 Changes for the West System:
*                     - Add more fields to file.
*
* 1999/04/27 mdemeest #677 Extract BKPF & BSEG fields from SAP into
*                          report that can be downloaded to C: drive
*                          Written for the Transportation Department
*
************************************************************************
TABLES: BKPF,           "Accounting Document header
        BSEG,           "Accounting Document segment
        LFA1,           "Vendor Master (General Section)
        LFB1,           "Vendor Master (Company code)
        MAKT,           "Material Description
        EKPO,           "Purchasing Document Item               i1205
        EKKO,           "Purchasing Document Header             i1205
        AFIH,           "Maintenance Order header               i1205
        ILOA,           "PM Object location and acct assignment i1205
        IFLOTX,         "Functional location: Short Texts       i1205
        AUFK.           "Order Master Data                      i1205

SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME.
PARAMETER:  OUTFILE(60)
              DEFAULT '/usr/sap/interfaces/$sys/OUTFILES/ZFFII005.csv'
                                  LOWER CASE.
SELECT-OPTIONS: S_BUKRS FOR  BKPF-BUKRS.
PARAMETER:      P_GJAHR LIKE BKPF-GJAHR DEFAULT SY-DATUM(4) OBLIGATORY.
SELECT-OPTIONS: S_MONAT FOR  BKPF-MONAT OBLIGATORY DEFAULT '01' TO '12',
                S_BLART FOR  BKPF-BLART,
                S_HKONT FOR  BSEG-HKONT,
                S_LIFNR FOR  BSEG-LIFNR,
                S_NPLNR FOR  BSEG-NPLNR,
                S_MATNR FOR  BSEG-MATNR,
                S_AUFNR FOR  BSEG-AUFNR,
                S_KOSTL FOR  BSEG-KOSTL,
                S_AUGDT FOR  BSEG-AUGDT.                       "SDP41375

*SELECTION-SCREEN SKIP 1.                                      "SDP41375
*SELECTION-SCREEN BEGIN OF LINE.                               "SDP41375
*   SELECTION-SCREEN COMMENT  1(26) TEXT-001.                  "SDP41375
*      PARAMETERS: OPNTRN   AS CHECKBOX.                       "SDP41375
*   SELECTION-SCREEN END OF LINE.                              "SDP41375
SELECTION-SCREEN END OF BLOCK BOX3.

DATA: WS_SYMBOLIC(4) TYPE C VALUE '$sys'. "99/05/10

DATA: TMP_NAME  LIKE LFA1-NAME1,
*      TMP_ZWELS LIKE LFB1-ZWELS,
      PRV-BELNR LIKE BKPF-BELNR,
      TMP_EBELN LIKE BSEG-EBELN,
      TMP_LIFNR LIKE BSEG-LIFNR,
      PRV_LIFNR LIKE BSEG-LIFNR.
DATA: MSG(100),                      "open file-system message i1205
      W_CHANGE(2) TYPE C VALUE ',^'. "To replace comma with ^  i1205
DATA: BIG_STRING TYPE STRING.

DATA: BEGIN OF WA OCCURS 50000,
      BUKRS LIKE BKPF-BUKRS,
      BELNR LIKE BKPF-BELNR,
      GJAHR LIKE BKPF-GJAHR,
      BLDAT LIKE BKPF-BLDAT,
      BUDAT LIKE BKPF-BUDAT,              "Posting date        i1205
      MONAT LIKE BKPF-MONAT,
      BLART LIKE BKPF-BLART,
      XBLNR LIKE BKPF-XBLNR,              "Invoice Ref. Doc.   i1205
      BKTXT LIKE BKPF-BKTXT,              "Inv. Ref. Doc.Text  i1205
      BSCHL LIKE BSEG-BSCHL,              "Posting Key         i1205
      DMBTR(16) TYPE C,
      SHKZG LIKE BSEG-SHKZG,
      MWSKZ LIKE BSEG-MWSKZ,              "Tax Code            i1205
      KOSTL     LIKE BSEG-KOSTL,
*      PROJK(24) TYPE C,
      AUFNR     LIKE BSEG-AUFNR,
      TPLNR LIKE CAUFVD-TPLNR,            "Func. Location      i1205
      PLTXT LIKE IFLOTX-PLTXT,            "Func. Location Text i1205
      NPLNR LIKE BSEG-NPLNR,              "Network Number      i1205
      KTEXT LIKE AUFK-KTEXT,              "Network Text        i1205
*      SAKNR     LIKE BSEG-SAKNR,
      HKONT     LIKE BSEG-HKONT,
      EBELN LIKE BSEG-EBELN,              "PO Number           i1205
      EBELP LIKE BSEG-EBELP,              "PO Item             i1205
      WERKS LIKE EKPO-WERKS,              "Plant               i1205
      LGORT LIKE EKPO-LGORT,              "Storage Location    i1205
      LIFNR     LIKE BSEG-LIFNR,
      NAME      LIKE LFA1-NAME1,
*      ZWELS     LIKE LFB1-ZWELS,
      MATNR     LIKE BSEG-MATNR,
      MAKTX     LIKE MAKT-MAKTX,
      SGTXT     LIKE BSEG-SGTXT,
      END OF WA.

*set up appropriate path dependent on client
AT SELECTION-SCREEN OUTPUT.
  REPLACE WS_SYMBOLIC WITH SY-SYSID INTO OUTFILE.
  CONDENSE OUTFILE NO-GAPS.


START-OF-SELECTION.
  OPEN DATASET OUTFILE FOR OUTPUT IN TEXT MODE MESSAGE MSG.
  IF SY-SUBRC NE '0'.
     MESSAGE E002 WITH OUTFILE MSG.
     STOP.
  ENDIF.

SELECT BUKRS BELNR GJAHR BLDAT MONAT BLART BUDAT XBLNR BKTXT
  INTO (BKPF-BUKRS, BKPF-BELNR, BKPF-GJAHR, BKPF-BLDAT, BKPF-MONAT,
        BKPF-BLART, BKPF-BUDAT, BKPF-XBLNR, BKPF-BKTXT)
  FROM BKPF
 WHERE BUKRS IN S_BUKRS
   AND GJAHR = P_GJAHR
   AND MONAT IN S_MONAT
   AND BLART IN S_BLART.

   CLEAR WA.
   PERFORM MOVE_BKPF_TO_WA.                      "Move info to work area

   SELECT  DMBTR KOSTL AUFNR HKONT AUGDT LIFNR MATNR
           SGTXT BSCHL MWSKZ EBELN EBELP NPLNR SHKZG
     INTO (BSEG-DMBTR, BSEG-KOSTL, BSEG-AUFNR, BSEG-HKONT, BSEG-AUGDT,
           BSEG-LIFNR, BSEG-MATNR, BSEG-SGTXT, BSEG-BSCHL, BSEG-MWSKZ,
           BSEG-EBELN, BSEG-EBELP, BSEG-NPLNR, BSEG-SHKZG)
     FROM  BSEG
     WHERE BUKRS EQ BKPF-BUKRS
       AND BELNR EQ BKPF-BELNR
       AND GJAHR EQ BKPF-GJAHR
       AND HKONT IN S_HKONT
       AND AUFNR IN S_AUFNR
       AND MATNR IN S_MATNR
       AND KOSTL IN S_KOSTL
       AND NPLNR IN S_NPLNR
       AND AUGDT IN S_AUGDT.                                   "SDP41375

    PERFORM MOVE_BSEG_TO_WA.    "Move info to work area        "SDP41375
*   IF OPNTRN = 'X'.                                           "SDP41375
*      PERFORM MOVE_BSEG_TO_WA.    "Move info to work area     "SDP41375
*   ELSE.                                                      "SDP41375
*      IF BSEG-AUGDT <> 0.                                     "SDP41375
*         PERFORM MOVE_BSEG_TO_WA. "Move info to work area      SDP41375
*      ENDIF.                                                  "SDP41375
*   ENDIF.                                                     "SDP41375

  ENDSELECT.                                     "END of BSEG
ENDSELECT.
                                   "END of BKPF

SORT WA BY BUKRS BELNR LIFNR DESCENDING GJAHR ASCENDING MONAT ASCENDING.

 CONCATENATE 'Company' 'Document' 'Year' 'Doc date' 'Posting Date'
             'Period'  'Doc Type'  'Ref Document' 'Doc Header Text'
             'Posting Key' 'Amount' 'Debit/Credit Ind' 'Tax Code'
             'Cost Center' 'Order' 'Func Location' 'F Loc Text'
             'Network No' 'N.W Text' 'G/L Acct' 'Purchasing Doc'
             'Purchasing Item' 'Plant' 'Location' 'Vendor'
             'Vendor Name' 'Material' 'Material Description' 'ItemText'
              INTO BIG_STRING SEPARATED BY ','.
   TRANSFER BIG_STRING TO OUTFILE.
   CLEAR BIG_STRING.

LOOP AT WA.
 TMP_LIFNR = WA-LIFNR.
 TMP_EBELN = WA-EBELN.
 AT NEW BELNR.
    CLEAR: WA-NAME, TMP_NAME.
    PRV_LIFNR = TMP_LIFNR.
    IF TMP_LIFNR = SPACE.
       SELECT SINGLE LIFNR INTO PRV_LIFNR
         FROM EKKO
        WHERE EBELN = TMP_EBELN.
    ENDIF.

   IF PRV_LIFNR <> SPACE.
   SELECT SINGLE NAME1 FROM LFA1 INTO TMP_NAME
     WHERE LIFNR = PRV_LIFNR.
*   SELECT SINGLE ZWELS FROM LFB1 INTO TMP_ZWELS
*     WHERE LIFNR = PRV_LIFNR
*       AND BUKRS = WA-BUKRS.
   ENDIF.
 ENDAT.

 IF NOT PRV_LIFNR IN S_LIFNR.  "Lifner could be coming from BSEG or LFA1
    CONTINUE.                  "That's why this check is not applied in
 ENDIF.                        "Select statements.

 TRANSLATE TMP_NAME USING W_CHANGE.
 WA-LIFNR = PRV_LIFNR.
 WA-NAME  = TMP_NAME.
* WA-ZWELS = TMP_ZWELS.

 CLEAR WA-MAKTX.
 IF WA-MATNR <> SPACE.
   SELECT SINGLE MAKTX FROM MAKT INTO WA-MAKTX
     WHERE MATNR = WA-MATNR
       AND SPRAS = SY-LANGU.

   TRANSLATE WA-MAKTX USING W_CHANGE.
 ENDIF.

 CLEAR: WA-WERKS, WA-LGORT, WA-TPLNR,                "i1205
        WA-PLTXT, WA-KTEXT.                          "i1205

 IF WA-EBELN <> SPACE.
    SELECT SINGLE WERKS LGORT                        "i1205
      INTO (WA-WERKS, WA-LGORT)                      "i1205
      FROM EKPO                                      "i1205
     WHERE EBELN = WA-EBELN                          "i1205
       AND EBELP = WA-EBELP.                         "i1205
 ENDIF.                                              "i1205

 IF WA-AUFNR  <> 0.                                  "i1205
    SELECT SINGLE ILOAN INTO AFIH-ILOAN              "i1205
      FROM AFIH                                      "i1205
     WHERE AUFNR = WA-AUFNR.                         "i1205
     IF SY-SUBRC = 0.
        SELECT SINGLE TPLNR INTO WA-TPLNR            "i1205
          FROM ILOA                                  "i1205
         WHERE ILOAN = AFIH-ILOAN.                   "i1205
         IF SY-SUBRC = 0.
            IF WA-TPLNR <> SPACE.                    "i1205
               SELECT SINGLE PLTXT INTO WA-PLTXT     "i1205
                 FROM IFLOTX                         "i1205
                WHERE TPLNR = WA-TPLNR.              "i1205
                TRANSLATE WA-PLTXT USING W_CHANGE.   "i1205
            ENDIF.                                   "i1205
         ENDIF.                                      "i1205
      ENDIF.                                         "i1205
 ENDIF.                                              "i1205

 IF WA-NPLNR <> SPACE.                               "i1205
    SELECT SINGLE KTEXT INTO WA-KTEXT                "i1205
      FROM AUFK                                      "i1205
     WHERE AUFNR = WA-NPLNR.                         "i1205
    TRANSLATE WA-KTEXT USING W_CHANGE.               "i1205
 ENDIF.                                              "i1205

 CONCATENATE  WA-BUKRS WA-BELNR WA-GJAHR WA-BLDAT WA-BUDAT WA-MONAT
              WA-BLART WA-XBLNR WA-BKTXT WA-BSCHL WA-DMBTR WA-SHKZG
              WA-MWSKZ WA-KOSTL          WA-AUFNR WA-TPLNR WA-PLTXT
              WA-NPLNR WA-KTEXT          WA-HKONT WA-EBELN WA-EBELP
              WA-WERKS WA-LGORT WA-LIFNR WA-NAME           WA-MATNR
              WA-MAKTX WA-SGTXT
              INTO  BIG_STRING SEPARATED BY ','.
   TRANSFER BIG_STRING TO OUTFILE.
   CLEAR BIG_STRING.
*   TRANSFER WA TO OUTFILE.

ENDLOOP.

CLOSE DATASET OUTFILE.

*---------------------- MOVE_BKPF_TO_WA  -------------------------------
* move fields from BKPF to OUTPUT RECORD
*-----------------------------------------------------------------------
FORM MOVE_BKPF_TO_WA.
  CLEAR WA.
  MOVE: BKPF-BUKRS TO WA-BUKRS,
        BKPF-BELNR TO WA-BELNR,
        BKPF-GJAHR TO WA-GJAHR,
        BKPF-BLDAT TO WA-BLDAT,
        BKPF-MONAT TO WA-MONAT,
        BKPF-BLART TO WA-BLART,
        BKPF-BUDAT TO WA-BUDAT,              " i1205
        BKPF-XBLNR TO WA-XBLNR,              " i1205
        BKPF-BKTXT TO WA-BKTXT.              " i1205
        TRANSLATE WA-XBLNR USING W_CHANGE.   " i1205
        TRANSLATE WA-BKTXT USING W_CHANGE.   " i1205

ENDFORM.

*---------------------- MOVE_BSET_TO_WA  -------------------------------
* move fields from BSEG to OUTPUT RECORD
*-----------------------------------------------------------------------
FORM MOVE_BSEG_TO_WA.
  CLEAR: WA-SHKZG, WA-DMBTR, WA-KOSTL,
         WA-AUFNR, WA-HKONT, WA-LIFNR,
         WA-MATNR, WA-SGTXT, WA-BSCHL, WA-MWSKZ, WA-EBELN, WA-EBELP.

  MOVE BSEG-SHKZG      TO WA-SHKZG.
  MOVE BSEG-DMBTR      TO WA-DMBTR.
  MOVE BSEG-KOSTL      TO WA-KOSTL.
*  WRITE BSEG-PROJK     TO WA-PROJK.
  MOVE BSEG-AUFNR      TO WA-AUFNR.
*  MOVE BSEG-SAKNR      TO WA-SAKNR.
  MOVE BSEG-HKONT      TO WA-HKONT.
  MOVE BSEG-LIFNR      TO WA-LIFNR.
  MOVE BSEG-MATNR      TO WA-MATNR.
  MOVE BSEG-SGTXT      TO WA-SGTXT.
  TRANSLATE WA-SGTXT USING W_CHANGE.         " i1205
  MOVE BSEG-BSCHL      TO WA-BSCHL.          " i1205
  MOVE BSEG-MWSKZ      TO WA-MWSKZ.          " i1205
  MOVE BSEG-EBELN      TO WA-EBELN.          " i1205
  MOVE BSEG-EBELP      TO WA-EBELP.          " i1205
  MOVE BSEG-NPLNR      TO WA-NPLNR.          " i1205
  APPEND WA.
ENDFORM.
