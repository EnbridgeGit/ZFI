* 2004/09/13 mdemeest 4.6C Upgrade -  Copied RFFORIC9 to ZNFAP007    UGL
************************************************************************
* Include RFFORIC9, used in the payment print program RFFOCA_T         *
* with subroutines for canadian DME (standard 005)                     *
*                                                                      *
************************************************************************

*----------------------------------------------------------------------*
* FORM DME_DOMESTIC                                                    *
*----------------------------------------------------------------------*
* program produces canadian DME in the standard 005                    *
* called by END-OF-SELECTION (RFFOCA_T)                                *
*----------------------------------------------------------------------*
* no USING-parameters                                                  *
*----------------------------------------------------------------------*
FORM DME_CANADA.


* structures for records
  TABLES:
    DTADCAA,           "A - header record
    DTADCAC,           "C - one of six transaction records
   *DTADCAC,           "C - copy of DTADCAC
    DTADCAH,           "H - prefix for each record
    DTADCAZ.           "Z - trailer record

  DATA:
    BEGIN OF T_RECORD,
      HEADER     LIKE DTADCAH,
      SEGMENT    LIKE DTADCAC OCCURS 6,
    END OF T_RECORD,
    BEGIN OF C_RECORD,
      HEADER     LIKE DTADCAH,
      SEGMENT1   LIKE DTADCAC,
      SEGMENT2   LIKE DTADCAC,
      SEGMENT3   LIKE DTADCAC,
      SEGMENT4   LIKE DTADCAC,
      SEGMENT5   LIKE DTADCAC,
      SEGMENT6   LIKE DTADCAC,
    END OF C_RECORD.

 DATA: help_can(4) type n.   "note #327208



*----------------------------------------------------------------------*
* preparations for DME                                                 *
*----------------------------------------------------------------------*

* sort of extract
  SORT BY
    REGUH-ZBUKR                        "paying company code
    REGUD-XEINZ                        "incoming/outgoing payment
    REGUH-UBNKS                        "country of house bank
    REGUH-UBNKY                        "house bank (destination)
    REGUH-UBNKL                        "bank number of house bank
    REGUH-RZAWE                        "payment method
    REGUH-ZBNKS                        "country of payee's bank
    REGUH-ZBNKY                        "bank key (for sort)
    REGUH-ZBNKL                        "bank number of payee's bank
    REGUH-ZBNKN                        "account number of payee
    REGUH-LIFNR                        "creditor number
    REGUH-KUNNR                        "debitor number
    REGUH-EMPFG                        "payee is CPD / alternative payee
    REGUH-VBLNR.                       "payment document number

  HLP_DTFOR = 'CDN005'.                "format description for FDTA

* If no file-name is specified and no name will be generated later
* (because of TemSe), a new name is generated here: CDN005.Date.Time.nn
  IF HLP_TEMSE NA PAR_DTYP AND         "No TemSe-file            AND
     PAR_UNIX EQ SPACE.                "name not specified yet
    PAR_UNIX    = HLP_DTFOR.
    PAR_UNIX+6  = '.'.
    WRITE SY-DATLO TO PAR_UNIX+7(6) DDMMYY.
    PAR_UNIX+13 = '.'.
    PAR_UNIX+14 = SY-TIMLO.
    PAR_UNIX+20 = '.'.
  ENDIF.

  CNT_FILENR = 0.


*----------------------------------------------------------------------*
* loop at extracted data                                               *
*----------------------------------------------------------------------*
  LOOP.


*-- New paying company code --------------------------------------------
    AT NEW REGUH-ZBUKR.

      PERFORM BUCHUNGSKREIS_DATEN_LESEN.

    ENDAT.


*-- new house bank (destination centre) --------------------------------
    AT NEW REGUH-UBNKL.

*     Modify title of file accompanying sheet
      T042Z-TEXT1 = TEXT_004.
      REPLACE '&' WITH REGUH-HBKID INTO T042Z-TEXT1.

*     create new disk/tape
      PERFORM ZUSATZFELD_FUELLEN USING *REGUT-DTKEY 'CDN'.
      PERFORM DATEI_OEFFNEN.

*     initialize fields for summary
      CLEAR:
        CNT_FORMULARE,
        CNT_HINWEISE,
        SUM_ABSCHLUSS,
        HLP_DTA_ZWELS.

*     read house bank data
      PERFORM HAUSBANK_DATEN_LESEN.
      SELECT SINGLE * FROM T012D
        WHERE BUKRS EQ REGUH-ZBUKR
        AND   HBKID EQ REGUH-HBKID.
      IF SY-SUBRC NE 0.
        MOVE-CORRESPONDING REGUH TO ERR_T012D.
        APPEND ERR_T012D.
        CLEAR T012D.
      ENDIF.

*     fill prefix of all records
      CLEAR DTADCAH.
      DTADCAH-TYPE  = 'A'.                "logical record type A,C,D,Z
      DTADCAH-RCNT  = 1.                  "logical record count
      DTADCAH-ORIG  = T012D-DTFIN.        "Originator's Id
      PERFORM COMPUTE_CREATION_NUMBER.    "file creation number
      PERFORM:
        DTA_TEXT_AUFBEREITEN USING DTADCAA-ORIG,
        DME_VALID_CHARACTERS USING DTADCAA-ORIG.

*     fill and store the header record
      CLEAR DTADCAA.
      MOVE-CORRESPONDING DTADCAH TO DTADCAA.


      DTADCAA-ADEST = T012D-DTBID.        "destination data centre
      DTADCAA-ACUST = SPACE.              "optional, leave blank
      DTADCAA-AFILL = SPACE.              "filler

*-------------------------------------------------------------------UGL
      dtadcaa-afill+1193(4)  = '0001'.                             "UGL
      dtadcaa-afill+1197(4)  = '01'.                               "UGL
      dtadcaa-afill+1199(9)  = reguh-ubnky.                        "UGL
      dtadcaa-afill+1208(12) = reguh-ubknt.                        "UGL
*-------------------------------------------------------------------UGL

      PERFORM CONVERT_DATE USING SY-DATLO CHANGING DTADCAA-ACRDT.

      PERFORM STORE_ON_FILE USING DTADCAA.
      PERFORM CR_LF.

*     prepare the first transaction record
      IF REGUD-XEINZ EQ SPACE.
        DTADCAH-TYPE = 'C'.
      ELSE.
        DTADCAH-TYPE = 'D'.
      ENDIF.
      ADD 1 TO DTADCAH-RCNT.
      CLEAR T_RECORD.
      T_RECORD-HEADER = DTADCAH.
      CNT_RECORDS = 1.

*     prepare the trailer record
      CLEAR DTADCAZ.

    ENDAT.


*-- new payment method -------------------------------------------------
    AT NEW REGUH-RZAWE.

      PERFORM ZAHLWEG_DATEN_LESEN.
      T042Z-TEXT1 = TEXT_004.
      REPLACE '&' WITH REGUH-HBKID INTO T042Z-TEXT1.

*     Store payment method in array of methods used
      PERFORM ZAHLWEG_EINFUEGEN USING REGUH-RZAWE HLP_DTA_ZWELS.

    ENDAT.


*-- new payment document number ----------------------------------------
    AT NEW REGUH-VBLNR.

      PERFORM ZAHLUNGS_DATEN_LESEN.
      PERFORM SUMMENFELDER_INITIALISIEREN.
      PERFORM BELEGDATEN_SCHREIBEN.
      SET LANGUAGE HLP_SPRACHE.

*     fill the transaction record (specific FI-/HR-fields)
      CLEAR DTADCAC.
      IF HLP_LAUFK NE 'P'.

*        DTADCAC-C1TYP   = 430.              "bill payment          UGL
         DTADCAC-C1typ   = 460.              "expense report        UGL

        DTADCAC-C1XRE   = REGUH-VBLNR.      "cross reference for FI
        IF REGUH-RPOST EQ 1.
          IF REGUP-XBLNR EQ SPACE.
            REGUP-XBLNR = REGUP-BELNR.
          ENDIF.
          DTADCAC-C1INF = REGUP-XBLNR.      "orig sundry info
          ADD 1 TO CNT_HINWEISE.
          MOVE-CORRESPONDING REGUH TO TAB_KEIN_AVIS.
          APPEND TAB_KEIN_AVIS.
        ELSE.
          DTADCAC-C1INF = TEXT-704.         "see payment advice
        ENDIF.
      ELSE.
        DTADCAC-C1TYP   = 200.              "payroll deposit
        DTADCAC-C1XRE   = REGUH-PERNR.      "cross reference for HR

        DTADCAC-C1INF   = REGUP-SGTXT.      "orig sundry info
*       DTADCAC-C1INF   = SPACE.            "note #185930  either..or...

      ENDIF.

*     fill the transaction record (general fields)
      PERFORM CONVERT_DATE USING REGUH-ZALDT CHANGING DTADCAC-C1DAT.
      DTADCAC-C1AMT     = REGUH-RWBTR.      "amount
      DTADCAC-C1INS     = REGUH-ZBNKL.      "bank and branch number
      DTADCAC-C1PAY     = REGUH-ZBNKN.      "account number
      DTADCAC-C1TRA     = 0.                "item trace no
      DTADCAC-C1STT     = 0.                "stored transaction type
      DTADCAC-C1SOR     = REGUD-AUST1.      "originator's short name
      DTADCAC-C1PYN     = REGUH-KOINH.      "payee's name
      DTADCAC-C1LOR     = REGUD-AUST2.      "originator's long name
      DTADCAC-C1CLI     = DTADCAA-ORIG.     "originating direct clearer

*     note #189586  (Royal Bank)       C1RID and C1RAC commented out
*     note #336624  (Bank of Montreal) C1RID and C1RAC used
      DTADCAC-C1RID     = REGUH-UBNKL.      "house bank (for returns)
      DTADCAC-C1RAC     = REGUH-UBKNT.      "house bank account ( " )

      DTADCAC-C1FIL     = SPACE.            "filler

*-------------------------------------------------------------------UGL
*      DTADCAC-C1SET     = SPACE.      "orig direct clearer settl   UGL
       DTADCAC-C1SET     = '01'.       "orig direct clearer settl   UGL
*-------------------------------------------------------------------UGL

      DTADCAC-C1INV     = 0.                "invalid data element ID
      PERFORM DTA_TEXT_AUFBEREITEN USING:
        DTADCAC-C1SOR, DTADCAC-C1PYN, DTADCAC-C1LOR, DTADCAC-C1INF.
      PERFORM DME_VALID_CHARACTERS USING:
        DTADCAC-C1SOR, DTADCAC-C1PYN, DTADCAC-C1LOR, DTADCAC-C1INF.

*     calculate the totals for the trailer record
      CASE DTADCAH-TYPE.
        WHEN 'C'.
          ADD DTADCAC-C1AMT TO DTADCAZ-TOTVC.
          ADD 1 TO DTADCAZ-TOTNC.
        WHEN 'D'.
          ADD DTADCAC-C1AMT TO DTADCAZ-TOTVD.
          ADD 1 TO DTADCAZ-TOTND.
*        WHEN 'E'.                           "E not supported
*          ADD DTADCAC-C1AMT TO DTADCAZ-TOTVE.
*          ADD 1 TO DTADCAZ-TOTNE.
*        WHEN 'F'.                           "F not supported
*          ADD DTADCAC-C1AMT TO DTADCAZ-TOTVF.
*          ADD 1 TO DTADCAZ-TOTNF.
      ENDCASE.

*     store transaction records (if more than six)
      IF CNT_RECORDS GT 6.
        C_RECORD-HEADER = T_RECORD-HEADER.
        DO 6 TIMES VARYING *DTADCAC
                      FROM C_RECORD-SEGMENT1
                      NEXT C_RECORD-SEGMENT2.
           READ TABLE T_RECORD-SEGMENT INDEX SY-INDEX INTO *DTADCAC.
        ENDDO.
        PERFORM STORE_ON_FILE USING C_RECORD.
        PERFORM CR_LF.

*       prepare the next transaction record
        ADD 1 TO DTADCAH-RCNT.
        CLEAR T_RECORD.
        T_RECORD-HEADER = DTADCAH.
        CNT_RECORDS = 1.
      ENDIF.

      INSERT DTADCAC INTO T_RECORD-SEGMENT INDEX CNT_RECORDS.
      ADD 1 TO CNT_RECORDS.

    ENDAT.


*-- end of payment document number -------------------------------------
    AT END OF REGUH-VBLNR.

*     add up total amount fields
      ADD 1            TO CNT_FORMULARE.
      ADD REGUH-RBETR  TO SUM_ABSCHLUSS.
      WRITE:
        CNT_HINWEISE   TO REGUD-AVISH,
        CNT_FORMULARE  TO REGUD-ZAHLT,
        SUM_ABSCHLUSS  TO REGUD-SUMME CURRENCY T001-WAERS.
      TRANSLATE:
        REGUD-AVISH USING ' *',
        REGUD-ZAHLT USING ' *',
        REGUD-SUMME USING ' *'.
      SET LANGUAGE SY-LANGU.           "back to user language

    ENDAT.


*-- end of house bank --------------------------------------------------
    AT END OF REGUH-UBNKL.

*     store the last transaction record
      C_RECORD-HEADER = T_RECORD-HEADER.
      CLEAR CNT_RECORDS.
      DO 6 TIMES VARYING *DTADCAC
                    FROM C_RECORD-SEGMENT1
                    NEXT C_RECORD-SEGMENT2.
        READ TABLE T_RECORD-SEGMENT INDEX SY-INDEX INTO *DTADCAC.
        IF SY-SUBRC = 0.
          ADD 1 TO CNT_RECORDS.
        ENDIF.
      ENDDO.
      HELP_CAN = 24 + 240 * CNT_RECORDS.   "note#327208
*     HLP_OFFSET = 24 + 240 * CNT_RECORDS.
      IF CNT_RECORDS NE 6.
        MOVE SPACE TO C_RECORD+HELP_CAN.   "note#327208
      ENDIF.

      PERFORM STORE_ON_FILE USING C_RECORD.
      PERFORM CR_LF.

*     store the trailer record
      DTADCAH-TYPE = 'Z'.
      ADD 1 TO DTADCAH-RCNT.
      MOVE-CORRESPONDING DTADCAH TO DTADCAZ.
      PERFORM STORE_ON_FILE USING DTADCAZ.
      PERFORM CR_LF.

*     close DME file
      SUM_REGUT = SUM_ABSCHLUSS.
      PERFORM DATEI_SCHLIESSEN.

*     print accompanying sheet and summary sheet
      PERFORM BEGLEITZETTEL_SCHREIBEN.
      PERFORM FORMULARABSCHLUSS_SCHREIBEN.

    ENDAT.

  ENDLOOP.

ENDFORM.                               "DME_CANADA



*----------------------------------------------------------------------*
* FORM BEGLEITZETTEL_SCHREIBEN                                         *
*----------------------------------------------------------------------*
* write accompanying sheet for each logical file                       *
*----------------------------------------------------------------------*
FORM BEGLEITZETTEL_SCHREIBEN.

* specify spool parameters for print of sheet and summary
  PERFORM FILL_ITCPO USING PAR_PRIW
                           'LIST7S'
                           PAR_SOFW
                           SPACE.
  IF PAR_PRIW EQ SPACE.
    FLG_DIALOG = 'X'.
  ELSE.
    FLG_DIALOG = SPACE.
  ENDIF.

* open form with file accompanying sheet
  CALL FUNCTION 'OPEN_FORM'
       EXPORTING
            FORM     = T042E-WFORN
            DEVICE   = 'PRINTER'
            LANGUAGE = T001-SPRAS
            OPTIONS  = ITCPO
            DIALOG   = FLG_DIALOG
       EXCEPTIONS
            FORM     = 1.
  IF SY-SUBRC EQ 1.                "abend:
    IF SY-BATCH EQ SPACE.          "form is not active
      MESSAGE A069 WITH T042E-WFORN.
    ELSE.
      MESSAGE S069 WITH T042E-WFORN.
      MESSAGE S094.
      STOP.
    ENDIF.
  ENDIF.

* write sheet
  SET COUNTRY SPACE.
  DO PAR_ANZB TIMES.

    CALL FUNCTION 'START_FORM'
         EXPORTING
              STARTPAGE = 'DTA'.

    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              WINDOW  = 'INLAND'
              ELEMENT = '525'
         EXCEPTIONS
              WINDOW  = 1
              ELEMENT = 2.

    IF SY-SUBRC EQ 2.
      ERR_ELEMENT-FNAME = T042E-WFORN.
      ERR_ELEMENT-FENST = 'INLAND'.
      ERR_ELEMENT-ELEMT = '525'.
      ERR_ELEMENT-TEXT  = TEXT-525.
      COLLECT ERR_ELEMENT.
    ENDIF.

    CALL FUNCTION 'END_FORM'.

  ENDDO.

ENDFORM.                               "BEGLEITZETTEL_SCHREIBEN



*----------------------------------------------------------------------*
* FORM FORMULARABSCHLUSS_SCHREIBEN                                     *
*----------------------------------------------------------------------*
* write summary form                                                   *
*----------------------------------------------------------------------*
FORM FORMULARABSCHLUSS_SCHREIBEN.

* copy array of payment methods used into REGUD-field
  CONDENSE HLP_DTA_ZWELS.
  REGUD-ZWELS = HLP_DTA_ZWELS.

  CALL FUNCTION 'START_FORM'
       EXPORTING
            STARTPAGE = 'LAST'.

* write summary for DME
  CALL FUNCTION 'WRITE_FORM'
       EXPORTING
            WINDOW  = 'SUMMARY'
            ELEMENT = '520'
       EXCEPTIONS
            WINDOW  = 1
            ELEMENT = 2.

  IF SY-SUBRC EQ 2.
    ERR_ELEMENT-FNAME = T042E-WFORN.
    ERR_ELEMENT-FENST = 'SUMMARY'.
    ERR_ELEMENT-ELEMT = '520'.
    ERR_ELEMENT-TEXT  = TEXT-520.
    COLLECT ERR_ELEMENT.
  ENDIF.

  CALL FUNCTION 'END_FORM'.

* close spool file
  CALL FUNCTION 'CLOSE_FORM'
       IMPORTING
            RESULT = ITCPP.

  CLEAR TAB_AUSGABE.
  TAB_AUSGABE-NAME    = ITCPP-TDTITLE.
  TAB_AUSGABE-DATASET = ITCPP-TDDATASET.
  TAB_AUSGABE-SPOOLNR = ITCPP-TDSPOOLID.
  COLLECT TAB_AUSGABE.

ENDFORM.                               "FORMULARABSCHLUSS_SCHREIBEN



*---------------------------------------------------------------------*
* FORM CR_LF                                                          *
*---------------------------------------------------------------------*
* transfer carriage return and line feed if desired                   *
*---------------------------------------------------------------------*
FORM cr_lf.

  DATA:
    DTA_CR(1)    TYPE X VALUE '0D',    "carriage return
    DTA_LF(1)    TYPE X VALUE '0A'.    "line feed

  IF par_crlf EQ 1 OR par_crlf EQ 3.
    PERFORM store_on_file USING dta_cr.
  ENDIF.
  IF par_crlf EQ 2 OR par_crlf EQ 3.
    PERFORM store_on_file USING dta_lf.
  ENDIF.

ENDFORM.                               "CR_LF




*----------------------------------------------------------------------*
* FORM DME_VALID_CHARACTERS                                            *
*----------------------------------------------------------------------*
* deletes invalid letters                                              *
*----------------------------------------------------------------------*
* TEXTFELD - text that is to be checked                                *
*----------------------------------------------------------------------*
FORM DME_VALID_CHARACTERS USING TEXTFELD.

* valid character set for validation
  DATA VALID_CHARS(100) TYPE C.
  VALID_CHARS    = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890 .,&-/+*$%"!%:'.
  VALID_CHARS+50 = 'abcdefghijklmnopqrstuvwxyz();''#<>=?@[]^_`{}|~'.

  WHILE TEXTFELD CN VALID_CHARS.
    WRITE SPACE TO TEXTFELD+SY-FDPOS(1).
  ENDWHILE.

ENDFORM.                               "DME_VALID_CHARACTERS



*---------------------------------------------------------------------*
*      Form  CONVERT_DATE                                             *
*---------------------------------------------------------------------*
*       convert a date from 'YYYYMMDD' to '0YYDDD' format             *
*---------------------------------------------------------------------*
*  -->  datum     date to be converted                                *
*  -->  cdate     date converted                                      *
*---------------------------------------------------------------------*
FORM CONVERT_DATE USING DATUM CHANGING CDATE.

  DATA:
    UP_NODAYS     TYPE I,
    UP_CDATE(6)   TYPE N,
    UP_BEGDATE    LIKE SY-DATLO.

  UP_BEGDATE      = DATUM.
  UP_BEGDATE+4(4) = '0101'.
  UP_NODAYS       = DATUM - UP_BEGDATE + 1.
  UP_CDATE+3(3)   = UP_NODAYS.
  UP_CDATE+1(2)   = UP_BEGDATE+2(2).
  CDATE           = UP_CDATE.

ENDFORM.                               "CONVERT_DATE



*----------------------------------------------------------------------*
* FORM COMPUTE_CREATION_NUMBER                                         *
*----------------------------------------------------------------------*
* compute the next file creation number for originator/data center     *
*----------------------------------------------------------------------*
FORM COMPUTE_CREATION_NUMBER.

  DATA:
    DTA_VERSION(4) TYPE C VALUE 1,     "version of table DTA_CRNO
    BEGIN OF DTA_ID,                   "Id of RFDT
      PROG(8) TYPE C VALUE 'RFFOCA_T',
      ID(14)  TYPE C VALUE 'DTADCAA-CRNO  ',
    END OF DTA_ID,
    BEGIN OF DTA_CRNO OCCURS 0,        "file number
      ORIG     LIKE DTADCAA-ORIG,
      ADEST    LIKE DTADCAA-ADEST,
      CRNO(4)  type n,
    END OF DTA_CRNO.


* new CRNO is desired
  if par_retr is initial.
*    import and read table
     REFRESH DTA_CRNO.
     IMPORT DTA_CRNO FROM DATABASE RFDT(FZ) ID DTA_ID.
     DTA_CRNO-ORIG  = T012D-DTFIN.
     DTA_CRNO-ADEST = T012D-DTBID.
     READ TABLE DTA_CRNO WITH KEY
       ORIG  = DTA_CRNO-ORIG
       ADEST = DTA_CRNO-ADEST.

*    compute next file number
     IF SY-SUBRC NE 0.
       DTA_CRNO-CRNO = 1.
       APPEND DTA_CRNO.
     ELSE.
        IF DTA_CRNO-CRNO EQ 9999.
           DTA_CRNO-CRNO = 1.
        ELSE.
           ADD 1 TO DTA_CRNO-CRNO.
        ENDIF.
        MODIFY DTA_CRNO INDEX SY-TABIX.
     ENDIF.

     DTADCAH-CRNO = DTA_CRNO-CRNO.


*    save table
     EXPORT DTA_CRNO TO DATABASE RFDT(FZ) ID DTA_ID.
     COMMIT WORK.

  else.
*    re-transmission
     DTADCAH-CRNO = par_CRNO.
  endif.


ENDFORM.
