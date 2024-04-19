REPORT ZFAPI007 NO STANDARD PAGE HEADING LINE-SIZE 132 LINE-COUNT 65
       MESSAGE-ID ZS.
* This program will read a file of previously created employee vendors
* and create change transactions to assign them all to clerk #04.
TABLES: LFA1,                                    "vendor mast details
        LFB1.                                    "vendor acctg details
DATA: POSTCD(6),
      SESSION_CTR TYPE I,
      ZIPCODE(7),
      TOWN_SW(1),
      STREET_SW(1),
      LONGZIP(13).
DATA MSG_TEXT(50).
DATA: CONVRULE(2) TYPE C VALUE ' /'.
* session header record
DATA: BEGIN OF ZBGR00.
       INCLUDE STRUCTURE BGR00.
DATA: END OF ZBGR00.
* document header record
DATA: BEGIN OF ZBLF00.
       INCLUDE STRUCTURE BLF00.
DATA: END OF ZBLF00.
* employee vendor general data record
DATA: BEGIN OF ZBLFA1.
       INCLUDE STRUCTURE BLFA1.
DATA: END OF ZBLFA1.
* employee vendor company code record
DATA: BEGIN OF ZBLFB1.
        INCLUDE STRUCTURE BLFB1.
DATA: END OF ZBLFB1.

DATA: NO_INPUT_REC(750) TYPE C.
PARAMETERS EMPLOYEE(128)
    DEFAULT '/usr/sap/interfaces/P01/CFAP001/CFAP001_69.CHK'
    LOWER CASE.
OPEN DATASET EMPLOYEE FOR OUTPUT IN TEXT MODE MESSAGE MSG_TEXT.
IF SY-SUBRC NE 0.
 WRITE:/ 'Employee change file cannot be opened. Reason:', MSG_TEXT.
 EXIT.
ENDIF.
MOVE SPACE TO NO_INPUT_REC.
TRANSLATE NO_INPUT_REC USING CONVRULE.
MOVE NO_INPUT_REC+0(38) TO ZBGR00.
MOVE '0' TO ZBGR00-STYPE.
MOVE 'ZAP PAY SEC ' TO ZBGR00-GROUP.
MOVE SY-MANDT TO ZBGR00-MANDT.
MOVE 'BATCH' TO ZBGR00-USNAM.
TRANSFER ZBGR00 TO EMPLOYEE LENGTH 750.
IF SY-SUBRC NE 0.
   WRITE:/ 'File cannot be transferred. Reason:', MSG_TEXT.
   EXIT.
ENDIF.
* loop through the file of  data.
SELECT * FROM LFA1 WHERE KTOKK = 'VNDR'.
  SELECT * FROM LFB1 WHERE LIFNR = LFA1-LIFNR
                     AND UZAWE = SPACE.
    WRITE:/ LFA1-LIFNR, ' ', LFA1-NAME1, ' ', LFB1-ZWELS, ' ',
      LFB1-UZAWE.
    MOVE NO_INPUT_REC+0(27) TO ZBLF00.
    MOVE '1' TO ZBLF00-STYPE.
    MOVE 'XK02' TO ZBLF00-TCODE.
    MOVE LFA1-LIFNR TO ZBLF00-LIFNR.
    MOVE LFB1-BUKRS TO ZBLF00-BUKRS.
    MOVE LFA1-KTOKK TO ZBLF00-KTOKK.
    TRANSFER ZBLF00 TO EMPLOYEE LENGTH 750.

    MOVE NO_INPUT_REC+0(239) TO ZBLFB1.
    MOVE '2' TO ZBLFB1-STYPE.
    MOVE 'BLFB1' TO ZBLFB1-TBNAM.
    MOVE '01' TO ZBLFB1-UZAWE.
    TRANSFER ZBLFB1 TO EMPLOYEE LENGTH 750.
    IF SY-SUBRC NE 0.
      WRITE:/ 'File cannot be transferred. Reason:', MSG_TEXT.
    EXIT.
    ENDIF.
  ENDSELECT.
ENDSELECT.
CLOSE DATASET EMPLOYEE.
*
