REPORT ZFAPI099 NO STANDARD PAGE HEADING LINE-SIZE 132 LINE-COUNT 65
       MESSAGE-ID ZS.
* This program will read a file of employee data extracted from the
* HR system and will reformat it in SAP batch input records to create
* or change employee vendors. The output file will be loaded into SAP
* using the RMIBLR00 load program.
TABLES: LFA1,                                    "vendor mast details
        LFBK,                                    "vendor mast banking
        LFB1.                                    "company code data
DATA: CRTE_SESS_CTR TYPE I,
      RECORD_CTR TYPE I,
      RECORD_CTR2(15),
      ADDR_UPD(1),
      BANK_UPD(1),
      NAME_UPD(1),
      TRAN_CODE(4),
      ACTION(3),
      EMPL_VNDR(8),
      NAME1(35),
      NAME2(35),
      NAME3(35),
      LAST_NAME(35),
      LNAME_INIT(1),
      STATUS_CD(1),
      STATUS_DESC(10),
      DAYS_MSG_FLG(1),
      DAYS_MSG(15),
      TRAN_LEN TYPE I.
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
* employee vendor banking data record
DATA: BEGIN OF ZBLFBK.
        INCLUDE STRUCTURE BLFBK.
DATA: END OF ZBLFBK.
* employee vendor company code record
DATA: BEGIN OF ZBLFB1.
        INCLUDE STRUCTURE BLFB1.
DATA: END OF ZBLFB1.
* employee vendor dunning data record
DATA: BEGIN OF ZBLFB5.
        INCLUDE STRUCTURE BLFB5.
DATA: END OF ZBLFB5.
* employee vendor purchasing data record
DATA: BEGIN OF ZBLFM1.
        INCLUDE STRUCTURE BLFM1.
DATA: END OF ZBLFM1.
DATA: NO_INPUT_REC(750).
*parameters: hrisfile like filenameci-fileintern
*            default 'Z_IFAP007_01'.
PARAMETERS: DELETFLE(750)
   DEFAULT '/usr/sap/interfaces/P01/CFAP001/CFAP001_66.CHK'
   LOWER CASE.
DATA MSG_TEXT(50).
OPEN DATASET DELETFLE FOR OUTPUT IN TEXT MODE MESSAGE MSG_TEXT.
IF SY-SUBRC NE 0.
 WRITE:/ 'Delete file cannot be opened. Reason:', MSG_TEXT.
 EXIT.
ENDIF.
MOVE 5000 TO CRTE_SESS_CTR.
MOVE 0 TO RECORD_CTR.
MOVE SPACE TO NO_INPUT_REC.
TRANSLATE NO_INPUT_REC USING CONVRULE.
* select all centra vendor records
SELECT * FROM LFB1 WHERE ALTKN LIKE '%CN'.
* create sesssion header for each 5000 documents.
    WRITE:  'Vendor # ', LFB1-LIFNR.
    ADD 1 TO RECORD_CTR.
    IF CRTE_SESS_CTR = 5000.
      MOVE NO_INPUT_REC+0(38) TO ZBGR00.
      MOVE '0' TO ZBGR00-STYPE.
      MOVE 'Delete Vend ' TO ZBGR00-GROUP.
      MOVE '055' TO ZBGR00-MANDT.
      MOVE 'BATCH' TO ZBGR00-USNAM.
      MOVE 1 TO CRTE_SESS_CTR.
      TRANSFER ZBGR00 TO DELETFLE.
      IF SY-SUBRC NE 0.
        WRITE:/ 'File cannot be transferred. Reason:', MSG_TEXT.
        EXIT.
      ENDIF.
*     write: / 'ZBGR00', zbgr00.
    ELSE.
      ADD 1 TO CRTE_SESS_CTR.
    ENDIF.
* create a document header for each document.
    MOVE NO_INPUT_REC+0(27) TO ZBLF00.
    MOVE '1' TO ZBLF00-STYPE.
    MOVE 'XK02' TO ZBLF00-TCODE.
    MOVE LFB1-LIFNR TO ZBLF00-LIFNR.
    MOVE 'UGL' TO ZBLF00-BUKRS.
    MOVE 'VNDR' TO ZBLF00-KTOKK.
    TRANSFER ZBLF00 TO DELETFLE.
    IF SY-SUBRC NE 0.
      WRITE:/ 'File cannot be transferred. Reason:', MSG_TEXT.
      EXIT.
    ENDIF.
* create a change general data record.
  MOVE NO_INPUT_REC+0(239) TO ZBLFB1.
  MOVE '2' TO ZBLFB1-STYPE.
  MOVE 'BLFB1' TO ZBLFB1-TBNAM.
  MOVE 'X' TO ZBLFB1-LOEVM.
  TRANSFER ZBLFB1 TO DELETFLE.
  IF SY-SUBRC NE 0.
     WRITE:/ 'File cannot be transferred. Reason:', MSG_TEXT.
     EXIT.
  ENDIF.
*  write: / 'ZBLFb1', zblf01.
ENDSELECT.
    WRITE: /3 'record counter ', RECORD_CTR.
WRITE:/ 'WAHOO!'.
CLOSE DATASET DELETFLE.
*
