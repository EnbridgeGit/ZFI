* loder, drew & associates

************************************************************************
*  PROGRAM NAME   : YFAPAUD1                                           *
*  PROJECT        : Finance                                            *
*  SUBSYSTE       : FI                                                 *
*  AUTHOR         : IMProgrammer                                       *
*  DATE WRITTEN   : 01/03/1998#                                        *
*  TABLES         : TVZBT,PAYR,LFA1,LFB1,BKPF,BSEG.                    *
*                                                                      *
*  TRANSACTION                                                         *
*  REPORT(S)      : AP VENDOR MASTER RECORDS                           *
*  INPUT FILES####: N/A                                                *
*  OUTPUT FILES####: /usrl/$sys/fin/outgoing/audven                    *
*                                                                      *
*  SAP RELEASE####: 3.0d                                               *
*  PROGRAM DESCRIPTION  : Output AP vendor information                 *
*                         for Loder Drew Audit                         *
************************************************************************
* REVISION LOG                                                         *
*                                                                      *
* LOG DATE          AUTHOR   DESCRIPTION                               *
*-------       ---------##--------------------##-----------------------
* 001 03/16/98      #Dchanger1##Change prog. Output to single file,##*
*							vendor master records only.			*
*												*
*002 05/08/98      #Kchanger2##Add PO Box field to file (lfa1-pfach)   *
*                                                                      *
* 003 99/05/03  mdemeest load & modify for Union Gas SAP               *
************************************************************************
report yfapaud1 line-size 255 line-count 58
		no standard page heading
		message-id gr.

***************	TABLES NEEDED  ****************
tables:
TVZBT,                                 " Payment terms description
PAYR,                                  " Payment transfer medium file
LFA1,                                  " Vendor Master
LFB1,                                  " Vendor Master (company)
BKPF,                                  " Accounting document segment
BSEG.                                  " Accounting document header

data:	ven_ctr type i,
WS_SYMBOLIC(4) TYPE C VALUE '$sys',
ZTERM(4) TYPE C.

DATA:BEGIN OF VENDORS OCCURS 0,
LIFNR(15) TYPE C,                      "Vendor Number
NAME1(40) TYPE C,                      "Vendor (full) Name
NAMEC(40) TYPE C,                      "Vendor Address - 1
NAME2(40) TYPE C,                      "Vendor Address - 2
NAME3(40) TYPE C,                      "Vendor Address - 3
ORT01(20) TYPE C,                      "Vendor City
REGIO(20) TYPE C,                      "Vendor State/Providence/Country
PSTLZ(10) TYPE C,                      "Vendor Zip/Post Code
TELF1(12) TYPE C,                      "Vendor Telephone Number
ERDAT(09) TYPE C,                      "Date record created
VTEXT(16) TYPE C,                      "Terms description
BRSCH(08) TYPE C,                      "SIC/Industry code
ACTFL(05)  TYPE C,                     "Active Flag
BUKRS(05) TYPE C,                      "Division (Co Code)
PFACH(10) TYPE C,                      "PO Box #
end of vendors.


parameters:
*outfile2 (34)#default '/usrl/$sys/fin/outgoing/audven' lower case "#003
OUTFILE2(60) DEFAULT '/usr/sap/interfaces/$sys/CFMM001/zfapaud1.sap'
                               LOWER CASE.                         "#003
SKIP 1.
SELECT-OPTIONS: S_BUKRS FOR LFB1-BUKRS NO INTERVALS OBLIGATORY.     "003

* ---------------------------------	START OF SELECTION  ----------------
start-of-selection.

*      Set up Path with appropriate system pointer
  REPLACE WS_SYMBOLIC WITH SY-SYSID INTO OUTFILE2.          "Log 001
  CONDENSE OUTFILE2 NO-GAPS.                                "Log 001
  open dataset outfile2 for output in text mode.

  CLEAR VENDORS.
  clear lfa1.
  clear lfb1.
  clear payr.
  clear bkpf.
  clear bseg.
  ven_ctr = 0.

  select * from lfa1.
    	
    MOVE LFA1-LIFNR TO VENDORS-LIFNR.
    MOVE LFA1-NAME1 TO VENDORS-NAME1.
    MOVE LFA1-NAME1 TO VENDORS-NAMEC.
    MOVE LFA1-NAME2 TO VENDORS-NAME2.
    MOVE LFA1-NAME3 TO VENDORS-NAME3.
    IF VENDORS-NAME1 EQ SPACE.
      MOVE LFA1-STRAS TO VENDORS-NAME1.
    ELSEIF VENDORS-NAME2 EQ SPACE.
      MOVE LFA1-STRAS TO VENDORS-NAME2.
    ELSEIF VENDORS-NAME3 EQ SPACE.
      MOVE LFA1-STRAS TO VENDORS-NAME3.
    ENDIF.
    MOVE LFA1-ORT01 TO VENDORS-ORT01.
    MOVE LFA1-REGIO TO VENDORS-REGIO.
    MOVE LFA1-PSTLZ TO VENDORS-PSTLZ.
    MOVE LFA1-TELF1 TO VENDORS-TELF1.
MOVE LFA1-PFACH TO VENDORS-PFACH.                           "Log 002
    IF VENDORS-TELF1 EQ SPACE.
      MOVE LFA1-TELF2 TO VENDORS-TELF1.
    ENDIF.
    MOVE LFA1-ERDAT+2(6) TO VENDORS-ERDAT.
    MOVE LFA1-BRSCH TO VENDORS-BRSCH.
    MOVE LFA1-LOEVM TO VENDORS-ACTFL.
    SELECT BUKRS LOEVM ZTERM FROM LFB1
        INTO (VENDORS-BUKRS, VENDORS-ACTFL, ZTERM)
        WHERE LIFNR EQ VENDORS-LIFNR                                "003
          AND BUKRS IN S_BUKRS.                                     "003
      IF VENDORS-ACTFL EQ SPACE.
        MOVE LFA1-LOEVM TO VENDORS-ACTFL.
      ENDIF.
      IF ZTERM NE SPACE.
        SELECT VTEXT FROM TVZBT INTO VENDORS-VTEXT
        WHERE ZTERM EQ ZTERM.
        ENDSELECT.
      ENDIF.
      APPEND VENDORS.                                               "003
      ENDSELECT.
*      append vendors.                                              "003
      ENDSELECT.

end-of-selection.
* -------------------------------------  END OF SELECTION  -------------



  SORT VENDORS BY LIFNR.

  LOOP AT VENDORS.
  TRANSFER VENDORS TO OUTFILE2.
  VEN_CTR = VEN_CTR + 1.
  ENDLOOP.

  WRITE:  / 'Vendors = ', VEN_CTR.
  CLOSE DATASET OUTFILE2.


************************  END OF 'YFAPAUD1' PROGRAM  ******************
