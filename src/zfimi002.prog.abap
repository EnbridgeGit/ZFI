REPORT ZFIMI002 MESSAGE-ID AT LINE-SIZE 132 LINE-COUNT 65 NO STANDARD
PAGE HEADING.
************************************************************************
*
*  PROGRAM:    ZFIMI002
*  PROGRAMMER: Nesh N. Laurencic / Omnilogic Systems Group
*  CLIENT:     Union Gas
*  DATE:       April 1998.
*  This ABAP will setup the necessary structure used by the load program
*  zkpaa001. For accounts with field status group: BREV, BSHT, EXMM,
*  REVN, RVOT we have to use program ZKPAA001.
*  For accounts with field status group: GASS, PAYR, PROJ, STCF we have
*  to use to use program rfbibl00. ( warning : structures are different)
*  This program will transfer balances from CGO to UGL.
*  THE LOGICAL FILE NAME IS ZFIMI002. ( SAME FILE ).
*  Dummy cost center is : 10999 (CGO). We have to change config into
*  UGL before we run UGL part of the program.
*  Dummy material is: 900853
*  Dummy WBS is     : 01-97-GTA-7630   UGL
*                   : 46-97-GTA-7630   CGO
*  Offset account is: 0000199998
*  Type of accounting document created is 'ZM'.
*  Program will move balances from CGO to UGL, and if you run second
*  set of options from selection screen, will make balance in CGO zero.
************************************************************************
*  2000/09/27 - gymana - 4.6B Upgrade
*               changed table DD03P to DD03L
************************************************************************

TABLES:  GLT0,    "G/L Account Master Record  Monthly Debits and Credits
         SKAT,    "G/L account master record (chart of accounts: descrip
         SKA1,    "G/L accounts master (chart of accounts)
         SKB1,    "G/L account master (company code)
         BSEG,                         "Accounting document segment
         T881,                         "FI-SL Ledger
         t001.                         "Company code

DATA: BEGIN OF ITAB OCCURS 1000,       " This one is for UGL
      RACCT LIKE GLT0-RACCT,           "Account number  CGO
      RACCT1 LIKE GLT0-RACCT,          "Target Account number UGL
      RACCT2 LIKE GLT0-RACCT,          "Target Account number CGO
      BUKRS LIKE GLT0-BUKRS,           "Company Code
      FSTAG LIKE SKB1-FSTAG,           "Field status group
      WRBTR LIKE BSEG-WRBTR,           "Amount in document (total)
      MWSKZ LIKE BSEG-MWSKZ,           "Tax code
      BLART LIKE BKPF-BLART,           "Document type
      BKTXT LIKE BKPF-BKTXT,           "Document header text
      PARGB LIKE COBL-PARGB,           "Trading partner's business
      MCOD1 LIKE SKA1-MCOD1,                                "
      MATNR LIKE BSEG-MATNR,           "Material number
      MENGE LIKE BSEG-MENGE,           "Quantity
      PERNR LIKE BSEG-PERNR,           "Personnel number
      PS_PSP_PNR LIKE COBL-PS_PSP_PNR, "WBS element
      KOSTL LIKE COBL-KOSTL,           "Cost center
      PRCTR LIKE COBL-PRCTR,           "Profit center
      BEWAR LIKE BBSEG-BEWAR,          "Transaction type for gen ledGER
      FMORE LIKE DKACB-FMORE,          "Indicator(all assignments popup)
      EXI   TYPE C,                    "Account exist in UGL
      BL2   TYPE C,                    "CGO acc. blocked
      BL3   TYPE C,                    "UGL acc. blocked
      BL4   TYPE C,                    "Chart of acc. blocked
      MITKZ LIKE SKB1-MITKZ,           "Account is reconciliation accoun
                                       " 'I' for internal posting added
      MWSKZ2 LIKE BSEG-MWSKZ,          "Tax code for target account UGL
      WAERS  TYPE C,                   "Currency    skb1-waers
      SAMETAXC TYPE C,                 "Tax code is different inicator
      XBILK LIKE SKA1-XBILK,           " Account is a balance sheet acc.
      hslvt like glt0-hslvt,       "G/L Balance from previous periods
      hsl01 like glt0-hsl01,           "Total of movements in period 1
      hsl02 like glt0-hsl01,           "Total of movements in period 2
      hsl03 like glt0-hsl01,           "Total of movements in period 3
      hsl04 like glt0-hsl01,           "Total of movements in period 4
      hsl05 like glt0-hsl01,           "Total of movements in period 5
      hsl06 like glt0-hsl01,           "Total of movements in period 6
      hsl07 like glt0-hsl01,           "Total of movements in period 7
      hsl08 like glt0-hsl01,           "Total of movements in period 8
      hsl09 like glt0-hsl01,           "Total of movements in period 9
      hsl10 like glt0-hsl01,           "Total of movements in period10
      hsl11 like glt0-hsl01,           "Total of movements in period11
      hsl12 like glt0-hsl01,           "Total of movements in period12
      END OF ITAB.

* This one will be for zeroing balances in CGO ( Just reversed - values)
DATA: BEGIN OF ITAB1 OCCURS 1000.
        INCLUDE STRUCTURE ITAB.
DATA: END OF ITAB1.

DATA: BEGIN OF ITAB2 OCCURS 1000.
        INCLUDE STRUCTURE ITAB.
DATA: END OF ITAB2.

DATA: BEGIN OF ITAB_USD OCCURS 1000.
        INCLUDE STRUCTURE ITAB.
DATA: END OF ITAB_USD.

DATA: BEGIN OF RECON_TAB OCCURS 10,
      RACCT LIKE GLT0-RACCT,
      RACCT1 LIKE GLT0-RACCT,          "UGL posting
      RACCT2 LIKE GLT0-RACCT,          "CGO posting
      END OF RECON_TAB.

* Field status groups  internal table
DATA: BEGIN OF GROUP_TAB OCCURS 10,
      FSTAG LIKE SKB1-FSTAG,           "Field status group
      MATNR LIKE BSEG-MATNR,           "Material number
      MENGE LIKE BSEG-MENGE,           "Quantity
      MEINS LIKE BSEG-MEINS,           "Base unit od measur.
      PERNR LIKE BSEG-PERNR,           "Personnel number
      PS_PSP_PNR(24) TYPE C,           "WBS element UGL code
      PS_PSP_PNR2(24) TYPE C,          "WBS element CGO code
      KOSTL LIKE COBL-KOSTL,           "Cost center UGL
      KOSTL2 LIKE COBL-KOSTL,          "Cost center CGO
      PRCTR LIKE COBL-PRCTR,           "Profit center
      BEWAR LIKE BBSEG-BEWAR,          "Transaction type for gen ledGER
      END OF GROUP_TAB.
* BDC TABLES
DATA:     BEGIN OF BDCDATA OCCURS 100. " For BDC
        INCLUDE STRUCTURE BDCDATA.
DATA:     END OF BDCDATA.

DATA:   BEGIN OF MESSTAB OCCURS 10.    "Transaction return
        INCLUDE STRUCTURE BDCMSGCOLL.  "messages
DATA:   END OF MESSTAB.

* In the case of per period.
data: xhsl(11) type c value 'itab1-hsl  ',
      CINDEX(2) TYPE N,
      INDEX1 TYPE I,
      TEMP(10) TYPE C,
      currbal like glt0-hslvt value 0, " Current balance
      RACCT   LIKE GLT0-RACCT VALUE '0000900853',   "Offset account
      DATE LIKE SY-DATUM,              " Date
      POSTDAT LIKE SY-DATUM,           " Posting date
      FLAG TYPE C VALUE 0,             " For loop control
      TABIX LIKE SY-TABIX.             " iNDEX

DATA: WRBTR(13) TYPE C.                "Money


FIELD-SYMBOLS: <F2> .
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME.

SELECT-OPTIONS :  SRACCT FOR GLT0-RACCT MATCHCODE OBJECT SAKO.
PARAMETERS:       PBUKRS LIKE  GLT0-BUKRS DEFAULT 'CGO',
                  PRYEAR LIKE GLT0-RYEAR DEFAULT '1998',
                  PMATNR LIKE BSEG-MATNR DEFAULT '900853'."Dummy mat.
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME.
SELECTION-SCREEN SKIP 2.
PARAMETERS: R1 RADIOBUTTON GROUP A DEFAULT 'X'.
SELECTION-SCREEN SKIP 2.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(53) TEXT-889 MODIF ID AA.
SELECTION-SCREEN END OF LINE.


SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(53) TEXT-890.
PARAMETERS: R2 RADIOBUTTON GROUP A.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN SKIP 2.


SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(53) TEXT-990.
PARAMETERS: R3 RADIOBUTTON GROUP A.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN ULINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(53) TEXT-887 MODIF ID AA.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(53) TEXT-891.
PARAMETERS: R4 RADIOBUTTON GROUP A.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN SKIP 2.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(53) TEXT-992.
PARAMETERS: R5 RADIOBUTTON GROUP A.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B2.

************************************************************************
* This part of data of the program is connected to ZKPAA001
************************************************************************
TABLES  :  DD03L.

FIELD-SYMBOLS: <F1> .
DATA      : T_CODE       LIKE BMM00-TCODE VALUE 'FB01',
            CHAR(21)     TYPE C,
            NODATA(40) VALUE '/'.

DATA      : PHYFILE LIKE FILENAMECI-FILEEXTERN.
DATA:       INDEX LIKE SY-INDEX.


SELECTION-SCREEN ULINE.
PARAMETERS: CORRFILE LIKE FILENAME-FILEINTERN
            DEFAULT 'ZFIMI002' NO-DISPLAY.
*SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME.
*SELECTION-SCREEN COMMENT 1(70) TEXT-CO1.
*SELECTION-SCREEN COMMENT /1(70) TEXT-CO2.
*SELECTION-SCREEN COMMENT /1(70) TEXT-CO3 MODIF ID A.
*SELECTION-SCREEN END OF BLOCK B1.

data:  wa_record(2200) type c.

DATA    : BEGIN OF Z_BGR00.            "Header
        INCLUDE STRUCTURE BGR00.
DATA    : END OF Z_BGR00.

DATA    : BEGIN OF Z_BBKPF.                                 " 0
        INCLUDE STRUCTURE BBKPF.
DATA    : END OF Z_BBKPF.

DATA    : BEGIN OF Z_ZCOPA_BSEG.                            " 1
        INCLUDE STRUCTURE ZCOPA_BSEG.
DATA    : END OF Z_ZCOPA_BSEG.

DATA    : BEGIN OF Z_BBSEG.
        INCLUDE STRUCTURE BBSEG.       "rfbibl00
DATA:     END OF Z_BBSEG.
INCLUDE <ICON>.

***********************************************************************

select single * from T001
    where bukrs = pbukrs.

PERFORM OPEN_OUTPUT_FILE.              " Get Physical name and open file
PERFORM INIT_STRUCTURES USING 'BGR00'. "I_BGR00.
PERFORM INIT_STRUCTURES USING 'BBKPF'. "I_BBKPF.
PERFORM INIT_STRUCTURES_SPECIAL USING 'ZCOPA_BSEG'.  "I_ZCOPA_BSEG
PERFORM INIT_STRUCTURES USING 'BBSEG'. "I_BBSEG.


* Modify screen
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    CHECK SCREEN-GROUP1 = 'AA'.
    SCREEN-INTENSIFIED = '1'.
    MODIFY SCREEN.
  ENDLOOP.

START-OF-SELECTION.
* Cross-reference tables
  PERFORM LOAD_RECONCILIATION_ACCOUNTS." LOAD Target GL exceptions
  PERFORM LOAD_FIELD_STATUS_GROUPS.

* Read data and load to itab ( Credits and Debts )
  SELECT * FROM GLT0 WHERE
                          RLDNR EQ '00' AND    "Ledger
                          RRCTY EQ '0'  AND    "Record type
                          RVERS EQ '001' AND   "Version
                          BUKRS EQ PBUKRS AND  "Company code
                          RYEAR EQ PRYEAR AND  "Fiscal year
                          RACCT IN SRACCT. "  "Account number
*                         rtcur eq 'CAD'.
*    if glt0-rtcur = 'CAD'.
    MOVE-CORRESPONDING GLT0 TO ITAB.
    APPEND ITAB.
*    elseif  glt0-rtcur = 'USD'.
*      move-corresponding glt0 to itab_usd.
*      append itab_usd.
*    endif.
  ENDSELECT.
  CHECK SY-SUBRC = 0.
* Sort CAD and USD internal tables
  SORT ITAB BY RACCT.
*  sort itab_usd by racct.
*  break-point.
  LOOP AT ITAB.

    AT END OF RACCT.
      SUM.
      MOVE ITAB TO ITAB1.              " Find balances
      itab1-wrbtr = itab1-hslvt + itab1-hsl01 + itab1-hsl02 +
                    itab1-hsl03 + itab1-hsl04 + itab1-hsl05 +
                    itab1-hsl06 + itab1-hsl07 + itab1-hsl08 +
                    itab1-hsl09 + itab1-hsl10 + itab1-hsl11 +
                    itab1-hsl12.
      MOVE PBUKRS TO ITAB1-BUKRS.
      MOVE: 'ZA' TO ITAB1-BLART,       "Document type
            'Balance transf. from CGO' TO ITAB1-BKTXT.  "Text
* Check the status field
      SELECT SINGLE * FROM SKB1 WHERE
                                     BUKRS EQ PBUKRS AND
                                     SAKNR EQ ITAB1-RACCT.
      CHECK SY-SUBRC = 0.
      MOVE SKB1-FSTAG TO ITAB1-FSTAG.  "Status field
      MOVE SKB1-MITKZ TO ITAB1-MITKZ.  " A D K or I for automatic post.
* Check if CGO account is blocked
      IF SKB1-XSPEB = 'X'.
        ITAB1-BL2 = 'X'.
      ENDIF.

* Check the tax code
      IF SKB1-MWSKZ EQ SPACE.
        ITAB1-MWSKZ = SPACE.
      ELSE.
        CASE SKB1-MWSKZ.
          WHEN '-'.
            ITAB1-MWSKZ = 'I0'.
          WHEN '*'.
            ITAB1-MWSKZ = 'I0'.
          WHEN '+'.
            ITAB1-MWSKZ = 'O0'.
          WHEN OTHERS.
            ITAB1-MWSKZ = SPACE.
        ENDCASE.

      ENDIF.
* Check if it's US account
      IF SKB1-WAERS = 'USD'.
        ITAB1-WAERS = 'U'.
      ENDIF.
* Check the type of account
      SELECT SINGLE * FROM SKA1 WHERE
                                    KTOPL   = 'COAT' AND
                                    SAKNR EQ ITAB1-RACCT.
      CHECK SY-SUBRC = 0.
* If it's balance sheet account
      MOVE: SKA1-XBILK TO ITAB1-XBILK.
* Check if it's blocked on chart acc. level
      IF SKA1-XSPEB = 'X'.
        ITAB1-BL4 = 'X'.
      ENDIF.

*Description
      SELECT SINGLE * FROM SKAT WHERE
                                     SPRAS EQ 'E' AND
                                     KTOPL EQ 'COAT' AND
                                     SAKNR EQ ITAB1-RACCT.
      CHECK SY-SUBRC = 0.
      MOVE: SKAT-MCOD1 TO ITAB1-MCOD1.

      PERFORM CHECK_THE_TARGET_GL.     " Finf account to post

      PERFORM CHECK_THE_GROUP.         " Check group
      APPEND ITAB1.
      CLEAR ITAB1.
    ENDAT.
  ENDLOOP.

* Just the Report
  IF R1 EQ 'X'.
* Print itab1
    FORMAT COLOR 2.
    WRITE: / TEXT-HE1 COLOR 5.
    SKIP 1.
    LOOP AT ITAB1.
      WRITE: /1 ITAB1-MITKZ,
         ITAB1-RACCT UNDER TEXT-001, ITAB1-XBILK UNDER TEXT-BSA COLOR 7,
                                             ITAB1-BUKRS UNDER TEXT-002,
                      ITAB1-RACCT1 UNDER TEXT-003, 'UGL' UNDER TEXT-004,
                 ITAB1-WRBTR UNDER TEXT-005, ITAB1-FSTAG UNDER TEXT-006,
                    ITAB1-MCOD1 UNDER TEXT-DAT, SY-TABIX UNDER TEXT-NUM.
      IF ITAB1-EXI = 'N'.              "Account doesn't exist
        WRITE:           ITAB1-EXI UNDER TEXT-EXI COLOR 6.
      ELSE.
        WRITE:           ITAB1-EXI UNDER TEXT-EXI.

      ENDIF.
*Print error if tax code in CGO and UGL are not the same
      IF ITAB1-SAMETAXC = 'N'.  "Tax code is not the same in target GL
        WRITE:           ITAB1-SAMETAXC UNDER TEXT-TAX COLOR 6.
      ELSE.
        WRITE:           ITAB1-SAMETAXC UNDER TEXT-TAX.
      ENDIF.
* Print error if account's are blocked
*CGO
      IF ITAB1-BL2 = 'X'.
        WRITE:           ITAB1-BL2 UNDER TEXT-BL2 COLOR 6.
      ENDIF.
*UGL
      IF ITAB1-BL3 = 'X'.
        WRITE:           ITAB1-BL3 UNDER TEXT-BL3 COLOR 6.
      ENDIF.
*Chart of accounts
      IF ITAB1-BL3 = 'X'.
        WRITE:           ITAB1-BL3 UNDER TEXT-BL3 COLOR 6.
      ENDIF.

* US account indicator
      IF ITAB1-WAERS = 'U'.
        WRITE: 2 ITAB1-WAERS.
      ENDIF.

    ENDLOOP.
    FORMAT RESET.
    LOOP AT ITAB1.
      AT LAST.
        SUM.
        WRITE: / SY-ULINE(15) UNDER TEXT-005.
        WRITE: /26 TEXT-TOT,
               ITAB1-WRBTR UNDER TEXT-005 COLOR 3 INTENSIFIED ON, 'CAD'.
      ENDAT.
    ENDLOOP.

  ENDIF.
* First group of accounts: BREV, BSHT, EXMM, REVN, RVOT.
* UGL
  IF R2 = 'X'.                         " BREV BSHT EXMM REVN RVOT
    DELETE ITAB1 WHERE
                       WRBTR =  '0'    OR
                       FSTAG EQ 'GASS' OR
                       FSTAG EQ 'PAYR' OR
                       FSTAG EQ 'PROJ' OR
                       FSTAG EQ 'STCF' OR
                       EXI   EQ 'N' OR "Delete if account doesn't exist
                       SAMETAXC EQ 'N' OR
                       BL2   EQ 'X' OR "Blocked CGO
                       BL3 EQ 'X' OR   "Blocked UGL
                       BL4 EQ 'X'.     "Blocked Chart acc. level

    SORT ITAB1 BY RACCT.

    READ TABLE ITAB1 INDEX 1.
    IF SY-SUBRC = 0.
      PERFORM CREATE_BDC_SESSION.
    ELSE.
      WRITE: 'There are no valid accounts selected !'.
    ENDIF.

  ENDIF.

* Second group of accounts: GASS, PAYR, PROJ, STCF
* UGL
  IF R3 = 'X'.                         "   GASS PAYR PROJ STCF
    DELETE ITAB1 WHERE
                       WRBTR = '0' OR  "No money to move
                       BL2   EQ 'X' OR "Blocked CGO
                       BL3 EQ 'X' OR   "Blocked UGL
                       BL4 EQ 'X' OR   "Blocked Chart acc. level
                       EXI EQ 'N' OR   "If account doesn't exist
                       SAMETAXC EQ 'N' OR   "If account doesn't exist
                       (
                       FSTAG NE 'GASS' AND
                       FSTAG NE 'PAYR' AND
                       FSTAG NE 'PROJ' AND
                       FSTAG NE 'STCF' ).

    SORT ITAB1 BY RACCT.
    READ TABLE ITAB1 INDEX 1.
    IF SY-SUBRC = 0.
      PERFORM CREATE_BDC_SESSION2.
    ELSE.
      WRITE: 'There are no valid accounts selected !'.
    ENDIF.
  ENDIF.
* Make balance 0 in CGO I group
* CGO
  IF R4 = 'X'.                         " BREV BSHT EXMM REVN RVOT
    DELETE ITAB1 WHERE
                       WRBTR =  '0'    OR
                       FSTAG EQ 'GASS' OR
                       FSTAG EQ 'PAYR' OR
                       FSTAG EQ 'PROJ' OR
                       FSTAG EQ 'STCF' OR
                       EXI   EQ 'N' OR "Delete if account doesn't exist
                       SAMETAXC EQ 'N' OR
                       BL2   EQ 'X' OR "Blocked CGO
                       BL3 EQ 'X' OR   "Blocked UGL
                       BL4 EQ 'X'.     "Blocked Chart acc. level


    READ TABLE ITAB1 INDEX 1.          " Check if there are any records
    IF SY-SUBRC = 0.
      LOOP AT ITAB1.
        ITAB1-WRBTR =  ITAB1-WRBTR * ( - 1 ).  "Reverse the ammounts
        MODIFY ITAB1.
      ENDLOOP.
      SORT ITAB1 BY RACCT.

      PERFORM CREATE_BDC_SESSION4.
    ELSE.
      WRITE: 'There are no valid accounts selected !'.
    ENDIF.

  ENDIF.

* Make balance 0 in CGO II group
* CGO
  IF R5 = 'X'.                         "   GASS PAYR PROJ STCF
    DELETE ITAB1 WHERE
                       WRBTR = '0' OR  "No money to move
                       BL2   EQ 'X' OR "Blocked CGO
                       BL3 EQ 'X' OR   "Blocked UGL
                       BL4 EQ 'X' OR   "Blocked Chart acc. level
                       EXI EQ 'N' OR   "If account doesn't exist
                       SAMETAXC EQ 'N' OR   "If account doesn't exist
                       (
                       FSTAG NE 'GASS' AND
                       FSTAG NE 'PAYR' AND
                       FSTAG NE 'PROJ' AND
                       FSTAG NE 'STCF' ).

    READ TABLE ITAB1 INDEX 1.
    IF SY-SUBRC = 0.
      LOOP AT ITAB1.
        ITAB1-WRBTR =  ITAB1-WRBTR * ( - 1 ).  "Reverse the ammounts
        MODIFY ITAB1.
      ENDLOOP.
      SORT ITAB1 BY RACCT.
      PERFORM CREATE_BDC_SESSION5.
    ELSE.
      WRITE: 'There are no valid accounts selected !'.
    ENDIF.

  ENDIF.
* Field symbols
*  READ TABLE ITAB1 INDEX 1.
*  DO 5 TIMES.
*    INDEX = INDEX + 1.
*    MOVE INDEX TO CINDEX.
*    xhsl+9(2) = cindex.
*    assign (xhsl) to <f1>.
*    WRITE <F1> TO TEMP .
*    WRITE: / TEMP.
*  ENDDO.

TOP-OF-PAGE.
  FORMAT RESET.
  FORMAT COLOR 1.
  WRITE: /1 TEXT-RPT, SY-REPID COLOR 4 INTENSIFIED ON,
         47 SY-TITLE COLOR 4 INTENSIFIED ON,
        100 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.


  WRITE: /1 TEXT-CLT, SY-MANDT UNDER SY-REPID, TEXT-USN, SY-UNAME.
  WRITE: TEXT-PGE UNDER TEXT-DTE, SY-PAGNO.
  FORMAT RESET.
  SKIP 1.
* And now,real top-of-page.
  FORMAT COLOR 1.
*First row
  WRITE: /3 TEXT-001, 10 TEXT-BSA,  14 TEXT-002, 20 TEXT-003,
         31  TEXT-OO4, 37 TEXT-005, 56 TEXT-006,
         63 TEXT-DAT, 92 TEXT-EXI, 98 TEXT-TAX, 107 TEXT-BL1,
                                                127 TEXT-NUM.
* Second row
  WRITE: / TEXT-EX1 UNDER TEXT-EXI,
           TEXT-TA1 UNDER TEXT-TAX,
           TEXT-BL2 UNDER TEXT-BL1,
           112 TEXT-BL3, 116 TEXT-BL4.
  SKIP.
  FORMAT RESET.

END-OF-PAGE.
*&---------------------------------------------------------------------*
*&      Form  CHECK_THE_TARGET_GL
*&---------------------------------------------------------------------*
*       Check the GL target account                                    *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_THE_TARGET_GL.

  READ TABLE RECON_TAB WITH KEY
                          RACCT = ITAB1-RACCT.
  IF SY-SUBRC <> 0.
    ITAB1-RACCT1 = ITAB1-RACCT.        "CGO = UGL
    ITAB1-RACCT2 = ITAB1-RACCT.        "CGO target = CGO
  ELSE.
    ITAB1-RACCT1 = RECON_TAB-RACCT1.                        "UGL
    ITAB1-RACCT2 = RECON_TAB-RACCT2.   "CGO target
  ENDIF.
* Check if GL account exist
  SELECT SINGLE * FROM SKB1 WHERE
                                 BUKRS EQ 'UGL' AND
                                 SAKNR EQ ITAB1-RACCT1.
  IF SY-SUBRC <> 0.
    ITAB1-EXI = 'N'.
  ELSE.
    ITAB1-EXI = 'Y'.
  ENDIF.
* Check if account is blocked
  IF SKB1-XSPEB = 'X'.
    ITAB1-BL3 = 'X'.
  ENDIF.
* Check the tax code
  IF SKB1-MWSKZ EQ SPACE.
    ITAB1-MWSKZ2 = SPACE.
  ELSE.
    CASE SKB1-MWSKZ.
      WHEN '-'.
        ITAB1-MWSKZ2 = 'I0'.
      WHEN '*'.
        ITAB1-MWSKZ2 = 'I0'.
      WHEN '+'.
        ITAB1-MWSKZ2 = 'O0'.
      WHEN OTHERS.
        ITAB1-MWSKZ2 = SPACE.
    ENDCASE.

  ENDIF.
* Compare tax codes dor LOAD accounts
  IF ITAB1-MWSKZ2 NE ITAB1-MWSKZ.
    ITAB1-SAMETAXC = 'N'.
  ELSE.
    ITAB1-SAMETAXC = 'Y'.
  ENDIF.
ENDFORM.                               " CHECK_THE_TARGET_GL
*&---------------------------------------------------------------------*
*&      Form  LOAD_RECONCILIATION_ACCOUNTS
*&---------------------------------------------------------------------*
*       Load target GL acc.                                            *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LOAD_RECONCILIATION_ACCOUNTS.
  RECON_TAB-RACCT = '0000140150'.
  RECON_TAB-RACCT1 = '0000140155'.
  RECON_TAB-RACCT2 = '0000140150'.
  APPEND RECON_TAB.

  RECON_TAB-RACCT = '0000142210'.
  RECON_TAB-RACCT1 = '0000142215'.
  RECON_TAB-RACCT2 = '0000142210'.
  APPEND RECON_TAB.

  RECON_TAB-RACCT = '0000179230'.
  RECON_TAB-RACCT1 = '0000179235'.
  RECON_TAB-RACCT2 = '0000179230'.
  APPEND RECON_TAB.

  RECON_TAB-RACCT = '0000252500'.
  RECON_TAB-RACCT1 = '0000252505'.
  RECON_TAB-RACCT2 = '0000252500'.
  APPEND RECON_TAB.

  RECON_TAB-RACCT = '0000254800'.
  RECON_TAB-RACCT1 = '0000254805'.
  RECON_TAB-RACCT2 = '0000254800'.
  APPEND RECON_TAB.

  RECON_TAB-RACCT = '0000254850'.
  RECON_TAB-RACCT1 = '0000254855'.
  RECON_TAB-RACCT2 = '0000254850'.
  APPEND RECON_TAB.

  RECON_TAB-RACCT = '0000350301'.
  RECON_TAB-RACCT1 = '0000350300'.
  RECON_TAB-RACCT2 = '0000350301'.
  APPEND RECON_TAB.

  RECON_TAB-RACCT = '0000350010'.
  RECON_TAB-RACCT1 = '0000350009'.
  RECON_TAB-RACCT2 = '0000350010'.
  APPEND RECON_TAB.

  RECON_TAB-RACCT = '0000350011'.
  RECON_TAB-RACCT1 = '0000350009'.
  RECON_TAB-RACCT2 = '0000350011'.
  APPEND RECON_TAB.

  RECON_TAB-RACCT = '0000350012'.
  RECON_TAB-RACCT1 = '0000350009'.
  RECON_TAB-RACCT2 = '0000350012'.
  APPEND RECON_TAB.

  RECON_TAB-RACCT = '0000350013'.
  RECON_TAB-RACCT1 = '0000350009'.
  RECON_TAB-RACCT2 = '0000350013'.
  APPEND RECON_TAB.

  RECON_TAB-RACCT = '0000350014'.
  RECON_TAB-RACCT1 = '0000350009'.
  RECON_TAB-RACCT2 = '0000350014'.
  APPEND RECON_TAB.

  RECON_TAB-RACCT = '0000350030'.
  RECON_TAB-RACCT1 = '0000350029'.
  RECON_TAB-RACCT2 = '0000350030'.
  APPEND RECON_TAB.

  RECON_TAB-RACCT = '0000350031'.
  RECON_TAB-RACCT1 = '0000350029'.
  RECON_TAB-RACCT2 = '0000350031'.
  APPEND RECON_TAB.

  RECON_TAB-RACCT = '0000350032'.
  RECON_TAB-RACCT1 = '0000350029'.
  RECON_TAB-RACCT2 = '0000350032'.
  APPEND RECON_TAB.

  RECON_TAB-RACCT = '0000350033'.
  RECON_TAB-RACCT1 = '0000350029'.
  RECON_TAB-RACCT2 = '0000350033'.
  APPEND RECON_TAB.

  RECON_TAB-RACCT = '0000350034'.
  RECON_TAB-RACCT1 = '0000350029'.
  RECON_TAB-RACCT2 = '0000350034'.
  APPEND RECON_TAB.

  RECON_TAB-RACCT = '0000350050'.
  RECON_TAB-RACCT1 = '0000350049'.
  RECON_TAB-RACCT2 = '0000350050'.
  APPEND RECON_TAB.

  RECON_TAB-RACCT = '0000350051'.
  RECON_TAB-RACCT1 = '0000350049'.
  RECON_TAB-RACCT2 = '0000350051'.
  APPEND RECON_TAB.

  RECON_TAB-RACCT = '0000350052'.
  RECON_TAB-RACCT1 = '0000350049'.
  RECON_TAB-RACCT2 = '0000350052'.
  APPEND RECON_TAB.

  RECON_TAB-RACCT = '0000350053'.
  RECON_TAB-RACCT1 = '0000350049'.
  RECON_TAB-RACCT2 = '0000350053'.
  APPEND RECON_TAB.

  RECON_TAB-RACCT = '0000350054'.
  RECON_TAB-RACCT1 = '0000350049'.
  RECON_TAB-RACCT2 = '0000350054'.
  APPEND RECON_TAB.

  RECON_TAB-RACCT = '0000350070'.
  RECON_TAB-RACCT1 = '0000350069'.
  RECON_TAB-RACCT2 = '0000350070'.
  APPEND RECON_TAB.

  RECON_TAB-RACCT = '0000350071'.
  RECON_TAB-RACCT1 = '0000350069'.
  RECON_TAB-RACCT2 = '0000350071'.
  APPEND RECON_TAB.

  RECON_TAB-RACCT = '0000350072'.
  RECON_TAB-RACCT1 = '0000350069'.
  RECON_TAB-RACCT2 = '0000350072'.
  APPEND RECON_TAB.

  RECON_TAB-RACCT = '0000350073'.
  RECON_TAB-RACCT1 = '0000350069'.
  RECON_TAB-RACCT2 = '0000350073'.
  APPEND RECON_TAB.

  RECON_TAB-RACCT = '0000350074'.
  RECON_TAB-RACCT1 = '0000350069'.
  RECON_TAB-RACCT2 = '0000350074'.
  APPEND RECON_TAB.

  RECON_TAB-RACCT = '0000350090'.
  RECON_TAB-RACCT1 = '0000350089'.
  RECON_TAB-RACCT2 = '0000350090'.
  APPEND RECON_TAB.

  RECON_TAB-RACCT = '0000350091'.
  RECON_TAB-RACCT1 = '0000350089'.
  RECON_TAB-RACCT2 = '0000350091'.
  APPEND RECON_TAB.

  RECON_TAB-RACCT = '0000350092'.
  RECON_TAB-RACCT1 = '0000350089'.
  RECON_TAB-RACCT2 = '0000350092'.
  APPEND RECON_TAB.

  RECON_TAB-RACCT = '0000350093'.
  RECON_TAB-RACCT1 = '0000350089'.
  RECON_TAB-RACCT2 = '0000350093'.
  APPEND RECON_TAB.

  RECON_TAB-RACCT = '0000350094'.
  RECON_TAB-RACCT1 = '0000350089'.
  RECON_TAB-RACCT2 = '0000350094'.
  APPEND RECON_TAB.
************************************************************************

ENDFORM.                               " LOAD_RECONCILIATION_ACCOUNTS
*&---------------------------------------------------------------------*
*&      Form  LOAD_FIELD_STATUS_GROUPS
*&---------------------------------------------------------------------*
*                                                                      *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LOAD_FIELD_STATUS_GROUPS.
  GROUP_TAB-FSTAG = 'BREV'.
  GROUP_TAB-MATNR = PMATNR.            "More data for this one
  APPEND GROUP_TAB.

  GROUP_TAB-FSTAG = 'BSHT'.
  APPEND GROUP_TAB.

  GROUP_TAB-FSTAG = 'EXMM'.
  GROUP_TAB-KOSTL = '10999'.
  GROUP_TAB-KOSTL2 = '10999'.
  APPEND GROUP_TAB.

  GROUP_TAB-FSTAG = 'GASS'.
  GROUP_TAB-MATNR = PMATNR.
  GROUP_TAB-MENGE = '0.001'.
  GROUP_TAB-MEINS = 'M3'.
  APPEND GROUP_TAB.

  GROUP_TAB-FSTAG = 'PAYR'.
  GROUP_TAB-PERNR = '99999'.
  APPEND GROUP_TAB.

  GROUP_TAB-FSTAG = 'PROJ'.
  GROUP_TAB-PS_PSP_PNR = '0197GTA7630'.
  GROUP_TAB-PS_PSP_PNR2 = '4697GTA7630'.
  APPEND GROUP_TAB.

  GROUP_TAB-FSTAG = 'REVN'.
  GROUP_TAB-MATNR = PMATNR.
  GROUP_TAB-KOSTL = '10999'.
  GROUP_TAB-KOSTL2 = '10999'.
* GROUP_TAB-PRCTR =
  APPEND GROUP_TAB.

  GROUP_TAB-FSTAG = 'RVOT'.
  GROUP_TAB-MATNR = PMATNR.
* GROUP_TAB-PRCTR =
  APPEND GROUP_TAB.

  GROUP_TAB-FSTAG = 'STCF'.
  GROUP_TAB-BEWAR = '199'.
  APPEND GROUP_TAB.
ENDFORM.                               " LOAD_FIELD_STATUS_GROUPS
*&---------------------------------------------------------------------*
*&      Form  CHECK_THE_GROUP
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_THE_GROUP.
  CLEAR GROUP_TAB.
  READ TABLE GROUP_TAB WITH KEY
                          FSTAG = ITAB1-FSTAG.
  CHECK SY-SUBRC = 0.
  MOVE-CORRESPONDING GROUP_TAB TO ITAB1.
ENDFORM.                               " CHECK_THE_GROUP
*&---------------------------------------------------------------------*
*&      Form  DO_THE_SAME_WITH_USD
*&---------------------------------------------------------------------*
*       All process with  USD                                          *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  CHECK_THE_GROUP_USD
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_THE_GROUP_USD.
  CLEAR GROUP_TAB.
  READ TABLE GROUP_TAB WITH KEY
                          FSTAG = ITAB2-FSTAG.
  CHECK SY-SUBRC = 0.
  MOVE-CORRESPONDING GROUP_TAB TO ITAB2.

ENDFORM.                               " CHECK_THE_GROUP_USD
*&---------------------------------------------------------------------*
*&      Form  WRITE_ITAB2
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM WRITE_ITAB2.
* Print itab2
  SKIP 1.
  FORMAT COLOR 2.
  WRITE: / TEXT-HE2 COLOR 5.
  SKIP 3.
  LOOP AT ITAB2.
WRITE: / ITAB2-RACCT UNDER TEXT-001, ITAB2-XBILK UNDER TEXT-BSA COLOR 7,
                                             ITAB2-BUKRS UNDER TEXT-002,
                      ITAB2-RACCT1 UNDER TEXT-003, 'UGL' UNDER TEXT-004,
                 ITAB2-WRBTR UNDER TEXT-005, ITAB2-FSTAG UNDER TEXT-006,
                    ITAB2-MCOD1 UNDER TEXT-DAT, SY-TABIX UNDER TEXT-NUM.
  ENDLOOP.
  FORMAT RESET.
  LOOP AT ITAB2.
    AT LAST.
      SUM.
      WRITE: / SY-ULINE(15) UNDER TEXT-005.
      WRITE: /26 TEXT-TOT,
               ITAB2-WRBTR UNDER TEXT-005 COLOR 3 INTENSIFIED ON, 'USD'.
    ENDAT.
  ENDLOOP.

ENDFORM.                               " WRITE_ITAB2
*&---------------------------------------------------------------------*
*&      Form  CREATE_BDC_SESSION
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_BDC_SESSION.
* Fill the struct. BGR00               HEADER of THE FILE
  MOVE '0'            TO  Z_BGR00-STYPE.
  MOVE 'ZFIMI002'     TO  Z_BGR00-GROUP.
  MOVE SY-MANDT       TO  Z_BGR00-MANDT.
  MOVE SY-UNAME       TO  Z_BGR00-USNAM.
  TRANSFER Z_BGR00    TO  PHYFILE LENGTH 167. "Another file is that size

* Fill the structure BBKPF.

  MOVE '1'            TO Z_BBKPF-STYPE." Obligatory
  MOVE T_CODE         TO Z_BBKPF-TCODE." T-code

  WRITE '19980531' TO DATE.            "Posting date
  MOVE DATE TO Z_BBKPF-BUDAT.          "Posting date
  MOVE SY-DATUM TO Z_BBKPF-BLDAT.      "Document date

  MOVE 'ZM' TO Z_BBKPF-BLART.          "Document type
  MOVE 'UGL' TO Z_BBKPF-BUKRS.         "Company code
  MOVE 'CAD' TO Z_BBKPF-WAERS.         "Currency

  MOVE 'Balance transf. from MNPP' TO Z_BBKPF-BKTXT.   "Header text.
  MOVE 'from MNPP' TO Z_BBKPF-XBLNR.   "Header text.  "Reference documen
  TRANSFER Z_BBKPF    TO  PHYFILE LENGTH 167."Another file is that size

* Fill the structure ZCOPA_BSEG.
  LOOP AT ITAB1.
    PERFORM INIT_STRUCTURES_SPECIAL USING 'ZCOPA_BSEG'. " I_BGR00.

    move '2'            to z_zcopa_bseg-stype." Obligatory
    MOVE 'Balance transf. from MNPP' TO Z_ZCOPA_BSEG-SGTXT.

    MOVE SY-TABIX TO TABIX.
* take account and posting key

    IF ITAB1-WRBTR > 0.
      move  '40' to z_zcopa_bseg-newbs.                     "Key
    ELSE.
      move  '50' to z_zcopa_bseg-newbs.                     "Key
    ENDIF.
    MOVE ITAB1-RACCT1 TO Z_ZCOPA_BSEG-HKONT. "Account

* CHECK THE BALANCE
    CURRBAL = CURRBAL + ITAB1-WRBTR.
* Just positive value
    IF ITAB1-WRBTR < 0.
      ITAB1-WRBTR = ITAB1-WRBTR * ( - 1 ).
    ENDIF.
    WRITE ITAB1-WRBTR TO WRBTR.
    move wrbtr to z_zcopa_bseg-wrbtr.  "Money

    IF ITAB1-MWSKZ NE SPACE.
      move itab1-mwskz to z_zcopa_bseg-mwskz.  "Tax code
    ENDIF.
    CASE ITAB1-FSTAG.
      WHEN 'BREV'.

        CLEAR GROUP_TAB.
        READ TABLE GROUP_TAB WITH KEY
                                      FSTAG = 'BREV'.
        CHECK SY-SUBRC = 0.

        move group_tab-matnr to z_zcopa_bseg-matnr. " matnr

        move '1' to z_zcopa_bseg-kndnr. "cbseg-kndnr.
        MOVE 'U999' TO Z_ZCOPA_BSEG-VKORG.  "cbseg-vkorg.
*        move '00' to z_zcopa_bseg-vtweg.    "cbseg-vtweg.
*        move '00' to z_zcopa_bseg-spart.    "cbseg-spart.
*        move '01' to z_zcopa_bseg-wwdvn.    "cbseg-wwdvn.
        MOVE 'Z'  TO Z_ZCOPA_BSEG-WWSCT.    "cbseg-wwsct.
*        move 'ZZ' to z_zcopa_bseg-wwseg.    "cbseg-wwseg.
*        move 'M2' to z_zcopa_bseg-wwrat.    "cbseg-wwrat.
*        move '10' to z_zcopa_bseg-wwser.    "cbseg-wwser.
        TRANSFER Z_ZCOPA_BSEG    TO  PHYFILE LENGTH 2167."Approx size

      WHEN 'BSHT'.

* THEN NO ACTIONS
        CLEAR GROUP_TAB.

        READ TABLE GROUP_TAB WITH KEY
                                      FSTAG = 'BSHT'.
        CHECK SY-SUBRC = 0.

        TRANSFER Z_ZCOPA_BSEG    TO  PHYFILE LENGTH 2167."Approx size

      WHEN 'EXMM'.
        IF ITAB1-RACCT1 = '0000303200' OR "Check for automatic CostC.ass
           ITAB1-RACCT1 = '0000319100' OR
           ITAB1-RACCT1 = '0000324101' OR
           ITAB1-RACCT1 = '0000325100' OR
           ITAB1-RACCT1 = '0000325200' OR
           ITAB1-RACCT1 = '0000325201'.
          GROUP_TAB-KOSTL = '10999'.   "DUMMY Cost Center
          MOVE GROUP_TAB-KOSTL TO Z_ZCOPA_BSEG-KOSTL.
        ENDIF.

        CLEAR GROUP_TAB.

        READ TABLE GROUP_TAB WITH KEY
                                      FSTAG = 'EXMM'.
        CHECK SY-SUBRC = 0.
        move group_tab-kostl to z_zcopa_bseg-kostl.
        TRANSFER Z_ZCOPA_BSEG    TO  PHYFILE LENGTH 2167."Approx size


      WHEN 'REVN'.

        CLEAR GROUP_TAB.

        READ TABLE GROUP_TAB WITH KEY
                                      FSTAG = 'REVN'.
        CHECK SY-SUBRC = 0.
        move group_tab-kostl to z_zcopa_bseg-kostl.
        move group_tab-matnr to z_zcopa_bseg-matnr.

        MOVE '1' TO Z_ZCOPA_BSEG-KNDNR."cbseg-kndnr.
        MOVE 'U999' TO Z_ZCOPA_BSEG-VKORG.  "cbseg-vkorg.
*        move '00' to z_zcopa_bseg-vtweg.    "cbseg-vtweg.
*        move '00' to z_zcopa_bseg-spart.    "cbseg-spart.
*        move '01' to z_zcopa_bseg-wwdvn.    "cbseg-wwdvn.
        move 'Z'  to z_zcopa_bseg-wwsct.    "cbseg-wwsct.
*        move 'ZZ' to z_zcopa_bseg-wwseg.    "cbseg-wwseg.
*        move 'M2' to z_zcopa_bseg-wwrat.    "cbseg-wwrat.
*        move '10' to z_zcopa_bseg-wwser.    "cbseg-wwser.
        TRANSFER Z_ZCOPA_BSEG    TO  PHYFILE LENGTH 2167."Approx size
      WHEN 'RVOT'.

        CLEAR GROUP_TAB.

        READ TABLE GROUP_TAB WITH KEY
                                      FSTAG = 'RVOT'.
        CHECK SY-SUBRC = 0.
        move group_tab-matnr to z_zcopa_bseg-matnr.

        move '1' to z_zcopa_bseg-kndnr. "cbseg-kndnr.
        MOVE 'U999' TO Z_ZCOPA_BSEG-VKORG.  "cbseg-vkorg.
*        move '00' to z_zcopa_bseg-vtweg.    "cbseg-vtweg.
*        move '00' to z_zcopa_bseg-spart.    "cbseg-spart.
*        move '01' to z_zcopa_bseg-wwdvn.    "cbseg-wwdvn.
        move 'Z'  to z_zcopa_bseg-wwsct.    "cbseg-wwsct.
*        move 'ZZ' to z_zcopa_bseg-wwseg.    "cbseg-wwseg.
*        move 'M2' to z_zcopa_bseg-wwrat.    "cbseg-wwrat.
*        move '10' to z_zcopa_bseg-wwser.    "cbseg-wwser.
        TRANSFER Z_ZCOPA_BSEG    TO  PHYFILE LENGTH 2167."Approx size


      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.
  PERFORM INIT_STRUCTURES_SPECIAL USING 'ZCOPA_BSEG'. " I_BGR00.
  PERFORM CLOSE_PROCESS.
  CLOSE DATASET PHYFILE.
  IF SY-SUBRC <> 0.
    WRITE: / 'Problem with closing a file !'.
  ELSE.
 WRITE: / 'Ok! Now run the program ZKPAA001 with the variant ZFIMI002 '
                                                                COLOR 7.
  ENDIF.

ENDFORM.                               " CREATE_BDC_SESSION

*&---------------------------------------------------------------------*
*&      Form  BDC_SCREEN
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BDC_SCREEN  USING PROGRAM DYNPRO.
  CLEAR BDCDATA.
  BDCDATA-PROGRAM  = PROGRAM.
  BDCDATA-DYNPRO   = DYNPRO.
  BDCDATA-DYNBEGIN = 'X'.
  APPEND BDCDATA.

ENDFORM.                               " BDC_SCREEN

*&---------------------------------------------------------------------*
*&      Form  BDC_FIELD
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BDC_FIELD USING FNAM FVAL.
  CLEAR BDCDATA.
  BDCDATA-FNAM = FNAM.
  BDCDATA-FVAL = FVAL.
  APPEND BDCDATA.

ENDFORM.                               " BDC_FIELD
*&---------------------------------------------------------------------*
*&      Form  CLOSE_PROCESS
*&---------------------------------------------------------------------*
*       Off set account balance                                        *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CLOSE_PROCESS.
  move '2'            to z_zcopa_bseg-stype." Obligatory
  MOVE 'Balance transf. from MNPP' TO Z_ZCOPA_BSEG-SGTXT.

  CURRBAL = CURRBAL * ( - 1 ).
* take account and posting key
  IF CURRBAL > 0.
    move  '40' to z_zcopa_bseg-newbs.                       "Key
  ELSE.
    move  '50' to z_zcopa_bseg-newbs.                       "Key
  ENDIF.
  MOVE '0000199998' TO Z_ZCOPA_BSEG-HKONT.  "Account

  IF CURRBAL < 0.
    CURRBAL = CURRBAL * ( - 1 ).
  ENDIF.
  move currbal to z_zcopa_bseg-wrbtr.  "Balance
  TRANSFER Z_ZCOPA_BSEG    TO  PHYFILE LENGTH 2167."Approx size
  PERFORM INIT_STRUCTURES USING 'ZCOPA_BSEG'. " ZCOPA_BSEG

ENDFORM.                               " CLOSE_PROCESS


*&---------------------------------------------------------------------*
*&      Form  OPEN_OUTPUT_FILE
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM OPEN_OUTPUT_FILE.
  CALL FUNCTION 'FILE_GET_NAME'
       EXPORTING
            LOGICAL_FILENAME = CORRFILE
       IMPORTING
            FILE_NAME        = PHYFILE
       EXCEPTIONS
            FILE_NOT_FOUND   = 1
            OTHERS           = 2.
  IF SY-SUBRC <> 0.
    MESSAGE E006 WITH CORRFILE.
  ELSE.
    OPEN DATASET PHYFILE FOR OUTPUT IN TEXT MODE.
  ENDIF.

ENDFORM.                               " OPEN_OUTPUT_FILE

*&---------------------------------------------------------------------*
*&      Form  INIT_STRUCTURES
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_STRUCTURES USING TABNAME.    "tab..
  SELECT * FROM DD03L WHERE TABNAME = TABNAME.
    CLEAR CHAR.
    CHAR(2)   = 'Z_'.
    CHAR+2(5) = TABNAME.
    CHAR+7(1) = '-'.
    CHAR+8    = DD03L-FIELDNAME.
    ASSIGN (CHAR) TO <F1>.
    <F1> = NODATA.
  ENDSELECT.

ENDFORM.                               " INIT_STRUCTURES
*&---------------------------------------------------------------------*
*&      Form  INIT_STRUCTURES_SPECIAL
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_STRUCTURES_SPECIAL USING TABNAME.
  SELECT * FROM DD03L WHERE TABNAME = TABNAME.
    CLEAR CHAR.
    CHAR(2)   = 'Z_'.
    CHAR+2(10) = TABNAME.
    CHAR+12(1) = '-'.
    CHAR+13    = DD03L-FIELDNAME.
    ASSIGN (CHAR) TO <F1>.
    <F1> = NODATA.
  ENDSELECT.

ENDFORM.                               " INIT_STRUCTURES_SPECIAL
*&---------------------------------------------------------------------*
*&      Form  CREATE_BDC_SESSION2
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_BDC_SESSION2.
  PERFORM INIT_STRUCTURES USING 'BGR00'. "I_BGR00.

* Fill the struct. BGR00               HEADER of THE FILE
  MOVE '0'            TO  Z_BGR00-STYPE.
  MOVE 'ZFIMI002'     TO  Z_BGR00-GROUP.
  MOVE SY-MANDT       TO  Z_BGR00-MANDT.
  MOVE SY-UNAME       TO  Z_BGR00-USNAM.
  move sy-datum to date.
  move date to Z_BGR00-start.
  TRANSFER Z_BGR00    TO  PHYFILE LENGTH 200. "Another file is that size

  PERFORM INIT_STRUCTURES USING 'BBKPF'. "I_BBKPF.
* Fill the structure BBKPF.

  MOVE '1'            TO Z_BBKPF-STYPE." Obligatory
  MOVE T_CODE         TO Z_BBKPF-TCODE." T-code

  WRITE sy-datum TO DATE.            "Posting date
  WRITE DATE TO Z_BBKPF-BUDAT.         "Posting date
  WRITE SY-DATUM TO Z_BBKPF-BLDAT.     "Document date

  MOVE 'ZM' TO Z_BBKPF-BLART.          "Document type
  MOVE pbukrs TO Z_BBKPF-BUKRS.         "Company code
  MOVE T001-waers TO Z_BBKPF-WAERS.         "Currency

  MOVE 'Transfer to PeopleSoft' TO Z_BBKPF-BKTXT.   "Header text.
  MOVE 'PeopleSoft' TO Z_BBKPF-XBLNR.  "Header text.  "Reference documen
  move z_bbkpf to wa_record.
  move wa_record+21(200) to wa_record+5(220).
  TRANSFER wa_record  TO  PHYFILE LENGTH 200."Another file is that size
*break-point.
* Fill the structure Z_BBSEG.
  LOOP AT ITAB1.
    PERFORM INIT_STRUCTURES USING 'BBSEG'. " I_BBSEG.

    MOVE '2'            TO Z_BBSEG-STYPE." Obligatory
    MOVE 'BBSEG'        TO Z_BBSEG-TBNAM.
    MOVE 'Balance transf. from MNPP' TO Z_BBSEG-SGTXT.

    MOVE SY-TABIX TO TABIX.
* take account and posting key

    IF ITAB1-WRBTR > 0.
      MOVE  '40' TO Z_BBSEG-NEWBS.                          "Key
    ELSE.
      MOVE  '50' TO Z_BBSEG-NEWBS.                          "Key
    ENDIF.
    MOVE ITAB1-RACCT1 TO Z_BBSEG-HKONT."Account

* CHECK THE BALANCE
    CURRBAL = CURRBAL + ITAB1-WRBTR.
* Just positive value
    IF ITAB1-WRBTR < 0.
      ITAB1-WRBTR = ITAB1-WRBTR * ( - 1 ).
    ENDIF.
    WRITE ITAB1-WRBTR TO WRBTR.
    MOVE WRBTR TO Z_BBSEG-WRBTR.       "Money

    IF ITAB1-MWSKZ NE SPACE.
      MOVE ITAB1-MWSKZ TO Z_BBSEG-MWSKZ.  "Tax code
    ENDIF.
    CASE ITAB1-FSTAG.

      WHEN 'GASS'.
        CLEAR GROUP_TAB.

        READ TABLE GROUP_TAB WITH KEY
                                      FSTAG = 'GASS'.
        CHECK SY-SUBRC = 0.
        MOVE GROUP_TAB-MATNR TO Z_BBSEG-MATNR.
        MOVE GROUP_TAB-MENGE TO Z_BBSEG-MENGE.
        MOVE GROUP_TAB-MEINS TO Z_BBSEG-MEINS.

        move z_bbseg to wa_record.
        move wa_record+31(2000) to wa_record+11(2020).
        TRANSFER wa_record TO  PHYFILE LENGTH 2167."Approx size

      WHEN 'PAYR'.
        CLEAR GROUP_TAB.

        READ TABLE GROUP_TAB WITH KEY
                                      FSTAG = 'PAYR'.
        CHECK SY-SUBRC = 0.
        MOVE GROUP_TAB-PERNR TO Z_BBSEG-PERNR.
        move z_bbseg to wa_record.
        move wa_record+31(2000) to wa_record+11(2020).
        TRANSFER wa_record TO  PHYFILE LENGTH 2167."Approx size

*        TRANSFER Z_BBSEG    TO  PHYFILE LENGTH 2167."Approx size


      WHEN 'PROJ'.
        CLEAR GROUP_TAB.

        READ TABLE GROUP_TAB WITH KEY
                                      FSTAG = 'PROJ'.
        CHECK SY-SUBRC = 0.
        MOVE GROUP_TAB-PS_PSP_PNR TO Z_BBSEG-PROJK.
        move z_bbseg to wa_record.
        move wa_record+31(2000) to wa_record+11(2020).
        TRANSFER wa_record TO  PHYFILE LENGTH 2167."Approx size

*        TRANSFER Z_BBSEG    TO  PHYFILE LENGTH 2167."Approx size



      WHEN 'STCF'.
        CLEAR GROUP_TAB.

        READ TABLE GROUP_TAB WITH KEY
                                      FSTAG = 'STCF'.
        CHECK SY-SUBRC = 0.
        MOVE GROUP_TAB-BEWAR TO Z_BBSEG-BEWAR.
        move z_bbseg to wa_record.
        move wa_record+31(2000) to wa_record+11(2020).
        TRANSFER wa_record TO  PHYFILE LENGTH 2167."Approx size

*       TRANSFER Z_BBSEG    TO  PHYFILE LENGTH 2167."Approx size

      WHEN OTHERS.
    ENDCASE.

*    IF TABIX = '3'.
*      PERFORM CLOSE_PROCESS.
*      FLAG = CURRBAL = TABIX = 0.
*      EXIT.
*    ENDIF.
  ENDLOOP.
  PERFORM INIT_STRUCTURES USING 'BBSEG'. " I_BBSEG.
  PERFORM CLOSE_PROCESS2.
  CLOSE DATASET PHYFILE.
  IF SY-SUBRC <> 0.
    WRITE: / 'Problem with closing a file !'.
  ELSE.
 WRITE: / 'Ok! Now run the program RFBIBL00 with the variant ZFIMI002 '
                                                                COLOR 7.
  ENDIF.

ENDFORM.                               " CREATE_BDC_SESSION2
*&---------------------------------------------------------------------*
*&      Form  CLOSE_PROCESS2
*&---------------------------------------------------------------------*
*       Process 2                                                      *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CLOSE_PROCESS2.
  MOVE '2'            TO Z_BBSEG-STYPE." Obligatory
  MOVE 'BBSEG'        TO Z_BBSEG-TBNAM.
  MOVE 'Balance transf. to PSoft' TO Z_BBSEG-SGTXT.

  CURRBAL = CURRBAL * ( - 1 ).
* take account and posting key
  IF CURRBAL > 0.
    MOVE  '40' TO Z_BBSEG-NEWBS.                            "Key
  ELSE.
    MOVE  '50' TO Z_BBSEG-NEWBS.                            "Key
  ENDIF.
  MOVE '0000199998' TO Z_BBSEG-HKONT.  "Account

  IF CURRBAL < 0.
    CURRBAL = CURRBAL * ( - 1 ).
  ENDIF.
  MOVE CURRBAL TO Z_BBSEG-WRBTR.       "Balance
  move z_bbseg to wa_record.
  move wa_record+31(2000) to wa_record+11(2020).
  TRANSFER wa_record TO  PHYFILE LENGTH 2167."Approx size

*  TRANSFER Z_BBSEG    TO  PHYFILE LENGTH 2167."Approx size
  PERFORM INIT_STRUCTURES USING 'BBSEG'. " BBSEG

ENDFORM.                               " CLOSE_PROCESS2

*&---------------------------------------------------------------------*
*&      Form  CREATE_BDC_SESSION4
*&---------------------------------------------------------------------*
*       MAke the balance 0 in CGO   I group of acc.                    *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_BDC_SESSION4.
* Fill the struct. BGR00               HEADER of THE FILE
  MOVE '0'            TO  Z_BGR00-STYPE.
  MOVE 'ZFIMI002'     TO  Z_BGR00-GROUP.
  MOVE SY-MANDT       TO  Z_BGR00-MANDT.
  MOVE SY-UNAME       TO  Z_BGR00-USNAM.
  write sy-datum to date.
  move date to Z_BGR00-start.
  TRANSFER Z_BGR00    TO  PHYFILE LENGTH 233. "Another file is that size

* Fill the structure BBKPF.

  MOVE '1'            TO Z_BBKPF-STYPE." Obligatory
  MOVE T_CODE         TO Z_BBKPF-TCODE." T-code

  WRITE sy-datum TO DATE.              "Posting date   "mdemeest
  MOVE DATE TO Z_BBKPF-BUDAT.          "Posting date
  MOVE date TO Z_BBKPF-BLDAT.      "Document date  "mdemeest

  MOVE 'ZM' TO Z_BBKPF-BLART.          "Document type
  MOVE pbukrs TO Z_BBKPF-BUKRS.         "Company code  "mdemeest
  MOVE t001-waers TO Z_BBKPF-WAERS.         "Currency

  MOVE 'Transfer $$ to PeopleSoft' TO Z_BBKPF-BKTXT.   "Header text.
  MOVE 'PeopleSoft'TO Z_BBKPF-XBLNR.   "Header text.  "Reference documen
  move Z_BBKPF to wa_record.
  move wa_record+21(200) to wa_record+5(220).
  TRANSFER wa_record   TO  PHYFILE LENGTH 233."Another file is that size

* Fill the structure ZCOPA_BSEG.
  LOOP AT ITAB1.
    PERFORM INIT_STRUCTURES_SPECIAL USING 'ZCOPA_BSEG'. " I_BGR00.

    move '2'            to z_zcopa_bseg-stype." Obligatory

    MOVE 'Transfer final balance to PSoft' TO Z_ZCOPA_BSEG-SGTXT.
    move 'BBSEG' to z_zcopa_bseg-tbnam.

    MOVE SY-TABIX TO TABIX.
* take account and posting key

    IF ITAB1-WRBTR > 0.
      move  '40' to z_zcopa_bseg-newbs.                     "Key
    ELSE.
      move  '50' to z_zcopa_bseg-newbs.                     "Key
    ENDIF.

* Watch this! itab1-racct2.  (See internal cross-reference table)
    MOVE ITAB1-RACCT2 TO Z_ZCOPA_BSEG-HKONT. "Account   ( CGO acc. )

* CHECK THE BALANCE
    CURRBAL = CURRBAL + ITAB1-WRBTR.
* Just positive value
    IF ITAB1-WRBTR < 0.
      ITAB1-WRBTR = ITAB1-WRBTR * ( - 1 ).
    ENDIF.
    WRITE ITAB1-WRBTR TO WRBTR.
    move wrbtr to z_zcopa_bseg-wrbtr.  "Money

    IF ITAB1-MWSKZ NE SPACE.
      move itab1-mwskz to z_zcopa_bseg-mwskz.  "Tax code
    ENDIF.
    CASE ITAB1-FSTAG.
      WHEN 'BREV'.
        CLEAR GROUP_TAB.
        READ TABLE GROUP_TAB WITH KEY
                                      FSTAG = 'BREV'.
        CHECK SY-SUBRC = 0.
        move group_tab-matnr to z_zcopa_bseg-matnr. " matnr

        move '1' to z_zcopa_bseg-kndnr. "cbseg-kndnr.
        MOVE 'C999' TO Z_ZCOPA_BSEG-VKORG.  "cbseg-vkorg.
*        MOVE '00' TO Z_ZCOPA_BSEG-VTWEG.    "cbseg-vtweg.
*        MOVE '00' TO Z_ZCOPA_BSEG-SPART.    "cbseg-spart.
*        MOVE '22' TO Z_ZCOPA_BSEG-WWDVN.    "cbseg-wwdvn.
        MOVE 'Z'  TO Z_ZCOPA_BSEG-WWSCT.    "cbseg-wwsct.
*        MOVE 'ZZ' TO Z_ZCOPA_BSEG-WWSEG.    "cbseg-wwseg.
*        MOVE '01' TO Z_ZCOPA_BSEG-WWRAT.    "cbseg-wwrat.
*        MOVE '15' TO Z_ZCOPA_BSEG-WWSER.    "cbseg-wwser.
        move z_zcopa_bseg to wa_record.
        move wa_record+31(2000) to wa_record+11(2000).
        TRANSFER wa_record    TO  PHYFILE LENGTH 2167."Approx size

      WHEN 'BSHT'.
* THEN NO ACTIONS
        CLEAR GROUP_TAB.

        READ TABLE GROUP_TAB WITH KEY
                                      FSTAG = 'BSHT'.
        CHECK SY-SUBRC = 0.
        move z_zcopa_bseg to wa_record.
        move wa_record+31(2000) to wa_record+11(2000).
        TRANSFER wa_record    TO  PHYFILE LENGTH 2167."Approx size

*        TRANSFER Z_ZCOPA_BSEG    TO  PHYFILE LENGTH 2167."Approx size

      WHEN 'EXMM'.
        IF ITAB1-RACCT2 = '0000303200' OR "Check for automatic CostC.ass
           ITAB1-RACCT2 = '0000319100' OR " in CGO
           ITAB1-RACCT2 = '0000324101' OR
           ITAB1-RACCT2 = '0000325100' OR
           ITAB1-RACCT2 = '0000325200' OR
           ITAB1-RACCT2 = '0000325201'.
          GROUP_TAB-KOSTL2 = '10999'.  "DUMMY Cost Center CGO
          MOVE GROUP_TAB-KOSTL2 TO Z_ZCOPA_BSEG-KOSTL.
        ENDIF.

        CLEAR GROUP_TAB.

        READ TABLE GROUP_TAB WITH KEY
                                      FSTAG = 'EXMM'.
        CHECK SY-SUBRC = 0.
        MOVE GROUP_TAB-KOSTL2 TO Z_ZCOPA_BSEG-KOSTL.
        move z_zcopa_bseg to wa_record.
        move wa_record+31(2000) to wa_record+11(2000).
        TRANSFER wa_record    TO  PHYFILE LENGTH 2167."Approx size

*        TRANSFER Z_ZCOPA_BSEG    TO  PHYFILE LENGTH 2167."Approx size

* We still don't now


      WHEN 'REVN'.
        CLEAR GROUP_TAB.

        READ TABLE GROUP_TAB WITH KEY
                                      FSTAG = 'REVN'.
        CHECK SY-SUBRC = 0.
        MOVE GROUP_TAB-KOSTL2 TO Z_ZCOPA_BSEG-KOSTL.
        move group_tab-matnr to z_zcopa_bseg-matnr.

        move '1' to z_zcopa_bseg-kndnr. "cbseg-kndnr.
        MOVE 'C999' TO Z_ZCOPA_BSEG-VKORG.  "cbseg-vkorg.
*        MOVE '00' TO Z_ZCOPA_BSEG-VTWEG.    "cbseg-vtweg.
*        MOVE '00' TO Z_ZCOPA_BSEG-SPART.    "cbseg-spart.
*        MOVE '22' TO Z_ZCOPA_BSEG-WWDVN.    "cbseg-wwdvn.
        MOVE 'Z'  TO Z_ZCOPA_BSEG-WWSCT.    "cbseg-wwsct.
*        MOVE 'ZZ' TO Z_ZCOPA_BSEG-WWSEG.    "cbseg-wwseg.
*        MOVE '01' TO Z_ZCOPA_BSEG-WWRAT.    "cbseg-wwrat.
*        MOVE '15' TO Z_ZCOPA_BSEG-WWSER.    "cbseg-wwser.
        move z_zcopa_bseg to wa_record.
        move wa_record+31(2000) to wa_record+11(2000).
        TRANSFER wa_record    TO  PHYFILE LENGTH 2167."Approx size

*       TRANSFER Z_ZCOPA_BSEG    TO  PHYFILE LENGTH 2167."Approx size

      WHEN 'RVOT'.
        CLEAR GROUP_TAB.

        READ TABLE GROUP_TAB WITH KEY
                                      FSTAG = 'RVOT'.
        CHECK SY-SUBRC = 0.
        move group_tab-matnr to z_zcopa_bseg-matnr.

        move '1' to z_zcopa_bseg-kndnr. "cbseg-kndnr.
        MOVE 'C999' TO Z_ZCOPA_BSEG-VKORG.  "cbseg-vkorg.
*        MOVE '00' TO Z_ZCOPA_BSEG-VTWEG.    "cbseg-vtweg.
*        MOVE '00' TO Z_ZCOPA_BSEG-SPART.    "cbseg-spart.
*        MOVE '22' TO Z_ZCOPA_BSEG-WWDVN.    "cbseg-wwdvn.
        MOVE 'Z'  TO Z_ZCOPA_BSEG-WWSCT.    "cbseg-wwsct.
*        MOVE 'ZZ' TO Z_ZCOPA_BSEG-WWSEG.    "cbseg-wwseg.
*        MOVE '01' TO Z_ZCOPA_BSEG-WWRAT.    "cbseg-wwrat.
*        MOVE '15' TO Z_ZCOPA_BSEG-WWSER.    "cbseg-wwser.
        move z_zcopa_bseg to wa_record.
        move wa_record+31(2000) to wa_record+11(2000).
        TRANSFER wa_record    TO  PHYFILE LENGTH 2167."Approx size

*        TRANSFER Z_ZCOPA_BSEG    TO  PHYFILE LENGTH 2167."Approx size


      WHEN OTHERS.

    ENDCASE.

*    IF TABIX = '3'.
*      PERFORM CLOSE_PROCESS.
*      FLAG = CURRBAL = TABIX = 0.
*      EXIT.
*    ENDIF.
  ENDLOOP.
  PERFORM INIT_STRUCTURES_SPECIAL USING 'ZCOPA_BSEG'. " I_BGR00.
  PERFORM CLOSE_PROCESS4.
  CLOSE DATASET PHYFILE.
  IF SY-SUBRC <> 0.
    WRITE: / 'Problem with closing a file !'.
  ELSE.
 WRITE: / 'Ok! Now run the program ZKPAA001 with the variant ZFIMI002 '
                                                                COLOR 7.
  ENDIF.

ENDFORM.                               " CREATE_BDC_SESSION4
*&---------------------------------------------------------------------*
*&      Form  CREATE_BDC_SESSION5
*&---------------------------------------------------------------------*
*       CGO   make the balance 0 group II                              *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_BDC_SESSION5.
  PERFORM INIT_STRUCTURES USING 'BGR00'. "I_BGR00.

* Fill the struct. BGR00               HEADER of THE FILE
  MOVE '0'            TO  Z_BGR00-STYPE.
  MOVE 'ZFIMI002'     TO  Z_BGR00-GROUP.
  MOVE SY-MANDT       TO  Z_BGR00-MANDT.
  MOVE SY-UNAME       TO  Z_BGR00-USNAM.
  TRANSFER Z_BGR00    TO  PHYFILE LENGTH 200. "Another file is that size

  PERFORM INIT_STRUCTURES USING 'BBKPF'. "I_BBKPF.
* Fill the structure BBKPF.

  MOVE '1'            TO Z_BBKPF-STYPE." Obligatory
  MOVE T_CODE         TO Z_BBKPF-TCODE." T-code

  WRITE sy-datum TO DATE.            "Posting date
  WRITE DATE TO Z_BBKPF-BUDAT.         "Posting date
  WRITE SY-DATUM TO Z_BBKPF-BLDAT.     "Document date

  MOVE 'ZM' TO Z_BBKPF-BLART.          "Document type
  MOVE pbukrs TO Z_BBKPF-BUKRS.         "Company code
  MOVE t001-waers TO Z_BBKPF-WAERS.         "Currency


  MOVE 'Balance transf. into CGO' TO Z_BBKPF-BKTXT.   "Header text.
  MOVE 'To CGO' TO Z_BBKPF-XBLNR.      "Header text.  "Reference documen
  move Z_BBKPF to wa_record.
  move wa_record+21(200) to wa_record+5(220).
  transfer wa_record to phyfile length 200.
*  TRANSFER Z_BBKPF    TO  PHYFILE LENGTH 200."Another file is that size
*break-point.
* Fill the structure Z_BBSEG.
  LOOP AT ITAB1.
    PERFORM INIT_STRUCTURES USING 'BBSEG'. " I_BBSEG.

    MOVE '2'            TO Z_BBSEG-STYPE." Obligatory
    MOVE 'BBSEG'        TO Z_BBSEG-TBNAM.
    MOVE 'Balance transf. from MNPP' TO Z_BBSEG-SGTXT.

    MOVE SY-TABIX TO TABIX.
* take account and posting key

    IF ITAB1-WRBTR > 0.
      MOVE  '40' TO Z_BBSEG-NEWBS.                          "Key
    ELSE.
      MOVE  '50' TO Z_BBSEG-NEWBS.                          "Key
    ENDIF.

* Watch: this i for CGO only
    MOVE ITAB1-RACCT2 TO Z_BBSEG-HKONT."Account

* CHECK THE BALANCE
    CURRBAL = CURRBAL + ITAB1-WRBTR.
* Just positive value
    IF ITAB1-WRBTR < 0.
      ITAB1-WRBTR = ITAB1-WRBTR * ( - 1 ).
    ENDIF.
    WRITE ITAB1-WRBTR TO WRBTR.
    MOVE WRBTR TO Z_BBSEG-WRBTR.       "Money

    IF ITAB1-MWSKZ NE SPACE.
      MOVE ITAB1-MWSKZ TO Z_BBSEG-MWSKZ.  "Tax code
    ENDIF.
    CASE ITAB1-FSTAG.

      WHEN 'GASS'.
        CLEAR GROUP_TAB.

        READ TABLE GROUP_TAB WITH KEY
                                      FSTAG = 'GASS'.
        CHECK SY-SUBRC = 0.
        MOVE GROUP_TAB-MATNR TO Z_BBSEG-MATNR.
        MOVE GROUP_TAB-MENGE TO Z_BBSEG-MENGE.
        MOVE GROUP_TAB-MEINS TO Z_BBSEG-MEINS.

        move z_bbseg to wa_record.
        move wa_record+31(2000) to wa_record+11(2020).

        TRANSFER wa_record   TO  PHYFILE LENGTH 2167."Approx size

      WHEN 'PAYR'.
        CLEAR GROUP_TAB.

        READ TABLE GROUP_TAB WITH KEY
                                      FSTAG = 'PAYR'.
        CHECK SY-SUBRC = 0.
        MOVE GROUP_TAB-PERNR TO Z_BBSEG-PERNR.

        move z_bbseg to wa_record.
        move wa_record+31(2000) to wa_record+11(2020).

        TRANSFER wa_record   TO  PHYFILE LENGTH 2167."Approx size
*        TRANSFER Z_BBSEG    TO  PHYFILE LENGTH 2167."Approx size


      WHEN 'PROJ'.
        CLEAR GROUP_TAB.

        READ TABLE GROUP_TAB WITH KEY
                                      FSTAG = 'PROJ'.
        CHECK SY-SUBRC = 0.
        MOVE GROUP_TAB-PS_PSP_PNR2 TO Z_BBSEG-PROJK.  "CGO projk

        move z_bbseg to wa_record.
        move wa_record+31(2000) to wa_record+11(2020).

        TRANSFER wa_record   TO  PHYFILE LENGTH 2167."Approx size

*       TRANSFER Z_BBSEG    TO  PHYFILE LENGTH 2167."Approx size



      WHEN 'STCF'.
        CLEAR GROUP_TAB.

        READ TABLE GROUP_TAB WITH KEY
                                      FSTAG = 'STCF'.
        CHECK SY-SUBRC = 0.
        MOVE GROUP_TAB-BEWAR TO Z_BBSEG-BEWAR.
        move z_bbseg to wa_record.
        move wa_record+31(2000) to wa_record+11(2020).

        TRANSFER wa_record   TO  PHYFILE LENGTH 2167."Approx size

*        TRANSFER Z_BBSEG    TO  PHYFILE LENGTH 2167."Approx size

      WHEN OTHERS.
    ENDCASE.

*    IF TABIX = '3'.
*      PERFORM CLOSE_PROCESS.
*      FLAG = CURRBAL = TABIX = 0.
*      EXIT.
*    ENDIF.
  ENDLOOP.
  PERFORM INIT_STRUCTURES USING 'BBSEG'. " I_BBSEG.
  PERFORM CLOSE_PROCESS5.
  CLOSE DATASET PHYFILE.
  IF SY-SUBRC <> 0.
    WRITE: / 'Problem with closing a file !'.
  ELSE.
 WRITE: / 'Ok! Now run the program RFBIBL00 with the variant ZFIMI002 '
                                                                COLOR 7.
  ENDIF.

ENDFORM.                               " CREATE_BDC_SESSION5
*&---------------------------------------------------------------------*
*&      Form  CLOSE_PROCESS4
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CLOSE_PROCESS4.
  MOVE '2'            TO Z_ZCOPA_BSEG-STYPE." Obligatory
  MOVE 'ZCOPA_BSEG'        TO Z_ZCOPA_BSEG-TBNAM.
  MOVE 'Balance transf.TO PSoft' TO Z_ZCOPA_BSEG-SGTXT.

  CURRBAL = CURRBAL * ( - 1 ).
* take account and posting key
  IF CURRBAL > 0.
    MOVE  '40' TO Z_ZCOPA_BSEG-NEWBS.                       "Key
  ELSE.
    MOVE  '50' TO Z_ZCOPA_BSEG-NEWBS.                       "Key
  ENDIF.
  MOVE '0000199998' TO Z_ZCOPA_BSEG-HKONT.  "Account

  IF CURRBAL < 0.
    CURRBAL = CURRBAL * ( - 1 ).
  ENDIF.
  MOVE CURRBAL TO Z_ZCOPA_BSEG-WRBTR.  "Balance
        move z_bbseg to wa_record.
        move wa_record+31(2000) to wa_record+11(2020).

        TRANSFER wa_record   TO  PHYFILE LENGTH 2167."Approx size

*  TRANSFER Z_ZCOPA_BSEG    TO  PHYFILE LENGTH 2167."Approx size
  PERFORM INIT_STRUCTURES_SPECIAL USING 'ZCOPA_BSEG'. " I_ZCOPA_BSEG


ENDFORM.                               " CLOSE_PROCESS4
*&---------------------------------------------------------------------*
*&      Form  CHECK_AUTOMATIC_ASSIGN_UGL
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  CLOSE_PROCESS5
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CLOSE_PROCESS5.
  MOVE '2'            TO Z_BBSEG-STYPE." Obligatory
  MOVE 'BBSEG'        TO Z_BBSEG-TBNAM.
  MOVE 'Balance transf. into MNPP' TO Z_BBSEG-SGTXT.

  CURRBAL = CURRBAL * ( - 1 ).
* take account and posting key
  IF CURRBAL > 0.
    MOVE  '40' TO Z_BBSEG-NEWBS.                            "Key
  ELSE.
    MOVE  '50' TO Z_BBSEG-NEWBS.                            "Key
  ENDIF.
  MOVE '0000199998' TO Z_BBSEG-HKONT.  "Account

  IF CURRBAL < 0.
    CURRBAL = CURRBAL * ( - 1 ).
  ENDIF.
  MOVE CURRBAL TO Z_BBSEG-WRBTR.       "Balance
  TRANSFER Z_BBSEG    TO  PHYFILE LENGTH 2167."Approx size
  PERFORM INIT_STRUCTURES USING 'BBSEG'. " BBSEG

ENDFORM.                               " CLOSE_PROCESS5
