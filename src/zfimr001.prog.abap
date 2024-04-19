REPORT ZFIMR001  MESSAGE-ID AT LINE-SIZE 132 LINE-COUNT 65 NO STANDARD
PAGE HEADING.
************************************************************************
*
*  PROGRAM:    ZFIMR001
*  PROGRAMMER: Nesh N. Laurencic / Omnilogic Systems Group
*  CLIENT:     Union Gas
*  DATE:       April 1998.
*  This report shows balances in CGO and UGL.
*  This is just for merger , on Wilma's request.
************************************************************************
TABLES:  GLT0,    "G/L Account Master Record  Monthly Debits and Credits
         SKAT,    "G/L account master record (chart of accounts: descrip
         SKA1,    "G/L accounts master (chart of accounts)
         SKB1,    "G/L account master (company code)
         BSEG,                         "Accounting document segment
         T881.                         "FI-SL Ledger

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

DATA: BEGIN OF ITAB3 OCCURS 1000.
        INCLUDE STRUCTURE ITAB.
DATA: END OF ITAB3.

DATA: TOTAL LIKE ITAB-WRBTR,
      INDEXX LIKE SY-TABIX,
      GTOTCGO LIKE ITAB-WRBTR,
      GTOTUGL LIKE ITAB-WRBTR.
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME.

SELECT-OPTIONS :  SRACCT FOR GLT0-RACCT MATCHCODE OBJECT SAKO.
PARAMETERS:       PRYEAR LIKE GLT0-RYEAR DEFAULT '1998'.
SELECTION-SCREEN END OF BLOCK B1.


START-OF-SELECTION.
* Read data and load to itab ( Credits and Debts )
  SELECT * FROM GLT0 WHERE
                          RLDNR EQ '00' AND    "Ledger
                          RRCTY EQ '0'  AND    "Record type
                          RVERS EQ '001' AND   "Version
                          (
                          BUKRS EQ 'CGO' OR " Company code
                          BUKRS EQ 'UGL'   ) AND

                          RYEAR EQ PRYEAR AND  "Fiscal year
                          RACCT IN SRACCT. "  "Account number
*                         rtcur eq 'CAD'.
*    if glt0-rtcur = 'CAD'.

    IF GLT0-BUKRS = 'CGO'.
      MOVE-CORRESPONDING GLT0 TO ITAB.
      APPEND ITAB.
    ELSE.
      MOVE-CORRESPONDING GLT0 TO ITAB1.
      APPEND ITAB1.
    ENDIF.
  ENDSELECT.
  SORT: ITAB BY RACCT,
        ITAB1 BY RACCT.

  LOOP AT ITAB.

    AT END OF RACCT.
      SUM.
      MOVE ITAB TO ITAB2.              " Find balances
      ITAB2-WRBTR = ITAB2-HSLVT + ITAB2-HSL01 + ITAB2-HSL02 +
                    ITAB2-HSL03 + ITAB2-HSL04 + ITAB2-HSL05 +
                    ITAB2-HSL06 + ITAB2-HSL07 + ITAB2-HSL08 +
                    ITAB2-HSL09 + ITAB2-HSL10 + ITAB2-HSL11 +
                    ITAB2-HSL12.
* Check the status field
      SELECT SINGLE * FROM SKB1 WHERE
                                     BUKRS EQ 'CGO' AND
                                     SAKNR EQ ITAB2-RACCT.
      CHECK SY-SUBRC = 0.
      MOVE SKB1-FSTAG TO ITAB2-FSTAG.  "Status field
      MOVE SKB1-MITKZ TO ITAB2-MITKZ.  " A D K or I for automatic post.
* Check if CGO account is blocked
      IF SKB1-XSPEB = 'X'.
        ITAB2-BL2 = 'X'.               "CGO Blocked
      ENDIF.

* Check the tax code
      IF SKB1-MWSKZ EQ SPACE.
        ITAB2-MWSKZ = SPACE.
      ELSE.
        CASE SKB1-MWSKZ.
          WHEN '-'.
            ITAB2-MWSKZ = 'I0'.
          WHEN '*'.
            ITAB2-MWSKZ = 'I0'.
          WHEN '+'.
            ITAB2-MWSKZ = 'O0'.
          WHEN OTHERS.
            ITAB2-MWSKZ = SPACE.
        ENDCASE.

      ENDIF.
* Check the type of account
      SELECT SINGLE * FROM SKA1 WHERE
                                    KTOPL   = 'COAT' AND
                                    SAKNR EQ ITAB2-RACCT.
      CHECK SY-SUBRC = 0.
* If it's balance sheet account
      MOVE: SKA1-XBILK TO ITAB2-XBILK.
* Check if it's blocked on chart acc. level
      IF SKA1-XSPEB = 'X'.
        ITAB2-BL4 = 'X'.
      ENDIF.

*Description
      SELECT SINGLE * FROM SKAT WHERE
                                     SPRAS EQ 'E' AND
                                     KTOPL EQ 'COAT' AND
                                     SAKNR EQ ITAB2-RACCT.
      CHECK SY-SUBRC = 0.
      MOVE: SKAT-MCOD1 TO ITAB2-MCOD1.

      APPEND ITAB2.
      CLEAR ITAB2.
    ENDAT.
  ENDLOOP.

  LOOP AT ITAB1.

    AT END OF RACCT.
      SUM.
      MOVE ITAB1 TO ITAB3.             " Find balances
      ITAB3-WRBTR = ITAB3-HSLVT + ITAB3-HSL01 + ITAB3-HSL02 +
                    ITAB3-HSL03 + ITAB3-HSL04 + ITAB3-HSL05 +
                    ITAB3-HSL06 + ITAB3-HSL07 + ITAB3-HSL08 +
                    ITAB3-HSL09 + ITAB3-HSL10 + ITAB3-HSL11 +
                    ITAB3-HSL12.
* Check the status field
      SELECT SINGLE * FROM SKB1 WHERE
                                     BUKRS EQ 'UGL' AND
                                     SAKNR EQ ITAB3-RACCT.
      CHECK SY-SUBRC = 0.
      MOVE SKB1-FSTAG TO ITAB3-FSTAG.  "Status field
      MOVE SKB1-MITKZ TO ITAB3-MITKZ.  " A D K or I for automatic post.
* Check if CGO account is blocked
      IF SKB1-XSPEB = 'X'.
        ITAB3-BL3 = 'X'.               "UGL Blocked
      ENDIF.

* Check the tax code
      IF SKB1-MWSKZ EQ SPACE.
        ITAB3-MWSKZ = SPACE.
      ELSE.
        CASE SKB1-MWSKZ.
          WHEN '-'.
            ITAB3-MWSKZ = 'I0'.
          WHEN '*'.
            ITAB3-MWSKZ = 'I0'.
          WHEN '+'.
            ITAB3-MWSKZ = 'O0'.
          WHEN OTHERS.
            ITAB3-MWSKZ = SPACE.
        ENDCASE.

      ENDIF.
* Check the type of account
      SELECT SINGLE * FROM SKA1 WHERE
                                    KTOPL   = 'COAT' AND
                                    SAKNR EQ ITAB3-RACCT.
      CHECK SY-SUBRC = 0.
* If it's balance sheet account
      MOVE: SKA1-XBILK TO ITAB3-XBILK.
* Check if it's blocked on chart acc. level
      IF SKA1-XSPEB = 'X'.
        ITAB3-BL4 = 'X'.
      ENDIF.

*Description
      SELECT SINGLE * FROM SKAT WHERE
                                     SPRAS EQ 'E' AND
                                     KTOPL EQ 'COAT' AND
                                     SAKNR EQ ITAB3-RACCT.
      CHECK SY-SUBRC = 0.
      MOVE: SKAT-MCOD1 TO ITAB3-MCOD1.

      APPEND ITAB3.
      CLEAR ITAB3.
    ENDAT.
  ENDLOOP.
  CLEAR: GTOTUGL, GTOTCGO.
* First UGL
  LOOP AT ITAB3.
  GTOTUGL = GTOTUGL + ITAB3-WRBTR.
    WRITE: /1 ITAB3-MITKZ, ITAB3-RACCT UNDER TEXT-001,
            ITAB3-XBILK UNDER TEXT-BSA COLOR 7,
            ITAB3-WRBTR UNDER TEXT-002,
            ITAB3-MWSKZ UNDER TEXT-TAX.  "UGL
    IF ITAB3-BL3 EQ 'X'.
      WRITE:  ITAB3-BL3   UNDER TEXT-BL2."UGL blocked.
    ENDIF.
    IF ITAB3-BL4 EQ 'X'.
      WRITE:  ITAB3-BL4   UNDER TEXT-BL4. "CHART
    ENDIF.
    CLEAR ITAB2.
    READ TABLE ITAB2 WITH KEY RACCT = ITAB3-RACCT.
    IF SY-SUBRC = 0.
      MOVE SY-TABIX TO INDEXX.
      TOTAL = ITAB2-WRBTR + ITAB3-WRBTR.
      WRITE:  ITAB2-WRBTR UNDER TEXT-003.

      GTOTCGO = GTOTCGO + ITAB2-WRBTR.

      IF ITAB2-BL2 EQ 'X'.
        WRITE ITAB2-BL2   UNDER TEXT-BL3.                   "CGO
      ENDIF.
      IF ITAB2-BL4 EQ 'X'.
        WRITE ITAB2-BL4   UNDER TEXT-BL4.                   "CHART
      ENDIF.
      DELETE ITAB2 INDEX INDEXX.
    ELSE.
      WRITE: '            N/A' UNDER TEXT-003.
      TOTAL = ITAB3-WRBTR.                                  "UGL
    ENDIF.
    WRITE:         TOTAL UNDER TEXT-005, ITAB3-FSTAG UNDER TEXT-006,
                    ITAB3-MCOD1 UNDER TEXT-DAT.

    CLEAR: TOTAL, INDEXX.

  ENDLOOP.
  ULINE.
  WRITE: / TEXT-HE9 COLOR 1.
  SKIP.
  SORT ITAB2 BY RACCT.
  LOOP AT ITAB2.
  GTOTCGO = GTOTCGO + ITAB2-WRBTR.
    WRITE: /1 ITAB2-MITKZ, ITAB2-RACCT UNDER TEXT-001,
            ITAB2-XBILK UNDER TEXT-BSA COLOR 7,
            ITAB2-WRBTR UNDER TEXT-003,
            ITAB2-MWSKZ UNDER TEXT-TAX.
    IF ITAB2-BL2 EQ 'X'.
      WRITE ITAB2-BL2 UNDER TEXT-BL3.
    ENDIF.
    IF ITAB2-BL4 EQ 'X'.
      WRITE  ITAB2-BL4   UNDER TEXT-BL4.
    ENDIF.
    WRITE: '         N/A' UNDER TEXT-002.
    TOTAL = ITAB2-WRBTR.
    WRITE:         TOTAL UNDER TEXT-005, ITAB2-FSTAG UNDER TEXT-006,
                   ITAB2-MCOD1 UNDER TEXT-DAT.

  ENDLOOP.
  FORMAT COLOR 3.
  ULINE.
  WRITE: / TEXT-GTO,
           GTOTUGL UNDER TEXT-002,
           GTOTCGO UNDER TEXT-003.
  FORMAT RESET.
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
  WRITE: /3 TEXT-001, 10 TEXT-BSA,  14 TEXT-002, 32 TEXT-003,
          44 TEXT-004, 48 TEXT-005, 68 TEXT-006,
         73 TEXT-DAT, 104 TEXT-EXI, 108 TEXT-TAX, 116 TEXT-BL1.

* Second row
  WRITE: /116 TEXT-LIN,
           117 TEXT-BL2,
           121 TEXT-BL3, 127 TEXT-BL4.
  SKIP.
  FORMAT RESET.

END-OF-PAGE.
