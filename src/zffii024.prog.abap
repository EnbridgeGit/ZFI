REPORT ZFFII024 MESSAGE-ID ZS.
************************************************************************
* 2005/04/06 mdemeest #--- S&T file - Checks the posting status of a
*                                     BDC  session.
*
************************************************************************
TABLES: APQI.              "BDC session status

*------------------------  Selection Screen  ---------------------------
* Input File
SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-001.
SELECTION-SCREEN END OF BLOCK BOX.
SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME TITLE TEXT-003.
SELECT-OPTIONS:
           S_BDC       FOR APQI-GROUPID  NO INTERVALS OBLIGATORY
                                      DEFAULT 'ZFI',
           S_CREDT     FOR APQI-CREDATE.
SELECTION-SCREEN END OF BLOCK BOX3.
*-------------------------  End of Input Screen  -----------------------


DATA:  WA_NUM TYPE I.

    SELECT * FROM APQI
        WHERE GROUPID IN S_BDC
          AND CREDATE IN S_CREDT
          AND QSTATE <> 'F'.
        IF  SY-SUBRC = '0'.
            MOVE 'X'  TO WA_NUM.
        ENDIF.
    ENDSELECT.

    WRITE: /1 'sy-subrc=', SY-SUBRC.
