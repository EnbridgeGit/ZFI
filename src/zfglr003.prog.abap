REPORT ZFGLR003 MESSAGE-ID ZZ NO STANDARD PAGE HEADING LINE-COUNT 65
       LINE-SIZE 110.
************************************************************************
*   Program: ZFGLR003.
*   Programmer: Dorothy Bialkowska.
*   Client: Centra - Union.
*   Date: 27 May, 1996.

*   The purpose of this report is to produce totals for specified
*   accounts.
************************************************************************
* 2005/08/26 mkhan TR105 Select Actual amount only as PLAN amount      *
*                     is going to be introduced in FI.                 *
*  *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *
*   modified by Nancy Gilligan, OmniLogic  98/10        D30K906113     *
*   - TOOK OUT HARD CODING FOR COMPANY NAME, STANDARDIZED HEADINGS,    *
*     SET COMPANY CODE BASED ON CLIENT, TOOK OUT HARD CODING FOR       *
*     MONTH NAMES (NOW SELECTING FROM T247).                           *
************************************************************************

TABLES:     GLT0, T001, SKAT, T247.

DATA: name1(20)          TYPE C,
      name2              LIKE name1,
      name               LIKE name1,
      total1             TYPE P DECIMALS 2,
      total2             LIKE total1,
      total3             LIKE total1,
      total4             LIKE total1,
      total5             LIKE total1,
      total6             LIKE total1,
      total7             LIKE total1,
      total8             LIKE total1,
      total9             LIKE total1,
      total10            LIKE total1,
      total11            LIKE total1,
      total12            LIKE total1,
************************************************************************
* tota11, total2, total3, ... total12 are used to calculate values
* stored in GLTO-TSL01, GLTO-TSL02, GLTO-TSLO3, ... GLTO-TSL12 for
* a given account number in a given fiscal year.
************************************************************************
      bigtot1            LIKE total1,
      bigtot2            LIKE total1,
      bigtot3            LIKE total1,
      bigtot4            LIKE total1,
      bigtot5            LIKE total1,
      bigtot6            LIKE total1,
      bigtot7            LIKE total1,
      bigtot8            LIKE total1,
      bigtot9            LIKE total1,
      bigtot10           LIKE total1,
      bigtot11           LIKE total1,
      bigtot12           LIKE total1,
************************************************************************
* for a given account in a given fiscal year, bigtot1, bigtot2,...
* bigtot12 store total values from the begining of the year to the end
* of period 1, 2, ...12.
************************************************************************
      ctotal            LIKE total1,
      ptotal            LIKE total1,
      mtotal            LIKE total1,
      string            LIKE SKAT-txt50,
      index             TYPE I,
      counter           TYPE I,
      value1            LIKE total1,
      value2            LIKE total1,
      value             LIKE total1,
      pvalue            LIKE total1,
      mvalue            LIKE total1,
      rvalue            LIKE total1,
      dummy             LIKE total1,
      phony             LIKE total1,

      BEGIN OF mytable OCCURS 12,
            myfield     LIKE GLT0-TSL01,
      END OF mytable,

      BEGIN OF worktab OCCURS 20,
            RACCT       LIKE GLT0-RACCT,
            RYEAR       LIKE GLT0-RYEAR,
            TSL01       LIKE GLT0-TSL01,
            TSL02       LIKE GLT0-TSL02,
            TSL03       LIKE GLT0-TSL03,
            TSL04       LIKE GLT0-TSL04,
            TSL05       LIKE GLT0-TSL05,
            TSL06       LIKE GLT0-TSL06,
            TSL07       LIKE GLT0-TSL07,
            TSL08       LIKE GLT0-TSL08,
            TSL09       LIKE GLT0-TSL09,
            TSL10       LIKE GLT0-TSL10,
            TSL11       LIKE GLT0-TSL11,
            TSL12       LIKE GLT0-TSL12,
            text        LIKE SKAT-txt50,
      END OF worktab,
************************************************************************
* Internal table WORKTAB will ALWAYS store data for YEAR2.
************************************************************************

      BEGIN OF pworktab OCCURS 20.
            INCLUDE STRUCTURE worktab.
DATA: END OF pworktab,
************************************************************************
* Internal table PWORKTAB will ALWAYS store data for YEAR1.
************************************************************************

      BEGIN OF mworktab OCCURS 20.
            INCLUDE STRUCTURE worktab.
DATA: END OF mworktab,
************************************************************************
* Internal table MWORKTAB will be built only if YEAR2 <> YEAR1 and
* YEAR1 <> YEAR3.  This table will store data for year immiedately
* after YEAR2 (=YEAR3).
************************************************************************

      BEGIN OF rworktab OCCURS 20.
            INCLUDE STRUCTURE worktab.
DATA: END OF rworktab,
************************************************************************
* Internal table RWORKTAB will be built only if YEAR2 <> YEAR1 and
* YEAR1 <> YEAR3.  This table will store data for years in between
* YEAR1 and YEAR3 (YEAR1 and YEAR3 NOT INCLUDED).
************************************************************************

      BEGIN OF phonytab OCCURS 20.
            INCLUDE STRUCTURE worktab.
DATA: END OF phonytab.
************************************************************************
* This table is intoduced to simplify calculation.
************************************************************************

SELECTION-SCREEN BEGIN OF BLOCK square WITH FRAME TITLE TEXT-013.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF LINE.
   SELECTION-SCREEN COMMENT 15(37) TEXT-001.
   PARAMETERS: month1(2)    TYPE N DEFAULT 01.
   SELECTION-SCREEN COMMENT 58(12) TEXT-003.
   PARAMETERS: year1(4)     TYPE N DEFAULT SY-DATUM+0(4).
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
   SELECTION-SCREEN COMMENT 15(37) TEXT-002.
   PARAMETERS: month2(2)    TYPE N DEFAULT SY-DATUM+4(2).
   SELECTION-SCREEN COMMENT 58(12) TEXT-003.
   PARAMETERS: year2(4)     TYPE N DEFAULT SY-DATUM+0(4).
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK square.
SELECTION-SCREEN SKIP 1.

DATA: year3       LIKE      year1,
      start       LIKE      year1,
      end         LIKE      year1,
      month       LIKE      month1.

PERFORM KeepControl.
year3 = year2 - 1.
PERFORM SetRange.

SELECTION-SCREEN BEGIN OF BLOCK square2 WITH FRAME TITLE TEXT-015.
SELECTION-SCREEN BEGIN OF LINE.
   SELECTION-SCREEN COMMENT 15(37) TEXT-016.
   PARAMETERS: ccode(4)    TYPE C.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK square2.
SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK square1 WITH FRAME TITLE TEXT-014.
SELECTION-SCREEN SKIP 1.
SELECT-OPTIONS:
       Group1 FOR GLT0-RACCT NO INTERVALS,
       Group2 FOR GLT0-RACCT NO INTERVALS,
       Group3 FOR GLT0-RACCT NO INTERVALS,
       Group4 FOR GLT0-RACCT NO INTERVALS,
       Group5 FOR GLT0-RACCT NO INTERVALS,
       Group6 FOR GLT0-RACCT NO INTERVALS,
       Group7 FOR GLT0-RACCT NO INTERVALS,
       Group8 FOR GLT0-RACCT NO INTERVALS,
       Group9 FOR GLT0-RACCT NO INTERVALS.
SELECTION-SCREEN END OF BLOCK square1.

*AT SELECTION SCREEN
AT SELECTION-SCREEN.
PERFORM TrapErrors.

* INITIALIZATION
INITIALIZATION.

* GET COMPANY CODE BASED ON CLIENT                       NGILLIGAN 98/10
IF SY-MANDT+2(1) = '1'.                                 "NGILLIGAN 98/10
   CCODE = 'UEC'.                                       "NGILLIGAN 98/10
ELSEIF SY-MANDT+2(1) = '0'.                             "NGILLIGAN 98/10
   CCODE = 'UGL'.                                       "NGILLIGAN 98/10
ENDIF.                                                  "NGILLIGAN 98/10


* TOP OF PAGE
TOP-OF-PAGE.
  WRITE: /1 TEXT-RPT, SY-REPID INTENSIFIED ON,          "ngilligan 98/10
         37 T001-BUTXT INTENSIFIED ON,                  "ngilligan 98/10
         80 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.     "ngilligan 98/10

  WRITE: /1 TEXT-CLT, SY-MANDT UNDER SY-REPID,          "ngilligan 98/10
                                        SY-SYSID.       "ngilligan 98/10
  WRITE: TEXT-018 UNDER T001-BUTXT.                     "ngilligan 98/10
  WRITE: TEXT-PGE UNDER TEXT-DTE, SY-PAGNO.             "ngilligan 98/10
       SKIP 1.                                          "ngilligan 98/10
  ULINE.                                                "ngilligan 98/10
       SKIP 1.                                          "ngilligan 98/10


*  FORMAT INTENSIFIED OFF.
*  WRITE: / TEXT-017, SY-DATUM, 95 TEXT-019, SY-PAGNO.
*  WRITE: / SY-UZEIT UNDER SY-DATUM, SY-REPID UNDER TEXT-019.
*  WRITE: /47 TEXT-018.
*  FORMAT INTENSIFIED ON.

START-OF-SELECTION.
* get company code text
 select single butxt from t001 into t001-butxt where bukrs = ccode.

PERFORM DisplayInfo.
************************************************************************
* For accunts in GROUP1:
************************************************************************

DESCRIBE TABLE Group1 LINES counter.
IF counter > 0.
   IF ( counter NE 1 ) OR ( Group1-LOW NE 0 ).
      PERFORM DisplayTitle USING TEXT-004.
      PERFORM GetSnapData TABLES Group1.
      PERFORM SortAllTab.
      PERFORM ShowBody.
      PERFORM DisplayFoot.
   ENDIF.
ENDIF.
************************************************************************
* For accounts in GROUP2:
************************************************************************

DESCRIBE TABLE Group2 LINES counter.
IF counter > 0.
   IF ( counter NE 1 ) OR ( Group2-LOW NE 0 ).
      PERFORM FreshStart.
      PERFORM DisplayTitle USING TEXT-005.
      PERFORM GetSheetData TABLES Group2.
      PERFORM SortTab.
      PERFORM BuildBody.
      PERFORM DisplayFoot.
   ENDIF.
ENDIF.

************************************************************************
* For accunts in GROUP3:
************************************************************************
DESCRIBE TABLE Group3 LINES counter.
IF counter > 0.
   IF ( counter NE 1 ) OR ( Group3-LOW NE 0 ).
      PERFORM DisplayTitle USING TEXT-006.
      PERFORM GetSnapData TABLES Group3.
      PERFORM SortAllTab.
      PERFORM ShowBody.
      PERFORM DisplayFoot.
   ENDIF.
ENDIF.

************************************************************************
* For accounts in GROUP4:
************************************************************************
DESCRIBE TABLE Group4 LINES counter.
IF counter > 0.
   IF ( counter NE 1 ) OR ( Group4-LOW NE 0 ).
      PERFORM FreshStart.
      PERFORM FreshStart.
      PERFORM DisplayTitle USING TEXT-007.
      PERFORM GetSheetData TABLES Group4.
      PERFORM SortTab.
      PERFORM BuildBody.
      PERFORM DisplayFoot.
   ENDIF.
ENDIF.

************************************************************************
* For accunts in GROUP5:
************************************************************************
DESCRIBE TABLE Group5 LINES counter.
IF counter > 0.
   IF ( counter NE 1 ) OR ( Group5-LOW NE 0 ).
      PERFORM FreshStart.
      PERFORM DisplayTitle USING TEXT-008.
      PERFORM GetSnapData TABLES Group5.
      PERFORM SortAllTab.
      PERFORM ShowBody.
      PERFORM DisplayFoot.
   ENDIF.
ENDIF.

************************************************************************
* For accounts in GROUP6:
************************************************************************
DESCRIBE TABLE Group6 LINES counter.
IF counter > 0.
   IF ( counter NE 1 ) OR ( Group6-LOW NE 0 ).
      PERFORM FreshStart.
      PERFORM DisplayTitle USING TEXT-009.
      PERFORM GetSheetData TABLES Group6.
      PERFORM SortTab.
      PERFORM BuildBody.
      PERFORM DisplayFoot.
   ENDIF.
ENDIF.

************************************************************************
* For accounts in GROUP7:
************************************************************************
DESCRIBE TABLE Group7 LINES counter.
IF counter > 0.
   IF ( counter NE 1 ) OR ( Group7-LOW NE 0 ).
      PERFORM FreshStart.
      PERFORM DisplayTitle USING TEXT-010.
      PERFORM GetSheetData TABLES Group7.
      PERFORM SortTab.
      PERFORM BuildBody.
      PERFORM DisplayFoot.
   ENDIF.
ENDIF.

************************************************************************
* For accounts in GROUP8:
************************************************************************
DESCRIBE TABLE Group8 LINES counter.
IF counter > 0.
   IF ( counter NE 1 ) OR ( Group8-LOW NE 0 ).
      PERFORM FreshStart.
      PERFORM DisplayTitle USING TEXT-011.
      PERFORM GetSheetData TABLES Group8.
      PERFORM SortTab.
      PERFORM BuildBody.
      PERFORM DisplayFoot.
   ENDIF.
ENDIF.

************************************************************************
* For accounts in GROUP9:
************************************************************************
DESCRIBE TABLE Group9 LINES counter.
IF counter > 0.
   IF ( counter NE 1 ) OR ( Group9-LOW NE 0 ).
      PERFORM FreshStart.
      PERFORM DisplayTitle USING TEXT-012.
      PERFORM GetSheetData TABLES Group9.
      PERFORM SortTab.
      PERFORM BuildBody.
      PERFORM DisplayFoot.
   ENDIF.
ENDIF.

************************************************************************
* Listed belowe are subrutines used by a program.
************************************************************************

FORM InitializeValues.
     CLEAR: total1, total2, total3, total4, total5, total6, total7,
            total8, total9, total9, total9, total10, total11, total12,
            bigtot1, bigtot2, bigtot3, bigtot4, bigtot5, bigtot6,
            bigtot7, bigtot8, bigtot9, bigtot10, bigtot11, bigtot12.
ENDFORM.

FORM CalcTotals USING phonytab-TSL01 phonytab-TSL02 phonytab-TSL03
     phonytab-TSL04 phonytab-TSL05 phonytab-TSL06  phonytab-TSL07
     phonytab-TSL08 phonytab-TSL09 phonytab-TSL010 phonytab-TSL11
     phonytab-TSL12.
     total1 = phonytab-TSL01.
     total2 = phonytab-TSL02.
     total3 = phonytab-TSL03.
     total4 = phonytab-TSL04.
     total5 = phonytab-TSL05.
     total6 = phonytab-TSL06.
     total7 = phonytab-TSL07.
     total8 = phonytab-TSL08.
     total9 = phonytab-TSL09.
     total10 = phonytab-TSL10.
     total11 = phonytab-TSL11.
     total12 = phonytab-TSL12.
ENDFORM.

FORM CalcPTotals USING phony.
     phony = total1 + total2 + total3 + total4 + total5 + total6
           + total7 + total8 + total9 + total10 + total11 + total12.
ENDFORM.

FORM CalcBigTotals.
     bigtot1 = total1.
     bigtot2 = bigtot1 + total2.
     bigtot3 = bigtot2 + total3.
     bigtot4 = bigtot3 + total4.
     bigtot5 = bigtot4 + total5.
     bigtot6 = bigtot5 + total6.
     bigtot7 = bigtot6 + total7.
     bigtot8 = bigtot7 + total8.
     bigtot9 = bigtot8 + total9.
     bigtot10 = bigtot9 + total10.
     bigtot11 = bigtot10 + total11.
     bigtot12 = bigtot11 + total12.
ENDFORM.

FORM PopulateTab.
     mytable-myfield = bigtot1.
     APPEND mytable.
     CLEAR mytable.
     mytable-myfield = bigtot2.
     APPEND mytable.
     CLEAR mytable.
     mytable-myfield = bigtot3.
     APPEND mytable.
     CLEAR mytable.
     mytable-myfield = bigtot4.
     APPEND mytable.
     CLEAR mytable.
     mytable-myfield = bigtot5.
     APPEND mytable.
     CLEAR mytable.
     mytable-myfield = bigtot6.
     APPEND mytable.
     CLEAR mytable.
     mytable-myfield = bigtot7.
     APPEND mytable.
     CLEAR mytable.
     mytable-myfield = bigtot8.
     APPEND mytable.
     CLEAR mytable.
     mytable-myfield = bigtot9.
     APPEND mytable.
     CLEAR mytable.
     mytable-myfield = bigtot10.
     APPEND mytable.
     CLEAR mytable.
     mytable-myfield = bigtot11.
     APPEND mytable.
     CLEAR mytable.
     mytable-myfield = bigtot12.
     APPEND mytable.
     CLEAR mytable.
ENDFORM.

FORM NameMonth USING month name.
* GET MONTH NAMES FROM TABLE T247                        NGILLIGAN 98/10
     SELECT SINGLE LTX FROM T247 INTO NAME WHERE SPRAS = SY-LANGU
                                             AND MNR   = MONTH.
ENDFORM.

FORM DisplayHeadings.
     ULINE.
     WRITE: / 'Account', 20 'Description', 69 'Current Value',
               94 'History Value'.
     ULINE.
ENDFORM.

FORM DisplayInfo.
     PERFORM NameMonth USING month1 name1.
     PERFORM NameMonth USING month2 name2.

     CONCATENATE name1 year1  INTO name1 SEPARATED BY SPACE.
     CONCATENATE name2 year2  INTO name2 SEPARATED BY SPACE.

     FORMAT INTENSIFIED ON.
     WRITE: / 'CONTROL PERIOD'.
     FORMAT INTENSIFIED OFF.
     WRITE: /16 'From the FIRST day of:'.
     FORMAT INTENSIFIED ON.
     WRITE: name1.
     FORMAT INTENSIFIED OFF.
     WRITE: /16 'to the LAST day of:'.
     FORMAT INTENSIFIED ON.
     WRITE: name2 UNDER name1.
     FORMAT INTENSIFIED OFF.
ENDFORM.

FORM DisplayFoot.
     SKIP 1.
     ULINE AT 60.
     write: /60 'Total:'.
     FORMAT INTENSIFIED ON.
     WRITE: 65 ctotal DECIMALS 2, 90 ptotal DECIMALS 2.
     FORMAT INTENSIFIED OFF.
ENDFORM.

FORM GrabValue1.
     index = month2.
     READ TABLE mytable INDEX index.
     value1 = mytable-myfield.
ENDFORM.

FORM GrabValue CHANGING dummy.
     index = month1 - 1.
     READ TABLE mytable INDEX index.
     dummy = mytable-myfield.
ENDFORM.

FORM Housekeeping.
     SUM.
     PERFORM CalcTotals USING worktab-TSL01 worktab-TSL02 worktab-TSL03
             worktab-TSL04 worktab-TSL05 worktab-TSL06 worktab-TSL07
             worktab-TSL08 worktab-TSL09 worktab-TSL10 worktab-TSL11
             worktab-TSL12.
     PERFORM CalcBigTotals.
     PERFORM PopulateTab.
ENDFORM.

FORM GetValue1.
     PERFORM Housekeeping.
     PERFORM GrabValue1.
ENDFORM.

FORM DisplayTitle USING string.
     SKIP 2.
     FORMAT INTENSIFIED ON.
     WRITE: / string.
     FORMAT INTENSIFIED OFF.
     PERFORM DisplayHeadings.
ENDFORM.

FORM PrintHistoryValue USING dummy.
     IF SY-SUBRC > 0.
        write: 90 '          N/A'.
     ELSE.
        write: 90 dummy DECIMALS 2.
     ENDIF.
ENDFORM.

FORM GrabDescription.
     SELECT * FROM T001
            WHERE BUKRS = GLT0-BUKRS.

            SELECT * FROM SKAT
                   WHERE KTOPL = T001-KTOPL
                   AND SAKNR = GLT0-RACCT
                   AND SPRAS = SY-LANGU.
                   IF GLT0-RYEAR = year2.
                      MOVE SKAT-TXT50 TO worktab-text.
                      APPEND worktab.
                    ENDIF.
             ENDSELECT.
     ENDSELECT.
ENDFORM.

FORM TrapErrors.
     DATA: date1      LIKE SY-DATUM,
           date2      LIKE SY-DATUM.

     IF year2 < year1.                        " error message will be
        MESSAGE E006.                         " displayed, if user
     ENDIF.                                   " enters incorrect years.

     IF ( month1 = 0 OR month1 > 12 ) OR ( month2 = 0 OR month2 > 12 ).
         MESSAGE E009.
     ENDIF.

     date1+2(2) = month1.
     date1+4(4) = year1.
     date2+2(2) = month2.
     date2+4(4) = year2.

     IF date1 > date2.
        MESSAGE E006.
     ENDIF.

ENDFORM.

FORM Build2Tables.
     CLEAR: worktab, pworktab.
     IF GLT0-RYEAR = year2.
        MOVE-CORRESPONDING GLT0 TO worktab.
     ELSE.
        MOVE-CORRESPONDING GLT0 TO pworktab.
        APPEND pworktab.
     ENDIF.
     PERFORM GrabDescription.
ENDFORM.

FORM Build4Tables.
     CLEAR: worktab, pworktab, mworktab, rworktab.
     IF GLT0-RYEAR = year2.
        MOVE-CORRESPONDING GLT0 TO worktab.
     ELSEIF GLT0-RYEAR = year3.
        MOVE-CORRESPONDING GLT0 TO mworktab.
        APPEND mworktab.
      ELSEIF GLT0-RYEAR = year1.
         MOVE-CORRESPONDING GLT0 TO pworktab.
         APPEND pworktab.
      ELSE.
         MOVE-CORRESPONDING GLT0 TO rworktab.
         APPEND rworktab.
      ENDIF.
      PERFORM GrabDescription.
ENDFORM.

FORM NextStep CHANGING dummy.
     IF month1 = '01'.
        dummy = 0.
     ELSE.
        PERFORM GrabValue CHANGING value2.
     ENDIF.
ENDFORM.

FORM KeepControl.
*    Because data is shown always >>to the end of the month prior to<<,
*    this is to ensure that >>January<< will be a valid entry.
     IF ( year1 <> year2 ) AND ( month2 = '01' ).
        month2 = '12'.
        year2 = year2 - 1.
     ELSE.
        month2 = month2 - 1.
     ENDIF.
ENDFORM.

FORM SetRange.
     IF year1 = year2.
        start = year3.
        end = year2.
     ELSE.
        start = year1.
        end = year2.
     ENDIF.
ENDFORM.

FORM FreshStart.
     CLEAR: value, ptotal, ctotal.
     PERFORM InitializeValues.
     CLEAR: mytable, worktab, pworktab.
     REFRESH: mytable, worktab, pworktab.
ENDFORM.

FORM SortTab.
     SORT: worktab BY RACCT ASCENDING.
     SORT: pworktab BY RACCT ASCENDING.
ENDFORM.

FORM SortAllTab.
     PERFORM SortTab.
     SORT: mworktab BY RACCT ASCENDING.
     SORT: rworktab BY RACCT ASCENDING.
ENDFORM.

FORM GetPvalue.
     PERFORM CalcTotals USING pworktab-TSL01 pworktab-TSL02
             pworktab-TSL03 pworktab-TSL04 pworktab-TSL05 pworktab-TSL06
             pworktab-TSL07 pworktab-TSL08 pworktab-TSL09 pworktab-TSL10
             pworktab-TSL11 pworktab-TSL12.
     PERFORM CalcPTotals USING pvalue.
ENDFORM.

FORM GetSnapData TABLES Group.
     SELECT * FROM GLT0
       WHERE RRCTY = '0'                                   "TR105
       AND   BUKRS = ccode
       AND   RYEAR BETWEEN start AND end
       AND   RACCT IN Group.
       IF  year1 = year2 OR ( year1 <> year2 AND year1 = year3 ).
          "(when start and end of the range happen within the same year,
          " or when the range spans over two consecutive years).
          PERFORM Build2Tables.
       ELSE.
          "(when year1 and year2 are not the same, and there is MORE
          " THEN ONE year gap between them).
          PERFORM Build4Tables.
       ENDIF.
     ENDSELECT.

************************************************************************
* At this point two OR four internal tables are built:
* - WORKTAB stores info. about "current" year (e.i. year2).
* - PWORKTAB stores info. for the entire year1.
* - MWORKTAB - for year3, if year3 is different from year1
* - RWORKTAB - for all years between year3 and year1.
************************************************************************
ENDFORM.

FORM GetSheetData TABLES group.
     SELECT * FROM GLT0
      WHERE RRCTY = '0'                      "TR105
        AND BUKRS = ccode
        AND RYEAR <= end
        AND RACCT IN  Group.
         PERFORM Build2Tables.
     ENDSELECT.
ENDFORM.

FORM BuildBody.
     LOOP AT worktab.
      MOVE worktab-text TO string.
      AT END OF RACCT.
         PERFORM GetValue1.
         value = value1.
         write: / worktab-RACCT, 20 STRING.
         REFRESH mytable.
         PERFORM InitializeValues.
         LOOP AT pworktab WHERE RACCT = worktab-racct.
              AT END OF RACCT.
                 SUM.
                 PERFORM GetPvalue.
             ENDAT.
         ENDLOOP.
         PERFORM PrintHistoryValue USING pvalue.
         ptotal = ptotal + pvalue.
         value = value + pvalue.
         ctotal = ctotal + value.
         write: 65 value DECIMALS 2.
         PERFORM InitializeValues.
         CLEAR: pvalue.
     ENDAT.
    ENDLOOP.
ENDFORM.

FORM ShowBody.
    IF year1 = year2.
       PERFORM FindTotals1.
    ENDIF.
    IF year2 <> year1 AND year1 = year3.
       PERFORM FindTotals2.
    ENDIF.
    IF ( year2 <> year1 ) AND ( year1 <> year3 ).
       PERFORM FindTotals3.
   ENDIF.
ENDFORM.

FORM FindTotals1.
     LOOP AT worktab.
          MOVE worktab-text TO string.
          AT END OF RACCT.
             PERFORM GetValue1.
             PERFORM NextStep CHANGING value2.
             value = value1 - value2.
             ctotal = ctotal + value.
             write: / worktab-RACCT, 20 string, 65 value DECIMALS 2.
             REFRESH mytable.
************************************************************************
             LOOP AT pworktab WHERE RACCT = worktab-RACCT.
                  AT END OF RACCT.
                     SUM.
                     PERFORM GetPvalue.
                  ENDAT.
             ENDLOOP.
             ptotal = ptotal + pvalue.
             PERFORM PrintHistoryValue USING pvalue.
             CLEAR pvalue.
************************************************************************
           ENDAT.
    ENDLOOP.
ENDFORM.

FORM FindTotals2.
   LOOP AT worktab.
      MOVE worktab-text TO string.
      AT END OF RACCT.
         PERFORM GetValue1.
         CLEAR mytable.
         REFRESH mytable.
         write: / worktab-RACCT, 20 string.
************************************************************************
         LOOP AT pworktab WHERE RACCT = worktab-RACCT.
              AT END OF RACCT.
                 SUM.
                 PERFORM GetPvalue.
                 PERFORM CalcBigTotals.
                 PERFORM PopulateTab.
                 PERFORM NEXTSTEP Changing mvalue.
              ENDAT.
          ENDLOOP.
          ptotal = ptotal + pvalue.
          IF SY-SUBRC > 0.
             value = value1.
             write: 65 value DECIMALS 2, 90 '          N/A'.
          ELSE.
             value2 = pvalue - mvalue.
             value = value1 + value2.
             write: 65 value DECIMALS 2, 90 pvalue DECIMALS 2.
          ENDIF.
          ctotal = ctotal + value.
          PERFORM InitializeValues.
          CLEAR: pvalue.
          CLEAR mytable.
          REFRESH MYTABLE.
************************************************************************
     ENDAT.
   ENDLOOP.
ENDFORM.

FORM FindTotals3.
   LOOP AT worktab.
      MOVE worktab-text TO string.
      AT END OF RACCT.
         WRITE: / worktab-RACCT, 20 string.
         PERFORM GetValue1.
         CLEAR mytable.
         REFRESH mytable.
************************************************************************
         LOOP AT mworktab WHERE RACCT = worktab-RACCT.
              AT END OF RACCT.
                 SUM.
                 PERFORM CalcTotals USING mworktab-TSL01 mworktab-TSL02
                         mworktab-TSL03 mworktab-TSL04 mworktab-TSL05
                         mworktab-TSL06 mworktab-TSL07 mworktab-TSL08
                         mworktab-TSL09 mworktab-TSL10 mworktab-TSL11
                         mworktab-TSL12.
                 PERFORM CalcPTotals USING mvalue.
              ENDAT.
          ENDLOOP.
          mtotal = mtotal + mvalue.
          PERFORM PrintHistoryValue USING mvalue.
************************************************************************
         LOOP AT rworktab WHERE RACCT = mworktab-RACCT.
              AT END OF RACCT.
                 SUM.
                 PERFORM CalcTotals USING rworktab-TSL01 rworktab-TSL02
                         rworktab-TSL03 rworktab-TSL04 rworktab-TSL05
                         rworktab-TSL06 rworktab-TSL07 rworktab-TSL08
                         rworktab-TSL09 rworktab-TSL10 rworktab-TSL11
                         rworktab-TSL12.
                 PERFORM CalcPTotals USING rvalue.
              ENDAT.
          ENDLOOP.
          rvalue = rvalue + mvalue.
          IF SY-SUBRC > 0.
             write: 83 '*'.
          ENDIF.
************************************************************************
         LOOP AT pworktab WHERE RACCT = rworktab-RACCT.
              AT END OF RACCT.
                 SUM.
                 PERFORM GetPvalue.
                 PERFORM CalcBigTotals.
                 PERFORM PopulateTab.
                 PERFORM NextStep CHANGING value2.
              ENDAT.
          ENDLOOP.
          pvalue = pvalue - value2.

          value = value1 + rvalue + pvalue.
          ctotal = ctotal + value.
          IF SY-SUBRC > 0.
              WRITE: 65 value DECIMALS 2, 83 '*'.
          ELSE.
              WRITE: 65 value DECIMALS 2.
          ENDIF.

          PERFORM InitializeValues.
          CLEAR: value, mvalue, rvalue, pvalue.
          REFRESH MYTABLE.
************************************************************************
     ENDAT.
   ENDLOOP.
   mtotal = ptotal.           "this is done only for a sake of display
ENDFORM.
