*&---------------------------------------------------------------------*
*& Report  ZCHECK_SKB1_NEW                                             *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  ZCHECK_SKB1_NEW.

************************************************************************
* SELECTION-SCREEN
************************************************************************

SELECTION-SCREEN BEGIN OF BLOCK params WITH FRAME TITLE progname.

  SELECTION-SCREEN SKIP 1.

  SELECTION-SCREEN BEGIN OF BLOCK descr WITH FRAME TITLE descr.
    SELECTION-SCREEN COMMENT /1(75) descr1.
    SELECTION-SCREEN COMMENT /1(75) descr2.
    SELECTION-SCREEN SKIP 1.
    SELECTION-SCREEN COMMENT /1(75) descr3.
    SELECTION-SCREEN SKIP 1.
    SELECTION-SCREEN COMMENT /1(75) descr4.
    SELECTION-SCREEN COMMENT /1(75) descr5.
    SELECTION-SCREEN COMMENT /1(75) descr6.
  SELECTION-SCREEN END OF BLOCK descr.

  SELECTION-SCREEN SKIP 1.

  SELECTION-SCREEN COMMENT /1(75) cbukrs.
  PARAMETERS: p_bukrs TYPE t093c-bukrs OBLIGATORY VALUE CHECK.

  SELECTION-SCREEN SKIP 1.

  SELECTION-SCREEN BEGIN OF BLOCK optns WITH FRAME TITLE optns.
    SELECTION-SCREEN BEGIN OF LINE.
      PARAMETERS: p_detail AS CHECKBOX DEFAULT 'X'.
      SELECTION-SCREEN COMMENT 4(75) cdetail.
    SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN END OF BLOCK optns.
SELECTION-SCREEN END OF BLOCK params.

************************************************************************
* INITIALIZATION
************************************************************************
INITIALIZATION.

  progname = 'ZCHECK_SKB1_NEW'.
  descr    = 'Description'.
  optns    = 'Options'.
  descr1 = 'For All G/L accounts affected by depreciation areas that post directly to'.
  descr2 = 'the General Ledger, the tax code has NOT to be a required field.'.
  descr3 = 'This report checks field XMWNO for all affected G/L accounts.'.
  descr4 = 'If some accounts require a tax code even when they are posted by '.
  descr5 = 'depreciation areas that do direct posting, they are displayed in an'.
  descr6 = 'error table.'.
  cbukrs = 'Enter company code below:'.
  cdetail = 'Display a detailed log'.

************************************************************************
* START-OF-SELECTION
************************************************************************
START-OF-SELECTION.


****************************
* DEFINITION nothing_found *
****************************
DEFINE nothing_found.
  WRITE 4 'Nothing found' COLOR COL_POSITIVE INVERSE.
END-OF-DEFINITION.


***************
* DECLARATION *
***************
TYPES:  BEGIN OF t095_key,
          ktopl TYPE t001-ktopl,
          ktogr TYPE ankb-ktogr,
          afabe TYPE ankb-afabe,
        END OF t095_key,

        BEGIN OF kto_wrong,
          ktopl   TYPE t001-ktopl,
          ktogr   TYPE ankb-ktogr,
          afabe   TYPE t095-afabe,
          saknr   TYPE skb1-saknr,
          xmwno   TYPE skb1-xmwno,
          fldname TYPE c LENGTH 8,
          flddesc TYPE c LENGTH 60,
        END OF kto_wrong.

DATA: ld_afapl TYPE t093c-afapl,
      ld_ktopl TYPE t001-ktopl,
      ld_ktonr TYPE skb1-saknr,
      ld_index LIKE sy-index,
      ld_linno LIKE sy-linno,
      ld_count TYPE i.

DATA: lt_t093 TYPE TABLE OF t093,
      ls_t093 LIKE LINE OF lt_t093.

DATA: lt_ankb TYPE TABLE OF ankb,
      ls_ankb LIKE LINE OF lt_ankb,
      lt_anka TYPE TABLE OF anka,
      ls_anka LIKE LINE OF lt_anka,
      ls_ankp TYPE ankp.

DATA: lt_t095_key TYPE TABLE OF t095_key,
      ls_t095_key LIKE LINE OF lt_t095_key.

DATA: lt_t095 TYPE TABLE OF t095,
      ls_t095 LIKE LINE OF lt_t095.

DATA: lt_kto_wrong TYPE TABLE OF kto_wrong,
      ls_kto_wrong LIKE LINE OF lt_kto_wrong.

DATA: ls_skb1 TYPE skb1.

" for technical information about fields of T095
DATA: lt_dfies   TYPE TABLE OF dfies,
      ls_dfies   LIKE LINE OF lt_dfies.

************
* GET DATA *
************

* get chart of accounts for selected company code
SELECT SINGLE ktopl FROM t001
                    INTO ld_ktopl
                    WHERE bukrs EQ p_bukrs.

* get chart of depreciation for selected company code
SELECT SINGLE afapl FROM t093c
                    INTO ld_afapl
                    WHERE bukrs EQ p_bukrs.

* get all depreciation areas that post directly to G/L
SELECT * FROM t093 INTO CORRESPONDING FIELDS OF ls_t093
                   WHERE afapl EQ ld_afapl
                   AND ( buhbkt EQ '4'
                         OR buhbkt EQ '6' ).
* If account assignment is supplied from another deprediation area,
*  get that depreciation area instead.
  IF ls_t093-abwber IS NOT INITIAL.
    SELECT SINGLE * FROM t093 INTO CORRESPONDING FIELDS OF ls_t093
                    WHERE afapl EQ ls_t093-afapl
                    AND afaber EQ ls_t093-abwber.
  ENDIF.
  APPEND ls_t093 TO lt_t093.
ENDSELECT.

* if no area does direct posting, we don't have to proceed.
DESCRIBE TABLE lt_t093 LINES ld_count.
IF ld_count LT 1.
  STOP. " continue with END-OF-SELECTION
ENDIF.

* get all asset classes with one of the found depreciation areas.
SELECT anlkl FROM ankb INTO CORRESPONDING FIELDS OF TABLE lt_ankb
                   FOR ALL ENTRIES IN lt_t093
                   WHERE afapl EQ ld_afapl
                   AND   afabe EQ lt_t093-afaber
                   AND   bdatu GE sy-datum "only entries still valid
                   GROUP BY anlkl.

* only proceed if at least one class was found.
DESCRIBE TABLE lt_ankb LINES ld_count.
IF ld_count LT 1.
  STOP.
ENDIF.

* get account determination of the found classes.
SELECT ktogr FROM anka INTO CORRESPONDING FIELDS OF ls_anka
                   FOR ALL ENTRIES IN lt_ankb
                   WHERE anlkl EQ lt_ankb-anlkl.
* check, if different acc. determination is set for chart of depr.
    SELECT SINGLE ktogr FROM ankp INTO CORRESPONDING FIELDS OF ls_ankp
                        WHERE anlkl EQ ls_anka-anlkl
                        AND   afapl EQ ld_afapl.
      IF sy-subrc EQ 0.
        IF ls_ankp-ktogr IS NOT INITIAL.
          " If there is an entry in ANKP, this field should never be
          " initial. The check is just to make sure.
          ls_anka-ktogr = ls_ankp-ktogr.
        ENDIF.
      ENDIF.
  APPEND ls_anka TO lt_anka.
ENDSELECT.

* delete duplicate information about found acc. determinations
SORT lt_anka ASCENDING BY ktogr.
DELETE ADJACENT DUPLICATES FROM lt_anka COMPARING ktogr.

* build up key-table for selection on T095
LOOP AT lt_anka INTO ls_anka.
  ls_t095_key-ktopl = ld_ktopl.
  ls_t095_key-ktogr = ls_anka-ktogr.
  LOOP AT lt_t093 INTO ls_t093.
    ls_t095_key-afabe = ls_t093-afaber.
    APPEND ls_t095_key TO lt_t095_key.
  ENDLOOP.
ENDLOOP.

* get technical info of fields in T095 to store them together with
*   further information in case of error later on.
CALL FUNCTION 'DDIF_FIELDINFO_GET'
  EXPORTING
    TABNAME              = 'T095'
    LANGU                = SY-LANGU
  TABLES
    DFIES_TAB            = lt_dfies
  EXCEPTIONS
    NOT_FOUND            = 1
    INTERNAL_ERROR       = 2
    OTHERS               = 3
          .
IF SY-SUBRC <> 0.
  MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
          WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.

* Though it has already been checked before that table lt_t093 and
* lt_ankb are not empty, it should not happen that lt_t095_key has
* no entries. Nevertheless, this check is made to make sure...
DESCRIBE TABLE lt_t095_key LINES ld_count.
IF ld_count LT 1.
  STOP.
ENDIF.

* get balance sheet accounts
SELECT * FROM t095 INTO CORRESPONDING FIELDS OF ls_t095
                   FOR ALL ENTRIES IN lt_t095_key
                   WHERE ktopl EQ lt_t095_key-ktopl
                   AND   ktogr EQ lt_t095_key-ktogr
                   AND   afabe EQ lt_t095_key-afabe.
* check flag XMWNO for each G/L account set in t095
  DO 21 TIMES VARYING ld_ktonr FROM ls_t095-ktansw NEXT ls_t095-ktanza.
    IF ld_ktonr IS NOT INITIAL. "ceck only if account is assigned
      SELECT SINGLE * FROM skb1 INTO CORRESPONDING FIELDS OF ls_skb1
                      WHERE bukrs EQ p_bukrs
                      AND   saknr EQ ld_ktonr.
        IF sy-subrc EQ 0.
          IF ls_skb1-xmwno NE 'X'. " WRONG CUSTOMIZING FOUND !!!
            " move ktopl, ktogr and afabe to error-structure
            MOVE-CORRESPONDING ls_t095 TO ls_kto_wrong.
            " read technical information of currently checked T095-account
            ld_index = sy-index + 4. "add 4 to skip key-fields of T095
            READ TABLE lt_dfies INDEX ld_index INTO ls_dfies.
            " fill other fields
            ls_kto_wrong-saknr = ld_ktonr.
            ls_kto_wrong-xmwno = ls_skb1-xmwno.
            ls_kto_wrong-fldname = ls_dfies-fieldname.
            ls_kto_wrong-flddesc = ls_dfies-fieldtext.
            APPEND ls_kto_wrong TO lt_kto_wrong.
          ENDIF.
        ENDIF.
    ENDIF.
  ENDDO.

ENDSELECT.

* sort error table for display
SORT lt_kto_wrong ASCENDING BY ktopl ktogr afabe.

************************************************************************
* FORMATTING DATA
************************************************************************
END-OF-SELECTION.

ULINE.

WRITE: 	/3(27) 'company code:' COLOR COL_HEADING,
        30 p_bukrs COLOR COL_TOTAL INTENSIFIED OFF, 40 '|'.
        NEW-LINE.

WRITE:   3(27) 'chart of depreciation:' COLOR COL_KEY.
IF ld_afapl IS INITIAL.
  WRITE: 30 ld_afapl COLOR COL_NEGATIVE INTENSIFIED OFF, 40 '|'.
  WRITE  43 'Missing chart of depreciation' COLOR COL_NEGATIVE INVERSE.
ELSE.
  WRITE: 30 ld_afapl COLOR COL_TOTAL INTENSIFIED OFF, 40 '|'.
ENDIF.
NEW-LINE.

WRITE:   3(27) 'chart of accounts:' COLOR COL_KEY.
IF ld_ktopl IS INITIAL.
  WRITE: 30 ld_ktopl COLOR COL_NEGATIVE INTENSIFIED OFF, 40 '|'.
  WRITE  43 'Missing chart of accounts' COLOR COL_NEGATIVE INVERSE.
ELSE.
  WRITE: 30 ld_ktopl COLOR COL_TOTAL INTENSIFIED OFF, 40 '|'.
ENDIF.
NEW-LINE.
ULINE.

IF p_detail EQ 'X'. " show a detailed report
      SKIP.
      WRITE: '+  The following depreciation areas post directly to G/L'
                COLOR COL_HEADING INVERSE.
      NEW-LINE.
      DESCRIBE TABLE lt_t093 LINES ld_count.
      IF ld_count GT 0.
        LOOP AT lt_t093 INTO ls_t093.
          WRITE ls_t093-afaber.
        ENDLOOP.
      ELSE.
        nothing_found.
      ENDIF.

      SKIP.
      WRITE: '+  The following asset classes have at least one of these depreciation areas'
                COLOR COL_HEADING INVERSE.
      NEW-LINE.
      DESCRIBE TABLE lt_ankb LINES ld_count.
      IF ld_count GT 0.
        LOOP AT lt_ankb INTO ls_ankb.
          WRITE ls_ankb-anlkl.
        ENDLOOP.
      ELSE.
        nothing_found.
      ENDIF.

      SKIP.
*      WRITE: 1(2) ' ' COLOR COL_HEADING.
      WRITE: '+  These classes use the following depreciation determinations'
                COLOR COL_HEADING INVERSE.
      NEW-LINE.
      DESCRIBE TABLE lt_anka LINES ld_count.
      IF ld_count GT 0.
        LOOP AT lt_anka INTO ls_anka.
          WRITE ls_anka-ktogr.
        ENDLOOP.
      ELSE.
        nothing_found.
      ENDIF.
      SKIP.

*      WRITE: 1(2) ' ' COLOR COL_HEADING.
      WRITE: '+  Depending on the found information the program checked all accounts '
                COLOR COL_HEADING INVERSE,
             72 'set in T095-entries for the following keys'
                COLOR COL_HEADING INVERSE.
      SKIP.
      ULINE AT 4(29).
      WRITE:  /4 '|', 5 'AFABE',
              14 '|', 15 'KTOGR',
              24 '|', 25 'KTOPL',
              32 '|'.
              NEW-LINE.
      LOOP AT lt_t095_key INTO ls_t095_key.
        WRITE:
              4  '|', ls_t095_key-afabe UNDER 'AFABE',
              14 '|', ls_t095_key-ktogr UNDER 'KTOGR',
              24 '|', ls_t095_key-ktopl UNDER 'KTOPL',
              32 '|'.
              NEW-LINE.
      ENDLOOP.
      ULINE AT 4(29).
      SKIP.
      ULINE.
ENDIF.

* Found the following errors in customizing of G/L accounts
WRITE: 'The following G/L accounts may cause problems:', /.

DESCRIBE TABLE lt_kto_wrong LINES ld_count.
IF ld_count LT 1.
  nothing_found.
ELSE. " show errors
  LOOP AT lt_kto_wrong INTO ls_kto_wrong.
    AT NEW ktogr. " new account determination
    ld_linno = sy-linno.
      ULINE AT 6(35). NEW-LINE.
    WRITE: 6 '|', 7 'chart of depreciation',
          30 '|', 31 ls_kto_wrong-ktopl, 40 '|'.
      NEW-LINE. ULINE AT 6(35). NEW-LINE.
    WRITE: 6 '|', 7 'account determination',
          30 '|', 31 ls_kto_wrong-ktogr, 40 '|'.
      NEW-LINE. ULINE AT 6(35). NEW-LINE.
    sy-linno = ld_linno.
    ENDAT.

    AT NEW afabe. " header for new areas of depreciation
      ULINE AT 45(91). NEW-LINE.
      WRITE:  45 '|', 46(88) 'Depreciation Area: ',
              65 ls_kto_wrong-afabe, 135 '|'.
      NEW-LINE. ULINE AT 45(91). NEW-LINE.
      WRITE:  45 '|', 46 'G/L account' COLOR COL_HEADING,
              57 '|', 58 'XMWNO' COLOR COL_NEGATIVE,
              63 '|', 64(8) 'field' COLOR COL_HEADING,
              73 '|', 74(60) 'description' COLOR COL_HEADING,
              135 '|'.
      NEW-LINE. ULINE AT 45(91).
    ENDAT.
                " display accounts
    NEW-LINE.
    WRITE:  45 '|', ls_kto_wrong-saknr UNDER 'G/L account'
                    COLOR COL_KEY,
            57 '|', ls_kto_wrong-xmwno UNDER 'XMWNO'
                    COLOR COL_NORMAL,
            63 '|', ls_kto_wrong-fldname UNDER 'field'
                    COLOR COL_NORMAL,
            73 '|', ls_kto_wrong-flddesc UNDER 'description'
                    COLOR COL_NORMAL,
            135 '|'.
    NEW-LINE.

    AT END OF afabe.
      ULINE AT 45(91). SKIP.
    ENDAT.
  ENDLOOP.
ENDIF.
