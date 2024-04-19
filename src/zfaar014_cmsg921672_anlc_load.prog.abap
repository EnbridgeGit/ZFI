*&---------------------------------------------------------------------*
*& Report  ZFAAR014_CMSG921672_ANLC_LOAD
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZFAAR014_CMSG921672_ANLC_LOAD.

TABLES: anlc.

PARAMETERS: p_bukrs                  LIKE anlc-bukrs DEFAULT 'UGL'.
SELECT-OPTIONS: s_anln1              FOR anlc-anln1 NO INTERVALS OBLIGATORY.
PARAMETERS: p_anln2                  LIKE anlc-anln2 DEFAULT '0',
            p_gjahr                  LIKE anlc-gjahr DEFAULT '2010',
            p_afabe                  LIKE anlc-afabe DEFAULT '21',
            p_pstbeg                 LIKE anlc-pstbeg DEFAULT '001',
            p_pstend                 LIKE anlc-pstend DEFAULT '012',
            p_pstcal                 LIKE anlc-pstcalc DEFAULT ' ',
            p_pstper                 LIKE anlc-pstper DEFAULT '001'.

SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31) FOR FIELD p_add.
PARAMETERS: p_add RADIOBUTTON GROUP r1 DEFAULT 'X'.
SELECTION-SCREEN COMMENT 41(31) FOR FIELD p_del.
PARAMETERS: p_del RADIOBUTTON GROUP r1.
SELECTION-SCREEN END OF LINE.

DATA: gt_anlc TYPE STANDARD TABLE OF anlc
        WITH HEADER LINE.

INITIALIZATION.
  MOVE: 'EQ'                         TO s_anln1-option,
        'I'                          TO s_anln1-sign,
        '465010330082'               TO s_anln1-low.
  APPEND s_anln1.
  MOVE: 'EQ'                         TO s_anln1-option,
        'I'                          TO s_anln1-sign,
        '465010420056'               TO s_anln1-low.
  APPEND s_anln1.
  MOVE: 'EQ'                         TO s_anln1-option,
        'I'                          TO s_anln1-sign,
        '475014330831'               TO s_anln1-low.
  APPEND s_anln1.

START-OF-SELECTION.

  MOVE: sy-mandt                    TO gt_anlc-mandt,
        p_bukrs                     TO gt_anlc-bukrs,
        p_anln2                     TO gt_anlc-anln2,
        p_gjahr                     TO gt_anlc-gjahr,
        p_afabe                     TO gt_anlc-afabe,
        p_pstbeg                    TO gt_anlc-pstbeg,
        p_pstend                    TO gt_anlc-pstend,
        p_pstcal                    TO gt_anlc-pstcalc,
        p_pstper                    TO gt_anlc-pstper.
  LOOP AT s_anln1.
    MOVE: s_anln1-low           TO gt_anlc-anln1.
    APPEND gt_anlc.
  ENDLOOP.

  IF p_add = 'X'.

    INSERT anlc FROM TABLE gt_anlc.

    IF sy-subrc = 0.

      WRITE: /1 sy-dbcnt,
                'entries added into table ANLC.'.

    ELSE.

      WRITE: /1 'Error adding entries into table ANLC. Return Code =',
                sy-subrc.

    ENDIF.

  ELSE.

    DELETE anlc FROM TABLE gt_anlc.

    IF sy-subrc = 0.

      WRITE: /1 sy-dbcnt,
                'entries deleted from table ANLC.'.

    ELSE.

      WRITE: /1 'Error deleting entries from table ANLC. Return Code =',
                sy-subrc.

    ENDIF.

  ENDIF.
