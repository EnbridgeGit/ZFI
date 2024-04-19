REPORT  zfapr015_ariba_balance LINE-SIZE 132 MESSAGE-ID zs
        LINE-COUNT 65 NO STANDARD PAGE HEADING.
************************************************************************
*  Author: Brian Boundy
*  Date  : April, 2011.
*  Description:
*
*  This program will select ariba balancing records and present to
*  the user to be printed.
************************************************************************
TABLES: bkpf. "(+)SKAPSE Ticket 78045

DATA: BEGIN OF ls_data,
          lifnr   LIKE ekko-lifnr,
          name1   LIKE lfa1-name1,
          bukrs   LIKE bkpf-bukrs,
          ebeln   LIKE ekbe-ebeln,
          gjahr   LIKE ekbe-gjahr,
          ebelp   LIKE ekbe-ebelp,
          belnr   LIKE ekbe-belnr,
          gjahrf  LIKE bkpf-gjahr,
          belnrf  LIKE bkpf-belnr,
          xblnr   LIKE bkpf-xblnr,
          dmbtr   LIKE bseg-dmbtr,
          waers   LIKE bkpf-waers,
      END OF ls_data.

DATA: lt_data LIKE TABLE OF ls_data,
      ls_bkpf LIKE bkpf,
      ls_bseg LIKE bseg,
      ls_ekbe LIKE ekbe.

data: ls_rbkp like rbkp.  "(+)PANUSURI Ticket 42086

DATA: lv_gjahr(4) TYPE c,
      lv_linecount  TYPE i,
      w_msg(50)     TYPE c.

************************************************************************
* SELECTION-SCREEN
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK box1 WITH FRAME.

PARAMETERS: p_budat LIKE bkpf-budat OBLIGATORY DEFAULT sy-datum.
*            p_blart LIKE bkpf-blart OBLIGATORY DEFAULT 'ZR',  "(-)SKAPSE Ticket 78045
SELECT-OPTIONS: s_blart FOR bkpf-blart OBLIGATORY NO INTERVALS."(+)SKAPSE Ticket 78045

   PARAMETERS:         p_usnam LIKE bkpf-usnam OBLIGATORY DEFAULT 'SONIC',
            p_bstat LIKE bkpf-bstat.

SELECTION-SCREEN END OF BLOCK box1.

************************************************************************
* INITIALIZATION
************************************************************************
INITIALIZATION.
*BOI by SKAPSE Ticket 78045
  s_blart-sign = 'I'.
  s_blart-option = 'EQ'.
  s_blart-low = 'ZR'.
  APPEND s_blart.

  s_blart-sign = 'I'.
  s_blart-option = 'EQ'.
  s_blart-low = 'ZN'.
  APPEND s_blart.
*EOI by SKAPSE Ticket 78045


************************************************************************
* START-OF-SELECTION
************************************************************************
START-OF-SELECTION.


  PERFORM get_data.
  PERFORM write_report_detail.


************************************************************************
* Get_Data
************************************************************************
FORM get_data.
  CLEAR: ls_bkpf, ls_ekbe, ls_bseg, ls_data.

  SELECT belnr gjahr xblnr waers bukrs awkey bstat
    INTO CORRESPONDING FIELDS OF ls_bkpf
    FROM bkpf
*    WHERE blart = p_blart "(-)SKAPSE Ticket 78045
    WHERE blart IN s_blart "(+)SKAPSE Ticket 78045

      AND budat = p_budat
      AND usnam = p_usnam
      AND bstat = p_bstat
  .

    CLEAR ls_bseg.
    IF ls_bkpf-bstat = ''.
      SELECT SINGLE dmbtr
         INTO CORRESPONDING FIELDS OF ls_bseg
        FROM bseg
        WHERE belnr = ls_bkpf-belnr
          AND bukrs = ls_bkpf-bukrs
          AND gjahr = ls_bkpf-gjahr
          AND buzei = '001'
      .
    ELSEIF ls_bkpf-bstat = 'V'.
      SELECT SINGLE dmbtr
         INTO CORRESPONDING FIELDS OF ls_bseg
        FROM vbsegk
        WHERE belnr = ls_bkpf-belnr
          AND bukrs = ls_bkpf-bukrs
          AND gjahr = ls_bkpf-gjahr
          AND buzei = '001'
      .
    ENDIF.

    clear ls_ekbe. "(+)PANUSURI Ticket 42086
    SELECT SINGLE ebeln gjahr ebelp belnr
      INTO CORRESPONDING FIELDS OF ls_ekbe
      FROM ekbe
      WHERE gjahr = ls_bkpf-awkey+10(4)
        AND belnr = ls_bkpf-awkey(10)
        AND ( vgabe = '2' OR vgabe = 'P' )
        AND buzei = '1'.

    if sy-subrc = 0.  "(+)PANUSURI Ticket 42086
      SELECT SINGLE ekko~lifnr lfa1~name1
        INTO CORRESPONDING FIELDS OF ls_data
        FROM ekko
          INNER JOIN lfa1
            ON ekko~lifnr = lfa1~lifnr
        WHERE ekko~ebeln = ls_ekbe-ebeln
          AND ekko~bukrs = ls_bkpf-bukrs.

      ls_data-bukrs  = ls_bkpf-bukrs.
      ls_data-ebeln  = ls_ekbe-ebeln.
      ls_data-gjahr  = ls_ekbe-gjahr.
      ls_data-ebelp  = ls_ekbe-ebelp.
      ls_data-belnr  = ls_ekbe-belnr.
      ls_data-gjahrf = ls_ekbe-gjahr.
      ls_data-belnrf = ls_ekbe-belnr.
      ls_data-xblnr  = ls_bkpf-xblnr.
      ls_data-dmbtr  = ls_bseg-dmbtr.
      ls_data-waers  = ls_bkpf-waers.

      APPEND ls_data TO lt_data.

*   BOI PANUSURI Ticket 42086
    elseif sy-subrc <> 0.
      clear ls_rbkp.
      select single belnr gjahr esrre
             into corresponding fields of ls_rbkp
             from rbkp
             where belnr = ls_bkpf-awkey(10)
             AND   gjahr = ls_bkpf-awkey+10(4).
      if sy-subrc = 0.
        SELECT SINGLE ekko~lifnr lfa1~name1
               INTO CORRESPONDING FIELDS OF ls_data
               FROM ekko
               INNER JOIN lfa1
               ON ekko~lifnr = lfa1~lifnr
               WHERE ekko~ebeln = ls_rbkp-esrre
               AND ekko~bukrs = ls_bkpf-bukrs.
        if sy-subrc = 0.
          ls_data-bukrs  = ls_bkpf-bukrs.
          ls_data-ebeln  = ls_rbkp-esrre.
          ls_data-gjahr  = ls_rbkp-gjahr.
          ls_data-ebelp  = ' '.
          ls_data-belnr  = ls_rbkp-belnr.
          ls_data-gjahrf = ls_rbkp-gjahr.
          ls_data-belnrf = ls_rbkp-belnr.
          ls_data-xblnr  = ls_bkpf-xblnr.
          ls_data-dmbtr  = ls_bseg-dmbtr.
          ls_data-waers  = ls_bkpf-waers.

          APPEND ls_data TO lt_data.
        endif.
      endif.
    endif.
    clear ls_data.
*   EOI PANUSURI Ticket 42086
  ENDSELECT.


ENDFORM.                    "get_data


************************************************************************
* Generate_Report_Header
************************************************************************
FORM generate_report_header USING in_cocode.
  NEW-PAGE.
  CLEAR lv_linecount.
  FORMAT INTENSIFIED ON.
  WRITE: /1 sy-repid(8), 56 text-h01.
  WRITE: 121 text-pge, sy-pagno.
  WRITE: /1 text-h03, p_budat.
  IF p_bstat = 'V'.
    WRITE: 57 text-h02, in_cocode, 118 text-h04, 'Parked'.
  ELSEIF p_bstat = ''.
    WRITE: 57 text-h02, in_cocode, 118 text-h04, 'Posted'.
  ELSE.
    WRITE: 57 text-h02, in_cocode, 118 text-h04, '??????'.
  ENDIF.
  SKIP 1.
  WRITE:  /2  text-003,   "Vendor #
          8  text-004,   "Vendor Name
          44  text-005,   "Ref. Doc #
          55  text-006,   "PYrRef
          62  text-007,   "Rflt
          68  text-008,   "Doc #
          79  text-009,   "Year
          84  text-010,   "FI Doc.
          95  text-011,   "Inv No.
          112 text-012,   "Amount in LC
          128 text-013.   "Currency Type
  SKIP 1.
  MOVE '5' TO lv_linecount.
  FORMAT INTENSIFIED OFF.

ENDFORM.                    "generate_report_header

************************************************************************
* Write_Report_Detail
************************************************************************
FORM write_report_detail.

  SORT lt_data BY bukrs lifnr ebeln.


  LOOP AT lt_data INTO ls_data.

    "Create headers at first record, at new company code, and when
    "lines hit 55 per page.
    AT NEW bukrs.
      PERFORM generate_report_header USING ls_data-bukrs.
    ENDAT.

    IF lv_linecount >= 55.
      PERFORM generate_report_header USING ls_data-bukrs.
    ENDIF.


    WRITE ls_data-lifnr  UNDER text-003.
    WRITE ls_data-name1  UNDER text-004.
    WRITE ls_data-ebeln  UNDER text-005.
    WRITE ls_data-gjahr  UNDER text-006.
    WRITE ls_data-ebelp  UNDER text-007.
    WRITE ls_data-belnr  UNDER text-008.
    WRITE ls_data-gjahrf UNDER text-009.
    WRITE ls_data-belnrf UNDER text-010.
    WRITE ls_data-xblnr  UNDER text-011.
    WRITE ls_data-dmbtr  UNDER text-012.
    WRITE ls_data-waers  UNDER text-013.
    SKIP.
    ADD +2 TO lv_linecount.

  ENDLOOP.

  IF sy-subrc = '4'.
    "No data, write an empty header.
    PERFORM generate_report_header USING 'NONE'.
    SKIP.
    WRITE: /53    '*** No data to process ***'.
    ADD +2 TO lv_linecount.
  ENDIF.

ENDFORM.                    "write_report_detail
