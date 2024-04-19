REPORT  zfapr016_passport_balance LINE-SIZE 156 MESSAGE-ID zs
        LINE-COUNT 65 NO STANDARD PAGE HEADING.
************************************************************************
*  Author: Brian Boundy
*  Date  : April, 2011.
*  Description:
*
*  This program will select passport balancing records and present to
*  the user to be printed.
************************************************************************
* Changes:
* 2013/12/02 gymana - Fix select statement by clearing internal table
* SDP56445            fields after each loop
* 2013/10/09 btboundy - add otrisk
* SDP56445
* 2013/08/21 gymana - Modify report to exclude cross company postings
* SDP47438
************************************************************************

DATA: BEGIN OF ls_data,
          lifnr     LIKE bseg-lifnr,
          name1     LIKE lfa1-name1,
          bukrs     LIKE bkpf-bukrs,
          belnr     LIKE bkpf-belnr,
          gjahr     LIKE bkpf-gjahr,
          cpudt(9)  TYPE c,
          cputm(5)  TYPE c,
          "bktxt     LIKE bkpf-bktxt,
          xblnr     LIKE bkpf-xblnr,
          dmbtr(11) TYPE c,
          waers     LIKE bkpf-waers,
          bldat(8) TYPE c,
          zterm     LIKE bseg-zterm,
          otrisk(6) TYPE c,                                 "SDP56445
      END OF ls_data.

DATA: lt_data  LIKE TABLE OF ls_data,
      ls_bkpf  LIKE bkpf,
      ls_bseg  LIKE bseg,
      ls_bseg2 LIKE bseg.

RANGES otrisk FOR bseg-sgtxt.

DATA: lv_linecount  TYPE i,
      w_msg(50)     TYPE c.

************************************************************************
* SELECTION-SCREEN
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK box1 WITH FRAME.

PARAMETERS: p_cpudt LIKE bkpf-cpudt OBLIGATORY DEFAULT sy-datum,
            p_blart LIKE bkpf-blart OBLIGATORY DEFAULT 'K3'.

SELECTION-SCREEN END OF BLOCK box1.

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
  CLEAR: ls_bkpf, ls_bseg, ls_data.

  SELECT belnr gjahr cpudt cputm belnr bldat bukrs waers xblnr "bktxt
    INTO CORRESPONDING FIELDS OF ls_bkpf
    FROM bkpf
    WHERE blart = p_blart
      AND cpudt = p_cpudt
  .

    SELECT SINGLE lifnr dmbtr zterm sgtxt
      INTO CORRESPONDING FIELDS OF ls_bseg
      FROM bseg
      WHERE belnr = ls_bkpf-belnr
        AND bukrs = ls_bkpf-bukrs
        AND gjahr = ls_bkpf-gjahr
        AND buzei = '001'
        "AND bschl = '31'                                   "SDP47438
        AND lifnr <> ''                                     "SDP56445
    .

    IF ls_bseg-sgtxt NS 'ELegal-'.
      CONTINUE.
    ENDIF.

    SELECT lifnr dmbtr zterm sgtxt                          "SDP56445
      INTO CORRESPONDING FIELDS OF ls_bseg2
      FROM bseg
      WHERE belnr = ls_bkpf-belnr
        AND bukrs = ls_bkpf-bukrs
        AND gjahr = ls_bkpf-gjahr.

      IF ls_bseg2-sgtxt CS 'OTRISK'. "Check if one of the lines has OTRISK.
        ls_data-otrisk = 'OTRISK'.
      ENDIF.

    ENDSELECT.


    SELECT SINGLE name1
      INTO CORRESPONDING FIELDS OF ls_data
      FROM lfa1
      WHERE lifnr = ls_bseg-lifnr
    .



    ls_data-lifnr = ls_bseg-lifnr.
    ls_data-bukrs = ls_bkpf-bukrs.
    ls_data-belnr = ls_bkpf-belnr.
    ls_data-gjahr = ls_bkpf-gjahr.

    CONCATENATE ls_bkpf-cpudt '-' INTO ls_data-cpudt.
    CONCATENATE ls_bkpf-cputm(2) ':' ls_bkpf-cputm+2(2)
                INTO ls_data-cputm.

    "ls_data-bktxt = ls_bkpf-bktxt.
    ls_data-xblnr = ls_bkpf-xblnr.
    ls_data-dmbtr = ls_bseg-dmbtr.
    ls_data-waers = ls_bkpf-waers.
    ls_data-bldat = ls_bkpf-bldat.
    ls_data-zterm = ls_bseg-zterm.

    APPEND ls_data TO lt_data.
    CLEAR: ls_bseg, ls_data.                                   "SDP56445
  ENDSELECT.


ENDFORM.                    "get_data


************************************************************************
* Generate_Report_Header
************************************************************************
FORM generate_report_header.
  NEW-PAGE.
  CLEAR lv_linecount.
  FORMAT INTENSIFIED ON.
  WRITE: /1 sy-repid(8), 56 text-h01.
  WRITE: 121 text-pge, sy-pagno.
  WRITE: /1 text-h03, p_cpudt.
  SKIP 1.
  WRITE:  /2  text-003,   "Vendor #
          12   text-004,   "Vendor Name
          47  text-005,   "Co. Code
          53  text-006,   "Doc #
          65  text-007,   "Year
          70  text-008,   "Entry Date
          79  text-009,   "Entry Time
          "71  text-010,   "Header Text
          87  text-011,   "Reference
          103 text-012,   "Amount
          114 text-013,   "Cur Type
          119 text-014,   "Doc Date
          128 text-015,   "Terms
          134 text-016.   "OTRISK
  SKIP 1.
  MOVE '5' TO lv_linecount.
  FORMAT INTENSIFIED OFF.

ENDFORM.                    "generate_report_header

************************************************************************
* Write_Report_Detail
************************************************************************
FORM write_report_detail.

  SORT lt_data BY bukrs lifnr.


  LOOP AT lt_data INTO ls_data.

    "Create headers at first record, at new company code, and when
    "lines hit 55 per page.
    AT FIRST.
      PERFORM generate_report_header.
    ENDAT.

    IF lv_linecount >= 55.
      PERFORM generate_report_header.
    ENDIF.


    WRITE ls_data-lifnr  UNDER text-003.
    WRITE ls_data-name1  UNDER text-004.
    WRITE ls_data-bukrs  UNDER text-005.
    WRITE ls_data-belnr  UNDER text-006.
    WRITE ls_data-gjahr  UNDER text-007.
    WRITE ls_data-cpudt  UNDER text-008.
    WRITE ls_data-cputm  UNDER text-009.
    "WRITE ls_data-bktxt  UNDER text-010.
    WRITE ls_data-xblnr  UNDER text-011.
    WRITE ls_data-dmbtr  UNDER text-012.
    WRITE ls_data-waers  UNDER text-013.
    WRITE ls_data-bldat  UNDER text-014.
    WRITE ls_data-zterm  UNDER text-015.
    WRITE ls_data-otrisk UNDER text-016.
    SKIP.
    ADD +2 TO lv_linecount.

  ENDLOOP.

  IF sy-subrc = '4'.
    "No data, write an empty header.
    PERFORM generate_report_header.
    SKIP.
    WRITE: /53    '*** No data to process ***'.
    ADD +2 TO lv_linecount.
  ENDIF.

ENDFORM.                    "write_report_detail
