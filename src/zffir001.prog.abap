REPORT ZFFIR001 MESSAGE-ID ZS NO STANDARD PAGE HEADING LINE-SIZE 255.
*-----------------------------------------------------------------------
*  THIS ABAP is in both the EAST and the WEST.  When changes required,
*  please do them in both the EAST and the WEST.
************************************************************************
* 2003/06/06 mdemeest 1019 Added WBS as selection criteria
* 2003/04/14 mdemeest 1019 Added network number selection for WEST
*                          (not used in EAST), making abap portable
*
* 1999/06/07 mdemeest 3.1i Changed variant to allow only 1 period to be
*                          entered so that the data is limited
*
* 1999/04/27 mdemeest #677 Extract BKPF & BSEG fields from SAP into
*                          report that can be downloaded to C: drive
*                          Written for the Transportation Department
*
************************************************************************
TABLES: BKPF, BSEG,
        LFA1,               "Vendor Name
        LFB1,
        MAKT.               "Material Description

selection-screen begin of block box5 with frame title text-001.
  selection-screen begin of line.
    selection-screen  comment 3(79) text-099.
  selection-screen end of line.

  selection-screen begin of line.
    selection-screen  comment 3(70) text-098.
  selection-screen end of line.
selection-screen end of block box5.

SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME." TITLE TEXT-001.

PARAMETER:    P_BUKRS LIKE BKPF-BUKRS memory id BUK       OBLIGATORY.
PARAMETER:    P_GJAHR LIKE BKPF-GJAHR DEFAULT SY-DATUM(4) OBLIGATORY,
              P_MONAT LIKE BKPF-MONAT OBLIGATORY DEFAULT SY-DATUM+4(2).
*select-options: s_monat for  bkpf-monat obligatory default '01'to'12',
SELECT-OPTIONS: S_BLART FOR  BKPF-BLART,
                S_HKONT FOR  BSEG-HKONT,                "G/L
                S_LIFNR FOR BSEG-LIFNR.                 "Vendor
SELECT-OPTIONS: S_MATNR FOR BSEG-MATNR,                 "Material
                S_AUFNR FOR BSEG-AUFNR,                 "Internal Order
                S_KOSTL FOR BSEG-KOSTL,                 "Cost Centre
                s_projk for bseg-projk no intervals,    "WBS
                s_nplnr for bseg-nplnr.                 "Network (West)
SELECTION-SCREEN END OF BLOCK BOX3.

selection-screen begin of block box4.
parameters:   p_rprt radiobutton  group rbcr,   "Print Report
              p_excl radiobutton  group rbcr.   "Excel spreadsheet
selection-screen end of block box4.
*-----------------------------------------------------------------------
data:  wa_lifnr     like lfa1-lifnr,
       wa_name1     like lfa1-name1,
       wa_zwels     like lfb1-zwels.

data:  retcode      like sy-subrc,
       W_OPTION(11) TYPE C  value 'start_excel',
       W_HEAD01(40) TYPE C,
       W_HEAD02(40) TYPE C,
       W_REPTTL     like sy-title.

data:  begin of prot_header occurs 1,
         spaltenname(20) type c,
         ddic_table(5)   type c,
         ddic_field(5)   type c,
         key type c,
       end of prot_header.

data:  begin of wa      occurs 0,
          bukrs     like bkpf-bukrs,
          gjahr(7)  type c,
          belnr     like bkpf-belnr,
          blart     like bkpf-blart,
          budat     like bkpf-budat,
          bldat     like bkpf-bldat,
          lifnr     like bseg-lifnr,
          name1     like lfa1-name1,
          end of wa.

DATA: BEGIN of EXCLTAB  OCCURS 0,
         bukrs      like bkpf-bukrs,             "Company Code
         gjahr(7)   type c,                      "Fiscal Year/Month
         belnr      like bkpf-belnr,             "Document Number
         bldat      like bkpf-bldat,             "Invoice Date
*         DMBTR      LIKE BSEG-DMBTR,             "Amount
         dmbtr(15)  type c,
         HKONT      LIKE BSEG-HKONT,
         BLART      LIKE BKPF-BLART,             "Document Type
         ZWELS      like lfb1-zwels,             "
         LIFNR      LIKE BSEG-LIFNR,             "Vendor #
         name1      like lfa1-name1,             "Vendor name
         AUFNR      LIKE BSEG-AUFNR,
*         PROJK      LIKE BSEG-PROJK,
         projk(24)  type c,
         KOSTL      LIKE BSEG-KOSTL,
         SGTXT      LIKE BSEG-SGTXT,
         MATNR      LIKE BSEG-MATNR,
         maktx      like makt-maktx,
         nplnr      like bseg-nplnr,

      END of EXCLTAB.

data  errortab like hrerror  occurs 0 with header line.


*-----------------------------------------------------------------------

SELECT * FROM BKPF                       "Selection based on fiscal year
   WHERE BUKRS = P_BUKRS                 "and month
     AND GJAHR = P_GJAHR
     AND MONAT = P_MONAT                                    "99/06/07
*    and monat in s_monat                                      "99/06/07
     AND BLART IN S_BLART.

  PERFORM MOVE_BKPF_TO_WA.                      "Move info to work area

endselect.

perform get_vendor.

loop at wa.
  SELECT * FROM BSEG
    WHERE BUKRS = wa-BUKRS
      AND BELNR = wa-BELNR
      AND GJAHR = wa-GJAHR(4)
      AND HKONT IN S_HKONT
      AND AUFNR IN S_AUFNR
      AND MATNR IN S_MATNR
      AND KOSTL IN S_KOSTL
      and projk in s_projk
      and nplnr in s_nplnr.


    PERFORM MOVE_BSEG_TO_WA.     "Move info to work area
  endselect.
endloop.
"END of BKPF

SORT excltab BY GJAHR BELNR LIFNR DESCENDING.

loop at excltab.
  on change of excltab-belnr.
    if excltab-lifnr <> ' '.
      clear:  wa_zwels.
      SELECT SINGLE ZWELS FROM LFB1 INTO wa_ZWELS
        WHERE LIFNR = excltab-LIFNR
          AND BUKRS = P_BUKRS.
    endif.
  endon.

  move wa_zwels  to excltab-zwels.

  modify excltab.

endloop.

perform create_output_report.


*---------------------------------  MOVE_BKPF_TO_WA --------------------
FORM MOVE_BKPF_TO_WA.
  CLEAR excltab.
  MOVE: bkpf-bukrs to wa-bukrs,
        bkpf-belnr to wa-belnr.
  concatenate: bkpf-gjahr text-slh bkpf-monat into wa-gjahr.
  move: bKPF-BLDAT TO wa-BLDAT,
        BKPF-BLART TO wa-BLART.
  append wa.
ENDFORM.

*---------------------------------  GET_VENDOR  ------------------------
* find the vendor number for the document
*-----------------------------------------------------------------------
form get_vendor.
  loop at wa.
    select single * from bseg
      where bukrs = wa-bukrs
        and belnr = wa-belnr
        and gjahr = wa-gjahr(4)
        and lifnr in s_lifnr
        and ( ( buzei = '001' and lifnr <> ' ')
           or ( buzei <> '001' and lifnr <> ' ' ) ).
    if sy-subrc = '0'.
      move bseg-lifnr        to wa-lifnr.
      select single * from lfa1
         where lifnr = wa-lifnr.
      if  sy-subrc = '0'.
        move lfa1-name1    to wa-name1.
      endif.
      modify wa.
    else.
      delete wa.
    endif.
  endloop.
endform.

*---------------------- MOVE_BSEG_TO_WA  -------------------------------
* move fields from BSEG to OUTPUT RECORD
*-----------------------------------------------------------------------
FORM MOVE_BSEG_TO_WA.
  CLEAR excltab.
  move wa-bukrs       to excltab-bukrs.
  move wa-belnr       to excltab-belnr.
  move wa-gjahr       to excltab-gjahr.
  move wa-lifnr       to excltab-lifnr.
  move wa-name1       to excltab-name1.
  move wa-blart       to excltab-blart.
  move wa-bldat       to excltab-bldat.

  MOVE BSEG-DMBTR      TO excltab-DMBTR.
  IF BSEG-SHKZG = 'H'.
    excltab-dmbtr = excltab-dmbtr * -1.
  ENDIF.

  MOVE BSEG-KOSTL      TO excltab-KOSTL.
  write BSEG-PROJK      TO excltab-PROJK.
  MOVE BSEG-AUFNR      TO excltab-AUFNR.
  MOVE BSEG-HKONT      TO excltab-HKONT.
  MOVE BSEG-MATNR      TO excltab-MATNR.
  MOVE BSEG-SGTXT      TO excltab-SGTXT.
  move bseg-nplnr      to excltab-nplnr.


  IF bseg-MATNR <> SPACE.
    SELECT SINGLE MAKTX FROM MAKT INTO excltab-MAKTX
      WHERE MATNR = bseg-MATNR
        AND SPRAS = SY-LANGU.
  ENDIF.

  append excltab.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM create_output_report                                     *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form create_output_report.
  perform prot_header.
  move text-ttl   to W_repttl.
  move text-dte   to w_head01.
  write sy-datum  to w_head01+6(10).
  move text-amp   to w_head01+17(1).
  move sy-uzeit   to w_head01+19(10).
  move text-clt   to w_head02.
  move sy-mandt   to w_head02+8(4).
  move sy-sysid   to w_head02+13(4).
  if p_rprt = 'X'.                                "from variant
    clear w_option.
    if sy-batch = 'X'.                            "run in background
      w_option = 'LINESELMOD:1'.
    endif.
  endif.

  call function 'HR_DISPLAY_BASIC_LIST'
       EXPORTING
            basic_list_title    = w_repttl
            file_name           = sy-cprog
            head_line1          = w_head01
            head_line2          = w_head02
            additional_options  = w_option
       IMPORTING
            return_code         = retcode
       TABLES
            data_tab            = excltab
            fieldname_tab       = prot_header
            error_tab           = errortab
       EXCEPTIONS
            download_problem    = 1
            no_data_tab_entries = 2
            table_mismatch      = 3
            print_problems      = 4
            others              = 5.
  if sy-subrc <> '0'.
    write: /1 'table download unsuccessful - reason = ', sy-subrc.
  endif.
endform.

*------------------------- PROT_HEADER  --------------------------------
*  Each field title in the spreadsheet must be added in the same order
*  as the order of the data.
*-----------------------------------------------------------------------
form prot_header.
  move text-002 to prot_header-spaltenname.        "Company code
  append prot_header.
  move text-004 to prot_header-spaltenname.        "Fiscal Year
  append prot_header.
  move text-006 to prot_header-spaltenname.        "Document #
  append prot_header.
  move text-008 to prot_header-spaltenname.        "Invoice Date
  append prot_header.
  move text-010 to prot_header-spaltenname.        "Amount
  append prot_header.
  move text-011 to prot_header-spaltenname.        "Cost Element
  append prot_header.
  move text-024 to prot_header-spaltenname.        "Document Type
  append prot_header.
  move text-026 to prot_header-spaltenname.        "P/M
  append prot_header.
  move text-013 to prot_header-spaltenname.        "Vendor #
  append prot_header.
  move text-014 to prot_header-spaltenname.        "Vendor Name
  append prot_header.
  move text-015 to prot_header-spaltenname.        "Internal Order
  append prot_header.
  move text-017 to prot_header-spaltenname.        "Project/WBS
  append prot_header.
  move text-018 to prot_header-spaltenname.        "P/M
  append prot_header.
  move text-020 to prot_header-spaltenname.        "P/M
  append prot_header.
  move text-021 to prot_header-spaltenname.        "Material #
  append prot_header.
  move text-023 to prot_header-spaltenname.        "Material Desc.
  append prot_header.
  move text-003 to prot_header-spaltenname.        "Network
  append prot_header.

endform.
