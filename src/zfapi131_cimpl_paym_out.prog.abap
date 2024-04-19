*&---------------------------------------------------------------------*
*& Report  ZFAPI131_CIMPL_PAYM_OUT
*&---------------------------------------------------------------------*

REPORT  zfapi131_cimpl_paym_out MESSAGE-ID zs.

************************************************************************
*  Client:     Spectra Energy.                                         *
*  Date:       October 24, 2019                                        *
*  Author:     Shamsiya Shaffe                                         *
*  Program Description:                                                *
*  Extract for Cimpl Payment reconciliation.                           *
*  Copy of UG Report - ZFAPR012_EBILL_PAY_RECONCILE                    *
************************************************************************
*CHANGES:                                                              *
* Date       By       Issue Description                                *
* 2019/10/24 Shaffes   Initial Development                             *
*                                                                      *
************************************************************************

TYPES:          ty_bkpf       TYPE bkpf,
                ty_bsak       TYPE bsak,
                ty_payr       TYPE payr,
                ty_bseg       TYPE bseg,
                ty_bbsseg     TYPE bbseg,
                BEGIN OF ty_payr_lookup,
                  mandt LIKE payr-mandt,      " Client
                  zbukr LIKE payr-zbukr,      " Paying company code
                  hbkid LIKE payr-hbkid,      " Short Key for a House Bank
                  hktid LIKE payr-hktid,      " ID for Account Details
                  rzawe LIKE payr-rzawe,      " Payment Method
                  chect LIKE payr-chect,      " Check number
                  gjahr LIKE payr-gjahr,      " Fiscal Year
                  vblnr LIKE payr-vblnr,      " Document Number of the Payment Document
                END OF ty_payr_lookup.

DATA:           lv_local      TYPE          integer,
                msg(80)       TYPE          c,
                lv_string     TYPE          string,
                lv_datetime   TYPE          string,
                lv_count      TYPE          i,
                lv_lines      TYPE          i,
                lv_system     TYPE          c,

                s_bkpf        TYPE          ty_bkpf,
                t_bkpf        LIKE TABLE OF s_bkpf,
                s_bsak        TYPE          ty_bsak,
                t_bsak        LIKE TABLE OF s_bsak,
                s_payr        TYPE          ty_payr,
                t_payr        LIKE TABLE OF s_payr,
                s_payr_lookup TYPE          ty_payr_lookup,
                t_payr_lookup LIKE TABLE OF s_payr_lookup,
                s_bseg        TYPE          ty_bseg,
                t_bseg        LIKE TABLE OF s_bseg,

                lv_datarec    TYPE          string,
                t_data        LIKE TABLE OF lv_datarec,

                lv_wrbtr      TYPE          string,
                lv_rwbtr      TYPE          string,
                lv_bldat(10)  TYPE          c,
                lv_zaldt(10)  TYPE          c,
                lv_sgtxt      TYPE          string,
                lv_null1      TYPE          string,
                lv_null2      TYPE          string,
                lv_lifnr      LIKE          bseg-lifnr,  " Account Number of Vendor or Creditor
                lv_vornr      LIKE          afvc-vornr,  " Operation/Activity Number
                lv_hkont      LIKE          bseg-hkont,  " General Ledger Account
                lv_aufnr      LIKE          bseg-aufnr,  " Order Number
                lv_poski      LIKE          prps-poski,  " Work Breakdown Structure Element (WBS Element)
                lv_blank      TYPE          string.

FIELD-SYMBOLS:  <curcol>      TYPE          any.
CONSTANTS:      delimtr       TYPE          c               VALUE '|'.
DATA: gt_reguh TYPE TABLE OF reguh,
      gs_reguh TYPE reguh,
      gt_regup TYPE TABLE OF regup,
      gs_regup TYPE regup,
      gt_bkpf  TYPE TABLE OF bkpf,
      gs_bkpf TYPE bkpf,
      gt_bsak TYPE TABLE OF bsak,
      gs_bsak TYPE bsak,
      gt_bseg TYPE TABLE OF bseg,
      gs_bseg TYPE bseg,
      gt_payr TYPE TABLE OF payr,
      gs_payr TYPE payr.

SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE text-001.
PARAMETERS:
  p_blart   LIKE bkpf-blart DEFAULT 'CM',
  p_zaldt   LIKE payr-zaldt.                  " Probable Payment Date (Cash Discount 1 Due)
SELECTION-SCREEN END OF BLOCK a1.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-002.
PARAMETERS:
    p_local   RADIOBUTTON GROUP rad1 DEFAULT 'X',
    p_lfile    TYPE        string,
    p_server  RADIOBUTTON GROUP rad1,
    p_sfile   LIKE        rfpdo-rfbifile.     " Name of the Batch Input File Path
SELECTION-SCREEN END OF BLOCK b1.


INITIALIZATION.
  CONCATENATE '/usr/sap/interfaces/' sy-sysid+0(3)
              '/CIMPL/Out/SAPUGL_PaidInvoices_' sy-datum '_' sy-uzeit '.txt'
              INTO p_sfile.

*Start of TR995 chnages
* CONCATENATE  'C:\SAPTEMP\EBILL_RECONCILE' sy-datum sy-uzeit '.dat'
  CONCATENATE  'H:\SAPTEMP\CIMPL_SAPUGL_PaidInvoices_' sy-datum '_' sy-uzeit '.txt'
               INTO p_lfile.
*Ene of TR995 Changes

  MOVE sy-datum TO p_zaldt.

*----------------------------------------------------------------------*
*       AT SELECTION-SCREEN
*----------------------------------------------------------------------*
*Start of TR995 changes
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_lfile.
  DATA: wif_window_title        TYPE string VALUE 'Please Select File',
        wif_initial_directory   TYPE string VALUE 'h:\',
        wit_filename_tab        TYPE filetable WITH HEADER LINE,
        wif_rc                  TYPE i.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = wif_window_title
      initial_directory       = wif_initial_directory
    CHANGING
      file_table              = wit_filename_tab[]
      rc                      = wif_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  IF ( sy-subrc = 0 ).
*Return user selection
    READ TABLE wit_filename_tab INDEX 1.
    IF sy-subrc IS INITIAL AND wif_rc > 0.
      p_lfile = wit_filename_tab.
    ELSE.
      CLEAR p_lfile.
    ENDIF.
  ENDIF.
*End of TR995 changes

START-OF-SELECTION.
  IF p_local = 'X'.
    lv_local = 1.
  ELSE.
    lv_local = 0.
  ENDIF.
  PERFORM gt_data_new.
*****************
*  PERFORM get_data.
  PERFORM output_data.




*----------------------------------------------------------------------*
FORM get_data.

  CLEAR: t_data.

  CONCATENATE 'AP BUSINESS UNIT' 'VOUCHER ID' 'INVOICE ID'
              'INVOICE DATE' 'VENDOR ID' 'INVOICE AMOUNT'
              'INVOICE CURRENCY CODE' 'PAID AMOUNT'
              'PAYMENT CURRENCY CODE' 'VOUCHER LINE NUM'
              'SHIP TO ID' 'CATEGORY ID' 'ALLOCATION ID'
              'GL BUSINESS UNIT' 'ACCOUNT' 'PROJECT' 'PRODUCT'
              'AFFILIATE' 'COST CATEGORY' 'PROCESS' 'ALLOCATION AMOUNT'
              'LOCATION' 'DEPT ID' 'PAYMENT METHOD' 'CHECK NUMBER'
              'PAYMENT DATE' 'GL ACCOUNT' 'COST CENTER'
              'INTERNAL ORDER' 'WBS ELEMENT' 'NETWORK ACTIVITY'
              'ACTIVITY'
              INTO  lv_datarec SEPARATED BY delimtr.
  APPEND lv_datarec TO t_data.



  SELECT * FROM payr INTO s_payr
    WHERE zaldt = p_zaldt
    .

    IF sy-subrc = 0.
      CONCATENATE s_payr-zaldt+4(2) '/' s_payr-zaldt+6(2) '/'
      s_payr-zaldt(4) INTO lv_zaldt.
      PERFORM get_bkpf_data.
    ENDIF.
  ENDSELECT.     " SELECT FROM PAYR
ENDFORM.                    "get_data

*----------------------------------------------------------------------*
FORM get_bkpf_data.

  SELECT * FROM bsak INTO s_bsak
        WHERE bukrs = s_payr-zbukr
          AND lifnr = s_payr-lifnr
          AND augbl = s_payr-vblnr
          AND gjahr = s_payr-gjahr
          .

    CLEAR: s_bkpf.

    SELECT SINGLE * FROM bkpf INTO s_bkpf
        WHERE bukrs = s_bsak-bukrs
          AND belnr = s_bsak-belnr
          AND gjahr = s_bsak-gjahr
          AND blart = p_blart
      .


    IF sy-subrc = 0.
      CONCATENATE s_bkpf-bldat+4(2) '/' s_bkpf-bldat+6(2) '/'
      s_bkpf-bldat(4) INTO lv_bldat.
      PERFORM get_bseg_record.

      CLEAR lv_lifnr.

      LOOP AT t_bseg INTO s_bseg.

        CLEAR: lv_sgtxt, lv_vornr.
        SELECT SINGLE vornr
          FROM afvc
          INTO lv_vornr
          WHERE aufpl = s_bseg-aufpl
            AND aplzl = s_bseg-aplzl
          .


        lv_wrbtr = s_bseg-wrbtr.
        lv_rwbtr = abs( s_payr-rwbtr ).


        SPLIT s_bseg-sgtxt AT '-' INTO lv_null1 lv_sgtxt lv_null2.
        lv_blank = ''.

        "At each line item 1 get lifnr
        "Also skip line item 1.
        IF s_bseg-buzei = '001'.
          lv_lifnr = s_bseg-lifnr.
          CONTINUE.
        ENDIF.

        lv_hkont = s_bseg-hkont.
        lv_aufnr = s_bseg-aufnr.
        SELECT SINGLE poski
          FROM prps
          INTO lv_poski
          WHERE pspnr = s_bseg-projk
        .


        SHIFT lv_lifnr LEFT DELETING LEADING '0'.
        SHIFT lv_hkont LEFT DELETING LEADING '0'.
        SHIFT lv_aufnr LEFT DELETING LEADING '0'.
        SHIFT lv_poski LEFT DELETING LEADING '0'.

        IF lv_poski = '0'.
          lv_poski = ''.
        ENDIF.


        IF s_bseg-sgtxt(5) = 'Cimpl'.

          CONCATENATE s_bkpf-bukrs s_bkpf-belnr s_bkpf-xblnr lv_bldat
                      lv_lifnr lv_wrbtr s_bkpf-waers lv_rwbtr
                      s_payr-waers s_bseg-buzei lv_blank lv_blank
                      lv_sgtxt lv_blank lv_blank lv_blank
                      lv_blank lv_blank lv_blank lv_blank
                      lv_blank lv_blank lv_blank s_payr-rzawe
                      s_payr-chect lv_zaldt lv_hkont s_bseg-kostl
                      lv_aufnr lv_poski s_bseg-nplnr lv_vornr
                      INTO  lv_datarec SEPARATED BY delimtr.
          APPEND lv_datarec TO t_data.
        ENDIF.
      ENDLOOP.     " LOOP AT T_BSEG
    ENDIF.

  ENDSELECT.     " SELECT FROM BSAK





ENDFORM.                    "get_bkpf_data


*----------------------------------------------------------------------*
FORM get_bseg_record.
  CLEAR: t_bseg, s_bseg.
  SELECT *
    FROM bseg CLIENT SPECIFIED
    INTO s_bseg
    WHERE mandt = s_bkpf-mandt
      AND bukrs = s_bkpf-bukrs
      AND belnr = s_bkpf-belnr
      AND gjahr = s_bkpf-gjahr.

    APPEND s_bseg TO t_bseg.

  ENDSELECT.     " SELECT FROM
ENDFORM.                    "get_payr_record


*----------------------------------------------------------------------*
FORM output_data.

  IF lv_local = 0.
    PERFORM open_csvfile.

    LOOP AT t_data INTO lv_datarec.
      TRANSFER lv_datarec TO p_sfile.
    ENDLOOP.     " LOOP AT T_DATA

    PERFORM close_csvfile.

  ELSE.

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename                = p_lfile
      TABLES
        data_tab                = t_data
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        OTHERS                  = 22.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    WRITE: 'File Outputed Successfully to: ', p_lfile.

  ENDIF.

ENDFORM.                    "output_data




*----------------------------------------------------------------------*
FORM open_csvfile.
  OPEN DATASET p_sfile FOR OUTPUT IN TEXT MODE MESSAGE msg ENCODING
  DEFAULT.
  IF sy-subrc NE '0'.
    WRITE msg.
    STOP.
  ENDIF.
ENDFORM.                    "OPEN_CSVFILE


*----------------------------------------------------------------------*
FORM close_csvfile.
  CLOSE DATASET p_sfile.
  IF sy-subrc NE '0'.
    WRITE msg.
    STOP.
  ELSE.
    WRITE:/ 'File Outputed Successfully to: ', p_sfile.
  ENDIF.
ENDFORM.                    "CLOSE_ALL_FILES
*&---------------------------------------------------------------------*
*&      Form  GT_DATA_NEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM gt_data_new .

  DATA: lv_tabix      TYPE sy-tabix,
        lv_tabix1     TYPE sy-tabix,
        lv_wrbtr      TYPE string,
        lv_rwbtr      TYPE string,
        lv_bldat(10)  TYPE c,
        lv_zaldt(10)  TYPE c,
        lv_sgtxt      TYPE string,
        lv_null1      TYPE string,
        lv_null2      TYPE string,
        lv_lifnr      LIKE bseg-lifnr, " Acct Number of Vendor
        lv_vornr      LIKE afvc-vornr, " Oper./Activity Number
        lv_hkont      LIKE bseg-hkont, " GL Account
        lv_aufnr      LIKE bseg-aufnr, " Order Number
        lv_poski      LIKE prps-poski, " WBS Element
        lv_blank      TYPE string,
        lv_perc       TYPE i,
        lv_text       TYPE char100.
  CLEAR: gt_reguh,
         gt_bkpf,
         gt_bsak,
         gt_bseg,
         gt_payr,
         t_data.

  CONCATENATE 'AP BUSINESS UNIT' 'VOUCHER ID' 'INVOICE ID'
              'INVOICE DATE' 'VENDOR ID' 'INVOICE AMOUNT'
              'INVOICE CURRENCY CODE' 'PAID AMOUNT'
              'PAYMENT CURRENCY CODE' 'VOUCHER LINE NUM'
              'SHIP TO ID' 'CATEGORY ID' 'ALLOCATION ID'
              'GL BUSINESS UNIT' 'ACCOUNT' 'PROJECT' 'PRODUCT'
              'AFFILIATE' 'COST CATEGORY' 'PROCESS' 'ALLOCATION AMOUNT'
              'LOCATION' 'DEPT ID' 'PAYMENT METHOD' 'CHECK NUMBER'
              'PAYMENT DATE' 'GL ACCOUNT' 'COST CENTER'
              'INTERNAL ORDER' 'WBS ELEMENT' 'NETWORK ACTIVITY'
              'ACTIVITY'
              INTO  lv_datarec SEPARATED BY delimtr.
  APPEND lv_datarec TO t_data.

  lv_text = 'Extracting Payment Header Data..'.
  lv_perc = 2.
  PERFORM gui_progress USING lv_perc lv_text.

  SELECT * FROM reguh INTO TABLE gt_reguh WHERE zaldt = p_zaldt
                                            AND xvorl = space
                                            AND vblnr <> space.
  IF gt_reguh[] IS INITIAL.
    WRITE : / 'No data to process..'.
    STOP.
  ENDIF.
  lv_text = 'Extracting Payment Line Item Data..'.
  lv_perc = 10.
  PERFORM gui_progress USING lv_perc lv_text.
  SELECT * FROM regup INTO TABLE gt_regup FOR ALL ENTRIES IN gt_reguh
                                         WHERE laufd = gt_reguh-laufd
                                           AND laufi = gt_reguh-laufi
                                           AND xvorl = space
                                           AND zbukr = gt_reguh-zbukr
                                           AND lifnr = gt_reguh-lifnr
                                           AND kunnr = gt_reguh-kunnr
                                           AND empfg = gt_reguh-empfg
                                           AND vblnr = gt_reguh-vblnr
                                           AND blart = p_blart.
 IF gt_regup[] IS INITIAL.
    WRITE : / 'No Payment Line item data to process..'.
    STOP.
  ENDIF.
  lv_text = 'Extracting Check Data..'.
  lv_perc = 25.
  PERFORM gui_progress USING lv_perc lv_text.
  SELECT * FROM payr INTO TABLE gt_payr
                FOR ALL ENTRIES IN gt_reguh
                WHERE zbukr = gt_reguh-zbukr
                  AND hbkid = gt_reguh-hbkid
                  AND hktid = gt_reguh-hktid
                  AND rzawe = gt_reguh-rzawe
                  AND zaldt = gt_reguh-zaldt
                  AND vblnr = gt_reguh-vblnr.
  lv_text = 'Extracting Payment Items (Cleared) Data..'.
  lv_perc = 35.
  PERFORM gui_progress USING lv_perc lv_text.
SELECT * FROM bseg INTO TABLE gt_bseg
         FOR ALL ENTRIES IN gt_regup
         WHERE bukrs = gt_regup-bukrs
           AND belnr = gt_regup-belnr
           AND gjahr = gt_regup-gjahr.
  lv_text = 'Preparing Data for output..'.
  lv_perc = 50.
  PERFORM gui_progress USING lv_perc lv_text.
  sort gt_reguh by zbukr vblnr lifnr kunnr empfg.
  SORT gt_regup by bukrs belnr gjahr vblnr lifnr.
  SORT gt_payr BY zbukr hbkid hktid rzawe laufd laufi zaldt vblnr.
  SORT gt_bseg BY bukrs belnr gjahr buzei augbl lifnr.
  delete ADJACENT DUPLICATES FROM gt_regup COMPARING
                      bukrs belnr gjahr vblnr lifnr.
    LOOP AT gt_regup INTO gs_regup. " FROM lv_tabix.
      CLEAR: gs_bkpf,
             lv_tabix1,
             LV_LIFNR,
             gs_payr,
             gs_bsak,
             lv_tabix,
             gs_reguh.
    READ TABLE gt_reguh INTO gs_reguh with key "laufd = gs_regup-laufd
                                               "laufi = gs_regup-laufi
                                               zbukr = gs_regup-zbukr
                                               lifnr = gs_regup-lifnr
                                               kunnr = gs_regup-kunnr
                                               empfg = gs_regup-empfg
                                               vblnr = gs_regup-vblnr.
    CONCATENATE gs_reguh-zaldt+4(2) '/' gs_reguh-zaldt+6(2) '/'
                gs_reguh-zaldt(4) INTO lv_zaldt.
    READ TABLE gt_payr INTO gs_payr WITH KEY zbukr = gs_reguh-zbukr
                                             hbkid = gs_reguh-hbkid
                                             hktid = gs_reguh-hktid
                                             rzawe = gs_reguh-rzawe
                                             lifnr = gs_reguh-lifnr
                                             "laufd = gs_reguh-laufd
                                             "laufi = gs_reguh-laufi
                                             zaldt = gs_reguh-zaldt
                                             vblnr = gs_reguh-vblnr.
      CONCATENATE gs_regup-bldat+4(2) '/' gs_regup-bldat+6(2) '/'
                  gs_regup-bldat(4) INTO lv_bldat.
      READ TABLE gt_bseg WITH KEY bukrs = gs_regup-bukrs
                              "    augbl = gs_regup-vblnr
                                  belnr = gs_regup-belnr
                                  gjahr = gs_regup-gjahr
                                  "augbl = gs_regup-vblnr
                                  TRANSPORTING NO FIELDS.
      lv_tabix1 = sy-tabix.
      LOOP AT gt_bseg INTO gs_bseg FROM lv_tabix1.
        "clearing document can't be used because clearing document populate
        "only at vendor line item while other lines of document don't have \
        "clearing document.
        IF gs_bseg-bukrs <> gs_regup-bukrs OR
           gs_bseg-belnr <> gs_regup-belnr OR
           gs_bseg-gjahr <> gs_regup-gjahr. " OR
"           gs_bseg-augbl <> gs_regup-vblnr.
          EXIT.
        ENDIF.
        CLEAR: lv_sgtxt, lv_vornr, lv_poski.
        IF gs_bseg-aufpl IS NOT INITIAL AND
           gs_bseg-aplzl IS NOT INITIAL.
          SELECT SINGLE vornr FROM afvc
              INTO lv_vornr
               WHERE aufpl = gs_bseg-aufpl
               AND aplzl = gs_bseg-aplzl .
        ENDIF.
        lv_wrbtr = gs_bseg-wrbtr.
*              lv_rwbtr = ABS( gs_payr-rwbtr ).
        lv_rwbtr = abs( gs_regup-wrbtr ). "Paid amount
        SPLIT gs_bseg-sgtxt AT '-' INTO lv_null1 lv_sgtxt lv_null2.
        lv_blank = ''.
        "At each line item 1 get lifnr
        "Also skip line item 1.
        IF gs_bseg-buzei = '001'.
          lv_lifnr = gs_bseg-lifnr.
          CONTINUE.
        ENDIF.
        lv_hkont = gs_bseg-hkont.
        lv_aufnr = gs_bseg-aufnr.
        SELECT SINGLE poski FROM prps
               INTO lv_poski
               WHERE pspnr = gs_bseg-projk.
        IF lv_poski = '0'.
          lv_poski = ''.
        ENDIF.
        SHIFT lv_lifnr LEFT DELETING LEADING '0'.
        SHIFT lv_hkont LEFT DELETING LEADING '0'.
        SHIFT lv_aufnr LEFT DELETING LEADING '0'.
        SHIFT lv_poski LEFT DELETING LEADING '0'.
        IF gs_bseg-sgtxt(5) = 'Cimpl'.
          CONCATENATE gs_regup-bukrs gs_regup-belnr gs_regup-xblnr
                      lv_bldat lv_lifnr lv_wrbtr  gs_regup-waers
                      lv_rwbtr gs_regup-waers     gs_bseg-buzei
                      lv_blank lv_blank lv_sgtxt  lv_blank
                      lv_blank lv_blank lv_blank  lv_blank
                      lv_blank lv_blank lv_blank  lv_blank
                      lv_blank gs_regup-zwels     gs_payr-chect
                      lv_zaldt lv_hkont gs_bseg-kostl  lv_aufnr
                      lv_poski gs_bseg-nplnr lv_vornr
              INTO  lv_datarec SEPARATED BY delimtr.
          APPEND lv_datarec TO t_data.
        ENDIF.
*        CLEAR lv_lifnr.
      ENDLOOP. "Bseg Loop
    ENDLOOP. "REGUP

*  ENDLOOP. "Reguh
ENDFORM.                    " GT_DATA_NEW
*&---------------------------------------------------------------------*
*&      Form  GUI_PROGRESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_PERC  text
*      -->P_LV_TEXT  text
*----------------------------------------------------------------------*
FORM gui_progress  USING    p_perc TYPE i
                            p_text TYPE char100.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = p_perc
      text       = p_text.

ENDFORM.                    " GUI_PROGRESS
