REPORT  zfapr012_ebill_pay_reconcile MESSAGE-ID zs.
************************************************************************
*  Client:     Spectra Energy.                                         *
*  Date:       January 2011                                            *
*  Author:     Brian Boundy                                            *
*  Program Description:                                                *
*  Extract for E-Billing Payment reconciliation.                       *
*                                                                      *
************************************************************************
*CHANGES:                                                              *
* Date       By       Issue Description                                *
* 2012/07/31 M Khan   TR995 Change C: drive to H: drive with           *
*                           directory and file selection using F4      *
* 2020/06/24 AHMADT   D30K930616 CHG0184039 Output file format changed *
*                     D30K930626                                       *
*                     D30K930630                                       *
*                     D30K930641                                       *
* 2020/07/23 AHMADT   D30K930643 CHG0187953 Changed Date field format  *
*                                and output file naming convention     *
*                     D30K930647                                       *
*                     D30K930649                                       *
* 2021/02/10 AHMADT   D30K930870 Team connect fie rename               *
************************************************************************

*  Start of changes by AHMADT for CHG0187953
TABLES : payr.
*  End of changes by AHMADT for CHG0187953

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
                lv_wrbtr1     TYPE          string, " Added by akmadasu CHG0184039
                lv_count1     TYPE          i,      " Added by akmadasu CHG0184039
                lv_index      TYPE          i,      " Added by akmadasu CHG0184039
                lv_rwbtr      TYPE          string,
                lv_bldat(10)  TYPE          c,
                lv_bldat1(8)  TYPE          c,      " Added by AHAMDT for CHG0187953
                lv_zaldt1(8)   TYPE         c,      " Added by AHMADT for CHG0187953
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




SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME.
PARAMETERS:
  p_blart   LIKE bkpf-blart DEFAULT 'K3'.
*  Start of changes by AHMADT for CHG0187953
*  p_zaldt   LIKE payr-zaldt.                  " Probable Payment Date (Cash Discount 1 Due)
  SELECT-OPTIONS : s_zaldt   FOR  payr-zaldt.
*  End of changes by AHMADT for CHG0187953
SELECTION-SCREEN END OF BLOCK a1.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
PARAMETERS:
    p_local   RADIOBUTTON GROUP rad1 DEFAULT 'X',
    p_lfile    TYPE        string,
    p_server  RADIOBUTTON GROUP rad1,
    p_sfile   LIKE        rfpdo-rfbifile.     " Name of the Batch Input File Path
SELECTION-SCREEN END OF BLOCK b1.


INITIALIZATION.
  CONCATENATE '/usr/sap/interfaces/' sy-sysid+0(3)
**--START OF CHANGES BY AKMADASU CHG0184039
*              '/IFAPR012/paymentrec' sy-datum sy-uzeit '.dat'
* Start of changes by AHMADT for CHG0187953
**               '/IFAPR012/UG_paymentrec' sy-datum sy-uzeit '.dat'
* Start of changes by AHMADT for CHG0199538
*                 '/EBILLING/EnbridgePAY_TeamConnectSAP_' sy-datum sy-uzeit '.txt'
                 '/EBILLING/EnbridgePAY_TeamConnectSAP_' sy-sysid sy-datum sy-uzeit '.txt'
* End of changes by AHMADT for CHG0199538
* End of changes by AHAMDT for CHG0187953
**--END OF CHANGES BY AKMADASU CHG0184039
              INTO p_sfile.

*Start of TR995 chnages
* CONCATENATE  'C:\SAPTEMP\EBILL_RECONCILE' sy-datum sy-uzeit '.dat'
* Start of changes by AHMADT for CHG0187953
*  CONCATENATE  'H:\SAPTEMP\EBILL_RECONCILE' sy-datum sy-uzeit '.dat'
*               INTO p_lfile.
* Start of changes by AHMADT for CHG0199538
*  CONCATENATE  'H:\SAPTEMP\EnbridgePAY_TeamConnectSAP_' sy-datum sy-uzeit '.txt'
  CONCATENATE  'H:\SAPTEMP\EnbridgePAY_TeamConnectSAP_' sy-sysid sy-datum sy-uzeit '.txt'
* End of changes by AHMADT for CHG0199538
               INTO p_lfile.
* End of changes by AHAMDT for CHG0187953
*Ene of TR995 Changes

*  Start of changes by AHMADT for CHG0187953
*  MOVE sy-datum TO p_zaldt.
   MOVE sy-datum TO s_zaldt-low.
   MOVE sy-datum TO s_zaldt-high.
   APPEND s_zaldt TO s_zaldt[].
*  End of changes by AHMADT for CHG0187953

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
  PERFORM get_data.
  PERFORM output_data.




*----------------------------------------------------------------------*
FORM get_data.

  CLEAR: t_data.
**--START OF CHANGES BY AKMADASU CHG0184039
*  CONCATENATE 'AP BUSINESS UNIT' 'VOUCHER ID' 'INVOICE ID'
*              'INVOICE DATE' 'VENDOR ID' 'INVOICE AMOUNT'
*              'INVOICE CURRENCY CODE' 'PAID AMOUNT'
*              'PAYMENT CURRENCY CODE' 'VOUCHER LINE NUM'
*              'SHIP TO ID' 'CATEGORY ID' 'ALLOCATION ID'
*              'GL BUSINESS UNIT' 'ACCOUNT' 'PROJECT' 'PRODUCT'
*              'AFFILIATE' 'COST CATEGORY' 'PROCESS' 'ALLOCATION AMOUNT'
*              'LOCATION' 'DEPT ID' 'PAYMENT METHOD' 'CHECK NUMBER'
*              'PAYMENT DATE' 'GL ACCOUNT' 'COST CENTER'
*              'INTERNAL ORDER' 'WBS ELEMENT' 'NETWORK ACTIVITY'
*              'ACTIVITY'
*              INTO  lv_datarec SEPARATED BY delimtr.
*  APPEND lv_datarec TO t_data.

  CONCATENATE 'INVOICE_NUMBER'
             'VENDOR_NUM'
             'INVOICE_DATE'
             'PAYMENT_NUMBER'
             'PAYMENT_DATE'
             'PAYMENT_AMOUNT'
             'CHECK_AMOUNT'
           INTO lv_datarec SEPARATED BY delimtr.
  APPEND lv_datarec TO t_data.
  CLEAR: lv_datarec.
**--END OF CHANGES BY AKMADASU CHG0184039


  SELECT * FROM payr INTO s_payr
*  Start of changes by AHMADT for CHG0187953
*                   WHERE zaldt = p_zaldt.
                    WHERE zaldt IN s_zaldt.
*  End of changes by AHMADT for CHG0187953


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
**--START OF CHANGES BY AKMADASU CHG0184039
      DESCRIBE TABLE t_bseg LINES lv_count1.
**--end OF CHANGES BY AKMADASU CHG0184039
      LOOP AT t_bseg INTO s_bseg.
        lv_index = lv_index + 1. " Added BY AKMADASU CHG0184039
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


        IF s_bseg-sgtxt(6) = 'ELegal'
**--START OF CHANGES BY AKMADASU CHG0184039
        OR s_bseg-sgtxt(5) = 'Ebill'.
*          CONCATENATE s_bkpf-bukrs s_bkpf-belnr s_bkpf-xblnr lv_bldat
*                      lv_lifnr lv_wrbtr s_bkpf-waers lv_rwbtr
*                      s_payr-waers s_bseg-buzei lv_blank lv_blank
*                      lv_sgtxt lv_blank lv_blank lv_blank
*                      lv_blank lv_blank lv_blank lv_blank
*                      lv_blank lv_blank lv_blank s_payr-rzawe
*                      s_payr-chect lv_zaldt lv_hkont s_bseg-kostl
*                      lv_aufnr lv_poski s_bseg-nplnr lv_vornr
*                      INTO  lv_datarec SEPARATED BY delimtr.
*          APPEND lv_datarec TO t_data.
          lv_wrbtr1 = lv_wrbtr1 + lv_wrbtr.

*   Start of changes by AHMADT for CHG0187953
        CONCATENATE lv_bldat+6(4)
                    lv_bldat+0(2)
                    lv_bldat+3(2)
               INTO lv_bldat1.
        CONCATENATE lv_zaldt+6(4)
                    lv_zaldt+0(2)
                    lv_zaldt+3(2)
               INTO lv_zaldt1.
*   End of changes by AHAMDT for CHG0187953
          IF lv_count1 = lv_index.
*   Start of changes by Akmadasu for CHG0187953
            CONDENSE: lv_wrbtr1 NO-GAPS,
                      lv_rwbtr  NO-GAPS.
*   Start of changes by Akmadasu for CHG0187953
            CONCATENATE s_bkpf-xblnr
                        lv_lifnr
*   Start of changes by AHMADT for CHG0187953
*                        lv_bldat
                        lv_bldat1
*   End of changes by AHAMDT for CHG0187953
                        s_payr-chect
*   Start of changes by AHMADT for CHG0187953
*                        lv_zaldt
                        lv_zaldt1
*   End of changes by AHAMDT for CHG0187953
                        lv_wrbtr1
                        lv_rwbtr
            INTO        lv_datarec SEPARATED BY delimtr.
            APPEND lv_datarec TO t_data.
            CLEAR: lv_datarec,lv_count1,lv_index, lv_wrbtr1,
                   lv_bldat1, lv_zaldt1. "Added by AHAMDT for CHG0187953
          ENDIF.
**--END OF CHANGES BY AKMADASU CHG0184039
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
