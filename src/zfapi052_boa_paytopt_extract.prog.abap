REPORT zfapi052_boa_paytopt_extract NO STANDARD PAGE HEADING
                                    LINE-SIZE 255 MESSAGE-ID ZACC.
*----------------------------------------------------------------------*
*   Report  ZFAPI052_BOA_PAYTOPT_EXTRACT
*
*   Date:       November, 2013
*   Author:     Glenn Ymana
*   Request:    SDP57595
*
*   Program Desc: This program will download AP data for Bank of
*                 America Payment optimization
*----------------------------------------------------------------------*
* Change History
*
*----------------------------------------------------------------------*

TABLES: lfa1,         "Vendor Master (General Section)
        bsak,         "Secondary Index for Vendors (Cleared Items)
        reguh,        "Settlement data from payment program
        bkpf,         "Document Header Table
        rseg.         "Document Item table: Incoming Invoice

TYPES: i_rec TYPE string.

DATA: itab_bsak LIKE bsak OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF itab_lfa1 OCCURS 0,
          lifnr LIKE lfa1-lifnr,
          name1 LIKE lfa1-name1,
          stras LIKE lfa1-stras,
          ort01 LIKE lfa1-ort01,
          regio LIKE lfa1-regio,
          pstlz LIKE lfa1-pstlz,
          land1 LIKE lfa1-land1,
          stcd1 LIKE lfa1-stcd1,
          stcd2 LIKE lfa1-stcd2,
      END OF itab_lfa1.

DATA: BEGIN OF itab_apdata OCCURS 0,
          mandt      LIKE sy-mandt,
          lifnr      LIKE lfa1-lifnr,
*          SITEID(30) TYPE C,
          empfb      LIKE bsak-empfb,
          name1      LIKE lfa1-name1,
          stras      LIKE lfa1-stras,
          ort01      LIKE lfa1-ort01,
          regio      LIKE lfa1-regio,
          pstlz      LIKE lfa1-pstlz,
          land1      LIKE lfa1-land1,
          taxid      LIKE lfa1-stcd1,
          augdt      LIKE bsak-augdt,
          WRBTR      LIKE bsak-WRBTR,
          rzawe      LIKE reguh-rzawe,
          zlsch      LIKE bsak-zlsch,
          xblnr      LIKE bkpf-xblnr,
          ebeln      LIKE rseg-ebeln,
          bukrs      LIKE bsak-bukrs,
          zterm      LIKE bsak-zterm,
          belnr      LIKE bsak-belnr,
          gjahr      LIKE bsak-gjahr,
          augbl      LIKE bsak-augbl,
          waers      LIKE bsak-waers,
      END OF itab_apdata.

DATA: BEGIN OF mat_header,
          matnr(8)  TYPE c VALUE 'Material',
          zzloc(1)  TYPE c VALUE 'L',
          lvorm(1)  TYPE c VALUE 'D',
          mtart(4)  TYPE c VALUE 'Mtyp',
          matkl(6)  TYPE c VALUE 'MatGrp',
          bismt(6)  TYPE c VALUE 'OldMat',
          meins(2)  TYPE c VALUE 'BM',
          mstae(2)  TYPE c VALUE 'MS',
          bstme(3)  TYPE c VALUE 'PBM',
          maktx(11) TYPE c VALUE 'Description',
          mfrpn(8)  TYPE c VALUE 'ManPrNum',
          zcdata(6) TYPE c VALUE 'ManNam',
          zquty(2)  TYPE c VALUE 'TS',
          zmpot(9)  TYPE c VALUE 'MatPOText',
          zirct(3)  TYPE c VALUE 'NIR',
          zclct(3)  TYPE c VALUE 'NCL',
      END OF mat_header.

DATA: mt_tab TYPE STANDARD TABLE OF i_rec,
      m_rec TYPE i_rec.

DATA: wa_dmbtr(20)      TYPE c,
      out_string        TYPE string.

DATA: TABCHAR(1) TYPE c VALUE cl_abap_char_utilities=>HORIZONTAL_TAB.
DATA: msg_text(50).

************************************************************************
*                   Selection Screen                                   *
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK box1 WITH FRAME TITLE text-000.
SELECT-OPTIONS: s_bukrs FOR bsak-bukrs OBLIGATORY,   "Company Code
                s_ktokk FOR lfa1-ktokk OBLIGATORY.     "Account Group
SELECT-OPTIONS: s_blart FOR bsak-blart NO INTERVALS, "Document Type
                s_tcode FOR bkpf-tcode NO INTERVALS, "Transaction Code
                s_lifnr FOR lfa1-lifnr,              "Vendor No.
                s_augdt FOR sy-datum OBLIGATORY.     "Clearing Date

SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF BLOCK box2 WITH FRAME TITLE text-003.
PARAMETERS: p_locfil LIKE filename-fileextern.
SELECTION-SCREEN END OF BLOCK box2.
SELECTION-SCREEN END OF BLOCK box1.

************************************************************************
*                   INITIALIZATION                                     *
************************************************************************
INITIALIZATION.
  CONCATENATE 'H:/SAPTEMP/' 'BoAPayOpt.txt' INTO p_locfil.

************************************************************************
*                 AT SELECTION-SCREEN                                  *
************************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_locfil.
  PERFORM search_filename USING p_locfil.

************************************************************************
*                   START-OF-SELECTION                                 *
************************************************************************
START-OF-SELECTION.

  PERFORM collect_lfa1_bsak_data.

************************************************************************
*                END-OF-SELECTION                                      *
************************************************************************
END-OF-SELECTION.
  PERFORM create_file.

************************************************************************
*                   COLLECT_LFA1_BSAK_DATA                             *
************************************************************************
FORM collect_lfa1_bsak_data.

data: lv_gjahr TYPE bsak-gjahr.

  REFRESH: itab_lfa1, itab_bsak, itab_apdata.
  CLEAR:   itab_lfa1, itab_bsak.

  SELECT lifnr name1 stras ort01 regio pstlz land1 stcd1 stcd2
    INTO itab_lfa1 FROM lfa1
   WHERE ktokk IN s_ktokk
     AND lifnr IN s_lifnr
   ORDER BY lifnr.

    IF sy-subrc = 0.
      APPEND itab_lfa1.
    ENDIF.
  ENDSELECT.

  LOOP AT itab_lfa1.

    CLEAR itab_apdata.
    MOVE-CORRESPONDING itab_lfa1 TO itab_apdata.

    IF itab_lfa1-stcd1 IS NOT INITIAL.
      MOVE itab_lfa1-stcd1 TO itab_apdata-taxid.
    ELSEIF itab_lfa1-stcd2 IS NOT INITIAL.
      MOVE itab_lfa1-stcd2 TO itab_apdata-taxid.
    ENDIF.

    SELECT * FROM bsak
     WHERE bukrs IN s_bukrs
       AND lifnr EQ itab_lfa1-lifnr
       AND augdt IN s_augdt
       AND blart IN s_blart.

      IF sy-subrc = 0.
        lv_gjahr = bsak-augdt(4).
        SELECT * FROM bkpf
          WHERE bukrs = bsak-bukrs
            AND belnr = bsak-augbl
            AND gjahr = lv_gjahr
            AND tcode IN s_tcode
            AND bstat EQ ' '.

          IF sy-subrc = 0.
            MOVE-CORRESPONDING bsak TO itab_apdata.
            PERFORM collect_reguh_data.
            PERFORM collect_bkpf_data.
            APPEND itab_apdata.
          ENDIF.
        ENDSELECT.

      ENDIF.
    ENDSELECT.

  ENDLOOP.
ENDFORM.                    "COLLECT_LFA1_BSAK_DATA

************************************************************************
*                   COLLECT_REGUH_DATA
************************************************************************
FORM collect_reguh_data.

  CLEAR itab_apdata-rzawe.

  SELECT * FROM reguh
   WHERE zbukr EQ bsak-bukrs
     AND lifnr EQ bsak-lifnr
     AND vblnr EQ bsak-augbl
     AND grpno EQ 0.

    IF sy-subrc = 0.
      MOVE reguh-rzawe TO itab_apdata-rzawe.
    ENDIF.

  ENDSELECT.

ENDFORM.                    "COLLECT_REGUH_DATA

************************************************************************
*                   COLLECT_BKPF_DATA
************************************************************************
FORM collect_bkpf_data.

  CLEAR itab_apdata-xblnr.

  SELECT * FROM bkpf
   WHERE bukrs EQ bsak-bukrs
     AND budat EQ bsak-budat
     AND belnr EQ bsak-belnr.

    IF sy-subrc = 0.
      MOVE bkpf-xblnr TO itab_apdata-xblnr.
      IF bkpf-awtyp = 'RMRP'.
        PERFORM collect_rseg_data.
      ENDIF.
    ENDIF.
  ENDSELECT.

ENDFORM.                    "COLLECT_BKPF_DATA

************************************************************************
*                   COLLECT_RSEG_DATA
************************************************************************
FORM collect_rseg_data.

  DATA: w_belnr(10)  TYPE c,
        w_gjahr(4)   TYPE c.

  MOVE bkpf-awkey+0(10) TO w_belnr.
  MOVE bkpf-awkey+10(4) TO w_gjahr.

  CLEAR itab_apdata-ebeln.

  SELECT SINGLE * FROM rseg
   WHERE belnr EQ w_belnr
     AND gjahr EQ w_gjahr.

  IF sy-subrc = 0.
    MOVE rseg-ebeln TO itab_apdata-ebeln.
  ENDIF.

ENDFORM.                    "COLLECT_RSEG_DATA

************************************************************************
*      CREATE FILE                                                     *
************************************************************************
FORM create_file.

  DATA: l_file TYPE string,
        l_subrc TYPE sy-subrc.

*   PERFORM GENERATE_HEADERS.

  LOOP AT itab_apdata.

*   CLEAR: MAT_STRING, CLASS_STRING, IR_STRING, OUT_STRING.

*  WRITE ITAB_APDATA RECORDS

    MOVE itab_apdata-wrbtr TO wa_dmbtr.

    CONCATENATE 'UG' itab_apdata-lifnr itab_apdata-empfb itab_apdata-name1
                itab_apdata-stras itab_apdata-ort01 itab_apdata-regio
                itab_apdata-pstlz itab_apdata-land1 itab_apdata-taxid
                itab_apdata-augdt wa_dmbtr itab_apdata-rzawe
                itab_apdata-zlsch
                itab_apdata-xblnr itab_apdata-ebeln itab_apdata-bukrs
                itab_apdata-zterm itab_apdata-belnr itab_apdata-gjahr
                itab_apdata-augbl itab_apdata-waers
                INTO out_string SEPARATED BY tabchar.

    m_rec = out_string.
    APPEND m_rec TO mt_tab.

  ENDLOOP.


*- Create Local File

  l_file = p_locfil.

  CALL METHOD cl_gui_frontend_services=>gui_download
    EXPORTING
      filename                = l_file
      filetype                = 'ASC'
    CHANGING
      data_tab                = mt_tab
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
      not_supported_by_gui    = 22
      error_no_gui            = 23
      OTHERS                  = 24.

  IF SY-SUBRC EQ 0.
    MESSAGE I101 WITH 'File Created Successfully'.
  ELSEIF SY-SUBRC EQ 15.
    MESSAGE I101 WITH 'Access denied creating the file'.
  ELSEIF SY-SUBRC NE 0.
    MESSAGE I101 WITH 'Error opening physical file' P_LOCFIL.
  ENDIF.

  ENDFORM.                    "CREATE_FILE

************************************************************************
*      SEARCH_FILENAME                                                 *
************************************************************************
FORM search_filename USING p_file.

  DATA: l_dir TYPE string,
        l_rc TYPE i,
        l_filetable TYPE filetable.

  l_dir = 'G:\'.

*- Call Method to retrieve filename
  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = 'Select a file...'
      default_extension       = 'CSV'
      initial_directory       = l_dir
    CHANGING
      file_table              = l_filetable
      rc                      = l_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  CHECK sy-subrc EQ 0.
  READ TABLE l_filetable INDEX 1 INTO p_file.

ENDFORM.                    " SEARCH_FILENAME
