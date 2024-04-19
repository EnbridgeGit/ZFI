*&---------------------------------------------------------------------*
*&  Include           ZFI_MASS_VOID_MAIN
*&---------------------------------------------------------------------*
*&-----------------------------------------------------------------------*
*& Report Name          : ZFI_MASS_VOID_MAIN                             *
*& Author               : KMB                                            *
*& Creation Date        : 12-Jun-2019                                    *
*& Transport no.        : D30K929933                                     *
*& Object ID            : CHG0147638                                     *
*& Application Area     : FI                                             *
*& Description          : Mass voiding of cheques                        *
*&-----------------------------------------------------------------------*

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
*Browse the path where file is stored
  CALL FUNCTION 'F4_FILENAME'         "Allows user to select path/file
        EXPORTING
             program_name  = syst-repid
             dynpro_number = syst-dynnr
             field_name    = 'p_file'
        IMPORTING
             file_name     = p_file.

START-OF-SELECTION.
  PERFORM f_file_upload.
  IF r_fch8 = gc_x.
    PERFORM f_fill_bdcdata_fch8.
  ELSEIF r_fch9 = gc_x.
    PERFORM f_fill_bdcdata_fch9.
  ENDIF.

END-OF-SELECTION.
  "Free all internal tables
  FREE: gt_record, gt_bdcdata, gt_msgtab.


*&---------------------------------------------------------------------*
*&      Form  F_FILE_UPLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_file_upload .

  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
      i_line_header        = gc_x
      i_tab_raw_data       = gt_raw
      i_filename           = p_file
    TABLES
      i_tab_converted_data = gt_record
    EXCEPTIONS
      conversion_failed    = 1
      OTHERS               = 2.
  IF sy-subrc <> 0.
    MESSAGE text-000 TYPE gc_e.
  ENDIF.

ENDFORM.                    " F_FILE_UPLOAD
*&---------------------------------------------------------------------*
*&      Form  F_BDC_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0136   text
*      -->P_RECORD_BUDAT_007  text
*----------------------------------------------------------------------*
FORM f_bdc_field  USING fnam fval TYPE any.
  CLEAR: gs_bdcdata.
  gs_bdcdata-fnam = fnam.
  gs_bdcdata-fval = fval.
  WRITE fval TO gs_bdcdata-fval LEFT-JUSTIFIED.
  APPEND gs_bdcdata TO gt_bdcdata.
ENDFORM.                    " F_BDC_FIELD
*&---------------------------------------------------------------------*
*&      Form  F_BDC_DYNPRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0091   text
*      -->P_0092   text
*----------------------------------------------------------------------*
FORM f_bdc_dynpro  USING  program dynpro.
  CLEAR gs_bdcdata.
  gs_bdcdata-program = program.
  gs_bdcdata-dynpro = dynpro.
  gs_bdcdata-dynbegin = gc_x.
  APPEND gs_bdcdata TO gt_bdcdata.
ENDFORM.                    " F_BDC_DYNPRO
*&---------------------------------------------------------------------*
*&      Form  f_fill_bdcdata_fch8
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_fill_bdcdata_fch8 .
  LOOP AT gt_record INTO gs_record.

*read dataset dataset into record.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gs_record-hktid_003
      IMPORTING
        output = gs_record-hktid_003.

    CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT'
      EXPORTING
        input  = sy-datum
      IMPORTING
        output = gv_budat_007.


    PERFORM f_bdc_dynpro      USING 'SAPMFCHK' '0800'.
    PERFORM f_bdc_field       USING 'BDC_CURSOR'
                                  'RF05R-MONAT'.
    PERFORM f_bdc_field       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM f_bdc_field       USING 'PAYR-ZBUKR'
                                  gs_record-zbukr_001.
    PERFORM f_bdc_field       USING 'PAYR-HBKID'
                                  gs_record-hbkid_002.
    PERFORM f_bdc_field       USING 'PAYR-HKTID'
                                  gs_record-hktid_003.
    PERFORM f_bdc_field       USING 'PAYR-CHECT'
                                  gs_record-chect_004.
    PERFORM f_bdc_field       USING 'PAYR-VOIDR'
                                  gs_record-voidr_005.
    PERFORM f_bdc_field       USING 'UF05A-STGRD'
                                  gs_record-stgrd_006.
    PERFORM f_bdc_field       USING 'RF05R-BUDAT'
                                  gv_budat_007.
    PERFORM f_bdc_field       USING 'RF05R-MONAT'
                                  gs_record-monat_008.
    PERFORM f_bdc_dynpro      USING 'SAPMFCHK' '0800'.
    PERFORM f_bdc_field       USING 'BDC_CURSOR'
                                  'PAYR-ZBUKR'.
    PERFORM f_bdc_field       USING 'BDC_OKCODE'
                                  '=RAGL'.
    PERFORM f_bdc_field       USING 'PAYR-ZBUKR'
                                  gs_record-zbukr_001.
    PERFORM f_bdc_field       USING 'PAYR-HBKID'
                                  gs_record-hbkid_002.
    PERFORM f_bdc_field       USING 'PAYR-HKTID'
                                  gs_record-hktid_003.
    PERFORM f_bdc_field       USING 'PAYR-CHECT'
                                  gs_record-chect_004.
    PERFORM f_bdc_field       USING 'PAYR-VOIDR'
                                  gs_record-voidr_005.
    PERFORM f_bdc_field       USING 'UF05A-STGRD'
                                  gs_record-stgrd_006.
    PERFORM f_bdc_field       USING 'RF05R-BUDAT'
                                  gv_budat_007.
    PERFORM f_bdc_field       USING 'RF05R-MONAT'
                                  gs_record-monat_008.
    PERFORM f_bdc_dynpro      USING 'SAPMFCHK' '0800'.
    PERFORM f_bdc_field       USING 'BDC_OKCODE'
                                  '/EEND'.
    PERFORM f_bdc_field       USING 'BDC_CURSOR'
                                  'PAYR-ZBUKR'.
    CLEAR gt_msgtab.
    gv_dismode = p_dismod.
*Call Transaction
    CALL TRANSACTION gc_fch8                             "#EC CI_CALLTA
                  USING gt_bdcdata
                  MODE  gv_dismode
                  MESSAGES INTO gt_msgtab.               "#EC CI_CALLTA

    CALL FUNCTION 'ABAP4_COMMIT_WORK'.

    FREE : gt_bdcdata, gv_dismode.

*convert bdcmsgcoll to bapiret2
    CALL FUNCTION 'CONVERT_BDCMSGCOLL_TO_BAPIRET2'
      TABLES
        imt_bdcmsgcoll = gt_msgtab[]
        ext_return     = gt_bapiret2.

*Display messages from BAPIRET2
    CALL FUNCTION 'RSCRMBW_DISPLAY_BAPIRET2'
      TABLES
        it_return = gt_bapiret2.

    CLEAR gs_record.
  ENDLOOP.
ENDFORM.                    " f_fill_bdcdata_fch8
*&---------------------------------------------------------------------*
*&      Form  F_FILL_BDCDATA_FCH9
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_fill_bdcdata_fch9 .
  LOOP AT gt_record INTO gs_record.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gs_record-hktid_003
      IMPORTING
        output = gs_record-hktid_003.

    PERFORM f_bdc_dynpro      USING 'SAPMFCHK' '0800'.
    PERFORM f_bdc_field       USING 'BDC_CURSOR'
                                  'PAYR-VOIDR'.
    PERFORM f_bdc_field       USING 'BDC_OKCODE'
                                  '=EDEL'.
    PERFORM f_bdc_field       USING 'PAYR-ZBUKR'
                                  gs_record-zbukr_001.
    PERFORM f_bdc_field       USING 'PAYR-HBKID'
                                  gs_record-hbkid_002.
    PERFORM f_bdc_field       USING 'PAYR-HKTID'
                                  gs_record-hktid_003.
    PERFORM f_bdc_field       USING 'PAYR-CHECT'
                                  gs_record-chect_004.
    PERFORM f_bdc_field       USING 'PAYR-VOIDR'
                                  gs_record-voidr_005.
    gv_dismode = p_dismod.
*Call Transaction
    CALL TRANSACTION gc_fch9                             "#EC CI_CALLTA
               USING gt_bdcdata
               MODE  gv_dismode
               MESSAGES INTO gt_msgtab.               "#EC CI_CALLTA

    CALL FUNCTION 'ABAP4_COMMIT_WORK'.

    FREE : gt_bdcdata, gv_dismode.

*convert bdcmsgcoll to bapiret2
    CALL FUNCTION 'CONVERT_BDCMSGCOLL_TO_BAPIRET2'
      TABLES
        imt_bdcmsgcoll = gt_msgtab[]
        ext_return     = gt_bapiret2.

*Display messages from BAPIRET2
    CALL FUNCTION 'RSCRMBW_DISPLAY_BAPIRET2'
      TABLES
        it_return = gt_bapiret2.

    CLEAR gs_record.
  ENDLOOP.
ENDFORM.                    " F_FILL_BDCDATA_FCH9
