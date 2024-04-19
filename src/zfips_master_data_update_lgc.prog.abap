*&---------------------------------------------------------------------*
*&  Include           ZFIPS_MASTER_DATA_UPDATE_LGC
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Program Name       :  ZFIPS_MASTER_DATA_UPDATE                      *
*& Author             :  KMB                                           *
*& Creation Date      :  19/11/2020                                    *
*& Object ID          :  CHG0203062                                    *
*& Application Area   :  FICO                                          *
*& Description        :  Optimization of PS master data update         *
*&-------------------------------------------------------------------- *
************************************************************************
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* 19-03-2020   KMB         D30K930800  CHG020306  Initial              *
************************************************************************

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
*Browse the path where file is stored
  CALL FUNCTION 'F4_FILENAME'         "Allows user to select path/file
        EXPORTING
             program_name  = syst-repid
             dynpro_number = syst-dynnr
             field_name    = 'p_file'
        IMPORTING
             file_name     = p_file.
  IF sy-subrc <> 0.
    MESSAGE text-002 TYPE gc_e.
  ENDIF.

START-OF-SELECTION.
  PERFORM f_file_upload.
  IF r_sf = gc_x.
    PERFORM f_fill_bdcdata_sf.
  ELSEIF r_est = gc_x.
    PERFORM f_fill_bdcdata_est.
  ELSEIF r_c55 = gc_x.
    PERFORM f_fill_bdcdata_c55.
  ENDIF.

END-OF-SELECTION.
  "Free all internal tables
  FREE: gt_record_c55, gt_record_sf, gt_record_est, gt_bdcdata, gt_msgtab.
