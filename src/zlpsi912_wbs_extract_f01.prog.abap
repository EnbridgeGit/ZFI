*&---------------------------------------------------------------------*
*&  Include           ZLPSI912_WBS_EXTRACT_F01
*&---------------------------------------------------------------------*
*& This include has been copied from include ZLPSI912_WBS_EXTRACT_F01
*& for CR CHG0172237 by AHMADT on 13/02/2020
************************************************************************
*                            Spectra Energy                            *
************************************************************************
*  Program:          ZLPSI008_WBS_EXTRACT                              *
*  Include:          ZLPSI008_WBS_EXTRACT_F01                          *
*  Author:           John Hartung                                      *
*  Date:             September 9, 2016                                 *
*  Ticket#:          ACR-2338                                          *
*  Application Area: FICO PS                                           *
*                                                                      *
*  Description:      PS WBS Extract including Long Text                *
*                    Forms Include - Subroutines                       *
*                                                                      *
************************************************************************
*----------------------- CHANGE LOG -----------------------------------*
*  Date    By        Description                                       *
* -------- --------- ------------------------------------------------- *
* 09/09/16 JRHARTUNG DECK916888 - ACR-2338 - Initial program           *
*                    DECK916894, DECK916910, DECK917113, DECK917151,   *
*                    DECK917204                                        *
*----------------------------------------------------------------------*
* 13/02/20 AHMADT    D30K930427  CHG0172237                            *
*                    Report copied from DEC.Commented all custom fields*
*                    and their logic of structure PRPS_R which were    *
*                    enhanced by using structure CI_PRPS.              *
* 12/10/20 AHMADT    D30K930709  CHG0172237                            *
*                    Added logic to create multiple files for every    *
*                    100000 records.                                   *
************************************************************************

*eject
*&---------------------------------------------------------------------*
*&      Form  f_select_xparam
*&---------------------------------------------------------------------*
*       Select the program parameter values
*----------------------------------------------------------------------*
FORM f_select_xparam.

  CLEAR    gt_xparam[].
  CLEAR    gv_obj_id.
  CLEAR    p_objid.

  SELECT   *
    INTO   TABLE gt_xparam
    FROM   zfit_xparam
   WHERE   paramtype = gc_param_out_int
     AND   subtype   = gc_param_obj_id.
  IF     ( sy-subrc EQ 0 ).
    SORT   gt_xparam           ASCENDING BY mandt paramtype subtype
                                            key1 key2 key3 key4 key5.
  ELSE.
    CLEAR  gt_xparam[].
    CLEAR  gv_obj_id.
    CLEAR  p_objid.
  ENDIF.

ENDFORM.                    " f_select_xparam
*eject
*&---------------------------------------------------------------------*
*&      Form  f_default_sel_screen_values
*&---------------------------------------------------------------------*
*       Default the selection screen values
*----------------------------------------------------------------------*
FORM f_default_sel_screen_values.

  DATA:    ls_xparam                   TYPE ty_wa_xparam.

* Set the object id and description
  IF     ( p_objid                       IS INITIAL ).

    CLEAR                                   ls_xparam.
    READ     TABLE gt_xparam           INTO ls_xparam
                                   WITH KEY key1 = gc_param_object.
    IF     ( sy-subrc EQ 0 ).
      CLEAR                                 p_objid.
      MOVE     ls_xparam-value1          TO p_objid.
      CLEAR                                 gv_obj_id.
      MOVE     ls_xparam-value1          TO gv_obj_id.
      CLEAR                                 p_objdsc.
      MOVE     ls_xparam-value2          TO p_objdsc.
    ENDIF.

  ENDIF.

* Set the application server outbound filepaths and filenames
  CLEAR                                     ls_xparam.
  READ     TABLE gt_xparam             INTO ls_xparam
                                   WITH KEY key1 = gc_param_fp_outb.
  IF   ( ( sy-subrc EQ 0 ) AND ( ls_xparam-value1 IS NOT INITIAL ) ).
    CLEAR                                   p_asfpof.
    MOVE   ls_xparam-value1              TO p_asfpof.
    CLEAR                                   p_asfnof.
    MOVE   ls_xparam-value2              TO p_asfnof.
  ENDIF.
  CLEAR                                     ls_xparam.
  READ     TABLE gt_xparam             INTO ls_xparam
                                   WITH KEY key1 = gc_param_fp_outa.
  IF   ( ( sy-subrc EQ 0 ) AND ( ls_xparam-value1 IS NOT INITIAL ) ).
    CLEAR                                   p_asfpaf.
    MOVE   ls_xparam-value1              TO p_asfpaf.
  ENDIF.

ENDFORM.                    " f_default_sel_screen_values
*eject
*&---------------------------------------------------------------------*
*&      Form  f_format_sel_screen_layout
*&---------------------------------------------------------------------*
*       Format the selection screen layout
*----------------------------------------------------------------------*
FORM f_format_sel_screen_layout.

  LOOP AT SCREEN.

* Set screen fields to display only
    IF     ( screen-group1               EQ gc_modif_id_dsp ).
      screen-input = 0.
      MODIFY   SCREEN.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " f_format_sel_screen_layout
*eject
*&---------------------------------------------------------------------*
*&      Form  f_initial_data_elements
*&---------------------------------------------------------------------*
*       Initial the data elements
*----------------------------------------------------------------------*
FORM f_initial_data_elements.

  CLEAR    gt_t001[].
  CLEAR    gt_t001w[].
  CLEAR    gt_tcj1t[].
*  Start of changes by AHMADT for CHG0172237
*  CLEAR    gt_budcat_val[].
*  CLEAR    gt_cmpsta_val[].
*  CLEAR    gt_mtrsta_val[].
*  CLEAR    gt_pl_val_pval[].
*  End of changes by AHMADT for CHG0172237

  CLEAR    gs_proj.
  CLEAR    gs_proj_p.
  CLEAR    gs_prps.
  CLEAR    gs_pstat.
  CLEAR    gs_t001.
  CLEAR    gs_t001w.
  CLEAR    gs_tcj1t.
*  Start of changes by AHMADT for CHG0172237
*  CLEAR    gs_budcat_val.
*  CLEAR    gs_cmpsta_val.
*  CLEAR    gs_mtrsta_val.
*  CLEAR    gs_pl_val_pval.
*  End of changes by AHMADT for CHG0172237

  CLEAR    gv_filename.
  CLEAR    gv_count_recs.

* Select the company codes
  SELECT   bukrs  butxt
    INTO   TABLE gt_t001
    FROM   t001
  ORDER BY bukrs.
  IF     ( sy-subrc NE 0 ).
    CLEAR  gt_t001[].
  ENDIF.

* Select the plants
  SELECT   werks  name1
    INTO   TABLE gt_t001w
    FROM   t001w
  ORDER BY werks.
  IF     ( sy-subrc NE 0 ).
    CLEAR  gt_t001w[].
  ENDIF.

* Select the project types
  SELECT   prart  pratx
    INTO   TABLE gt_tcj1t
    FROM   tcj1t
   WHERE   langu = 'E'
  ORDER BY prart.
  IF     ( sy-subrc NE 0 ).
    CLEAR  gt_tcj1t[].
  ENDIF.
*  Start of changes by AHMADT for CHG0172237
*eject
* Select the budget category/topics
*  SELECT   cvalue  cdescription
*    INTO   TABLE gt_budcat_val
*    FROM   zpst_budcat_val
*  ORDER BY cvalue.
*  IF     ( sy-subrc NE 0 ).
*    CLEAR  gt_budcat_val[].
*  ENDIF.

** Select the area descriptions
*  SELECT   ovalue  odescription
*    INTO   TABLE gt_cmpsta_val
*    FROM   zpst_cmpsta_val
*  ORDER BY ovalue.
*  IF     ( sy-subrc NE 0 ).
*    CLEAR  gt_cmpsta_val[].
*  ENDIF.

* Select the budget year descriptions
*  SELECT   mvalue  mdescription
*    INTO   TABLE gt_mtrsta_val
*    FROM   zpst_mtrsta_val
*  ORDER BY mvalue.
*  IF     ( sy-subrc NE 0 ).
*    CLEAR  gt_mtrsta_val[].
*  ENDIF.

* Select the region descriptions
*  SELECT   pvalue  pdescription
*    INTO   TABLE   gt_pl_val_pval
*    FROM   zpst_pl_val_pval
*  ORDER BY pvalue.
*  IF     ( sy-subrc NE 0 ).
*    CLEAR  gt_pl_val_pval[].
*  ENDIF.
*  End of changes by AHMADT for CHG0172237

ENDFORM.                    " f_initial_data_elements
*eject
*&---------------------------------------------------------------------*
*&      Form  f_open_output_dataset
*&---------------------------------------------------------------------*
*       Open the output dataset.
*----------------------------------------------------------------------*
FORM f_open_output_dataset
  CHANGING cv_filename                 TYPE text255
           cv_filename1                TYPE text255
           cv_subrc                    TYPE sysubrc.

  DATA:    ls_out_txt                  TYPE ty_wa_out_txt.

  DATA:    lv_filename                 TYPE text255,
           lv_filename1                TYPE text255,
           lv_string                   TYPE STRING,
           lv_rc                       TYPE numc3.

  CLEAR    cv_filename.
  CLEAR    cv_subrc.

  CLEAR                                     lv_filename.
  MOVE     p_asfnof                      TO lv_filename.
  IF     ( lv_filename                   CS 'YYYYMMDD' ).
    REPLACE  'YYYYMMDD'                  IN lv_filename
                                       WITH gv_datum_run.
  ENDIF.
  IF     ( lv_filename                   CS 'HHMMSS' ).
    REPLACE  'HHMMSS'                    IN lv_filename
                                       WITH gv_uzeit_run.
  ENDIF.
  CONCATENATE                               p_asfpaf
                                            lv_filename
                                       INTO lv_filename.
lv_filename1 = lv_filename.
REPLACE all OCCURRENCES OF 'Arch/' in lv_filename1 with space.
*  Start of changes by AHMADT for CHG0172237
REPLACE ALL OCCURRENCES OF '.txt' IN lv_filename  WITH '_1.txt'.
*  End of changes by AHMADT for CHG0172237
  OPEN     DATASET lv_filename FOR OUTPUT
                                IN TEXT MODE
                          ENCODING DEFAULT.
  cv_subrc = sy-subrc.

  IF       ( cv_subrc EQ 0 ).
    MOVE     lv_filename                 TO cv_filename.
    WRITE: /                     text-m01,  lv_filename.
    MESSAGE  s000(zfi01)    WITH text-m01   lv_filename.
  ELSE.
    lv_rc = cv_subrc.
    WRITE: /                     text-e01,  lv_rc, lv_filename.
    MESSAGE  i000(zfi01)    WITH text-e01   lv_rc  lv_filename.
    RETURN.
  ENDIF.
*  Start of changes by AHMADT for CHG0172237
REPLACE ALL OCCURRENCES OF '.txt' IN lv_filename1  WITH '_1.txt'.
*  End of changes by AHMADT for CHG0172237
  OPEN     DATASET lv_filename1 FOR OUTPUT
                                IN TEXT MODE
                          ENCODING DEFAULT.
  cv_subrc = sy-subrc.

  IF       ( cv_subrc EQ 0 ).
    MOVE     lv_filename1                 TO cv_filename1.
    WRITE: /                     text-m01,  lv_filename1.
    MESSAGE  s000(zfi01)    WITH text-m01   lv_filename1.
  ELSE.
    lv_rc = cv_subrc.
    WRITE: /                     text-e01,  lv_rc, lv_filename1.
    MESSAGE  i000(zfi01)    WITH text-e01   lv_rc  lv_filename1.
    RETURN.
  ENDIF.
*eject
* Write the column header record
  CLEAR                                     ls_out_txt.
  MOVE     text-c01                      TO ls_out_txt-prart.
  MOVE     text-c02                      TO ls_out_txt-pratx.
*  Start of changes by AHMADT for CHG0172237
*  MOVE     text-c03                      TO ls_out_txt-zzcomp_sta_id.
*  MOVE     text-c04                      TO ls_out_txt-odescription.
*  MOVE     text-c15                      TO ls_out_txt-zzloc_cd.
*  MOVE     text-c16                      TO ls_out_txt-zzpl_no.
*  MOVE     text-c17                      TO ls_out_txt-pdescription.
*  MOVE     text-c19                      TO ls_out_txt-zzinsv_dt_c.
*  MOVE     text-c21                      TO ls_out_txt-zzca_ex.
*  MOVE     text-c23                      TO ls_out_txt-cdescription.
*  MOVE     text-c24                      TO ls_out_txt-zzpl_seg.
*  MOVE     text-c25                      TO ls_out_txt-zzili_seg.
*  MOVE     text-c26                      TO ls_out_txt-zztwr_site.
*  MOVE     text-c27                      TO ls_out_txt-mdescription.
*  End of changes by AHMADT for CHG0172237
  MOVE     text-c05                      TO ls_out_txt-pbukr.
  MOVE     text-c06                      TO ls_out_txt-butxt.
  MOVE     text-c07                      TO ls_out_txt-werks.
  MOVE     text-c08                      TO ls_out_txt-name1.
  MOVE     text-c09                      TO ls_out_txt-stufe.
  MOVE     text-c10                      TO ls_out_txt-posid.
  MOVE     text-c11                      TO ls_out_txt-post1.
  MOVE     text-c12                      TO ls_out_txt-usr00.
  MOVE     text-c13                      TO ls_out_txt-status.
  MOVE     text-c14                      TO ls_out_txt-tplnr.
  MOVE     text-c18                      TO ls_out_txt-pende_c.
  MOVE     text-c20                      TO ls_out_txt-zzfact_rec_dt_c.
  MOVE     text-c22                      TO ls_out_txt-cvalue_c.
  MOVE     text-c28                      TO ls_out_txt-loevm.
  MOVE     text-c29                      TO ls_out_txt-long_text.

  CLEAR                                     lv_string.
  CONCATENATE           text-c01  text-c02  text-c03
                        text-c04  text-c05  text-c06
                        text-c07  text-c08  text-c09
                        text-c10  text-c11  text-c12
                        text-c13  text-c14  text-c15
                        text-c16  text-c17  text-c18
                        text-c19  text-c20  text-c21
                        text-c22  text-c23  text-c24
                        text-c25  text-c26  text-c27
                                  text-c28  text-c29
                                       INTO lv_string
                               SEPARATED BY gc_tab.

  IF       ( p_inclch                    IS NOT INITIAL ).
    TRANSFER lv_string                   TO lv_filename.
    TRANSFER lv_string                   TO lv_filename1.
    ADD      1                           TO gv_count_recs.
  ENDIF.

ENDFORM.                    " f_open_output_dataset
*eject
*&---------------------------------------------------------------------*
*&      Form  f_close_output_dataset
*&---------------------------------------------------------------------*
*       Close the output dataset.
*----------------------------------------------------------------------*
FORM f_close_output_dataset
  USING    iv_filename                 TYPE text255
           iv_filename1                TYPE text255.

  IF     ( iv_filename IS INITIAL ).
    RETURN.
  ENDIF.

  CLOSE    DATASET iv_filename.


  IF     ( iv_filename1 IS INITIAL ).
    RETURN.
  ENDIF.

  CLOSE    DATASET iv_filename1.

  SKIP     1.
  WRITE:   /                     text-m02,  gv_count_recs.
  MESSAGE  s000(zfi01)      WITH text-m02   gv_count_recs.

ENDFORM.                    " f_close_output_dataset
*eject
*&---------------------------------------------------------------------*
*&      Form  f_output_wbs
*&---------------------------------------------------------------------*
*       Output the WBS element
*----------------------------------------------------------------------*
FORM f_output_wbs
  USING    is_proj                     TYPE ty_wa_proj
           is_prps                     TYPE ty_wa_prps
           is_pstat                    TYPE ty_wa_pstat.

  DATA:    ls_out_txt                  TYPE ty_wa_out_txt.

  DATA:    lv_posid                    TYPE ps_posid,
           lv_status                   TYPE text100,
           lv_pende_c                  TYPE char10,
           lv_zzinsv_dt_c              TYPE char10,
           lv_zzfact_rec_dt_c          TYPE char10,
           lv_cvalue_c                 TYPE char3,
           lv_long_text                TYPE STRING,
           lv_string                   TYPE STRING,
*  Start of changes by AHMADT for CHG0172237
           lv_filename                 TYPE char255,
           lv_filename1                TYPE char255,
           ls_xparam1                  TYPE ty_wa_xparam.
*  End of changes by AHMADT for CHG0172237

  IF     ( is_prps-posnr                 IS INITIAL ).
    RETURN.
  ENDIF.

  CLEAR    ls_out_txt.

* Read the project type
  IF     ( is_prps-prart                 IS INITIAL        ).
    CLEAR  gs_tcj1t.
  ELSEIF ( is_prps-prart                 NE gs_tcj1t-prart ).
    CLEAR                                   gs_tcj1t.
    READ     TABLE gt_tcj1t            INTO gs_tcj1t
                             WITH TABLE KEY prart = is_prps-prart.
    IF     ( sy-subrc NE 0 ).
      CLEAR  gs_tcj1t.
    ENDIF.
  ENDIF.

*  Start of changes by AHMADT for CHG0172237
* Read the area description
*  IF     ( is_prps-zzcomp_sta_id         IS INITIAL              ).
*    CLEAR  gs_cmpsta_val.
*  ELSEIF ( is_prps-zzcomp_sta_id         NE gs_cmpsta_val-ovalue ).
*    CLEAR                                   gs_cmpsta_val.
*    READ     TABLE gt_cmpsta_val       INTO gs_cmpsta_val
*                    WITH TABLE KEY ovalue = is_prps-zzcomp_sta_id.
*    IF     ( sy-subrc NE 0 ).
*      CLEAR  gs_cmpsta_val.
*    ENDIF.
*  ENDIF.
*  End of changes by AHMADT for CHG0172237

*eject
* Read the company code
  IF     ( is_prps-pbukr                 IS INITIAL       ).
    CLEAR  gs_t001.
  ELSEIF ( is_prps-pbukr                 NE gs_t001-bukrs ).
    CLEAR                                   gs_t001.
    READ     TABLE gt_t001             INTO gs_t001
                             WITH TABLE KEY bukrs = is_prps-pbukr.
    IF     ( sy-subrc NE 0 ).
      CLEAR  gs_t001.
    ENDIF.
  ENDIF.

* Read the plant
  IF     ( is_prps-werks                 IS INITIAL        ).
    CLEAR  gs_t001w.
  ELSEIF ( is_prps-werks                 NE gs_t001w-werks ).
    CLEAR                                   gs_t001w.
    READ     TABLE gt_t001w            INTO gs_t001w
                             WITH TABLE KEY werks = is_prps-werks.
    IF     ( sy-subrc NE 0 ).
      CLEAR  gs_t001w.
    ENDIF.
  ENDIF.

* Format the WBS element
  CLEAR    lv_posid.

  CALL FUNCTION 'CONVERSION_EXIT_ABPSN_OUTPUT'
    EXPORTING
      INPUT  = is_prps-posid
    IMPORTING
      OUTPUT = lv_posid.
  IF     ( sy-subrc NE 0 ).
    CLEAR  lv_posid.
  ENDIF.

* Format the WBS status
  CLEAR                                     lv_status.
  CONCATENATE                               is_pstat-sttxt_int '//'
                                            is_pstat-sttxt_ext
                                       INTO lv_status
                               SEPARATED BY SPACE.
  CONDENSE                                  lv_status.

*  Start of changes by AHMADT for CHG0172237
* Read the region description
*  IF     ( is_prps-zzpl_no               NE gs_pl_val_pval-pvalue ).
*    CLEAR                                   gs_pl_val_pval.
*    READ     TABLE gt_pl_val_pval      INTO gs_pl_val_pval
*                    WITH TABLE KEY pvalue = is_prps-zzpl_no.
*    IF     ( sy-subrc NE 0 ).
*      CLEAR  gs_pl_val_pval.
*    ENDIF.
*  ENDIF.
*  End of changes by AHMADT for CHG0172237

*eject
* Format the basic finish date
  CLEAR                                     lv_pende_c.
  IF   ( ( is_prps-pende                 IS NOT INITIAL ) AND
         ( is_prps-pende                 NE '00000000'  )     ).
    CONCATENATE                             is_prps-pende+0(4) '-'
                                            is_prps-pende+4(2) '-'
                                            is_prps-pende+6(2)
                                       INTO lv_pende_c.
  ENDIF.

* Format the actual in-service date
*  Start of changes by AHMADT for CHG0172237
*  CLEAR                                     lv_zzinsv_dt_c.
*  IF   ( ( is_prps-zzinsv_dt             IS NOT INITIAL ) AND
*         ( is_prps-zzinsv_dt             NE '00000000'  )     ).
*    CONCATENATE                             is_prps-zzinsv_dt+0(4) '-'
*                                            is_prps-zzinsv_dt+4(2) '-'
*                                            is_prps-zzinsv_dt+6(2)
*                                       INTO lv_zzinsv_dt_c.
*  ENDIF.


* Format the facility rec received date
*  CLEAR                                     lv_zzfact_rec_dt_c.
*  IF   ( ( is_prps-zzfact_rec_date       IS NOT INITIAL ) AND
*         ( is_prps-zzfact_rec_date       NE '00000000'  )     ).
*    CONCATENATE                     is_prps-zzfact_rec_date+0(4) '-'
*                                    is_prps-zzfact_rec_date+4(2) '-'
*                                    is_prps-zzfact_rec_date+6(2)
*                                       INTO lv_zzfact_rec_dt_c.
*  ENDIF.

* Read the budget category/topic description
*  IF     ( is_prps-zzbud_cat             IS INITIAL              ).
*    CLEAR  gs_budcat_val.
*  ELSEIF ( is_prps-zzbud_cat             NE gs_budcat_val-cvalue ).
*    CLEAR                                   gs_budcat_val.
*    READ     TABLE gt_budcat_val       INTO gs_budcat_val
*                    WITH TABLE KEY cvalue = is_prps-zzbud_cat.
*    IF     ( sy-subrc NE 0 ).
*      CLEAR  gs_budcat_val.
*    ENDIF.
*  ENDIF.

*  CLEAR                                     lv_cvalue_c.
*  IF     ( is_prps-zzbud_cat             IS NOT INITIAL ).
*    MOVE   is_prps-zzbud_cat             TO lv_cvalue_c.
*  ENDIF.

*eject
* Read the budget year description
*  IF     ( is_prps-zzmtr_sta_id          IS INITIAL              ).
*    CLEAR  gs_mtrsta_val.
*  ELSEIF ( is_prps-zzmtr_sta_id          NE gs_mtrsta_val-mvalue ).
*    CLEAR                                   gs_mtrsta_val.
*    READ     TABLE gt_mtrsta_val       INTO gs_mtrsta_val
*                    WITH TABLE KEY mvalue = is_prps-zzmtr_sta_id.
*    IF     ( sy-subrc NE 0 ).
*      CLEAR  gs_mtrsta_val.
*    ENDIF.
*  ENDIF.
*  End of changes by AHMADT for CHG0172237

* Get the WBS element long text

  CLEAR    lv_long_text.

  PERFORM  f_get_wbs_long_text        USING is_prps-posnr
                                   CHANGING lv_long_text.

  CLEAR                                     ls_out_txt.
  MOVE     is_prps-prart                 TO ls_out_txt-prart.
  MOVE     gs_tcj1t-pratx                TO ls_out_txt-pratx.
*  Start of changes by AHMADT for CHG0172237
*  MOVE     is_prps-zzcomp_sta_id         TO ls_out_txt-zzcomp_sta_id.
*  MOVE     gs_cmpsta_val-odescription    TO ls_out_txt-odescription.
*  MOVE     is_prps-zzloc_cd              TO ls_out_txt-zzloc_cd.
*  MOVE     is_prps-zzpl_no               TO ls_out_txt-zzpl_no.
*  MOVE     gs_pl_val_pval-pdescription   TO ls_out_txt-pdescription.
*  MOVE     lv_zzinsv_dt_c                TO ls_out_txt-zzinsv_dt_c.
*  MOVE     is_prps-zzca_ex               TO ls_out_txt-zzca_ex.
*  MOVE     gs_budcat_val-cdescription    TO ls_out_txt-cdescription.
*  MOVE     is_prps-zzpl_seg              TO ls_out_txt-zzpl_seg.
*  MOVE     is_prps-zzili_seg             TO ls_out_txt-zzili_seg.
*  MOVE     is_prps-zztwr_site            TO ls_out_txt-zztwr_site.
*  MOVE     gs_mtrsta_val-mdescription    TO ls_out_txt-mdescription.
*  End of changes by AHMADT for CHG0172237
  MOVE     is_prps-pbukr                 TO ls_out_txt-pbukr.
  MOVE     gs_t001-butxt                 TO ls_out_txt-butxt.
  MOVE     is_prps-werks                 TO ls_out_txt-werks.
  MOVE     gs_t001w-name1                TO ls_out_txt-name1.
  MOVE     is_prps-stufe                 TO ls_out_txt-stufe.
  MOVE     lv_posid                      TO ls_out_txt-posid.
  MOVE     is_prps-post1                 TO ls_out_txt-post1.
  MOVE     is_prps-usr00                 TO ls_out_txt-usr00.
  MOVE     lv_status                     TO ls_out_txt-status.
  MOVE     is_prps-tplnr                 TO ls_out_txt-tplnr.
  MOVE     lv_pende_c                    TO ls_out_txt-pende_c.
  MOVE     lv_zzfact_rec_dt_c            TO ls_out_txt-zzfact_rec_dt_c.
  MOVE     lv_cvalue_c                   TO ls_out_txt-cvalue_c.
  MOVE     is_prps-loevm                 TO ls_out_txt-loevm.
  MOVE     lv_long_text                  TO ls_out_txt-long_text.

*eject
  CLEAR                                     lv_string.
  CONCATENATE                               ls_out_txt-prart
                                            ls_out_txt-pratx
*  Start of changes by AHMADT for CHG0172237
*                                            ls_out_txt-zzcomp_sta_id
*                                            ls_out_txt-odescription
*                                            ls_out_txt-cdescription
*                                            ls_out_txt-zzpl_seg
*                                            ls_out_txt-zzili_seg
*                                            ls_out_txt-zztwr_site
*                                            ls_out_txt-mdescription
*                                            ls_out_txt-zzloc_cd
*                                            ls_out_txt-zzpl_no
*                                            ls_out_txt-pdescription
*                                            ls_out_txt-zzinsv_dt_c
*                                            ls_out_txt-zzca_ex
*  End of changes by AHMADT for CHG0172237
                                            ls_out_txt-pbukr
                                            ls_out_txt-butxt
                                            ls_out_txt-werks
                                            ls_out_txt-name1
                                            ls_out_txt-stufe
                                            ls_out_txt-posid
                                            ls_out_txt-post1
                                            ls_out_txt-usr00
                                            ls_out_txt-status
                                            ls_out_txt-tplnr
                                            ls_out_txt-pende_c
                                            ls_out_txt-zzfact_rec_dt_c
                                            ls_out_txt-cvalue_c
                                            ls_out_txt-loevm
                                            ls_out_txt-long_text
                                       INTO lv_string
                               SEPARATED BY gc_tab.
*  replace all OCCURRENCES OF 'Arch/' in gv_filename1 with space.
*  Start of changes by AHMADT for CHG0172237
  CLEAR ls_xparam1.
  READ TABLE gt_xparam INTO ls_xparam1 WITH KEY key1 = gc_record.
  IF sy-subrc = 0.
  IF gv_count_recs < ls_xparam1-value1.

    TRANSFER   lv_string                     TO gv_filename.
    TRANSFER   lv_string                     TO gv_filename1.
    ADD        1                             TO gv_count_recs.
    CLEAR      lv_string.
  ELSE.
    PERFORM  f_close_output_dataset       USING gv_filename
                                                gv_filename1.
    CLEAR       gv_count_recs.
    CONDENSE    gv_file_count           NO-GAPS.
    CONCATENATE '_' gv_file_count '.txt' INTO gv_file_ext.

    REPLACE ALL OCCURRENCES OF gv_file_ext_last IN gv_filename  WITH gv_file_ext.
    REPLACE ALL OCCURRENCES OF gv_file_ext_last IN gv_filename1 WITH gv_file_ext.

    gv_file_ext_last = gv_file_ext.
    ADD 1 TO    gv_file_count.

  OPEN     DATASET gv_filename  FOR OUTPUT
                                 IN TEXT MODE
                           ENCODING DEFAULT.

  IF       ( sy-subrc EQ 0 ).
    WRITE: /                     text-m01,  gv_filename.
    MESSAGE  s000(zfi01)    WITH text-m01   gv_filename.
  ELSE.
    WRITE: /                     text-e01,   gv_filename.
    MESSAGE  i000(zfi01)    WITH text-e01    gv_filename.
    RETURN.
  ENDIF.
  OPEN     DATASET gv_filename1 FOR OUTPUT
                                 IN TEXT MODE
                           ENCODING DEFAULT.
    TRANSFER   lv_string                     TO gv_filename.
    TRANSFER   lv_string                     TO gv_filename1.
    ADD        1                             TO gv_count_recs.
    CLEAR      lv_string.

  IF       ( sy-subrc EQ 0 ).
    WRITE: /                     text-m01,  gv_filename.
    MESSAGE  s000(zfi01)    WITH text-m01   gv_filename.
  ELSE.
    WRITE: /                     text-e01,   gv_filename.
    MESSAGE  i000(zfi01)    WITH text-e01    gv_filename.
    RETURN.
  ENDIF.
  ENDIF.
  ENDIF.
*  TRANSFER   lv_string                   TO gv_filename.
*  TRANSFER   lv_string                   TO gv_filename1.
*  ADD        1                           TO gv_count_recs.

*  End of changes by AHMADT for CHG0172237

ENDFORM.                    " f_output_wbs
*eject
*&---------------------------------------------------------------------*
*&      Form  f_get_wbs_long_text
*&---------------------------------------------------------------------*
*       Get the WBS element long text
*----------------------------------------------------------------------*
FORM f_get_wbs_long_text
  USING    iv_posnr                    TYPE ps_posnr
  CHANGING cv_long_text                TYPE STRING.

  DATA:    ls_lines                    TYPE tline,
           lt_lines                    TYPE STANDARD TABLE OF tline.

  DATA:    lv_id                       TYPE tdid,
           lv_name                     TYPE tdobname,
           lv_object                   TYPE tdobject,
           lv_text                     TYPE text256,
           lv_string                   TYPE STRING.

  CLEAR    cv_long_text.

  IF     ( iv_posnr                      IS INITIAL ).
    RETURN.
  ENDIF.

* Build the output records for the long text
  CLEAR                                     lv_id.
  MOVE     'LTXT'                        TO lv_id.
  CLEAR                                     lv_name.
  CONCATENATE 'E' iv_posnr             INTO lv_name.
  CLEAR                                     lv_object.
  MOVE     'PMS'                         TO lv_object.

  CLEAR    lt_lines[].

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      CLIENT                  = sy-mandt
      ID                      = lv_id
      LANGUAGE                = 'E'
      NAME                    = lv_name
      OBJECT                  = lv_object
    TABLES
      LINES                   = lt_lines
    EXCEPTIONS
      ID                      = 1
      LANGUAGE                = 2
      NAME                    = 3
      NOT_FOUND               = 4
      OBJECT                  = 5
      REFERENCE_CHECK         = 6
      WRONG_ACCESS_TO_ARCHIVE = 7
      OTHERS                  = 8.

  IF     ( sy-subrc NE 0 ).
    RETURN.
  ENDIF.

*eject
  CLEAR                                     lv_string.

  CLEAR                                     ls_lines.
  LOOP AT  lt_lines                    INTO ls_lines.

    IF     ( ls_lines-tdline             IS NOT INITIAL ).

      CLEAR                                 lv_text.
      MOVE     ls_lines-tdline           TO lv_text.

      SHIFT    lv_text                 LEFT DELETING LEADING SPACE.

      PERFORM  f_string_remove_spcl_char
                                   CHANGING lv_text.

      IF       ( lv_text                 IS NOT INITIAL ).
        IF     ( lv_string               IS INITIAL     ).
          MOVE   lv_text                 TO lv_string.
        ELSE.
          CONCATENATE                       lv_string
                                            lv_text
                                       INTO lv_string
                               SEPARATED BY SPACE.
        ENDIF.
      ENDIF.

    ENDIF.

    CLEAR  ls_lines.
  ENDLOOP.

  CLEAR                                     cv_long_text.
  IF     ( lv_string                     IS NOT INITIAL ).
    MOVE   lv_string                     TO cv_long_text.
  ENDIF.

ENDFORM.                    " f_get_wbs_long_text
*eject
*&---------------------------------------------------------------------*
*&      Form  f_string_remove_spcl_char
*&---------------------------------------------------------------------*
*       Remove special, non-printable, characters from a string
*----------------------------------------------------------------------*
FORM f_string_remove_spcl_char
  CHANGING cv_text                     TYPE text256.

  DATA:    lv_text                     TYPE text256,
           lv_strlen                   TYPE syindex,
           lv_ptr1                     TYPE syindex,
           lv_ptr2                     TYPE syindex.

  FIELD-SYMBOLS: <ptr1>                TYPE ANY,
                 <ptr2>                TYPE ANY.

  IF     ( cv_text IS INITIAL ).
    RETURN.
  ENDIF.

  CLEAR    lv_text.
  CLEAR    lv_strlen.
  CLEAR    lv_ptr1.
  CLEAR    lv_ptr2.

  ASSIGN   cv_text TO <ptr1>.
  ASSIGN   lv_text TO <ptr2>.

  lv_strlen = STRLEN( cv_text ).

  DO       lv_strlen TIMES.

    IF     ( <ptr1>+lv_ptr1(1)           GE SPACE ).

      MOVE   <ptr1>+lv_ptr1(1)           TO <ptr2>+lv_ptr2(1).

      ADD    1                           TO lv_ptr2.

    ENDIF.

    ADD      1                           TO lv_ptr1.

  ENDDO.

  CLEAR                                     cv_text.
  IF     ( lv_text                       IS NOT INITIAL ).
    MOVE   lv_text                       TO cv_text.
  ENDIF.

ENDFORM.                    " f_string_remove_spcl_char
*eject
*&---------------------------------------------------------------------*
*&      Form  f_archive_dataset
*&---------------------------------------------------------------------*
*       Archive the output dataset
*----------------------------------------------------------------------*
FORM f_archive_dataset
  USING    iv_filename                 TYPE text255.

  DATA:    ls_return                   TYPE bapireturn.

  DATA:    lt_token                    TYPE STANDARD TABLE OF btcxpgpar
                                       WITH HEADER LINE.

  DATA:    lv_lines                    TYPE syindex,
           lv_token                    TYPE btcxpgpar,
           lv_source_dir               TYPE btcxpgpar,
           lv_target_dir               TYPE btcxpgpar,
           lv_source_fname             TYPE btcxpgpar,
           lv_target_fname             TYPE btcxpgpar.

  IF     ( iv_filename                   IS INITIAL ).
    RETURN.
  ENDIF.

  CLEAR    lt_token[].
  CLEAR    lv_token.

  SPLIT    iv_filename                   AT '/'   "Changed from '\' to '/' by AHMADT for CHG0172237
                                       INTO TABLE lt_token.

  DESCRIBE TABLE lt_token             LINES lv_lines.

  READ     TABLE lt_token             INDEX lv_lines.

  MOVE           lt_token                TO lv_token.

  TRANSLATE                                 lv_token USING '._'.

  IF     ( p_asfnof                      NS 'YYYYMMDD' ).
    CONCATENATE                             lv_token '_'
                                            gv_datum_run
                                       INTO lv_token.
  ENDIF.

  IF     ( p_asfnof                      NS 'HHMMSS'   ).
    CONCATENATE                             lv_token '_'
                                            gv_uzeit_run
                                       INTO lv_token.
  ENDIF.

*eject
  IF     ( p_test                        IS INITIAL ).
    CONCATENATE                             lv_token
                                            '.ARC'
                                       INTO lv_token.
  ELSE.
    CONCATENATE                             lv_token
                                            '.TST'
                                       INTO lv_token.
  ENDIF.

  CLEAR:   lv_source_dir, lv_source_fname.
  CLEAR:   lv_target_dir, lv_target_fname.

  MOVE     p_asfpaf                      TO lv_source_dir.
  MOVE     lt_token                      TO lv_source_fname.
  MOVE     p_asfpaf                      TO lv_target_dir.
  MOVE     lv_token                      TO lv_target_fname.

  CLEAR    ls_return.

  CALL FUNCTION 'ZFI_FILE_HANDLE'
    EXPORTING
      i_source_dir        = lv_source_dir
      i_target_dir        = lv_target_dir
      i_source_fname      = lv_source_fname
      i_target_fname      = lv_target_fname
      i_command           = 'C'
      i_date_time_stamp   = space
      i_rename_arc_to_new = space
    IMPORTING
      e_return            = ls_return.

  IF     ( ls_return-type                CA 'aAeE' ).
    WRITE: /                     text-e02,  ls_return-message.
    MESSAGE  i000(zfi01)    WITH text-e02   ls_return-message.
    RETURN.
  ENDIF.

*eject
  IF     ( p_test                        IS INITIAL ).

    CLEAR                                   lv_target_dir.
    MOVE     p_asfpof                    TO lv_target_dir.
    CLEAR                                   lv_target_fname.
    MOVE     lt_token                    TO lv_target_fname.

    CLEAR    ls_return.

    CALL FUNCTION 'ZFI_FILE_HANDLE'
      EXPORTING
        i_source_dir        = lv_source_dir
        i_target_dir        = lv_target_dir
        i_source_fname      = lv_source_fname
        i_target_fname      = lv_target_fname
        i_command           = 'M'
        i_date_time_stamp   = space
        i_rename_arc_to_new = space
      IMPORTING
        e_return            = ls_return.

    IF     ( ls_return-type              CA 'aAeE' ).
      WRITE: /                   text-e03,  ls_return-message.
      MESSAGE  i000(zfi01)  WITH text-e03   ls_return-message.
      RETURN.
    ENDIF.

  ENDIF.

  DELETE   DATASET iv_filename.

ENDFORM.                    " f_archive_dataset
*eject
*&---------------------------------------------------------------------*
*&      Form  f_print_report_header
*&---------------------------------------------------------------------*
*       Print the top of page report header
*----------------------------------------------------------------------*
FORM f_print_report_header.

  CLEAR                                     gv_sysid.
  MOVE     sy-sysid                      TO gv_sysid.
  MOVE     sy-mandt                      TO gv_sysid+4(3).
  CLEAR                                     gv_uname.
  MOVE     sy-uname                      TO gv_uname.
  CLEAR                                     gv_pagno.
  MOVE     sy-pagno                      TO gv_pagno.
  CLEAR                                     gv_cprog.
  MOVE     sy-cprog                      TO gv_cprog.
  CLEAR                                     gv_datum.
  WRITE:   sy-datum                      TO gv_datum.
  CLEAR                                     gv_uzeit.
  WRITE:   sy-uzeit                      TO gv_uzeit.

  WRITE: /001 text-h11,
          012 gv_sysid,
          044 text-h12,
          056 gv_cprog,
          101 text-h13,
          111 gv_datum.

  WRITE: /001 text-h21,
          012 gv_uname,
          101 text-h23,
          113 gv_uzeit.

  WRITE: /105 text-h33,
          116 gv_pagno.

ENDFORM.                    " f_print_report_header
