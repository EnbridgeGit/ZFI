*&---------------------------------------------------------------------*
*& Report  ZLPSI912_WBS_EXTRACT
*&
*&---------------------------------------------------------------------*
*& This report has been copied from report ZLPSI912_WBS_EXTRACT from
*& DEC system for CR CHG0172237 by AHMADT on 13/02/2020
*&---------------------------------------------------------------------*

REPORT  ZLPSI912_WBS_EXTRACT MESSAGE-ID zfi01
                             LINE-COUNT 65
                             LINE-SIZE 121
                             NO STANDARD PAGE HEADING.
************************************************************************
*                            Spectra Energy                            *
************************************************************************
*  Program:          ZLPSI008_WBS_EXTRACT                              *
*  Author:           John Hartung                                      *
*  Date:             September 9, 2016                                 *
*  Ticket#:          ACR-2338                                          *
*  Application Area: FICO PS                                           *
*                                                                      *
*  Description:      PS WBS Extract including Long Text                *
*                                                                      *
************************************************************************
*----------------------- CHANGE LOG -----------------------------------*
*  Date    By        Description                                       *
* -------- --------- ------------------------------------------------- *
* 09/09/16 JRHARTUNG DECK916888 - ACR-2338 - Initial program           *
*                    DECK916894, DECK916910, DECK917113, DECK917151,   *
*                    DECK917204                                        *
*----------------------------------------------------------------------*
* 31/01/20 AHMADT    DECK920477 - Object ID changed from I_CL2C_PS_008 *
*                    to I_CL2C_PS_009 for CHG0172224                   *
*----------------------------------------------------------------------*
* 13/02/20 AHMADT    D30K930427  CHG0172237                            *
*                    Report copied from DEC.Commented all custom fields*
*                    and their logic of structure PRPS_R which were    *
*                    enhanced by using structure CI_PRPS.              *
************************************************************************

*eject
************************************************************************
*                              Top Include                             *
************************************************************************
INCLUDE zlpsi912_wbs_extract_top.
************************************************************************
*                            Initialization                            *
************************************************************************
INITIALIZATION.

* Select the program parameter values
  PERFORM  f_select_xparam.

************************************************************************
*                         At Selection-Screen                          *
************************************************************************
AT SELECTION-SCREEN OUTPUT.

* Default the selection screen values
  PERFORM  f_default_sel_screen_values.

* Format the selection screen layout
  PERFORM  f_format_sel_screen_layout.

AT SELECTION-SCREEN.

  CLEAR                                     gv_datum_run.
  MOVE     sy-datum                      TO gv_datum_run.
  CLEAR                                     gv_uzeit_run.
  MOVE     sy-uzeit                      TO gv_uzeit_run.

*eject
************************************************************************
*                          Start Of Selection                          *
************************************************************************
START-OF-SELECTION.

* Initial the data elements
  PERFORM  f_initial_data_elements.

* Open the output dataset.
  CLEAR    gv_subrc.

  PERFORM  f_open_output_dataset   CHANGING gv_filename
                                            gv_filename1
                                            gv_subrc.

  IF     ( gv_subrc NE 0 ).
    LEAVE  TO LIST-PROCESSING.
    RETURN.
  ENDIF.

* Project Definition
GET      proj   FIELDS pspnr  pspid  objnr.

  CLEAR                                     gv_objnr.
  MOVE     proj-objnr                    TO gv_objnr.

  CLEAR                                     gs_proj_p.
  MOVE     proj-pspnr                    TO gs_proj_p-pspnr.
  MOVE     proj-pspid                    TO gs_proj_p-pspid.
  MOVE     proj-objnr                    TO gs_proj_p-objnr.

* WBS Element
GET      prps_r FIELDS pspnr  posid  post1  objnr  psphi  pbukr
                       prart  stufe  tplnr  werks  usr00  loevm.
*                       zzbud_cat     zzmtr_sta_id  zzpl_no
*                       zzpl_seg      zzili_seg     zzcomp_sta_id
*                       zztwr_site    zzloc_cd      zzca_ex
*                       zzinsv_dt     zzfact_rec_date.

  CLEAR                                     gv_objnr.
  MOVE     prps_r-objnr                  TO gv_objnr.

  PERFORM  f_output_wbs               USING gs_proj
                                            gs_prps
                                            gs_pstat.

*eject
  CLEAR                                     gs_proj.
  CLEAR                                     gs_prps.
  CLEAR                                     gs_pstat.

  MOVE     gs_proj_p                     TO gs_proj.
  MOVE     prps_r-pspnr                  TO gs_prps-posnr.
  MOVE     prps_r-posid                  TO gs_prps-posid.
  MOVE     prps_r-post1                  TO gs_prps-post1.
  MOVE     prps_r-objnr                  TO gs_prps-objnr.
  MOVE     prps_r-psphi                  TO gs_prps-psphi.
  MOVE     prps_r-pbukr                  TO gs_prps-pbukr.
  MOVE     prps_r-prart                  TO gs_prps-prart.
  MOVE     prps_r-stufe                  TO gs_prps-stufe.
  MOVE     prps_r-tplnr                  TO gs_prps-tplnr.
  MOVE     prps_r-werks                  TO gs_prps-werks.
  MOVE     prps_r-usr00                  TO gs_prps-usr00.
  MOVE     prps_r-loevm                  TO gs_prps-loevm.
*  MOVE     prps_r-zzbud_cat              TO gs_prps-zzbud_cat.
*  MOVE     prps_r-zzmtr_sta_id           TO gs_prps-zzmtr_sta_id.
*  MOVE     prps_r-zzpl_no                TO gs_prps-zzpl_no.
*  MOVE     prps_r-zzpl_seg               TO gs_prps-zzpl_seg.
*  MOVE     prps_r-zzili_seg              TO gs_prps-zzili_seg.
*  MOVE     prps_r-zzcomp_sta_id          TO gs_prps-zzcomp_sta_id.
*  MOVE     prps_r-zztwr_site             TO gs_prps-zztwr_site.
*  MOVE     prps_r-zzloc_cd               TO gs_prps-zzloc_cd.
*  MOVE     prps_r-zzca_ex                TO gs_prps-zzca_ex.
*  MOVE     prps_r-zzinsv_dt              TO gs_prps-zzinsv_dt.
*  MOVE     prps_r-zzfact_rec_date        To gs_prps-zzfact_rec_date.

GET      prte   FIELDS posnr  pende.

  CLEAR                                     gs_prps-pende.
  IF     ( prte-posnr                    EQ gs_prps-posnr ).
    MOVE   prte-pende                    TO gs_prps-pende.
  ENDIF.

GET      jsto.

GET      pstat.

  CLEAR                                     gs_pstat.
  IF     ( pstat-objnr                   EQ gs_prps-objnr ).
    MOVE   pstat                         TO gs_pstat.
  ENDIF.

*eject
************************************************************************
*                           End Of Selection                           *
************************************************************************
END-OF-SELECTION.

* Output the WBS element
  PERFORM  f_output_wbs               USING gs_proj
                                            gs_prps
                                            gs_pstat.

* Close the output dataset.
  PERFORM  f_close_output_dataset     USING gv_filename
                                            gv_filename1.

* Archive the output dataset
*  PERFORM  f_archive_dataset          USING gv_filename.

************************************************************************
*                             Top-Of-Page                              *
************************************************************************
TOP-OF-PAGE.

* Print the top of page report header
  PERFORM  f_print_report_header.

  INCLUDE zlpsi912_wbs_extract_f01.
