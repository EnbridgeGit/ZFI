*&---------------------------------------------------------------------*
*&  Include           ZFI_ORD_IDOC_PROCESS_F01
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Program Name       :   ZFI_ORD_IDOC_PROCESS                          *
* Include            :   ZFI_ORD_IDOC_PROCESS_F01                      *
* Author             :   AKMADASU                                      *
* Date               :   Jan 23, 2022                                  *
* Technical Contact  :   Ashok Madasu                                  *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :   To Process Order IDOCS                        *
* Notes              :                                                 *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS         Description                     *
* ---------------------------------------------------------------------*
* 23-Jan-2022  AKMADASU    D30K931986-Initial development              *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SUBMIT_IDOC_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SUBMIT_IDOC_PROCESS .
  FIELD-SYMBOLS:<lt_idocout> TYPE ANY TABLE,
                <fs_idocout> TYPE ANY.
  DATA:lr_idocout TYPE REF TO data.
  cl_salv_bs_runtime_info=>set(
   EXPORTING display = abap_false
   metadata = abap_false
   data = abap_true ).
  SUBMIT rbdapp01 WITH docnum IN docnum
                    WITH credat IN credat
                    WITH cretim IN cretim
                    WITH status IN status
                    WITH mestyp IN mestyp
                    WITH mescod IN mescod
                    WITH mesfct IN mesfct
                    WITH sndprt IN sndprt
                    WITH sndprn IN sndprn
                    WITH sndpfc IN sndpfc
                    WITH p_pcksiz = p_pcksiz
                    WITH test   IN test
                    WITH obj_type IN obj_type
                    WITH p_output = p_output
                    AND RETURN.
  IF SY-SUBRC IS INITIAL.
    TRY.
        cl_salv_bs_runtime_info=>get_data_ref(
        IMPORTING r_data = lr_idocout ).
        ASSIGN lr_idocout->* TO <lt_idocout>.
      CATCH cx_salv_bs_sc_runtime_info.
        MESSAGE `Unable to retrieve ALV data` TYPE 'E'.
    ENDTRY.
    cl_salv_bs_runtime_info=>clear_all( ).
    IF <lt_idocout> IS ASSIGNED.
      LOOP AT <lt_idocout> ASSIGNING <fs_idocout>.
        MOVE-CORRESPONDING <fs_idocout> TO ls_output.
        APPEND ls_output TO lt_output.
        clear:ls_output.
      ENDLOOP.
    ENDIF.
*WAIT UP TO 1 SECONDS.
    SELECT DOCNUM
           LOGDAT
           LOGTIM
           COUNTR
           STATUS
*           STATXT
           STAPA2
           STAPA3
           FROM edids
           INTO TABLE gt_edids
           WHERE docnum IN docnum
           AND status   = '53'.
    IF sy-subrc = 0.
      SORT GT_EDIDS BY DOCNUM.
*      gt_edidser[] = gt_edids[].
*      delete gt_edids   where status ne '53'.
*      delete gt_edidser where status eq '53'.
      LOOP AT gt_edids INTO gs_edids.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = gs_edids-stapa3
          IMPORTING
            OUTPUT = gs_vbeln-low.
        .
        gs_vbeln-sign = 'I'.
        gs_vbeln-option = 'EQ'.
        APPEND gs_vbeln TO lr_vbeln.
        CLEAR gs_vbeln.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.                    " SUBMIT_IDOC_PROCESS
*&---------------------------------------------------------------------*
*&      Form  SUBMIT_PGI_BILLING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SUBMIT_PGI_BILLING .
  IF LR_VBELN IS NOT INITIAL.
    FIELD-SYMBOLS:<lt_del> TYPE ANY TABLE,
                 <fs_del> TYPE ANY,
                 <lv_vbeln> TYPE ANY,
                 <lv_msg>   TYPE ANY.
    DATA:lr_del TYPE REF TO data.
    cl_salv_bs_runtime_info=>set(
     EXPORTING display = abap_false
     metadata = abap_false
     data = abap_true ).
    SUBMIT ZFI_PGI_BILLING_CREATE WITH s_vbeln IN lr_vbeln AND RETURN.
    IF sy-subrc = 0.
      TRY.
          cl_salv_bs_runtime_info=>get_data_ref(
          IMPORTING r_data = lr_del ).
          ASSIGN lr_del->* TO <lt_del>.
        CATCH cx_salv_bs_sc_runtime_info.
          MESSAGE `Unable to retrieve ALV data` TYPE 'S'.
      ENDTRY.
      cl_salv_bs_runtime_info=>clear_all( ).
      IF <lt_del> IS ASSIGNED.
        LOOP AT <lt_del> ASSIGNING <fs_del>.
          ASSIGN COMPONENT 'VBELN' OF STRUCTURE <fs_del> TO <lv_vbeln>.
          IF <lv_vbeln> IS ASSIGNED.
            ls_output-docnum = <lv_vbeln>.
            UNASSIGN <lv_vbeln>.
          ENDIF.
          ASSIGN COMPONENT 'MSG' OF STRUCTURE <fs_del> TO <lv_msg>.
          IF <lv_msg> IS ASSIGNED.
            ls_output-statxt = <lv_msg>.
            UNASSIGN <lv_msg>.
          ENDIF.
          APPEND ls_output TO lt_output.
          clear:ls_output.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.
  REFRESH lr_vbeln.
ENDFORM.                    " SUBMIT_PGI_BILLING
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_OUTPUT .
  LOOP AT lt_output INTO ls_output.
    WRITE:/ ls_output-docnum, ls_output-statxt.
    clear:ls_output.
  ENDLOOP.
ENDFORM.                    " DISPLAY_OUTPUT
