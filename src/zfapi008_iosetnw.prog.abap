*&---------------------------------------------------------------------*
*& Report  ZFAPI008_IOSETNW                                            *
*&                                                                     *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Program Name       :  ZFAPI008_IOSETNW                              *
*& Author             :  Tawfeeq Ahamd                                 *
*& Date               :  22-May-2020                                   *
*& Change Request     :  CHG0180384                                    *
*& Purpose            :  Extract Internal Order and Network Settlement *
*&                       data                                          *
*&---------------------------------------------------------------------*
*&                      Modification Log                               *
*&                                                                     *
*& Changed On   Changed By    CTS        Description                   *
*& --------------------------------------------------------------------*
*& 22-May-2020  AHMADT        D30K930537 CHG0180384 Initial Development*
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  zfapi008_iosetnw.

INCLUDE zfapi008_iosetnw_top.
INCLUDE zfapi008_iosetnw_ss.
INCLUDE zfapi008_iosetnw_form.

INITIALIZATION.
  t1 = text-002.
  t2 = text-003.
  t3 = text-004.

AT SELECTION-SCREEN OUTPUT.

  PERFORM f_ss_output.
  PERFORM f_ss_initialization.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_ord_p.
  CALL FUNCTION 'F4_FILENAME'
    IMPORTING
      file_name = p_ord_p.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_wbs_p.
  CALL FUNCTION 'F4_FILENAME'
    IMPORTING
      file_name = p_wbs_p.

START-OF-SELECTION.

   PERFORM f_order_extract.                  "create order extract file
   IF p_pres = 'X'.
     PERFORM f_down_to_pres USING gt_order   "Download file to presentation server
                                  p_ord_p.
   ELSEIF p_app = 'X'.
     PERFORM f_upload_order_app.             "Download file to application server
   ENDIF.



   PERFORM f_wbs_extract.                    "Create Project WBS file
   IF p_pres = 'X'.
      PERFORM f_down_to_pres USING gt_proj   "Downoad file to presentation server
                                   p_wbs_p.
   ELSEIF p_app = 'X'.
      PERFORM f_upload_project_app.          "Download file to application server
   ENDIF.
