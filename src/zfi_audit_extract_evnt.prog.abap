*----------------------------------------------------------------------*
* Report Name: ZFI_AUDIT_EXTRACT_ENNT
* Author:	     KBANERJEE-Kaushiki Banerjee
* Date:	       December 4th,2018
*
* Logical Database: NA
* SAPScript name:   NA
* Application Area: FI
* Description:  This tool will allow the user with the opportunity to
*               receive the extract for Audit related tables.The extract
*               can be obtained as online report,and can also be
*               downloaded to local PC as excel or other types of files.
*               This program also allows users to send mails to the
*               specific person and attaching the table extract in mail.
*               If no mail receipient is specifiedmail is sent to the
*               person who executes this program.
************************************************************************
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* 04-DEC-2018  KBANERJEE   D30K929436  CHG0137915 -Initial Development *
* 22-JAN-2019  KBANERJEE   D30K929560  UAT issues resolution           *
*                          D30K929545                                  *
*                          D30K929519                                  *
*                          D30K929542                                  *
*                          D30K929483                                  *
*                          D30K929459                                  *
*                          D30K929696                                  *
* 17-FEB-2020 KBANERJEE    D30K930437 CHG0174059_DFCT0018156-Code fixes*
*                                     to audit extract programs        *
************************************************************************
INITIALIZATION.
  PERFORM f_initialize.

AT SELECTION-SCREEN OUTPUT.
  PERFORM f_screen_pbo.

AT SELECTION-SCREEN ON p_table.
  REFRESH gt_ddfields.
  CALL FUNCTION 'STATUS_BUFFER_REFRESH'
    EXPORTING
      i_free = 'X'.
  SPLIT p_table AT '|' INTO gv_table gv_desc.
  PERFORM:f_get_table_val CHANGING gt_ddfields,
          f_toggle_sel_flds  USING gt_ddfields.
*At selection screen value request for fieldname
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fpth1.
  PERFORM f_file_f4.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_field1.
  PERFORM f_custom_f4_fld  USING 'P_FIELD1'
                        CHANGING gt_tabf4 gt_ddfields gt_selfld_value.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_field2.
  PERFORM f_custom_f4_fld USING 'P_FIELD2'
                       CHANGING gt_tabf4 gt_ddfields  gt_selfld_value.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_field3.
  PERFORM f_custom_f4_fld  USING 'P_FIELD3'
                        CHANGING gt_tabf4 gt_ddfields gt_selfld_value.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_field1-low.
  PERFORM f_custom_shlp USING 'S_FIELD1-LOW' gt_selfld_value
                     CHANGING gt_tabf4 gt_ddfields.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_field1-high.
  PERFORM f_custom_shlp USING 'S_FIELD1-HIGH' gt_selfld_value
                     CHANGING gt_tabf4 gt_ddfields.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_field2-low.
  PERFORM f_custom_shlp USING 'S_FIELD2-LOW'  gt_selfld_value
                     CHANGING gt_tabf4 gt_ddfields.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_field2-high.
  PERFORM f_custom_shlp USING 'S_FIELD2-HIGH' gt_selfld_value
                     CHANGING gt_tabf4 gt_ddfields.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_field3-low.
  PERFORM f_custom_shlp USING 'S_FIELD3-LOW'  gt_selfld_value
                     CHANGING gt_tabf4 gt_ddfields.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_field3-high.
  PERFORM f_custom_shlp USING 'S_FIELD3-HIGH' gt_selfld_value
                     CHANGING gt_tabf4 gt_ddfields.
*At selection screen---------------------------------------------------
AT SELECTION-SCREEN.
  PERFORM f_reset_rbut.
  PERFORM f_validations.
  PERFORM f_validate_input.
*At start-of-selection --------------------------------------------
START-OF-SELECTION.
* Perform the Primary Index calculation
  READ TABLE gt_ddfields INTO gs_ddfields INDEX 1.
  IF gs_ddfields-fieldname NE gc_spras.
    CLEAR gs_ddfields.
    READ TABLE gt_ddfields INTO gs_ddfields INDEX 2.
    IF gs_ddfields-fieldname NE gc_spras.
      CLEAR gs_ddfields.
      READ TABLE gt_ddfields INTO gs_ddfields INDEX 3.
      IF gs_ddfields-fieldname NE gc_spras.
        PERFORM f_primary_index USING gt_ddfields CHANGING gt_wh_fields.
      ENDIF."third primary key check for language key
    ENDIF."second primary key check for Language key
  ENDIF."first primary key check for language key

  PERFORM f_process.
*At end-of-selection --------------------------------------------
END-OF-SELECTION.
  PERFORM f_output.
