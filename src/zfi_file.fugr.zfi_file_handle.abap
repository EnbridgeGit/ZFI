FUNCTION ZFI_FILE_HANDLE.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_SOURCE_DIR) TYPE  SXPGCOLIST-PARAMETERS
*"     REFERENCE(I_TARGET_DIR) TYPE  SXPGCOLIST-PARAMETERS OPTIONAL
*"     REFERENCE(I_SOURCE_FNAME) TYPE  SXPGCOLIST-PARAMETERS
*"     REFERENCE(I_TARGET_FNAME) TYPE  SXPGCOLIST-PARAMETERS
*"     REFERENCE(I_COMMAND) TYPE  C
*"     REFERENCE(I_DATE_TIME_STAMP) TYPE  C OPTIONAL
*"     REFERENCE(I_RENAME_ARC_TO_NEW) TYPE  C OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_RETURN) TYPE  BAPIRETURN
*"--------------------------------------------------------------------
*&---------------------------------------------------------------------*
*& Function Module    :  ZFI_FILE_HANDLE                               *
*& Author             :  Vijay Rajaputra (Copy of US System)           *
*& Creation Date      :  02-Aug-2018                                   *
*& Object ID          :  NA                                            *
*& Application Area   :  NA                                            *
*& Description        :  Common FM for File Archiving                  *
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                      Modification Log(Latest Version on Top)         *
*----------------------------------------------------------------------*
* Version No    :                                                      *
* Date          :                                                      *
* Modified By   :                                                      *
* Correction No :                                                      *
* Description   :                                                      *
*----------------------------------------------------------------------*

  CLEAR    e_return.

  CONCATENATE i_source_dir i_source_fname
              INTO gv_source_file.

  CONDENSE gv_source_file.

  CONCATENATE i_target_dir i_target_fname
              INTO gv_target_file.

  IF i_date_time_stamp = gc_x.
    CONCATENATE gv_target_file
                gc_sep
                sy-datum
                gc_sep
                sy-uzeit
                INTO gv_target_file.
  ENDIF.

  IF i_rename_arc_to_new = gc_x.
    IF gv_target_file CS gc_arc.
      REPLACE FIRST OCCURRENCE OF gc_arc IN gv_target_file WITH gc_new.
    ELSE.
      CONCATENATE gv_target_file gc_new INTO gv_target_file.
    ENDIF.
  ENDIF.

  CONDENSE gv_target_file.

  CONCATENATE gv_source_file gv_target_file
              INTO gv_file
              SEPARATED BY space.

  IF i_command = gc_c.
    gv_command = gc_copy_command.
  ELSEIF i_command = gc_m.
    gv_command = gc_move_command.
  ELSEIF i_command = gc_r.
    gv_command = gc_ren_command.
  ENDIF.

  CLEAR    gv_status.
  CLEAR    gv_exitcode.

  CLEAR    git_exec_protocol[].

  CALL FUNCTION 'SXPG_COMMAND_EXECUTE'
    EXPORTING
      commandname                   = gv_command
      additional_parameters         = gv_file
    IMPORTING
      status                        = gv_status
      exitcode                      = gv_exitcode
    TABLES
      exec_protocol                 = git_exec_protocol
    EXCEPTIONS
      no_permission                 = 1
      command_not_found             = 2
      parameters_too_long           = 3
      security_risk                 = 4
      wrong_check_call_interface    = 5
      program_start_error           = 6
      program_termination_error     = 7
      x_error                       = 8
      parameter_expected            = 9
      too_many_parameters           = 10
      illegal_command               = 11
      wrong_asynchronous_parameters = 12
      cant_enq_tbtco_entry          = 13
      jobcount_generation_error     = 14
      OTHERS                        = 15.

  gv_subrc = sy-subrc.

  IF  ( ( gv_subrc NE 0 ) OR ( gv_status EQ gc_e ) ).

    CLEAR                                   e_return.
    MOVE     gc_e                        TO e_return-type.

    IF     ( i_command                   EQ gc_c ).
      MOVE   text-001                    TO e_return-message_v1.
    ELSEIF ( i_command                   EQ gc_m ).
      MOVE   text-002                    TO e_return-message_v1.
    ELSEIF ( i_command                   EQ gc_r ).
      MOVE   text-003                    TO e_return-message_v1.
    ENDIF.

    CLEAR                                   gwa_exec_protocol.
    READ     TABLE git_exec_protocol   INTO gwa_exec_protocol INDEX 1.
    IF ( sy-subrc EQ 0 ).
      MOVE  gwa_exec_protocol-message+00(50)
                                         TO e_return-message_v2.
      MOVE  gwa_exec_protocol-message+50(50)
                                         TO e_return-message_v3.
      MOVE  gwa_exec_protocol-message+100(28)
                                         TO e_return-message_v4.
    ELSE.
      CLEAR gwa_exec_protocol.
    ENDIF.

    IF     ( e_return-message_v1     IS     INITIAL ) AND
           ( e_return-message_v2     IS NOT INITIAL ).
      MOVE   gwa_exec_protocol-message   TO e_return-message.
    ELSEIF ( e_return-message_v1     IS NOT INITIAL ) AND
           ( e_return-message_v2     IS     INITIAL ).
      MOVE   e_return-message_v1         TO e_return-message.
    ELSEIF ( e_return-message_v1     IS NOT INITIAL ) AND
           ( e_return-message_v2     IS NOT INITIAL ).
      CONCATENATE                           e_return-message_v1 ':'
                                            gwa_exec_protocol-message
                                       INTO e_return-message
                                            SEPARATED BY SPACE.
    ENDIF.
  ENDIF.



ENDFUNCTION.
