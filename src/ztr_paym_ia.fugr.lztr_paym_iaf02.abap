*&---------------------------------------------------------------------*
*&  Include           LZTR_PAYM_IAF02
*&---------------------------------------------------------------------*
************************************************************************
*                            Spectra Energy                            *
************************************************************************
*  Function Group:   ZTR_PAYM_IA                                       *
*  Include:          LZTR_PAYM_IAF02                                   *
*  Author:           Sajjad Ahmad                                      *
*  Date:             Jan 30, 2012                                      *
*  Track #:
*  Application Area: FICO                                              *
*                                                                      *
*  Description:      Treasury Payments IA - F02 Include - Forms        *
*                                                                      *
************************************************************************
*----------------------- CHANGE LOG -----------------------------------*
*  Date    TR # By      Description                                    *
* -------- ---- ------- ---------------------------------------------- *
************************************************************************

*&---------------------------------------------------------------------*
*&      Form  f_identify_interface
*&---------------------------------------------------------------------*
*       Search the full filename for the identity of the interface
*----------------------------------------------------------------------*
FORM f_identify_interface
  TABLES   cit_xparam      STRUCTURE gwa_xparam
  USING    iv_string       TYPE STRING
  CHANGING cv_interface_id TYPE zparamsubtype
           cv_filepath     TYPE fsnam
           cv_filename     TYPE fsnam
           cv_subrc        TYPE sysubrc.

  DATA:    lv_interface_id TYPE zparamsubtype,
           lv_filepath     TYPE fsnam,
           lv_filename     TYPE fsnam,
           lv_offset       TYPE I,
           lv_length       TYPE I,
           lv_i1           TYPE I,
           lv_i2           TYPE I,
           lv_i3           TYPE I,
           lv_token        TYPE STRING,
           lv_token_p      TYPE STRING.

  CLEAR    cit_xparam[].
  CLEAR    cv_interface_id.
  CLEAR    cv_filepath.
  CLEAR    cv_filename.
  CLEAR    cv_subrc.

  CLEAR    lv_interface_id.
  CLEAR    lv_filepath.
  CLEAR    lv_filename.

* Search the string using a pattern; return offset, length, and token

  PERFORM  f_parse_string_using_pattern
                              USING    iv_string
                                       gc_pattern_ia_id
                              CHANGING lv_offset
                                       lv_length
                                       lv_token
                                       cv_subrc.

  IF   ( cv_subrc EQ 0 ).
    IF   ( lv_token IS INITIAL ).
      RETURN.
    ENDIF.
  ELSE.
* Error
    RETURN.
  ENDIF.

*eject
* Set the interface ID
  TRANSLATE lv_token USING '\ '.
  CONDENSE  lv_token NO-GAPS.

  CLEAR                                lv_interface_id.
  MOVE     lv_token                 TO lv_interface_id.

* Retrieve the IA parameters for the interface ID
  SELECT   *
    INTO   TABLE cit_xparam
    FROM   zfit_xparam
   WHERE   paramtype = gc_paramtype_oi
     AND   subtype   = lv_interface_id.
  IF ( sy-subrc NE 0 ).
    CLEAR: cit_xparam[].
    RETURN.
  ENDIF.

* Identify the filename
  CLEAR                                lv_token.
  MOVE     iv_string                TO lv_token.

  DO 12 TIMES.

    IF   ( lv_token CS '\' ).
      lv_offset = sy-fdpos + 1.
      CLEAR                            lv_token_p.
      MOVE     lv_token+lv_offset   TO lv_token_p.
      CLEAR                            lv_token.
      MOVE     lv_token_p           TO lv_token.
    ELSE.
      EXIT.
    ENDIF.

  ENDDO.

  CLEAR                                lv_filename.
  MOVE     lv_token                 TO lv_filename.

* Identify the filepath
  CLEAR                                lv_token.
  MOVE     iv_string                TO lv_token.

  IF   ( lv_token CS lv_filename ).
    lv_offset = sy-fdpos.
    CLEAR                              lv_token_p.
    MOVE     lv_token+0(lv_offset)  TO lv_token_p.
  ENDIF.

  CLEAR                                lv_filepath.
  MOVE     lv_token_p               TO lv_filepath.

*eject
* Perform a validity check
  CLEAR    lv_i1.
  CLEAR    lv_i2.
  CLEAR    lv_i3.
  IF   ( lv_token CS lv_filepath ).
    lv_i1 = sy-fdpos.
  ENDIF.
  IF   ( lv_token CS lv_interface_id ).
    lv_i2 = sy-fdpos.
  ENDIF.
  IF   ( lv_token CS lv_filename ).
    lv_i3 = sy-fdpos.
  ENDIF.
  IF   ( lv_i1 LT lv_i2 ) AND ( lv_i2 LT lv_i3 ).
  ELSE.
    cv_subrc = 6. "raise identification_error
    RETURN.
  ENDIF.

  cv_interface_id = lv_interface_id.
  cv_filepath     = lv_filepath.
  cv_filename     = lv_filename.

ENDFORM.                    " f_identify_interface
*eject
*&---------------------------------------------------------------------*
*&      Form  f_parse_string_using_pattern
*&---------------------------------------------------------------------*
*       Search the string using pattern; return offset, length, & token
*----------------------------------------------------------------------*
FORM f_parse_string_using_pattern
  USING    iv_text    TYPE STRING
           iv_pattern TYPE STRING
  CHANGING cv_offset  TYPE I
           cv_length  TYPE I
           cv_token   TYPE STRING
           cv_subrc   TYPE sysubrc.

  DATA:    lv_success TYPE char1,
           lv_offset  TYPE I,
           lv_length  TYPE I,
           lv_token   TYPE STRING.

  DATA:    lr_matcher TYPE REF TO cl_abap_matcher.

  CLEAR    cv_offset.
  CLEAR    cv_length.
  CLEAR    cv_token.

  CHECK  ( cv_subrc   IS INITIAL ).

  IF     ( iv_text    IS INITIAL ).
    cv_subrc = 1. "raise no_text
    RETURN.
  ENDIF.

  IF     ( iv_pattern IS INITIAL ).
    cv_subrc = 2. "raise no_pattern
    RETURN.
  ENDIF.

* Search the string using the pattern
  TRY.

      CALL METHOD cl_abap_matcher=>contains
        EXPORTING
          pattern = iv_pattern
          text    = iv_text
        RECEIVING
          success = lv_success.

    CATCH cx_sy_matcher.
      cv_subrc = 3. "raise search_error

    CATCH cx_sy_regex.
      cv_subrc = 4. "raise pattern_error

  ENDTRY.

  IF     ( lv_success IS INITIAL ).
    RETURN.
  ENDIF.

*eject
* Get the offset and length

  CALL METHOD cl_abap_matcher=>get_object
    RECEIVING
      matcher = lr_matcher.

  TRY.

      lv_offset = lr_matcher->get_offset( ).

      lv_length = lr_matcher->get_length( ).

    CATCH CX_SY_MATCHER.
      cv_subrc = 5. "raise parse_error

  ENDTRY.

  IF     ( lv_length IS INITIAL ).
    RETURN.
  ENDIF.

* Set the token
  lv_token = iv_text+lv_offset(lv_length).

  IF     ( lv_token IS INITIAL ).
    RETURN.
  ENDIF.

  cv_offset = lv_offset.
  cv_length = lv_length.
  cv_token  = lv_token.

ENDFORM.                    " f_parse_string_using_pattern
*eject
*&---------------------------------------------------------------------*
*&      Form  f_submit_file_handler
*&---------------------------------------------------------------------*
*       Submit a job to archive the payment file in the interface arch
*----------------------------------------------------------------------*
FORM f_submit_file_handler
  TABLES   iit_xparam      STRUCTURE gwa_xparam
  USING    iv_interface_id TYPE zparamsubtype
           iv_filepath     TYPE fsnam
           iv_filename     TYPE fsnam
  CHANGING cv_subrc        TYPE sysubrc.

  CONSTANTS:
           lc_jobname_def  TYPE char25 VALUE 'ZTR_FH',
           lc_username_def TYPE char25 VALUE 'BATCH_FI'.

  DATA:    lv_jobname      TYPE btcjob,
           lv_jobcount     TYPE btcjobcnt,
           lv_program      TYPE programm,
           lv_uname        TYPE syuname,
           lv_token1       TYPE char25,
           lv_token2       TYPE char25,
           lv_token3       TYPE char25.

  DATA:    lwa_xparam      TYPE ty_wa_xparam,
           lwa_print_parameters
                           TYPE pri_params.

  CHECK  ( cv_subrc IS INITIAL ).

  CLEAR    lv_jobname.
  CLEAR    lv_jobcount.
  CLEAR    lv_program.
  CLEAR    lv_uname.
  CLEAR    lv_token1.
  CLEAR    lv_token2.
  CLEAR    lv_token3.

* Read the program parameter table for the file handler job defaults
  CLEAR                                lwa_xparam.
  READ     TABLE iit_xparam       INTO lwa_xparam
                              WITH KEY key1 = gc_fh_job.
  IF ( sy-subrc NE 0 ).
    CLEAR        lwa_xparam.
  ENDIF.

* Determine the user name
  CLEAR                                lv_uname.
  MOVE     lwa_xparam-value2        TO lv_uname.
  IF     ( lv_uname                 IS INITIAL ).
*   MOVE   lc_username_def          TO lv_uname.            "DECK904560
    MOVE   sy-uname                 TO lv_uname.            "DECK904560
  ENDIF.

*eject
* Determine the job name
  CLEAR                                lv_token1.
  MOVE     lwa_xparam-value1        TO lv_token1.
  IF     ( lv_token1                IS INITIAL ).
    MOVE       lc_jobname_def       TO lv_token1.
    MOVE       iv_interface_id      TO lv_token2.
    TRANSLATE                          lv_token2 USING '_ '.
    CONDENSE                           lv_token2 NO-GAPS.
    MOVE       iv_filename          TO lv_token3.
    TRANSLATE                          lv_token3 USING '_ '.
    CONDENSE                           lv_token3 NO-GAPS.
    CLEAR                              lv_jobname.
    CONCATENATE                        lv_token1 '_'
                                       lv_token2 '_'
                                       lv_token3
                                  INTO lv_jobname.
  ELSE.
    MOVE       iv_filename          TO lv_token2.
    CLEAR                              lv_jobname.
    CONCATENATE                        lv_token1 '_'
                                       lv_token2
                                  INTO lv_jobname.
  ENDIF.

* Open the job

  CALL FUNCTION 'JOB_OPEN'
    EXPORTING
      DELANFREP        = ' '
      JOBGROUP         = ' '
      JOBNAME          = lv_jobname
      SDLSTRTDT        = sy-datum
      SDLSTRTTM        = sy-uzeit
*     JOBCLASS         =
    IMPORTING
      JOBCOUNT         = lv_jobcount
    EXCEPTIONS
      CANT_CREATE_JOB  = 1
      INVALID_JOB_DATA = 2
      JOBNAME_MISSING  = 3
      OTHERS           = 4.

  IF ( sy-subrc NE 0 ).
    cv_subrc = 11. "raise job_open_error
    RETURN.
  ENDIF.

*eject
* Set the print parameters

  CALL FUNCTION 'GET_PRINT_PARAMETERS'
    EXPORTING
      IMMEDIATELY            = ' '
      NO_DIALOG              = gc_x
    IMPORTING
      OUT_PARAMETERS         = lwa_print_parameters
    EXCEPTIONS
      ARCHIVE_INFO_NOT_FOUND = 1
      INVALID_PRINT_PARAMS   = 2
      INVALID_ARCHIVE_PARAMS = 3
      OTHERS                 = 4.

  IF ( sy-subrc NE 0 ).
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

* Submit the program via job
  CLEAR                                lv_program.
  MOVE     gc_program_fh            TO lv_program.

  SUBMIT  (lv_program)
                     VIA JOB lv_jobname NUMBER lv_jobcount
                    USER lv_uname
                    WITH p_intrty = gc_paramtype_oi
                    WITH p_intrfc = iv_interface_id
                    WITH p_filpth = iv_filepath
                    WITH p_filnam = iv_filename
                      TO SAP-SPOOL AND RETURN WITHOUT SPOOL DYNPRO
                         SPOOL PARAMETERS lwa_print_parameters.

  IF ( sy-subrc NE 0 ).
    cv_subrc = 12. "raise job_submit_error
    RETURN.
  ENDIF.

*eject
* Close the job

  CALL FUNCTION 'JOB_CLOSE'
    EXPORTING
      JOBCOUNT             = lv_jobcount
      JOBNAME              = lv_jobname
      STRTIMMED            = gc_x
    EXCEPTIONS
      CANT_START_IMMEDIATE = 1
      INVALID_STARTDATE    = 2
      JOBNAME_MISSING      = 3
      JOB_CLOSE_FAILED     = 4
      JOB_NOSTEPS          = 5
      JOB_NOTEX            = 6
      LOCK_FAILED          = 7
      INVALID_TARGET       = 8
      OTHERS               = 9.

  IF ( sy-subrc NE 0 ).
    cv_subrc = 13. "raise job_close_error
    RETURN.
  ENDIF.

ENDFORM.                    " f_submit_file_handler
