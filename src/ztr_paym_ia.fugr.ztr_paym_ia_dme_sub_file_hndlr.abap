FUNCTION ZTR_PAYM_IA_DME_SUB_FILE_HNDLR.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_LAUFD) TYPE  LAUFD OPTIONAL
*"     VALUE(I_LAUFI) TYPE  LAUFI OPTIONAL
*"     VALUE(I_XVORL) TYPE  XVORL_FPM OPTIONAL
*"     VALUE(I_XDME) TYPE  XDME1_FPM OPTIONAL
*"     VALUE(I_XDME_FILE_SYSTEM) TYPE  DFILESYST OPTIONAL
*"     VALUE(I_DME_FILE_NAME) TYPE  FSNAM OPTIONAL
*"     VALUE(I_DME_FILE_NAME_SCREEN) TYPE  FSNAM OPTIONAL
*"  EXCEPTIONS
*"      SUBMIT_TO_IA_ERROR
*"----------------------------------------------------------------------
************************************************************************
*                            Spectra Energy                            *
************************************************************************
*  Function Module:  ZTR_PAYM_IA_DME_SUB_FILE_HNDLR                    *
*  Function Group:   ZTR_PAYM_IA                                       *
*  Author:           Sajjad Ahmad                                      *
*  Date:             Jan 30, 2012                                      *
*  Track #:                                                            *
*  Application Area: FICO                                              *
*                                                                      *
*  Description:                                                        *
*  - Treasury Payments Interface Architecture DME Submit File Handler  *
*                                                                      *
*    This function module is used to submit a job that will archive    *
*    a Treasury Payment DME file on the file system.                   *
*                                                                      *
*    Import Parameters                                                 *
*                                                                      *
*      I_LAUFD                "Payment Run Date                        *
*      I_LAUFI                "Payment Run Identifier                  *
*      I_XVORL                "Payment Run Proposal                    *
*      I_XDME                 "Payment Medium Indicator                *
*      I_XDME_FILE_SYSTEM     "DME File System 1=TemSe / 2=FileSystem  *
*      I_DME_FILE_NAME        "DME Filename                            *
*      I_DME_FILE_NAME_SCREEN "DME Filename Screen                     *
*                                                                      *
*    For the function to successfully submit a background job, the     *
*    following import parameters must be set as follows:               *
*                                                                      *
*      I_XVORL                EQ SPACE (i.e. not a payment proposal)   *
*      I_XDME                 EQ 'X'                                   *
*      I_XDME_FILE_SYSTEM     NE '1' (i.e. EQ '2')                     *
*      I_DME_FILE_NAME        full file path and name (w/ sequence no. *
*      I_DME_FILE_NAME_SCREEN full file path and name                  *
*                                                                      *
************************************************************************
*----------------------- CHANGE LOG -----------------------------------*
*  Date    TR # By      Description                                    *
* -------- ---- ------- ---------------------------------------------- *
************************************************************************

*eject
  CLEAR                                gv_laufd.
  MOVE     i_laufd                  TO gv_laufd.
  CLEAR                                gv_laufi.
  MOVE     i_laufi                  TO gv_laufi.
  CLEAR                                gv_xvorl.
  MOVE     i_xvorl                  TO gv_xvorl.
  CLEAR                                gv_xdme.
  MOVE     i_xdme                   TO gv_xdme.
  CLEAR                                gv_xdme_file_system.
  MOVE     i_xdme_file_system       TO gv_xdme_file_system.
  CLEAR                                gv_dme_file_name.
  MOVE     i_dme_file_name          TO gv_dme_file_name.
  CLEAR                                gv_dme_file_name_scr.
  MOVE     i_dme_file_name_screen   TO gv_dme_file_name_scr.

* Validate the import parameters
  IF   ( ( gv_xvorl                 IS INITIAL ) AND
         ( gv_xdme              IS NOT INITIAL ) AND
         ( gv_xdme_file_system      NE 1       ) AND
         ( gv_dme_file_name     IS NOT INITIAL ) AND
         ( gv_dme_file_name_scr IS NOT INITIAL )     ).
  ELSE.
    RETURN.
  ENDIF.

* Search the full filename for the identity of the interface
  CLEAR                                gv_string.
  MOVE     gv_dme_file_name         TO gv_string.

  CLEAR                                git_xparam[].
  CLEAR                                gv_interface_id.
  CLEAR                                gv_filepath.
  CLEAR                                gv_filename.
  CLEAR                                gv_subrc.

  PERFORM  f_identify_interface
                              TABLES   git_xparam
                              USING    gv_string
                              CHANGING gv_interface_id
                                       gv_filepath
                                       gv_filename
                                       gv_subrc.

  IF   ( gv_subrc EQ 0 ).
    IF   ( gv_interface_id IS INITIAL ).
      RETURN.
    ENDIF.
  ELSE.
* Error
    RAISE    submit_to_ia_error.
    RETURN.
  ENDIF.

*eject
* Submit a job to archive the payment file in the interface architecture

  PERFORM  f_submit_file_handler
                              TABLES   git_xparam
                              USING    gv_interface_id
                                       gv_filepath
                                       gv_filename
                              CHANGING gv_subrc.

  IF ( gv_subrc NE 0 ).
* Error
    RAISE    submit_to_ia_error.
    RETURN.
  ENDIF.


ENDFUNCTION.
