*&---------------------------------------------------------------------*
*&  Include           ZFAPI029_DOA_EXTRACT_TOP
*&---------------------------------------------------------------------*

************************************************************************
*                            Spectra Energy                            *
************************************************************************
*  Program:          ZFAPI029_DOA_EXTRACT                              *
*  Include:          ZFAPI029_DOA_EXTRACT_TOP                          *
*  Author:           MNagarathina                                      *
*  Date:             August 09, 2012                                   *
*  Application Area: FICO AP                                           *
*                                                                      *
*  Description:      AP DOA and User Roles Extract                     *
*                                                                      *
*                                                                      *
************************************************************************
*----------------------- CHANGE LOG -----------------------------------*
*  Date      By           Transport   Description                      *
* ---------- ------------ ----------  -------------------------------- *
*                                                                      *
*----------------------------------------------------------------------*
************************************************************************

*eject
************************************************************************
*                          Custom Data Types                           *
************************************************************************
TYPES:  ty_wa_xparam     TYPE zfit_xparam.       "Parameter Master

TYPES:  ty_it_xparam     TYPE STANDARD TABLE OF ty_wa_xparam.

TYPES:  ty_wa_t003t      TYPE t003t,
* Begin of Changes by <Chaya>
*        ty_wa_zfit_doa   TYPE zfit_doa,
         ty_wa_zfit_doa   TYPE zfit_doa_new,
* End of Changes by <Chaya.
        ty_wa_agr_users  TYPE agr_users.

TYPES:  ty_it_t003t      TYPE STANDARD TABLE OF ty_wa_t003t,
        ty_it_zfit_doa   TYPE STANDARD TABLE OF ty_wa_zfit_doa,
        ty_it_agr_users  TYPE STANDARD TABLE OF ty_wa_agr_users.

TYPES: BEGIN OF ty_wa_msgs,                      "Message List
        line             TYPE text132,
       END   OF ty_wa_msgs.

TYPES:  ty_it_msgs       TYPE STANDARD TABLE OF ty_wa_msgs.

TYPES: BEGIN OF ty_wa_errs,                      "Error List
        line             TYPE text132,
       END   OF ty_wa_errs.

TYPES:  ty_it_errs       TYPE STANDARD TABLE OF ty_wa_errs.

TYPES: BEGIN OF ty_wa_output,                    "Output
        record           TYPE string,            "Record
       END   OF ty_wa_output.

TYPES:  ty_it_output     TYPE STANDARD TABLE OF ty_wa_output.

TYPES:  ty_wa_list_text  TYPE solisti1.          "Single List Text

TYPES:  ty_it_list_text  TYPE STANDARD TABLE OF ty_wa_list_text.

************************************************************************
*                              Constants                               *
************************************************************************
CONSTANTS:
        gc_x             TYPE char1              "X / Yes / True
                         VALUE 'X',
        gc_c             TYPE char1              "C / Copy
                         VALUE 'C',
        gc_m             TYPE char1              "M / Move
                         VALUE 'M',
        gc_r             TYPE char1              "C / Rename
                         VALUE 'R',
        gc_pipelit       TYPE char4              "Delimiter-Pipe Literal
                         VALUE 'PIPE',
        gc_abap_util     TYPE char25             "Class ABAP Char Util
                         VALUE 'CL_ABAP_CHAR_UTILITIES',
        gc_filename      TYPE zparamvalue        "Filename
                         VALUE 'SAP_USRECONyyyymmddhhmmss.txt',
        gc_extension_arch                        "File Extension-Archive
                         TYPE char4
                         VALUE '.ARC',
        gc_extension_err TYPE char4              "File Extension-Error
                         VALUE '.ERR'.

CONSTANTS:
        gc_param_out_int TYPE zparamkey          "Outbound Interface
                         VALUE 'OUTBOUND_INTERFACE',
        gc_param_obj_id  TYPE zparamkey          "Object ID
                         VALUE 'I_P2C_AP_029',
        gc_filepath_out  TYPE zparamkey          "Output Filepath
                         VALUE 'OUTPUT_FILEPATH',
        gc_filepath_arch TYPE zparamkey          "Archive Filepath
                         VALUE 'ARCHIVE_FILEPATH',
        gc_filepath_err  TYPE zparamkey          "Error Filepath
                         VALUE 'ERROR_FILEPATH',
        gc_file_delimit  TYPE zparamkey          "File Delimiter
                         VALUE 'FILE_DELIMITER',
        gc_email         TYPE zparamkey          "Email
                         VALUE 'EMAIL',
        gc_email_id      TYPE zparamkey          "Email ID
                         VALUE 'EMAIL_ID',
        gc_email_std_txt TYPE zparamkey          "Email Standard Text
                         VALUE 'STANDARD_TEXT'.

CONSTANTS:
        gc_modif_id_dsp  TYPE char3              "ModifID-Display Only
                         VALUE 'DSP',
        gc_modif_id_dlm  TYPE char3              "ModifID-Input Delimite
                         VALUE 'DLM',
        gc_modif_id_fpt  TYPE char3              "ModifID-Input Filepath
                         VALUE 'FPT',
        gc_modif_id_fnm  TYPE char3              "ModifID-Input Filename
                         VALUE 'FNM',
        gc_modif_id_ofp  TYPE char3              "ModifID-Input output filenanme
                         VALUE 'OFP'.

************************************************************************
*                              Variables                               *
************************************************************************
DATA:   gv_ucomm         TYPE syucomm,           "Function Code
        gv_ok_code       TYPE syucomm,           "Function Code
        gv_sysid         TYPE sysysid.           "System ID

DATA:   gv_obj_id        TYPE char14,            "Object ID
        gv_flag_pres     TYPE flag.              "Flag-PresServer-Output

DATA:   gv_filename_ff   TYPE text1024,          "Filename - Path & Name
        gv_filename_fn   TYPE text128,           "Filename - Name Only
        gv_count_recs    TYPE syindex,           "Count-Records
        gv_count_errs    TYPE syindex,           "Count-Errors
        gv_flag_error    TYPE flag.              "Flag-Error

DATA:   gv_ff_doctype    TYPE text1024,
        gv_ff_doctype_ug    TYPE text1024,
        gv_ff_doctype_sw    TYPE text1024,
        gv_ff_doa        TYPE text1024,
        gv_ff_agrusers   TYPE text1024.

DATA:   gv_ff_ofname     TYPE text1024.

************************************************************************
*                              Structures                              *
************************************************************************
DATA:   gwa_output       TYPE ty_wa_output.      "Output

************************************************************************
*                           Internal Tables                            *
************************************************************************
DATA:   git_xparam       TYPE ty_it_xparam.      "Parameter Master

DATA:   git_t003t        TYPE ty_it_t003t,
        git_t003t_ug        TYPE ty_it_t003t,
        git_t003t_sw        TYPE ty_it_t003t,
        git_zfit_doa     TYPE ty_it_zfit_doa,
        git_agr_users    TYPE ty_it_agr_users.

DATA:   gwa_t003t        TYPE ty_wa_t003t,
        gwa_zfit_doa     TYPE ty_wa_zfit_doa,
        gwa_agr_users    TYPE ty_wa_agr_users.

DATA:   git_msgs         TYPE ty_it_msgs.        "Message List

DATA:   git_errs         TYPE ty_it_errs.        "Error List

************************************************************************
*                          Reference Variables                         *
************************************************************************
DATA:   gv_table         TYPE REF TO cl_salv_table,
        gv_columns       TYPE REF TO cl_salv_columns_table,
        gv_column        TYPE REF TO cl_salv_column_table.

*eject
************************************************************************
*                           Selection Screen                           *
************************************************************************

* Run Options
SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: BEGIN OF BLOCK ssb1 WITH FRAME TITLE text-010.
SELECTION-SCREEN: SKIP 1.
PARAMETERS:       p_objid  TYPE zparamkey
                           VISIBLE LENGTH 12
                           OBLIGATORY
                           MODIF ID dsp.
SELECTION-SCREEN: COMMENT 50(45) p_objdsc.
PARAMETERS:       p_email  TYPE so_recname.
SELECTION-SCREEN: SKIP 1.
PARAMETERS:       cb_test  TYPE c
                           AS CHECKBOX
                           DEFAULT gc_x
                           USER-COMMAND cmd.

* Output File
SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: BEGIN OF BLOCK ssb2 WITH FRAME TITLE text-020.
SELECTION-SCREEN: SKIP 1.
PARAMETERS:       rb_appl  RADIOBUTTON GROUP rbg1
                           DEFAULT 'X'
                           USER-COMMAND cmd.
PARAMETERS:       rb_pres  RADIOBUTTON GROUP rbg1.
SELECTION-SCREEN: SKIP 1.
PARAMETERS:       p_delim  TYPE zparamkey
                           AS LISTBOX VISIBLE LENGTH 12
                           DEFAULT gc_pipelit
                           MODIF ID dlm.
PARAMETERS:       p_ofpath TYPE localfile
                           MODIF ID ofp.
PARAMETERS:       p_fname  TYPE localfile
                           MODIF ID fnm.
PARAMETERS:       p_fpath  TYPE localfile
                           MODIF ID fpt.
SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: END   OF BLOCK ssb2.

SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: END   OF BLOCK ssb1.
