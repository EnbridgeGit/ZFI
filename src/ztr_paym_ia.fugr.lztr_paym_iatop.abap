FUNCTION-POOL ZTR_PAYM_IA.                  "MESSAGE-ID ..
************************************************************************
*                            Spectra Energy                            *
************************************************************************
*  Function Group:   ZTR_PAYM_IA                                       *
*  Include:          LZTR_PAYM_IATOP                                   *
*  Author:           Sajjad Ahmad                                      *
*  Date:             Jan 30, 2012                                      *
*  Track #:          Developed as required by US                       *
*  Application Area: FICO                                              *
*                                                                      *
*  Description:      Treasury Payments IA - Top Include - Data Decls.  *
*                                                                      *
************************************************************************
*----------------------- CHANGE LOG -----------------------------------*
*  Date    TR # By      Description                                    *
* -------- ---- ------- ---------------------------------------------- *
************************************************************************

************************************************************************
*                                Tables                                *
************************************************************************
TABLES: zfit_xparam.                             "Parameter Master

************************************************************************
*                          Custom Data Types                           *
************************************************************************
TYPES:  ty_wa_return     TYPE bapiret2.          "Return BAPIRET2

TYPES:  ty_it_return     TYPE STANDARD TABLE OF ty_wa_return.

TYPES:  ty_wa_xparam     TYPE zfit_xparam.       "Parameter Master

TYPES:  ty_it_xparam     TYPE STANDARD TABLE OF ty_wa_xparam.

*eject
************************************************************************
*                              Constants                               *
************************************************************************
CONSTANTS:
        gc_x             TYPE char1              "X / True / Yes
                         VALUE 'X',
        gc_e             TYPE char1              "E / Error
                         VALUE 'E',
        gc_c             TYPE char1              "C / Copy
                         VALUE 'C',
        gc_m             TYPE char1              "M / Move
                         VALUE 'M',
        gc_r             TYPE char1              "R / Rename
                         VALUE 'R',
        gc_s             TYPE char1              "S / Success
                         VALUE 'S',
        gc_u             TYPE char1              "U / Recvr Type-IntAddr
                         VALUE 'U',
        gc_msgid         TYPE symsgid            "Message ID / Class
                         VALUE ' ',
        gc_file_handler  TYPE zparamkey          "Key-File Handler
                         VALUE 'FILE_HANDLER',
        gc_fh_job        TYPE zparamkey          "Key-File Hndlr Job Dfl
                         VALUE 'FILE_HANDLER_JOB',             "Defaults
        gc_change_filename                       "Key-Change Filename
                         TYPE zparamkey
                         VALUE 'CHANGE_FILENAME',
        gc_rename_file   TYPE zparamkey          "Key-Rename File
                         VALUE 'RENAME_FILE',
        gc_archive_file  TYPE zparamkey          "Key-Archive File
                         VALUE 'ARCHIVE_FILE',
        gc_append_dts    TYPE zparamkey          "Key-Append Date-DTS
                         VALUE 'APPEND_DTS',
        gc_append_file_ext                       "Key-Append File Extnsn
                         TYPE zparamkey
                         VALUE 'APPEND_FILE_EXT',
        gc_copy          TYPE zparamkey          "Key-Archive-Copy
                         VALUE 'COPY',
        gc_move          TYPE zparamkey          "Key-Archive-MOVE
                         VALUE 'MOVE',
        gc_email         TYPE zparamkey          "Key-Email
                         VALUE 'EMAIL',
        gc_email_recipnt TYPE zparamkey          "Key-Email Recipient
                         VALUE 'RECIPIENT',
        gc_email_std_txt TYPE zparamkey          "Key-Email Standard Txt
                         VALUE 'STANDARD_TEXT',
        gc_error         TYPE zparamkey          "Key-Error
                         VALUE 'ERROR',
        gc_job_name      TYPE zparamkey          "Key-Job Name
                         VALUE 'JOB_NAME'.

*eject
CONSTANTS:
        gc_com_typ_email TYPE so_snd_art         "Communicatn Type-Email
                         VALUE 'INT',
        gc_obj_sns       TYPE so_obj_sns         "Object Sensitivity
                         VALUE 'O',
        gc_id_standard   TYPE tdid               "Read Text ID-Standard
                         VALUE 'ST',
        gc_langu_english TYPE spras              "Read Text Lang-English
                         VALUE 'E',
        gc_object_text   TYPE tdobject           "Read Text Object-Text
                         VALUE 'TEXT',
        gc_doc_type_raw  TYPE so_obj_tp          "Document Type-Raw
                         VALUE 'RAW'.

CONSTANTS:
        gc_pattern_ia_id TYPE STRING             "Interface Architctr ID
           VALUE '\\I_\w{3}\_(AP|TR)_\d{3,5}\\',
        gc_paramtype_oi  TYPE zparamtype         "Parameter Type - OI
                         VALUE 'OUTBOUND_INTERFACE',
        gc_program_fh    TYPE programm           "Program-File Handler
                         VALUE 'ZTR_PAYM_IA_FILE_HANDLER'.

************************************************************************
*                              Variables                               *
************************************************************************
DATA:   gv_subrc         TYPE sysubrc,           "Return Code
        gv_string        TYPE STRING.            "Character String

DATA:   gv_interface_type                        "Interface Type
                         TYPE zparamtype,
        gv_interface_id  TYPE zparamsubtype,     "Interface ID
        gv_filepath      TYPE fsnam,             "Filepath
        gv_filename      TYPE fsnam.             "Filename

DATA:   gv_laufd         TYPE laufd,             "Payment Run Date
        gv_laufi         TYPE laufi,             "Payment Run Identifctn
        gv_xvorl         TYPE xvorl_fpm,         "Payment Run Proposal
        gv_xdme          TYPE xdme1_fpm,         "Payment Medium Indictr
        gv_xdme_file_system                      "DME File System
                         TYPE dfilesyst,         "1=TemSe / 2=FileSystem
        gv_dme_file_name TYPE fsnam,             "DME Filename
        gv_dme_file_name_scr                     "DME Filename Screen
                         TYPE fsnam.             "DME Filename

*eject
************************************************************************
*                              Structures                              *
************************************************************************
DATA:   gwa_return       TYPE ty_wa_return.      "Return BAPIRET2

DATA:   gwa_xparam       TYPE ty_wa_xparam.      "Parameter Master

DATA:   gwa_document_data                        "API Content
                         TYPE sodocchgi1.

DATA:   gwa_packing_list TYPE sopcklsti1.        "API Packing List

DATA:   gwa_contents_txt TYPE solisti1.          "API Content

DATA:   gwa_receivers    TYPE somlreci1.         "API Recipient List

************************************************************************
*                           Internal Tables                            *
************************************************************************
DATA:   git_return       TYPE ty_it_return.      "Return BAPIRET2

DATA:   git_xparam       TYPE ty_it_xparam.      "Parameter Master

DATA:   git_packing_list TYPE TABLE OF sopcklsti1."API Packing List

DATA:   git_contents_txt TYPE TABLE OF solisti1. "API Content

DATA:   git_receivers    TYPE TABLE OF somlreci1."API Recipient List
