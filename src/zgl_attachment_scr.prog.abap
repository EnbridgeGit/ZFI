*&---------------------------------------------------------------------*
*&  Include           ZGL_ATTACHMENT_SCR
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
* Program Name       :  ZGL_ATTACHMENT                                 *
* Include            :  ZGL_ATTACHMENT_SCR                             *
* Author             :  Manvitha Dadi                                  *
* Date               :  28-Jul-2021                                    *
* Technical Contact  :  Ashok Madasu/Manvitha Dadi                     *
* Purpose            :  Attach any file from appplication server       *
*                       to FB03 Screen                                 *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By    CTS        Description                    *
* ---------------------------------------------------------------------*
* 28-Jul-2021  DADIM         D30K931398 CHG0221895 - Create custom     *
*                                       tcode for FB03/FBV3 attachments*
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: p_belnr  TYPE belnr_d OBLIGATORY,
            p_bukrs TYPE bukrs OBLIGATORY,
            p_gjahr TYPE gjahr OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-014.
PARAMETERS: p_dir RADIOBUTTON GROUP rad1 USER-COMMAND ucom1,
            p_net RADIOBUTTON GROUP rad1 DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-005.
PARAMETERS: p_appl RADIOBUTTON GROUP rad2 USER-COMMAND ucom2,
            p_pres RADIOBUTTON GROUP rad2 DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-010.
PARAMETERS: p_file1 TYPE string LOWER CASE MODIF ID m1.
SELECTION-SCREEN END OF BLOCK b4.

SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE text-013.
PARAMETERS: p_file2 TYPE string MODIF ID m2 DEFAULT 'H:\'.
SELECTION-SCREEN END OF BLOCK b5.
