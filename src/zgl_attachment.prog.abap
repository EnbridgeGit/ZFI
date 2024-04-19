*&---------------------------------------------------------------------*
*& Report  ZGL_ATTACHMENT
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
* Program Name       :  ZGL_ATTACHMENT                                 *
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

REPORT  zgl_attachment NO STANDARD PAGE HEADING.

"--------------------------Data Declaration----------------------------"
INCLUDE zgl_attachment_top.

"---------------------------Selection Screen---------------------------"
INCLUDE zgl_attachment_scr.

"---------------------------Events-------------------------------------"
INCLUDE zgl_attachment_lgc.

"---------------------------Subroutines--------------------------------"
INCLUDE zgl_attachment_f01.
