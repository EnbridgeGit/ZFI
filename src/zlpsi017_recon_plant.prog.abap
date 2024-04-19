*&---------------------------------------------------------------------*
*& Program Name       : ZLPSI017_RECON_PLANT                           *
*& Author             : Krishna Kancherla                              *
*& Creation Date      : 03-Oct-12                                      *
*& Object ID          : I_CL2C_PS_017                                  *
*& Application Area   : Project Systems                                *
*& Transport Request  : DECK907435                                     *
*& Description        : Reconciliation SAP to Power Plant
*                       This program will pull accumulated GL Account
*                       Balances for specific Ledger & for a Company Code
*                       for a specific Period and keeps the extracted
*                       file in application/presentation server.
*                       T.Code for this Program is ZFI_RECON
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*                      Modification Log(Latest Version on Top)         *
*----------------------------------------------------------------------*
* Version No    : 2.0                                                   *
* Date          : 31-Oct-2012                                          *
* Modified By   : Krishna Kancherla                                    *
* Correction No : DECK907770                                           *
* Description   : 1) Changed the Delimiter to TAB in Ouput file        *
*                 2) Removed the Timestamp in the filename
*                 3) Move the -ve sign of amount field to front
*----------------------------------------------------------------------*
* Version No    : 1.0                                                   *
* Date          : 18-Oct-2012                                          *
* Modified By   : Krishna Kancherla                                    *
* Correction No : DECK907641                                           *
* Description   : Replace Ledger Code with Description in Ouput file   *
*----------------------------------------------------------------------*
REPORT  zlpsi017_recon_plant.

*----------------------------------------------------------------------*
* Include - Data Declarations                                          *
*----------------------------------------------------------------------*
INCLUDE zlpsi017_recon_plant_top.

*----------------------------------------------------------------------*
* Selection Screen.                                                    *
*----------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-001.

PARAMETERS: p_year    TYPE faglflext-ryear DEFAULT sy-datum+0(4) OBLIGATORY,
            p_perd(2) TYPE n               DEFAULT sy-datum+4(2) OBLIGATORY.

SELECT-OPTIONS: s_bukrs  FOR faglflext-rbukrs, "NO INTERVALS,
                s_racct  FOR faglflext-racct,
                s_ledger FOR faglflext-rldnr  NO INTERVALS.

SELECTION-SCREEN END OF BLOCK blk1.

SELECTION-SCREEN BEGIN OF BLOCK blk2 WITH FRAME TITLE text-002.

PARAMETERS: rb_pres RADIOBUTTON GROUP grp1 USER-COMMAND cmd,
            rb_appl RADIOBUTTON GROUP grp1  DEFAULT 'X'.

PARAMETERS: p_opath TYPE rlgrap-filename.

SELECTION-SCREEN END OF BLOCK blk2.

*----------------------------------------------------------------------*
* Initialization                                                       *
*----------------------------------------------------------------------*
INITIALIZATION.

  PERFORM f_select_xparam.


*----------------------------------------------------------------------*
* At Selection-Screen On Value Request                                 *
*----------------------------------------------------------------------*

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_opath.

  PERFORM f_f4_help USING p_opath.

*----------------------------------------------------------------------*
* Start-of-Selection                                                   *
*----------------------------------------------------------------------*

START-OF-SELECTION.

  PERFORM f_validate.

  PERFORM f_get_data.

*----------------------------------------------------------------------*
* End-of-Selection                                                     *
*----------------------------------------------------------------------*

END-OF-SELECTION.

  CHECK gv_err_flag = ''.

* Display Result
  PERFORM f_display_result.

  PERFORM f_generate_file.

*----------------------------------------------------------------------*
* Include - Subroutines                                                *
*----------------------------------------------------------------------*

  INCLUDE zlpsi017_recon_plant_sub.
