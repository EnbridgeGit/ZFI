*&---------------------------------------------------------------------*
*& Report  ZFAPR021_IN_HOUSE_CHECK
*&---------------------------------------------------------------------*
REPORT  ZFAPR021_IN_HOUSE_CHECK  MESSAGE-ID zfi01
                                 LINE-SIZE 120
                                 NO STANDARD PAGE HEADING.

*&---------------------------------------------------------------------*
*& Program Name       :  ZFAPR021_IN_HOUSE_CHECK                       *
*& Author             :  Babumiya Mohammad                             *
*& Creation Date      :  23-Aug-2011                                   *
*& Object ID          :  F_P2C_AP_003_In_House_Check_(AP) US Instance  *
*& Application Area   :  FI-AP                                         *
*& Description        :  In House Check Printing                       *
*&                       Copy of US program ZFAPF003_IN_HOUSE_CHECK    *
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                           Modification Log                           *
*----------------------------------------------------------------------*
* Version No    : 1                                                    *
* Date          : 23-Aug-2011                                          *
* Modified By   : Babumiya Mohammad                                    *
* Correction No : DECK900992                                           *
* Description   : Initial program development                          *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Version No    : 2                                                    *
* Date          : 20-Aug-2013                                          *
* Modified By   : John Hartung                                         *
* Correction No : D30K922271, D30K923471, D30K924001                   *
* Description   : SDP42671-Convert to UG systems                       *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Version No    : 3                                                    *
* Date          : 02-Jan-2015                                          *
* Modified By   : John Hartung                                         *
* Correction No : D30K925153, D30K925270, D30K925486                   *
* Description   : SDP42671 1. Add invoice header long text             *
*                          2. Correct payment advice total/subtotal    *
*                 SDP86117 D30K925924 - retransport to correct         *
*                          import errors (wrong order)import errors    *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Version No    : 4                                                    *
* Date          : 04-Feb-2016                                          *
* Modified By   : John Hartung                                         *
* Correction No : D30K926579                                           *
* Description   : ACR-307  1. Change the address of the Spectra office *
*                             location that is printed on the check    *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Version No    : 5                                                    *
* Date          : 25-Jan-2017                                          *
* Modified By   : John Hartung                                         *
* Correction No : D30K927881                                           *
* Description   : ACR-2755 1. Enhancements for Enbridge integration    *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Version No    : 6                                                    *
* Date          : 10-Apr-2017                                          *
* Modified By   : John Hartung                                         *
* Correction No : D30K928144                                           *
* Description   : ACR-4084 JPMC In-House Checks - format remittance text
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Version No    : 7                                                    *
* Date          : 28-Mar-2019                                          *
* Modified By   : John Hartung                                         *
* Correction No : D30K929884                                           *
* Description   : CHG0148429 - Incorporate TD Bank                     *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Version No    : 8                                                    *
* Date          : 10-Jun-2019                                          *
* Modified By   : John Hartung                                         *
* Correction No : D30K930133                                           *
* Description   : CHG0148429 - Select Options for House Bank and Accnt *
*                              Add option to override Company Name     *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Version No    : 9                                                    *
* Date          : 20-Sep-2019                                          *
* Modified By   : John Hartung                                         *
* Correction No : D30K930167                                           *
* Description   : CHG0160810 - Add US Check Form to SW and UG          *
*                              Add Payment Method P "Post-Dated"       *
*----------------------------------------------------------------------*

INCLUDE zfapr021_in_house_check_top.

************************************************************************
*                            Initialization                            *
************************************************************************
INITIALIZATION.

  PERFORM  f_set_payment_method     USING   SPACE.          "D30K929884

  PERFORM  f_set_check_form         USING   SPACE.          "D30K929884

*eject
************************************************************************
*                         At Selection-Screen                          *
************************************************************************
AT SELECTION-SCREEN OUTPUT.

* Disable the selection-screen inputs
  PERFORM  f_disable_input_fields.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_laufd.

* Get the input values for the run date
  PERFORM  f_f4_help_laufd_laufi.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_laufi.

* Get the input values for the run ID
  PERFORM  f_f4_help_laufd_laufi.

AT SELECTION-SCREEN ON s_zbukr.

* Validate the paying company code
  PERFORM  f_validate_company_code.

AT SELECTION-SCREEN ON p_hbkid.

* Validate the house bank ID
  PERFORM  f_validate_house_bank_id.

AT SELECTION-SCREEN ON p_hktid.

* Validate the account ID
  PERFORM  f_validate_account_id.

AT SELECTION-SCREEN ON p_frmnm.

* Validate the SAPScript form
  PERFORM  f_validate_form.

AT SELECTION-SCREEN.

  IF     ( ( p_checf     IS INITIAL ) AND ( p_chect     IS INITIAL ) ).
  ELSEIF ( ( p_checf IS NOT INITIAL ) AND ( p_chect     IS INITIAL ) ).
    CLEAR                                   p_chect.
    MOVE     p_checf                   TO   p_chect.
  ELSEIF ( ( p_checf     IS INITIAL ) AND ( p_chect IS NOT INITIAL ) ).
    CLEAR                                   p_checf.
    MOVE     p_chect                   TO   p_checf.
  ELSEIF   ( p_checf                   GT   p_chect ).
    MESSAGE  e000 WITH text-205.
  ENDIF.

  IF       ( sy-ucomm                  EQ   'RBGBCMD' ).    "D30K929884

    PERFORM  f_set_payment_method   USING   'X'.            "D30K929884

    PERFORM  f_set_check_form       USING   'X'.            "D30K929884

  ENDIF.                                                    "D30K929884

*eject
************************************************************************
*                          Start Of Selection                          *
************************************************************************
START-OF-SELECTION.

* Initial the data elements
  PERFORM  f_initial_data_elements.

* Select the data
  PERFORM  f_select_data.

************************************************************************
*                           End Of Selection                           *
************************************************************************
END-OF-SELECTION.

* Open the form
  PERFORM  f_open_form.

* Write to the form
  PERFORM  f_write_form.

* Close the form
  PERFORM  f_close_form.

* Update the extraction date
  PERFORM  f_call_trans_fchx.

* Display the output spool
  PERFORM  f_display_spool.

  INCLUDE zfapr021_in_house_check_f01.
