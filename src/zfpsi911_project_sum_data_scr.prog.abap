*&---------------------------------------------------------------------*
*&  Include           ZFPSI911_PROJECT_SUM_DATA_SCR
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Program Name       :  ZFPSI911_PROJECT_SUM_DATA_SCR                 *
*& Author             :  Kavya M B                                     *
*& Creation Date      :  13/09/2019                                    *
*& Object ID          :  I_P2C_PS_911                                  *
*& Application Area   :  FICO                                          *
*& Description        :  This interface is to send Project System      *
*                        master data and transactions data in a flat   *
*                        file to a dedicated location on Windows that  *
*                        to be picked up by Workato and delivered to   *
*                        destination system with SQL database.         *
*&-------------------------------------------------------------------- *
************************************************************************
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* DD-MMM-YYYY  User ID     TR#         Change Description              *
* 18-10-2019   KMB         D30K930178  CHG0159956 - PS PMO reporting   *
* 22-10-2019   KMB         D30K930222  CHG0159956 - FUT issue          *
* 20-11-2019   KMB         D30K930293  CHG0159956 - FUT issue          *
* 29-11-2019   KMB         D30K930315  CHG0159956 - File size fiel     *
************************************************************************

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-000.
PARAMETERS : p_erpid TYPE char05.   "ERP ID
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-001.
SELECT-OPTIONS : s_bukrs  FOR prps-pbukr,      "Company Code
                 s_ptype  FOR prps-prart,      "Proj type
                 s_wbs    FOR prps-posid.      "Proj number
PARAMETERS :     p_gjahr  TYPE rpsco-gjahr,    "Fiscal Year
                 p_monat  TYPE rpsco-wrttp.    "Period
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-002.
PARAMETERS : p_all AS CHECKBOX, "All
             p_for AS CHECKBOX. "Forecast
PARAMETERS : p_record TYPE n LENGTH 10. "Added by KMB on 15-11-2019 CHG0159956 - FUT issue fixing
PARAMETERS:  p_fbrk  TYPE numc4 DEFAULT 70. "Added by KMB on 29-11-2019 CHG0159956 - File size field
SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-003.
PARAMETERS : p_app   RADIOBUTTON GROUP rad2 DEFAULT 'X' USER-COMMAND cmnd, "Application Server
             p_pre   RADIOBUTTON GROUP rad2.                               "Presentation Server
SELECTION-SCREEN END OF BLOCK b4.

SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE text-004.
PARAMETERS: p_fpath1 TYPE localfile,        "File Path
            p_fname1 TYPE localfile,        "Filename-All Data
            p_fname2 TYPE localfile.        "Filename-Forecast Data
SELECTION-SCREEN END OF BLOCK b5.
