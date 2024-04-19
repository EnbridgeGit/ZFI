*&---------------------------------------------------------------------*
*& Report  ZFPSI911_PROJECT_SUMMARY_DATA
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Program Name       :  ZFPSI911_PROJECT_SUMMARY_DATA                 *
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


REPORT  zfpsi911_project_summary_data.

******data declaration ******
INCLUDE zfpsi911_project_sum_data_top.

******selection screen
INCLUDE zfpsi911_project_sum_data_scr.

********program logic****
INCLUDE zfpsi911_project_sum_data_lgc.

******* form routines******
INCLUDE zfpsi911_project_sum_data_f01.
