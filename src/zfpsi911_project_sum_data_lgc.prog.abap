*&---------------------------------------------------------------------*
*&  Include           ZFPSI911_PROJECT_SUM_DATA_LGC
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Program Name       :  ZFPSI911_PROJECT_SUM_DATA_LGC                 *
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


INITIALIZATION.

  PERFORM f_fill_file_path CHANGING p_fpath1.

AT SELECTION-SCREEN.
  PERFORM f_validate.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fpath1.
  PERFORM f_get_f4_help_file_path.

START-OF-SELECTION.
  PERFORM f_get_data_from_sel.                "Get distinct project number from PRPR table

END-OF-SELECTION.
  PERFORM f_get_data.                         "Get approriate data from all tables, format and transport data
