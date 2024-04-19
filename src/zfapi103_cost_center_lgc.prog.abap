*******************************************************************************
*                            Spectra Energy                                   *
*&---------------------------------------------------------------------       *
*& Program Name       :  ZFAPI103_COST_CENTER                                 *
*& Author             :  Kalinga Keshari Rout                                 *
*& Creation Date      :  January 09, 2018                                     *
*& Object ID          :                                                       *
*& Application Area   :  FICO                                                 *
*& Description        :  Program extracts master data for both cases          *
*&                       full load and delta load and file created in         *
*&                       application server  .Frequency od daat upload        *
*&                        weekly                                              *
*&--------------------------------------------------------------------------- *
*                      Modification Log                                       *
* Changed On   Changed By  CTS        Description                             *
* ----------------------------------------------------------------------------*
* 19-Jan-2018  KROUT       D30K928561 CHG0100807 # Initial development        *
*                          D30K928677                                         *
*-----------------------------------------------------------------------------*

INITIALIZATION.

  PERFORM  f_get_file_path CHANGING p_path2.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path2.

  PERFORM  f_get_f4_help_file_path.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path1.

  PERFORM  f_get_f4_help_file_path1.

************************************************************************
*                        START-OF-SELECTION                            *
************************************************************************
START-OF-SELECTION.

  PERFORM  f_get_data.

  PERFORM  f_format_data.

************************************************************************
*                         END-OF-SELECTION
************************************************************************
END-OF-SELECTION.

  IF       ( p_dis IS NOT INITIAL ).
    PERFORM  f_display_data.
  ELSE.
    PERFORM  f_transport_data.
  ENDIF.
