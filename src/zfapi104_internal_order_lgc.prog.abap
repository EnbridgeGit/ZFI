*&---------------------------------------------------------------------*
*&  Include           ZFAPI104_INTERNAL_ORDER_LGC
*&---------------------------------------------------------------------*
************************************************************************
*                            Enbridge Energy                           *
*&---------------------------------------------------------------------*
*& Program Name       :  ZFAPI104_INTERNAL_ORDER                       *
*& Author             :  Kalinga Keshari Rout                          *
*& Creation Date      :  January 09, 2018                              *
*& Object ID          :                                                *
*& Application Area   :  FICO                                          *
*& Description        :  Program extracts master data for both cases   *
*&                       full load and delta load and file created in  *
*&                       application server  .Frequency of data upload *
*&                        weekly                                       *
*&---------------------------------------------------------------------*
*                         Modification Log                             *
*                                                                      *
* Changed On   Changed By  CTS         Description                     *
* -------------------------------------------------------------------- *
* 19-Jan-2018  KROUT       D30K928564  CHG0100808  Initial Development *
*                          D30K928834, D30K928890                      *
*----------------------------------------------------------------------*

INITIALIZATION.

  PERFORM f_get_file_path CHANGING p_path2 .

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path2.
  PERFORM f_get_f4_help_file_path.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path1.
  PERFORM f_get_f4_help_file_path1.

************************************************************************
*                        START-OF-SELECTION                            *
************************************************************************
START-OF-SELECTION.
* Initial the data elements
  PERFORM f_initial_data_elements.
  PERFORM f_get_data.          " Get data from database & prepare final output
  PERFORM f_format_data .

END-OF-SELECTION.

  IF p_dis =  gc_x."'X'.
    PERFORM f_display_data .  " To display or Place the file in Application layer.
  ELSE.
    PERFORM f_transport_data . " To place the file in Application layer.
  ENDIF.
