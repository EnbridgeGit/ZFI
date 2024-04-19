*&---------------------------------------------------------------------*
*& Report  ZFAPI103_COST_CENTER
*&---------------------------------------------------------------------*
REPORT  ZFAPI103_COST_CENTER.

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
*&                       application server  .Frequency of data upload        *
*&                       weekly                                               *
*&--------------------------------------------------------------------------- *
*                      Modification Log                                       *
* Changed On   Changed By  CTS        Description                             *
* ----------------------------------------------------------------------------*
* 09-Jan-2018  KROUT       D30K928561 CHG0100807 # Initial development        *
*                          D30K928677, D30K928832                             *
*-----------------------------------------------------------------------------*

INCLUDE zfapi103_cost_center_top.

INCLUDE zfapi103_cost_center_scr.

INCLUDE zfapi103_cost_center_lgc.

INCLUDE zfapi103_cost_center_f01.
