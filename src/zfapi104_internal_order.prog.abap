*&---------------------------------------------------------------------*
*& Report  ZFAPI104_INTERNAL_ORDER
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
*                          D30K928834, D30K928890, D30K928945          *
*                          D30K929023                                  *
*----------------------------------------------------------------------*
REPORT  ZFAPI104_INTERNAL_ORDER.

******data declaration ******
INCLUDE zfapi104_internal_order_top .

******selection screen
INCLUDE zfapi104_internal_order_scr .

********program logic****
INCLUDE zfapi104_internal_order_lgc.

******* form routines******
INCLUDE zfapi104_internal_order_f01 .
