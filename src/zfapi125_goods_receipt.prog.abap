*&---------------------------------------------------------------------*
*& Report  ZFAPI125_GOODS_RECEIPT
*&---------------------------------------------------------------------*
************************************************************************
*                            Enbridge Energy                           *
*&---------------------------------------------------------------------*
*& Program Name       :  ZFAPI125_GOODS_RECEIPT                        *
*& Author             :  Kalinga Keshari Rout                          *
*& Creation Date      :  March 26, 2018                                *
*& Object ID          :                                                *
*& Application Area   :  FICO                                          *
*& Description        :  Program extracts goods receipt history data   *
*                        for both cases full load and delta load and   *
*&                       file created application server.              "
*&---------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* 26-Mar-2018  KROUT       D30K928872  CHG0106485  Initial Development *
*                          D30K928904, D30K928931, D30K928955,         *
*                          D30K928981, D30K929077                      *
*----------------------------------------------------------------------*
REPORT  zfapi125_goods_receipt  MESSAGE-ID zfi01
                                NO STANDARD PAGE HEADING.

INCLUDE zfapi125_goods_receipt_top.

INCLUDE zfapi125_goods_receipt_scr.

INCLUDE zfapi125_goods_receipt_lgc.

INCLUDE zfapi125_goods_receipt_f01.
