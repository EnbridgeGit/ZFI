*&---------------------------------------------------------------------*
*& Report  ZFAPI118_POST_IAP_DOCUMENT
*&---------------------------------------------------------------------*
REPORT  zfapi118_post_iap_document  MESSAGE-ID zfi01
                                    LINE-COUNT 65
                                    LINE-SIZE 121
                                    NO STANDARD PAGE HEADING.

************************************************************************
*                            Spectra Energy                            *
************************************************************************
*  Program:          ZFAPI118_POST_IAP_DOCUMENT                        *
*  Author:           Vijay Rajaputra ( Copy of US System )             *
*  Date:             Aug, 01 2018                                      *
*  Application Area: FICO AP                                           *
*                                                                      *
*  Description:      Post IAP Documents for PO and Non-PO Invoices     *
*                    and Credit Memos                                  *
*                                                                      *
************************************************************************
*----------------------- CHANGE LOG -----------------------------------*
*  Date    By        Description                                       *
* -------- --------- ------------------------------------------------- *
* 08/01/18 VRAJAPUTRA D30K928896 - CHG0109670 - Initial program        *
*                                                                      *
*----------------------------------------------------------------------*
************************************************************************

INCLUDE zfapi118_post_iap_document_top.

INCLUDE zfapi118_post_iap_document_scr.

INCLUDE zfapi118_post_iap_document_f01.

INCLUDE zfapi118_post_iap_document_lgc.
