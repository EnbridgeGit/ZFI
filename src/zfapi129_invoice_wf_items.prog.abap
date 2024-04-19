*&---------------------------------------------------------------------*
*& Report  ZFAPI129_INVOICE_WF_ITEMS
*&---------------------------------------------------------------------*
************************************************************************
*                               Enbridge                               *
************************************************************************
* Program Name       :  ZFAPI129_INVOICE_WF_ITEMS                      *
* Author             :  Vijay Rajaputra                                *
* Creation Date      :  05-Nov-2018                                    *
* Application Area   :  FICO                                           *
* Technical Contact  :  Vijay Rajaputra                                *
*                                                                      *
* Purpose            :  Invoice Workflow Items Extract Interface - IAP *
*                                                                      *
*----------------------------------------------------------------------*
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* 05-Nov-2018  VRAJAPUTRA  D30K929291  CHG0130803  Initial development *
*                          D30K929374                                  *
* 22-Jan-2019  VRAJAPUTRA  D30K929505  CHG0135339  Duplicate checks    *
*                          D30K929531                                  *
* 31-Jan-2019  VRAJAPUTRA  D30K929554  CHG0136094  Duplicate checks    *
* 13-Feb-2019  VRAJAPUTRA  D30K929576  CHG0137162  Additions to Extract*
*&---------------------------------------------------------------------*
REPORT  zfapi129_invoice_wf_items  MESSAGE-ID zfi01
                                   NO STANDARD PAGE HEADING.

INCLUDE zfapi129_invoice_wf_items_top.

INCLUDE zfapi129_invoice_wf_items_scr.

INCLUDE zfapi129_invoice_wf_items_lgc.

INCLUDE zfapi129_invoice_wf_items_f01.

INCLUDE zfapi129_invoice_wf_items_f02.
