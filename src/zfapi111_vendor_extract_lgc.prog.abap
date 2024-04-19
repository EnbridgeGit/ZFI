*&---------------------------------------------------------------------*
*&  Include           ZFAPI111_VENDOR_EXTRACT_LGC
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Program Name       :   ZFAPI111_VENDOR_EXTRACT                       *
* Program Include    :   ZFAPI111_VENDOR_EXTRACT_LGC                   *
* Author             :                                                 *
* Date               :   Feb 01, 2018                                  *
* Technical Contact  :   Chaitanya Palyam                              *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :   ATCAT Vendor Master Interface                 *
* Notes              :                                                 *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS         Description                     *
* ---------------------------------------------------------------------*
* 01-Feb-2018  CPALYAM     D30K928649  CHG0100816-Initial development  *
*                          D30K928653, D30K928683, D30K928691          *
*                          D30K928693, D30K928882, D30K928892          *
*----------------------------------------------------------------------*

*=======================================================================
* START-OF-SELECTION
*=======================================================================
START-OF-SELECTION.

* Create Instance
  CREATE OBJECT gref_util.

* Main process
  PERFORM  f_process_main.

*=======================================================================
* END-OF-SELECTION
*=======================================================================
END-OF-SELECTION.

  PERFORM  f_write_record_count.
