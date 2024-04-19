*&---------------------------------------------------------------------*
*&  Include           ZFAPI120_BANK_KEY_LGC
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Program Name       :   ZFAPI120_BANK_KEY                             *
* Author             :                                                 *
* Date               :   Mar 22, 2018                                  *
* Technical Contact  :   Chaitanya Palyam                              *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :   Bank Key Extract Interface                    *
* Notes              :                                                 *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS         Description                     *
* ---------------------------------------------------------------------*
* 01-Feb-2018  CPALYAM     D30K928630  CHG0106152-Initial development  *
*                          D30K928713, D30K928842, D30K928902          *
*----------------------------------------------------------------------*

*=======================================================================
* START-OF-SELECTION
*=======================================================================
START-OF-SELECTION.

* Create Instance
  CREATE OBJECT gref_util.

* Initial the data elements
  PERFORM f_initial_data_elements.
* Fetch data
  PERFORM f_get_data.

* Update data to outpur table
  PERFORM f_fill_data.

*=======================================================================
* END-OF-SELECTION
*=======================================================================
END-OF-SELECTION.
  IF p_pres = abap_true.
    PERFORM f_create_header_rec.
    PERFORM f_write_to_presentation.
  ELSEIF  p_appl = abap_true.
    PERFORM f_write_to_appl_server.
  ENDIF.
