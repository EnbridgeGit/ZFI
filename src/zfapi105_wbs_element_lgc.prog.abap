*&---------------------------------------------------------------------*
*&  Include           ZFAPI105_WBS_ELEMENT_LGC
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Program Name       :   ZFAPI105_WBS_ELEMENT                          *
* Author             :                                                 *
* Date               :   Jan 10, 2018                                  *
* Technical Contact  :   Chaitanya Palyam                              *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :   For WBS data through interface for IAP        *
* Notes              :                                                 *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS         Description                     *
* ---------------------------------------------------------------------*
* 10-Jan-2018  CPALYAM     D30K928578  CHG0100809-Initial Development  *                                                                     *
*                          D30K928836, D30K928894                      *
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
