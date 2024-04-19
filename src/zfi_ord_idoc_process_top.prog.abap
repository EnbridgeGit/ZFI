*&---------------------------------------------------------------------*
*&  Include           ZFI_ORD_IDOC_PROCESS_TOP
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Program Name       :   ZFI_ORD_IDOC_PROCESS                          *
* Include            :   ZFI_ORD_IDOC_PROCESS_TOP                      *
* Author             :   AKMADASU                                      *
* Date               :   Jan 23, 2022                                  *
* Technical Contact  :   Ashok Madasu                                  *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :   To Process Order IDOCS                        *
* Notes              :                                                 *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS         Description                     *
* ---------------------------------------------------------------------*
* 23-Jan-2022  AKMADASU    D30K931986-Initial development              *
*----------------------------------------------------------------------*
* table declarations needed for the selection screens
TABLES: edidc, tbd55, serial, bdfields.
*--Getting the delivery numbers form EDIDS table
TYPES:BEGIN OF ty_edids,
        DOCNUM  TYPE EDI_DOCNUM,
        LOGDAT  TYPE EDI_LOGDAT,
        LOGTIM  TYPE EDI_LOGTIM,
        COUNTR  TYPE EDI_COUNTR,
        STATUS  TYPE EDI_STATUS,
*        STATXT  TYPE EDI_STATXT,
        STAPA2  TYPE EDI_STAPA2,
        STAPA3  TYPE EDI_STAPA3,
      END OF ty_edids.
TYPES: BEGIN OF ty_output,
           docnum TYPE edi_docnum,
           mestyp TYPE edi_mestyp,
           status TYPE edi_status,
           statusicon(60),
           statxt TYPE edi_statxt,
         END OF ty_output.
DATA:gt_edids    TYPE TABLE OF ty_edids,
     gt_edidser  TYPE TABLE OF ty_edids,
     gs_edids    TYPE ty_edids,
     lr_vbeln    TYPE TABLE OF ISAUTO_XLO_RANGE_S_VBELN_VL,
     gs_vbeln    TYPE ISAUTO_XLO_RANGE_S_VBELN_VL.

  DATA:lt_output TYPE TABLE OF ty_output,
       ls_output TYPE ty_output.
