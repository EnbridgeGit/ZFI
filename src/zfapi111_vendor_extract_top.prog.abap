*&---------------------------------------------------------------------*
*&  Include           ZFAPI111_VENDOR_EXTRACT_TOP
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Program Name       :   ZFAPI111_VENDOR_EXTRACT                       *
* Program Include    :   ZFAPI111_VENDOR_EXTRACT_TOP                   *
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

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES:   lfa1, lfb1, sscrfields.

*=======================================================================
* GLOBAL TYPES
*=======================================================================
TYPES: BEGIN OF gty_lfa1_key,
        lifnr           TYPE lifnr,
       END   OF gty_lfa1_key.

TYPES:  gtt_lfa1_key    TYPE STANDARD TABLE OF gty_lfa1_key.

TYPES: BEGIN OF gty_lfb1_key,
        lifnr           TYPE lifnr,
        bukrs           TYPE bukrs,
       END   OF gty_lfb1_key.

TYPES:  gtt_lfb1_key    TYPE STANDARD TABLE OF gty_lfb1_key.

TYPES: BEGIN OF gty_lfa1,
        lifnr           TYPE lifnr,
        land1           TYPE land1_gp,
        name1           TYPE name1_gp,
        name2           TYPE name2_gp,
        name3           TYPE name3_gp,
        name4           TYPE name4_gp,
        ort01           TYPE ort01_gp,
        pfach           TYPE pfach,
        pstl2           TYPE pstl2,
        pstlz           TYPE pstlz,
        regio           TYPE regio,
        stras           TYPE stras_gp,
        adrnr           TYPE adrnr,
        ernam           TYPE ernam_rf,
        ktokk           TYPE ktokk,
        loevm           TYPE loevm_x,
        sperr           TYPE sperb_x,
        stcd1           TYPE stcd1,
        stcd2           TYPE stcd2,
        telf1           TYPE telf1,
        telf2           TYPE telf2,
        telfx           TYPE telfx,
        stceg           TYPE stceg,
        lfurl           TYPE url,
        updat           TYPE updat_rf,
        j_sc_currency   TYPE /sapnea/j_sc_currency,
       END   OF gty_lfa1.

TYPES:  gtt_lfa1        TYPE STANDARD TABLE OF gty_lfa1.

TYPES: BEGIN OF gty_lfb1,
        lifnr           TYPE lifnr,
        bukrs           TYPE bukrs,
        sperr           TYPE sperb_b,
        loevm           TYPE loevm_b,
        zterm           TYPE dzterm,
       END   OF gty_lfb1.

TYPES:  gtt_lfb1        TYPE STANDARD TABLE OF gty_lfb1.

TYPES: BEGIN OF gty_knvk,
        parnr           TYPE parnr,
        lifnr           TYPE lifnr,
        prsnr           TYPE ad_persnum,
       END   OF gty_knvk.

TYPES:  gtt_knvk        TYPE STANDARD TABLE OF gty_knvk.

TYPES: BEGIN OF gty_adrc,
        addrnumber      TYPE ad_addrnum,
        date_from       TYPE ad_date_fr,
        str_suppl1      TYPE ad_strspp1,
        str_suppl2      TYPE ad_strspp2,
        str_suppl3      TYPE ad_strspp3,
       END   OF gty_adrc.

TYPES:  gtt_adrc        TYPE STANDARD TABLE OF gty_adrc.

TYPES: BEGIN OF gty_adr6,
        addrnumber      TYPE ad_addrnum,
        persnumber      TYPE ad_persnum,
        date_from       TYPE ad_date_fr,
        smtp_addr       TYPE ad_smtpadr,
       END   OF gty_adr6.

TYPES:  gtt_adr6        TYPE STANDARD TABLE OF gty_adr6.

TYPES:  gty_cdhdr       TYPE cdhdr.

TYPES:  gtt_cdhdr       TYPE STANDARD TABLE OF gty_cdhdr.

TYPES: BEGIN OF gty_objectid,
        objectid        TYPE cdobjectv,
       END   OF gty_objectid.

TYPES:  gtt_objectid    TYPE STANDARD TABLE OF gty_objectid.

TYPES: BEGIN OF gty_chngs,
        lifnr           TYPE lifnr,
        bukrs           TYPE bukrs,
        udate           TYPE cddatum,
        changenr        TYPE cdchangenr,
        username        TYPE cdusername,
       END   OF gty_chngs.

TYPES: gtt_chngs        TYPE STANDARD TABLE OF gty_chngs.

TYPES: BEGIN OF gty_update_last,
        lifnr           TYPE lifnr,
        udate           TYPE cddatum,
        username        TYPE cdusername,
       END   OF gty_update_last.

TYPES: gtt_update_last  TYPE STANDARD TABLE OF gty_update_last.

TYPES: BEGIN OF gty_output,
        vulid           TYPE text20,
        lifnr           TYPE lifnr,
        name1           TYPE name1_gp,
        zterm           TYPE dzterm,
        bukrs           TYPE bukrs,
        ktokk           TYPE ktokk,
        status          TYPE char1,
        stras           TYPE stras_gp,
        ort01           TYPE ort01_gp,
        regio           TYPE regio,
        pstlz           TYPE pstlz,
        land1           TYPE land1_gp,
        telf1           TYPE telf1,
        name2           TYPE name2_gp,
        str_suppl1      TYPE ad_strspp1,
        str_suppl2      TYPE ad_strspp2,
        str_suppl3      TYPE ad_strspp3,
        stcd1           TYPE stcd1,
        updat1          TYPE text10,
        updat2          TYPE text10,
        prefvend        TYPE char1,
        erpid           TYPE char5,
        waers           TYPE waers,
        ernam           TYPE ernam_rf,
        telf2           TYPE telf2,
        telfx           TYPE telfx,
        lfurl           TYPE ad_smtpadr,
        comments        TYPE text100,
       END   OF gty_output.

TYPES: gtt_output       TYPE STANDARD TABLE OF gty_output.

*=======================================================================
* GLOBAL CONSTANTS
*=======================================================================
CONSTANTS:
      gc_x              TYPE c                VALUE 'X',
      gc_fslash         TYPE char1            VALUE '/',
      gc_ucomm_onli     TYPE sscrfields-ucomm VALUE 'ONLI'.

*=======================================================================
* GLOBAL VARIABLES
*=======================================================================
DATA: gv_pathsplit      TYPE string,
      gv_path           TYPE string,
      gv_filename1      TYPE string,
      gv_filename2      TYPE text255,
      gv_fname1         TYPE string,
      gv_flag           TYPE c,
      gv_updat          TYPE sydatum,
      gv_append         TYPE xflag,
      gv_rec_count      TYPE i.

*=======================================================================
* GLOBAL WORKAREAS
*=======================================================================
DATA: gs_data_tab       TYPE string,
      gs_output         TYPE gty_output,
      gs_lfa1           TYPE gty_lfa1,
      gs_lfb1           TYPE gty_lfb1,
      gs_knvk           TYPE gty_knvk,
      gs_adrc           TYPE gty_adrc,
      gs_adr6           TYPE gty_adr6,
      gs_update_last    TYPE gty_update_last.

*=======================================================================
* GLOBAL INTERNAL TABLES
*=======================================================================
DATA: gt_pathsplit      TYPE STANDARD TABLE OF string,
      gt_data_tab       TYPE STANDARD TABLE OF string,
      gt_output         TYPE gtt_output,
      gt_lfa1           TYPE gtt_lfa1,
      gt_lfb1           TYPE gtt_lfb1,
      gt_knvk           TYPE gtt_knvk,
      gt_adrc           TYPE gtt_adrc,
      gt_adr6           TYPE gtt_adr6,
      gt_update_last    TYPE gtt_update_last.

******************************************************************************************************
* REFERRENCE VARIABLES
******************************************************************************************************
* Reference for Interface monitor
DATA: gref_util         TYPE REF TO zcl_iap_interface_util.
