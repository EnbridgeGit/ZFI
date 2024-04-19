*&---------------------------------------------------------------------*
*& Report  ZFAPI008_IOSETNW                                            *
*&                                                                     *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Program Name       :  ZFAPI008_IOSETNW                              *
*& Include Name       :  ZFAPI008_IOSETNW_TOP                          *
*& Author             :  Tawfeeq Ahamd                                 *
*& Date               :  22-May-2020                                   *
*& Change Request     :  CHG0180384                                    *
*& Purpose            :  Extract Internal Order and Network Settlement *
*&                       data                                          *
*&---------------------------------------------------------------------*
*&                      Modification Log                               *
*&                                                                     *
*& Changed On   Changed By    CTS        Description                   *
*& --------------------------------------------------------------------*
*& 22-May-2020  AHMADT        D30K930537 CHG0180384 Initial Development*
*&                            D30K930582                               *
*& 01-Jul-2020  AHMADT        D30K930610 CHG0185539 Added 2 Reciever   *
*&                            types and alphanumeric field adjustments *
*&---------------------------------------------------------------------*


TABLES:  aufk, prps.

*&---------------------------------------------------------------------*
*& Global Types                                                        *
*&                                                                     *
*&---------------------------------------------------------------------*
TYPES: BEGIN OF gty_order,
       sender_type      TYPE c LENGTH 15,
       receiver_type    TYPE c LENGTH 15,
       sender           TYPE c LENGTH 15,
       receiver         TYPE c LENGTH 19,
       sender_text      TYPE aufk-ktext ,
       receiver_text    TYPE c LENGTH 40,
       percent          TYPE c LENGTH 10,
       settlement_type  TYPE c LENGTH 20,
       activity         TYPE c LENGTH 8,
       END OF gty_order.

TYPES: BEGIN OF gty_prps,
       pspnr     TYPE prps-pspnr,
       poski     TYPE prps-poski,
       psphi     TYPE prps-psphi,
       post1     TYPE prps-post1,
       erdat     TYPE prps-erdat,
       aedat     TYPE prps-aedat,
       END OF gty_prps.

TYPES : BEGIN OF gty_proj,
        pspnr     TYPE char20,
        poski     TYPE prps-poski,
        psphi     TYPE char20,
        post1     TYPE prps-post1,
        END OF gty_proj.

TYPES : BEGIN OF gty_cobrb,
        objnr       TYPE cobrb-objnr,
        perbz       TYPE cobrb-perbz,
        prozs       TYPE cobrb-prozs,
        letja       TYPE cobrb-letja,
        avorg       TYPE cobrb-avorg,
        konty       TYPE cobrb-konty,
        bukrs       TYPE cobrb-bukrs,
        hkont       TYPE cobrb-hkont,
        kostl       TYPE cobrb-kostl,
        aufnr       TYPE cobrb-aufnr,      "Added by AHMADT for CHG0185539 on 19/06/2020
        ps_psp_pnr  TYPE cobrb-ps_psp_pnr,
        anln1       TYPE cobrb-anln1,      "Added by AHMADT for CHG0185539 on 19/06/2020
        END OF gty_cobrb.

TYPES: BEGIN OF gty_cskt,
       spras        TYPE cskt-spras,
       kostl        TYPE cskt-kostl,
       ktext        TYPE cskt-ktext,
       END OF gty_cskt.

TYPES: BEGIN OF gty_prps1,
       pspnr        TYPE prps-pspnr,
       post1        TYPE prps-post1,
       END OF gty_prps1.


TYPES: BEGIN OF gty_skat,
       spras        TYPE skat-spras,
       saknr        TYPE skat-saknr,
       txt20        TYPE skat-txt20,
       END OF gty_skat.

TYPES: BEGIN OF gty_aufk,
       aufnr         TYPE aufk-aufnr,
       objnr         TYPE aufk-objnr,
       ktext         TYPE aufk-ktext,
       erdat         TYPE aufk-erdat,
       aedat         TYPE aufk-aedat,
       autyp         TYPE aufk-autyp,
       END OF gty_aufk.

TYPES : BEGIN OF gty_zfit,
        paramtype     TYPE zfit_xparam-paramtype,
        subtype       TYPE zfit_xparam-subtype,
        key1          TYPE zfit_xparam-key1,
        value1        TYPE zfit_xparam-value1,
        value2        TYPE zfit_xparam-value2,
        END OF gty_zfit.
* Start of changes by AHMADT for CHG0185539
TYPES: BEGIN OF gty_anla,
       anln1         TYPE anla-anln1,
       anln2         TYPE anla-anln2,
       txt50         TYPE anla-txt50,
       END OF gty_anla.
* End of changes by AHMADT for CHG0185539


*&---------------------------------------------------------------------*
*& Internal Tables                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*
DATA: gt_aufk         TYPE STANDARD TABLE OF gty_aufk,
      gt_cobrb        TYPE STANDARD TABLE OF gty_cobrb,
      gt_cobrb_k      TYPE STANDARD TABLE OF gty_cobrb,
      gt_cobrb_h      TYPE STANDARD TABLE OF gty_cobrb,
      gt_cobrb_p      TYPE STANDARD TABLE OF gty_cobrb,
* Start of changes by AHMADT for CHG0185539
      gt_cobrb_a      TYPE STANDARD TABLE OF gty_cobrb,
      gt_anla         TYPE STANDARD TABLE OF gty_anla,
      gt_aufk_a       TYPE STANDARD TABLE OF gty_aufk,
      gt_cobrb_1      TYPE STANDARD TABLE OF gty_cobrb,
* End of changes by AHMADT for CHG0185539
      gt_cskt         TYPE STANDARD TABLE OF gty_cskt,
      gt_skat         TYPE STANDARD TABLE OF gty_skat,
      gt_prps1        TYPE STANDARD TABLE OF gty_prps1,
      gt_prps         TYPE STANDARD TABLE OF gty_prps,
      gt_proj         TYPE STANDARD TABLE OF gty_proj,
      gt_zfit         TYPE STANDARD TABLE OF gty_zfit,
      gt_order        TYPE STANDARD TABLE OF gty_order.

*&---------------------------------------------------------------------*
*& Work Areas                                                          *
*&                                                                     *
*&---------------------------------------------------------------------*
DATA: gs_zfit         TYPE gty_zfit,
      gs_proj         TYPE gty_proj,
      gs_prps         TYPE gty_prps,
      gs_order        TYPE gty_order,
      gs_prps1        TYPE gty_prps1,
      gs_skat         TYPE gty_skat,
      gs_aufk         TYPE gty_aufk,
      gs_cobrb        TYPE gty_cobrb,
      gs_cskt         TYPE gty_cskt,
      gs_anla         TYPE gty_anla, "Added by AHMADT for CHG0185539
      gs_aufk_a       TYPE gty_aufk. "Added by AHMADT for CHG0185539

*&---------------------------------------------------------------------*
*& Global Variables                                                    *
*&                                                                     *
*&---------------------------------------------------------------------*
DATA: gv_objttl       TYPE char50,
      gv_obj_id       TYPE char18,
      gv_path         TYPE string,
      gv_data         TYPE string,
      gv_percent      TYPE string,
      gv_pnr          TYPE char40.

*&---------------------------------------------------------------------*
*& Constants                                                           *
*&                                                                     *
*&---------------------------------------------------------------------*
CONSTANTS: gc_paramtype TYPE char10 VALUE 'OUTBOUND',
           gc_subtype   TYPE char10 VALUE 'OBJECTID',
           gc_obj_id    TYPE char10 VALUE 'P_OBJID',
           gc_obj_ttl   TYPE char10 VALUE 'P_OBJTTL',
           gc_rep       TYPE char20 VALUE 'ZFAPI008_IOSETNW',
           gc_file_path TYPE char15 VALUE 'FILE_PATH',
           gc_csv       TYPE char4  VALUE '.CSV',
           gc_under     TYPE char1  VALUE '_',
           gc_comma     TYPE char1  VALUE ',',
           gc_dash      TYPE char1  VALUE '-', "Added by AHMADT for CHG0185539
           gc_zero      TYPE char1  VALUE '0'. "Added by AHMADT for CHG0185539
