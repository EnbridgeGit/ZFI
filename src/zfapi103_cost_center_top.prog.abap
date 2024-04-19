*******************************************************************************
*                            Spectra Energy                                   *
*&---------------------------------------------------------------------       *
*& Program Name       :  ZFAPI103_COST_CENTER                                 *
*& Author             :  Kalinga Keshari Rout                                 *
*& Creation Date      :  January 09, 2018                                     *
*& Object ID          :                                                       *
*& Application Area   :  FICO                                                 *
*& Description        :  Program extracts master data for both cases          *
*&                       full load and delta load and file created in         *
*&                       application server  .Frequency od daat upload        *
*&                        weekly                                              *
*&--------------------------------------------------------------------------- *
*                      Modification Log                                       *
* Changed On   Changed By  CTS        Description                             *
* ----------------------------------------------------------------------------*
* 19-Jan-2018  KROUT       D30K928561 CHG0100807 # Initial development        *
*                          D30K928677                                         *
*-----------------------------------------------------------------------------*

TABLES: csks, sscrfields.

TYPES: BEGIN OF ty_csks_key,
        kokrs            TYPE kokrs,
        kostl            TYPE kostl,
       END OF ty_csks_key.

TYPES: BEGIN OF ty_csks,
        kokrs            TYPE kokrs,
        kostl            TYPE kostl,
        datbi            TYPE datbi,
        datab            TYPE datab,
        bkzkp            TYPE bkzkp,
        bukrs            TYPE bukrs,
       END OF ty_csks.

TYPES: BEGIN OF ty_cskt,
        spras            TYPE spras,
        kokrs            TYPE kokrs,
        kostl            TYPE kostl,
        datbi            TYPE datbi,
        ktext            TYPE ktext,
       END   OF ty_cskt.

TYPES: BEGIN OF ty_final,
        erpid            TYPE char5,
        kokrs            TYPE kokrs,
        kostl            TYPE kostl,
        bukrs            TYPE bukrs,
        ktext            TYPE ktext,
        bkzkp            TYPE bkzkp,
      END OF ty_final.

DATA:   gt_csks_key      TYPE STANDARD TABLE OF ty_csks_key,
        gs_csks_key      TYPE ty_csks_key,
        gt_csks          TYPE STANDARD TABLE OF ty_csks,
        gs_csks          TYPE ty_csks,
        gt_cskt          TYPE STANDARD TABLE OF ty_cskt,
        gs_cskt          TYPE ty_cskt,
        gt_final         TYPE STANDARD TABLE OF  ty_final,
        gs_final         TYPE ty_final,
        gt_editpos       TYPE STANDARD TABLE OF cdred,
        gs_editpos       TYPE cdred,
        gv_chk           TYPE c.

CONSTANTS:
        gc_ucomm_onli    TYPE sscrfields-ucomm  VALUE 'ONLI'.
