*&---------------------------------------------------------------------*
*& Report  ZDELETE_ERS_DATA
*&
*&---------------------------------------------------------------------*
*& Correction report for table EKRS and EKRSDC, see note 1901279
*& The program selects/deletes entries from table EKRS and EKRSDC of
*& deleted purchase order items.
*&
*& ROOT CAUSE:
*& When the evaluated receipt settlement is active for the PO item
*& (EKPO-XERSY = "X") each goods movement creates an entry in table
*& EKRS (and EKRSDC, for planned delivery costs additional customizing
*& is necessary). The entries in EKRS/EKRSDC are deleted when the
*& corresponding goods movement is settled via the evaluated receipt
*& settlement (ERS) run (transaction MRRL/MRDC)
*&
*& It is possible to delete PO items when the GR quantity is equal to
*& the IR quantity. MM-PUR does not check whether entries in table EKRS
*& and/or EKRSDC exist.
*&
*& EXAMPLE:
*& A goods receipt is posted and cancelled. The GR was neither settled
*& by the ERS run (MRRL/MRDC) nor was an invoice document created
*& manually (MIRO). So the GR quantity and the IR quantity are zero.
*& There are two entries in table EKRS (and EKRSDC). The ERS run
*& would create a so called ERS Zero Document (RBKP-IVTYP = "2").
*& The PO item can be deleted. Because of the entries in table
*& EKRS/EKRSDC the ERS run will try to settle the goods movement but it
*& fails because the PO item is deleted.
*& MRRL: Error message ME706
*& MRDC: Error message M8607
*&---------------------------------------------------------------------*
*&
*& SELECT-OPTIONS:
*&   so_bukrs     company code
*&   so_ekorg     purchase organization
*&   so_ebeln     purchase order
*&   so_ebelp     purchase order item
*&
*& PARAMETERS:
*&   p_xersy      ERS settlement
*&   p_pack       PACKAGE SIZE
*&   p_update     update indicator
*&   p_user       user name
*&---------------------------------------------------------------------*
* Change history: ZDELETE_ERS_DATA
* 20.08.13: Version 1, Created by : PhGr
* 29.10.13: Version 2, Changed by : PhGr, enable background processing
* 13.08.14: Version 3, Changed by : PhGr, make so_ebeln OBLIGATORY
*&---------------------------------------------------------------------*

REPORT  zdelete_ers_data.

*--- Data declaration -------------------------------------------------*
TYPE-POOLS abap.
* INCLUDE mrm_const_mrm

TYPES: BEGIN OF ty_ekko,
         ebeln LIKE ekko-ebeln,
         bukrs LIKE ekko-bukrs,
         bstyp LIKE ekko-bstyp,
         bsart LIKE ekko-bsart,
         loekz LIKE ekko-loekz,
         lifnr LIKE ekko-lifnr,
         ekorg LIKE ekko-ekorg,
         lifre LIKE ekko-lifre,
       END OF ty_ekko,

       BEGIN OF ty_ekpo,
         ebeln LIKE ekpo-ebeln,
         ebelp LIKE ekpo-ebelp,
         loekz LIKE ekpo-loekz,
         elikz LIKE ekpo-elikz,
         erekz LIKE ekpo-erekz,
         pstyp LIKE ekpo-pstyp,
         knttp LIKE ekpo-knttp,
         kzvbr LIKE ekpo-kzvbr,
         vrtkz LIKE ekpo-vrtkz,
         twrkz LIKE ekpo-twrkz,
         wepos LIKE ekpo-wepos,
         weunb LIKE ekpo-weunb,
         repos LIKE ekpo-repos,
         webre LIKE ekpo-webre,
         lebre LIKE ekpo-lebre,
         xersy LIKE ekpo-xersy,
       END OF ty_ekpo.

DATA: s_ekko    TYPE ty_ekko,
      s_ekpo    TYPE ty_ekpo,
      s_ekrs    LIKE ekrs,
      s_ekrsdc  LIKE ekrsdc.

DATA: count     TYPE i.

DATA: t_ekko TYPE SORTED TABLE OF ty_ekko WITH UNIQUE KEY ebeln,
      t_ekpo TYPE SORTED TABLE OF ty_ekpo WITH UNIQUE KEY ebeln ebelp,
      t_ekrs TYPE TABLE OF ekrs,
      t_ekrsdc TYPE TABLE OF ekrsdc.

* DATA: tab_fieldcat TYPE TABLE OF slis_fieldcat_alv.
* DATA: s_fieldcat TYPE slis_fieldcat_alv.

*--- Message text variables
DATA: mess_text_001_ekrs TYPE string VALUE
        ' entries deleted from table EKRS',
      mess_text_001_ekrsdc TYPE string VALUE
        ' entries deleted from table EKRSDC',
      mess_text_002_ekrs TYPE string VALUE
        ' entries selected from table EKRS',
      mess_text_002_ekrsdc TYPE string VALUE
        ' entries selected from table EKRSDC',
      mess_text_003      TYPE string VALUE
        'UPDATE can be done by SAP development support only!',
      mess_text_004      TYPE string VALUE
        'No data selected'.


*&---------------------------------------------------------------------*
*&       Class LCL_ZDELETE_ERS_DATA
*&---------------------------------------------------------------------*
CLASS lcl_zdelete_ers_data DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      screen_description,
      authorization_check,
      database_select,
     data_initialization,
     result_display,
     ekrs_delete,
     ekrsdc_delete.

ENDCLASS.               "LCL_ZDELETE_ERS_DATA


*--- Selection screens -------------------------------------------------
* MODIF ID: Screen Groups
* SC0: Hide user input for p_user
* SC1: Screen elements for user ONLYSAP only
* SC3: Hide screen elements
*--- Block 1: Selection Criteria
SELECTION-SCREEN BEGIN OF BLOCK block_1 WITH FRAME TITLE titel_1.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(26) txtbukrs MODIF ID sc3.
SELECT-OPTIONS: so_bukrs FOR s_ekko-bukrs MODIF ID sc3.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(26) txtekorg MODIF ID sc3.
SELECT-OPTIONS: so_ekorg FOR s_ekko-ekorg MODIF ID sc3.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(26) txtebeln.
SELECT-OPTIONS: so_ebeln FOR s_ekpo-ebeln OBLIGATORY.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(26) txtebelp MODIF ID sc3.
SELECT-OPTIONS: so_ebelp FOR s_ekpo-ebelp MODIF ID sc3.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.           "ERS settlement
SELECTION-SCREEN COMMENT 1(29) txters MODIF ID sc3.
PARAMETERS: p_xersy   TYPE c AS CHECKBOX DEFAULT 'X' MODIF ID sc3.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK block_1.

*--- Block 2: Processing options
SELECTION-SCREEN BEGIN OF BLOCK block_2 WITH FRAME TITLE titel_2.
*package-wise processing to prevent error TSV_TNEW_PAGE_ALLOC_FAILED
SELECTION-SCREEN BEGIN OF LINE.           "Package size processing
SELECTION-SCREEN COMMENT 1(26) txtpck MODIF ID sc3.
PARAMETERS: p_pack   TYPE i DEFAULT '100000' MODIF ID sc3.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK block_2.

*--- Block 3: UPDATE options
SELECTION-SCREEN BEGIN OF BLOCK block_3 WITH FRAME TITLE titel_3.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(26) txtupdt MODIF ID sc1.
PARAMETERS: p_update   TYPE abap_bool AS CHECKBOX DEFAULT abap_false MODIF ID sc1.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(26) txtuser.
PARAMETERS: p_user     TYPE rbkp-usnam DEFAULT sy-uname MODIF ID sc0.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK block_3.


*-----------------------------------------------------------------------
INITIALIZATION.
*-----------------------------------------------------------------------
*--- Set descriptions
  lcl_zdelete_ers_data=>screen_description( ).

*--- Handling selection screen events
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-group1 = 'SC0'.
      screen-invisible = '1'.
    ELSEIF screen-group1 = 'SC1' AND ( p_user NE 'ONLYSAP' ).
      CLEAR: p_update.
      screen-active = '0'.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

AT SELECTION-SCREEN ON p_user.
*--- Authorization check
  IF sy-batch = abap_false.
    lcl_zdelete_ers_data=>authorization_check( ).
  ENDIF.

*-----------------------------------------------------------------------
* Main program
*-----------------------------------------------------------------------
START-OF-SELECTION.

*--- Initialization
  lcl_zdelete_ers_data=>data_initialization( ).

*--- Built up internal tables
  lcl_zdelete_ers_data=>database_select( ).

*--- Update and List Output:
  IF NOT ( t_ekrs IS INITIAL AND t_ekrsdc IS INITIAL ).
    lcl_zdelete_ers_data=>ekrs_delete( ).
    lcl_zdelete_ers_data=>ekrsdc_delete( ).
    lcl_zdelete_ers_data=>result_display( ).
  ELSE.
    MESSAGE s303(me) WITH mess_text_004.
  ENDIF.

*&---------------------------------------------------------------------*
*&       Class (Implementation)  LCL_ZDELETE_ERS_DATA
*&---------------------------------------------------------------------*
CLASS lcl_zdelete_ers_data IMPLEMENTATION.

*----------------------------------------------------------------------*
* METHOD screen_description
*----------------------------------------------------------------------*
  METHOD screen_description.

    sy-title = 'Delete entries in DB table EKRS/EKRSDC (Version 3)'.

    titel_1  = 'Selection Criteria'.
    txtbukrs = 'Company Code'.
    txtekorg = 'Purchasing Organization'.
    txtebeln = 'Purchasing Document Number'.
    txtebelp = 'Purchasing Document Item'.
    txters   = 'ERS settlement'.

    titel_2  = 'Processing Options'.
    txtpck   = 'Package size'.

    titel_3 = 'UPDATE Options'.
    txtupdt = 'UPDATE Mode'.
    txtuser = 'User name'.

  ENDMETHOD.                    "screen_description
*----------------------------------------------------------------------*
* METHOD authorization_check
*----------------------------------------------------------------------*
  METHOD authorization_check.
    DATA: returncode(1) TYPE c,
          question      TYPE string.

*   Do not forget that no line in the program must be longer than 73 Col
*   just in case output is changed from list to ALV.
    CONCATENATE: 'Do you really want to DELETE the entries in table'
                 ' EKRS/EKRSDC?' INTO question.

    LOOP AT SCREEN.
      IF screen-group1 = 'SC1' AND ( p_user EQ 'ONLYSAP' ).
        screen-invisible = '0'.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.

    IF p_update IS NOT INITIAL.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = ' '
          text_question         = question
          text_button_1         = 'Yes'
          icon_button_1         = 'ICON_OKAY'
          text_button_2         = 'Cancel'
          icon_button_2         = 'ICON_CANCEL'
          default_button        = '2'
          display_cancel_button = ' '
        IMPORTING
          answer                = returncode.

      IF returncode = 2.
        CLEAR: p_update.
        LEAVE SCREEN.
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "authorization_check
*----------------------------------------------------------------------*
* METHOD data_initialization
*----------------------------------------------------------------------*
  METHOD data_initialization.

    CLEAR: count.
    CLEAR: s_ekko, s_ekpo, s_ekrs, s_ekrsdc.
    CLEAR: t_ekko, t_ekpo, t_ekrs, t_ekrsdc.

  ENDMETHOD.                    "data_initialization
*----------------------------------------------------------------------*
* METHOD database_select
*----------------------------------------------------------------------*
  METHOD database_select.

    IF so_bukrs AND so_ekorg IS INITIAL.
      SELECT ebeln ebelp loekz elikz erekz pstyp knttp kzvbr vrtkz
             twrkz wepos weunb repos webre lebre xersy
         FROM ekpo INTO TABLE t_ekpo PACKAGE SIZE p_pack
              WHERE ebeln IN so_ebeln
              AND   ebelp IN so_ebelp
              AND   loekz IN ('L')
              AND   xersy EQ p_xersy.

        IF t_ekpo IS NOT INITIAL.
          SELECT * FROM ekrs APPENDING TABLE t_ekrs
            FOR ALL ENTRIES IN t_ekpo
                WHERE ebeln EQ t_ekpo-ebeln
                AND   ebelp EQ t_ekpo-ebelp.

          SELECT * FROM ekrsdc APPENDING TABLE t_ekrsdc
            FOR ALL ENTRIES IN t_ekpo
                WHERE ebeln EQ t_ekpo-ebeln
                AND   ebelp EQ t_ekpo-ebelp.
        ENDIF.
      ENDSELECT.

    ELSE.
      SELECT ebeln bukrs bstyp bsart loekz lifnr ekorg lifre
        FROM ekko INTO TABLE t_ekko PACKAGE SIZE p_pack
            WHERE ebeln IN so_ebeln
            AND   bukrs IN so_bukrs
            AND   ekorg IN so_ekorg.

        IF NOT t_ekko IS INITIAL.
          SELECT ebeln ebelp loekz elikz erekz pstyp knttp kzvbr vrtkz
                 twrkz wepos weunb repos webre lebre xersy
             FROM ekpo INTO TABLE t_ekpo
              FOR ALL ENTRIES IN t_ekko
                  WHERE ebeln EQ t_ekko-ebeln
                  AND   ebelp IN so_ebelp
                  AND   loekz IN ('L')
                  AND   xersy EQ p_xersy.

          IF t_ekpo IS NOT INITIAL.
            SELECT * FROM ekrs APPENDING TABLE t_ekrs
              FOR ALL ENTRIES IN t_ekpo
                  WHERE ebeln EQ t_ekpo-ebeln
                  AND   ebelp EQ t_ekpo-ebelp.

            SELECT * FROM ekrsdc APPENDING TABLE t_ekrsdc
              FOR ALL ENTRIES IN t_ekpo
                  WHERE ebeln EQ t_ekpo-ebeln
                  AND   ebelp EQ t_ekpo-ebelp.
          ENDIF.
        ENDIF.
      ENDSELECT.
    ENDIF.

  ENDMETHOD.                    "database_select
*----------------------------------------------------------------------*
* METHOD result_display
*----------------------------------------------------------------------*
  METHOD result_display.

    SORT t_ekrs BY ebeln ebelp gjahr belnr buzei.
    SORT t_ekrsdc BY ebeln ebelp gjahr belnr buzei stunr zaehk.

*-- EKRS table entries ------------------------------------------------*
    ULINE AT 1(80).
    WRITE: /1 '|',
          2(78) '* TABLE - EKRS *' CENTERED
            COLOR COL_TOTAL,
          80 '|'.
    ULINE AT /1(80).

    NEW-LINE.
    FORMAT COLOR COL_HEADING.
    WRITE: 2(8)'BUDAT', 12(10)'LIFNR', 24(10)'BELNR', 36(5)'BUZEI',
           42(5)'GJAHR', 48(10)'EBELN', 60(5)'EBELP'.
    FORMAT COLOR OFF.

    NEW-LINE.
    ULINE AT 1(80).

    LOOP AT t_ekrs INTO s_ekrs.
      NEW-LINE.
      FORMAT COLOR COL_KEY.
      WRITE: 2(8)   s_ekrs-budat,
             12(10) s_ekrs-lifnr,
             24(10) s_ekrs-belnr,
             36(4)  s_ekrs-buzei,
             42(4)  s_ekrs-gjahr.
      FORMAT COLOR OFF.
      WRITE: 48(10) s_ekrs-ebeln,
             60(5)  s_ekrs-ebelp.

      count = count + 1.
    ENDLOOP.

    NEW-LINE.
    SKIP.
    IF p_update IS INITIAL.
      WRITE: count, mess_text_002_ekrs.
    ELSE.
      WRITE: count, mess_text_001_ekrs.
    ENDIF.
    SKIP. SKIP.

    CLEAR: count.

*-- EKRSDC table entries  ---------------------------------------------*
    ULINE AT 1(80).
    WRITE: /1 '|',
          2(78) '* TABLE - EKRSDC *' CENTERED
            COLOR COL_TOTAL,
          80 '|'.
    ULINE AT /1(80).

    NEW-LINE.
    FORMAT COLOR COL_HEADING.
    WRITE: 2(5)'GJAHR', 8(10)'BELNR', 20(5)'BUZEI', 26(5)'STUNR',
           32(5)'ZAEHK', 38(5)'KSCHL', 44(10)'EBELN', 56(5)'EBELP'.
    FORMAT COLOR OFF.

    NEW-LINE.
    ULINE AT 1(80).

    LOOP AT t_ekrsdc INTO s_ekrsdc.
      NEW-LINE.
      FORMAT COLOR COL_KEY.
      WRITE: 2(5)   s_ekrsdc-gjahr,
             8(10)  s_ekrsdc-belnr,
             20(5)  s_ekrsdc-buzei,
             26(5)  s_ekrsdc-stunr,
             32(5)  s_ekrsdc-zaehk,
             38(5)  s_ekrsdc-kschl.
      FORMAT COLOR OFF.
      WRITE: 44(10) s_ekrs-ebeln,
             56(5)  s_ekrs-ebelp.
      count = count + 1.
    ENDLOOP.

    NEW-LINE.
    SKIP.
    IF p_update IS INITIAL.
      WRITE: count, mess_text_002_ekrsdc.
    ELSE.
      WRITE: count, mess_text_001_ekrsdc.
    ENDIF.
    SKIP. SKIP.

  ENDMETHOD.                    "result_display
*----------------------------------------------------------------------*
* METHOD ekrs_delete
*----------------------------------------------------------------------*
  METHOD ekrs_delete.
    CHECK p_update IS NOT INITIAL.
    CHECK t_ekrs IS NOT INITIAL.

    CALL FUNCTION 'ME_UPDATE_EKRS'
      EXPORTING
        i_insert = ' '
        i_delete = 'X'
      TABLES
        t_ekrs   = t_ekrs.

  ENDMETHOD.                    "ekrs_delete
*----------------------------------------------------------------------*
* METHOD ekrsdc_delete
*----------------------------------------------------------------------*
  METHOD ekrsdc_delete.
    CHECK p_update IS NOT INITIAL.
    CHECK t_ekrsdc IS NOT INITIAL.

    CALL FUNCTION 'ME_UPDATE_EKRSDC'
      EXPORTING
        i_insert = ' '
        i_delete = 'X'
      TABLES
        t_ekrsdc = t_ekrsdc.

  ENDMETHOD.                    "ekrsdc_delete
ENDCLASS.               "LCL_ZDELETE_ERS_DATA
