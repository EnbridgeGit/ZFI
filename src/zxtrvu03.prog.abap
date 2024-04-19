**&---------------------------------------------------------------------*
**&  Include           ZXTRVU03
**&---------------------------------------------------------------------*
**"----------------------------------------------------------------------
**"*"Local Interface:
**"  TABLES
**"      KMSUM STRUCTURE  PTRV_KMSUM
**"      BELEG STRUCTURE  PTK03
**"      EXBEL STRUCTURE  PTK33
**"      ABZUG STRUCTURE  PTK04
**"      TRANSPORT STRUCTURE  GLO_PTRV_TRANSP
**"      ZIEL STRUCTURE  PTK05
**"      ZWECK STRUCTURE  PTK09
**"      KONTI STRUCTURE  PTK17
**"      VSCH STRUCTURE  PTK08
**"      KMVER STRUCTURE  PTK10
**"      PAUFA STRUCTURE  PTK21
**"      UEBPA STRUCTURE  PTK25
**"      BELER STRUCTURE  PTK20
**"      VPFPS STRUCTURE  PTK22
**"      VPFPA STRUCTURE  PTK23
**"      ROT STRUCTURE  PTK30
**"      RUW STRUCTURE  PTK27
**"      AEND STRUCTURE  PTK11
**"      KOSTR STRUCTURE  PTK14
**"      KOSTZ STRUCTURE  PTK16
**"      KOSTB STRUCTURE  PTK07
**"      KOSTK STRUCTURE  PTK18
**"      V0SPLIT STRUCTURE  PTP70
**"      EDITOR STRUCTURE  PTP71
**"      USER STRUCTURE  PTK99
**"      HINZ_WERB_S STRUCTURE  FITV_HINZ_WERB_S OPTIONAL
**"      HINZ_WERB_B STRUCTURE  FITV_HINZ_WERB_B OPTIONAL
**"  CHANGING
**"     VALUE(TRIP_HEADER) LIKE  PTRV_HEAD STRUCTURE  PTRV_HEAD
**"     VALUE(TRIP_PERIOD) LIKE  PTRV_PERIO STRUCTURE  PTRV_PERIO
**"     VALUE(TRIP_STATUS) LIKE  PTK12 STRUCTURE  PTK12
**"     VALUE(CONTINUE_WITH_UPDATE) TYPE  XFELD DEFAULT 'Y'
**"----------------------------------------------------------------------
*TYPES : BEGIN OF t_bukrs,
*         bukrs TYPE bukrs,
*        END OF t_bukrs.
*
DATA :  ls_ptk03 TYPE ptk03,
*
*        ls_ptk33 TYPE ptk33,
*
*        lt_bukrs TYPE STANDARD TABLE OF t_bukrs,
*        ls_bukrs LIKE LINE OF lt_bukrs,
*
        lv_empcompcode TYPE bukrs,
*
*        lv_wagetype TYPE t706b4-lgarl,
*        ls_t706k TYPE t706k,
*
*        lv_glacc TYPE saknr,
*
*        lv_milcode TYPE spkzl,
*
*        lv_firma TYPE t706b1-firma,
*
*        ls_ptk07 TYPE ptk07,
*        ls_ptk17 TYPE ptk17,
*        ls_ptk10 TYPE ptk10,
*        ls_ptk18 TYPE ptk18,
*        ls_ptk14 TYPE ptk14,
*
*        ls_pa0001 TYPE pa0001,
*
**        ls_zftv_cccode TYPE zftv_cccode,
        ls_tax_codes TYPE zftv_ca_tax_xref.
*
*
*
*
*
"Get employee Company Code
SELECT SINGLE bukrs FROM pa0001
                    INTO lv_empcompcode
                    WHERE pernr = trip_header-pernr.


"Loop through Line Items
LOOP AT beleg INTO ls_ptk03.
*
*  "Warn about OOP expenses with non OOP expense types
*  IF ls_ptk03-paper_receipt = 'X'. "Not credit card.
*    "Loop up expense type
*    SELECT SINGLE firma
*      FROM t706b1
*      INTO lv_firma
*      WHERE morei = 'UG'
*        AND spkzl = ls_ptk03-spkzl.
*
*    "If payed by company give error
*    IF lv_firma = 'X'.
*      MESSAGE ID 'ZFITA_MSG' TYPE 'E' NUMBER '011' WITH ls_ptk03-spkzl ls_ptk03-belnr.
*    ENDIF.
*  ENDIF.
*
*
*
  "Tax Code determination
  CLEAR: ls_tax_codes.

  IF ls_ptk03-lndfr = 'CA'.
    SELECT SINGLE * FROM zftv_ca_tax_xref  INTO ls_tax_codes
      WHERE spkzl = ls_ptk03-spkzl
        AND rgion = ls_ptk03-rgion.
    IF sy-subrc = 0.
      ls_ptk03-mwskz = ls_tax_codes-mwskz.
    ELSE.
      ls_ptk03-mwskz = 'T2'.
    ENDIF.
  ELSE.
    ls_ptk03-mwskz = 'I0'. "UG
    "ls_ptk03-mwskz = 'XX'. "SW
  ENDIF.
  MODIFY beleg FROM ls_ptk03 TRANSPORTING mwskz.

*  "Validate the entry is not 0 dollars.
*  IF ls_ptk03-betrg = 0.
*    MESSAGE ID 'ZFITA_MSG' TYPE 'E' NUMBER '010' WITH ls_ptk03-belnr.
*  ENDIF.
*
*********************************
***Company Code Validation
*********************************
*  "Ensure Document Number exists for this line
*  CHECK ls_ptk03-belnr IS NOT INITIAL.
*
*  "Get the GL account for the Travel Expense
*  SELECT SINGLE saknr FROM zftv_teexpcode
*                      INTO lv_glacc
*                      WHERE spkzl = ls_ptk03-spkzl.
*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*    EXPORTING
*      input  = lv_glacc
*    IMPORTING
*      output = lv_glacc.
*
*  "Get the Company Codes for this GL
*  SELECT bukrs FROM skb1
*                    INTO TABLE lt_bukrs
*                    WHERE saknr = lv_glacc.
*
*
*  "Validate Trip Line Cost Assignment matches GL Company Code
*  IF kostb[] IS NOT INITIAL.
*    READ TABLE kostb INTO ls_ptk07 WITH KEY belnr = ls_ptk03-belnr.
*    IF sy-subrc EQ 0.
*      READ TABLE konti INTO ls_ptk17 WITH KEY kokey = ls_ptk07-kokey.
*      IF sy-subrc EQ 0.
*        READ TABLE lt_bukrs INTO ls_bukrs WITH KEY bukrs = ls_ptk17-bukrs.
*        IF sy-subrc NE 0.
*          MESSAGE ID 'ZFITA_MSG' TYPE 'E' NUMBER '003' WITH ls_ptk17-bukrs ls_ptk03-belnr.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*  ENDIF.
*
*  "Validate Entire Trip Cost Assigment matches GL Company Code
*  IF kostr[] IS NOT INITIAL.
*    LOOP AT kostr INTO ls_ptk14.
*      IF NOT ls_ptk14-kokey IS INITIAL.
*        IF konti[] IS NOT INITIAL.
*          READ TABLE konti INTO ls_ptk17 WITH KEY kokey = ls_ptk14-kokey.
*          IF sy-subrc EQ 0.
*            READ TABLE lt_bukrs INTO ls_bukrs WITH KEY bukrs = ls_ptk17-bukrs.
*            IF sy-subrc NE 0.
*              MESSAGE ID 'ZFITA_MSG' TYPE 'E' NUMBER '004' WITH ls_ptk17-bukrs ls_ptk03-spkzl.
*            ENDIF.
*          ENDIF.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.
*
*  "Validate Default Accounting if not specified
*  IF  kostr[] IS INITIAL AND kostb[] IS INITIAL.
*    IF lv_empcompcode IS NOT INITIAL.
*      "Default to employee company code
*      READ TABLE lt_bukrs INTO ls_bukrs WITH KEY bukrs = lv_empcompcode.
*      IF sy-subrc NE 0.
*        MESSAGE ID 'ZFITA_MSG' TYPE 'E' NUMBER '005' WITH lv_empcompcode ls_ptk03-spkzl.
*      ENDIF.
*    ENDIF.
*  ENDIF.
ENDLOOP.
*
*"Loop through Credit Card Entries
*LOOP AT exbel INTO ls_ptk33.
*
*ENDLOOP.
*
*"Loop through Mileage entries
*LOOP AT kmver INTO ls_ptk10.
*
*  "Get GL for Mileage Code
*  CONCATENATE 'FAK' ls_ptk10-kzpmf INTO lv_milcode.
*  SELECT SINGLE saknr FROM zftv_teexpcode
*                           INTO lv_glacc
*                           WHERE spkzl = lv_milcode.
**                    WHERE spkzl = ls_ptk03-spkzl.
*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*    EXPORTING
*      input  = lv_glacc
*    IMPORTING
*      output = lv_glacc.
*
*  "Get Company Codes for this GL
*  SELECT bukrs FROM skb1
*                    INTO TABLE lt_bukrs
*                    WHERE saknr = lv_glacc.
*
*  "Validate Trip Line Cost Assignment matches GL Company Code
*  IF NOT ls_ptk10-kmvnr IS INITIAL.
*    IF kostk[] IS NOT INITIAL.
*      READ TABLE kostk INTO ls_ptk18 WITH KEY kmvnr = ls_ptk10-kmvnr.
*      IF sy-subrc EQ 0.
*        READ TABLE konti INTO ls_ptk17 WITH KEY kokey = ls_ptk18-kokey.
*        IF sy-subrc EQ 0.
*          READ TABLE lt_bukrs INTO ls_bukrs WITH KEY bukrs = ls_ptk17-bukrs.
*          IF sy-subrc NE 0.
*            MESSAGE ID 'ZFITA_MSG' TYPE 'E' NUMBER '006' WITH ls_ptk17-bukrs ls_ptk03-belnr.
*          ENDIF.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*
*    "Validate Entire Trip Cost Assigment matches GL Company Code
*    IF NOT kostr[] IS INITIAL.
*      LOOP AT kostr INTO ls_ptk14.
*        IF konti[] IS NOT INITIAL.
*          READ TABLE konti INTO ls_ptk17 WITH KEY kokey = ls_ptk14-kokey."kostr-kokey.
*          IF sy-subrc EQ 0.
*            READ TABLE lt_bukrs INTO ls_bukrs WITH KEY bukrs = ls_ptk17-bukrs.
*            IF sy-subrc NE 0.
*              MESSAGE ID 'ZFITA_MSG' TYPE 'E' NUMBER '007' WITH ls_ptk17-bukrs ls_ptk03-spkzl.
*            ENDIF.
*          ENDIF.
*        ENDIF.
*      ENDLOOP.
*    ENDIF.
*  ENDIF.
*
*  "Validate Default Accounting if not specified
*  IF  kostr[] IS INITIAL AND kostb[] IS INITIAL.
*    IF lv_empcompcode IS NOT INITIAL.
*      "Default to employee company code
*      READ TABLE lt_bukrs INTO ls_bukrs WITH KEY bukrs = lv_empcompcode.
*      IF sy-subrc NE 0.
*        MESSAGE ID 'ZFITA_MSG' TYPE 'E' NUMBER '005' WITH lv_empcompcode ls_ptk03-spkzl.
*      ENDIF.
*    ENDIF.
*  ENDIF.
*ENDLOOP.
