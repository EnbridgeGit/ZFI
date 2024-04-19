*&---------------------------------------------------------------------*
*& Program ZFAPR021_IN_HOUSE_CHECK_SRP                                 *
*&                                                                     *
*&---------------------------------------------------------------------*
*& Developer: A. Coulter
*& Date:      February 20, 2008
*& Original Transport: RD1K900122
*& Description: This program contains forms that are called from
*&              sapscript Z_AP_CHEQUE to get additional information for
*&              display purposes.
*----------------------------------------------------------------------*
* HOW TO TEST 1
* This method results in a new check number being generated
* 1. Go to TCode FCH7 to reprint checks.
* 2. Enter "Paying company code = 1000", House Bank = BNS, account Id = CADC, Printer for forms = LOCL
* 3. Enter a check number. To find a check number to reprint, you:
*    a) Goto TCode FCHN
*    b) Enter the paying company code, house bank and account_id the same as in step 2.
*    c) run the report. Any record with a "Enca./Void" value of blank (enchasement date is blank meaning the
*       check as not been cashed) can be reprinted.
* 4. Select menu Check -> Reprint
* 5. Go to Spool SP01 to view the check
*
* HOW TO TEST 2
* This method allows you to test without generating a new check number
* 1. Go to table REGUH and find a check number. Use the Run On field to limit the records.
* 2. Note the check number, fiscal year and company code.
* 3. Go to TCode FBZ5. Enter the check number, company code and fiscal year.
* 4. Enter 'C' for the payment method. Enter ZPDF for the "Printer for Forms".
* 5. Go to Menu "Check -> Print. Click on the "Old Check" button otherwise a new check number will be generated.
*
*
*&---------------------------------------------------------------------*
* C H A N G E   L O G                                                  *
* Author      : Kevin Barter                                           *
* Date        : 2009/09/19                                             *
* Issue No.   : 877                                                    *
* Description : Change the logic on which signatures can be pre        *
*               printed                                                *
*----------------------------------------------------------------------*
* Author      : Pavel Kratochvil
* Date        : 2011/12/02
* Feature     : 10612, 10631
* Description : - add new bank account: 87310-12
*----------------------------------------------------------------------*
* Author      : Corey LeDrew
* Date        : Oct 28 2015
* Feature     : FETR0012705
* Description : Modified rules for when 1st and 2nd signature get printed.
*               Added documentation to FORMS get_signature_1 and get_signature_2 to describe what
*               the routines are doing, the parameters and the rules. Added spaces in the code.
*----------------------------------------------------------------------*
* Author      : Corey LeDrew
* Date        : Jan 26 2016
* Feature     : FETR0012789
* Description : A bug when getting customer address information. The "Value" field in in_tab was not checked to
*               see if it was intial when name is REGUH-LIFNR
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* 11-Feb-2019  JRHARTUNG   D30K929884  CHG0146958  Initial development *
*                          D30K930133  CHG0148429  Initial development *
*                                                                      *                                                     *
*              Called by form:  ZFAPF021_TD_CA                         *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*

REPORT  zfi_cheque_forms.

*&---------------------------------------------------------------------*
*&      Form  GET_COMPANY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_company TABLES in_tab STRUCTURE itcsy
                        out_tab STRUCTURE itcsy.

  DATA: l_bukrs TYPE bukrs,
        l_adrnr TYPE adrnr,
        ls_adrc TYPE adrc.


  READ TABLE in_tab WITH KEY 'REGUP-ZBUKR'.
  CHECK sy-subrc = 0.
  l_bukrs = in_tab-value.

  SELECT SINGLE adrnr INTO l_adrnr
    FROM t001
    WHERE bukrs = l_bukrs.

  SELECT SINGLE * INTO ls_adrc
    FROM adrc
    WHERE addrnumber = l_adrnr.

  LOOP AT out_tab.
    CASE out_tab-name.
      WHEN 'NAME1'.
        out_tab-value = ls_adrc-name1.
      WHEN 'STREET'.
        out_tab-value = ls_adrc-street.
      WHEN 'CITY'.
        out_tab-value = ls_adrc-city1.
      WHEN 'REGION'.
        out_tab-value = ls_adrc-region.
      WHEN 'POSTCODE'.
        out_tab-value = ls_adrc-post_code1.
      WHEN 'PHONE'.
        out_tab-value = ls_adrc-tel_number.
      WHEN 'FAX'.
        out_tab-value = ls_adrc-fax_number.
      WHEN OTHERS.
    ENDCASE.
    MODIFY out_tab.
  ENDLOOP.

ENDFORM.                    " GET_COMPANY

*&---------------------------------------------------------------------*
*&      Form  get_company_enb
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IN_TAB     text
*      -->OUT_TAB    text
*----------------------------------------------------------------------*
FORM get_company_enb TABLES in_tab STRUCTURE itcsy
                        out_tab STRUCTURE itcsy.

  DATA: l_bukrs TYPE bukrs,
        l_adrnr TYPE adrnr,
        ls_adrc TYPE adrc.

  READ TABLE in_tab WITH KEY 'REGUP-ZBUKR'.
  CHECK sy-subrc = 0.
  l_bukrs = in_tab-value.

  SELECT SINGLE adrnr INTO l_adrnr
    FROM t001
    WHERE bukrs = l_bukrs.

  SELECT SINGLE * INTO ls_adrc
    FROM adrc
    WHERE addrnumber = l_adrnr.

  LOOP AT out_tab.
    CASE out_tab-name.
      WHEN 'NAME1'.
        out_tab-value = ls_adrc-name1.
      WHEN 'NAME3'.
        out_tab-value = |{ ls_adrc-name3 } { ls_adrc-name4 } |.
      WHEN 'STREET'.
        out_tab-value = ls_adrc-street.
      WHEN 'CITY'.
        out_tab-value = ls_adrc-city1.
      WHEN 'REGION'.
        out_tab-value = ls_adrc-region.
      WHEN 'POSTCODE'.
        out_tab-value = ls_adrc-post_code1.
      WHEN 'PHONE'.
        out_tab-value = ls_adrc-tel_number.
      WHEN 'FAX'.
        out_tab-value = ls_adrc-fax_number.
      WHEN OTHERS.
    ENDCASE.
    MODIFY out_tab.
  ENDLOOP.

ENDFORM.                    " GET_COMPANY

*&---------------------------------------------------------------------*
*&      Form  GET_BANK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_bank TABLES in_tab STRUCTURE itcsy
                        out_tab STRUCTURE itcsy.

  DATA: l_bankl TYPE bankk,
        l_banks TYPE banks,
        l_adrnr TYPE adrnr,
        ls_adrc TYPE adrc,
        l_len TYPE i.

  READ TABLE in_tab WITH KEY 'GWA_REGUH-UBNKL'.
  IF sy-subrc = 0.
    l_bankl = in_tab-value.
*    l_len = strlen( l_bankl ).
*    if l_len = 10.
*      l_bankl = l_bankl+1(8).
*    endif.
  ELSE.
    EXIT.
  ENDIF.

  READ TABLE in_tab WITH KEY 'GWA_REGUH-UBNKS'.
  IF sy-subrc = 0.
    l_banks = in_tab-value.
  ELSE.
    EXIT.
  ENDIF.

  SELECT SINGLE adrnr INTO l_adrnr
    FROM bnka
    WHERE banks = l_banks
    AND   bankl = l_bankl.

  SELECT SINGLE * INTO ls_adrc
    FROM adrc
    WHERE addrnumber = l_adrnr.

  LOOP AT out_tab.
    CASE out_tab-name.
      WHEN 'BANKNAME'.
        out_tab-value = ls_adrc-name1.
      WHEN 'BANKSTREET'.
        out_tab-value = ls_adrc-street.
      WHEN 'CITYPROV'.
        CONCATENATE ls_adrc-city1 ls_adrc-region INTO out_tab-value
          SEPARATED BY ' '.
      WHEN 'BANKPOSTCODE'.
        out_tab-value = ls_adrc-post_code1.
      WHEN OTHERS.
    ENDCASE.
    MODIFY out_tab.
  ENDLOOP.
ENDFORM.                    " GET_BANK

*------------------------------------------------------------------------------------------
* FORM:  GET_VENDOR_ADDRESS
*
* PARMS: in_tab - contains name and value pair. Contains 2 records as follows
*                 NAME          VALUE
*                 REGUH-KUNNR   Customer Number if this is a customer check otherwise blank
*                 REGUH_LIFNR   Vendor Number if this is a vendor check otherwise blank
* RETURN: out-tab - Contains the full address of either the customer or vendor.
*
* HOW TO TEST 1
* This method results in a new check number being generated
* 1. Go to TCode FCH7 to reprint checks.
* 2. Enter "Paying company code = 1000", House Bank = BNS, account Id = CADC, Printer for forms = LOCL
* 3. Enter "Check Lot Number" as 1. Enter a check number. To find a check number to reprint, you:
*    a) Goto TCode FCHN
*    b) Enter the paying company code, house bank and account_id the same as in step 2.
*    c) run the report. Any record with a "Enca./Void" value of blank (enchasement date is blank meaning the
*       check as not been cashed) can be reprinted.
* 4. Select menu Check -> Reprint
* 5. Go to Spool SP01 to view the check
*
* HOW TO TEST 2
* This method allows you to test without generating a new check number
* 1. Go to table REGUH and find a check number. Use the Run On field to limit the records.
* 2. Note the check number, fiscal year and company code.
* 3. Go to TCode FBZ5. Enter the check number, company code and fiscal year.
* 4. Enter 'C' for the payment method. Enter ZPDF for the "Printer for Forms".
* 5. Go to Menu "Check -> Print. Click on the "Old Check" button otherwise a new check number will be generated.
*
* CHANGES:
* FETR0012789 - Corey L - Jan 26 2016. Customer address was not printing because the value field in in_tab was not
*               checked to see if it was initial or not.
*----------------------------------------------------------------------------------------------
FORM get_vendor_address TABLES in_tab STRUCTURE itcsy
                        out_tab STRUCTURE itcsy.

  TYPE-POOLS szadr.
  DATA: l_lifnr TYPE kunnr,
        l_adrnr TYPE ad_addrnum,
        l_addr_type TYPE ad_adrtype,
        l_name TYPE char80,
        l_care_of TYPE char80,
        l_street  TYPE char80,  " FETR0012789. Some customers have street filled in rather then street1
        l_street1 TYPE char80,
        l_street2 TYPE char80,
        l_city_prov TYPE char80,
        l_postcode TYPE char80,
        l_land TYPE char80,
        lt_address TYPE szadr_printform_table,
        ls_address LIKE LINE OF lt_address.

  FIELD-SYMBOLS <LFS_in_tab> LIKE LINE OF in_tab.

  READ TABLE in_tab WITH KEY 'GWA_REGUH-LIFNR' ASSIGNING <LFS_in_tab>.
  IF <LFS_in_tab>-value IS NOT INITIAL. " This is a vendor cheque. FETR0012789
    l_lifnr = in_tab-value.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = l_lifnr
      IMPORTING
        output = l_lifnr.
    SELECT SINGLE adrnr INTO l_adrnr
      FROM lfa1
      WHERE lifnr = l_lifnr.
  ELSE.
    READ TABLE in_tab WITH KEY 'GWA_REGUH-KUNNR'. " This is a customer cheque
    IF sy-subrc = 0.
      l_lifnr = in_tab-value.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = l_lifnr
        IMPORTING
          output = l_lifnr.
      SELECT SINGLE adrnr INTO l_adrnr
        FROM kna1
        WHERE kunnr = l_lifnr.
    ELSE.
      EXIT.
    ENDIF.
  ENDIF.
  l_addr_type = '1'.

  CALL FUNCTION 'ADDRESS_INTO_PRINTFORM'
    EXPORTING
      address_type            = l_addr_type
      address_number          = l_adrnr
    IMPORTING
      address_printform_table = lt_address.

  LOOP AT lt_address INTO ls_address.
    CASE ls_address-line_type.
      WHEN '1'. " Name 1. You can View types from Domain AD_LINE_TP
        l_name = ls_address-address_line.
      WHEN '2'. " Name 2
        IF l_name+34(1) <> ' '.
          CONCATENATE l_name ls_address-address_line INTO l_name.
        ELSE.
          CONCATENATE l_name ls_address-address_line INTO l_name
          SEPARATED BY space.
        ENDIF.
      WHEN '5'. " c/0 name
        CONCATENATE 'Y' ls_address-address_line INTO l_care_of.
      WHEN '6'. " Street 2
        CONCATENATE 'Y' ls_address-address_line INTO l_street1.
      WHEN '7'. " Street 3
        CONCATENATE 'Y' ls_address-address_line INTO l_street2.
      WHEN 'O'. " City Line
        l_city_prov = ls_address-address_line.
      WHEN 'L'. " Country
        l_land = ls_address-address_line.
      WHEN 'S'. " Street.                                        FETR0012789
        CONCATENATE 'Y' ls_address-address_line INTO l_street. " FETR0012789
      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.

  LOOP AT out_tab.
    CASE out_tab-name.
      WHEN 'VENDOR_NAME'.
        out_tab-value = l_name.
      WHEN 'CARE_OF'.
        out_tab-value = l_care_of.
      WHEN 'VENDOR_STREET1'.
        IF l_street1 IS NOT INITIAL.  " FETR0012789
          out_tab-value = l_street1.
        ELSE.                         " FETR0012789
          out_tab-value = l_street.   " FETR0012789
        ENDIF.                        " FETR0012789
      WHEN 'VENDOR_STREET2'.
        out_tab-value = l_street2.
      WHEN 'VENDOR_CITY_PROV'.
        out_tab-value = l_city_prov.
      WHEN 'VENDOR_LAND'.
        out_tab-value = l_land.
      WHEN OTHERS.
    ENDCASE.
    MODIFY out_tab.
  ENDLOOP.

ENDFORM.                    " GET_VENDOR_ADDRESS


*&----------------------------------------------------------------------------------------------------*
*& FORM: get_signature_1
*& DESC: This routine determines if the first signature should be printed on the check. The rules regarding this
*&       are dependant on the check amount and in some cases the bank code. If the signature is required, then
*&       the out_tab-value will equal 'Y'.
*&
*& PARMS: in_tab   Contains check amount, company code and bank code
*&        out_tab  Contains 1 Name, Value pair with value set to 'Y' or 'N'. 'Y' = print signature on check
*
*& RULES: 1. If company code is 1000 or 1100 and check amount < 20,000 then 1st signature is required
*&        2. If company code is 2000 or 2100 and check amount < 20,000 then 1st signature is required
*&        3. If company code is 3000 or 3100 and check amount < 20,000  and bank code is either
*&           ( '03071-14', '03571-11', '03070-17') then 2nd signature is required.
*-------------------------------------------------------------------------------------------------------*
FORM get_signature_1 TABLES in_tab STRUCTURE itcsy
                        out_tab STRUCTURE itcsy.

*  CONSTANTS: c_20000 TYPE char20 VALUE '           20,000.00',
*             c_50000 TYPE char20 VALUE '           50,000.00',
**--          KNB-877 Start 19.06.2009 10:56:38
*             c_1000  TYPE char20 VALUE '            1,000.00'.
**--          KNB-877 End 19.06.2009 10:56:38

  DATA: l_wrbtr TYPE char20,
        l_ubknt TYPE ubknt,
        l_bukrs TYPE bukrs,
        l_amount TYPE char20,
        l_answer TYPE char1,
        l_flag TYPE char1.

  l_answer = 'Y'.
*
*  LOOP AT in_tab.
*    CASE in_tab-name.
*      WHEN 'REGUH-RWBTR'.       " Check Amount
*        l_wrbtr = in_tab-value.
*      WHEN 'REGUP-ZBUKR'.
*        l_bukrs = in_tab-value. " Company Code
*      WHEN 'REGUH-UBKNT'.
*        l_ubknt = in_tab-value. " Bank Code. EX ('5119146','01326-16', '86340-17','87310-12')
*      WHEN OTHERS.
*    ENDCASE.
*  ENDLOOP.
*
*  SHIFT l_wrbtr LEFT DELETING LEADING space.
*  SHIFT l_wrbtr RIGHT DELETING TRAILING space.
*
*  " For company 1000\1100, if the check amount is less then 20,000 then the 1st signature is printed.
*  IF l_bukrs = '1000' OR l_bukrs = '1100'.
*    IF l_wrbtr < c_20000.
*      l_answer = 'Y'.
*    ENDIF.
*
*  " For company 2000\2100, if the check amount is less then 20,000 then the 1st signature is printed.
*  ELSEIF l_bukrs = '2000' OR l_bukrs = '2100'.
*    IF l_wrbtr < c_20000.
*      l_answer = 'Y'.
*    ENDIF.
*
*  " For company 3000\3100, if the check amount is less then 20,000 and bank code is either ('03071-14',
*  " '03571-11','03070-17'), then the 1st signature is printed.
*  ELSEIF l_bukrs = '3000' OR l_bukrs = '3100'.
*    IF l_ubknt = '03071-14'
*    OR l_ubknt = '03571-11'
*    OR l_ubknt = '03070-17'.
*      IF l_wrbtr < c_20000.
*        l_answer = 'Y'.
*      ENDIF.
*    ENDIF.
*  ENDIF.
*
*  IMPORT l_flag = l_flag FROM MEMORY ID 'Z_AP_CHEQUE'.
*  IF l_flag = 'X' OR l_flag = 'S'.
*    l_answer = 'N'.
*  ENDIF.

  LOOP AT out_tab.
    out_tab-value = l_answer.
    MODIFY out_tab.
  ENDLOOP.

ENDFORM.                    " GET_SIGNATURE_1


*&----------------------------------------------------------------------------------------------*
*& FORM: get_signature_2
*& DESC: This routine determines if a 2nd signature should be printed on the check. The rules regarding this
*&       are dependant on the check amount and in some cases the bank code. If the signature is required, then
*&       the out_tab-value will equal 'Y'.
*&
*& PARMS: in_tab   Contains check amount, company code and bank code
*&        out_tab   Contains 1 Name, Value pair with value set to 'Y' or 'N'. 'Y' = print signature on check
*&
*& RULES: 1. If company code is 1000 or 1100 and check amount < 150,000 then 2nd signature is required
*&        2. If company code is 2000 or 2100 and check amount < 150,000 then 2nd signature is required
*&        3. If company code is 3000 or 3100 and check amount < 150,000  and bank code is either
*&           ( '03071-14', '03571-11', '03070-17') then 2nd signature is required
*-----------------------------------------------------------------------------------------------*
FORM get_signature_2 TABLES in_tab STRUCTURE itcsy
                        out_tab STRUCTURE itcsy.

*  CONSTANTS: c_10000 TYPE char20 VALUE '           10,000.00',
*             c_20000 TYPE char20 VALUE '           20,000.00',
*             c_50000 TYPE char20 VALUE '           50,000.00',
*             c_150000 TYPE char20 VALUE '          150,000.00'.
  DATA: l_wrbtr TYPE char20,
        l_ubknt TYPE ubknt,
        l_bukrs TYPE bukrs,
        l_amount TYPE char20,
        l_answer TYPE char1,
        l_flag TYPE char1.

  l_answer = 'Y'.

*  LOOP AT in_tab.
*    CASE in_tab-name.
*      WHEN 'REGUH-RWBTR'. " Check Amount
*        l_wrbtr = in_tab-value.
*      WHEN 'REGUP-ZBUKR'. " Company Code
*        l_bukrs = in_tab-value.
*      WHEN 'REGUH-UBKNT'. " Bank Code. EX ('5119146','01326-16', '86340-17','87310-12')
*        l_ubknt = in_tab-value.
*      WHEN OTHERS.
*    ENDCASE.
*  ENDLOOP.
*
*  SHIFT l_wrbtr LEFT DELETING LEADING space.
*  SHIFT l_wrbtr RIGHT DELETING TRAILING space.
*
*  " For company 1000\1100, if the check amount is less then 150,000 then the 2nd signature is printed.
*  IF l_bukrs = '1000' OR l_bukrs = '1100'.
*    IF l_wrbtr < c_150000.
*      l_answer = 'Y'.
*    ENDIF.
*
*  " For company 2000\2100, if the check amount is less then 150,000 then the 2nd signature is printed.
*  ELSEIF l_bukrs = '2000' OR l_bukrs = '2100'.
*    IF l_wrbtr < c_150000.
*      l_answer = 'Y'.
*    ENDIF.
*
*  " For company 3000\3100, if the check amount is less then 150,000 then the 2nd signature is printed
*  " if bank account is in ('03071-14','03571-11','03070-17') .
*  ELSEIF l_bukrs = '3000' OR l_bukrs = '3100'.
*    IF l_ubknt = '03071-14'
*    OR l_ubknt = '03571-11'
*    OR l_ubknt = '03070-17'.
*      IF l_wrbtr < c_150000.
*        l_answer = 'Y'.
*      ENDIF.
*    ENDIF.
*  ENDIF.
*
*  IMPORT l_flag = l_flag FROM MEMORY ID 'Z_AP_CHEQUE'.
*  IF l_flag = 'X' OR l_flag = 'S'.
*    l_answer = 'N'.
*  ENDIF.
  LOOP AT out_tab.
    out_tab-value = l_answer.
    MODIFY out_tab.
  ENDLOOP.

ENDFORM.                    " GET_SIGNATURE_2
*&---------------------------------------------------------------------*
*&      Form  GET_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_date  TABLES in_tab STRUCTURE itcsy
                        out_tab STRUCTURE itcsy.

  LOOP AT out_tab.
    out_tab-value = sy-datum.
    MODIFY out_tab.
  ENDLOOP.

ENDFORM.                    " GET_DATE
*&---------------------------------------------------------------------*
*&      Form  GET_NET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_net  TABLES in_tab STRUCTURE itcsy
                        out_tab STRUCTURE itcsy.

  DATA: l_wrbtr TYPE wrbtv,
        l_wabzg TYPE wabzv,
        l_belnr TYPE belnr_d,
        l_zbukr TYPE bukrs,
        l_gjahr TYPE gjahr,
        l_xblnr TYPE xblnr1.

  LOOP AT in_tab.
    CASE in_tab-name.
      WHEN 'REGUD-WRBTR'.
        TRANSLATE in_tab-value USING ', '.
        CONDENSE in_tab-value NO-GAPS.

        " Corey L. See if value contains xxx and default value. This allows you to test form in SE71 without short dump
        IF in_tab-value CS 'XXX'.
          l_wrbtr = 0.
        ELSE.
          l_wrbtr = in_tab-value.
        ENDIF.

      WHEN 'REGUD-WABZG'.
        TRANSLATE in_tab-value USING ', '.
        CONDENSE in_tab-value NO-GAPS.

        " Corey L. See if value contains xxx and default value. This allows you to test form in SE71 without short dump
        IF in_tab-value CS 'XXX'.
          l_wabzg = 0.
        ELSE.
          l_wabzg = in_tab-value.
        ENDIF.

      WHEN 'REGUP-BELNR'.
        l_belnr = in_tab-value.
      WHEN 'REGUP-ZBUKR'.
        l_zbukr = in_tab-value.
      WHEN 'REGUP-GJAHR'.
        l_gjahr = in_tab-value.
      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.

  " Jan 7 2009.   Corey L.  Added l_gjahr parameter
  SELECT SINGLE xblnr INTO l_xblnr
    FROM bkpf
    WHERE belnr = l_belnr
    AND   bukrs = l_zbukr
    AND   gjahr = l_gjahr.

  l_wrbtr = l_wrbtr - l_wabzg.
  LOOP AT out_tab.
    CASE out_tab-name.
      WHEN 'NET_DOLLARS'.
        out_tab-value = l_wrbtr.
        SHIFT out_tab-value LEFT DELETING LEADING ' '.
      WHEN 'VENDOR_REF'.
        out_tab-value = l_xblnr.
      WHEN OTHERS.
    ENDCASE.
    MODIFY out_tab.
  ENDLOOP.

ENDFORM.                    " GET_NET
*&---------------------------------------------------------------------*
*&      Form  GET_GROSS
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_gross  TABLES in_tab STRUCTURE itcsy
                        out_tab STRUCTURE itcsy.

  DATA: l_wrbtr TYPE char20.

  LOOP AT in_tab.
    CASE in_tab-name.
      WHEN 'REGUD-SWNES'.
        TRANSLATE in_tab-value USING '* '.
        CONDENSE in_tab-value NO-GAPS.
        l_wrbtr = in_tab-value.
      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.

  LOOP AT out_tab.
    out_tab-value = l_wrbtr.
    SHIFT out_tab-value LEFT DELETING LEADING ' '.
    MODIFY out_tab.
  ENDLOOP.

ENDFORM.                    " GET_GROSS
*&---------------------------------------------------------------------*
*&      Form  SET_VOID_FLAG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_void_flag TABLES in_tab STRUCTURE itcsy
                        out_tab STRUCTURE itcsy.

  DATA: l_flag.

  l_flag = 'X'.
  EXPORT l_flag = l_flag TO MEMORY ID 'Z_AP_CHEQUE'.

ENDFORM.                    " SET_VOID_FLAG
*&---------------------------------------------------------------------*
*&      Form  GET_VOID_FLAG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_void_flag TABLES in_tab STRUCTURE itcsy
                        out_tab STRUCTURE itcsy.

  DATA: l_flag,
        l_signature_flag.

  IMPORT l_flag = l_flag FROM MEMORY ID 'Z_AP_CHEQUE'.
  FREE MEMORY ID 'Z_AP_CHEQUE'.
  IF l_flag = 'X'.
    l_signature_flag = 'X'.
  ENDIF.

  LOOP AT out_tab.
    out_tab-value = l_flag.
    MODIFY out_tab.
  ENDLOOP.

  IF l_signature_flag = 'X'.
    l_flag = 'S'.
    EXPORT l_flag = l_flag TO MEMORY ID 'Z_AP_CHEQUE'.
  ENDIF.

ENDFORM.                    " GET_VOID_FLAG
*&---------------------------------------------------------------------*
*&      Form  ZERO_FILL_CHECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zero_fill_chect TABLES in_tab STRUCTURE itcsy
                        out_tab STRUCTURE itcsy.

  DATA: l_chect TYPE char7.

  LOOP AT in_tab.
    CASE in_tab-name.
      WHEN 'REGUD-CHECT'.
        l_chect = in_tab-value.
      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.
  SHIFT l_chect RIGHT DELETING TRAILING ' '.
  TRANSLATE l_chect USING ' 0'.
  LOOP AT out_tab.
    out_tab-value = l_chect.
    MODIFY out_tab.
  ENDLOOP.

ENDFORM.                    " ZERO_FILL_CHECT
*eject
*&---------------------------------------------------------------------*
*&      f_format_var_boa_check_micr
*----------------------------------------------------------------------*
*       Format the BOA US check MICR variables
*----------------------------------------------------------------------*
FORM f_format_var_boa_check_micr
  TABLES   iit_in_tab    STRUCTURE itcsy
           cit_out_tab   STRUCTURE itcsy.

  CONSTANTS:
           lc_name_1     TYPE tdtprgname
                         VALUE 'VAR1',
           lc_name_2     TYPE tdtprgname
                         VALUE 'VAR2',
           lc_name_3     TYPE tdtprgname
                         VALUE 'VAR3'.

  DATA:    lv_subrc      TYPE sysubrc,
           lv_tabix      TYPE sytabix,
           lv_var1(10)   TYPE c, "check number
           lv_var2(09)   TYPE c, "bank routing number
           lv_var3(12)   TYPE c, "bank account number
           lv_char18(18) TYPE c.

  CLEAR    lv_var1.
  CLEAR    lv_var2.
  CLEAR    lv_var3.

* Process the check number
  CLEAR          iit_in_tab.
  READ     TABLE iit_in_tab INDEX 1.
  IF ( sy-subrc EQ 0 ).
    CLEAR                              lv_char18.
    MOVE     iit_in_tab-value       TO lv_char18.
    SHIFT    lv_char18           RIGHT DELETING TRAILING SPACE.
    TRANSLATE                          lv_char18 USING ' 0'.
    CLEAR                              lv_var1.
    MOVE     lv_char18+08(10)       TO lv_var1.
  ENDIF.

* Process the bank routing number
  CLEAR          iit_in_tab.
  READ     TABLE iit_in_tab INDEX 2.
  IF ( sy-subrc EQ 0 ).
    CLEAR                              lv_char18.
    MOVE     iit_in_tab-value       TO lv_char18.
    SHIFT    lv_char18           RIGHT DELETING TRAILING SPACE.
    TRANSLATE                          lv_char18 USING ' 0'.
    CLEAR                              lv_var2.
    MOVE     lv_char18+09(09)       TO lv_var2.
  ENDIF.

* Process the bank account number
  CLEAR          iit_in_tab.
  READ     TABLE iit_in_tab INDEX 3.
  IF ( sy-subrc EQ 0 ).
    CLEAR                              lv_char18.
    MOVE     iit_in_tab-value       TO lv_char18.
    SHIFT    lv_char18           RIGHT DELETING TRAILING SPACE.
    TRANSLATE                          lv_char18 USING ' 0'.
    CLEAR                              lv_var3.
    MOVE     lv_char18+06(12)       TO lv_var3.
  ENDIF.

* Assign the output variables to the output parameter table

* Assign the check number
  CLEAR          cit_out_tab.
  READ     TABLE cit_out_tab  WITH KEY name = lc_name_1.
  lv_subrc = sy-subrc.
  lv_tabix = sy-tabix.
  IF ( lv_subrc EQ 0 ).
    CLEAR                              cit_out_tab-value.
    MOVE     lv_var1                TO cit_out_tab-value.
    MODIFY                             cit_out_tab
                                 INDEX lv_tabix.
  ENDIF.

* Assign the bank routing number
  CLEAR          cit_out_tab.
  READ     TABLE cit_out_tab  WITH KEY name = lc_name_2.
  lv_subrc = sy-subrc.
  lv_tabix = sy-tabix.
  IF ( lv_subrc EQ 0 ).
    CLEAR                              cit_out_tab-value.
    MOVE     lv_var2                TO cit_out_tab-value.
    MODIFY                             cit_out_tab
                                 INDEX lv_tabix.
  ENDIF.

* Assign the bank account number
  CLEAR          cit_out_tab.
  READ     TABLE cit_out_tab  WITH KEY name = lc_name_3.
  lv_subrc = sy-subrc.
  lv_tabix = sy-tabix.
  IF ( lv_subrc EQ 0 ).
    CLEAR                              cit_out_tab-value.
    MOVE     lv_var3                TO cit_out_tab-value.
    MODIFY                             cit_out_tab
                                 INDEX lv_tabix.
  ENDIF.

  CLEAR    iit_in_tab.
  CLEAR    cit_out_tab.

ENDFORM.                    " f_format_var_boa_check_micr
*eject
*&---------------------------------------------------------------------*
*&      f_format_var_td_check_micr
*----------------------------------------------------------------------*
*       Format the TD Canadian check MICR variables
*----------------------------------------------------------------------*
FORM f_format_var_td_check_micr
  TABLES   iit_in_tab    STRUCTURE itcsy
           cit_out_tab   STRUCTURE itcsy.

  CONSTANTS:
           lc_name_1     TYPE tdtprgname    "check number
                         VALUE 'VAR1',
           lc_name_2     TYPE tdtprgname    "bank routing nmbr CA part 1
                         VALUE 'VAR2',
           lc_name_3     TYPE tdtprgname    "bank routing nmbr CA part 2
                         VALUE 'VAR3',
           lc_name_4     TYPE tdtprgname    "bank routing nmbr US
                         VALUE 'VAR4',
           lc_name_5     TYPE tdtprgname    "bank account nmbr
                         VALUE 'VAR5'.

  DATA:    lv_subrc      TYPE sysubrc,
           lv_tabix      TYPE sytabix,
           lv_var1(10)   TYPE c,            "check number
           lv_var2(05)   TYPE c,            "bank routing nmbr CA part 1
           lv_var3(03)   TYPE c,            "bank routing nmbr CA part 2
           lv_var4(09)   TYPE c,            "bank routing nmbr US
           lv_var5(05)   TYPE c,            "bank account nmbr
           lv_char18(18) TYPE c.

  CLEAR    lv_var1.
  CLEAR    lv_var2.
  CLEAR    lv_var3.
  CLEAR    lv_var4.
  CLEAR    lv_var5.

* Process the check number
  CLEAR          iit_in_tab.
  READ     TABLE iit_in_tab INDEX 1.
  IF ( sy-subrc EQ 0 ).
    CLEAR                              lv_char18.
    MOVE     iit_in_tab-value       TO lv_char18.
    SHIFT    lv_char18           RIGHT DELETING TRAILING SPACE.
    TRANSLATE                          lv_char18 USING ' 0'.
    CLEAR                              lv_var1.
    MOVE     lv_char18+08(10)       TO lv_var1.
  ENDIF.

* Process the bank routing number
  CLEAR          iit_in_tab.
  READ     TABLE iit_in_tab INDEX 2.
  IF ( sy-subrc EQ 0 ).
    CLEAR                              lv_char18.
    MOVE     iit_in_tab-value       TO lv_char18.
    SHIFT    lv_char18           RIGHT DELETING TRAILING SPACE.
    TRANSLATE                          lv_char18 USING ' 0'.
    CLEAR                              lv_var2.
    MOVE     lv_char18+13(05)       TO lv_var2.
    CLEAR                              lv_var3.
    MOVE     lv_char18+10(03)       TO lv_var3.
    CLEAR                              lv_var4.
    MOVE     lv_char18+09(09)       TO lv_var4.
  ENDIF.

* Process the bank account number
  CLEAR          iit_in_tab.
  READ     TABLE iit_in_tab INDEX 3.
  IF ( sy-subrc EQ 0 ).
    CLEAR                              lv_char18.
    MOVE     iit_in_tab-value       TO lv_char18.
    SHIFT    lv_char18           RIGHT DELETING TRAILING SPACE.
    TRANSLATE                          lv_char18 USING ' 0'.
    CLEAR                              lv_var5.
    MOVE     lv_char18+13(05)       TO lv_var5.
  ENDIF.

* Assign the output variables to the output parameter table

* Assign the check number
  CLEAR          cit_out_tab.
  READ     TABLE cit_out_tab  WITH KEY name = lc_name_1.
  lv_subrc = sy-subrc.
  lv_tabix = sy-tabix.
  IF ( lv_subrc EQ 0 ).
    CLEAR                              cit_out_tab-value.
    MOVE     lv_var1                TO cit_out_tab-value.
    MODIFY                             cit_out_tab
                                 INDEX lv_tabix.
  ENDIF.

* Assign the bank routing number CA part 1
  CLEAR          cit_out_tab.
  READ     TABLE cit_out_tab  WITH KEY name = lc_name_2.
  lv_subrc = sy-subrc.
  lv_tabix = sy-tabix.
  IF ( lv_subrc EQ 0 ).
    CLEAR                              cit_out_tab-value.
    MOVE     lv_var2                TO cit_out_tab-value.
    MODIFY                             cit_out_tab
                                 INDEX lv_tabix.
  ENDIF.

* Assign the bank routing number CA part 2
  CLEAR          cit_out_tab.
  READ     TABLE cit_out_tab  WITH KEY name = lc_name_3.
  lv_subrc = sy-subrc.
  lv_tabix = sy-tabix.
  IF ( lv_subrc EQ 0 ).
    CLEAR                              cit_out_tab-value.
    MOVE     lv_var3                TO cit_out_tab-value.
    MODIFY                             cit_out_tab
                                 INDEX lv_tabix.
  ENDIF.

* Assign the bank routing number US
  CLEAR          cit_out_tab.
  READ     TABLE cit_out_tab  WITH KEY name = lc_name_4.
  lv_subrc = sy-subrc.
  lv_tabix = sy-tabix.
  IF ( lv_subrc EQ 0 ).
    CLEAR                              cit_out_tab-value.
    MOVE     lv_var4                TO cit_out_tab-value.
    MODIFY                             cit_out_tab
                                 INDEX lv_tabix.
  ENDIF.

* Assign the bank account number
  CLEAR          cit_out_tab.
  READ     TABLE cit_out_tab  WITH KEY name = lc_name_5.
  lv_subrc = sy-subrc.
  lv_tabix = sy-tabix.
  IF ( lv_subrc EQ 0 ).
    CLEAR                              cit_out_tab-value.
    MOVE     lv_var5                TO cit_out_tab-value.
    MODIFY                             cit_out_tab
                                 INDEX lv_tabix.
  ENDIF.

  CLEAR    iit_in_tab.
  CLEAR    cit_out_tab.

ENDFORM.                    " f_format_var_td_check_micr
