REPORT ZFAPI002 LINE-SIZE 132 LINE-COUNT 65
       MESSAGE-ID ZS.
* This program will read a file of employee data extracted from the
* Ceridian system and will reformat it in SAP batch input records to * *
* create or change employee vendors. The output file will be loaded into
* SAP using the RMIBKR00 load program.

* UPGRADE NOTES:  Please review during upgrade
* During an upgrade, check all code containing NO_INPUT_REC as well
* as the field lengths of vndrcrte & vndrchng.
*----------------------------------------------------------------------
* 2011/11/03 SAHMAD   update Cash Management Group to default value AP
* 2009/02/09 mdemeest TR580 - Extened BLFA1 from 1301 to 1318 bytes
* 2007/11/30 mdemeest TR423 - Rewrite with Ceridian file as source

* 2006/10/05 mdemeest TRxxx - Clerk id to be assigned to Employee
*                        vendors, now part of variant.
* 2006/05/08 mdemeest 4.7 Changed length for ZBLFA1, ZBKFBK, ZBLFB1
* 2006/03/27 gymana 4.7  Change to add lock date (ZBGR00-START = SYDATUM
*                        -1) and ZBLFA1-BEGRU must be '/////'
* 00/09/21 mdemeest 4.6B PSTL2 must be spaces rather than '//////////'
* 00/06/14 mdemeest 4.6B Expand vndrcrte & vndrchng from 768 to 1093
*                        to accomodate additional SAP fields in layouts
*                        Check all code containing NO_INPUT_REC
* 99/06/14 mdemeest 3.1I Expand vndrcrte & vndrchng from 750 to 768
* 99/06/04 mdemeest 3.1I Changed lengths for no_input_rec
*----------------------------------------------------------------------

TABLES: LFA1,                                    "vendor mast details
        LFBK,                                   "vendor mast banking
        ZFHCC.                                  "company code xref
*-------------------------------------------------------------------
* input file - contains all info to create/update a vendor
*-------------------------------------------------------------------
data: employee_rec(150)    type c.
DATA: BEGIN OF employee,
        EMPLID(6),
        COMPANY(3),
        NAME(35),
        ADDRESS(30),
        CITY(30),
        PROV(2),
        COUNTRY(3),
        POSTCODE(10),
        TRANSIT(11),
        ACCOUNT(17),
        EMAIL(40),
      END OF EMPLOYEE.


data:  begin of wa_xref   occurs 0,
         old_empl(7)   type c,
         new_empl(6)   type c,
         empl_name(30) type c,
       end of wa_xref.
*------------------------------------------------------------------
DATA: CRTE_SESS_CTR TYPE I,
      CHNG_SESS_CTR TYPE I,
      CONVRULE(2)   TYPE C   VALUE ' /',
      ADDR_UPD(1),
      BANK_UPD(1),
      NAME_UPD(1),
      TRAN_CODE(4),
      ACTION(3),
      EMPL_VNDR(10),
      NAME1(35),
      NAME2(35),
      NAME3(35),
      LAST_NAME(35),
      LNAME_INIT(1),
      STATUS_CD(1),
      STATUS_DESC(10),
      DAYS_MSG_FLG(1),
      DAYS_MSG(15),
      TRAN_LEN TYPE I.

*--------------------------------------------------------------
* session header record
DATA: BEGIN OF ZBGR00.
       INCLUDE STRUCTURE BGR00.
DATA: END OF ZBGR00.
* document header record
DATA: BEGIN OF ZBLF00.
       INCLUDE STRUCTURE BLF00.
DATA: END OF ZBLF00.
* employee vendor general data record
DATA: BEGIN OF ZBLFA1.
       INCLUDE STRUCTURE BLFA1.
DATA: END OF ZBLFA1.
* employee vendor banking data record
DATA: BEGIN OF ZBLFBK.
        INCLUDE STRUCTURE BLFBK.
DATA: END OF ZBLFBK.
* employee vendor company code record
DATA: BEGIN OF ZBLFB1.
        INCLUDE STRUCTURE BLFB1.
DATA: END OF ZBLFB1.
* employee vendor dunning data record
DATA: BEGIN OF ZBLFB5.
        INCLUDE STRUCTURE BLFB5.
DATA: END OF ZBLFB5.
* employee vendor purchasing data record
DATA: BEGIN OF ZBLFM1.
        INCLUDE STRUCTURE BLFM1.
DATA: END OF ZBLFM1.
*-------------------------------------------------------------
DATA: NO_INPUT_REC(1500).                   "4.6B 2000/06/19 mdemeest
*parameters: hrisfile like filenameci-fileintern
*            default 'Z_IFAP007_01'.
DATA: EMPLDATA(191) TYPE C,
      VNDRCRTE(1092) TYPE C,
      VNDRCHNG(1092) TYPE C.

* VARIANT - INPUT SCREEN -----------------------------------------------
PARAMETERS:
           INBOUND LIKE FILENAME-FILEINTERN,
           p_xref    like filename-fileextern default
                     '/usr/sap/interfaces/P01/IFAP007/emplxref.dat',
           OUTFILE1 LIKE FILENAME-FILEINTERN,
           OUTFILE2 LIKE FILENAME-FILEINTERN,
           p_zwels    like zblfb1-zwels  obligatory.
selection-screen  skip.
parameter: p_busab  like zblfb1-busab obligatory."Clerk Assigned to Empl

*-----------------------------------------------------------------
* There is one input file from Ceridian.
* There are 2 output files: the first will contain all new vendors to be
* created (VNDRCRTE); the second will contain all the vendors to be
* updated (VNDRCHNG).
*----------------------------------------------------------------------

DATA MSG_TEXT(50).
CALL FUNCTION 'FILE_GET_NAME'
     EXPORTING
          CLIENT           = SY-MANDT
          LOGICAL_FILENAME = INBOUND
          OPERATING_SYSTEM = SY-OPSYS
     IMPORTING
          FILE_NAME = EMPLDATA
     EXCEPTIONS
          FILE_NOT_FOUND   = 01.
     IF SY-SUBRC <> 0.
       IF SY-INDEX = 1.
         MESSAGE E006 WITH INBOUND.
       ENDIF.
     ENDIF.
CALL FUNCTION 'FILE_GET_NAME'
     EXPORTING
          CLIENT           = SY-MANDT
          LOGICAL_FILENAME = OUTFILE1
          OPERATING_SYSTEM = SY-OPSYS
     IMPORTING
          FILE_NAME = VNDRCRTE
     EXCEPTIONS
          FILE_NOT_FOUND   = 01.
     IF SY-SUBRC <> 0.
       IF SY-INDEX = 1.
         MESSAGE E006 WITH OUTFILE1.
       ENDIF.
     ENDIF.
CALL FUNCTION 'FILE_GET_NAME'
     EXPORTING
          CLIENT           = SY-MANDT
          LOGICAL_FILENAME = OUTFILE2
          OPERATING_SYSTEM = SY-OPSYS
     IMPORTING
          FILE_NAME = VNDRCHNG
     EXCEPTIONS
          FILE_NOT_FOUND   = 01.
     IF SY-SUBRC <> 0.
       IF SY-INDEX = 1.
         MESSAGE E006 WITH OUTFILE2.
       ENDIF.
     ENDIF.

OPEN DATASET EMPLDATA FOR INPUT IN TEXT MODE MESSAGE MSG_TEXT.
IF SY-SUBRC NE 0.
 WRITE:/ 'Employee file cannot be opened. Reason:', MSG_TEXT.
 EXIT.
ENDIF.

OPEN DATASET VNDRCRTE FOR OUTPUT IN TEXT MODE MESSAGE MSG_TEXT.
IF SY-SUBRC NE 0.
 WRITE:/ 'Vendor create file cannot be opened. Reason:', MSG_TEXT.
 EXIT.
ENDIF.

OPEN DATASET VNDRCHNG FOR OUTPUT IN TEXT MODE MESSAGE MSG_TEXT.
IF SY-SUBRC NE 0.
 WRITE:/ 'Vendor change file cannot be opened. Reason:', MSG_TEXT.
 EXIT.
ENDIF.

*--------------------------------------------------------------------

if p_xref = space.
else.
   open dataset p_xref for input in text mode.
   perform xref_read.
   sort wa_xref by new_empl.
endif.



*-----------------------------------------------------------------
MOVE 5000 TO CRTE_SESS_CTR.
MOVE 5000 TO CHNG_SESS_CTR.
MOVE SPACE TO NO_INPUT_REC.
TRANSLATE NO_INPUT_REC USING CONVRULE.

data: wafirstname(15) type c.
data: walastname(15)  type c.
data: wapost1_3(3)    type c.
data: wapost4_6(3)    type c.
data: watran1(4)      type c.
data: watran2(5)      type c.

* loop throught the file of employee data splitting the record
* into fields and then because the vendor sending the data is
* using formats different from the formats required by SAP,
* adjusting the data within the field. (ie name, postal code and
* bank info
DO.
  READ DATASET EMPLDATA INTO EMPLOYEE_REC.                  "TRxxx
  IF SY-SUBRC NE 0.                "Exit loop at end of file or error
     EXIT.
  endif.

  clear: wafirstname, walastname, wapost1_3, wapost4_6, watran1,
                                                        watran2.
  split employee_rec at '|' into
        employee-emplid  employee-company
        employee-name    employee-address
        employee-city    employee-prov
        employee-country employee-postcode
        employee-transit employee-account
        employee-email.
  split employee-name at ',' into walastname wafirstname.
  clear employee-name.
  shift wafirstname left deleting leading space.
  concatenate wafirstname walastname into employee-name
               separated by space.
  move employee-postcode+0(3)  to wapost1_3.
  move employee-postcode+3(3)  to wapost4_6.
  clear employee-postcode.
  concatenate wapost1_3 wapost4_6 into employee-postcode
               separated by space.
*  split employee-transit at ', ' into watran1 watran2.
*  clear employee-transit.
*  concatenate watran1 watran2 into employee-transit.

*---------------------------------------------------------------------
* Check the vendor master table to see if the employee already exists.
* if the vendor does not exist, we will add a new vendor entry.
* if the vendor exists, check name and address data for changes.
*---------------------------------------------------------------------
  MOVE SPACE       TO ACTION.
  MOVE SPACE       TO TRAN_CODE.
  CONCATENATE '0000' EMPLOYEE-EMPLID INTO EMPL_VNDR.
  SELECT SINGLE * FROM LFA1 WHERE LIFNR = EMPL_VNDR.
    if sy-subrc = '0'.
     move 'CHG'    to action.
     move 'XK02'   to tran_code.
  else.
     move 'ADD'    to action.
     move 'XK01'   to tran_code.
  endif.
*---------------------------------------------------------------------
* If the employee record does not have a valid Company Code as listed
* in table ZFHCC, then 'IGNore' the employee by overwriting the action
* field with 'IGN' for IGNORE.  All sections in the abap are executed
* based on the action value
*---------------------------------------------------------------------
  select single * from ZFHCC
    where hbukrs = employee-company.
  if sy-subrc = '0'.
  else.
    move 'IGN'   to action.
    clear tran_code.
    write: /1 employee-emplid, ' has invalid company code ',
                                            employee-company.
  endif.
*---------------------------------------------------------------------
* Determine if name or address need to be updated.
*---------------------------------------------------------------------
  MOVE 'N'   TO ADDR_UPD.
  MOVE 'N'   TO NAME_UPD.
  MOVE 'N'   TO DAYS_MSG_FLG.

  IF LFA1-STRAS NE EMPLOYEE-ADDRESS AND ACTION EQ 'CHG'.
     MOVE 'y' TO ADDR_UPD.
  ENDIF.
  IF LFA1-ORT01 NE EMPLOYEE-CITY AND ACTION EQ 'CHG'.
     MOVE 'y' TO ADDR_UPD.
  ENDIF.
  IF LFA1-NAME1 NE EMPLOYEE-NAME AND ACTION EQ 'CHG'.
     MOVE 'y' TO NAME_UPD.
  ENDIF.
*----------------------------------------------------------------------
* if a new vendor has been found or a name or address data
* change is required, the appropriate records will be formatted to
* create SAP vendor create or change transactions.
*
* a new batch input session will be created for every 5000 documents.
*----------------------------------------------------------------------
  IF ACTION = 'ADD'.
    IF CRTE_SESS_CTR = 5000.
      perform create_batch_header.
      MOVE 'ZVNDR LD EMP' TO ZBGR00-GROUP.
      MOVE 1 TO CRTE_SESS_CTR.
      TRANSFER ZBGR00     TO VNDRCRTE.
      IF SY-SUBRC NE 0.
        WRITE:/ 'File cannot be transferred. Reason:', MSG_TEXT.
        EXIT.
      ENDIF.
    ELSE.
      ADD 1 TO CRTE_SESS_CTR.
    ENDIF.
  ENDIF.

  IF ACTION = 'CHG' AND ( ADDR_UPD = 'y' OR NAME_UPD = 'y' ).
    IF CHNG_SESS_CTR = 5000.
      perform create_batch_header.
      MOVE 'ZVNDR CH EMP' TO ZBGR00-GROUP.
      MOVE 1 TO CHNG_SESS_CTR.
      TRANSFER ZBGR00 TO VNDRCHNG.
      IF SY-SUBRC NE 0.
        WRITE:/ 'File cannot be transferred. Reason:', MSG_TEXT.
        EXIT.
      ENDIF.
    ELSE.
      ADD 1 TO CHNG_SESS_CTR.
    ENDIF.
  ENDIF.
*-------------------------------------------------------------------
* a document header is created for each add or change.
*-------------------------------------------------------------------
  IF ACTION = 'ADD' OR
    ( ACTION = 'CHG' AND ( ADDR_UPD = 'y' OR NAME_UPD = 'y' ) ).
    MOVE NO_INPUT_REC+0(43) TO ZBLF00.
    MOVE '1'                TO ZBLF00-STYPE.
    MOVE TRAN_CODE          TO ZBLF00-TCODE.
    MOVE EMPL_VNDR          TO ZBLF00-LIFNR.
    SELECT single * FROM ZFHCC WHERE HBUKRS = EMPLOYEE-COMPANY.
    if sy-subrc = '0'.
       MOVE ZFHCC-SBUKRS    TO ZBLF00-BUKRS.
    endif.
    MOVE 'EMPL' TO ZBLF00-KTOKK.
    IF ACTION EQ 'ADD'.
      TRANSFER ZBLF00 TO VNDRCRTE.
    ELSEIF ACTION EQ 'CHG'.
      TRANSFER ZBLF00 TO VNDRCHNG.
    ENDIF.
    IF SY-SUBRC NE 0.
      WRITE:/ 'File cannot be transferred. Reason:', MSG_TEXT.
      EXIT.
    ENDIF.
  ENDIF.
*------------------------------------------------------------------
* a general data record is created for each new vendor and for
* name or address changes.
*------------------------------------------------------------------
  IF ACTION = 'ADD' OR
     ( ACTION = 'CHG' AND ( ADDR_UPD = 'y' OR NAME_UPD = 'y' ) ).
    MOVE NO_INPUT_REC+0(1318) TO ZBLFA1.        "6.0 upgrade
    MOVE '2' TO ZBLFA1-STYPE.
    MOVE 'BLFA1' TO ZBLFA1-TBNAM.
    MOVE EMPLOYEE-NAME TO ZBLFA1-NAME1.
    NAME1 = NAME2 = NAME3 = LAST_NAME = SPACE.
    SPLIT EMPLOYEE-NAME AT '. ' INTO NAME1 NAME2 NAME3.
    IF NAME3 > SPACE.
      MOVE NAME3 TO LAST_NAME.
      MOVE LAST_NAME+0(1) TO LNAME_INIT.
    ELSE.
      MOVE NAME2 TO LAST_NAME.
      MOVE LAST_NAME+0(1) TO LNAME_INIT.
    ENDIF.
    MOVE walastname+0(10) TO ZBLFA1-SORTL.
    MOVE EMPLOYEE-ADDRESS TO ZBLFA1-STRAS.
    MOVE EMPLOYEE-CITY TO ZBLFA1-ORT01.
    MOVE EMPLOYEE-POSTCODE TO ZBLFA1-PSTLZ.
    MOVE EMPLOYEE-PROV TO ZBLFA1-REGIO.
    move space         to zblfa1-pstl2.
    IF EMPLOYEE-COUNTRY = 'CAN'.
      MOVE 'CA' TO ZBLFA1-LAND1.
    ELSE.
      MOVE 'US' TO ZBLFA1-LAND1.
    ENDIF.
    MOVE 'E' TO ZBLFA1-SPRAS.
    MOVE 'I0' TO ZBLFA1-STCD1.
    MOVE 'EMPLOYEE' TO ZBLFA1-KONZS.
    MOVE 'X' TO ZBLFA1-XZEMP.
    IF ACTION EQ 'ADD'.
      TRANSFER ZBLFA1 TO VNDRCRTE.
    ELSEIF ACTION EQ 'CHG'.
      TRANSFER ZBLFA1 TO VNDRCHNG.
    ENDIF.
    IF SY-SUBRC NE 0.
      WRITE:/ 'File cannot be transferred. Reason:', MSG_TEXT.
      EXIT.
    ENDIF.
  ENDIF.
*----------------------------------------------------------------
* a bank data record is only created for all new employee vendors.
*-----------------------------------------------------------------
  IF ACTION = 'ADD'.
    MOVE NO_INPUT_REC+0(392) TO ZBLFBK.
    MOVE '2' TO ZBLFBK-STYPE.
    MOVE 'BLFBK' TO ZBLFBK-TBNAM.
    MOVE 0 TO TRAN_LEN.
    TRAN_LEN = STRLEN( EMPLOYEE-TRANSIT ).
    IF TRAN_LEN = 9.
      MOVE EMPLOYEE-TRANSIT TO ZBLFBK-BANKL.
    ELSE.
      CONCATENATE '0' EMPLOYEE-TRANSIT INTO ZBLFBK-BANKL.
    ENDIF.
    MOVE EMPLOYEE-ACCOUNT TO ZBLFBK-BANKN.
*----------------------------------------------------------------------
*  This code is only for the initial Ceridian load.  This code can be
*  removed after January 31,2008
   if p_xref <> space.
      perform get_existing_banking_info.
   endif.
*  end of initial Ceridican load --------------------------------------
    MOVE 'CA' TO ZBLFBK-BANKS.
    TRANSFER ZBLFBK TO VNDRCRTE.
    IF SY-SUBRC NE 0.
      WRITE:/ 'File cannot be transferred. Reason:', MSG_TEXT.
      EXIT.
    ENDIF.
  ENDIF.
*--------------------------------------------------------------------
* a company code data record is created for all new employee vendors.
*---------------------------------------------------------------------
  IF ACTION = 'ADD'.
    MOVE NO_INPUT_REC+0(486) TO ZBLFB1.
    move '2'                 TO ZBLFB1-STYPE.
    MOVE 'BLFB1'             TO ZBLFB1-TBNAM.
    MOVE '251000'            TO ZBLFB1-AKONT.
    MOVE '009'               TO ZBLFB1-ZUAWA.
    MOVE 'N00'               TO ZBLFB1-ZTERM.
    MOVE 'X'                 TO ZBLFB1-REPRF.
    MOVE p_zwels             TO ZBLFB1-ZWELS.
    MOVE '01'                TO ZBLFB1-UZAWE.
    move employee-email      to ZBLFB1-INTAD.
*update Cash Management Group to default value #AP#
    MOVE 'AP'                TO ZBLFB1-FDGRV .

* The AP department has made a decision that all employee vendors will
* be assigned to clerk entered in the variant.
    MOVE p_busab             TO ZBLFB1-BUSAB.
*---------------------------------------------------------------------
* This code is only for the original Ceridian load - can be removed
* after 2008/01/31
    if p_xref <> space.
       perform get_cross_reference.
    endif.
* end of code from original Ceridian load
*---------------------------------------------------------------------
    TRANSFER ZBLFB1          TO VNDRCRTE.
    IF SY-SUBRC NE 0.
      WRITE:/ 'File cannot be transferred. Reason:', MSG_TEXT.
      EXIT.
    ENDIF.
  ENDIF.
*----------------------------------------------------------------
* a dunning data record is created for all new employee vendors.
*----------------------------------------------------------------
  IF ACTION = 'ADD'.
    MOVE NO_INPUT_REC+0(65) TO ZBLFB5.
    MOVE '2' TO ZBLFB5-STYPE.
    MOVE 'BLFB5' TO ZBLFB5-TBNAM.
    TRANSFER ZBLFB5 TO VNDRCRTE.
    IF SY-SUBRC NE 0.
      WRITE:/ 'File cannot be transferred. Reason:', MSG_TEXT.
      EXIT.
    ENDIF.
  ENDIF.
*---------------------------------------------------------------
  IF ACTION = 'ADD'.
    WRITE: /3 ZBLF00-LIFNR, 13 ZBLF00-BUKRS, 20 ZBLFA1-NAME1,
        58 'New Employee Vendor'.
     WRITE: /.
  ENDIF.
  IF ACTION = 'CHG' AND NAME_UPD = 'y'.
    WRITE: /3 ZBLF00-LIFNR, 13 ZBLF00-BUKRS, 20 ZBLFA1-NAME1,
        58 'Employee Name Change'.
    WRITE: /19 'Old Name: ', 31 LFA1-NAME1.
    WRITE: /.
  ENDIF.
  IF ACTION = 'CHG' AND ADDR_UPD = 'y'.
    WRITE: /3 ZBLF00-LIFNR, 13 ZBLF00-BUKRS, 20 ZBLFA1-NAME1,
        58 'Employee Address Change'.
    WRITE: /15 'Old Address: ', 28 LFA1-STRAS, 65 LFA1-ORT01,
        102 LFA1-REGIO, 107 LFA1-LAND1, 115 LFA1-PSTLZ.
    WRITE: /15 'New Address: ', 28 ZBLFA1-STRAS, 65 ZBLFA1-ORT01,
        102 ZBLFA1-REGIO, 107 ZBLFA1-LAND1, 115 ZBLFA1-PSTLZ.
    WRITE: /.
  ENDIF.

ENDDO.
WRITE:/ 'PROGRAM END'.
CLOSE DATASET EMPLDATA.
CLOSE DATASET VNDRCRTE.
CLOSE DATASET VNDRCHNG.
*
TOP-OF-PAGE.
WRITE: / SY-DATUM COLOR COL_GROUP INTENSIFIED OFF INVERSE ON.
WRITE: 28 TEXT-100 COLOR COL_HEADING, 120 SY-REPID.

WRITE: / SY-UZEIT COLOR COL_GROUP INTENSIFIED OFF INVERSE ON.
WRITE: 51 'Employee Vendor Status Report'.
WRITE: 120 'PAGE:'.
WRITE: 129(3) SY-PAGNO COLOR COL_GROUP INTENSIFIED OFF INVERSE ON.

ULINE.
WRITE: /.
WRITE: /3 TEXT-101, 13 TEXT-102,
       22 TEXT-103, 62 TEXT-104.
ULINE.
WRITE: /.

form create_batch_header.
      MOVE NO_INPUT_REC+0(38) TO ZBGR00.
      MOVE '0' TO ZBGR00-STYPE.
      MOVE SY-MANDT TO ZBGR00-MANDT.
      ZBGR00-START = SY-DATUM - 1.
      MOVE 'BATCH' TO ZBGR00-USNAM.
endform.

form xref_read.
data:  inrec(300) type c.
  do.
    read dataset p_xref into inrec.
    if sy-subrc <> 0.
       exit.
    else.
       split inrec at ',' into wa_xref-old_empl
                               wa_xref-new_empl
                               wa_xref-empl_name.
       append wa_xref.
    endif.
  enddo.
  sort wa_xref by new_empl.
endform.
*-----------------------------------------------------------------------
* GET_CROSS_REFERENCE & GET_EXISTING_BANKING_INFO can be removed after
* Jan. 31/2008
form get_cross_reference.
   read table wa_xref with key new_empl = employee-emplid binary search.
   if sy-subrc = '0'.
      concatenate 'E' wa_xref-old_empl into zblfb1-altkn.
   endif.
endform.

form get_existing_banking_info.
data:  wa_old_empl  like lfbk-lifnr.
   read table wa_xref with key new_empl = employee-emplid binary search.
   if sy-subrc = '0'.
      concatenate 'E' wa_xref-old_empl into wa_old_empl.
      select single * from lfbk
        where lifnr = wa_xref.
      if sy-subrc = '0'.
         move lfbk-bankl to zblfbk-bankl.
         move lfbk-bankn to zblfbk-bankn.
      endif.
   endif.
endform.
*  end of code to be removed
*-----------------------------------------------------------------------
