*****************************************************************
*****************************************************************
REPORT ztax_val_reconn MESSAGE-ID ztax. "LINE-SIZE 255.
* Validate tax charged/accrued/not calculated on AP invoices
*
* Programmer: Dietmar Hinz, TaxAutomation Inc.
* Date written: 05/05/2003
*
* Modification log
* Mod#       Date     User            Desc.:
* 0          05/06/03 Dietmar Hinz    Start development
* D81K925014 05/03/07 Kushal Thakore  Fixed BSET lookup
* 0          04/26/10 Steve Baciak    Changed the following
*            1. created internal table ztax_validate1 replaces
*               external table ztax_validate
*            2. created INCLUDE zsjb_validation_top - data definitions
*                               zsjb_validation_class - Class related processing
*                               zsjb_validation_build_tax_cf01 - All forms for processing
*                               zsjb_validation_alv_fieldcaf01 - Only ALV Field catalog definition
*            3. Exchange rate displayd on ALV - eliminate '/' prefix
*            4. Add comments through out program and delete old commented out code (some)
*            5. Replace do loops with replace all statement - four places
*            6. In FORM additional_fields add join to EKKO to pull BUKRS to use partial key
*               in BKPF select also on line 631 in include zsjb_validation_build_tax_cf01
*            7. Eliminate PERFORM determine_prod_code in FORM recalculate_tax.
*            8. Add F4 help to out put file on selection screen.
*            9. Added AB03 asset document to menu and hot spot.
*           10. Changed block from 1 to A1 and 2 to B2 - extended pgm check fix
*           11. Added type any to passing parms in forms
*           12. Unicode error with csks-kosar changed to csks_kosar in form determine_kosar
*           13. Added message 'No value for Navigation.' to hotspot and menu
*           When installing at client site - if text element added it would eliminate
*           code inspector error - string will not translate
*           05/03/10  SBaciak   added ADRN2 code
*****************************************************************
********-------->>>>> Instructions for implementing
* Required for installation at customer site.
* Texts required

*Selection Texts:
*BELNR-Document Number
*BELNRI-Invoice doc. number
*BLART1-Document type
*BUDAT1-Posting Date
*BUKRS-Company Code
*CPUDT-Entry date
*EBELN-Purchasing Document
*FILE_PC-Output File on PC
*FILE_SRV-Input File on Server
*GJAHR-Fiscal Year
*GRIR-GR/IR Account
*KOSTL-Cost Center
*LIFNR-Vendor
*MATKL-Material group
*MATNR-Material
*MWSKZ-Tax Code
*MWSKZ_NT-Non-taxable tax code
*O_FILE-Output File Name
*REGIO-Region
*SAKNR-G/L account no.
*USNAM-User name
*WAERS-Currency
*WERKS-Plant

*Text Elements: - Can cut and paste
*050-Document selections
*051-File Output
*052-PC File Transfer
*100-Display FI Document
*200-Display LIV invoice Document
*300-Display Purchase Order
*400-Display Vendor Master Record
*500-Display Cost Center
*600-Display GL Account
*700-Display Asset

*Messages: - Using SE91 create message class "ZTAX" and create the following messages
*001-Please specify a goods-issue date range
*002-& &
*004-Please specify either a cost center or internal order
*005-Please specify an order type
*006-Please specify an order reason
*100-Please specify a posting date range
*101-Error displaying document image
*102-Please specify either a company code or a posting date range
*103-Error opening file & on server
*104-Error writing file & on PC
*105-File & created on PC
*106-To execute a file transfer, specify both the input and output file name
*396-Account & & does not appear to be a tax liability account. Please check!
*397-Unable to connect to Taxware Database "& - Note change Taxware to correct tax app
*398-No records found in Taxware or SAP    "& - Note change Taxware to correct tax app
*399-RFC destination not maintained in table TTXD for tax procedure &

* Includes needed - Find include in pgm and double click to create object - upload code from text file
** DATA DEFINITIONS
*INCLUDE ztax_validation_top.
** all class definition and processing.
*INCLUDE ztax_validation_class.
** all forms
*INCLUDE ztax_validation_build_tax_cf01.
** field cat definition only
*INCLUDE ztax_validation_alv_fieldcaf01.

* GUI status needed - find the "SET" lines in the code.
* SET PF-STATUS '100'. ( in include ZTAX_VALIDATION_BUILD_TAX_CF01)
* Create normal screen
* enter "DMA: AP tax validation report" in short text
* enter "BACK" for green arrow
* enter "EXIT" for yellow arrow
* enter "EXIT" for red X
* under "Function Keys" menu
* GUI title
* SET TITLEBAR '100'. ( in include ZTAX_VALIDATION_BUILD_TAX_CF01) - Double click the 100
* If avaialble upload screen from text file else.
* Enter "DMA: AP tax validation report" in title
* Create screen 100 - Find "call screen 100" in program - Double click the 100
*     ( in include ZTAX_VALIDATION_BUILD_TAX_CF01)
* Double click the 100 and choose "Create Object" and goto "Layout"
* create ALV control by choosing the second from the last box/option on the left of the screen
* Enter "ZTAX_VALIDATE_CONT1" for the name

** To output all columns when generating spool request in batch, select printer with device type SWIN or PDF1
** Those printers have an output format X_65_1024/4, which allows for output of 1024 columns

*
* *DATA DEFINITIONS
INCLUDE ztax_val_recon_top.

* Selection screen

SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE text-050."#EC *

SELECT-OPTIONS bukrs FOR bkpf-bukrs.
SELECT-OPTIONS vbund FOR bseg-vbund.
SELECT-OPTIONS bbukrs FOR bkpf-bukrs.
SELECT-OPTIONS belnr FOR bkpf-belnr.
SELECT-OPTIONS gjahr FOR bkpf-gjahr.
SELECT-OPTIONS blart1 FOR bkpf-blart.
SELECT-OPTIONS belnri FOR rbkp-belnr.
SELECT-OPTIONS ebeln FOR ekpo-ebeln.
SELECT-OPTIONS budat1 FOR bkpf-budat.
SELECT-OPTIONS cpudt FOR bkpf-cpudt.
SELECT-OPTIONS saknr FOR bsak-saknr.
SELECT-OPTIONS kostl FOR bsak-kostl.
SELECT-OPTIONS prctr FOR bseg-prctr.
SELECT-OPTIONS lifnr FOR bsak-lifnr.
SELECT-OPTIONS werks FOR rseg-werks.
SELECT-OPTIONS regio FOR lfa1-regio.
SELECT-OPTIONS matnr FOR ekpo-matnr.
SELECT-OPTIONS matkl FOR ekpo-matkl.
SELECT-OPTIONS mwskz FOR bsak-mwskz.
SELECT-OPTIONS mwskz_nt FOR bsak-mwskz NO-DISPLAY.
SELECT-OPTIONS waers FOR bkpf-waers.
SELECT-OPTIONS usnam FOR bkpf-usnam.
SELECT-OPTIONS grir FOR bsak-saknr NO-DISPLAY.
*ARAMETERS: grir LIKE bseg-saknr DEFAULT '200040'.
*parameters: tolov like bseg-wrbtr default 0.
*parameters: tolun like bseg-wrbtr default 0.
*parameters: mantax like bseg-saknr default '2070020'.
SELECTION-SCREEN END OF BLOCK a1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-051.
PARAMETERS: o_file LIKE rlgrap-filename LOWER CASE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 33(56) text-099.
SELECTION-SCREEN END OF LINE.
*PARAMETERS: blart like bkpf-blart default 'SB'.
*PARAMETERS: budat like bkpf-budat default sy-datum.
*PARAMETERS: bldat like bkpf-bldat default sy-datum.
*PARAMETERS: bschl1 like bseg-bschl default '50'.
*PARAMETERS: bschl2 like bseg-bschl default '40'.
*parameters: mwskz1 like bseg-mwskz default 'U1'.
*parameters: mwskz2 like bseg-mwskz default 'I0'.
*PARAMETERS: bktxt like bkpf-bktxt default 'Tax Accrual Posting'.
SELECTION-SCREEN END OF BLOCK b2.
SELECTION-SCREEN BEGIN OF BLOCK 3 WITH FRAME TITLE text-052.
PARAMETERS: file_srv LIKE rlgrap-filename LOWER CASE.
PARAMETERS: file_pc LIKE rlgrap-filename LOWER CASE.
SELECTION-SCREEN END OF BLOCK 3.
*
SELECTION-SCREEN BEGIN OF BLOCK 4 WITH FRAME TITLE text-053.
PARAMETERS: recon as checkbox.
SELECTION-SCREEN END OF BLOCK 4.
*SELECTION-SCREEN BEGIN OF BLOCK 3 with frame.
*SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN COMMENT 1(26) TEXT-052 FOR FIELD test.
*SELECTION-SCREEN POSITION POS_LOW.
*PARAMETERS: test AS CHECKBOX default 'X'.
*SELECTION-SCREEN END OF LINE.
*SELECTION-SCREEN END OF BLOCK 3.
SELECT-OPTIONS:s_bukrs FOR bkpf-bukrs NO-DISPLAY,
               s_belnr FOR bkpf-belnr NO-DISPLAY,
               s_gjahr FOR bkpf-gjahr NO-DISPLAY,
               s_blart FOR bkpf-blart NO-DISPLAY,
               s_budat FOR bkpf-budat NO-DISPLAY.


* all class definition and processing.
INCLUDE ztax_val_recon_class.

* all forms
INCLUDE ztax_val_recon_build_tax_cf01.

* field cat definition only
INCLUDE ztax_val_recon_alv_fieldcaf01.

************************************************************************
*                   INITIALIZATION                                     *
************************************************************************
INITIALIZATION.

* Build tables of exempt and non-exempt tax codes
  PERFORM initialization.

*----------------------------------------------------------------------*
* at selection-screen
*----------------------------------------------------------------------*
* Ensure that a date has been entered
AT SELECTION-SCREEN.
  IF file_srv IS INITIAL AND
     file_pc IS INITIAL.
    DESCRIBE TABLE budat1 LINES lin.
    DESCRIBE TABLE bukrs LINES lin1.
    DESCRIBE TABLE belnr LINES lin2.

    IF lin2 IS INITIAL.
      IF lin IS INITIAL.
        MESSAGE e100.
      ENDIF.
    ENDIF.

    IF NOT lin2 IS INITIAL AND
       lin1 IS INITIAL AND
       lin IS INITIAL.
      MESSAGE e102.
    ENDIF.

    IF lin1 IS INITIAL.
      MESSAGE e107.
    ENDIF.
  ENDIF.

  IF ( file_srv IS INITIAL AND
       NOT file_pc IS INITIAL ) OR
     ( NOT file_srv IS INITIAL AND
       file_pc IS INITIAL ).
    MESSAGE e106.
  ENDIF.

  SELECT * FROM t001
         WHERE bukrs IN bukrs.
    AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
                ID 'BUKRS' FIELD t001-bukrs
                ID 'ACTVT' FIELD '03'.
    if sy-subrc ne 0.
      message e002 with 'No authorization for company code ' t001-bukrs.
    endif.
  ENDSELECT.

  if not recon is initial.
*Begin of change by somsx
*    select * from t001 where bukrs in bukrs.  "somsx
*    endselect.
*    if sy-dbcnt ne 1.
*      message e109.
*    endif.
*End of change by somsx
    describe table saknr lines lin3.
    if lin3 is initial.
      message e108.
    endif.
  endif.

* change prefix in function module in the form below .. SAP directory prefix
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR o_file.
*  PERFORM get_pc_filename USING o_file.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR file_pc.
  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    EXPORTING
      mask      = '*'
      static    = 'X'
    CHANGING
      file_name = file_pc.


************************************************************************
*                   Top Of Page                                        *
************************************************************************


************************************************************************
*                    START-OF-SELECTION                                *
************************************************************************
START-OF-SELECTION.

* File Transfer only
  IF NOT file_srv IS INITIAL AND
     NOT file_pc IS INITIAL.
    PERFORM file_transfer.
    STOP.
  ENDIF.


* Get the Country Reference of the Tax Jurisdiction Code
  SELECT SINGLE * FROM ttxd
         WHERE kalsm = kalsm.

** Need to convert state to numeric Vertex format
  IF ttxd-xextn = 'V'.  "Vertex
    PERFORM convert_regio.
  ELSEIF ttxd-xextn = space. "Internal SAP jurisdictions / adjust for each client
    PERFORM convert_regio_internal.
  ENDIF.

**Populate table with GR/IR accounts
  REFRESH grir.
  SELECT SINGLE ktopl INTO ktopl FROM t001 WHERE bukrs IN bukrs.
  SELECT konts INTO grir-low FROM t030
         WHERE ktopl = ktopl AND
               ktosl = 'WRX'.
    grir-sign = 'I'.
    grir-option = 'EQ'.
    APPEND grir.
  ENDSELECT.



* get the data and populate FIH and FIP
* get invoice receipts
  PERFORM main_select.

  DESCRIBE TABLE item LINES lin.
  IF lin IS INITIAL.
    WRITE: / 'No invoice documents found for selection'.
    SKIP.
    WRITE: / 'END OF REPORT'.
    STOP.
  ELSE.

    PERFORM add_tax_and_fields.

    PERFORM output.
  ENDIF.

END-OF-SELECTION.
