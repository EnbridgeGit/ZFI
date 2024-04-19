************************************************************************
* Report         : ZFFII017B
* Author         : Joanne Harwood
* Create Date    : 2007/11/26
* Description    : FI/GL: Hyperion Interface - Trial Balance Extract
* Category       : Extract & Report
* Modification#  : Issue TR495
* Supporting Doc : ZFFII017 Development Specification Document.doc
* Spec. Author   : Andy Tattersall
*----------------------------------------------------------------------*
* Change History
*----------------------------------------------------------------------*
* Chng.Req#  |Date       |Developer       |Description
*----------------------------------------------------------------------*
* D30K913809 |2007/11/23 |Joanne Harwood  |Issue TR495
*
*----------------------------------------------------------------------*
*  Description
*  -----------
*  NOTE: This is a copy of ZFFII017 that has been modified for the
*        Hyperion/SAP Interface....see specification for details
*        - Additional Code commented with 'TR495'
*        - Code has been modulized and nested SELECT statements
*          removed, specifically nested GLT0 statements
*
*  Details from ZFFII017
*  ---------------------
*  This program will download G/L YTD Trial Balances into a file for
*  upload into Hyperion system for consolidation purpose. This program
*  can be run to produce a second file for IFRS using a different
*  variant. Both files will have the exact same format, but content will
*  be based on Ledger used.
*
*  File Header and Journal Headers will be part of each detail record.
*  A Journal Header will be created for each company code.
*
*  - All accounts from table FAGLFLEXT that match the selection criteria
*    will be downloaded to file even if no info exist in ZACCT table.
*    When no account info is found in ZACCT table, the record is
*    created with default values.
*
*  - If SAP company code doesn't exist in ZJHDR table, don't process
*    any account under this company and create exception record for
*    report printing.
*
*  - If SAP account Number is not found in ZACCT table then use the
*    default data fed through variants and create a record for the file
*    and create an exception record as well for printing in the report.
*
*  - Hard coded delimeter Hex '07' is used to separate the fields.
*
*  - This program will abend if file header total debit amount is not
*    equal to file header total credit amount.
*
*  - To keep the program consistent between EAST and WEST systems, the
*    program uses SY-HOST field to determine the system and in the
*    INITIALIZATION step HOST dependent fields are populated here,
*    for example report headings, file path, file number.
*
* Additions for ZFFI017B
* ----------------------
* 1) New Table ZPSACCT - mapping table to assign an Account Validation/
*                        Sign value to the Peoplesoft Account# in ZACCT
*                      - the field D_IC_ACCT will determine what
*                        sign-flipping or calculation rule should be
*                        applied to YTD Account Balance
*
*   ZPSACCT-D_IC_ACCT Values & Operation:
*----------------------------------------------------------------------
*   'A' - Not IC Acct - Sign Flipped - Acct Bal * -1
*
*   'B' - Not IC Acct - Sign Not Flipped - no change
*
*   'C' - Not IC Acct - Affiliate needed Sign Flipped - Acct Bal * -1
*                       Attach Affiliate from ZJHDR/ZACCT tables
*
*   'D' - Not IC Acct - Affiliate needed Sign not Flipped - no change
*                       Attach Affiliate from ZJHDR/ZACCT tables
*
*   'E' - IC Acct     - Not Balancing Sign Flipped - Acct Bal * -1
*                       Attach Affiliate from ZJHDR/ZACCT tables
*
*   'F' - IC Acct     - Not Balancing Sign not Flipped - no change
*                       Attach Affiliate from ZJHDR/ZACCT tables
*
*   'G' - IC Acct     - Sign Flipped - Acct Bal * -1
*                       Attach Affiliate from ZJHDR/ZACCT tables
*
*   'H' - IC Acct     - Sign Not Flipped - no change
*                       Attach Affiliate from ZJHDR/ZACCT tables
*
*   'X' - Exclude from Hyperion Extract - Exclude Amounts from file
*----------------------------------------------------------------------
* 2) Based on the Peoplesoft Account# in table ZPSACCT the code
*    performs the appropriate operation based on the rule assigned
*
* 3) File extract will be changed for Hyperion requirements
*
*    Header Line1 (Chars 1-7 = ACTUALS)
*    Header Line2 (Chars 1-2 = <Beginning Month>)
*    Header Line3 (Chars 1-2 = <Ending Month>)
*
*    Detail Line Format
*    Field 1 = Peoplesoft Business Unit (ZACCT-PSACT)
*    Field 2 = Account . Sub-Account (sub account is affiliate)
*    Field 3 = YTD Account Balance (w/leading sign if negative)
*
*    Note: field delimiters = comma
*
*----------------------------------------------------------------------*
*CHANGES:
*Date       TR#  BY         Description
*--------   ---  ---------  --------------------------------------------
*15-01-09   654  M Khan     Add program code to create Opening Balance
*                           (i.e., Carryforward Balance).
*2009/09/14 582  G Ymana    Changes made for the IFRS project. Coding
*                           was added to allow selection of new ledger
*                           and table GLT0 was replaced by FAGLFLEXT.
*                           Also, this program will be run twice to
*                           create two feeds: one for GAAP and one for
*                           IFRS. Run will depend on variant.
*2010/03/02 582  G Ymana    Report format changes
*2010/05/14 831  G Ymana    Modified file scenario based on ledger
*----------------------------------------------------------------------*

REPORT ZFFII017B NO STANDARD PAGE HEADING LINE-COUNT 65
                                          LINE-SIZE 120 MESSAGE-ID ZS.

INCLUDE ZFFII017B_TOP.
INCLUDE ZFFII017B_FORMS.

*----------------------------------------------------------------------*
*   I N I T I A L I Z A T I O N
*----------------------------------------------------------------------*
INITIALIZATION.
*   PERFORM SET_REPORT_HEADER.              "TR564
*   PERFORM SET_PATH_FILENAME.              "TR582

*----------------------------------------------------------------------*
*   A T    S E L E C T I O N - S C R E E N
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
   PERFORM VALID_PERIOD_CHECK.
   PERFORM SET_REPORT_HEADER.              "TR564

*----------------------------------------------------------------------*
*   S T A R T - O F - S E L E C T I O N
*----------------------------------------------------------------------*
START-OF-SELECTION.
   PERFORM SETUP_DATES.
   PERFORM GET_GL_DATA.
   PERFORM BUILD_EXTRACT.

*----------------------------------------------------------------------*
*   E N D - O F - S E L E C T I O N
*----------------------------------------------------------------------*
END-OF-SELECTION.
   PERFORM PRINT_REPORT.
   PERFORM CREATE_OUTPUT_FILE.

*----------------------------------------------------------------------*
*   T O P - O F - P A G E
*----------------------------------------------------------------------*
TOP-OF-PAGE.
   PERFORM WRITE_TOP_OF_PAGE.
