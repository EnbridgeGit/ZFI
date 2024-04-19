REPORT ZFAPC001 LINE-SIZE 160.
************************************************************************
* Conversion program -
* Program to load bank records using transaction FI01.
* Program support create only - no changes supported.
* The intended use is a one time load prior to implementation
* which outputs failed trasnactions for manual update.
************************************************************************
* Input record format follows
DATA: BEGIN OF BANK_RECORD,
         INSTITUTION_ID(9),                "Transaction Bank Key
         INSTITUTION_NAME(36),             "Transaction Bank Field
         BRANCH_NAME(36),                  "Transaction Branch Field
         BRANCH_ADDRESS(35),               "Transaction Street Field
         GARBAGE1(1),                      "35 Chars in SAP so truncate
         BRANCH_TOWN(35),                  "Transaction City Field
         GARBAGE2(1),                      "35 Chars in SAP so truncate
     END OF BANK_RECORD.
DATA: BEGIN OF BDCDATA OCCURS 500.
         INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDCDATA.

* physical file name as translated from logical file name
DATA: PHYSFILE LIKE FILENAME-FILEINTERN.
* Number of records read from physical file.
DATA: NUMREC(6) TYPE N.
* Number of records processed OK.
DATA:NUMRECOK(6) TYPE N.

* Selection screen
PARAMETERS: FNAME LIKE FILENAME-FILEEXTERN DEFAULT 'ZCFAP001_09'.
PARAMETERS: TESTRUN AS CHECKBOX DEFAULT 'X'.


START-OF-SELECTION.
* Initialize
CLEAR NUMREC.
CLEAR NUMRECOK.

PERFORM OPEN_FILE.

DO.
   READ DATASET PHYSFILE INTO BANK_RECORD.
   IF SY-SUBRC <> 0.
      EXIT.
   ELSE.
      NUMREC = NUMREC + 1.
   ENDIF.
   IF TESTRUN <> 'X'.
      PERFORM POST_BANK_RECORD.                 "Perform Actual Posting
   ELSE.
      PERFORM REPORT_OF_FILE.                   "Output report of data
   ENDIF.
ENDDO.

PERFORM CLOSE_FILE.

************************************************************************
* Routine to create a bank record using Call Transaction
* Failed trasnactions are written to report for manual correction
************************************************************************
FORM POST_BANK_RECORD.
   REFRESH BDCDATA.
   PERFORM BDC_SCREEN USING 'SAPMF02B' '100'.
   PERFORM BDC_FIELD USING 'BNKA-BANKS' 'CA'.
   PERFORM BDC_FIELD USING 'BNKA-BANKL' BANK_RECORD-INSTITUTION_ID.
   PERFORM BDC_SCREEN USING 'SAPMF02B' '110'.
   PERFORM BDC_FIELD USING 'BNKA-BANKA' BANK_RECORD-INSTITUTION_NAME.
   PERFORM BDC_FIELD USING 'BNKA-STRAS' BANK_RECORD-BRANCH_ADDRESS.
   PERFORM BDC_FIELD USING 'BNKA-ORT01' BANK_RECORD-BRANCH_TOWN.
   PERFORM BDC_FIELD USING 'BNKA-BRNCH' BANK_RECORD-BRANCH_NAME.
   PERFORM BDC_FIELD USING 'BDC_OKCODE' '/11'.
   CALL TRANSACTION 'FI01' USING BDCDATA MODE 'N' UPDATE 'A'.
   IF SY-SUBRC <> 0.
      PERFORM REPORT_OF_FILE.
   ENDIF.
ENDFORM.
************************************************************************
* Routine to provide printed output of data.
* Used in test mode to produce load detail report and in execution
* (Non-test mode) to output details of failed transactions
************************************************************************
FORM REPORT_OF_FILE.
 WRITE: / BANK_RECORD-INSTITUTION_ID,
          BANK_RECORD-INSTITUTION_NAME,
          BANK_RECORD-BRANCH_NAME,
          BANK_RECORD-BRANCH_ADDRESS,
          BANK_RECORD-BRANCH_TOWN.
ENDFORM.
************************************************************************
* Routine to translate logical file to physical file and
* open the file for input in text mode.
* Program terminates within routine if open fails.
************************************************************************
FORM OPEN_FILE.

   CALL FUNCTION 'FILE_GET_NAME'
      EXPORTING
         LOGICAL_FILENAME        = FNAME
      IMPORTING
         FILE_NAME               = PHYSFILE
      EXCEPTIONS
         FILE_NOT_FOUND          = 1
         OTHERS                  = 2.
   IF SY-SUBRC <> 0.
      WRITE: / ' Unable to translate logical file name...Terminating'.
      EXIT.
   ENDIF.
   OPEN DATASET PHYSFILE FOR INPUT IN TEXT MODE.
   IF SY-SUBRC <> 0.
      WRITE: / ' Unable to open file ', PHYSFILE, ' ...Terminating'.
      EXIT.
   ENDIF.
ENDFORM.
************************************************************************
* Routine to close the input file. No error checking is performed
************************************************************************
FORM CLOSE_FILE.
   CLOSE DATASET PHYSFILE.
ENDFORM.


************************************************************************
*     FORM BDC_SCREEN
************************************************************************
*  Description:
*  - This routine adds an entry to the table BDCDATA with screen
*    information from a particular transaction.  This is used as part
*    of the process for creating data for batch input.
*
*  Parameters:
*      -->  PROGRAM - Program name of the screen
*           DNYPRO  - Screen number
************************************************************************
FORM BDC_SCREEN USING PROGRAM DYNPRO.

  CLEAR BDCDATA.
  BDCDATA-PROGRAM = PROGRAM.
  BDCDATA-DYNPRO = DYNPRO.
  BDCDATA-DYNBEGIN = 'X'.
  APPEND BDCDATA.

ENDFORM.
************************************************************************
*     FORM BDC_FIELD
************************************************************************
*  Description:
*  - This routine adds an entry to the table BDCDATA with field
*    information from a particular transaction.  This is used as part
*    of the process for creating data for batch input.
*
*  Parameters:
*      -->  FNAM - name of the field on the screen
*           FVAL - value to be entered for that field on the
*                  screen
************************************************************************
FORM BDC_FIELD USING FNAM FVAL.

  CLEAR BDCDATA.
  BDCDATA-FNAM = FNAM.
  BDCDATA-FVAL = FVAL.
  APPEND BDCDATA.

ENDFORM.
