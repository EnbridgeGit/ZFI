REPORT ZFAPC006 MESSAGE-ID ZS.

************************************************************************
*  CREATED BY: NANCY GILLIGAN, OMNILOGIC  11/98                        *
*  PURPOSE:  THIS PROGRAM COPIES INFORMATION ON ALL BANKS IN CLIENT    *
*            XX0 AND TRANSFERS THEM TO CLIENT XX1 VIA BDC SESSION.     *
*  REQUESTED BY: LAURIE, ISSUE LOG #
*  98/11/26 mdemeest flat file                                     *
************************************************************************

TABLES: BNKA.

data: bank_info(200).
data: outfile like filename-fileintern
       value '/usr/sap/interfaces/D30/UEC/IFFI0201/BANK.sap'.

*DATA  : BEGIN OF BDC_DATA OCCURS 100.
*          INCLUDE STRUCTURE BDCDATA.
*DATA  : END OF BDC_DATA.

*DATA  : G_TRANSCODE    LIKE TSTC-TCODE VALUE 'FI01'.


SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) TEXT-001.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
SELECT-OPTIONS:
    S_BANKL     FOR   BNKA-BANKL,   "BANK KEY
    S_BANKS     FOR   BNKA-BANKS NO INTERVALS.   "BANK COUNTR
SELECTION-SCREEN SKIP.
PARAMETERS:
     P_MANDT    LIKE SY-MANDT DEFAULT '021' OBLIGATORY,   "TARGET CLIENT
     p_sysid    like sy-sysid default 'D22' obligatory.    "platform
SELECTION-SCREEN END OF BLOCK BOX1.


*******************************  MAIN  *********************************
START-OF-SELECTION.


 move p_SYsid(3) to outfile+20(3).
 open dataset outfile for output in text mode.


*pERFORM OPEN_BDC.
  SELECT * FROM BNKA WHERE BANKL IN S_BANKL
                       AND BANKS IN S_BANKS.

    PERFORM PUT_VALUES.
*   PERFORM INSERT_SESSION USING G_TRANSCODE.
  ENDSELECT.    " SELECT FROM ZV_LFA1

  CLOSE DATASET OUTFILE.
* IF SY-SUBRC NE 0.
*   SKIP 2.
*   WRITE: TEXT-005.
* ELSE.
*   PERFORM CLOSE_BDC.
* ENDIF.

**************************** SUB ROUTINES ******************************
*---------------------------------------------------------------------*
*       FORM OPEN_BDC                                                 *
*---------------------------------------------------------------------*
*       This routine will attempt to open the BDC session             *
*---------------------------------------------------------------------*
FORM OPEN_BDC.
  CALL FUNCTION 'BDC_OPEN_GROUP'
       EXPORTING
           CLIENT              = P_MANDT
           GROUP               = 'ZFI_ZFAPC003'
*         HOLDDATE            =
           KEEP                = 'X'
           USER                = SY-UNAME
       EXCEPTIONS
            CLIENT_INVALID      = 1
            DESTINATION_INVALID = 2
            GROUP_INVALID       = 3
            GROUP_IS_LOCKED     = 4
            HOLDDATE_INVALID    = 5
            INTERNAL_ERROR      = 6
            QUEUE_ERROR         = 7
            RUNNING             = 8
            SYSTEM_LOCK_ERROR   = 9
            USER_INVALID        = 10
            OTHERS              = 11.

  IF SY-SUBRC NE 0.
    MESSAGE E004 WITH 'ZFI_ZFAPC003'.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM CLOSE_BDC                                                *
*---------------------------------------------------------------------*
*   - This routine will attempt to close the BDC session              *
*---------------------------------------------------------------------*
FORM CLOSE_BDC.
  CALL FUNCTION 'BDC_CLOSE_GROUP'
       EXCEPTIONS
            NOT_OPEN    = 1
            QUEUE_ERROR = 2
            OTHERS      = 3.
  IF SY-SUBRC EQ 0.
    MESSAGE I003 WITH 'ZFI_ZFAPC003'.
  ELSE.
    MESSAGE I019 WITH TEXT-002.
  ENDIF.
ENDFORM.
****************************** SUBROUTINES *****************************
FORM PUT_VALUES.

* PERFORM BDC_SCREEN USING 'SAPMF02B' '0100'.
* PERFORM BDC_FIELD  USING 'BNKA-BANKS' BNKA-BANKS.3
* PERFORM BDC_FIELD  USING 'BNKA-BANKL' BNKA-BANKL.15
  clear bank_info.
  move '0100'          to bank_info+0(4).
  move bnka-banks      to bank_info+4(3).
  move bnka-bankl      to bank_info+7(15).
  transfer bank_info to outfile length 200.

* PERFORM BDC_SCREEN USING 'SAPMF02B' '0110'.
* PERFORM BDC_FIELD  USING 'BNKA-BANKA' BNKA-BANKA.60
* PERFORM BDC_FIELD  USING 'BNKA-PROVZ' BNKA-PROVZ.3
* PERFORM BDC_FIELD  USING 'BNKA-STRAS' BNKA-STRAS.35
* PERFORM BDC_FIELD  USING 'BNKA-ORT01' BNKA-ORT01.35
* PERFORM BDC_FIELD  USING 'BNKA-BRNCH' BNKA-BRNCH.40
  clear bank_info.
  move '0110'          to bank_info+0(4).
  move bnka-banka      to bank_info+4(60).
  move bnka-provz      to bank_info+64(3).
  move bnka-stras      to bank_info+67(35).
  move bnka-ort01      to bank_info+102(35).
  move bnka-brnch      to bank_info+137(40).
  transfer bank_info to outfile length 200.

* PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/11'.

ENDFORM.

*-----------------------------------------------------------------------
*      FORM INSERT_SESSION
*-----------------------------------------------------------------------
*    Description:
*    - This routine inserts the BDC data for one transaction into the
*      batch input session.
*-----------------------------------------------------------------------
FORM INSERT_SESSION USING G_TRANSCODE.
*  CALL FUNCTION 'BDC_INSERT'
*       EXPORTING
*            TCODE          = G_TRANSCODE
*       TABLES
*            DYNPROTAB      = BDC_DATA
*       EXCEPTIONS
*            INTERNAL_ERROR = 1
*            NOT_OPEN       = 2
*            QUEUE_ERROR    = 3
*            TCODE_INVALID  = 4
*            OTHERS         = 5.
*
*Clear BDC_DATA.
*Refresh bdc_data.

  IF SY-SUBRC NE 0.
    MESSAGE I013.
  ENDIF.
ENDFORM.

*-----------------------------------------------------------------------
*      FORM BDC_SCREEN
*-----------------------------------------------------------------------
*    Description:
*    - This routine adds an entry to the table BDCDATA with screen
*      information from a particular transaction.  This is used as part
*      of the process for creating data for batch input.
*    Parameters:
*      -->  PROGRAM - Program name of the screen
*           DNYPRO  - Screen number
*-----------------------------------------------------------------------
FORM BDC_SCREEN USING PROGRAM DYNPRO.
*  CLEAR BDC_DATA.
*  BDC_DATA-PROGRAM = PROGRAM.
*  BDC_DATA-DYNPRO = DYNPRO.
*  BDC_DATA-DYNBEGIN = 'X'.
*  APPEND BDC_DATA.
ENDFORM.

*-----------------------------------------------------------------------
*      FORM BDC_FIELD
*-----------------------------------------------------------------------
*    Description:
*    - This routine adds an entry to the table BDCDATA with field
*      information from a particular transaction.  This is used as part
*      of the process for creating data for batch input.
*    Parameters:
*      -->  fnam - name of the field on the screen
*           fval - value to be entered for that field on the screen.
*-----------------------------------------------------------------------
FORM BDC_FIELD USING FNAM FVAL.
*  CLEAR BDC_DATA.
*  BDC_DATA-FNAM = FNAM.
*  BDC_DATA-FVAL = FVAL.
*  APPEND BDC_DATA.
ENDFORM.
