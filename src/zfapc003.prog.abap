REPORT ZFAPC003 MESSAGE-ID ZS.

************************************************************************
*  CREATED BY: NANCY GILLIGAN, OMNILOGIC  11/98                        *
*  PURPOSE:  THIS PROGRAM COPIES INFORMATION ON ALL BANKS IN CLIENT    *
*            XX0 AND TRANSFERS THEM TO CLIENT XX1 VIA BDC SESSION.     *
*  REQUESTED BY: LAURIE, ISSUE LOG #                                   *
************************************************************************

TABLES: BNKA.

DATA  : BEGIN OF BDC_DATA OCCURS 100.
          INCLUDE STRUCTURE BDCDATA.
DATA  : END OF BDC_DATA.

DATA  : G_TRANSCODE    LIKE TSTC-TCODE VALUE 'FI01'.


SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) TEXT-001.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
SELECT-OPTIONS:
    S_BANKL     FOR   BNKA-BANKL,   "BANK KEY
    S_BANKS     FOR   BNKA-BANKS NO INTERVALS.   "BANK COUNTR
SELECTION-SCREEN SKIP.
PARAMETERS:
     P_MANDT    LIKE SY-MANDT DEFAULT '021' OBLIGATORY.   "TARGET CLIENT
SELECTION-SCREEN END OF BLOCK BOX1.


*******************************  MAIN  *********************************
START-OF-SELECTION.

PERFORM OPEN_BDC.

  SELECT * FROM BNKA WHERE BANKL IN S_BANKL
                       AND BANKS IN S_BANKS.

    PERFORM PUT_VALUES.
    PERFORM INSERT_SESSION USING G_TRANSCODE.
  ENDSELECT.    " SELECT FROM ZV_LFA1

  IF SY-SUBRC NE 0.
    SKIP 2.
    WRITE: TEXT-005.
  ELSE.
    PERFORM CLOSE_BDC.
  ENDIF.

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

  PERFORM BDC_SCREEN USING 'SAPMF02B' '0100'.
  PERFORM BDC_FIELD  USING 'BNKA-BANKS' BNKA-BANKS.
  PERFORM BDC_FIELD  USING 'BNKA-BANKL' BNKA-BANKL.

  PERFORM BDC_SCREEN USING 'SAPMF02B' '0110'.
  PERFORM BDC_FIELD  USING 'BNKA-BANKA' BNKA-BANKA.
  PERFORM BDC_FIELD  USING 'BNKA-PROVZ' BNKA-PROVZ.
  PERFORM BDC_FIELD  USING 'BNKA-STRAS' BNKA-STRAS.
  PERFORM BDC_FIELD  USING 'BNKA-ORT01' BNKA-ORT01.
  PERFORM BDC_FIELD  USING 'BNKA-BRNCH' BNKA-BRNCH.

  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/11'.

ENDFORM.

*-----------------------------------------------------------------------
*      FORM INSERT_SESSION
*-----------------------------------------------------------------------
*    Description:
*    - This routine inserts the BDC data for one transaction into the
*      batch input session.
*-----------------------------------------------------------------------
FORM INSERT_SESSION USING G_TRANSCODE.
  CALL FUNCTION 'BDC_INSERT'
       EXPORTING
            TCODE          = G_TRANSCODE
       TABLES
            DYNPROTAB      = BDC_DATA
       EXCEPTIONS
            INTERNAL_ERROR = 1
            NOT_OPEN       = 2
            QUEUE_ERROR    = 3
            TCODE_INVALID  = 4
            OTHERS         = 5.

clear BDC_DATA.
refresh bdc_data.

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
  CLEAR BDC_DATA.
  BDC_DATA-PROGRAM = PROGRAM.
  BDC_DATA-DYNPRO = DYNPRO.
  BDC_DATA-DYNBEGIN = 'X'.
  APPEND BDC_DATA.
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
  CLEAR BDC_DATA.
  BDC_DATA-FNAM = FNAM.
  BDC_DATA-FVAL = FVAL.
  APPEND BDC_DATA.
ENDFORM.
