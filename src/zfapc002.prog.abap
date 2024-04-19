REPORT ZFAPC002 MESSAGE-ID ZS.

************************************************************************
*  CREATED BY: NANCY GILLIGAN, OMNILOGIC  11/98                        *
*  PURPOSE:  THIS PROGRAM GATHERS INFORMATION ON VENDORS (FROM COMPANY *
*            UGL IN GROUP BUILDER, INSTALLER OR DEALER AND VENDORS     *
*            FROM COMPANIES TAF AND UELP IN GROUP EMPLOYEE ) IN CLIENT *
*            XX0 AND TRANSFERS THIS DATA VIA BDC SESSION TO CLIENT XX1.*
*  REQUESTED BY: LAURIE, ISSUE LOG #                                   *
************************************************************************

TABLES: ZV_LFA1, BNKA.

DATA  : BEGIN OF BDCDATA OCCURS 100.
          INCLUDE STRUCTURE BDCDATA.
DATA  : END OF BDCDATA.

DATA  : G_TRANSCODE    LIKE TSTC-TCODE VALUE 'FK01',
        NEW_BUKRS      LIKE ZV_LFA1-BUKRS VALUE 'UEC'.


SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) TEXT-001.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
SELECT-OPTIONS:
     S_KONZS        FOR   ZV_LFA1-KONZS NO INTERVALS,   "GROUP KEY
     S_LIFNR        FOR   ZV_LFA1-LIFNR.        "VENDOR # (TESTING)
SELECTION-SCREEN SKIP.
PARAMETERS:
     P_MANDT       LIKE   SY-MANDT DEFAULT '021'.  "TARGET CLIENT
SELECTION-SCREEN END OF BLOCK BOX1.


** INITIALIZATION
INITIALIZATION.
S_KONZS-OPTION = 'EQ'.
S_KONZS-SIGN = 'I'.
S_KONZS-LOW = 'EMPLOYEE'.
APPEND S_KONZS.
S_KONZS-OPTION = 'EQ'.
S_KONZS-SIGN = 'I'.
S_KONZS-LOW = 'BUILDER'.
APPEND S_KONZS.
S_KONZS-OPTION = 'EQ'.
S_KONZS-SIGN = 'I'.
S_KONZS-LOW = 'INSTALLER'.
APPEND S_KONZS.
S_KONZS-OPTION = 'EQ'.
S_KONZS-SIGN = 'I'.
S_KONZS-LOW = 'DEALER'.
APPEND S_KONZS.

*******************************  MAIN  *********************************
START-OF-SELECTION.

PERFORM OPEN_BDC.

  SELECT * FROM ZV_LFA1 WHERE KONZS IN S_KONZS
                          AND BUKRS IN ('TAF', 'UELP', 'UGL').

* change accounting clerk number for new client  (per Laurie Callow)
  if zv_lfa1-busab = '09'.
     zv_lfa1-busab = '01'.
  else.  " do nothing, the clerk number is okay
  endif.

* ONLY PROCESS IF COMPANY CODE AND GROUP GO TOGETHER
  IF ZV_LFA1-KONZS = 'EMPLOYEE'
      AND ( ZV_LFA1-BUKRS EQ 'TAF' OR ZV_LFA1-BUKRS EQ 'UELP' ).
*          CONTINUE.
  ELSEIF ZV_LFA1-BUKRS EQ 'UGL'
      AND ( ZV_LFA1-KONZS EQ 'BUILDER'
          OR ZV_LFA1-KONZS EQ 'INSTALLER'
          OR ZV_LFA1-KONZS EQ 'DEALER' ).
*          CONTINUE.
  ELSE.  " DOESN'T FIT CRITERIA - GO TO NEXT SELECT.
     EXIT.
  ENDIF.

    G_TRANSCODE = 'FK01'.
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
           GROUP               = 'ZFI_ZFAPC002'
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
    MESSAGE E004 WITH 'ZFI_ZFAPC002'.
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
    MESSAGE I003 WITH 'ZFI_ZFAPC002'.
  ELSE.
    MESSAGE I019 WITH TEXT-002.
  ENDIF.
ENDFORM.
****************************** SUBROUTINES *****************************
FORM PUT_VALUES.

  PERFORM BDC_SCREEN USING 'SAPMF02K' '0105'.
  PERFORM BDC_FIELD  USING 'RF02K-LIFNR' ZV_LFA1-LIFNR.
  PERFORM BDC_FIELD  USING 'RF02K-BUKRS' NEW_BUKRS.
  PERFORM BDC_FIELD  USING 'RF02K-KTOKK' ZV_LFA1-KTOKK.

  PERFORM BDC_SCREEN USING 'SAPMF02K' '0110'.
  PERFORM BDC_FIELD  USING 'LFA1-NAME1' ZV_LFA1-NAME1.
  PERFORM BDC_FIELD  USING 'LFA1-NAME2' ZV_LFA1-NAME2.
  PERFORM BDC_FIELD  USING 'LFA1-SORTL' ZV_LFA1-SORTL.
  PERFORM BDC_FIELD  USING 'LFA1-STRAS' ZV_LFA1-STRAS.
  PERFORM BDC_FIELD  USING 'LFA1-PFACH' ZV_LFA1-PFACH.
  PERFORM BDC_FIELD  USING 'LFA1-ORT01' ZV_LFA1-ORT01.
  PERFORM BDC_FIELD  USING 'LFA1-PSTLZ' ZV_LFA1-PSTLZ.
  PERFORM BDC_FIELD  USING 'LFA1-LAND1' ZV_LFA1-LAND1.
  PERFORM BDC_FIELD  USING 'LFA1-REGIO' ZV_LFA1-REGIO.
  PERFORM BDC_FIELD  USING 'LFA1-SPRAS' ZV_LFA1-SPRAS.
  PERFORM BDC_FIELD  USING 'LFA1-TELX1' ZV_LFA1-TELX1.
  PERFORM BDC_FIELD  USING 'LFA1-TELF1' ZV_LFA1-TELF1.
  PERFORM BDC_FIELD  USING 'LFA1-TELFX' ZV_LFA1-TELFX.

  PERFORM BDC_SCREEN USING 'SAPMF02K' '0120'.
  PERFORM BDC_FIELD  USING 'LFA1-KONZS' ZV_LFA1-KONZS.
  PERFORM BDC_FIELD  USING 'LFA1-STCD1' ZV_LFA1-STCD1.

  PERFORM BDC_SCREEN USING 'SAPMF02K' '0130'.
  PERFORM BDC_FIELD  USING 'LFBK-BANKS(1)' ZV_LFA1-BANKS.
  PERFORM BDC_FIELD  USING 'LFBK-BANKL(1)' ZV_LFA1-BANKL.
  PERFORM BDC_FIELD  USING 'LFBK-BANKN(1)' ZV_LFA1-BANKN.

   PERFORM BDC_SCREEN USING 'SAPMF02K' '0130'.
   PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/8'.

  PERFORM BDC_SCREEN USING 'SAPMF02K' '0210'.
  PERFORM BDC_FIELD  USING 'LFB1-AKONT' ZV_LFA1-AKONT.
  PERFORM BDC_FIELD  USING 'LFB1-ZUAWA' ZV_LFA1-ZUAWA.

  PERFORM BDC_SCREEN USING 'SAPMF02K' '0215'.
  PERFORM BDC_FIELD  USING 'LFB1-ZTERM' ZV_LFA1-ZTERM.
  PERFORM BDC_FIELD  USING 'LFB1-REPRF' ZV_LFA1-REPRF.
  PERFORM BDC_FIELD  USING 'LFB1-ZWELS' ZV_LFA1-ZWELS.
  PERFORM BDC_FIELD  USING 'LFB1-ZAHLS' ZV_LFA1-ZAHLS.
  PERFORM BDC_FIELD  USING 'LFB1-LNRZB' ZV_LFA1-LNRZB.
  PERFORM BDC_FIELD  USING 'LFB1-ZGRUP' ZV_LFA1-ZGRUP.
  PERFORM BDC_FIELD  USING 'LFB1-XEDIP' ZV_LFA1-XEDIP.

  PERFORM BDC_SCREEN USING 'SAPMF02K' '0220'.
  PERFORM BDC_FIELD  USING 'LFB1-BUSAB' ZV_LFA1-BUSAB.
  PERFORM BDC_FIELD  USING 'LFB1-EIKTO' ZV_LFA1-EIKTO.
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
            DYNPROTAB      = BDCDATA
       EXCEPTIONS
            INTERNAL_ERROR = 1
            NOT_OPEN       = 2
            QUEUE_ERROR    = 3
            TCODE_INVALID  = 4
            OTHERS         = 5.

refresh BDCDATA.
clear BDCDATA.
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
  CLEAR BDCDATA.
  BDCDATA-PROGRAM = PROGRAM.
  BDCDATA-DYNPRO = DYNPRO.
  BDCDATA-DYNBEGIN = 'X'.
  APPEND BDCDATA.
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
  CLEAR BDCDATA.
  BDCDATA-FNAM = FNAM.
  BDCDATA-FVAL = FVAL.
  APPEND BDCDATA.
ENDFORM.
