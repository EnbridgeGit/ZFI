REPORT ZFAPC004 MESSAGE-ID ZS.

************************************************************************
*  CREATED BY: NANCY GILLIGAN, OMNILOGIC  11/98                        *
*  PURPOSE:  THIS PROGRAM GATHERS INFORMATION ON VENDORS (FROM COMPANY *
*            UGL IN GROUP BUILDER, INSTALLER OR DEALER AND VENDORS     *
*            FROM COMPANIES TAF AND UELP IN GROUP EMPLOYEE ) IN CLIENT *
*            XX0 AND TRANSFERS THIS DATA VIA BDC SESSION TO CLIENT XX1.*
*  REQUESTED BY: LAURIE, ISSUE LOG #                                   *
************************************************************************

TABLES: ZV_LFA1, BNKA.

data:  vendor_info(260).
*DATA  : BEGIN OF BDCDATA OCCURS 100.
*          INCLUDE STRUCTURE BDCDATA.
*DATA  : END OF BDCDATA.

*DATA  : G_TRANSCODE    LIKE TSTC-TCODE VALUE 'FK01',
data:        NEW_BUKRS      LIKE ZV_LFA1-BUKRS VALUE 'UEC'.
data: outfile like filename-fileintern
          value '/usr/sap/interfaces/D30/UEC/IFFI0201/'.



SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) TEXT-001.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
SELECT-OPTIONS:
     S_KONZS        FOR   ZV_LFA1-KONZS NO INTERVALS,   "GROUP KEY
     S_LIFNR        FOR   ZV_LFA1-LIFNR.        "VENDOR # (TESTING)
SELECTION-SCREEN SKIP.
PARAMETERS:
     P_MANDT       LIKE   SY-MANDT DEFAULT '021',  "TARGET CLIENT
     p_sysid       like   sy-sysid default 'P01'.  "TArget system
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
 concatenate outfile s_konzs+3 '.sap' into outfile.
 move p_sysid(3) to outfile+20(3).
 open dataset outfile for output in text mode.
*PERFORM OPEN_BDC.

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
    PERFORM PUT_VALUES.
*          CONTINUE.
  ELSEIF ZV_LFA1-BUKRS EQ 'UGL'
      AND ( ZV_LFA1-KONZS EQ 'BUILDER'
          OR ZV_LFA1-KONZS EQ 'INSTALLER'
          OR ZV_LFA1-KONZS EQ 'DEALER' ).
    PERFORM PUT_VALUES.
*          CONTINUE.
  ELSE.  " DOESN'T FIT CRITERIA - GO TO NEXT SELECT.
*    write:  'doesn't fit'
  ENDIF.

*    G_TRANSCODE = 'FK01'.
*   PERFORM PUT_VALUES.
*    PERFORM INSERT_SESSION USING G_TRANSCODE.
  ENDSELECT.    " SELECT FROM ZV_LFA1

     IF SY-SUBRC NE 0.
       SKIP 2.
       WRITE: TEXT-005.
*    ELSE.
*      PERFORM CLOSE_BDC.
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

*  PERFORM BDC_SCREEN USING 'SAPMF02K' '0105'.                (4)
*  PERFORM BDC_FIELD  USING 'RF02K-LIFNR' ZV_LFA1-LIFNR.       (10)
*  PERFORM BDC_FIELD  USING 'RF02K-BUKRS' NEW_BUKRS.           (4)
*  PERFORM BDC_FIELD  USING 'RF02K-KTOKK' ZV_LFA1-KTOKK.       (4)
  clear vendor_info.
  move '0105'          to vendor_info+0(4).
  move zv_lfa1-lifnr   to vendor_info+4(10).
  move new_bukrs       to vendor_info+14(4).
  move zv_lfa1-ktokk   to vendor_info+18(4).
  transfer vendor_info to outfile length 260.

*  PERFORM BDC_SCREEN USING 'SAPMF02K' '0110'.
*  PERFORM BDC_FIELD  USING 'LFA1-NAME1' ZV_LFA1-NAME1.(35)
*  PERFORM BDC_FIELD  USING 'LFA1-NAME2' ZV_LFA1-NAME2.(35)
*  PERFORM BDC_FIELD  USING 'LFA1-SORTL' ZV_LFA1-SORTL.(10)
*  PERFORM BDC_FIELD  USING 'LFA1-STRAS' ZV_LFA1-STRAS.(35)
*  PERFORM BDC_FIELD  USING 'LFA1-PFACH' ZV_LFA1-PFACH.(10)
*  PERFORM BDC_FIELD  USING 'LFA1-ORT01' ZV_LFA1-ORT01.(35)
*  PERFORM BDC_FIELD  USING 'LFA1-PSTLZ' ZV_LFA1-PSTLZ.(10)
*  PERFORM BDC_FIELD  USING 'LFA1-LAND1' ZV_LFA1-LAND1.(3)
*  PERFORM BDC_FIELD  USING 'LFA1-REGIO' ZV_LFA1-REGIO.(3)
*  PERFORM BDC_FIELD  USING 'LFA1-SPRAS' ZV_LFA1-SPRAS.(1)
*  PERFORM BDC_FIELD  USING 'LFA1-TELX1' ZV_LFA1-TELX1.(30)
*  PERFORM BDC_FIELD  USING 'LFA1-TELF1' ZV_LFA1-TELF1.(16)
*  PERFORM BDC_FIELD  USING 'LFA1-TELFX' ZV_LFA1-TELFX.(31)
  clear vendor_info.
  move '0110'          to vendor_info+0(4).
  move zv_lfa1-name1   to vendor_info+4(35).
  move zv_lfa1-name2   to vendor_info+39(35).
  move zv_lfa1-sortl   to vendor_info+74(10).
  move zv_lfa1-stras   to vendor_info+84(35).
  move zv_lfa1-pfach   to vendor_info+119(10).
  move zv_lfa1-ort01   to vendor_info+129(35).
  move zv_lfa1-pstlz   to vendor_info+164(10).
  move zv_lfa1-land1   to vendor_info+174(3).
  move zv_lfa1-regio   to vendor_info+177(3).
  move zv_lfa1-spras   to vendor_info+180(1).
  move zv_lfa1-telx1   to vendor_info+181(30).
  move zv_lfa1-telf1   to vendor_info+211(16).
  move zv_lfa1-telfx   to vendor_info+227(31).
  transfer vendor_info to outfile length 260.

*  PERFORM BDC_SCREEN USING 'SAPMF02K' '0120'.
*  PERFORM BDC_FIELD  USING 'LFA1-KONZS' ZV_LFA1-KONZS. 10
*  PERFORM BDC_FIELD  USING 'LFA1-STCD1' ZV_LFA1-STCD1. 16
  clear vendor_info.
  move '0120'          to vendor_info+0(4).
  move zv_lfa1-konzs   to vendor_info+4(10).
  move zv_lfa1-stcd1   to vendor_info+14(16).
  move zv_lfa1-begru   to vendor_info+30(4).
  transfer vendor_info to outfile length 260.

*  PERFORM BDC_SCREEN USING 'SAPMF02K' '0130'.
*  PERFORM BDC_FIELD  USING 'LFBK-BANKS(1)' ZV_LFA1-BANKS.3
*  PERFORM BDC_FIELD  USING 'LFBK-BANKL(1)' ZV_LFA1-BANKL.15
*  PERFORM BDC_FIELD  USING 'LFBK-BANKN(1)' ZV_LFA1-BANKN.18
  clear vendor_info.
  move '0130'          to vendor_info+0(4).
  move zv_lfa1-banks   to vendor_info+4(3).
  move zv_lfa1-bankl   to vendor_info+7(15).
  move zv_lfa1-bankn   to vendor_info+22(18).
  transfer vendor_info to outfile length 260.

*   PERFORM BDC_SCREEN USING 'SAPMF02K' '0130'.
*   PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/8'.

*  PERFORM BDC_SCREEN USING 'SAPMF02K' '0210'.
*  PERFORM BDC_FIELD  USING 'LFB1-AKONT' ZV_LFA1-AKONT.10
*  PERFORM BDC_FIELD  USING 'LFB1-ZUAWA' ZV_LFA1-ZUAWA.3
  clear vendor_info.
  move '0210'          to vendor_info+0(4).
  move zv_lfa1-akont   to vendor_info+4(10).
  move zv_lfa1-zuawa   to vendor_info+14(3).
  transfer vendor_info to outfile length 260.

*  PERFORM BDC_SCREEN USING 'SAPMF02K' '0215'.
*  PERFORM BDC_FIELD  USING 'LFB1-ZTERM' ZV_LFA1-ZTERM.4
*  PERFORM BDC_FIELD  USING 'LFB1-REPRF' ZV_LFA1-REPRF.1
*  PERFORM BDC_FIELD  USING 'LFB1-ZWELS' ZV_LFA1-ZWELS.10
*  PERFORM BDC_FIELD  USING 'LFB1-ZAHLS' ZV_LFA1-ZAHLS.1
*  PERFORM BDC_FIELD  USING 'LFB1-LNRZB' ZV_LFA1-LNRZB.10
*  PERFORM BDC_FIELD  USING 'LFB1-ZGRUP' ZV_LFA1-ZGRUP.2
*  PERFORM BDC_FIELD  USING 'LFB1-XEDIP' ZV_LFA1-XEDIP.1
  clear vendor_info.
  move '0215'          to vendor_info+0(4).
  move zv_lfa1-zterm   to vendor_info+4(4).
  move zv_lfa1-reprf   to vendor_info+8(1).
  move zv_lfa1-reprf   to vendor_info+8(1).
  move zv_lfa1-zwels   to vendor_info+9(10).
  move zv_lfa1-zahls   to vendor_info+19(1).
  move zv_lfa1-lnrzb   to vendor_info+20(10).
  move zv_lfa1-zgrup   to vendor_info+30(2).
  move zv_lfa1-xedip   to vendor_info+32(1).
  transfer vendor_info to outfile length 260.

*  PERFORM BDC_SCREEN USING 'SAPMF02K' '0220'.
*  PERFORM BDC_FIELD  USING 'LFB1-BUSAB' ZV_LFA1-BUSAB.2
*  PERFORM BDC_FIELD  USING 'LFB1-EIKTO' ZV_LFA1-EIKTO.12
*  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/11'.
  clear vendor_info.
  move '0220'          to vendor_info+0(4).
  move zv_lfa1-busab   to vendor_info+4(2).
  move zv_lfa1-eikto   to vendor_info+6(12).
  transfer vendor_info to outfile length 260.
ENDFORM.

*-----------------------------------------------------------------------
*      FORM INSERT_SESSION
*-----------------------------------------------------------------------
*    Description:
*    - This routine inserts the BDC data for one transaction into the
*      batch input session.
*-----------------------------------------------------------------------
*FORM INSERT_SESSION USING G_TRANSCODE.
*  CALL FUNCTION 'BDC_INSERT'
*       EXPORTING
*            TCODE          = G_TRANSCODE
*       TABLES
*            DYNPROTAB      = BDCDATA
*       EXCEPTIONS
*            INTERNAL_ERROR = 1
*            NOT_OPEN       = 2
*            QUEUE_ERROR    = 3
*            TCODE_INVALID  = 4
*            OTHERS         = 5.

*refresh BDCDATA.
*clear BDCDATA.
*  IF SY-SUBRC NE 0.
*    MESSAGE I013.
*  ENDIF.
*ENDFORM.

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
*FORM BDC_SCREEN USING PROGRAM DYNPRO.
*  CLEAR BDCDATA.
*  BDCDATA-PROGRAM = PROGRAM.
*  BDCDATA-DYNPRO = DYNPRO.
*  BDCDATA-DYNBEGIN = 'X'.
*  APPEND BDCDATA.
*ENDFORM.

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
*FORM BDC_FIELD USING FNAM FVAL.
*  CLEAR BDCDATA.
*  BDCDATA-FNAM = FNAM.
*  BDCDATA-FVAL = FVAL.
*  APPEND BDCDATA.
*ENDFORM.
