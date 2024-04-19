*&---------------------------------------------------------------------*
*&  Include           ZXM08U20
*&---------------------------------------------------------------------*
*& MODS:
*& 2013/04/09 Implementing note 159455 and move BLDAT (doc. date)
*& SDP27938   into E_RBKPV_ERS_CHANGE-BLDAT.
*& GYMANA
*& 2014/02/04 Commented out. - no longer required by Business
*& SDP27938
*& GYMANA
*&---------------------------------------------------------------------*

*MOVE-CORRESPONDING I_RBKPV TO E_RBKPV_ERS_CHANGE.
*
*SELECT BLDAT FROM MKPF
*   INTO E_RBKPV_ERS_CHANGE-BLDAT
*  WHERE MBLNR = T_SELWENR-LFBNR.
*ENDSELECT.
*
*MOVE 'X' TO E_CHANGE.
