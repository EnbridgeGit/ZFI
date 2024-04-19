*&---------------------------------------------------------------------*
*&  Include           ZFI_WBS_STATUSCHANGE_MN
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
* Program Name       :  ZFI_WBS_STATUSCHANGE                           *
* Include            :  ZFI_WBS_STATUSCHANGE_MN                        *
* Author             :  Ashok Madasu                                   *
* Date               :  20-Aug-2018                                    *
* Technical Contact  :  Ashok Madasu                                   *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :  WBS status change report                       *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By    CTS        Description                    *
* ---------------------------------------------------------------------*
* 22/08/2019   akmadasu    D30K930092  CHG0138619 - in-service and     *
*                          D30K930231  completion date updation to WBSe*
* 22/08/2019   JOOKONTR    D30K930297  CHG0164542 Changes in all       *
*                                      options                         *
*&---------------------------------------------------------------------*

START-OF-SELECTION.

  IF R_CLSD IS NOT INITIAL OR R_REV IS NOT INITIAL OR RT_CLSD IS NOT INITIAL  "Added Condition by JOOKONTR CHG0138619
                           OR R_TTOR IS NOT INITIAL "Added Condition by JOOKONTR CHG0138619
                           OR R_RTOT IS NOT INITIAL OR R_CRTOR IS NOT INITIAL. "Added Condition by JOOKONTR CHG0164542
    IF S_PSPNR IS NOT INITIAL.
      MESSAGE 'Enter Project Number only' TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ELSEIF S_PSPNR1 IS INITIAL.
      MESSAGE 'Enter Project Number' TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ELSEIF S_PSPNR IS INITIAL AND S_PSPNR1 IS INITIAL.
      MESSAGE 'Enter Project Number' TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ELSE.
    IF S_PSPNR1 IS NOT INITIAL.
      MESSAGE 'Enter WBS Number only' TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ELSEIF S_PSPNR IS INITIAL.
      MESSAGE 'EnterWBS Number' TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ELSEIF S_PSPNR IS INITIAL AND S_PSPNR1 IS INITIAL.
      MESSAGE 'EnterWBS Number' TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE  LIST-PROCESSING.
    ENDIF.
  ENDIF.
**--START OF CHANGES BY AKMADASU CHG0138619
  "CHG0121630 - Powerplan development for Req 11
  IF  R_RTOT IS NOT INITIAL AND P_USR08 IS INITIAL.
    MESSAGE TEXT-600 TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE  LIST-PROCESSING.
  ELSEIF R_CLSD IS NOT INITIAL AND P_USR09 IS INITIAL.
    MESSAGE TEXT-601 TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE  LIST-PROCESSING.
  ELSEIF R_CRTOR IS NOT INITIAL AND ( P_PLFAZ IS INITIAL OR P_PLSEZ IS INITIAL OR P_EPROG IS INITIAL ).
    MESSAGE TEXT-602 TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE  LIST-PROCESSING.
  ENDIF.
**--END OF CHANGES BY AKMADASU CHG0138619
  "CHG0121630 - Powerplan development for Req 11
  PERFORM GET_DATA.
  PERFORM UPDATE_STATUS.
  PERFORM BUILD_FIELDCATALOG.
  IF GT_FINAL[] IS NOT INITIAL.
    PERFORM DISPLAY_ALV.
  ELSE.
    WRITE : / 'No data to output......'.
  ENDIF.
