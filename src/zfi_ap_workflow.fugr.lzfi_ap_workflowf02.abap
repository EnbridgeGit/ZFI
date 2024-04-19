*----------------------------------------------------------------------*
***INCLUDE LZFI_AP_WORKFLOWF02 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_RECIPENT_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_RECIPENT_LIST .

  DATA: LTP_OBJKEY    TYPE SWO_TYPEID,
        LTP_WI        TYPE SWW_WIID,
        LTA_WI_USER   TYPE STANDARD TABLE OF SWWUSERWI,
        LWA_WI_USER   TYPE SWWUSERWI,
        LTP_RCOWNER   TYPE WFSYST-INITIATOR,
        LV_RCOWNER    TYPE USERNAME,
        LTP_LOG_DEST  TYPE TB_RFCDEST.

****OBSOLETE AS OF WORKFLOW VERSION 11
  IF GTP_DECISION_TYPE = 'NOTI'.
    PERFORM GET_EMAIL_ID USING GTP_USER SPACE.
  ENDIF.
****OBSOLETE AS OF WORKFLOW VERSION 11

  IF GTP_DECISION_TYPE = 'CODE'.
    PERFORM GET_EMAIL_ID USING GTP_USER SPACE.
  ENDIF.

  IF GTP_DECISION_TYPE = 'APPR'.
    PERFORM GET_EMAIL_ID USING GTP_USER SPACE.
  ENDIF.

  IF GTP_DECISION_TYPE = 'REJT'.
    PERFORM GET_EMAIL_ID USING GTP_USER SPACE.
  ENDIF.

  IF  GTP_DECISION_TYPE = 'DELE'.
    PERFORM GET_EMAIL_ID USING GTP_USER SPACE.
  ENDIF.

*  IF gtp_decision_type = 'COAP'.
*    PERFORM get_email_id USING sy-uname space.
*  ENDIF.

*  IF gtp_decision_type = 'FORW'.
*    PERFORM get_email_id USING gtp_user space.
*  ENDIF.

  IF GTP_DECISION_TYPE = 'REMI'.
    CALL FUNCTION 'ZFI_AP_GET_ROUTE_CODE_OWNER'
      EXPORTING
        IMP_BUKRS    = GTP_BUKRS
        IMP_BELNR    = GTP_BELNR
        IMP_GJAHR    = GTP_GJAHR
      IMPORTING
        EXP_RC_OWNER = LTP_RCOWNER
      EXCEPTIONS
        NOBODY_FOUND = 1
        OTHERS       = 2.
    IF SY-SUBRC <> 0.
* Implement suitable error handling here
    ENDIF.

    LV_RCOWNER = LTP_RCOWNER(2).

    PERFORM GET_EMAIL_ID USING LV_RCOWNER  SPACE.
    PERFORM GET_EMAIL_ID USING GTP_USER SPACE.
  ENDIF.
ENDFORM.                    " GET_RECIPENT_LIST
*&---------------------------------------------------------------------*
*&      Form  GET_EMAIL_ID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LTP_USERNAME  text
*      -->P_0041   text
*----------------------------------------------------------------------*
FORM GET_EMAIL_ID  USING IM_USERNAME TYPE USERNAME
                         IM_COPY     TYPE FLAG.

  DATA: LTP_EMAIL_ID    TYPE AD_SMTPADR,
        LST_RECLIST     TYPE SOMLRECI1,
        LST_ADDRNUMBER  TYPE AD_ADDRNUM,
        LST_PERSNUMBER  TYPE AD_PERSNUM .

  SELECT SINGLE ADDRNUMBER PERSNUMBER
    FROM USR21
    INTO (LST_ADDRNUMBER , LST_PERSNUMBER )
    WHERE BNAME = IM_USERNAME
  .

  SELECT SINGLE SMTP_ADDR
    FROM ADR6
    INTO LTP_EMAIL_ID
    WHERE ADDRNUMBER = LST_ADDRNUMBER
      AND PERSNUMBER = LST_PERSNUMBER
  .

  LST_RECLIST-RECEIVER = LTP_EMAIL_ID.
  LST_RECLIST-COPY = IM_COPY.
  LST_RECLIST-REC_TYPE = 'U'.
  INSERT  LST_RECLIST INTO TABLE GTA_RECLIST.
ENDFORM.                    " GET_EMAIL_ID
*&---------------------------------------------------------------------*
*&      Form  SET_MAIL_BODY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_MAIL_BODY .

  REFRESH : GTA_OBJTXT.

  DATA: LST_OBJTXT     TYPE SOLISTI1,
        LTP_OBJKEY     TYPE SWO_TYPEID,
        LV_SYSTEM      LIKE ZVAR-VALUE1.

  CONCATENATE GTP_BUKRS GTP_BELNR GTP_GJAHR INTO LTP_OBJKEY.

  SELECT SINGLE VALUE1
    FROM ZVAR
    INTO LV_SYSTEM
    WHERE PROGRAMM  = 'ALL'
      AND VARNAME   = 'SYSTEM'
  .

  CASE GTP_DECISION_TYPE.
****OBSOLETE AS OF WORKFLOW VERSION 11
    WHEN 'NOTI'.
      "***SET SUBJECT LINE***
      CONCATENATE 'Invoice ' GTP_BUKRS ' ' GTP_BELNR ' ' GTP_GJAHR ': Coding/Approval Reqd'
      INTO GST_DOC_CHNG-OBJ_DESCR RESPECTING BLANKS.

      "***SET MAIL BODY***
      CONCATENATE 'Dear SAP' LV_SYSTEM 'User,' INTO LST_OBJTXT-LINE SEPARATED BY SPACE.
      INSERT LST_OBJTXT INTO TABLE GTA_OBJTXT.

      "BLANK LINE
      CLEAR LST_OBJTXT-LINE.
      INSERT LST_OBJTXT INTO TABLE GTA_OBJTXT.

      CONCATENATE 'Invoice' GTP_BUKRS GTP_BELNR GTP_GJAHR 'is in your workflow queue and is waiting for Coding/Approval.'
      INTO LST_OBJTXT-LINE SEPARATED BY SPACE.
      INSERT LST_OBJTXT INTO TABLE GTA_OBJTXT.
****OBSOLETE AS OF WORKFLOW VERSION 11
    WHEN 'CODE'.
      "***SET SUBJECT LINE***
      CONCATENATE 'Invoice ' GTP_BUKRS ' ' GTP_BELNR ' ' GTP_GJAHR ': Coding Required'
      INTO GST_DOC_CHNG-OBJ_DESCR RESPECTING BLANKS.

      "***SET MAIL BODY***
      CONCATENATE 'Dear SAP' LV_SYSTEM 'User,' INTO LST_OBJTXT-LINE SEPARATED BY SPACE.
      INSERT LST_OBJTXT INTO TABLE GTA_OBJTXT.

      "BLANK LINE
      CLEAR LST_OBJTXT-LINE.
      INSERT LST_OBJTXT INTO TABLE GTA_OBJTXT.

      CONCATENATE 'Invoice' GTP_BUKRS GTP_BELNR GTP_GJAHR 'is in your workflow queue and is waiting for Coding.'
      INTO LST_OBJTXT-LINE SEPARATED BY SPACE.
      INSERT LST_OBJTXT INTO TABLE GTA_OBJTXT.

    WHEN 'APPR'.
      "***SET SUBJECT LINE***
      CONCATENATE 'Invoice ' GTP_BUKRS ' ' GTP_BELNR ' ' GTP_GJAHR ': Approval Required'
      INTO GST_DOC_CHNG-OBJ_DESCR RESPECTING BLANKS.

      "***SET MAIL BODY***
      CONCATENATE 'Dear SAP' LV_SYSTEM 'User,' INTO LST_OBJTXT-LINE SEPARATED BY SPACE.
      INSERT LST_OBJTXT INTO TABLE GTA_OBJTXT.

      "BLANK LINE
      CLEAR LST_OBJTXT-LINE.
      INSERT LST_OBJTXT INTO TABLE GTA_OBJTXT.

      CONCATENATE 'Invoice' GTP_BUKRS GTP_BELNR GTP_GJAHR 'is in your workflow queue and is waiting for Approval.'
      INTO LST_OBJTXT-LINE SEPARATED BY SPACE.
      INSERT LST_OBJTXT INTO TABLE GTA_OBJTXT.

    WHEN 'REJT'.
      "***SET SUBJECT LINE***
      CONCATENATE 'Invoice ' GTP_BUKRS ' ' GTP_BELNR ' ' GTP_GJAHR ': Rejected'
      INTO GST_DOC_CHNG-OBJ_DESCR RESPECTING BLANKS.

      "***SET MAIL BODY***
      CONCATENATE 'Dear SAP' LV_SYSTEM 'User,' INTO LST_OBJTXT-LINE SEPARATED BY SPACE.
      INSERT LST_OBJTXT INTO TABLE GTA_OBJTXT.

      "BLANK LINE
      CLEAR LST_OBJTXT-LINE.
      INSERT LST_OBJTXT INTO TABLE GTA_OBJTXT.

      CONCATENATE 'Invoice' GTP_BUKRS GTP_BELNR GTP_GJAHR 'has been rejected.'
      INTO LST_OBJTXT-LINE SEPARATED BY SPACE.
      INSERT LST_OBJTXT INTO TABLE GTA_OBJTXT.

    WHEN 'DELE'.
      "***SET SUBJECT LINE***
      CONCATENATE 'Invoice ' GTP_BUKRS ' ' GTP_BELNR ' ' GTP_GJAHR ': Deleted'
      INTO GST_DOC_CHNG-OBJ_DESCR RESPECTING BLANKS.

      "***SET MAIL BODY***
      CONCATENATE 'Dear SAP' LV_SYSTEM 'User,' INTO LST_OBJTXT-LINE SEPARATED BY SPACE.
      INSERT LST_OBJTXT INTO TABLE GTA_OBJTXT.

      "BLANK LINE
      CLEAR LST_OBJTXT-LINE.
      INSERT LST_OBJTXT INTO TABLE GTA_OBJTXT.

      CONCATENATE 'Invoice' GTP_BUKRS GTP_BELNR GTP_GJAHR 'has been deleted.'
      INTO LST_OBJTXT-LINE SEPARATED BY SPACE.
      INSERT LST_OBJTXT INTO TABLE GTA_OBJTXT.

*    WHEN 'COAP'.
*      "***SET SUBJECT LINE***
*      CONCATENATE 'Invoice ' gtp_bukrs ' ' gtp_belnr ' ' gtp_gjahr ': Approved'
*      INTO gst_doc_chng-obj_descr RESPECTING BLANKS.
*
*      "***SET MAIL BODY***
*      CONCATENATE 'Dear SAP' lv_system 'User,' INTO lst_objtxt-line SEPARATED BY space.
*      INSERT lst_objtxt INTO TABLE gta_objtxt.
*
*      "BLANK LINE
*      CLEAR lst_objtxt-line.
*      INSERT lst_objtxt INTO TABLE gta_objtxt.
*
*      CONCATENATE 'Invoice' gtp_bukrs gtp_belnr gtp_gjahr 'has been approved.'
*      INTO lst_objtxt-line SEPARATED BY space.
*      INSERT lst_objtxt INTO TABLE gta_objtxt.
*
*      PERFORM f_set_comments.

*    WHEN 'FORW'.
*      "***SET SUBJECT LINE***
*      CONCATENATE 'Invoice ' gtp_bukrs ' ' gtp_belnr ' ' gtp_gjahr ': Forwarded'
*      INTO gst_doc_chng-obj_descr RESPECTING BLANKS.
*
*      "***SET MAIL BODY***
*      CONCATENATE 'Dear SAP' lv_system 'User,' INTO lst_objtxt-line SEPARATED BY space.
*      INSERT lst_objtxt INTO TABLE gta_objtxt.
*
*      "BLANK LINE
*      CLEAR lst_objtxt-line.
*      INSERT lst_objtxt INTO TABLE gta_objtxt.
*
*      CONCATENATE 'Invoice' gtp_bukrs gtp_belnr gtp_gjahr 'has been forwarded.'
*      INTO lst_objtxt-line SEPARATED BY space.
*      INSERT lst_objtxt INTO TABLE gta_objtxt.
*
*      PERFORM f_set_comments.

    WHEN 'REMI'.
      "***SET SUBJECT LINE***
      CONCATENATE 'Invoice ' GTP_BUKRS ' ' GTP_BELNR ' ' GTP_GJAHR ': Approval Reminder'
      INTO GST_DOC_CHNG-OBJ_DESCR RESPECTING BLANKS.

      "***SET MAIL BODY***
      CONCATENATE 'Dear SAP' LV_SYSTEM 'User,' INTO LST_OBJTXT-LINE SEPARATED BY SPACE.
      INSERT LST_OBJTXT INTO TABLE GTA_OBJTXT.

      "BLANK LINE
      CLEAR LST_OBJTXT-LINE.
      INSERT LST_OBJTXT INTO TABLE GTA_OBJTXT.

      CONCATENATE 'REMINDER: Invoice' GTP_BUKRS GTP_BELNR GTP_GJAHR 'is waiting for your approval.'
      INTO LST_OBJTXT-LINE SEPARATED BY SPACE.
      INSERT LST_OBJTXT INTO TABLE GTA_OBJTXT.

  ENDCASE.

  "BLANK LINE
  CLEAR LST_OBJTXT-LINE.
  INSERT LST_OBJTXT INTO TABLE GTA_OBJTXT.

  "Get the Comments
  CALL FUNCTION 'ZFI_GET_APPROVER_COMMENT'
    EXPORTING
      IMP_WORKITEM_ID = GTP_WORKITEM_TOP
      IMP_OBJECTKEY   = LTP_OBJKEY
    TABLES
      USER_COMMENT    = LTA_USER_COMMENT.

  "Print Comments
  IF NOT LTA_USER_COMMENT IS INITIAL.

    LST_OBJTXT-LINE = 'Workflow History: (User - Comment)'.
    INSERT LST_OBJTXT INTO TABLE GTA_OBJTXT.

    SORT LTA_USER_COMMENT DESCENDING BY TIMESTAMP.
    LOOP AT LTA_USER_COMMENT INTO LWA_USER_COMMENT.
      CONCATENATE LWA_USER_COMMENT-USERNAME '-' LWA_USER_COMMENT-COMMENTS INTO LST_OBJTXT-LINE SEPARATED BY SPACE.
      INSERT LST_OBJTXT INTO TABLE GTA_OBJTXT.
    ENDLOOP.

    "BLANK LINE
    CLEAR LST_OBJTXT-LINE.
    INSERT LST_OBJTXT INTO TABLE GTA_OBJTXT.
  ENDIF.


*Start of changes by AHMADT for CHG0163977 D30K930299
IF GTP_DECISION_TYPE = 'REMI' OR GTP_DECISION_TYPE = 'APPR'.
      "BLANK LINE
      CLEAR LST_OBJTXT-LINE.
      APPEND LST_OBJTXT TO GTA_OBJTXT.

      CLEAR LST_OBJTXT-LINE.
      LST_OBJTXT-LINE =  'You can access your workflow inbox by clicking on the SAP Portal (UWL) link below:'.
      APPEND LST_OBJTXT TO GTA_OBJTXT.

      CLEAR LST_OBJTXT-LINE.
      LST_OBJTXT-LINE = 'https://sapportal.gtna.gt.ds/irj/portal/UWL'.
      APPEND LST_OBJTXT TO GTA_OBJTXT.

      "BLANK LINE
      CLEAR LST_OBJTXT-LINE.
      APPEND LST_OBJTXT TO GTA_OBJTXT.

      CLEAR LST_OBJTXT-LINE.
      LST_OBJTXT-LINE = 'If you need help to process this work item, please see the support document at the link below: '.
      APPEND LST_OBJTXT TO GTA_OBJTXT.

      "BLANK LINE
      CLEAR LST_OBJTXT-LINE.
      APPEND LST_OBJTXT TO GTA_OBJTXT.

      CLEAR LST_OBJTXT-LINE.
      LST_OBJTXT-LINE = 'AP Invoice Approval Instructions'.
      APPEND LST_OBJTXT TO GTA_OBJTXT.

      CLEAR LST_OBJTXT-LINE.
      LST_OBJTXT-LINE = 'https://elink.enbridge.com/PoliciesResources/Finance/AccountsPayable/Pages/Training-Resources.aspx'.
      APPEND LST_OBJTXT TO GTA_OBJTXT.

      "BLANK LINE
      CLEAR LST_OBJTXT-LINE.
      APPEND LST_OBJTXT TO GTA_OBJTXT.
ENDIF.
*End of changes by AHMADT for CHG0163977 D30K930299

  CASE LV_SYSTEM.
    WHEN 'EAST'.
      LST_OBJTXT-LINE = 'If you have any questions please contact Accounts Payable Helpdesk at 1-855-252-1066 or APCAEastInquiry@spectraenergy.com'.
      INSERT LST_OBJTXT INTO TABLE GTA_OBJTXT.
    WHEN 'WEST'.
      LST_OBJTXT-LINE = 'If you have any questions please contact Accounts Payable Helpdesk at 1-855-252-1066 or APCAWestInquiry@spectraenergy.com'.
      INSERT LST_OBJTXT INTO TABLE GTA_OBJTXT.
  ENDCASE.

  "BLANK LINE
  CLEAR LST_OBJTXT-LINE.
  INSERT LST_OBJTXT INTO TABLE GTA_OBJTXT.

  CONCATENATE 'This is a SAP -' SY-SYSID 'generated e-mail. Do not reply.' INTO LST_OBJTXT-LINE SEPARATED BY SPACE.
  INSERT LST_OBJTXT INTO TABLE GTA_OBJTXT.
ENDFORM.                    " SET_MAIL_BODY
*&---------------------------------------------------------------------*
*&      Form  SET_MAILING_PARAMETERS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_MAILING_PARAMETERS .

  DATA: LTP_TAB_LINES  TYPE SY-TABIX,
        LST_OBJPACK    TYPE SOPCKLSTI1,
        LST_OBJBIN     TYPE SOLISTI1.

* Creation of the entry for the document
  DESCRIBE TABLE GTA_OBJTXT LINES LTP_TAB_LINES.
  CLEAR: LST_OBJPACK-TRANSF_BIN.
  LST_OBJPACK-HEAD_START = 1.
  LST_OBJPACK-HEAD_NUM   = 0.
  LST_OBJPACK-BODY_START = 1.
  LST_OBJPACK-BODY_NUM   = LTP_TAB_LINES.
  LST_OBJPACK-DOC_TYPE   = 'RAW'.
  INSERT LST_OBJPACK INTO TABLE GTA_OBJPACK.
ENDFORM.                    " SET_MAILING_PARAMETERS
*&---------------------------------------------------------------------*
*&      Form  F_SET_COMMENTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SET_COMMENTS .

  DATA: LTP_OBJKEY     TYPE SWO_TYPEID,
        LTP_COMMENTS   TYPE CHAR255.

  CONCATENATE GTP_BUKRS GTP_BELNR GTP_GJAHR INTO LTP_OBJKEY.

  IF GTP_COMMENT IS INITIAL.
    LTP_COMMENTS = 'NO USER COMMENT'.
  ELSE.
    LTP_COMMENTS = GTP_COMMENT.
  ENDIF.

  CALL FUNCTION 'ZFI_SET_APPROVERCOMMENT'
    EXPORTING
      IMP_WORKITEM_ID = GTP_WORKITEM_TOP
      IMP_OBJECT_KEY  = LTP_OBJKEY
      IMP_USERNAME    = SY-UNAME
      IMP_COMMENTS    = LTP_COMMENTS.

ENDFORM.                    " F_SET_COMMENTS

*&---------------------------------------------------------------------*
*&      Form  SET_FATCA_COMMNETS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_FATCA_COMMNETS USING P_UCOMM TYPE SYUCOMM.

  DATA: LTP_OBJKEY     TYPE SWO_TYPEID,
        LT_CONTAINER   TYPE STANDARD TABLE OF SWR_CONT,
        RETURN_CODE    TYPE SYSUBRC,
        LTP_COMMENTS   TYPE CHAR255,
        LV_COMMENTS    TYPE CHAR255.

  FIELD-SYMBOLS: <CONTAINER> LIKE LINE OF LT_CONTAINER.

  CALL FUNCTION 'SAP_WAPI_READ_CONTAINER'
    EXPORTING
      WORKITEM_ID      = GTP_WORKITEM_TOP
    IMPORTING
      RETURN_CODE      = RETURN_CODE
    TABLES
      SIMPLE_CONTAINER = LT_CONTAINER.

  READ TABLE LT_CONTAINER ASSIGNING <CONTAINER> WITH KEY ELEMENT = 'RCOFATCA'.
  CHECK <CONTAINER> IS ASSIGNED.
  IF <CONTAINER>-VALUE EQ 'X'.
    CONCATENATE GTP_BUKRS GTP_BELNR GTP_GJAHR INTO LTP_OBJKEY.
    LTP_COMMENTS =  'CODE WT & RPT TAX AMOUNTS BASED ON GEOGRAPHY OF SERVICES PERFORMED'.
    IF P_UCOMM NE 'COMPLETE'.
      SELECT SINGLE COMMENTS INTO LV_COMMENTS
                             FROM ZFIT_APPRL_CMNT
                             WHERE OBJECT_KEY  EQ LTP_OBJKEY
                             AND   WORKITEM_ID EQ GTP_WORKITEM_TOP
                             AND   USERNAME    EQ SY-UNAME.

      CHECK LTP_COMMENTS NE LV_COMMENTS.
    ENDIF.

    CALL FUNCTION 'ZFI_SET_APPROVERCOMMENT'
      EXPORTING
        IMP_WORKITEM_ID = GTP_WORKITEM_TOP
        IMP_OBJECT_KEY  = LTP_OBJKEY
        IMP_USERNAME    = SY-UNAME
        IMP_COMMENTS    = LTP_COMMENTS.
  ENDIF.

ENDFORM.                    " SET_FATCA_COMMNETS

*&---------------------------------------------------------------------*
*&      Form  CHECK_FATCA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LTP_EVENT  text
*      <--P_LV_FATCA  text
*----------------------------------------------------------------------*
FORM CHECK_FATCA CHANGING P_FATCA   TYPE ANY.

  DATA: LTA_CONTAINER TYPE STANDARD TABLE OF SWR_CONT,
        RETURN_CODE   TYPE SYSUBRC.

  FIELD-SYMBOLS: <CONTAINER> LIKE LINE OF LTA_CONTAINER.

  CLEAR P_FATCA.

  CALL FUNCTION 'SAP_WAPI_READ_CONTAINER'
    EXPORTING
      WORKITEM_ID      = GTP_WORKITEM_TOP
    IMPORTING
      RETURN_CODE      = RETURN_CODE
    TABLES
      SIMPLE_CONTAINER = LTA_CONTAINER.

  READ TABLE LTA_CONTAINER ASSIGNING <CONTAINER> WITH KEY ELEMENT = 'RCOFATCA'.
  CHECK <CONTAINER> IS ASSIGNED.
  IF <CONTAINER>-VALUE EQ 'X'.
    P_FATCA = <CONTAINER>-VALUE.
  ENDIF.

ENDFORM.                    " CHECK_FATCA
