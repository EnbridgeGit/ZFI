*****           Implementation of object type ZUSR01DOHR           *****
INCLUDE <OBJECT>.
BEGIN_DATA OBJECT. " Do not change.. DATA is generated
* only private members may be inserted into structure private
DATA:
" begin of private,
"   to declare private attributes remove comments and
"   insert private attributes here ...
" end of private,
  BEGIN OF KEY,
      ID LIKE USR01-BNAME,
  END OF KEY,
      EMAILADDRESS TYPE ADR6-SMTP_ADDR.
END_DATA OBJECT. " Do not change.. DATA is generated

GET_PROPERTY EMAILADDRESS CHANGING CONTAINER.
CALL FUNCTION 'ZWF_FIND_EMAIL'
  EXPORTING
    ID    = object-key-ID
  IMPORTING
    EMAIL = OBJECT-EMAILADDRESS.

SWC_SET_ELEMENT CONTAINER 'EmailAddress' OBJECT-EMAILADDRESS.
END_PROPERTY.

BEGIN_METHOD CREATEINSTANCE CHANGING CONTAINER.
DATA:
      USR_OBJ TYPE SWC_OBJECT,
      ID    TYPE USR01-BNAME,
      USER TYPE WFSYST-INITIATOR.

SWC_GET_ELEMENT CONTAINER 'User' USER.
ID = USER+2(12).
swc_create_object USR_OBJ 'USR01DOHR' ID.

SWC_SET_ELEMENT CONTAINER 'USR_OBJ' USR_OBJ.
END_METHOD.

BEGIN_METHOD GETWAITINGAGENT CHANGING CONTAINER.
DATA:
      ID TYPE SWWWIHEAD-WI_ID,
      deadline_ID TYPE SWWWIHEAD-WI_ID,
      AGENT TYPE SWC_OBJECT,
      ltp_user TYPE username,
      LTA_AGENT TYPE TABLE OF SWRAGENT WITH HEADER LINE.

SWC_GET_ELEMENT CONTAINER 'ID' ID.

SELECT SINGLE WI_ID FROM SWWWIHEAD INTO deadline_ID WHERE
  WI_TYPE = 'W' AND
  WI_STAT IN ('READY', 'STARTED') AND
  WI_CHCKWI = ID.

IF NOT deadline_ID IS INITIAL.
  CALL FUNCTION 'SAP_WAPI_WORKITEM_RECIPIENTS'
    EXPORTING
      WORKITEM_ID = deadline_ID
    TABLES
      RECIPIENTS  = LTA_AGENT.

  READ TABLE LTA_AGENT WITH KEY OTYPE = 'US'.
  IF SY-SUBRC = 0.
    LTP_USER = LTA_AGENT-OBJID.
    SWC_CREATE_OBJECT AGENT 'USR01DOHR' LTP_USER.
  ENDIF.
ENDIF.

SWC_SET_ELEMENT CONTAINER 'Agent' AGENT.
END_METHOD.

BEGIN_METHOD FINDDEADLINEDATE CHANGING CONTAINER.
DATA:
      CURRENTDATE TYPE SYST-DATUM,
      DEADLINEDATE TYPE SYST-DATUM,
      PERIOD TYPE ZFIT_REM_DAYS-REM_DAYS,
      ltp_fcal TYPE wfcid,
      WFTYPE TYPE ZFIT_REM_DAYS-WF_TYPE.
SWC_GET_ELEMENT CONTAINER 'CurrentDate' CURRENTDATE.
SWC_GET_ELEMENT CONTAINER 'Period' PERIOD.
SWC_GET_ELEMENT CONTAINER 'Wftype' WFTYPE.

* Get Factory Calendar
SELECT SINGLE fcal FROM zfit_fcalendar INTO ltp_fcal.
IF sY-SUBRC NE 0.
  exit_return 9001  'Factory Calendar Not Maintained ' ' ' ' ' ' '.
ENDIF.

IF Period IS INITIAL.
  SELECT SINGLE REM_DAYS FROM ZFIT_REM_DAYS INTO PERIOD
  WHERE WF_TYPE = WFTYPE.
  IF SY-SUBRC NE 0.
    PERIOD = 5.
  ENDIF.
ENDIF.

DO PERIOD TIMES.
  CURRENTDATE = CURRENTDATE + 1.
  CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
    EXPORTING
      DATE                         = CURRENTDATE
      FACTORY_CALENDAR_ID          = ltp_fcal
    IMPORTING
      DATE                         = DEADLINEDATE
    EXCEPTIONS
      CALENDAR_BUFFER_NOT_LOADABLE = 1
      CORRECT_OPTION_INVALID       = 2
      DATE_AFTER_RANGE             = 3
      DATE_BEFORE_RANGE            = 4
      DATE_INVALID                 = 5
      FACTORY_CALENDAR_NOT_FOUND   = 6
      OTHERS                       = 7.
  IF SY-SUBRC <> 0.
    exit_return 9001  'Subrc value of' sy-subrc 'Returned' ' '.
  ELSE.
    CURRENTDATE = DEADLINEDATE.
  ENDIF.

ENDDO.

SWC_SET_ELEMENT CONTAINER 'DeadlineDate' DEADLINEDATE.
END_METHOD.

BEGIN_METHOD FINDUSERDOA CHANGING CONTAINER.
DATA:
      AMOUNT TYPE ZFIT_DOA_NEW-AMOUNT,
      DOCTYPE TYPE ZFIT_DOA_NEW-DOC_TYPE,
      TCODE TYPE ERG001-TCODE,
      AGENT TYPE WFSYST-AGENT,
      CURRENCY TYPE BKPF-WAERS,
      DOA TYPE LXE_NUMBER-STAT.
DATA: ltp_log_dest   TYPE tb_rfcdest,
      ltp_pernr TYPE p_pernr,
      ltp_lookup_key TYPE zfi_lookup_key,
      ltp_doa   TYPE zfi_doa_validity,
      MGR_GRADE TYPE TRFGR,
      W_DOA LIKE ZFIT_DOA_NEW,
      UNAME LIKE SY-UNAME,
      w_sys type CHAR02.

DATA: lta_infty_0008 TYPE hrfrpbs4_infty0008_tab,
      lst_infty_0008 TYPE hrfrpbs4_infty0008.

SWC_GET_ELEMENT CONTAINER 'Amount' AMOUNT.
SWC_GET_ELEMENT CONTAINER 'DocType' DOCTYPE.
SWC_GET_ELEMENT CONTAINER 'Tcode' TCODE.
SWC_GET_ELEMENT CONTAINER 'Agent' AGENT.
SWC_GET_ELEMENT CONTAINER 'Currency' CURRENCY.

IF Currency is initial.
  Currency  = 'USD'.
ENDIF.
UNAME = AGENT+2(12).

*  exit_return 9001  'User ' UNAME 'does not have Employee Number' ' '.
ltp_lookup_key = UNAME.
IF sy-SYSID CP '*EC'.
  w_sys = 'US'.
ELSEIF sy-SYSID CP '*11'.
  w_sys = 'SW'.
ELSE.
  w_sys = 'UG'.
ENDIF.


*   Validate DOA
CALL FUNCTION 'ZFI_DOA_VALIDATION'
  EXPORTING
    IMP_SYSTEM     = w_sys
    IMP_LOOKUP_KEY = ltp_lookup_key
    IMP_TCODE      = Tcode
    IMP_DOC_TYPE   = DOCTYPE
    IMP_AMOUNT     = amount
    IMP_WAERS      = currency
  IMPORTING
    EXP_DOA        = ltp_doa
  EXCEPTIONS
    BAD_LOOKUP_KEY = 1
    NO_DOA_FOUND   = 2
    NO_GRADE       = 3
    OTHERS         = 4.

CASE SY-SUBRC.
  WHEN 0.
    DOA = ltp_doa.
  WHEN 1.
    exit_return 9001  'User ' UNAME 'does not have Employee Number' ' '.
  WHEN 2.
    exit_return 9001  'No DoA information available for ' UNAME ' ' ' '.
  WHEN 3.
    exit_return 9001  'Manager grade not defined for  ' UNAME ' ' ' '.
  WHEN 4.
    exit_return 9001  'Error during DoA determination for ' UNAME ' ' ' '.
ENDCASE.

SWC_SET_ELEMENT CONTAINER 'DoA' DOA.
END_METHOD.

BEGIN_METHOD MSGPOPUP CHANGING CONTAINER.
DATA:
      VAR1 TYPE WFSYST-TASK_NAME,
      VAR2 TYPE WFSYST-TASK_NAME.
SWC_GET_ELEMENT CONTAINER 'var1' VAR1.
SWC_GET_ELEMENT CONTAINER 'var2' VAR2.

message i000(ZFI_WORKFLOW) with var1 var2 ' ' ' '.
END_METHOD.

BEGIN_METHOD POPUPTEXTINPUT CHANGING CONTAINER.
DATA:
      TEXT TYPE TOAER-TEXT,
      TEXT1 TYPE TOAER-TEXT,
      VALUE TYPE TOAER-TEXT,
      VALUE1 TYPE TOAER-TEXT,
      lv_hide TYPE char1,
      lv_twoline type char1,
      TITLE TYPE TF090-DEP_FIELDS,
      ANS TYPE SYUCOMM,
      EXCLUSION TYPE USR01-BNAME OCCURS 0,
      UID TYPE USR01-BNAME,
      ROLE TYPE AGR_NAME occurs 0,
      UROLE TYPE AGR_NAME,
      wa_role type agr_name,
      O_USER TYPE swc_object,
      OBJKEY TYPE SWO_TYPEID,
      WIID TYPE  sww_wiid,
      TUPDATE TYPE char1,
      SINCLUDE TYPE char1,
      w_tab type ZFIT_APPRL_CMNT,
      IT_ROLE like AGR_LOGSYS occurs 0 with header line,
      T_EXCL LIKE ZFIT_TNE_ADMIN occurs 0 with header line.

SWC_GET_ELEMENT CONTAINER 'Text' TEXT.
SWC_GET_ELEMENT CONTAINER 'Text1' TEXT1.
SWC_GET_ELEMENT CONTAINER 'Hide' lv_hide.
SWC_GET_ELEMENT CONTAINER 'Title' TITLE.
SWC_GET_TABLE CONTAINER 'Role' Role.
SWC_GET_TABLE CONTAINER 'Exclusion' EXCLUSION.
SWC_GET_ELEMENT CONTAINER 'ObjectKey' OBJKEY.
SWC_GET_ELEMENT CONTAINER 'WIID' WIID.
SWC_GET_ELEMENT CONTAINER 'TableUpdate' TUPDATE.
SWC_GET_ELEMENT CONTAINER 'SelfInclude' SINCLUDE.

loop at exclusion into uid.
  t_excl-tneadmin = uid.
  append t_excl.
endloop.

loop at ROLE into wa_role.
  it_role-agr_name = wa_role.
  append it_role.
endloop.

IF Text = 'SELECT USER'.
  lv_twoline = 'X'.
ENDIF.

CALL FUNCTION 'Z_POPUP_TEXT_INPUT'
  EXPORTING
    SOURCETEXT   = Text
    TITEL        = Title
    START_COLUMN = 25
    START_ROW    = 6
    self_include = SINCLUDE
    SOURCETEXT1  = Text1
    hide         = lv_hide
    twoline      = lv_twoline
  IMPORTING
    ANSWER       = ANS
    USERROLE     = UROLE
  TABLES
    T_EXCLUDE    = t_excl
    IM_ROLE      = IT_role
  CHANGING
    TARGETTEXT   = Value
    TARGETTEXT1  = Value1.
IF TUPDATE = 'X' and ANS NE 'CANC'.
  clear w_tab.
  w_tab-mandt = sy-mandt.
  w_tab-object_key = objkey.
  w_tab-workitem_id = wiid.
  w_tab-username = sy-uname.
  IF lv_twoline = 'X'.
    w_tab-comments = value1.
  else.
    w_tab-comments = value.
  endif.

  CALL FUNCTION 'BKK_CHANGE_TO_TIMESTAMP'
* EXPORTING
*   DATLO_IV           = SY-DATUM
*   TIMLO_IV           = SY-UZEIT
*   TZONE_IV           = SY-ZONLO
   IMPORTING
     TIMESTAMP_EV       = w_tab-timestamp
            .
  modify ZFIT_APPRL_CMNT from w_tab.
ENDIF.

IF Text = 'SELECT USER' and ANS NE 'CANC'.
  swc_create_object O_USER 'USR01DOHR' VALUE.
  SWC_SET_ELEMENT CONTAINER 'UserObj' O_USER.
  Concatenate 'US' VALUE into VALUE.
  Condense Value no-gaps.
ENDIF.
IF Text CS 'Comments' OR Text CS 'Reason'.
  Concatenate sy-uname ':-' VALUE into VALUE separated by space.
ENDIF.

IF lv_twoline = 'X'.
  Concatenate sy-uname ':-' VALUE1 into VALUE1 separated by space.
ENDIF.

SWC_SET_ELEMENT CONTAINER 'Choice' ANS.
SWC_SET_ELEMENT CONTAINER 'Value' VALUE.
SWC_SET_ELEMENT CONTAINER 'Value1' VALUE1.
SWC_SET_ELEMENT CONTAINER 'UserRole' UROLE.

END_METHOD.

BEGIN_METHOD READ_USER_COMMENT CHANGING CONTAINER.
DATA: w_doc_id LIKE  SOFOLENTI1-DOC_ID,
      W_IND TYPE I,
      w_doc type swc_object,
      w_FOLDERTYPE type SO_FOL_TP,
      W_FOLDERYEAR type SO_FOL_yr,
      w_FOLDERNUMBER type SO_FOL_no,
      w_TYPE type SO_DOC_TP,
      W_YEAR type SO_DOC_yr,
      w_userid like wfsyst-agent,
      w_oneline type char80,
      w_one255 type char255,
      w_NUMBER type SO_DOC_no.
DATA: IT_CONT TYPE TABLE OF SOLISTI1,
      W_CONT LIKE SOLISTI1.

SWC_GET_ELEMENT CONTAINER 'CommentObj' w_doc.
SWC_GET_ELEMENT CONTAINER 'userid' w_userid.

SWC_GET_PROPERTY w_doc 'FolderType' w_FOLDERTYPE.
SWC_GET_PROPERTY w_doc 'FolderYear' W_FOLDERYEAR.
SWC_GET_PROPERTY w_doc 'FolderNumber' w_FOLDERNUMBER.
SWC_GET_PROPERTY w_doc 'Type' w_TYPE.
SWC_GET_PROPERTY w_doc 'Year' W_YEAR.
SWC_GET_PROPERTY w_doc 'Number' w_NUMBER.

CONCATENATE w_FOLDERTYPE W_FOLDERYEAR w_FOLDERNUMBER
            w_TYPE W_YEAR w_NUMBER
   into w_doc_id.
CONDENSE w_doc_id NO-GAPS.

CALL FUNCTION 'SO_DOCUMENT_READ_API1'
  EXPORTING
    DOCUMENT_ID                = w_doc_id
  TABLES
    OBJECT_CONTENT             = IT_CONT
  EXCEPTIONS
    DOCUMENT_ID_NOT_EXIST      = 1
    OPERATION_NO_AUTHORIZATION = 2
    X_ERROR                    = 3
    OTHERS                     = 4.

IF sy-subrc = 0.
  LOOP AT IT_CONT INTO W_CONT.
    w_ind = sy-tabix.
    W_CONT = W_CONT+3.
    MODIFY IT_CONT FROM W_CONT INDEX W_IND.
  ENDLOOP.
  READ TABLE IT_CONT INTO W_CONT INDEX 1.
  if sy-subrc = 0.
    w_oneline = w_cont+0(65).
    CONCATENATE w_userid ':' w_oneline into w_oneline SEPARATED BY space.
    w_one255 = w_cont+0(235).
    CONCATENATE w_userid ':' w_one255 into w_one255 SEPARATED BY space.
  endif.
ENDIF.
SWC_SET_ELEMENT CONTAINER 'OnelineComment' w_oneline.
SWC_SET_ELEMENT CONTAINER 'oneline255' w_one255.
SWC_SET_TABLE CONTAINER 'Contents' IT_CONT.
END_METHOD.
