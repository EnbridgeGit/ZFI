REPORT ZFAPI041 MESSAGE-ID 00  no standard page heading.
*----------------------------------------------------------------------*
*  Author:      Mohammad T. Khan                                       *
*  Date:        June 2009.                                             *
*  Issue Log:   TR703                                                  *
*  Description:                                                        *
*  This program will report the unsuccessful eimails for Purchase      *
*  orders and Expense Reports. The information will be sent to the     *
*  related departments through email.                                  *
*  ********* N O T E **********                                        *
*        If new message numbers without description disply frequently  *
*  in future then we need to change the program or we can display it   *
*  from table T100 with ARBGB = 'XS' and MSGNR = message#.             *
*----------------------------------------------------------------------*
TABLES:
   SOST,                   "SAPoffice: Status log table
   SOOS,                   "SAPoffice: send process
   SOSTSTATUS,             "Structure for Status in SOST
   ADCP,                   "Person/Address Assignment (Bus.Address Ser.)
   ADRP,                   "Persons (Business Address Services)
   LFB1.                   "Vendor Master (Company Code)
TYPES T_TAB_SOST TYPE SOXSP2TAB.
DATA: W_ADRTP  TYPE SX_ADDRTYP VALUE 'INT',    "SAPconnect: Address type
      MAINTAB  TYPE T_TAB_SOST,                "Main table
      MAINTAB_LINE LIKE LINE OF MAINTAB,
      G_SEL_SENDER TYPE RANGE OF XUBNAME WITH HEADER LINE,
      ALL_WAITING VALUE 'X'.
DATA: OBJTXT LIKE SOLI OCCURS 10 WITH HEADER LINE.
DATA: RECLIST LIKE SOMLRECI1 OCCURS 5 WITH HEADER LINE.
DATA: DOC_CHNG LIKE SODOCCHGI1.
DATA: TAB_LINES LIKE SY-TABIX.
DATA: W_DATE_LOW(10),
      W_DATE_HIGH(10),
      EXPENSE_DATA TYPE C,
      OTF_DATA     TYPE C,
      W_REASON(33).

*----------------------  Selection Screen  ----------------------------*
SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME.
SELECTION-SCREEN SKIP 1.
SELECT-OPTIONS S_DATE FOR SOOS-SDDAT NO-EXTENSION.
SELECT-OPTIONS S_TIME FOR SOOS-SDTIM NO-EXTENSION.
PARAMETERS:
     EC_MAIL TYPE TEXT100 DEFAULT 'ontuglapnotify@spectraenergy.com',
     PO_MAIL TYPE TEXT100 DEFAULT 'psupport@uniongas.com'.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-104.
      PARAMETERS: B_RAW    AS CHECKBOX DEFAULT 'X',
                  B_OTF    AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK BOX2.
*SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME TITLE TEXT-103.
      PARAMETERS: B_ERROR    AS CHECKBOX DEFAULT 'X',
                  B_OK       AS CHECKBOX,
                  B_WAIT     AS CHECKBOX,
                  B_TRANST   AS CHECKBOX,
                  B_INCONS   AS CHECKBOX,
                  B_FUTURE   AS CHECKBOX,
                  B_RETRY    AS CHECKBOX,
                  B_DIRECT   AS CHECKBOX,
                  B_ACTIVE   AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK BOX3.
SELECTION-SCREEN END OF BLOCK BOX.

*----------------------  START OF SELECTION ---------------------------*
START-OF-SELECTION.
  PERFORM GET_ERROR_EAMIL_DATA.
  IF NOT MAINTAB[] IS INITIAL.
     IF EXPENSE_DATA = 'Y' AND B_RAW = 'X'.
        PERFORM SET_UP_HEADER USING TEXT-111 EC_MAIL.
        PERFORM SEND_EMAIL USING 'RAW'.    "Expense Cheque
     ENDIF.
     IF OTF_DATA = 'Y' AND B_OTF = 'X'.
        PERFORM SET_UP_HEADER USING TEXT-110 PO_MAIL.
        PERFORM SEND_EMAIL USING 'OTF'.    "Purchase Order
     ENDIF.
  ENDIF.
  IF ( EXPENSE_DATA = 'Y' AND B_RAW = 'X' ) OR
     ( OTF_DATA     = 'Y' AND B_OTF = 'X' ).
     MESSAGE I208 WITH TEXT-119.
  ELSE.
     MESSAGE I208 WITH TEXT-120.
  ENDIF.

*--------------------GET_ERROR_EAMIL_DATA------------------------------*
FORM GET_ERROR_EAMIL_DATA.

        MOVE: B_ERROR  TO  SOSTSTATUS-ERROR,
              B_OK     TO  SOSTSTATUS-OK,
              B_WAIT   TO  SOSTSTATUS-WAIT,
              B_TRANST TO  SOSTSTATUS-TRANSIT,
              B_INCONS TO  SOSTSTATUS-INCONS,
              B_FUTURE TO  SOSTSTATUS-FUTURE,
              B_RETRY  TO  SOSTSTATUS-RETRY,
              B_DIRECT TO  SOSTSTATUS-DIRECT,
              B_ACTIVE TO  SOSTSTATUS-ACTIVE.

    CALL FUNCTION 'SX_SNDREC_SELECT'
     EXPORTING
        SND_ART             = W_ADRTP
        SND_DATE            = S_DATE[]
        SND_TIME            = S_TIME[]
*       DEL_DATE            =
*       DEL_TIME            =
        STATUS              = SOSTSTATUS
*        NOTIFICATIONS       = g_stanot
*        SENDER              = g_sel_sender[]
*        MAXSEL              = g_maxsel
*        MAXSEL              = 500
        ALL_WAITING         = ALL_WAITING
     IMPORTING
       SNDRECS             =  MAINTAB.

 IF SY-SUBRC = 0.
    LOOP AT MAINTAB INTO MAINTAB_LINE WHERE OBJTP = 'RAW'.
         IF SY-TFILL > 0.
            MOVE 'Y' TO EXPENSE_DATA.
         ENDIF.
         EXIT.
    ENDLOOP.
    LOOP AT MAINTAB INTO MAINTAB_LINE WHERE OBJTP = 'OTF'.
         IF SY-TFILL > 0.
            MOVE 'Y' TO OTF_DATA.
         ENDIF.
         EXIT.
    ENDLOOP.
    LOOP AT MAINTAB INTO MAINTAB_LINE WHERE STATUS <> '806'.
         SELECT PERSNUMBER INTO ADCP-PERSNUMBER
           FROM ADCP
          WHERE SO_KEY = MAINTAB_LINE-ADRNR.

         SELECT NAME_LAST INTO ADRP-NAME_LAST
           FROM ADRP
          WHERE PERSNUMBER = ADCP-PERSNUMBER.
           MOVE ADRP-NAME_LAST TO MAINTAB_LINE-MSGV1.
         MODIFY MAINTAB FROM MAINTAB_LINE.
         ENDSELECT.
         ENDSELECT.
    ENDLOOP.
*Add Latest Vendor Number using ERDAT - Date of Creation
    LOOP AT MAINTAB INTO MAINTAB_LINE.
       SELECT SINGLE LIFNR INTO LFB1-LIFNR
         FROM LFB1
        WHERE INTAD = MAINTAB_LINE-MSGV1
          AND ERDAT =
     ( SELECT MAX( ERDAT ) FROM LFB1 WHERE INTAD = MAINTAB_LINE-MSGV1 ).
          IF SY-SUBRC = 0.
             WRITE LFB1-LIFNR TO MAINTAB_LINE-FORNO NO-ZERO.
             MODIFY MAINTAB FROM MAINTAB_LINE.
          ENDIF.
    ENDLOOP.
 ENDIF.

 ENDFORM.

*----------------------- SET UP HEADER   ------------------------------*
FORM SET_UP_HEADER USING TEXT1 W_MAIL.
     DATA: W_TEXT(25) TYPE C,
           W_SYMBOLIC(7) TYPE C VALUE ' Error '.

     WRITE S_DATE-LOW  TO W_DATE_LOW  DD/MM/YYYY.
     WRITE S_DATE-HIGH TO W_DATE_HIGH DD/MM/YYYY.
     IF S_DATE-HIGH <> SPACE.
        CONCATENATE TEXT1 W_DATE_LOW TEXT-113 W_DATE_HIGH
            INTO DOC_CHNG-OBJ_DESCR SEPARATED BY SPACE. "Email Subject
     ELSE.
        CONCATENATE TEXT1 TEXT-114 W_DATE_LOW
            INTO DOC_CHNG-OBJ_DESCR SEPARATED BY SPACE. "Email Subject
     ENDIF.
     IF B_ERROR  = 'X'   AND B_OK = SPACE     AND B_WAIT   = SPACE AND
        B_TRANST = SPACE AND B_INCONS = SPACE AND B_FUTURE = SPACE AND
        B_RETRY  = SPACE AND B_DIRECT = SPACE AND B_ACTIVE = SPACE.
        "DO NOTHING
     ELSE.
        REPLACE W_SYMBOLIC WITH '' INTO DOC_CHNG-OBJ_DESCR.
     ENDIF.
     RECLIST-RECEIVER = W_MAIL.                        "Email Recipient
     OBJTXT = TEXT-101.
     APPEND OBJTXT.
     OBJTXT = TEXT-102.
     APPEND OBJTXT.
ENDFORM.
*---------------------- SEND EMAIL ------------------------------------*
FORM SEND_EMAIL USING W_OBJTP.    "W_OBJTP = RAW or OTF"

DATA: KOUNT(3) TYPE N,
      W_DATE(10),
      W_TIME(8).

LOOP AT MAINTAB INTO MAINTAB_LINE WHERE OBJTP = W_OBJTP.
   KOUNT = KOUNT + 1.
   RECLIST-REC_TYPE = 'U'.              "Recepient Type-Internet address
   RECLIST-EXPRESS = SPACE.             "Send Express - NO
   RECLIST-COM_TYPE = 'INT'.            "Transmission Method - Internet
   APPEND RECLIST.

* Email Contents - Max 255 characters per line
  CASE MAINTAB_LINE-STATUS.
       WHEN '73'.
       CONCATENATE MAINTAB_LINE-STATUS '-' TEXT-117 INTO W_REASON.
       WHEN 'W'.
       CONCATENATE '672'               '-' TEXT-118 INTO W_REASON.
       WHEN '806'.
       CONCATENATE MAINTAB_LINE-STATUS '-' TEXT-115 INTO W_REASON.
       WHEN '816'.
       CONCATENATE MAINTAB_LINE-STATUS '-' TEXT-116 INTO W_REASON.
       WHEN OTHERS.
       CONCATENATE MAINTAB_LINE-STATUS '-'          INTO W_REASON.
  ENDCASE.
   CONCATENATE MAINTAB_LINE-STAT_DATE+0(4) MAINTAB_LINE-STAT_DATE+4(2)
               MAINTAB_LINE-STAT_DATE+6(2) INTO W_DATE SEPARATED BY '/'.
   CONCATENATE MAINTAB_LINE-STAT_TIME+0(2) MAINTAB_LINE-STAT_TIME+2(2)
               MAINTAB_LINE-STAT_TIME+4(2) INTO W_TIME SEPARATED BY ':'.
*   CONDENSE: MAINTAB_LINE-OBJNO,  MAINTAB_LINE-OBJTP,
*             MAINTAB_LINE-STAT_DATE, MAINTAB_LINE-STAT_TIME,
*             MAINTAB_LINE-MSGV1,     MAINTAB_LINE-STATUS.
   CONCATENATE KOUNT TEXT-109 MAINTAB_LINE-OBJNO+7(5) MAINTAB_LINE-OBJTP
               W_DATE W_TIME  MAINTAB_LINE-FORNO
               INTO OBJTXT SEPARATED BY SPACE.
               MOVE W_REASON TO OBJTXT+44.
               MOVE MAINTAB_LINE-MSGV1 TO OBJTXT+79.
   APPEND OBJTXT.
   CLEAR W_REASON.
ENDLOOP.

   DESCRIBE TABLE OBJTXT LINES TAB_LINES.
   READ TABLE OBJTXT INDEX TAB_LINES.
   DOC_CHNG-DOC_SIZE = ( TAB_LINES - 1 ) * 255 + STRLEN( OBJTXT ).

* Sending the email
CALL FUNCTION 'SO_NEW_DOCUMENT_SEND_API1'
  EXPORTING
    DOCUMENT_DATA                    = DOC_CHNG
*   DOCUMENT_TYPE                    = 'RAW'
    PUT_IN_OUTBOX                    = 'X'
    COMMIT_WORK                      = 'X'
* IMPORTING
*   SENT_TO_ALL                      =
*   NEW_OBJECT_ID                    =
  TABLES
*    OBJECT_HEADER                    = OBJHEAD
    OBJECT_CONTENT                   = OBJTXT
*   CONTENTS_HEX                     =
*   OBJECT_PARA                      =
*   OBJECT_PARB                      =
    RECEIVERS                        = RECLIST
 EXCEPTIONS
   TOO_MANY_RECEIVERS               = 1
   DOCUMENT_NOT_SENT                = 2
   DOCUMENT_TYPE_NOT_EXIST          = 3
   OPERATION_NO_AUTHORIZATION       = 4
   PARAMETER_ERROR                  = 5
   X_ERROR                          = 6
   ENQUEUE_ERROR                    = 7
   OTHERS                           = 8
          .
IF SY-SUBRC <> 0.
 MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.
REFRESH: RECLIST, OBJTXT.
CLEAR:   RECLIST, OBJTXT.

ENDFORM.
