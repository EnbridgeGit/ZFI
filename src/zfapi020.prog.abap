REPORT ZFAPI020 MESSAGE-ID 00  no standard page heading
             line-size 80 line-count 65.
*__________________________________________________________________
* NOTE:  E-MAILS WILL BE SENT OUT THROUGH SAPCONNECT.
*        ALL THE SETTING IN SAPCONNECT(Trans SCOT) HAS TO BE OK
*        BEFORE EXECUTING THIS ABAP.
*__________________________________________________________________
* Program      :  ZFAPI020
* Created ON   :  jANUARY 19, 2001.
* Created By   :  Mark Dufault
*__________________________________________________________________
* This program READS reguh DATA TO DETERMINE the records that need
* to be placed in the email file.  The records are used to send EFT
* emails to all people who received an EFT expense cheque deposit.
*__________________________________________________________________
* 2013/02/01 gymana   SDP39808 - Fix bug that was causing duplicate
*                                to be selected due to multiple REGUH
*                                entries.
* 2013/01/24 gymana   #SDP38863 - Change selection criteria for REGUH
*                                 to include payment method
* 2007/05/11 Mohammad #426 Changed to use SAPCONNECT system for
*                          sending emails.
* 2001/05/09 mdemeest #--- Changed selection from BSAK to REGUH
* 2001/03/28 mdemeest #--- Combine monies for same document taking into
*                          account positives & negatives.
* 2001/03/23 ndemeest #--- Travel advances 'KA'

TABLES: REGUH,                   "Settlement Data from Payment Program
        BNK_BATCH_HEADER,        "Batch Header Table           SDP39808
        LFB1.                    " Vendor file

Data:   amt like reguh-rbetr.

DATA:   BEGIN OF OUT_FILE,
         EFTVENDOR     LIKE LFB1-LIFNR,     "EMPLOYEE #        L= 10
         EFTAMOUNT(13) TYPE C,              "AMOUNT OF DEPOSIT L= 13
         EFTEMAIL      LIKE LFB1-INTAD,     "EMAIL ADDRESS     L= 130
         EFTDATE(8)    TYPE c,              "Date              L=8
       END OF OUT_FILE.

DATA: BEGIN OF RPT_FILE occurs 0,
         EFTVENDOR    LIKE LFB1-LIFNR,     "EMPLOYEE #          L= 10
         EFTEMAIL     LIKE LFB1-INTAD,     "EMAIL ADDRESS       L= 130
         EFTDATE(8)   TYPE C,              "TRANSACTION DATE    L= 8
         EFTAMOUNT    like bsak-wrbtr,   "AMOUNT OF DEPOSIT   L= 13

       END OF RPT_FILE.

DATA: OBJTXT LIKE SOLI OCCURS 10 WITH HEADER LINE.
DATA: RECLIST LIKE SOMLRECI1 OCCURS 5 WITH HEADER LINE.
DATA: DOC_CHNG LIKE SODOCCHGI1.
DATA: TAB_LINES LIKE SY-TABIX.

*___________________________________________________________________
* parameters
*__________________________________________________________________
SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME.

*Parameters:  RFCDEST(32) default 'Z_ACTIVEWORKS_EFT_EMAIL'.

Parameters:  P_CCODE like reguh-zbukr    memory id BUK    obligatory,
             P_DATE  like sy-datum       default sy-datum obligatory,
*             p_laufi like reguh-laufi    default 'EMPL'   obligatory,
             p_rzawe like reguh-rzawe  default 'F' obligatory, "SDP38863
             p_intad like lfb1-intad   obligatory              "SDP38863
                          default 'ontuglapnotify@spectraenergy.com'.
SELECTION-SCREEN END OF BLOCK BLK1.

*___________________________________________________________________
* Event : start-of-selection.... open file to OUTPUT
*____________________________________________________________________
START-OF-SELECTION.

    CLEAR OUT_FILE.

* Select all possible Employee vendor records from REGUH
  SELECT * FROM REGUH
    where laufd = p_date              "Date of run selected
*      and laufi = p_laufi            "Identification ie EMPL
      and rzawe = p_rzawe             "Payment Method        "SDP38863
      and xvorl = ' '                 "Non proposals
      and zbukr = p_ccode.            "Company code

     SELECT * FROM BNK_BATCH_HEADER                           "SDP39808
      WHERE LAUFD_F = REGUH-LAUFD                             "SDP39808
        AND LAUFI_F = REGUH-LAUFI.                            "SDP39808

*-----------------------------------------------------------------------
* Get VENDOR EMAIL ADDRESS
* A problem is indicated by putting an "X" in out_file-eftemail.
* ActiveWorks is looking for the "X" to send an email to AcctsPayable.
*-----------------------------------------------------------------------
     IF SY-SUBRC = '0'.                                       "SDP39808
        SELECT single * FROM LFB1
          WHERE LIFNR = REGUH-LIFNR
            and bukrs = reguh-zbukr.
          if sy-subrc = '0'.
             move lfb1-intad to rpt_file-eftemail.
             if lfb1-intad = space.                   "No email address
                move p_intad to rpt_file-eftemail.  "default to variant
             endif.
          else.
             move p_intad    to rpt_file-eftemail.
          endif.

*          move reguh-lifnr    to     rpt_file-eftvendor.
          if reguh-lifnr+0(1) = '0'.
             move reguh-lifnr+3(7)    to     rpt_file-eftvendor.
          else.
             move reguh-lifnr         to     rpt_file-eftvendor.
          endif.

          compute amt = reguh-rbetr * -1.
          move amt            to     rpt_file-eftamount.
          move reguh-laufd    to     rpt_file-eftdate.
          collect rpt_file.
       ENDIF.
     endselect.                                               "SDP39808
endselect.

   DOC_CHNG-OBJ_DESCR = TEXT-110.     "Email Subject

loop at rpt_file.
     REFRESH: RECLIST, OBJTXT.
     CLEAR:   RECLIST, OBJTXT.
       move rpt_file-eftvendor to out_file-eftvendor.
       move rpt_file-eftamount to out_file-eftamount.
       move rpt_file-eftemail  to out_file-eftemail.
       move rpt_file-eftdate   to out_file-eftdate.

   RECLIST-RECEIVER = OUT_FILE-EFTEMAIL. "Email Recipient
   RECLIST-REC_TYPE = 'U'.              "Recepient Type-Internet address
   RECLIST-EXPRESS = SPACE.             "Send Express - NO
   RECLIST-COM_TYPE = 'INT'.            "Transmission Method - Internet
   APPEND RECLIST.

* Email Contents - Max 255 characters per line
   OBJTXT = TEXT-101.
   APPEND OBJTXT.
   CONDENSE out_file-eftamount.
   CONCATENATE TEXT-DLR OUT_FILE-EFTAMOUNT TEXT-102
         OUT_FILE-EFTVENDOR TEXT-103 INTO OBJTXT SEPARATED BY SPACE.
   APPEND OBJTXT.
   OBJTXT = TEXT-106.
   APPEND OBJTXT.
   OBJTXT = TEXT-104.
   APPEND OBJTXT.
   OBJTXT = TEXT-105.
   APPEND OBJTXT.

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

*       call function 'Z_SEND_EFT_EMAIL' in background task
*         destination rfcdest
*         exporting eftvendor = out_file-eftvendor     "Vendor id
*                   eftamount = out_file-eftamount     "Amount deposited
*                   eftemail  = out_file-eftemail      "Email address
*                   eftdate   = out_file-eftdate.      "Date of Deposit
endloop.

*  commit work.


  sort rpt_file by eftvendor.

  perform print_report.

  skip 2.
  write: / text-end under text-001.

form print_report.
  loop at rpt_file.
    write: / rpt_file-eftvendor under text-011,
             rpt_file-eftemail  under text-013,
             rpt_file-eftdate   under text-012,
        (13) rpt_file-eftamount under text-014.
  endloop.

endform.

top-of-page.
write: /1 text-rpt,  sy-repid, 20 text-001,
                            50 text-dte, sy-datum, text-amp, sy-uzeit.
write: / text-clt under text-rpt, sy-mandt under sy-repid, sy-sysid,
       (10) p_date using edit mask '____/__/__' under text-001,
       text-pge under text-dte, sy-pagno under sy-datum.
uline.
write: /1 text-011,  12 text-012, 24 text-013, 57 text-014.
uline.
skip 1.
