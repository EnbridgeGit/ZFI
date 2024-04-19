REPORT ZFAPI019 no standard page heading LINE-SIZE 132 LINE-COUNT 65
       MESSAGE-ID ZS.
* This program will select all "EMPLOYEE" vendors and compare their
* email addresses to those on a file extracted from the Exchange Server.
* Two reports will be produced.
*  - invalid email addresses
*  - blank email addresses
* AP clerk will then address both reports and fix the addresses

* within SAP.  This is for the EFT Process.
*
*----------------------------------------------------------------------
* 2001/02/06 mdemeest #--- New request
*----------------------------------------------------------------------


TABLES: lfa1,                                   "vendor mast
        LFb1.                                   "vendor mast details
*---------------------------------------------------------------------
* contains all the email addresses from the exchange server
*---------------------------------------------------------------------
data:  begin of email  occurs 0,
      address_id(30) type c.
data:  end of email.

data:  email_address(2000) type c.
data:  email1_address(200) type c.
data:  email2_address(50)  type c.
data:  email3_address(50)  type c.
data:  email4_address(50)  type c.
data:  domain(30) type c.
*---------------------------------------------------------------------
data:  title(30) type c.
data:  input_rec.


SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME.

parameters:
     p_infile LIKE FILENAME-FILEINTERN lower case obligatory
               default '/usr/sap/interfaces/D30/IFAP007/email.txt',
     p_domain(30) type c  lower case obligatory default 'uniongas.com'.
SELECTION-SCREEN END OF BLOCK BOX.


SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-000.

parameters:
     p_rpt1 as checkbox default 'X',
     p_rpt2 as checkbox default 'X'.
SELECTION-SCREEN END OF BLOCK BOX1.

data: msg_text(50) type c.

*-------------------  TOP-OF-PAGE  ------------------------------------
top-of-page.
write: / text-rpt, sy-repid, 40 title,
         70 TEXT-DTE, SY-DATUM, TEXT-AMP, sy-uzeit.
write: / text-clt under text-rpt, sy-mandt under sy-repid,
         p_domain under title,
         text-pge under text-dte, sy-pagno under sy-datum.
write: /.
uline.
write: /.
write: /1 text-001,    30 text-002,        70 text-003.

*----------------------------------------------------------------------



start-of-selection.
*----------------------------------------------------------------------
* Open as input the file containing email addresses from the Exchange
* Server
*-----------------------------------------------------------------------
 OPEN DATASET p_infile FOR INPUT IN TEXT MODE MESSAGE MSG_TEXT.
 if sy-subrc ne 0.
    WRITE:/ 'Input File cannot be opened. Reason:', MSG_TEXT.
 endif.
*-----------------------------------------------------------------------
* Loop thru input file, selecting the email addresses and storing them
* in an internal table, called "EMAIL_ADDRESS".
*-----------------------------------------------------------------------
 do.
 READ DATASET p_infile into email_address.
 if sy-subrc <> '0'.           "End of file, exit do... enddo
    exit.
 endif.

 if email_address cs p_domain.
   perform find_address using email_address.
 endif.

 enddo.
*----------------------------------------------------------------------
* Sort Internal table of EMAIL address
*----------------------------------------------------------------------
 sort email.

*  loop at email.
*    write: / email-address_id.
*  endloop.
*----------------------------------------------------------------------
* if checkbox for report 1, execute the non-blank email address report
*----------------------------------------------------------------------
 if p_rpt1 = 'X'.
   move 'EMail Address Verification' to title.
   concatenate '%' p_domain into domain.  "eg %uniongas.com
   select * from lfb1
    where lifnr like 'E%'
      and intad like domain.
*---------------------------------------------------------------------
* Changes info on SAP to lower case so that names can be selected
*---------------------------------------------------------------------
 translate lfb1-intad using
         'AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYyZz'.


    read table email with key address_id = lfb1-intad binary search.
    if sy-subrc <> '0'.
       perform read_lfa1.
       write: / lfb1-lifnr under text-001,
                lfb1-intad under text-003.
       if sy-subrc = '0'.
          write lfa1-name1 under text-002.
       endif.
    endif.
   endselect.

   write: /.
   write: / text-004 under title.
 endif.
*-----------------------------------------------------------------------
* if checkbox for report 2, execute the blank email address report
*----------------------------------------------------------------------
 if p_rpt2 = 'X'.
   new-page.
    move 'EMail Addresses Required' to title.
    select * from lfb1
       where lifnr like 'E%'
         and intad = ' '.
       perform read_lfa1.
       write: / lfb1-lifnr under text-001.
       if sy-subrc = '0'.
          write: lfa1-name1 under text-002.
       endif.
    endselect.

   write: /.
   write: / text-004 under title.
 endif.
*--------------------------- FIND_ADDRESS ------------------------------
*
form find_address using email_info.
data:  email_info1(1000),
       email_info2(2000),
       email_info3(50),
       email_info4(2000).

 clear: email_info1,
        email_info2,
        email_info3,     "This contains the email address
        email_info4.

 split email_info at 'SMTP:' into email_info1 email_info2.

 split email_info2 at '%' into email_info3 email_info4.

 if sy-subrc = '4'.
    split email_info2 at '"' into email_info3 email_info4.
 endif.

 if sy-subrc = '4'.
    split email_info2 at ',' into email_info3 email_info4.
 endif.
*---------------------------------------------------------------------
* Changes info on Server file to lower case
*---------------------------------------------------------------------
 translate email_info3 using
         'AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYyZz'.
 append email_info3 to email.

endform.
*--------------------------  READ_LFA1  ------------------------------
*  Get vendor (employee) name
*---------------------------------------------------------------------
form read_lfa1.
  select single * from lfa1
     where lifnr = lfb1-lifnr.
endform.








