REPORT ZZMLTST1.
*-----------------------------------------------------------------------
* Developer:  ML DeMeester
* Date:       May 10,2001
* Purpose:    Test program to try out the Invoice Receipt for CARS.
*
*
*-----------------------------------------------------------------------
tables: bkpf.                                         "document table
parameters:
   p_amt    like bapiaccr01-amt_base,
   p_ref_no like bapiache03-obj_key.
Data:
  ache03 like bapiache03,                             "document header
  acpa00 like bapiacpa00,
  acap03 like bapiacap03  occurs 10 with header line,
  acgl03 like bapiacgl03  occurs 10 with header line,
  actx01 like bapiactx01  occurs 10 with header line,
  accr01 like bapiaccr01  occurs 10 with header line,
  ret2   like bapiret2    occurs 10 with header line.
data: w_rollback.

  perform document_header.
  perform vendor_address.
  perform vendor_line.
  perform tax_line.
  perform gl_line.
  perform vendor_amounts.
  perform post_info.

write: /.
write: / ret2.
write: /.
write: / 'ache03', ache03.
write: /.
write: / 'acap03', acap03.
write: /.
loop at accr01.
  write: / 'accr01', accr01.
endloop.
write: / 'ALL DONE'.

*--------------------DOCUMENT_HEADER----------------------------------
*-----------------------------------------------------------------------
form document_header.
*  ache03-obj_type    = 'BKPF'.                     "See table TTYP
  ache03-obj_type    = 'ZCARS'.                     "See table TTYP
*  ache03-obj_key     = p_ref_no.                    "Work Order
  ache03-obj_key     = '*UGL 2002'.
  ache03-obj_sys     = 'D30'.                      "Logical source
  ache03-username    = 'mdemeest'.                  "User name
  ache03-header_txt  = 'NO TERMS2'.
  ache03-comp_code   = 'UGL'.                       "Company Code
  ache03-doc_date    = '20020516'.                "Document Date
  ache03-pstng_date  = '20020516'.                "Posting date
  ache03-doc_type    = 'KN'.                        "Document Type
  ache03-ref_doc_no  = p_ref_no.                    "Reference
endform.
*---------------------  VENDOR_ADDRESS  --------------------------------
*-----------------------------------------------------------------------
form vendor_address.
*  acpa00-name       = 'test ml'.

*  acpa00-name_2     = 'test ml2'.
*  acpa00-postl_code = 'N7M 5J7'.
*  acpa00-city       = 'CHATHAM'.
*  acpa00-country    = 'ca'.
*  acpa00-street     = '100 bapi ave'.
*  acpa00-region     = 'ON'.
endform.


*------------------------- VENDOR_LINE (AccountPayable) ----------------
*-----------------------------------------------------------------------
form vendor_line.
  acap03-itemno_acc = '0000000001'.                       "Line number
*  acap03-vendor_no  = '0000023881'.                     "Vendor no.020
  acap03-vendor_no  = '0000018482'.                      "Vendor no.030
  acap03-pmnttrms   = 'N30'.                        "Payment terms
  acap03-item_text  = 'BAPI test 2001/08/01'.                "Text
  append acap03.

endform.
*--------------------------- GL_LINE  ----------------------------------
form gl_line.
  clear acgl03.
  acgl03-itemno_acc = '2'.
 acgl03-gl_account = '0000420010'.
  acgl03-item_text  = 'GL text - item 1'.
 acgl03-wbs_element = '01973095456'.
*  acgl03-orderid = '000000210040'.
  acgl03-tax_code = 'I0'.

  append acgl03.
  clear acgl03.
  acgl03-itemno_acc = '3'.
 acgl03-gl_account = '0000420010'.
  acgl03-item_text  = 'GL text - item 2'.
* acgl03-wbs_element = '01991005231'.
  acgl03-orderid = '000000210040'.

  acgl03-tax_code = 'I0'.

   append acgl03.

  clear acgl03.
  acgl03-itemno_acc = '4'.
  acgl03-gl_account = '0000420011'.
  acgl03-item_text  = 'GL text - item 3'.
* acgl03-wbs_element = '01991005231'.   "good
 acgl03-wbs_element = '01973095456'.    "closed in 030
* acgl03-orderid = '000000210040'.

  acgl03-tax_code = 'I0'.

  append acgl03.


endform.
*---------------------- VENDOR_AMOUNTS  --------------------------------
*-----------------------------------------------------------------------
form vendor_amounts.
  accr01-itemno_acc = '1'.                          "Line item
  accr01-currency   = 'CAD'.                        "Currency
  accr01-amt_base   = 100 * -1.                         "Amount
  accr01-amt_doccur = 100 * -1.
  accr01-disc_base  = 100 * -1.
  append accr01.


  accr01-itemno_acc = '2'.                          "Line item
  accr01-currency   = 'CAD'.                        "Currency
  accr01-amt_base   = 100.                                "Amount
*  accr01-curr_type  = '10'.
 accr01-amt_doccur = 100.
  accr01-disc_base  = 100.
  append accr01.

  accr01-itemno_acc = '3'.                          "Line item
  accr01-currency   = 'CAD'.                        "Currency
  accr01-amt_base   = 25 * -1.                                "Amount
* accr01-curr_type  = '10'.
 accr01-amt_doccur = 25 * -1.
  accr01-disc_base  = 25 * -1.
  append accr01.

  accr01-itemno_acc = '4'.                          "Line item
  accr01-currency   = 'CAD'.                        "Currency
  accr01-amt_base   = 25.                                "Amount
*  accr01-curr_type  = '10'.
 accr01-amt_doccur = 25.
  accr01-disc_base  = 25.
  append accr01.

*  accr01-itemno_acc = '5'.                          "Line item
*  accr01-currency   = 'CAD'.                        "Currency
*  accr01-amt_base   = 7.                                "Amount
* accr01-amt_doccur = 7.
* accr01-disc_base = 7.
*  append accr01.


endform.

form tax_line.
* actx01-itemno_acc = '5'.
*  actx01-gl_account = '0000256950'.
*  actx01-tax_code   = 'I0'.
*  actx01-acct_key = 'VST'.
*  append actx01.
endform.
*------------------------  POST_INFO -----------------------------------
* call the BAPI function module to post invoice
*-----------------------------------------------------------------------
form post_info.
  CALL FUNCTION 'BAPI_ACC_INVOICE_RECEIPT_CHECK'

      exporting
         documentheader         = ache03
         customercpd            = acpa00
      tables
         accountpayable         = acap03
         accountgl              = acgl03
         accounttax             = actx01
         currencyamount         = accr01
         return                 = ret2.

 write: / 'check - return code =', ret2, '*****'.

  CALL FUNCTION 'BAPI_ACC_INVOICE_RECEIPT_POST'

      exporting
         documentheader         = ache03
         customercpd            = acpa00
      tables
         accountpayable         = acap03
         accountgl              = acgl03
         accounttax             = actx01
         currencyamount         = accr01
         return                 = ret2.




   write: / 'post - return code =', ret2, '*****'.


   if ret2-type = 'S'.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
*      commit work.
   endif.
   if ret2-type = 'S'.
      select single * from bkpf
        where awtyp = ache03-obj_type
          and awkey = ache03-obj_key.
*          and awsys = ache03-obj_sys.
*          and bukrs = ache03-comp_code
*          and cpudt = sy-datum.
      write: / 'Document Number = ', bkpf-belnr.

      write: / ache03.
      write: /  'Yo MaryLou, successful'.
      write: / 'Document Number = ', bkpf-belnr.
   else.
      call function 'BAPI_TRANSACTION_ROLLBACK'.
      write: / 'Better luck next time MaryLou - Rollback performed'.
   endif.
write: / 'Document Number = ', bkpf-belnr.
WRITE: / ache03.
endform.





























