*loder, drew & associates
*sample sap r-3 data extraction program

************************************************************************
*		PROGRAM NAME			: YFAPAUD2				*	
*		PROJECT				: Finance					*
*		SUBSYSTEM				: FI					*
*		AUTHOR				: IMProgrammer				*
*		DATE WRITTEN				: 01/03/1998				*
*		TABLES					: TVZBT,PAYR,LFA1,LFB1,BKPF,BSEG.	*
*												*
*	   	TRANSACTION									*
*		REPORT(S)				: AP INVOICE TRANSACTIONS		*
*		INPUT FILES				: N/A					*
*	 	OUTPUT FILES				: /usrl/$sys/fin/outgoing/audinv		*
*												*
*	 	SAP RELEASE				: 3.1h					*
*  PROGRAM DESCRIPTION: Output AP invoice information                  *
*                       for Loder Drew Audit                           *
************************************************************************
*REVISION LOG                                                          *
*                                                                      *
*LOG  DATE       AUTHOR     DESCRIPTION                                *
*---- - -------- -------  ---------------------------------------------*
* 004 99/06/07   mdemeest   Changed cheque # from 8 to 13 characters   *
*                                                                      *
* 001 03/11/98   Dchanger1  Add Symbolic for Instance, Get Prog        *
*                           to output unix Dataset.                    *
*                                                                      *
* 002 03/23/98   Kchanger2  Add Doc type to file,                      *
*                           Chq PAYR select for check range.           *
*                                                                      *
* 003 99/05/03   MDEMEEST   Changed company code from parameter to     *
*                           selection
* 004 2002/02/06   mdemeest   added voucher number (payr-laufi)        *
************************************************************************
*report yfapaud2 line-size 255 line-count 58      "mdemeest 99/05/03
REPORT ZFAPAUD2 LINE-SIZE 255 LINE-COUNT 58       "mdemeest 99/05/03
                              NO STANDARD PAGE HEADING
                              MESSAGE-ID GR.

*************** TABLES NEEDED  ****************
tables:
TVZBT,                                 " Payment terms description
PAYR,                                  " Payment transfer medium file
LFA1,                                  " Vendor Master
LFB1,                                  " Vendor Master (company)
BKPF,                                  " Accounting document segment
BSEG.                                  " Account document header

data:	wk_lifnr like lfa1-lifnr,
WS_SYMBOLIC(4) TYPE C VALUE '$sys',                         "Log 001
VEN_CTR TYPE I,
INV_CTR  TYPE I.

DATA: BEGIN OF INVOICES OCCURS 0,
LIFNR(15) TYPE C,                      "Vendor #
NAME(15) TYPE C,                       "Vendor Short Name
XBLNR(20) TYPE C,                      "Invoice #
BUZEI(02) TYPE C,                      "Line item #
MENGE(07) TYPE C,                      "Line item quantity
SGTXT(15) TYPE C,                      "Line description
MATNR(15) TYPE C,                      "Part # (Material)
PEINH(08) TYPE C,                      "Line unit cost
WRBTR(13) TYPE C,                      "Gross Invoice
SKNTO(13) TYPE C,                      "Discount
NEBTR(13) TYPE C,                      "Net amt paid
BUDAT(09) TYPE C,                      "Invoice date
*chect(08) type c,                      "Check number
CHECT(13) TYPE C,                      "Check number           99/06/07
LAUFD(09) TYPE C,                      "Check date
BELNR(10)  TYPE C,                     "Batch-Voucher # (Doc #)
RWBTR(13) TYPE C,                      "Net check amount

VOIDC(06) TYPE C,                      "Void check info
VOIDR(02) TYPE C,                      "Void check reason code
EBELN(10) TYPE C,                      "PO#
BLDAT(09)  TYPE C,                     "Data entry date
BUKRS(04) TYPE C,                      "Division/Subsidiary
PSWSL(04) TYPE C,                      "Transaction currency key
MISCD(07) TYPE C,                      "Miscellaneous
DOCTYP(2) TYPE C,                      "Doc type#LOG 002
AUGB1(10) TYPE C,                      "Clearing Doc
laufi  like payr-laufi,                "Batch/Voucher Number        "004
end of invoices.

SELECT-OPTIONS: CO_CODE FOR BSEG-BUKRS OBLIGATORY NO INTERVALS.     "003
parameters:
*co_code#like bseg-bukrs obligatory,                                "003
FYEAR LIKE BSEG-GJAHR  OBLIGATORY.
select-options:
	doctype   for bkpf-blart  obligatory,
	acctnum  for bseg-hkont obligatory,
	chkrang   for payr-chect.
parameters:
*outfile1(34) default '/usrl/$sys/fin/outgoing/audinv'." mdemeest
OUTFILE1(60) DEFAULT '/usr/sap/interfaces/$sys/CFMM001/zfapaud2.sap'
                                           LOWER CASE.       "mdemeest
* --------------------------------- START OF SELECTION  ----------------
start-of-selection.

*      Set up Path with appropriate system pointer
  REPLACE WS_SYMBOLIC WITH SY-SYSID INTO OUTFILE1.          "Log 001
  condense outfile1 no-gaps.
                                                            "Log 001
  open dataset outfile1 for output in text mode.
  CLEAR INVOICES.
  clear lfa1.
  clear lfb1.
  clear payr.
  clear bkpf.
  clear bseg.
  ven_ctr = 0.
  inv_ctr  = 0.
  select * from bkpf
*  where bukrs = co_code                "Company code               003
   WHERE BUKRS IN CO_CODE                "Company code              003
  AND     GJAHR = FYEAR                "fiscal year
  AND     BLART IN DOCTYPE.
                                       "document type

    SELECT * FROM BSEG
    WHERE BUKRS = BKPF-BUKRS           "Company code
    AND     BELNR  = BKPF-BELNR        "Document number
    AND     GJAHR  = BKPF-GJAHR        "Fiscal year(s)
     AND     HKONT IN ACCTNUM.
      IF SY-SUBRC = 0.
        MOVE BSEG-LIFNR TO INVOICES-LIFNR.
        SELECT * FROM LFA1 WHERE LIFNR EQ INVOICES-LIFNR.
          MOVE LFA1-NAME1+0(15) TO INVOICES-NAME. "Change namel to name1
        ENDSELECT.
        MOVE BKPF-BELNR TO INVOICES-BELNR.
        MOVE BKPF-XBLNR TO INVOICES-XBLNR.
        		move bkpf-blart to invoices-doctyp.
        		move bseg-wrbtr to invoices-wrbtr.
        		move bseg-ebeln to invoices-ebeln.
        		move bseg-buzei+1(2) to invoices-buzei.
        		move bseg-menge to invoices-menge.
        		if bseg-sgtxt ne space.
        		move bseg-sgtxt to invoices-sgtxt.
      else.
        		move bkpf-bktxt to invoices-sgtxt.
        		endif .

        		move bseg-matnr to invoices-matnr.
        		move bseg-peinh to invoices-peinh.
        		move bseg-wrbtr to invoices-wrbtr.
        		if bseg-shkzg eq 'S'.
        		move 'debit   ' to invoices-miscd.
        		else.
        		move 'credit   ' to invoices-miscd.
        		endif.
        		move bseg-sknto to invoices-sknto.
        		if bseg-sknto = 0.
        		move bseg-wrbtr to invoices-nebtr.
        		elseif bseg-sknto = space.
        		move bseg-wrbtr to invoices-nebtr.
        		else.
        		invoices-nebtr = bseg-wrbtr - bseg-sknto.
        		endif.
        		move bkpf-budat+2(6) to invoices-budat.
        		move bkpf-bldat+2(6) to invoices-bldat.
        		move bseg-bukrs	    to invoices-bukrs.
*   ##move bseg-psws1#    to invoices-psws1. "mdemeest
        MOVE BSEG-PSWSL     TO INVOICES-PSWSL. "mdemeest 99/05/03
        MOVE BSEG-AUGBL     TO INVOICES-AUGB1.
        		if invoices-doctyp eq 'ZP'.
        		select * from payr
        		where zbukr = bseg-bukrs		                        "Log 003
        		and     gjahr  = fyear		                          "Log 003
        		and     vblnr  = invoices-belnr.
        	                                                   "Log 003
    		endselect.
        		elseif invoices-doctyp eq 'KZ'.
        		select * from payr
        		where zbukr = bseg-bukrs		                        "Log 003
        		and     gjahr  = fyear		                          "Log 003
        		and     vblnr  = invoices-belnr.
        	                                                   "Log 003
    		endselect.
        		else.
        		select * from payr
        		where zbukr = bseg-bukrs		                        "Log 003
        	and     gjahr  = fyear		                           "Log 003
        		and     vblnr  = invoices-augb1.
        	                                                   "Log 003
    		endselect.
        		endif.
*      where#zbukr#= bseg-bukrs
*and#hbkid#= bseg-hbkid
*#and#hktid#= 'PYMT'
*#and#vblnr#= bseg-augb1##                             "LOG 002
*#and#vblnr#= bseg-belnr
*#and#chect#in chkrang.
                                                            "LOG 002
        		if sy-subrc eq 0.
        		move payr-chect to invoices-chect.
        		payr-rwbtr =  - payr-rwbtr.  		  "Remove the minus sign
        		move payr-rwbtr to invoices-rwbtr.
        		move payr-laufd+2(6) to invoices-laufd.
        		move payr-voidd+2(6) to invoices-voidc.
        		move payr-voidr+0(2) to invoices-voidr.
        move payr-laufi        to invoices-laufi.                  "004
        else.
        clear: invoices-chect, invoices-rwbtr, invoices-laufd,
              invoices-voidc, invoices-voidr, invoices-laufi.
        		endif.
        	endif.
        	       	append invoices.
      endselect.
      		endselect.

end-of-selection.
* -------------------------------------  END OF SELECTION  -------------


  		
 SORT INVOICES BY LIFNR XBLNR.
  LOOP AT INVOICES.
    PERFORM GET_PO.
*transfer invoices to outfilel.   "mdemeest 99/05/03            "Log 001
TRANSFER INVOICES TO OUTFILE1.    "mdemeest 9/05/03             "Log 001
    INV_CTR = INV_CTR + 1.
    IF INV_CTR < 400.
      WRITE: / INVOICES-LIFNR, INVOICES-XBLNR,
      INVOICES-BELNR,
      INVOICES-WRBTR, INVOICES-SKNTO, INVOICES-NEBTR,
      INVOICES-CHECT,
      INVOICES-RWBTR,
      INVOICES-MISCD,
      INVOICES-DOCTYP,
      INVOICES-EBELN,
*     invoices-psws1. "mdemeest 99/05/03
      INVOICES-PSWSL.                  "mdemeest 99/05/03
    ENDIF.
  ENDLOOP.
  WRITE: / 'Invoices = ', INV_CTR.
  CLOSE DATASET OUTFILE1.                                   "Log 001


*****************************************************************
* 	Form GET_PO						*
*****************************************************************

FORM GET_PO.
  CLEAR BSEG.
  SELECT EBELN FROM BSEG INTO INVOICES-EBELN
  WHERE BUKRS = CO_CODE
  AND     GJAHR  = FYEAR
  AND     BELNR  = INVOICES-BELNR
  AND     HKONT = '0000022126'.
  ENDSELECT.
ENDFORM.

************************END OF 'YFAPAUD2' PROGRAM**********************
