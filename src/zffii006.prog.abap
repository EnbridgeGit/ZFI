*
REPORT ZFFII006 MESSAGE-ID ZS NO STANDARD PAGE HEADING LINE-SIZE 255.
************************************************************************
* Changes:
* 2001/08/13 #--- mdemeest Mainframe elimination
* 2011/12/14 gymana TR928 Add logic to handle closed banks
************************************************************************
TABLES: check_head,          "Header
        check_rec.           "Detail
*        LFA1,               "Vendor Name
*        LFB1,
*        LFB1,
*        MAKT.               "Material Description

SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME TITLE TEXT-001.
PARAMETER:  INFILE(60)
              default '/usr/sap/interfaces/P01/FGLPAYREC/recon.chk'
                                                          lower case.
PARAMETER:  OUTFILE(60)
              DEFAULT '/usr/sap/interfaces/P01/FGLPAYREC/recon.sap'
                                                          LOWER CASE.

*PARAMETER:      P_BUKRS LIKE BKPF-BUKRS DEFAULT 'UGL'       OBLIGATORY.
*PARAMETER:      P_GJAHR LIKE BKPF-GJAHR DEFAULT SY-DATUM(4) OBLIGATORY.
*ELECT-OPTIONS: S_MONAT FOR  BKPF-MONAT OBLIGATORY DEFAULT '01' TO '12',
*               S_BLART FOR  BKPF-BLART,
*               S_HKONT FOR  BSEG-HKONT,
*               S_LIFNR FOR BSEG-LIFNR.
*ELECT-OPTIONS: S_MATNR FOR BSEG-MATNR,
*               S_AUFNR FOR BSEG-AUFNR,
*               S_KOSTL FOR BSEG-KOSTL.
SELECTION-SCREEN END OF BLOCK BOX3.
*
*ATA: WS_SYMBOLIC(4) TYPE C VALUE '$sys'. "99/05/10
*
*ATA: TMP_NAME  LIKE LFA1-NAME1.
*ATA: TMP_ZWELS LIKE LFB1-ZWELS.
*ata: wa-maktx like makt-maktx.
*ATA: PRV-BELNR LIKE BKPF-BELNR.

*ATA: PRV-LIFNR LIKE BSEG-LIFNR.
*ATA: TMP-LIFNR LIKE BSEG-LIFNR.

data:  inrec(80)   type c.
data:  wa_rec_in   type i.
data:  wa_document type i.
data:  wa_sap_out  type i.
data:  wa_document_amt  type p decimals 2.
data:  wa_flag(1)  type c  value 'X'.
data:  wa_amt      type p  decimals 2.
*set up appropriate path dependent on client
AT SELECTION-SCREEN OUTPUT.
  move sy-sysid(3) to infile+20(3).          "Correct file paths
  move sy-sysid(3) to outfile+20(3).


START-OF-SELECTION.
  OPEN DATASET INFILE  for input  in text mode.
  OPEN DATASET OUTFILE FOR OUTPUT IN TEXT MODE.
  perform process_input.
  perform write_report.

form process_input.
  do.
     read dataset infile into inrec.
     if sy-subrc = '0'.
        if inrec+23(2) = 'HR'.
           clear check_head.
           if wa_flag = 'X'.
              move ' ' to wa_flag.                "First time thru
           else.
             write: / text-013, wa_document_amt.  "More than 1 document
           endif.
           compute wa_document = wa_document + 1.
           move '1'             to check_head-rectp.       "Record type
           move '/'             to check_head-hdnum.
           move '/'             to check_head-sbank.
           move '0010'          to check_head-bankl(4).    "Routing# Pt1
           move inrec(5)        to check_head-bankl+4(5).  "Routing# Pt2
*           move inrec+10(7)     to check_head-accnr(7).    "Our Acct#

           if inrec+10(7) = '0003301'.                            "TR928
              move 'CLOSED*0003301' to check_head-accnr.          "TR928
           endif.                                                 "TR928
           if inrec+10(7) = '0206717'.                            "TR928
              move 'CLOSED*0206717' to check_head-accnr.          "TR928
           endif.                                                 "TR928

           move '/'             to check_head-paytp.
           move inrec+17(2)     to check_head-crdat+2(2).  "Create Year
           move inrec+19(2)     to check_head-crdat+4(2).  "Create Month
           move inrec+21(2)     to check_head-crdat+6(2).  "Create Day
           move '20'            to check_head-crdat(2).    "Create Cent
*           if inrec+17(2) > '95'.
*              move '19'         to check_head-crdat(2).
*           endif.
           transfer check_head  to outfile length 80.
        else.
           clear check_rec.
           compute wa_rec_in = wa_rec_in + 1.
           move '5'             to check_rec-rectp.
           move '20'            to check_rec-valut(2).
           move inrec+37(2)     to check_rec-valut+2(2).
           move inrec+39(2)     to check_rec-valut+4(2).
           move inrec+41(2)     to check_rec-valut+6(2).
           move inrec+17(10)    to check_rec-cknum.
           move inrec+27(10)    to check_rec-amount.
           move '/'             to check_rec-bankl.
           move '/'             to check_rec-accnr.
           move check_rec-valut to check_rec-pdate.
           move '/'             to check_rec-bnktc.
           transfer check_rec   to outfile length 80.
           compute wa_sap_out = wa_sap_out + 1.
           compute wa_document_amt = wa_document_amt +
                                     ( check_rec-amount / 100 ).
        endif.
     else.
       close dataset: infile, outfile.
       exit.
     endif.
  enddo.
endform.


form write_report.
  write: / text-rpt, sy-repid, 50 text-dte, sy-datum.
  skip 3.
  write: / text-013, wa_document_amt.
  write: / text-011, wa_document.
  write: / text-010, wa_rec_in.
  write: / text-012, wa_sap_out.
endform.
