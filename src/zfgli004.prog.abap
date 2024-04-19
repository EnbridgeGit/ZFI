REPORT  zfgli004 MESSAGE-ID zs.

*-----------------------------------------------------------------------
*  Author    : Glenn Ymana                        SAP : East
*  Date      : March, 2007               Program Type : Interface
*  Issue Log : TR153
*-----------------------------------------------------------------------
*  Title : Mercator Replacement - IFFI051 Banner G/L Interface
*-----------------------------------------------------------------------
*  Part 1 for DATESPLIT & MOVEVOL processes:
*            : this program will create an output file to be passed to
*            : main translation program ZFGLI005.
*-----------------------------------------------------------------------
*  Description:
*     - Create one or more ABAP programs to replace 6 Mercator mapping
*     - programs that take an input file and creates multiple ZBSEG
*     - format BDC session: (Gas, Rental, Other, Error).
*----------------------------------------------------------------------
* Changes:
* 2007/08/22 gymana - TR392 - Added new Rate Class field in input file
* 2008/04/10 gymana - TR540 - Added Service Class check when creating
*                             delivery records for gas commodity records
*-----------------------------------------------------------------------
*2019/08/01  ahmadt - D30K930071 - CHG0155278
*                     Added an extra elseif condition to read if
**                    classifiation codes are equal to 'CLC' or 'CLF'
*                     D30K930075
*-----------------------------------------------------------------------
DATA:  BEGIN OF bann_file OCCURS 0,
       process_date       TYPE d,
       serv_type(4)       TYPE c,
       serv_cat(4)        TYPE c,
       gl_classcd(4)      TYPE c,
       serv_class(2)      TYPE c,
       town_cd(2)         TYPE c,
       munic_cd(4)        TYPE c,
       budget_ind(1)      TYPE c,
       cust_num(16)       TYPE n,
       trans_amt_sign(1)  TYPE c,
       trans_amt(13)      TYPE c,
       budg_amt_sign(1)   TYPE c,
       budget_amt(11)     TYPE c,
       cons_sign(1)       TYPE c,
       cons(13)           TYPE c,
       no_of_custs(6)     TYPE n,
       eff_date           TYPE d,
       rate_class(4)      TYPE c,
       END OF bann_file.

DATA:  BEGIN OF wa OCCURS 0,
       process_date        TYPE d,
       serv_type(4)        TYPE c,
       serv_cat(4)         TYPE c,
       gl_classcd(4)       TYPE c,
       serv_class(2)       TYPE c,
       town_cd(2)          TYPE c,
       munic_cd(4)         TYPE c,
       budget_ind(1)       TYPE c,
       cust_num(16)        TYPE n,
       trans_amt(14)       TYPE p DECIMALS 2,
       budget_amt(12)      TYPE p DECIMALS 2,
       cons(14)            TYPE p DECIMALS 3,
       no_of_custs(6)      TYPE n,
       eff_date            TYPE d,
       rate_class(4)       TYPE c,
       END OF wa.

DATA:  BEGIN OF itab OCCURS 0,
       process_date        TYPE d,
       serv_type(4)        TYPE c,
       serv_cat(4)         TYPE c,
       gl_classcd(4)       TYPE c,
       serv_class(2)       TYPE c,
       town_cd(2)          TYPE c,
       munic_cd(4)         TYPE c,
       budget_ind(1)       TYPE c,
       cust_num(16)        TYPE n,
       trans_amt(14)       TYPE c,
       budget_amt(12)      TYPE c,
       cons(14)            TYPE c,
       no_of_custs(6)      TYPE n,
       eff_date            TYPE d,
       rate_class(4)       TYPE c,
       END OF itab.

DATA: BEGIN OF idate OCCURS 0,
        seqno    LIKE zbseg-zterm,
        pdate(8) TYPE c.
DATA: END OF idate.

DATA: wa_seqno            TYPE i,
      wa_seqnum           TYPE i,
      wa_docsize          TYPE i,
      w_cons(14)          TYPE p DECIMALS 3,
      w_trans_amt(14)     TYPE p DECIMALS 2,
      w_budget_amt(12)    TYPE p DECIMALS 2,
      va_cons(14)         TYPE p DECIMALS 3,
      va_trans_amt(14)    TYPE p DECIMALS 2,
      va_budget_amt(12)   TYPE p DECIMALS 2.


DATA: delimiter VALUE '09' TYPE x.
DATA: outrec(2000), infile(70), outfile(70).
DATA: wrk_symbolic(4) TYPE c VALUE '$sys'.

* variables
DATA: va_date   TYPE d.
*constants
*Start of Changes done by ahmadt on 01/08/2019 for CHG0155278
DATA: gc_clc type c length 3 value 'CLC',
      gc_clf type c length 3 value 'CLF'.
*End of Changes done by ahmadt on 01/08/2019 for CHG0155278

FIELD-SYMBOLS: <f1>.

*-----------------------------------------------------------------------
* selection screen
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK box0 WITH FRAME.
SELECTION-SCREEN BEGIN OF BLOCK box1 WITH FRAME TITLE text-100.

PARAMETERS: p_file1 LIKE filename-fileextern OBLIGATORY DEFAULT
              '/usr/sap/interfaces/$sys/BANNER/zbis100.dat'.

SELECTION-SCREEN SKIP.
PARAMETERS: p_file2 LIKE filename-fileextern OBLIGATORY DEFAULT
              '/usr/sap/interfaces/$sys/BANNER/zbis100.tmp'.

SELECTION-SCREEN END OF BLOCK box1.
SELECTION-SCREEN END OF BLOCK box0.

*----------------------------------------------------------------------
* AT SELECTION-SCREEN
*----------------------------------------------------------------------
AT SELECTION-SCREEN OUTPUT.
  REPLACE wrk_symbolic WITH sy-sysid INTO:
     p_file1, p_file2.
  CONDENSE: p_file1 NO-GAPS, p_file2 NO-GAPS.

*-----------------------------------------------------------------------
* start-of-selection
*-----------------------------------------------------------------------
START-OF-SELECTION.
  PERFORM open_files.
  PERFORM f_datesplit.
  PERFORM f_movevol.
  PERFORM f_create_output_file.
  CLOSE DATASET: p_file1, p_file2.

*-----------------------------------------------------------------------
* this routine will create an output file to be passed on to XLATBANN
*-----------------------------------------------------------------------
FORM f_create_output_file.
  LOOP AT itab.
    TRANSFER itab TO p_file2 LENGTH 103.
  ENDLOOP.
ENDFORM.                    "F_CREATE_OUTPUT_FILE

*-----------------------------------------------------------------------
*  This routine will look at all commodity records ("C" - e.g. 'GABC')
*  and create a corresponding delivery record ("D" - e.g. 'GABD')'
*  containing the consumption (volume) amount. Any franchised, Company
*  Used, CMP commodity records will retain its volume amount.
*-----------------------------------------------------------------------
FORM f_movevol.
  REFRESH itab.
  LOOP AT wa.
*    move wa-TRANS_AMT to w_trans_amt.
*    if wa-TRANS_AMT_SIGN = '-'.
*      w_trans_amt = w_trans_amt * -1.
*    endif.
*    move wa-BUDGET_AMT to w_budget_amt.
*    if wa-BUDG_AMT_SIGN = '-'.
*      w_budget_amt = w_budget_amt * -1.
*    endif.
*    move wa-CONS to w_cons.
*    if wa-CONS_SIGN = '-'.
*      w_cons = w_cons * -1.
*    endif.

    PERFORM f_write_record.

    IF ( wa-gl_classcd+3(1) = 'C' AND
         wa-serv_type(3) = 'GAS' AND
         ( wa-cons <> 0 OR wa-no_of_custs <> 0 ) AND
         wa-gl_classcd(3) <> 'OWN' ).
*     -
      PERFORM f_create_vol_record.
      PERFORM f_create_neg_vol_record.

*Start of Changes done by ahmadt on 01/08/2019 for CHG0155278
    ELSEIF ( wa-gl_classcd(3) = gc_clc OR wa-gl_classcd(3) = gc_clf ) AND
       ( wa-cons <> 0 OR wa-no_of_custs <> 0 ) AND
       wa-gl_classcd(3) <> 'OWN'.
      PERFORM f_create_vol_record.
      PERFORM f_create_neg_vol_record.
*End of Changes done by ahmadt on 01/08/19 for CHG0155278

    ELSE.
      IF wa-gl_classcd+3(1) = ' ' AND wa-gl_classcd+2(1) = 'C' AND
         wa-serv_type(3) = 'GAS' AND
         ( wa-cons <> 0 OR wa-no_of_custs <> 0 ) AND
         wa-gl_classcd(3) <> 'OWN'.
*     -
        PERFORM f_create_vol_record.
        PERFORM f_create_neg_vol_record.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                    "F_MOVEVOL

*-----------------------------------------------------------------------
* form F_CREATE_NEG_VOL_RECORD
*-----------------------------------------------------------------------
FORM f_create_neg_vol_record.
  CLEAR: va_trans_amt, va_budget_amt, va_cons.
  MOVE-CORRESPONDING wa TO itab.
  MOVE va_date TO itab-process_date.
**--START OF CHANGES BY AKMADASU FOR CHG0155278
  if wa-gl_classcd(3) = gc_clc OR wa-gl_classcd(3) = gc_clf.
    " Donothing.
  Else.
**-- END OF CHANGES BY AKMADASU FOR CHG0155278
    IF wa-gl_classcd+3(1) = ' '.
      CONCATENATE wa-gl_classcd(2) 'D' INTO itab-gl_classcd.
    ELSE.
      CONCATENATE wa-gl_classcd(3) 'D' INTO itab-gl_classcd.
    ENDIF.
  ENDIF. " ADDED BY AKMADASU CHG0155278
  MOVE '0.01' TO va_trans_amt.
  IF wa-cons > 0.
    itab-trans_amt = va_trans_amt * -1.
  ELSE.
    itab-trans_amt = va_trans_amt.
  ENDIF.

  va_budget_amt = 0.
  itab-budget_amt = va_budget_amt.
  va_cons = 0.
  itab-cons = va_cons.
  MOVE '000000'       TO itab-no_of_custs.
  APPEND itab. CLEAR itab.
ENDFORM.                    "F_CREATE_NEG_VOL_RECORD

*-----------------------------------------------------------------------
* form F_CREATE_VOL_RECORD
*-----------------------------------------------------------------------
FORM f_create_vol_record.
  CLEAR: va_trans_amt, va_budget_amt, va_cons.
  MOVE-CORRESPONDING wa TO itab.
  MOVE va_date TO itab-process_date.
**--START OF CHANGES BY AKMADASU FOR CHG0155278
  if wa-gl_classcd(3) = gc_clc OR wa-gl_classcd(3) = gc_clf.
    " Donothing.
  Else.
**-- END OF CHANGES BY AKMADASU FOR CHG0155278
    IF wa-gl_classcd+3(1) = ' '.
      CONCATENATE wa-gl_classcd(2) 'D' INTO itab-gl_classcd.
    ELSE.
      CONCATENATE wa-gl_classcd(3) 'D' INTO itab-gl_classcd.
    ENDIF.
  ENDIF. " ADDED BY AKMADASU FOR CHG0155278
  MOVE '0.01' TO va_trans_amt.
  IF wa-cons > 0.
    itab-trans_amt = va_trans_amt.
  ELSE.
    itab-trans_amt = va_trans_amt * -1.
  ENDIF.
  va_budget_amt = 0.
  itab-budget_amt = va_budget_amt.
  itab-cons = wa-cons.
  APPEND itab. CLEAR itab.
ENDFORM.                    "F_CREATE_VOL_RECORD

*-----------------------------------------------------------------------
* form F_WRITE_RECORD
*-----------------------------------------------------------------------
FORM f_write_record.
  MOVE-CORRESPONDING wa TO itab.
  MOVE va_date TO itab-process_date.
  itab-trans_amt = wa-trans_amt.
  itab-budget_amt = wa-budget_amt.

  IF ( wa-gl_classcd(3) = 'OWN' OR wa-gl_classcd = 'FRAN' OR
       wa-gl_classcd(3) = 'CMP' ).
    itab-cons = wa-cons.
  ELSE.
    va_cons = 0.
    itab-cons = va_cons.
  ENDIF.

  APPEND itab. CLEAR itab.
ENDFORM.                    "F_WRITE_RECORD

*-----------------------------------------------------------------------
*  This routine reads all the records from the input area, and
*  determines the most recent process date which will be used for
*  the posting date.
*-----------------------------------------------------------------------
FORM f_datesplit.
  REFRESH bann_file.
  DO.
    READ DATASET p_file1 INTO bann_file.
    IF ( bann_file IS INITIAL OR sy-subrc <> 0 ).
      EXIT.
    ELSE.
      MOVE-CORRESPONDING bann_file TO wa.
      MOVE bann_file-trans_amt TO wa-trans_amt.
      IF bann_file-trans_amt_sign = '-'.
        wa-trans_amt = wa-trans_amt * -1.
      ENDIF.
      MOVE bann_file-budget_amt TO wa-budget_amt.
      IF bann_file-budg_amt_sign = '-'.
        wa-budget_amt = wa-budget_amt * -1.
      ENDIF.
      MOVE bann_file-cons TO wa-cons.
      IF bann_file-cons_sign = '-'.
        wa-cons = wa-cons * -1.
      ENDIF.
      APPEND wa.
      IF va_date < bann_file-process_date.
        MOVE bann_file-process_date TO va_date.
      ENDIF.
    ENDIF.
  ENDDO.
ENDFORM.                    "F_DATESPLIT

*-----------------------------------------------------------------------
*  Routine to open the physical file to determine if there are any
*  errors reading it.
*-----------------------------------------------------------------------
FORM open_files.
  DATA: msg(100).
*-----------------------------------------------------------------------
  OPEN DATASET p_file1 FOR INPUT IN TEXT MODE MESSAGE msg.
  IF ( sy-subrc <> 0 ).
    MESSAGE e002 WITH infile msg.
  ENDIF.
*-----------------------------------------------------------------------
  OPEN DATASET p_file2 FOR OUTPUT IN TEXT MODE MESSAGE msg.
  IF ( sy-subrc <> 0 ).
    MESSAGE e002 WITH outfile msg.
  ENDIF.
*-----------------------------------------------------------------------
ENDFORM.                    "OPEN_FILES
