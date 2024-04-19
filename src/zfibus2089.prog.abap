*****           Implementation of object type ZFIBUS2089           *****
INCLUDE <OBJECT>.
BEGIN_DATA OBJECT. " Do not change.. DATA is generated
* only private members may be inserted into structure private
DATA:
" begin of private,
"   to declare private attributes remove comments and
"   insert private attributes here ...
" end of private,
  BEGIN OF KEY,
      EMPLOYEENUMBER LIKE PSKEY-PERNR,
      TRIPNUMBER LIKE BAPITRIP-TRIPNO,
  END OF KEY,
      TOTALCOSTS TYPE PTRV_SHDR-TRIP_TOTAL,
      COVERSHEET TYPE TKE_DIST-CHECK_ORG,
      OUTOFPOCKET TYPE WFSYST-PRIORITY,
      OOPAMOUNT TYPE PTRV_SREC-LOC_AMOUNT,
      OOPCURR TYPE PTRV_SREC-LOC_CURR.
END_DATA OBJECT. " Do not change.. DATA is generated

BEGIN_METHOD GETNEXTWORKINGDATE CHANGING CONTAINER.

DATA:
      ENDDATE TYPE SYST-DATUM,
      ENDTIME TYPE SYST-UZEIT.

CALL FUNCTION 'ZFI_GET_NEXT_WORKING_DATE'
  IMPORTING
    EXP_END_DATE             = ENDDATE
    EXP_END_TIME             = ENDTIME
  EXCEPTIONS
    INVALID_FACTORY_CALENDER = 9001
    OTHERS                   = 02.

CASE SY-SUBRC.
  WHEN 0.            " OK
  WHEN 9001.         " INVALID_FACTORY_CALENDAR
    EXIT_RETURN 9001 sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  WHEN OTHERS.       " to be implemented
ENDCASE.
SWC_SET_ELEMENT CONTAINER 'EndDate' ENDDATE.
SWC_SET_ELEMENT CONTAINER 'EndTime' ENDTIME.

END_METHOD.

GET_PROPERTY TOTALCOSTS CHANGING CONTAINER.
Data: w_pernr like BAPIEMPL-PERNR,
      w_reinr like BAPITRIP-TRIPNO,
      w_curr like PTRV_PERIO-WAERS.
Data: t_amt type table of BAPITRVSUM with header line.

w_pernr = object-key-employeenumber.
w_reinr = object-key-tripnumber.
SWC_GET_PROPERTY SELF 'Currency' w_curr.

CALL FUNCTION 'BAPI_TRIP_GET_DETAILS'
  EXPORTING
    EMPLOYEENUMBER = w_pernr
    TRIPNUMBER     = w_reinr
  TABLES
    AMOUNTS        = t_amt.
read table t_amt with key currency = w_curr.
if sy-subrc = 0.
  OBJECT-TOTALCOSTS = t_amt-trip_total.
endif.

SWC_SET_ELEMENT CONTAINER 'TotalCosts' OBJECT-TOTALCOSTS.
END_PROPERTY.

GET_PROPERTY COVERSHEET CHANGING CONTAINER.
DATA: COUNT LIKE  SY-INDEX,
      ZKEY like SWOTOBJID-OBJKEY.
DATA: T_ATT type table of BDN_CON with header line.

ZKEY = OBJECT-KEY.
CALL FUNCTION 'BDS_ALL_CONNECTIONS_GET'
  EXPORTING
    CLASSNAME        = 'BUS2089'
    CLASSTYPE        = 'BO'
    OBJKEY           = ZKEY
  IMPORTING
    COUNT            = COUNT
  TABLES
    ALL_CONNECTIONS  = T_ATT
  EXCEPTIONS
    NO_OBJECTS_FOUND = 1
    ERROR_KPRO       = 2
    INTERNAL_ERROR   = 3
    NOT_AUTHORIZED   = 4
    OTHERS           = 5.
IF COUNT GE 1.
  READ TABLE T_ATT WITH KEY DOC_TYPE = 'ZTERECPDF'.
  IF sy-subrc = 0.
    OBJECT-COVERSHEET = 'Y'.
  ELSE.
    OBJECT-COVERSHEET = 'N'.
  ENDIF.
ELSE.
  OBJECT-COVERSHEET = 'N'.
ENDIF.

SWC_SET_ELEMENT CONTAINER 'CoverSheet' OBJECT-COVERSHEET.
END_PROPERTY.

GET_PROPERTY OUTOFPOCKET CHANGING CONTAINER.
Data: lv_pernr type PERNR_D,
      lv_reinr type REINR,
      lv_mileage type PTRV_PD_MILEAGE,
      lv_type type SPKZL.

lv_pernr = object-key-employeenumber.
lv_reinr = object-key-tripnumber.

OBJECT-OUTOFPOCKET = '0'.

select EXP_TYPE from PTRV_SREC into lv_type
where pernr = lv_pernr and reinr = lv_reinr.

  select single SPKZL from T706B1 into lv_type
  where MOREI = 'UG' AND
        SPKZL = lv_type AND
        ENDDA GE sy-datum AND
        FIRMA = ' '.
  IF sy-subrc = 0.
    OBJECT-OUTOFPOCKET = '1'.
  ENDIF.

endselect.

IF OBJECT-OUTOFPOCKET = '0'.
  select single PD_MILEAGE from PTRV_SHDR into lv_mileage
  where pernr = lv_pernr and reinr = lv_reinr and PD_MILEAGE GT 0.
  if sy-subrc = 0.
    OBJECT-OUTOFPOCKET = '1'.
  else.
    OBJECT-OUTOFPOCKET = '0'.
  endif.
ENDIF.
SWC_SET_ELEMENT CONTAINER 'OutofPocket' OBJECT-OUTOFPOCKET.
END_PROPERTY.

GET_PROPERTY OOPAMOUNT CHANGING CONTAINER.
Data: lv_amount type PTRV_LOC_AMOUNT,
      lv_mileage TYPE PTRV_PD_MILEAGE,
      lv_type type SPKZL.

clear: OBJECT-OOPAMOUNT, lv_amount.

select LOC_AMOUNT EXP_TYPE from PTRV_SREC into (lv_amount, lv_type)
where pernr = object-key-employeenumber and
      reinr = object-key-tripnumber.

  select single SPKZL from T706B1 into lv_type
  where MOREI = 'UG' AND
        SPKZL = lv_type AND
        ENDDA GE sy-datum AND
        FIRMA = ' '.
  IF sy-subrc = 0.
    OBJECT-OOPAMOUNT = OBJECT-OOPAMOUNT + lv_amount.
  ENDIF.
endselect.

clear lv_mileage.
select single PD_MILEAGE from PTRV_SHDR into lv_mileage
where pernr = object-key-employeenumber
and reinr = object-key-tripnumber
and PD_MILEAGE GT 0.

OBJECT-OOPAMOUNT = OBJECT-OOPAMOUNT + lv_mileage.

SWC_SET_ELEMENT CONTAINER 'OOPAmount' OBJECT-OOPAMOUNT.
END_PROPERTY.

GET_PROPERTY OOPCURR CHANGING CONTAINER.
DATA: lv_type type SPKZL,
      lv_curr TYPE PTRV_SREC-LOC_CURR.

select LOC_CURR EXP_TYPE from PTRV_SREC into (lv_curr, lv_type)
where pernr = object-key-employeenumber and
      reinr = object-key-tripnumber.

  select single SPKZL from T706B1 into lv_type
  where MOREI = 'UG' AND
        SPKZL = lv_type AND
        ENDDA GE sy-datum AND
        FIRMA = ' '.
  IF sy-subrc = 0.
    OBJECT-OOPCURR = lv_curr.
  ENDIF.
endselect.

SWC_SET_ELEMENT CONTAINER 'OOPCurr' OBJECT-OOPCURR.
END_PROPERTY.

BEGIN_METHOD GET_EMAILS CHANGING CONTAINER.
DATA:
      EMAILS TYPE ADR6-SMTP_ADDR OCCURS 0,
      EMAIL_ADD TYPE ADR6-SMTP_ADDR,
      INITIATOR TYPE WFSYST-INITIATOR,
      UserID TYPE WFSYST-AGENT,
      EMP_Obj TYPE SWC_OBJECT,
      usr_obj TYPE SWC_OBJECT,
      PERNR TYPE PTRV_HEAD-PERNR,
      ID    TYPE USR01-BNAME,
      Emp   TYPE USR01-BNAME.

SWC_GET_ELEMENT CONTAINER 'Initiator' INITIATOR.
ID = INITIATOR+2(12).

SWC_GET_PROPERTY SELF 'Employee' EMP_Obj.
SWC_GET_PROPERTY EMP_Obj 'Number' PERNR.

SWC_CREATE_OBJECT usr_obj 'USR01DOHR' ID.

CALL FUNCTION 'ZWF_FIND_EMAIL'
  EXPORTING
    ID    = ID
  IMPORTING
    EMAIL = EMAIL_ADD.

If NOT EMAIL_ADD IS INITIAL.
  APPEND EMAIL_ADD to EMAILS.
  CLEAR EMAIL_ADD.
ENDIF.

CALL FUNCTION 'ZWF_FIND_EMAIL'
  EXPORTING
    PERNR  = PERNR
  IMPORTING
    EMAIL  = EMAIL_ADD
    USERID = UserID.
If NOT EMAIL_ADD IS INITIAL AND USERID NE INITIATOR.
  APPEND EMAIL_ADD to EMAILS.
ENDIF.
SWC_SET_ELEMENT CONTAINER 'UserID' UserID.
SWC_SET_ELEMENT CONTAINER 'Initiator_obj' Usr_obj.
SWC_SET_TABLE CONTAINER 'Emails' EMAILS.
END_METHOD.
