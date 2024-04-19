"Name: \PR:SAPLPTRM_WEB_EMPLOYEE_SERVICES\FO:GET_USER_DATA\SE:END\EI
ENHANCEMENT 0 ZEEMREMOVEPERNRCHECK.
*BTBOUNDY EEM WebDyn Pro ABAP
*Remove the return code of a bad pernr so Function PTRM_WEB_CE_GET_PERS_ASGMT_ESS does not raise an exception, just return no pernr.
  p_subrc = 0.
ENDENHANCEMENT.
