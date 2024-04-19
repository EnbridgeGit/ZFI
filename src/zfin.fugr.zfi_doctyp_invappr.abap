FUNCTION ZFI_DOCTYP_INVAPPR.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      APPROVERS STRUCTURE  AGR_USERS
*"      DOCTYPES STRUCTURE  T003T
*"----------------------------------------------------------------------

  SELECT *
    FROM t003t
    INTO CORRESPONDING FIELDS OF TABLE DOCTYPES
   WHERE spras EQ 'E'.

  SELECT *
    FROM agr_users
    INTO CORRESPONDING FIELDS OF TABLE APPROVERS
   WHERE agr_name LIKE 'Z:FI_AP_DOA_INVOICE_APPRV%'.


ENDFUNCTION.
