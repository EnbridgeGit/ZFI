FUNCTION ZSAMPLE_PROCESS_00002050.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_REGUH) LIKE  REGUH STRUCTURE  REGUH
*"     VALUE(I_GJAHR) LIKE  REGUD-GJAHR
*"     VALUE(I_NACHA) LIKE  FINAA-NACHA
*"     VALUE(I_AFORN) LIKE  T042B-AFORN
*"  CHANGING
*"     VALUE(C_ITCPO) LIKE  ITCPO STRUCTURE  ITCPO
*"     VALUE(C_ARCHIVE_INDEX) LIKE  TOA_DARA STRUCTURE  TOA_DARA
*"       DEFAULT SPACE
*"     VALUE(C_ARCHIVE_PARAMS) LIKE  ARC_PARAMS STRUCTURE  ARC_PARAMS
*"       DEFAULT SPACE
*"----------------------------------------------------------------------
*TDDSET  Spool device
*TDPRINTER Printer
*TDNEWID New Spool Request

*break sahmad.
*C_ITCPO-TDNEWID = 'X'.

ENDFUNCTION.
