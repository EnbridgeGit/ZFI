FUNCTION Z_SAMPLE_INTERFACE_00002110.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_REGUH) LIKE  REGUH STRUCTURE  REGUH
*"  TABLES
*"      T_REGUP STRUCTURE  REGUP
*"      T_REGUP_LST STRUCTURE  REGUP_LST
*"  CHANGING
*"     VALUE(C_REGUH_LST) LIKE  REGUH_LST STRUCTURE  REGUH_LST
*"----------------------------------------------------------------------

CONCATENATE i_reguh-bnkl1
              i_reguh-bnkn1
              INTO gv_chaintext
              SEPARATED BY space.

  c_reguh_lst-zzchaintext = gv_chaintext.


ENDFUNCTION.
