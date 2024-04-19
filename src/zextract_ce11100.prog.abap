*&---------------------------------------------------------------------*
*& Report  ZEXTRACT_CE11100
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZEXTRACT_CE11100.

tables: ce11100 .

types: begin of ty_ce110,
         gjahr type gjahr,
         perde type PERIODE,
         werks type werks_d,
         wwprg type RKEG_WWPRG,
         wwrat type RKEG_WWRAT,
         wwser type RKEG_WWSER,
         wwsld type RKEG_WWSLD,
         vvcgg type RKE2_VVCGG,
       end of ty_ce110 .

data: gt_ce110 type STANDARD TABLE OF ty_ce110,
      gw_ce110 type ty_ce110 .

PARAMETERS: p_gjahr type gjahr,
            p_perde type PERIODE,
            p_werks type werks_d,
            p_wwprg type RKEG_WWPRG,
            p_wwrat type RKEG_WWRAT,
            p_wwser type RKEG_WWSER,
            p_wwsld type RKEG_WWSLD .
*            p_wwpce type RKEG_WWPCE .

select-OPTIONS: s_vrgar for ce11100-vrgar,
                s_kndnr for ce11100-kndnr .

PARAMETERS:
p_bukrs   TYPE t001-bukrs .             " OBLIGATORY,
*p_perio   LIKE ce11100-perio .         " OBLIGATORY .


  SELECT gjahr perde werks wwprg wwrat wwser wwsld vvcgg
    INTO TABLE gt_ce110
    FROM ce11100
    WHERE paledger  = '01'
      AND vrgar     IN s_vrgar
      AND versi     = ''
*      AND perio     = p_perio
      AND kndnr     IN s_kndnr
      AND artnr     = 'NATGAS'
      AND bukrs     = p_bukrs
      AND gjahr     = p_gjahr
      AND perde     = p_perde
      AND werks     = p_werks
      AND wwprg     = p_wwprg
      AND wwrat     = p_wwrat
      AND wwser     = p_wwser
      AND wwsld     = p_wwsld .
*      AND wwpce     = p_wwpce .
*    GROUP BY gjahr perde werks wwprg wwrat wwser wwsld.



    write: 'A' .
