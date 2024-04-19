FUNCTION-POOL zap_vendor_activity_bal.      "MESSAGE-ID ..

* INCLUDE LZAP_VENDOR_ACTIVITY_BALD...       " Local class definition

TYPES: BEGIN OF ty_vbsegk,
           ausbk  TYPE ausbk,
           belnr  TYPE belnr_d,
           gjahr  TYPE gjahr,
           bzkey  TYPE buzei,
           lifnr  TYPE lifnr,
           shkzg  TYPE shkzg,
           bukrs  TYPE bukrs,
           waers  TYPE waers,
           umskz  TYPE umskz,
           dmbtr  TYPE dmbtr,
         END OF ty_vbsegk,

         BEGIN OF ty_vbkpf,
           ausbk TYPE ausbk,
           bukrs TYPE bukrs,
           belnr TYPE belnr,
           gjahr TYPE gjahr,
           cpudt TYPE cpudt,
         END OF ty_vbkpf.

TYPES: BEGIN OF ty_bsik,
         bukrs  TYPE  bukrs,
         lifnr  TYPE lifnr,
         umsks  TYPE umsks,
         umskz  TYPE umskz,
         augdt  TYPE augdt,
         augbl  TYPE augbl,
         zuonr  TYPE dzuonr,
         gjahr  TYPE gjahr,
         belnr  TYPE belnr_d,
         buzei  TYPE buzei,
         zumsk  TYPE dzumsk,
         dmbtr  TYPE dmbtr,
         shkzg  TYPE shkzg,
         waers  TYPE waers,
       END OF ty_bsik.

TYPES: BEGIN OF ty_bsak,
        bukrs TYPE bukrs,
        lifnr	TYPE lifnr,
        umsks	TYPE umsks,
        umskz	TYPE umskz,
        augdt	TYPE augdt,
        augbl	TYPE augbl,
        zuonr	TYPE dzuonr,
        gjahr	TYPE gjahr,
        belnr	TYPE belnr_d,
        buzei	TYPE buzei,
        shkzg TYPE shkzg,
        waers TYPE waers,
        dmbtr TYPE dmbtr,
       END OF ty_bsak.

TYPES: BEGIN OF ty_lfa1,
         lifnr TYPE lifnr,
         name1 TYPE name1_gp,
         stras TYPE stras_gp,
         ort01 TYPE ort01_gp,
         regio TYPE regio,
         land1 TYPE land1_gp,
         date  TYPE datum,
         ktokk TYPE ktokk,
         loevm TYPE loevm,
         sperr TYPE sperr,
       END OF ty_lfa1.

TYPES: BEGIN OF ty_po,
          lifnr TYPE ekko-lifnr,
          ebeln TYPE  ebeln,
          ebelp TYPE ebelp,
          aedat TYPE ekko-aedat,
          brtwr TYPE ekpo-brtwr,
          menge TYPE ekpo-menge,
       END OF ty_po.

TYPES: BEGIN OF ty_ekbe_e,
  ebeln	TYPE ebeln,
ebelp	TYPE ebelp,
zekkn	TYPE dzekkn,
vgabe	TYPE vgabe,
gjahr	TYPE mjahr,
belnr	TYPE mblnr,
buzei	TYPE mblpo,
         bewtp TYPE bewtp,
         menge TYPE menge_d,
       END OF ty_ekbe_e.

TYPES: BEGIN OF ty_ekbe_q,
  ebeln	TYPE ebeln,
ebelp	TYPE ebelp,
zekkn	TYPE dzekkn,
vgabe	TYPE vgabe,
gjahr	TYPE mjahr,
belnr	TYPE mblnr,
buzei	TYPE mblpo,
         bewtp TYPE bewtp,
         dmbtr TYPE dmbtr,
       END OF ty_ekbe_q.

TYPES: BEGIN OF ty_output,
         lifnr      TYPE lifnr,
         name1      TYPE name1_gp,
         stras      TYPE stras_gp,
         ort01      TYPE ort01_gp,
         regio      TYPE regio,
         land1      TYPE land1_gp,
         credit_tot TYPE maxbt_sum,
         debit_tot  TYPE maxbt_sum,
         currency   TYPE waers,
         date       TYPE datum,
         ktokk      TYPE ktokk,
         po_tot     TYPE bbwert,
         po_bal_qty TYPE menge_d,
         po_bal_amt TYPE dmbtr,
       END OF ty_output.

TYPES: t_ty_output TYPE STANDARD TABLE OF ty_output.

DATA: gt_lfa1        TYPE STANDARD TABLE OF ty_lfa1,
      gt_output      TYPE t_ty_output,
      gv_active(6)   TYPE n,
      gv_new(6)      TYPE n,
      gv_inactive(6) TYPE n.

FIELD-SYMBOLS: <output> LIKE LINE OF gt_output.
