*&---------------------------------------------------------------------*
*&  Include           ZFGLI007_TOP
*&---------------------------------------------------------------------*

TABLES: DD03L,
        T001,
        T003,
        T006,
        TCURC.

TYPES: BEGIN OF ty_input,
         post_date(10),     "Posting date
         doc_date(10),      "Document date
         doc_type(4),       "Document type
         tax_code(4),       "Tax code
         profit_cntr(12),   "Profit center
         acc_class(6),      "Account class
         gl_ac(17),         "G/L Account number
         recon_key(20),     "Reconciliation key
         post_key(4),       "Posting key
         amount(31),        "Amount
       END OF ty_input,

       BEGIN OF ty_bseg_sort,
         budat TYPE bkpf-budat,   "Posting date
         bldat TYPE bkpf-bldat.   "Document date
         INCLUDE STRUCTURE zpa_bseggs.   "Batch input structure for line items
TYPES: END OF TY_BSEG_SORT,

       BEGIN OF ty_ska1,
         saknr TYPE saknr,
       END OF ty_ska1.


DATA: filepath(55),
      outrec(750),                    "output record, to write to output file
      nodata VALUE '/',
      line_count TYPE i VALUE 0,
      gv_errorfile,

      gs_input TYPE ty_input,          "input data work area
      gt_input TYPE TABLE OF ty_input,

      gs_bgr00 TYPE bgr00,             "BDC session data structure
      gs_bbkpf TYPE bbkpf,             "FI doc Hdr structure
      gs_bbseg TYPE ZPA_BSEGGS,        "FI doc items structure

      gs_bbseg_sort TYPE ty_bseg_sort,
      gt_bbseg_sort TYPE TABLE OF ty_bseg_sort, "intermediate table to compile & map data
      gt_bbseg_error TYPE TABLE OF ty_bseg_sort,"holds error records

      gs_map_table1 TYPE ZLFICIS01,

      gs_map_table2 TYPE ZLSDCIS02,
      gt_map_table2 TYPE TABLE OF ZLSDCIS02,

      gv_saknr TYPE saknr,
      gt_ska1 TYPE TABLE OF ty_ska1.      "holds GLs that belong to P&L group


FIELD-SYMBOLS: <fs1>.


DATA: BEGIN OF gs_errlog,
        gl_acc TYPE hkont ,
        amount TYPE wrbtr,
      END OF gs_errlog,

      gt_errlog LIKE TABLE OF gs_errlog,    "for writing error logs to screen

      BEGIN OF gs_cskb,
        kstar TYPE kstar,
        katyp TYPE katyp,
        kostl TYPE kostl,
        aufnr TYPE aufnr,
      END OF gs_cskb,

      gt_cskb LIKE TABLE OF gs_cskb.
