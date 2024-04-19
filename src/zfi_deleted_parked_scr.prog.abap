*&---------------------------------------------------------------------*
*&  Include           ZFI_DELETED_PARKED_SCR
*&---------------------------------------------------------------------*

selection-screen begin of block B1 with frame title TEXT-006.
select-options: S_BUKRS for bkpf-bukrs no intervals,   "Company Code
                S_BELNR for bkpf-Belnr no intervals,   "Document Number
                S_BLART for bkpf-Blart no intervals,   "Document Type
                S_GJAHR for bkpf-GJAHR no intervals.   "Fiscal Year

select-options:
 S_CPUDT for BKPF-CPUDT  obligatory lower case.        "Entry Date

selection-screen end of block B1.

selection-screen begin of block B2 with frame title TEXT-007.
select-options: S_MAILTO for GV_ADDRESS obligatory no intervals.     "Recepient Email IDs
selection-screen end of block B2.
