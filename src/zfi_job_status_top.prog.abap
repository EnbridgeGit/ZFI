*&---------------------------------------------------------------------*
*&  Include           ZFI_JOB_STATUS_TOP
*&---------------------------------------------------------------------*

TABLES : tbtco.
SELECT-OPTIONS : p_job FOR tbtco-jobname NO INTERVALS.

DATA : gv_job_read_jobhead TYPE tbtcjob,
       gv_msg_text TYPE char100,
       gs_job_info TYPE tbtco,
       gr_status TYPE RANGE OF btcstatus,
       gs_status LIKE LINE OF gr_status.
CONSTANTS : gc_read_jobhead_only LIKE btch0000-int4 VALUE 19.
