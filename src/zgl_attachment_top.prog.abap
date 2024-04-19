*&---------------------------------------------------------------------*
*&  Include           ZGL_ATTACHMENT_TOP
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
* Program Name       :  ZGL_ATTACHMENT                                 *
* Include            :  ZGL_ATTACHMENT_TOP                             *
* Author             :  Manvitha Dadi                                  *
* Date               :  28-Jul-2021                                    *
* Technical Contact  :  Ashok Madasu/Manvitha Dadi                     *
* Purpose            :  Attach any file from appplication server       *
*                       to FB03 Screen                                 *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By    CTS        Description                    *
* ---------------------------------------------------------------------*
* 28-Jul-2021  DADIM         D30K931398 CHG0221895 - Create custom     *
*                                       tcode for FB03/FBV3 attachments*
*&---------------------------------------------------------------------*


TABLES : sscrfields.
DATA : gt_content  TYPE  STANDARD TABLE OF soli,
       gt_objhead  TYPE STANDARD TABLE OF soli,
       gs_folmem_k TYPE sofmk,
       gs_note     TYPE borident,
       gs_object   TYPE borident,
       gs_obj_id   TYPE soodk,
       gs_content  TYPE soli,
       gs_fol_id   TYPE soodk,
       gs_obj_data TYPE sood1.

DATA : gv_ep_note   TYPE borident-objkey,
       gv_belnr     TYPE belnr_d,
       gv_file      TYPE string,
       gv_filename  TYPE string,
       gv_extension TYPE string.

DATA : gr_extension TYPE RANGE OF char4,
       gs_extension LIKE LINE OF gr_extension.

CONSTANTS : gc_bkpf       TYPE saeanwdid VALUE 'BKPF',
            gc_ext        LIKE soodk-objtp VALUE 'EXT',
            gc_msg        TYPE char7 VALUE 'MESSAGE',
            gc_atta       LIKE breltyp-reltype VALUE 'ATTA',
            gc_ucomm_onli TYPE sscrfields-ucomm   VALUE 'ONLI',
            gc_x          TYPE char1 VALUE 'X',
            gc_a1         TYPE saearchivi VALUE 'A1',
            gc_scms       TYPE vscan_profile VALUE '/SCMS/KPRO_CREATE',
            gc_asc        TYPE char10 VALUE 'ASC'.

CONSTANTS : gc_doc  TYPE saedoktyp VALUE 'ZJEPSUPDOC',
            gc_docx TYPE saedoktyp VALUE 'ZJEPSUPDOX',
            gc_xls  TYPE saedoktyp VALUE 'ZJEPSUPXLS',
            gc_xlsx TYPE saedoktyp VALUE 'ZJEPSUPXLX',
            gc_pdf  TYPE saedoktyp VALUE 'ZJEPSUPPDF',
            gc_zip  TYPE saedoktyp VALUE 'ZJEPSUPZIP',
            gc_dc   TYPE char4 VALUE 'DOC',
            gc_dx   TYPE char4 VALUE 'DOCX',
            gc_xs   TYPE char4 VALUE 'XLS',
            gc_xx   TYPE char4 VALUE 'XLSX',
            gc_pf   TYPE char4 VALUE 'PDF',
            gc_zp   TYPE char4 VALUE 'ZIP'.
