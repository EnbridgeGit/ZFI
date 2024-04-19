REPORT zfapi035 MESSAGE-ID zs.

************************************************************************
*  Project:     Mercator replacement                                   *
*  Interface:   Banner-AP customer refund (IFAP021)                    *
*  Author:      Mohammad T. Khan                                       *
*  Date:        August, 2005                                           *
*  Description:                                                        *
*     - The purpose of this program is to map the data for Banner-AP   *
*       customer refund interface. It will replace the mapping through *
*       Mercator.                                                      *
*                                                                      *
*  2006/04/04 GYMANA - Modified initialization of BGR00, BBKPF & ZBSEG *
*                      tables so no changes have to be made to this    *
*                      ABAP when changes are made to any of the tables *
*                                                                      *
*  2011/06/27 GYMANA - TR804 (Cost of Gas project)                     *
*                      Changed code to use posting key '02' instead of *
*                      '40'.  Also changed the variant to use GL acct  *
*                      'BANNER'                                        *
*  2011/07/26 GYMANA - TR804 - Change text to: <CustNO'-'PremNO>'      *
*                      In order for the text to fit, Leading two zeroes*
*                      are truncated                                   *
*  2011/09/23 GYMANA - TR804 - Reset CUST/PREM text back (leading      *
*                      zeroes and no dash '-'.                         *
*  2012/03/14 GYMANA - TR926 - Fix Address logic to move file street   *
*                      address to bseg street field.                   *
*  2012/10/29 GYMANA - SDP28245 - Change Address logic. Not working    *
*                      correctly                                       *
************************************************************************
TABLES:  dd03l.

DATA: msg(100) TYPE c.

*-----------------------------------------------------------------------
FIELD-SYMBOLS: <f1>.
DATA:    char(21)    TYPE c,
         nodata(1)   VALUE '/'.
*-----------------------------------------------------------------------

*Input file record
DATA: BEGIN OF inrec,
        chrq_num(15)    TYPE c,
        tran_type(2)    TYPE c,
        cust_code(9)    TYPE c,
        prem_code(7)    TYPE c,
        cust_name(90)   TYPE c,
        street_adrs(60) TYPE c,
        address02(60)   TYPE c,
        address03(60)   TYPE c,
        city_name(20)   TYPE c,
        stat_code(3)    TYPE c,
        zip_code(10)    TYPE c,
        nation(28)      TYPE c,
        check_date(11)  TYPE c,
        net_amount(11)  TYPE c,
        user_id(30)     TYPE c,
        actv_date(11)   TYPE c,
        seq_num(10)     TYPE c,
        street_name(30) TYPE c,
        street_num(12)  TYPE c,
        pdir_code(2)    TYPE c,
        ssfx_code(6)    TYPE c,
        pdir_post(2)    TYPE c,
        prem_zipc(10)   TYPE c,
        prem_city(20)   TYPE c,
        prem_state(3)   TYPE c,
        prem_sdscr(30)  TYPE c,
        prem_utyp(6)    TYPE c,
        prem_unit(6)    TYPE c,
        land_code(4)    TYPE c,
        prem_utdesc(35) TYPE c,
        attn_line(30)   TYPE c,

      END OF inrec.

*Output file Session Header
DATA: BEGIN OF z_bgr00.
        INCLUDE STRUCTURE bgr00.
DATA: END OF z_bgr00.

*Output file Document header
DATA: BEGIN OF z_bbkpf.
        INCLUDE STRUCTURE bbkpf.
DATA: END OF z_bbkpf.

*Output file Document line item
DATA: BEGIN OF z_zbseg.
        INCLUDE STRUCTURE zbseg.
DATA: END OF z_zbseg.

DATA: wrk_symbolic(4) TYPE c VALUE '$sys',
      wrk_amount      TYPE p DECIMALS 2,
      kount           TYPE i,
      rec3(73)        TYPE c,
      addr_string     TYPE string,
      string_length   TYPE i value 0.


SELECTION-SCREEN BEGIN OF BLOCK box1 WITH FRAME TITLE text-001.
SELECTION-SCREEN SKIP 1.
PARAMETER:  infile LIKE filename-fileextern
              DEFAULT '/usr/sap/interfaces/$sys/BANNER/zbis102.tmp'.
SELECTION-SCREEN SKIP 1.
PARAMETER: outfile LIKE filename-fileextern
              DEFAULT '/usr/sap/interfaces/$sys/BANNER/zbis102.chk'.
SELECTION-SCREEN SKIP 1.
PARAMETER:
 p_group  LIKE  bgr00-group DEFAULT 'ZAP-BANNER',
 p_tcode(4)                 DEFAULT 'FB01',
 p_blart  LIKE  bbkpf-blart DEFAULT 'K7',
 p_bukrs  LIKE  bbkpf-bukrs DEFAULT 'UGL',
 p_waers  LIKE  bbkpf-waers DEFAULT 'CAD',
 p_bktxt  LIKE  bbkpf-bktxt DEFAULT 'BANN CUSTOMER REFUND',
 p_hkontc LIKE  zbseg-hkont DEFAULT 'OTCUST', "GL Acct-Credit Entry (31)
 p_hkontd LIKE  zbseg-hkont DEFAULT 'BANNER'. "GL Acct-Debit Entry 40

SELECTION-SCREEN END OF BLOCK box1.

AT SELECTION-SCREEN OUTPUT.
  REPLACE wrk_symbolic WITH sy-sysid INTO: infile, outfile.
  CONDENSE: infile, outfile NO-GAPS.


START-OF-SELECTION.
*Replace system id in the file paths
  REPLACE wrk_symbolic WITH sy-sysid INTO: infile, outfile.
  CONDENSE infile  NO-GAPS.
  CONDENSE outfile NO-GAPS.

*Open Input File
  OPEN DATASET infile FOR INPUT IN TEXT MODE MESSAGE msg.
  IF ( sy-subrc <> 0 ).
    MESSAGE e002 WITH infile msg.
  ENDIF.

*Open Output File
  OPEN DATASET outfile FOR OUTPUT IN TEXT MODE MESSAGE msg.
  IF ( sy-subrc <> 0 ).
    MESSAGE e002 WITH outfile msg.
  ENDIF.

*Output file session header record type 0.
  PERFORM init_structures USING 'BGR00'.
  MOVE '0'           TO z_bgr00-stype.
  MOVE p_group       TO z_bgr00-group.
  MOVE sy-mandt      TO z_bgr00-mandt.    "Client
  MOVE 'BATCH'       TO z_bgr00-usnam.
  MOVE '////////'    TO z_bgr00-start.    "Session date
  TRANSFER z_bgr00 TO outfile.

* Do until the end of file.
  DO.
    READ DATASET infile INTO inrec.

*   If file is empty, stop run.
    IF ( sy-index = 1 ).
      IF ( sy-subrc = 4 ).
        MESSAGE i028.
        STOP.
      ENDIF.
    ENDIF.

*   When end of file, Exit.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

*Output file document header records type 1.
    DATA: wrk_month(3) TYPE c,
          wrk_mnr LIKE t247-mnr.

    PERFORM init_structures USING 'BBKPF'.
    MOVE '1'        TO  z_bbkpf-stype.
    MOVE:  p_tcode  TO  z_bbkpf-tcode,
           p_blart  TO  z_bbkpf-blart,
           p_bukrs  TO  z_bbkpf-bukrs,
           p_waers  TO  z_bbkpf-waers,
           p_bktxt  TO  z_bbkpf-bktxt,
           sy-datum TO  z_bbkpf-bldat,
           inrec-check_date+3(3) TO wrk_month.

    SELECT SINGLE mnr INTO wrk_mnr FROM t247 WHERE ktx = wrk_month.
    CONCATENATE inrec-check_date+7(4) wrk_mnr inrec-check_date+0(2)
                INTO z_bbkpf-budat.
    z_bbkpf-monat  =  inrec-check_date+3(3).
    CONCATENATE inrec-cust_code inrec-prem_code INTO
    z_bbkpf-xblnr.
    TRANSFER z_bbkpf TO outfile.

*Output file document line item records type 2.
*Line item with key 31
    PERFORM init_structures USING 'ZBSEG'.
    MOVE '2'               TO z_zbseg-stype.
    MOVE 'ZBSEG'           TO z_zbseg-tbnam.
    MOVE '31'              TO z_zbseg-newbs.
    MOVE 'N00'             TO z_zbseg-zterm.
    MOVE 'E'               TO z_zbseg-spras.
    WRITE inrec-net_amount TO z_zbseg-wrbtr RIGHT-JUSTIFIED.
    MOVE: sy-datum         TO z_zbseg-zfbdt,
          inrec-cust_name  TO z_zbseg-name1,
          inrec-zip_code   TO z_zbseg-pstlz,
          inrec-stat_code  TO z_zbseg-regio,
          p_hkontc         TO z_zbseg-hkont.

    IF inrec-city_name <> space.
      z_zbseg-ort01 = inrec-city_name.
    ELSE.
      z_zbseg-ort01 = '/'.
    ENDIF.

    IF inrec-nation  = 'CANADA'.
      z_zbseg-land1 = 'CA'.
    ELSEIF inrec-nation = 'U.S.A.'.
      z_zbseg-land1 = 'US'.
    ELSE.
      z_zbseg-land1 = '/'.
    ENDIF.

*    IF inrec-street_adrs <> space AND                            "TR926
*     ( inrec-address02 <> space  OR  inrec-address03 <> space ). "TR926
*      z_zbseg-name2 = inrec-street_adrs.                         "TR926
*    ELSEIF inrec-attn_line <> space AND inrec-nation = 'CANADA'. "TR926
*      z_zbseg-name2 = inrec-attn_line.                           "TR926
*    ELSE.                                                        "TR926
*      z_zbseg-name2 = inrec-street_adrs.                         "TR926
*    ENDIF.                                                       "TR926
*                                                                 "TR926
*    IF inrec-attn_line <> space   AND inrec-nation = 'CANADA'.   "TR926
*      z_zbseg-name3 = inrec-street_adrs.                         "TR926
*    ELSE.                                                        "TR926
*      z_zbseg-name3 = '/'.                                       "TR926
*    ENDIF.                                                       "TR926
*                                                                 "TR926
*    IF inrec-address02 <> space.                                 "TR926
*      CONCATENATE inrec-address02 inrec-address03 INTO           "TR926
*                      z_zbseg-stras SEPARATED BY space.          "TR926
*    ELSEIF inrec-address03 <> space.                             "TR926
*      MOVE inrec-address03 TO z_zbseg-stras.                     "TR926
*    ENDIF.                                                       "TR926
*
*    IF inrec-attn_line <> space   AND inrec-nation = 'CANADA'.   "TR926
*      z_zbseg-name2 = inrec-attn_line.                           "TR926
*    ELSE.                                                        "TR926
*      z_zbseg-name2 = '/'.                                       "TR926
*    ENDIF.                                                       "TR926
*                                                                 "TR926
*    IF inrec-street_adrs <> space.                               "TR926
*      z_zbseg-stras = inrec-street_adrs.                         "TR926
*    ELSE.                                                        "TR926
*      IF inrec-address02 <> space.                               "TR926
*        CONCATENATE inrec-address02 inrec-address03 INTO         "TR926
*                      z_zbseg-stras SEPARATED BY space.          "TR926
*      ELSEIF inrec-address03 <> space.                           "TR926
*        MOVE inrec-address03 TO z_zbseg-stras.                   "TR926
*      ENDIF.                                                     "TR926
*    ENDIF.                                                       "TR926

* 2012/10/29 - SDP28245 - GYMANA****************************************
* New Address Logic
    CONCATENATE inrec-address02 inrec-address03 INTO
                addr_string SEPARATED BY space.
    SHIFT addr_string LEFT DELETING LEADING space.
    string_length = strlen( addr_string ).

    IF inrec-street_adrs = space.
       IF inrec-attn_line <> space.
          z_zbseg-name2 = inrec-attn_line.
          z_zbseg-stras = addr_string.
       ELSE.
          IF string_length < 35.
             z_zbseg-stras = addr_string.
          ELSE.
             z_zbseg-name2 = inrec-address02.
             z_zbseg-stras = inrec-address03.
          ENDIF.
       ENDIF.
    ELSE.
       IF inrec-attn_line <> space.
          z_zbseg-name2 = inrec-attn_line.
          IF addr_string = space.
             z_zbseg-stras = inrec-street_adrs.
          ELSE.
             CONCATENATE inrec-address02
                         inrec-address03
                         inrec-street_adrs INTO
                         z_zbseg-stras SEPARATED BY space.
          ENDIF.
       ELSE.
          IF addr_string = space.
             z_zbseg-stras = inrec-street_adrs.
          ELSE.
             z_zbseg-name2 = inrec-street_adrs.
             z_zbseg-stras = addr_string.
          ENDIF.
       ENDIF.
    ENDIF.
* 2012/10/29 - SDP28245 - GYMANA****************************************

    TRANSFER z_zbseg TO outfile.

*Output file document line item records type 2.
*Line item with key 02
    PERFORM init_structures USING 'ZBSEG'.
    MOVE '2'               TO z_zbseg-stype.
    MOVE 'ZBSEG'           TO z_zbseg-tbnam.
    MOVE '02'              TO z_zbseg-newbs.

    CONCATENATE inrec-cust_code inrec-prem_code INTO
    z_zbseg-sgtxt.
    WRITE inrec-net_amount TO z_zbseg-wrbtr RIGHT-JUSTIFIED.
    WRITE p_hkontd TO  z_zbseg-hkont LEFT-JUSTIFIED.
*  MOVE  P_HKONTD TO  Z_ZBSEG-HKONT.
    TRANSFER z_zbseg TO outfile.


*Output file document line item records type 3.
    CONCATENATE '3REFUND' space space space inrec-street_num
      inrec-pdir_code inrec-street_name inrec-ssfx_code inrec-pdir_post
      inrec-prem_utyp inrec-prem_unit space space space inrec-prem_city
      inrec-prem_state inrec-prem_zipc INTO rec3 SEPARATED BY space.

    TRANSFER rec3 TO outfile.

  ENDDO.
  CLOSE DATASET: infile, outfile.

  CLEAR msg.
  MOVE '**** OUTPUT FILE CREATED *****' TO msg.
  MESSAGE i019 WITH msg.

*======================  INIT_STRUCTURES  =============================
*  Used to initialize the record to '/'
*======================================================================
FORM init_structures USING tablenam.
  SELECT * FROM dd03l WHERE tabname = tablenam.
    CLEAR char.
    char(2) = 'Z_'.
    char+2(5) = tablenam.
    char+7(1) = '-'.
    char+8    = dd03l-fieldname.
    ASSIGN (char) TO <f1>.
    <f1> = nodata.
  ENDSELECT.
ENDFORM.                    "INIT_STRUCTURES
********************* END OF PROGRAM ********************
