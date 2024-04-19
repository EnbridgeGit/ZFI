* 4.70 - EA 1.0
* XOWALNK013374 02012002 Enhancements Credit Card Clearing
*                        change '-' to '_'

***INCLUDE RPC1TA00.

***********************************************************************
********* Data-Definition TA-Cluster PCL1 *****************************
***********************************************************************

* begin of XOWK013374
*DATA: BEGIN OF TA-KEY,                 "Schlüssel TA-Cluster
*        CCOMP(2),            "Datenelement CComp
*      END OF TA-KEY.
*
*DATA: BEGIN OF CCDAT OCCURS 100,
*        CCOMP(2),
*        ABNUM(10),
*        BEGDA LIKE SY-DATUM,
*        BEGUZ LIKE SY-UZEIT,
*        ENDDA LIKE SY-DATUM,
*        ENDUZ LIKE SY-UZEIT,
*        UNAME LIKE SY-UNAME,
*        REPID LIKE SY-REPID,
*        FINAL(1),
*      END OF CCDAT.
*
*DATA: RP-EXP-TA-SUBRC LIKE SY-SUBRC,
*      RP-IMP-TA-SUBRC LIKE SY-SUBRC.
*
** Export TA-Cluster to PCL1
*DEFINE RP-EXP-C1-TA.
*  PCL1-AEDTM = SY-DATUM.
*  PCL1-UNAME = SY-UNAME.
*  PCL1-PGMID = SY-REPID.
*
*  EXPORT
*         CCDAT
*         TO DATABASE PCL1(TA) ID TA-KEY.
*
*  RP-EXP-TA-SUBRC = SY-SUBRC.
*END-OF-DEFINITION.
*
** Import TA-Cluster to PCL1
*DEFINE RP-IMP-C1-TA.
*
*  IMPORT
*         CCDAT
*         FROM DATABASE PCL1(TA) ID TA-KEY.
*
*  RP-IMP-TA-SUBRC = SY-SUBRC.
*
*END-OF-DEFINITION.

DATA: BEGIN OF TA_KEY,                 "Schlüssel TA-Cluster
        CCOMP type CCOMP,          "Datenelement CComp
      END OF TA_KEY.

data: ccdat type table of ptk35 with header line.

DATA: RP_EXP_TA_SUBRC LIKE SY-SUBRC,
      RP_IMP_TA_SUBRC LIKE SY-SUBRC.

* Export TA-Cluster to PCL1
DEFINE RP_EXP_C1_TA.
  PCL1-AEDTM = SY-DATUM.
  PCL1-UNAME = SY-UNAME.
  PCL1-PGMID = SY-REPID.

  EXPORT
         CCDAT
         TO DATABASE PCL1(TA) ID TA_KEY.

  RP_EXP_TA_SUBRC = SY-SUBRC.
END-OF-DEFINITION.

* Import TA-Cluster to PCL1
DEFINE RP_IMP_C1_TA.

  IMPORT
         CCDAT
         FROM DATABASE PCL1(TA) ID TA_KEY.

  RP_IMP_TA_SUBRC = SY-SUBRC.

END-OF-DEFINITION.
* end of XOWK013374
