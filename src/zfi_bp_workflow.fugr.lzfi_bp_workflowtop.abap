FUNCTION-POOL ZFI_BP_WORKFLOW.              "MESSAGE-ID ..


DATA: gtp_businesspartner TYPE but000-partner,
      gtp_approver_level  TYPE z_gl_approver_level,
      gtp_okcode      TYPE syucomm,
      gtp_comment     TYPE zfis_comments-comments,
      gtp_username    TYPE username,
      gtp_manager     TYPE username,
      gtp_comments    TYPE zfis_comments-comments,
      gtp_workflow_id TYPE swr_struct-workitemid.

DATA: gta_objtxt     TYPE swftlisti1,
      gst_doc_chng   TYPE sodocchgi1,
      gta_objpack    TYPE swftpcklst,
      gta_objbin     TYPE swftlisti1,
      gta_reclist    TYPE bubas_t_receiver.

DATA: gtp_decision_type TYPE zfis_tv_workflow-conf_type.
