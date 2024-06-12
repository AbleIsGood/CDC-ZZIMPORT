FUNCTION z_mm_dtimp_001.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(IO_DATA) TYPE REF TO  DATA OPTIONAL
*"     VALUE(IV_STRUC) TYPE  ZZESTRUCTNAME OPTIONAL
*"  EXPORTING
*"     REFERENCE(EO_DATA) TYPE REF TO  DATA
*"----------------------------------------------------------------------
*&---按模板创建数据，iv_struc 批导设置是提供结构参数
  CREATE DATA eo_data TYPE TABLE OF (iv_struc).
*&---数据赋值（IN ==> OUT）
  eo_data->* = io_data->*.

*&---循环处理数据
*&---=============================使用API 步骤01
*&---=========1.API 类使用变量
*&---定义场景使用变量
  DATA: lr_cscn TYPE if_com_scenario_factory=>ty_query-cscn_id_range.
*&---Find CA by Scenario ID
  lr_cscn = VALUE #( ( sign = 'I' option = 'EQ' low = 'YY1_API' ) ).
*&---创建实例
  DATA(lo_factory) = cl_com_arrangement_factory=>create_instance( ).
  lo_factory->query_ca(
          EXPORTING
            is_query           = VALUE #( cscn_id_range = lr_cscn )
          IMPORTING
            et_com_arrangement = DATA(lt_ca) ).
  IF lt_ca IS INITIAL.
    EXIT.
  ENDIF.

*&---take the first one
  READ TABLE lt_ca INTO DATA(lo_ca) INDEX 1.
*&---get destination based on Communication Arrangement and the service ID
  TRY.
      DATA(lo_dest) = cl_http_destination_provider=>create_by_comm_arrangement(
                  comm_scenario  = 'YY1_API'
                  service_id     = 'YY1_API_REST'
                  comm_system_id = lo_ca->get_comm_system_id( ) ).
    CATCH cx_http_dest_provider_error INTO DATA(lx_http_dest_provider_error).
*              out->write( lx_http_dest_provider_error->get_text( ) ).
      EXIT.
  ENDTRY.

*&---财务信息
  TYPES: BEGIN OF ty_account,
           GLAccount        TYPE string,
           CostCenter       TYPE string,
           WBSElement       TYPE string,
           MasterFixedAsset TYPE string,
           FixedAsset       TYPE string,
         END OF ty_account,
         BEGIN OF to_PurchaseReqnAcctAssgmt,
           results TYPE STANDARD TABLE OF ty_account WITH DEFAULT KEY,
         END OF to_PurchaseReqnAcctAssgmt,
*&---行信息
         BEGIN OF ty_item,
           PurchaseRequisitionType        TYPE string,
           "PurchaseRequisition            TYPE STRING,
           "PurchaseRequisitionItem        TYPE STRING,
           AccountAssignmentCategory      TYPE string,
           Material                       TYPE string,
           PurchaseRequisitionItemText    TYPE string,
           RequestedQuantity              TYPE string,
           BaseUnit                       TYPE string,
           DeliveryDate                   TYPE string,
           MaterialGroup                  TYPE string,
           Plant                          TYPE string,
           StorageLocation                TYPE string,
           PurchasingGroup                TYPE string,
           RequisitionerName              TYPE string,
           RequirementTracking            TYPE string,
           PurchaseRequisitionPrice       TYPE string,
           TaxCode                        TYPE string,
           PurchaseRequisitionReleaseDate TYPE string,

           to_PurchaseReqnAcctAssgmt type to_PurchaseReqnAcctAssgmt,
         END OF ty_item,
         results TYPE STANDARD TABLE OF ty_item WITH DEFAULT KEY,

*&---申请
         BEGIN OF to_PurchaseReqnItem,

           results            TYPE STANDARD TABLE OF ty_item WITH DEFAULT KEY,
         END OF to_PurchaseReqnItem,
         BEGIN OF ty_reqitem,
           PurReqnDescription TYPE string,
           to_PurchaseReqnItem TYPE to_PurchaseReqnItem,
         END OF ty_reqitem.
  DATA:ls_account type ty_account,
       ls_item    TYPE ty_item,
       ls_reqitem TYPE ty_reqitem.
*&---抬头
  TYPES:BEGIN OF ty_head,
        PurchaseRequisition    TYPE STRING,
          message              TYPE bapi_msg,
          type                 TYPE bapi_mtype,
        END OF ty_head.
  data:ls_head type ty_head,
       lt_head TYPE TABLE of ty_head.
*&---API
  DATA:lt_mapping TYPE /ui2/cl_json=>name_mappings,
       lv_json    TYPE /ui2/cl_json=>json.
*&---API 返回结构处理
  DATA:BEGIN OF ls_result,
         conditionrecord TYPE string,
       END OF ls_result.
  DATA:ls_data  TYPE REF TO data,
       ls_datae TYPE REF TO data.

*&---导入结构JSON MAPPING
  lt_mapping = VALUE #(
                        ( abap = 'PurReqnDescription'          json = 'PurReqnDescription'         )

                        ( abap = 'to_PurchaseReqnItem'          json = 'to_PurchaseReqnItem'         )
                        ( abap = 'results'                      json = 'results'                     )
                        ( abap = 'PurchaseRequisitionType'      json = 'PurchaseRequisitionType'     )
                        ( abap = 'PurchaseRequisition'          json = 'PurchaseRequisition'     )
                        ( abap = 'PurchaseRequisitionItem'      json = 'PurchaseRequisitionItem'     )
                        ( abap = 'AccountAssignmentCategory'    json = 'AccountAssignmentCategory'   )
                        ( abap = 'Material'                     json = 'Material'                    )
                        ( abap = 'PurchaseRequisitionItemText'  json = 'PurchaseRequisitionItemText' )
                        ( abap = 'RequestedQuantity'            json = 'RequestedQuantity'           )
                        ( abap = 'BaseUnit'                     json = 'BaseUnit'                    )
                        ( abap = 'DeliveryDate'                 json = 'DeliveryDate'                )
                        ( abap = 'MaterialGroup'                json = 'MaterialGroup'               )
                        ( abap = 'Plant'                        json = 'Plant'                       )
                        ( abap = 'StorageLocation'              json = 'StorageLocation'             )
                        ( abap = 'PurchasingGroup'              json = 'PurchasingGroup'             )
                        ( abap = 'RequisitionerName'            json = 'RequisitionerName'           )
                        ( abap = 'RequirementTracking'          json = 'RequirementTracking'         )
                        ( abap = 'PurchaseRequisitionPrice'     json = 'PurchaseRequisitionPrice'    )
                        ( abap = 'TaxCode'                      json = 'TaxCode'                     )
                        ( abap = 'PurchaseRequisitionReleaseDate'   json = 'PurchaseRequisitionReleaseDate'      )

                        ( abap = 'to_PurchaseReqnAcctAssgmt'    json = 'to_PurchaseReqnAcctAssgmt'         )
                        ( abap = 'GLAccount'                    json = 'GLAccount'         )
                        ( abap = 'CostCenter'                   json = 'CostCenter'         )
                        ( abap = 'WBSElement'                   json = 'WBSElement'         )
                        ( abap = 'MasterFixedAsset'             json = 'MasterFixedAsset'    )
                        ( abap = 'FixedAsset'                   json = 'FixedAsset'         )

                       ).
*&---抬头行数处理
  clear:lt_head.
  loop at eo_data->* ASSIGNING FIELD-SYMBOL(<ls_head>).
    clear:ls_head.
    ls_head-purchaserequisition = <ls_head>-('PurchaseRequisition').
    APPEND ls_head to lt_head.
  ENDLOOP.
  SORT lt_head.
  delete ADJACENT DUPLICATES FROM lt_head.

*&---循环调用API 处理
  DATA:lv_date  TYPE string.
  loop at lt_head into ls_head.
  CLEAR:ls_reqitem.
  LOOP AT eo_data->* ASSIGNING FIELD-SYMBOL(<line>).
    if ls_head-PurchaseRequisition <> <line>-('PurchaseRequisition').
      CONTINUE.
    ENDIF.
*&---==================组织json 数据
*&---有效范围
    clear:ls_item.
    ls_reqitem-purreqndescription = <line>-('purreqndescription').

*&---有效期到，转换为DATE T TIME famat
    lv_date = <line>-('purchaserequisitionreleasedate').
    if lv_date is not INITIAL.
    lv_date = lv_date+0(4) && '-' && lv_date+4(2) && '-' && lv_date+6(2) && 'T00:00:00'.
    ls_item-purchaserequisitionreleasedate   =  lv_date .
    endif.
*&---有效期从，转换为DATE T TIME famat


    ls_item-purchaserequisitiontype        = <line>-('purchaserequisitiontype').
    ls_item-AccountAssignmentCategory        = <line>-('AccountAssignmentCategory').
    ls_item-material                  = <line>-('material').
    ls_item-purchaserequisitionitemtext    = <line>-('purchaserequisitionitemtext').
    DATA:lv_quantity TYPE p LENGTH 13 DECIMALS 3.
    lv_quantity = <line>-('requestedquantity').
    ls_item-requestedquantity        = lv_quantity.
    CONDENSE ls_item-requestedquantity.
    ls_item-baseunit        = <line>-('baseunit').

    lv_date = <line>-('deliverydate').
    if lv_date is not INITIAL.
    lv_date = lv_date+0(4) && '-' && lv_date+4(2) && '-' && lv_date+6(2) && 'T00:00:00'.
    ls_item-deliverydate = lv_date.
    endif.

    ls_item-materialgroup                  = <line>-('materialgroup').
    ls_item-plant        = <line>-('plant').
    ls_item-storagelocation        = <line>-('storagelocation').
    ls_item-purchasinggroup        = <line>-('purchasinggroup').
    ls_item-requisitionername        = <line>-('requisitionername').
    ls_item-requirementtracking        = <line>-('requirementtracking').

    DATA:lv_price TYPE p LENGTH 11 DECIMALS 2.
    lv_price = <line>-('purchaserequisitionprice').
    ls_item-purchaserequisitionprice        = lv_price.
    CONDENSE ls_item-purchaserequisitionprice.

    "ls_item-companycode        = <line>-('companycode').
    ls_item-taxcode        = <line>-('taxcode').

    ls_account-glaccount = <line>-('glaccount').
    ls_account-CostCenter = <line>-('CostCenter').
    ls_account-WBSElement = <line>-('WBSElement').
    ls_account-MasterFixedAsset = <line>-('MasterFixedAsset').
    ls_account-FixedAsset = <line>-('FixedAsset').
    APPEND ls_account to ls_item-to_purchasereqnacctassgmt-results.
    APPEND ls_item TO ls_reqitem-to_purchasereqnitem-results.
**&---==========条件头，条件有效期内表
*    ls_condition-conditiontable            = '501'.                  " 固定条件表
*    ls_condition-conditiontype             = 'ZTX1'.                 " 固定条件类型
*    DATA:lv_amount TYPE p LENGTH 15 DECIMALS 3.
*    lv_amount = <line>-('amount').
*    ls_condition-conditionratevalue        = lv_amount."<line>-('amount').      " 导入值
*    CONDENSE ls_condition-conditionratevalue.
*    ls_condition-conditionratevalueunit    = <line>-('amountc').     " 导入值单位，%
*    APPEND ls_validity TO ls_condition-to_purgprcgcndnrecdvalidity.
  ENDLOOP.

*&---接口HTTP 链接调用
    TRY.
        DATA(lo_http_client) = cl_web_http_client_manager=>create_by_http_destination( lo_dest ).
        DATA(lo_request) = lo_http_client->get_http_request(   ).
        lo_http_client->enable_path_prefix( ).

        DATA(lv_uri_path) = |/API_PURCHASEREQ_PROCESS_SRV/A_PurchaseRequisitionHeader|.
        lo_request->set_uri_path( EXPORTING i_uri_path = lv_uri_path ).
        lo_request->set_header_field( i_name = 'Accept' i_value = 'application/json').
        "lo_request->set_header_field( i_name = 'If-Match' i_value = '*' ).
        lo_http_client->set_csrf_token(  ).

        lo_request->set_content_type( 'application/json' ).
        lv_json = /ui2/cl_json=>serialize(
                    data          = ls_reqitem
                    name_mappings = lt_mapping ).
        lo_request->set_text( lv_json ).

*&---执行http post 方法
        DATA(lo_response) = lo_http_client->execute( if_web_http_client=>post ).
*&---获取http reponse 数据
        DATA(lv_response) = lo_response->get_text(  ).

        "<line>-('Message') = lv_response.
*&---确定http 状态
        FIELD-SYMBOLS:<lfs_error>,
                      <lfs_code>,
                      <lfs_message>,
                      <lfs_value>,
                      <lfs_data>.
        DATA(status) = lo_response->get_status( ).
        IF status-code = '201'.
          /ui2/cl_json=>deserialize(
           EXPORTING
             json         = lv_response
             pretty_name  = /ui2/cl_json=>pretty_mode-user
             assoc_arrays = abap_true
           CHANGING
             data         = ls_data ).
          LS_HEAD-('Type') = ''.
          ASSIGN ls_data->* TO <lfs_data>.
          IF sy-subrc = 0.
            ASSIGN COMPONENT 'D' OF STRUCTURE <lfs_data> TO FIELD-SYMBOL(<lfs_d>).
            IF sy-subrc = 0.
              ASSIGN <lfs_d>->* TO FIELD-SYMBOL(<lfs_df>).
              IF sy-subrc = 0.
                ASSIGN COMPONENT 'PURCHASEREQUISITION'  OF STRUCTURE <lfs_df> TO <lfs_message>.
                IF sy-subrc = 0.
                  LS_HEAD-('Message') = '采购申请创建成功:'  && <lfs_message>->*.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ELSE.
          /ui2/cl_json=>deserialize(
                EXPORTING
                  json         = lv_response
                  pretty_name  = /ui2/cl_json=>pretty_mode-user
                  assoc_arrays = abap_true
                CHANGING
                  data         = ls_datae ).
          LS_HEAD-('Type') = 'E'.
          ASSIGN ls_datae->* TO <lfs_data>.
          IF sy-subrc = 0.
            ASSIGN COMPONENT 'ERROR' OF STRUCTURE <lfs_data> TO <lfs_error>.
            IF sy-subrc = 0.
*              <line>-('Message') = <line>-('supplier') && '/' &&
*                                   <line>-('material') && '/' &&
*                                   <line>-('ekorg')    && '/' &&
*                                  'Created Error'.
              ASSIGN <lfs_error>->* TO FIELD-SYMBOL(<lfs_data_error>).
              IF sy-subrc = 0.
                ASSIGN COMPONENT 'CODE'  OF STRUCTURE <lfs_data_error> TO <lfs_code>.
                IF sy-subrc = 0.
                  LS_HEAD-('Message') = LS_HEAD-('Message') && '/Error Code:' && <lfs_code>->*.
                  ASSIGN COMPONENT 'MESSAGE'  OF STRUCTURE <lfs_data_error> TO <lfs_message>.
                  IF sy-subrc = 0.
                    ASSIGN <lfs_message>->* TO FIELD-SYMBOL(<lfs_error_messsage>).
                    IF sy-subrc = 0.
                      ASSIGN COMPONENT 'VALUE'  OF STRUCTURE <lfs_error_messsage> TO <lfs_value>.
                      IF sy-subrc = 0.
                        LS_HEAD-('Message') = LS_HEAD-('Message') && '/,Error Message:' && <lfs_value>->*.
                      ENDIF..
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      CATCH cx_web_http_client_error INTO DATA(lx_web_http_client_error).
        RETURN.
    ENDTRY.
    MODIFY LT_HEAD FROM LS_HEAD.
    ENDLOOP.
    loop at eo_data->* ASSIGNING FIELD-SYMBOL(<LS_MSG>).
     READ TABLE LT_HEAD INTO LS_HEAD WITH KEY PurchaseRequisition = <LS_MSG>-('PurchaseRequisition').
     IF SY-SUBRC = 0.
        <LS_MSG>-('Type') = LS_HEAD-type.
        <LS_MSG>-('Message') = LS_HEAD-Message.
     ENDIF.

    ENDLOOP.
ENDFUNCTION.
