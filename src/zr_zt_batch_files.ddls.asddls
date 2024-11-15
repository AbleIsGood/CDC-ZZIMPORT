@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: '##GENERATED ZZT_BATCH_FILES'
define root view entity ZR_ZT_BATCH_FILES
  as select from zzt_batch_files
  association [0..1] to ZR_ZT_BATCH_CONFIG as _configuration on $projection.UuidConf = _configuration.UUID
{
  key uuid                                 as UUID,
      @Consumption.valueHelpDefinition: [{  entity: {   name: 'ZC_ZT_BATCH_CONFIG' ,
                                                              element: 'UUID'  }     }]
      uuid_conf                            as UuidConf,
      @Semantics.mimeType: true
      mime_type                            as MimeType,
      @Semantics.largeObject:
      { mimeType: 'MimeType',
      fileName: 'FileName',
      contentDispositionPreference: #ATTACHMENT }
      attachment                           as Attachment,
      file_name                            as FileName,
      jobcount                             as Jobcount,
      jobname                              as Jobname,
      cast( loghandle as abap.char( 22 ) ) as LogHandle,
      @Semantics.user.createdBy: true
      created_by                           as CreatedBy,
      @Semantics.systemDateTime.createdAt: true
      created_at                           as CreatedAt,
      @Semantics.user.lastChangedBy: true
      last_changed_by                      as LastChangedBy,
      @Semantics.systemDateTime.lastChangedAt: true
      last_changed_at                      as LastChangedAt,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      local_last_changed_at                as LocalLastChangedAt,
 

      
      _configuration
}
