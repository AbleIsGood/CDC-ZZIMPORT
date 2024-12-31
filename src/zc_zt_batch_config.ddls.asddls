@AccessControl.authorizationCheck: #CHECK
@Metadata.allowExtensions: true
@EndUserText.label: 'Projection View for ZR_ZT_BATCH_CONFIG'
define root view entity ZC_ZT_BATCH_CONFIG
  provider contract transactional_query
  as projection on ZR_ZT_BATCH_CONFIG
{
  key UUID,
  Object,
  Objectname,
  Fmname,
  Mimetype,
  Sheetname,
  Structname,
  MimeTypeForTemplate,
  Template,
  FileName,
  StartLine,
  StartColumn,
  LocalLastChangedAt
  
}
