---
title: "Behaviour and Policy"
slug: "behaviour-and-policy"
draft: false
images: []
weight: 9998
type: docs
toc: true
---

## Auto Version File If Exists With The same name
If file exists with same name then it will update the file with new version.

For register bean Class in `service-context.xml` file

    <bean id="autoVersionByNameBehaviour" class="test.demoamp.AutoVersionByNameBehaviour" init-method="init">
         <property name="policyComponent" ref="policyComponent"/>
         <property name="nodeService" ref="NodeService"/>
         <property name="contentService" ref="ContentService"/>
         <property name="siteService" ref="SiteService" />
         <property name="fileFolderService" ref="FileFolderService"/>
         
         <property name="activityService" ref="activityService"/>
         </bean>


and the java class


    import java.net.URLEncoder;
    
    import org.alfresco.model.ContentModel;
    import org.alfresco.repo.node.NodeServicePolicies;
    import org.alfresco.repo.policy.Behaviour;
    import org.alfresco.repo.policy.JavaBehaviour;
    import org.alfresco.repo.policy.PolicyComponent;
    import org.alfresco.service.cmr.activities.ActivityService;
    import org.alfresco.service.cmr.model.FileFolderService;
    import org.alfresco.service.cmr.model.FileInfo;
    import org.alfresco.service.cmr.repository.ChildAssociationRef;
    import org.alfresco.service.cmr.repository.ContentReader;
    import org.alfresco.service.cmr.repository.ContentService;
    import org.alfresco.service.cmr.repository.ContentWriter;
    import org.alfresco.service.cmr.repository.NodeRef;
    import org.alfresco.service.cmr.repository.NodeService;
    import org.alfresco.service.cmr.site.SiteInfo;
    import org.alfresco.service.cmr.site.SiteService;
    import org.apache.commons.io.FilenameUtils;
    import org.json.JSONStringer;
    import org.json.JSONWriter;
    
    public class AutoVersionByNameBehaviour
    implements NodeServicePolicies.OnCreateNodePolicy {
        private PolicyComponent policyComponent;
        private NodeService nodeService;
        private ContentService contentService;
        private ActivityService activityService;
        private SiteService siteService;
        private FileFolderService fileFolderService;
    
        public void init() {
            this.policyComponent.bindClassBehaviour(NodeServicePolicies.OnCreateNodePolicy.QNAME, ContentModel.TYPE_CONTENT, (Behaviour)new JavaBehaviour((Object)this, "onCreateNode", Behaviour.NotificationFrequency.TRANSACTION_COMMIT));
        }
    
        public void onCreateNode(ChildAssociationRef childAssocRef) {
            NodeRef previouslyExistentDoc;
            NodeRef uploadedNodeRef = childAssocRef.getChildRef();
            if (this.nodeService.exists(uploadedNodeRef) && this.isContentDoc(uploadedNodeRef) && (previouslyExistentDoc = this.existedPreviousDocument(uploadedNodeRef)) != null) {
                ContentReader reader = this.contentService.getReader(uploadedNodeRef, ContentModel.PROP_CONTENT);
                ContentWriter writer = this.contentService.getWriter(previouslyExistentDoc, ContentModel.PROP_CONTENT, true);
                writer.putContent(reader);
                this.nodeService.addAspect(uploadedNodeRef, ContentModel.ASPECT_HIDDEN, null);
                this.postActivityUpdated(previouslyExistentDoc);
                this.nodeService.deleteNode(uploadedNodeRef);
            }
        }
    
        private void postActivityUpdated(NodeRef nodeRef) {
            SiteInfo siteInfo = this.siteService.getSite(nodeRef);
            String jsonActivityData = "";
            try {
                JSONWriter jsonWriter = new JSONStringer().object();
                jsonWriter.key("title").value((Object)this.nodeService.getProperty(nodeRef, ContentModel.PROP_NAME).toString());
                jsonWriter.key("nodeRef").value((Object)nodeRef.toString());
                StringBuilder sb = new StringBuilder("document-details?nodeRef=");
                sb.append(URLEncoder.encode(nodeRef.toString(), "UTF-8"));
                jsonWriter.key("page").value((Object)sb.toString());
                jsonActivityData = jsonWriter.endObject().toString();
            }
            catch (Exception e) {
                throw new RuntimeException(e);
            }
            FileInfo fileInfo = this.fileFolderService.getFileInfo(nodeRef);
            this.activityService.postActivity("org.alfresco.documentlibrary.file-updated", siteInfo == null ? null : siteInfo.getShortName(), siteInfo == null ? null : "documentLibrary", jsonActivityData, null, fileInfo);
        }
    
        private boolean isContentDoc(NodeRef nodeRef) {
            return this.nodeService.getType(this.nodeService.getPrimaryParent(nodeRef).getParentRef()).isMatch(ContentModel.TYPE_FOLDER);
        }
    
        private NodeRef existedPreviousDocument(NodeRef currentNodeRef) {
            String fileName = AutoVersionByNameBehaviour.cleanNumberedSuffixes(this.nodeService.getProperty(currentNodeRef, ContentModel.PROP_NAME).toString());
            if (!fileName.equals(this.nodeService.getProperty(currentNodeRef, ContentModel.PROP_NAME).toString())) {
                NodeRef folder = this.nodeService.getPrimaryParent(currentNodeRef).getParentRef();
                return this.nodeService.getChildByName(folder, ContentModel.ASSOC_CONTAINS, fileName);
            }
            return null;
        }
    
        public static String cleanNumberedSuffixes(String fileName) {
            String cleanedFileName = fileName;
            String baseName = FilenameUtils.getBaseName((String)fileName);
            if (baseName.indexOf("-") != -1 && AutoVersionByNameBehaviour.isInteger(baseName.substring(baseName.lastIndexOf("-") + 1, baseName.length()))) {
                return baseName.substring(0, baseName.lastIndexOf("-")) + FilenameUtils.EXTENSION_SEPARATOR_STR + FilenameUtils.getExtension((String)fileName);
            }
            return cleanedFileName;
        }
    
        public static boolean isInteger(String s) {
            boolean isValidInteger = false;
            try {
                Integer.parseInt(s);
                isValidInteger = true;
            }
            catch (NumberFormatException var2_2) {
                // empty catch block
            }
            return isValidInteger;
        }
    
        public void setPolicyComponent(PolicyComponent policyComponent) {
            this.policyComponent = policyComponent;
        }
    
        public void setNodeService(NodeService nodeService) {
            this.nodeService = nodeService;
        }
    
        public void setContentService(ContentService contentService) {
            this.contentService = contentService;
        }
    
        public void setActivityService(ActivityService activityService) {
            this.activityService = activityService;
        }
    
        public void setSiteService(SiteService siteService) {
            this.siteService = siteService;
        }
    
        public void setFileFolderService(FileFolderService fileFolderService) {
            this.fileFolderService = fileFolderService;
        }

}


